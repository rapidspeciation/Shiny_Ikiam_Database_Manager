// What to mark in the team's paper Emergidos notebook, which lists every
// butterfly of the insectary in Insectary ID order (the sheet's rows). Two lists:
//
// «Actualizar el cuaderno» (Censo): after a census, every butterfly of the censused
// species in the insectary on that day, each with what to do on paper: ☺ seen,
// ✗ disappeared (to write), ▬ already dead in the database (to highlight with the
// marker pen: a death before or after the census).
//
// «Filas para resaltar» (Muertes): every butterfly whose death was entered in
// the database since a moment, to highlight it. The moment a death was entered
// comes from the history (changes of Death_date / Death_cause, made in the app
// or seen in Google Sheets by the sync, with their actions' times), not from
// the sheet's Death_date: that is the day it died, often entered days later
// (and a census's disappearances carry the census day). The history starts
// when the app started following the workbook; deaths entered before that
// cannot be dated. Deaths kept in the app (not yet in Google Sheets) are listed
// too, marked `staged`.
//
// The first list reads a few cells of every butterfly of Insectary_data (about
// 60 ms on the team's workbook): read once per state of the local copy, in the
// Revisión worker thread where there is one (server/checks-host.mjs), and kept.

import { binomial, inTaxon, isAlive, serialOf, speciesKey, stagedButterflies, DISAPPEARED } from './census.mjs';
import { recordsStamp } from './checks.mjs';
import { msgError } from './messages.mjs';

const SHEET = 'Insectary_data';
const DEATH = ['Death_date', 'Death_cause'];
const parse = text => (text ? JSON.parse(text) : null);
const blank = v => v === null || v === undefined || (typeof v === 'string' && v.trim() === '');
const text = v => (blank(v) ? '' : String(v).trim());
const upper = v => text(v).toUpperCase();
const fail = (code, message, status = 400) => msgError(message, { code, status });
const ISO_DAY = /^\d{4}-\d{2}-\d{2}$/;
const validDay = day => ISO_DAY.test(day) && !Number.isNaN(Date.parse(day));
/** A death as a line shows it: its date (a serial, or what the cell says), its cause, and whether it is only in the app. */
const deathOf = (values, staged) => ({
  date: typeof values.Death_date === 'number' ? values.Death_date : text(values.Death_date) || null,
  cause: text(values.Death_cause),
  staged,
});

/** Where a finished census's disappearances are: true while not in Google Sheets (kept in the app, being written, refused). */
function censusWaiting(store, c) {
  if (c.action_id)
    return store.db.prepare('SELECT status FROM actions WHERE id = ?').get(c.action_id)?.status === 'failed';
  if (c.outbox_id) {
    const item = store.outbox?.get(c.outbox_id);
    return !!item && item.status !== 'done';
  }
  if (!c.staged_entry) return false;
  return !!store.db
    .prepare("SELECT 1 FROM staged WHERE entry_id = ? AND status IN ('staged','sent') LIMIT 1")
    .get(c.staged_entry);
}

/**
 * «Actualizar el cuaderno»: the butterflies of `species` (binomials; their
 * subspecies together, as a census takes them) in the insectary on `day`, with
 * the finished censuses of that day. Without `species`, those censused that day.
 *
 * In the list: every butterfly of a census of that day (its list, as it
 * finished), and every butterfly with an Insectary ID that entered on or before
 * the day: alive on it (alive now, or dead on it or after), or dead before it
 * when it entered since `since` (an ISO day: the notebook's pages in use;
 * without it, all of them). One with no entry date is listed when alive that day.
 *
 * Each line: `census` (seen / disappeared / excluded in a census of the day, or
 * null), `death` (its death in the sheet with the entries kept in the app;
 * `staged` when only in the app) and `todo`: 'write' (✗ disappeared in the
 * census) or 'highlight' (▬ any death in the database other than the census's
 * own disappearance), or null. A disappearance undone since (alive again) is
 * `undone` and nothing to do.
 */
export async function notebookUpdate(store, query = {}) {
  const day = String(query.day ?? '');
  if (!validDay(day)) throw fail('INVALID_DAY', 'Fecha no válida');
  const serial = serialOf(day);
  const since = blank(query.since) ? null : String(query.since);
  if (since !== null && !validDay(since)) throw fail('INVALID_DAY', 'Fecha no válida');
  const sinceSerial = since === null ? null : serialOf(since);
  const censuses = store.db
    .prepare("SELECT * FROM censuses WHERE day = ? AND status = 'finished' ORDER BY created_at")
    .all(day);
  const asked = (Array.isArray(query.species) ? query.species : text(query.species).split(','))
    .map(s => binomial(s.replace(/\s+/g, ' ')))
    .filter(Boolean);
  const wanted = [
    ...new Map((asked.length ? asked : censuses.map(c => binomial(c.species))).map(s => [speciesKey(s), s])).values(),
  ];
  // Each species name of the sheet compared once (a few hundred names, thousands of rows).
  const matches = new Map();
  const ofWanted = value => {
    const key = typeof value === 'string' ? value : String(value ?? '');
    let hit = matches.get(key);
    if (hit === undefined) matches.set(key, (hit = wanted.some(s => inTaxon(s, key))));
    return hit;
  };
  const used = censuses.filter(c => wanted.some(s => inTaxon(s, c.species)));

  // What each census of the day found, by row (by Insectary ID for one marked while kept in the app).
  const found = new Map();
  const foundById = new Map();
  for (const c of used) {
    const waiting = censusWaiting(store, c);
    for (const b of parse(c.result_json)?.roster ?? []) {
      const at = { status: b.status, censusId: c.id, species: c.species, note: b.note ?? '', waiting, roster: b };
      found.set(b.recordId, at);
      if (String(b.recordId).startsWith('staged:')) foundById.set(upper(b.id), at);
    }
  }
  const censusOf = (recordId, id) => found.get(recordId) ?? foundById.get(upper(id)) ?? null;
  const facts = wanted.length ? (await freshFacts(store)).facts : [];

  const lines = [];
  const take = (recordId, row, values, species, onlyInApp, deathStaged) => {
    const id = text(values.Insectary_ID);
    if (!id) return;
    const at = censusOf(recordId, id);
    const entered = typeof values.Intro2Insectary_date === 'number' ? values.Intro2Insectary_date : null;
    const alive = isAlive(values);
    if (!at) {
      if (!ofWanted(species)) return;
      // In the insectary that day: alive now, or dead on it or after.
      const then = alive || (typeof values.Death_date === 'number' && values.Death_date >= serial);
      if (entered === null ? !then : entered > serial) return;
      // Dead before the day, of a page no longer in use.
      if (sinceSerial !== null && !then && !(entered !== null && entered >= sinceSerial)) return;
    }
    const death = alive ? null : deathOf(values, onlyInApp || deathStaged);
    const census = at ? { status: at.status, censusId: at.censusId, note: at.note, waiting: at.waiting } : null;
    let todo = null;
    let undone = false;
    if (census?.status === 'disappeared') {
      const own = death && death.cause === DISAPPEARED && death.date === serial;
      if (own || (!death && census.waiting)) todo = 'write';
      else if (!death) undone = true;
    }
    if (death && todo !== 'write') todo = 'highlight';
    lines.push({
      recordId,
      id,
      row,
      species: text(species),
      sex: text(values.Sex),
      entered,
      wild: /wild/i.test(text(values.Wild_Reared)),
      census,
      death,
      todo,
      ...(undone ? { undone: true } : {}),
      ...(onlyInApp ? { staged: true } : {}),
    });
  };

  // The sheet's butterflies of these species (and every row a census of the day listed), with the entries kept in the app on them.
  const staged = store.staged.ofSheet(SHEET);
  for (const f of facts) {
    const edits = staged.edits.get(f.recordId);
    if (!edits && !found.has(f.recordId) && !ofWanted(f.values.SPECIES)) continue;
    const values = { ...f.values };
    let deathStaged = false;
    for (const change of edits ?? []) {
      values[change.field] = change.after;
      if (DEATH.includes(change.field)) deathStaged = true;
    }
    take(f.recordId, f.row, values, values.SPECIES, false, deathStaged);
  }
  for (const b of stagedButterflies(store, staged)) take(b.recordId, b.row, b.values, b.species, true, false);

  return {
    day,
    since,
    species: wanted,
    censuses: used.map(c => ({ id: c.id, species: c.species, waiting: censusWaiting(store, c) })),
    // Every species censused that day (to add one to the list).
    censused: [...new Map(censuses.map(c => [speciesKey(binomial(c.species)), binomial(c.species)])).values()],
    lines: lines.sort((a, b) => (a.row ?? Infinity) - (b.row ?? Infinity) || a.id.localeCompare(b.id)),
  };
}

/** The cells of a butterfly the notebook's list reads. */
const FACT_FIELDS = [
  'Insectary_ID',
  'SPECIES',
  'Sex',
  'Intro2Insectary_date',
  'Wild_Reared',
  'Death_date',
  'Death_cause',
];
const factsCache = new WeakMap();
export const factsStamp = store => recordsStamp(store);
/**
 * Every butterfly of Insectary_data (a typed row with an Insectary ID), in sheet order, with the
 * cells the notebook's list reads: { stamp, facts: [{ recordId, row, values }] }. Here, in this
 * thread; the app's requests ask freshFacts (the Revisión worker where there is one).
 */
export function factsEntry(store) {
  const stamp = factsStamp(store);
  const hit = factsCache.get(store);
  if (hit?.stamp === stamp) return hit;
  const started = Date.now();
  const rows = store.db
    .prepare(
      `SELECT id, row_num, ${FACT_FIELDS.map((f, i) => `json_extract(values_json, '$.${f}') f${i}`).join(', ')}
       FROM records WHERE sheet = ? AND missing = 0 AND observed = 1 ORDER BY row_num`,
    )
    .all(SHEET);
  const facts = [];
  for (const r of rows) {
    if (blank(r.f0)) continue;
    const values = {};
    FACT_FIELDS.forEach((f, i) => {
      if (r[`f${i}`] !== null) values[f] = r[`f${i}`];
    });
    facts.push({ recordId: r.id, row: r.row_num, values });
  }
  return keepFacts(store, { stamp, facts, ms: Date.now() - started });
}
/** Facts read in the worker: kept while the copy is as they were read. */
export function keepFacts(store, entry) {
  if (entry.stamp === factsStamp(store)) factsCache.set(store, entry);
  return entry;
}
export function cachedFacts(store) {
  const hit = factsCache.get(store);
  return hit?.stamp === factsStamp(store) ? hit : null;
}
const runners = new WeakMap();
/** Who reads the facts for the app's requests (server/checks-host.mjs: a worker thread), by store. */
export function useFactsRunner(store, runner) {
  if (runner) runners.set(store, runner);
  else runners.delete(store);
}
/** The facts as of now: the kept ones when nothing changed, else read after this call. A promise. */
export async function freshFacts(store) {
  const runner = runners.get(store);
  return runner ? runner.fresh() : factsEntry(store);
}

/** The settings key of when a person last opened «Filas para resaltar». */
const openedKey = user => `deaths-highlights-opened:${user.id || user.username}`;
/** When the history starts: the first save the app recorded (deaths entered before cannot be dated). */
const historyStart = store => store.db.prepare('SELECT min(created_at) t FROM actions').get()?.t ?? null;

/**
 * «Filas para resaltar»: the butterflies whose death was entered since `since`
 * (an ISO moment) and are still dead, from the history: for each row, the last
 * time it went from alive (no Death_date, no Death_cause) to dead, through any
 * save (the app's, an undo's, or an edit seen in Google Sheets). A death undone
 * since is not listed; one undone and entered again counts from the second time.
 * Plus the deaths kept in the app (staged), entered when they were kept.
 */
export function enteredDeaths(store, query = {}) {
  const since = blank(query.since) ? null : String(query.since);
  if (since === null || Number.isNaN(Date.parse(since))) throw fail('INVALID_DAY', 'Fecha no válida');
  const from = new Date(since).toISOString();
  const names = new Map(
    store.db
      .prepare('SELECT id, display_name FROM users')
      .all()
      .map(u => [u.id, u.display_name]),
  );
  const changes = store.db
    .prepare(
      `SELECT c.record_id, c.field, c.before_json, c.after_json, a.created_at, a.actor, a.source FROM changes c JOIN actions a ON a.id = c.action_id
       WHERE c.sheet = ? AND c.field IN ('Death_date','Death_cause') AND a.status IN ('verified','observed')
         AND c.record_id IN (SELECT c2.record_id FROM changes c2 JOIN actions a2 ON a2.id = c2.action_id
           WHERE c2.sheet = ? AND c2.field IN ('Death_date','Death_cause') AND a2.created_at >= ? AND a2.status IN ('verified','observed'))
       ORDER BY c.rowid`,
    )
    .all(SHEET, SHEET, from);
  const byRecord = new Map();
  for (const c of changes) {
    const list = byRecord.get(c.record_id) ?? [];
    list.push(c);
    byRecord.set(c.record_id, list);
  }
  const staged = store.staged.ofSheet(SHEET);
  const items = [];
  const record = store.db.prepare('SELECT id, row_num, values_json, missing FROM records WHERE id = ?');
  const facts = (recordId, row, values) => ({
    recordId,
    id: text(values.Insectary_ID),
    row,
    species: text(values.SPECIES),
    sex: text(values.Sex),
    entered: typeof values.Intro2Insectary_date === 'number' ? values.Intro2Insectary_date : null,
  });
  for (const [recordId, list] of byRecord) {
    const r = record.get(recordId);
    if (!r || r.missing) continue;
    const values = parse(r.values_json) ?? {};
    // Each field before its first change in the history (or as it is, never changed), then every change in order.
    const state = Object.fromEntries(DEATH.map(f => [f, values[f] ?? null]));
    for (const f of DEATH) {
      const first = list.find(c => c.field === f);
      if (first) state[f] = parse(first.before_json);
    }
    let entered = null;
    for (const c of list) {
      const wasDead = !isAlive(state);
      state[c.field] = parse(c.after_json);
      if (!wasDead && !isAlive(state)) entered = c;
    }
    let current = values;
    let deathStaged = false;
    for (const change of staged.edits.get(recordId) ?? []) {
      current = { ...current, [change.field]: change.after };
      if (DEATH.includes(change.field)) deathStaged = true;
    }
    if (!entered || entered.created_at < from || isAlive(current) || blank(current.Insectary_ID)) continue;
    const by = [
      ...new Set(list.filter(c => c.created_at >= entered.created_at).map(c => names.get(c.actor) ?? c.actor)),
    ];
    items.push({
      ...facts(recordId, r.row_num, current),
      death: deathOf(current, deathStaged),
      enteredAt: entered.created_at,
      source: entered.source === 'sheet_reconciliation' ? 'sheets' : 'app',
      by,
    });
  }
  // Kept in the app: a row alive in the sheet that an entry makes dead, or a new row entered dead.
  const listed = new Set(items.map(i => i.recordId));
  for (const [recordId, list] of staged.edits) {
    if (listed.has(recordId) || !list.some(c => DEATH.includes(c.field))) continue;
    const r = record.get(recordId);
    if (!r || r.missing) continue;
    const values = parse(r.values_json) ?? {};
    if (!isAlive(values)) continue;
    const current = { ...values };
    for (const c of list) current[c.field] = c.after;
    const at = list
      .filter(c => DEATH.includes(c.field))
      .map(c => c.item.createdAt)
      .sort()[0];
    if (isAlive(current) || blank(current.Insectary_ID) || at < from) continue;
    items.push({
      ...facts(recordId, r.row_num, current),
      death: deathOf(current, true),
      enteredAt: at,
      source: 'app',
      by: [...new Set(list.map(c => c.actor))],
    });
  }
  for (const b of stagedButterflies(store, staged)) {
    if (isAlive(b.values) || b.item.createdAt < from) continue;
    items.push({
      ...facts(b.recordId, b.row, { ...b.values, SPECIES: b.species }),
      death: deathOf(b.values, true),
      enteredAt: b.item.createdAt,
      source: 'app',
      by: [b.item.actorName],
    });
  }
  return {
    since: from,
    historyStart: historyStart(store),
    items: items.sort((a, b) => (a.row ?? Infinity) - (b.row ?? Infinity) || a.id.localeCompare(b.id)),
  };
}

/** This person opened «Filas para resaltar» now: the next time it starts here. Returns the time before. */
export function openedHighlights(store, user) {
  const before = store.getSetting(openedKey(user));
  store.setSetting(openedKey(user), new Date().toISOString());
  return { previous: before };
}
