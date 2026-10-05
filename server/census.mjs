// Censuses of the insectary (Censo tab). Paper method: all butterflies of one
// species go into a small cage and are released one by one into the big cage;
// for each, someone reads the wing ID, finds it in the notebook and draws a
// smiley. Afterwards the IDs without a smiley are marked disappeared
// (Death_cause Disappearance, Death_date the census day).
//
// Here a census lives in the app (censuses, census_marks): several people mark
// the same census from their phones at once, each mark with who and when, seen
// by everyone through /api/pulse. Finishing stages the disappearances as an
// entry kept in the app (server/staged.mjs, purpose «censo»): they wait for
// «Guardar en Google Sheets» with the Emergidos and Clutches entries and go
// through the outbox when Google is busy. The cells written for each
// disappearance come from the client (lib/deaths.ts deathCells, what Muertes
// writes for a death not preserved); the server checks they are deaths of
// exactly the butterflies not seen, on the census day. Undoing that entry
// («Deshacer» in the bar, or «Reabrir») opens the census again.
//
// "Alive in the insectary": an Insectary_data row with an Insectary ID, typed
// (not a pre-made row), with no Death_date and no Death_cause, everyone's
// entries kept in the app counted (a butterfly emerged and not yet in the
// sheet is alive; a death kept in the app is a death). The sheet has no
// column for released or shipped butterflies: whatever leaves the insectary
// gets a Death_cause, so "no date and no cause" is the whole definition.

import { randomUUID } from 'node:crypto';
import { msg, msgError } from './messages.mjs';
import { ecuadorDay } from './clutches.mjs';

const SHEET = 'Insectary_data';
/** The Death_cause list's value for a butterfly not found (3,000 rows use it). */
export const DISAPPEARED = 'Disappearance';
/** The cells a disappearance may write (lib/deaths.ts deathCells: the date, the cause and the not-preserved block). */
const DEATH_FIELDS = new Set([
  'Death_date',
  'Death_cause',
  'Preserved_Dead_Alive',
  'CAM_ID',
  'Tube_1_id',
  'Tube_1_tissue',
  'T1_Preservation_medium',
  'Tube_2_id',
  'Tube_2_tissue',
  'T2_Preservation_medium',
  'Tube_3_id',
  'Tube_3_tissue',
  'Tube_4_id',
  'Tube_4_tissue',
  'Preservation_medium',
  'Preservation_date',
  'Location_body',
]);
/** What a mark says: seen alive, left out of the disappearances (in another cage…), or an ID read that is in no row. */
export const MARK_KINDS = new Set(['seen', 'excluded', 'unknown']);
/** What looked different from the record on a butterfly seen alive (kept for review, never corrected here). */
export const DOUBTS = new Set(['sex', 'species', 'other']);

const parse = text => (text ? JSON.parse(text) : null);
const json = value => JSON.stringify(value);
const now = () => new Date().toISOString();
const fail = (code, message, status = 400, details) => msgError(message, { code, status, details });
const blank = v => v === null || v === undefined || (typeof v === 'string' && v.trim() === '');
const text = v => (blank(v) ? '' : String(v).trim());
const upper = v => text(v).toUpperCase();
/** Species compared as people write them: case and spacing aside. */
export const speciesKey = v => text(v).replace(/\s+/g, ' ').toLowerCase();
/** The sheet's serial day of an ISO date (2026-10-05 → 46300). */
export const serialOf = day => Math.round((Date.parse(`${day}T00:00:00Z`) - Date.UTC(1899, 11, 30)) / 86_400_000);
/** Alive: no death date and no cause ("NA" in Death_date says neither, so not alive). */
export const isAlive = values => blank(values?.Death_date) && blank(values?.Death_cause);

export function initCensus(db) {
  db.exec(`CREATE TABLE IF NOT EXISTS censuses(id TEXT PRIMARY KEY, request_id TEXT UNIQUE, species TEXT NOT NULL, day TEXT NOT NULL,
      status TEXT NOT NULL DEFAULT 'open', expected INTEGER NOT NULL DEFAULT 0, created_by TEXT NOT NULL, created_at TEXT NOT NULL,
      finished_by TEXT, finished_at TEXT, finish_request TEXT, staged_entry TEXT, action_id TEXT, result_json TEXT,
      notebook_by TEXT, notebook_at TEXT);
    CREATE INDEX IF NOT EXISTS censuses_status ON censuses(status, day);
    CREATE TABLE IF NOT EXISTS census_marks(id TEXT PRIMARY KEY, request_id TEXT UNIQUE, census_id TEXT NOT NULL, record_id TEXT,
      insectary_id TEXT NOT NULL, kind TEXT NOT NULL, species TEXT, doubt TEXT, note TEXT, actor TEXT NOT NULL,
      created_at TEXT NOT NULL, updated_at TEXT NOT NULL, UNIQUE(census_id, record_id));
    CREATE INDEX IF NOT EXISTS census_marks_census ON census_marks(census_id, created_at);`);
}

/** Open pages are told (GET /api/pulse carries `census`, which they compare). */
function bump(store) {
  store.censusStamp = (store.censusStamp ?? 0) + 1;
  store.bumpLive?.('census');
}
const people = store => new Map(store.db.prepare('SELECT id, display_name FROM users').all().map(u => [u.id, u.display_name]));

/** One butterfly as a census lists it. */
function entryOf(recordId, row, values, species, staged = false) {
  const entered = values.Intro2Insectary_date;
  return {
    recordId,
    id: text(values.Insectary_ID),
    row,
    species: text(species),
    sex: text(values.Sex),
    clutch: text(values['CLUTCH NUMBER']),
    entered: typeof entered === 'number' ? entered : null,
    wild: /wild/i.test(text(values.Wild_Reared)),
    ...(staged ? { staged: true } : {}),
  };
}

/**
 * Every butterfly alive in the insectary (or of one species), in the sheet's
 * order (= the notebook's: Insectary IDs are pre-made in order), with the
 * entries kept in the app on top.
 */
export function aliveButterflies(store, { species = null } = {}) {
  const want = species === null ? null : speciesKey(species);
  const staged = store.staged.ofSheet(SHEET);
  const out = [];
  const rows = store.db
    .prepare(
      `SELECT id, row_num, values_json FROM records WHERE sheet=? AND missing=0 AND observed=1 AND (
         (coalesce(json_extract(values_json,'$.Death_date'),'')='' AND coalesce(json_extract(values_json,'$.Death_cause'),'')='')
         OR id IN (SELECT record_id FROM staged WHERE kind='edit' AND sheet=? AND status IN ('staged','sent')))`,
    )
    .all(SHEET, SHEET);
  for (const r of rows) {
    const values = parse(r.values_json) ?? {};
    for (const change of staged.edits.get(r.id) ?? []) values[change.field] = change.after;
    if (blank(values.Insectary_ID) || !isAlive(values)) continue;
    if (want !== null && speciesKey(values.SPECIES) !== want) continue;
    out.push(entryOf(r.id, r.row_num, values, values.SPECIES));
  }
  // Emerged and not yet in the sheet: its species is its clutch's (the sheet's formula gives it once written).
  if (staged.creates.length) {
    const clutchSpecies = new Map();
    for (const r of store.db
      .prepare(
        "SELECT json_extract(values_json,'$.\"CLUTCH NUMBER\"') n, json_extract(values_json,'$.SPECIES') s FROM records WHERE sheet='Insectary_stocks' AND missing=0",
      )
      .all())
      if (!blank(r.n)) clutchSpecies.set(text(r.n), r.s);
    const premade = store.db.prepare(
      "SELECT row_num FROM records WHERE sheet=? AND missing=0 AND observed=0 AND upper(trim(json_extract(values_json,'$.Insectary_ID')))=?",
    );
    for (const item of staged.creates) {
      const values = item.values ?? {};
      if (blank(values.Insectary_ID) || !isAlive(values)) continue;
      const kind = text(values.SPECIES) || text(clutchSpecies.get(text(values['CLUTCH NUMBER'])));
      if (want !== null && speciesKey(kind) !== want) continue;
      const row = premade.get(SHEET, upper(values.Insectary_ID))?.row_num ?? null;
      out.push(entryOf(item.rowId, row, values, kind, true));
    }
  }
  return out.sort((a, b) => (a.row ?? Infinity) - (b.row ?? Infinity));
}

/** Species alive in the insectary now, most butterflies first: what a census can be started for. */
export function aliveSpecies(store) {
  const counts = new Map();
  for (const b of aliveButterflies(store)) {
    if (!b.species || /^(NA|null)$/i.test(b.species)) continue;
    const key = speciesKey(b.species);
    const seen = counts.get(key) ?? { species: b.species, alive: 0 };
    seen.alive++;
    counts.set(key, seen);
  }
  return [...counts.values()].sort((a, b) => b.alive - a.alive || a.species.localeCompare(b.species));
}

function shapeMark(m, names) {
  return {
    id: m.id,
    recordId: m.record_id,
    insectaryId: m.insectary_id,
    kind: m.kind,
    species: m.species,
    doubt: m.doubt,
    note: m.note,
    actor: m.actor,
    actorName: names.get(m.actor) ?? m.actor,
    createdAt: m.created_at,
    updatedAt: m.updated_at,
  };
}
const marksOf = (store, censusId) => store.db.prepare('SELECT * FROM census_marks WHERE census_id = ? ORDER BY created_at, rowid').all(censusId);

/**
 * Which mark a butterfly of the list has: by its row; a butterfly marked while
 * it was an entry kept in the app (staged:…) and written since is found by its ID.
 */
function markFinder(marks) {
  const byRecord = new Map();
  const byId = new Map();
  for (const m of marks) {
    if (!m.record_id) continue;
    byRecord.set(m.record_id, m);
    if (m.record_id.startsWith('staged:')) byId.set(upper(m.insectary_id), m);
  }
  return b => byRecord.get(b.recordId) ?? byId.get(upper(b.id)) ?? null;
}

/** Where the census's disappearances are: none, kept in the app, being written, or in Google Sheets. */
function deathsState(store, c) {
  if (c.status !== 'finished') return null;
  if (c.action_id) return 'written';
  if (!c.staged_entry) return 'none';
  const rows = store.db.prepare("SELECT status FROM staged WHERE entry_id = ? AND status IN ('staged','sent')").all(c.staged_entry);
  if (!rows.length) return 'written';
  return rows.some(r => r.status === 'sent') ? 'sending' : 'staged';
}

function summaryOf(store, c, names, roster = null) {
  const marks = marksOf(store, c.id);
  const result = parse(c.result_json);
  const count = kind => marks.filter(m => m.kind === kind).length;
  const seen = marks.filter(m => m.kind === 'seen');
  const own = speciesKey(c.species);
  const markOf = markFinder(marks);
  const listed = result ? result.roster : roster;
  // Seen of the list (a butterfly recorded dead and seen alive is a finding, not one of the list).
  const seenListed = listed
    ? result
      ? listed.filter(b => b.status === 'seen').length
      : listed.filter(b => markOf(b)?.kind === 'seen').length
    : seen.filter(m => speciesKey(m.species) === own).length;
  const listedIds = new Set((listed ?? []).map(b => b.recordId));
  return {
    id: c.id,
    species: c.species,
    day: c.day,
    status: c.status,
    expected: c.expected,
    createdBy: c.created_by,
    createdByName: names.get(c.created_by) ?? c.created_by,
    createdAt: c.created_at,
    finishedAt: c.finished_at,
    finishedByName: c.finished_by ? (names.get(c.finished_by) ?? c.finished_by) : null,
    people: [...new Set(marks.map(m => names.get(m.actor) ?? m.actor))],
    counts: {
      // The butterflies of the list now (open), or when it finished.
      roster: listed ? listed.length : c.expected,
      seen: seenListed,
      excluded: count('excluded'),
      disappeared: result ? result.roster.filter(b => b.status === 'disappeared').length : 0,
      otherSpecies: seen.filter(m => speciesKey(m.species) !== own).length,
      // Seen, of the species, but not on the list: recorded dead (or a repeated ID); to look at.
      offList: listed ? seen.filter(m => speciesKey(m.species) === own && !listedIds.has(m.record_id) && !listed.some(b => markOf(b) === m)).length : 0,
      unknown: count('unknown'),
      doubts: seen.filter(m => m.doubt).length,
    },
    deaths: deathsState(store, c),
    notebookAt: c.notebook_at,
    notebookByName: c.notebook_by ? (names.get(c.notebook_by) ?? c.notebook_by) : null,
  };
}

/** Pairs of characters people corrected in Insectary IDs (the history: one character changed), most frequent first. */
export function learnedLookAlikes(store) {
  const counts = new Map();
  const rows = store.db
    .prepare("SELECT before_json b, after_json a FROM changes WHERE sheet = ? AND field = 'Insectary_ID' AND before_json IS NOT NULL AND after_json IS NOT NULL")
    .all(SHEET);
  for (const r of rows) {
    const a = upper(parse(r.b));
    const b = upper(parse(r.a));
    if (!a || !b || a.length !== b.length || a === b) continue;
    const at = [...a].map((ch, i) => (ch === b[i] ? -1 : i)).filter(i => i >= 0);
    if (at.length !== 1) continue;
    const pair = [a[at[0]], b[at[0]]].sort().join('');
    counts.set(pair, (counts.get(pair) ?? 0) + 1);
  }
  return [...counts]
    .sort((x, y) => y[1] - x[1])
    .map(([pair, n]) => ({ a: pair[0], b: pair[1], n }));
}

/** The Censo tab's start: species alive (with how many), censuses open now, the latest ones, learned look-alikes. */
export function censusOverview(store, { limit = 40 } = {}) {
  const names = people(store);
  const open = store.db.prepare("SELECT * FROM censuses WHERE status = 'open' ORDER BY created_at DESC").all();
  const past = store.db
    .prepare("SELECT * FROM censuses WHERE status != 'open' ORDER BY day DESC, created_at DESC LIMIT ?")
    .all(Math.min(Math.max(Number(limit) || 40, 1), 200));
  return {
    species: aliveSpecies(store),
    open: open.map(c => summaryOf(store, c, names, aliveButterflies(store, { species: c.species }))),
    history: past.map(c => summaryOf(store, c, names)),
    lookAlikes: learnedLookAlikes(store),
    stamp: store.censusStamp ?? 0,
  };
}

function getCensus(store, id) {
  const c = store.db.prepare('SELECT * FROM censuses WHERE id = ?').get(String(id ?? ''));
  if (!c) throw fail('CENSUS_NOT_FOUND', 'Ese censo no existe', 404);
  return c;
}
const requireOpen = c => {
  if (c.status !== 'open') throw fail('CENSUS_CLOSED', 'Ese censo ya terminó', 409);
};

/**
 * A census with its list and everyone's marks: while open, the butterflies of
 * its species alive now; once finished, the list as it was then, each with
 * what happened to it (seen, disappeared, excluded).
 */
export function censusDetail(store, id) {
  const c = getCensus(store, id);
  const names = people(store);
  const marks = marksOf(store, c.id);
  const result = parse(c.result_json);
  const roster = result ? result.roster : c.status === 'open' ? aliveButterflies(store, { species: c.species }) : [];
  return {
    census: summaryOf(store, c, names, roster),
    roster,
    marks: marks.map(m => shapeMark(m, names)),
    stamp: store.censusStamp ?? 0,
  };
}

/** Starts a census of a species on a day (today in Ecuador by default); one already open for both is joined. */
export function startCensus(store, body, user) {
  store.validateRole(user);
  store.requireRequestId(body.requestId);
  const prior = store.db.prepare('SELECT id FROM censuses WHERE request_id = ?').get(body.requestId);
  if (prior) return { ...censusDetail(store, prior.id), joined: false, duplicate: true };
  const species = text(body.species).replace(/\s+/g, ' ');
  if (!species) throw fail('INVALID_VALUES', 'Elige la especie del censo');
  const day = body.day ? String(body.day) : ecuadorDay();
  if (!/^\d{4}-\d{2}-\d{2}$/.test(day) || Number.isNaN(Date.parse(day))) throw fail('INVALID_DAY', 'Fecha no válida');
  const same = store.db
    .prepare("SELECT * FROM censuses WHERE status = 'open' AND day = ?")
    .all(day)
    .find(c => speciesKey(c.species) === speciesKey(species));
  if (same) return { ...censusDetail(store, same.id), joined: true };
  const id = randomUUID();
  const actor = user.id || user.username;
  const expected = aliveButterflies(store, { species }).length;
  store.db
    .prepare('INSERT INTO censuses(id, request_id, species, day, status, expected, created_by, created_at) VALUES(?, ?, ?, ?, ?, ?, ?, ?)')
    .run(id, body.requestId, species, day, 'open', expected, actor, now());
  bump(store);
  return { ...censusDetail(store, id), joined: false };
}

/** The butterfly a mark is about: a sheet row or an entry kept in the app (staged:<clientId>). */
function butterflyOf(store, recordId) {
  if (recordId.startsWith('staged:')) {
    const item = store.staged.ofSheet(SHEET).creates.find(i => i.rowId === recordId);
    return item ? { id: text(item.values?.Insectary_ID), species: text(item.values?.SPECIES) } : null;
  }
  const record = store.getRecord(recordId);
  if (!record || record.sheet !== SHEET || record.missing || blank(record.values?.Insectary_ID)) return null;
  return { id: text(record.values.Insectary_ID), species: text(record.values.SPECIES) };
}
const cleanNote = v => text(v).slice(0, 300) || null;
function cleanDoubt(v) {
  if (blank(v)) return null;
  if (!DOUBTS.has(String(v))) throw fail('INVALID_VALUES', 'Duda no válida');
  return String(v);
}

/**
 * Marks a butterfly in an open census: seen alive (the smiley), excluded from
 * the disappearances (with why), or an ID read that is in no row (`text`). Two
 * people marking the same butterfly: the second is told who marked it first
 * (`already`); a request sent twice gives the same mark.
 */
export function addMark(store, censusId, body, user) {
  store.validateRole(user);
  store.requireRequestId(body.requestId);
  const names = people(store);
  const prior = store.db.prepare('SELECT * FROM census_marks WHERE request_id = ?').get(body.requestId);
  if (prior) return { mark: shapeMark(prior, names), duplicate: true };
  const c = getCensus(store, censusId);
  requireOpen(c);
  const kind = body.kind ? String(body.kind) : 'seen';
  if (!MARK_KINDS.has(kind)) throw fail('INVALID_VALUES', 'Marca no válida');
  const actor = user.id || user.username;
  const at = now();
  const insert = store.db.prepare(
    `INSERT INTO census_marks(id, request_id, census_id, record_id, insectary_id, kind, species, doubt, note, actor, created_at, updated_at)
     VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT(census_id, record_id) DO NOTHING`,
  );
  if (kind === 'unknown') {
    const typed = upper(body.text).replace(/\s+/g, '').slice(0, 40);
    if (!typed) throw fail('INVALID_VALUES', 'Escribe el ID que se leyó');
    const id = randomUUID();
    insert.run(id, body.requestId, c.id, null, typed, kind, null, null, cleanNote(body.note), actor, at, at);
    bump(store);
    return { mark: shapeMark(store.db.prepare('SELECT * FROM census_marks WHERE id = ?').get(id), names) };
  }
  const recordId = text(body.recordId);
  const butterfly = recordId ? butterflyOf(store, recordId) : null;
  if (!butterfly) throw fail('RECORD_NOT_FOUND', 'Esa mariposa no está en Insectary_data', 404);
  const doubt = cleanDoubt(body.doubt);
  const existing = store.db.prepare('SELECT * FROM census_marks WHERE census_id = ? AND record_id = ?').get(c.id, recordId);
  if (existing) {
    if (existing.kind === kind) return { mark: shapeMark(existing, names), already: true };
    if (kind === 'excluded')
      throw fail('CENSUS_SEEN', msg('{id} ya está marcada como vista', { id: butterfly.id }), 409);
    // Left out before, then seen after all: seen.
    store.db
      .prepare('UPDATE census_marks SET kind = ?, request_id = ?, doubt = ?, note = ?, actor = ?, updated_at = ? WHERE id = ?')
      .run(kind, body.requestId, doubt, cleanNote(body.note), actor, at, existing.id);
    bump(store);
    return { mark: shapeMark(store.db.prepare('SELECT * FROM census_marks WHERE id = ?').get(existing.id), names) };
  }
  const id = randomUUID();
  const result = insert.run(id, body.requestId, c.id, recordId, butterfly.id, kind, butterfly.species, doubt, cleanNote(body.note), actor, at, at);
  if (!result.changes) {
    const first = store.db.prepare('SELECT * FROM census_marks WHERE census_id = ? AND record_id = ?').get(c.id, recordId);
    return { mark: shapeMark(first, names), already: true };
  }
  bump(store);
  return { mark: shapeMark(store.db.prepare('SELECT * FROM census_marks WHERE id = ?').get(id), names) };
}

function getMark(store, censusId, markId) {
  const m = store.db.prepare('SELECT * FROM census_marks WHERE id = ? AND census_id = ?').get(String(markId ?? ''), String(censusId ?? ''));
  if (!m) throw fail('MARK_NOT_FOUND', 'Esa marca ya no está', 404);
  return m;
}

/** A doubt about a butterfly seen (its sex or species looks different) and a short note; kept for review. */
export function updateMark(store, censusId, markId, body, user) {
  store.validateRole(user);
  requireOpen(getCensus(store, censusId));
  const m = getMark(store, censusId, markId);
  const doubt = Object.hasOwn(body, 'doubt') ? cleanDoubt(body.doubt) : m.doubt;
  const note = Object.hasOwn(body, 'note') ? cleanNote(body.note) : m.note;
  store.db.prepare('UPDATE census_marks SET doubt = ?, note = ?, updated_at = ? WHERE id = ?').run(doubt, note, now(), m.id);
  bump(store);
  return { mark: shapeMark(store.db.prepare('SELECT * FROM census_marks WHERE id = ?').get(m.id), people(store)) };
}

/** Undoes a mark (a wrong tap) while the census is open. */
export function removeMark(store, censusId, markId, user) {
  store.validateRole(user);
  requireOpen(getCensus(store, censusId));
  const m = getMark(store, censusId, markId);
  store.db.prepare('DELETE FROM census_marks WHERE id = ?').run(m.id);
  bump(store);
  return { removed: m.id };
}

/**
 * Finishes a census: the butterflies of its species alive now and not marked
 * die as disappeared on the census day. `edits` are their cells as Muertes
 * writes them (lib/deaths.ts deathCells): exactly one per butterfly not seen,
 * with Death_date the census day and Death_cause Disappearance. They are kept in
 * the app (server/staged.mjs) until «Guardar en Google Sheets», all or none.
 * When the list changed meanwhile (someone marked one, a butterfly emerged or
 * died) nothing is kept and the person reviews it again.
 */
export async function finishCensus(store, censusId, body, user) {
  store.validateRole(user);
  store.requireRequestId(body.requestId);
  const edits = Array.isArray(body.edits) ? body.edits : [];
  if (edits.length > 500) throw fail('BATCH_TOO_LARGE', msg('Guarda como máximo {n} filas a la vez', { n: 500 }));
  return store.staged.serial(async () => {
    const c = getCensus(store, censusId);
    if (c.status === 'finished' && c.finish_request === body.requestId) return { ...censusDetail(store, c.id), duplicate: true };
    requireOpen(c);
    const roster = aliveButterflies(store, { species: c.species });
    const marks = marksOf(store, c.id);
    const markOf = markFinder(marks);
    const missing = roster.filter(b => !markOf(b));
    const want = new Set(missing.map(b => b.recordId));
    const got = new Set(edits.map(e => String(e?.id ?? '')));
    const extra = [...got].filter(id => !want.has(id));
    const lacking = [...want].filter(id => !got.has(id));
    if (extra.length || lacking.length || got.size !== edits.length)
      throw fail('CENSUS_CHANGED', 'El censo cambió mientras lo revisabas: revísalo otra vez', 409, { extra, lacking });
    const serial = serialOf(c.day);
    for (const e of edits) {
      const values = e?.values ?? {};
      const label = roster.find(b => b.recordId === e.id)?.id ?? e.id;
      if (values.Death_date !== serial || values.Death_cause !== DISAPPEARED || Object.keys(values).some(f => !DEATH_FIELDS.has(f)))
        throw fail('INVALID_VALUES', msg('{id}: solo la fecha del censo, Disappearance y las celdas de una muerte sin preservar', { id: label }));
    }
    let entryId = null;
    if (edits.length) {
      const out = await store.staged.stageNow({ requestId: body.requestId, purpose: 'censo', partial: false }, user, edits, []);
      entryId = out.entryId;
    }
    const result = {
      roster: roster.map(b => {
        const m = markOf(b);
        return { ...b, status: m ? m.kind : 'disappeared', ...(m?.note ? { note: m.note } : {}), ...(m?.doubt ? { doubt: m.doubt } : {}) };
      }),
    };
    store.db
      .prepare(
        "UPDATE censuses SET status = 'finished', finished_by = ?, finished_at = ?, finish_request = ?, staged_entry = ?, result_json = ? WHERE id = ?",
      )
      .run(user.id || user.username, now(), body.requestId, entryId, json(result), c.id);
    bump(store);
    return { ...censusDetail(store, c.id), staged: store.staged.summary().staged };
  });
}

/**
 * Opens a finished census again (to mark one more, or leave one out): its
 * disappearances kept in the app are undone (staged.remove, which reopens it).
 * Not once they are being written or in Google Sheets.
 */
export async function reopenCensus(store, censusId, user) {
  store.validateRole(user);
  const c = getCensus(store, censusId);
  if (c.status !== 'finished') throw fail('CENSUS_NOT_FINISHED', 'Ese censo no está terminado', 409);
  const state = deathsState(store, c);
  if (state === 'sending') throw fail('STAGED_SENT', 'Se está escribiendo en Google Sheets; deshazlo desde Historial cuando esté guardado', 409);
  if (state === 'written')
    throw fail('CENSUS_WRITTEN', 'Las desapariciones ya están en Google Sheets: corrígelas en Muertes o deshazlas en Historial', 409);
  if (state === 'staged') await store.staged.remove({ entryId: c.staged_entry }, user);
  else reopenFinished(store.db, c.id);
  bump(store);
  return censusDetail(store, c.id);
}
function reopenFinished(db, id) {
  db.prepare(
    "UPDATE censuses SET status = 'open', finished_by = NULL, finished_at = NULL, finish_request = NULL, staged_entry = NULL, result_json = NULL WHERE id = ?",
  ).run(id);
}
/** Entries kept in the app undone (server/staged.mjs remove): the censuses they finished are open again. */
export function censusEntriesUndone(store, entryIds) {
  if (!entryIds.length) return;
  const rows = store.db
    .prepare(`SELECT id FROM censuses WHERE status = 'finished' AND staged_entry IN (${entryIds.map(() => '?').join(',')})`)
    .all(...entryIds);
  for (const r of rows) reopenFinished(store.db, r.id);
  if (rows.length) bump(store);
}

/** Abandons an open census (its marks stay, for the history). */
export function cancelCensus(store, censusId, user) {
  store.validateRole(user);
  const c = getCensus(store, censusId);
  requireOpen(c);
  store.db.prepare("UPDATE censuses SET status = 'cancelled', finished_by = ?, finished_at = ? WHERE id = ?").run(user.id || user.username, now(), c.id);
  bump(store);
  return censusDetail(store, c.id);
}

/** The paper notebook was brought up to date with this census (or not yet, `done: false`). */
export function setCensusNotebook(store, censusId, body, user) {
  store.validateRole(user);
  const c = getCensus(store, censusId);
  if (c.status !== 'finished') throw fail('CENSUS_NOT_FINISHED', 'Ese censo no está terminado', 409);
  if (body.done === false) store.db.prepare('UPDATE censuses SET notebook_by = NULL, notebook_at = NULL WHERE id = ?').run(c.id);
  else store.db.prepare('UPDATE censuses SET notebook_by = ?, notebook_at = ? WHERE id = ?').run(user.id || user.username, now(), c.id);
  bump(store);
  return censusDetail(store, c.id);
}
