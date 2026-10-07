// Clutches checked on a phone or tablet (the Clutches tab's cards). Several
// people go through the clutches in parallel, one with the paper notebook and
// others with the app; each clutch looked at is marked "checked" here, with the
// fields its check changed (or none), so everyone sees which clutches were
// checked today, by whom, and which changed. The changes themselves are written
// to the sheet by the usual save (records/batch) and read back from the history.
//
// A check lives only in the app (this table): it is never written to Google
// Sheets nor recorded as a save in the history, belongs to one day in Ecuador
// (the next day starts with none) and is seen by everyone. A check is either
// "checked" or "checked, needs verification" (couldn't find all larvae…), with
// an optional short reason, so someone else looks again; the latest one counts.
//
// Also app-only: what the paper cannot hold (clutch_events): per clutch and
// day, larvae hatched, died, disappeared (not the same as died) or preserved
// (with their Insectary IDs), and the same for eggs and pupae; the clutches'
// photos (server/clutch-photos.mjs), listed with the day and the timeline; one
// team setting (whether preserved larvae are taken off NUMBER OF LARVAE: not by
// default since 5 Oct 2026, the count holds the larvae used); and the
// notebook's list: the changes made through the app since the paper notebook
// was last brought up to date, leaving out what came from the notebook itself.

import { randomUUID } from 'node:crypto';
import { isSumField, moduleMap, simpleSum } from './schema.mjs';
import { requireAdmin } from './auth.mjs';

const SHEET = 'Insectary_stocks';
const fail = (code, message, status = 400) => Object.assign(new Error(message), { code, status });
const parse = text => (text ? JSON.parse(text) : null);

export function initClutches(db) {
  db.exec(`CREATE TABLE IF NOT EXISTS clutch_checks(id TEXT PRIMARY KEY, request_id TEXT UNIQUE, record_id TEXT NOT NULL,
      clutch TEXT, day TEXT NOT NULL, actor TEXT NOT NULL, fields_json TEXT NOT NULL, action_id TEXT, created_at TEXT NOT NULL);
    CREATE INDEX IF NOT EXISTS clutch_checks_day ON clutch_checks(day, record_id);
    CREATE TABLE IF NOT EXISTS clutch_events(id TEXT PRIMARY KEY, request_id TEXT UNIQUE, record_id TEXT NOT NULL, clutch TEXT,
      day TEXT NOT NULL, stage TEXT NOT NULL, kind TEXT NOT NULL, count INTEGER NOT NULL, ids_json TEXT NOT NULL DEFAULT '[]',
      note TEXT, actor TEXT NOT NULL, action_id TEXT, created_at TEXT NOT NULL);
    CREATE INDEX IF NOT EXISTS clutch_events_record ON clutch_events(record_id, day);
    CREATE INDEX IF NOT EXISTS clutch_events_created ON clutch_events(created_at);`);
  const columns = new Set(db.prepare('PRAGMA table_info(clutch_checks)').all().map(c => c.name));
  // A check that still needs verification, and why (older databases).
  if (!columns.has('state')) db.exec("ALTER TABLE clutch_checks ADD COLUMN state TEXT NOT NULL DEFAULT 'checked'");
  if (!columns.has('note')) db.exec('ALTER TABLE clutch_checks ADD COLUMN note TEXT');
  // The entry kept in the app a check or event went with (server/staged.mjs), until it is written.
  if (!columns.has('staged_entry')) db.exec('ALTER TABLE clutch_checks ADD COLUMN staged_entry TEXT');
  const eventColumns = new Set(db.prepare('PRAGMA table_info(clutch_events)').all().map(c => c.name));
  if (!eventColumns.has('staged_entry')) db.exec('ALTER TABLE clutch_events ADD COLUMN staged_entry TEXT');
  // The term an event wrote in its stage's count (+27, −2; none for preserved ones kept counted), the
  // group it happened to (and, hatched or pupated, the earlier stage's group it came from), whether its
  // day is known (larvae found already big: the hatch day is NA), and the step it was recorded in.
  for (const [column, type] of [
    ['field', 'TEXT'],
    ['term', 'INTEGER'],
    ['group_id', 'TEXT'],
    ['from_group_id', 'TEXT'],
    ['day_known', 'INTEGER NOT NULL DEFAULT 1'],
    ['step_id', 'TEXT'],
    // A term written before the app (in the sheet or the notebook) given its event when it first got a
    // photo, a note or a loss (`adopted`); where the term sits in its group's parentheses (`term_at`, a
    // hint to tell equal terms apart); the term a loss was taken from (`of_event_id`: −1 from the +27).
    ['adopted', 'INTEGER NOT NULL DEFAULT 0'],
    ['term_at', 'INTEGER'],
    ['of_event_id', 'TEXT'],
  ])
    if (!eventColumns.has(column)) db.exec(`ALTER TABLE clutch_events ADD COLUMN ${column} ${type}`);
  initClutchGroups(db);
}

/** Check states: looked at and fine, or looked at and someone should look again. */
export const CHECK_STATES = new Set(['checked', 'verify']);
/**
 * What can happen to a clutch's eggs, larvae, pupae and adults: the gain of the
 * stage, then the losses; eggs that never hatched (no term: the eggs laid stay
 * counted), a correction of the total (its own term, ±), and a transfer between
 * groups (a term in each group's parentheses).
 */
export const EVENT_KINDS = {
  egg: ['laid', 'died', 'disappeared', 'preserved', 'not_hatched', 'correction', 'transfer'],
  larva: ['hatched', 'died', 'disappeared', 'preserved', 'correction', 'transfer'],
  pupa: ['pupated', 'died', 'disappeared', 'preserved', 'correction', 'transfer'],
  adult: ['emerged', 'correction'],
};
/** The count column each stage's events write their terms in. */
export const STAGE_FIELD = { egg: 'NUMBER OF EGGS', larva: 'NUMBER OF LARVAE', pupa: 'NUMBER OF PUPA', adult: 'NUMBER OF ADULTS' };
const LOSS_KINDS = new Set(['died', 'disappeared', 'preserved']);
const SUBTRACT_KEY = 'clutches.subtractPreserved';
const UP_TO_KEY = 'clutches.notebookUpTo';
/** Columns the sheet fills with its own formulas (counted from Insectary_data): never in the notebook. */
const FORMULA_COLUMNS = new Set(['Earliest Emerge Date', 'Number of Adults in Insectary_data']);

/** Today in Ecuador (the insectary's day), as an ISO date. */
const ECUADOR_DAY = new Intl.DateTimeFormat('en-CA', { timeZone: 'America/Guayaquil' });
export const ecuadorDay = (now = new Date()) => ECUADOR_DAY.format(now);
/** The UTC instants an Ecuador day runs between (UTC−5 all year). */
function dayRange(day) {
  const start = Date.parse(`${day}T05:00:00.000Z`);
  return [new Date(start).toISOString(), new Date(start + 86_400_000).toISOString()];
}
function dayOf(query) {
  const day = query?.day ? String(query.day) : ecuadorDay();
  if (!/^\d{4}-\d{2}-\d{2}$/.test(day) || Number.isNaN(Date.parse(day))) throw fail('INVALID_DAY', 'Invalid day');
  return day;
}

/**
 * The sum formulas of every clutch's counts (=3+5-2: the table only carries
 * what they add up to), each clutch's last change in the history, what its
 * app-only events add up to per stage (`tallies`) and the team's settings.
 */
export function clutchState(store) {
  const mod = moduleMap.get(SHEET);
  const sums = {};
  const byNumber = new Map();
  for (const r of store.db
    .prepare(`SELECT id, formulas_json, json_extract(values_json, '$."CLUTCH NUMBER"') clutch FROM records WHERE sheet=? AND missing=0 AND row_num>?`)
    .all(SHEET, mod.headerRow)) {
    const formulas = parse(r.formulas_json) || {};
    for (const [field, formula] of Object.entries(formulas)) {
      const sum = isSumField(SHEET, field) ? simpleSum(formula) : null;
      if (sum) (sums[r.id] ??= {})[field] = sum;
    }
    const number = clutchText(r.clutch);
    if (number && !byNumber.has(number)) byNumber.set(number, r.id);
  }
  const last = {};
  // SQLite takes the other columns from the row holding the max().
  for (const r of store.db
    .prepare(
      `SELECT c.record_id, max(a.created_at) at, a.actor, u.display_name name FROM changes c JOIN actions a ON a.id = c.action_id
       LEFT JOIN users u ON u.id = a.actor WHERE c.sheet = ? AND c.moved = 0 AND a.status IN ('verified', 'observed') GROUP BY c.record_id`,
    )
    .all(SHEET))
    last[r.record_id] = { at: r.at, actor: r.actor, name: r.name ?? null };
  // What each clutch's events add up to: the app's events and the eggs and larvae registered in Insectary_data.
  const events = new Map();
  for (const e of store.db.prepare('SELECT record_id, stage, kind, count, ids_json FROM clutch_events').all()) {
    if (!events.has(e.record_id)) events.set(e.record_id, []);
    events.get(e.record_id).push({ stage: e.stage, kind: e.kind, count: e.count, ids: parse(e.ids_json) || [] });
  }
  const young = new Map();
  for (const y of youngRows(store)) {
    const id = byNumber.get(y.clutch);
    if (!id) continue;
    if (!young.has(id)) young.set(id, []);
    young.get(id).push(y);
  }
  const tallies = {};
  for (const id of new Set([...events.keys(), ...young.keys()])) tallies[id] = tally(events.get(id) ?? [], young.get(id) ?? []);
  return { sums, last, tallies, settings: clutchSettings(store) };
}

/** CLUTCH NUMBER as text, the same way both sheets write it (1012, "994(6)"). */
const clutchText = v => (v === null || v === undefined || v === '' ? '' : String(v).trim());
/** An Excel serial day as an ISO date. */
const isoOfSerial = serial => new Date(Date.UTC(1899, 11, 30) + Math.round(serial) * 86_400_000).toISOString().slice(0, 10);
const stageOfLife = lifestage => {
  const s = String(lifestage ?? '').trim();
  if (/^egg$/i.test(s)) return 'egg';
  if (/larva|pre-?pupa/i.test(s)) return 'larva';
  if (/^pupa$/i.test(s)) return 'pupa';
  return null;
};
/**
 * The eggs, larvae and pupae of clutches registered one by one in
 * Insectary_data (the Emergidos cards since Sep 2026): preserved alive
 * (Killed_Preserved) or found dead, with their Insectary ID and day.
 */
function youngRows(store, clutch = null) {
  const out = [];
  // The eggs and larvae entered in Emergidos and kept in the app (server/staged.mjs) count as well.
  const staged = (store.staged?.ofSheet('Insectary_data').creates ?? []).map(item => ({
    clutch: item.values['CLUTCH NUMBER'] ?? null,
    lifestage: item.values.LIFESTAGE ?? null,
    id: item.values.Insectary_ID ?? null,
    death: item.values.Death_date ?? null,
    preserved: item.values.Preservation_date ?? null,
    cause: item.values.Death_cause ?? null,
  }));
  for (const r of [
    ...store.db
      .prepare(
        `SELECT json_extract(values_json, '$."CLUTCH NUMBER"') clutch, json_extract(values_json, '$.LIFESTAGE') lifestage,
         json_extract(values_json, '$.Insectary_ID') id, json_extract(values_json, '$.Death_date') death,
         json_extract(values_json, '$.Preservation_date') preserved, json_extract(values_json, '$.Death_cause') cause
       FROM records WHERE sheet = 'Insectary_data' AND missing = 0 AND json_extract(values_json, '$.LIFESTAGE') IS NOT NULL`,
      )
      .all(),
    ...staged.filter(r => r.lifestage !== null),
  ]) {
    const stage = stageOfLife(r.lifestage);
    const number = clutchText(r.clutch);
    if (!stage || !number || !r.id || (clutch !== null && number !== clutch)) continue;
    const serial = typeof r.preserved === 'number' ? r.preserved : typeof r.death === 'number' ? r.death : null;
    out.push({
      id: String(r.id).trim().toUpperCase(),
      clutch: number,
      stage,
      lifestage: String(r.lifestage),
      kind: r.cause === 'Killed_Preserved' ? 'preserved' : 'died',
      day: serial !== null && serial > 30000 && serial < 80000 ? isoOfSerial(serial) : null,
    });
  }
  return out;
}
/**
 * What a clutch's events add up to, per stage: gained (hatched, pupated…),
 * died, disappeared and preserved. An egg or larva registered in
 * Insectary_data counts once: not again when an event already names its ID.
 */
export function tally(events, young = []) {
  const out = {};
  const at = stage => (out[stage] ??= { gained: 0, died: 0, disappeared: 0, preserved: 0, notHatched: 0 });
  const named = new Set();
  for (const e of events) {
    const kinds = EVENT_KINDS[e.stage];
    if (!kinds?.includes(e.kind)) continue;
    // Corrections and transfers are in the sum; they are no gain or loss of their own.
    if (e.kind === 'correction' || e.kind === 'transfer') continue;
    at(e.stage)[e.kind === kinds[0] ? 'gained' : e.kind === 'not_hatched' ? 'notHatched' : e.kind] += e.count;
    for (const id of e.ids ?? []) named.add(String(id).toUpperCase());
  }
  for (const y of young) if (!named.has(y.id)) at(y.stage)[y.kind] += 1;
  return out;
}

/**
 * The team's settings for clutches: whether preserved larvae are taken off
 * NUMBER OF LARVAE. Not by default (the team's convention since 5 Oct 2026:
 * the count holds the larvae used, so only those that died or disappeared are
 * taken off); an administrator can change it.
 */
export function clutchSettings(store) {
  return { subtractPreserved: store.getSetting(SUBTRACT_KEY) === '1' };
}
/** Changes the team's setting (administrators): one convention for the Clutches and Emergidos saves. */
export function setClutchSettings(store, body, user) {
  requireAdmin(user);
  if (typeof body.subtractPreserved !== 'boolean') throw fail('INVALID_SETTING', 'subtractPreserved must be true or false');
  store.setSetting(SUBTRACT_KEY, body.subtractPreserved ? '1' : '0');
  return clutchSettings(store);
}

const shape = r => ({
  id: r.id,
  recordId: r.record_id,
  clutch: r.clutch,
  day: r.day,
  actor: r.actor,
  username: r.username ?? null,
  name: r.name ?? null,
  fields: parse(r.fields_json) || [],
  state: r.state ?? 'checked',
  note: r.note ?? null,
  actionId: r.action_id,
  stagedEntry: r.staged_entry ?? null,
  createdAt: r.created_at,
});
const shapeEvent = r => ({
  id: r.id,
  recordId: r.record_id,
  clutch: r.clutch,
  day: r.day,
  stage: r.stage,
  kind: r.kind,
  count: r.count,
  ids: parse(r.ids_json) || [],
  note: r.note ?? null,
  field: r.field ?? null,
  term: r.term ?? null,
  groupId: r.group_id ?? null,
  fromGroupId: r.from_group_id ?? null,
  dayKnown: r.day_known !== 0,
  stepId: r.step_id ?? null,
  adopted: r.adopted === 1,
  termAt: r.term_at ?? null,
  ofEventId: r.of_event_id ?? null,
  actor: r.actor,
  username: r.username ?? null,
  name: r.name ?? null,
  actionId: r.action_id,
  stagedEntry: r.staged_entry ?? null,
  createdAt: r.created_at,
});
const EVENT_SELECT = 'SELECT e.*, u.username, u.display_name name FROM clutch_events e LEFT JOIN users u ON u.id = e.actor';
/** A clutch's photo as the day and the timeline list it (the bytes: server/clutch-photos.mjs). */
const PHOTO_SELECT =
  'SELECT p.id, p.record_id, p.clutch, p.day, p.event_id, p.group_id, p.note, p.actor, p.width, p.height, p.bytes, p.thumb_bytes, p.created_at, u.username, u.display_name name FROM clutch_photos p LEFT JOIN users u ON u.id = p.actor';
const shapePhoto = r => ({
  id: r.id,
  recordId: r.record_id,
  clutch: r.clutch,
  day: r.day,
  eventId: r.event_id ?? null,
  groupId: r.group_id ?? null,
  note: r.note ?? null,
  actor: r.actor,
  username: r.username ?? null,
  name: r.name ?? null,
  width: r.width,
  height: r.height,
  bytes: r.bytes,
  thumbBytes: r.thumb_bytes,
  createdAt: r.created_at,
});

/**
 * One day's checks and changes of clutches: the checks marked in the app, and
 * every change to Insectary_stocks that day (app, assistant or Google Sheets),
 * as each field's value before the first change and after the last, with the
 * changes still standing (`parts`: change, save and person) to undo them.
 */
export function clutchDay(store, query = {}) {
  const day = dayOf(query);
  const checks = store.db
    .prepare(
      `SELECT k.*, u.username, u.display_name name FROM clutch_checks k LEFT JOIN users u ON u.id = k.actor
       WHERE k.day = ? ORDER BY k.created_at`,
    )
    .all(day)
    .map(shape);
  const events = store.db.prepare(`${EVENT_SELECT} WHERE e.day = ? ORDER BY e.created_at`).all(day).map(shapeEvent);
  const photos = store.db.prepare(`${PHOTO_SELECT} WHERE p.day = ? ORDER BY p.created_at`).all(day).map(shapePhoto);
  const [from, to] = dayRange(day);
  const rows = store.db
    .prepare(
      `SELECT c.id change_id, c.record_id, c.field, c.before_json, c.after_json, a.id action_id, a.source, a.reverses, a.actor,
         a.created_at, u.username, u.display_name name, r.values_json
         FROM changes c JOIN actions a ON a.id = c.action_id LEFT JOIN users u ON u.id = a.actor
         LEFT JOIN records r ON r.id = c.record_id
       WHERE c.sheet = ? AND c.moved = 0 AND a.created_at >= ? AND a.created_at < ? AND a.status IN ('verified', 'observed')
       ORDER BY a.created_at, a.id`,
    )
    .all(SHEET, from, to);
  const byKey = new Map();
  for (const r of rows) {
    const key = `${r.record_id}\u0000${r.field}`;
    let change = byKey.get(key);
    if (!change) {
      const values = parse(r.values_json) || {};
      change = {
        recordId: r.record_id,
        clutch: values['CLUTCH NUMBER'] === null || values['CLUTCH NUMBER'] === undefined ? '' : String(values['CLUTCH NUMBER']),
        species: values.SPECIES === null || values.SPECIES === undefined ? '' : String(values.SPECIES),
        field: r.field,
        before: parse(r.before_json),
        after: null,
        actors: [],
        at: r.created_at,
        parts: [],
      };
      byKey.set(key, change);
    }
    // The changes still standing, to undo them from the day's list: an undo takes back the ones it reverses.
    if (r.source === 'undo' && r.reverses) {
      const reversed = new Set(r.reverses.split(','));
      change.parts = change.parts.filter(p => !reversed.has(p.actionId));
    } else change.parts.push({ changeId: r.change_id, actionId: r.action_id, actor: r.actor });
    change.after = parse(r.after_json);
    change.at = r.created_at;
    const who = r.actor === 'unknown' ? 'Google Sheets' : (r.name ?? r.username ?? r.actor);
    if (!change.actors.includes(who)) change.actors.push(who);
    if (r.actor !== 'unknown' && !(change.actorIds ??= []).includes(r.actor)) change.actorIds.push(r.actor);
  }
  const same = (a, b) => JSON.stringify(a ?? null) === JSON.stringify(b ?? null);
  const changes = [...byKey.values()].filter(c => !same(c.before, c.after)).map(c => ({ ...c, actorIds: c.actorIds ?? [] }));
  // A new clutch: its number was written that day.
  const created = new Set(changes.filter(c => c.field === 'CLUTCH NUMBER' && c.before === null).map(c => c.recordId));
  for (const c of changes) c.isNew = created.has(c.recordId);
  // Entries kept in the app, not in the sheet yet (server/staged.mjs): listed too, marked, without undo (undone in the tab).
  for (const line of stagedLines(store, from, to)) changes.push({ ...line, actorIds: line.actorIds, parts: [] });
  const notes = store.db.prepare(`${NOTE_SELECT} WHERE n.day = ? ORDER BY n.updated_at`).all(day).map(shapeNote);
  return { day, checks, changes, events, photos, notes };
}

/**
 * The Clutches entries kept in the app (server/staged.mjs) entered between `from` and `to`,
 * as the day's and the notebook's lists show changes: per clutch and field, before (the
 * sheet's) → after, who and when, `staged: true`; a new clutch's every cell (`isNew`).
 */
function stagedLines(store, from, to) {
  if (!store.staged) return [];
  const { edits, creates } = store.staged.ofSheet(SHEET);
  const out = [];
  const within = at => at >= from && at < to;
  const valuesOf = store.db.prepare('SELECT values_json FROM records WHERE id = ?');
  for (const [recordId, list] of edits) {
    const values = parse(valuesOf.get(recordId)?.values_json) || {};
    const byField = new Map();
    for (const e of list) {
      if (!within(e.item.updatedAt) && !within(e.item.createdAt)) continue;
      const line = byField.get(e.field) ?? {
        recordId,
        clutch: clutchText(values['CLUTCH NUMBER']),
        species: clutchText(values.SPECIES),
        field: e.field,
        before: e.before,
        after: null,
        actors: [],
        actorIds: [],
        at: e.at,
        isNew: false,
        staged: true,
      };
      line.after = e.after;
      line.at = e.at;
      if (!line.actors.includes(e.actor)) line.actors.push(e.actor);
      if (!line.actorIds.includes(e.item.actor)) line.actorIds.push(e.item.actor);
      byField.set(e.field, line);
    }
    out.push(...[...byField.values()].filter(l => JSON.stringify(l.before ?? null) !== JSON.stringify(l.after ?? null)));
  }
  for (const item of creates) {
    if (!within(item.createdAt) && !within(item.updatedAt)) continue;
    for (const [field, after] of Object.entries(item.values))
      if (after !== null && after !== '')
        out.push({
          recordId: item.rowId,
          clutch: clutchText(item.values['CLUTCH NUMBER']),
          species: clutchText(item.values.SPECIES),
          field,
          before: null,
          after,
          actors: [item.actorName],
          actorIds: [item.actor],
          at: item.updatedAt,
          isNew: true,
          staged: true,
        });
  }
  return out;
}

/**
 * Marks a clutch as checked today by this person, with the fields the check
 * changed (none: "no change"); `state` "verify" when it needs someone to look
 * again, with an optional short reason (`note`).
 */
export function addClutchCheck(store, body, user) {
  const prior = store.db.prepare('SELECT k.*, u.username, u.display_name name FROM clutch_checks k LEFT JOIN users u ON u.id = k.actor WHERE k.request_id = ?').get(body.requestId);
  if (prior) return { check: shape(prior), duplicate: true };
  const record = store.db.prepare('SELECT id, sheet, values_json FROM records WHERE id = ? AND missing = 0').get(String(body.recordId || ''));
  if (!record || record.sheet !== SHEET) throw fail('RECORD_NOT_FOUND', 'Clutch not found', 404);
  const known = new Set(moduleMap.get(SHEET).fields.map(f => f.key));
  const fields = body.fields === undefined || body.fields === null ? [] : body.fields;
  if (!Array.isArray(fields) || fields.length > 40 || fields.some(f => typeof f !== 'string' || !known.has(f)))
    throw fail('INVALID_FIELDS', 'Invalid fields');
  const state = body.state === undefined || body.state === null ? 'checked' : body.state;
  if (!CHECK_STATES.has(state)) throw fail('INVALID_STATE', 'Invalid state');
  const note = cleanNote(body.note);
  const actionId = body.actionId ? String(body.actionId) : null;
  if (actionId && !store.db.prepare('SELECT 1 FROM actions WHERE id = ?').get(actionId)) throw fail('ACTION_NOT_FOUND', 'Save not found', 404);
  const values = parse(record.values_json) || {};
  const clutch = values['CLUTCH NUMBER'] === null || values['CLUTCH NUMBER'] === undefined ? null : String(values['CLUTCH NUMBER']);
  const id = randomUUID();
  store.db
    .prepare(
      'INSERT INTO clutch_checks(id,request_id,record_id,clutch,day,actor,fields_json,action_id,created_at,state,note,staged_entry) VALUES(?,?,?,?,?,?,?,?,?,?,?,?)',
    )
    .run(id, body.requestId, record.id, clutch, ecuadorDay(), user.id, JSON.stringify([...new Set(fields)]), actionId, new Date().toISOString(), state, note, stagedEntryOf(store, body));
  const row = store.db.prepare('SELECT k.*, u.username, u.display_name name FROM clutch_checks k LEFT JOIN users u ON u.id = k.actor WHERE k.id = ?').get(id);
  return { check: shape(row), duplicate: false };
}

/**
 * The entry kept in the app (server/staged.mjs) a check or event went with, when its
 * changes are not in the sheet yet: its save becomes the check's once written.
 */
function stagedEntryOf(store, body) {
  if (!body.stagedEntry) return null;
  const entry = String(body.stagedEntry);
  if (!store.db.prepare('SELECT 1 FROM staged WHERE entry_id = ?').get(entry)) return null;
  return entry;
}

/** A short reason or note: trimmed, up to 200 characters, or none. */
function cleanNote(value) {
  if (value === undefined || value === null) return null;
  if (typeof value !== 'string') throw fail('INVALID_NOTE', 'Invalid note');
  const text = value.trim().replace(/\s+/g, ' ');
  if (text.length > 200) throw fail('INVALID_NOTE', 'The note is too long (200 characters at most)');
  return text || null;
}

/** Takes back a check marked by mistake: one's own, or anyone's for reviewers and admins. */
export function removeClutchCheck(store, id, user) {
  const row = store.db.prepare('SELECT * FROM clutch_checks WHERE id = ?').get(id);
  if (!row) throw fail('CHECK_NOT_FOUND', 'Check not found', 404);
  if (row.actor !== user.id && !['reviewer', 'admin'].includes(user.role)) throw fail('FORBIDDEN', 'Only your own checks', 403);
  store.db.prepare('DELETE FROM clutch_checks WHERE id = ?').run(id);
  return { removed: id };
}

// --- Events the paper cannot hold: hatched, died, disappeared, preserved (app-only)

function stocksRecord(store, recordId) {
  const record = store.db.prepare('SELECT id, sheet, values_json FROM records WHERE id = ? AND missing = 0').get(String(recordId || ''));
  if (!record || record.sheet !== SHEET) throw fail('RECORD_NOT_FOUND', 'Clutch not found', 404);
  return record;
}
/** Insectary IDs as written on the tubes (H0E, W0B.1), upper case, each once. */
function cleanIds(value) {
  if (value === undefined || value === null) return [];
  if (!Array.isArray(value) || value.length > 200) throw fail('INVALID_IDS', 'Invalid Insectary IDs');
  const out = [];
  for (const raw of value) {
    const id = typeof raw === 'string' ? raw.trim().toUpperCase() : '';
    if (!/^[A-Z0-9]{2,8}(\.\d{1,2})?$/.test(id)) throw fail('INVALID_IDS', `Invalid Insectary ID: ${String(raw).slice(0, 20)}`);
    if (!out.includes(id)) out.push(id);
  }
  return out;
}

/**
 * Records what happened to some of a clutch's eggs, larvae, pupae or adults on
 * a day (today by default): `kind` one of EVENT_KINDS[stage], `count` how
 * many, the Insectary IDs of those preserved if known, a short note, and the
 * save that changed the count with it (if any). Only in the app.
 */
export function addClutchEvent(store, body, user) {
  const prior = store.db.prepare(`${EVENT_SELECT} WHERE e.request_id = ?`).get(body.requestId);
  if (prior) return { event: shapeEvent(prior), duplicate: true };
  const record = stocksRecord(store, body.recordId);
  const actionId = body.actionId ? String(body.actionId) : null;
  if (actionId && !store.db.prepare('SELECT 1 FROM actions WHERE id = ?').get(actionId)) throw fail('ACTION_NOT_FOUND', 'Save not found', 404);
  const id = insertEvent(store, record, { ...body, actionId }, user, { requestId: body.requestId, stagedEntry: stagedEntryOf(store, body) });
  return { event: shapeEvent(store.db.prepare(`${EVENT_SELECT} WHERE e.id = ?`).get(id)), duplicate: false };
}

/** An event's day: today unless an earlier one is given (ISO), never one to come. */
function eventDay(value) {
  const today = ecuadorDay();
  const day = value === undefined || value === null ? today : String(value);
  if (!/^\d{4}-\d{2}-\d{2}$/.test(day) || Number.isNaN(Date.parse(day)) || day > today || day < '2020-01-01') throw fail('INVALID_DAY', 'Invalid day');
  return day;
}
/** The stage before (where a hatch or a pupation comes from). */
const STAGE_BEFORE = { larva: 'egg', pupa: 'larva', adult: 'pupa' };
/**
 * An event checked: its stage and kind, how many, the term it writes in the
 * stage's count (a gain +count, a loss −count or none, a correction or a
 * transfer ±count, eggs that did not hatch none) and the groups it names, of
 * this clutch: `groupId` of the stage's count, `fromGroupId` of the stage
 * before (hatched from the eggs of box A) or, a transfer, of the same stage.
 */
function checkedEvent(store, record, body) {
  const kinds = EVENT_KINDS[body.stage];
  if (!kinds) throw fail('INVALID_STAGE', 'Invalid stage');
  if (!kinds.includes(body.kind)) throw fail('INVALID_KIND', 'Invalid event for this stage');
  const count = body.count;
  if (!Number.isInteger(count) || count < 1 || count > 9999) throw fail('INVALID_COUNT', 'The count must be a whole number from 1 to 9999');
  const ids = cleanIds(body.ids);
  if (ids.length > count) throw fail('INVALID_IDS', 'More Insectary IDs than the count');
  const term = body.term === undefined || body.term === null ? null : body.term;
  const both = body.kind === 'correction' || body.kind === 'transfer';
  // A term written before the app, adopted: its event says only what the sign does (a gain of the
  // stage, or a minus whose cause is not known: a correction); the formula already holds it.
  const adopted = body.adopted === true;
  if (adopted && (term === null || (term > 0 ? body.kind !== kinds[0] : body.kind !== 'correction')))
    throw fail('INVALID_TERM', 'An adopted term is a gain of its stage, or a minus as a correction');
  if (term !== null) {
    if (!Number.isInteger(term)) throw fail('INVALID_TERM', 'Invalid term');
    const ok = body.kind === kinds[0] ? term === count : LOSS_KINDS.has(body.kind) ? term === -count : both ? Math.abs(term) === count : false;
    if (!ok) throw fail('INVALID_TERM', 'The term does not match the event');
  } else if (both) throw fail('INVALID_TERM', 'A correction or a transfer needs its term');
  const groupOf = (value, stage) => {
    if (value === undefined || value === null || value === '') return null;
    const g = store.db.prepare('SELECT id, stage FROM clutch_groups WHERE id = ? AND record_id = ?').get(String(value), record.id);
    if (!g || g.stage !== stage) throw fail('GROUP_NOT_FOUND', 'Group not found for this clutch and stage', 404);
    return g.id;
  };
  return {
    stage: body.stage,
    kind: body.kind,
    count,
    ids,
    term,
    field: term === null ? null : STAGE_FIELD[body.stage],
    groupId: groupOf(body.groupId, body.stage),
    fromGroupId: STAGE_BEFORE[body.stage] || body.kind === 'transfer' ? groupOf(body.fromGroupId, body.kind === 'transfer' ? body.stage : STAGE_BEFORE[body.stage]) : null,
    day: eventDay(body.day),
    dayKnown: body.dayKnown !== false,
    note: cleanNote(body.note),
    adopted,
    termAt: termAtOf(body.termAt),
    ofEventId: ofEventOf(store, record, body.ofEventId, body.stage),
  };
}
const termAtOf = value => {
  if (value === undefined || value === null) return null;
  if (!Number.isInteger(value) || value < 0 || value > 999) throw fail('INVALID_TERM', 'Invalid term position');
  return value;
};
/** The term of this clutch's stage a loss was taken from (its event). */
function ofEventOf(store, record, value, stage) {
  if (value === undefined || value === null || value === '') return null;
  const e = store.db.prepare('SELECT id, stage, term FROM clutch_events WHERE id = ? AND record_id = ?').get(String(value), record.id);
  if (!e || e.stage !== stage || e.term === null) throw fail('EVENT_NOT_FOUND', 'The term it was taken from is not of this clutch and stage', 404);
  return e.id;
}
/** Stores one event (checked); its id. */
function insertEvent(store, record, body, user, { requestId = null, stepId = null, stagedEntry = null } = {}) {
  const e = checkedEvent(store, record, body);
  const values = parse(record.values_json) || {};
  const id = randomUUID();
  store.db
    .prepare(
      `INSERT INTO clutch_events(id,request_id,record_id,clutch,day,stage,kind,count,ids_json,note,actor,action_id,created_at,staged_entry,
         field,term,group_id,from_group_id,day_known,step_id,adopted,term_at,of_event_id) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)`,
    )
    .run(
      id,
      requestId,
      record.id,
      clutchText(values['CLUTCH NUMBER']) || null,
      e.day,
      e.stage,
      e.kind,
      e.count,
      JSON.stringify(e.ids),
      e.note,
      user.id,
      body.actionId ?? null,
      new Date().toISOString(),
      stagedEntry,
      e.field,
      e.term,
      e.groupId,
      e.fromGroupId,
      e.dayKnown ? 1 : 0,
      stepId,
      e.adopted ? 1 : 0,
      e.termAt,
      e.ofEventId,
    );
  return id;
}

/** Takes back an event recorded by mistake: one's own, or anyone's for reviewers and admins. */
export function removeClutchEvent(store, id, user) {
  const row = store.db.prepare('SELECT * FROM clutch_events WHERE id = ?').get(id);
  if (!row) throw fail('EVENT_NOT_FOUND', 'Event not found', 404);
  if (row.actor !== user.id && !['reviewer', 'admin'].includes(user.role)) throw fail('FORBIDDEN', 'Only your own events', 403);
  store.db.prepare('DELETE FROM clutch_events WHERE id = ?').run(id);
  // Its photos stay, as the day's; losses taken from it stay, no longer linked.
  store.db.prepare('UPDATE clutch_photos SET event_id = NULL WHERE event_id = ?').run(id);
  store.db.prepare('UPDATE clutch_events SET of_event_id = NULL WHERE of_event_id = ?').run(id);
  return { removed: id };
}

/**
 * What is said of an event, corrected: its day (or that it is not known), the
 * cause of a loss (among the losses that write the same term), its group and
 * its note. Its count and term stay: the sum changes only by its own steps.
 */
export function updateClutchEvent(store, id, body, user) {
  const row = store.db.prepare('SELECT * FROM clutch_events WHERE id = ?').get(String(id));
  if (!row) throw fail('EVENT_NOT_FOUND', 'Event not found', 404);
  if (!row.adopted && row.actor !== user.id && !['reviewer', 'admin'].includes(user.role)) throw fail('FORBIDDEN', 'Only your own events', 403);
  const record = stocksRecord(store, row.record_id);
  const kind = body.kind === undefined || body.kind === null ? row.kind : String(body.kind);
  if (kind !== row.kind && !(LOSS_KINDS.has(kind) && LOSS_KINDS.has(row.kind))) throw fail('INVALID_KIND', 'Only the cause of a loss can change');
  const e = checkedEvent(store, record, {
    stage: row.stage,
    kind,
    count: row.count,
    ids: parse(row.ids_json) || [],
    term: row.term,
    groupId: body.groupId === undefined ? row.group_id : body.groupId,
    fromGroupId: row.from_group_id,
    day: body.day === undefined || body.day === null ? row.day : body.day,
    dayKnown: body.dayKnown === undefined ? row.day_known !== 0 : body.dayKnown !== false,
    note: body.note === undefined ? row.note : body.note,
  });
  store.db
    .prepare('UPDATE clutch_events SET kind = ?, group_id = ?, day = ?, day_known = ?, note = ? WHERE id = ?')
    .run(e.kind, e.groupId, e.day, e.dayKnown ? 1 : 0, e.note, row.id);
  return { event: shapeEvent(store.db.prepare(`${EVENT_SELECT} WHERE e.id = ?`).get(row.id)) };
}


/**
 * One clutch's events, every day (oldest first), with the eggs and larvae of
 * it registered in Insectary_data (Emergidos), its photos, what they add up
 * to, and the team's settings: the clutch editor's timeline.
 */
export function clutchEvents(store, query = {}) {
  const record = stocksRecord(store, query.recordId);
  const events = store.db.prepare(`${EVENT_SELECT} WHERE e.record_id = ? ORDER BY e.day, e.created_at`).all(record.id).map(shapeEvent);
  const number = clutchText((parse(record.values_json) || {})['CLUTCH NUMBER']);
  const young = number ? youngRows(store, number) : [];
  const photos = store.db.prepare(`${PHOTO_SELECT} WHERE p.record_id = ? ORDER BY p.day, p.created_at`).all(record.id).map(shapePhoto);
  const groups = store.db.prepare(`${GROUP_SELECT} WHERE g.record_id = ? ORDER BY g.field, g.ended_at IS NOT NULL, g.position, g.created_at`).all(record.id).map(shapeGroup);
  const log = store.db.prepare(`${LOG_SELECT} WHERE l.record_id = ? ORDER BY l.day, l.created_at`).all(record.id).map(shapeLog);
  const notes = store.db.prepare(`${NOTE_SELECT} WHERE n.record_id = ? ORDER BY n.day`).all(record.id).map(shapeNote);
  return { recordId: record.id, clutch: number, events, young, photos, groups, log, notes, tally: tally(events, young), settings: clutchSettings(store) };
}

// --- The notebook's list: what the app changed since the paper notebook was brought up to date

const NOTEBOOK_REASON = /cuaderno|notebook/i;
/** When the paper notebook was last brought up to date with the app (shared by everyone), or null. */
export function notebookUpTo(store) {
  try {
    return parse(store.getSetting(UP_TO_KEY));
  } catch {
    return null;
  }
}
/** "The notebook is up to date until <at>": remembered for everyone, with who said so. */
export function setNotebookUpTo(store, body, user) {
  const at = typeof body.at === 'string' ? Date.parse(body.at) : NaN;
  if (Number.isNaN(at) || at > Date.now() + 5 * 60_000 || at < Date.parse('2020-01-01')) throw fail('INVALID_DATE', 'Invalid date and time');
  const upTo = { at: new Date(at).toISOString(), by: user.id, name: user.displayName || user.username, setAt: new Date().toISOString() };
  store.setSetting(UP_TO_KEY, JSON.stringify(upTo));
  return { upTo };
}

/**
 * Where a save came from, for the notebook: the app (cards, table), the
 * assistant (not from the notebook), the notebook itself (an assistant
 * proposal read from notebook photos: "Cuaderno Posturas …", match_notebook),
 * Google Sheets (typed in the sheet), or something else (imports). An undo
 * belongs where the save it takes back came from.
 */
function sourceOf(store) {
  const memo = new Map();
  let proposal;
  try {
    proposal = store.db.prepare(
      'SELECT reason, page_json IS NOT NULL page FROM ai_proposals WHERE owner_id = ? AND applied_at >= ? AND applied_at <= ? ORDER BY applied_at LIMIT 1',
    );
  } catch {
    proposal = null;
  }
  const byId = store.db.prepare('SELECT id, source, reason, reverses, actor, created_at FROM actions WHERE id = ?');
  const of = (action, depth = 0) => {
    if (memo.has(action.id)) return memo.get(action.id);
    let source = 'other';
    if (action.source === 'app') source = 'app';
    else if (action.source === 'sheet_reconciliation') source = 'sheets';
    else if (action.source === 'ai_approved') {
      source = NOTEBOOK_REASON.test(action.reason ?? '') ? 'notebook' : 'assistant';
      if (source === 'assistant' && proposal) {
        // The person may give the save their own reason ("Confirmado en el chat"): the proposal written just then says where it came from.
        const until = new Date(Date.parse(action.created_at) + 60_000).toISOString();
        const p = proposal.get(action.actor, action.created_at, until);
        if (p && (p.page || NOTEBOOK_REASON.test(p.reason ?? ''))) source = 'notebook';
      }
    } else if (action.source === 'undo' && action.reverses && depth < 5) {
      const sources = action.reverses
        .split(',')
        .map(id => byId.get(id))
        .filter(Boolean)
        .map(a => of(a, depth + 1));
      source = sources.includes('app') ? 'app' : (sources[0] ?? 'other');
    }
    memo.set(action.id, source);
    return source;
  };
  return of;
}

/** "994(2)" → [994, 2, ""]: clutch number ascending, batches N(k) under N, other forms after. */
function notebookOrder(clutch) {
  const m = /^\s*(\d+)\s*(?:\((\d+)\))?\s*(.*)$/.exec(clutch);
  return m ? [Number(m[1]), m[2] ? Number(m[2]) : 1, m[3]] : [Infinity, 0, clutch];
}
const byNotebook = (a, b) => {
  const [x, y] = [notebookOrder(a.clutch), notebookOrder(b.clutch)];
  return x[0] - y[0] || x[1] - y[1] || x[2].localeCompare(y[2], 'en', { numeric: true });
};

/**
 * The changes to clutches made through the app by everyone between `from`
 * and `to` (ISO instants; by default since the notebook was last brought up to
 * date, else since this morning, until now), clutch by clutch in the
 * notebook's order, each field as before → after with who and when, and the
 * app-only events; to copy into the paper notebook by hand. Left out: what
 * came from the notebook (assistant proposals read from its photos) and,
 * unless `sheets` is "1", what was typed in Google Sheets (the notebook's own
 * transcription, as a rule). Changes before and after a left-out one are
 * listed apart, so a term the notebook already has is never shown as new.
 */
export function notebookChanges(store, query = {}) {
  const upTo = notebookUpTo(store);
  const instant = (value, fallback) => {
    if (value === undefined || value === null || value === '') return fallback;
    const t = Date.parse(String(value));
    if (Number.isNaN(t)) throw fail('INVALID_DATE', 'Invalid date and time');
    return new Date(t).toISOString();
  };
  const from = instant(query.from, upTo?.at ?? dayRange(ecuadorDay())[0]);
  const to = instant(query.to, new Date(Date.now() + 1000).toISOString());
  if (from >= to) throw fail('INVALID_RANGE', 'The start must be before the end');
  const withSheets = query.sheets === '1' || query.sheets === 'true';
  const include = new Set(['app', 'assistant', ...(withSheets ? ['sheets'] : [])]);
  const sourceFor = sourceOf(store);
  const order = new Map(moduleMap.get(SHEET).fields.map((f, i) => [f.key, i]));
  const rows = store.db
    .prepare(
      `SELECT c.record_id, c.field, c.before_json, c.after_json, a.id, a.source, a.reason, a.reverses, a.actor, a.created_at,
         u.username, u.display_name name, r.values_json
       FROM changes c JOIN actions a ON a.id = c.action_id LEFT JOIN users u ON u.id = a.actor LEFT JOIN records r ON r.id = c.record_id
       WHERE c.sheet = ? AND c.moved = 0 AND a.created_at >= ? AND a.created_at < ? AND a.status IN ('verified', 'observed')
       ORDER BY a.created_at, a.id`,
    )
    .all(SHEET, from, to);
  const otherFormula = v => !!v && typeof v === 'object' && typeof v.formula === 'string' && !simpleSum(v.formula);
  const clutches = new Map();
  const clutchOf = (recordId, valuesJson) => {
    let c = clutches.get(recordId);
    if (!c) {
      const values = parse(valuesJson) || {};
      c = {
        recordId,
        clutch: clutchText(values['CLUTCH NUMBER']),
        species: clutchText(values.SPECIES),
        isNew: false,
        lines: [],
        events: [],
      };
      clutches.set(recordId, c);
    }
    return c;
  };
  /** The run of included changes still open per record and field (closed by a left-out change). */
  const open = new Map();
  const excluded = { notebook: 0, sheets: 0 };
  for (const r of rows) {
    if (FORMULA_COLUMNS.has(r.field)) continue;
    const before = parse(r.before_json);
    const after = parse(r.after_json);
    if (otherFormula(before) || otherFormula(after)) continue;
    const source = sourceFor(r);
    const key = `${r.record_id}\u0000${r.field}`;
    if (!include.has(source)) {
      if (source === 'notebook' || source === 'sheets') excluded[source]++;
      open.delete(key);
      continue;
    }
    const who = r.actor === 'unknown' ? 'Google Sheets' : (r.name ?? r.username ?? r.actor);
    let line = open.get(key);
    if (!line) {
      line = { field: r.field, before, after, actors: [], sources: [], firstAt: r.created_at, at: r.created_at };
      clutchOf(r.record_id, r.values_json).lines.push(line);
      open.set(key, line);
    }
    line.after = after;
    line.at = r.created_at;
    if (!line.actors.includes(who)) line.actors.push(who);
    if (!line.sources.includes(source)) line.sources.push(source);
  }
  const same = (a, b) => JSON.stringify(a ?? null) === JSON.stringify(b ?? null);
  // Entries kept in the app (not in the sheet yet): made through the app, so they go in the notebook too, marked.
  for (const line of stagedLines(store, from, to)) {
    if (FORMULA_COLUMNS.has(line.field)) continue;
    const c = clutches.get(line.recordId) ?? clutchOf(line.recordId, JSON.stringify({ 'CLUTCH NUMBER': line.clutch, SPECIES: line.species }));
    c.lines.push({ field: line.field, before: line.before, after: line.after, actors: line.actors, sources: ['app'], firstAt: line.at, at: line.at, staged: true });
  }
  for (const c of clutches.values()) {
    c.lines = c.lines.filter(l => !same(l.before, l.after)).sort((a, b) => (order.get(a.field) ?? 99) - (order.get(b.field) ?? 99) || a.firstAt.localeCompare(b.firstAt));
    // A new clutch: its number was written then.
    c.isNew = c.lines.some(l => l.field === 'CLUTCH NUMBER' && (l.before === null || l.before === ''));
  }
  const valuesOf = store.db.prepare('SELECT values_json FROM records WHERE id = ?');
  // Transfers between groups are in the count's change (its parentheses); not listed one by one.
  for (const e of store.db.prepare(`${EVENT_SELECT} WHERE e.created_at >= ? AND e.created_at < ? AND e.kind <> 'transfer' ORDER BY e.day, e.created_at`).all(from, to))
    clutchOf(e.record_id, valuesOf.get(e.record_id)?.values_json).events.push(shapeEvent(e));
  const list = [...clutches.values()].filter(c => c.lines.length || c.events.length).sort(byNotebook);
  return { from, to, upTo, sheets: withSheets, excluded, clutches: list };
}

// --- Groups, steps, the history of regroupings and formula edits, the day's notes (app-only)
//
// A stage's count is written in the sheet as one parenthesized sub-sum per group
// (box A, box B: =(6-2)+(5+3); a single group is the plain sum =27-2-11-3). The
// app keeps what the parentheses cannot: each group's label, where it came from
// (the larvae of box A hatched from the eggs of box A) and its photos, by
// position: the open groups of a count, in the order of its parentheses. The
// frontend works out the formula (lib/clutchGroups.ts); a step stores at once
// what one action recorded besides it: the events (one per term), the groups as
// they are after it, and a line of history (a regrouping, a formula edited by
// hand). A step can be taken back as a whole (the person's Undo).

const LABEL_MAX = 40;
const DAY_NOTE_MAX = 20_000;
const LOG_KINDS = new Set(['regroup', 'formula']);
const STAGE_OF_FIELD = Object.fromEntries(Object.entries(STAGE_FIELD).map(([stage, field]) => [field, stage]));

export function initClutchGroups(db) {
  db.exec(`CREATE TABLE IF NOT EXISTS clutch_groups(id TEXT PRIMARY KEY, record_id TEXT NOT NULL, field TEXT NOT NULL, stage TEXT NOT NULL,
      position INTEGER NOT NULL, label TEXT, origin_id TEXT, actor TEXT NOT NULL, created_at TEXT NOT NULL, ended_at TEXT, step_id TEXT);
    CREATE INDEX IF NOT EXISTS clutch_groups_record ON clutch_groups(record_id, field);
    CREATE TABLE IF NOT EXISTS clutch_log(id TEXT PRIMARY KEY, record_id TEXT NOT NULL, clutch TEXT, stage TEXT, field TEXT, day TEXT NOT NULL,
      kind TEXT NOT NULL, before_text TEXT, after_text TEXT, note TEXT, actor TEXT NOT NULL, step_id TEXT, created_at TEXT NOT NULL);
    CREATE INDEX IF NOT EXISTS clutch_log_record ON clutch_log(record_id, day);
    CREATE TABLE IF NOT EXISTS clutch_steps(id TEXT PRIMARY KEY, request_id TEXT UNIQUE, record_id TEXT NOT NULL, actor TEXT NOT NULL,
      data_json TEXT NOT NULL, created_at TEXT NOT NULL);
    CREATE TABLE IF NOT EXISTS clutch_notes(id TEXT PRIMARY KEY, record_id TEXT NOT NULL, clutch TEXT, day TEXT NOT NULL, text TEXT NOT NULL,
      actor TEXT NOT NULL, created_at TEXT NOT NULL, updated_by TEXT NOT NULL, updated_at TEXT NOT NULL, UNIQUE(record_id, day));
    CREATE INDEX IF NOT EXISTS clutch_notes_day ON clutch_notes(day);`);
}

const GROUP_SELECT = 'SELECT g.* FROM clutch_groups g';
const shapeGroup = r => ({
  id: r.id,
  recordId: r.record_id,
  field: r.field,
  stage: r.stage,
  position: r.position,
  label: r.label ?? null,
  originId: r.origin_id ?? null,
  actor: r.actor,
  createdAt: r.created_at,
  endedAt: r.ended_at ?? null,
});
const LOG_SELECT = 'SELECT l.*, u.username, u.display_name name FROM clutch_log l LEFT JOIN users u ON u.id = l.actor';
const shapeLog = r => ({
  id: r.id,
  recordId: r.record_id,
  clutch: r.clutch,
  stage: r.stage,
  field: r.field,
  day: r.day,
  kind: r.kind,
  before: r.before_text,
  after: r.after_text,
  note: r.note ?? null,
  actor: r.actor,
  username: r.username ?? null,
  name: r.name ?? null,
  stepId: r.step_id ?? null,
  createdAt: r.created_at,
});
const NOTE_SELECT =
  'SELECT n.*, u.username, u.display_name name, w.display_name updated_name FROM clutch_notes n LEFT JOIN users u ON u.id = n.actor LEFT JOIN users w ON w.id = n.updated_by';
const shapeNote = r => ({
  id: r.id,
  recordId: r.record_id,
  clutch: r.clutch,
  day: r.day,
  text: r.text,
  actor: r.actor,
  username: r.username ?? null,
  name: r.name ?? null,
  updatedBy: r.updated_by,
  updatedName: r.updated_name ?? null,
  createdAt: r.created_at,
  updatedAt: r.updated_at,
});

const cleanLabel = value => {
  if (value === undefined || value === null) return null;
  if (typeof value !== 'string') throw fail('INVALID_LABEL', 'Invalid group label');
  const text = value.trim().replace(/\s+/g, ' ');
  if (text.length > LABEL_MAX) throw fail('INVALID_LABEL', `The group label is too long (${LABEL_MAX} characters at most)`);
  return text || null;
};
const openGroups = (store, recordId, field) =>
  store.db.prepare('SELECT * FROM clutch_groups WHERE record_id = ? AND field = ? AND ended_at IS NULL ORDER BY position').all(recordId, field);
/** Runs `fn` in one transaction. */
function inTransaction(store, fn) {
  store.db.exec('BEGIN IMMEDIATE');
  try {
    const out = fn();
    store.db.exec('COMMIT');
    return out;
  } catch (e) {
    store.db.exec('ROLLBACK');
    throw e;
  }
}
const stepOut = (store, stepId, recordId) => ({
  id: stepId,
  events: store.db.prepare(`${EVENT_SELECT} WHERE e.step_id = ? ORDER BY e.created_at, e.rowid`).all(stepId).map(shapeEvent),
  groups: store.db.prepare(`${GROUP_SELECT} WHERE g.record_id = ? AND g.ended_at IS NULL ORDER BY g.field, g.position`).all(recordId).map(shapeGroup),
  log: store.db.prepare(`${LOG_SELECT} WHERE l.step_id = ? ORDER BY l.created_at`).all(stepId).map(shapeLog),
});

/**
 * One action on a clutch, recorded at once (only in the app): its events, the
 * groups of each count it touched as they are after it (`groups`: per count,
 * the whole list of open groups in the order of its parentheses, each an
 * existing `id` or a new one with a `key` the events can name, its label and
 * the earlier stage's group it came from), and history lines (`log`: a
 * regrouping or a formula edited by hand, before → after). The first time a
 * count gets groups, its events without one go to the first group (the terms
 * written before are wrapped in its parentheses).
 */
export function addClutchStep(store, body, user) {
  const prior = store.db.prepare('SELECT id, record_id FROM clutch_steps WHERE request_id = ?').get(body.requestId);
  if (prior) return { step: stepOut(store, prior.id, prior.record_id), duplicate: true };
  const record = stocksRecord(store, body.recordId);
  const groups = body.groups ?? [];
  const events = body.events ?? [];
  const log = body.log ?? [];
  if (![groups, events, log].every(Array.isArray) || groups.length > 6 || events.length > 60 || log.length > 6) throw fail('INVALID_STEP', 'Invalid step');
  if (!groups.length && !events.length && !log.length) throw fail('INVALID_STEP', 'Nothing to record');
  const stepId = randomUUID();
  const now = new Date().toISOString();
  const values = parse(record.values_json) || {};
  const clutch = clutchText(values['CLUTCH NUMBER']) || null;
  const stagedEntry = stagedEntryOf(store, body);
  return inTransaction(store, () => {
    const data = { events: [], log: [], fields: {}, created: [], adopted: [] };
    const keys = new Map();
    for (const g of groups) {
      const field = String(g?.field ?? '');
      const stage = STAGE_OF_FIELD[field];
      if (!stage || stage === 'adult') throw fail('INVALID_FIELD', 'Groups are kept for the eggs, larvae and pupae');
      if (data.fields[field]) throw fail('INVALID_STEP', 'A count named twice');
      if (!Array.isArray(g.list) || g.list.length > 26) throw fail('INVALID_GROUPS', 'Invalid groups');
      const before = openGroups(store, record.id, field);
      data.fields[field] = before.map(b => ({ id: b.id, position: b.position, label: b.label }));
      const kept = new Set();
      g.list.forEach((item, position) => {
        const label = cleanLabel(item?.label);
        if (item?.id) {
          const own = before.find(b => b.id === item.id);
          if (!own || kept.has(own.id)) throw fail('GROUP_NOT_FOUND', 'Group not found for this count', 404);
          kept.add(own.id);
          store.db.prepare('UPDATE clutch_groups SET position = ?, label = ? WHERE id = ?').run(position, label, own.id);
          return;
        }
        const key = item?.key ? String(item.key) : '';
        if (!key || keys.has(key)) throw fail('INVALID_GROUPS', 'A new group needs its own key');
        let origin = null;
        if (item.originId) {
          const o = store.db.prepare('SELECT id, stage FROM clutch_groups WHERE id = ? AND record_id = ?').get(String(item.originId), record.id);
          if (!o || o.stage !== STAGE_BEFORE[stage]) throw fail('GROUP_NOT_FOUND', 'The group it came from is not of this clutch', 404);
          origin = o.id;
        }
        const id = randomUUID();
        store.db
          .prepare('INSERT INTO clutch_groups(id,record_id,field,stage,position,label,origin_id,actor,created_at,step_id) VALUES(?,?,?,?,?,?,?,?,?,?)')
          .run(id, record.id, field, stage, position, label, origin, user.id, now, stepId);
        keys.set(key, id);
        data.created.push(id);
      });
      for (const b of before) if (!kept.has(b.id)) store.db.prepare('UPDATE clutch_groups SET ended_at = ? WHERE id = ?').run(now, b.id);
      // The terms written before the count had groups are the first group's now.
      const first = openGroups(store, record.id, field)[0];
      if (!before.length && first) {
        const loose = store.db.prepare('SELECT id FROM clutch_events WHERE record_id = ? AND stage = ? AND group_id IS NULL').all(record.id, stage).map(r => r.id);
        for (const id of loose) store.db.prepare('UPDATE clutch_events SET group_id = ? WHERE id = ?').run(first.id, id);
        data.adopted.push(...loose);
      }
    }
    const resolve = (id, key) => (key ? (keys.get(String(key)) ?? fail('GROUP_NOT_FOUND', 'Unknown group key', 404)) : (id ?? null));
    for (const e of events) {
      if (!e || typeof e !== 'object') throw fail('INVALID_STEP', 'Invalid event');
      const groupId = resolve(e.groupId, e.groupKey);
      const fromGroupId = resolve(e.fromGroupId, e.fromGroupKey);
      if (groupId instanceof Error) throw groupId;
      if (fromGroupId instanceof Error) throw fromGroupId;
      data.events.push(insertEvent(store, record, { ...e, groupId, fromGroupId, actionId: null }, user, { stepId, stagedEntry }));
    }
    for (const l of log) {
      if (!l || !LOG_KINDS.has(l.kind)) throw fail('INVALID_LOG', 'Invalid history line');
      const field = String(l.field ?? '');
      if (!isSumField(SHEET, field)) throw fail('INVALID_FIELD', 'Not a count');
      const text = v => (v === undefined || v === null ? null : String(v).slice(0, 2000));
      const id = randomUUID();
      store.db
        .prepare('INSERT INTO clutch_log(id,record_id,clutch,stage,field,day,kind,before_text,after_text,note,actor,step_id,created_at) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?)')
        .run(id, record.id, clutch, STAGE_OF_FIELD[field] ?? null, field, ecuadorDay(), l.kind, text(l.before), text(l.after), cleanNote(l.note), user.id, stepId, now);
      data.log.push(id);
    }
    store.db.prepare('INSERT INTO clutch_steps(id,request_id,record_id,actor,data_json,created_at) VALUES(?,?,?,?,?,?)').run(stepId, body.requestId, record.id, user.id, JSON.stringify(data), now);
    return { step: stepOut(store, stepId, record.id), duplicate: false };
  });
}

/**
 * Takes a step back as a whole: its events and history lines go, the groups of
 * each count it touched are as they were before it (the groups it made go,
 * their photos stay the day's). One's own, or anyone's for reviewers and admins.
 */
export function undoClutchStep(store, id, user) {
  const row = store.db.prepare('SELECT * FROM clutch_steps WHERE id = ?').get(String(id));
  if (!row) throw fail('STEP_NOT_FOUND', 'Step not found', 404);
  if (row.actor !== user.id && !['reviewer', 'admin'].includes(user.role)) throw fail('FORBIDDEN', 'Only your own steps', 403);
  const data = parse(row.data_json) || {};
  return inTransaction(store, () => {
    for (const eventId of data.events ?? []) {
      store.db.prepare('DELETE FROM clutch_events WHERE id = ?').run(eventId);
      store.db.prepare('UPDATE clutch_photos SET event_id = NULL WHERE event_id = ?').run(eventId);
      store.db.prepare('UPDATE clutch_events SET of_event_id = NULL WHERE of_event_id = ?').run(eventId);
    }
    for (const eventId of data.adopted ?? []) store.db.prepare('UPDATE clutch_events SET group_id = NULL WHERE id = ?').run(eventId);
    for (const [field, before] of Object.entries(data.fields ?? {})) {
      store.db.prepare('UPDATE clutch_groups SET ended_at = ? WHERE record_id = ? AND field = ? AND ended_at IS NULL').run(row.created_at, row.record_id, field);
      for (const g of before) store.db.prepare('UPDATE clutch_groups SET ended_at = NULL, position = ?, label = ? WHERE id = ?').run(g.position, g.label, g.id);
    }
    for (const groupId of data.created ?? []) {
      store.db.prepare('DELETE FROM clutch_groups WHERE id = ?').run(groupId);
      store.db.prepare('UPDATE clutch_photos SET group_id = NULL WHERE group_id = ?').run(groupId);
      store.db.prepare('UPDATE clutch_events SET group_id = NULL WHERE group_id = ?').run(groupId);
      store.db.prepare('UPDATE clutch_events SET from_group_id = NULL WHERE from_group_id = ?').run(groupId);
    }
    for (const logId of data.log ?? []) store.db.prepare('DELETE FROM clutch_log WHERE id = ?').run(logId);
    store.db.prepare('DELETE FROM clutch_steps WHERE id = ?').run(row.id);
    return { removed: row.id };
  });
}

/** A group's label changed (box A → plant 2). */
export function renameClutchGroup(store, id, body) {
  const row = store.db.prepare('SELECT * FROM clutch_groups WHERE id = ?').get(String(id));
  if (!row) throw fail('GROUP_NOT_FOUND', 'Group not found', 404);
  store.db.prepare('UPDATE clutch_groups SET label = ? WHERE id = ?').run(cleanLabel(body.label), row.id);
  return { group: shapeGroup(store.db.prepare(`${GROUP_SELECT} WHERE g.id = ?`).get(row.id)) };
}

/**
 * The day's note of a clutch (one per clutch and day, as long as needed; only
 * in the app: the sheet's NOTES keeps the short dated lines): written or
 * rewritten by anyone; an empty text takes it away.
 */
export function setClutchNote(store, body, user) {
  const record = stocksRecord(store, body.recordId);
  const day = eventDay(body.day);
  if (typeof body.text !== 'string') throw fail('INVALID_NOTE', 'Invalid note');
  const text = body.text.replace(/\r\n/g, '\n').trim();
  if (text.length > DAY_NOTE_MAX) throw fail('INVALID_NOTE', 'The note is too long');
  const prior = store.db.prepare('SELECT * FROM clutch_notes WHERE record_id = ? AND day = ?').get(record.id, day);
  const now = new Date().toISOString();
  if (!text) {
    if (prior) store.db.prepare('DELETE FROM clutch_notes WHERE id = ?').run(prior.id);
    return { note: null, removed: prior?.id ?? null };
  }
  if (prior) store.db.prepare('UPDATE clutch_notes SET text = ?, updated_by = ?, updated_at = ? WHERE id = ?').run(text, user.id, now, prior.id);
  else {
    const values = parse(record.values_json) || {};
    store.db
      .prepare('INSERT INTO clutch_notes(id,record_id,clutch,day,text,actor,created_at,updated_by,updated_at) VALUES(?,?,?,?,?,?,?,?,?)')
      .run(randomUUID(), record.id, clutchText(values['CLUTCH NUMBER']) || null, day, text, user.id, now, user.id, now);
  }
  return { note: shapeNote(store.db.prepare(`${NOTE_SELECT} WHERE n.record_id = ? AND n.day = ?`).get(record.id, day)) };
}
