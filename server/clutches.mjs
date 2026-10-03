// Clutches checked on a phone or tablet (the Clutches tab's cards). Several
// people go through the clutches in parallel, one with the paper notebook and
// others with the app; each clutch looked at is marked "checked" here, with the
// fields its check changed (or none), so everyone sees which clutches were
// checked today, by whom, and which changed. The changes themselves are written
// to the sheet by the usual save (records/batch) and read back from the history.
//
// A check lives only in the app (this table): it is never written to Google
// Sheets nor recorded as a save in the history, belongs to one day in Ecuador
// (the next day starts with none) and is seen by everyone.

import { randomUUID } from 'node:crypto';
import { isSumField, moduleMap, simpleSum } from './schema.mjs';

const SHEET = 'Insectary_stocks';
const fail = (code, message, status = 400) => Object.assign(new Error(message), { code, status });
const parse = text => (text ? JSON.parse(text) : null);

export function initClutches(db) {
  db.exec(`CREATE TABLE IF NOT EXISTS clutch_checks(id TEXT PRIMARY KEY, request_id TEXT UNIQUE, record_id TEXT NOT NULL,
      clutch TEXT, day TEXT NOT NULL, actor TEXT NOT NULL, fields_json TEXT NOT NULL, action_id TEXT, created_at TEXT NOT NULL);
    CREATE INDEX IF NOT EXISTS clutch_checks_day ON clutch_checks(day, record_id);`);
}

/** Today in Ecuador (the insectary's day), as an ISO date. */
export const ecuadorDay = (now = new Date()) => new Intl.DateTimeFormat('en-CA', { timeZone: 'America/Guayaquil' }).format(now);
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
 * what they add up to) and each clutch's last change in the history.
 */
export function clutchState(store) {
  const mod = moduleMap.get(SHEET);
  const sums = {};
  for (const r of store.db
    .prepare('SELECT id, formulas_json FROM records WHERE sheet=? AND missing=0 AND row_num>?')
    .all(SHEET, mod.headerRow)) {
    const formulas = parse(r.formulas_json) || {};
    for (const [field, formula] of Object.entries(formulas)) {
      const sum = isSumField(SHEET, field) ? simpleSum(formula) : null;
      if (sum) (sums[r.id] ??= {})[field] = sum;
    }
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
  return { sums, last };
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
  actionId: r.action_id,
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
  return { day, checks, changes };
}

/** Marks a clutch as checked today by this person, with the fields the check changed (none: "no change"). */
export function addClutchCheck(store, body, user) {
  const prior = store.db.prepare('SELECT k.*, u.username, u.display_name name FROM clutch_checks k LEFT JOIN users u ON u.id = k.actor WHERE k.request_id = ?').get(body.requestId);
  if (prior) return { check: shape(prior), duplicate: true };
  const record = store.db.prepare('SELECT id, sheet, values_json FROM records WHERE id = ? AND missing = 0').get(String(body.recordId || ''));
  if (!record || record.sheet !== SHEET) throw fail('RECORD_NOT_FOUND', 'Clutch not found', 404);
  const known = new Set(moduleMap.get(SHEET).fields.map(f => f.key));
  const fields = body.fields === undefined || body.fields === null ? [] : body.fields;
  if (!Array.isArray(fields) || fields.length > 40 || fields.some(f => typeof f !== 'string' || !known.has(f)))
    throw fail('INVALID_FIELDS', 'Invalid fields');
  const actionId = body.actionId ? String(body.actionId) : null;
  if (actionId && !store.db.prepare('SELECT 1 FROM actions WHERE id = ?').get(actionId)) throw fail('ACTION_NOT_FOUND', 'Save not found', 404);
  const values = parse(record.values_json) || {};
  const clutch = values['CLUTCH NUMBER'] === null || values['CLUTCH NUMBER'] === undefined ? null : String(values['CLUTCH NUMBER']);
  const id = randomUUID();
  store.db
    .prepare('INSERT INTO clutch_checks(id,request_id,record_id,clutch,day,actor,fields_json,action_id,created_at) VALUES(?,?,?,?,?,?,?,?,?)')
    .run(id, body.requestId, record.id, clutch, ecuadorDay(), user.id, JSON.stringify([...new Set(fields)]), actionId, new Date().toISOString());
  const row = store.db.prepare('SELECT k.*, u.username, u.display_name name FROM clutch_checks k LEFT JOIN users u ON u.id = k.actor WHERE k.id = ?').get(id);
  return { check: shape(row), duplicate: false };
}

/** Takes back a check marked by mistake: one's own, or anyone's for reviewers and admins. */
export function removeClutchCheck(store, id, user) {
  const row = store.db.prepare('SELECT * FROM clutch_checks WHERE id = ?').get(id);
  if (!row) throw fail('CHECK_NOT_FOUND', 'Check not found', 404);
  if (row.actor !== user.id && !['reviewer', 'admin'].includes(user.role)) throw fail('FORBIDDEN', 'Only your own checks', 403);
  store.db.prepare('DELETE FROM clutch_checks WHERE id = ?').run(id);
  return { removed: id };
}
