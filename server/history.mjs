// Historial: saves grouped by person, purpose and time, and their undo.
//
// Every action (one save) records its purpose: the tab or flow it came from
// (Colecta, Muertes, Tubos…). Older actions get one inferred from what they
// changed. A group is a run of actions by the same person with the same purpose
// where each follows the previous one by less than a time window (30 minutes;
// 2 minutes for changes read from Google Sheets, so each sync is its own group).
// A group's id is the id of its oldest action, so it does not change as the
// person keeps saving; any action id of a group finds the group.
//
// Syncs used to log the formulas of every row that moved in the sheet (rows
// inserted above shift their references: =M12963 → =M12972). The sync no longer
// does; those older cells stay in the database, marked changes.moved, and are
// left out here.

import { randomUUID } from 'node:crypto';
import { msg, msgn, tpl } from './messages.mjs';
import { comparable, moduleMap } from './schema.mjs';
import { formulaRowShift } from './sheets.mjs';
import { RESULT_BUDGET, fitList } from './tool-budget.mjs';

/** Purpose → label shown to people (Spanish, like the rest of the app). */
export const PURPOSES = {
  colecta: 'Colecta',
  monitoreo: 'Monitoreo',
  muertes: 'Muertes',
  emergidos: 'Emergidos',
  clutches: 'Clutches',
  censo: 'Censo',
  tubos: 'Tubos',
  tablas: 'Tablas',
  revision: 'Revisión',
  cambio_id: 'Cambio de ID',
  asistente: 'Asistente',
  deshacer: 'Deshacer',
  sheets: 'Google Sheets',
  importacion: 'Importación',
};

/** Purposes a client may declare for its own save (the tab it was made in). */
export const CLIENT_PURPOSES = new Set(['colecta', 'monitoreo', 'muertes', 'emergidos', 'clutches', 'censo', 'tubos', 'tablas', 'revision']);
export const cleanPurpose = value => (CLIENT_PURPOSES.has(value) ? value : null);

const MINUTE = 60_000;
/** The longest pause between two saves of one group. */
export const gapFor = purpose => (purpose === 'sheets' ? 2 * MINUTE : 30 * MINUTE);

const SOURCE_PURPOSE = { undo: 'deshacer', ai_approved: 'asistente', sheet_reconciliation: 'sheets', import: 'importacion' };
const REASON_PURPOSE = { death: 'muertes', preservation: 'muertes', collection: 'colecta', emergence: 'emergidos', tubes: 'tubos' };
/** Columns the Tubos tab writes: CAM, tubes, tissues, media, preservation. */
const TUBE_COLUMN = /^(CAM_ID|Tube_\d_(id|tissue)(_LEGS)?|T\d_Preservation_medium|Preservation_(medium|date)|Preserved_Dead_Alive|Location_body)$/;
const empty = value => value === null || value === undefined || value === '';

/**
 * The purpose of a save that did not declare one. `changes` are
 * { sheet, field, before, after, isNew, rowPurpose } (rowPurpose: the row's
 * Purpose column in Collection_data, when known).
 */
export function inferPurpose({ source, reason } = {}, changes = []) {
  if (SOURCE_PURPOSE[source]) return SOURCE_PURPOSE[source];
  const text = String(reason ?? '');
  if (/^(Cambiar|Intercambiar) Insectary ID/.test(text)) return 'cambio_id';
  const typed = /^(death|preservation|collection|emergence|tubes)\b/.exec(text)?.[1];
  if (typed) return REASON_PURPOSE[typed];
  if (!changes.length) return 'tablas';
  const sheets = new Set(changes.map(c => c.sheet));
  const fields = new Set(changes.map(c => c.field));
  const onlyTubes = [...fields].every(f => TUBE_COLUMN.test(f)) && !changes.some(c => c.isNew);
  if (sheets.has('Collection_data')) {
    const monitoring = changes.some(
      c =>
        c.sheet === 'Collection_data' &&
        (/^monitoring/i.test(String(c.rowPurpose ?? '')) || (c.field === 'Purpose' && /^monitoring/i.test(String(c.after ?? '')))),
    );
    if (monitoring) return 'monitoreo';
    return onlyTubes ? 'tubos' : 'colecta';
  }
  if (sheets.has('Insectary_data')) {
    if (fields.has('Death_date') || fields.has('Death_cause')) return 'muertes';
    const emerged = changes.some(
      c =>
        c.sheet === 'Insectary_data' &&
        (c.isNew || empty(c.before)) &&
        ['CLUTCH NUMBER', 'Intro2Insectary_date'].includes(c.field) &&
        (c.isNew || changes.some(o => o.sheet === 'Insectary_data' && o.field === 'Intro2Insectary_date' && empty(o.before))),
    );
    if (emerged) return 'emergidos';
  }
  if (onlyTubes) return 'tubos';
  if (sheets.size === 1 && sheets.has('Insectary_stocks')) return 'clutches';
  return 'tablas';
}

const parse = value => {
  if (value === null || value === undefined) return null;
  try {
    return JSON.parse(value);
  } catch {
    return null;
  }
};
/** SQL: the change `c` is not one of those formulas moved with their row (changes.moved). */
const unmoved = c => `${c}.moved = 0`;
const fail = (code, message, status = 400, details) => Object.assign(new Error(message), { code, status, details });

/**
 * Adds the purpose and moved columns (older databases), the indexes the
 * Historial needs, and purposes for older saves.
 */
export function initHistory(db) {
  const has = db
    .prepare('PRAGMA table_info(actions)')
    .all()
    .some(c => c.name === 'purpose');
  if (!has) db.exec('ALTER TABLE actions ADD COLUMN purpose TEXT');
  const moved = db
    .prepare('PRAGMA table_info(changes)')
    .all()
    .some(c => c.name === 'moved');
  if (!moved) {
    db.exec('ALTER TABLE changes ADD COLUMN moved INTEGER NOT NULL DEFAULT 0');
    markMovedFormulas(db);
  }
  db.exec(`CREATE INDEX IF NOT EXISTS changes_action ON changes(action_id);
    CREATE INDEX IF NOT EXISTS actions_created ON actions(created_at, id);
    CREATE INDEX IF NOT EXISTS actions_key ON actions(actor, purpose, created_at, id);`);
  backfillPurposes(db);
}

/**
 * Marks (changes.moved = 1) the formulas syncs logged only because their row
 * moved: the same formula with every relative row reference shifted by one
 * amount (formulaRowShift). Returns how many.
 */
export function markMovedFormulas(db) {
  db.function('formula_moved', { deterministic: true }, (before, after) =>
    formulaRowShift(parse(before)?.formula, parse(after)?.formula) ? 1 : 0,
  );
  return db
    .prepare(
      `UPDATE changes SET moved = 1 WHERE moved = 0 AND before_json LIKE '{"formula"%' AND after_json LIKE '{"formula"%'
         AND action_id IN (SELECT id FROM actions WHERE source = 'sheet_reconciliation') AND formula_moved(before_json, after_json)`,
    )
    .run().changes;
}

/** Infers and stores the purpose of every save that has none (once; later saves record theirs). */
export function backfillPurposes(db) {
  if (!db.prepare('SELECT 1 FROM actions WHERE purpose IS NULL LIMIT 1').get()) return 0;
  const rows = db
    .prepare(
      `SELECT a.id, a.source, a.reason, json_extract(a.result_json, '$.created') created,
        c.record_id, c.sheet, c.field, c.before_json, c.after_json,
        CASE WHEN c.sheet = 'Collection_data' THEN json_extract(r.values_json, '$.Purpose') END row_purpose
      FROM actions a LEFT JOIN changes c ON c.action_id = a.id LEFT JOIN records r ON r.id = c.record_id
      WHERE a.purpose IS NULL ORDER BY a.id`,
    )
    .all();
  const update = db.prepare('UPDATE actions SET purpose = ? WHERE id = ?');
  let count = 0;
  db.exec('BEGIN IMMEDIATE');
  try {
    for (let i = 0; i < rows.length; ) {
      const first = rows[i];
      const created = new Set((parse(first.created) ?? []).map(c => c.recordId));
      const changes = [];
      for (; i < rows.length && rows[i].id === first.id; i++) {
        const c = rows[i];
        if (c.field !== null)
          changes.push({
            sheet: c.sheet,
            field: c.field,
            before: parse(c.before_json),
            after: parse(c.after_json),
            isNew: created.has(c.record_id),
            rowPurpose: c.row_purpose,
          });
      }
      update.run(inferPurpose(first, changes), first.id);
      count++;
    }
    db.exec('COMMIT');
  } catch (e) {
    db.exec('ROLLBACK');
    throw e;
  }
  return count;
}

const likeText = text => `%${String(text).replace(/[\\%_]/g, m => '\\' + m)}%`;

/** The ids a person filter names: the user id itself, or users whose name or username contains it. */
function actorIds(db, text) {
  const value = String(text).trim();
  const like = likeText(value);
  const ids = db
    .prepare("SELECT id FROM users WHERE id = ? OR username LIKE ? ESCAPE '\\' OR display_name LIKE ? ESCAPE '\\'")
    .all(value, like, like)
    .map(r => r.id);
  return [...new Set([value, ...ids])];
}

/** Which actions match the sheet / text / record filters (`has(id)`), or null when there are none. */
function matchingActions(db, { sheet, text, recordId }) {
  if (!sheet && !text && !recordId) return null;
  if (!text && !recordId) {
    // Only a sheet: most saves match, so each one is checked when its group comes up (by index).
    const touches = db.prepare(`SELECT 1 FROM changes c WHERE action_id = ? AND sheet = ? AND ${unmoved('c')} LIMIT 1`);
    const known = new Map();
    return {
      has: id => known.get(id) ?? known.set(id, !!touches.get(id, String(sheet))).get(id),
    };
  }
  const clauses = [unmoved('c')];
  const args = [];
  if (sheet) {
    clauses.push('sheet = ?');
    args.push(String(sheet));
  }
  if (recordId) {
    clauses.push('record_id = ?');
    args.push(String(recordId));
  }
  if (text) {
    const like = likeText(text);
    // Labels are matched in the (small) records table first, not once per change.
    clauses.push(
      "(record_id IN (SELECT id FROM records WHERE label LIKE ? ESCAPE '\\') OR field LIKE ? ESCAPE '\\' OR before_json LIKE ? ESCAPE '\\' OR after_json LIKE ? ESCAPE '\\')",
    );
    args.push(like, like, like, like);
  }
  const ids = new Set(db.prepare(`SELECT action_id FROM changes c WHERE ${clauses.join(' AND ')}`).all(...args).map(r => r.action_id));
  // A note of the save (its reason) matches too.
  if (text && !sheet && !recordId)
    for (const r of db.prepare("SELECT id FROM actions WHERE reason LIKE ? ESCAPE '\\'").all(likeText(text))) ids.add(r.id);
  return ids;
}

const dayStart = iso => (/^\d{4}-\d{2}-\d{2}$/.test(String(iso)) ? Date.parse(`${iso}T00:00:00-05:00`) : NaN);
const dayEnd = iso => (/^\d{4}-\d{2}-\d{2}$/.test(String(iso)) ? Date.parse(`${iso}T23:59:59.999-05:00`) : NaN);

/**
 * Walks the saves newest first and yields the groups in order (newest save
 * first). Only the person and purpose filters narrow the walk: they never split
 * a group. `stopBefore` (ms): no group starting before it is needed.
 */
function* scanGroups(db, { actors, purpose, stopBefore } = {}) {
  const clauses = [];
  const args = [];
  if (actors) {
    clauses.push(`actor IN (${actors.map(() => '?').join(',')})`);
    args.push(...actors);
  }
  if (purpose) {
    clauses.push('purpose = ?');
    args.push(purpose);
  }
  const rows = db
    .prepare(
      `SELECT id, actor, purpose, source, status, reason, created_at FROM actions ${clauses.length ? 'WHERE ' + clauses.join(' AND ') : ''} ORDER BY created_at DESC, id DESC`,
    )
    .iterate(...args);
  const open = new Map();
  const queue = [];
  for (const row of rows) {
    const t = Date.parse(row.created_at);
    // Groups whose next save could not be this old any more are complete.
    while (queue.length && queue[0].oldestT - t >= gapFor(queue[0].purpose)) {
      const done = queue.shift();
      if (open.get(done.key) === done) open.delete(done.key);
      yield done;
    }
    const key = `${row.actor}\u0000${row.purpose}`;
    const current = open.get(key);
    const action = { id: row.id, createdAt: row.created_at, status: row.status, source: row.source, reason: row.reason };
    if (current && current.oldestT - t < gapFor(current.purpose)) {
      current.actions.push(action);
      current.oldestT = t;
      continue;
    }
    if (stopBefore !== undefined && t < stopBefore) {
      // Older saves only matter to groups already started.
      if (!queue.length) return;
      continue;
    }
    const group = { key, actor: row.actor, purpose: row.purpose, actions: [action], newestT: t, oldestT: t };
    open.set(key, group);
    queue.push(group);
  }
  yield* queue;
}

/**
 * Groups of saves, newest first, with filters: user (id, username or name),
 * purpose, sheet, from/to (YYYY-MM-DD, days in Ecuador), text (identifier,
 * field, value or note), recordId. A group matches when any of its saves does.
 * `until`: also load down to the group holding this group or action id.
 */
export function historyGroups(store, query = {}) {
  const db = store.db;
  const limit = Math.min(Math.max(Number(query.limit) || 20, 1), 100);
  const offset = Math.max(Number(query.offset) || 0, 0);
  const person = query.user ?? query.actor;
  const actors = person ? actorIds(db, person) : null;
  const purpose = query.purpose ? String(query.purpose) : null;
  const text = query.text ?? query.q;
  const matches = matchingActions(db, { sheet: query.sheet, text, recordId: query.recordId });
  const from = query.from ? dayStart(query.from) : NaN;
  const to = query.to ? dayEnd(query.to) : NaN;
  const inRange = a => {
    const t = Date.parse(a.createdAt);
    return !(t < from) && !(t > to);
  };
  const until = query.until ? String(query.until) : null;
  let found = !until;
  const out = [];
  let seen = 0;
  let more = false;
  for (const scanned of scanGroups(db, { actors, purpose, stopBefore: Number.isNaN(from) ? undefined : from })) {
    if (matches && !scanned.actions.some(a => matches.has(a.id))) continue;
    const group = withoutMoves(db, scanned);
    if (!group) {
      if (!found && scanned.actions.some(a => a.id === until)) found = true;
      continue;
    }
    if ((query.from || query.to) && !group.actions.some(inRange)) continue;
    if (seen++ < offset) continue;
    if (out.length >= limit && found) {
      more = true;
      break;
    }
    // Down to the linked group, at most 1000 groups.
    if (out.length >= 1000) {
      more = true;
      break;
    }
    out.push(group);
    if (!found && scanned.actions.some(a => a.id === until)) found = true;
  }
  const groups = describeGroups(store, out, matches);
  return { groups, offset, limit, next: more ? offset + out.length : null, ...(until ? { found } : {}) };
}

/**
 * The group holding a group or action id: the run of saves by that person and
 * purpose around it. `single`: only that save.
 */
export function findGroup(db, id, { single = false } = {}) {
  const action = db.prepare('SELECT id, actor, purpose, source, status, reason, created_at FROM actions WHERE id = ?').get(String(id ?? ''));
  if (!action) return null;
  const gap = gapFor(action.purpose);
  const pick = r => ({ id: r.id, createdAt: r.created_at, status: r.status, source: r.source, reason: r.reason });
  const newer = db.prepare(
    `SELECT id, source, status, reason, created_at FROM actions WHERE actor = ? AND purpose IS ? AND (created_at > ? OR (created_at = ? AND id > ?)) ORDER BY created_at, id`,
  );
  const older = db.prepare(
    `SELECT id, source, status, reason, created_at FROM actions WHERE actor = ? AND purpose IS ? AND (created_at < ? OR (created_at = ? AND id < ?)) ORDER BY created_at DESC, id DESC`,
  );
  const actions = [pick(action)];
  const key = `${action.actor}\u0000${action.purpose}`;
  if (single) return { key, actor: action.actor, purpose: action.purpose, actions, single: true };
  let last = Date.parse(action.created_at);
  for (const r of newer.iterate(action.actor, action.purpose, action.created_at, action.created_at, action.id)) {
    const t = Date.parse(r.created_at);
    if (t - last >= gap) break;
    actions.unshift(pick(r));
    last = t;
  }
  last = Date.parse(action.created_at);
  for (const r of older.iterate(action.actor, action.purpose, action.created_at, action.created_at, action.id)) {
    const t = Date.parse(r.created_at);
    if (last - t >= gap) break;
    actions.push(pick(r));
    last = t;
  }
  return {
    key,
    actor: action.actor,
    purpose: action.purpose,
    actions,
    newestT: Date.parse(actions[0].createdAt),
    oldestT: Date.parse(actions.at(-1).createdAt),
  };
}

/**
 * A sync's group without the saves that only moved formulas with their rows
 * (null when that was all of it); it keeps the id of the whole group.
 */
function withoutMoves(db, group) {
  if (group.purpose !== 'sheets') return group;
  const ids = group.actions.map(a => a.id);
  const kept = new Set();
  for (const part of chunks(ids))
    for (const r of db
      .prepare(`SELECT DISTINCT action_id FROM changes c WHERE action_id IN (${marks(part)}) AND ${unmoved('c')}`)
      .all(...part))
      kept.add(r.action_id);
  if (kept.size === ids.length) return group;
  if (!kept.size) return null;
  return { ...group, id: group.id ?? ids.at(-1), actions: group.actions.filter(a => kept.has(a.id)) };
}

/** Identifier columns of every sheet (Insectary_ID, CAM_ID, CLUTCH NUMBER…), for labels of rows that no longer have them. */
const IDENTITY = [...new Set([...moduleMap.values()].flatMap(m => m.identityFields ?? []))];
const IDENTITY_SQL = IDENTITY.map(f => `'${f.replace(/'/g, "''")}'`).join(',') || "''";
/** A row's label: its current one, else the identifier it had in these changes (a new row undone since), else sheet and row. */
function rowLabel(label, sheet, row, identity) {
  if (label && label !== `${sheet} record`) return label;
  if (identity !== null && identity !== undefined && typeof identity !== 'object' && String(identity).trim()) return String(identity);
  return `${sheet} fila ${row}`;
}

const chunks = (list, size = 500) => Array.from({ length: Math.ceil(list.length / size) }, (_, i) => list.slice(i * size, i * size + size));
const marks = list => list.map(() => '?').join(',');

/**
 * Which saves were undone, and which of their cells: undo actions that are
 * themselves undone (a redo) do not count.
 */
function undoneIndex(db, actionIds) {
  const wanted = new Set(actionIds);
  const undos = db
    .prepare("SELECT id, reverses FROM actions WHERE source = 'undo' AND status = 'verified' AND reverses IS NOT NULL")
    .all()
    .map(r => ({ id: r.id, reverses: r.reverses.split(',') }));
  const reversedUndos = new Set(undos.flatMap(u => u.reverses));
  const active = undos.filter(u => !reversedUndos.has(u.id) && u.reverses.some(id => wanted.has(id)));
  const byAction = new Map();
  const cells = new Map();
  for (const part of chunks(active.map(u => u.id)))
    for (const c of db.prepare(`SELECT action_id, record_id, field FROM changes WHERE action_id IN (${marks(part)})`).all(...part))
      cells.set(c.action_id, [...(cells.get(c.action_id) ?? []), `${c.record_id}\u0000${c.field}`]);
  for (const u of active)
    for (const id of u.reverses)
      if (wanted.has(id)) {
        const entry = byAction.get(id) ?? { by: u.id, cells: new Set() };
        for (const cell of cells.get(u.id) ?? []) entry.cells.add(cell);
        byAction.set(id, entry);
      }
  return byAction;
}

const UNDOABLE = new Set(['verified', 'observed']);

function names(db, ids) {
  const map = new Map();
  for (const part of chunks([...new Set(ids)]))
    for (const r of db.prepare(`SELECT id, display_name FROM users WHERE id IN (${marks(part)})`).all(...part)) map.set(r.id, r.display_name);
  return map;
}

/** Created rows of each action (result_json.created), without parsing the rows it stores. */
function createdRows(db, actionIds) {
  const map = new Map();
  for (const part of chunks(actionIds))
    for (const r of db.prepare(`SELECT id, json_extract(result_json, '$.created') created FROM actions WHERE id IN (${marks(part)})`).all(...part))
      map.set(r.id, new Set((parse(r.created) ?? []).map(c => c.recordId)));
  return map;
}

/** Summaries of groups: counts, rows, a short text; one query per group for its rows. */
function describeGroups(store, groups, matches = null) {
  const db = store.db;
  const ids = groups.flatMap(g => g.actions.map(a => a.id));
  const people = names(db, groups.map(g => g.actor));
  const undone = undoneIndex(db, ids);
  const created = createdRows(db, ids);
  const perRecord = new Map();
  return groups.map(group => {
    const actionIds = group.actions.map(a => a.id);
    const written = group.actions.filter(a => a.status !== 'failed').map(a => a.id);
    const key = marks(written);
    // Formulas moved with their rows are left out.
    const keep = ` AND ${unmoved('c')}`;
    const statement =
      perRecord.get(written.length) ??
      perRecord
        .set(
          written.length,
          written.length
            ? db.prepare(
                `SELECT c.record_id, min(c.sheet) sheet, min(c.row_num) row_num, r.label, count(*) cells, min(c.rowid) first,
                   max(CASE WHEN c.field IN (${IDENTITY_SQL}) THEN coalesce(nullif(c.after_json, 'null'), c.before_json) END) identity
                 FROM changes c LEFT JOIN records r ON r.id = c.record_id WHERE c.action_id IN (${key})${keep} GROUP BY c.record_id ORDER BY first`,
              )
            : null,
        )
        .get(written.length);
    const rows = statement ? statement.all(...written) : [];
    const fields = written.length
      ? db.prepare(`SELECT DISTINCT field FROM changes c WHERE action_id IN (${key})${keep}`).all(...written).map(r => r.field)
      : [];
    const newRows = new Set(written.flatMap(id => [...(created.get(id) ?? [])]));
    const labels = rows.map(r => rowLabel(r.label, r.sheet, r.row_num, parse(r.identity)));
    const statuses = {};
    for (const a of group.actions) statuses[a.status] = (statuses[a.status] ?? 0) + 1;
    const undoable = group.actions.filter(a => UNDOABLE.has(a.status));
    // Cells of confirmed saves, and how many of them a later undo put back.
    const undoableIds = undoable.map(a => a.id);
    const undoableCells = undoableIds.length
      ? db.prepare(`SELECT count(*) n FROM changes c WHERE action_id IN (${marks(undoableIds)})${keep}`).get(...undoableIds).n
      : 0;
    let undoneCells = 0;
    for (const a of undoable.filter(a => undone.has(a.id))) {
      const cells = undone.get(a.id).cells;
      for (const c of db.prepare(`SELECT record_id, field FROM changes c WHERE action_id = ?${keep}`).all(a.id))
        if (cells.has(`${c.record_id}\u0000${c.field}`)) undoneCells++;
    }
    const counts = {
      actions: group.actions.length,
      rows: rows.length,
      newRows: rows.filter(r => newRows.has(r.record_id)).length,
      cells: rows.reduce((n, r) => n + r.cells, 0),
    };
    const id = group.id ?? actionIds.at(-1);
    const summary = summaryMessage({ purpose: group.purpose, ...counts, labels });
    return {
      id,
      purpose: group.purpose,
      purposeLabel: PURPOSES[group.purpose] ?? group.purpose ?? 'Otro',
      actor: group.actor,
      actorName: group.actor === 'unknown' ? null : (people.get(group.actor) ?? null),
      start: group.actions.at(-1).createdAt,
      end: group.actions[0].createdAt,
      counts,
      sheets: [...new Set(rows.map(r => r.sheet))],
      fields: fields.slice(0, 20),
      labels: compressLabels(labels).slice(0, 12),
      summary: summary.text,
      summaryMsg: summary.msg,
      reasons: [...new Set(group.actions.map(a => a.reason).filter(Boolean))].slice(0, 3),
      statuses,
      undone: undoneCells ? (undoneCells >= undoableCells ? 'all' : 'some') : null,
      undoable: undoableCells > undoneCells,
      actionIds,
      ...(matches ? { matched: actionIds.filter(a => matches.has(a)) } : {}),
      link: group.single ? `#/historial?accion=${id}` : `#/historial?grupo=${id}`,
    };
  });
}

/**
 * "A0D–A8D, CAM079891–CAM079895, 12B": labels that differ only in their
 * last number and follow each other become a range.
 */
export function compressLabels(labels) {
  const unique = [...new Set(labels.filter(Boolean).map(String))].sort((a, b) =>
    a.localeCompare(b, 'en', { numeric: true }),
  );
  const out = [];
  let run = null;
  const flush = () => {
    if (run) out.push(run.first === run.last ? run.first : run.count === 2 ? `${run.first}, ${run.last}` : `${run.first}–${run.last}`);
    run = null;
  };
  for (const label of unique) {
    const m = /^(.*?)(\d+)(\D*)$/.exec(label);
    const part = m && { head: m[1], n: Number(m[2]), width: m[2].length, tail: m[3] };
    if (run && part && run.head === part.head && run.tail === part.tail && run.width === part.width && part.n === run.n + 1) {
      Object.assign(run, { n: part.n, last: label, count: run.count + 1 });
      continue;
    }
    flush();
    run = part ? { ...part, first: label, last: label, count: 1 } : { head: null, first: label, last: label, count: 1 };
  }
  flush();
  return out;
}

const NEW_NOUN = {
  colecta: [tpl('{n} mariposa de colecta'), tpl('{n} mariposas de colecta')],
  monitoreo: [tpl('{n} captura de monitoreo'), tpl('{n} capturas de monitoreo')],
  emergidos: [tpl('{n} emergido'), tpl('{n} emergidos')],
  clutches: [tpl('{n} clutch nuevo'), tpl('{n} clutches nuevos')],
};
const EDIT_NOUN = {
  muertes: [tpl('{n} muerte registrada'), tpl('{n} muertes registradas')],
  censo: [tpl('{n} desaparecida en un censo'), tpl('{n} desaparecidas en un censo')],
  tubos: [tpl('{n} mariposa con tubos o CAM'), tpl('{n} mariposas con tubos o CAM')],
  clutches: [tpl('{n} clutch actualizado'), tpl('{n} clutches actualizados')],
  sheets: [tpl('{n} fila cambiada en Google Sheets'), tpl('{n} filas cambiadas en Google Sheets')],
  deshacer: [tpl('{n} fila restaurada'), tpl('{n} filas restauradas')],
  asistente: [tpl('{n} fila escrita por el asistente'), tpl('{n} filas escritas por el asistente')],
  cambio_id: [tpl('{n} fila con el ID cambiado'), tpl('{n} filas con el ID cambiado')],
};

/**
 * "12 mariposas de colecta: A0D–A8D, CAM079891–CAM079902", with its
 * descriptor for the interface's language (server/messages.mjs).
 */
export function summaryMessage({ purpose, rows = 0, newRows = 0, labels = [] }) {
  const edited = rows - newRows;
  let head;
  if (!rows) head = msg('Sin cambios guardados');
  else if (newRows && edited)
    head = msg('{new} y {edited}', {
      new: msgn(newRows, '{n} fila nueva', '{n} filas nuevas'),
      edited: msgn(edited, '{n} editada', '{n} editadas'),
    });
  else if (newRows) head = msgn(newRows, ...(NEW_NOUN[purpose] ?? [tpl('{n} fila nueva'), tpl('{n} filas nuevas')]));
  else head = msgn(edited, ...(EDIT_NOUN[purpose] ?? [tpl('{n} fila editada'), tpl('{n} filas editadas')]));
  const items = compressLabels(labels);
  if (!items.length) return head;
  if (items.length <= 6) return msg('{head}: {items}', { head, items });
  return msg('{head}: {items} y {more} más', { head, items: items.slice(0, 6), more: items.length - 6 });
}
export const summaryText = group => summaryMessage(group).text;

/**
 * One group with its saves and every change (sheet, row, record label, field,
 * before → after). `single`: only the save with this id.
 */
export function historyGroup(store, id, { single = false } = {}) {
  const db = store.db;
  const found = findGroup(db, id, { single });
  if (!found) throw fail('GROUP_NOT_FOUND', 'No se encontró ese guardado en el historial', 404);
  // A sync that only moved formulas: its oldest save, with no changes.
  const oldest = found.actions.at(-1);
  const group = withoutMoves(db, found) ?? { ...found, id: oldest.id, actions: [oldest], movedOnly: true };
  const [summary] = describeGroups(store, [group]);
  const ids = group.actions.map(a => a.id);
  const undone = undoneIndex(db, ids);
  const created = createdRows(db, ids);
  const changes = new Map();
  for (const part of chunks(ids))
    for (const c of db
      .prepare(
        `SELECT c.id, c.action_id, c.record_id, c.sheet, c.row_num, c.field, c.before_json, c.after_json, r.label
         FROM changes c LEFT JOIN records r ON r.id = c.record_id WHERE c.action_id IN (${marks(part)}) AND ${unmoved('c')}
         ORDER BY c.rowid`,
      )
      .all(...part)) {
      const list = changes.get(c.action_id) ?? changes.set(c.action_id, []).get(c.action_id);
      list.push({
        id: c.id,
        recordId: c.record_id,
        label: c.label ?? null,
        sheet: c.sheet,
        row: c.row_num,
        field: c.field,
        before: parse(c.before_json),
        after: parse(c.after_json),
        isNew: created.get(c.action_id)?.has(c.record_id) ?? false,
        undone: undone.get(c.action_id)?.cells.has(`${c.record_id}\u0000${c.field}`) ?? false,
      });
    }
  // Rows without their identifier now (a new row undone since): the identifier they had in these changes.
  const identities = new Map();
  for (const list of changes.values())
    for (const c of list)
      if (IDENTITY.includes(c.field) && !identities.has(c.recordId)) {
        const value = c.after ?? c.before;
        if (value !== null && typeof value !== 'object' && String(value).trim()) identities.set(c.recordId, value);
      }
  for (const list of changes.values()) for (const c of list) c.label = rowLabel(c.label, c.sheet, c.row, identities.get(c.recordId));
  const reverses = new Map(
    ids.length
      ? db
          .prepare(`SELECT id, reverses FROM actions WHERE id IN (${marks(ids)})`)
          .all(...ids)
          .map(r => [r.id, r.reverses])
      : [],
  );
  return {
    ...summary,
    ...(group.movedOnly ? { movedOnly: true } : {}),
    actions: group.actions.map(a => ({
      id: a.id,
      createdAt: a.createdAt,
      source: a.source,
      status: a.status,
      reason: a.reason,
      reverses: reverses.get(a.id) ?? null,
      reversedBy: undone.get(a.id)?.by ?? null,
      undoable: UNDOABLE.has(a.status),
      changes: changes.get(a.id) ?? [],
    })),
  };
}

/**
 * What to undo: whole groups, saves and/or single changes. Changes already
 * undone are left out, so "Deshacer todo" on a partly undone group undoes the rest.
 */
export function undoSelection(store, { groupIds, actionIds, changeIds } = {}) {
  const db = store.db;
  const actions = new Set(Array.isArray(actionIds) ? actionIds.map(String) : []);
  for (const gid of Array.isArray(groupIds) ? groupIds : []) {
    const group = findGroup(db, gid);
    if (!group) throw fail('GROUP_NOT_FOUND', 'No se encontró ese guardado en el historial', 404);
    for (const a of group.actions) if (UNDOABLE.has(a.status)) actions.add(a.id);
  }
  // A single change names its save.
  const picked = Array.isArray(changeIds) ? changeIds.map(String) : null;
  if (picked?.length)
    for (const part of chunks(picked))
      for (const r of db.prepare(`SELECT DISTINCT action_id FROM changes WHERE id IN (${marks(part)})`).all(...part)) actions.add(r.action_id);
  const ids = [...actions];
  if (!ids.length) throw fail('INVALID_SELECTION', 'Elige al menos un cambio para deshacer');
  for (const part of chunks(ids)) {
    const rows = db.prepare(`SELECT id, status FROM actions WHERE id IN (${marks(part)})`).all(...part);
    if (rows.length !== part.length) throw fail('INVALID_SELECTION', 'Un guardado elegido no existe', 404);
    if (rows.some(r => !UNDOABLE.has(r.status)))
      throw fail('INVALID_SELECTION', 'Solo se deshacen guardados confirmados en Google Sheets', 409);
  }
  const undone = undoneIndex(db, ids);
  const wanted = picked ? new Set(picked) : null;
  const changes = [];
  const holding = new Set();
  // Formulas a sync logged only because their row moved are not undone with their save: their old references are wrong now.
  for (const part of chunks(ids))
    for (const c of db
      .prepare(`SELECT id, action_id, record_id, field, moved FROM changes c WHERE action_id IN (${marks(part)}) ORDER BY rowid`)
      .all(...part))
      if ((wanted ? wanted.has(c.id) : !c.moved) && !undone.get(c.action_id)?.cells.has(`${c.record_id}\u0000${c.field}`)) {
        changes.push(c.id);
        holding.add(c.action_id);
      }
  if (!changes.length) throw fail('NOTHING_TO_UNDO', 'Esos cambios ya están deshechos', 409);
  return { actionIds: ids.filter(id => holding.has(id)), changeIds: changes };
}

/** Previews an undo of groups, saves or changes (history/preview). */
export function previewEdits(store, body = {}) {
  const selection = undoSelection(store, body);
  return { ...describePreview(store, store.previewUndo(selection)), selection };
}

/**
 * Undoes groups, saves or changes as one new save (source undo, purpose
 * deshacer), which can itself be undone. A retried request returns its first outcome.
 */
export async function undoEdits(store, body = {}, user) {
  store.validateRole(user);
  const prior = typeof body.requestId === 'string' ? store.actionByRequest(body.requestId) : null;
  if (prior && prior.status !== 'failed') return store.undo({ requestId: body.requestId }, user);
  const selection = undoSelection(store, body);
  return store.undo({ ...selection, requestId: body.requestId, reason: body.reason }, user);
}

/** Undo preview in words for people and the assistant: row label, field, value now → value after undoing. */
export function describePreview(store, preview) {
  const labelOf = new Map();
  const label = id => {
    if (!labelOf.has(id)) {
      const record = store.getRecord(id);
      labelOf.set(id, record ? { label: record.label, sheet: record.sheet, row: record.row } : { label: null, sheet: null, row: null });
    }
    return labelOf.get(id);
  };
  const item = c => ({ ...c, ...label(c.recordId) });
  return { ...preview, changes: preview.changes.map(item), conflicts: preview.conflicts.map(item) };
}

const ROW = 'SELECT id, sheet, row_num, label, missing FROM records';
const SORT = 'ORDER BY missing, sheet, row_num';

/**
 * The rows a name points to, best first: rows labelled so, rows with it in an
 * identifier column, rows gone from the sheet, the same ignoring case, rows that
 * had it as identifier before. `rows`: the first of these that finds any; `others`: the rest.
 */
function rowsNamed(db, name) {
  const value = String(name ?? '').trim();
  if (!value) return { rows: [], others: [] };
  // One pass over the records (a scan): labels in any case, and identifier values.
  const found = db
    .prepare(
      `${ROW} r WHERE label = ? COLLATE NOCASE
         OR (identity_json LIKE ? ESCAPE '\\' AND EXISTS (SELECT 1 FROM json_each(r.identity_json) j WHERE CAST(j.value AS TEXT) = ?)) ${SORT}`,
    )
    .all(value, likeText(value), value);
  const byLabel = found.filter(r => r.label === value);
  const byIdentity = found.filter(r => r.label !== value && r.label.toLowerCase() !== value.toLowerCase());
  const anyCase = found.filter(r => r.label !== value && r.label.toLowerCase() === value.toLowerCase());
  const live = list => list.filter(r => !r.missing);
  const formerly = [];
  const known = new Set(found.map(r => r.id));
  const values = [JSON.stringify(value), ...(/^\d+$/.test(value) ? [value] : [])];
  const past = db
    .prepare(
      `SELECT DISTINCT record_id FROM changes WHERE field IN (${IDENTITY_SQL}) AND (before_json IN (${marks(values)}) OR after_json IN (${marks(values)}))`,
    )
    .all(...values, ...values)
    .map(r => r.record_id)
    .filter(id => !known.has(id));
  for (const id of past) {
    const r = db.prepare(`${ROW} WHERE id = ?`).get(id);
    if (r) formerly.push({ ...r, formerly: value });
  }
  const tiers = [
    live(byLabel),
    live(byIdentity),
    [...byLabel, ...byIdentity].filter(r => r.missing),
    anyCase,
    formerly,
  ];
  const rows = tiers.find(t => t.length) ?? [];
  const chosen = new Set(rows.map(r => r.id));
  const others = [...new Map(tiers.flat().filter(r => !chosen.has(r.id)).map(r => [r.id, r])).values()];
  return { rows, others };
}

/**
 * Every change to one row, oldest first, one entry per save (formulas moved
 * with their row left out). The row: `recordId`, or `id` (a label or
 * identifier; several rows with it: { rows } to pick from). Filters: fields,
 * from/to (YYYY-MM-DD, days in Ecuador); pages of `limit` saves.
 */
export function recordHistory(store, query = {}) {
  const db = store.db;
  const brief = r => ({
    recordId: r.id,
    sheet: r.sheet,
    row: r.row_num,
    label: r.label,
    ...(r.missing ? { deleted: true } : {}),
    ...(r.formerly ? { formerly: r.formerly } : {}),
  });
  let record;
  let others = [];
  if (query.recordId) {
    const id = String(query.recordId);
    record =
      db.prepare(`${ROW} WHERE id = ?`).get(id) ??
      db.prepare('SELECT record_id id, sheet, row_num, NULL label, 1 missing FROM changes WHERE record_id = ? ORDER BY rowid DESC LIMIT 1').get(id);
  } else {
    const named = rowsNamed(db, query.id);
    if (named.rows.length > 1) return { rows: named.rows.map(brief), others: named.others.slice(0, 10).map(brief) };
    [record] = named.rows;
    others = named.others;
  }
  if (!record) throw fail('RECORD_NOT_FOUND', `No row found for ${String(query.recordId ?? query.id ?? '').slice(0, 80)}`, 404);
  const clauses = ['c.record_id = ?', unmoved('c')];
  const args = [record.id];
  const fields = (Array.isArray(query.fields) ? query.fields : query.fields ? [query.fields] : []).map(String).filter(Boolean);
  if (fields.length) {
    clauses.push(`c.field IN (${marks(fields)})`);
    args.push(...fields);
  }
  const from = query.from ? dayStart(query.from) : NaN;
  const to = query.to ? dayEnd(query.to) : NaN;
  if (!Number.isNaN(from)) {
    clauses.push('a.created_at >= ?');
    args.push(new Date(from).toISOString());
  }
  if (!Number.isNaN(to)) {
    clauses.push('a.created_at <= ?');
    args.push(new Date(to).toISOString());
  }
  const rows = db
    .prepare(
      `SELECT c.action_id, c.sheet, c.field, c.before_json, c.after_json, a.created_at, a.actor, a.purpose, a.source, a.status, a.reason
       FROM changes c JOIN actions a ON a.id = c.action_id WHERE ${clauses.join(' AND ')} ORDER BY a.created_at, a.id, c.rowid`,
    )
    .all(...args);
  const saves = [];
  for (const r of rows) {
    if (saves.at(-1)?.actionId !== r.action_id)
      saves.push({
        actionId: r.action_id,
        createdAt: r.created_at,
        actor: r.actor,
        purpose: r.purpose,
        source: r.source,
        status: r.status,
        reason: r.reason,
        cells: [],
      });
    saves.at(-1).cells.push({ sheet: r.sheet, field: r.field, before: parse(r.before_json), after: parse(r.after_json) });
  }
  const limit = Math.min(Math.max(Number(query.limit) || 100, 1), 500);
  const offset = Math.max(Number(query.offset) || 0, 0);
  const page = saves.slice(offset, offset + limit);
  const ids = page.map(s => s.actionId);
  const people = names(db, page.map(s => s.actor));
  const undone = undoneIndex(db, ids);
  const created = createdRows(db, ids);
  for (const save of page) {
    save.actorName = save.actor === 'unknown' ? null : (people.get(save.actor) ?? null);
    for (const c of save.cells) {
      if (created.get(save.actionId)?.has(record.id)) c.isNew = true;
      if (undone.get(save.actionId)?.cells.has(`${record.id}\u0000${c.field}`)) c.undone = true;
    }
  }
  return {
    row: brief(record),
    saves: page,
    total: saves.length,
    next: offset + limit < saves.length ? offset + limit : null,
    ...(others.length ? { others: others.slice(0, 10).map(brief) } : {}),
  };
}

/** Saves of one person, purpose and source less than this apart show as one edit in a cell's history. */
export const EDIT_GAP = 10 * MINUTE;

/**
 * When the change log starts: the first save, and the first edit read from
 * Google Sheets (edits typed there before it are not known).
 */
function logStart(db) {
  const first = db.prepare('SELECT created_at t FROM actions ORDER BY created_at LIMIT 1').get();
  const sheets = db.prepare("SELECT created_at t FROM actions WHERE source = 'sheet_reconciliation' ORDER BY created_at LIMIT 1").get();
  return { since: first?.t ?? null, sheetsSince: sheets?.t ?? null };
}

/**
 * The history of one cell (`field`) or of its whole row (no field), oldest
 * first, as edits: a person's saves with one purpose less than EDIT_GAP apart
 * are one edit, each cell with its value before the first and after the last
 * (`edits`: how many times it changed in between). Saves not written (failed)
 * stay apart. `first`/`last`: the edit's oldest and newest save ids.
 */
export function cellHistory(store, { recordId, field } = {}) {
  if (!recordId) throw fail('RECORD_NOT_FOUND', 'Falta la fila', 400);
  const fields = field ? [String(field)] : [];
  const out = recordHistory(store, { recordId: String(recordId), fields, limit: 500 });
  const edits = [];
  for (const save of out.saves) {
    const last = edits.at(-1);
    const joins =
      last &&
      last.actor === save.actor &&
      last.purpose === save.purpose &&
      last.source === save.source &&
      last.status !== 'failed' &&
      save.status !== 'failed' &&
      Date.parse(save.createdAt) - Date.parse(last.end) < EDIT_GAP;
    const edit = joins
      ? last
      : {
          first: save.actionId,
          last: save.actionId,
          actionIds: [],
          start: save.createdAt,
          end: save.createdAt,
          actor: save.actor,
          actorName: save.actorName,
          purpose: save.purpose,
          source: save.source,
          status: save.status,
          reasons: [],
          cells: [],
        };
    if (!joins) edits.push(edit);
    edit.actionIds.push(save.actionId);
    edit.last = save.actionId;
    edit.end = save.createdAt;
    edit.status = save.status;
    if (save.reason && !edit.reasons.includes(save.reason)) edit.reasons.push(save.reason);
    for (const c of save.cells) {
      const cell = edit.cells.find(x => x.field === c.field);
      if (!cell) {
        edit.cells.push({ field: c.field, before: c.before, after: c.after, edits: 1, ...(c.isNew ? { isNew: true } : {}), ...(c.undone ? { undone: true } : {}) });
        continue;
      }
      cell.after = c.after;
      cell.edits++;
      if (c.isNew) cell.isNew = true;
      // Undone when its last change was put back.
      if (c.undone) cell.undone = true;
      else delete cell.undone;
    }
  }
  return {
    row: out.row,
    field: field ? String(field) : null,
    edits: edits.map(e => ({ ...e, link: `#/historial?grupo=${e.last}` })),
    saves: out.total,
    // More than 500 saves: the oldest are shown.
    more: out.next !== null,
    ...logStart(store.db),
  };
}

/** Rows of a sheet shown as they were at one moment, at most. */
const AS_OF_ROWS = 200;
const isFormula = value => !!value && typeof value === 'object' && 'formula' in value;
function int(value, fallback, min, max) {
  if (value === undefined || value === null || value === '') return fallback;
  const n = Math.trunc(Number(value));
  return Number.isFinite(n) ? Math.min(Math.max(n, min), max) : fallback;
}

/**
 * Rows of a sheet as they were at one moment: just before (or after, `side`)
 * the save `action`, or at the time `at`. Each cell gets back the value it had
 * then by undoing every later change, newest first (the same as taking the
 * value before the first later change). Rows a later save created come back
 * empty (`absent`); rows that are empty pre-made rows now and were then are
 * left out. The window: rows `from`..`to`, or `row` ± `context` (15).
 *
 * A formula cell that had the same formula then shows today's value; one that
 * had another formula shows that formula's text. Formulas moved with their row
 * by a sync are not undone. What the log does not hold is not known: edits
 * typed in Google Sheets before `sheetsSince`, the order of edits made between
 * two readings of the sheet, and rows typed into the sheet (not through the
 * app), which show as they are now.
 *
 * `changed`: per row, the cells that differ from now, with their value now.
 * `touched`: per row, the cells the save itself changed.
 */
export function sheetAsOf(store, query = {}) {
  const db = store.db;
  const mod = moduleMap.get(String(query.module || ''));
  if (!mod) throw fail('MODULE_NOT_FOUND', 'Hoja desconocida', 404);
  let cutoff;
  let args;
  let action = null;
  let at;
  const side = query.side === 'after' ? 'after' : 'before';
  if (query.action) {
    const found = findGroup(db, String(query.action), { single: true });
    if (!found) throw fail('GROUP_NOT_FOUND', 'No se encontró ese guardado en el historial', 404);
    [action] = describeGroups(store, [found]);
    at = found.actions[0].createdAt;
    const last = db.prepare('SELECT max(rowid) n FROM changes WHERE action_id = ?').get(found.actions[0].id).n ?? 0;
    // Saves at the same moment are ordered by their changes (as undo orders them).
    const later = '(a.created_at > ? OR (a.created_at = ? AND c.rowid > ?))';
    cutoff = side === 'before' ? `(c.action_id = ? OR ${later})` : `(c.action_id <> ? AND ${later})`;
    args = [found.actions[0].id, at, at, last];
  } else {
    const t = Date.parse(String(query.at ?? ''));
    if (Number.isNaN(t)) throw fail('INVALID_MOMENT', 'Momento no válido');
    at = new Date(t).toISOString();
    cutoff = 'a.created_at > ?';
    args = [at];
  }
  const row = int(query.row, NaN, 0, 2_000_000_000);
  const context = int(query.context, 15, 0, 100);
  let from = int(query.from, NaN, 0, 2_000_000_000);
  let to = int(query.to, NaN, 0, 2_000_000_000);
  if (Number.isNaN(from) || Number.isNaN(to)) {
    if (Number.isNaN(row)) throw fail('INVALID_RANGE', 'Rango de filas no válido');
    [from, to] = [Math.max(0, row - context), row + context];
  }
  if (to < from) throw fail('INVALID_RANGE', 'Rango de filas no válido');
  const records = db
    .prepare(
      `SELECT id, row_num, version, observed, values_json, formulas_json FROM records
       WHERE sheet = ? AND missing = 0 AND row_num > ? AND row_num < 2000000000 AND row_num BETWEEN ? AND ? ORDER BY row_num LIMIT ?`,
    )
    .all(mod.id, mod.headerRow, from, to, AS_OF_ROWS);
  // The value each cell had then: the one before its first later change.
  const then = new Map();
  const laterActions = new Map();
  const touched = {};
  for (const part of chunks(records.map(r => r.id)))
    for (const c of db
      .prepare(
        `SELECT c.record_id, c.field, c.before_json, c.action_id FROM changes c JOIN actions a ON a.id = c.action_id
         WHERE c.record_id IN (${marks(part)}) AND a.status <> 'failed' AND ${unmoved('c')} AND ${cutoff}
         ORDER BY a.created_at, c.rowid`,
      )
      .all(...part, ...args)) {
      const cells = then.get(c.record_id) ?? then.set(c.record_id, new Map()).get(c.record_id);
      if (!cells.has(c.field)) cells.set(c.field, parse(c.before_json));
      const ids = laterActions.get(c.record_id) ?? laterActions.set(c.record_id, new Set()).get(c.record_id);
      ids.add(c.action_id);
    }
  if (action)
    for (const part of chunks(records.map(r => r.id)))
      for (const c of db
        .prepare(`SELECT record_id, field FROM changes c WHERE action_id = ? AND record_id IN (${marks(part)}) AND ${unmoved('c')}`)
        .all(action.id, ...part))
        (touched[c.record_id] ??= []).push(c.field);
  // Rows a later save created (or inserted) were not there yet.
  const later = [...new Set([...laterActions.values()].flatMap(s => [...s]))];
  const created = createdRows(db, later);
  for (const part of chunks(later))
    for (const r of db.prepare(`SELECT action_id, record_id FROM inserted_rows WHERE action_id IN (${marks(part)})`).all(...part))
      (created.get(r.action_id) ?? created.set(r.action_id, new Set()).get(r.action_id)).add(r.record_id);
  const keys = mod.fields.map(f => f.key);
  const rows = [];
  const changed = {};
  const absent = [];
  for (const r of records) {
    const cells = then.get(r.id);
    if (!r.observed && !cells) continue;
    const values = JSON.parse(r.values_json);
    const formulas = JSON.parse(r.formulas_json);
    if ([...(laterActions.get(r.id) ?? [])].some(id => created.get(id)?.has(r.id))) absent.push(r.id);
    for (const [field, before] of cells ?? []) {
      const now = formulas[field] ? { formula: formulas[field] } : (values[field] ?? null);
      if (comparable(before) === comparable(now)) continue;
      (changed[r.id] ??= {})[field] = now;
      if (isFormula(before)) {
        values[field] = before.formula;
        formulas[field] = before.formula;
      } else {
        values[field] = before ?? null;
        delete formulas[field];
      }
    }
    rows.push({
      id: r.id,
      row: r.row_num,
      version: r.version,
      observed: Boolean(r.observed),
      v: keys.map(k => values[k] ?? null),
      f: keys.flatMap((k, i) => (formulas[k] ? [i] : [])),
    });
  }
  const bounds = db
    .prepare('SELECT min(row_num) a, max(row_num) b FROM records WHERE sheet = ? AND missing = 0 AND row_num > ? AND row_num < 2000000000')
    .get(mod.id, mod.headerRow);
  return {
    module: mod.id,
    from,
    to: records.length === AS_OF_ROWS ? records.at(-1).row_num : to,
    first: bounds.a ?? null,
    last: bounds.b ?? null,
    at,
    side,
    action: action && {
      id: action.id,
      actor: action.actor,
      actorName: action.actorName,
      purpose: action.purpose,
      createdAt: at,
      reasons: action.reasons,
      summary: action.summary,
      summaryMsg: action.summaryMsg,
      link: `#/historial?grupo=${action.id}`,
    },
    rows,
    changed,
    touched,
    absent,
    ...logStart(db),
  };
}

// ---------------------------------------------------------------------------
// The assistant's tools (Chat and T3 Code through MCP).

const DAY = 864e5;
const EPOCH = Date.UTC(1899, 11, 30);
/**
 * Dates of date columns as YYYY-MM-DD, formulas as their text (`formulas`) or as
 * "(formula)": a sync logs a row's lookup formulas whole, kilobytes a row.
 */
function readable(sheet, field, value, formulas = true) {
  if (value && typeof value === 'object' && 'formula' in value) return formulas ? `fórmula ${value.formula}` : '(formula)';
  const type = moduleMap.get(sheet)?.fields.find(f => f.key === field)?.type;
  if (type === 'date' && typeof value === 'number') return new Date(EPOCH + Math.round(value) * DAY).toISOString().slice(0, 10);
  return value;
}
const local = iso =>
  new Intl.DateTimeFormat('es-EC', { dateStyle: 'short', timeStyle: 'short', timeZone: 'America/Guayaquil' }).format(new Date(iso));

const selectionProps = {
  groupIds: { type: 'array', items: { type: 'string' }, description: 'Whole saves (group ids from list_history)' },
  actionIds: { type: 'array', items: { type: 'string' }, description: 'Single saves inside a group (actions[].id)' },
  changeIds: { type: 'array', items: { type: 'string' }, description: 'Single cells (changes[].id); only these are undone' },
};

export const HISTORY_TOOLS = [
  {
    type: 'function',
    function: {
      name: 'list_history',
      description:
        [
          'The Historial: every save to the workbook, newest first, grouped by person, purpose and time (saves less than 30 minutes apart; typed in Google Sheets: 2 minutes).',
          '- Purpose: sheets = typed directly in Google Sheets, asistente = an applied proposal, deshacer = an undo.',
          '- Filter by user, purpose, sheet, dates and text (an identifier such as A0D or CAM079891, a field or a value). With a filter, `matched` lists the saves (action ids) of a group that match.',
          '- Each group has a summary, counts and `url`: give the person that link. It opens the Historial tab at that save, where they can also undo it themselves (all of it, one save, one row or single cells).',
        ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          user: { type: 'string', description: 'Username, name or part of it' },
          purpose: { type: 'string', enum: Object.keys(PURPOSES) },
          sheet: { type: 'string' },
          from: { type: 'string', description: 'YYYY-MM-DD (day in Ecuador)' },
          to: { type: 'string', description: 'YYYY-MM-DD (inclusive)' },
          text: { type: 'string', description: 'Identifier, field, value or note' },
          recordId: { type: 'string', description: 'Only saves that changed this row (app record id)' },
          limit: { type: 'integer', description: '1 to 30, default 10' },
          offset: { type: 'integer' },
        },
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'get_history_group',
      description:
        [
          'A group of the Historial (`id`: a group id, or any action id inside it), or one save alone (`actionId`), with its changes: sheet, row, record label, field, before → after, and whether it was already undone. Give the person its `url`.',
          '- recordId, field and text keep only the changes that match; `cells` counts them.',
          '- Formula cells read "(formula)" (`formulas: true` gives their text); saves with nothing to show are counted in savesWithoutChanges.',
          '- Up to maxChanges changes per answer, fewer when they are long; `next` is the offset of the rest.',
        ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          id: { type: 'string' },
          actionId: { type: 'string', description: 'Only this save, not its whole group' },
          recordId: { type: 'string' },
          field: { type: 'string', description: 'Column name' },
          text: { type: 'string', description: 'Label, field or value' },
          maxChanges: { type: 'integer', description: 'Default 150' },
          formulas: { type: 'boolean' },
          offset: { type: 'integer' },
        },
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'record_history',
      description:
        [
          'Every change to one row, oldest first: when, who, why, each field before → after, and a link to each save.',
          '- `id`: a label or identifier (Insectary_ID, CAM_ID, clutch number…). When several rows have it, they are listed with their recordId instead.',
          '- `others`: further rows with that name (another sheet, gone from the sheet, or formerly so named).',
          '- Formula cells read "(formula)" unless `formulas: true`. A long history comes in parts: `next` is the offset of the rest.',
        ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          id: { type: 'string', description: 'A0D, CAM079891, 1014…' },
          recordId: { type: 'string' },
          fields: { type: 'array', items: { type: 'string' }, description: 'Only these columns' },
          from: { type: 'string', description: 'YYYY-MM-DD (day in Ecuador)' },
          to: { type: 'string', description: 'YYYY-MM-DD (inclusive)' },
          limit: { type: 'integer', description: 'Saves per answer, default 100' },
          formulas: { type: 'boolean' },
          offset: { type: 'integer' },
        },
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'preview_undo',
      description:
        [
          'What undoing would do, without writing: each cell with its value now and the value it goes back to, and conflicts (a cell edited again later: undo that later save first, or correct the cell with `propose_changes`; a row gone).',
          'Pass a whole group, some of its saves, or single changes. Show the result to the person in a few lines and ask before `undo_edits`.',
        ].join('\n'),
      parameters: { type: 'object', properties: selectionProps },
    },
  },
  {
    type: 'function',
    function: {
      name: 'undo_edits',
      description:
        'Undo in Google Sheets what `preview_undo` showed, as the person you are talking with (their permissions). Only with confirmed: true after their latest message explicitly approved this undo ("sí, deshazlo"). The undo is itself a save in the Historial and can be undone.',
      parameters: {
        type: 'object',
        properties: {
          ...selectionProps,
          confirmed: { type: 'boolean', description: 'true only after the person explicitly confirmed in the chat' },
          reason: { type: 'string' },
        },
        required: ['confirmed'],
      },
    },
  },
];

export const HISTORY_TOOL_NAMES = new Set(HISTORY_TOOLS.map(t => t.function.name));

const strings = value => (Array.isArray(value) ? value.map(v => String(v).slice(0, 120)).slice(0, 5000) : undefined);

/** Runs a history tool for the assistant; `publicUrl` makes the links absolute. */
export async function runHistoryTool(store, name, args = {}, context = {}, { publicUrl = '', requestId } = {}) {
  const url = link => `${String(publicUrl || '').replace(/\/+$/, '')}/${link}`;
  const brief = g => ({
    id: g.id,
    purpose: g.purpose,
    purposeLabel: g.purposeLabel,
    user: g.actorName ?? g.actor,
    start: g.start,
    end: g.end,
    when: `${local(g.start)} – ${local(g.end)}`,
    summary: g.summary,
    counts: g.counts,
    sheets: g.sheets,
    // A sync of many rows touches many columns and gives many reasons: the first ones.
    fields: g.fields?.slice(0, 30),
    ...(g.fields?.length > 30 ? { fieldsCount: g.fields.length } : {}),
    reasons: g.reasons?.slice(0, 10),
    ...(g.reasons?.length > 10 ? { reasonsCount: g.reasons.length } : {}),
    undone: g.undone,
    undoable: g.undoable,
    // The saves of a long group that match the filters (all of them: left out).
    ...(g.matched && g.matched.length < g.counts.actions
      ? { matched: g.matched.slice(0, 20), ...(g.matched.length > 20 ? { matchedCount: g.matched.length } : {}) }
      : {}),
    url: url(g.link),
  });
  // Formula texts only when asked: a sync logs whole lookup formulas.
  const formulas = args.formulas === true;
  /** A value as the assistant reads it. */
  const shown = (sheet, field, value) => {
    const v = readable(sheet, field, value, formulas);
    if (v === null || v === undefined || v === '') return '(empty)';
    return typeof v === 'object' ? JSON.stringify(v) : String(v);
  };
  try {
    if (name === 'list_history') {
      const out = historyGroups(store, {
        user: args.user,
        purpose: args.purpose,
        sheet: args.sheet,
        from: args.from,
        to: args.to,
        text: args.text,
        recordId: args.recordId,
        limit: Math.min(Number(args.limit) || 10, 30),
        offset: args.offset,
      });
      const start = Math.max(Number(args.offset) || 0, 0);
      return fitList({ groups: out.groups.map(brief), next: out.next }, 'groups', RESULT_BUDGET - 300, kept =>
        kept < out.groups.length ? { truncated: true, next: start + kept } : {},
      ).out;
    }
    if (name === 'get_history_group') {
      const group = args.actionId
        ? historyGroup(store, String(args.actionId), { single: true })
        : historyGroup(store, String(args.id ?? ''));
      const asked = Math.min(Math.max(Number(args.maxChanges) || 150, 1), 2000);
      const offset = Math.max(Number(args.offset) || 0, 0);
      const text = args.text ? String(args.text).toLowerCase() : '';
      const field = args.field ? String(args.field).toLowerCase() : '';
      const filtered = !!(args.recordId || field || text);
      const words = c =>
        [c.label, c.field, readable(c.sheet, c.field, c.before), readable(c.sheet, c.field, c.after)].map(v =>
          (v && typeof v === 'object' ? JSON.stringify(v) : String(v ?? '')).toLowerCase(),
        );
      const keep = c =>
        (!args.recordId || c.recordId === String(args.recordId)) &&
        (!field || c.field.toLowerCase() === field) &&
        (!text || words(c).some(w => w.includes(text)));
      const matching = group.actions.map(a => (filtered ? a.changes.filter(keep) : a.changes));
      const cells = matching.reduce((n, list) => n + list.length, 0);
      // Changes are counted across the saves; an answer holds those from `offset` to offset + max.
      const answer = max => {
        let index = 0;
        let quiet = 0;
        const actions = [];
        group.actions.forEach((a, k) => {
          const first = index;
          index += matching[k].length;
          const changes = matching[k].slice(Math.max(offset - first, 0), Math.max(offset + max - first, 0)).map(c => ({
            id: c.id,
            label: c.label,
            sheet: c.sheet,
            row: c.row,
            field: c.field,
            before: readable(c.sheet, c.field, c.before, formulas),
            after: readable(c.sheet, c.field, c.after, formulas),
            ...(c.isNew ? { newRow: true } : {}),
            ...(c.undone ? { undone: true } : {}),
          }));
          if (!changes.length) {
            // A save with nothing to show (its formulas only moved with their rows): counted, not listed.
            if (!matching[k].length && !filtered && !offset) quiet++;
            return;
          }
          actions.push({
            id: a.id,
            at: local(a.createdAt),
            status: a.status,
            ...(a.reason ? { reason: a.reason } : {}),
            undone: !!a.reversedBy,
            changes,
            total: a.changes.length,
          });
        });
        const next = offset + max < cells ? offset + max : null;
        return {
          ...brief(group),
          ...(group.movedOnly ? { note: 'Rows inserted or deleted above moved these rows: their formulas followed, nothing was edited.' } : {}),
          actions,
          ...(quiet ? { savesWithoutChanges: quiet } : {}),
          cells,
          next,
          ...(max < asked && next !== null ? { truncated: true } : {}),
        };
      };
      // As many changes as fit in one answer (`next` is the offset of the rest).
      const fits = max => JSON.stringify(answer(max)).length <= RESULT_BUDGET - 300;
      let max = asked;
      if (!fits(max)) {
        let low = 1,
          high = max - 1;
        while (low < high) {
          const mid = Math.ceil((low + high) / 2);
          if (fits(mid)) low = mid;
          else high = mid - 1;
        }
        max = low;
      }
      return answer(max);
    }
    if (name === 'record_history') {
      const out = recordHistory(store, {
        id: args.id,
        recordId: args.recordId,
        fields: args.fields,
        from: args.from,
        to: args.to,
        limit: args.limit,
        offset: args.offset,
      });
      if (out.rows) return { rows: out.rows, ...(out.others.length ? { others: out.others } : {}) };
      const flags = c => `${c.isNew ? ' (new row)' : ''}${c.undone ? ' (undone later)' : ''}`;
      const view = {
        row: out.row,
        saves: out.saves.map(s => ({
          at: local(s.createdAt),
          who: s.actorName ?? s.actor,
          purpose: PURPOSES[s.purpose] ?? s.purpose,
          ...(s.reason ? { reason: s.reason } : {}),
          ...(UNDOABLE.has(s.status) ? {} : { status: s.status }),
          cells: s.cells.map(c => `${c.field}: ${shown(c.sheet, c.field, c.before)} → ${shown(c.sheet, c.field, c.after)}${flags(c)}`),
          url: url(`#/historial?accion=${s.actionId}`),
        })),
        total: out.total,
        next: out.next,
        ...(out.others ? { others: out.others } : {}),
      };
      // As many saves as fit in one answer: `next` is the offset of the rest. A save too long
      // alone (a sync of a whole row's formulas, asked with formulas) shows its first cells.
      const start = Math.max(Number(args.offset) || 0, 0);
      const budget = RESULT_BUDGET - 300;
      const fitted = fitList(view, 'saves', budget, kept => (kept < view.saves.length ? { truncated: true, next: start + kept } : {}));
      if (fitted.kept || !view.saves.length) return fitted.out;
      const [first] = view.saves;
      const room = budget - JSON.stringify({ ...view, saves: [{ ...first, cells: [] }], truncated: true, next: start + 1 }).length - 100;
      const one = fitList(first, 'cells', room, kept => ({ moreCells: `${first.cells.length - kept} more cells of this save not shown: ask for some fields` }));
      return { ...view, saves: [one.out], truncated: true, next: view.saves.length > 1 || out.next !== null ? start + 1 : null };
    }
    if (name === 'preview_undo' || name === 'undo_edits') {
      const selection = {
        groupIds: strings(args.groupIds),
        actionIds: strings(args.actionIds),
        changeIds: strings(args.changeIds),
      };
      const preview = previewEdits(store, selection);
      const cell = c => ({
        label: c.label,
        sheet: c.sheet,
        row: c.row,
        field: c.field,
        now: readable(c.sheet, c.field, c.before),
        back: readable(c.sheet, c.field, c.after),
        ...(c.reason ? { conflict: c.reason } : {}),
      });
      const view = {
        eligible: preview.eligible,
        changes: preview.changes.map(cell),
        conflicts: preview.conflicts.map(cell),
        // Rows the save inserted (a suffixed Insectary ID): undoing deletes them and the rows below move up.
        ...(preview.rowDeletes?.length ? { rowsDeleted: preview.rowDeletes.map(d => ({ label: d.label, sheet: d.sheet, row: d.row })) } : {}),
      };
      if (name === 'preview_undo') return { ...view, next: 'Show this to the person and ask; undo_edits only after they confirm.' };
      if (args.confirmed !== true)
        return { error: 'Not confirmed: show preview_undo to the person and call again with confirmed: true only after they approve.' };
      if (!preview.eligible) return { error: 'Some cells changed after that save; nothing was undone', ...view };
      const result = await undoEdits(
        store,
        { ...selection, requestId: requestId ?? `ai-undo-${randomUUID()}`, reason: String(args.reason || tpl('Deshecho desde el asistente')).slice(0, 300) },
        context.user,
      );
      const undo = result.action ?? result.actions?.[0];
      return {
        status: result.status,
        undone: view.changes.length,
        ...(undo ? { undoAction: undo.id, url: url(`#/historial?grupo=${undo.id}`) } : {}),
      };
    }
  } catch (e) {
    return { error: String(e.message).slice(0, 300), ...(e.code ? { code: e.code } : {}) };
  }
  return { error: 'Unknown tool' };
}
