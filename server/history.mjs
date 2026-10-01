// Historial: saves grouped by person, purpose and time, and their undo.
//
// Every action (one save) records its purpose: the tab or flow it came from
// (Colecta, Muertes, Tubos…). Older actions get one inferred from what they
// changed. A group is a run of actions by the same person with the same purpose
// where each follows the previous one by less than a time window (30 minutes;
// 2 minutes for changes read from Google Sheets, so each sync is its own group).
// A group's id is the id of its oldest action, so it does not change as the
// person keeps saving; any action id of a group finds the group.

import { randomUUID } from 'node:crypto';
import { msg, msgn, tpl } from './messages.mjs';
import { moduleMap } from './schema.mjs';

/** Purpose → label shown to people (Spanish, like the rest of the app). */
export const PURPOSES = {
  colecta: 'Colecta',
  monitoreo: 'Monitoreo',
  muertes: 'Muertes',
  emergidos: 'Emergidos',
  clutches: 'Clutches',
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
export const CLIENT_PURPOSES = new Set(['colecta', 'monitoreo', 'muertes', 'emergidos', 'clutches', 'tubos', 'tablas', 'revision']);
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
const fail = (code, message, status = 400, details) => Object.assign(new Error(message), { code, status, details });

/** Adds the purpose column (older databases), the indexes the Historial needs, and purposes for older saves. */
export function initHistory(db) {
  const has = db
    .prepare('PRAGMA table_info(actions)')
    .all()
    .some(c => c.name === 'purpose');
  if (!has) db.exec('ALTER TABLE actions ADD COLUMN purpose TEXT');
  db.exec(`CREATE INDEX IF NOT EXISTS changes_action ON changes(action_id);
    CREATE INDEX IF NOT EXISTS actions_created ON actions(created_at, id);
    CREATE INDEX IF NOT EXISTS actions_key ON actions(actor, purpose, created_at, id);`);
  backfillPurposes(db);
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
    const touches = db.prepare('SELECT 1 FROM changes WHERE action_id = ? AND sheet = ? LIMIT 1');
    const known = new Map();
    return {
      has: id => known.get(id) ?? known.set(id, !!touches.get(id, String(sheet))).get(id),
    };
  }
  const clauses = [];
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
  const ids = new Set(db.prepare(`SELECT action_id FROM changes WHERE ${clauses.join(' AND ')}`).all(...args).map(r => r.action_id));
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
  for (const group of scanGroups(db, { actors, purpose, stopBefore: Number.isNaN(from) ? undefined : from })) {
    if (matches && !group.actions.some(a => matches.has(a.id))) continue;
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
    if (!found && group.actions.some(a => a.id === until)) found = true;
  }
  const groups = describeGroups(store, out, matches);
  return { groups, offset, limit, next: more ? offset + out.length : null, ...(until ? { found } : {}) };
}

/** The group holding a group or action id: the run of saves by that person and purpose around it. */
export function findGroup(db, id) {
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
    key: `${action.actor}\u0000${action.purpose}`,
    actor: action.actor,
    purpose: action.purpose,
    actions,
    newestT: Date.parse(actions[0].createdAt),
    oldestT: Date.parse(actions.at(-1).createdAt),
  };
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
    const statement =
      perRecord.get(written.length) ??
      perRecord
        .set(
          written.length,
          written.length
            ? db.prepare(
                `SELECT c.record_id, min(c.sheet) sheet, min(c.row_num) row_num, r.label, count(*) cells, min(c.rowid) first,
                   max(CASE WHEN c.field IN (${IDENTITY_SQL}) THEN coalesce(nullif(c.after_json, 'null'), c.before_json) END) identity
                 FROM changes c LEFT JOIN records r ON r.id = c.record_id WHERE c.action_id IN (${key}) GROUP BY c.record_id ORDER BY first`,
              )
            : null,
        )
        .get(written.length);
    const rows = statement ? statement.all(...written) : [];
    const fields = written.length
      ? db.prepare(`SELECT DISTINCT field FROM changes WHERE action_id IN (${key})`).all(...written).map(r => r.field)
      : [];
    const newRows = new Set(written.flatMap(id => [...(created.get(id) ?? [])]));
    const labels = rows.map(r => rowLabel(r.label, r.sheet, r.row_num, parse(r.identity)));
    const statuses = {};
    for (const a of group.actions) statuses[a.status] = (statuses[a.status] ?? 0) + 1;
    const undoable = group.actions.filter(a => UNDOABLE.has(a.status));
    // Cells of confirmed saves, and how many of them a later undo put back.
    const undoableIds = undoable.map(a => a.id);
    const undoableCells = undoableIds.length
      ? db.prepare(`SELECT count(*) n FROM changes WHERE action_id IN (${marks(undoableIds)})`).get(...undoableIds).n
      : 0;
    let undoneCells = 0;
    for (const a of undoable.filter(a => undone.has(a.id))) {
      const cells = undone.get(a.id).cells;
      for (const c of db.prepare('SELECT record_id, field FROM changes WHERE action_id = ?').all(a.id))
        if (cells.has(`${c.record_id}\u0000${c.field}`)) undoneCells++;
    }
    const counts = {
      actions: group.actions.length,
      rows: rows.length,
      newRows: rows.filter(r => newRows.has(r.record_id)).length,
      cells: rows.reduce((n, r) => n + r.cells, 0),
    };
    const id = actionIds.at(-1);
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
      link: `#/historial?grupo=${id}`,
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

/** One group with its saves and every change (sheet, row, record label, field, before → after). */
export function historyGroup(store, id) {
  const db = store.db;
  const group = findGroup(db, id);
  if (!group) throw fail('GROUP_NOT_FOUND', 'No se encontró ese guardado en el historial', 404);
  const [summary] = describeGroups(store, [group]);
  const ids = group.actions.map(a => a.id);
  const undone = undoneIndex(db, ids);
  const created = createdRows(db, ids);
  const changes = new Map();
  for (const part of chunks(ids))
    for (const c of db
      .prepare(
        `SELECT c.id, c.action_id, c.record_id, c.sheet, c.row_num, c.field, c.before_json, c.after_json, r.label
         FROM changes c LEFT JOIN records r ON r.id = c.record_id WHERE c.action_id IN (${marks(part)}) ORDER BY c.rowid`,
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
  for (const part of chunks(ids))
    for (const c of db.prepare(`SELECT id, action_id, record_id, field FROM changes WHERE action_id IN (${marks(part)}) ORDER BY rowid`).all(...part))
      if ((!wanted || wanted.has(c.id)) && !undone.get(c.action_id)?.cells.has(`${c.record_id}\u0000${c.field}`)) changes.push(c.id);
  if (!changes.length) throw fail('NOTHING_TO_UNDO', 'Esos cambios ya están deshechos', 409);
  return { actionIds: ids, changeIds: changes };
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

// ---------------------------------------------------------------------------
// The assistant's tools (Chat and T3 Code through MCP).

const DAY = 864e5;
const EPOCH = Date.UTC(1899, 11, 30);
/** Dates of date columns as YYYY-MM-DD, formulas as their text. */
function readable(sheet, field, value) {
  if (value && typeof value === 'object' && 'formula' in value) return `fórmula ${value.formula}`;
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
        'The Historial: every save to the workbook, grouped by person, purpose and time (saves less than 30 minutes apart; typed in Google Sheets: 2 minutes), newest first. Purpose sheets = typed directly in Google Sheets, asistente = an applied proposal, deshacer = an undo. Filter by user, purpose, sheet, dates and text (an identifier such as A0D or CAM079891, a field or a value). Each group has a summary, counts and `url`: always give the person that link (it opens the Historial tab at that save, where they can also undo it themselves: all of it, one save, one row or single cells).',
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
        'One save of the Historial (a group id, or any action id inside it) with every change: sheet, row, record label, field, before → after, and whether it was already undone. Give the person its `url`.',
      parameters: {
        type: 'object',
        properties: {
          id: { type: 'string' },
          maxChanges: { type: 'integer', description: 'Default 300' },
        },
        required: ['id'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'preview_undo',
      description:
        'What undoing would do, without writing: each cell with its value now and the value it goes back to, and conflicts (a cell edited again later: undo that later save first, or correct the cell with propose_changes; a row gone). Pass a whole group, some of its saves, or single changes. Always show this to the person in a few lines and ask before undo_edits.',
      parameters: { type: 'object', properties: selectionProps },
    },
  },
  {
    type: 'function',
    function: {
      name: 'undo_edits',
      description:
        'Undo in Google Sheets what preview_undo showed, as the person you are talking with (their permissions). Only with confirmed: true AFTER their latest message explicitly approved this undo ("sí, deshazlo"). The undo is itself a save in the Historial and can be undone.',
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
    fields: g.fields,
    reasons: g.reasons,
    undone: g.undone,
    undoable: g.undoable,
    url: url(g.link),
  });
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
      return { groups: out.groups.map(brief), next: out.next };
    }
    if (name === 'get_history_group') {
      const group = historyGroup(store, String(args.id ?? ''));
      const max = Math.min(Math.max(Number(args.maxChanges) || 300, 1), 2000);
      let left = max;
      const actions = group.actions.map(a => {
        const changes = a.changes.slice(0, Math.max(left, 0)).map(c => ({
          id: c.id,
          label: c.label,
          sheet: c.sheet,
          row: c.row,
          field: c.field,
          before: readable(c.sheet, c.field, c.before),
          after: readable(c.sheet, c.field, c.after),
          ...(c.isNew ? { newRow: true } : {}),
          ...(c.undone ? { undone: true } : {}),
        }));
        left -= changes.length;
        return { id: a.id, at: local(a.createdAt), status: a.status, reason: a.reason, undone: !!a.reversedBy, changes, total: a.changes.length };
      });
      return { ...brief(group), actions, ...(left < 0 || group.counts.cells > max ? { truncated: true } : {}) };
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
      const view = { eligible: preview.eligible, changes: preview.changes.map(cell), conflicts: preview.conflicts.map(cell) };
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
