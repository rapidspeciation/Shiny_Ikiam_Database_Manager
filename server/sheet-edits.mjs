// Cells of a pending proposal edited in the sheet after the assistant read
// them. A proposal keeps, for each cell of an existing row it changes, what the
// sheet had when it was drafted (change.before). Someone may edit those cells
// meanwhile (in Google Sheets, or in the app) while the person reviews 25
// photos: such a cell is told apart in the table, with what was read then and
// what the sheet has now, and the sheet's value wins unless the person chooses
// the proposal's (change.sheetEdits = { field: { use: 'sheet' | 'proposal',
// now: the sheet's value they decided on, by, at } }). A decision holds while
// the sheet still has that value; a cell edited again after it is told apart
// again, and applying refuses it until someone looks. A new row whose
// pre-made row (its Insectary ID) was typed into meanwhile is left out.
//
// The same comparison as the save's (cellHolds in batch.mjs): a count kept as
// a sum compares by its formula, a formula typed over by the value it gives.

import { cellHolds } from './batch.mjs';
import { isSumField, simpleSum } from './schema.mjs';

const now = () => new Date().toISOString();

/**
 * A sheet cell as the review table shows it: a count kept as a sum shows its
 * formula (=23+8+4+3), as the person types it, not the total it computes to.
 */
export function shownValue(record, field) {
  const sum = record && isSumField(record.sheet, field) ? simpleSum(record.formulas?.[field]) : null;
  return sum ?? record?.values?.[field] ?? null;
}

const replacing = (change, record, field) => !!record?.formulas?.[field] && !!change.replaceFormula?.includes(field);
/** Whether the row's cell still holds `expected`. */
const holds = (change, record, field, expected) => cellHolds(change.sheet, field, expected, record, replacing(change, record, field));

/** Whether a proposed cell of an existing row has another value in the sheet than the assistant read. */
export function editedInSheet(change, record, field) {
  if (change.create || !record || record.missing || !change.before || !Object.hasOwn(change.before, field)) return false;
  return !holds(change, record, field, change.before[field]);
}

/**
 * The proposed cells of an existing row edited in the sheet since they were
 * read: field → { read, now, use?, decidedBy?, decidedAt?, again? }. `use`:
 * what the person chose while the sheet still has `now`; `again`: edited
 * again after they chose. A context row writes nothing, so it has none.
 */
export function sheetChangesOf(change, record) {
  if (change.create || change.context || !record || record.missing) return {};
  const out = {};
  for (const field of Object.keys(change.values ?? {})) {
    if (!editedInSheet(change, record, field)) continue;
    const decided = change.sheetEdits?.[field];
    const settled = !!decided && holds(change, record, field, decided.now);
    out[field] = {
      read: change.before[field],
      now: shownValue(record, field),
      ...(settled ? { use: decided.use, decidedBy: decided.by ?? null, decidedAt: decided.at ?? null } : decided ? { again: true } : {}),
    };
  }
  return out;
}

/**
 * Who edited a cell last, and when, since `since` (the proposal's time): the
 * app's save (`by` its person) or the sheet as the app saw it, by its edit
 * trigger ('sheets') or a sheet read ('sync'). Empty when not recorded.
 */
export function lastEdit(db, recordId, field, since) {
  try {
    const r = db
      .prepare(
        `SELECT a.actor, a.source, a.created_at, a.result_json, u.display_name FROM changes c
         JOIN actions a ON a.id = c.action_id LEFT JOIN users u ON u.id = a.actor
         WHERE c.record_id = ? AND c.field = ? AND a.status IN ('verified', 'observed') AND a.created_at >= ?
         ORDER BY a.created_at DESC, c.rowid DESC LIMIT 1`,
      )
      .get(recordId, field, since ?? '');
    if (!r) return {};
    if (r.source !== 'sheet_reconciliation') return { source: 'app', by: r.display_name || r.actor, at: r.created_at };
    const via = (() => {
      try {
        return JSON.parse(r.result_json ?? 'null')?.via;
      } catch {
        return null;
      }
    })();
    return { source: via === 'hook' ? 'sheets' : 'sync', at: r.created_at };
  } catch {
    return {};
  }
}

const insectaryId = change => (change.create && change.sheet === 'Insectary_data' ? change.values?.Insectary_ID : null) || null;

/**
 * The rows in use holding the Insectary IDs of a proposal's new rows (one read
 * for them all): Insectary ID → { recordId, row, label }.
 */
export function takenRows(store, changes) {
  const ids = [...new Set(changes.map(insectaryId).filter(Boolean).map(String))];
  if (!ids.length) return new Map();
  try {
    const rows = store.db
      .prepare(
        `SELECT id, row_num, label, json_extract(values_json, '$.Insectary_ID') insectary_id FROM records
         WHERE sheet = 'Insectary_data' AND missing = 0 AND observed = 1
           AND json_extract(values_json, '$.Insectary_ID') IN (SELECT value FROM json_each(?))`,
      )
      .all(JSON.stringify(ids));
    return new Map(rows.map(r => [String(r.insectary_id), { recordId: r.id, row: r.row_num, label: r.label }]));
  } catch {
    return new Map();
  }
}

/**
 * A new Insectary_data row whose pre-made row (the one its Insectary ID
 * picks) someone typed into meanwhile: { recordId, row, label }; null otherwise.
 * `taken`: takenRows of its proposal, when read already.
 */
export function takenRow(store, change, taken = null) {
  const id = insectaryId(change);
  if (!id) return null;
  return (taken ?? takenRows(store, [change])).get(String(id)) ?? null;
}

/** The row with the person's choice for a cell edited in the sheet ('sheet' or 'proposal'), on the sheet's value now. */
export function decide(change, record, field, use, by) {
  return { ...change, sheetEdits: { ...change.sheetEdits, [field]: { use, now: shownValue(record, field), by, at: now() } } };
}

/** The row without a choice on `field` (it is no longer a cell edited in the sheet, or is read again). */
export function forget(change, field) {
  if (!change.sheetEdits?.[field]) return change;
  const { [field]: _, ...rest } = change.sheetEdits;
  return { ...change, sheetEdits: Object.keys(rest).length ? rest : undefined };
}

/**
 * The rows as the save writes them (`entries`: [index, change]): a cell edited
 * in the sheet since it was read keeps the sheet's value (left out) unless the
 * person chose the proposal's, then written over what the sheet has now
 * (expected); a new row whose pre-made row was taken is left out. Returns
 * { writes, kept, again }: `kept` the cells (and rows) left as the sheet has
 * them, `again` the cells edited again after the person chose (the save waits
 * for them).
 */
export function resolveSheetEdits(store, entries) {
  const writes = [];
  const kept = [];
  const again = [];
  const inUse = takenRows(store, entries.map(([, change]) => change));
  for (const [index, change] of entries) {
    const taken = takenRow(store, change, inUse);
    if (taken) {
      kept.push({ index, label: change.label, sheet: change.sheet, field: null, rowTaken: taken });
      continue;
    }
    const record = change.create ? null : store.getRecord(change.recordId);
    const edited = sheetChangesOf(change, record);
    if (!Object.keys(edited).length) {
      writes.push([index, change]);
      continue;
    }
    const values = { ...change.values };
    const before = { ...change.before };
    for (const [field, cell] of Object.entries(edited)) {
      const where = { index, label: change.label, sheet: change.sheet, row: change.row, field, read: cell.read, now: cell.now };
      if (cell.again) again.push(where);
      else if (cell.use === 'proposal') before[field] = change.sheetEdits[field].now;
      else {
        delete values[field];
        kept.push(where);
      }
    }
    writes.push([index, { ...change, values, before }]);
  }
  return { writes, kept, again };
}
