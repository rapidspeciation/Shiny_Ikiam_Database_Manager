// A proposal left in needs_review (its save was cut by a restart, or Google did not
// confirm it) is compared with the sheet after each sync: when every cell it writes
// holds the proposal's value, it was written after all and is marked applied; else
// it stays, with the cells that differ (get_proposal: sheetCheck).

import { cellHolds } from './batch.mjs';

/**
 * A proposal row's record as the sheet has it now: its own, or, when that one is gone (a sync
 * that made the row a new record), the record at the same sheet row with the same label (its
 * Insectary_ID, CAM…). Null when neither.
 */
export function currentRecord(store, change, created = {}) {
  const recordId = change.create ? created[change.clientId] : change.recordId;
  const own = recordId ? store.getRecord(recordId) : null;
  if (own && !own.missing) return own;
  const row = own?.row > 0 ? own.row : change.row;
  if (!(row > 0)) return null;
  const there = store.getRecordBySheetRow(change.sheet, row);
  const label = String(change.label || own?.label || '').trim().toUpperCase();
  return there && !there.missing && label && String(there.label ?? '').trim().toUpperCase() === label ? there : null;
}

/**
 * The cells of a proposal's rows that the sheet does not hold: [{ index, label, row,
 * field, proposal, sheet }]. `created`: clientId → recordId of its new rows written.
 * Rows only shown (context) or without values are not looked at; a new row not written
 * counts as differing in all its cells. Returns { cells, differ }.
 */
export function compareWithSheet(store, changes, created = {}) {
  const differ = [];
  let cells = 0;
  changes.forEach((c, index) => {
    if (c.context || c.placeholder || !Object.keys(c.values ?? {}).length) return;
    const record = currentRecord(store, c, created);
    for (const [field, value] of Object.entries(c.values)) {
      cells++;
      const now = record && !record.missing ? record : null;
      if (now && cellHolds(c.sheet, field, value ?? null, now)) continue;
      differ.push({
        index,
        label: c.label || now?.label || '',
        row: now?.row ?? c.row ?? null,
        field,
        proposal: value ?? null,
        sheet: now ? (now.formulas?.[field] ? { formula: now.formulas[field] } : (now.values?.[field] ?? null)) : null,
      });
    }
  });
  return { cells, differ };
}
