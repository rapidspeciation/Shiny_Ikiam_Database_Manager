// Doubtful cells of a proposal. match_notebook puts the cells it is unsure of
// into the proposal (never leaves them out) with their doubt: how sure the
// reading was, the other readings and why (change.doubts = { field: {
// confidence, alternatives, reason, checked? } }). A doubtful cell counts as
// reviewed once the person edits it in the table (personEdits: a value typed,
// an alternative picked, the sheet's value kept), takes the AI's value with its
// button, or marks it checked (checked = { by, at, how }). Applying asks first
// while some are not (the table's dialog, apply_proposal's answer).

//
// Unreadable cells (match_notebook's null): change.unreadable = { field: {
// reason?, partial? } }. They have no value (never in change.values), so
// applying never writes them; the person fills one by typing a value (the
// assistant, with update_proposal, on their word), and from then on it is
// written like any other cell. Applying with some still empty leaves them as
// the sheet has them; the table and apply_proposal's answer list them.

const now = () => new Date().toISOString();

/**
 * The unreadable cells still empty (nobody gave them a value), in the rows
 * `indexes` (every row when not given): { index, label, sheet, row, field,
 * reason, partial }. Context rows are never written, so none of theirs count.
 */
export function unfilledUnreadable(changes, indexes = null) {
  const out = [];
  const rows = Array.isArray(indexes) && indexes.length ? indexes : changes.map((_, i) => i);
  for (const index of rows) {
    const change = changes[index];
    if (!change || change.context) continue;
    for (const [field, cell] of Object.entries(change.unreadable ?? {})) {
      if (field in (change.values ?? {})) continue;
      out.push({
        index,
        label: change.label,
        sheet: change.sheet,
        row: change.row ?? null,
        field,
        reason: cell?.reason ?? null,
        partial: cell?.partial ?? [],
      });
    }
  }
  return out;
}

/** A doubtful cell the person has looked at: edited, or marked checked. */
export const reviewed = (change, field) => !!change.doubts?.[field]?.checked || !!change.personEdits?.[field];

/**
 * The doubtful cells that would be written without a review, in the rows
 * `indexes` (every row when not given): { index, label, sheet, field, value,
 * alternatives, confidence, reason }. A cell set back to the sheet's value is
 * not written, so it is not one of them; context rows are never written.
 */
export function uncheckedDoubts(changes, indexes = null) {
  const out = [];
  const rows = Array.isArray(indexes) && indexes.length ? indexes : changes.map((_, i) => i);
  for (const index of rows) {
    const change = changes[index];
    if (!change || change.context) continue;
    for (const [field, doubt] of Object.entries(change.doubts ?? {})) {
      if (!(field in (change.values ?? {})) || reviewed(change, field)) continue;
      out.push({
        index,
        label: change.label,
        sheet: change.sheet,
        field,
        value: change.values[field],
        alternatives: doubt.alternatives ?? [],
        confidence: doubt.confidence ?? null,
        reason: doubt.reason ?? null,
      });
    }
  }
  return out;
}

/** The row with a doubtful cell marked checked (or unchecked again). Other cells are not touched. */
export function setChecked(change, field, checked, by, how = 'table') {
  const doubt = change.doubts?.[field];
  if (!doubt) return change;
  const next = { ...doubt };
  if (checked) next.checked = { by, at: now(), how };
  else delete next.checked;
  return { ...change, doubts: { ...change.doubts, [field]: next } };
}

/** The row without a doubt on `field` (the assistant wrote another value there, on the person's word). */
export function dropDoubt(change, field) {
  if (!change.doubts?.[field]) return change;
  const { [field]: _, ...rest } = change.doubts;
  return { ...change, doubts: Object.keys(rest).length ? rest : undefined };
}

/** The row as written when the person applies "only the sure cells": its unchecked doubtful cells left out. */
export function withoutUnchecked(change) {
  const values = { ...change.values };
  for (const field of Object.keys(change.doubts ?? {})) if (!reviewed(change, field)) delete values[field];
  return { ...change, values };
}

/**
 * The checks carried to a page matched again (match_notebook with
 * replaceProposalId): a cell checked before stays checked while the new reading
 * gives it the same value.
 */
export function carryChecks(old, fresh, sameRow, same) {
  return fresh.map(change => {
    const before = old.find(o => sameRow(o, change));
    if (!before?.doubts || !change.doubts) return change;
    let out = change;
    for (const [field, doubt] of Object.entries(before.doubts))
      if (doubt.checked && out.doubts?.[field] && same(before.values?.[field], out.values?.[field]))
        out = { ...out, doubts: { ...out.doubts, [field]: { ...out.doubts[field], checked: doubt.checked } } };
    return out;
  });
}
