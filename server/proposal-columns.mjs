// Which columns the proposals («Cambios propuestos») show and which the
// assistant's tools may write, per sheet: one place for the table
// (reviewColumns in server/notebook.mjs, readView in server/proposal-view.mjs,
// the "+ Columna…" list) and for the tools (propose_changes, update_proposal,
// match_notebook in server/assistant.mjs and server/notebook*.mjs).

import { moduleMap } from './schema.mjs';

/** A sheet's columns in its order, up to and including `last`. */
const upTo = (sheet, last) => {
  const keys = moduleMap.get(sheet)?.fields.map(f => f.key) ?? [];
  return keys.slice(0, keys.indexOf(last) + 1);
};
/** A sheet's columns in its order after `last`. */
const after = (sheet, last) => {
  const keys = moduleMap.get(sheet)?.fields.map(f => f.key) ?? [];
  return keys.slice(keys.indexOf(last) + 1);
};

/**
 * The columns every proposal of the sheet shows, in this order, whatever it
 * changes (the table adds the changed ones after them; the assistant's `view`
 * may put others first). Insectary_data: every column up to and including
 * Notes_Insectary_data (column AF), in the sheet's order.
 */
export const DEFAULT_COLUMNS = {
  Insectary_data: upTo('Insectary_data', 'Notes_Insectary_data'),
};

/**
 * Columns never shown in a proposal (not by default, not by the assistant's
 * view, not in the table's "+ Columna…" list) and never written by the tools.
 * - Insectary_data after Notes_Insectary_data (racks, manifests, the
 *   collection and identification block): another workflow's columns.
 */
export const HIDDEN_COLUMNS = {
  Insectary_data: new Set(after('Insectary_data', 'Notes_Insectary_data')),
};

/**
 * Columns the tools never write (the hidden ones, and these):
 * - Insectary_data Photo_dorsal / Photo_ventral: left to their formula (a link
 *   to the photo found in Photo_links by CAM_ID). Shown, as they are.
 */
export const NOT_WRITTEN = {
  Insectary_data: new Set(['Photo_dorsal', 'Photo_ventral']),
};

export const isHiddenColumn = (sheet, field) => !!HIDDEN_COLUMNS[sheet]?.has(field);
export const isNotWritten = (sheet, field) => !!NOT_WRITTEN[sheet]?.has(field) || isHiddenColumn(sheet, field);
/** Why a column is not shown, for the view's refusals. */
export const hiddenWhy = (sheet, field) =>
  `${field} is not shown in proposals of ${sheet}: its columns end at ${DEFAULT_COLUMNS[sheet]?.at(-1) ?? 'its notes'}; the ones after belong to another workflow`;
/** Why a column is not written, for the tools' refusals. */
export const notWrittenWhy = (sheet, field) =>
  isHiddenColumn(sheet, field)
    ? `${field} is not written in proposals of ${sheet}: its columns end at ${DEFAULT_COLUMNS[sheet]?.at(-1) ?? 'its notes'}; the ones after belong to another workflow`
    : `${field} is left to its formula in ${sheet} and is not written`;
