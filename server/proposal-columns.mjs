// Which columns the proposals («Cambios propuestos») show and which the
// assistant's tools may write, per sheet: one place for the table
// (reviewColumns in server/notebook.mjs, readView in server/proposal-view.mjs,
// the "+ Columna…" list) and for the tools (propose_changes, update_proposal,
// match_notebook in server/assistant.mjs and server/notebook*.mjs).

/**
 * The columns every proposal of the sheet shows, in this order, whatever it
 * changes (the table adds the changed ones after them; the assistant's `view`
 * may put others first). Asked by Franz Chandi (curator), 5 Oct 2026.
 */
export const DEFAULT_COLUMNS = {
  Insectary_data: [
    'Insectary_ID',
    'Wild_Reared',
    'CLUTCH NUMBER',
    'Stock_of_origin',
    'SPECIES',
    'Sex',
    'Collection_location',
    'Intro2Insectary_date',
    'Death_date',
    'Death_cause',
    'Research_purpose',
    'Pedigree',
    'Preservation_date',
    'LIFESTAGE',
    'CAM_ID_CollData',
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
    'Preserved_Dead_Alive',
    'Location_body',
    'Notes_Insectary_data',
  ],
};

/**
 * Columns never shown in a proposal (not by default, not by the assistant's
 * view, not in the table's "+ Columna…" list) and never written by the tools.
 * - Insectary_data Preservation_medium: deprecated. Each tube's medium is in
 *   T1_/T2_Preservation_medium; the old single column is no longer kept up.
 */
export const HIDDEN_COLUMNS = {
  Insectary_data: new Set(['Preservation_medium']),
};

/**
 * Columns the tools never write (the hidden ones, and these):
 * - Insectary_data Photo_dorsal / Photo_ventral: left to their formula (a link
 *   to the photo found in Photo_links by CAM_ID). Not shown by default; a
 *   person may add them to the table to read them.
 */
export const NOT_WRITTEN = {
  Insectary_data: new Set(['Preservation_medium', 'Photo_dorsal', 'Photo_ventral']),
};

export const isHiddenColumn = (sheet, field) => !!HIDDEN_COLUMNS[sheet]?.has(field);
export const isNotWritten = (sheet, field) => !!NOT_WRITTEN[sheet]?.has(field) || isHiddenColumn(sheet, field);
/** Why a column is not written, for the tools' refusals. */
export const notWrittenWhy = (sheet, field) =>
  isHiddenColumn(sheet, field)
    ? `${field} is deprecated in ${sheet} and is not written (each tube's medium goes in T1_/T2_Preservation_medium)`
    : `${field} is left to its formula in ${sheet} and is not written`;
