// Columns named like a date that are not the date the team recorded: the Sanger manifest block's
// DATE_OF_COLLECTION and DATE_OF_PRESERVATION. In Collection_data they are formulas on Collection_date (AD)
// and Preservation_date (AL): for a row dated before 14 Apr 2023 whose genus is not in Lists' Genera_0176
// (AG:AH) they give that date plus 451 days, else the date itself. In Insectary_data DATE_OF_COLLECTION is
// =M (Preservation_date). Elsewhere (F1/F2_MutationRate, Ithomiini_tube_locations_18Jun26) they are lookups
// for the manifests. The tools that show these columns (query, find_records, get_record, describe_sheet)
// add DATE_NOTE, and a column not found lists them after the recorded dates, marked.

/** The recorded column each manifest date stands for, per sheet. */
export const MANIFEST_DATES = {
  Collection_data: { DATE_OF_COLLECTION: 'Collection_date', DATE_OF_PRESERVATION: 'Preservation_date' },
  Insectary_data: { DATE_OF_COLLECTION: 'Preservation_date' },
};

/** A manifest date's name, in a SQL text or as a key. */
const NAME = /\bDATE_OF_(?:COLLECTION|PRESERVATION)\b/i;
export const isManifestDate = name => /^DATE_OF_(?:COLLECTION|PRESERVATION)(?:_text)?$/i.test(String(name ?? ''));

export const DATE_NOTE =
  "DATE_OF_COLLECTION and DATE_OF_PRESERVATION are formulas of the Sanger manifest block, not the date recorded: in Collection_data they give Collection_date and Preservation_date plus 451 days for rows dated before 14 Apr 2023 whose genus is not in Lists' Genera_0176; in Insectary_data DATE_OF_COLLECTION copies Preservation_date. When a butterfly was collected: Collection_data.Collection_date (reared ones: Insectary_data.Intro2Insectary_date); preserved: Preservation_date.";

/** The recorded column a manifest date of `sheet` is made from ("Collection_date"; other sheets: the Collection_data one), or null. */
export function recordedDate(sheet, column) {
  const key = String(column ?? '').replace(/_text$/i, '').toUpperCase();
  return MANIFEST_DATES[sheet]?.[key] ?? (isManifestDate(key) ? (key === 'DATE_OF_COLLECTION' ? 'Collection_date' : 'Preservation_date') : null);
}

/** A query answer (its text) with the note first when the statement or its columns name a manifest date. */
export function withQueryNote(text, sql, columns = []) {
  if (typeof text !== 'string') return text;
  return NAME.test(String(sql ?? '')) || columns.some(isManifestDate) ? `(${DATE_NOTE})\n${text}` : text;
}

/** A tool's JSON answer with `dateNote` when it shows a manifest date (a column, a key or a field name). */
export function withDateNote(out) {
  if (!out || typeof out !== 'object' || out.error || out.dateNote) return out;
  const shows = value => {
    if (Array.isArray(value)) return value.some(v => (typeof v === 'string' ? isManifestDate(v) : shows(v)));
    if (value && typeof value === 'object') return Object.entries(value).some(([k, v]) => isManifestDate(k) || (k === 'key' && isManifestDate(v)) || shows(v));
    return false;
  };
  const where = out.columns ?? out.values ?? out.found ?? out.latestRows;
  return shows(where) ? { ...out, dateNote: DATE_NOTE } : out;
}
