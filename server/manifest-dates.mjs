// Columns named like a date that are not the date the team recorded: the Sanger manifest block's
// DATE_OF_COLLECTION and DATE_OF_PRESERVATION. In Collection_data they are formulas on Collection_date (AD)
// and Preservation_date (AL): for a row dated before 14 Apr 2023 whose genus is not in Lists' Genera_0176
// (AG:AH) they give that date plus 451 days, else the date itself. In Insectary_data DATE_OF_COLLECTION is
// =M (Preservation_date). Elsewhere (F1/F2_MutationRate, Ithomiini_tube_locations_18Jun26) they are lookups
// for the manifests. The tools that show these columns (query, search_text, find_records, count_records,
// get_record, describe_sheet) add DATE_NOTE, and a column not found lists them after the recorded dates, marked.

/** The recorded column each manifest date is computed from, in the sheets where that is known. */
export const MANIFEST_DATES = {
  Collection_data: { DATE_OF_COLLECTION: 'Collection_date', DATE_OF_PRESERVATION: 'Preservation_date' },
  Insectary_data: { DATE_OF_COLLECTION: 'Preservation_date' },
};

/** A manifest date's name (or its _text twin), in a SQL text or as a key. */
const NAME = /\bDATE_OF_(?:COLLECTION|PRESERVATION)(?:_text)?\b/i;
export const isManifestDate = name => /^DATE_OF_(?:COLLECTION|PRESERVATION)(?:_text)?$/i.test(String(name ?? ''));

export const DATE_NOTE =
  "DATE_OF_COLLECTION and DATE_OF_PRESERVATION are formulas of the Sanger manifest block, computed from the recorded dates: in Collection_data they give Collection_date and Preservation_date plus 451 days for rows dated before 14 Apr 2023 whose genus is not in Lists' Genera_0176; in Insectary_data DATE_OF_COLLECTION copies Preservation_date. When a butterfly was collected: Collection_data.Collection_date; its arrival or emergence in the insectary: Insectary_data.Intro2Insectary_date; preserved: Preservation_date.";

/** The recorded column a manifest date of `sheet` is computed from ("Collection_date"), or null. */
export function recordedDate(sheet, column) {
  const key = String(column ?? '').replace(/_text$/i, '').toUpperCase();
  return MANIFEST_DATES[sheet]?.[key] ?? null;
}

/** A query answer (its text) with the note first when the statement or its columns name a manifest date. */
export function withQueryNote(text, sql, columns = []) {
  if (typeof text !== 'string') return text;
  return NAME.test(String(sql ?? '')) || columns.some(isManifestDate) ? `(${DATE_NOTE})\n${text}` : text;
}

/** Whether a tool's answer or arguments show a manifest date: a key, a `key` field, a list item, a groupBy name ("DATE_OF_COLLECTION:month"). */
function shows(value) {
  if (typeof value === 'string') return isManifestDate(value.replace(/:[a-z]+$/i, ''));
  if (Array.isArray(value)) return value.some(shows);
  if (value && typeof value === 'object')
    return Object.entries(value).some(([k, v]) => isManifestDate(k) || ((k === 'key' || k === 'groupBy' || k === 'fields') && shows(v)) || (typeof v === 'object' && shows(v)));
  return false;
}

/** A tool's JSON answer with `dateNote` when it, or the arguments it was asked with, show a manifest date. */
export function withDateNote(out, args = null) {
  if (!out || typeof out !== 'object' || Array.isArray(out) || out.error || out.dateNote) return out;
  return shows(out) || (args && shows({ fields: args.fields, groupBy: args.groupBy, filters: args.filters, field: args.field && { [args.field]: 1 } }))
    ? { ...out, dateNote: DATE_NOTE }
    : out;
}
