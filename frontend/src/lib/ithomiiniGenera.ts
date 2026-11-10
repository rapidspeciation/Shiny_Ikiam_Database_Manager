/**
 * The genera of the tribe Ithomiini (Nymphalidae: Danainae): the one list the app uses to say whether a name is an
 * Ithomiini (Identificar's «Ithomiini» badge and sex-estimate gate, signed in or not; the server's «Fotos para el
 * campo»; scripts/field-photos.py reads this file; tools/whatsapp KEYTERMS is checked against it).
 *
 * The 46 genera Butterflies of America files under Danainae: Ithomiini (Warren et al.,
 * https://www.butterfliesofamerica.com/L/Nymphalidae.htm, as parsed into the lab's Taxonomy_v07Sep26, the same 46 as
 * Taxonomy_v18Jun25), plus Leucothyris Boisduval, 1870, a synonym of Oleria that the identifier's head keeps as a
 * genus. Frozen 10 Nov 2026 from WingsClassificator
 * reports/model_autoresearch_20261109/taxonomic/audit/tribe_audit.py (REFERENCE_GENERA, REFERENCE_SYNONYMS).
 * Before, the list was the head's genera the workbook files under Tribe Ithomiini: it missed Athyrtis and Leucothyris
 * (no workbook row: «No parece una Ithomiini.») and, signed in, Elzunia. Not the workbook's Tribe column: a genus with
 * no row there is still an Ithomiini. Change only with a dated note and a source.
 */
export const ITHOMIINI_GENERA: readonly string[] = Object.freeze([
  'Aeria', 'Aremfoxia', 'Athesis', 'Athyrtis', 'Brevioleria', 'Callithomia', 'Ceratinia', 'Dircenna', 'Elzunia',
  'Episcada', 'Epityches', 'Eutresis', 'Forbestra', 'Godyris', 'Greta', 'Haenschia', 'Heterosais', 'Hyalenna',
  'Hyalyris', 'Hypoleria', 'Hypomenitis', 'Hyposcada', 'Hypothyris', 'Ithomia', 'Leucothyris', 'Mcclungia',
  'Mechanitis', 'Megoleria', 'Melinaea', 'Methona', 'Napeogenes', 'Oleria', 'Ollantaya', 'Olyras', 'Pachacutia',
  'Pagyris', 'Paititia', 'Patricia', 'Placidina', 'Pseudoscada', 'Pteronymia', 'Sais', 'Scada', 'Thyridia',
  'Tithorea', 'Veladyris', 'Velamysta',
])

const SET = new Set(ITHOMIINI_GENERA)

/** Whether a genus is an Ithomiini («Leucothyris» yes, «Macrosoma», a moth, no). Case as written in the list. */
export const isIthomiiniGenus = (genus: string | null | undefined) => !!genus && SET.has(genus)
