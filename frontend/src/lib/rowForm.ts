import type { Field } from './types'

/**
 * The Buscador on a phone: the grid (default) or one row at a time as a form.
 * The choice is kept in this browser; anything unknown stored there means the grid.
 */
export type PhoneView = 'grid' | 'form'
export const PHONE_VIEW_KEY = 'phone-view'
export const phoneViewFor = (stored: unknown): PhoneView => (stored === 'form' ? 'form' : 'grid')

/**
 * The form's sections, most used first. A column goes to the first section whose
 * rule it matches; the sheet's own ID columns always open the form. Sections
 * folded by default hold columns that are mostly formulas or rarely typed.
 */
export interface FormSection {
  key: string
  /** Spanish key of its title (translated where shown). */
  title: string
  folded: boolean
  fields: Field[]
}

const RULES: { key: string; title: string; folded?: boolean; test: RegExp }[] = [
  // The Sanger/ToL manifest columns, in capitals, filled by formulas from the columns above.
  {
    key: 'manifest',
    title: 'Manifiesto (fórmulas)',
    folded: true,
    test: /^(COLLECTOR_|COLLECTED_BY$|IDENTIFIE[DR]_|DATE_OF_|DECIMAL_|COLLECTION_LOCATION$|HABITAT$|ELEVATION$|TAXON_ID$|COMMON_NAME$|ToLID$|Specimen ID$)/,
  },
  { key: 'storage', title: 'Racks y manifiestos', folded: true, test: /_(rack|manifest)$/i },
  { key: 'notes', title: 'Notas y fotos', test: /notes?(_|$)|^photo_/i },
  {
    key: 'field',
    title: 'En el campo',
    test: /^(country|side_andes|collection_|transect|bait|forest_stratum|collector|rainfall|cloud_cover|flight_height|butterfly_weight|location$|start_time|end_time|datalogger)/i,
  },
  { key: 'dates', title: 'Fechas y muerte', test: /date|death|^intro2/i },
  { key: 'tubes', title: 'Tubos y preservación', test: /^tube_|^t\d_|preserv|^location_|splitted/i },
  {
    key: 'id',
    title: 'Identidad',
    test: /(^|_)id($|_)|cam_id|clutch|wild_reared|stock_of_origin|species|^sex$|lifestage|^(family|subfamily|tribe|genus)$|subspecies|^identifier$|purpose|pedigree|data_entry_order|release_collect|fieldmark|generation/i,
  },
]
const ORDER = ['id', 'field', 'dates', 'tubes', 'notes', 'storage', 'manifest', 'other']
const TITLES: Record<string, string> = Object.fromEntries([...RULES.map(r => [r.key, r.title]), ['other', 'Otras columnas']])
const FOLDED = new Set(RULES.filter(r => r.folded).map(r => r.key))

/** The section a column belongs to. */
export function sectionOf(key: string, identity: string[] = []): string {
  if (identity.includes(key)) return 'id'
  return RULES.find(r => r.test.test(key))?.key ?? 'other'
}

/**
 * The columns in sections, in the sheet's order within each. A sheet whose
 * columns fall in a single section gets no sections (one plain list).
 */
export function formSections(fields: Field[], identity: string[] = []): FormSection[] {
  const by = new Map<string, Field[]>()
  for (const field of fields) {
    const key = sectionOf(field.key, identity)
    by.set(key, [...(by.get(key) ?? []), field])
  }
  const sections = ORDER.filter(key => by.has(key)).map(key => ({
    key,
    title: TITLES[key],
    folded: FOLDED.has(key),
    // Within «Notas y fotos», the notes before the photo links.
    fields: key === 'notes' ? noteFirst(by.get(key)!) : by.get(key)!,
  }))
  // A section of one column is not worth a header: it joins the neighbour before it (or after, if first)
  // that is folded the same way.
  for (let i = 0; i < sections.length && sections.length > 1; i++) {
    const s = sections[i]
    // «Otras columnas» keeps its own header (its columns belong nowhere else), and nothing is folded into the formula section.
    if (s.fields.length !== 1 || s.key === 'id' || s.key === 'other') continue
    const into = [sections[i - 1], sections[i + 1]].find(n => n && n.folded === s.folded && n.key !== 'manifest')
    if (!into) continue
    into.fields = i > 0 && into === sections[i - 1] ? [...into.fields, ...s.fields] : [...s.fields, ...into.fields]
    sections.splice(i, 1)
    i = -1
  }
  // The notes right after the identity: a note is what people most often open a row to write (hands-on, 7 Nov 2026).
  const notes = sections.findIndex(s => s.key === 'notes')
  if (notes > 1) sections.splice(1, 0, ...sections.splice(notes, 1))
  if (sections.length > 1) return sections
  return [{ key: 'all', title: '', folded: false, fields }]
}

/** A sheet's note column (Notes_Insectary_data, Notes_Collection_data…): shown as «Nota», its column name beside it. */
export const isNoteField = (key: string) => /^notes?(_|$)/i.test(key)

/** The note columns first, the others after them, each in the sheet's order. */
const noteFirst = (fields: Field[]) => [...fields.filter(f => isNoteField(f.key)), ...fields.filter(f => !isNoteField(f.key))]

/**
 * The side form's two groups (a laptop's, or a phone's grid view): first the row's ID columns and its notes, then
 * every other column in the sheet's order. Without a note column it stays one list.
 */
export function noteGroups(fields: Field[], identity: string[] = []): FormSection[] {
  const first = fields.filter(f => identity.includes(f.key) || isNoteField(f.key))
  if (!first.some(f => isNoteField(f.key))) return [{ key: 'all', title: '', folded: false, fields }]
  const ids = first.filter(f => !isNoteField(f.key))
  const head = [...ids, ...first.filter(f => isNoteField(f.key))]
  return [
    { key: 'first', title: '', folded: false, fields: head },
    { key: 'all', title: '', folded: false, fields: fields.filter(f => !head.includes(f)) },
  ]
}
