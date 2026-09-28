/** One butterfly in the Colecta list, before it is saved to Collection_data (and Insectary_data). */
export type Fate = 'insectario' | 'preservada' | 'liberada'
export interface Draft {
  key: string
  location: string
  species: string
  subspecies: string
  sex: '' | 'female' | 'male' | 'NA'
  fate: Fate
  time: string
  purpose: string
  notes: string
  insectaryId: string
  cam: string
  tube: string
}
/** The Release_Collect value each fate is saved as; the list shows it exactly as the sheet will have it. */
export const FATES: Record<Fate, { label: string; value: string }> = {
  insectario: { label: 'Collected_Sent2Insectary', value: 'Collected_Sent2Insectary' },
  preservada: { label: 'Collected_Preserved', value: 'Collected_Preserved' },
  liberada: { label: 'Released_Unmarked', value: 'Released_Unmarked' },
}
/** Sex as saved in the sheet. */
export const SEX_VALUES = ['female', 'male', 'NA'] as const

/** The list's columns, named as the sheet's columns. */
export const HEADERS: Record<Column, string> = {
  location: 'Collection_location',
  species: 'SPECIES',
  subspecies: 'Subspecies_Form',
  sex: 'Sex',
  fate: 'Release_Collect',
  time: 'Collection_time',
  ids: 'Insectary_ID / CAM_ID · Tube_1_id',
  purpose: 'Purpose',
  notes: 'Notes_Collection_data',
}

/**
 * The list's columns in the order they appear, which is also the order cells
 * pasted from a spreadsheet are spread over ("ids" is the Insectary ID, or
 * "CAM · tubo" for a preserved butterfly).
 */
export const COLUMNS = ['location', 'species', 'subspecies', 'sex', 'fate', 'time', 'ids', 'purpose', 'notes'] as const
export type Column = (typeof COLUMNS)[number]

/** What the "ids" column shows: the Insectary ID, or the CAM and tube of a preserved butterfly. */
export const idsText = (d: Draft) =>
  d.fate === 'insectario' ? d.insectaryId : d.fate === 'preservada' ? [d.cam, d.tube].filter(Boolean).join(' · ') : ''
