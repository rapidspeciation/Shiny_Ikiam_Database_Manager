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
export const FATES: Record<Fate, { label: string; value: string }> = {
  insectario: { label: 'Al insectario', value: 'Collected_Sent2Insectary' },
  preservada: { label: 'Preservada', value: 'Collected_Preserved' },
  liberada: { label: 'Liberada', value: 'Released_Unmarked' },
}
export const SEXES = { female: '♀', male: '♂', NA: '?' } as const

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
