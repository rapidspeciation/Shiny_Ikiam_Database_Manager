/** One butterfly in the Colecta list, before it is saved to Collection_data (and Insectary_data). */
export type Fate = 'insectario' | 'preservada' | 'liberada'
export interface Draft {
  key: string
  location: string
  species: string
  subspecies: string
  sex: '' | Sex
  fate: Fate
  time: string
  purpose: string
  notes: string
  insectaryId: string
  cam: string
  tube: string
  /** Preservation_medium of a preserved butterfly's tube. */
  medium: string
}
/** The Release_Collect value each fate is saved as; the list shows it exactly as the sheet will have it. */
export const FATES: Record<Fate, { label: string; value: string }> = {
  insectario: { label: 'Collected_Sent2Insectary', value: 'Collected_Sent2Insectary' },
  preservada: { label: 'Collected_Preserved', value: 'Collected_Preserved' },
  liberada: { label: 'Released_Unmarked', value: 'Released_Unmarked' },
}
/**
 * Sex as Collection_data's list allows it: the team writes NOT_COLLECTED when it
 * is not known and "female ?" / "male ?" when it is not sure.
 */
export const SEX_VALUES = ['female', 'male', 'female ?', 'male ?', 'NOT_COLLECTED'] as const
export type Sex = (typeof SEX_VALUES)[number]
/** Insectary_data's list has no "?" values: an unsure sex is saved as the sex. */
export const insectarySex = (sex: Sex | '') => (sex === 'female ?' ? 'female' : sex === 'male ?' ? 'male' : sex)

/** The list's columns, named as the sheet's columns. */
export const HEADERS: Record<Column, string> = {
  location: 'Collection_location',
  species: 'SPECIES',
  subspecies: 'Subspecies_Form',
  sex: 'Sex',
  fate: 'Release_Collect',
  time: 'Collection_time',
  insectaryId: 'Insectary_ID',
  cam: 'CAM_ID',
  tube: 'Tube_1_id',
  medium: 'Preservation_medium',
  purpose: 'Purpose',
  notes: 'Notes_Collection_data',
}

/**
 * The list's columns in the order they appear, which is also the order cells
 * pasted from a spreadsheet are spread over. Insectary_ID is for butterflies
 * sent to the insectary; CAM_ID, Tube_1_id and Preservation_medium for those
 * preserved in the field.
 */
export const COLUMNS = [
  'location',
  'species',
  'subspecies',
  'sex',
  'fate',
  'time',
  'insectaryId',
  'cam',
  'tube',
  'medium',
  'purpose',
  'notes',
] as const
export type Column = (typeof COLUMNS)[number]

/** Whether a column is filled for a row's Release_Collect (the others are left out of the sheet, or NA). */
export function applies(d: Pick<Draft, 'fate'>, column: Column): boolean {
  if (column === 'insectaryId') return d.fate === 'insectario'
  if (column === 'cam' || column === 'tube' || column === 'medium') return d.fate === 'preservada'
  return true
}
/** Why a column is not filled for a row (typing there says so). */
export const notApplicable = (column: Column) =>
  column === 'insectaryId'
    ? 'Insectary_ID es solo para Collected_Sent2Insectary: cambia primero Release_Collect'
    : `${HEADERS[column]} es solo para Collected_Preserved: cambia primero Release_Collect`
