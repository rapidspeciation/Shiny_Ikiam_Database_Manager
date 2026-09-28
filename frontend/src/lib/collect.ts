import { parseFate, parseSex } from './paste'

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
  /** Who caught it and who identified it: the header's people by default, changeable per butterfly. */
  collector: string
  identifier: string
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
  collector: 'Collector',
  identifier: 'Identifier',
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
  'collector',
  'identifier',
] as const
export type Column = (typeof COLUMNS)[number]

/** Whether a column is filled for a row's Release_Collect (the others are left out of the sheet, or NA). */
export function applies(d: Pick<Draft, 'fate'>, column: Column): boolean {
  if (column === 'insectaryId') return d.fate === 'insectario'
  if (column === 'cam' || column === 'tube' || column === 'medium') return d.fate === 'preservada'
  return true
}
export const CAM_ID = /^CAM\d{4,}$/i
export const TUBE_ID = /^[A-Z]{2}\d{7,9}$/i
export const INSECTARY_ID = /^[0-9A-ZÑ]{2,6}$/i
const NA = /^(NA|N\/A)$/i

/**
 * Why a value typed or pasted does not fit an ID or time column, or null.
 * A block pasted with its columns shifted put a note, upper-cased, in
 * Tube_1_id; such values are left out and the notice names them.
 */
export function misfit(column: Column, text: string): string | null {
  const t = text.trim()
  if (!t || NA.test(t)) return null
  if (column === 'cam')
    // "CAM079895", or CAM and tube together ("CAM079895 · FS90415305 (Flash frozen)").
    return /CAM\d{4,}/i.test(t) ? null : 'no es un CAM_ID (CAM + número, p. ej. CAM079895)'
  if (column === 'tube') return TUBE_ID.test(t) ? null : 'no es un Tube_1_id (2 letras y 7 a 9 cifras, p. ej. FS90415305)'
  if (column === 'insectaryId')
    return INSECTARY_ID.test(t) ? null : 'no es un Insectary_ID (p. ej. N9D); para CAM y tubo usa CAM_ID y Tube_1_id'
  if (column === 'time') return /^\d{1,2}[:.h]?\d{2}$/.test(t) ? null : 'no es una hora (hh:mm)'
  if (column === 'sex') return parseSex(t) ? null : 'no es un Sex (female, male, female ?, male ?, NOT_COLLECTED)'
  if (column === 'fate') return parseFate(t) ? null : 'no es un Release_Collect'
  return null
}

/** Why a column is not filled for a row (typing there says so). */
export const notApplicable = (column: Column) =>
  column === 'insectaryId'
    ? 'Insectary_ID es solo para Collected_Sent2Insectary: cambia primero Release_Collect'
    : `${HEADERS[column]} es solo para Collected_Preserved: cambia primero Release_Collect`

/** What a Colecta list will save, to read over before saving (a wrong day, sexes swapped). */
export interface CollectSummary {
  places: string[]
  insectary: { female: number; male: number; other: number }
  preserved: number
  released: number
  /** First and last CAM of the preserved butterflies, and whether they run without gaps. */
  cams: { first: string; last: string; consecutive: boolean } | null
  species: { name: string; female: number; male: number; other: number }[]
}
export function summarize(drafts: Draft[]): CollectSummary {
  const sexOf = (d: Draft) => (d.sex.startsWith('female') ? 'female' : d.sex.startsWith('male') ? 'male' : 'other')
  const insectary = { female: 0, male: 0, other: 0 }
  const species = new Map<string, { name: string; female: number; male: number; other: number }>()
  for (const d of drafts) {
    if (d.fate === 'insectario') insectary[sexOf(d)]++
    const name = [d.species, d.subspecies].filter(Boolean).join(' ') || '(sin especie)'
    const s = species.get(name) || { name, female: 0, male: 0, other: 0 }
    s[sexOf(d)]++
    species.set(name, s)
  }
  const number = (id: string) => Number(/(\d+)$/.exec(id)?.[1])
  const cams = drafts
    .filter(d => d.fate === 'preservada' && d.cam)
    .map(d => d.cam)
    .sort((a, b) => number(a) - number(b))
  const total = (s: { female: number; male: number; other: number }) => s.female + s.male + s.other
  return {
    places: [...new Set(drafts.map(d => d.location).filter(Boolean))],
    insectary,
    preserved: drafts.filter(d => d.fate === 'preservada').length,
    released: drafts.filter(d => d.fate === 'liberada').length,
    cams: cams.length
      ? { first: cams[0], last: cams.at(-1)!, consecutive: number(cams.at(-1)!) - number(cams[0]) === cams.length - 1 }
      : null,
    species: [...species.values()].sort((a, b) => total(b) - total(a) || a.name.localeCompare(b.name)),
  }
}
