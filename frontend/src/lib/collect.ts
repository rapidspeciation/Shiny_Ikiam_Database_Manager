import { parseFate, parseSex } from './paste'
import { t } from './i18n'

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
  /** Butterfly_weight in g of a preserved butterfly (blank: not weighed, saved as NA). */
  weight?: string
  /** Preserved_dead_alive of a preserved butterfly: Dead when found dead or dying (blank: Alive). */
  deadAlive?: '' | 'Alive' | 'Dead'
  /** Who caught it and who identified it: the header's people by default, changeable per butterfly. */
  collector: string
  identifier: string
  /** Weather when it was caught: the header's by default, changeable per butterfly (it changes during the day). */
  rainfall: string
  cloud: string
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
  weight: 'Butterfly_weight',
  deadAlive: 'Preserved_dead_alive',
  purpose: 'Purpose',
  notes: 'Notes_Collection_data',
  collector: 'Collector',
  identifier: 'Identifier',
  rainfall: 'Rainfall',
  cloud: 'Cloud_cover',
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
  'weight',
  'deadAlive',
  'purpose',
  'notes',
  'collector',
  'identifier',
  'rainfall',
  'cloud',
] as const
export type Column = (typeof COLUMNS)[number]

/** Whether a column is filled for a row's Release_Collect (the others are left out of the sheet, or NA). */
export function applies(d: Pick<Draft, 'fate'>, column: Column): boolean {
  if (column === 'insectaryId') return d.fate === 'insectario'
  if (column === 'cam' || column === 'tube' || column === 'medium' || column === 'weight' || column === 'deadAlive')
    return d.fate === 'preservada'
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
  const v = text.trim()
  if (!v || NA.test(v)) return null
  if (column === 'cam')
    // "CAM079895", or CAM and tube together ("CAM079895 · FS90415305 (Flash frozen)").
    return /CAM\d{4,}/i.test(v) ? null : t('no es un CAM_ID (CAM + número, p. ej. CAM079895)')
  if (column === 'tube') return TUBE_ID.test(v) ? null : t('no es un Tube_1_id (2 letras y 7 a 9 cifras, p. ej. FS90415305)')
  if (column === 'insectaryId')
    return INSECTARY_ID.test(v) ? null : t('no es un Insectary_ID (p. ej. N9D); para CAM y tubo usa CAM_ID y Tube_1_id')
  if (column === 'time') return /^\d{1,2}[:.h]?\d{2}$/.test(v) ? null : t('no es una hora (hh:mm)')
  if (column === 'weight') return parseWeight(v) !== null ? null : t('no es un peso en gramos (p. ej. 0.152)')
  if (column === 'deadAlive') return /^(alive|dead|viva|muerta)$/i.test(v) ? null : t('no es Alive ni Dead')
  if (column === 'sex') return parseSex(v) ? null : t('no es un Sex (female, male, female ?, male ?, NOT_COLLECTED)')
  if (column === 'fate') return parseFate(v) ? null : t('no es un Release_Collect')
  return null
}

/** Why a column is not filled for a row (typing there says so). */
export const notApplicable = (column: Column) =>
  column === 'insectaryId'
    ? t('Insectary_ID es solo para Collected_Sent2Insectary: cambia primero Release_Collect')
    : t('{column} es solo para Collected_Preserved: cambia primero Release_Collect', { column: HEADERS[column] })

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
    const name = [d.species, d.subspecies].filter(Boolean).join(' ') || t('(sin especie)')
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

/** A butterfly nothing was written in yet (its ID, CAM and tube are filled in by the app). */
export const isEmptyDraft = (d: Draft) => !d.species && !d.subspecies && !d.sex && !d.time && !d.purpose && !d.notes

/**
 * Values of a column, the latest first: the order they were last used in the
 * last `window` rows (a trip goes back to the same places and species). Blank
 * and NA left out.
 */
export function rankByRecency(values: unknown[], { window = 400 } = {}): string[] {
  const last = new Map<string, number>()
  values.slice(-window).forEach((v, i) => {
    const s = v === null || v === undefined ? '' : String(v).trim()
    if (s && !/^(NA|N\/A)$/i.test(s)) last.set(s, i)
  })
  return [...last.keys()].sort((a, b) => last.get(b)! - last.get(a)!)
}

/** Species of the list with how many of each, by sex and fate (the cards' running totals). */
export interface SpeciesTotal {
  species: string
  total: number
  female: number
  male: number
  other: number
  insectario: number
  preservada: number
  liberada: number
}
export function speciesTotals(drafts: Draft[]): SpeciesTotal[] {
  const out = new Map<string, SpeciesTotal>()
  for (const d of drafts) {
    if (isEmptyDraft(d)) continue
    const name = [d.species, d.subspecies].filter(Boolean).join(' ') || t('(sin especie)')
    const s = out.get(name) || { species: name, total: 0, female: 0, male: 0, other: 0, insectario: 0, preservada: 0, liberada: 0 }
    s.total++
    s[d.sex.startsWith('female') ? 'female' : d.sex.startsWith('male') ? 'male' : 'other']++
    s[d.fate]++
    out.set(name, s)
  }
  return [...out.values()].sort((a, b) => b.total - a.total || a.species.localeCompare(b.species))
}

/** Adds a quick phrase after what a note already says ("Worn wings; Photo taken"), once. */
export function addPhrase(note: string, phrase: string) {
  const text = note.trim()
  if (!text) return phrase
  return text.split(/;\s*/).includes(phrase) ? text : `${text}; ${phrase}`
}

/**
 * The team's phrases for Notes_Collection_data (data-rules notes.md), in
 * English. "Preserved dead ~2h" goes with Preserved_dead_alive Dead.
 */
export const COLLECT_NOTE_PHRASES = [
  'Sexed by genitalia',
  'Preserved dead ~2h',
  'Recapture',
  'Butterfly sent to insectary for live photos',
  "The scale's battery ran out, so the individual could not be weighed",
] as const

/** A weight typed in grams ("0.152", "0,152", ".15 g") as the sheet's number, or null; blank or NA is ''. */
export function parseWeight(text: string): number | '' | null {
  const v = text.trim().replace(/\s*g$/i, '').replace(',', '.')
  if (!v || /^(NA|N\/A)$/i.test(v)) return ''
  if (!/^\d*\.?\d+$/.test(v)) return null
  const n = Number(v)
  // A butterfly weighs well under 5 g; more is a typo (milligrams, a CAM pasted a column off).
  return Number.isFinite(n) && n > 0 && n < 5 ? Math.round(n * 1000) / 1000 : null
}

/** A note already dated and signed as the team writes them ("29/9/26 AA: …", "22Ago26 PAS …", "16/9/2026 AA:"). */
const SIGNED = /^\s*(\d{1,2}[/.-]\d{1,2}[/.-]\d{2,4}|\d{1,2}\s*[A-Za-z]{3}\s*\d{2,4})\s+[A-Z]{2,4}\b/
/** The note as saved: "d/m/yy INI: text" (the day it is written, who writes it), unless already signed. */
export function signNote(text: string, day: string, initials: string): string {
  const note = text.trim()
  if (!note || SIGNED.test(note)) return note
  return `${day} ${initials}: ${note}`
}

/** "PAS - Patricio Salazar" → "PAS" (a person's code in the Lists). */
export const personCode = (person: string) => person.split(' - ')[0].trim()
/** "S&C_(sun_&_cloud_patches)" → { code: "S&C", words: "sun & cloud patches" }. */
export function weatherLabel(value: string) {
  const m = /^(.*?)_\((.*)\)$/.exec(value.trim())
  return m ? { code: m[1], words: m[2].replace(/_/g, ' ') } : { code: value, words: '' }
}

/**
 * A name typed as in Insectary_data ("Methona confusa psamathe") split into
 * Collection_data's SPECIES and Subspecies_Form, when the sheet lists the first
 * two words as a species ("methona confusa" → "Methona confusa"). Null when the
 * name is listed as it is, is a hybrid, or its species is not listed.
 */
export function splitSpecies(typed: string, species: string[]): { species: string; form: string } | null {
  const words = typed.trim().split(/\s+/)
  if (words.length < 3 || / x |\bVS\b/i.test(typed)) return null
  const name = typed.trim().toLowerCase()
  if (species.some(s => s.toLowerCase() === name)) return null
  const head = words.slice(0, 2).join(' ').toLowerCase()
  const listed = species.find(s => s.toLowerCase() === head)
  return listed ? { species: listed, form: words.slice(2).join(' ') } : null
}

/** One choice of the species search: a species, or a species with a form the team uses ("Mechanitis messenoides deceptus"). */
export interface SpeciesEntry {
  species: string
  form: string
  label: string
}
/**
 * The species search's entries, best first: the species and forms of this list
 * (most recent first), the latest collected, then the rest of the sheet's
 * species; each species followed by its forms (most used first).
 */
export function speciesEntries(
  ordered: string[],
  formsOf: (species: string) => string[],
  listed: { species: string; subspecies: string }[] = [],
): SpeciesEntry[] {
  const out: SpeciesEntry[] = []
  const seen = new Set<string>()
  const push = (species: string, form: string) => {
    const label = [species, form].filter(Boolean).join(' ')
    if (!species || seen.has(label)) return
    seen.add(label)
    out.push({ species, form, label })
  }
  for (const d of [...listed].reverse()) if (d.species) push(d.species, d.subspecies)
  for (const species of ordered) {
    push(species, '')
    for (const form of formsOf(species)) push(species, form)
  }
  return out
}
/**
 * The entries matching what was typed: every word typed begins a word of the
 * name ("mech mess dec", "pol. eurydice", "deceptus", "salapia"), the order of
 * `entries` kept; an exact name first. At most `limit`.
 */
export function searchSpecies(entries: SpeciesEntry[], typed: string, limit = 60): SpeciesEntry[] {
  const words = typed
    .toLowerCase()
    .split(/[\s.]+/)
    .filter(Boolean)
  if (!words.length) return entries.slice(0, limit)
  const exact: SpeciesEntry[] = []
  const hits: SpeciesEntry[] = []
  const query = words.join(' ')
  for (const e of entries) {
    const name = e.label.toLowerCase()
    const parts = name.split(/\s+/)
    if (name === query) exact.push(e)
    else if (words.every(w => parts.some(p => p.startsWith(w)))) hits.push(e)
    if (exact.length + hits.length >= limit * 3) break
  }
  return [...exact, ...hits].slice(0, limit)
}
