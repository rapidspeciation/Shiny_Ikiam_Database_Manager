import { isBlank } from './cells'
import { deathCells, searchKey, type Entry } from './deaths'
import { appendNote, noteDay } from './clutches'
import type { CellValue, TableRow } from './types'

/**
 * Censo (server/census.mjs): everyone marks the butterflies of one species
 * seen alive as they are released one by one; those not seen die as
 * disappeared on the census day. These are the census's shapes and what the
 * tab works out from them: progress, the butterflies not seen yet, the
 * findings and the cells of each disappearance (the notebook's lines: lib/paperNotebook.ts).
 */

/** The Death_cause list's value for a butterfly not found. */
export const DISAPPEARED = 'Disappearance'
/** A butterfly this long in the insectary is flagged in the review: it may have died long ago without being recorded. */
export const OLD_DAYS = 365

export interface RosterEntry {
  /** A sheet row, or staged:<clientId> for a butterfly emerged and not yet in Google Sheets. */
  recordId: string
  id: string
  /** Its row in the sheet (= the notebook's order); null for one with no pre-made row. */
  row: number | null
  species: string
  sex: string
  clutch: string
  /** Intro2Insectary_date: emerged (reared) or brought in (wild-caught). */
  entered: number | null
  wild: boolean
  staged?: boolean
  /** Once finished: what happened to it. */
  status?: 'seen' | 'excluded' | 'disappeared'
  note?: string
  doubt?: Doubt
}
export type MarkKind = 'seen' | 'excluded' | 'unknown'
export type Doubt = 'sex' | 'species' | 'other'
export interface CensusMark {
  id: string
  recordId: string | null
  insectaryId: string
  kind: MarkKind
  /** The butterfly's species in the sheet when it was marked. */
  species: string | null
  doubt: Doubt | null
  note: string | null
  actor: string
  actorName: string
  createdAt: string
  updatedAt: string
  /** Shown before the server answered (this device's tap). */
  sending?: boolean
}
export interface CensusSummary {
  id: string
  species: string
  /** ISO date. */
  day: string
  status: 'open' | 'finished' | 'cancelled'
  expected: number
  createdByName: string
  createdAt: string
  finishedAt: string | null
  finishedByName: string | null
  people: string[]
  counts: {
    roster: number
    seen: number
    excluded: number
    disappeared: number
    otherSpecies: number
    offList: number
    unknown: number
    doubts: number
  }
  /** Where its disappearances are: none to write, kept in the app, being written, in Google Sheets. */
  /** Where its disappearances are: kept in the app (staged), waiting for Google (queued, staged saving off), being written, written, or refused (failed). */
  deaths: null | 'none' | 'staged' | 'sending' | 'queued' | 'written' | 'failed'
  notebookAt: string | null
  notebookByName: string | null
}
export interface CensusDetail {
  census: CensusSummary
  roster: RosterEntry[]
  marks: CensusMark[]
  stamp: number
}
export interface CensusOverview {
  species: { species: string; alive: number; subspecies?: { species: string; alive: number }[] }[]
  open: CensusSummary[]
  history: CensusSummary[]
  stamp: number
}

const speciesKey = (v: unknown) =>
  String(v ?? '')
    .trim()
    .replace(/\s+/g, ' ')
    .toLowerCase()
/**
 * A butterfly's species (`b`) within the census's (`a`), to as many words as the census names: a census of
 * «Mechanitis polymnia» takes every subspecies; one named to the subspecies, only that one.
 */
export function sameSpecies(a: unknown, b: unknown) {
  const words = speciesKey(a).split(' ').filter(Boolean)
  return words.length > 0 && speciesKey(b).split(' ').slice(0, words.length).join(' ') === words.join(' ')
}

/** The mark of a butterfly of the list: by its row; one marked while it was in the app (staged:…) by its ID. */
export function markFinder(marks: CensusMark[]): (b: { recordId: string; id: string }) => CensusMark | null {
  const byRecord = new Map<string, CensusMark>()
  const byId = new Map<string, CensusMark>()
  for (const m of marks) {
    if (!m.recordId) continue
    byRecord.set(m.recordId, m)
    if (m.recordId.startsWith('staged:')) byId.set(searchKey(m.insectaryId), m)
  }
  return b => byRecord.get(b.recordId) ?? byId.get(searchKey(b.id)) ?? null
}

/** How far a census is: seen and left out of the list, those left, of how many. */
export function progressOf(roster: RosterEntry[], marks: CensusMark[]) {
  const markOf = markFinder(marks)
  let seen = 0
  let excluded = 0
  for (const b of roster) {
    const m = markOf(b)
    if (m?.kind === 'seen') seen++
    else if (m?.kind === 'excluded') excluded++
  }
  return { seen, excluded, left: roster.length - seen - excluded, total: roster.length }
}

/** The butterflies of the list with no mark yet (in the sheet's order). */
export function notSeen(roster: RosterEntry[], marks: CensusMark[]): RosterEntry[] {
  const markOf = markFinder(marks)
  return roster.filter(b => !markOf(b))
}

/** Something to look at after a census, kept for review (never corrected here). */
export interface Finding {
  mark: CensusMark
  kind: 'doubt' | 'otherSpecies' | 'offList' | 'unknown'
}
/**
 * The findings: a butterfly seen whose sex or species looked different (a
 * doubt), one of another species found in this cage, one of the species seen
 * but not on the list (recorded dead, or a repeated ID), an ID read that is in
 * no row.
 */
export function findingsOf(species: string, roster: RosterEntry[], marks: CensusMark[]): Finding[] {
  const listed = new Set(roster.map(b => b.recordId))
  const markOf = markFinder(marks)
  const ofList = new Set(roster.map(b => markOf(b)).filter((m): m is CensusMark => !!m))
  const out: Finding[] = []
  for (const mark of marks) {
    if (mark.kind === 'unknown') out.push({ mark, kind: 'unknown' })
    else if (mark.kind !== 'seen') continue
    else if (!sameSpecies(mark.species, species)) out.push({ mark, kind: 'otherSpecies' })
    else if (!ofList.has(mark) && !listed.has(mark.recordId ?? '')) out.push({ mark, kind: 'offList' })
    else if (mark.doubt) out.push({ mark, kind: 'doubt' })
  }
  return out
}

export interface DeathEdit {
  id: string
  values: Record<string, CellValue>
  expected: Record<string, CellValue>
}
const NOTES = 'Notes_Insectary_data'
/** The note on a butterfly the census did not find (the team's words for it). */
export const CENSUS_NOTE = 'Disappeared in census'

/**
 * The cells of each butterfly not seen, as Muertes writes a death not preserved
 * (lib/deaths.ts deathCells: Death_date, Death_cause Disappearance, and for a
 * row without CAM or tube the NA / NOT_COLLECTED block), read from the sheet
 * with everyone's entries kept in the app (`rows`), with what each cell holds
 * now (what the server checks it still holds). Returns the edits and the
 * butterflies whose row is not loaded.
 */
export function disappearanceEdits(
  missing: RosterEntry[],
  rows: Map<string, TableRow>,
  serial: number,
  sign?: { today: number; initials: string },
): { edits: DeathEdit[]; absent: string[] } {
  const edits: DeathEdit[] = []
  const absent: string[] = []
  const get = (row: TableRow, field: string): CellValue => row.values[field] ?? null
  for (const b of missing) {
    const row = rows.get(b.recordId)
    if (!row) {
      absent.push(b.id)
      continue
    }
    const cells = deathCells(row, get, { serial, cause: DISAPPEARED, notPreserved: true })
    const values: Record<string, CellValue> = {}
    const expected: Record<string, CellValue> = {}
    for (const c of cells) {
      values[c.field] = c.value
      // Exactly what the cell holds (the server compares it with its copy): '' and NA as they are.
      expected[c.field] = get(row, c.field)
    }
    // How it was found: «d/m/yy INI: Disappeared in census» after the notes it has (the census day in it when not today).
    if (sign) {
      const text = sign.today === serial ? CENSUS_NOTE : `${CENSUS_NOTE} of ${noteDay(serial)}`
      values[NOTES] = appendNote(get(row, NOTES), text, sign.today, sign.initials)
      expected[NOTES] = get(row, NOTES)
    }
    edits.push({ id: b.recordId, values, expected })
  }
  return { edits, absent }
}

/**
 * Every butterfly with an Insectary ID, repeated IDs included (an old series'
 * B9D dead and this year's alive), to match what is read on a wing.
 */
export function censusIndex(rows: TableRow[]): Entry[] {
  const out: Entry[] = []
  for (const row of rows) {
    const raw = row.values.Insectary_ID
    if (!row.observed || isBlank(raw)) continue
    const id = String(raw).trim()
    out.push({ row, id, key: searchKey(id), samples: [], order: row.row })
  }
  return out
}

/** Days from `from` to `to` (serial dates), or null. */
export const daysBetween = (from: number | null, to: number) => (from === null ? null : Math.max(0, to - from))
