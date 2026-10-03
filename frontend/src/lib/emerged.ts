import { isBlank } from './cells'
import { appendNote, appendTerm, countValue, noteDay, totalOf, type Count } from './clutches'
import { clutchSettings } from './clutchSettings'
import { serialFromIso } from './dates'
import { deathCells, KILLED } from './deaths'
import type { CellValue, TableRow } from './types'

/**
 * Registering what emerged from a clutch (Emergidos): one new Insectary_data
 * row per butterfly, written into the next pre-made row (its Insectary ID is
 * on the wing), with the values the data rules give an emergence, a death on
 * the day it emerged (deformed, dead, killed and preserved), or an egg or
 * larva preserved; and what the clutch's row in Insectary_stocks gets (the
 * adults as one more term of NUMBER OF ADULTS, the first emergence date, the
 * larvae or eggs preserved taken off their count as the team's setting says,
 * with a note). Shared by the
 * cards and their tests; nothing here touches the store.
 */
export const MODULE = 'Insectary_data'
export const STOCKS = 'Insectary_stocks'
/** The Mechanitis messenoides stock lines: their Stock_of_origin is the clutch's subspecies. */
export const STOCK_ORIGINS = ['deceptus', 'messenoides', 'intermedia']
export const CROSS_PURPOSE = 'F1/F2 mutation rate'

/** An adult that emerged, or an egg or larva preserved from the clutch (since Sep 2026). */
export type Kind = 'adult' | 'young'
/** What became of an adult on its emergence day. */
export type Fate = 'alive' | 'deformed' | 'dead' | 'preserved'
export type Sex = 'female' | 'male' | 'NA'

/** One butterfly (or egg, larva) being registered: a card. */
export interface Draft {
  /** Stable key of the card (its Insectary ID can change). */
  key: string
  /** The Insectary ID written on its wing: a free pre-made row. */
  id: string
  /** CLUTCH NUMBER as the Insectary_stocks row writes it. */
  clutch: string
  /** The emergence day (adults) or the preservation day (eggs, larvae), ISO. */
  date: string
  kind: Kind
  sex: Sex
  fate: Fate
  /** What emerged when it is not the clutch's species ('' = the clutch's, left to the formula). */
  species: string
  /** LIFESTAGE of an egg or larva. */
  stage: string
  /** An egg or larva found dead (Other / Dead) rather than killed to be preserved. */
  foundDead: boolean
  /** A note added to Notes_Insectary_data, dated and signed (English). */
  note: string
  cam: string
  tube: string
}

/** LIFESTAGE values, as the sheet writes them (used only for eggs and larvae). */
export const LIFESTAGES = ['Egg', '1st instar larva', '2nd instar larva', '3rd instar larva', '4th instar larva', '5th instar larva', 'Pre-pupa']
/** The protocol preserves F1 larvae at the 4th instar. */
export const DEFAULT_STAGE = '4th instar larva'

/** Phrases written in the Emergidos notebook's notes (English, as in the sheet): quick buttons. */
export const EMERGED_NOTE_PHRASES = [
  'Deformed wings, can fly',
  "Deformed wings, can't fly",
  'Deformed abdomen',
  'Deformed antennae',
  "Couldn't emerge well",
  'Emerged dead',
  'Very small',
]

// --- Species and stock

export const isHybrid = (species: string) => / x |\bVS\b/i.test(species)

/** Stock_of_origin for a clutch's species: its subspecies for the messenoides stock lines, else NA. */
export function stockOrigin(clutchSpecies: string): string {
  const words = clutchSpecies.trim().split(/\s+/)
  const origin = words.slice(0, 2).join(' ').toLowerCase() === 'mechanitis messenoides' ? words[2]?.toLowerCase() : ''
  return origin && STOCK_ORIGINS.includes(origin) ? origin : 'NA'
}

/**
 * What may emerge from a clutch instead of its species: the other subspecies
 * of the same species seen in the sheet (eurydice from a proceriformis clutch,
 * intermedia from a deceptus one), hybrids only for a hybrid clutch. The
 * clutch's own species first.
 */
export function siblingSpecies(clutchSpecies: string, known: Iterable<string>): string[] {
  const species = clutchSpecies.trim()
  const words = species.split(/\s+/)
  if (words.length < 2) return species ? [species] : []
  const stem = words.slice(0, 2).join(' ').toLowerCase()
  const hybrid = isHybrid(species)
  const seen = new Set<string>()
  for (const value of known) {
    const v = String(value ?? '').trim()
    if (v && v !== species && v.toLowerCase().startsWith(stem + ' ') && (hybrid || !isHybrid(v))) seen.add(v)
  }
  return [species, ...[...seen].sort()]
}

/** The species a card shows: its own, else the clutch's. */
export const speciesOf = (d: Pick<Draft, 'species'>, clutchSpecies: string) => d.species || clutchSpecies

/** The clutch's species when it is a real taxon (field eggs have NA: what emerged must be typed). */
export const knownSpecies = (value: CellValue | undefined) => {
  const s = isBlank(value) ? '' : String(value).trim()
  return /^(NA|N\/A)$/i.test(s) ? '' : s
}

// --- Insectary IDs

const norm = (id: string) => id.trim().toUpperCase()

/**
 * The ID the next card gets: after the last ID the cards hold (in the sheet's
 * order of free pre-made rows), else the first free one after the last row
 * used (`first`); never one a card holds. A card whose ID was changed to an
 * earlier empty row (the wing says H0B) is followed by H1B. Null when the
 * pre-made rows have run out.
 */
export function nextId(order: string[], first: string, held: string[]): string | null {
  const taken = new Set(held.map(norm))
  const index = new Map(order.map((id, i) => [norm(id), i]))
  const positions = held.map(id => index.get(norm(id))).filter((i): i is number => i !== undefined)
  const start = positions.length ? Math.max(...positions) + 1 : (index.get(norm(first)) ?? 0)
  for (let i = start; i < order.length; i++) if (!taken.has(norm(order[i]))) return order[i]
  return null
}

/**
 * Free IDs left between the cards' IDs (in the sheet's order): a skipped ID on
 * the wings ("se saltaron del G9E al H1E") leaves an empty row in the sheet.
 */
export function skippedIds(order: string[], held: string[]): string[] {
  const taken = new Set(held.map(norm))
  const index = new Map(order.map((id, i) => [norm(id), i]))
  const positions = held.map(id => index.get(norm(id))).filter((i): i is number => i !== undefined)
  if (positions.length < 2) return []
  const lo = Math.min(...positions)
  const hi = Math.max(...positions)
  return order.slice(lo, hi + 1).filter(id => !taken.has(norm(id)))
}

/** A suffixed ID (W0B.1): the same ID written on a second butterfly; the server places its row. */
export const isSuffixed = (id: string) => /^[A-Z0-9]+\.[1-9]\d*$/.test(norm(id))

export type IdProblem = '' | 'empty' | 'repeated' | 'used' | 'not-free'
/**
 * What is wrong with a card's ID: empty, on another card, already a butterfly
 * of the sheet (`used`), or not a free pre-made row (`free`). A suffixed ID
 * (W0B.1) goes through: the server checks its group.
 */
export function idProblem(id: string, others: string[], free: Set<string>, used: Set<string>): IdProblem {
  const key = norm(id)
  if (!key) return 'empty'
  if (others.some(o => norm(o) === key)) return 'repeated'
  if (used.has(key)) return 'used'
  if (isSuffixed(key)) return ''
  return free.has(key) ? '' : 'not-free'
}

// --- The row a card writes

export interface RowContext {
  /** CLUTCH NUMBER exactly as the stocks row holds it (a number, or text such as "994(6)"). */
  clutchValue: CellValue
  /** The clutch's species ('' when unknown or NA). */
  clutchSpecies: string
  /** The clutch's Generation (F1, F2, Backcross, NA). */
  generation: string
  /** Formula columns of the pre-made row: never written (SPECIES only when it differs). */
  formulas: string[]
  /** Today (a serial), for the note's date, and who signs it. */
  today: number
  initials: string
  /** The medium of a preserved body (Flash frozen by default). */
  medium: string
}

/** The note a card adds by default: an egg or larva preserved, as the team writes it ("Preserved alive 3rd instar"). */
export function youngNote(stage: string, foundDead: boolean): string {
  const what = stage === 'Egg' ? 'egg' : stage === 'Pre-pupa' ? 'prepupa' : stage.replace(/ larva$/, '')
  return foundDead ? `Found dead, ${what}` : `Preserved alive ${what}`
}

/**
 * The values of the new Insectary_data row for a card. An emergence:
 * Reared, the clutch, Stock_of_origin (the clutch's subspecies for the
 * messenoides stock, else NA), Sex, Intro2Insectary_date; SPECIES only when
 * what emerged differs from the clutch (else the formula stays); a hybrid
 * gets Research_purpose F1/F2 mutation rate. Deformed or dead on its
 * emergence day: that day as Death_date, Deformed or Unknown, and the
 * not-preserved block. Killed and preserved: its CAM and tube, whole organism.
 * An egg or larva: Sex NOT_COLLECTED, Intro2Insectary_date NA, its LIFESTAGE,
 * preserved (Killed_Preserved and Alive, or Other and Dead when found dead),
 * F1/F2 mutation rate. Formula columns are left alone.
 */
export function draftValues(d: Draft, ctx: RowContext): Record<string, CellValue> {
  const serial = serialFromIso(d.date)
  const own = d.species.trim()
  const species = own && own !== ctx.clutchSpecies ? own : ''
  const hybrid = isHybrid(ctx.clutchSpecies) || isHybrid(species)
  const cross = hybrid || /^(F1|F2|Backcross)$/i.test(ctx.generation.trim())
  const values: Record<string, CellValue> = {
    Insectary_ID: norm(d.id),
    Wild_Reared: 'Reared',
    'CLUTCH NUMBER': ctx.clutchValue,
    Stock_of_origin: stockOrigin(ctx.clutchSpecies),
  }
  if (species) values.SPECIES = species
  let death: Parameters<typeof deathCells>[2] | null = null
  let note = d.note.trim()
  if (d.kind === 'young') {
    values.Sex = 'NOT_COLLECTED'
    values.Intro2Insectary_date = 'NA'
    values.LIFESTAGE = d.stage || DEFAULT_STAGE
    values.Research_purpose = CROSS_PURPOSE
    death = {
      serial,
      cause: d.foundDead ? 'Other' : KILLED,
      notPreserved: false,
      preserve: { cam: norm(d.cam), tube: norm(d.tube), medium: ctx.medium },
    }
    if (!note) note = youngNote(values.LIFESTAGE as string, d.foundDead)
  } else {
    values.Sex = d.sex
    values.Intro2Insectary_date = serial
    if (d.fate === 'alive') {
      if (hybrid) values.Research_purpose = CROSS_PURPOSE
    } else if (d.fate === 'preserved') {
      values.Research_purpose = cross ? CROSS_PURPOSE : 'NA'
      death = { serial, cause: KILLED, notPreserved: false, preserve: { cam: norm(d.cam), tube: norm(d.tube), medium: ctx.medium } }
    } else {
      values.Research_purpose = hybrid ? CROSS_PURPOSE : 'NA'
      death = { serial, cause: d.fate === 'deformed' ? 'Deformed' : 'Unknown', notPreserved: true }
    }
  }
  if (death) {
    // The same cells Muertes writes for a death (lib/deaths.ts), on the new row.
    const row: TableRow = { id: d.key, row: 0, version: 0, observed: false, values: { ...values }, formulas: ctx.formulas }
    const get = (_: TableRow, field: string) => (field in values ? values[field] : null)
    for (const cell of deathCells(row, get, death)) values[cell.field] = cell.value
  }
  if (note) values.Notes_Insectary_data = appendNote(null, note, ctx.today, ctx.initials)
  // The pre-made row's formulas stay (SPECIES only when what emerged differs; the server checks).
  for (const field of ctx.formulas) if (field !== 'Insectary_ID' && field !== 'SPECIES') delete values[field]
  return values
}

/** A card that needs a CAM and a tube (killed and preserved, or an egg or larva). */
export const preserving = (d: Pick<Draft, 'kind' | 'fate'>) => d.kind === 'young' || d.fate === 'preserved'
/** An adult that emerged (counted in NUMBER OF ADULTS), whatever became of it. */
export const isAdult = (d: Pick<Draft, 'kind'>) => d.kind === 'adult'

// --- The clutch's row in Insectary_stocks

/** What one save adds to a clutch: adults per emergence day, eggs and larvae preserved per day. */
export interface ClutchTally {
  clutch: string
  /** Adults by emergence day (serial → how many), in day order. */
  adults: [number, number][]
  eggs: number
  larvae: number
  /** Of those, the ones found dead (preserved, but a death: always taken off the count). */
  eggsDead: number
  larvaeDead: number
  /** The days eggs or larvae were preserved (serials). */
  preservedOn: number[]
}
/** The cards of each clutch, counted for its stocks row (in the order the clutches first appear). */
export function tallies(drafts: Draft[]): ClutchTally[] {
  const out = new Map<string, ClutchTally & { byDay: Map<number, number> }>()
  for (const d of drafts) {
    const serial = serialFromIso(d.date)
    if (serial === null) continue
    let t = out.get(d.clutch)
    if (!t) out.set(d.clutch, (t = { clutch: d.clutch, adults: [], eggs: 0, larvae: 0, eggsDead: 0, larvaeDead: 0, preservedOn: [], byDay: new Map() }))
    if (d.kind === 'adult') t.byDay.set(serial, (t.byDay.get(serial) ?? 0) + 1)
    else {
      if (d.stage === 'Egg') {
        t.eggs++
        if (d.foundDead) t.eggsDead++
      } else {
        t.larvae++
        if (d.foundDead) t.larvaeDead++
      }
      if (!t.preservedOn.includes(serial)) t.preservedOn.push(serial)
    }
  }
  return [...out.values()].map(({ byDay, ...t }) => ({ ...t, adults: [...byDay].sort((a, b) => a[0] - b[0]), preservedOn: t.preservedOn.sort((a, b) => a - b) }))
}

/** One cell the clutch's row gets, with what it showed before (a sum: its formula, which the save compares). */
export interface StockCell {
  field: string
  value: CellValue
  before: CellValue
}
export interface StockPlan {
  cells: StockCell[]
  /** Why a count could not take its subtraction (it would go below 0): it is left as it is. */
  skipped: string[]
}
/**
 * What the clutch's row gets for a save's cards: each emergence day's adults
 * as one more term of NUMBER OF ADULTS (=2+2 → =2+2+3), EMERGENCE DATE when
 * empty (the first day); eggs and larvae preserved said in NOTES ("3 larvae
 * preserved 2/10") and taken off NUMBER OF EGGS / NUMBER OF LARVAE (−3) as
 * the team's setting says (`subtractPreserved`, Clutches' settings: yes by
 * default); those found dead are a death, taken off always. `count(field)`: a
 * count as the person sees it; `value(field)`: the cell as the sheet has it
 * (a sum as its formula).
 */
export function stockPlan(
  tally: ClutchTally,
  count: (field: string) => Count,
  value: (field: string) => CellValue,
  { today, initials, subtractPreserved = clutchSettings.subtractPreserved }: { today: number; initials: string; subtractPreserved?: boolean },
): StockPlan {
  const cells: StockCell[] = []
  const skipped: string[] = []
  const addTerms = (field: string, terms: number[]) => {
    const c = count(field)
    if (c.text || c.na) {
      skipped.push(field)
      return
    }
    let now = c.terms
    for (const n of terms) {
      const r = appendTerm(now, n)
      if (!r.ok) {
        skipped.push(field)
        return
      }
      now = r.terms
    }
    if (now !== c.terms) cells.push({ field, value: countValue(now), before: value(field) })
  }
  if (tally.adults.length) {
    addTerms('NUMBER OF ADULTS', tally.adults.map(([, n]) => n))
    const first = tally.adults[0][0]
    if (isBlank(value('EMERGENCE DATE'))) cells.push({ field: 'EMERGENCE DATE', value: first, before: value('EMERGENCE DATE') })
  }
  const parts: string[] = []
  // Taken off the count: all of them, or (preserved ones kept counted) only those found dead.
  const off = (all: number, dead: number) => (subtractPreserved ? all : dead)
  if (tally.larvae) {
    if (off(tally.larvae, tally.larvaeDead ?? 0)) addTerms('NUMBER OF LARVAE', [-off(tally.larvae, tally.larvaeDead ?? 0)])
    parts.push(`${tally.larvae} ${tally.larvae === 1 ? 'larva' : 'larvae'}`)
  }
  if (tally.eggs) {
    if (off(tally.eggs, tally.eggsDead ?? 0)) addTerms('NUMBER OF EGGS', [-off(tally.eggs, tally.eggsDead ?? 0)])
    parts.push(`${tally.eggs} ${tally.eggs === 1 ? 'egg' : 'eggs'}`)
  }
  if (parts.length) {
    const days = tally.preservedOn.map(s => noteDay(s).replace(/\/\d+$/, '')).join(', ')
    cells.push({ field: 'NOTES', value: appendNote(value('NOTES'), `${parts.join(' and ')} preserved ${days}`, today, initials), before: value('NOTES') })
  }
  return { cells, skipped }
}

/** A count's change in short, for the save's summary: "=2+2 → =2+2+3 (7)". */
export function countChange(before: Count, after: CellValue): string {
  const from = before.terms.length ? (countValue(before.terms) as string) : '—'
  const terms = String(after ?? '').slice(1).match(/[+-]?\d+/g)?.map(Number) ?? []
  return `${from} → ${after} (${totalOf(terms)})`
}

// --- Plausibility of the day

/**
 * Why an emergence day looks wrong for the clutch (the notebook's month slips:
 * "19/7" for September): in the future, before the eggs were laid, or before
 * the first pupa. '' when it looks right or the clutch has no dates.
 */
export function dayDoubt(day: number, today: number, laid: CellValue, pupa: CellValue): '' | 'future' | 'before-laid' | 'before-pupa' {
  if (day > today) return 'future'
  if (typeof laid === 'number' && day < laid) return 'before-laid'
  if (typeof pupa === 'number' && day < pupa) return 'before-pupa'
  return ''
}
