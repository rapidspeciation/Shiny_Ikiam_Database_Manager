import { isBlank } from './cells'
import { appendNote, eventNote, groupsValue, totalOf, type Count } from './clutches'
import { appendInGroup } from './clutchGroups'
import { clutchSettings } from './clutchSettings'
import { serialFromIso } from './dates'
import { CROSS_PURPOSE, deathCells, KILLED } from './deaths'
import { assign, normalizeId } from './tubes'
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
export { CROSS_PURPOSE }

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
  /** An egg or larva: its CAM and tube as typed (undefined follows the suggestion, '' is a box emptied on purpose). */
  typedCam?: string
  typedTube?: string
  /** An egg or larva: what it has of its own instead of the batch's (YoungBatch). */
  own?: YoungOwn
  /** The Insectary ID the server holds for this card (server/holds.mjs), once it answered. */
  held?: string
  /** Its hold: being asked, not held (someone else has the ID, or it is not free), or no answer (no signal). */
  hold?: 'waiting' | 'refused' | 'offline'
  /**
   * «Sobrescribir de todas formas»: the ID (upper case) whose row already holds a butterfly, confirmed to be
   * written over (an ID changed afterwards needs it again). The save edits that row instead of a free one.
   */
  overwrite?: string
}

/** LIFESTAGE of a butterfly with a date in Intro2Insectary_date (emerged or brought in). */
export const ADULT = 'Adult'
/** LIFESTAGE values, as the sheet writes them (used only for eggs and larvae). */
export const LIFESTAGES = ['Egg', '1st instar larva', '2nd instar larva', '3rd instar larva', '4th instar larva', '5th instar larva', 'Pre-pupa']
/** F1 larvae are preserved at the 3rd instar (the team, 5 Oct 2026). */
export const DEFAULT_STAGE = '3rd instar larva'
/** A pupa preserved: its day as a pupa, as the sheet's LIFESTAGE list writes it (Pupa day 1 … Pupa day 12). */
export const PUPA_STAGES = Array.from({ length: 12 }, (_, i) => `Pupa day ${i + 1}`)
/** Every stage an egg, larva or pupa can be preserved at («+ Preservados…»), in the list's order. */
export const PRESERVED_STAGES = [...LIFESTAGES, ...PUPA_STAGES]
/** Which count of the clutch a stage belongs to: eggs, larvae (prepupae too) or pupae. */
export const stageGroup = (stage: string): 'egg' | 'larva' | 'pupa' => (stage === 'Egg' ? 'egg' : /^Pupa day /.test(stage) ? 'pupa' : 'larva')

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
  /** Research_purpose of an egg or larva (F1/F2 mutation rate when not given). */
  purpose?: string
}

/** The note a card adds by default: an egg or larva preserved, as the team writes it ("Preserved alive 3rd instar"). */
export function youngNote(stage: string, foundDead: boolean): string {
  const what = stage === 'Egg' ? 'egg' : stage === 'Pre-pupa' ? 'prepupa' : stageGroup(stage) === 'pupa' ? stage.toLowerCase() : stage.replace(/ larva$/, '')
  return foundDead ? `Found dead, ${what}` : `Preserved alive ${what}`
}

/**
 * The values of the new Insectary_data row for a card. An emergence:
 * Reared, the clutch, Stock_of_origin (the clutch's subspecies for the
 * messenoides stock, else NA), Sex, Intro2Insectary_date, LIFESTAGE Adult; SPECIES only when
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
    values.Research_purpose = ctx.purpose?.trim() || CROSS_PURPOSE
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
    // An entry date: an adult (team rule, 5 Oct 2026).
    if (serial !== null) values.LIFESTAGE = ADULT
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

/** A card confirmed to write over the row of its ID, which holds a butterfly already. */
export const overwriting = (d: Pick<Draft, 'id' | 'overwrite'>) => !!d.overwrite && d.overwrite === norm(d.id)

/**
 * The edit that writes a card over a row with data («Sobrescribir de todas
 * formas»): the card's values as on a new row (the row's own formulas left
 * alone, SPECIES typed over its formula only when it differs), the cells it
 * held and the card does not set emptied (a death, a CAM, a note of the old
 * butterfly), and `expected` what the person saw, so a change meanwhile is
 * refused rather than lost. Kept in the app and undone from Historial like any save.
 */
export function overwriteEdit(
  d: Draft,
  row: TableRow,
  ctx: Omit<RowContext, 'formulas'>,
): { id: string; values: Record<string, CellValue>; expected: Record<string, CellValue>; replaceFormula: string[] } {
  const formulas = new Set(row.formulas)
  const values = draftValues(d, { ...ctx, formulas: row.formulas })
  delete values.Insectary_ID
  for (const [field, value] of Object.entries(row.values))
    if (field !== 'Insectary_ID' && !formulas.has(field) && !(field in values) && !isBlank(value)) values[field] = null
  for (const field of Object.keys(values)) if (values[field] === (row.values[field] ?? null) && !formulas.has(field)) delete values[field]
  const expected = Object.fromEntries(Object.keys(values).map(f => [f, row.values[f] ?? null]))
  return { id: row.id, values, expected, replaceFormula: 'SPECIES' in values && formulas.has('SPECIES') ? ['SPECIES'] : [] }
}

/** A card that needs a CAM and a tube (killed and preserved, or an egg or larva). */
export const preserving = (d: Pick<Draft, 'kind' | 'fate'>) => d.kind === 'young' || d.fate === 'preserved'
/** An adult that emerged (counted in NUMBER OF ADULTS), whatever became of it. */
export const isAdult = (d: Pick<Draft, 'kind'>) => d.kind === 'adult'

// --- Eggs and larvae preserved: the batch's medium, rack, first CAM and purpose; each card's CAM and tube

/** The media a body can be preserved in: flash frozen in the dry shipper; ethanol when it fails. */
export const MEDIUMS = ['Flash frozen', 'Ethanol', 'DMSO']
/** The stages offered first: the protocol preserves F1 larvae at the 3rd or 4th instar. */
export const MAIN_STAGES = ['3rd instar larva', '4th instar larva']

/** What an egg or larva card has of its own instead of the batch's (set with it selected). */
export interface YoungOwn {
  medium?: string
  purpose?: string
  /** The first CAM of its run (its own CAM series). */
  camFrom?: string
  /** The first tube of its run (another rack: an ethanol one, or the next rack when one runs out). */
  tubeFrom?: string
}
export type YoungField = keyof YoungOwn

/** The batch of eggs and larvae being preserved: what every card takes unless it has its own. */
export interface YoungBatch {
  medium: string
  purpose: string
  /** The first CAM ('' = the next free one). */
  camStart: string
  /** The first tube of a rack chosen or typed ('' = the app's rack for the medium). */
  tubeStart: string
  /** What «+ N larvae» adds. */
  stage: string
  foundDead: boolean
  /** CAMs and tubes of an odd form kept as written on their labels. */
  accepted?: string[]
}
export const YOUNG_BATCH: YoungBatch = {
  medium: 'Flash frozen',
  purpose: CROSS_PURPOSE,
  camStart: '',
  tubeStart: '',
  stage: DEFAULT_STAGE,
  foundDead: false,
}
const BATCH_FIELD = { medium: 'medium', purpose: 'purpose', camFrom: 'camStart', tubeFrom: 'tubeStart' } as const

/** A card's value of a field: its own, else the batch's. */
export const youngValue = (d: Pick<Draft, 'own'>, batch: YoungBatch, field: YoungField): string =>
  d.own?.[field] ?? batch[BATCH_FIELD[field]]

/** The value these cards share for a field, or undefined when they differ. */
export function sharedYoung(cards: Pick<Draft, 'own'>[], batch: YoungBatch, field: YoungField): string | undefined {
  if (!cards.length) return undefined
  const first = youngValue(cards[0], batch, field)
  return cards.every(d => youngValue(d, batch, field) === first) ? first : undefined
}

/**
 * One value set in the batch panel: for the selected cards only (their own
 * value; dropped when it is the batch's), or with none selected for the whole
 * batch (no card keeps its own value of that field). A new medium for the
 * whole batch drops the rack chosen for the old one: the tubes then come from
 * the app's rack for that medium (flash frozen and ethanol tubes live in
 * different racks).
 */
export function setYoung(
  drafts: Draft[],
  batch: YoungBatch,
  selected: string[],
  field: YoungField,
  value: string,
): { drafts: Draft[]; batch: YoungBatch } {
  const key = BATCH_FIELD[field]
  const run = field === 'camFrom' || field === 'tubeFrom'
  const v = run ? normalizeId(value) : value
  const without = (d: Draft): Draft => {
    if (d.own?.[field] === undefined) return d
    const { [field]: _dropped, ...rest } = d.own
    return { ...d, own: Object.keys(rest).length ? rest : undefined }
  }
  if (!selected.length) {
    const next = { ...batch, [key]: v }
    if (field === 'medium' && v !== batch.medium) next.tubeStart = ''
    return { drafts: drafts.map(d => (d.kind === 'young' ? without(d) : d)), batch: next }
  }
  const chosen = new Set(selected)
  return {
    batch,
    drafts: drafts.map(d => {
      if (d.kind !== 'young' || !chosen.has(d.key)) return d
      if (run ? !v : v === batch[key]) return without(d)
      return { ...d, own: { ...d.own, [field]: v } }
    }),
  }
}

/**
 * Where a card's CAM and tube runs start: its own first CAM, else the batch's,
 * else the next free one (`camFirst`); its own first tube, else the batch's
 * rack when the card is in the batch's medium, else the app's rack for its
 * medium (`rackFor`: the crosses' rack in that medium, lib/deaths bestRack).
 */
export function youngStarts(
  d: Pick<Draft, 'own'>,
  batch: YoungBatch,
  { camFirst, rackFor }: { camFirst: string; rackFor: (medium: string) => string },
): { cam: string; tube: string } {
  const medium = youngValue(d, batch, 'medium')
  const cam = d.own?.camFrom || batch.camStart || camFirst
  const tube = d.own?.tubeFrom || (batch.tubeStart && medium === batch.medium ? batch.tubeStart : rackFor(medium))
  return { cam: normalizeId(cam), tube: normalizeId(tube) }
}

export interface Sample {
  value: string
  /** Handed out by the app (the next free one), not typed. */
  auto: boolean
}
/**
 * Each egg or larva card's CAM and tube, in the cards' order. Cards drawing
 * from the same run (the same first CAM, the same rack) take its next free IDs
 * one after another; a CAM or tube typed on a card stays, and the next cards
 * of its run go on from it (the rack is there). `run(start, count)` gives free
 * IDs from `start`; IDs typed on any card or in `taken` (other cards' CAMs and
 * tubes) are never handed out.
 */
export function youngSamples(
  cards: { key: string; cam: string; tube: string; typedCam?: string; typedTube?: string }[],
  { run, taken = new Set<string>() }: { run: (start: string, count: number) => string[]; taken?: Set<string> },
): Record<string, { cam: Sample; tube: Sample }> {
  const reserved = new Set(taken)
  for (const c of cards) {
    if (c.typedCam) reserved.add(normalizeId(c.typedCam))
    if (c.typedTube) reserved.add(normalizeId(c.typedTube))
  }
  const out: Record<string, { cam: Sample; tube: Sample }> = {}
  for (const c of cards) out[c.key] = { cam: { value: '', auto: true }, tube: { value: '', auto: true } }
  for (const kind of ['cam', 'tube'] as const) {
    const runs = new Map<string, typeof cards>()
    for (const c of cards) runs.set(c[kind], [...(runs.get(c[kind]) ?? []), c])
    for (const [start, list] of runs) {
      const needs = list.map(c => ({ id: c.key, cam: kind === 'cam', tubes: kind === 'tube' ? 1 : 0 }))
      const typed = Object.fromEntries(list.map(c => [c.key, kind === 'cam' ? { cam: c.typedCam } : { tubes: [c.typedTube] }]))
      const got = assign(needs, typed, { camStart: kind === 'cam' ? start : '', tubeStart: kind === 'tube' ? start : '', run, taken: reserved })
      for (const c of list) {
        const sample = kind === 'cam' ? got[c.key].cam! : got[c.key].tubes[0]
        out[c.key][kind] = sample
        if (sample.value) reserved.add(sample.value)
      }
    }
  }
  return out
}

// --- The clutch's row in Insectary_stocks

/** Eggs, larvae or pupae preserved on one day, of one stage, alive or found dead: one note in NOTES. */
export interface YoungGroup {
  day: number
  stage: 'egg' | 'larva' | 'pupa'
  lifestage: string
  dead: boolean
  ids: string[]
}
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
  /** Pupae preserved (Pupa day N), and those found dead: taken off NUMBER OF PUPA as larvae off theirs. */
  pupae?: number
  pupaeDead?: number
  /** The days eggs, larvae or pupae were preserved (serials). */
  preservedOn: number[]
  /** The eggs and larvae by day, stage and fate, with their Insectary IDs (for the notes). */
  groups: YoungGroup[]
}
/** The cards of each clutch, counted for its stocks row (in the order the clutches first appear). */
export function tallies(drafts: Draft[]): ClutchTally[] {
  const out = new Map<string, ClutchTally & { byDay: Map<number, number> }>()
  for (const d of drafts) {
    const serial = serialFromIso(d.date)
    if (serial === null) continue
    let t = out.get(d.clutch)
    if (!t) out.set(d.clutch, (t = { clutch: d.clutch, adults: [], eggs: 0, larvae: 0, eggsDead: 0, larvaeDead: 0, pupae: 0, pupaeDead: 0, preservedOn: [], groups: [], byDay: new Map() }))
    if (d.kind === 'adult') t.byDay.set(serial, (t.byDay.get(serial) ?? 0) + 1)
    else {
      const stage = stageGroup(d.stage || DEFAULT_STAGE)
      const lifestage = d.stage || DEFAULT_STAGE
      let g = t.groups.find(x => x.day === serial && x.stage === stage && x.lifestage === lifestage && x.dead === d.foundDead)
      if (!g) t.groups.push((g = { day: serial, stage, lifestage, dead: d.foundDead, ids: [] }))
      g.ids.push(norm(d.id))
      if (stage === 'egg') {
        t.eggs++
        if (d.foundDead) t.eggsDead++
      } else if (stage === 'pupa') {
        t.pupae = (t.pupae ?? 0) + 1
        if (d.foundDead) t.pupaeDead = (t.pupaeDead ?? 0) + 1
      } else {
        t.larvae++
        if (d.foundDead) t.larvaeDead++
      }
      if (!t.preservedOn.includes(serial)) t.preservedOn.push(serial)
    }
  }
  return [...out.values()].map(({ byDay, ...t }) => ({
    ...t,
    adults: [...byDay].sort((a, b) => a[0] - b[0]),
    preservedOn: t.preservedOn.sort((a, b) => a - b),
    groups: t.groups.sort((a, b) => a.day - b.day),
  }))
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
 * empty (the first day); eggs, larvae and pupae preserved taken off NUMBER OF EGGS /
 * NUMBER OF LARVAE / NUMBER OF PUPA (−3) as the team's setting says (`subtractPreserved`,
 * Clutches' settings: kept counted by default); those found dead are a death,
 * taken off always. Each day's adults and each group of eggs or larvae get a
 * dated, signed note in NOTES, as Clutches writes its events (lib/clutches
 * eventNote: "3 adults emerged", "2 larvae preserved as 3rd instar (E4E,
 * E5E)", with the day when it was not today). `count(field)`: a count as the
 * person sees it; `value(field)`: the cell as the sheet has it (a sum as its
 * formula).
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
    // In the last group's parentheses when the count is kept in groups (=(6-2)+(5+3)).
    let now = c.groups
    for (const n of terms) {
      const r = appendInGroup(now, null, n)
      if (!r.ok) {
        skipped.push(field)
        return
      }
      now = r.groups
    }
    if (now !== c.groups) cells.push({ field, value: groupsValue(now), before: value(field) })
  }
  if (tally.adults.length) {
    addTerms('NUMBER OF ADULTS', tally.adults.map(([, n]) => n))
    const first = tally.adults[0][0]
    if (isBlank(value('EMERGENCE DATE'))) cells.push({ field: 'EMERGENCE DATE', value: first, before: value('EMERGENCE DATE') })
  }
  // Taken off the count: all of them, or (preserved ones kept counted) only those found dead.
  const off = (all: number, dead: number) => (subtractPreserved ? all : dead)
  if (tally.larvae && off(tally.larvae, tally.larvaeDead ?? 0)) addTerms('NUMBER OF LARVAE', [-off(tally.larvae, tally.larvaeDead ?? 0)])
  if (tally.eggs && off(tally.eggs, tally.eggsDead ?? 0)) addTerms('NUMBER OF EGGS', [-off(tally.eggs, tally.eggsDead ?? 0)])
  if (tally.pupae && off(tally.pupae, tally.pupaeDead ?? 0)) addTerms('NUMBER OF PUPA', [-off(tally.pupae, tally.pupaeDead ?? 0)])
  const notes = [
    ...tally.adults.map(([day, n]) => eventNote({ stage: 'adult', kind: 'emerged', count: n, day }, today)),
    ...(tally.groups ?? []).map(g =>
      g.dead
        ? eventNote({ stage: g.stage, kind: 'died', count: g.ids.length, ids: g.ids, day: g.day }, today).replace(' died', ' found dead, preserved')
        : g.stage === 'pupa'
          ? // "2 pupae preserved as pupa day 3 (S8E, S9E)", as the larvae's "as 3rd instar".
            eventNote({ stage: 'pupa', kind: 'preserved', count: g.ids.length, ids: g.ids, day: g.day }, today).replace(' preserved', ` preserved as ${g.lifestage.toLowerCase()}`)
          : eventNote({ stage: g.stage, kind: 'preserved', count: g.ids.length, ids: g.ids, lifestage: g.lifestage, day: g.day }, today),
    ),
  ]
  if (notes.length)
    cells.push({ field: 'NOTES', value: notes.reduce<CellValue>((all, n) => appendNote(all, n, today, initials), value('NOTES')), before: value('NOTES') })
  return { cells, skipped }
}

/** A count's change in short, for the save's summary: "=2+2 → =2+2+3 (7)". */
export function countChange(before: Count, after: CellValue): string {
  const from = before.terms.length ? (groupsValue(before.groups) as string) : '—'
  const terms = String(after ?? '').match(/[+-]?\d+/g)?.map(Number) ?? []
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
