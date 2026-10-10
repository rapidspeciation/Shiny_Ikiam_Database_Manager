import { isBlank, typedWhereFormulaFails } from './cells'
import { appendNote, noteDay } from './clutches'
import { serialFromIso } from './dates'
import type { CellValue, TableRow } from './types'

/**
 * Registering deaths of insectary butterflies (Insectary_data), shared by the
 * Muertes tab on computers and on phones, so a death is written the same way
 * whichever screen is used; and looking butterflies up by ID, CAM or tube
 * (the phone's search).
 */

/** A cell's value as the person sees it: the pending edit if there is one, else the sheet's. */
export type Getter = (row: TableRow, field: string) => CellValue

/**
 * What the team writes for a butterfly that was not preserved (Unknown,
 * Disappearance, Eaten…): no CAM, no tubes (NA), their tissues and media
 * NOT_COLLECTED (Franz, 1 Oct 2026: the intended method; NA in the tissues
 * of 2026 was only quicker to type), and no research purpose (NA; Pedigree,
 * a formula on it, then gives NA too).
 */
export const NOT_PRESERVED: Record<string, string> = {
  Preserved_Dead_Alive: 'NA',
  CAM_ID: 'NA',
  Tube_1_id: 'NA',
  Tube_1_tissue: 'NOT_COLLECTED',
  T1_Preservation_medium: 'NOT_COLLECTED',
  Tube_2_id: 'NA',
  Tube_2_tissue: 'NOT_COLLECTED',
  T2_Preservation_medium: 'NOT_COLLECTED',
  Tube_3_id: 'NA',
  Tube_3_tissue: 'NOT_COLLECTED',
  Tube_4_id: 'NA',
  Tube_4_tissue: 'NOT_COLLECTED',
  Preservation_medium: 'NOT_COLLECTED',
  Preservation_date: 'NA',
  Location_body: 'NA',
  Research_purpose: 'NA',
}
/** A reared butterfly has no Collection_data row: NA, where the row's cell is typed (the rows before ID H0B; a formula since). */
const COLL_CAM = 'CAM_ID_CollData'
/**
 * The columns a death fills in a row: its date and cause, then those of the
 * not-preserved block or of a preserved body (deathCells), and CAM_ID_CollData.
 * Cambios propuestos moves them together (lib/proposalMove).
 */
export const DEATH_COLUMNS = ['Death_date', 'Death_cause', ...Object.keys(NOT_PRESERVED), COLL_CAM]
export const KILLED = 'Killed_Preserved'
export const WHOLE = 'WHOLE_ORGANISM'
/** The purpose of most butterflies with a sample (cross parents and their offspring): the one offered first. */
export const CROSS_PURPOSE = 'F1/F2 mutation rate'

/**
 * Its only sample is a wing clip, taken alive (a cross or pheromone parent):
 * the body is still to be preserved, or not.
 */
export const onlyWingClip = (get: (field: string) => CellValue) =>
  /WING CLIP/i.test(String(get('Tube_1_tissue') ?? '')) && [2, 3, 4].every(n => isBlank(get(`Tube_${n}_id`)))
/** The clip's own cells, which a death leaves as they are. */
const CLIP_CELLS = ['CAM_ID', 'Tube_1_id', 'Tube_1_tissue', 'T1_Preservation_medium']

/** One cell a death fills: only when empty (or NA), or, with `overwrite`, also over a placeholder. */
export interface DeathCell {
  field: string
  value: CellValue
  overwrite?: boolean
}

/** A value the team writes for "nothing here yet", which a preservation may replace (TubesView). */
export const placeholder = (value: CellValue) =>
  isBlank(value) || /^(NOT_COLLECTED|NOT_PROVIDED)$/.test(String(value ?? '').trim())

/** The first tube slot free for a new tube (Tube_2 after a wing clip), or null when the row has no room. */
export function firstEmptySlot(get: (field: string) => CellValue): number | null {
  for (let slot = 1; slot <= 4; slot++)
    if (isBlank(get(`Tube_${slot}_id`))) {
      // "NA" in an ID cell after a whole-organism tube means "no more tubes".
      if (get(`Tube_${slot}_id`) === 'NA' && slot > 1) return null
      return slot
    }
  return null
}

/** The body preserved whole at death: its CAM (kept when it has one), the tube and its medium. */
export interface Preservation {
  cam: string
  tube: string
  medium: string
}

/**
 * The cells a death writes in one row, in order, each only where the cell is
 * empty or NA (formula cells never, but the Tube 2 medium: typedWhereFormulaFails):
 * Death_date and Death_cause; then either the not-preserved block (cause other
 * than Killed_Preserved, no CAM or tube yet; after a wing clip, what the clip
 * left empty) or, for a preserved body, what Tubos writes for a whole organism
 * (TubesView's assign with tissue WHOLE_ORGANISM, the preservation date being
 * the death date); and CAM_ID_CollData NA for a reared butterfly. Later steps
 * see the cells earlier ones filled, as the desktop's sequence of fills did.
 * `purpose`: the Research_purpose of a butterfly with a sample (a body
 * preserved now, a wing clip); without one, a body's stays as it is and a
 * clipped butterfly gets the cross purpose.
 */
export function deathCells(
  row: TableRow,
  get: Getter,
  {
    serial,
    cause,
    notPreserved,
    preserve,
    purpose,
  }: { serial: number | null; cause: string; notPreserved: boolean; preserve?: Preservation; purpose?: string },
): DeathCell[] {
  const out: DeathCell[] = []
  const now = new Map<string, CellValue>()
  const value = (field: string) => (now.has(field) ? now.get(field)! : get(row, field))
  const set = (field: string, v: CellValue, overwrite = false) => {
    if (row.formulas.includes(field) && !typedWhereFormulaFails('Insectary_data', field)) return
    const current = value(field)
    if (!overwrite && !isBlank(current)) return
    if (current === v) return
    now.set(field, v)
    out.push(overwrite ? { field, value: v, overwrite } : { field, value: v })
  }
  // Also over NOT_COLLECTED, e.g. left by Muertes on a butterfly then preserved after all.
  const put = (field: string, v: CellValue) => set(field, v, placeholder(value(field)))
  if (serial !== null) set('Death_date', serial)
  if (cause) set('Death_cause', cause)
  const why = value('Death_cause')
  if (preserve) {
    if (isBlank(value('CAM_ID')) && preserve.cam) set('CAM_ID', preserve.cam)
    const slot = firstEmptySlot(value)
    if (slot !== null && preserve.tube) {
      set(`Tube_${slot}_id`, preserve.tube)
      set(`Tube_${slot}_tissue`, WHOLE)
      if (slot <= 2) put(`T${slot}_Preservation_medium`, preserve.medium)
      if (serial !== null) {
        put('Preservation_date', serial)
        put('Death_date', serial)
      }
      if (isBlank(why)) put('Death_cause', KILLED)
      put('Preserved_Dead_Alive', isBlank(why) || why === KILLED ? 'Alive' : 'Dead')
      put('Preservation_medium', 'NOT_COLLECTED')
      put('Location_body', 'Ikiam')
      for (let next = slot + 1; next <= 4; next++) {
        set(`Tube_${next}_id`, 'NA')
        put(`Tube_${next}_tissue`, 'NOT_COLLECTED')
        if (next <= 2) put(`T${next}_Preservation_medium`, 'NOT_COLLECTED')
      }
      if (purpose) set('Research_purpose', purpose)
    }
  } else if (notPreserved && !isBlank(why) && why !== KILLED) {
    // Not preserved: only rows without a CAM or tube yet (a preserved one keeps its IDs)…
    if (isBlank(value('CAM_ID')) && isBlank(value('Tube_1_id'))) for (const [field, v] of Object.entries(NOT_PRESERVED)) set(field, v)
    // …or with a wing clip only: the clip's CAM and tube stay, the rest as above, with the purpose it was clipped for.
    else if (onlyWingClip(value)) {
      set('Research_purpose', purpose || CROSS_PURPOSE)
      for (const [field, v] of Object.entries(NOT_PRESERVED)) if (!CLIP_CELLS.includes(field)) set(field, v)
    }
  }
  if (out.length && value('Wild_Reared') === 'Reared') set(COLL_CAM, 'NA')
  return out
}

/** What a butterfly being preserved still lacks before Save: its CAM, its tube, a free tube slot, or a value another one has. */
export interface PreservationGap {
  id: string
  /** The tube slot its body goes to (Tube_2 after a wing clip), null when the row has no room. */
  slot: number | null
  /** It has a CAM already (a wing clip): kept, nothing to type. */
  keepsCam: boolean
  cam: '' | 'missing' | 'repeated'
  tube: '' | 'missing' | 'repeated'
  /** The butterfly that has the same CAM or tube first ("repeated"). */
  with?: string
  /** The value repeated. */
  value?: string
}
/** Whether a butterfly being preserved still lacks something (see preservationGaps). */
export const hasGap = (g: PreservationGap) => g.slot === null || !!g.cam || !!g.tube
/**
 * Each butterfly being preserved and what it still lacks, in the order given
 * (`samples`: the CAM and tube typed for each ID). A CAM or tube typed for two
 * of them is "repeated" on the second, and so is one another butterfly has in
 * the sheet already (`used`: search key → its Insectary ID, see usedSamples).
 */
export function preservationGaps(
  rows: TableRow[],
  get: Getter,
  samples: Record<string, { cam: string; tube: string } | undefined>,
  used: Map<string, string> = new Map(),
): PreservationGap[] {
  const seen = new Map(used)
  return rows.map(row => {
    const id = String(row.values.Insectary_ID ?? '')
    const s = samples[id]
    const keepsCam = !isBlank(get(row, 'CAM_ID'))
    const gap: PreservationGap = { id, slot: firstEmptySlot(f => get(row, f)), keepsCam, cam: '', tube: '' }
    const check = (kind: 'cam' | 'tube', value: string | undefined) => {
      const v = searchKey(value ?? '')
      if (!v) gap[kind] = 'missing'
      else if (seen.has(v) && seen.get(v) !== id) {
        gap[kind] = 'repeated'
        gap.with ??= seen.get(v)
        gap.value ??= v
      } else seen.set(v, id)
    }
    if (!keepsCam) check('cam', s?.cam)
    check('tube', s?.tube)
    return gap
  })
}

// --- Alive or dead

export type LifeState = 'alive' | 'dead' | 'unknown'
export interface Life {
  state: LifeState
  death: number | null
  cause: string
}
/**
 * Blank death cells mean the butterfly is alive (the typing lags by a couple of
 * days); a death date or a cause means it died; a death date "NA" (eggs and
 * larvae preserved, old rows) says neither.
 */
export function lifeOf(get: (field: string) => CellValue): Life {
  const date = get('Death_date')
  const cause = get('Death_cause')
  const causeText = isBlank(cause) ? '' : String(cause).trim()
  if (typeof date === 'number') return { state: 'dead', death: date, cause: causeText }
  if (causeText) return { state: 'dead', death: null, cause: causeText }
  if (date === null || date === undefined || String(date).trim() === '') return { state: 'alive', death: null, cause: '' }
  return { state: 'unknown', death: null, cause: '' }
}

/** Days from entering the insectary (emergence or capture) to its death, or to today while alive. */
export function daysAlive(get: (field: string) => CellValue, today: number): number | null {
  const start = get('Intro2Insectary_date')
  if (typeof start !== 'number') return null
  const life = lifeOf(get)
  const end = life.death ?? (life.state === 'alive' ? today : null)
  return end === null || end < start ? null : end - start
}

// --- Search

/** One butterfly in the search index. */
export interface Entry {
  row: TableRow
  id: string
  key: string
  /** CAM and tube IDs, upper case, to find a butterfly by its sample. */
  samples: string[]
  /** Order in the sheet: newer rows have higher numbers. */
  order: number
}
const SAMPLE_FIELDS = ['CAM_ID', 'Tube_1_id', 'Tube_2_id', 'Tube_3_id', 'Tube_4_id']
/** Searchable text: upper case, spaces gone (a phone keyboard may add one). */
export const searchKey = (text: string) => text.toUpperCase().replace(/\s+/g, '')

/** Every butterfly with an Insectary ID (the first row of a repeated ID), built once per version of the sheet. */
export function buildIndex(rows: TableRow[]): Entry[] {
  const seen = new Set<string>()
  const out: Entry[] = []
  for (const row of rows) {
    const raw = row.values.Insectary_ID
    if (!row.observed || isBlank(raw)) continue
    const id = String(raw).trim()
    const key = searchKey(id)
    if (seen.has(key)) continue
    seen.add(key)
    const samples: string[] = []
    for (const f of SAMPLE_FIELDS) {
      const v = row.values[f]
      if (!isBlank(v)) samples.push(searchKey(String(v)))
    }
    out.push({ row, id, key, samples, order: row.row })
  }
  return out
}

/** Every CAM and tube the sheet's butterflies have (search key → Insectary ID), "NA" left out. */
export function usedSamples(index: Entry[]): Map<string, string> {
  const out = new Map<string, string>()
  for (const e of index) for (const s of e.samples) if (s !== 'NA' && !out.has(s)) out.set(s, e.id)
  return out
}

export interface Suggestion {
  entry: Entry
  /** Found by its CAM or tube: which one. */
  via?: string
  /** Found by the ID matcher (lib/idMatch.ts): positions read as a look-alike, and whether it is out of what was looked for. */
  match?: { at: number[]; greyed: boolean }
}
/**
 * The butterflies matching what is typed, best first: the exact ID while
 * alive, IDs starting with it (living ones first, newest first), the exact ID
 * of a dead one, IDs containing it, then (from three characters) CAM and tube
 * IDs starting with it. `alive` says whether a row is alive now (with pending edits).
 */
export function suggest(
  index: Entry[],
  typed: string,
  { alive, skip = new Set(), limit = 8 }: { alive: (entry: Entry) => boolean; skip?: Set<string>; limit?: number },
): Suggestion[] {
  const q = searchKey(typed)
  if (!q) return []
  const scored: { s: Suggestion; tier: number; order: number }[] = []
  for (const entry of index) {
    if (skip.has(entry.id)) continue
    // Whether it lives is asked only of the matches (it reads pending edits).
    let tier = -1
    if (entry.key === q) tier = alive(entry) ? 0 : 2
    else if (entry.key.startsWith(q)) tier = alive(entry) ? 1 : 4
    else if (entry.key.includes(q)) tier = alive(entry) ? 3 : 5
    if (tier >= 0) {
      scored.push({ s: { entry }, tier, order: entry.order })
      continue
    }
    if (q.length >= 3) {
      const sample = entry.samples.find(v => v.startsWith(q))
      if (sample) scored.push({ s: { entry, via: sample }, tier: sample === q ? 0 : 6, order: entry.order })
    }
  }
  scored.sort((a, b) => a.tier - b.tier || b.order - a.order)
  return scored.slice(0, limit).map(x => x.s)
}

/** Letters and digits that look alike on a wing or a paper label. */
const LOOK_ALIKE: Record<string, string[]> = {
  '0': ['O', 'D', 'Q'],
  O: ['0', 'D', 'Q'],
  D: ['0', 'O'],
  Q: ['0', 'O'],
  '1': ['I', 'L', '7'],
  I: ['1', 'L'],
  L: ['1', 'I'],
  '7': ['1'],
  '5': ['S'],
  S: ['5'],
  '8': ['B'],
  B: ['8'],
  '2': ['Z'],
  Z: ['2'],
  '6': ['G'],
  G: ['6'],
}
/**
 * IDs that exist and look like the one typed: one character read as its
 * look-alike (0/O, 1/I, 8/B…), two neighbours swapped, one character too many
 * (A6EE), or a letter missing at the end (B9 for B9D). `known`: every ID's search key → the ID.
 */
export function lookAlikes(typed: string, known: Map<string, string>, limit = 4): string[] {
  const q = searchKey(typed)
  if (!q) return []
  const out = new Set<string>()
  const add = (key: string) => {
    const id = known.get(key)
    if (id && key !== q) out.add(id)
  }
  for (let i = 0; i < q.length; i++) for (const other of LOOK_ALIKE[q[i]] ?? []) add(q.slice(0, i) + other + q.slice(i + 1))
  for (let i = 0; i + 1 < q.length; i++) add(q.slice(0, i) + q[i + 1] + q[i] + q.slice(i + 2))
  for (let i = 0; i < q.length && q.length > 2; i++) add(q.slice(0, i) + q.slice(i + 1))
  for (const letter of 'DEABCF') add(q + letter)
  return [...out].slice(0, limit)
}

// --- Causes

/**
 * The Death_cause list (the sheet's own values), most used in the last year
 * first, then the rest of the list in its order. Values used but missing from
 * the list are left out: the buttons offer only what the sheet accepts.
 */
export function rankCauses(list: string[], rows: TableRow[], today: number, days = 365): string[] {
  const counts = new Map<string, number>()
  for (const row of rows) {
    const date = row.values.Death_date
    const cause = row.values.Death_cause
    if (typeof date !== 'number' || today - date > days || isBlank(cause)) continue
    counts.set(String(cause), (counts.get(String(cause)) ?? 0) + 1)
  }
  return list
    .filter(c => !isBlank(c))
    .map((cause, i) => ({ cause, n: counts.get(cause) ?? 0, i }))
    .sort((a, b) => b.n - a.n || a.i - b.i)
    .map(x => x.cause)
}

// --- Racks

export interface RackSuggestion {
  value: string
  medium?: string
  context?: string
  /** The newest preservation day of its run (a serial date), when known. */
  date?: number | null
}
/** A rack whose last tube is this many days older than another insectary rack's is no longer in use. */
const STALE_RACK_DAYS = 14
/**
 * The rack for a dead body, as Tubos picks it: the crosses' rack when most of
 * the butterflies belong to crosses, else the insectary's, in the medium chosen
 * (flash frozen and ethanol tubes live in different racks); but when that rack's
 * last tube is weeks older than the other insectary rack's, the one in use.
 */
export function bestRack<T extends RackSuggestion>(racks: T[], rows: TableRow[], medium: string): T | undefined {
  const crosses = rows.filter(r => /F1\/F2|WEST x EAST|cross|mutation/i.test(String(r.values.Research_purpose ?? ''))).length
  const context = rows.length && crosses * 2 >= rows.length ? 'Cruces' : 'Insectario'
  const insectary = racks.filter(s => s.context === 'Cruces' || s.context === 'Insectario')
  // The team often fills one rack for crosses and insectary alike (Sep–Oct 2026): a context's own rack
  // left weeks behind gives way to the insectary rack in use.
  const own = racks.find(s => s.context === context && s.medium === medium)
  const newest = insectary.filter(s => s.medium === medium).sort((a, b) => (b.date ?? -Infinity) - (a.date ?? -Infinity))[0]
  if (own && newest && typeof own.date === 'number' && typeof newest.date === 'number' && newest.date - own.date > STALE_RACK_DAYS)
    return newest
  return (
    own ||
    insectary.find(s => s.medium === medium) ||
    racks.find(s => s.context === context) ||
    racks[0]
  )
}

// --- What a card shows

/** A butterfly's key data for a card, read as the person sees it (pending edits included). */
export interface Facts {
  species: string
  sex: string
  clutch: string
  /** Caught in the field (Wild-caught) rather than reared from a clutch. */
  wild: boolean
  /** Intro2Insectary_date: the emergence (reared) or capture (wild-caught) day. */
  entered: number | null
  days: number | null
  life: Life
  notes: string
}
export function factsOf(get: (field: string) => CellValue, today: number): Facts {
  const text = (field: string) => {
    const v = get(field)
    return isBlank(v) ? '' : String(v).trim()
  }
  const entered = get('Intro2Insectary_date')
  return {
    species: text('SPECIES'),
    sex: text('Sex'),
    clutch: text('CLUTCH NUMBER'),
    wild: /wild/i.test(text('Wild_Reared')),
    entered: typeof entered === 'number' ? entered : null,
    days: daysAlive(get, today),
    life: lifeOf(get),
    notes: text('Notes_Insectary_data'),
  }
}

// --- The death a card is recorded with

/** What a death is registered with: the date (ISO), the cause, preserved or not, and a note to add. */
export interface DeathChoice {
  date: string
  cause: string
  preserved: boolean
  /** Text added to Notes_Insectary_data on Save ("d/m/yy INI: text", after the notes there); '' adds none. */
  note: string
}
export type ChoiceField = keyof DeathChoice

/** Phrases the team writes in the notes of a death (English, as in the sheet): quick buttons. */
export const DEATH_NOTE_PHRASES = [
  'Only wings found',
  'Eaten by something',
  'Head eaten',
  "Deformed wings, can't fly",
  'Emerged incomplete',
  'With fungi',
  'Too dry to preserve',
  'Preserved for pheromones',
  'Preserved in ultrafridge at -80ºC',
]

export const NOTES = 'Notes_Insectary_data'
/**
 * The note a card adds on Save: Notes_Insectary_data with "d/m/yy INI: text"
 * after what is there (" | "), never replacing it; null when the note is empty
 * (or the cell is a formula).
 */
export function noteCell(row: TableRow, get: Getter, note: string, today: number, initials: string): DeathCell | null {
  const text = note.trim()
  if (!text || row.formulas.includes(NOTES)) return null
  return { field: NOTES, value: appendNote(get(row, NOTES), text, today, initials), overwrite: true }
}

/**
 * The cells recording a card's death writes, with its own date, cause,
 * preservation and note: what the table's «Escribir fecha y causa»
 * writes (deathCells), plus, for a body preserved now, its CAM, tube (`sample`),
 * the medium and the `purpose`; then the note, dated `today` and signed with `initials`.
 * A butterfly already recorded dead gets no tube here (Tubos does), but its note.
 */
export function cardCells(
  row: TableRow,
  get: Getter,
  choice: DeathChoice,
  {
    sample,
    medium,
    purpose,
    today,
    initials = '',
  }: { sample?: { cam: string; tube: string }; medium: string; purpose?: string; today: number; initials?: string },
): DeathCell[] {
  const serial = choice.date ? serialFromIso(choice.date) : null
  const dying = lifeOf(f => get(row, f)).state !== 'dead'
  const preserve =
    choice.preserved && dying
      ? { cam: sample?.cam.trim().toUpperCase() || '', tube: sample?.tube.trim().toUpperCase() || '', medium }
      : undefined
  const cells = deathCells(row, get, { serial, cause: choice.cause, notPreserved: !choice.preserved, preserve, purpose })
  const note = noteCell(row, get, choice.note ?? '', today, initials)
  return note ? [...cells, note] : cells
}

// --- A butterfly the sheet already has dead (a misread ID, found dead again): its death replaced

/** The death the sheet has for a butterfly (date and cause), or null while it is alive. */
export interface PriorDeath {
  date: number | null
  cause: string
}
export function priorDeath(get: (field: string) => CellValue): PriorDeath | null {
  const life = lifeOf(get)
  return life.state === 'dead' ? { date: life.death, cause: life.cause } : null
}

/** Its CAM or first tube holds an ID (a body preserved before): replacing the death keeps them. */
export const hasSampleIds = (get: (field: string) => CellValue) => !isBlank(get('CAM_ID')) || !isBlank(get('Tube_1_id'))

/**
 * The note a replaced death adds, in English: "Found dead today; replaces the
 * death recorded on 3/9/26 (Unknown), probably a misread ID".
 */
export function replaceNote(prior: PriorDeath, serial: number | null, today: number): string {
  const when = serial === null || serial === today ? 'today' : `on ${noteDay(serial)}`
  const was = [prior.date !== null ? `on ${noteDay(prior.date)}` : '', prior.cause ? `(${prior.cause})` : ''].filter(Boolean).join(' ')
  return `Found dead ${when}; replaces the death recorded${was ? ` ${was}` : ''}, probably a misread ID`
}

/**
 * The cells replacing the death a butterfly has in the sheet: Death_date and
 * Death_cause written over the old ones; a body preserved now (when the row
 * holds no CAM or tube ID yet) written as for a new death, over the old
 * not-preserved block (NA, NOT_COLLECTED); a not-preserved death fills what
 * is empty; then the note saying what it replaces (and the card's own note),
 * dated `today` and signed. Undo in Historial puts the old death back.
 */
export function replaceCells(
  row: TableRow,
  get: Getter,
  choice: DeathChoice,
  {
    sample,
    medium,
    purpose,
    today,
    initials = '',
  }: { sample?: { cam: string; tube: string }; medium: string; purpose?: string; today: number; initials?: string },
): DeathCell[] {
  const value = (field: string) => get(row, field)
  const prior = priorDeath(value) ?? { date: null, cause: '' }
  const serial = choice.date ? serialFromIso(choice.date) : null
  const out: DeathCell[] = []
  const over = (field: string, v: CellValue) => {
    if (!row.formulas.includes(field) && value(field) !== v) out.push({ field, value: v, overwrite: true })
  }
  if (serial !== null) over('Death_date', serial)
  if (choice.cause) over('Death_cause', choice.cause)
  const preserving = choice.preserved && !hasSampleIds(value)
  // The rest of the death sees the new date and cause, and (preserving now) the old not-preserved block as empty.
  const now = new Map(out.map(c => [c.field, c.value]))
  const after: Getter = (r, field) =>
    now.has(field) ? now.get(field)! : preserving && field in NOT_PRESERVED && value(field) === NOT_PRESERVED[field] ? null : get(r, field)
  const preserve = preserving
    ? { cam: sample?.cam.trim().toUpperCase() || '', tube: sample?.tube.trim().toUpperCase() || '', medium }
    : undefined
  for (const c of deathCells(row, after, { serial, cause: choice.cause, notPreserved: !choice.preserved, preserve, purpose }))
    if (!now.has(c.field)) out.push(isBlank(value(c.field)) ? c : { ...c, overwrite: true })
  const text = [replaceNote(prior, serial, today), (choice.note ?? '').trim()].filter(Boolean).join('; ')
  const note = noteCell(row, get, text, today, initials)
  return note ? [...out, note] : out
}

/** The death date chosen falls before the butterfly entered the insectary (emerged or caught): worth a look, not a block. */
export const diesBeforeEntry = (date: string, entered: number | null) => {
  const serial = date ? serialFromIso(date) : null
  return serial !== null && entered !== null && serial < entered
}

// --- The cause buttons

/**
 * The causes in the order the buttons show them: those recorded today first,
 * most first (a tie in the list's order), each with its count; then the rest
 * in the list's order.
 */
export function causesByToday(list: string[], today: Map<string, number>): { cause: string; today: number }[] {
  const out = list.map((cause, i) => ({ cause, today: today.get(cause) ?? 0, i }))
  out.sort((a, b) => (b.today > 0 || a.today > 0 ? b.today - a.today : 0) || a.i - b.i)
  return out.map(({ cause, today }) => ({ cause, today }))
}

/** The cause a number key picks (1–9: the button in that place), or null. */
export const causeForKey = (key: string, causes: string[]) => (/^[1-9]$/.test(key) ? (causes[Number(key) - 1] ?? null) : null)

/** Why a butterfly's death cannot be written yet: '' when it can. */
export type Lack = '' | 'date' | 'bad-date' | 'cause' | 'sample'
export function lackOf(choice: DeathChoice, dying: boolean, gap?: PreservationGap): Lack {
  if (!choice.date) return 'date'
  if (serialFromIso(choice.date) === null) return 'bad-date'
  if (dying && !choice.cause) return 'cause'
  if (gap && hasGap(gap)) return 'sample'
  return ''
}
