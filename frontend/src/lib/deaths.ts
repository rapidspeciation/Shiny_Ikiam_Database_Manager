import { isBlank } from './cells'
import { appendNote } from './clutches'
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
 * of 2026 was only quicker to type).
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
}
/**
 * The columns a death fills in a row: its date and cause, then those of the
 * not-preserved block or of a preserved body (deathCells), and Research_purpose,
 * which the notebook's template adds (server/notebook.mjs NOT_PRESERVED).
 * Cambios propuestos moves them together (lib/proposalMove).
 */
export const DEATH_COLUMNS = ['Death_date', 'Death_cause', ...Object.keys(NOT_PRESERVED), 'Research_purpose']
export const KILLED = 'Killed_Preserved'
export const WHOLE = 'WHOLE_ORGANISM'

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
 * empty or NA (formula cells never): Death_date and Death_cause; then either
 * the not-preserved block (cause other than Killed_Preserved, no CAM or tube
 * yet) or, for a preserved body, what Tubos writes for a whole organism
 * (TubesView's assign with tissue WHOLE_ORGANISM, the preservation date being
 * the death date). Later steps see the cells earlier ones filled, as the
 * desktop's sequence of fills did.
 */
export function deathCells(
  row: TableRow,
  get: Getter,
  { serial, cause, notPreserved, preserve }: { serial: number | null; cause: string; notPreserved: boolean; preserve?: Preservation },
): DeathCell[] {
  const out: DeathCell[] = []
  const now = new Map<string, CellValue>()
  const value = (field: string) => (now.has(field) ? now.get(field)! : get(row, field))
  const set = (field: string, v: CellValue, overwrite = false) => {
    if (row.formulas.includes(field)) return
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
    }
  } else if (
    notPreserved &&
    !isBlank(why) &&
    why !== KILLED &&
    isBlank(value('CAM_ID')) &&
    isBlank(value('Tube_1_id'))
  )
    // Not preserved: only rows without a CAM or tube yet (a preserved one keeps its IDs).
    for (const [field, v] of Object.entries(NOT_PRESERVED)) set(field, v)
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

// --- Each card its own date, cause, preservation and note

/** What a death is registered with: the date (ISO), the cause, preserved or not, and a note to add. */
export interface DeathChoice {
  date: string
  cause: string
  preserved: boolean
  /** Text added to Notes_Insectary_data on Save ("d/m/yy INI: text", after the notes there); '' adds none. */
  note: string
}
export type ChoiceField = keyof DeathChoice
/** The values a card has of its own (a note typed in its editor, an unfinished one's choices), by Insectary ID; the rest come from the panel. */
export type OwnChoices = Record<string, Partial<DeathChoice>>

/** A card's date, cause, preservation and note: its own where it has them, else the panel's (for all cards). */
export function choiceFor(all: DeathChoice, own: OwnChoices, id: string): DeathChoice {
  const mine = own[id]
  return mine ? { ...all, ...mine } : all
}

/**
 * One value chosen in the panel. With cards selected it is theirs only (their
 * own value; the same as the panel's is not kept as their own); with none
 * selected it is every card's: the panel's value, and no card keeps its own
 * value of that field. Returns the new panel values and own values (the old
 * ones are not changed).
 */
export function setChoice<F extends ChoiceField>(
  all: DeathChoice,
  own: OwnChoices,
  selected: string[],
  field: F,
  value: DeathChoice[F],
): { all: DeathChoice; own: OwnChoices } {
  const out: OwnChoices = {}
  const put = (id: string, mine: Partial<DeathChoice>) => {
    if (Object.keys(mine).length) out[id] = mine
  }
  if (!selected.length) {
    for (const [id, mine] of Object.entries(own)) {
      const { [field]: _dropped, ...rest } = mine
      put(id, rest)
    }
    return { all: { ...all, [field]: value }, own: out }
  }
  const chosen = new Set(selected)
  for (const [id, mine] of Object.entries(own)) if (!chosen.has(id)) put(id, mine)
  for (const id of selected) {
    const { [field]: _old, ...rest } = own[id] ?? {}
    put(id, all[field] === value ? rest : { ...rest, [field]: value })
  }
  return { all, own: out }
}

/** The value these cards share for a field, or undefined when they differ (none: undefined). */
export function sharedChoice<F extends ChoiceField>(all: DeathChoice, own: OwnChoices, ids: string[], field: F): DeathChoice[F] | undefined {
  if (!ids.length) return undefined
  const first = choiceFor(all, own, ids[0])[field]
  return ids.every(id => choiceFor(all, own, id)[field] === first) ? first : undefined
}

/** Own values of cards no longer chosen are forgotten. */
export function keepOwn(own: OwnChoices, ids: string[]): OwnChoices {
  const keep = new Set(ids)
  return Object.fromEntries(Object.entries(own).filter(([id]) => keep.has(id)))
}

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
 * The cells "Save" writes for one card, with its own date, cause,
 * preservation and note (choiceFor): what the table's «Escribir fecha y causa»
 * writes (deathCells), plus, for a body preserved now, its CAM, tube (`sample`)
 * and the medium; then the note, dated `today` and signed with `initials`.
 * A butterfly already recorded dead gets no tube here (Tubos does), but its note.
 */
export function cardCells(
  row: TableRow,
  get: Getter,
  choice: DeathChoice,
  {
    sample,
    medium,
    today,
    initials = '',
  }: { sample?: { cam: string; tube: string }; medium: string; today: number; initials?: string },
): DeathCell[] {
  const serial = choice.date ? serialFromIso(choice.date) : null
  const dying = lifeOf(f => get(row, f)).state !== 'dead'
  const preserve =
    choice.preserved && dying
      ? { cam: sample?.cam.trim().toUpperCase() || '', tube: sample?.tube.trim().toUpperCase() || '', medium }
      : undefined
  const cells = deathCells(row, get, { serial, cause: choice.cause, notPreserved: !choice.preserved, preserve })
  const note = noteCell(row, get, choice.note ?? '', today, initials)
  return note ? [...cells, note] : cells
}

// --- Choosing: one butterfly at a time, or several as an explicit group

/** What is chosen: the Insectary IDs (one, unless `several`), and whether taps build a group. */
export interface Picking {
  picked: string[]
  several: boolean
}
/** A new choice, and the IDs it leaves behind whose registering is kept (setAside). */
export interface PickResult extends Picking {
  left: string[]
}
const sameId = (a: string, b: string) => searchKey(a) === searchKey(b)

/**
 * One ID chosen (a suggestion, Enter, «¿Quisiste decir?»). One at a time it
 * replaces the butterfly there (left behind); in several, or with `add`
 * (Ctrl or Shift click, a long press), it joins the group, and leaves it when
 * already in (nothing left behind: taking it out drops what it had).
 */
export function pickOne(p: Picking, id: string, add = false): PickResult {
  const has = p.picked.some(x => sameId(x, id))
  if (p.several || add) {
    if (!has) return { picked: [...p.picked, id], several: true, left: [] }
    return p.several ? { picked: p.picked.filter(x => !sameId(x, id)), several: true, left: [] } : { ...p, left: [] }
  }
  if (has && p.picked.length === 1) return { ...p, left: [] }
  return { picked: [id], several: false, left: p.picked.filter(x => !sameId(x, id)) }
}

/**
 * Several IDs at once (a range B0D-B9D, a list pasted): they become the group
 * (after the ones already in it, in several), and the butterfly there one at a
 * time is left behind unless it is one of them. A single ID is pickOne.
 */
export function pickMany(p: Picking, ids: string[]): PickResult {
  if (!ids.length) return { ...p, left: [] }
  if (ids.length === 1) return pickOne(p, ids[0])
  const picked = p.several ? [...p.picked] : []
  for (const id of ids) if (!picked.some(x => sameId(x, id))) picked.push(id)
  const left = p.several ? [] : p.picked.filter(x => !picked.some(y => sameId(x, y)))
  return { picked, several: true, left }
}

/** «Seleccionar varias»: the butterfly there starts the group. */
export const startSeveral = (p: Picking): Picking => ({ picked: p.picked, several: true })
/** Leaving several («Una a una», «Vaciar»): back to one at a time, keeping a group of one, else with none chosen. */
export const endSeveral = (p: Picking): Picking => ({ picked: p.picked.length === 1 ? p.picked : [], several: false })

/** Why a butterfly's death cannot be written yet: '' when it can. */
export type Lack = '' | 'date' | 'bad-date' | 'cause' | 'sample'
export function lackOf(choice: DeathChoice, dying: boolean, gap?: PreservationGap): Lack {
  if (!choice.date) return 'date'
  if (serialFromIso(choice.date) === null) return 'bad-date'
  if (dying && !choice.cause) return 'cause'
  if (gap && hasGap(gap)) return 'sample'
  return ''
}

/**
 * What becomes of butterflies no longer chosen, one at a time: nothing when
 * nothing was registered for them (looked up to see if alive); else those with
 * all they need go to the pending changes (saved like any edit) and the rest
 * wait as unfinished, with what they lack.
 */
export function setAside(
  ids: string[],
  touched: boolean,
  lack: (id: string) => Lack,
): { pending: string[]; unfinished: { id: string; lack: Exclude<Lack, ''> }[] } {
  const out = { pending: [] as string[], unfinished: [] as { id: string; lack: Exclude<Lack, ''> }[] }
  if (!touched) return out
  for (const id of ids) {
    const why = lack(id)
    if (why) out.unfinished.push({ id, lack: why })
    else out.pending.push(id)
  }
  return out
}
