import { isBlank } from './cells'
import { appendNote, noteDay } from './clutches'
import { serialFromIso } from './dates'
import { KILLED, NOT_PRESERVED, WHOLE, firstEmptySlot, placeholder, type DeathCell, type Getter } from './deaths'
import type { CellValue, TableRow } from './types'

/**
 * Giving insectary butterflies their CAM and tubes (Tubos), shared by its
 * cards and its table: what a sample writes in a row, the IDs typed or
 * scanned checked for their form and repeats, and the next free CAMs and
 * tubes handed to the cards in order.
 */

export { WHOLE }
export const WING_CLIP = '**OTHER_SOMATIC_ANIMAL_TISSUE** | WING CLIP'
export const NOTES = 'Notes_Insectary_data'

/**
 * What goes in the tubes: the whole body, a wing clip (the butterfly lives
 * on), a body split in parts (one tube each), or nothing (not preserved after
 * all: CAM and tubes NA, tissues and media NOT_COLLECTED).
 */
export type SampleKind = 'whole' | 'clip' | 'parts' | 'none'
export const kindOfTissue = (tissue: string): SampleKind =>
  tissue === WHOLE ? 'whole' : /WING CLIP/i.test(tissue) ? 'clip' : tissue === 'NOT_COLLECTED' ? 'none' : 'parts'
export const isBody = (kind: SampleKind) => kind === 'whole' || kind === 'parts'

/** How a butterfly's sample is taken. */
export interface TubeChoice {
  kind: SampleKind
  /** The tissue of each tube of a split body ('parts'). */
  parts: string[]
  medium: string
  /** The preservation day (a body) or the day of the clip, ISO. */
  date: string
  /** After a body, the tube columns left: ID NA, tissue and medium NOT_COLLECTED. */
  closeRest: boolean
}
export type ChoiceKey = keyof TubeChoice
/** Values a card has of its own (set with it selected), by Insectary ID; the rest come from the panel. */
export type OwnChoices = Record<string, Partial<TubeChoice>>

/** A card's choice: its own values where it has them, else the panel's. */
export function choiceFor(all: TubeChoice, own: OwnChoices, id: string): TubeChoice {
  const mine = own[id]
  return mine ? { ...all, ...mine } : all
}
/**
 * One value set in the panel: for the selected cards only (their own value,
 * dropped when it equals the panel's), or with none selected for all (no card
 * keeps its own value of that field).
 */
export function setChoice<F extends ChoiceKey>(
  all: TubeChoice,
  own: OwnChoices,
  selected: string[],
  field: F,
  value: TubeChoice[F],
): { all: TubeChoice; own: OwnChoices } {
  const out: OwnChoices = {}
  const put = (id: string, mine: Partial<TubeChoice>) => {
    if (Object.keys(mine).length) out[id] = mine
  }
  const same = (a: unknown, b: unknown) => JSON.stringify(a) === JSON.stringify(b)
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
    put(id, same(all[field], value) ? rest : { ...rest, [field]: value })
  }
  return { all, own: out }
}
/** The value these cards share for a field, or undefined when they differ. */
export function sharedChoice<F extends ChoiceKey>(all: TubeChoice, own: OwnChoices, ids: string[], field: F): TubeChoice[F] | undefined {
  if (!ids.length) return undefined
  const first = JSON.stringify(choiceFor(all, own, ids[0])[field])
  return ids.every(id => JSON.stringify(choiceFor(all, own, id)[field]) === first) ? choiceFor(all, own, ids[0])[field] : undefined
}

// --- Slots

/** How many tubes a choice takes. */
export const tubesOf = (choice: TubeChoice) =>
  choice.kind === 'none' ? 0 : choice.kind === 'parts' ? Math.max(1, choice.parts.length) : 1
/** The tissue of each tube of a choice. */
export const tissuesOf = (choice: TubeChoice): string[] =>
  choice.kind === 'whole' ? [WHOLE] : choice.kind === 'clip' ? [WING_CLIP] : choice.kind === 'parts' ? choice.parts : []

/** The empty tube columns a new sample can take, in order (from the first empty one). */
export function freeSlots(get: (field: string) => CellValue): number[] {
  const first = firstEmptySlot(get)
  if (first === null) return []
  const out: number[] = []
  for (let slot = first; slot <= 4 && isBlank(get(`Tube_${slot}_id`)); slot++) out.push(slot)
  return out
}
/** The tubes a row has already (ID and tissue), "NA" left out. */
export function tubesIn(get: (field: string) => CellValue): { slot: number; tube: string; tissue: string }[] {
  const out: { slot: number; tube: string; tissue: string }[] = []
  for (let slot = 1; slot <= 4; slot++) {
    const tube = get(`Tube_${slot}_id`)
    if (isBlank(tube)) continue
    const tissue = get(`Tube_${slot}_tissue`)
    out.push({ slot, tube: String(tube).trim(), tissue: isBlank(tissue) ? '' : String(tissue).trim() })
  }
  return out
}
/** Not preserved yet: no CAM and no tube (only such a row can be marked "not preserved"). */
export const untouched = (get: (field: string) => CellValue) => isBlank(get('CAM_ID')) && isBlank(get('Tube_1_id'))

// --- The cells a card writes

/**
 * The cells one card writes, in order, only where a cell is empty or NA
 * (formula cells never; `overwrite` also over a NOT_COLLECTED placeholder):
 * - a body (whole or in parts): the CAM when the row has none, each tube with
 *   its tissue and medium in the row's free tube columns, then what Tubos and
 *   Muertes write for a preserved body (Preservation_date and Death_date,
 *   Death_cause Killed_Preserved if empty, Preserved_Dead_Alive,
 *   Preservation_medium NOT_COLLECTED, Location_body Ikiam) and, with
 *   `closeRest`, the tube columns left as NA / NOT_COLLECTED;
 * - a wing clip: the CAM, the tube, its tissue and medium, and the note
 *   "d/m/yy INI: Wing clip d/m/yy" (the clip's day: there is no column for it);
 * - not preserved: CAM and tubes NA, tissues and media NOT_COLLECTED (rows
 *   without CAM or tube only).
 */
export function tubeCells(
  row: TableRow,
  get: Getter,
  choice: TubeChoice,
  sample: { cam: string; tubes: string[] },
  { today, initials }: { today: number; initials: string },
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
  const put = (field: string, v: CellValue) => set(field, v, placeholder(value(field)))
  if (choice.kind === 'none') {
    if (untouched(value)) for (const [field, v] of Object.entries(NOT_PRESERVED)) set(field, v)
    return out
  }
  const slots = freeSlots(value)
  const tissues = tissuesOf(choice)
  if (!slots.length || !tissues.length) return out
  if (isBlank(value('CAM_ID')) && sample.cam.trim()) set('CAM_ID', sample.cam.trim().toUpperCase())
  let last = 0
  tissues.forEach((tissue, i) => {
    const slot = slots[i]
    const tube = sample.tubes[i]?.trim().toUpperCase()
    if (slot === undefined || !tube) return
    set(`Tube_${slot}_id`, tube)
    put(`Tube_${slot}_tissue`, tissue)
    if (slot <= 2) put(`T${slot}_Preservation_medium`, choice.medium)
    last = slot
  })
  if (!last) return out
  const serial = choice.date ? serialFromIso(choice.date) : null
  if (choice.kind === 'clip') {
    // Wing clips have no date column: the day goes in the notes, as the team writes it.
    if (serial === null || row.formulas.includes(NOTES)) return out
    const clipped = `Wing clip ${noteDay(serial)}`
    const notes = value(NOTES)
    if (!String(notes ?? '').includes(clipped)) set(NOTES, appendNote(notes, clipped, today, initials), true)
    return out
  }
  const why = value('Death_cause')
  if (serial !== null) {
    put('Preservation_date', serial)
    put('Death_date', serial)
  }
  if (isBlank(why)) put('Death_cause', KILLED)
  put('Preserved_Dead_Alive', isBlank(why) || why === KILLED ? 'Alive' : 'Dead')
  put('Preservation_medium', 'NOT_COLLECTED')
  put('Location_body', 'Ikiam')
  if (choice.closeRest)
    for (let next = last + 1; next <= 4; next++) {
      set(`Tube_${next}_id`, 'NA')
      put(`Tube_${next}_tissue`, 'NOT_COLLECTED')
      if (next <= 2) put(`T${next}_Preservation_medium`, 'NOT_COLLECTED')
    }
  return out
}

// --- The form of a CAM or tube typed or scanned

/** Upper case, spaces gone (a scanner or a phone keyboard may add them). */
export const normalizeId = (text: string) => text.toUpperCase().replace(/\s+/g, '')
const TUBE = /^([A-Z]{2})(\d+)$/
/** FluidX tubes: two letters and eight digits (FS90415474). */
export const TUBE_DIGITS = 8
const CAM = /^CAM(\d+)$/
export const CAM_DIGITS = 6

export interface FormProblem {
  /** `cam`: a CAM typed in a tube box; `tube`: a tube in a CAM box; `digits`: a digit too few or too many; `format`: neither. */
  problem: 'format' | 'digits' | 'cam' | 'tube'
  /** The reading one digit away that lands next to `near` (the rack's run, the other cards' IDs). */
  fix?: string
}
/**
 * Whether a tube (or CAM, `kind`) is written as one: a FluidX tube has two
 * letters and eight digits; a 7- or 9-digit one is a dropped or doubled digit
 * (FS5848994 for FS50848994), and the reading closest to `near` is offered.
 */
export function formProblem(kind: 'cam' | 'tube', text: string, near: string[] = []): FormProblem | null {
  const value = normalizeId(text)
  if (!value || value === 'NA') return null
  if (kind === 'tube') {
    if (CAM.test(value)) return { problem: 'cam' }
    const m = TUBE.exec(value)
    if (!m) return { problem: 'format' }
    if (m[2].length === TUBE_DIGITS) return null
    return { problem: 'digits', fix: closestReading(m[1], m[2], TUBE_DIGITS, near) }
  }
  if (TUBE.test(value) && !value.startsWith('CA')) return { problem: 'tube' }
  const m = CAM.exec(value)
  if (!m) return { problem: 'format' }
  if (m[1].length === CAM_DIGITS) return null
  return { problem: 'digits', fix: closestReading('CAM', m[1], CAM_DIGITS, near) }
}
/** Every number one digit away in length (a digit put in anywhere, or one taken out) with the right length. */
export function readings(digits: string, length: number): string[] {
  const out = new Set<string>()
  if (digits.length === length - 1)
    for (let i = 0; i <= digits.length; i++) for (let d = 0; d <= 9; d++) out.add(`${digits.slice(0, i)}${d}${digits.slice(i)}`)
  if (digits.length === length + 1) for (let i = 0; i < digits.length; i++) out.add(digits.slice(0, i) + digits.slice(i + 1))
  return [...out].filter(r => r.length === length)
}
/** The reading within 50 of one of `near` (same prefix), closest first; none when nothing is near. */
function closestReading(prefix: string, digits: string, length: number, near: string[]): string | undefined {
  const anchors = near
    .map(normalizeId)
    .filter(v => v.startsWith(prefix) && /^\d+$/.test(v.slice(prefix.length)) && v.length - prefix.length === length)
    .map(v => Number(v.slice(prefix.length)))
    .filter(n => Number.isFinite(n))
  let best: { value: string; gap: number } | undefined
  for (const r of readings(digits, length)) {
    const n = Number(r)
    for (const a of anchors) {
      const gap = Math.abs(n - a)
      if (gap <= 50 && (!best || gap < best.gap)) best = { value: `${prefix}${r}`, gap }
    }
  }
  return best?.value
}

/** Whether this browser can read barcodes with the camera (Chrome on Android can; TubeScanner). */
export const canScan = () =>
  typeof window !== 'undefined' && 'BarcodeDetector' in window && !!navigator.mediaDevices?.getUserMedia

/** The ID after this one ("FS90415474" → "FS90415475"), keeping the digits' width. */
export function nextAfter(id: string): string {
  const m = /^([A-Za-z]+)(\d+)$/.exec(id.trim())
  return m ? `${m[1].toUpperCase()}${String(Number(m[2]) + 1).padStart(m[2].length, '0')}` : id
}
/** `count` IDs from `start` on, skipping those in `used` (a stand-in until the server's run arrives). */
export function localRun(start: string, count: number, used: (id: string) => boolean = () => false): string[] {
  const out: string[] = []
  let id = normalizeId(start)
  if (!/^[A-Z]+\d+$/.test(id)) return out
  for (let guard = 0; out.length < count && guard < count + 5000; guard++, id = nextAfter(id)) if (!used(id)) out.push(id)
  return out
}

// --- The next free CAMs and tubes, handed to the cards in order

/** What a card needs: a CAM (it has none yet) and how many tubes. */
export interface Need {
  id: string
  cam: boolean
  tubes: number
}
/**
 * What the person typed (or scanned) on a card: undefined follows the
 * suggestion; '' is a box emptied on purpose (left empty, never refilled).
 */
export interface Typed {
  cam?: string | null
  tubes?: (string | null | undefined)[]
}
export interface Assigned {
  cam: { value: string; auto: boolean } | null
  tubes: { value: string; auto: boolean }[]
}
/**
 * The CAM and tubes of each card, in the cards' order: what was typed stays;
 * the rest take the next free IDs of their run. A run starts at `camStart` /
 * `tubeStart` and, after a well-formed CAM or tube typed on a card, goes on
 * from the one after it (tubes are taken from the rack in order, so a scanned tube says
 * where the rack is). `run(start, count)` gives free IDs from `start` (the
 * server's, skipping IDs used anywhere); an ID typed on any card, or in
 * `taken`, is never handed out again.
 */
export function assign(
  needs: Need[],
  typed: Record<string, Typed | undefined>,
  { camStart, tubeStart, run, taken = new Set() }: { camStart: string; tubeStart: string; run: (start: string, count: number) => string[]; taken?: Set<string> },
): Record<string, Assigned> {
  const reserved = new Set(taken)
  for (const n of needs) {
    const t = typed[n.id]
    if (t?.cam) reserved.add(normalizeId(t.cam))
    for (const v of t?.tubes ?? []) if (v) reserved.add(normalizeId(v))
  }
  const remainingCams = (from: number) => needs.slice(from).filter(n => n.cam).length
  const remainingTubes = (from: number) => needs.slice(from).reduce((s, n) => s + n.tubes, 0)
  /** A run being handed out: its IDs and how many were taken. */
  const runner = (start: string, count: number) => ({ list: start ? run(start, count + reserved.size + 2) : [], at: 0 })
  let cams = runner(camStart, remainingCams(0))
  let tubes = runner(tubeStart, remainingTubes(0))
  const take = (r: { list: string[]; at: number }) => {
    while (r.at < r.list.length) {
      const v = r.list[r.at++]
      if (!reserved.has(v)) {
        reserved.add(v)
        return v
      }
    }
    return ''
  }
  const out: Record<string, Assigned> = {}
  needs.forEach((n, i) => {
    const t = typed[n.id]
    let cam: Assigned['cam'] = null
    if (n.cam) {
      if (t?.cam !== undefined && t.cam !== null) {
        cam = { value: normalizeId(t.cam), auto: false }
        if (cam.value && !formProblem('cam', cam.value)) cams = runner(nextAfter(cam.value), remainingCams(i + 1))
      } else cam = { value: take(cams), auto: true }
    }
    const list: Assigned['tubes'] = []
    for (let k = 0; k < n.tubes; k++) {
      // (null: a box never typed in, as JSON keeps an undefined in a list)
      const v = t?.tubes?.[k]
      if (v !== undefined && v !== null) {
        const value = normalizeId(v)
        list.push({ value, auto: false })
        if (value && !formProblem('tube', value)) tubes = runner(nextAfter(value), remainingTubes(i) - k - 1)
      } else list.push({ value: take(tubes), auto: true })
    }
    out[n.id] = { cam, tubes: list }
  })
  return out
}

// --- What still blocks a card

export type Field = 'cam' | number
export type Problem =
  | { kind: 'slots'; need: number; free: number }
  | { kind: 'date' }
  | { kind: 'badDate' }
  | { kind: 'parts' }
  | { kind: 'missing'; field: Field }
  | { kind: 'repeated'; field: Field; value: string; with: string }
  | { kind: 'used'; field: Field; value: string; where: string }
  | { kind: 'form'; field: Field; value: string; form: FormProblem }

/**
 * What each card still lacks before Save, in the cards' order: room for its
 * tubes, the date, the tissues of a split body, and each CAM and tube: missing,
 * written wrong (unless `accepted`), on two cards, or used already (`used`:
 * ID → where, from the sheets and other unsaved changes).
 */
export function problemsOf(
  cards: { id: string; choice: TubeChoice; free: number; needsCam: boolean; assigned: Assigned | undefined }[],
  { used, accepted = new Set(), near = [] }: { used: Pick<Map<string, string>, 'has' | 'get'>; accepted?: Set<string>; near?: string[] },
): Map<string, Problem[]> {
  const seen = new Map<string, string>()
  const out = new Map<string, Problem[]>()
  for (const card of cards) {
    const list: Problem[] = []
    const { choice } = card
    const need = tubesOf(choice)
    if (choice.kind !== 'none') {
      if (need > card.free) list.push({ kind: 'slots', need, free: card.free })
      if (choice.kind === 'parts' && (!choice.parts.length || choice.parts.some(p => !p))) list.push({ kind: 'parts' })
      if (!choice.date) list.push({ kind: 'date' })
      else if (serialFromIso(choice.date) === null) list.push({ kind: 'badDate' })
      const check = (field: Field, kind: 'cam' | 'tube', value: string) => {
        if (!value) return list.push({ kind: 'missing', field })
        const form = accepted.has(value) ? null : formProblem(kind, value, near)
        if (form) list.push({ kind: 'form', field, value, form })
        const other = seen.get(value)
        if (other !== undefined && other !== card.id) list.push({ kind: 'repeated', field, value, with: other })
        else if (used.has(value)) list.push({ kind: 'used', field, value, where: used.get(value)! })
        seen.set(value, card.id)
      }
      if (card.needsCam) check('cam', 'cam', card.assigned?.cam?.value ?? '')
      card.assigned?.tubes.slice(0, Math.min(need, card.free)).forEach((t, k) => check(k, 'tube', t.value))
    }
    out.set(card.id, list)
  }
  return out
}
