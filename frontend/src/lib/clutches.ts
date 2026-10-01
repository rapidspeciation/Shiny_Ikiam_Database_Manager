import { isBlank } from './cells'
import { simpleSum } from './sums'
import type { CellValue, TableRow } from './types'

/**
 * Clutches (Insectary_stocks) on a phone or tablet: the counts kept as the
 * notebook sums them, which clutches are still going, the parents written in
 * NOTES, and the day's changes as text to copy into the notebook. Shared by the
 * cards screen and its tests; nothing here touches the store.
 */
export const MODULE = 'Insectary_stocks'

/** The counts kept as sums (server/schema.mjs SUM_FIELDS), in the order of the notebook's columns. */
export const COUNTS = [
  'NUMBER OF EGGS',
  'NUMBER OF LARVAE',
  'NUMBER OF PUPA',
  'NUMBER OF ADULTS',
  'NUMBER OF PUPAE/LARVAE FOR DISECTIONS',
] as const
export type CountField = (typeof COUNTS)[number]
/** The stage each count belongs to, and the date column of its first day. */
export const STAGES: { count: CountField; date: string | null; stage: Stage }[] = [
  { count: 'NUMBER OF EGGS', date: 'DATE LAID', stage: 'egg' },
  { count: 'NUMBER OF LARVAE', date: 'HATCHING DATE', stage: 'larva' },
  { count: 'NUMBER OF PUPA', date: 'PUPA DATE', stage: 'pupa' },
  { count: 'NUMBER OF ADULTS', date: 'EMERGENCE DATE', stage: 'adult' },
]
export type Stage = 'egg' | 'larva' | 'pupa' | 'adult'
export const DATES = ['DATE LAID', 'HATCHING DATE', 'PUPA DATE', 'EMERGENCE DATE'] as const

// --- Counts kept as sums: =3+5-2

/**
 * A count as the person sees it: its terms (=3+5-2 → [3, 5, -2]); `na` when
 * the cell says NA (the stage never came); `text` when it holds something that
 * is not a number or a sum (shown as it is, never rewritten).
 */
export interface Count {
  terms: number[]
  na: boolean
  text: string | null
}
export function readCount(value: CellValue | undefined): Count {
  if (value === null || value === undefined) return { terms: [], na: false, text: null }
  if (typeof value === 'number') return Number.isInteger(value) && value >= 0 ? { terms: [value], na: false, text: null } : { terms: [], na: false, text: String(value) }
  const raw = String(value).trim()
  if (!raw) return { terms: [], na: false, text: null }
  if (/^(NA|N\/A)$/i.test(raw)) return { terms: [], na: true, text: null }
  if (/^\d+$/.test(raw)) return { terms: [Number(raw)], na: false, text: null }
  const sum = simpleSum(raw)
  if (!sum) return { terms: [], na: false, text: raw }
  return { terms: (sum.slice(1).match(/[+-]?\d+/g) || []).map(Number), na: false, text: null }
}
export const totalOf = (terms: number[]) => terms.reduce((a, b) => a + b, 0)
/** The sheet's formula for terms: [3, 5, -2] → "=3+5-2"; nothing for no terms. */
export function formulaOf(terms: number[]): string | null {
  if (!terms.length) return null
  return '=' + terms.map((t, i) => (i === 0 ? String(t) : t < 0 ? `-${-t}` : `+${t}`)).join('')
}
/** The terms as chips: 3, +5, −2. */
export const termLabels = (terms: number[]) => terms.map((t, i) => (i === 0 ? String(t) : t < 0 ? `−${-t}` : `+${t}`))
/** "3 +5 −2", compact, for a card. */
export const termsText = (terms: number[]) => termLabels(terms).join(' ')

export type CountResult = { ok: true; terms: number[] } | { ok: false; reason: 'empty' | 'negative' | 'unchanged' | 'first' }

/**
 * One more term: +5 (hatched, pupated, emerged more) or −2 (died, missing). A
 * lone 0 is replaced (=0 then 3 hatched gives =3), the first term cannot be a
 * loss, and the total never goes below 0.
 */
export function appendTerm(terms: number[], n: number): CountResult {
  if (!Number.isInteger(n) || n === 0) return { ok: false, reason: 'empty' }
  const base = terms.length === 1 && terms[0] === 0 ? [] : terms
  if (!base.length && n < 0) return { ok: false, reason: 'first' }
  if (totalOf(base) + n < 0) return { ok: false, reason: 'negative' }
  return { ok: true, terms: [...base, n] }
}
/**
 * "Counted today: N": the difference to the total is added as a term (total 6,
 * counted 3 → −3; counted 8 → +2); the same total changes nothing; an empty
 * count starts at N.
 */
export function countedToday(terms: number[], counted: number): CountResult {
  if (!Number.isInteger(counted) || counted < 0) return { ok: false, reason: 'empty' }
  if (!terms.length) return { ok: true, terms: [counted] }
  const diff = counted - totalOf(terms)
  if (diff === 0) return { ok: false, reason: 'unchanged' }
  if (terms.length === 1 && terms[0] === 0) return { ok: true, terms: [counted] }
  return { ok: true, terms: [...terms, diff] }
}
/** Undoes the last term (yesterday's −3, when the 3 turn up again today). */
export const removeLast = (terms: number[]) => terms.slice(0, -1)

/** The value a count cell gets for its terms: the team's formula, or empty. */
export const countValue = (terms: number[]): CellValue => formulaOf(terms)

/**
 * A count's cell as the person sees it: an unsaved edit first, then the sheet's
 * formula (the table only carries what a formula adds up to), then its value.
 * `formulas`: the sum formulas of the sheet by field (GET clutches/state).
 */
export function countCell(
  row: TableRow,
  field: string,
  get: (row: TableRow, field: string) => CellValue,
  dirty: boolean,
  formulas: Record<string, string> | undefined,
): CellValue {
  if (dirty) return get(row, field)
  return formulas?.[field] ?? row.values[field] ?? null
}
/** A formula cell that is not a count kept as a sum (a real formula of the sheet): never written. */
export const lockedFormula = (row: TableRow, field: string, formulas: Record<string, string> | undefined) =>
  row.formulas.includes(field) && !formulas?.[field]

// --- Which clutches are still going

/** A clutch laid (or first dated) longer ago than this is no longer listed as ongoing. */
export const ONGOING_DAYS = 90
/** After the first emergence, the rest of the pupae emerge within about this many days. */
export const EMERGING_DAYS = 21
/** Rows of the sheet near its end without any date yet (just added) still count as ongoing. */
const UNDATED_TAIL = 40
const ENDED_NOTE =
  /\b(clutch (is )?dead|all (the )?(eggs|larvae|larva|pupae|of them) (are |were )?(dead|died|disappeared)|all (dead|died|disappeared)|no hatch|don'?t hatch|did ?n[o']t hatch|clutch to stock)\b/i

export type Ended = null | 'old' | 'never' | 'none-left' | 'emerged' | 'note'
export interface ClutchState {
  stage: Stage | null
  /** When it started: DATE LAID, else its first stage date. */
  start: number | null
  /** Why it is no longer going, or null while it is. */
  ended: Ended
}
const date = (v: CellValue | undefined) => (typeof v === 'number' && v > 30000 && v < 80000 ? v : null)

/**
 * Where a clutch is and whether it is still going. A clutch has ended when a
 * stage after the first one counted is NA (never came: a failed clutch), its
 * latest count is 0 (all dead, preserved or gone), all its pupae have emerged
 * (or the first emergence was more than three weeks ago), its notes say so
 * ("No hatch", "clutch is dead", "all larvae died"), or it started more than
 * 90 days ago. `count(field)` reads the counts as the person sees them.
 */
export function clutchState(
  values: (field: string) => CellValue,
  count: (field: CountField) => Count,
  today: number,
  { undated = false }: { undated?: boolean } = {},
): ClutchState {
  const start = date(values('DATE LAID')) ?? STAGES.map(s => (s.date ? date(values(s.date)) : null)).find(d => d !== null) ?? null
  let first = -1
  let last = -1
  let never = false
  STAGES.forEach((s, i) => {
    const c = count(s.count)
    if (c.terms.length || c.text) {
      if (first < 0) first = i
      last = i
    } else if (c.na && first >= 0) never = true
  })
  // A stage date set without its count (pupae noted by date only) still says where it is.
  STAGES.forEach((s, i) => {
    if (s.date && date(values(s.date)) !== null && i > last && i > 0) last = i
  })
  const stage = last >= 0 ? STAGES[last].stage : null
  const totals = STAGES.map(s => count(s.count)).map(c => (c.terms.length ? totalOf(c.terms) : null))
  const notes = String(values('NOTES') ?? '')
  let ended: Ended = null
  if (start !== null ? start <= today - ONGOING_DAYS : !undated) ended = 'old'
  else if (never) ended = 'never'
  else if (last >= 0 && totals[last] === 0) ended = 'none-left'
  else if (
    totals[3] !== null &&
    totals[3]! > 0 &&
    (totals[3]! >= (totals[2] ?? 0) || ((date(values('EMERGENCE DATE')) ?? today) <= today - EMERGING_DAYS))
  )
    ended = 'emerged'
  else if (ENDED_NOTE.test(notes)) ended = 'note'
  return { stage, start, ended }
}

/** A row holding a clutch (not only a pre-written number): a species, a date or a count. */
export function hasClutch(values: (field: string) => CellValue): boolean {
  // "NA" counts: a clutch of field larvae has SPECIES and DATE LAID "NA".
  return ['SPECIES', 'DATE LAID', 'HATCHING DATE', 'NOTES', ...COUNTS].some(f => String(values(f) ?? '').trim() !== '')
}
/** Rows near the end of the sheet: an undated clutch there was just added. */
export const undatedTail = (index: number, total: number) => index >= total - UNDATED_TAIL

// --- Clutch numbers

/** "994(2)" → { base: 994, batch: 2 }; "994" → batch 1; "831 (3)" → 3; other forms → null. */
export function clutchNumber(text: string): { base: number; batch: number } | null {
  const m = /^\s*(\d+)\s*(?:\((\d+)\))?\s*$/.exec(text)
  return m ? { base: Number(m[1]), batch: m[2] ? Number(m[2]) : 1 } : null
}
/** The next free plain number: the highest one used + 1. */
export function nextClutch(numbers: string[]): string {
  let max = 0
  for (const n of numbers) {
    const parsed = parseInt(n, 10)
    if (Number.isFinite(parsed) && parsed > max && parsed < 100000) max = parsed
  }
  return String(max + 1)
}
/** The next batch of a mating's clutch: 994 and 994(2) used → "994(3)" (the current form, no space). */
export function nextBatch(base: number, numbers: string[]): string {
  let batch = 0
  for (const n of numbers) {
    const parsed = clutchNumber(n)
    if (parsed && parsed.base === base) batch = Math.max(batch, parsed.batch)
  }
  return batch ? `${base}(${batch + 1})` : String(base)
}

// --- Parents in NOTES

/** The parents as the team writes them, female first: "U8A♀ + C8B♂". */
export const parentsText = (female: string, male: string) => `${female.trim().toUpperCase()}♀ + ${male.trim().toUpperCase()}♂`
/**
 * The parents written in a clutch's NOTES: "U8A♀ + C8B♂" (2026) or the older
 * "F1 clutch parents J7A + P5A" / "J7A+ P5A" (female first in both).
 */
export function parentsOf(notes: CellValue | undefined): { female: string; male: string; text: string } | null {
  const s = String(notes ?? '')
  const m = /([A-Z0-9]{2,6})\s*♀\s*\+\s*([A-Z0-9]{2,6})\s*♂/i.exec(s) ?? /parents\s+([A-Z0-9]{2,6})\s*\+\s*([A-Z0-9]{2,6})/i.exec(s)
  return m ? { female: m[1].toUpperCase(), male: m[2].toUpperCase(), text: m[0] } : null
}
/** A clutch of the same mating (the same parents, in the same order), to number the next batch. */
export function sameMating(rows: { number: string; notes: CellValue }[], female: string, male: string): string | null {
  const f = female.trim().toUpperCase()
  const m = male.trim().toUpperCase()
  if (!f || !m) return null
  let found: { base: number } | null = null
  for (const r of rows) {
    const p = parentsOf(r.notes)
    const n = clutchNumber(r.number)
    if (p && n && p.female === f && p.male === m) found = n
  }
  return found ? String(found.base) : null
}

// --- Notes

/** "1/10/26": the day a note is written, as the team dates notes. */
export function noteDay(serial: number): string {
  const d = new Date(Date.UTC(1899, 11, 30) + serial * 86_400_000)
  return `${d.getUTCDate()}/${d.getUTCMonth() + 1}/${String(d.getUTCFullYear()).slice(2)}`
}
/** A new note after the old ones, "d/m/yy INI: text", joined with " | " (newest last). */
export function appendNote(current: CellValue | undefined, text: string, today: number, initials: string): string {
  const note = `${noteDay(today)} ${initials}: ${text.trim()}`
  const old = isBlank(current) ? '' : String(current).trim()
  return old ? `${old} | ${note}` : note
}
/** The notes of a cell one by one (as written, joined with " | "). */
export const notesOf = (value: CellValue | undefined) =>
  isBlank(value)
    ? []
    : String(value)
        .split(/\s+\|\s+/)
        .map(s => s.trim())
        .filter(Boolean)

/** Phrases the team writes in clutch notes (English, as in the sheet): quick buttons. */
export const NOTE_PHRASES = [
  'Some eggs dry',
  'Some eggs with fungi',
  'No hatch',
  '1 larva dead',
  '1 pupa dead',
  'Plant with ants',
  'Plant with fungi',
  'Plant changed',
]

// --- The day's changes, to copy into the notebook

export interface DayChange {
  recordId: string
  clutch: string
  species: string
  field: string
  before: CellValue | { formula: string }
  after: CellValue | { formula: string }
  actors: string[]
  isNew: boolean
}
const plain = (v: DayChange['before']): CellValue => (v && typeof v === 'object' ? v.formula : v)
/** A count's value in a change: "=12+13 (25)". */
function countText(v: DayChange['before']): string {
  const value = plain(v)
  if (isBlank(value)) return '—'
  const c = readCount(value)
  if (c.text) return c.text
  const f = formulaOf(c.terms)
  return c.terms.length > 1 ? `${f} (${totalOf(c.terms)})` : (f ?? String(value))
}
/**
 * One field's change in words: a count as the terms added and its totals
 * ("−3 (6 → 3) · =3+5-2-3"; a term taken back: "removed −3"), or as both sums;
 * dates day-first; a note as what was added to it ("+ 1/10/26 FCH: …").
 */
export function changeText(
  c: Pick<DayChange, 'field' | 'before' | 'after'>,
  formatDate: (serial: number) => string,
  removedWord = 'removed',
): string {
  const before = plain(c.before)
  const after = plain(c.after)
  if ((COUNTS as readonly string[]).includes(c.field)) {
    // What the notebook needs: the terms added (or taken back) and the totals, then the whole sum.
    const a = readCount(before)
    const b = readCount(after)
    if (!a.text && !b.text && !a.na && !b.na && a.terms.length && b.terms.length) {
      const [short, long] = a.terms.length <= b.terms.length ? [a.terms, b.terms] : [b.terms, a.terms]
      if (short.every((t, i) => t === long[i]) && short.length !== long.length) {
        const diff = termLabels(long).slice(short.length).map(l => (/^[+−]/.test(l) ? l : `+${l}`))
        const verb = a.terms.length < b.terms.length ? diff.join(' ') : `${removedWord} ${diff.join(' ')}`
        return `${verb} (${totalOf(a.terms)} → ${totalOf(b.terms)}) · ${formulaOf(b.terms)}`
      }
    }
    return `${countText(c.before)} → ${countText(c.after)}`
  }
  const show = (v: CellValue) => (isBlank(v) && !/^NA$/i.test(String(v ?? '')) ? '—' : (DATES as readonly string[]).includes(c.field) && typeof v === 'number' ? formatDate(v) : String(v))
  if (c.field === 'NOTES') {
    const old = isBlank(before) ? '' : String(before).trim()
    const now = isBlank(after) ? '' : String(after).trim()
    if (old && now.startsWith(old)) return `+ ${now.slice(old.length).replace(/^\s*\|\s*/, '')}`
    return `${old || '—'} → ${now || '—'}`
  }
  return `${show(before)} → ${show(after)}`
}
/** The day's changes as plain text, one clutch per line, to copy into the paper notebook. */
export function dayText(
  title: string,
  clutches: { clutch: string; species: string; changes: Pick<DayChange, 'field' | 'before' | 'after'>[] }[],
  formatDate: (serial: number) => string,
  removedWord = 'removed',
): string {
  const lines = [title]
  for (const c of clutches) {
    lines.push('')
    lines.push(c.species ? `${c.clutch} · ${c.species}` : c.clutch)
    for (const ch of c.changes) lines.push(`  ${ch.field}: ${changeText(ch, formatDate, removedWord)}`)
  }
  return lines.join('\n')
}
