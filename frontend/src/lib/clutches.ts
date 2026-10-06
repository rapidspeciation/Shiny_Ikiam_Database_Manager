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
/**
 * The new total typed over a count's total (tapping "32" and typing 30): the
 * same as "Counted today", so the difference is added to the sum (32 → 30
 * adds −2, 32 → 35 adds +3, the same total changes nothing). Up to four digits.
 */
export function typedTotal(terms: number[], typed: string): CountResult {
  const text = typed.trim()
  if (!/^\d{1,4}$/.test(text)) return { ok: false, reason: 'empty' }
  return countedToday(terms, Number(text))
}
/**
 * What a result does to the count, in short, for its button: the term added
 * ("+3", "−2"), "= 5" when the count starts (or a lone 0 is replaced), "" when
 * nothing would change or the number is not valid.
 */
export function effectLabel(terms: number[], result: CountResult): string {
  if (!result.ok) return ''
  const base = terms.length === 1 && terms[0] === 0 ? [] : terms
  if (!base.length || result.terms.length <= base.length) return `= ${totalOf(result.terms)}`
  const added = result.terms[result.terms.length - 1]
  return added < 0 ? `−${-added}` : `+${added}`
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
/**
 * The next new clutch number: one more than the highest number used (batches
 * count by their number: 1012(3) is 1012), never one already taken. `used`:
 * the numbers of the rows holding a clutch and of the clutches being added.
 */
export function nextClutch(used: string[]): string {
  let max = 0
  const taken = new Set<string>()
  for (const n of used) {
    const text = n.trim()
    taken.add(text)
    const parsed = parseInt(text, 10)
    if (Number.isFinite(parsed) && parsed > max && parsed < 100000) max = parsed
  }
  let next = max + 1
  while (taken.has(String(next))) next++
  return String(next)
}
/**
 * The next batch of a mating's clutch, in the current form (no space): 994
 * and 994(2) used → "994(3)"; only 1016 → "1016(2)"; the older "831 (3)" →
 * "831(4)"; a number not used yet → itself.
 */
export function nextBatch(base: number, numbers: string[]): string {
  let batch = 0
  for (const n of numbers) {
    const parsed = clutchNumber(n)
    if (parsed && parsed.base === base) batch = Math.max(batch, parsed.batch)
  }
  return batch ? `${base}(${batch + 1})` : String(base)
}

/** A clutch (all the batches of one number) as the new clutch's number list shows it. */
export interface ClutchOption {
  base: number
  /** Its batches so far: 994 … 994(8). */
  numbers: string[]
  /** The number the new row gets: the next batch, 994(9). */
  next: string
  species: string
  generation: string
  parents: { female: string; male: string } | null
  /** DATE LAID of its latest batch (a date serial), if any. */
  laid: number | null
}
/**
 * Every clutch number with its batches, newest first (the latest batch's row
 * last in the sheet), each with what a new batch takes from it: species,
 * generation and parents (from the latest batch whose NOTES have them).
 */
export function clutchOptions(rows: { number: string; species: CellValue; generation: CellValue; laid: CellValue; notes: CellValue }[], numbers: string[]): ClutchOption[] {
  const byBase = new Map<number, { rows: typeof rows; last: number }>()
  rows.forEach((r, i) => {
    const n = clutchNumber(r.number)
    if (!n) return
    const entry = byBase.get(n.base) ?? { rows: [], last: i }
    entry.rows.push(r)
    entry.last = i
    byBase.set(n.base, entry)
  })
  const text = (v: CellValue) => (isBlank(v) ? '' : String(v).trim())
  return [...byBase.entries()]
    .sort((a, b) => b[1].last - a[1].last)
    .map(([base, { rows: batch }]) => {
      const latest = batch[batch.length - 1]
      const withParents = [...batch].reverse().find(r => parentsOf(r.notes))
      const parents = withParents ? parentsOf(withParents.notes) : null
      const laid = [...batch].reverse().map(r => r.laid).find(v => typeof v === 'number')
      return {
        base,
        numbers: batch.map(r => r.number.trim()),
        next: nextBatch(base, numbers),
        species: text([...batch].reverse().map(r => r.species).find(v => !isBlank(v)) ?? latest.species),
        generation: text([...batch].reverse().map(r => r.generation).find(v => !isBlank(v)) ?? null),
        parents: parents ? { female: parents.female, male: parents.male } : null,
        laid: typeof laid === 'number' ? laid : null,
      }
    })
}

// --- Parents in NOTES

/** The parents as the team writes them, female first: "U8A♀ + C8B♂". */
export const parentsText = (female: string, male: string) => `${female.trim().toUpperCase()}♀ + ${male.trim().toUpperCase()}♂`
/** An Insectary ID in an older note: 3–4 capitals and digits, with at least one of each (U7A, 22L, 91Z). */
const NOTE_ID = String.raw`(?=[A-Z]*\d)(?=\d*[A-Z])[A-Z0-9]{3,4}`
const PARENTS_MARKED = /([A-Z0-9]{2,6})\s*♀\s*\+\s*([A-Z0-9]{2,6})\s*♂/i
const PARENTS_PAIR = new RegExp(String.raw`(?<![A-Za-z0-9])(${NOTE_ID})\s*\+\s*(${NOTE_ID})(?![A-Za-z0-9])`)
export interface WrittenParents {
  female: string
  male: string
  /** The part of NOTES that names them ("U8A♀ + C8B♂", "J7A+ P5A"). */
  text: string
  /** Where that part starts in NOTES. */
  index: number
}
/**
 * The parents written in a clutch's NOTES, female first: "U8A♀ + C8B♂" (the
 * 2026 form, looked for first), or a pair of IDs joined by "+" in the older
 * forms ("F1 clutch parents J7A + P5A", "J7A+ P5A", "F1F2 --> 0AW+6CI",
 * "F1/F2 mom 22L+20L"). Counts such as "1+1 larvae" are not IDs.
 */
export function parentsOf(notes: CellValue | undefined): WrittenParents | null {
  const s = isBlank(notes) ? '' : String(notes)
  const m = PARENTS_MARKED.exec(s) ?? PARENTS_PAIR.exec(s)
  return m ? { female: m[1].toUpperCase(), male: m[2].toUpperCase(), text: m[0], index: m.index } : null
}
/**
 * NOTES with the parents changed: the part that names them is rewritten in the
 * standard form ("F1 clutch parents J7A+ P5A | …" → "F1 clutch parents J7A♀ +
 * P5A♂ | …"), the rest kept as written; with none written, a new dated note
 * "1/10/26 FCH: U8A♀ + C8B♂" goes after the others.
 */
export function withParents(notes: CellValue | undefined, female: string, male: string, today: number, initials: string): string {
  const text = parentsText(female, male)
  const found = parentsOf(notes)
  if (!found) return appendNote(notes, text, today, initials)
  const s = String(notes)
  return s.slice(0, found.index) + text + s.slice(found.index + found.text.length)
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
/**
 * NOTES without one note added in this session (an event taken back): its
 * last occurrence goes, with the " | " that joined it; the rest stays as written.
 */
export function withoutNote(current: CellValue | undefined, note: string): CellValue {
  const text = isBlank(current) ? '' : String(current)
  const at = note ? text.lastIndexOf(note) : -1
  if (at < 0) return current ?? null
  const before = text.slice(0, at).replace(/\s*\|\s*$/, '')
  const after = text.slice(at + note.length).replace(/^\s*\|\s*/, '')
  const out = before && after ? `${before} | ${after}` : before || after
  return out.trim() ? out : null
}
/** The notes of a cell one by one (as written, joined with " | "). */
export const notesOf = (value: CellValue | undefined) =>
  isBlank(value)
    ? []
    : String(value)
        .split(/\s+\|\s+/)
        .map(s => s.trim())
        .filter(Boolean)

/**
 * A note split into who wrote it and when ("29/9/26 FCH", "6 JUN 24 KG") and
 * its text, to show them apart; a note without that start is all text.
 */
export function noteParts(note: string): { head: string; text: string } {
  const m = /^(\d{1,2}[/-]\d{1,2}[/-]\d{2,4}|\d{1,2} [A-Za-z]{3,4} \d{2,4})\s+([A-Z]{1,4})\s*:\s*/.exec(note)
  return m ? { head: `${m[1]} ${m[2]}`, text: note.slice(m[0].length) } : { head: '', text: note }
}

/** Phrases the team writes in clutch notes (English, as in the sheet): quick buttons. */
export const NOTE_PHRASES = [
  'Some eggs dry',
  'Some eggs with fungi',
  'All eggs turn black',
  'No hatch',
  '1 larva dead',
  '1 pupa dead',
  'Plant with ants',
  'Plant with fungi',
  'Plant changed',
  'Larvae moved to another plant',
  'Larvae dissected for cell culture',
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
  /** Kept in the app (Emergidos, Clutches), not in Google Sheets yet (server/staged.mjs). */
  staged?: boolean
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
/** The day's changes as plain text, one clutch per line, to copy into the paper notebook (with its events, already worded). */
export function dayText(
  title: string,
  clutches: { clutch: string; species: string; changes: Pick<DayChange, 'field' | 'before' | 'after'>[]; events?: string[] }[],
  formatDate: (serial: number) => string,
  removedWord = 'removed',
): string {
  const lines = [title]
  for (const c of clutches) {
    lines.push('')
    lines.push(c.species ? `${c.clutch} · ${c.species}` : c.clutch)
    for (const ch of c.changes) lines.push(`  ${ch.field}: ${changeText(ch, formatDate, removedWord)}`)
    for (const e of c.events ?? []) lines.push(`  ${e}`)
  }
  return lines.join('\n')
}

// --- Daily review marks (only in the app): checked, or checked but someone should look again

export type ReviewState = 'none' | 'checked' | 'verify'
/** A clutch's marks of the day: the latest one says where it stands. */
export function reviewState(checks: { state?: string; createdAt: string }[]): ReviewState {
  if (!checks.length) return 'none'
  const latest = [...checks].sort((a, b) => a.createdAt.localeCompare(b.createdAt)).at(-1)!
  return latest.state === 'verify' ? 'verify' : 'checked'
}
/** Why a clutch needs someone to look again, in English as the notes: quick buttons. */
export const VERIFY_REASONS = ["Couldn't find all larvae", 'Count unsure', 'Larvae on another plant', 'Check the plant', 'Pupae to count']
/** The order that brings what is left to do first: to verify, then not looked at yet, then checked. */
export const REVIEW_ORDER: Record<ReviewState, number> = { verify: 0, none: 1, checked: 2 }

// --- Events the paper cannot hold (only in the app): hatched, died, disappeared, preserved

export type EventKind = 'laid' | 'hatched' | 'pupated' | 'emerged' | 'died' | 'disappeared' | 'preserved'
export type Loss = 'died' | 'disappeared' | 'preserved'
/** What can happen to each stage (server/clutches.mjs EVENT_KINDS): its gain first, then the losses. */
export const EVENT_KINDS: Record<Stage, EventKind[]> = {
  egg: ['laid', 'died', 'disappeared', 'preserved'],
  larva: ['hatched', 'died', 'disappeared', 'preserved'],
  pupa: ['pupated', 'died', 'disappeared', 'preserved'],
  adult: ['emerged'],
}
export const LOSSES: Loss[] = ['died', 'disappeared', 'preserved']
/** The stage a count belongs to (none for the dissections). */
export const stageOfCount = (field: string): Stage | null => STAGES.find(s => s.count === field)?.stage ?? null
/** The stage's gain: hatched for larvae, pupated for pupae… */
export const gainOf = (stage: Stage): EventKind => EVENT_KINDS[stage][0]
/** A stage whose losses can be told apart (eggs, larvae, pupae: adults leave the clutch for Insectary_data). */
export const hasLosses = (stage: Stage | null): stage is Stage => !!stage && EVENT_KINDS[stage].length > 1

export interface ClutchEvent {
  id: string
  recordId: string
  clutch: string | null
  /** The day it happened (ISO). */
  day: string
  stage: Stage
  kind: EventKind
  count: number
  ids: string[]
  note: string | null
  actor: string
  username: string | null
  name: string | null
  actionId: string | null
  createdAt: string
}
/** An egg or larva of the clutch registered one by one in Insectary_data (Emergidos). */
export interface YoungRow {
  id: string
  clutch: string
  stage: Stage
  lifestage: string
  kind: 'preserved' | 'died'
  day: string | null
}
export interface StageTally {
  gained: number
  died: number
  disappeared: number
  preserved: number
}
export type ClutchTallies = Partial<Record<Stage, StageTally>>

/** Insectary IDs typed in one box ("h0e, H1E h2e") → ["H0E", "H1E", "H2E"]; what is not an ID is left out. */
export function parseIds(text: string): string[] {
  const out: string[] = []
  for (const raw of text.toUpperCase().split(/[\s,;]+/)) if (/^[A-Z0-9]{2,8}(\.\d{1,2})?$/.test(raw) && !out.includes(raw)) out.push(raw)
  return out
}

/** Whether a loss is taken off the count: always, except preserved ones when the team keeps them counted. */
export const lossTakesOff = (kind: Loss, subtractPreserved: boolean) => kind !== 'preserved' || subtractPreserved

// --- The note an event writes in NOTES (English, as the sheet's notes)

const NOUN: Record<Stage, [string, string]> = { egg: ['egg', 'eggs'], larva: ['larva', 'larvae'], pupa: ['pupa', 'pupae'], adult: ['adult', 'adults'] }
/** "3rd instar larva" → "3rd instar", "Pre-pupa" → "prepupae", for "preserved as …". */
function asStage(lifestage: string, count: number): string {
  const s = lifestage.trim()
  if (!s || /^egg$/i.test(s)) return ''
  if (/^pre-?pupa$/i.test(s)) return count === 1 ? 'prepupa' : 'prepupae'
  return s.replace(/\s+larvae?$/i, '')
}
/**
 * What an event says in the clutch's NOTES, after "d/m/yy INI: ", as the team
 * writes it: "5 larvae died", "2 larvae disappeared", "5 larvae preserved as
 * 3rd instar (R0C, R1C)", "3 larvae hatched", "3 pupated", "4 adults emerged",
 * "7 eggs laid"; with the day it happened when that was not the day written
 * ("… on 4/10/26"): the sheet keeps one date per stage, the notes the rest.
 */
export function eventNote(
  e: { stage: Stage; kind: EventKind; count: number; ids?: string[]; lifestage?: string; day?: number | null },
  today: number,
): string {
  const [one, many] = NOUN[e.stage]
  const noun = e.count === 1 ? one : many
  let text: string
  switch (e.kind) {
    case 'laid':
      text = `${e.count} ${noun} laid`
      break
    case 'hatched':
      text = `${e.count} ${e.count === 1 ? 'larva' : 'larvae'} hatched`
      break
    case 'pupated':
      text = `${e.count} pupated`
      break
    case 'emerged':
      text = `${e.count} ${e.count === 1 ? 'adult' : 'adults'} emerged`
      break
    case 'preserved': {
      const as = e.stage === 'larva' && e.lifestage ? asStage(e.lifestage, e.count) : ''
      text = `${e.count} ${noun} preserved${as ? ` as ${as}` : ''}`
      break
    }
    default:
      text = `${e.count} ${noun} ${e.kind}`
  }
  if (e.ids?.length) text += ` (${e.ids.join(', ')})`
  if (e.day !== undefined && e.day !== null && e.day !== today) text += ` on ${noteDay(e.day)}`
  return text
}

// --- What is in the cage now, and when the next stages come

/** What a count gained: its positive terms (=12+5-2 → 17: hatched, pupated, emerged so far). */
export const gainsOf = (terms: number[]) => terms.reduce((a, t) => (t > 0 ? a + t : a), 0)

export interface Expected {
  /** Eggs not hatched yet (null: no eggs counted). */
  eggs: number | null
  /** Larvae that should be in the cage to count today. */
  larvae: number | null
  /** Pupae that should be in the cage. */
  pupae: number | null
}
/**
 * What should be in the cage today, from the sheet's counts as the team keeps
 * them (a stage's count is never lowered when they move on: NUMBER OF LARVAE
 * holds the larvae that hatched, less those that died or disappeared) and the
 * preserved ones the app knows of: eggs = eggs − hatched; larvae = larvae −
 * pupated; pupae = pupae − emerged; each less its preserved ones when the team
 * keeps them counted (`subtractPreserved` false). Never below 0.
 */
export function expectedNow(
  counts: { eggs: Count; larvae: Count; pupae: Count; adults: Count },
  preserved: { egg?: number; larva?: number; pupa?: number },
  subtractPreserved: boolean,
): Expected {
  const total = (c: Count) => (c.terms.length ? totalOf(c.terms) : null)
  const left = (c: Count, next: Count, kept: number | undefined) => {
    const t = total(c)
    if (t === null) return null
    return Math.max(0, t - gainsOf(next.terms) - (subtractPreserved ? 0 : (kept ?? 0)))
  }
  return {
    eggs: left(counts.eggs, counts.larvae, preserved.egg),
    larvae: left(counts.larvae, counts.pupae, preserved.larva),
    pupae: left(counts.pupae, counts.adults, preserved.pupa),
  }
}

/** Days from laying to hatching (egg), hatching to pupation (larva) and pupation to emergence (pupa). */
export interface Durations {
  egg: number
  larva: number
  pupa: number
  /** Where they come from: the species' clutches, its genus', or all clutches (or the usual days). */
  from: 'species' | 'genus' | 'all'
}
/** The usual days when the sheet has too few clutches (medians of 2023–26: 5, 16 and 8). */
export const USUAL_DURATIONS: Durations = { egg: 5, larva: 16, pupa: 8, from: 'all' }
/** A stage's days are believed within these bounds (a typo gives 300). */
const BOUNDS = { egg: [1, 20], larva: [5, 45], pupa: [3, 25] } as const
const MIN_CLUTCHES = 5
const median = (xs: number[]) => {
  const s = [...xs].sort((a, b) => a - b)
  const m = s.length >> 1
  return s.length % 2 ? s[m] : Math.round((s[m - 1] + s[m]) / 2)
}
export interface StageDurations {
  of: (species: string) => Durations
}
/**
 * Each species' typical days per stage, from the clutches in the sheet (the
 * median of HATCHING DATE − DATE LAID, PUPA DATE − HATCHING DATE, EMERGENCE
 * DATE − PUPA DATE); with fewer than five clutches, its genus' (Mechanitis,
 * Melinaea), else all clutches', else the usual days.
 */
export function stageDurations(rows: { species: CellValue; laid: CellValue; hatch: CellValue; pupa: CellValue; emerge: CellValue }[]): StageDurations {
  type Lists = { egg: number[]; larva: number[]; pupa: number[] }
  const species = new Map<string, Lists>()
  const genus = new Map<string, Lists>()
  const all: Lists = { egg: [], larva: [], pupa: [] }
  const at = (map: Map<string, Lists>, key: string) => map.get(key) ?? (map.set(key, { egg: [], larva: [], pupa: [] }), map.get(key)!)
  for (const r of rows) {
    const name = isBlank(r.species) ? '' : String(r.species).trim()
    const known = !!name && !/^NA$/i.test(name)
    const add = (stage: keyof Lists, from: CellValue, to: CellValue) => {
      const a = date(from)
      const b = date(to)
      if (a === null || b === null) return
      const days = b - a
      const [lo, hi] = BOUNDS[stage]
      if (days < lo || days > hi) return
      all[stage].push(days)
      if (known) {
        at(species, name)[stage].push(days)
        at(genus, name.split(/\s+/)[0])[stage].push(days)
      }
    }
    add('egg', r.laid, r.hatch)
    add('larva', r.hatch, r.pupa)
    add('pupa', r.pupa, r.emerge)
  }
  const memo = new Map<string, Durations>()
  return {
    of(name: string) {
      const key = name.trim()
      const hit = memo.get(key)
      if (hit) return hit
      const out: Durations = { ...USUAL_DURATIONS }
      const sources = new Set<Durations['from']>()
      for (const stage of ['egg', 'larva', 'pupa'] as const) {
        const own = species.get(key)?.[stage] ?? []
        const kin = genus.get(key.split(/\s+/)[0] ?? '')?.[stage] ?? []
        if (own.length >= MIN_CLUTCHES) {
          out[stage] = median(own)
          sources.add('species')
        } else if (kin.length >= MIN_CLUTCHES) {
          out[stage] = median(kin)
          sources.add('genus')
        } else if (all[stage].length >= MIN_CLUTCHES) out[stage] = median(all[stage])
      }
      out.from = sources.has('species') ? 'species' : sources.has('genus') ? 'genus' : 'all'
      memo.set(key, out)
      return out
    },
  }
}

export interface Predicted {
  /** The day each next stage is expected (a date serial), or null when it is not to come. */
  hatch: number | null
  pupa: number | null
  emerge: number | null
}
/**
 * When the next hatching, pupation and emergence are expected: from the day the
 * stage before last grew (the latest laid, hatched or pupated event the app
 * knows of, else the sheet's first date of that stage, else the date expected
 * for it) plus the species' days. A stage is to come while something of the
 * stage before it is expected in the cage (or nothing of it was counted yet
 * and the stage before is coming). A date long gone (more than half the
 * stage's days, at least 3, after it was due: eggs that dried, larvae that
 * pupated uncounted) is not given: what is left will not come.
 */
export function predict(
  dates: { laid: CellValue; hatch: CellValue; pupa: CellValue },
  expected: Expected,
  durations: Durations,
  latest: { laid?: number | null; hatched?: number | null; pupated?: number | null } = {},
  today: number | null = null,
): Predicted {
  const later = (a: number | null | undefined, b: number | null) => (a !== null && a !== undefined && (b === null || a > b) ? a : b)
  const fromLaid = later(latest.laid, date(dates.laid))
  const hatchDay = fromLaid !== null ? fromLaid + durations.egg : null
  const fromHatch = later(latest.hatched, date(dates.hatch)) ?? hatchDay
  const pupaDay = fromHatch !== null ? fromHatch + durations.larva : null
  const fromPupa = later(latest.pupated, date(dates.pupa)) ?? pupaDay
  const emergeDay = fromPupa !== null ? fromPupa + durations.pupa : null
  const eggs = expected.eggs !== null && expected.eggs > 0
  const larvae = expected.larvae !== null ? expected.larvae > 0 : eggs
  const pupae = expected.pupae !== null ? expected.pupae > 0 : larvae
  const live = (day: number | null, days: number) => (day !== null && (today === null || day >= today - Math.max(3, Math.ceil(days / 2))) ? day : null)
  return {
    hatch: eggs ? live(hatchDay, durations.egg) : null,
    pupa: larvae ? live(pupaDay, durations.larva) : null,
    emerge: pupae ? live(emergeDay, durations.pupa) : null,
  }
}

/**
 * A clutch's outlook for the round: what should be in the cage today and when
 * the next stages come, from its counts as the person sees them, the preserved
 * ones the app knows of (ClutchTallies) and the team's setting.
 */
export function outlook(
  values: (field: string) => CellValue,
  count: (field: CountField) => Count,
  tallies: ClutchTallies | undefined,
  subtractPreserved: boolean,
  durations: Durations,
  latest: { laid?: number | null; hatched?: number | null; pupated?: number | null } = {},
  today: number | null = null,
): { expected: Expected; predicted: Predicted } {
  const expected = expectedNow(
    { eggs: count('NUMBER OF EGGS'), larvae: count('NUMBER OF LARVAE'), pupae: count('NUMBER OF PUPA'), adults: count('NUMBER OF ADULTS') },
    { egg: tallies?.egg?.preserved, larva: tallies?.larva?.preserved, pupa: tallies?.pupa?.preserved },
    subtractPreserved,
  )
  const predicted = predict({ laid: values('DATE LAID'), hatch: values('HATCHING DATE'), pupa: values('PUPA DATE') }, expected, durations, latest, today)
  // Eggs whose hatching is long gone (dried, never hatched) are not to count.
  if (expected.eggs && predicted.hatch === null && today !== null) expected.eggs = null
  return { expected, predicted }
}
/** The latest day each stage grew, from a clutch's events (laid, hatched, pupated), as date serials. */
export function latestGains(events: Pick<ClutchEvent, 'kind' | 'day'>[]): { laid: number | null; hatched: number | null; pupated: number | null } {
  const out = { laid: null as number | null, hatched: null as number | null, pupated: null as number | null }
  for (const e of events) {
    if (e.kind !== 'laid' && e.kind !== 'hatched' && e.kind !== 'pupated') continue
    const m = /^(\d{4})-(\d{2})-(\d{2})$/.exec(e.day)
    if (!m) continue
    const serial = Math.round((Date.UTC(+m[1], +m[2] - 1, +m[3]) - Date.UTC(1899, 11, 30)) / 86_400_000)
    if (out[e.kind] === null || serial > out[e.kind]!) out[e.kind] = serial
  }
  return out
}

/** The eggs of a new clutch as typed: 12, 3+5+7 or =3+5 (groups on other leaves or days) → their terms; null when not that. */
export function eggGroups(text: string): number[] | null {
  const raw = text.trim().replace(/\s+/g, '')
  if (!raw) return []
  if (!/^=?\d{1,4}(\+\d{1,4})*$/.test(raw)) return null
  const terms = raw.replace(/^=/, '').split('+').map(Number)
  return terms.length > 1 && terms.some(n => n <= 0) ? null : terms
}

/** An event in short: "+4 hatched", "−2 preserved (M0E, N9E)". `word` names the kind in the person's language. */
export function eventText(
  e: Pick<ClutchEvent, 'kind' | 'count' | 'ids' | 'note' | 'stage'>,
  word: (e: Pick<ClutchEvent, 'kind' | 'stage' | 'count'>) => string,
): string {
  const gain = EVENT_KINDS[e.stage]?.[0] === e.kind
  const ids = e.ids.length ? ` (${e.ids.join(', ')})` : ''
  const note = e.note ? ` · ${e.note}` : ''
  return `${gain ? '+' : '−'}${e.count} ${word(e)}${ids}${note}`
}

// --- The notebook's list: what the app changed since the notebook was brought up to date

export interface NotebookLine {
  field: string
  before: DayChange['before']
  after: DayChange['after']
  actors: string[]
  sources: string[]
  /** Kept in the app, not in Google Sheets yet (server/staged.mjs). */
  staged?: boolean
  firstAt: string
  at: string
}
export interface NotebookClutch {
  recordId: string
  clutch: string
  species: string
  isNew: boolean
  lines: NotebookLine[]
  events: ClutchEvent[]
}
/**
 * The notebook's list as plain text to copy by hand: one block per clutch in
 * the notebook's order, each field's change (the terms added for a count) with
 * who and when, then the app-only events.
 */
export function notebookText(
  title: string,
  clutches: NotebookClutch[],
  opts: {
    formatDate: (serial: number) => string
    when: (iso: string) => string
    who: (name: string) => string
    event: (e: ClutchEvent) => string
    removedWord?: string
    newWord?: string
  },
): string {
  const lines = [title]
  for (const c of clutches) {
    lines.push('')
    lines.push([c.isNew ? `${c.clutch} (${opts.newWord ?? 'new'})` : c.clutch, c.species].filter(Boolean).join(' · '))
    for (const l of c.lines)
      lines.push(`  ${l.field}: ${changeText(l, opts.formatDate, opts.removedWord)}  [${l.actors.map(opts.who).join(', ')} ${opts.when(l.at)}]`)
    for (const e of c.events) lines.push(`  ${opts.event(e)}  [${opts.who(e.name || e.username || '')} ${opts.when(e.createdAt)}]`)
  }
  return lines.join('\n')
}

// --- A count's chips in the editor: struck out to take them from the sum, today's told apart, linked to their events

/** The sum's terms: the chips not struck out (`struck`: indexes into `base`). */
export const struckTerms = (base: number[], struck: number[]) => base.filter((_, i) => !struck.includes(i))

/**
 * A chip tapped: struck out of the sum (it stays in place, crossed out), or put
 * back if it was. Refused when the sum would start with a loss or go below 0.
 */
export function toggleStrike(
  base: number[],
  struck: number[],
  index: number,
): { ok: true; struck: number[]; terms: number[] } | { ok: false; reason: 'first' | 'negative' } {
  if (index < 0 || index >= base.length) return { ok: true, struck, terms: struckTerms(base, struck) }
  const next = struck.includes(index) ? struck.filter(i => i !== index) : [...struck, index].sort((a, b) => a - b)
  const terms = struckTerms(base, next)
  if (terms.length && terms[0] < 0) return { ok: false, reason: 'first' }
  if (totalOf(terms) < 0) return { ok: false, reason: 'negative' }
  return { ok: true, struck: next, terms }
}

/**
 * The chips once the count changed (a +N added, Undo, another person's edit):
 * the struck ones stay while the sum is still the same, or the same with terms
 * added after it; any other change starts the chips again from the new sum.
 */
export function rebaseChips(base: number[], struck: number[], terms: number[]): { base: number[]; struck: number[] } {
  const now = struckTerms(base, struck)
  if (now.length <= terms.length && now.every((t, i) => t === terms[i])) return { base: [...base, ...terms.slice(now.length)], struck }
  return { base: [...terms], struck: [] }
}

/**
 * How a count changed today: the terms of this morning's sum still at its start
 * (`kept`: the chips before it are from earlier days), the terms added today and
 * those of the morning no longer there.
 */
export function todaySplit(morning: number[], now: number[]): { kept: number; added: number[]; removed: number[] } {
  let kept = 0
  while (kept < morning.length && kept < now.length && morning[kept] === now[kept]) kept++
  return { kept, added: now.slice(kept), removed: morning.slice(kept) }
}

/**
 * The event behind each chip of a stage's count (its id, or null): a + is that
 * stage's gain of the same number (5 hatched for +5), a − a death or
 * disappearance (or preserved ones, when the team takes them off) of that many.
 * Matched from the newest chip and the newest event back, each event once; a
 * recount has none.
 */
export function chipEvents(
  terms: number[],
  events: Pick<ClutchEvent, 'id' | 'kind' | 'count' | 'stage' | 'day' | 'createdAt'>[],
  stage: Stage,
  subtractPreserved: boolean,
): (string | null)[] {
  const signed = (e: (typeof events)[number]) =>
    e.kind === gainOf(stage)
      ? e.count
      : e.kind === 'died' || e.kind === 'disappeared' || (e.kind === 'preserved' && subtractPreserved)
        ? -e.count
        : null
  const pool = events
    .filter(e => e.stage === stage && signed(e) !== null)
    .sort((a, b) => b.day.localeCompare(a.day) || b.createdAt.localeCompare(a.createdAt))
  const used = new Set<string>()
  const out: (string | null)[] = terms.map(() => null)
  for (let i = terms.length - 1; i >= 0; i--) {
    const e = pool.find(x => !used.has(x.id) && signed(x) === terms[i])
    if (!e) continue
    used.add(e.id)
    out[i] = e.id
  }
  return out
}
