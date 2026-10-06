import { DEATH_COLUMNS, placeholder } from './deaths'
import { cellOf, readOnlyRow, rowKey, type ProposalChange } from './proposals'
import type { CellValue } from './types'

/**
 * Alt+↑ / Alt+↓ in a proposal's table (Cambios propuestos): the selected cells
 * go one row up or down in their columns, for a value the assistant read on
 * the line above or below. What moves is the proposal's side of the cells
 * (the assistant's values and the person's): the neighbouring row's go to the
 * other end of the selection, so nothing is lost, and a cell with nothing
 * proposed takes the sheet's value of its own row. The rows go as the table
 * shows them (sheet or notebook order), past the slim rows between them; a
 * row that cannot be edited stops the move (a page line as the sheet has it
 * can: the cells moved there make it a row of the proposal).
 * A death (Death_date or Death_cause in Insectary_data) moves whole: its date
 * and cause, the columns its template fills (lib/deaths DEATH_COLUMNS), which
 * only go where the row's sheet cell is empty, NA or NOT_COLLECTED (as the
 * template fills them), and the note proposed with it (what it adds after the
 * sheet's note).
 */

export const DEATH_NOTE = 'Notes_Insectary_data'
const DEATH_CORE = ['Death_date', 'Death_cause']

/** An edit as the table sends its own (`use: 'sheet'`: back to the sheet's value). */
export interface MoveEdit {
  key: string
  field: string
  value: CellValue
  before: CellValue
  use?: 'sheet'
}
export interface MovePlan {
  ok: true
  edits: MoveEdit[]
  /** The rows the cells came from and those they went to, as the table shows them (their IDs). */
  from: string[]
  to: string[]
  /** The rows the cells are in now (to select them). */
  target: string[]
  death: boolean
  /** The death's note moved too. */
  note: boolean
  /** Cells of a death's template not written: the row's sheet has a value there. */
  kept: number
}
export interface MoveRefusal {
  ok: false
  /** nothing: no cell of the table chosen; edge: no row beyond; readonly / locked: a row or cell that cannot change; empty: nothing proposed there. */
  why: 'nothing' | 'edge' | 'readonly' | 'locked' | 'empty'
  label?: string
  field?: string
}

/** A cell's proposal side: its value, or null when the proposal holds none there (the sheet's shows). */
type Side = { v: CellValue } | null
const sideOf = (c: ProposalChange, field: string): Side => (field in c.values ? { v: c.values[field] ?? null } : null)
const sheetOf = (c: ProposalChange, field: string): CellValue =>
  c.create ? null : (c.current?.[field] ?? c.rowValues?.[field] ?? null)
const same = (a: CellValue | undefined, b: CellValue | undefined) => JSON.stringify(a ?? null) === JSON.stringify(b ?? null)
const noteText = (v: CellValue | undefined) => String(v ?? '').trim()

/**
 * The part of a row's note that is its death's: what the proposal adds after
 * the sheet's note, when the proposal writes the death in the same row; null
 * otherwise (a note of something else stays where it is).
 */
export function deathNote(c: ProposalChange): string | null {
  if (!(DEATH_NOTE in c.values) || !DEATH_CORE.some(f => f in c.values)) return null
  const note = noteText(c.values[DEATH_NOTE])
  const was = noteText(sheetOf(c, DEATH_NOTE))
  if (!note || note === was) return null
  if (!was) return note
  if (!note.startsWith(was)) return null
  return note.slice(was.length).replace(/^\s*\|\s*/, '').trim() || null
}

/**
 * What a move does. `rows`: the table's rows in the order shown, null for a
 * slim row; `top`–`bottom`: the selection's first and last (their indexes in
 * `rows`); `fields`: its columns of the sheet.
 */
export function planMove({
  rows,
  top,
  bottom,
  fields,
  dir,
  sheet,
  newRowFormulas = [],
}: {
  rows: (ProposalChange | null)[]
  top: number
  bottom: number
  fields: string[]
  dir: 'up' | 'down'
  sheet: string
  newRowFormulas?: string[]
}): MovePlan | MoveRefusal {
  const block = rows.slice(top, bottom + 1).filter((c): c is ProposalChange => !!c)
  if (!block.length || !fields.length) return { ok: false, why: 'nothing' }
  // The next row of the table beyond the selection, past the slim rows.
  let at = dir === 'up' ? top - 1 : bottom + 1
  while (at >= 0 && at < rows.length && !rows[at]) at += dir === 'up' ? -1 : 1
  const next = rows[at]
  if (!next) return { ok: false, why: 'edge' }
  // The rows in the order shown; each takes the cells of the row after it (up) or before it (down).
  const ring = dir === 'up' ? [next, ...block] : [...block, next]
  const giver = (k: number) => ring[(k + (dir === 'up' ? 1 : ring.length - 1)) % ring.length]
  const stuck = ring.find(readOnlyRow)
  if (stuck) return { ok: false, why: 'readonly', label: stuck.label }
  const locked = (c: ProposalChange, field: string) => cellOf(c, field, newRowFormulas).kind === 'locked'
  for (const field of fields)
    for (const c of ring) if (locked(c, field)) return { ok: false, why: 'locked', label: c.label, field }

  const death = sheet === 'Insectary_data' && fields.some(f => DEATH_CORE.includes(f))
  const core = [...new Set([...fields, ...(death ? DEATH_CORE : [])])].filter(f => !ring.some(c => locked(c, f)))
  const extra = death ? DEATH_COLUMNS.filter(f => !core.includes(f) && !ring.some(c => locked(c, f))) : []
  const edits: MoveEdit[] = []
  /** The cell as `side` says: the value, or back to the sheet's (nothing proposed). */
  const put = (c: ProposalChange, field: string, side: Side) => {
    const now = sideOf(c, field)
    if (!side) {
      if (now) edits.push({ key: rowKey(c), field, value: null, before: now.v, use: 'sheet' })
    } else if (now ? !same(now.v, side.v) : !same(side.v, sheetOf(c, field)))
      edits.push({ key: rowKey(c), field, value: side.v, before: now ? now.v : null })
  }
  for (const field of core) {
    const sides = ring.map(c => sideOf(c, field))
    ring.forEach((c, k) => put(c, field, sides[ring.indexOf(giver(k))]))
  }
  // The template's cells go only where the row's sheet has none (empty, NA, NOT_COLLECTED).
  let kept = 0
  for (const field of extra) {
    const sides = ring.map(c => sideOf(c, field))
    ring.forEach((c, k) => {
      let side = sides[ring.indexOf(giver(k))]
      const own = sheetOf(c, field)
      if (side && !placeholder(own) && !same(side.v, own)) {
        side = null
        kept++
      }
      put(c, field, side)
    })
  }
  // The death's note: what it adds after each row's own note goes with it.
  let note = false
  if (death && !core.includes(DEATH_NOTE) && !ring.some(c => locked(c, DEATH_NOTE))) {
    const parts = ring.map(deathNote)
    if (parts.some(Boolean)) {
      note = true
      ring.forEach((c, k) => {
        const own = noteText(sheetOf(c, DEATH_NOTE))
        const base = parts[k] !== null ? own : noteText(sideOf(c, DEATH_NOTE)?.v ?? own)
        const part = parts[ring.indexOf(giver(k))]
        const text = part ? (base ? `${base} | ${part}` : part) : base
        put(c, DEATH_NOTE, text === own ? null : { v: text })
      })
    }
  }
  if (!edits.length) return { ok: false, why: 'empty' }
  const target = dir === 'up' ? ring.slice(0, block.length) : ring.slice(1)
  return {
    ok: true,
    edits,
    from: block.map(c => c.label),
    to: target.map(c => c.label),
    target: target.map(rowKey),
    death,
    note,
    kept,
  }
}

/** The rows a move names: one ID, or the first and last («L3C–L5C»). */
export const rowsText = (labels: string[]) => (labels.length > 1 ? `${labels[0]}–${labels[labels.length - 1]}` : (labels[0] ?? ''))
