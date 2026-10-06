import type { CellValue } from './types'
import { cellId, rowKey, type ProposalChange } from './proposals'

/**
 * Undo and redo of the person's own edits in a proposal's table (Ctrl+Z,
 * Ctrl+Shift+Z / Ctrl+Y). A step holds how its cells were before: the
 * person's value or none, set back to the sheet's, checked or not, and for a
 * cell edited in the sheet, whose value is kept (the sheet's or the
 * proposal's). Undoing
 * sends what puts them back as edits of the table (the same POST …/edit), and
 * keeps how they were just before, to redo. A cell the assistant changed since
 * (its value, or its doubt) is left as it is now: the person is told.
 * Kept per table while the page is open (historyFor), up to LIMIT steps.
 */
export interface CellSnap {
  key: string
  field: string
  /** The proposal holds a value for the cell (`value`); absent, the cell shows the sheet's value (or a new row's empty one). */
  has: boolean
  value: CellValue
  /** The person's mark on the cell (typed over, or set back to the sheet's value). */
  mine: boolean
  /** The assistant doubts the cell's reading, and whether it is marked as checked. */
  doubt: boolean
  checked: boolean
  /** A cell edited in the sheet since the proposal read it: the value kept ('proposal': the proposal's over it). */
  sheetUse?: 'sheet' | 'proposal'
  /** What the assistant has there (its value and doubt): another value later means the assistant revised the cell. */
  ai: string
}
export type Step = CellSnap[]

/** An edit that puts a cell back as a snapshot had it, as the table sends its own edits. */
export interface RestoreEdit {
  key: string
  field: string
  value: CellValue
  before: CellValue
  use?: 'sheet'
}
export interface RestoreCheck {
  key: string
  field: string
  checked: boolean
}
export interface RestoreChoice {
  key: string
  field: string
  use: 'sheet' | 'proposal'
}
export interface RestoreOps {
  edit?: RestoreEdit
  check?: RestoreCheck
  sheet?: RestoreChoice
}

const NONE = '\u0000none'
/** The assistant's side of a cell: what it proposed (the person's edits keep it aside) and whether it doubts it. */
function aiSide(change: ProposalChange, field: string) {
  const mark = change.personEdits?.[field]
  const value = mark ? ('ai' in mark ? mark.ai : NONE) : field in change.values ? change.values[field] : NONE
  const doubt = change.doubts?.[field]
  return JSON.stringify([value ?? null, !!doubt, doubt?.alternatives ?? null, !!change.unreadable?.[field]])
}

/** How a cell is now. */
export function snapCell(change: ProposalChange, field: string): CellSnap {
  const has = field in change.values
  const edited = change.create ? undefined : change.sheetChanged?.[field]
  return {
    key: rowKey(change),
    field,
    has,
    value: has ? (change.values[field] ?? null) : null,
    mine: !!change.personEdits?.[field],
    doubt: !!change.doubts?.[field],
    checked: !!change.doubts?.[field]?.checked,
    ...(edited ? { sheetUse: edited.use === 'proposal' && !edited.again ? 'proposal' : 'sheet' } : {}),
    ai: aiSide(change, field),
  }
}

const same = (a: CellValue | undefined, b: CellValue | undefined) => JSON.stringify(a ?? null) === JSON.stringify(b ?? null)

/**
 * What puts the cell `now` back as `to` had it: an edit (its value, or back to the
 * sheet's), the checked mark of a doubtful cell, and the value kept of a cell
 * edited in the sheet (a value typed there had chosen the proposal's); nothing
 * for a cell already so.
 * The server marks the cell the person's or not as it always does (a value the
 * assistant proposed is the assistant's again).
 */
export function restoreOps(to: CellSnap, now: CellSnap): RestoreOps {
  const out: RestoreOps = {}
  const before = now.has ? now.value : null
  if (to.has !== now.has || to.mine !== now.mine || (to.has && !same(to.value, now.value)))
    out.edit = to.has
      ? { key: to.key, field: to.field, value: to.value, before }
      : { key: to.key, field: to.field, value: null, before, use: 'sheet' }
  if (to.doubt && to.checked !== now.checked) out.check = { key: to.key, field: to.field, checked: to.checked }
  // (Sent after the edit, which the server takes first: a value typed chooses the proposal's.)
  if (to.sheetUse && (to.sheetUse !== now.sheetUse || out.edit)) out.sheet = { key: to.key, field: to.field, use: to.sheetUse }
  return out
}

/**
 * Puts a step's cells back: the edits, checks and choices to send, the step that undoes
 * this (the cells as they are now, to redo), and how many cells were left because
 * the assistant changed them since (`revised`) or their row is no longer in the
 * proposal (`gone`).
 */
export function replay(step: Step, find: (key: string) => ProposalChange | undefined) {
  const edits: RestoreEdit[] = []
  const checks: RestoreCheck[] = []
  const sheets: RestoreChoice[] = []
  const inverse: Step = []
  let revised = 0
  let gone = 0
  for (const to of step) {
    const change = find(to.key)
    if (!change) {
      gone++
      continue
    }
    const now = snapCell(change, to.field)
    if (now.ai !== to.ai) {
      revised++
      continue
    }
    const ops = restoreOps(to, now)
    if (ops.edit) edits.push(ops.edit)
    if (ops.check) checks.push(ops.check)
    if (ops.sheet) sheets.push(ops.sheet)
    inverse.push(now)
  }
  return { edits, checks, sheets, inverse, revised, gone }
}

export const LIMIT = 100

/** The steps of one table: those to undo (the last done at the end) and those to redo. */
export class UndoHistory {
  done: Step[] = []
  undone: Step[] = []
  constructor(readonly limit = LIMIT) {}
  /** A new edit: its cells as they were before it; what was undone can no longer be redone. */
  record(step: Step) {
    if (!step.length) return
    this.done.push(step)
    if (this.done.length > this.limit) this.done.splice(0, this.done.length - this.limit)
    this.undone = []
  }
  /** Undoes the last step (null: nothing to undo). */
  undo(find: (key: string) => ProposalChange | undefined) {
    return this.move(this.done, this.undone, find)
  }
  /** Redoes the last step undone (null: nothing to redo). */
  redo(find: (key: string) => ProposalChange | undefined) {
    return this.move(this.undone, this.done, find)
  }
  private move(from: Step[], to: Step[], find: (key: string) => ProposalChange | undefined) {
    const step = from.pop()
    if (!step) return null
    const out = replay(step, find)
    if (out.inverse.length) {
      to.push(out.inverse)
      if (to.length > this.limit) to.splice(0, to.length - this.limit)
    }
    return out
  }
}

/** The steps of each proposal table, kept while the page is open (a table shown again finds its own). */
const histories = new Map<string, UndoHistory>()
export function historyFor(key: string): UndoHistory {
  let h = histories.get(key)
  if (!h) histories.set(key, (h = new UndoHistory()))
  return h
}

/** A step being gathered (a paste, a fill, a cut moved): each cell once, as it was before the first of its edits. */
export class StepBuilder {
  private cells = new Map<string, CellSnap>()
  add(change: ProposalChange, field: string) {
    const id = cellId(rowKey(change), field)
    if (!this.cells.has(id)) this.cells.set(id, snapCell(change, field))
  }
  take(): Step {
    const out = [...this.cells.values()]
    this.cells.clear()
    return out
  }
}
