import { cellOf, pageNote, rowKey, type ProposalChange } from './proposals'
import type { Msg } from './i18n'
import type { CellValue } from './types'

/**
 * A proposal's rows for a phone: each row with its changed cells one under the other, the sheet's value over the
 * new one (ProposalStack.vue), so every change is on the screen without scrolling sideways. The table stays one
 * tap away for editing.
 *
 * How a cell is shown:
 *  - `ai`, `person`: written on «Aplicar» (the assistant's value, or the one the person typed);
 *  - `doubtful`: the assistant's, not sure of it and nobody reviewed it yet;
 *  - `unreadable`: nobody could read it and nobody filled it: not written;
 *  - `kept`: edited in the sheet after the proposal: the sheet's value stays (the proposal's is `aside`);
 *  - `reverted`: the person set it back to the sheet's value (the assistant's is `aside`): not written;
 *  - `warned`: a preserved butterfly would be left without it (its CAM or tube).
 */
export type StackState = 'ai' | 'person' | 'doubtful' | 'unreadable' | 'kept' | 'reverted' | 'warned'
export interface StackField {
  field: string
  state: StackState
  /** What the sheet has now (undefined in a row not written yet). */
  was: CellValue | undefined
  /** What the cell holds after «Aplicar». */
  value: CellValue
  /** A value set aside, not written (see `kept` and `reverted`). */
  aside?: CellValue
  /** Why it needs a look (the assistant's doubt, why it is unreadable, what is missing). */
  why?: string
  whyMsg?: Msg
  /** `value` is what the formula the proposal writes will give. */
  formula?: boolean
  /** Decided in the table: a doubtful reading to confirm, an unreadable cell to fill, a sheet's edit to choose. */
  needsTable: boolean
}
export interface StackRow {
  key: string
  label: string
  /** The sheet's row (null for a row not written yet). */
  row: number | null
  create: boolean
  note: string
  /** The page's line the sheet would refuse: its note says why, nothing of it is written. */
  refused: boolean
  /** Written already by an earlier «Aplicar» of its sheet. */
  written: boolean
  /** Already dead in the sheet (its Death_date), as the table marks its ID. */
  dead: boolean
  fields: StackField[]
}

const same = (a: CellValue | undefined, b: CellValue | undefined) => JSON.stringify(a ?? null) === JSON.stringify(b ?? null)
const blank = (v: CellValue | undefined) => v === null || v === undefined || v === ''

/** A row's cell for the list, or null when the proposal leaves it as the sheet has it. */
function fieldOf(change: ProposalChange, field: string, newRowFormulas: string[]): StackField | null {
  const cell = cellOf(change, field, newRowFormulas)
  const base = { field, was: change.create ? undefined : cell.was, value: cell.value }
  if (cell.kind === 'kept')
    return { ...base, state: 'kept', aside: cell.proposalValue, needsTable: !cell.sheetEdit?.use || !!cell.sheetEdit.again }
  if (cell.kind === 'unreadable')
    return {
      ...base,
      state: 'unreadable',
      why: cell.unreadable?.reason ?? undefined,
      whyMsg: cell.unreadable?.reasonMsg,
      needsTable: true,
    }
  if (cell.kind === 'reverted') return { ...base, state: 'reverted', aside: cell.ai, needsTable: false }
  if (cell.kind === 'proposed' || (cell.kind === 'person' && field in change.values)) {
    const formula = !!cell.formulaWrite && cell.computed !== undefined
    const value = formula ? (cell.computed ?? null) : cell.value
    // A new row's empty cell, or a value the sheet already has: nothing to review.
    const changes = change.create ? !blank(value) : !same(cell.value, cell.was) || !!cell.formulaWrite
    if (!changes && !cell.doubtful && !cell.warning) return null
    if (cell.doubtful)
      return { ...base, value, state: 'doubtful', why: cell.doubt?.reason ?? undefined, whyMsg: cell.doubt?.reasonMsg, formula, needsTable: true }
    if (cell.warning && blank(value)) return { ...base, value, state: 'warned', why: cell.warning.text, whyMsg: cell.warning.msg, needsTable: false }
    return { ...base, value, state: cell.kind === 'person' ? 'person' : 'ai', formula, needsTable: false }
  }
  if (cell.warning) return { ...base, state: 'warned', why: cell.warning.text, whyMsg: cell.warning.msg, needsTable: false }
  return null
}

/**
 * The rows to review, in the table's order, each with the cells the proposal changes or marks among `fields` (the
 * table's columns, in its order). Lines shown only to follow the page (never written) are left to the table; one the
 * sheet would refuse stays, with its note.
 */
export function stackRows(rows: ProposalChange[], fields: string[], newRowFormulas: string[] = []): StackRow[] {
  return rows.flatMap((c): StackRow[] => {
    const refused = !!c.page?.error
    if ((c.context || c.placeholder || c.gap) && !refused) return []
    const list = refused ? [] : fields.flatMap(f => fieldOf(c, f, newRowFormulas) ?? [])
    if (!list.length && !refused) return []
    return [
      {
        key: rowKey(c),
        label: c.label,
        row: c.row,
        create: !!c.create,
        note: pageNote(c),
        refused,
        written: !!c.applied,
        dead: !!c.sheetDeath,
        fields: list,
      },
    ]
  })
}

/** How many cells of the list wait for a decision in the table. */
export const stackToDecide = (rows: StackRow[]) => rows.reduce((n, r) => n + r.fields.filter(f => f.needsTable).length, 0)
