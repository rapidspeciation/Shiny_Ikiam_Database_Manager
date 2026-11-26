import { cellOf, pageNote, rowKey, type ProposalChange } from './proposals'
import { t, type Msg } from './i18n'
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
  /** Not written on the notebook line: from the page, the note or what the team always writes (the table's «deducida»). */
  inferred?: boolean
  /** A column that only carries the template's NA or NOT_COLLECTED: written all the same, folded in the list as in the table. */
  template?: boolean
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
  /**
   * Nothing of the row is written, and its note says why: the sheet would refuse the line, or its pre-made row was
   * used by someone meanwhile.
   */
  refused: boolean
  /** A line of the page with no row to write to (not in the sheet, several rows could be it, no legible ID): to look at. */
  look: boolean
  /** Marked by the assistant for a look (the table's yellow row). */
  marked: boolean
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
    return { ...base, value, state: cell.kind === 'person' ? 'person' : 'ai', formula, inferred: cell.inferred, needsTable: false }
  }
  if (cell.warning) return { ...base, state: 'warned', why: cell.warning.text, whyMsg: cell.warning.msg, needsTable: false }
  return null
}

/** A row's note as the table says it: a new row whose pre-made row someone used meanwhile says first why it is not written. */
export const stackNote = (c: ProposalChange) =>
  c.rowTaken
    ? [t('Su fila sin usar ({row}) ya se usó en la hoja: esta fila no se escribe', { row: c.rowTaken.row }), pageNote(c)].filter(Boolean).join(' · ')
    : pageNote(c)

/**
 * The rows to review, in the table's order, each with the cells the proposal changes or marks among `fields` (every
 * column of the table in its order, the folded ones too: nothing «Aplicar» writes is left out; those in `template`
 * are flagged, to fold them). Lines shown only to follow the page (never written) are left to the table; one the
 * sheet would refuse, one whose row was taken and one with no row to write to stay, each with its note and no cells.
 */
export function stackRows(rows: ProposalChange[], fields: string[], newRowFormulas: string[] = [], template: string[] = []): StackRow[] {
  const folded = new Set(template)
  return rows.flatMap((c): StackRow[] => {
    const refused = !!c.page?.error || !!c.rowTaken
    const look = !refused && !!c.placeholder && c.page?.status !== 'crossed'
    if ((c.context || c.placeholder || c.gap) && !refused && !look) return []
    const list =
      refused || look
        ? []
        : fields.flatMap((f): StackField[] => {
            const cell = fieldOf(c, f, newRowFormulas)
            // Only a plain write folds: a cell that waits for a decision or carries a warning always shows.
            return cell ? [folded.has(f) && cell.state === 'ai' ? { ...cell, template: true } : cell] : []
          })
    if (!list.length && !refused && !look) return []
    return [
      {
        key: rowKey(c),
        label: c.label,
        row: c.row,
        create: !!c.create,
        note: stackNote(c),
        refused,
        look,
        marked: !!c.highlight,
        written: !!c.applied,
        dead: !!c.sheetDeath,
        fields: list,
      },
    ]
  })
}

/** The one note every row of the list shares, to say it once above them; '' when they differ or there is one row. */
export function sharedNote(rows: StackRow[]): string {
  if (rows.length < 2) return ''
  const notes = new Set(rows.map(r => r.note))
  const [note] = notes
  return notes.size === 1 && note ? note : ''
}

/** How many cells of the list wait for a decision in the table. */
export const stackToDecide = (rows: StackRow[]) => rows.reduce((n, r) => n + r.fields.filter(f => f.needsTable).length, 0)

/**
 * How many template cells every row of the list folds, when it is the same on each (a page of new rows): said once
 * above the rows. 0 when the rows differ or fold nothing (then each row says its own).
 */
export function sharedFold(rows: StackRow[]): number {
  const counts = new Set(rows.filter(r => r.fields.length).map(r => r.fields.filter(f => f.template).length))
  const [count] = counts
  return rows.length > 1 && counts.size === 1 && count ? count : 0
}
