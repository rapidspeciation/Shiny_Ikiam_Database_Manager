import type { CellValue } from './types'

/**
 * A table of sheet rows the assistant shows beside the chat (show_rows): listed
 * with the proposals, but only to read. Each row comes with the sheet's current
 * values of the table's columns and the assistant's notes. Pure helpers, kept
 * apart from the grid so they can be tested.
 */
export interface TableRow {
  /** The row's app ID. */
  key: string
  recordId: string
  /** null: the row is no longer in the sheet. */
  row: number | null
  label: string
  values: Record<string, CellValue>
  missing?: boolean
  /** The assistant's note on the row. */
  note?: string
  /** Its notes on cells, by column. */
  cells?: Record<string, string>
  /** The whole row marked, or some of its cells. */
  highlight?: boolean
  marked?: string[]
}

/** What the grid holds for a row: its own columns (__key, __row, __label, __note) and the table's. */
export type GridRow = Record<string, CellValue> & {
  __key: string
  __row: number | null
  __label: string
  __note: string
}

export function gridRows(rows: TableRow[], fields: string[]): GridRow[] {
  return rows.map(r => {
    const out = { __key: r.key, __row: r.row, __label: r.label, __note: r.note ?? '' } as GridRow
    for (const f of fields) out[f] = r.values[f] ?? null
    return out
  })
}

/** Whether a cell is marked: its row, or the cell itself. */
export const isMarked = (row: TableRow | undefined, field: string) => !!row && (!!row.highlight || !!row.marked?.includes(field))

const empty = (v: CellValue | undefined) => v === null || v === undefined || v === ''

/**
 * The order of two cells when the person sorts a column: numbers (and dates,
 * kept as serials) by value and before text, text as people read it (numbers
 * inside it in order: W2B before W10B), empty cells last whichever way.
 */
export function compareCells(a: CellValue | undefined, b: CellValue | undefined, dir: 'asc' | 'desc' = 'asc'): number {
  if (empty(a) || empty(b)) return empty(a) === empty(b) ? 0 : (empty(a) ? 1 : -1) * (dir === 'desc' ? -1 : 1)
  if (typeof a === 'number' && typeof b === 'number') return a - b
  if (typeof a === 'number') return -1
  if (typeof b === 'number') return 1
  return String(a).localeCompare(String(b), undefined, { numeric: true, sensitivity: 'base' })
}

/** A table's line in the panel: how many rows, and how many of them are no longer in the sheet. */
export function tableCounts(rows: Pick<TableRow, 'missing'>[]) {
  return { rows: rows.length, missing: rows.filter(r => r.missing).length }
}
