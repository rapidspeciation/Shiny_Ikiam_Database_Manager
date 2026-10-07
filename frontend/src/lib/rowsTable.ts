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
  /** Where it is on the table's notebook photos (0 = the first), as a page's proposal row says it. */
  page?: { photo: number; line?: number }
}

/** What the grid holds for a row: its own columns (__key, __row, __label, __note, __line) and the table's. */
export type GridRow = Record<string, CellValue> & {
  __key: string
  __row: number | null
  __label: string
  __note: string
  __line: string
}

/** Whether the rows are on more than one photo (their "Línea" then says the photo too). */
export const severalPhotos = (rows: TableRow[]) => new Set(rows.flatMap(r => (r.page ? [r.page.photo] : []))).size > 1

/**
 * A table's notebook photos as their thumbnails say them (as a page's proposal does): the
 * lines of its rows on each and how many rows; every photo, those without rows too.
 */
export function tablePhotos(rows: Pick<TableRow, 'page'>[], photos: number) {
  const out = new Map<number, { photo: number; from: number; to: number; rows: number }>()
  for (let n = 0; n < photos; n++) out.set(n, { photo: n, from: 0, to: 0, rows: 0 })
  for (const r of rows) {
    if (!r.page || r.page.photo >= photos) continue
    const s = out.get(r.page.photo)!
    s.rows++
    if (r.page.line) {
      s.from = s.from ? Math.min(s.from, r.page.line) : r.page.line
      s.to = Math.max(s.to, r.page.line)
    }
  }
  return [...out.values()]
}

/** A row's place on the photos, as the "Línea" column shows it: the line, or photo·line when there are several. */
export function lineText(row: TableRow, several: boolean) {
  if (!row.page) return ''
  const line = row.page.line ? String(row.page.line) : ''
  return several ? [row.page.photo + 1, line].filter(Boolean).join('·') : line
}

/** The photo and line of a row selected in the table, for the photo beside it (null: none). */
export const rowPlace = (row: TableRow | undefined): [number | null, number | null] =>
  row?.page ? [row.page.photo, row.page.line ?? null] : [null, null]

export function gridRows(rows: TableRow[], fields: string[]): GridRow[] {
  const several = severalPhotos(rows)
  return rows.map(r => {
    const out = { __key: r.key, __row: r.row, __label: r.label, __note: r.note ?? '', __line: lineText(r, several) } as GridRow
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
