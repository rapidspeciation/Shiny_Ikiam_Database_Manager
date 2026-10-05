import { describe, expect, it } from 'vitest'
import { compareCells, gridRows, isMarked, tableCounts, type TableRow } from '../rowsTable'
import type { CellValue } from '../types'

const row = (key: string, values: Record<string, CellValue>, extra: Partial<TableRow> = {}): TableRow => ({
  key,
  recordId: key,
  row: 2,
  label: key,
  values,
  ...extra,
})

describe('a table of rows the assistant shows (show_rows)', () => {
  it("gives the grid each row's own columns and the table's, empty where the row has nothing", () => {
    const rows = [
      row('a', { SPECIES: 'Mechanitis polymnia', Notes: 'x' }, { note: 'the first' }),
      row('b', {}, { row: null, missing: true }),
    ]
    expect(gridRows(rows, ['SPECIES', 'Dissection_date'])).toEqual([
      {
        __key: 'a',
        __row: 2,
        __label: 'a',
        __note: 'the first',
        SPECIES: 'Mechanitis polymnia',
        Dissection_date: null,
      },
      { __key: 'b', __row: null, __label: 'b', __note: '', SPECIES: null, Dissection_date: null },
    ])
    expect(tableCounts(rows)).toEqual({ rows: 2, missing: 1 })
  })

  it('marks a whole row, or only the cells the assistant names', () => {
    expect(isMarked(row('a', {}, { highlight: true }), 'SPECIES')).toBe(true)
    expect(isMarked(row('a', {}, { marked: ['Notes'] }), 'Notes')).toBe(true)
    expect(isMarked(row('a', {}, { marked: ['Notes'] }), 'SPECIES')).toBe(false)
    expect(isMarked(undefined, 'SPECIES')).toBe(false)
  })

  it('sorts numbers (and dates) by value before text, text as read, and empty cells last either way', () => {
    const values: CellValue[] = ['W10B', null, 46000, 'w2b', '', 45000, 'Mechanitis']
    const asc = [...values].sort((a, b) => compareCells(a, b, 'asc'))
    expect(asc).toEqual([45000, 46000, 'Mechanitis', 'w2b', 'W10B', null, ''])
    // Tabulator turns the order round for a descending sort; the empty cells stay last.
    const desc = [...values].sort((a, b) => -compareCells(a, b, 'desc'))
    expect(desc.slice(0, 5)).toEqual(['W10B', 'w2b', 'Mechanitis', 46000, 45000])
    expect(desc.slice(5).every(v => v === null || v === '')).toBe(true)
  })
})
