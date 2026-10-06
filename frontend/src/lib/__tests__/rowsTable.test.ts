import { describe, expect, it } from 'vitest'
import { compareCells, gridRows, isMarked, lineText, rowPlace, severalPhotos, tableCounts, type TableRow } from '../rowsTable'
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
        __line: '',
        SPECIES: 'Mechanitis polymnia',
        Dissection_date: null,
      },
      { __key: 'b', __row: null, __label: 'b', __note: '', __line: '', SPECIES: null, Dissection_date: null },
    ])
    expect(tableCounts(rows)).toEqual({ rows: 2, missing: 1 })
  })

  it('says where each row is on the notebook photos, and gives the photo and line of the row selected', () => {
    const one = [row('a', {}, { page: { photo: 0, line: 3 } }), row('b', {})]
    expect(severalPhotos(one)).toBe(false)
    expect(gridRows(one, []).map(r => r.__line)).toEqual(['3', ''])
    // On several photos: the photo (from 1) and the line; a row on a photo without its line, the photo.
    const two = [row('a', {}, { page: { photo: 0, line: 3 } }), row('b', {}, { page: { photo: 1, line: 12 } }), row('c', {}, { page: { photo: 1 } })]
    expect(severalPhotos(two)).toBe(true)
    expect(two.map(r => lineText(r, true))).toEqual(['1·3', '2·12', '2'])
    expect(rowPlace(two[1])).toEqual([1, 12])
    expect(rowPlace(two[2])).toEqual([1, null])
    expect(rowPlace(row('d', {}))).toEqual([null, null])
    expect(rowPlace(undefined)).toEqual([null, null])
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
