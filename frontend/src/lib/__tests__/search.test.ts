import { describe, expect, it } from 'vitest'
import { mergeRows, rangeFor, stepMatch } from '../search'
import type { TableRow } from '../types'

const row = (n: number, values: TableRow['values'] = {}): TableRow => ({
  id: `r${n}`,
  row: n,
  version: 1,
  observed: true,
  values,
  formulas: [],
})

describe('Buscador', () => {
  it('rows read while scrolling join those shown, in sheet order, the newer copy kept', () => {
    const shown = [row(10), row(11, { A: 'old' }), row(12)]
    const merged = mergeRows(shown, [row(8), row(9), row(11, { A: 'new' })])
    expect(merged.map(r => r.row)).toEqual([8, 9, 10, 11, 12])
    expect(merged[3].values.A).toBe('new')
  })

  it('↑ and ↓ go to the previous and next match, round at the ends', () => {
    const matches = [5, 20, 300]
    expect(stepMatch(matches, 20, 1)).toBe(300)
    expect(stepMatch(matches, 300, 1)).toBe(5)
    expect(stepMatch(matches, 5, -1)).toBe(300)
    expect(stepMatch(matches, 20, -1)).toBe(5)
    // From a row that is not a match (clicked elsewhere): the nearest one that way.
    expect(stepMatch(matches, 100, 1)).toBe(300)
    expect(stepMatch(matches, 100, -1)).toBe(20)
    expect(stepMatch([], 7, 1)).toBe(7)
  })

  it('a match is shown with the rows around it: nothing read when loaded, the gap read when close, a new run when far', () => {
    const loaded = { from: 100, to: 140 }
    expect(rangeFor(loaded, 120, 12, 40)).toBeNull()
    expect(rangeFor(loaded, 150, 12, 40)).toEqual({ from: 141, to: 162, replace: false })
    expect(rangeFor(loaded, 95, 12, 40)).toEqual({ from: 83, to: 99, replace: false })
    expect(rangeFor(loaded, 900, 12, 40)).toEqual({ from: 888, to: 912, replace: true })
    expect(rangeFor(null, 5, 12, 40)).toEqual({ from: 0, to: 17, replace: true })
  })
})
