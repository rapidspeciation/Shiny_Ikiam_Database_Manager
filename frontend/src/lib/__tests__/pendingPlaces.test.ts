import { describe, expect, it } from 'vitest'
import { pendingPlaces } from '../pendingPlaces'

describe('pendingPlaces', () => {
  const edit = (label: string, row: number, ...fields: string[]) => ({
    module: 'Insectary_data',
    label,
    row,
    values: Object.fromEntries(fields.map(f => [f, 'x'])),
  })
  it('names the edited row and its columns', () => {
    expect(pendingPlaces([edit('R6E', 13698, 'Insectary_ID')], [])).toEqual({
      shown: [{ module: 'Insectary_data', label: 'R6E', row: 13698, fields: ['Insectary_ID'] }],
      more: 0,
    })
  })
  it('lists edits before new rows, and counts the rest', () => {
    const out = pendingPlaces([edit('A1', 2, 'Sex'), edit('A2', 3, 'Sex', 'Notes')], [{ module: 'Collection_data', label: 'CAM1' }], 2)
    expect(out.shown.map(p => p.label)).toEqual(['A1', 'A2'])
    expect(out.more).toBe(1)
    expect(pendingPlaces([], [{ module: 'Collection_data', label: 'CAM1' }]).shown[0]).toEqual({
      module: 'Collection_data',
      label: 'CAM1',
      row: null,
      fields: [],
    })
  })
})
