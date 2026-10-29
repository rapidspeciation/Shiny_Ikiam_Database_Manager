import { describe, expect, it } from 'vitest'
import { cellCounts, notReady, rackCells, rackOfChange, splitTube, type DepositRack, type RackDeposit } from '../rackDeposit'
import type { ProposalChange } from '../proposals'

const rack = (over: Partial<DepositRack> = {}): DepositRack => ({
  reading: 'r1',
  rack_id: 'FK00274230',
  code_beside: 'A12',
  code_options: ['A12', 'H1'],
  assumed: true,
  wells: {
    A1: { t: 'FS50849233', s: 'new', n: 1 },
    B1: { t: 'FS50849234', s: 'same', r: 'rec-b1' },
    C1: { t: 'FS50849235', s: 'moved', r: 'rec-c1', was: { rack: 'FK00999999', well: 'E5' } },
    D1: { s: 'empty' },
    E1: { t: 'FS50849237', s: 'check', was: [{ rack: 'FK1', well: 'A1' }, { rack: 'FK2', well: 'B2' }] },
  },
  missing: ['F1'],
  problems: [],
  need_rows: ['FS50849233'],
  elsewhere: [],
  duplicates: ['FS50849237'],
  unset: ['caja'],
  fill: { E1: 'FS50849237' },
  ...over,
})
const change = (over: Partial<ProposalChange>): ProposalChange => ({ index: 0, key: 'k', recordId: null, sheet: 'T', row: null, label: '', values: {}, ...over })
const created = change({ key: 'n1', create: true, values: { Tube_ID: 'FS50849233', Rack_ID: 'FK00274230', Tube_location: 'A1' } })
const moved = change({ key: 'rec-c1', recordId: 'rec-c1', label: 'FS50849235', values: { Rack_ID: 'FK00274230', Tube_location: 'C1' } })

describe('a rack of a deposit', () => {
  it('each well as the proposal has it now: new, moved, same, empty, to check, missing', () => {
    const cells = rackCells(rack(), [created, moved])
    expect(cells.size).toBe(96)
    expect(cells.get('A1')).toMatchObject({ tube: 'FS50849233', state: 'new', needsRows: true })
    expect(cells.get('B1')?.state).toBe('same')
    expect(cells.get('C1')).toMatchObject({ state: 'moved', was: [{ rack: 'FK00999999', well: 'E5' }] })
    expect(cells.get('D1')?.state).toBe('empty')
    expect(cells.get('E1')).toMatchObject({ state: 'check', byHand: true })
    expect(cells.get('E1')?.was).toHaveLength(2)
    expect(cells.get('F1')?.state).toBe('missing')
    expect(cells.get('H12')?.state).toBe('missing')
    const n = cellCounts(cells)
    expect([n.new, n.moved, n.same, n.empty, n.check]).toEqual([1, 1, 1, 1, 1])
  })

  it('a row taken out of the table: its well says so (not «nuevo» any more)', () => {
    const cells = rackCells(rack(), [moved])
    expect(cells.get('A1')?.state).toBe('left')
    expect(rackCells(rack(), [created]).get('C1')?.state).toBe('left')
  })

  it('a row of the table belongs to the rack holding its tube', () => {
    const d: RackDeposit = { responsable: 'Franz', ultracongelador: 'LBI062', racks: [rack(), rack({ reading: 'r2', rack_id: 'FK2', wells: { A1: { t: 'FS1', s: 'new' } } })], sheet: 'T', unset: [] }
    expect(rackOfChange(d, created)).toBe('r1')
    expect(rackOfChange(d, moved)).toBe('r1')
    expect(rackOfChange(d, change({ create: true, values: { Tube_ID: 'FS1' } }))).toBe('r2')
    expect(rackOfChange(d, change({ create: true, values: { Tube_ID: 'FS9' } }))).toBeNull()
  })

  it('the .xlsx waits for the depositor, the rack code and every well', () => {
    const d: RackDeposit = { responsable: '', ultracongelador: 'LBI062', racks: [rack({ rack_id: null })], sheet: 'T', unset: [] }
    const why = notReady(d)
    expect(why.some(w => /responsable|depositor/.test(w))).toBe(true)
    expect(why.some(w => /código del rack|rack code/.test(w))).toBe(true)
    expect(why.some(w => /F1/.test(w))).toBe(true)
    expect(notReady({ ...d, responsable: 'Franz', racks: [rack({ missing: [] })] })).toEqual([])
  })

  it('a tube ID: its series apart from the four digits compared', () => {
    expect(splitTube('FS50849233')).toEqual(['FS5084', '9233'])
    expect(splitTube(null)).toEqual(['', ''])
  })
})
