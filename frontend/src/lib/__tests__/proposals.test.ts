import { describe, expect, it } from 'vitest'
import {
  ID_COLUMN,
  cellId,
  cellOf,
  changedCells,
  changedText,
  chosenIndexes,
  panelShare,
  sheetGroups,
  withLocal,
  type Proposal,
  type ProposalChange,
} from '../proposals'

const created = (key: string, values: ProposalChange['values'], extra: Partial<ProposalChange> = {}): ProposalChange => ({
  index: 0,
  key,
  clientId: key,
  recordId: null,
  sheet: 'Collection_data',
  row: null,
  label: '',
  create: true,
  before: {},
  values,
  current: {},
  ...extra,
})
const edited = (key: string, values: ProposalChange['values'], extra: Partial<ProposalChange> = {}): ProposalChange => ({
  index: 0,
  key,
  recordId: key,
  sheet: 'Collection_data',
  row: 12,
  label: 'CAM000001',
  before: { Sex: 'male' },
  values,
  current: { Sex: 'male' },
  rowValues: { Sex: 'male', SPECIES: 'Oleria gunilla', Tribe: 'Ithomiini' },
  formulas: ['Tribe'],
  ...extra,
})
const proposal = (changes: ProposalChange[], extra: Partial<Proposal> = {}): Proposal => ({
  id: 'p1',
  reason: 'Recorrido',
  status: 'pending',
  fields: [...new Set(changes.flatMap(c => Object.keys(c.values)))],
  types: {},
  applied: null,
  changes: changes.map((c, index) => ({ ...c, index })),
  ...extra,
})

describe('a cell of the proposal table', () => {
  it("tells the assistant's value, the person's, the sheet's and formulas apart", () => {
    const row = edited('r1', { Sex: 'female', SPECIES: 'Hypothyris anastasia' }, { personEdits: { SPECIES: { ai: 'Oleria gunilla' } } })
    expect(cellOf(row, 'Sex')).toMatchObject({ value: 'female', kind: 'proposed', was: 'male' })
    expect(cellOf(row, 'SPECIES')).toMatchObject({ value: 'Hypothyris anastasia', kind: 'person', ai: 'Oleria gunilla', aiProposed: true })
    // A column added to the table: the row's value in the sheet, grey; a formula is locked.
    expect(cellOf(edited('r1', {}), 'SPECIES')).toMatchObject({ value: 'Oleria gunilla', kind: 'sheet' })
    expect(cellOf(edited('r1', {}), 'Tribe').kind).toBe('locked')
    // New rows: empty cells, and the pre-made row's formula columns locked.
    const fresh = created('c1', { Sex: 'male' })
    expect(cellOf(fresh, 'Sex').kind).toBe('proposed')
    expect(cellOf(fresh, 'Notes_Collection_data')).toMatchObject({ value: null, kind: 'empty' })
    expect(cellOf(fresh, 'Tribe', ['Tribe']).kind).toBe('locked')
    // A new row's cell the person emptied is still theirs.
    expect(cellOf(created('c1', {}, { personEdits: { Sex: { ai: 'male' } } }), 'Sex')).toMatchObject({ value: null, kind: 'person' })
  })
})

describe('the tables of a proposal', () => {
  it('one per sheet, with the columns in the sheet order and those the person added', () => {
    const p = proposal([
      created('c1', { SPECIES: 'Oleria gunilla', Sex: 'male' }),
      { ...edited('r1', { Death_date: 46000 }), sheet: 'Insectary_data' },
    ])
    const order = (sheet: string) => (sheet === 'Collection_data' ? ['Sex', 'SPECIES', 'Collector'] : undefined)
    const groups = sheetGroups(p, { Collection_data: ['Collector'] }, order)
    expect(groups.map(g => [g.sheet, g.fields, g.changes.length])).toEqual([
      ['Collection_data', ['Sex', 'SPECIES', 'Collector'], 1],
      ['Insectary_data', ['Death_date'], 1],
    ])
  })
})

describe('what the assistant changed', () => {
  it('lists the cells whose value changed between two revisions, and every cell of a new row', () => {
    const before = proposal([created('c1', { SPECIES: 'Oleria gunilla', Sex: 'male' }), edited('r1', { Sex: 'female' })])
    const after = proposal([
      created('c1', { SPECIES: 'Hypothyris anastasia', Sex: 'male', Collection_time: 0.4 }),
      edited('r1', { Sex: 'female' }),
      created('c2', { Sex: 'female' }),
    ])
    expect(changedCells(before, after).sort()).toEqual(
      [cellId('c1', 'SPECIES'), cellId('c1', 'Collection_time'), cellId('c2', 'Sex')].sort(),
    )
    // The first time it is shown, or another proposal: nothing flashes.
    expect(changedCells(undefined, after)).toEqual([])
    expect(changedCells({ ...before, id: 'p2' }, after)).toEqual([])
    expect(changedText(1)).toBe('La IA cambió 1 celda')
    expect(changedText(3)).toBe('La IA cambió 3 celdas')
  })
})

describe("the person's unsaved edits", () => {
  it('stay over a revision that arrives meanwhile, marked as theirs', () => {
    const server = proposal([created('c1', { SPECIES: 'Oleria gunilla', Sex: 'male' }), edited('r1', { Sex: 'female' })])
    const local = new Map([
      [cellId('c1', 'SPECIES'), 'Hypothyris anastasia'],
      [cellId('c1', 'Sex'), null],
    ])
    const shown = withLocal(server, local)
    expect(shown.changes[0].values).toEqual({ SPECIES: 'Hypothyris anastasia' })
    expect(shown.changes[0].personEdits?.SPECIES).toEqual({ ai: 'Oleria gunilla' })
    // Untouched rows keep their object (their table row is not redrawn).
    expect(shown.changes[1]).toBe(server.changes[1])
    expect(withLocal(server, new Map())).toBe(server)
  })
})

describe('applying', () => {
  it('takes the ticked rows that have something to write, by their index now', () => {
    const p = proposal([created('c1', { Sex: 'male' }), created('c2', {}), edited('r1', { Sex: 'female' }), created('c3', { Sex: 'female' })])
    expect(chosenIndexes(p, new Set())).toEqual([0, 2, 3])
    expect(chosenIndexes(p, new Set(['r1']))).toEqual([0, 3])
  })
})

describe('the panel beside T3', () => {
  it('takes the share of the space up to the dragged divider, between 20 and 80 %', () => {
    // At the right of a 1000 px wide area starting at 0: the divider at 600 px leaves 40 % to the panel.
    expect(panelShare(600, 0, 1000)).toBe(40)
    expect(panelShare(50, 0, 1000)).toBe(80)
    expect(panelShare(990, 0, 1000)).toBe(20)
    // Below: measured from the bottom of a 800 px tall area starting at 100.
    expect(panelShare(500, 100, 800)).toBe(50)
  })
  it('does not offer long lists for identifier columns', () => {
    expect(['CAM_ID', 'Tube_1_id', 'FieldMark_ID', 'Insectary_ID'].every(f => ID_COLUMN.test(f))).toBe(true)
    expect(['SPECIES', 'Sex', 'Collector', 'CLUTCH NUMBER', 'Identifier', 'ID_status'].some(f => ID_COLUMN.test(f))).toBe(false)
  })
})
