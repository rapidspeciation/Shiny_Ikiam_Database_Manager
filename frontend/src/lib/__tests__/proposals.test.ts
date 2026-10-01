import { describe, expect, it } from 'vitest'
import {
  ID_COLUMN,
  cellId,
  cellOf,
  changedCells,
  changedText,
  notApplied,
  panelShare,
  rowsToWrite,
  selectionActions,
  sheetGroups,
  uncheckedDoubts,
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
    // A new row's cell the person emptied: the assistant's value is kept aside, not written.
    expect(cellOf(created('c1', {}, { personEdits: { Sex: { ai: 'male' } } }), 'Sex')).toMatchObject({
      value: null,
      kind: 'reverted',
      ai: 'male',
    })
  })
  it("a cell set back to the sheet's value shows the sheet's value, the assistant's kept aside", () => {
    const row = edited('r1', {}, { personEdits: { Sex: { ai: 'female' } } })
    expect(cellOf(row, 'Sex')).toMatchObject({ value: 'male', kind: 'reverted', ai: 'female', aiProposed: true })
    // Even a suggestion to empty the cell.
    expect(cellOf(edited('r1', {}, { personEdits: { Sex: { ai: null } } }), 'Sex')).toMatchObject({ kind: 'reverted', ai: null })
  })
  it('an existing cell the proposal empties ({ clear: true }) is a change with the value it removes, shown as "vaciar"', () => {
    expect(cellOf(edited('r1', { Sex: null }), 'Sex')).toMatchObject({ value: null, kind: 'proposed', was: 'male' })
    // Leaving the column out is no change: the sheet's value, grey.
    expect(cellOf(edited('r1', {}), 'Sex')).toMatchObject({ value: 'male', kind: 'sheet' })
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
      [cellId('c1', 'SPECIES'), { value: 'Hypothyris anastasia' }],
      [cellId('c1', 'Sex'), { value: null }],
    ])
    const shown = withLocal(server, local)
    expect(shown.changes[0].values).toEqual({ SPECIES: 'Hypothyris anastasia' })
    expect(shown.changes[0].personEdits?.SPECIES).toEqual({ ai: 'Oleria gunilla' })
    // Untouched rows keep their object (their table row is not redrawn).
    expect(shown.changes[1]).toBe(server.changes[1])
    expect(withLocal(server, new Map())).toBe(server)
  })
  it('"Valor de la hoja" sets cells back with the assistant\'s value aside; "Valor de la IA" takes it again', () => {
    const server = proposal([
      created('c1', { SPECIES: 'Oleria gunilla', Sex: 'male' }),
      edited('r1', { Sex: 'female', SPECIES: 'Hypothyris anastasia' }, { personEdits: { SPECIES: { ai: 'Oleria gunilla' } } }),
    ])
    const back = withLocal(
      server,
      new Map([
        [cellId('c1', 'Sex'), { value: null, use: 'sheet' as const }],
        [cellId('r1', 'Sex'), { value: null, use: 'sheet' as const }],
        [cellId('r1', 'SPECIES'), { value: 'Oleria gunilla', use: 'ai' as const }],
      ]),
    )
    expect(back.changes[0].values).toEqual({ SPECIES: 'Oleria gunilla' })
    expect(cellOf(back.changes[0], 'Sex')).toMatchObject({ kind: 'reverted', ai: 'male' })
    expect(cellOf(back.changes[1], 'Sex')).toMatchObject({ value: 'male', kind: 'reverted', ai: 'female' })
    expect(cellOf(back.changes[1], 'SPECIES')).toMatchObject({ value: 'Oleria gunilla', kind: 'proposed' })
    expect(back.changes[1].personEdits).toEqual({ Sex: { ai: 'female' } })
    // A cell the person added (the assistant proposed nothing there) set back: no mark left.
    const added = withLocal(
      proposal([edited('r1', { Flight_height: 2 }, { personEdits: { Flight_height: {} } })]),
      new Map([[cellId('r1', 'Flight_height'), { value: null, use: 'sheet' as const }]]),
    )
    expect(added.changes[0].values).toEqual({})
    expect(added.changes[0].personEdits).toBeUndefined()
  })
})

describe('the buttons for the selected cells', () => {
  it('count the cells each one can change', () => {
    const row = edited(
      'r1',
      { Sex: 'female', SPECIES: 'Hypothyris anastasia', Flight_height: 2 },
      { personEdits: { SPECIES: { ai: 'Oleria gunilla' }, Flight_height: {}, Collector: { ai: 'FCH' } } },
    )
    const cells = ['Sex', 'SPECIES', 'Flight_height', 'Collector', 'Tribe'].map(f => cellOf(row, f))
    // Sex (the AI's), SPECIES and Flight_height (typed) go back to the sheet; SPECIES and Collector (set back) take the AI's again.
    expect(selectionActions(cells)).toEqual({ sheet: 3, ai: 2, check: 0 })
    expect(selectionActions([cellOf(row, 'Tribe')])).toEqual({ sheet: 0, ai: 0, check: 0 })
  })
})

describe('doubtful cells', () => {
  const doubt = { confidence: 0.5, alternatives: [848], reason: 'Clutch 848 entre líneas del 843 (misma emergencia)' }
  it('are the assistant values nobody reviewed: edited, set back or marked checked they are not', () => {
    const row = edited(
      'r1',
      { 'CLUTCH NUMBER': 843, Sex: 'female', Death_date: 46000 },
      {
        doubts: { 'CLUTCH NUMBER': doubt, Sex: { confidence: 0.4 }, Death_date: { confidence: 0.5, checked: { by: 'AA' } }, Notes: { confidence: 0.3 } },
        personEdits: { Sex: { ai: 'male' } },
        inferred: ['Death_date'],
        hints: { Death_date: { text: 'De la nota: «preserved»' } },
      },
    )
    const clutch = cellOf(row, 'CLUTCH NUMBER')
    expect(clutch.doubtful).toBe(true)
    expect(clutch.doubt).toEqual(doubt)
    expect(cellOf(row, 'Sex').doubtful).toBe(false)
    expect(cellOf(row, 'Death_date')).toMatchObject({ doubtful: false, inferred: true, hint: { text: 'De la nota: «preserved»' } })
    // «Marcar revisadas» counts only the unreviewed ones.
    expect(selectionActions(['CLUTCH NUMBER', 'Sex', 'Death_date'].map(f => cellOf(row, f)))).toMatchObject({ check: 1 })
    // The dialog and the server count the same cells; a doubt on a cell not written is none.
    const p = proposal([row, edited('r2', { Sex: 'male' }, { index: 1, doubts: { Sex: { confidence: 0.2 } } })])
    expect(uncheckedDoubts(p).map(d => [d.key, d.field])).toEqual([
      ['r1', 'CLUTCH NUMBER'],
      ['r2', 'Sex'],
    ])
    expect(uncheckedDoubts(p, [1]).map(d => d.key)).toEqual(['r2'])
  })
  it('marked checked in the table show so before the save comes back', () => {
    const p = proposal([edited('r1', { 'CLUTCH NUMBER': 843 }, { doubts: { 'CLUTCH NUMBER': doubt } })])
    const marked = withLocal(p, new Map(), new Map([[cellId('r1', 'CLUTCH NUMBER'), true]]))
    expect(cellOf(marked.changes[0], 'CLUTCH NUMBER').doubtful).toBe(false)
    expect(uncheckedDoubts(marked)).toEqual([])
    expect(uncheckedDoubts(withLocal(marked, new Map(), new Map([[cellId('r1', 'CLUTCH NUMBER'), false]])))).toHaveLength(1)
  })
})

describe('applying', () => {
  it('writes the rows with something left to write, and counts the suggestions set aside', () => {
    const p = proposal([
      created('c1', { Sex: 'male' }),
      // Every cell set back: dropped.
      created('c2', {}, { personEdits: { Sex: { ai: 'male' }, SPECIES: { ai: 'Oleria gunilla' } } }),
      edited('r1', { Sex: 'female' }),
      edited('r2', {}, { personEdits: { Sex: { ai: 'female' } } }),
      created('c3', { Sex: 'female' }),
    ])
    expect(rowsToWrite(p)).toEqual([0, 2, 4])
    expect(notApplied(p)).toBe(3)
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
