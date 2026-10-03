import { describe, expect, it } from 'vitest'
import {
  ID_COLUMN,
  cellComments,
  cellId,
  cellOf,
  cellOrder,
  changedCells,
  changedText,
  expandProposal,
  nextCell,
  notApplied,
  pageNote,
  panelShare,
  photoSummaries,
  readOnlyRow,
  rowsToWrite,
  sampleWarnings,
  selectionActions,
  sheetGroups,
  uncheckedDoubts,
  unfilledUnreadable,
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
  it("say why beside the cell (a corner mark): the doubt's reason, a hint, a warning, why unreadable", () => {
    const row = edited(
      'r1',
      { 'CLUTCH NUMBER': 843, Death_date: 46000, Sex: 'male', Notes: 'x' },
      {
        doubts: { 'CLUTCH NUMBER': doubt, Sex: { confidence: 0.5, checked: { by: 'AA' } } },
        inferred: ['Death_date'],
        hints: { Death_date: { text: 'De la nota: «preserved»' } },
        warnings: { CAM_ID: { text: 'Preservada sin CAM' } },
        unreadable: { Tube_1_id: { reason: 'manchado', partial: ['FS1?'] } },
      },
    )
    const said = (f: string) => cellComments(cellOf(row, f)).map(n => [n.label, n.text, n.kind])
    expect(said('CLUTCH NUMBER')).toEqual([['Dudosa', doubt.reason, 'doubt']])
    expect(said('Sex')).toEqual([['Revisada', 'Lectura dudosa (por AA)', 'hint']])
    expect(said('Death_date')).toEqual([['No escrito en la línea', 'De la nota: «preserved»', 'hint']])
    expect(said('CAM_ID')).toEqual([['Falta', 'Preservada sin CAM', 'doubt']])
    expect(said('Tube_1_id')).toEqual([['Ilegible', 'manchado', 'unreadable']])
    // Nothing said: no mark.
    expect(said('Notes')).toEqual([])
    // A hint is about the assistant's value: once the person typed over it, it is not shown.
    const typed = { ...row, personEdits: { Death_date: { ai: 46000 } } }
    expect(cellComments(cellOf(typed, 'Death_date'))).toEqual([])
    // Filled by hand, an unreadable cell still says why it was.
    const filled = { ...row, values: { ...row.values, Tube_1_id: 'FS12' }, personEdits: { Tube_1_id: {} } }
    expect(cellComments(cellOf(filled, 'Tube_1_id'))).toEqual([
      { label: 'Ilegible en el cuaderno', text: 'manchado (rellenada a mano)', kind: 'hint' },
    ])
  })
  it('go one after another in the order the tables show them, the first again after the last', () => {
    const order = cellOrder([
      { keys: ['a', 'b'], fields: ['Sex', 'CLUTCH NUMBER'] },
      { keys: ['c'], fields: ['SPECIES'] },
    ])
    const cells = [
      { key: 'c', field: 'SPECIES' },
      { key: 'b', field: 'Sex' },
      { key: 'a', field: 'CLUTCH NUMBER' },
      { key: 'gone', field: 'Sex' },
    ]
    expect(nextCell(cells, order)).toEqual({ key: 'a', field: 'CLUTCH NUMBER' })
    expect(nextCell(cells, order, { key: 'a', field: 'CLUTCH NUMBER' })).toEqual({ key: 'b', field: 'Sex' })
    // From a cell no longer doubtful (just checked): the one after where it is.
    expect(nextCell(cells, order, { key: 'a', field: 'Sex' })).toEqual({ key: 'a', field: 'CLUTCH NUMBER' })
    expect(nextCell(cells, order, { key: 'b', field: 'CLUTCH NUMBER' })).toEqual({ key: 'c', field: 'SPECIES' })
    // A cell not shown goes last; after the last, the first again.
    expect(nextCell(cells, order, { key: 'c', field: 'SPECIES' })).toEqual({ key: 'gone', field: 'Sex' })
    expect(nextCell(cells, order, { key: 'gone', field: 'Sex' })).toEqual({ key: 'a', field: 'CLUTCH NUMBER' })
    // The cell just checked (still listed until saved) is never the next; alone, there is none.
    expect(nextCell([{ key: 'a', field: 'Sex' }], order, { key: 'a', field: 'Sex' })).toBeNull()
    expect(nextCell([], order)).toBeNull()
  })
  it('marked checked in the table show so before the save comes back', () => {
    const p = proposal([edited('r1', { 'CLUTCH NUMBER': 843 }, { doubts: { 'CLUTCH NUMBER': doubt } })])
    const marked = withLocal(p, new Map(), new Map([[cellId('r1', 'CLUTCH NUMBER'), true]]))
    expect(cellOf(marked.changes[0], 'CLUTCH NUMBER').doubtful).toBe(false)
    expect(uncheckedDoubts(marked)).toEqual([])
    expect(uncheckedDoubts(withLocal(marked, new Map(), new Map([[cellId('r1', 'CLUTCH NUMBER'), false]])))).toHaveLength(1)
  })
})

describe('unreadable cells', () => {
  const smudged = { reason: 'smudged', partial: ['1?/9'] }
  it('show empty with their own kind until someone fills them, and are never written meanwhile', () => {
    const row = edited('r1', {}, { current: { Death_date: null, Sex: 'female' }, unreadable: { Death_date: smudged } })
    const cell = cellOf(row, 'Death_date')
    expect(cell).toMatchObject({ kind: 'unreadable', value: null, unreadable: smudged, doubtful: false })
    // Neither of the buttons applies to it, nor «Marcar revisadas».
    expect(selectionActions([cell])).toEqual({ sheet: 0, ai: 0, check: 0 })
    // A new row's unreadable cell is empty too.
    expect(cellOf(created('c1', { Sex: 'male' }, { unreadable: { CAM_ID: {} } }), 'CAM_ID')).toMatchObject({ kind: 'unreadable', value: null })
    // Its column shows even with nothing to write in it.
    expect(sheetGroups(proposal([row]))[0].fields).toContain('Death_date')
    const p = proposal([row, created('c1', { Sex: 'male' }, { index: 1, unreadable: { CAM_ID: {} } })])
    expect(unfilledUnreadable(p).map(u => [u.key, u.field])).toEqual([
      ['r1', 'Death_date'],
      ['c1', 'CAM_ID'],
    ])
    // A row whose only cells are unreadable writes nothing yet.
    expect(rowsToWrite(p)).toEqual([1])
  })
  it('typed by the person become theirs (written), and go back to unreadable when emptied', () => {
    const p = proposal([edited('r1', {}, { current: { Death_date: null }, unreadable: { Death_date: smudged } })])
    const typed = withLocal(p, new Map([[cellId('r1', 'Death_date'), { value: 46000 }]]))
    expect(cellOf(typed.changes[0], 'Death_date')).toMatchObject({ kind: 'person', value: 46000, unreadable: smudged })
    expect(unfilledUnreadable(typed)).toEqual([])
    expect(rowsToWrite(typed)).toEqual([0])
    const back = withLocal(typed, new Map([[cellId('r1', 'Death_date'), { value: null, use: 'sheet' }]]))
    expect(cellOf(back.changes[0], 'Death_date').kind).toBe('unreadable')
    expect(unfilledUnreadable(back)).toHaveLength(1)
  })
  it('a preserved butterfly left without CAM or tube: those cells are marked and shown, and counted per row', () => {
    const why = { text: 'Preservada sin CAM_ID: pregunta a quien la preservó' }
    const row = edited(
      'r1',
      { Death_cause: 'Killed_Preserved' },
      { sheet: 'Insectary_data', current: { Death_cause: null }, warnings: { CAM_ID: why, Tube_1_id: why } },
    )
    expect(cellOf(row, 'CAM_ID')).toMatchObject({ kind: 'sheet', value: null, warning: why })
    expect(cellOf(row, 'Death_cause').warning).toBeUndefined()
    expect(sheetGroups(proposal([row]))[0].fields).toEqual(expect.arrayContaining(['CAM_ID', 'Tube_1_id']))
    expect(sampleWarnings(proposal([row])).map(w => [w.key, w.field])).toEqual([
      ['r1', 'CAM_ID'],
      ['r1', 'Tube_1_id'],
    ])
  })
  it('in a context row (never written) do not count', () => {
    const p = proposal([edited('r1', {}, { context: true, unreadable: { Sex: {} } })])
    expect(unfilledUnreadable(p)).toEqual([])
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

describe("a notebook page's proposal", () => {
  const line = (n: number, extra: Partial<ProposalChange> = {}): ProposalChange => ({
    ...edited(`r${n}`, {}, { sheet: 'Insectary_data', label: `${n}AB`, index: -n, context: true }),
    page: { photo: 0, line: n, raw: `${n}AB ♀`, status: 'match' },
    ...extra,
  })
  it("shows what the SPECIES formula will give, grey and never written, where the sheet's cell is blank", () => {
    const row = edited('r1', { 'CLUTCH NUMBER': 838 }, { sheet: 'Insectary_data', rowValues: {}, current: undefined, formulaGives: { SPECIES: 'Mechanitis lysimnia' } })
    expect(cellOf(row, 'SPECIES')).toMatchObject({ value: 'Mechanitis lysimnia', kind: 'sheet', fromFormula: true, was: null })
    expect(proposal([row]).changes[0].values).toEqual({ 'CLUTCH NUMBER': 838 })
    // Typed by the person: theirs, not the formula's.
    const typed = { ...row, values: { ...row.values, SPECIES: 'Mechanitis polymnia' }, personEdits: { SPECIES: {} } }
    expect(cellOf(typed, 'SPECIES')).toMatchObject({ value: 'Mechanitis polymnia', kind: 'person' })
    expect(cellOf(typed, 'SPECIES').fromFormula).toBeUndefined()
  })
  it("typing the species the formula gives leaves it to the formula (not a change), the assistant's other one aside", () => {
    const row = edited(
      'r1',
      { 'CLUTCH NUMBER': 838, SPECIES: 'Mechanitis messenoides deceptus' },
      { sheet: 'Insectary_data', rowValues: {}, current: undefined, formulaGives: { SPECIES: 'Mechanitis messenoides intermedia' } },
    )
    const p = proposal([row])
    expect(cellOf(p.changes[0], 'SPECIES')).toMatchObject({ kind: 'proposed', value: 'Mechanitis messenoides deceptus' })
    const typed = withLocal(p, new Map([[cellId('r1', 'SPECIES'), { value: ' mechanitis messenoides  intermedia' }]]))
    expect(typed.changes[0].values).toEqual({ 'CLUTCH NUMBER': 838 })
    expect(cellOf(typed.changes[0], 'SPECIES')).toMatchObject({
      kind: 'reverted',
      value: 'Mechanitis messenoides intermedia',
      fromFormula: true,
      ai: 'Mechanitis messenoides deceptus',
    })
    // Another species is the person's, written over the formula.
    const other = withLocal(p, new Map([[cellId('r1', 'SPECIES'), { value: 'Mechanitis polymnia' }]]))
    expect(cellOf(other.changes[0], 'SPECIES')).toMatchObject({ kind: 'person', value: 'Mechanitis polymnia' })
  })
  it('makes the lean proposal whole: hints from its table, formula columns from its sheet', () => {
    const lean = {
      ...proposal([
        edited('r1', { Tube_2_id: 'NA' }, { formulas: undefined, hints: { Tube_2_id: 0 } as never }),
        edited('r2', {}, { formulas: ['Tribe', 'Genus'] }),
      ]),
      hintTable: [{ msg: { key: 'Individuo preservado: lo que el equipo escribe siempre' } }],
      sheetFormulas: { Collection_data: ['Tribe'] },
    }
    const whole = expandProposal(lean)
    expect(whole.changes[0].hints).toEqual({ Tube_2_id: { text: '', msg: { key: 'Individuo preservado: lo que el equipo escribe siempre' } } })
    expect(whole.changes[0].formulas).toEqual(['Tribe'])
    expect(whole.changes[1].formulas).toEqual(['Tribe', 'Genus'])
    const plain = proposal([])
    expect(expandProposal(plain)).toBe(plain)
  })
  it("orders the columns as the notebook, then the implied ones, then the template's last (to fold)", () => {
    const p = {
      ...proposal([
        edited(
          'r1',
          { Death_date: 46000, Death_cause: 'Unknown', Wild_Reared: 'Reared', Tube_2_id: 'NA', Tube_2_tissue: 'NOT_COLLECTED', Sex: 'male', Notes: 'x' },
          { sheet: 'Insectary_data', inferred: ['Wild_Reared', 'Tube_2_id', 'Tube_2_tissue'] },
        ),
        line(2),
      ]),
      page: { kind: 'emergence', sheet: 'Insectary_data', columns: ['Insectary_ID', 'SPECIES', 'Sex', 'Death_date', 'Death_cause'], photos: 1 },
    }
    const sheetOrder = ['Insectary_ID', 'Notes', 'Tube_2_tissue', 'Tube_2_id', 'Wild_Reared', 'Death_cause', 'Death_date', 'Sex']
    const [g] = sheetGroups(p, {}, () => sheetOrder)
    expect(g.fields).toEqual(['Sex', 'Death_date', 'Death_cause', 'Wild_Reared', 'Notes', 'Tube_2_tissue', 'Tube_2_id'])
    expect(g.template).toEqual(['Tube_2_tissue', 'Tube_2_id'])
  })
  it('says why a line writes nothing, keeps it read-only, and counts each photo', () => {
    expect(pageNote(line(2))).toBe('Línea 2: «2AB ♀» · Ya está así en la hoja')
    const missing = line(4, {
      placeholder: true,
      recordId: null,
      page: { photo: 1, line: 4, raw: '4AB', status: 'missing', near: [{ value: '4AD', row: 9 }] },
    })
    expect(pageNote(missing)).toBe('Línea 4: «4AB» · No está en la hoja; ¿quisiste decir 4AD (fila 9)?')
    const refused = line(5, { page: { photo: 1, line: 5, raw: '5AB', status: 'match', error: 'Tube_1_id FD1 ya está en Insectary_data fila 9' } })
    expect(pageNote(refused)).toBe('Línea 5: «5AB» · No se puede escribir: Tube_1_id FD1 ya está en Insectary_data fila 9')
    expect([line(2), missing, refused, edited('r1', { Sex: 'female' })].map(readOnlyRow)).toEqual([true, true, false, false])
    const written = edited('r1', { Sex: 'female' }, { page: { photo: 0, line: 1 } })
    expect(photoSummaries([written, line(2), line(3), missing, refused])).toEqual([
      { photo: 0, from: 1, to: 3, change: 1, same: 2, other: 0 },
      { photo: 1, from: 4, to: 5, change: 0, same: 0, other: 2 },
    ])
  })
})
