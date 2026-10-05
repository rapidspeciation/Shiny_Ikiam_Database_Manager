import { describe, expect, it } from 'vitest'
import {
  columnChoices,
  inCustomOrder,
  inNotebookOrder,
  inSheetOrder,
  moveColumn,
  orderColumns,
  readCustom,
  readView,
  viewFor,
  writeCustom,
  writeView,
  type KeptStorage,
} from '../proposalColumns'

/** Insectary_data's columns as the sheet has them (left to right). */
const SHEET = [
  'Insectary_ID',
  'Wild_Reared',
  'CLUTCH NUMBER',
  'Stock_of_origin',
  'SPECIES',
  'Sex',
  'Intro2Insectary_date',
  'Death_date',
  'Death_cause',
  'CAM_ID',
  'Tube_1_id',
  'T1_Preservation_medium',
  'Preserved_Dead_Alive',
  'Notes_Insectary_data',
]
/** The Emergidos notebook's columns, as the page has them (server/notebook.mjs KINDS.emergence). */
const NOTEBOOK = [
  'Insectary_ID',
  'SPECIES',
  'Sex',
  'CLUTCH NUMBER',
  'Stock_of_origin',
  'Intro2Insectary_date',
  'Death_date',
  'Death_cause',
  'CAM_ID',
  'Tube_1_id',
  'Notes_Insectary_data',
]
/** The table's columns as the server sends them (any order), the row's ID left to its own column. */
const FIELDS = ['SPECIES', 'Sex', 'CLUTCH NUMBER', 'Death_date', 'Notes_Insectary_data', 'Wild_Reared', 'T1_Preservation_medium', 'Stock_of_origin', 'Preserved_Dead_Alive']

const memory = (): KeptStorage & { data: Map<string, string> } => {
  const data = new Map<string, string>()
  return {
    data,
    getItem: k => data.get(k) ?? null,
    setItem: (k, v) => void data.set(k, v),
    removeItem: k => void data.delete(k),
  }
}

describe("a proposal table's column views", () => {
  it("'Hoja': every column in the sheet's order, those the sheet does not list at the end", () => {
    expect(orderColumns('sheet', FIELDS, { sheetOrder: SHEET })).toEqual([
      'Wild_Reared',
      'CLUTCH NUMBER',
      'Stock_of_origin',
      'SPECIES',
      'Sex',
      'Death_date',
      'T1_Preservation_medium',
      'Preserved_Dead_Alive',
      'Notes_Insectary_data',
    ])
    expect(inSheetOrder(['Zeta', 'Sex', 'Alpha', 'SPECIES'], SHEET)).toEqual(['SPECIES', 'Sex', 'Zeta', 'Alpha'])
  })
  it("'Cuaderno': the notebook's columns left to right, then the implied ones, then the rest in the sheet's order", () => {
    expect(orderColumns('notebook', FIELDS, { sheetOrder: SHEET, notebook: NOTEBOOK, implied: ['Preserved_Dead_Alive'] })).toEqual([
      'SPECIES',
      'Sex',
      'CLUTCH NUMBER',
      'Stock_of_origin',
      'Death_date',
      'Notes_Insectary_data',
      'Preserved_Dead_Alive',
      'Wild_Reared',
      'T1_Preservation_medium',
    ])
    // Without implied columns, the rest simply follows the sheet.
    expect(inNotebookOrder(['Sex', 'Wild_Reared', 'SPECIES'], NOTEBOOK, SHEET)).toEqual(['SPECIES', 'Sex', 'Wild_Reared'])
  })
  it("'Cuaderno' on a proposal without a notebook page is the sheet's order", () => {
    expect(orderColumns('notebook', FIELDS, { sheetOrder: SHEET })).toEqual(orderColumns('sheet', FIELDS, { sheetOrder: SHEET }))
    expect(viewFor('notebook', false)).toBe('sheet')
    expect(viewFor('notebook', true)).toBe('notebook')
    expect(viewFor('custom', false)).toBe('custom')
  })
  it("'Personal': the person's columns first in their order, new ones after in the sheet's order, hidden ones out", () => {
    const custom = { order: ['Death_date', 'SPECIES', 'Location_body'], hidden: ['Wild_Reared', 'Stock_of_origin'] }
    const sheet = [...SHEET, 'Location_body']
    expect(orderColumns('custom', FIELDS, { sheetOrder: sheet, custom })).toEqual([
      'Death_date',
      'SPECIES',
      // Placed by the person though the proposal does not bring it: added to the table.
      'Location_body',
      'CLUTCH NUMBER',
      'Sex',
      'T1_Preservation_medium',
      'Preserved_Dead_Alive',
      'Notes_Insectary_data',
    ])
    // Nothing chosen yet: the sheet's order.
    expect(inCustomOrder(FIELDS, null, SHEET)).toEqual(orderColumns('sheet', FIELDS, { sheetOrder: SHEET }))
    // A column not in the sheet nor in the table is not added.
    expect(inCustomOrder(['Sex'], { order: ['Gone', 'Sex'], hidden: [] }, SHEET)).toEqual(['Sex'])
  })
  it("'Personal': a hidden column the proposal writes or marks still shows, where the person placed it", () => {
    const custom = { order: ['Sex', 'Wild_Reared', 'SPECIES'], hidden: ['Wild_Reared', 'Death_date'] }
    expect(orderColumns('custom', ['SPECIES', 'Sex', 'Wild_Reared', 'Death_date'], { sheetOrder: SHEET, custom, keep: ['Wild_Reared'] })).toEqual([
      'Sex',
      'Wild_Reared',
      'SPECIES',
    ])
  })
  it('moves a column in the list (drag or arrows)', () => {
    expect(moveColumn(['a', 'b', 'c', 'd'], 0, 2)).toEqual(['b', 'c', 'a', 'd'])
    expect(moveColumn(['a', 'b', 'c', 'd'], 3, 0)).toEqual(['d', 'a', 'b', 'c'])
    expect(moveColumn(['a', 'b', 'c'], 0, 9)).toEqual(['b', 'c', 'a'])
    expect(moveColumn(['a', 'b'], 5, 0)).toEqual(['a', 'b'])
  })
})

describe("the person's column choices, kept in the browser", () => {
  it('the view per person, the sheet\'s by default', () => {
    const s = memory()
    expect(readView(s, 'ana')).toBe('sheet')
    writeView(s, 'ana', 'notebook')
    expect(readView(s, 'ana')).toBe('notebook')
    expect(readView(s, 'luis')).toBe('sheet')
    // Something unknown kept there: the sheet's.
    s.setItem('ithomiini:proposal-view:luis', 'columns')
    expect(readView(s, 'luis')).toBe('sheet')
  })
  it('the own columns per person and sheet, and reset', () => {
    const s = memory()
    const mine = { order: ['Sex', 'SPECIES'], hidden: ['Wild_Reared'] }
    writeCustom(s, 'ana', 'Insectary_data', mine)
    expect(readCustom(s, 'ana', 'Insectary_data')).toEqual(mine)
    expect(readCustom(s, 'ana', 'Insectary_stocks')).toBeNull()
    expect(readCustom(s, 'luis', 'Insectary_data')).toBeNull()
    // Damaged or odd data: what can be read of it, else nothing.
    s.setItem('ithomiini:proposal-columns-custom:luis:Insectary_data', '{"order":["Sex",3],"hidden":"x"}')
    expect(readCustom(s, 'luis', 'Insectary_data')).toEqual({ order: ['Sex'], hidden: [] })
    s.setItem('ithomiini:proposal-columns-custom:luis:Insectary_data', '{oops')
    expect(readCustom(s, 'luis', 'Insectary_data')).toBeNull()
    writeCustom(s, 'ana', 'Insectary_data', null)
    expect(readCustom(s, 'ana', 'Insectary_data')).toBeNull()
  })
  it('shared by every card of the page and kept in localStorage', () => {
    localStorage.clear()
    const a = columnChoices('tester')
    const b = columnChoices('tester')
    expect(a.view).toBe('sheet')
    a.setView('custom')
    expect(b.view).toBe('custom')
    a.setCustom('Insectary_data', { order: ['Sex'], hidden: ['SPECIES'] })
    expect(b.custom('Insectary_data')).toEqual({ order: ['Sex'], hidden: ['SPECIES'] })
    expect(JSON.parse(localStorage.getItem('ithomiini:proposal-columns-custom:tester:Insectary_data')!)).toEqual({
      order: ['Sex'],
      hidden: ['SPECIES'],
    })
    expect(localStorage.getItem('ithomiini:proposal-view:tester')).toBe('custom')
    // Another person on the same browser starts from the sheet's view.
    expect(columnChoices('other').view).toBe('sheet')
    expect(columnChoices('tester').view).toBe('custom')
    b.setCustom('Insectary_data', null)
    expect(localStorage.getItem('ithomiini:proposal-columns-custom:tester:Insectary_data')).toBeNull()
  })
})
