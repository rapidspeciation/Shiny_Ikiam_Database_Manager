import { describe, expect, it } from 'vitest'
import { dateProblem, localProblems, type CheckSheet } from '../saveChecks'
import type { Field, TableRow } from '../types'

const columns: Field[] = [
  { key: 'Insectary_ID', label: 'Insectary ID', type: 'text' },
  { key: 'CAM_ID', label: 'CAM ID', type: 'text' },
  { key: 'Tube_1_id', label: 'Tube 1 id', type: 'text' },
  { key: 'Death_date', label: 'Death date', type: 'date' },
  { key: 'Death_cause', label: 'Death cause', type: 'text' },
  { key: 'Preserved_Dead_Alive', label: 'Preserved Dead Alive', type: 'text' },
]
const row = (id: string, n: number, values: TableRow['values']): TableRow => ({
  id,
  row: n,
  version: 1,
  observed: true,
  values,
  formulas: [],
})
const rows = [
  row('r1', 13384, { Insectary_ID: 'N2D', CAM_ID: 'CAM078274', Tube_1_id: 'FS90415320' }),
  row('r2', 13385, { Insectary_ID: 'N3D' }),
  row('r3', 13386, { Insectary_ID: 'N4D' }),
]
const rules = {
  unique: ['Insectary_ID', 'CAM_ID', 'Tube_1_id'],
  lists: { Preserved_Dead_Alive: { strict: true, source: 'lista fija de la hoja', values: new Set(['Dead', 'Alive', 'NA']) } },
}

describe('checks before saving', () => {
  it('refuses dates with an impossible year, never NaN', () => {
    const date = columns[3]
    expect(dateProblem(date, 46292)).toBeNull()
    expect(dateProblem(date, 'NA')).toBeNull()
    expect(dateProblem(date, null)).toBeNull()
    expect(dateProblem(date, Number.NaN)).toMatch(/Fecha no válida en Death_date/)
    // 21/09/92026, as a date box let it be typed.
    expect(dateProblem(date, 32_902_000)).toMatch(/entre 1990 y 2099/)
  })

  it('flags a repeated CAM with the row that holds it, a value outside a strict list, and a tube used in another sheet', () => {
    const sheets: Record<string, CheckSheet> = {
      Insectary_data: { rows, columns, rules },
      Collection_data: {
        rows: [row('c1', 8133, { CAM_ID: 'CAM079905', Tube_1_id: 'FS90415311' })],
        columns,
        rules: { unique: ['CAM_ID', 'Tube_1_id'], lists: {} },
      },
    }
    const problems = localProblems(
      [
        { module: 'Insectary_data', id: 'r2', isNew: false, values: { CAM_ID: 'CAM078274', Death_date: 46292 } },
        { module: 'Insectary_data', id: 'r3', isNew: false, values: { Preserved_Dead_Alive: 'alive?', Tube_1_id: 'FS90415311' } },
      ],
      sheets,
    )
    expect(problems).toEqual({
      'r2:CAM_ID': 'CAM078274 ya está usado en Insectary_data fila 13384 (N2D)',
      'r3:Preserved_Dead_Alive': expect.stringMatching(/«alive\?» no está en la lista/),
      'r3:Tube_1_id': 'FS90415311 ya está usado en Collection_data fila 8133 (CAM079905)',
    })
  })

  it('flags two unsaved rows given the same CAM (a fill-down), and moving a CAM away frees it', () => {
    const sheets = { Insectary_data: { rows, columns, rules } }
    const problems = localProblems(
      [
        { module: 'Insectary_data', id: 'r2', isNew: false, values: { CAM_ID: 'CAM078276' } },
        { module: 'Insectary_data', id: 'r3', isNew: false, values: { CAM_ID: 'CAM078276' } },
      ],
      sheets,
    )
    expect(problems['r2:CAM_ID']).toBe('CAM078276 ya está usado en Insectary_data fila 13386 (N4D), sin guardar todavía')
    expect(Object.keys(problems)).toHaveLength(2)
    // N2D's CAM moves to N3D while N2D gets another one: no repeat.
    expect(
      localProblems(
        [
          { module: 'Insectary_data', id: 'r1', isNew: false, values: { CAM_ID: 'CAM078280' } },
          { module: 'Insectary_data', id: 'r2', isNew: false, values: { CAM_ID: 'CAM078274' } },
        ],
        sheets,
      ),
    ).toEqual({})
  })
})
