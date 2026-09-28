import { beforeEach, describe, expect, it, vi } from 'vitest'
import { markRaw } from 'vue'
import { createPinia, setActivePinia } from 'pinia'
import { dateProblem, localProblems, type CheckSheet } from '../saveChecks'
import type { Field, TableRow } from '../types'
import { usePending } from '../../stores/pending'
import { useTables } from '../../stores/tables'
import { verificationsFor } from '../verifications'

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

describe('saving pending changes', () => {
  let sent: { edits: { id: string; values: Record<string, unknown> }[]; partial: boolean }[] = []
  let answer: () => Response
  beforeEach(async () => {
    setActivePinia(createPinia())
    localStorage.clear()
    sent = []
    answer = () => new Response(JSON.stringify({ records: [], skipped: [], created: [] }))
    vi.stubGlobal(
      'fetch',
      vi.fn(async (url: string, init?: RequestInit) => {
        if (url.startsWith('api/verifications'))
          return new Response(
            JSON.stringify({
              unique: rules.unique,
              lists: { Preserved_Dead_Alive: { strict: true, source: 'x', values: ['Dead', 'Alive', 'NA'] } },
            }),
          )
        sent.push(JSON.parse(String(init?.body)))
        return answer()
      }),
    )
    useTables().tables.Insectary_data = markRaw({ module: 'Insectary_data', revision: '1', columns, rows, headerProblems: [] })
    verificationsFor('Insectary_data')
    await new Promise(resolve => setTimeout(resolve, 0))
  })

  it('saves every other change and keeps a repeated CAM and a broken date pending, with the reason', async () => {
    const pending = usePending()
    pending.setAutoSave(false)
    pending.setCell('Insectary_data', rows[1], 'N3D', 'CAM_ID', 'CAM078274')
    pending.setCell('Insectary_data', rows[1], 'N3D', 'Death_date', 46292)
    pending.setCell('Insectary_data', rows[2], 'N4D', 'Death_date', 32_902_000)
    pending.setCell('Insectary_data', rows[2], 'N4D', 'Death_cause', 'Unknown')
    const result = await pending.save('')
    expect(sent).toHaveLength(1)
    expect(sent[0].partial).toBe(true)
    expect(sent[0].edits).toEqual([
      { id: 'r2', values: { Death_date: 46292 }, expected: { Death_date: null } },
      { id: 'r3', values: { Death_cause: 'Unknown' }, expected: { Death_cause: null } },
    ])
    expect(result).toEqual({ saved: 2, left: 2 })
    expect(Object.keys(pending.edits.r2.values)).toEqual(['CAM_ID'])
    expect(Object.keys(pending.edits.r3.values)).toEqual(['Death_date'])
    expect(pending.issues['r2:CAM_ID']).toBe('CAM078274 ya está usado en Insectary_data fila 13384 (N2D)')
    expect(pending.issues['r3:Death_date']).toMatch(/Fecha no válida/)
  })

  it('keeps what the server left out red, and automatic saving does not send it again until it is edited', async () => {
    const pending = usePending()
    pending.setAutoSave(false)
    answer = () =>
      new Response(
        JSON.stringify({
          records: [],
          created: [],
          skipped: [
            { id: 'r2', field: 'CAM_ID', code: 'DUPLICATE_ID', message: 'CAM079001 ya está usado en Collection_data fila 9000' },
          ],
        }),
      )
    pending.setCell('Insectary_data', rows[1], 'N3D', 'CAM_ID', 'CAM079001')
    pending.setCell('Insectary_data', rows[2], 'N4D', 'Death_cause', 'Unknown')
    expect(await pending.save('')).toEqual({ saved: 1, left: 1 })
    expect(pending.errors['r2:CAM_ID']).toMatch(/Collection_data fila 9000/)
    expect(pending.edits.r3).toBeUndefined()
    // Another change: automatic saving sends it, not the refused CAM.
    pending.setCell('Insectary_data', rows[2], 'N4D', 'Sex', 'male')
    answer = () => new Response(JSON.stringify({ records: [], skipped: [], created: [] }))
    await pending.save('', { retryRefused: false })
    expect(sent[1].edits).toEqual([{ id: 'r3', values: { Sex: 'male' }, expected: { Sex: null } }])
    expect(pending.errors['r2:CAM_ID']).toBeDefined()
    // Nothing else to send: no request at all.
    expect(await pending.save('', { retryRefused: false })).toEqual({ saved: 0, left: 1 })
    expect(sent).toHaveLength(2)
  })

  it("leaves a walk's new rows for Guardar: automatic saving sends only the other changes", async () => {
    const pending = usePending()
    pending.setAutoSave(false)
    pending.addCreate('Insectary_data', 'captura', { Insectary_ID: 'Z9Z' }, { manual: true })
    pending.setCell('Insectary_data', rows[2], 'N4D', 'Death_cause', 'Unknown')
    await pending.save('', { retryRefused: false, auto: true })
    expect(sent[0].edits).toEqual([{ id: 'r3', values: { Death_cause: 'Unknown' }, expected: { Death_cause: null } }])
    expect(JSON.stringify(sent[0])).not.toContain('Z9Z')
    expect(pending.creates).toHaveLength(1)
    // Guardar sends it.
    await pending.save('')
    expect(JSON.stringify(sent[1])).toContain('Z9Z')
  })
})
