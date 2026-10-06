import { describe, expect, it } from 'vitest'
import { changeCount, claimHolder, googleNotice, overlaySums, overlayTable, stagedSummary, usedWhere, type StagedItem } from '../staged'
import type { Table } from '../types'

const table = (module: string, rows: Table['rows']): Table => ({ module, revision: '1', columns: [], headerProblems: [], rows })
const item = (over: Partial<StagedItem>): StagedItem => ({
  id: 'i1',
  entryId: 'e1',
  purpose: 'emergidos',
  kind: 'create',
  sheet: 'Insectary_data',
  recordId: null,
  clientId: 'c1',
  rowId: 'staged:c1',
  label: null,
  values: {},
  actor: 'ana',
  actorName: 'Ana',
  createdAt: '2026-10-05T15:00:00Z',
  updatedAt: '2026-10-05T15:00:00Z',
  status: 'staged',
  ...over,
})

describe('the sheet with the entries kept in the app on top', () => {
  const insectary = table('Insectary_data', [
    { id: 'r2', row: 2, version: 1, observed: true, values: { Insectary_ID: 'A0E', Sex: 'female' }, formulas: [] },
    { id: 'r3', row: 3, version: 1, observed: false, values: { Insectary_ID: 'A1E', SPECIES: 'Mechanitis' }, formulas: ['Insectary_ID', 'SPECIES'] },
    { id: 'r4', row: 4, version: 1, observed: false, values: { Insectary_ID: 'A2E' }, formulas: ['Insectary_ID'] },
  ])

  it('puts a new butterfly in the pre-made row of its ID, as the row everyone sees, marked with who', () => {
    const out = overlayTable(insectary, [item({ label: 'A1E', values: { Insectary_ID: 'A1E', Sex: 'male', Intro2Insectary_date: 46300 } })])
    const row = out.table!.rows[1]
    expect(row.id).toBe('staged:c1')
    expect(row.row).toBe(3)
    expect(row.observed).toBe(true)
    expect(row.values).toMatchObject({ Insectary_ID: 'A1E', Sex: 'male', SPECIES: 'Mechanitis' })
    expect(row.formulas).toEqual(['Insectary_ID', 'SPECIES'])
    expect(out.marks['staged:c1']).toMatchObject({ create: true, who: ['Ana'], sent: false })
    expect(insectary.rows[1].id).toBe('r3') // the sheet's copy is left as it is
  })

  it("an edit's cells over its row; a sum shows its total, and its text goes to the clutch editors", () => {
    const stocks = table('Insectary_stocks', [{ id: 's1', row: 2, version: 1, observed: true, values: { 'CLUTCH NUMBER': 990, 'NUMBER OF LARVAE': 10 }, formulas: [] }])
    const out = overlayTable(stocks, [
      item({ kind: 'edit', sheet: 'Insectary_stocks', recordId: 's1', rowId: 's1', clientId: null, values: { 'NUMBER OF LARVAE': { formula: '=10+4' } } }),
      item({ id: 'i2', kind: 'edit', sheet: 'Insectary_stocks', recordId: 's1', rowId: 's1', clientId: null, values: { NOTES: 'ok' }, actorName: 'Luis', status: 'sent' }),
    ])
    expect(out.table!.rows[0].values).toMatchObject({ 'NUMBER OF LARVAE': 14, NOTES: 'ok' })
    expect(out.sums).toEqual({ s1: { 'NUMBER OF LARVAE': '=10+4' } })
    expect(out.marks.s1).toMatchObject({ fields: ['NUMBER OF LARVAE', 'NOTES'], who: ['Ana', 'Luis'], sent: true })
    expect(overlaySums({ s1: { 'NUMBER OF EGGS': '=12' } }, out.sums)).toEqual({ s1: { 'NUMBER OF EGGS': '=12', 'NUMBER OF LARVAE': '=10+4' } })
  })

  it('a new clutch goes after the last row; other sheets and an empty list leave the table alone', () => {
    const stocks = table('Insectary_stocks', [{ id: 's1', row: 7, version: 1, observed: true, values: { 'CLUTCH NUMBER': 990 }, formulas: [] }])
    const out = overlayTable(stocks, [item({ sheet: 'Insectary_stocks', values: { 'CLUTCH NUMBER': 991 } })])
    expect(out.table!.rows.at(-1)).toMatchObject({ id: 'staged:c1', row: 8, values: { 'CLUTCH NUMBER': 991 } })
    expect(overlayTable(stocks, []).table).toBe(stocks)
    expect(overlayTable(stocks, [item({ values: { Insectary_ID: 'A1E' } })]).table).toBe(stocks)
  })

  it('what «Guardar en Google Sheets» will write, by sheet and row, and how many rows', () => {
    const items = [
      item({ label: 'A1E', values: { Insectary_ID: 'A1E', Sex: 'male' } }),
      item({ id: 'i2', kind: 'edit', sheet: 'Insectary_stocks', recordId: 's1', rowId: 's1', clientId: null, label: '990', values: { 'NUMBER OF ADULTS': { formula: '=1' }, NOTES: 'x' }, actorName: 'Luis' }),
      item({ id: 'i3', clientId: 'c3', rowId: 'staged:c3', status: 'sent', values: { Insectary_ID: 'A2E' } }),
    ]
    expect(changeCount(items)).toBe(2)
    expect(changeCount(items, 'sent')).toBe(1)
    const summary = stagedSummary(items)
    expect(summary.map(g => [g.sheet, g.rows.map(r => [r.label, r.isNew, r.cells.map(c => `${c.field}=${c.value}`).join(','), r.who.join()])])).toEqual([
      ['Insectary_data', [['A1E', true, 'Insectary_ID=A1E,Sex=male', 'Ana']]],
      ['Insectary_stocks', [['990', false, 'NUMBER OF ADULTS==1,NOTES=x', 'Luis']]],
    ])
  })

  it('who holds an identifier, and where a used CAM or tube is', () => {
    const claims = [{ kind: 'insectary' as const, value: 'A4E', itemId: 'i1', actor: 'ana', actorName: 'Ana' }]
    expect(claimHolder(claims, 'insectary', ' a4e ')?.actorName).toBe('Ana')
    expect(claimHolder(claims, 'cam', 'A4E')).toBeNull()
    expect(usedWhere({ sheet: null, row: null, label: null, claimedBy: 'Ana' })).toBe('Ana, en la app (aún no en Google Sheets)')
    expect(usedWhere({ sheet: 'Insectary_data', row: 12, label: 'A0E' })).toBe('Insectary_data fila 12 (A0E)')
  })
})

describe('the banner about Google', () => {
  it('busy, slow, writing what waited, or nothing', () => {
    expect(googleNotice('busy', 3)).toEqual({
      kind: 'busy',
      text: 'Google Sheets no responde (está recalculando la hoja): los guardados se conservan aquí y se escriben cuando responda; 3 esperando',
    })
    expect(googleNotice('slow', 0)?.kind).toBe('slow')
    expect(googleNotice('ok', 2)).toEqual({ kind: 'writing', text: 'Escribiendo en Google Sheets 2 guardados que esperaban' })
    expect(googleNotice('ok', 0)).toBeNull()
  })
})
