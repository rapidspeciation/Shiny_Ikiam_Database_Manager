import { describe, expect, it } from 'vitest'
import {
  editMoment,
  formatMoment,
  formatWhen,
  linkedSave,
  mainPurpose,
  momentRange,
  purposeFromHash,
  rowsOf,
  stepEdit,
  timeRange,
  type CellEdit,
} from '../history'
import type { HistoryChange } from '../types'

describe('purpose of a save', () => {
  it('is the data-entry tab the change was typed in', () => {
    expect(purposeFromHash('#/muertes')).toBe('muertes')
    expect(purposeFromHash('#/colecta?paso=2')).toBe('colecta')
    expect(purposeFromHash('#/posturas')).toBe('clutches')
    expect(purposeFromHash('#/revision')).toBe('revision')
    expect(purposeFromHash('#/historial?grupo=x')).toBeUndefined()
    expect(purposeFromHash('')).toBeUndefined()
  })
  it('of a save with changes from two tabs is the one most changes came from', () => {
    expect(mainPurpose(['tubos', 'muertes', 'tubos', undefined])).toBe('tubos')
    expect(mainPurpose([undefined])).toBeUndefined()
  })
})

describe('times in the Historial', () => {
  it('are day first, in Ecuador time', () => {
    expect(formatWhen('2026-09-28T19:05:00Z')).toBe('28-Sep-26 14:05')
    expect(formatWhen('2026-09-29T04:30:00Z')).toBe('28-Sep-26 23:30')
    expect(formatWhen('nope')).toBe('')
  })
  it('show a range once per day', () => {
    expect(timeRange('2026-09-28T19:05:00Z', '2026-09-28T19:32:00Z')).toBe('28-Sep-26 14:05–14:32')
    expect(timeRange('2026-09-28T19:05:00Z', '2026-09-28T19:05:30Z')).toBe('28-Sep-26 14:05')
    expect(timeRange('2026-09-29T04:50:00Z', '2026-09-29T05:10:00Z')).toBe('28-Sep-26 23:50 – 29-Sep-26 00:10')
  })
})

describe('changes of a save', () => {
  const change = (id: string, recordId: string, field: string, extra: Partial<HistoryChange> = {}): HistoryChange => ({
    id,
    recordId,
    sheet: 'Insectary_data',
    row: 10,
    field,
    before: null,
    after: 'x',
    label: recordId.toUpperCase(),
    ...extra,
  })
  it('are grouped by row in the order the rows were first changed', () => {
    const rows = rowsOf([
      change('1', 'a0d', 'Death_date'),
      change('2', 'a1d', 'Death_date', { isNew: true }),
      change('3', 'a0d', 'Death_cause'),
      change('4', 'x', 'Sex', { label: undefined, row: 44 }),
    ])
    expect(rows.map(r => [r.label, r.isNew, r.changes.map(c => c.id)])).toEqual([
      ['A0D', false, ['1', '3']],
      ['A1D', true, ['2']],
      ['Insectary_data fila 44', false, ['4']],
    ])
  })
})

describe('links to a save', () => {
  it('name a group or an action', () => {
    expect(linkedSave({ grupo: 'g1' })).toBe('g1')
    expect(linkedSave({ accion: ' a1 ' })).toBe('a1')
    expect(linkedSave({ grupo: ['g2', 'g3'] })).toBe('g2')
    expect(linkedSave({})).toBeNull()
  })
})

describe("a cell's history and the sheet as it was", () => {
  it('reads moments day first in Ecuador, a range on one day once', () => {
    expect(formatMoment('2026-10-02T19:05:00Z')).toBe('02/10/26 14:05')
    expect(momentRange('2026-10-02T19:05:00Z', '2026-10-02T19:12:00Z')).toBe('02/10/26 14:05–14:12')
    expect(momentRange('2026-10-02T04:58:00Z', '2026-10-02T05:04:00Z')).toBe('01/10/26 23:58 – 02/10/26 00:04')
    expect(momentRange('2026-10-02T19:05:00Z', '2026-10-02T19:05:30Z')).toBe('02/10/26 14:05')
  })
  it('steps to the previous or next edit on the same side, and stops at the ends', () => {
    const edit = (first: string, ...rest: string[]) => ({ first, last: rest.at(-1) ?? first, actionIds: [first, ...rest] }) as CellEdit
    const edits = [edit('a'), edit('b1', 'b2', 'b3'), edit('c')]
    expect(editMoment(edits[1], 'before')).toEqual({ action: 'b1', side: 'before' })
    expect(editMoment(edits[1], 'after')).toEqual({ action: 'b3', side: 'after' })
    expect(stepEdit(edits, 'a', 'before', 1)).toEqual({ action: 'b1', side: 'before' })
    expect(stepEdit(edits, 'c', 'after', -1)).toEqual({ action: 'b3', side: 'after' })
    // From a save inside an edit (a link from the Historial).
    expect(stepEdit(edits, 'b2', 'before', 1)).toEqual({ action: 'c', side: 'before' })
    expect(stepEdit(edits, 'c', 'before', 1)).toBeNull()
    expect(stepEdit(edits, 'a', 'before', -1)).toBeNull()
    expect(stepEdit(edits, 'zz', 'before', 1)).toBeNull()
  })
})
