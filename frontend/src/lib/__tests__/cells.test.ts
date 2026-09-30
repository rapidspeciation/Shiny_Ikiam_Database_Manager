import { describe, expect, it } from 'vitest'
import { displayValue, editText, isBlank, normalizeInput } from '../cells'
import { formatSerial, isoToSerial, parseDateInput, serialToIso } from '../dates'
import { buildOptions } from '../options'
import type { Table } from '../types'

const date = { key: 'Death_date', type: 'date' as const }
const text = { key: 'Sex', type: 'text' as const }

describe('dates', () => {
  it('round-trips Sheets serial numbers', () => {
    expect(isoToSerial('2025-08-14')).toBe(45883)
    expect(serialToIso(45883)).toBe('2025-08-14')
    expect(formatSerial(45883)).toBe('14-Aug-25')
  })
  it('reads the formats people type', () => {
    for (const input of ['14-Aug-25', '14-ago-2025', '2025-08-14', '14/08/2025', '45883'])
      expect(parseDateInput(input)).toBe(45883)
    expect(parseDateInput('31/02/2025')).toBeNull()
    expect(parseDateInput('mañana')).toBeNull()
  })
})

describe('cells', () => {
  it('normalizes dates, NA, numbers and times', () => {
    expect(normalizeInput('14-Aug-25', date)).toEqual({ ok: true, value: 45883 })
    expect(normalizeInput('na', date)).toEqual({ ok: true, value: 'NA' })
    expect(normalizeInput('someday', date).ok).toBe(false)
    expect(normalizeInput('12', { key: 'NUMBER OF EGGS', type: 'number' })).toEqual({ ok: true, value: 12 })
    expect(normalizeInput('994(6)', { key: 'CLUTCH NUMBER', type: 'number' })).toEqual({ ok: true, value: '994(6)' })
    expect(normalizeInput('13:30', { key: 'Collection_time', type: 'text' })).toEqual({ ok: true, value: 0.5625 })
    expect(normalizeInput('=A1', text).ok).toBe(false)
    expect(normalizeInput('  ', text)).toEqual({ ok: true, value: null })
  })
  it('displays dates and times like the sheet', () => {
    expect(displayValue(45883, date)).toBe('14-Aug-25')
    expect(displayValue(0.5625, { key: 'Collection_time', type: 'text' })).toBe('13:30')
    expect(displayValue(null, text)).toBe('')
  })
  it('opens a cell to edit as a person types it: dates day first, times as hours', () => {
    // Not the serial number the sheet stores (46168), and read back to the same day.
    expect(editText(46168, date)).toBe('26/05/2026')
    for (const typed of [editText(46168, date), '26/5/26', '26-May-26', '26/05/2026'])
      expect(normalizeInput(typed, date)).toEqual({ ok: true, value: 46168 })
    expect(editText(0.5625, { key: 'Collection_time', type: 'text' })).toBe('13:30')
    // Anything else as shown: text, a count kept as a sum, NA in a date column, an empty cell.
    expect(editText('=17+37', { key: 'NUMBER OF EGGS', type: 'number' })).toBe('=17+37')
    expect(editText('NA', date)).toBe('NA')
    expect(editText(null, date)).toBe('')
  })
  it('treats empty and NA as blank', () => {
    expect([null, '', 'NA', ' n/a ', 'female'].map(isBlank)).toEqual([true, true, true, true, false])
  })
})

describe('options', () => {
  it('offers Lists values first and most used values next', () => {
    const table: Table = {
      module: 'Insectary_data',
      revision: '1',
      headerProblems: [],
      columns: [
        { key: 'Sex', label: 'Sex', type: 'text' },
        { key: 'Tube_1_tissue', label: 'Tube 1 tissue', type: 'text' },
      ],
      rows: [
        { id: 'a', row: 2, version: 1, observed: true, formulas: [], values: { Sex: 'NOT_COLLECTED', Tube_1_tissue: 'HEAD' } },
        { id: 'b', row: 3, version: 1, observed: true, formulas: [], values: { Sex: 'NOT_COLLECTED', Tube_1_tissue: 'HEAD' } },
        { id: 'c', row: 4, version: 1, observed: false, formulas: [], values: { Sex: 'ignored', Tube_1_tissue: null } },
      ],
    }
    const lists: Table = {
      module: 'Lists',
      revision: '1',
      headerProblems: [],
      columns: [{ key: 'ORGANISM_PART', label: 'ORGANISM_PART', type: 'text' }],
      rows: [
        { id: 'l1', row: 2, version: 1, observed: true, formulas: [], values: { ORGANISM_PART: 'WHOLE_ORGANISM' } },
        { id: 'l2', row: 3, version: 1, observed: true, formulas: [], values: { ORGANISM_PART: 'HEAD' } },
        { id: 'l3', row: 4, version: 1, observed: true, formulas: [], values: { ORGANISM_PART: 'NA' } },
      ],
    }
    const options = buildOptions(table, lists)
    expect(options.Sex).toEqual(['male', 'female', 'NA', 'NOT_COLLECTED'])
    expect(options.Tube_1_tissue).toEqual(['HEAD', 'WHOLE_ORGANISM'])
  })
})
