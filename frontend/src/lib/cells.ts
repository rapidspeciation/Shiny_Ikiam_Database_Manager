import { isSumField, simpleSum } from './sums'
import { dayFirst, formatSerial, inDateRange, parseDateInput, serialToIso } from './dates'
import type { CellValue, Field } from './types'
import { t } from './i18n'

const TIME_FIELD = /(^|_)time$/i

/** Text shown in a cell, in filters and in search. */
export function displayValue(value: CellValue | undefined, field?: Pick<Field, 'type' | 'key'>): string {
  if (value === null || value === undefined) return ''
  if (field?.type === 'date' && typeof value === 'number') return formatSerial(value)
  // Times are stored by Sheets as a fraction of a day.
  if (field && TIME_FIELD.test(field.key) && typeof value === 'number' && value >= 0 && value < 1) {
    const minutes = Math.round(value * 24 * 60)
    return `${String(Math.floor(minutes / 60)).padStart(2, '0')}:${String(minutes % 60).padStart(2, '0')}`
  }
  if (typeof value === 'boolean') return value ? 'TRUE' : 'FALSE'
  return String(value)
}

/**
 * A cell's value as its editor opens with it: what a person would type, not
 * what the sheet stores. Dates day first (26/05/2026, read back in any form
 * parseDateInput knows), times 09:05; the rest as shown.
 */
export function editText(value: CellValue | undefined, field?: Pick<Field, 'type' | 'key'>): string {
  if (field?.type === 'date' && typeof value === 'number' && inDateRange(value)) return dayFirst(serialToIso(value))
  return displayValue(value, field)
}

export type Normalized = { ok: true; value: CellValue } | { ok: false; message: string }

const NA = /^(NA|N\/A)$/i

/**
 * Converts what a person typed or pasted into the value stored in the sheet.
 * Dates become Sheets serial numbers; "NA" is kept as text because the
 * workbook uses it deliberately.
 */
export function normalizeInput(raw: unknown, field: Pick<Field, 'type' | 'key'>, module = ''): Normalized {
  if (raw === null || raw === undefined) return { ok: true, value: null }
  if (typeof raw === 'number' || typeof raw === 'boolean') return { ok: true, value: raw }
  const text = String(raw).trim()
  if (!text) return { ok: true, value: null }
  if (NA.test(text)) return { ok: true, value: 'NA' }
  // Counts kept as sums (=12+15) are written as such; other formulas only in Google Sheets.
  const sum = isSumField(module, field.key) ? simpleSum(text) : null
  if (sum) return { ok: true, value: sum }
  if (text.startsWith('=')) return { ok: false, message: t('Las fórmulas solo se editan en Google Sheets') }
  if (field.type === 'date') {
    const serial = parseDateInput(text)
    return serial === null
      ? { ok: false, message: t('Fecha no válida en {field}: escribe 27/9, 27-sep o 27/9/26', { field: field.key }) }
      : { ok: true, value: serial }
  }
  const time = TIME_FIELD.test(field.key) ? /^(\d{1,2}):(\d{2})$/.exec(text) : null
  if (time && +time[1] < 24 && +time[2] < 60) return { ok: true, value: (+time[1] * 60 + +time[2]) / (24 * 60) }
  if (field.type === 'number' && /^-?\d+(\.\d+)?$/.test(text)) return { ok: true, value: Number(text) }
  return { ok: true, value: text }
}

/**
 * Formula cells that take a typed value all the same: the Tube 2 medium. The
 * server writes it only in the rows whose formula would not give it (the
 * formula of the rows before ID H0B has no case for NOT_COLLECTED, none has
 * one for a body's medium); in the others the formula stays (server/batch.mjs
 * TYPED_WHERE_FORMULA_FAILS).
 */
export const typedWhereFormulaFails = (module: string, field: string) => module === 'Insectary_data' && field === 'T2_Preservation_medium'

/** Values that mean "nothing recorded" in the workbook. */
export function isBlank(value: CellValue | undefined): boolean {
  return value === null || value === undefined || /^\s*(|NA|N\/A)\s*$/i.test(String(value))
}
