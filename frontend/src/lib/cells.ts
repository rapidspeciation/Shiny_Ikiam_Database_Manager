import { formatSerial, parseDateInput } from './dates'
import type { CellValue, Field } from './types'

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

export type Normalized = { ok: true; value: CellValue } | { ok: false; message: string }

const NA = /^(NA|N\/A)$/i

/**
 * Converts what a person typed or pasted into the value stored in the sheet.
 * Dates become Sheets serial numbers; "NA" is kept as text because the
 * workbook uses it deliberately.
 */
export function normalizeInput(raw: unknown, field: Pick<Field, 'type' | 'key'>): Normalized {
  if (raw === null || raw === undefined) return { ok: true, value: null }
  if (typeof raw === 'number' || typeof raw === 'boolean') return { ok: true, value: raw }
  const text = String(raw).trim()
  if (!text) return { ok: true, value: null }
  if (NA.test(text)) return { ok: true, value: 'NA' }
  if (text.startsWith('=')) return { ok: false, message: 'Las fórmulas solo se editan en Google Sheets' }
  if (field.type === 'date') {
    const serial = parseDateInput(text)
    return serial === null
      ? { ok: false, message: `Fecha no válida en ${field.key}: use 14-Aug-25 o 2025-08-14` }
      : { ok: true, value: serial }
  }
  const time = TIME_FIELD.test(field.key) ? /^(\d{1,2}):(\d{2})$/.exec(text) : null
  if (time && +time[1] < 24 && +time[2] < 60) return { ok: true, value: (+time[1] * 60 + +time[2]) / (24 * 60) }
  if (field.type === 'number' && /^-?\d+(\.\d+)?$/.test(text)) return { ok: true, value: Number(text) }
  return { ok: true, value: text }
}

/** Values that mean "nothing recorded" in the workbook. */
export function isBlank(value: CellValue | undefined): boolean {
  return value === null || value === undefined || /^\s*(|NA|N\/A)\s*$/i.test(String(value))
}
