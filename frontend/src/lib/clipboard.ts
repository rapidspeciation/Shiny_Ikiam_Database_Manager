/**
 * What copying from a grid puts on the clipboard: plain text only, the cells
 * as tab-separated rows, so a plain Ctrl+V in Google Sheets or Excel pastes the
 * values without the app's colours, and the app's own grids read it back.
 */
import { displayValue } from './cells'
import { inDateRange, serialToIso } from './dates'
import type { CellValue, Field } from './types'

/**
 * A cell's value as copied: as the grid shows it, except dates, which go as
 * 2026-10-04. Google Sheets (in any language or region) and Excel read that
 * form as a date; the grid's "4-Oct-26" is read as a date only where months are
 * English, and its two-digit year from 30 on as 19xx. Times go as 09:05, which
 * spreadsheets read as times. A formula cell carries the value it shows.
 */
export function copyText(value: CellValue | undefined, field?: Pick<Field, 'type' | 'key'>): string {
  if (field?.type === 'date' && typeof value === 'number' && inDateRange(value)) return serialToIso(value)
  return displayValue(value, field)
}

/**
 * Rows of cells as tab-separated lines. A cell with a tab or a new line, or one
 * that starts with a quote, goes between quotes (quotes inside doubled), as
 * Google Sheets and Excel copy such a cell, so it stays one cell when pasted.
 */
export function toTsv(rows: string[][]): string {
  return rows.map(row => row.map(quoted).join('\t')).join('\n')
}

const quoted = (text: string) => (/[\t\n\r]|^"/.test(text) ? `"${text.replace(/"/g, '""')}"` : text)
