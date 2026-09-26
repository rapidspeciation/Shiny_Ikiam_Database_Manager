import { isBlank } from './cells'
import type { CellValue, Field, TableRow } from './types'
import { usePending } from '../stores/pending'

/** Puts the given columns first, keeping the sheet order for the rest. */
export function orderColumns(columns: Field[], first: string[]): Field[] {
  const head = first.map(key => columns.find(c => c.key === key)).filter((c): c is Field => !!c)
  return [...head, ...columns.filter(c => !first.includes(c.key))]
}

/** Rows of a sheet matching the given identifiers, in the order given. */
export function rowsById(rows: TableRow[], field: string, ids: string[]): TableRow[] {
  const byId = new Map<string, TableRow>()
  for (const row of rows) {
    const id = row.values[field]
    if (id !== null && id !== undefined && row.observed && !byId.has(String(id))) byId.set(String(id), row)
  }
  return ids.map(id => byId.get(id)).filter((r): r is TableRow => !!r)
}

/**
 * Sets a cell as a pending change unless it already holds a value (or is a
 * formula). Returns true when something changed. Used by the "fill" buttons.
 */
export function fillIfBlank(module: string, row: TableRow, label: string, field: string, value: CellValue, overwrite = false) {
  const pending = usePending()
  if (row.formulas.includes(field)) return false
  if (!overwrite && !isBlank(pending.value(row, field))) return false
  if (pending.value(row, field) === value) return false
  pending.setCell(module, row, label, field, value)
  return true
}
