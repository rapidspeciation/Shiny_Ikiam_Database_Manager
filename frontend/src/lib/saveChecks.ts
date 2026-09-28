import { inDateRange } from './dates'
import { blankOrNA, listProblem, type Verifications } from './verifications'
import type { CellValue, Field, TableRow } from './types'

/**
 * The checks a pending change must pass before it is sent to Google Sheets,
 * the same the server makes (server/batch.mjs, server/schema.mjs): a date
 * with a sensible year, a value from a strict list, an ID nobody else holds.
 * A cell that fails stays pending and red; the other changes are saved.
 */

const TUBE_FIELD = /^Tube_\d_id(?:_LEGS)?$/

export function dateProblem(field: Pick<Field, 'key' | 'type'> | undefined, value: CellValue | undefined): string | null {
  if (field?.type !== 'date' || value === null || value === undefined || value === '') return null
  // "Days difference (…)" columns count days, not dates.
  if (/^days difference/i.test(field.key) || /^\s*(NA|N\/A)\s*$/i.test(String(value))) return null
  if (typeof value === 'number' && inDateRange(value)) return null
  return `Fecha no válida en ${field.key}: usa 14-Aug-25 o 2025-08-14, entre 1990 y 2099`
}

export interface CheckChange {
  module: string
  /** Row id of an edit, or clientId of a new row. */
  id: string
  isNew: boolean
  values: Record<string, CellValue>
}
export interface CheckSheet {
  rows: TableRow[]
  columns: Field[]
  rules?: Verifications
}

type Holder = { id: string; sheet: string; row: number | null; label: string; unsaved: boolean }
const isId = (value: CellValue | undefined) => !blankOrNA(value) && /\d/.test(String(value))
const labelOf = (values: Record<string, CellValue>) =>
  String(values.Insectary_ID ?? values.CAM_ID ?? values.FieldMark_ID ?? '').trim()

function where(value: string, h: Holder) {
  if (h.row === null) return `${value} ya está en una fila nueva sin guardar de ${h.sheet}`
  const label = h.label && h.label !== value ? ` (${h.label})` : ''
  return `${value} ya está usado en ${h.sheet} fila ${h.row}${label}${h.unsaved ? ', sin guardar todavía' : ''}`
}

/**
 * Why each pending cell cannot be saved, keyed "rowId:field" (or
 * "clientId:field" for new rows). Repeats are looked for in the sheets given,
 * with the pending changes applied; tube IDs across all of them.
 */
export function localProblems(changes: CheckChange[], sheets: Record<string, CheckSheet | undefined>): Record<string, string> {
  const out: Record<string, string> = {}
  const pendingOf = new Map(changes.map(c => [c.id, c]))
  const indexes = new Map<string, Map<string, Holder[]>>()
  const hold = (scope: string, value: CellValue, h: Holder) => {
    const index = indexes.get(scope) ?? indexes.set(scope, new Map()).get(scope)!
    const key = String(value).trim()
    index.set(key, [...(index.get(key) || []), h])
  }
  // Built only when a change touches an ID column, and once per sheet.
  const built = new Set<string>()
  const build = (module: string) => {
    if (built.has(module)) return
    built.add(module)
    const sheet = sheets[module]
    if (!sheet?.rules) return
    const fields = sheet.rules.unique
    for (const r of sheet.rows) {
      if (!r.observed && !pendingOf.has(r.id)) continue
      const edit = pendingOf.get(r.id)
      for (const f of fields) {
        const unsaved = !!edit && f in edit.values
        const value = unsaved ? edit!.values[f] : r.values[f]
        if (!isId(value)) continue
        const values = edit ? { ...r.values, ...edit.values } : r.values
        hold(TUBE_FIELD.test(f) ? 'tube' : `${module}:${f}`, value, {
          id: r.id,
          sheet: module,
          row: r.row,
          label: labelOf(values),
          unsaved,
        })
      }
    }
    for (const c of changes)
      if (c.isNew && c.module === module)
        for (const f of fields)
          if (isId(c.values[f]))
            hold(TUBE_FIELD.test(f) ? 'tube' : `${module}:${f}`, c.values[f], {
              id: c.id,
              sheet: module,
              row: null,
              label: '',
              unsaved: true,
            })
  }

  for (const c of changes) {
    const sheet = sheets[c.module]
    if (!sheet) continue
    for (const [field, value] of Object.entries(c.values)) {
      const key = `${c.id}:${field}`
      const date = dateProblem(
        sheet.columns.find(col => col.key === field),
        value,
      )
      if (date) {
        out[key] = date
        continue
      }
      const rules = sheet.rules
      if (!rules) continue
      if (rules.lists[field]?.strict && value !== null && value !== '') {
        const problem = listProblem(rules, field, value)
        if (problem) {
          out[key] = problem
          continue
        }
      }
      if (!rules.unique.includes(field) || !isId(value)) continue
      const tube = TUBE_FIELD.test(field)
      // Tube IDs are unique across the workbook: every loaded sheet counts.
      for (const module of tube ? Object.keys(sheets) : [c.module]) build(module)
      const others = (indexes.get(tube ? 'tube' : `${c.module}:${field}`)?.get(String(value).trim()) || []).filter(
        h => h.id !== c.id,
      )
      if (others.length) out[key] = where(String(value).trim(), others.find(h => !h.unsaved) ?? others[0])
    }
  }
  return out
}
