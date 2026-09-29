import { reactive } from 'vue'
import { api } from './api'
import type { CellValue } from './types'
import { t } from './i18n'

/**
 * The Google Sheet's own checks for one sheet, as the server reads them
 * (server/verifications.mjs): columns whose values must not repeat, and the
 * dropdown lists (strict ones refuse other values when typed in the sheet).
 */
export interface Verifications {
  unique: string[]
  lists: Record<string, { strict: boolean; source: string; values: Set<string> }>
}

const loaded = reactive<Record<string, Verifications>>({})
const loading = new Set<string>()

/** The rules of a sheet: undefined until they arrive (then reactive). */
export function verificationsFor(module: string): Verifications | undefined {
  if (!loaded[module] && !loading.has(module)) {
    loading.add(module)
    api<{ unique: string[]; lists: Record<string, { strict: boolean; source: string; values: string[] }> }>(
      `verifications?module=${encodeURIComponent(module)}`,
    )
      .then(v => {
        loaded[module] = {
          unique: v.unique,
          lists: Object.fromEntries(Object.entries(v.lists).map(([f, l]) => [f, { ...l, values: new Set(l.values) }])),
        }
      })
      .catch(() => {})
      .finally(() => loading.delete(module))
  }
  return loaded[module]
}

export const blankOrNA = (value: CellValue | undefined) =>
  value === null || value === undefined || /^\s*(|NA|N\/A)\s*$/i.test(String(value))

/** Why a value is outside its column's list (as Google's red corner), or null. */
export function listProblem(rules: Verifications | undefined, field: string, value: CellValue | undefined): string | null {
  const list = rules?.lists[field]
  if (!list || value === null || value === undefined) return null
  const text = String(value).trim()
  if (!text || list.values.has(text)) return null
  const vars = { text, source: list.source }
  return list.strict
    ? t('«{text}» no está en la lista de la hoja ({source}); la hoja no lo acepta', vars)
    : t('«{text}» no está en la lista de la hoja ({source})', vars)
}

/** For each column that must not repeat: value → the rows that hold it. */
export function repeats(rules: Verifications | undefined, rows: { row: number | null; values: Record<string, CellValue> }[]) {
  const out = new Map<string, Map<string, (number | null)[]>>()
  for (const field of rules?.unique || []) {
    const seen = new Map<string, (number | null)[]>()
    for (const r of rows) {
      const value = r.values[field]
      // IDs hold a digit; texts such as "not given" or NOT_COLLECTED may repeat.
      if (blankOrNA(value) || !/\d/.test(String(value))) continue
      const key = String(value).trim()
      seen.set(key, [...(seen.get(key) || []), r.row])
    }
    for (const [key, list] of seen) if (list.length < 2) seen.delete(key)
    out.set(field, seen)
  }
  return out
}
