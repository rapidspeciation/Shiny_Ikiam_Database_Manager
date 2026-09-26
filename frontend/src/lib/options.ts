import type { Table } from './types'

/** Columns whose choices come from a column of the Lists sheet. */
const LIST_SOURCES: [RegExp, string][] = [
  [/^SPECIES$/, 'Insectary_species'],
  [/^(Research_purpose|Purpose)$/, 'Research_purpose'],
  [/^Location_body$/, 'Tissue locations'],
  [/^Tube_\d+_tissue$/, 'ORGANISM_PART'],
  [/^(Collector|Identifier|Collectors_initials)$/, 'Abbr_name'],
  [/^Country$/, 'Country'],
  [/^CAM_ID_insectary$/, 'InsectaryWild&Reared_CAMid'],
]

/** Fixed choices carried over from the original Shiny app. */
const FIXED: Record<string, string[]> = {
  Sex: ['male', 'female', 'NA'],
  Preserved_Dead_Alive: ['Dead', 'Alive', 'NA'],
  Preserved_dead_alive: ['Dead', 'Alive', 'NA'],
  Stock_of_origin: ['NA', 'deceptus', 'messenoides', 'intermedia'],
  Release_Collect: ['Collected_Sent2Insectary', 'Collected_Preserved'],
}

/** Identifiers, links and notes are free text even when few values are used yet. */
const FREE_TEXT = /(^|[_ ])id$|CAM_ID|Tube_\d_id|Photo|Notes|URL|Link|Path|Specimen ID|ToLID|SAMPLE_ID|EMAIL/i

/** Above this many distinct values a column is treated as free text. */
const MAX_CHOICES = 150

export function listColumn(lists: Table | undefined, column: string): string[] {
  if (!lists) return []
  const out: string[] = []
  for (const row of lists.rows) {
    const value = row.values[column]
    const text = value === null || value === undefined ? '' : String(value).trim()
    if (text && !/^(NA|N\/A)$/i.test(text)) out.push(text)
  }
  return [...new Set(out)]
}

/**
 * Dropdown suggestions for every text column of a sheet. Choices are the
 * canonical Lists values plus values already used in the column, most used
 * first. Typing a value that is not in the list is still allowed.
 */
export function buildOptions(table: Table, lists?: Table, extra: Record<string, string[]> = {}): Record<string, string[]> {
  const options: Record<string, string[]> = {}
  for (const field of table.columns) {
    if (field.type !== 'text') continue
    const counts = new Map<string, number>()
    for (const row of table.rows) {
      if (!row.observed || row.formulas.includes(field.key)) continue
      const value = row.values[field.key]
      if (value === null || value === undefined) continue
      const text = String(value).trim()
      if (text && text.length <= 80) counts.set(text, (counts.get(text) || 0) + 1)
    }
    const listName = LIST_SOURCES.find(([pattern]) => pattern.test(field.key))?.[1]
    const canonical = [...(FIXED[field.key] || []), ...listColumn(lists, listName || ''), ...(extra[field.key] || [])]
    if (!canonical.length && (counts.size > MAX_CHOICES || FREE_TEXT.test(field.key))) continue
    const used = [...counts.entries()].sort((a, b) => b[1] - a[1]).map(([value]) => value)
    // Explicit choices (e.g. clutches, newest first) keep their order; otherwise
    // the most used values come first, as in the original app.
    const merged = extra[field.key]
      ? [...new Set([...canonical, ...used.slice(0, MAX_CHOICES)])]
      : [...new Set([...used.filter(v => canonical.includes(v)), ...canonical, ...used.slice(0, MAX_CHOICES)])]
    if (merged.length) options[field.key] = merged
  }
  return options
}
