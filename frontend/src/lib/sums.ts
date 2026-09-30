/**
 * Counts the team types as sums, one term per day or group of eggs (=12+15).
 * The app writes them as such formulas (the server keeps the same list:
 * SUM_FIELDS in server/schema.mjs) and refuses any other formula.
 */
const SUM_FIELDS: Record<string, ReadonlySet<string>> = {
  Insectary_stocks: new Set([
    'NUMBER OF EGGS',
    'NUMBER OF LARVAE',
    'NUMBER OF PUPA',
    'NUMBER OF ADULTS',
    // Typed as =2 or =2+6 in the sheet too.
    'NUMBER OF PUPAE/LARVAE FOR DISECTIONS',
  ]),
}
export const isSumField = (module: string, field: string) => !!SUM_FIELDS[module]?.has(field)

/**
 * "12+15", "= 12 + 15", "27-5" (27 larvae, 5 died) or "=27" as "=12+15" /
 * "=27-5" / "=27"; null when it is not a simple sum (a plain 27 stays a number).
 */
export function simpleSum(text: string): string | null {
  const t = text.trim()
  if (!/^=?\s*\d+(?:\s*[+-]\s*\d+)*$/.test(t) || (!t.startsWith('=') && !/[+-]/.test(t))) return null
  return '=' + t.replace(/^=/, '').replace(/\s+/g, '')
}

/**
 * What a count written as a sum adds up to ("=12+15" → 27, "=27-5" → 22), to
 * show beside it; null for a single number or anything that is not a sum.
 */
export function sumTotal(value: unknown): number | null {
  const sum = typeof value === 'string' ? simpleSum(value) : null
  const terms = sum?.slice(1).match(/[+-]?\d+/g)
  return terms && terms.length > 1 ? terms.reduce((total, term) => total + Number(term), 0) : null
}
