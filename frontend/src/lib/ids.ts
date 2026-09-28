/**
 * Reading lists of Insectary IDs typed or pasted into the ID picker: single
 * IDs, and ranges such as "B0D-B9D" or "E9D – F8D", which run in the sheet's
 * pre-made row order (N9D is followed by O0D).
 */

/** The IDs and ranges in a text; a range keeps its dash ("B0D-B9D") as one token. */
export function idTokens(text: string): string[] {
  return text
    .replace(/\s*[-–—]\s*/g, '-')
    .split(/[\s,;]+/)
    .filter(Boolean)
}

/**
 * The IDs the tokens stand for, matched without regard to capitals: `found`
 * in the order given (a range expanded in `order`, the sheet's row order), and
 * `missing` for tokens that match nothing.
 */
export function resolveIds(tokens: string[], order: string[]): { found: string[]; missing: string[] } {
  const at = new Map<string, number>()
  order.forEach((id, i) => {
    const key = id.toUpperCase()
    if (!at.has(key)) at.set(key, i)
  })
  const found: string[] = []
  const missing: string[] = []
  for (const token of tokens) {
    const single = at.get(token.toUpperCase())
    if (single !== undefined) {
      found.push(order[single])
      continue
    }
    const range = /^(.+?)-(.+)$/.exec(token)
    const from = range ? at.get(range[1].toUpperCase()) : undefined
    const to = range ? at.get(range[2].toUpperCase()) : undefined
    if (from === undefined || to === undefined) {
      missing.push(token)
      continue
    }
    found.push(...order.slice(Math.min(from, to), Math.max(from, to) + 1))
  }
  return { found: [...new Set(found)], missing }
}
