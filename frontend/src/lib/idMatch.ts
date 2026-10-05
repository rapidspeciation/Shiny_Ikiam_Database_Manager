/**
 * Finding a butterfly by the Insectary ID written on its wing (A1B, O8E,
 * W2B.1), when the wing is worn and a character is hard to read: what is typed
 * is a pattern, one character per position, where
 *   `*` or `?`        = this position could not be read (any character);
 *   `[BD]` or `[B/D]` = one of these;
 * and any other character may also be its look-alike (6 for 8, D for B…), at
 * most two of them. Shared by Censo and Muertes (and the table's ID picker).
 *
 * Ranking: the exact ID, then IDs the wildcards or alternatives fit, then IDs
 * that start with it (A1 → A1B, W2B → W2B.1), then one look-alike, then two.
 * Within each, butterflies alive first, then the species and the sex given (a
 * census's species, the sex seen on the butterfly), then the newest rows. A
 * butterfly recorded dead ranks lower still: below a living one whose ID starts
 * with what was typed, above any look-alike.
 */

/**
 * Characters people confuse on a worn wing or a paper label: every pair within
 * a group looks alike. Add a group (or a pair) here; the app also learns the
 * pairs the team corrected in Insectary IDs (GET /api/census lookAlikes).
 */
export const LOOK_ALIKE_GROUPS: string[][] = [
  ['6', '8'],
  ['3', '8'],
  ['4', '9'],
  ['2', '1'],
  ['8', '9'],
  ['1', '7'],
  ['0', '8'],
  ['5', '6'],
  ['B', 'D', '8'],
  ['O', '0', 'Q', 'D'],
  ['I', '1', 'L'],
  ['S', '5'],
  ['Z', '2'],
  ['E', 'F'],
  ['C', 'G'],
  ['P', 'R'],
  ['U', 'V'],
  ['M', 'N'],
]

/** Character → the characters it may have been read as. */
export type LookAlikeTable = Map<string, Set<string>>

/** The table from the groups and any extra pairs (learned from the history of corrections). */
export function lookAlikeTable(extra: [string, string][] = [], groups: string[][] = LOOK_ALIKE_GROUPS): LookAlikeTable {
  const table: LookAlikeTable = new Map()
  const pair = (a: string, b: string) => {
    if (!a || !b || a === b) return
    ;(table.get(a) ?? table.set(a, new Set()).get(a)!).add(b)
    ;(table.get(b) ?? table.set(b, new Set()).get(b)!).add(a)
  }
  for (const group of groups) for (const a of group) for (const b of group) pair(a.toUpperCase(), b.toUpperCase())
  for (const [a, b] of extra) pair(a.toUpperCase(), b.toUpperCase())
  return table
}
export const DEFAULT_LOOK_ALIKES = lookAlikeTable()

/** One position of what was typed: any character, or one of these. */
export type Slot = { any: true } | { any: false; chars: string[] }
export interface Pattern {
  slots: Slot[]
  /** No wildcard and no alternatives: plain text. */
  literal: boolean
  /** Positions with a character (or alternatives) given. */
  given: number
}

/** "a1 b", "A?B", "A[1/7]B" → its positions (upper case, spaces gone). */
export function parsePattern(text: string): Pattern {
  const s = text.toUpperCase().replace(/\s+/g, '')
  const slots: Slot[] = []
  let literal = true
  for (let i = 0; i < s.length; i++) {
    const c = s[i]
    if (c === '*' || c === '?') {
      slots.push({ any: true })
      literal = false
    } else if (c === '[') {
      const end = s.indexOf(']', i + 1)
      const inside = s.slice(i + 1, end < 0 ? s.length : end)
      const chars = [...new Set([...inside].filter(ch => !'/,|'.includes(ch)))]
      slots.push(chars.length ? { any: false, chars } : { any: true })
      literal = false
      i = end < 0 ? s.length : end
    } else slots.push({ any: false, chars: [c] })
  }
  return { slots, literal, given: slots.filter(x => !x.any).length }
}
/** Whether a text has a wildcard or alternatives (a pattern, not just an ID). */
export const isPattern = (text: string) => /[*?[]/.test(text)

/** How an ID fits what was typed: look-alikes used (and where), and whether the ID goes on after it. */
export interface Fit {
  subs: number
  /** Positions read as a look-alike (to highlight). */
  at: number[]
  /** The ID is longer than what was typed (A1 → A1B). */
  prefix: boolean
}
export function fitOf(key: string, p: Pattern, table: LookAlikeTable, maxSubs: number): Fit | null {
  const n = p.slots.length
  if (!n || key.length < n) return null
  const at: number[] = []
  for (let i = 0; i < n; i++) {
    const slot = p.slots[i]
    if (slot.any) continue
    const c = key[i]
    if (slot.chars.includes(c)) continue
    if (at.length < maxSubs && slot.chars.some(ch => table.get(ch)?.has(c))) {
      at.push(i)
      continue
    }
    return null
  }
  return { subs: at.length, at, prefix: key.length > n }
}

export type MatchKind = 'exact' | 'pattern' | 'prefix' | 'lookalike'
/** The match's step in the ranking: 0 exact, 1 wildcards/alternatives, 2 starts with it, 3–4 one look-alike, 5–6 two. */
export function tierOf(fit: Fit, p: Pattern): number {
  if (!fit.subs) return fit.prefix ? 2 : p.literal ? 0 : 1
  return 1 + fit.subs * 2 + (fit.prefix ? 1 : 0)
}
export const kindOf = (fit: Fit, p: Pattern): MatchKind =>
  fit.subs ? 'lookalike' : fit.prefix ? 'prefix' : p.literal ? 'exact' : 'pattern'

/** Something with an ID to match: its search key (upper case, no spaces) and its order (newer rows higher). */
export interface Matchable {
  key: string
  order: number
}
export interface IdMatch<T extends Matchable> {
  item: T
  kind: MatchKind
  tier: number
  /** Positions read as a look-alike. */
  at: number[]
  alive: boolean
  /** Of the species given (true when none was given). */
  sameSpecies: boolean
  /** Of the sex given (true when none was given). */
  sameSex: boolean
}

export type SexFilter = '' | 'female' | 'male' | 'unknown'
/** The sex as the filter names it: female, male, or unknown (NA, empty, a doubt). */
export function sexOf(value: unknown): Exclude<SexFilter, ''> {
  const s = String(value ?? '').trim().toLowerCase()
  return s === 'female' ? 'female' : s === 'male' ? 'male' : 'unknown'
}
const speciesKey = (v: unknown) => String(v ?? '').trim().replace(/\s+/g, ' ').toLowerCase()

export interface MatchOptions<T> {
  /** Alive now (asked only of the matches). */
  alive?: (item: T) => boolean
  /** The item's species and sex, to rank by the species and sex given. */
  speciesOf?: (item: T) => unknown
  sexOf?: (item: T) => unknown
  species?: string
  sex?: SexFilter
  /** Items left out (already chosen). */
  skip?: (item: T) => boolean
  table?: LookAlikeTable
  limit?: number
}

/** A dead butterfly drops this many steps: below a living one whose ID starts with the text, above any look-alike. */
const DEAD_STEP = 2.5

/** The items whose ID fits what was typed, best first (see the top of this file). */
export function matchIds<T extends Matchable>(items: T[], typed: string, opts: MatchOptions<T> = {}): IdMatch<T>[] {
  const p = parsePattern(typed)
  if (!p.slots.length || !p.given) return []
  const table = opts.table ?? DEFAULT_LOOK_ALIKES
  // Look-alikes need enough of the ID: two of two given positions, three for an ID still being typed.
  const maxSubs = p.given >= 3 ? 2 : p.given === 2 ? 1 : 0
  const species = opts.species ? speciesKey(opts.species) : ''
  const sex = opts.sex ?? ''
  const out: (IdMatch<T> & { score: number })[] = []
  for (const item of items) {
    const fit = fitOf(item.key, p, table, maxSubs)
    if (!fit) continue
    // A look-alike in an ID still being typed (A7 for A1…) is noise: only for whole IDs, or from three positions.
    if (fit.subs && fit.prefix && p.slots.length < 3) continue
    if (opts.skip?.(item)) continue
    const alive = opts.alive ? opts.alive(item) : true
    const tier = tierOf(fit, p)
    out.push({
      item,
      kind: kindOf(fit, p),
      tier,
      at: fit.at,
      alive,
      sameSpecies: !species || speciesKey(opts.speciesOf?.(item)) === species,
      sameSex: !sex || sexOf(opts.sexOf?.(item)) === sex,
      score: tier + (alive ? 0 : DEAD_STEP),
    })
  }
  out.sort(
    (a, b) =>
      a.score - b.score ||
      Number(b.sameSpecies) - Number(a.sameSpecies) ||
      Number(b.sameSex) - Number(a.sameSex) ||
      b.item.order - a.item.order,
  )
  return out.slice(0, opts.limit ?? 8).map(({ score: _score, ...m }) => m)
}

/** Plain IDs (the table's ID picker) as items to match: newest first in `ids` → higher order. */
export function idItems(ids: string[]): (Matchable & { id: string })[] {
  return ids.map((id, i) => ({ id, key: id.toUpperCase().replace(/\s+/g, ''), order: ids.length - i }))
}
