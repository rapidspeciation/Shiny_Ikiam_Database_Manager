import { api } from './api'
import type { TableRow } from './types'
import type { WireRow } from '../stores/tables'

/**
 * The Buscador (server/search.mjs): a text in every sheet, each sheet with
 * its matches and the rows around one of them; more rows by range.
 */
export interface SearchSheet {
  module: string
  /** Rows with the text in some cell. */
  total: number
  /** Rows where a whole cell is the text; `idExact` where that cell is the sheet's ID column. */
  exact: number
  idExact: number
  /** Columns holding the text, the most matches first. */
  columns: string[]
  /** Row numbers of the matches, in sheet order (at most 1000: `truncated`). */
  matches: number[]
  truncated: boolean
  /** The match the sheet opens at. */
  focus: number
  /** The sheet's first and last rows, where scrolling stops. */
  first: number | null
  last: number | null
  /** Columns missing from the live sheet: last values, read-only. */
  unavailable: string[]
  /** Rows around `focus`; null for sheets further down the list (read when opened). */
  window: { from: number; to: number; rows: WireRow[] } | null
}

export interface SearchReply {
  query: string
  sheets: SearchSheet[]
  took: number
}

interface RangeReply {
  module: string
  from: number
  to: number
  rows: WireRow[]
}

/** Shortest text searched. */
export const MIN_QUERY = 2
/** Rows read before and after a match. */
export const CONTEXT = 12
/** Rows read at a time while scrolling. */
export const STEP = 40

export function searchSheets(q: string, pin: string, signal?: AbortSignal) {
  const query = new URLSearchParams({ q, context: String(CONTEXT), ...(pin ? { pin } : {}) })
  return api<SearchReply>(`search?${query}`, { signal })
}

export function sheetRows(module: string, from: number, to: number) {
  const query = new URLSearchParams({ module, from: String(from), to: String(to) })
  return api<RangeReply>(`search/rows?${query}`)
}

/** Both lists' rows in sheet order; a row in both is taken from `more` (the newer copy). */
export function mergeRows(rows: TableRow[], more: TableRow[]): TableRow[] {
  const byId = new Map(rows.map(r => [r.id, r]))
  for (const r of more) byId.set(r.id, r)
  return [...byId.values()].sort((a, b) => a.row - b.row)
}

/** The match after (or before) row `current`, going round at the ends as Ctrl+F does. */
export function stepMatch(matches: number[], current: number, dir: 1 | -1): number {
  if (!matches.length) return current
  if (dir > 0) return matches.find(r => r > current) ?? matches[0]
  return matches.findLast(r => r < current) ?? matches[matches.length - 1]
}

export interface Loaded {
  from: number
  to: number
}

/**
 * What to read to show row `row` with `context` rows around it: nothing when
 * they are loaded already; the rows between when it is a short way past the
 * loaded ones (they stay one run of rows); otherwise a new run around it.
 */
export function rangeFor(
  loaded: Loaded | null,
  row: number,
  context = CONTEXT,
  step = STEP,
): { from: number; to: number; replace: boolean } | null {
  const from = Math.max(0, row - context)
  const to = row + context
  if (!loaded || to < loaded.from - step || from > loaded.to + step) return { from, to, replace: true }
  if (from < loaded.from) return { from, to: loaded.from - 1, replace: false }
  if (to > loaded.to) return { from: loaded.to + 1, to, replace: false }
  return null
}
