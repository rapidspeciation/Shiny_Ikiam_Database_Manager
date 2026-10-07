import { nextId } from './emerged'

/**
 * Emergidos' «Siguiente ID»: which run of free pre-made Insectary IDs the
 * buttons (+♀, +♂, Sin sexo, Varios…, + Larvas…) take their IDs from. By
 * default the one after the last row used (the buttons as always); a colleague
 * who wrote IDs on paper while offline chooses that older gap and records them
 * in order, while the others go on with the latest. A gap is kept by its rows,
 * so it stays chosen while its IDs are used (S3E taken: S4E–T9E). IDs held by
 * someone else's cards or entries are never in `order` (the free IDs and this
 * person's own cards), so they are skipped. Nothing here touches the network.
 */

/** A run of consecutive free pre-made rows, as the server answers it (server/grid.mjs insectaryGaps). */
export interface IdGap {
  from: string | null
  to: string | null
  rowFrom: number
  rowTo: number
  /** Free IDs nobody holds. */
  free: number
  /** IDs held by cards or entries not in the sheet yet (anyone's). */
  held: number
  /** The gap after the last row used: what the buttons take by default. */
  latest: boolean
}

/** The gap chosen: its rows (kept for the session, per person). Null: the latest one. */
export interface GapChoice {
  rowFrom: number
  rowTo: number
}

/** One line of the selector: the IDs this person can take from a gap now. */
export interface GapOption {
  rowFrom: number
  rowTo: number
  latest: boolean
  /** Its IDs nobody holds and no card of this person has, in sheet order. */
  ids: string[]
}

const norm = (id: string) => id.trim().toUpperCase()

/** The IDs of `order` (free and this person's cards', in sheet order) within the rows of a gap. */
export function gapIds(order: string[], rowOf: Map<string, number>, gap: GapChoice): string[] {
  return order.filter(id => {
    const row = rowOf.get(norm(id))
    return row !== undefined && row >= gap.rowFrom && row <= gap.rowTo
  })
}

/**
 * The ID the next card gets: with no gap chosen, as always (lib/emerged
 * nextId: after the cards' last ID, else the first free one after the last
 * row used); in a chosen gap, after the cards' last ID in it, else from its
 * start, never one a card holds (an ID skipped there, a card taken away: the
 * first one left). Null when the gap has none left.
 */
export function nextFor(order: string[], first: string, held: string[], gap: GapChoice | null, rowOf: Map<string, number>): string | null {
  if (!gap) return nextId(order, first, held)
  const ids = gapIds(order, rowOf, gap)
  const taken = new Set(held.map(norm))
  return nextId(ids, ids[0] ?? '', held) ?? ids.find(id => !taken.has(norm(id))) ?? null
}

/** `count` IDs for the next cards («Varios…», «+ N larvas»), one after another as the taps would give them. */
export function nextMany(order: string[], first: string, held: string[], gap: GapChoice | null, rowOf: Map<string, number>, count: number): string[] {
  const out: string[] = []
  for (let i = 0; i < count; i++) {
    const id = nextFor(order, first, [...held, ...out], gap, rowOf)
    if (!id) break
    out.push(id)
  }
  return out
}

/**
 * The selector's lines, newest gap first: each with the IDs this person can
 * take from it now (`mine`: their cards' IDs, already taken). A gap with none
 * is left out, except the latest and the one chosen (it says it is full).
 */
export function gapOptions(gaps: IdGap[], order: string[], rowOf: Map<string, number>, mine: string[], chosen: GapChoice | null): GapOption[] {
  const taken = new Set(mine.map(norm))
  const out: GapOption[] = []
  for (const g of [...gaps].sort((a, b) => b.rowFrom - a.rowFrom)) {
    const ids = gapIds(order, rowOf, g).filter(id => !taken.has(norm(id)))
    if (ids.length || g.latest || (chosen && sameGap(g, chosen))) out.push({ rowFrom: g.rowFrom, rowTo: g.rowTo, latest: g.latest, ids })
  }
  // A chosen gap whose rows all went (used by others, saved): still listed, full.
  if (chosen && !out.some(o => sameGap(o, chosen))) out.push({ ...chosen, latest: false, ids: [] })
  return out
}

/** Whether a gap (as answered now) is the one chosen: their rows overlap (a run split or shrunk by IDs used stays chosen). */
export function sameGap(a: GapChoice, b: GapChoice): boolean {
  return a.rowFrom <= b.rowTo && b.rowFrom <= a.rowTo
}

/** The choice kept: null for the latest gap (the buttons as always). */
export function choose(option: GapOption | null): GapChoice | null {
  return option && !option.latest ? { rowFrom: option.rowFrom, rowTo: option.rowTo } : null
}

/** «S3E–T9E», «S3E»: a gap's first and last IDs free now. */
export function gapSpan(ids: string[]): string {
  return ids.length > 1 ? `${ids[0]}–${ids.at(-1)}` : (ids[0] ?? '')
}
