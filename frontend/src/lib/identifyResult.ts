/**
 * What the Identificar tab says about a result (views/IdentifyView.vue): the headline, the candidates grouped by
 * genus, and the flash-frozen gap against the reference-genome target. Pure functions, tested on their own.
 *
 * The scores are the head's probabilities, not calibrated on people's photos: the tab never turns them into words
 * like «muy probable». It only names the genus instead of the species when the species is unclear and the genus is
 * not (CLEAR, a cut not yet checked against held-out photos: DESIGN.md, «Not in this version»).
 */
import type { StockCount } from './frozenStock'

export const CLEAR = 0.8

export interface Candidate {
  name: string
  p: number
}
export interface GenusGroup {
  genus: string
  p: number
  species: Candidate[]
}

/** The species in score order (the first `top`) and every genus's total, from all species' scores. */
export function rank(scores: ArrayLike<number>, species: string[], top = 5): { species: Candidate[]; genus: Map<string, number> } {
  const genus = new Map<string, number>()
  for (let i = 0; i < species.length; i++) {
    const g = species[i].split(' ')[0]
    genus.set(g, (genus.get(g) ?? 0) + scores[i])
  }
  const order = Array.from({ length: species.length }, (_, i) => i).sort((a, b) => scores[b] - scores[a] || a - b)
  return { species: order.slice(0, top).map(i => ({ name: species[i], p: scores[i] })), genus }
}

/** The candidates under their genus (the genus's total over all its species), genera in score order. */
export function groupByGenus(candidates: Candidate[], genus: Map<string, number>): { groups: GenusGroup[]; rest: number } {
  const groups: GenusGroup[] = []
  for (const c of candidates) {
    const g = c.name.split(' ')[0]
    let group = groups.find(x => x.genus === g)
    if (!group) groups.push((group = { genus: g, p: genus.get(g) ?? c.p, species: [] }))
    group.species.push(c)
  }
  groups.sort((a, b) => b.p - a.p)
  const shown = groups.reduce((a, g) => a + g.p, 0)
  return { groups, rest: Math.max(0, 1 - shown) }
}

export type Headline = { kind: 'species'; name: string } | { kind: 'genus'; genus: string } | { kind: 'unsure' }

/** The species when its score is clear, else its genus when that is, else «not sure». */
export function headline(top: Candidate | undefined, genus: Map<string, number>): Headline {
  if (!top) return { kind: 'unsure' }
  if (top.p >= CLEAR) return { kind: 'species', name: top.name }
  const g = top.name.split(' ')[0]
  if ((genus.get(g) ?? 0) >= CLEAR) return { kind: 'genus', genus: g }
  return { kind: 'unsure' }
}

/** «78 %», «<1 %». */
export const percent = (p: number) => (p < 0.005 ? '<1 %' : `${Math.round(p * 100)} %`)

/** true / false when the workbook's Ithomiini genera are known (the team's snapshot), else null. */
export function isIthomiini(name: string, genera: string[] | undefined | null): boolean | null {
  if (!genera?.length) return null
  return genera.includes(name.split(' ')[0])
}

// ---------------------------------------------------------------- the reference-genome target

/** 3 males and 3 females per subspecies (or per species without subspecies) on each side of the Andes. */
export const TARGET = 3
export type AndesSide = 'East' | 'West'
export const SIDES: AndesSide[] = ['East', 'West']
type Cell = Omit<StockCount, 'total'>
export type StockDetail = Record<string, Partial<Record<AndesSide | 'other', Cell>>>

export interface GapRow {
  /** '' for the species itself (no subspecies written). */
  subspecies: string
  side: AndesSide
  M: number
  F: number
  unknown: number
  missingM: number
  missingF: number
}

export interface StockGaps {
  /** Rows set against the target: per subspecies when the inventory names any, else per species. */
  rows: GapRow[]
  /** Specimens without a side (Center, NA, empty) or, with subspecies, without one written: counted, no target. */
  apart: { label: 'sin-lado' | 'sin-subespecie'; side?: AndesSide; M: number; F: number; unknown: number }[]
  /**
   * The sides still short: the total, each short row (its subspecies, '' for the species) and how many specimens of
   * that side have no subspecies written (they may belong to a short one).
   */
  missing: { side: AndesSide; M: number; F: number; short: { subspecies: string; M: number; F: number }[]; unassigned: number }[]
  bySubspecies: boolean
}

const zero = (): Cell => ({ M: 0, F: 0, unknown: 0 })
const plus = (a: Cell, b: Cell | undefined): Cell => ({ M: a.M + (b?.M ?? 0), F: a.F + (b?.F ?? 0), unknown: a.unknown + (b?.unknown ?? 0) })

/**
 * The gap of a species: `detail` is its frozen-stock detail (subspecies -> side -> counts), `recorded` the sides
 * where the head records it in Ecuador. A side is shown when it is recorded there or holds stock. The model does
 * not identify subspecies: every subspecies in the inventory gets its own rows, none is picked.
 */
export function stockGaps(detail: StockDetail | undefined, recorded: { East: boolean; West: boolean } | undefined): StockGaps {
  const d = detail ?? {}
  const named = Object.keys(d).filter(s => s)
  const bySubspecies = named.length > 0
  const rows: GapRow[] = []
  const apart: StockGaps['apart'] = []
  const row = (subspecies: string, side: AndesSide, c: Cell): GapRow => ({
    subspecies,
    side,
    ...c,
    missingM: Math.max(0, TARGET - c.M),
    missingF: Math.max(0, TARGET - c.F),
  })
  const sideHas = (side: AndesSide) => !!recorded?.[side] || Object.values(d).some(s => s[side])
  if (bySubspecies) {
    for (const ssp of named.sort())
      for (const side of SIDES) if (d[ssp][side]) rows.push(row(ssp, side, plus(zero(), d[ssp][side])))
    // A recorded side without any stock of any subspecies: one row for the species, all missing.
    for (const side of SIDES) if (recorded?.[side] && !rows.some(r => r.side === side)) rows.push(row('', side, zero()))
    for (const side of SIDES) {
      const c = d['']?.[side]
      if (c && c.M + c.F + c.unknown) apart.push({ label: 'sin-subespecie', side, ...plus(zero(), c) })
    }
  } else {
    for (const side of SIDES) if (sideHas(side)) rows.push(row('', side, plus(zero(), d['']?.[side])))
  }
  const other = Object.values(d).reduce((a, s) => plus(a, s.other), zero())
  if (other.M + other.F + other.unknown) apart.push({ label: 'sin-lado', ...other })
  const missing = SIDES.map(side => {
    const short = rows.filter(r => r.side === side && r.missingM + r.missingF > 0)
    const u = apart.find(a => a.label === 'sin-subespecie' && a.side === side)
    return {
      side,
      M: short.reduce((a, r) => a + r.missingM, 0),
      F: short.reduce((a, r) => a + r.missingF, 0),
      short: short.map(r => ({ subspecies: r.subspecies, M: r.missingM, F: r.missingF })),
      // Only males and females can fill a gap.
      unassigned: u ? u.M + u.F : 0,
    }
  }).filter(m => m.short.length > 0)
  return { rows, apart, missing, bySubspecies }
}
