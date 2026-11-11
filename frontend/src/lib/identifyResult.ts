/**
 * What the Identificar tab says about a result (views/IdentifyView.vue): the headline, the candidates grouped by
 * genus, and the flash-frozen gap against the reference-genome target. Pure functions, tested on their own.
 *
 * The scores are the head's probabilities, not calibrated on people's photos: the tab never turns them into words
 * like «muy probable». It only names the genus instead of the species when the species is unclear and the genus is
 * not (CLEAR, a cut not yet checked against held-out photos: DESIGN.md, «Not in this version»); with the flag
 * TRIBE_RUNG, then the tribe and the subfamily.
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

export type Headline =
  | { kind: 'species'; name: string }
  | { kind: 'genus'; genus: string }
  | { kind: 'tribe' | 'subfamily'; name: string; p: number }
  | { kind: 'unsure' }

/**
 * The tribe and subfamily answer (WingsClassificator X-A8, S1-confirmed with the same CLEAR): on (Franz, 11 Nov 2026:
 * deploy it to test in production), with a line saying it is there for testing. A page built with VITE_TRIBE_RUNG=0
 * keeps the old ladder: species, genus, «not sure».
 */
export const TRIBE_RUNG = import.meta.env.VITE_TRIBE_RUNG !== '0'

/** Every tribe's and subfamily's total: the sum of its species' scores (all of them, not only the listed ones). */
export interface Higher {
  tribe: Map<string, number>
  subfamily: Map<string, number>
}

/**
 * A name the page may show as a tribe or subfamily: not a placeholder of the head's taxonomy (UNKNOWN_tribe_<genus>,
 * UNKNOWN_subfamily_<genus>, any UNKNOWN_*) and not «incertae sedis». A species under such a name has no tribe (or
 * subfamily) for that rung: it still counts for the other one.
 */
export const shownTaxon = (name: string | undefined | null) => !!name && !name.startsWith('UNKNOWN') && name !== 'incertae sedis'

function best(totals: Map<string, number> | undefined): { name: string; p: number } | null {
  let out: { name: string; p: number } | null = null
  for (const [name, p] of totals ?? []) if (shownTaxon(name) && (!out || p > out.p)) out = { name, p }
  return out
}

/**
 * The species when its score is clear, else its genus when that is, else (with the flag `on` and the totals
 * `higher`) the tribe, then the subfamily, whose total is clear, else «not sure». Used for both the server's answer
 * and the phone's.
 */
export function headline(top: Candidate | undefined, genus: Map<string, number>, higher: Higher | null = null, on = TRIBE_RUNG): Headline {
  if (!top) return { kind: 'unsure' }
  if (top.p >= CLEAR) return { kind: 'species', name: top.name }
  const g = top.name.split(' ')[0]
  if ((genus.get(g) ?? 0) >= CLEAR) return { kind: 'genus', genus: g }
  if (on && higher) {
    for (const kind of ['tribe', 'subfamily'] as const) {
      const b = best(higher[kind])
      if (b && b.p >= CLEAR) return { kind, ...b }
    }
  }
  return { kind: 'unsure' }
}

/**
 * The head's tribe and subfamily per species (tools/identifier/taxonomy_6fa6ab78.json, from the head's own taxonomy by
 * tools/identifier/export_taxonomy.py; the server identifier reads the same file): indexes into `tribes` and
 * `subfamilies`, -1 for none (placeholders are already left out); species not listed have neither.
 */
export interface Taxonomy {
  head: string
  tribes: string[]
  subfamilies: string[]
  species: Record<string, [number, number]>
}

/** The phone's totals: every species' score (`scores`, in the order of `species`) summed by its tribe and subfamily. */
export function higherRanks(scores: ArrayLike<number>, species: string[], tax: Taxonomy): Higher {
  const out: Higher = { tribe: new Map(), subfamily: new Map() }
  for (let i = 0; i < species.length; i++) {
    const pair = tax.species[species[i]]
    if (!pair) continue
    const [t, s] = pair
    const tn = t >= 0 ? tax.tribes[t] : undefined
    const sn = s >= 0 ? tax.subfamilies[s] : undefined
    if (shownTaxon(tn)) out.tribe.set(tn!, (out.tribe.get(tn!) ?? 0) + scores[i])
    if (shownTaxon(sn)) out.subfamily.set(sn!, (out.subfamily.get(sn!) ?? 0) + scores[i])
  }
  return out
}

/**
 * The subspecies named under the headline (views/IdentifyView.vue): the most probable subspecies leaf of the top
 * species (worker.py's subspecies scores, after the side's prior), `ssp` most probable first, every subspecies leaf
 * the head has for that species. None when the head names no subspecies (schema 1), when only the genus is clear
 * (no species to hang it on), or when the subspecies carries little of the species' score (the species' own leaf,
 * «subspecies not given», holds the rest: naming one would overclaim). `kind` 'if' when the species itself is not
 * clear: the line then says «if it is this species». `alt`: the second when it is close (at least half the first).
 * `only`: the head knows just this one subspecies of the species, so naming it says little.
 */
export const SSP_MIN = 0.005
export const SSP_SHARE = 0.2
export interface SubspeciesHint {
  kind: 'species' | 'if'
  name: string
  p: number
  alt?: Candidate
  only: boolean
}
export function subspeciesHint(said: Headline | null, top: Candidate | undefined, ssp: Candidate[] | undefined): SubspeciesHint | null {
  if (!said || said.kind === 'genus' || said.kind === 'tribe' || said.kind === 'subfamily' || !top || !ssp?.length) return null
  const [first, second] = ssp
  if (first.p < SSP_MIN || first.p < SSP_SHARE * top.p) return null
  const alt = second && second.p >= SSP_MIN && second.p >= first.p / 2 ? second : undefined
  return { kind: said.kind === 'species' ? 'species' : 'if', name: first.name, p: first.p, alt, only: ssp.length === 1 }
}

/** «Mechanitis messenoides deceptus» -> «M. messenoides deceptus». */
export const shortName = (name: string) => {
  const [g, ...rest] = name.split(' ')
  return rest.length ? `${g[0]}. ${rest.join(' ')}` : name
}

/** The subspecies' own word(s), lower case as the inventory writes them (server/frozen-stock.mjs subspeciesOf). */
export const epithet = (name: string) => (name.split(' ')[2] ?? '').toLowerCase()

/** «78 %», «<1 %». */
export const percent = (p: number) => (p < 0.005 ? '<1 %' : `${Math.round(p * 100)} %`)

/** true / false when a list of Ithomiini genera is given (lib/ithomiiniGenera.ts), else null. */
export function isIthomiini(name: string, genera: readonly string[] | undefined | null): boolean | null {
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
 * where the head records it in Ecuador. A side is shown when it is recorded there or holds stock. Every subspecies
 * in the inventory gets its own rows (the panel marks the one the AI finds closest, StockPanel `focus`).
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
