/**
 * What the Identificar tab says about a result (views/IdentifyView.vue): the headline, the candidates grouped by
 * genus, and the flash-frozen gap against the reference-genome target. Pure functions, tested on their own.
 *
 * The scores are the head's probabilities, not calibrated on people's photos: the tab never turns them into words
 * like «muy probable». It always names its guess, the top species and its genus, as the Wings Gallery does
 * (Franz, 15 Nov 2026), each with its score and marked «Incierto» below the engine's cut (THRESHOLDS); `headline` is
 * the most specific level that does clear its cut (species, genus, tribe, subfamily, or none), shown as the certain one.
 */
import type { StockCount } from './frozenStock'

/** Which model read the photo: the phone's small model, or BioCLIP (the server, and the phone's «Más preciso»). */
export type IdEngine = 'small' | 'bioclip'
export interface Cuts {
  species: number
  genus: number
  /** The tribe and the subfamily (WingsClassificator X-A8, S1-confirmed at 0.80). */
  higher: number
  /** Genus not clear: two genera are named when the top two genera's totals sum to at least this (`genusPair`). */
  pair: number
  /** ... and the second genus's total is at least this; below it, the top species and its genus alone (as before). */
  second: number
}
/**
 * The cuts per engine: below them the species (genus) is shown as «Incierto», and the tribe or subfamily is not
 * shown as certain. Measured 16 Nov 2026 on 2,872 photos no model trained on (.work/identify-answers-20261115/
 * measure_20261116T0331Z/REPORT.md, PREREG.md D5): a species is right about 9 times in 10 from a score of 0.70 for
 * both engines (0.60 to 0.70: 79 to 88 %; 0.50 to 0.60: 60 to 87 %), a genus 97 % from 0.80 (0.50 to 0.80: 86 to 94 %). A new value: change this
 * one line and the test that names the values.
 * `pair` and `second` (17 Nov 2026, same photos, .work/two-genera-20261117/RESULT.md): the summed share 0.70 was
 * chosen for the small model on one half and checked once on the other; BioCLIP takes the same 0.70 (the lead's
 * stricter choice after the numbers: its preregistered 0.20 never bound a shown pair, and its pairs summing under 0.70
 * held the truth only 79 % of the time). `second`: BioCLIP 0.15 (chosen and checked), small 0.02 (the lead's display
 * choice: no «o Y 1 %»). Where two genera are named, the true genus is one of them on 92 % (small) and 98 %
 * (BioCLIP) of the checking half's photos (not a fresh check: the 0.70 and 0.02 were set after both halves were read).
 */
// prettier-ignore
export const THRESHOLDS: Record<IdEngine, Cuts> = { small: { species: 0.7, genus: 0.8, higher: 0.8, pair: 0.7, second: 0.02 }, bioclip: { species: 0.7, genus: 0.8, higher: 0.8, pair: 0.7, second: 0.15 } }

/** The engine of an answer: the server's and the phone's «Más preciso» (`full`) are BioCLIP; the rest the small one. */
export const engineOf = (source: 'online' | 'phone', kind?: 'full' | 'small' | null): IdEngine =>
  source === 'online' || kind === 'full' ? 'bioclip' : 'small'

export interface Candidate {
  name: string
  p: number
}
export interface GenusGroup {
  genus: string
  p: number
  species: Candidate[]
}

/**
 * The species in score order (the first `top`), every genus's total, and every genus's best species, from all species'
 * scores.
 */
export function rank(scores: ArrayLike<number>, species: string[], top = 5): { species: Candidate[]; genus: Map<string, number>; best: Map<string, Candidate> } {
  const genus = new Map<string, number>()
  for (let i = 0; i < species.length; i++) {
    const g = species[i].split(' ')[0]
    genus.set(g, (genus.get(g) ?? 0) + scores[i])
  }
  const order = Array.from({ length: species.length }, (_, i) => i).sort((a, b) => scores[b] - scores[a] || a - b)
  const best = new Map<string, Candidate>()
  for (const i of order) {
    const g = species[i].split(' ')[0]
    if (!best.has(g)) best.set(g, { name: species[i], p: scores[i] })
  }
  return { species: order.slice(0, top).map(i => ({ name: species[i], p: scores[i] })), genus, best }
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
 * The level that is clear: the species when its score clears `cuts.species`, else its genus when the genus total
 * clears `cuts.genus`, else (with the totals `higher`) the tribe, then the subfamily, whose total clears
 * `cuts.higher`, else «not sure». Used for both the server's answer and the phone's, with the cuts of the engine that
 * read the photo.
 */
export function headline(top: Candidate | undefined, genus: Map<string, number>, higher: Higher | null = null, cuts: Cuts = THRESHOLDS.bioclip): Headline {
  if (!top) return { kind: 'unsure' }
  if (top.p >= cuts.species) return { kind: 'species', name: top.name }
  const g = top.name.split(' ')[0]
  if ((genus.get(g) ?? 0) >= cuts.genus) return { kind: 'genus', genus: g }
  if (higher) {
    for (const kind of ['tribe', 'subfamily'] as const) {
      const b = best(higher[kind])
      if (b && b.p >= cuts.higher) return { kind, ...b }
    }
  }
  return { kind: 'unsure' }
}

/** A genus named in the two-genera answer: its total and its best species (null when no listed species is of it). */
export interface GenusPick {
  genus: string
  p: number
  species: Candidate | null
}
/**
 * When the genus is not clear (the headline is a tribe, a subfamily or «not sure»), the two genera to name instead of
 * the top species alone (Franz, 17 Nov 2026: «Mechanitis messenoides o Melinaea isocomma»), each with its total and
 * its best species; null to keep the top species and its genus as before. Rule (.work/two-genera-20261117/
 * PREREG_two_genera.md): the top two genera by total (ties: the higher best species, then by name) when their totals
 * sum to at least `cuts.pair` and the second's is at least `cuts.second`, and only when the top species' genus is one
 * of the two (the top species is always named). `best`: every genus's best species when known (the phone), else the
 * listed species (the server lists its first 5).
 */
export function genusPair(
  said: Headline | null,
  top: Candidate | undefined,
  genus: Map<string, number>,
  species: Candidate[],
  cuts: Cuts,
  best?: Map<string, Candidate>,
): [GenusPick, GenusPick] | null {
  if (!said || !top || said.kind === 'species' || said.kind === 'genus') return null
  const bestOf = (g: string) => best?.get(g) ?? species.find(c => c.name.split(' ')[0] === g) ?? null
  const picks = [...genus].map(([g, p]) => ({ genus: g, p, species: bestOf(g) }))
  picks.sort((a, b) => b.p - a.p || (b.species?.p ?? 0) - (a.species?.p ?? 0) || (a.genus < b.genus ? -1 : a.genus > b.genus ? 1 : 0))
  const [first, second] = picks
  if (!first || !second || first.p + second.p < cuts.pair || second.p < cuts.second) return null
  const g = top.name.split(' ')[0]
  if (first.genus !== g && second.genus !== g) return null
  return [first, second]
}

/**
 * The names under the candidate thumbnails: «M. messenoides», with the genus written out when another genus in the
 * list starts with the same letter (Mechanitis and Melinaea are not both «M.»).
 */
export function stripNames(names: string[]): Map<string, string> {
  const genera = new Set(names.map(n => n.split(' ')[0]))
  const out = new Map<string, string>()
  for (const n of names) {
    const g = n.split(' ')[0]
    const clash = [...genera].some(o => o !== g && o[0] === g[0])
    out.set(n, clash ? n : shortName(n))
  }
  return out
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

/** A species' tribe and subfamily as the page may show them (placeholders already left out of the file): null for none. */
export interface TaxonLabel {
  tribe: string | null
  subfamily: string | null
}
const nameAt = (list: string[], i: number | undefined) => (i !== undefined && i >= 0 && shownTaxon(list[i]) ? list[i] : null)
export function taxonOf(species: string, tax: Taxonomy | null): TaxonLabel | null {
  const pair = tax?.species[species]
  return pair ? { tribe: nameAt(tax!.tribes, pair[0]), subfamily: nameAt(tax!.subfamilies, pair[1]) } : null
}
/** A species' tribe and subfamily for the small label by the headline; null when the file names neither (or not the species). */
export function ranksAbove(species: string, tax: Taxonomy | null): TaxonLabel | null {
  const l = taxonOf(species, tax)
  return l && (l.tribe || l.subfamily) ? l : null
}

/** A rank shown under an uncertain species: its name, its total, and whether it clears its cut. */
export interface Rung {
  kind: 'genus' | 'tribe' | 'subfamily'
  name: string
  p: number
  sure: boolean
}
/**
 * The ranks under the guessed (top) species: none when the species is clear (its name says the genus); else its genus
 * (sure when it clears its cut, else «Incierto»), then the tribe or subfamily that clears its cut, when the genus does
 * not. That last one is a total over all its species, a statement about the photo: the guess may lie outside it.
 */
export function rungs(said: Headline | null, guess: Candidate | undefined, genus: Map<string, number>): Rung[] {
  if (!said || !guess || said.kind === 'species') return []
  const g = guess.name.split(' ')[0]
  const out: Rung[] = [{ kind: 'genus', name: g, p: genus.get(g) ?? guess.p, sure: said.kind === 'genus' && said.genus === g }]
  if (said.kind === 'tribe' || said.kind === 'subfamily') out.push({ kind: said.kind, name: said.name, p: said.p, sure: true })
  return out
}

/**
 * The subspecies named under the headline (views/IdentifyView.vue): the most probable subspecies leaf of the top
 * species (worker.py's subspecies scores, after the side's prior), `ssp` most probable first, every subspecies leaf
 * the head has for that species. None when the head names no subspecies (schema 1), or when the subspecies carries
 * little of the species' score (the species' own leaf, «subspecies not given», holds the rest: naming one would
 * overclaim). `kind` 'if' whenever the species itself is not clear (an uncertain guess: never a confident
 * subspecies under it): the line then says «if it is this species». `alt`: the second when it is close (at least half the first).
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
  if (!said || !top || !ssp?.length) return null
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
