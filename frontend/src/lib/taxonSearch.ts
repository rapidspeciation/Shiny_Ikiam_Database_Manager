/**
 * The taxon search of the Identificar tab (components/identify/TaxonSearch.vue): one box for a genus, a species or a
 * subspecies, typed whole or in part («mechanitis», «mech poly», «M. polymnia», «polymnia», «casabranca»). The names
 * come from the photo packs' lists (lib/photoPacks.ts): the installed pack offline, the server's whole list online.
 */
import type { Group, PackTaxon } from './photoPacks'

export type HitRank = 'genus' | 'species' | 'subspecies'
export interface Hit {
  name: string
  rank: HitRank
  family?: string
  group?: Group
  /** A genus: how many species the lists name. */
  species?: number
}
export interface Catalog {
  /** Every name once (a genus, a species, a subspecies), with its words normalised. */
  entries: (Hit & { words: string[]; key: string })[]
  /** A genus' species, sorted. */
  speciesOf: Map<string, string[]>
  /** A species' subspecies, sorted. */
  sspOf: Map<string, string[]>
  taxa: Map<string, Pick<PackTaxon, 'name' | 'rank' | 'genus' | 'species' | 'family' | 'group'>>
}

/** Lower case, no accents, a dot or hyphen as a space. */
export const norm = (s: string) =>
  s
    .normalize('NFD')
    .replace(/[̀-ͯ]/g, '')
    .toLowerCase()
    .replace(/[.\-_,;]+/g, ' ')
    .replace(/\s+/g, ' ')
    .trim()

type Named = Pick<PackTaxon, 'name' | 'rank' | 'genus' | 'species' | 'family' | 'group'>
/** The catalog of several lists' taxa (the first list's entry wins for a name). */
export function catalog(...lists: Named[][]): Catalog {
  const taxa = new Map<string, Named>()
  for (const list of lists) for (const t of list) if (!taxa.has(t.name)) taxa.set(t.name, t)
  const speciesOf = new Map<string, Set<string>>()
  const sspOf = new Map<string, Set<string>>()
  const genusInfo = new Map<string, { family?: string; group?: Group }>()
  for (const t of taxa.values()) {
    const sp = t.rank === 'species' ? t.name : t.species
    if (!speciesOf.has(t.genus)) speciesOf.set(t.genus, new Set())
    speciesOf.get(t.genus)!.add(sp)
    if (t.rank === 'subspecies') {
      if (!sspOf.has(sp)) sspOf.set(sp, new Set())
      sspOf.get(sp)!.add(t.name)
    }
    if (!genusInfo.has(t.genus)) genusInfo.set(t.genus, { family: t.family, group: t.group })
  }
  const entries: Catalog['entries'] = []
  for (const [g, set] of speciesOf) entries.push({ name: g, rank: 'genus', species: set.size, ...genusInfo.get(g), words: [norm(g)], key: norm(g) })
  // A species named only through its subspecies' entries is still a name to find.
  const named = new Set(taxa.keys())
  for (const set of speciesOf.values())
    for (const sp of set)
      if (!named.has(sp)) {
        const g = sp.split(' ')[0]
        entries.push({ name: sp, rank: 'species', ...genusInfo.get(g), words: norm(sp).split(' '), key: norm(sp) })
      }
  for (const t of taxa.values()) entries.push({ name: t.name, rank: t.rank, family: t.family, group: t.group, words: norm(t.name).split(' '), key: norm(t.name) })
  const sorted = (m: Map<string, Set<string>>) => new Map([...m].map(([k, v]) => [k, [...v].sort()]))
  return { entries, speciesOf: sorted(speciesOf), sspOf: sorted(sspOf), taxa }
}

const RANK_ORDER: Record<HitRank, number> = { genus: 0, species: 1, subspecies: 2 }

/**
 * The names a query matches, best first. Each word typed must start a word of the name, in order («mech poly»,
 * «m polymnia»); a single word may also start the name's epithet («polymnia», «casabranca»). Best: the name typed
 * whole, then a name that starts with the query, then by rank (genus, species, subspecies) and name.
 */
export function search(cat: Catalog, query: string, limit = 8): Hit[] {
  const q = norm(query)
  if (q.length < 2) return []
  const tokens = q.split(' ')
  const scored: { e: Catalog['entries'][number]; score: number }[] = []
  for (const e of cat.entries) {
    if (tokens.length > e.words.length) continue
    let score: number | null = null
    if (e.key === q) score = 0
    else if (e.key.startsWith(q)) score = 1
    else {
      // In order, each token the start of a later word; the first token may start at any word.
      let w = 0
      let ok = true
      for (const tk of tokens) {
        while (w < e.words.length && !e.words[w].startsWith(tk)) w++
        if (w >= e.words.length) {
          ok = false
          break
        }
        w++
      }
      if (ok) score = 2
    }
    if (score != null) scored.push({ e, score })
  }
  scored.sort((a, b) => a.score - b.score || RANK_ORDER[a.e.rank] - RANK_ORDER[b.e.rank] || a.e.name.localeCompare(b.e.name))
  return scored.slice(0, limit).map(({ e }) => ({ name: e.name, rank: e.rank, ...(e.family ? { family: e.family } : {}), ...(e.group ? { group: e.group } : {}), ...(e.species != null ? { species: e.species } : {}) }))
}
