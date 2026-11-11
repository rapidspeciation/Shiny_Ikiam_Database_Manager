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
  // A name with no family is a misspelt head label («Hwy», «M», «Psilogramm»: neither iNaturalist nor GBIF knows it,
  // and it has no photo): not offered.
  for (const list of lists) for (const t of list) if (t.family && !taxa.has(t.name)) taxa.set(t.name, t)
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

// ---------------------------------------------------------------- what a chosen taxon shows

/** A reference photo as lib/identifyRefs.ts gives it (only the fields shown). */
export interface ShownPhoto {
  src: string
  credit: string
  license: string
  level: 'species' | 'subspecies' | 'genus' | 'specimen'
  taxon?: string
  sex?: string | null
  source?: string | null
}
export interface Card {
  key: string
  /** The taxon the photo shows, in full (the full-screen view's name). */
  name: string
  /** The words under the card: an epithet (the header already names the genus or species), or null. */
  label: string | null
  /** A genus card whose species has only a subspecies' photo: that subspecies' epithet. */
  viaSsp: string | null
  /** What kind of photo, when the card's place does not say it. */
  kicker: 'species' | 'subspecies' | 'specimen' | null
  photo: ShownPhoto
}
export interface TaxonView {
  /** A species or subspecies: its photo, large. */
  lead: Card | null
  /** 'species': a genus' species; 'more': more photos of the species (and the team's specimens); 'ssp': its subspecies. */
  sections: { id: 'species' | 'more' | 'ssp'; cards: Card[] }[]
  /** Names the lists know with no photo here (a genus' species, a species' subspecies). */
  noPhoto: string[]
}
const epithetOf = (name: string, words: number) => name.split(' ').slice(words).join(' ')

/**
 * The photos of a chosen genus, species or subspecies, from `refs` (species name → its photos, as referencePhotos
 * gives them: the species' own, its subspecies' with `taxon`, the team's specimens). A genus card falls back to a
 * saved subspecies photo and names it; a species' extra photos are not called subspecies; subspecies cards carry
 * their epithet alone.
 */
export function taxonView(cat: Catalog, hit: Hit, refs: Record<string, { photos: ShownPhoto[] } | undefined>): TaxonView {
  const all = (sp: string) => (refs[sp]?.photos ?? []).filter(p => p.level !== 'genus')
  const own = (sp: string) => all(sp).filter(p => p.level !== 'subspecies')
  const ssp = (sp: string) => all(sp).filter(p => p.level === 'subspecies' && p.taxon)
  const ownCard = (sp: string, p: ShownPhoto, i: number): Card => ({
    key: `${sp}#${i}`,
    name: sp,
    label: null,
    viaSsp: null,
    kicker: p.level === 'specimen' ? 'specimen' : 'species',
    photo: p,
  })
  const sspCard = (p: ShownPhoto): Card => ({ key: p.taxon!, name: p.taxon!, label: epithetOf(p.taxon!, 2), viaSsp: null, kicker: 'subspecies', photo: p })
  if (hit.rank === 'genus') {
    const cards: Card[] = []
    const noPhoto: string[] = []
    for (const sp of cat.speciesOf.get(hit.name) ?? []) {
      const p = own(sp).find(x => x.level === 'species') ?? own(sp)[0]
      const s = p ? null : ssp(sp)[0]
      if (p) cards.push({ key: sp, name: sp, label: epithetOf(sp, 1), viaSsp: null, kicker: null, photo: p })
      else if (s) cards.push({ key: sp, name: s.taxon!, label: epithetOf(sp, 1), viaSsp: epithetOf(s.taxon!, 2), kicker: null, photo: s })
      else noPhoto.push(sp)
    }
    return { lead: null, sections: [{ id: 'species', cards }], noPhoto }
  }
  const sp = hit.name.split(' ').slice(0, 2).join(' ')
  const mine = own(sp).map((p, i) => ownCard(sp, p, i))
  const subs = ssp(sp).map(sspCard)
  const withPhoto = new Set(subs.map(c => c.name))
  let lead: Card | null
  let more: Card[]
  let rest: Card[]
  if (hit.rank === 'species') {
    lead = mine[0] ?? subs[0] ?? null
    more = mine.slice(1)
    rest = subs.filter(c => c !== lead)
  } else {
    const self = subs.filter(c => c.name === hit.name)
    lead = self[0] ?? mine[0] ?? null
    more = [...self.slice(1), ...mine].filter(c => c !== lead)
    rest = subs.filter(c => c.name !== hit.name)
  }
  const noPhoto = (cat.sspOf.get(sp) ?? []).filter(n => !withPhoto.has(n) && n !== hit.name)
  return {
    lead,
    sections: [
      { id: 'more' as const, cards: more },
      { id: 'ssp' as const, cards: rest },
    ].filter(x => x.cards.length),
    noPhoto,
  }
}
