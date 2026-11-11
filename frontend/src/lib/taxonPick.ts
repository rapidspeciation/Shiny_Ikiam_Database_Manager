/**
 * The names «Es otra especie» offers, behind a small interface: for now the identification model's own lists (the
 * head's species and subspecies, the genera they make), or the result's candidates when no head is at hand. The
 * offline packs branch's lib/taxonSearch.ts (catalog() and search()) can stand behind the same `TaxonSource` once the
 * labs are combined.
 */

export type TaxonRank = 'genus' | 'species' | 'subspecies'
export interface TaxonHit {
  name: string
  rank: TaxonRank
}
export interface TaxonSource {
  /** Up to `limit` names matching the typed text (start of a word first), or none for an empty text. */
  search(query: string, limit?: number): TaxonHit[]
  /** Whether the name is one the source knows (a name typed freely is not). */
  knows(name: string): boolean
}

const fold = (s: string) =>
  s
    .normalize('NFD')
    .replace(/\p{M}/gu, '')
    .toLowerCase()
    .replace(/\s+/g, ' ')
    .trim()

/** A source over a list of names: species («Genus epithet»), subspecies (three words) and their genera. */
export function listSource(species: string[], subspecies: string[] = []): TaxonSource {
  const all = new Map<string, TaxonHit>()
  const add = (name: string, rank: TaxonRank) => {
    const n = name.trim().replace(/\s+/g, ' ')
    if (n && !all.has(fold(n))) all.set(fold(n), { name: n, rank })
  }
  for (const s of species) {
    add(s.split(' ')[0], 'genus')
    add(s, s.trim().split(/\s+/).length >= 3 ? 'subspecies' : 'species')
  }
  for (const s of subspecies) add(s, 'subspecies')
  const items = [...all.entries()]
  return {
    search(query, limit = 8) {
      const q = fold(query)
      if (q.length < 2) return []
      const first: TaxonHit[] = [], word: TaxonHit[] = [], inside: TaxonHit[] = []
      for (const [key, hit] of items) {
        if (key.startsWith(q)) first.push(hit)
        else if (key.includes(` ${q}`)) word.push(hit)
        else if (key.includes(q)) inside.push(hit)
      }
      const order = { genus: 0, species: 1, subspecies: 2 }
      const sort = (a: TaxonHit, b: TaxonHit) => order[a.rank] - order[b.rank] || a.name.localeCompare(b.name)
      return [...first.sort(sort), ...word.sort(sort), ...inside.sort(sort)].slice(0, limit)
    },
    knows: name => all.has(fold(name)),
  }
}

/** The head's names (lib/offlineId.ts HeadMeta): its species, and its subspecies leaves (schema 2). */
export function headSource(meta: { species: string[]; leaves?: string[]; is_ssp?: string }): TaxonSource {
  const ssp = meta.leaves && meta.is_ssp ? meta.leaves.filter((_, i) => meta.is_ssp![i] === '1') : []
  return listSource(meta.species, ssp)
}
