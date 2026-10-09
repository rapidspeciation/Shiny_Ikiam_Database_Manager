import { reactive } from 'vue'
import { api } from './api'

/**
 * What a species looks like (server/species-photos.mjs): iNaturalist photos of
 * live butterflies, with their attribution, and the team's specimen photos.
 * Kept for the page's life; names the server has not looked up yet are asked
 * again every few seconds until it has (it asks iNaturalist one a second).
 */
export interface InatPhoto {
  id: number
  thumb: string
  url: string
  attribution: string
  license: string
  observation?: string
}
export interface SpeciesPhotos {
  inat: {
    taxon: { id: number; name: string; rank: string; common: string | null; url: string }
    level: 'subspecies' | 'species' | 'genus'
    synonymOf?: string
    photos: InatPhoto[]
  } | null
  pending: boolean
  specimens: { cam: string; sex: string | null; dorsal: string; ventral: string | null }[]
}

const cache = reactive(new Map<string, SpeciesPhotos>())
const wanted = new Set<string>()
const asking = new Set<string>()
let timer: ReturnType<typeof setTimeout> | null = null
let polls = 0

/** The name a table row is looked up by: species, plus its subspecies when the report splits them. */
export const photoName = (species: string, subspecies = '') =>
  species && species !== 'Sin especie' ? `${species} ${subspecies}`.trim() : ''

async function load() {
  timer = null
  const names = [...wanted]
  wanted.clear()
  if (!names.length) return
  for (const name of names) asking.add(name)
  try {
    const { species } = await api<{ species: Record<string, SpeciesPhotos> }>('species-photos', {
      method: 'POST',
      body: { names },
    })
    for (const [name, value] of Object.entries(species)) cache.set(name, value)
  } catch {
    // No photos is fine: the table keeps its placeholders.
    for (const name of names) if (!cache.has(name)) cache.set(name, { inat: null, pending: false, specimens: [] })
  }
  for (const name of names) asking.delete(name)
  const pending = [...cache].filter(([, v]) => v.pending).map(([k]) => k)
  // About 2 s per name on the server; stop after ~10 minutes.
  if (pending.length && polls++ < 120) {
    for (const name of pending) wanted.add(name)
    timer = setTimeout(load, 5000)
  }
}

/** The photos of a name (undefined while unknown); asking for it joins the next request. */
export function speciesPhotos(name: string): SpeciesPhotos | undefined {
  if (!name) return undefined
  const hit = cache.get(name)
  if (!hit && !wanted.has(name) && !asking.has(name)) {
    wanted.add(name)
    polls = 0
    timer ??= setTimeout(load, 50)
  }
  return hit
}
