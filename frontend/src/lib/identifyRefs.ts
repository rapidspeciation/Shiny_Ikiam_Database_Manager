/**
 * Reference photos for the Identificar tab: what a candidate species looks like, to compare with the person's photo.
 * Online from /api/species-photos (server/species-photos.mjs: iNaturalist's CC-licensed photos with their credit, and
 * the team's own specimens), the iNaturalist ones through the same-origin proxy (/api/identify/ref-photo: the page is
 * cross-origin isolated). Every list seen and the «Fotos para el campo» set are kept on the phone (Cache Storage, with
 * the models), and the service worker keeps each photo shown, so they work again without signal.
 */
import { api } from './api'
import { appRoot } from './appRoot'
import { CACHE } from './offlineEngine'

export interface RefPhoto {
  src: string
  credit: string
  license: string
  /** 'species' (or subspecies), 'genus' (iNaturalist had none of the species: another species of the genus), 'specimen' (the project's). */
  level: 'species' | 'genus' | 'specimen'
  sex?: string | null
}
export interface Refs {
  photos: RefPhoto[]
  taxonUrl: string | null
}

interface InatEntry {
  level: 'subspecies' | 'species' | 'genus'
  taxon?: { url?: string }
  photos: { thumb: string; attribution: string; license: string }[]
}
interface ApiEntry {
  inat: InatEntry | null
  pending: boolean
  specimens?: { cam: string; sex: string | null; dorsal: string }[]
}

/** «© Ana Pérez» from iNaturalist's «(c) Ana Pérez, some rights reserved (CC BY-SA), uploaded by …» (the licence is shown apart). */
export function shortCredit(attribution: string): string {
  const m = /^\(c\)\s*([^,]+)/i.exec(attribution.trim())
  return m ? `© ${m[1].trim()}` : attribution.trim()
}

const KEY = () => new URL('prueba-offline/refs.json', appRoot()).href
const FIELD = 'identify:field-photos'
export const refPhotoSrc = (thumb: string) => `${appRoot()}api/identify/ref-photo?u=${encodeURIComponent(thumb)}`

async function kept(): Promise<Record<string, Refs>> {
  try {
    const r = await (await caches.open(CACHE)).match(KEY())
    return r ? ((await r.json()) as Record<string, Refs>) : {}
  } catch {
    return {}
  }
}
async function keep(add: Record<string, Refs>) {
  try {
    // Only iNaturalist's photos are kept (the team's specimens need the sign-in and the network).
    const all = { ...(await kept()) }
    for (const [name, refs] of Object.entries(add)) all[name] = { ...refs, photos: refs.photos.filter(p => p.level !== 'specimen') }
    await (await caches.open(CACHE)).put(KEY(), new Response(JSON.stringify(all), { headers: { 'content-type': 'application/json' } }))
  } catch {
    /* no storage: the photos are asked again next time */
  }
}

function fromApi(entry: ApiEntry): Refs {
  const inat = entry.inat
  const level = inat?.level === 'genus' ? 'genus' : 'species'
  const photos: RefPhoto[] = (inat?.photos ?? []).map(p => ({ src: refPhotoSrc(p.thumb), credit: shortCredit(p.attribution), license: p.license, level }))
  for (const s of entry.specimens ?? [])
    photos.push({ src: `${appRoot()}api/photo/${encodeURIComponent(s.dorsal)}?w=400`, credit: s.cam, license: '', level: 'specimen', sex: s.sex })
  return { photos, taxonUrl: inat?.taxon?.url ?? null }
}

/**
 * The photos of `names`: asked online (names the server is still looking up come back `pending`), else what the phone
 * kept. `pending`: names to ask again in a few seconds.
 */
export async function referencePhotos(names: string[]): Promise<{ refs: Record<string, Refs>; pending: string[] }> {
  const have = await kept()
  if (navigator.onLine) {
    try {
      const { species } = await api<{ species: Record<string, ApiEntry> }>('species-photos', { method: 'POST', body: { names } })
      const refs: Record<string, Refs> = {}
      const pending: string[] = []
      for (const name of names) {
        const e = species[name]
        if (!e) continue
        if (e.pending && !e.inat) pending.push(name)
        refs[name] = e.inat || e.specimens?.length ? fromApi(e) : (have[name] ?? { photos: [], taxonUrl: null })
      }
      await keep(Object.fromEntries(Object.entries(refs).filter(([, r]) => r.photos.length)))
      return { refs, pending }
    } catch {
      /* offline after all: what the phone kept */
    }
  }
  return { refs: Object.fromEntries(names.filter(n => have[n]).map(n => [n, have[n]])), pending: [] }
}

export interface FieldSet {
  count: number
  bytes: number
  at: string
}
export function fieldSet(): FieldSet | null {
  try {
    return JSON.parse(localStorage.getItem(FIELD) || 'null')
  } catch {
    return null
  }
}

/**
 * «Fotos para el campo»: one photo per Ithomiini species the server has from iNaturalist, saved on the phone. Returns
 * what was saved and how many species the server is still looking up (ask again later for those).
 */
export async function saveFieldPhotos(progress: (done: number, total: number) => void): Promise<FieldSet & { pending: number; total: number }> {
  const list = await api<{ species: { name: string; thumb: string; attribution: string; license: string; taxonUrl: string | null }[]; pending: number; total: number }>(
    'identify/field-photos',
  )
  const cache = await caches.open(CACHE)
  let bytes = 0,
    done = 0
  const refs: Record<string, Refs> = {}
  for (const s of list.species) {
    const src = refPhotoSrc(s.thumb)
    try {
      let r = await cache.match(src)
      if (!r) {
        const got = await fetch(src, { credentials: 'same-origin' })
        if (got.ok) {
          await cache.put(src, got.clone())
          r = got
        }
      }
      if (r) {
        bytes += (await r.clone().blob()).size
        refs[s.name] = { photos: [{ src, credit: shortCredit(s.attribution), license: s.license, level: 'species' }], taxonUrl: s.taxonUrl }
      }
    } catch {
      /* this one is skipped; the next try takes it */
    }
    progress(++done, list.species.length)
  }
  // Lists seen online keep their extra photos; the field photo is added where the phone had none.
  const have = await kept()
  await keep(Object.fromEntries(Object.entries(refs).map(([n, r]) => [n, have[n]?.photos.length ? have[n] : r])))
  const set = { count: Object.keys(refs).length, bytes, at: new Date().toISOString() }
  try {
    localStorage.setItem(FIELD, JSON.stringify(set))
  } catch {
    /* shown again as not saved */
  }
  return { ...set, pending: list.pending, total: list.total }
}
