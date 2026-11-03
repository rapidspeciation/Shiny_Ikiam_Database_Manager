/**
 * Reference photos for the Identificar tab: what a candidate species looks like, to compare with the person's photo.
 * Without an account, and for «Fotos para el campo»: the static field set beside the page (identificar/field/, one
 * CC-licensed iNaturalist photo per Ithomiini species of the head with its credit, built 3 Nov 2026; index.json).
 * Signed in, online, also from /api/species-photos (server/species-photos.mjs: iNaturalist's CC-licensed photos with their credit, and
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
  /** The photo's own page (iNaturalist), for its credit. */
  source?: string | null
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

/** An iNaturalist photo's page from its file URL (…/photos/<id>/small.jpg). */
export const inatPhotoPage = (url: string) => {
  const m = /\/photos\/(\d+)\//.exec(url)
  return m ? `https://www.inaturalist.org/photos/${m[1]}` : null
}

/** The Creative Commons deed of a licence code as iNaturalist gives it (CC-BY-NC, CC0, …), or null. */
export function licenseUrl(code: string): string | null {
  const c = code.trim().toUpperCase()
  if (c === 'CC0') return 'https://creativecommons.org/publicdomain/zero/1.0/'
  const m = /^CC-(BY(?:-NC)?(?:-SA|-ND)?)$/.exec(c)
  return m ? `https://creativecommons.org/licenses/${m[1].toLowerCase()}/4.0/` : null
}

/** «© Ana Pérez» from iNaturalist's «(c) Ana Pérez, some rights reserved (CC BY-SA), uploaded by …» (the licence is shown apart). */
export function shortCredit(attribution: string): string {
  const m = /^\(c\)\s*([^,]+)/i.exec(attribution.trim())
  return m ? `© ${m[1].trim()}` : attribution.trim()
}

const KEY = () => new URL('prueba-offline/refs.json', appRoot()).href
const FIELD_DIR = () => new URL('identificar/field/', appRoot()).href

interface FieldEntry { name: string; file: string; attribution: string; license: string; taxonUrl: string | null; source?: string }
let fieldMemo: Promise<FieldEntry[]> | null = null
/** The static field set's list (the service worker keeps it for use without signal). */
function fieldIndex(): Promise<FieldEntry[]> {
  fieldMemo ??= fetch(new URL('index.json', FIELD_DIR()).href, { credentials: 'same-origin' })
    .then(r => (r.ok ? r.json() : Promise.reject(new Error(`HTTP ${r.status}`))))
    .then((v: { species: FieldEntry[] }) => v.species)
    .catch(err => {
      fieldMemo = null
      throw err
    })
  return fieldMemo
}
const fieldRefs = (e: FieldEntry): Refs => ({
  photos: [{ src: new URL(e.file, FIELD_DIR()).href, credit: shortCredit(e.attribution), license: e.license, level: 'species', source: e.source ?? null }],
  taxonUrl: e.taxonUrl,
})
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
  const photos: RefPhoto[] = (inat?.photos ?? []).map(p => ({ src: refPhotoSrc(p.thumb), credit: shortCredit(p.attribution), license: p.license, level, source: inatPhotoPage(p.thumb) }))
  for (const s of entry.specimens ?? [])
    photos.push({ src: `${appRoot()}api/photo/${encodeURIComponent(s.dorsal)}?w=400`, credit: s.cam, license: '', level: 'specimen', sex: s.sex })
  return { photos, taxonUrl: inat?.taxon?.url ?? null }
}

/**
 * The photos of `names`: signed in and online, asked to the server (names it is still looking up come back
 * `pending`); else what the phone kept, then the static field set. `pending`: names to ask again in a few seconds.
 */
export async function referencePhotos(names: string[], { signedIn = true }: { signedIn?: boolean } = {}): Promise<{ refs: Record<string, Refs>; pending: string[] }> {
  // Without an account only the static set (lists kept from a team session point at the team's photo proxy).
  if (!signedIn) return { refs: await withField({}, names), pending: [] }
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
      return { refs: await withField(refs, names), pending }
    } catch {
      /* offline after all: what the phone kept */
    }
  }
  return { refs: await withField(Object.fromEntries(names.filter(n => have[n]).map(n => [n, have[n]])), names), pending: [] }
}

/** The field set's photo for names that have none yet (when the set is reachable or kept). */
async function withField(refs: Record<string, Refs>, names: string[]): Promise<Record<string, Refs>> {
  const missing = names.filter(n => !refs[n]?.photos.length)
  if (!missing.length) return refs
  const field = await fieldIndex().catch(() => [] as FieldEntry[])
  const out = { ...refs }
  for (const n of missing) {
    const e = field.find(f => f.name === n)
    if (e) out[n] = fieldRefs(e)
  }
  return out
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
 * «Fotos para el campo»: the static field set (one photo per Ithomiini species with a CC-licensed iNaturalist photo),
 * saved on the phone. The same for everyone: no account, nothing asked to the team's API.
 */
export async function saveFieldPhotos(progress: (done: number, total: number) => void): Promise<FieldSet & { pending: number; failed: number; total: number }> {
  const species = await fieldIndex()
  const cache = await caches.open(CACHE)
  let bytes = 0,
    done = 0,
    failed = 0
  const refs: Record<string, Refs> = {}
  for (const s of species) {
    const src = new URL(s.file, FIELD_DIR()).href
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
        refs[s.name] = fieldRefs(s)
      } else failed++
    } catch {
      failed++ // the next «Actualizar fotos» takes it
    }
    progress(++done, species.length)
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
  return { ...set, pending: 0, failed, total: species.length }
}
