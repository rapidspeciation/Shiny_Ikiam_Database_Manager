/**
 * Reference photos for the Identificar tab: what a candidate species looks like, to compare with the person's photo.
 * Without an account, and for «Fotos para el campo»: the static field set beside the page (identificar/field/,
 * scripts/field-photos.py: one CC-licensed iNaturalist photo per Ithomiini species of the head and, since 4 Nov 2026,
 * per subspecies where iNaturalist has one, each with its credit, its size and whether it is recorded in Ecuador;
 * index.json). The person chooses what to save (FieldScope); a subspecies' photo is shown after its species' photos.
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
  /**
   * 'species', 'subspecies' (the field set's photo of one of the species' subspecies, named in `taxon`), 'genus'
   * (iNaturalist had none of the species: another species of the genus), 'specimen' (the project's).
   */
  level: 'species' | 'subspecies' | 'genus' | 'specimen'
  /** The subspecies' full name, for level 'subspecies'. */
  taxon?: string
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

interface FieldEntry {
  name: string
  file: string
  attribution: string
  license: string
  taxonUrl: string | null
  source?: string | null
  /** Recorded in Ecuador (scripts/field-photos.py). */
  ec?: boolean
  bytes?: number
}
export interface FieldIndex {
  species: FieldEntry[]
  subspecies?: FieldEntry[]
  /** How many the head has, and how many of those have a photo (all, and in Ecuador). */
  counts?: Record<'species' | 'subspecies', { head: number; photos: number; ecuador: number; ecuador_photos: number }>
}
let fieldMemo: Promise<FieldIndex> | null = null
/** The static field set's list (the service worker keeps it for use without signal). */
export function fieldIndex(): Promise<FieldIndex> {
  fieldMemo ??= fetch(new URL('index.json', FIELD_DIR()).href, { credentials: 'same-origin' })
    .then(r => (r.ok ? r.json() : Promise.reject(new Error(`HTTP ${r.status}`))))
    .catch(err => {
      fieldMemo = null
      throw err
    })
  return fieldMemo
}
const fieldPhoto = (e: FieldEntry, level: 'species' | 'subspecies'): RefPhoto => ({
  src: new URL(e.file, FIELD_DIR()).href,
  credit: shortCredit(e.attribution),
  license: e.license,
  level,
  source: e.source ?? null,
  ...(level === 'subspecies' ? { taxon: e.name } : {}),
})
const fieldRefs = (e: FieldEntry): Refs => ({ photos: [fieldPhoto(e, 'species')], taxonUrl: e.taxonUrl })
const speciesOf = (ssp: string) => ssp.split(' ').slice(0, 2).join(' ')
const FIELD = 'identify:field-photos'
const SCOPE = 'identify:field-scope'

/** What «Fotos para el campo» saves: the species (all or those recorded in Ecuador) and the subspecies (none, all, Ecuador). */
export interface FieldScope {
  species: 'all' | 'ec'
  ssp: 'none' | 'all' | 'ec'
  /** Saved by itself after the model's download (the box in the download panel). */
  auto: boolean
}
/**
 * Default: every species (the model also suggests species of other countries, and all of them take only about 3 MB
 * more than Ecuador's), the subspecies of Ecuador (the team's country; all of them would double the size).
 */
export const DEFAULT_SCOPE: FieldScope = { species: 'all', ssp: 'ec', auto: true }
export function fieldScope(): FieldScope {
  try {
    const v = JSON.parse(localStorage.getItem(SCOPE) || 'null') as Partial<FieldScope> | null
    return {
      species: v?.species === 'ec' ? 'ec' : 'all',
      ssp: v?.ssp === 'none' || v?.ssp === 'all' ? v.ssp : 'ec',
      auto: v?.auto !== false,
    }
  } catch {
    return { ...DEFAULT_SCOPE }
  }
}
export function setFieldScope(scope: FieldScope) {
  try {
    localStorage.setItem(SCOPE, JSON.stringify(scope))
  } catch {
    /* private mode: the default next time */
  }
}
/** The entries a scope takes. */
export function inScope(index: FieldIndex, scope: Pick<FieldScope, 'species' | 'ssp'>): { species: FieldEntry[]; subspecies: FieldEntry[] } {
  return {
    species: index.species.filter(e => scope.species === 'all' || e.ec),
    subspecies: scope.ssp === 'none' ? [] : (index.subspecies ?? []).filter(e => scope.ssp === 'all' || e.ec),
  }
}
/** Photos and bytes of a list of entries, each file once (a few species share a photo). */
export function sizeOf(entries: FieldEntry[]): { photos: number; bytes: number } {
  const files = new Map(entries.map(e => [e.file, e.bytes ?? 0]))
  return { photos: entries.length, bytes: [...files.values()].reduce((a, b) => a + b, 0) }
}
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

/**
 * The field set's photo for names that have none yet, and after a species' photos those of its subspecies (when the
 * set is reachable or kept; a subspecies photo the phone did not save and cannot fetch is left out by RefCompare).
 */
/** The field set's files on the phone. */
async function savedFieldFiles(): Promise<Set<string>> {
  try {
    return new Set((await (await caches.open(CACHE)).keys()).map(r => r.url).filter(u => u.startsWith(FIELD_DIR())))
  } catch {
    return new Set()
  }
}
async function withField(refs: Record<string, Refs>, names: string[]): Promise<Record<string, Refs>> {
  const field = await fieldIndex().catch(() => null)
  if (!field) return refs
  const saved = navigator.onLine ? null : await savedFieldFiles()
  const out = { ...refs }
  for (const n of names) {
    if (!out[n]?.photos.length) {
      const e = field.species.find(f => f.name === n)
      if (e) out[n] = fieldRefs(e)
    }
    // Without signal only those the phone saved (the others would show as missing).
    const ssp = (field.subspecies ?? []).filter(f => speciesOf(f.name) === n && (!saved || saved.has(new URL(f.file, FIELD_DIR()).href)))
    if (!ssp.length) continue
    const base = out[n] ?? { photos: [], taxonUrl: null }
    const own = base.photos.filter(p => p.level !== 'subspecies')
    const have = new Set(own.map(p => p.src))
    out[n] = { ...base, photos: [...own, ...ssp.map(e => fieldPhoto(e, 'subspecies')).filter(p => !have.has(p.src))] }
  }
  return out
}

export interface FieldSet {
  /** Photos saved (species and subspecies). */
  count: number
  bytes: number
  at: string
  /** What was saved (absent: a set saved before 4 Nov 2026, every species). */
  scope?: Pick<FieldScope, 'species' | 'ssp'>
  ssp?: number
}
export function fieldSet(): FieldSet | null {
  try {
    return JSON.parse(localStorage.getItem(FIELD) || 'null')
  } catch {
    return null
  }
}

/**
 * «Fotos para el campo»: the photos of the static field set the scope takes, saved on the phone; those it no longer
 * takes are removed. The same for everyone: no account, nothing asked to the team's API.
 */
export async function saveFieldPhotos(
  scope: Pick<FieldScope, 'species' | 'ssp'>,
  progress: (done: number, total: number) => void,
): Promise<FieldSet & { failed: number; total: number }> {
  const index = await fieldIndex()
  const take = inScope(index, scope)
  const entries = [...take.species, ...take.subspecies]
  const cache = await caches.open(CACHE)
  const files = [...new Set(entries.map(e => new URL(e.file, FIELD_DIR()).href))]
  let bytes = 0,
    done = 0,
    failed = 0
  const ok = new Set<string>()
  for (const src of files) {
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
        ok.add(src)
      } else failed++
    } catch {
      failed++ // the next «Actualizar fotos» takes it
    }
    progress(++done, files.length)
  }
  // Photos of the set the scope no longer takes: off the phone.
  const all = new Set([...index.species, ...(index.subspecies ?? [])].map(e => new URL(e.file, FIELD_DIR()).href))
  for (const req of await cache.keys()) if (all.has(req.url) && !files.includes(req.url)) await cache.delete(req)
  // Lists seen online keep their extra photos; the field photo is added where the phone had none, and a kept
  // list of only field photos that are gone now is dropped.
  const have = await kept()
  const refs: Record<string, Refs> = {}
  for (const e of take.species) if (ok.has(new URL(e.file, FIELD_DIR()).href)) refs[e.name] = have[e.name]?.photos.length ? have[e.name] : fieldRefs(e)
  await keep(refs)
  await dropGone(all, ok)
  const saved = entries.filter(e => ok.has(new URL(e.file, FIELD_DIR()).href))
  const set: FieldSet = { count: saved.length, bytes, at: new Date().toISOString(), scope: { species: scope.species, ssp: scope.ssp }, ssp: saved.length - take.species.filter(e => ok.has(new URL(e.file, FIELD_DIR()).href)).length }
  try {
    localStorage.setItem(FIELD, JSON.stringify(set))
  } catch {
    /* shown again as not saved */
  }
  return { ...set, failed, total: files.length }
}

/** Kept lists whose photos are all field photos no longer on the phone. */
async function dropGone(field: Set<string>, ok: Set<string>) {
  try {
    const all = await kept()
    const left = Object.fromEntries(Object.entries(all).filter(([, r]) => !r.photos.length || r.photos.some(p => !field.has(p.src) || ok.has(p.src))))
    if (Object.keys(left).length !== Object.keys(all).length)
      await (await caches.open(CACHE)).put(KEY(), new Response(JSON.stringify(left), { headers: { 'content-type': 'application/json' } }))
  } catch {
    /* no storage */
  }
}
