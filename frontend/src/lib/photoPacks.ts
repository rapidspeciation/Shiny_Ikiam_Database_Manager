/**
 * Photo packs for the Identificar tab: one reference photo per taxon, kept on the phone to compare a result and to
 * look a taxon up without signal (the Descargas place, components/identify/PackPanel.vue; the search,
 * components/identify/TaxonSearch.vue). The server's list is identificar/packs/index.json (scripts/photo-packs.py):
 * one entry per taxon with its rank, genus, family, group, countries (ISO2) and photo with its credit.
 *
 * The person chooses a scope: which taxa (Ithomiini; plus the other Nymphalidae; plus every butterfly; plus moths),
 * which region (Ecuador, one or several countries, the whole Neotropics) and whether to add the subspecies. Every
 * option says how many photos and MB it takes, from the list, before anything is saved. One pack is on the phone
 * at a time: saving another choice adds what is new and removes what it no longer takes.
 *
 * The installed pack is kept in Cache Storage with the models (offlineEngine CACHE): its photos under their URLs
 * (the service worker serves them offline) and its own list (the taxa saved, with the server's version) under
 * INSTALLED_KEY, so the phone knows what it has without signal and when the server has a newer list.
 */
import { appRoot } from './appRoot'
import { intlLocale } from './i18n'
import { CACHE } from './offlineEngine'

export type Group = 'ithomiini' | 'nymphalidae' | 'butterfly' | 'moth'
export type Rank = 'species' | 'subspecies'

export interface PackPhoto {
  /** Relative to the index's `base` (default photos/). */
  file: string
  attribution: string
  license: string
  taxonUrl: string | null
  /** The photo's own page (iNaturalist), for its credit. */
  source: string | null
  /**
   * Listed but not on this server yet (the builder made only some regions' photos, scripts/photo-packs.py --cell):
   * counted in sizes with `bytes` estimated, never saved or shown.
   */
  missing?: boolean
}
export interface PackTaxon {
  name: string
  rank: Rank
  /** The species' name (the taxon's own for a species). */
  species: string
  genus: string
  family: string
  group: Group
  /** ISO 3166-1 alpha-2 codes of the Neotropical countries where it is recorded (empty: none; in no region). */
  countries: string[]
  /** null: no free photo of it (still listed, so the search names it; signed in and online the server may have one). */
  photo: PackPhoto | null
  /** The photo file's size. */
  bytes: number
}
export interface PackIndex {
  schema: 'ithomiini-photo-packs/1'
  /** Changes whenever a photo or an entry changes (the builder's hash): an installed pack with another is out of date. */
  version: string
  built: string
  /** Where the photo files are, relative to the index (default photos/). */
  base?: string
  source?: string
  /** A note for the preview when only part of the regions has photos (said in the panel, plainly). */
  sample?: string
  taxa: PackTaxon[]
}

/** The taxa choices, each one taking the groups of the one before. */
export type TaxaScope = 'ithomiini' | 'nymphalidae' | 'butterflies' | 'all'
export const TAXA_SCOPES: TaxaScope[] = ['ithomiini', 'nymphalidae', 'butterflies', 'all']
export const SCOPE_GROUPS: Record<TaxaScope, Group[]> = {
  ithomiini: ['ithomiini'],
  nymphalidae: ['ithomiini', 'nymphalidae'],
  butterflies: ['ithomiini', 'nymphalidae', 'butterfly'],
  all: ['ithomiini', 'nymphalidae', 'butterfly', 'moth'],
}
/** 'neotropics': the taxa recorded in any Neotropical country; else in at least one of these countries (ISO2). */
export type Region = 'neotropics' | string[]
export interface PackScope {
  taxa: TaxaScope
  region: Region
  /** Also the subspecies' photos (a species' photo always comes). */
  ssp: boolean
}
/** Ecuador, butterflies and moths, with subspecies (Franz, 11 Nov 2026): a moth like Urania leilus has its photo. */
export const DEFAULT_PACK: PackScope = { taxa: 'all', region: ['EC'], ssp: true }

export const INDEX_URL = () => new URL('identificar/packs/index.json', appRoot()).href
/** Where the phone keeps the installed pack's list (not a file on the server). */
export const INSTALLED_KEY = () => new URL('identificar/packs/installed.json', appRoot()).href
const SCOPE_KEY = 'identify:pack-scope'

const isIso = (c: unknown): c is string => typeof c === 'string' && /^[A-Z]{2}$/.test(c)
export function sameRegion(a: Region, b: Region): boolean {
  if (a === 'neotropics' || b === 'neotropics') return a === b
  return a.length === b.length && [...a].sort().join() === [...b].sort().join()
}
export const sameScope = (a: PackScope, b: PackScope) => a.taxa === b.taxa && a.ssp === b.ssp && sameRegion(a.region, b.region)

/** A scope read back (from storage or anywhere): anything unknown falls back to the default's part. */
export function cleanScope(v: unknown): PackScope {
  const o = (v && typeof v === 'object' ? v : {}) as Partial<Record<keyof PackScope, unknown>>
  const region = o.region === 'neotropics' ? 'neotropics' : Array.isArray(o.region) && o.region.length && o.region.every(isIso) ? [...new Set(o.region)] : [...(DEFAULT_PACK.region as string[])]
  return {
    taxa: TAXA_SCOPES.includes(o.taxa as TaxaScope) ? (o.taxa as TaxaScope) : DEFAULT_PACK.taxa,
    region,
    ssp: typeof o.ssp === 'boolean' ? o.ssp : DEFAULT_PACK.ssp,
  }
}
export function packScope(): PackScope {
  try {
    return cleanScope(JSON.parse(localStorage.getItem(SCOPE_KEY) || 'null'))
  } catch {
    return cleanScope(null)
  }
}
export function setPackScope(scope: PackScope) {
  try {
    localStorage.setItem(SCOPE_KEY, JSON.stringify(scope))
  } catch {
    /* private mode: the default next time */
  }
}

/** The entries of a list a scope takes: with a photo on the server (`withMissing`: also those listed but not made). */
export function packTaxa(index: Pick<PackIndex, 'taxa'>, scope: PackScope, { withMissing = false } = {}): PackTaxon[] {
  const groups = new Set(SCOPE_GROUPS[scope.taxa])
  const region = scope.region === 'neotropics' ? null : new Set(scope.region)
  return index.taxa.filter(
    // «Neotrópico»: any Neotropical country (the head is worldwide: a taxon with none recorded is in no region).
    t => t.photo && (withMissing || !t.photo.missing) && groups.has(t.group) && (scope.ssp || t.rank === 'species') && (region ? t.countries.some(c => region.has(c)) : t.countries.length > 0),
  )
}
export interface Size {
  /** Every photo of the choice (each file once) and its bytes: real, or estimated for those not on the server. */
  photos: number
  bytes: number
  /** Of those, the ones not on the server yet. */
  missing: number
  missingBytes: number
}
/** Photos and bytes of entries, each file once (a few taxa share a photo). */
export function packSize(entries: PackTaxon[]): Size {
  const files = new Map<string, [number, boolean]>()
  for (const e of entries) if (e.photo) files.set(e.photo.file, [e.bytes, !!e.photo.missing])
  let bytes = 0,
    missing = 0,
    missingBytes = 0
  for (const [b, m] of files.values()) {
    bytes += b
    if (m) {
      missing++
      missingBytes += b
    }
  }
  return { photos: files.size, bytes, missing, missingBytes }
}
/** A choice's size, the photos not on the server yet included (estimated) and counted apart. */
export const scopeSize = (index: Pick<PackIndex, 'taxa'>, scope: PackScope) => packSize(packTaxa(index, scope, { withMissing: true }))

/** Each country's size for a scope's taxa and subspecies choice, in one pass over the list (the country picker). */
export function countrySizes(index: Pick<PackIndex, 'taxa'>, scope: Pick<PackScope, 'taxa' | 'ssp'>): Map<string, Size> {
  const groups = new Set(SCOPE_GROUPS[scope.taxa])
  const files = new Map<string, Map<string, [number, boolean]>>()
  for (const t of index.taxa) {
    if (!t.photo || !groups.has(t.group) || (!scope.ssp && t.rank !== 'species')) continue
    for (const c of t.countries) {
      let m = files.get(c)
      if (!m) files.set(c, (m = new Map()))
      m.set(t.photo.file, [t.bytes, !!t.photo.missing])
    }
  }
  // Like packSize: the photos not on the server yet counted apart, so the row says «≈».
  return new Map(
    [...files].map(([c, m]) => {
      const z: Size = { photos: m.size, bytes: 0, missing: 0, missingBytes: 0 }
      for (const [b, miss] of m.values()) {
        z.bytes += b
        if (miss) {
          z.missing++
          z.missingBytes += b
        }
      }
      return [c, z]
    }),
  )
}

/** Every country the list names, Ecuador first, then by the name shown. */
export function packCountries(index: Pick<PackIndex, 'taxa'>, nameOf: (iso: string) => string = c => c): string[] {
  const all = new Set<string>()
  for (const t of index.taxa) for (const c of t.countries) all.add(c)
  return [...all].sort((a, b) => (a === 'EC' ? -1 : b === 'EC' ? 1 : nameOf(a).localeCompare(nameOf(b))))
}

/** «12.3» MB, one decimal under 10 MB, whole above, in the app's language («3,7» in Spanish). */
export function mbText(bytes: number): string {
  const mb = bytes / 1048576
  return mb < 10
    ? Math.max(0.1, Math.round(mb * 10) / 10).toLocaleString(intlLocale(), { minimumFractionDigits: 1, maximumFractionDigits: 1 })
    : Math.round(mb).toLocaleString(intlLocale())
}
/** A count in the app's language («4.852» in Spanish, «4,852» in English), whatever the phone's language. */
export const numText = (n: number) => n.toLocaleString(intlLocale())

/** What the photos a choice takes cost to save now: those not on the phone yet (the installed pack's are kept). */
export function toDownload(index: Pick<PackIndex, 'taxa'>, scope: PackScope, have: Pick<PackIndex, 'taxa'> | null | undefined): number {
  const kept = new Set((have?.taxa ?? []).flatMap(t => (t.photo ? [t.photo.file] : [])))
  const files = new Map<string, number>()
  for (const t of packTaxa(index, scope)) if (!kept.has(t.photo!.file)) files.set(t.photo!.file, t.bytes)
  return [...files.values()].reduce((a, b) => a + b, 0)
}

/**
 * Compare without a saved photo, offline: which choice in Descargas would bring one. `scope`: a choice that takes a
 * photo of the taxon the server has (its own, else a subspecies'), with what it would download; 'none': the list
 * names it but has no photo of it to download (not made yet, or recorded in no Neotropical country); 'downloads': no
 * list on the phone to tell, so only the way to Descargas; null: nothing to say.
 */
export type PackHint = { kind: 'scope'; scope: PackScope; bytes: number } | { kind: 'none' } | { kind: 'downloads' }
export function packHint(index: Pick<PackIndex, 'taxa'> | null, name: string, current: PackScope): PackHint | null {
  if (!index) return { kind: 'downloads' }
  const named = index.taxa.filter(x => x.name === name || x.species === name)
  if (!named.length) return null
  const usable = named.filter(x => x.photo && !x.photo.missing && x.countries.length)
  // The taxon's own photo first, then a subspecies' (which needs the subspecies choice).
  const entry = usable.find(x => x.name === name) ?? usable.find(x => x.rank === 'species') ?? usable[0]
  if (!entry) return named.some(x => x.photo) ? { kind: 'none' } : null
  const level = Math.max(TAXA_SCOPES.findIndex(k => SCOPE_GROUPS[k].includes(entry.group)), TAXA_SCOPES.indexOf(current.taxa))
  const region: Region = current.region !== 'neotropics' && entry.countries.some(c => current.region.includes(c)) ? [...current.region] : 'neotropics'
  const scope: PackScope = { taxa: TAXA_SCOPES[level], region, ssp: current.ssp || entry.rank === 'subspecies' }
  const z = scopeSize(index, scope)
  return { kind: 'scope', scope, bytes: z.bytes - z.missingBytes }
}

/** The photo's URL: its file under the list's base (relative to the list's own URL). */
export const photoSrc = (index: Pick<PackIndex, 'base'>, indexUrl: string, file: string) => new URL(file, new URL(index.base ?? 'photos/', indexUrl)).href

// ---------------------------------------------------------------- the server's list and the installed pack

let onlineMemo: Promise<PackIndex> | null = null
/** The server's whole list (asked once per page; with no signal the service worker may answer with the last one seen). */
export function onlineIndex(): Promise<PackIndex> {
  onlineMemo ??= fetch(INDEX_URL(), { credentials: 'same-origin', cache: 'no-cache' })
    .then(r => (r.ok ? (r.json() as Promise<PackIndex>) : Promise.reject(new Error(`HTTP ${r.status}`))))
    .then(v => (Array.isArray(v?.taxa) ? v : Promise.reject(new Error('not a pack list'))))
    .catch(err => {
      onlineMemo = null
      throw err
    })
  return onlineMemo
}
/** Forget the memo (tests, a manual «check again»). */
export const resetOnlineIndex = () => (onlineMemo = null)

export interface Installed {
  scope: PackScope
  version: string
  /** Photos and bytes on the phone. */
  photos: number
  bytes: number
  at: string
  /** Photos that could not be saved (the next update takes them). */
  failed: number
}
/** The installed pack: its list (only the taxa saved, `base` absolute) and what was saved. */
export interface InstalledPack {
  index: PackIndex
  installed: Installed
}

let installedMemo: Promise<InstalledPack | null> | null = null
export function installedPack(): Promise<InstalledPack | null> {
  installedMemo ??= (async () => {
    try {
      const r = await (await caches.open(CACHE)).match(INSTALLED_KEY())
      if (!r) return null
      const v = (await r.json()) as InstalledPack
      return v?.index?.taxa && v.installed ? v : null
    } catch {
      return null
    }
  })()
  return installedMemo
}

/** Whether the server's list is newer than the installed pack's. */
export const updateAvailable = (installed: Installed | null | undefined, online: Pick<PackIndex, 'version'> | null | undefined) =>
  !!installed && !!online && installed.version !== online.version

/** Runs `fn` over `items`, `n` at a time. */
async function pool<T>(items: T[], n: number, fn: (item: T) => Promise<void>) {
  let next = 0
  await Promise.all(Array.from({ length: Math.min(n, items.length) }, async () => {
    while (next < items.length) await fn(items[next++])
  }))
}

/** What a save did: the pack now on the phone, and whether the new choice took (all its photos saved). */
export interface InstallResult {
  /** The pack on the phone after the save: the new one, or the one before when the new one did not complete. */
  pack: InstalledPack | null
  /** Photos of the new choice that could not be saved, of `total`. */
  failed: number
  total: number
  /** The new choice did not complete and the pack before it stays (its record and its photos). */
  kept: boolean
}
const isQuota = (err: unknown) => /quota/i.test(`${(err as Error)?.name} ${(err as Error)?.message}`)
/** Every photo key the phone has under `base` (a pack's photo folder; lists are not photos). */
async function keysUnder(cache: Cache, base: string): Promise<Request[]> {
  return (await cache.keys()).filter(r => r.url.startsWith(base) && !/\.json(\?|$)/.test(r.url))
}

/**
 * Saves the photos `scope` takes from the server's list on this phone. Photos already on the phone are not
 * downloaded again (a changed photo has another name). Only when
 * every photo of the new choice is saved does it replace the pack before (its record, and the photos it no longer
 * takes, with any other photo left under the folders); otherwise the pack before stays as it was and the result says
 * how many failed, for a retry. The first pack on a phone is kept even when some photos failed (Update completes it).
 * A full phone stops the save at once with an Error('quota'), the pack before untouched.
 */
export async function installPack(index: PackIndex, scope: PackScope, progress: (done: number, total: number) => void, indexUrl = INDEX_URL()): Promise<InstallResult> {
  const take = packTaxa(index, scope)
  const cache = await caches.open(CACHE)
  const src = (t: PackTaxon) => photoSrc(index, indexUrl, t.photo!.file)
  const want = new Map<string, number>()
  for (const t of take) want.set(src(t), t.bytes)
  const files = [...want.keys()]
  const ok = new Map<string, number>()
  let done = 0
  let quota: unknown = null
  progress(0, files.length)
  await pool(files, 4, async u => {
    if (quota) return
    try {
      // A photo's name holds a hash of its bytes (scripts/photo-packs.py): one on the phone is the right one.
      let r = await cache.match(u)
      let size = want.get(u) ?? 0
      if (!r) {
        const got = await fetch(u, { credentials: 'same-origin', cache: 'reload' })
        if (got.ok && (got.headers.get('content-type') ?? 'image/').startsWith('image/')) {
          size = (await got.clone().blob()).size
          await cache.put(u, got)
          r = got
        }
      }
      if (r) ok.set(u, size)
    } catch (err) {
      if (isQuota(err)) quota = err
      /* otherwise counted as failed: a retry takes it */
    }
    progress(++done, files.length)
  })
  if (quota) throw new Error('quota')
  const before = await installedPack()
  const failed = files.length - ok.size
  // Not complete over a pack, or nothing saved at all: nothing recorded (the pack before, if any, stays).
  if ((before && failed) || (files.length && !ok.size)) return { pack: before, failed, total: files.length, kept: true }
  const base = new URL(index.base ?? 'photos/', indexUrl).href
  const pack: InstalledPack = {
    index: { ...index, base, taxa: take.filter(t => ok.has(src(t))) },
    installed: {
      scope: { ...scope, region: scope.region === 'neotropics' ? 'neotropics' : [...scope.region] },
      version: index.version,
      photos: ok.size,
      bytes: [...ok.values()].reduce((a, b) => a + b, 0),
      at: new Date().toISOString(),
      failed,
    },
  }
  // The new record first: if it cannot be written (a full phone, the page closed), the record before still points
  // at its photos, all still there.
  try {
    await cache.put(INSTALLED_KEY(), new Response(JSON.stringify(pack), { headers: { 'content-type': 'application/json' } }))
  } catch (err) {
    throw isQuota(err) ? new Error('quota') : err
  }
  installedMemo = Promise.resolve(pack)
  // Then whatever else is under the folders goes (the pack before's other photos, photos seen online, photos left
  // by a Delete cut short); cut short itself, the next save takes them.
  const bases = new Set([base, ...(before ? [before.index.base ?? base] : [])])
  for (const b of bases) await pool((await keysUnder(cache, b)).filter(r => !ok.has(r.url)), 8, async r => void (await cache.delete(r)))
  return { pack, failed, total: files.length, kept: false }
}

/**
 * The installed pack off the phone: its record first (a Delete cut short leaves no pack that says it has photos it
 * lost), then every photo under its folder (also photos seen online there), several at a time.
 */
export async function removePack(): Promise<void> {
  const before = await installedPack()
  const cache = await caches.open(CACHE)
  await cache.delete(INSTALLED_KEY())
  installedMemo = Promise.resolve(null)
  if (before) await pool(await keysUnder(cache, before.index.base ?? new URL('photos/', INDEX_URL()).href), 8, async r => void (await cache.delete(r)))
}

// ---------------------------------------------------------------- photos of a taxon, for Compare and the search

export interface TaxonPhoto {
  /** The taxon the photo shows. */
  name: string
  rank: Rank
  src: string
  attribution: string
  license: string
  source: string | null
  taxonUrl: string | null
}
/** A list ready to look names up in: by name, and each species' subspecies. */
export interface PackLookup {
  byName: Map<string, PackTaxon>
  sspOf: Map<string, PackTaxon[]>
  src: (t: PackTaxon) => string
}
export function lookup(index: PackIndex, indexUrl: string): PackLookup {
  const byName = new Map<string, PackTaxon>()
  const sspOf = new Map<string, PackTaxon[]>()
  for (const t of index.taxa) {
    byName.set(t.name, t)
    if (t.rank === 'subspecies') sspOf.set(t.species, [...(sspOf.get(t.species) ?? []), t])
  }
  for (const list of sspOf.values()) list.sort((a, b) => a.name.localeCompare(b.name))
  return { byName, sspOf, src: t => photoSrc(index, indexUrl, t.photo!.file) }
}
export const taxonPhoto = (l: PackLookup, t: PackTaxon): TaxonPhoto | null =>
  t.photo && !t.photo.missing ? { name: t.name, rank: t.rank, src: l.src(t), attribution: t.photo.attribution, license: t.photo.license, source: t.photo.source, taxonUrl: t.photo.taxonUrl } : null
/** A species' photo then its subspecies' photos (those the list has a photo of). */
export function speciesPhotos(l: PackLookup, species: string): TaxonPhoto[] {
  const own = l.byName.get(species)
  return [own, ...(l.sspOf.get(species) ?? [])].flatMap(t => (t ? (taxonPhoto(l, t) ?? []) : []))
}

/**
 * Where photos come from now: the installed pack, and with signal the server's whole list after it (online beyond
 * the pack). Without signal only the pack (the server's photos would not load).
 */
export async function packSources(online = navigator.onLine): Promise<PackLookup[]> {
  return [...(await installedSources()), ...(online ? await onlineSources() : [])]
}
/** The installed pack as a source (from the phone's cache: no network). Each list is indexed once per version. */
let installedLookup: { pack: InstalledPack; l: PackLookup } | null = null
export async function installedSources(): Promise<PackLookup[]> {
  const pack = await installedPack()
  if (!pack) return []
  if (installedLookup?.pack !== pack) installedLookup = { pack, l: lookup(pack.index, INSTALLED_KEY()) }
  return [installedLookup.l]
}
let onlineLookup: { index: PackIndex; l: PackLookup } | null = null
/** The server's whole list as a source ([] when it cannot be read). */
export async function onlineSources(): Promise<PackLookup[]> {
  const index = await onlineIndex().catch(() => null)
  if (!index) return []
  if (onlineLookup?.index !== index) onlineLookup = { index, l: lookup(index, INDEX_URL()) }
  return [onlineLookup.l]
}
/** speciesPhotos from the first source that has the species' photo, its subspecies from any (each photo once). */
export function photosFrom(sources: PackLookup[], species: string): TaxonPhoto[] {
  const seen = new Set<string>()
  const out: TaxonPhoto[] = []
  for (const s of sources)
    for (const p of speciesPhotos(s, species))
      if (!seen.has(p.name)) {
        seen.add(p.name)
        out.push(p)
      }
  // The species' own photo first, then its subspecies by name.
  return out.sort((a, b) => (a.rank === b.rank ? (a.rank === 'species' ? 0 : a.name.localeCompare(b.name)) : a.rank === 'species' ? -1 : 1))
}
