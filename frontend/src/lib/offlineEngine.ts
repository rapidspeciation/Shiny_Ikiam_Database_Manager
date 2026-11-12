/**
 * The offline identifier without its page: the models' list, download (Cache Storage, 32 MB parts kept in 4 MB
 * pieces as they come; a dropped signal is waited for and the download goes on from its last byte, lib/resumable.ts),
 * the frozen-stock snapshot, loading the models and running a photo (wing detector -> crop -> encoder -> the
 * feature). Used by the Identificar tab (views/IdentifyView.vue). It keeps the test page's cache and URLs
 * (/prueba-offline/models/…, src/offline/main.ts): a phone that downloaded there does not download again.
 * Signed in, the models come from the server; without an account (or when the server says to sign in) from
 * Hugging Face (lib/publicModels.ts), the same files: either counts as the other (offlineId.ts sameFile).
 * The scores come from the feature (lib/offlineId.ts speciesScores), so changing the side of the Andes needs no new run.
 */
import type * as Ort from 'onnxruntime-web'
import {
  chooseModel,
  cropBox,
  detectorInput,
  encoderInput,
  ENCODER_ROLE,
  exifOrientation,
  letterboxCrop,
  makeHead,
  MODEL_ROLES,
  adoptVersions,
  isCurrent,
  offerFor,
  partKey,
  rolesStored,
  readMasks,
  roleOf,
  selectMasks,
  storedName,
  studentInput,
  studentSizeOf,
  type FileRole,
  type Head,
  type HeadMeta,
  type ModelKind,
  type Rgba,
  type StoredFile,
} from './offlineId'
import type { Photo } from './offlinePhoto'
import type { FrozenStock } from './frozenStock'
import { PUBLIC_FILES } from './publicModels'
import { parseCalibration, SEX_DEFAULT, SEX_FILE, SEX_ROLES, sexInput, type SexCalibration, type SexModelInfo } from './sexModel'
import { Sha256 } from './sha256'
import { contentRange, DownloadError, request, retry, statusError } from './resumable'

type OrtModule = typeof Ort
/**
 * `url`: a public copy (Hugging Face) fetched in ranges; else the server's parts. `parts`: each part's SHA-256.
 * `version` and `sha256`: the model version offered and the whole file's SHA-256 (checked before the file is taken).
 * `calibration`: the sex model's (lib/sexModel.ts parseCalibration).
 */
export interface ModelFile {
  name: string
  bytes: number
  etag: string
  role: FileRole
  wire?: number
  encoding?: string | null
  url?: string
  parts?: string[]
  version?: string
  sha256?: string
  calibration?: unknown
  /** The model's notes for Descargas (its manifest's: measured facts), per language. */
  notes?: { es?: string; en?: string }
  /** The small model's input size (lib/offlineId.ts studentSizeOf). */
  input?: { size: number; values?: string }
}
/**
 * What the list offers for a model against the phone (lib/offlineId.ts offerFor): `state` 'none' (not offered),
 * 'missing' (a first download), 'update' (a newer version than the phone's), 'current'; the bytes to download (plain,
 * on the wire) and the whole model's; the installed and the offered version of its main file (the encoder, the
 * small model or the sex model; null: not known).
 */
export interface Offer {
  state: 'none' | 'missing' | 'update' | 'current'
  plain: number
  wire: number
  total: number
  from: string | null
  to: string | null
  /** The main file's size on the phone and on the list (null: none). */
  fromBytes: number | null
  toBytes: number | null
  /** The listed main file's notes (its manifest's), per language. */
  notes: { es?: string; en?: string } | null
}
export interface Stored { files: Record<string, StoredFile>; at: string }

export const CACHE = 'ithomiini-prueba-offline'
/**
 * The list this build asks for: the versioned one (server/offline-id.mjs LIST_CAPS), which may offer models a build
 * from before versions cannot run (the small model at another input size, a sex model under another name).
 */
export const LIST_PATH = 'models/list.json?caps=versions'
export const PART = 32 * 1024 * 1024
/**
 * A part comes in pieces of this size, each kept on the phone as it arrives (pieceKey), so a download cut off by a
 * reload or a closed browser goes on from its last piece; the part replaces its pieces once whole and checked.
 */
export const PIECE = 4 * 1024 * 1024
export const pieceKey = (name: string, i: number, slot: string | undefined, k: number) => `${partKey(name, i, slot)}&piece=${k}`
/** A download's progress: bytes done, bytes in all, and 'waiting' while the connection is down. */
export type Progress = (done: number, total: number, state?: 'waiting') => void
interface Inflight {
  listeners: Set<Progress>
  last: [number, number, ('waiting' | undefined)?] | null
  promise: Promise<void>
  /** Ends it (the model removed, signed out): DownloadError('cancelled'). */
  stop: AbortController
}
/**
 * The downloads running on this page, by folder and roles: another engine of the page (Revisión's sex estimate, the
 * view's own) that starts the same download joins it instead of downloading it twice.
 */
const inflight = new Map<string, Inflight>()
/** models/begun.json written one change at a time. */
let begunWrites: Promise<void> = Promise.resolve()
export const KINDS: ModelKind[] = ['small', 'full']
const ORT_FILES = ['ort.wasm.min.mjs', 'ort-wasm-simd-threaded.mjs', 'ort-wasm-simd-threaded.wasm']
const ORT_GPU_FILES = ['ort.webgpu.min.mjs', 'ort-wasm-simd-threaded.asyncify.mjs', 'ort-wasm-simd-threaded.asyncify.wasm']

/** What the list call found: the files and where from, or why there are none (no signal, the server's answer). */
export type Listing = { files: ModelFile[]; source: 'server' | 'public' } | { error: 'signin' | 'offline' | 'server'; status?: number }

/** The Hugging Face copies as a listing (no request: the list is part of the page). */
export const publicListing = (): ModelFile[] =>
  PUBLIC_FILES.map(f => ({
    name: f.name,
    bytes: f.bytes,
    etag: `"sha256-${f.sha256}"`,
    role: f.role,
    wire: f.bytes,
    encoding: null,
    url: f.url,
    parts: f.parts,
    version: f.version ?? contentVersion(f.sha256),
    sha256: f.sha256,
    ...(f.input ? { input: f.input } : {}),
    ...(f.notes ? { notes: f.notes } : {}),
  }))

/** A file's version when its source names none: its content (the server's default too, server/offline-id.mjs). */
export const contentVersion = (sha256: string) => `sha256-${sha256.slice(0, 12)}`

async function sha256Hex(data: BufferSource): Promise<string> {
  return [...new Uint8Array(await crypto.subtle.digest('SHA-256', data))].map(b => b.toString(16).padStart(2, '0')).join('')
}

/** What a photo gave: the feature for the head, the square the AI looked at (224 px RGB) and how it was cut. */
export interface RunResult {
  feature: Float32Array
  square: Uint8Array
  status: 'success' | 'centre_crop_fallback' | string
  ms: number
}

export interface Engine {
  base: string
  list(): Promise<Listing>
  stored(): Promise<Stored | null>
  /**
   * Whether each model can run from what is on the phone (whatever its version: an older one works until its update
   * is in). With a list, files kept before versions existed that are the listed bytes get their version recorded.
   */
  ready(files: ModelFile[] | null): Promise<Record<ModelKind, boolean>>
  /** What the list offers for a model or the sex model against what is stored (sizes and versions). */
  offer(which: ModelKind | 'sex', files: ModelFile[], stored: Stored | null): Offer
  /** What a model still needs to download: plain bytes (space on the phone) and on the wire (mobile data). */
  toDownload(kind: ModelKind, files: ModelFile[], stored: Stored | null): { plain: number; wire: number; total: number }
  /**
   * Downloads what a model lacks or the newer version the list offers. Each new file is kept beside the old one,
   * checked (each part's hash when listed, then the whole file's SHA-256), and only then replaces it in one write of
   * stored.json: until that moment the phone keeps using the old version, and a cut or a bad file leaves it as it was.
   * No signal is no failure: the progress says 'waiting' and the download goes on by itself once the network is back
   * (from its last byte); a reload goes on from the pieces kept (saved, «Continuar»). A second call while it runs
   * joins it. The progress reaches the total before the check (the view says «Comprobando…» then).
   */
  download(kind: ModelKind, files: ModelFile[], progress: Progress): Promise<void>
  /** A download of this model or of the sex model is running on this page (download() joins it). */
  busy(which: ModelKind | 'sex'): boolean
  /** What a cut download left on the phone toward the list's version (bytes done of the bytes it needs): «Continuar». */
  saved(which: ModelKind | 'sex', files: ModelFile[]): Promise<{ done: number; total: number }>
  /** The team's sex estimate (lib/sexModel.ts): whether its files are on the phone, their size, the download. */
  sexReady(files?: ModelFile[] | null): Promise<boolean>
  /** The sex model on the phone: its file, version and calibration; null when there is none. */
  sexInfo(): Promise<SexModelInfo | null>
  /** The sex model that gave the last sexLogit (its calibration reads that score), or null. */
  sexLoaded(): SexModelInfo | null
  sexToDownload(files: ModelFile[], stored: Stored | null): { plain: number; wire: number; total: number }
  downloadSex(files: ModelFile[], progress: Progress): Promise<void>
  /** The wing detector's 224 px square of a photo (no encoder needed). */
  square(photo: Photo): Promise<{ square: Uint8Array; status: string }>
  /** The sex model's logit for a 224 px square. */
  sexLogit(square: Uint8Array): Promise<number>
  /** Removes the sex model from this phone (signed out: it is the team's). */
  forgetSex(): Promise<void>
  /** Whether this page, its scripts and the runtime are on the phone (it opens without signal); online, keeps them first. */
  pageOffline(): Promise<boolean>
  remove(): Promise<void>
  /**
   * One identifier model off the phone: its own files, and the detector and head too when nothing else on the phone
   * needs them (the other model, the sex model). The photo packs and the rest of the cache stay.
   */
  removeModel(kind: ModelKind): Promise<void>
  /** The models in memory for `kind`; the first time reads them from storage (the slow step). */
  load(kind: ModelKind): Promise<void>
  loaded(kind: ModelKind): boolean
  /** Goes up by one each time something is read into memory (the runtime, the detector, an encoder, the head, the sex
   * model): a photo whose phase saw it move had to load (lib/identifyTiming.ts timed), whatever the durations. */
  loadGeneration(): number
  run(photo: Photo, kind: ModelKind): Promise<RunResult>
  head(): Head | null
  loadHead(): Promise<Head>
  stock(): Promise<{ stock: FrozenStock; asOf: string } | null>
  refreshStock(): Promise<void>
  /** Removes the team's inventory kept on this phone (someone without an account is using it). */
  forgetStock(): Promise<void>
  lastMs: Record<string, number>
}

/**
 * `base`: the test page's folder (absolute, ending in /prueba-offline/), where the models, the runtime and the cache
 * keys live. `pageFiles`: the page's own files to keep for airplane mode (its document, scripts and styles).
 * `signedIn`: whether a team member is signed in (the server's models and inventory); false: Hugging Face's models,
 * no request to the team's API.
 */
export function createEngine(
  base: string,
  pageFiles: () => string[] = () => [],
  { signedIn = () => true, runtime }: { signedIn?: () => boolean; /** Tests: a stand-in for the runtime's import. */ runtime?: (url: string) => Promise<OrtModule> } = {},
): Engine {
  const at = (path: string) => new URL(path, base).href
  const threads = self.crossOriginIsolated ? Math.max(1, Math.min(4, navigator.hardwareConcurrency || 4)) : 1
  const lastMs: Record<string, number> = {}
  const stockKey = at('models/frozen-stock.json')

  async function stored(): Promise<Stored | null> {
    const r = await (await caches.open(CACHE)).match(at('models/stored.json'))
    return r ? r.json() : null
  }

  async function list(): Promise<Listing> {
    if (!navigator.onLine) return { error: 'offline' }
    if (!signedIn()) return { files: publicListing(), source: 'public' }
    const r = await fetch(at(LIST_PATH), { credentials: 'same-origin', cache: 'no-store' }).catch(() => null)
    if (!r) return { error: 'offline' }
    // The session ended meanwhile: the public copies.
    if (r.status === 401) return { files: publicListing(), source: 'public' }
    if (!r.ok) return { error: 'server', status: r.status }
    const body = (await r.json()) as { files: ModelFile[]; partBytes?: number }
    if (body.partBytes && body.partBytes !== PART) return { error: 'server', status: 409 }
    return { files: body.files, source: 'server' }
  }

  /**
   * What a file keeps beside its version: the sex model's calibration, the small model's input size. Written in the
   * same stored.json as the file, so an update swaps the model and these together (and a cut keeps the old pair).
   */
  const extraOf = (f: ModelFile): Partial<StoredFile> =>
    f.role === 'sex' && f.calibration ? { calibration: f.calibration } : f.role === 'student' && f.input ? { input: { size: f.input.size } } : {}

  const putStored = async (cache: Cache, s: Stored) =>
    cache.put(at('models/stored.json'), new Response(JSON.stringify(s), { headers: { 'content-type': 'application/json' } }))

  async function ready(files: ModelFile[] | null) {
    const s = await stored()
    // Files kept before versions existed (or before the lists said their part hashes) that are the very bytes listed:
    // the version, SHA-256 and part hashes are recorded, so they count as that version (and as the public copy).
    if (s && files) {
      const sex = sexFiles(files)
      const listed = [...files.filter(f => f.role !== 'sex'), ...sex]
      if (adoptVersions(s.files, listed, f => extraOf(f as ModelFile))) await putStored(await caches.open(CACHE), s)
    }
    return { full: rolesStored(MODEL_ROLES.full, s?.files ?? null), small: rolesStored(MODEL_ROLES.small, s?.files ?? null) }
  }

  function toDownload(kind: ModelKind, files: ModelFile[], s: Stored | null) {
    return roleSizes(MODEL_ROLES[kind], files, s)
  }

  function roleSizes(roles: FileRole[], files: ModelFile[], s: Stored | null) {
    const wanted = files.filter(f => roles.includes(f.role))
    const need = wanted.filter(f => !isCurrent(s?.files[f.name], f))
    return {
      plain: need.reduce((a, f) => a + f.bytes, 0),
      wire: need.reduce((a, f) => a + (f.wire ?? f.bytes), 0),
      total: wanted.reduce((a, f) => a + f.bytes, 0),
    }
  }

  function offer(which: ModelKind | 'sex', files: ModelFile[], s: Stored | null): Offer {
    const roles = which === 'sex' ? SEX_ROLES : MODEL_ROLES[which]
    const list = which === 'sex' ? sexFiles(files) : files
    const { state } = offerFor(roles, s?.files ?? null, list)
    const main: FileRole = which === 'sex' ? 'sex' : ENCODER_ROLE[which]
    const have = s && storedName(s.files, main)
    const listed = list.find(f => f.role === main)
    return {
      state,
      ...roleSizes(roles, list, s),
      from: have ? (s.files[have].version ?? null) : null,
      to: listed?.version ?? null,
      fromBytes: have ? s.files[have].bytes : null,
      toBytes: listed?.bytes ?? null,
      notes: listed?.notes ?? null,
    }
  }

  /** The page, its scripts and the runtime, for airplane mode (the service worker serves them from this cache). */
  async function keepPage(cache: Cache, signal?: AbortSignal, flowing?: () => void) {
    const ort = [...ORT_FILES, ...('gpu' in navigator ? ORT_GPU_FILES : [])].map(f => at(`ort/${f}`))
    for (const u of [...pageFiles(), ...ort]) {
      // Watched like the models' parts (a stall or `offline` is a network error, tried again; lib/resumable.ts).
      await request(
        u,
        { cache: 'no-cache' },
        async (r, read) => {
          // A refusal or a server error is the caller's (retry tries a server error again).
          if (!r.ok) throw statusError(r.status, []) ?? new Error(`${u}: HTTP ${r.status}`)
          const parts: Uint8Array[] = []
          for (;;) {
            const { done, value } = await read()
            if (done) break
            parts.push(value)
          }
          // Its own headers (type, the page's isolation headers for airplane mode), less those of the bytes on the wire.
          const headers = new Headers(r.headers)
          headers.delete('content-encoding')
          headers.delete('content-length')
          await cache.put(u, new Response(new Blob(parts as BlobPart[]), { status: r.status, statusText: r.statusText, headers }))
        },
        signal,
        flowing,
      )
    }
  }

  async function pageOffline(): Promise<boolean> {
    const cache = await caches.open(CACHE)
    const urls = [...pageFiles(), ...ORT_FILES.map(f => at(`ort/${f}`))]
    const have = async () => (await Promise.all(urls.map(u => cache.match(u)))).every(Boolean)
    if (await have()) return true
    if (!navigator.onLine) return false
    try {
      await keepPage(cache)
    } catch {
      return false
    }
    return have()
  }

  /** The list again, after a file changed on the server meanwhile (no list: the network, tried again). */
  async function relist(): Promise<ModelFile[]> {
    const l = await list()
    if ('error' in l) throw new DownloadError(l.error === 'server' ? 'server' : l.error === 'offline' ? 'network' : 'refused', l.error)
    return l.files
  }
  async function download(kind: ModelKind, files: ModelFile[], progress: Progress) {
    return downloadRoles(kind, MODEL_ROLES[kind], files, progress, relist)
  }

  /**
   * The list with only a sex model the phone can use: one with its SHA-256 (the whole file is checked against it) and
   * a valid calibration (lib/sexModel.ts parseCalibration); model B's bytes also get its part hashes.
   */
  function sexFiles(files: ModelFile[]): ModelFile[] {
    return files.flatMap(f => {
      if (f.role !== 'sex') return [f]
      if (!f.sha256 || !/^[0-9a-f]{64}$/.test(f.sha256) || !parseCalibration(f.calibration)) return []
      return [f.sha256 === SEX_FILE.sha256 && f.bytes === SEX_FILE.bytes && !f.parts ? { ...f, parts: SEX_FILE.parts } : f]
    })
  }

  /** Every cached part of a file (any slot). */
  async function partsOf(cache: Cache, name: string): Promise<Request[]> {
    const path = new URL(at(`models/${name}`)).pathname
    return (await cache.keys()).filter(r => {
      const u = new URL(r.url)
      return u.pathname === path && u.searchParams.has('part')
    })
  }

  /**
   * Removes every cached part whose file has one of these roles (roleOf by its name), except files named in `keep`
   * (those still in stored.json): what a cut download or update left under a name stored.json never had.
   */
  async function dropRoles(cache: Cache, roles: Set<FileRole>, keep: Set<string> = new Set()) {
    const models = new URL(at('models/')).pathname
    for (const r of await cache.keys()) {
      const u = new URL(r.url)
      if (!u.pathname.startsWith(models) || !u.searchParams.has('part')) continue
      const name = decodeURIComponent(u.pathname.slice(models.length))
      if (!keep.has(name) && roles.has(roleOf(name))) await cache.delete(r)
    }
  }

  /**
   * Removes a file's parts: all, or all but those of the slot kept (`keep.slot` undefined: the plain keys). Pieces go
   * in any case (a page closed between a part's write and its pieces' removal left them).
   */
  async function dropParts(cache: Cache, name: string, keep?: { slot?: string }) {
    for (const r of await partsOf(cache, name)) {
      const q = new URL(r.url).searchParams
      if (keep && (q.get('v') ?? undefined) === keep.slot && !q.has('piece')) continue
      await cache.delete(r)
    }
  }

  /** Whether every part of a stored file is in the cache with its length. */
  async function complete(cache: Cache, name: string, f: StoredFile) {
    for (let i = 0; i * PART < f.bytes; i++) {
      const have = await cache.match(at(partKey(name, i, f.slot)))
      if (!have || Number(have.headers.get('x-bytes')) !== Math.min(f.bytes, (i + 1) * PART) - i * PART) return false
    }
    return true
  }

  /** The SHA-256 of a file as stored (its parts read one at a time); '' when a part is missing. */
  async function storedSha256(cache: Cache, name: string, bytes: number, slot?: string) {
    const h = new Sha256()
    for (let i = 0; i * PART < bytes; i++) {
      const r = await cache.match(at(partKey(name, i, slot)))
      if (!r) return ''
      h.update(new Uint8Array(await r.arrayBuffer()))
    }
    return h.hex()
  }

  /** A session no longer the current version: dropped now, released a little later (a photo may still be running). */
  const retire = (x: Ort.InferenceSession | null | undefined) => x && setTimeout(() => x.release().catch(() => {}), 60_000)
  function forgetLoaded(roles: FileRole[]) {
    if (roles.includes('detector')) {
      retire(detector)
      detector = null
      detectorOnGpu = false
    }
    for (const k of KINDS)
      if (roles.includes(ENCODER_ROLE[k])) {
        retire(encoders[k])
        delete encoders[k]
        delete encoderSize[k]
      }
    if (roles.includes('head') || roles.includes('head-meta')) headLoaded = null
    if (roles.includes('sex')) {
      retire(sexSession)
      sexSession = null
      sexSessionInfo = null
    }
  }

  /** Where a file's new bytes go: its own keys beside an older version under the same name (newSlot), else its own. */
  function slotFor(f: ModelFile, old: StoredFile | undefined) {
    return old && !isCurrent(old, f) ? newSlot(f, old) : old?.slot
  }

  /** The pieces of part `i` kept on the phone (from its start, of this listed file): their bytes and the source's ETag. */
  async function keptPieces(cache: Cache, f: ModelFile, i: number, slot: string | undefined, len: number) {
    const blobs: Blob[] = []
    let src: string | null = null
    for (let k = 0; (k + 1) * PIECE < len; k++) {
      const r = await cache.match(at(pieceKey(f.name, i, slot, k)))
      if (!r || r.headers.get('x-etag') !== f.etag || Number(r.headers.get('x-bytes')) !== PIECE) break
      blobs.push(await r.blob())
      src = r.headers.get('x-src') || src
    }
    return { blobs, src }
  }
  async function dropPieces(cache: Cache, name: string, i: number, slot: string | undefined, len: number) {
    for (let k = 0; k * PIECE < len; k++) await cache.delete(at(pieceKey(name, i, slot, k)))
  }

  /** The downloads begun and not finished on this phone (models/begun.json), by model. */
  const begunKey = () => at('models/begun.json')
  async function begun(): Promise<Partial<Record<ModelKind | 'sex', string>>> {
    try {
      return (await (await (await caches.open(CACHE)).match(begunKey()))?.json()) ?? {}
    } catch {
      return {}
    }
  }
  function markBegun(which: ModelKind | 'sex', on: boolean) {
    // One write at a time (the sex model and an identifier model may start or end together).
    begunWrites = begunWrites.then(async () => {
      // Never in the way of the download itself (a full phone says so where the bytes go).
      try {
        const now = await begun()
        if (!!now[which] === on) return
        if (on) now[which] = new Date().toISOString()
        else delete now[which]
        await (await caches.open(CACHE)).put(begunKey(), new Response(JSON.stringify(now), { headers: { 'content-type': 'application/json' } }))
      } catch {
        /* «Continuar» is not offered for it after a reload; the download goes on */
      }
    })
    return begunWrites
  }

  /** Bytes of a listed file already on the phone toward its download: whole parts and kept pieces. */
  async function savedOf(cache: Cache, f: ModelFile, slot: string | undefined) {
    let bytes = 0
    for (let i = 0; i * PART < f.bytes; i++) {
      const len = Math.min(f.bytes, (i + 1) * PART) - i * PART
      const part = await cache.match(at(partKey(f.name, i, slot)))
      if (part && part.headers.get('x-etag') === f.etag && Number(part.headers.get('x-bytes')) === len) bytes += len
      else bytes += (await keptPieces(cache, f, i, slot, len)).blobs.length * PIECE
    }
    return bytes
  }

  /** What a download cut off (page closed, phone restarted) left of a model's update or first download: «Continuar». */
  async function saved(which: ModelKind | 'sex', files: ModelFile[]): Promise<{ done: number; total: number }> {
    if (!(await begun())[which]) return { done: 0, total: 0 }
    const roles = which === 'sex' ? SEX_ROLES : MODEL_ROLES[which]
    const list = (which === 'sex' ? sexFiles(files) : files).filter(f => roles.includes(f.role))
    const before = await stored()
    const cache = await caches.open(CACHE)
    let done = 0, total = 0
    for (const f of list) {
      const old = before?.files[f.name]
      if (isCurrent(old, f) && (await complete(cache, f.name, old!))) continue
      total += f.bytes
      done += await savedOf(cache, f, slotFor(f, old))
    }
    return { done, total }
  }

  /**
   * One part into the cache, resumed from the last good byte: the pieces a cut download kept (PIECE each, also after
   * a reload) and, within this page, every byte that came. The server's part is asked whole (compressed on the wire)
   * and, after a cut, for the rest with a Range (206); Hugging Face always by range of the file. A changed file (its
   * ETag, Content-Range or the server's part ETag) is a DownloadError('changed'); a part whose hash is wrong is
   * downloaded again (three times, then 'corrupt').
   */
  async function fetchPart(cache: Cache, f: ModelFile, i: number, slot: string | undefined, count: (n: number) => void, waiting: (on: boolean) => void, signal: AbortSignal) {
    const start = i * PART, end = Math.min(f.bytes, start + PART) - 1, len = end - start + 1
    const kept = await keptPieces(cache, f, i, slot, len)
    let pieces = kept.blobs
    let tail: BlobPart[] = []
    let tailBytes = 0
    let src = kept.src ?? sources.get(f.name) ?? null
    const got = () => pieces.length * PIECE + tailBytes
    const reset = async () => {
      count(-got())
      pieces = []
      tail = []
      tailBytes = 0
      await dropPieces(cache, f.name, i, slot, len)
    }
    count(got())
    let copies = 0
    async function requestPart(flowing: () => void): Promise<'again' | void> {
      if (got() === len) return
      const from = got()
      const url = f.url ?? at(partKey(f.name, i))
      const init: RequestInit = f.url
        ? { headers: { range: `bytes=${start + from}-${end}` }, credentials: 'omit', cache: 'no-store', mode: 'cors' }
        : { credentials: 'same-origin', cache: 'no-store', ...(from ? { headers: { range: `bytes=${from}-` } } : {}) }
      return request(url, init, async (r, read): Promise<'again' | void> => {
        if (f.url) {
          const whole = r.status === 200 && start === 0 && end === f.bytes - 1 && from === 0
          const fail = whole ? null : statusError(r.status, [206])
          if (fail) throw fail
          const range = contentRange(r.headers.get('content-range'))
          if (r.status === 206 && (!range || range.start !== start + from || range.end !== end || range.total !== f.bytes)) throw new DownloadError('changed', 'changed-on-server')
          // The same copy as the bytes kept (the CDN's ETag), or this part again from its first byte: the URL is pinned
          // to a commit and every part has its hash, so another ETag is another copy of the same file (a CDN edge
          // after a network switch, a storage move), never mixed into one part.
          const tag = r.headers.get('etag')
          if (tag && src && tag !== src && from) {
            await reset()
            sources.set(f.name, (src = tag))
            if (++copies > 3) throw new DownloadError('changed', 'changed-on-server')
            return 'again'
          }
          if (tag) sources.set(f.name, (src = tag))
        } else {
          const fail = statusError(r.status, [200, 206])
          if (fail) throw fail
          // The part's ETag is the listed file's: a model changed since the list is not mixed in.
          if (r.headers.get('etag') !== `W/${f.etag.slice(0, -1)}-p${i}"` || Number(r.headers.get('x-part-bytes')) !== len) throw new DownloadError('changed', 'changed-on-server')
          if (r.status === 206) {
            const range = contentRange(r.headers.get('content-range'))
            if (!range || range.start !== from || range.end !== len - 1 || range.total !== len) throw new DownloadError('changed', 'changed-on-server')
          } else if (from) await reset() // a server that does not resume a part (before 12 Nov 2026): the part again
        }
        for (;;) {
          const { done: finished, value } = await read()
          if (finished) break
          if (got() + value.length > len) throw new DownloadError('corrupt', 'too long')
          tail.push(value as Uint8Array<ArrayBuffer>)
          tailBytes += value.length
          count(value.length)
          // A whole piece in: kept now, so a reload goes on from here (the last piece goes with the part).
          if (tailBytes >= PIECE && (pieces.length + 1) * PIECE < len) {
            const all = new Blob(tail)
            const piece = all.slice(0, PIECE)
            await cache.put(at(pieceKey(f.name, i, slot, pieces.length)), new Response(piece, { headers: { 'x-etag': f.etag, 'x-bytes': String(PIECE), ...(src ? { 'x-src': src } : {}) } }))
            pieces.push(piece)
            tail = [all.slice(PIECE)]
            tailBytes -= PIECE
          }
        }
        if (got() !== len) throw new DownloadError('network', 'incomplete')
      }, signal, flowing)
    }
    for (let bad = 0; ; ) {
      await retry(async flowing => {
        // A part begun again (another copy of the file answered: its bytes from the start) needs no wait.
        while ((await requestPart(flowing)) === 'again');
      }, waiting, signal)
      const blob = new Blob([...pieces, ...tail])
      // The part's hash when the list gives it (Hugging Face's always, the server's when its parts are made).
      if (f.parts?.[i] && crypto.subtle && (await sha256Hex(await blob.arrayBuffer())) !== f.parts[i]) {
        await reset()
        if (++bad >= 3) throw new DownloadError('corrupt', 'corrupt')
        continue
      }
      await cache.put(at(partKey(f.name, i, slot)), new Response(blob, { headers: { 'x-etag': f.etag, 'x-bytes': String(len) } }))
      await dropPieces(cache, f.name, i, slot, len)
      return
    }
  }
  /** The source's ETag of each file this page downloads (Hugging Face's CDN): a later range must be the same file. */
  const sources = new Map<string, string>()

  /**
   * A download of these roles, joined by a second call while it runs (another engine of the page): one download,
   * every caller hears its progress. A file changed on the server meanwhile, or whose whole hash is wrong, starts over
   * cleanly from a new list (twice at most).
   */
  function downloadRoles(which: ModelKind | 'sex', roles: FileRole[], files: ModelFile[], progress: Progress, relist: () => Promise<ModelFile[]>): Promise<void> {
    const key = `${base}|${roles.join(',')}`
    const running = inflight.get(key)
    if (running) {
      running.listeners.add(progress)
      if (running.last) progress(...running.last)
      return running.promise.finally(() => running.listeners.delete(progress))
    }
    const job: Inflight = { listeners: new Set([progress]), last: null, promise: Promise.resolve(), stop: new AbortController() }
    const signal = job.stop.signal
    const tell: Progress = (...a) => {
      job.last = a
      for (const l of job.listeners) l(...a)
    }
    job.promise = (async () => {
      // Begun: «Continuar» after a reload is offered for this model only (the files it shares with another, such as
      // the detector, do not make the other one look half done).
      await markBegun(which, true)
      let list = files
      for (let restart = 0; ; restart++) {
        try {
          await downloadOnce(roles, list, tell, signal)
          await markBegun(which, false)
          return
        } catch (err) {
          const kind = (err as DownloadError).kind
          // A whole file whose hash is wrong (a server without part hashes): its parts went, once more from the start.
          const whole = kind === 'corrupt' && (err as { whole?: boolean }).whole && restart < 1
          if (!whole && (kind !== 'changed' || restart >= 2)) throw kind === 'changed' ? Object.assign(new Error('changed-on-server'), { final: true }) : err
          if (kind === 'changed') list = await retry(relist, on => tell(job.last?.[0] ?? 0, job.last?.[1] ?? 1, on ? 'waiting' : undefined), signal)
        }
      }
    })().finally(() => inflight.delete(key))
    inflight.set(key, job)
    return job.promise
  }

  async function downloadOnce(roles: FileRole[], files: ModelFile[], progress: Progress, signal: AbortSignal) {
    const wanted = files.filter(f => roles.includes(f.role))
    if (wanted.length !== roles.length) throw new Error('missing-on-server')
    const cache = await caches.open(CACHE)
    let done = 0, total = 0, waitingNow = false
    const tellWait = (on: boolean) => {
      waitingNow = on
      progress(done, Math.max(1, total), on ? 'waiting' : undefined)
    }
    await retry(flowing => keepPage(cache, signal, flowing), tellWait, signal)
    if (signedIn()) await refreshStock()
    const before = await stored()
    // The files whose listed version is on the phone already (perhaps from the other source: the same bytes): kept
    // as they are, and not counted in the progress (its total is what this download fetches).
    const have = new Set<string>()
    for (const f of wanted) if (isCurrent(before?.files[f.name], f) && (await complete(cache, f.name, before!.files[f.name]))) have.add(f.name)
    total = wanted.filter(f => !have.has(f.name)).reduce((a, f) => a + f.bytes, 0)
    progress(0, total)
    const count = (n: number) => {
      done += n
      progress(done, total, waitingNow ? 'waiting' : undefined)
    }
    // The new files, written to stored.json at the end only: until then the old versions are what the phone uses.
    const fresh: Record<string, StoredFile> = {}
    const label = (f: ModelFile): Partial<StoredFile> => ({
      ...(f.version ? { version: f.version } : {}),
      ...(f.sha256 ? { sha256: f.sha256 } : {}),
      ...extraOf(f),
    })
    for (const f of wanted) {
      const old = before?.files[f.name]
      if (have.has(f.name)) {
        fresh[f.name] = { ...old!, role: f.role, ...(f.parts && !old!.parts ? { parts: f.parts } : {}), ...label(f) }
        continue
      }
      // Another version under the same name: the new one gets its own keys, so the old one stays whole until the swap.
      const slot = slotFor(f, old)
      try {
        for (let i = 0; i * PART < f.bytes; i++) {
          const len = Math.min(f.bytes, (i + 1) * PART) - i * PART
          const part = await cache.match(at(partKey(f.name, i, slot)))
          if (part && part.headers.get('x-etag') === f.etag && Number(part.headers.get('x-bytes')) === len) {
            count(len)
            continue
          }
          await fetchPart(cache, f, i, slot, count, tellWait, signal)
        }
      } catch (err) {
        // The file changed on the server: its new bytes so far go (the installed version stays), and it starts over.
        if ((err as DownloadError).kind === 'changed') await dropNew(cache, f.name, old, slot)
        throw err
      }
      // The whole file against the listed SHA-256 before it may replace anything; a bad copy is thrown away.
      if (f.sha256 && (await storedSha256(cache, f.name, f.bytes, slot)) !== f.sha256) {
        // Every part of the new bytes goes (the installed version's own slot stays), so the restart fetches them again.
        await dropParts(cache, f.name, old && old.slot !== slot ? { slot: old.slot } : undefined)
        sources.delete(f.name)
        throw Object.assign(new DownloadError('corrupt', 'corrupt'), { final: true, whole: true })
      }
      fresh[f.name] = { etag: f.etag, bytes: f.bytes, role: f.role, ...(f.parts ? { parts: f.parts } : {}), ...(slot ? { slot } : {}), ...label(f) }
    }
    if (signal.aborted) throw new DownloadError('cancelled')
    // The swap, one write of stored.json. Read again: another download (the sex model beside an identifier model)
    // may have finished meanwhile, and its files stay. The roles downloaded here now name the new files.
    const latest = (await stored())?.files ?? {}
    const next: Stored = { files: {}, at: new Date().toISOString() }
    for (const [name, f] of Object.entries(latest)) if (!roles.includes(roleOf(name, f.role))) next.files[name] = { ...f, role: roleOf(name, f.role) }
    Object.assign(next.files, fresh)
    await putStored(cache, next)
    forgetLoaded(roles)
    // Then the old versions' parts go: a replaced file entirely, a file kept under its name its other slots.
    const names = new Set([...Object.keys(latest).filter(n => roles.includes(roleOf(n, latest[n].role))), ...Object.keys(fresh)])
    for (const name of names) await dropParts(cache, name, fresh[name] ? { slot: fresh[name].slot } : undefined)
  }

  /** Stops the downloads of these roles running on this page, and waits until they have stopped. */
  async function cancel(roles: FileRole[]) {
    const jobs = [...inflight].filter(([k]) => k.startsWith(`${base}|`) && k.slice(base.length + 1).split(',').some(r => roles.includes(r as FileRole)))
    for (const [, j] of jobs) j.stop.abort()
    await Promise.all(jobs.map(([, j]) => j.promise.catch(() => {})))
  }

  /** A new version's parts and pieces off the phone; the installed version (its own slot) stays. */
  async function dropNew(cache: Cache, name: string, old: StoredFile | undefined, slot: string | undefined) {
    sources.delete(name)
    if (!old) return dropParts(cache, name)
    if (old.slot === slot) return
    for (const r of await partsOf(cache, name)) if ((new URL(r.url).searchParams.get('v') ?? undefined) === slot) await cache.delete(r)
  }

  /** The keys of a new version of a stored file: from its SHA-256 (or ETag), never the old one's. */
  function newSlot(f: ModelFile, old: StoredFile) {
    // FNV-1a of the ETag when the list gives no SHA-256.
    const fnv = () => [...f.etag].reduce((h, c) => Math.imul(h ^ c.charCodeAt(0), 16777619) >>> 0, 2166136261).toString(16)
    const base = f.sha256 ? f.sha256.slice(0, 16) : `e${fnv()}`
    return base === old.slot ? `${base}b` : base
  }

  async function readModel(name: string): Promise<Uint8Array> {
    const s = await stored()
    const f = s?.files[name]
    if (!f) throw new Error('not-downloaded')
    const cache = await caches.open(CACHE)
    const out = new Uint8Array(f.bytes)
    for (let i = 0, off = 0; off < f.bytes; i++) {
      const r = await cache.match(at(partKey(name, i, f.slot)))
      if (!r) throw new Error('not-downloaded')
      const part = new Uint8Array(await r.arrayBuffer())
      out.set(part, off)
      off += part.length
    }
    return out
  }

  // ---------------------------------------------------------------- the models in memory

  let ort: OrtModule | null = null
  let gpuOrt: OrtModule | null = null
  let detector: Ort.InferenceSession | null = null
  let detectorOnGpu = false
  const encoders: Partial<Record<ModelKind, Ort.InferenceSession>> = {}
  /** The input size of each encoder in memory (the small model's own, from the file it was read from). */
  const encoderSize: Partial<Record<ModelKind, number>> = {}
  let headLoaded: Head | null = null

  /** Reads into memory so far (Engine.loadGeneration). */
  let generation = 0

  async function importOrt(file: string): Promise<OrtModule> {
    const m = runtime ? await runtime(at(`ort/${file}`)) : ((await import(/* @vite-ignore */ at(`ort/${file}`))) as OrtModule)
    generation++
    m.env.wasm.wasmPaths = at('ort/')
    m.env.logLevel = 'error'
    return m
  }

  async function load(kind: ModelKind) {
    const s = await stored()
    if (!s) throw new Error('not-downloaded')
    const name = (role: FileRole) => {
      const n = storedName(s.files, role)
      if (!n) throw new Error('not-downloaded')
      return n
    }
    await loadDetector(s)
    if (!encoders[kind]) {
      const t = performance.now()
      const file = name(ENCODER_ROLE[kind])
      let bytes: Uint8Array | null = await readModel(file)
      encoders[kind] = await ort!.InferenceSession.create(bytes, { executionProviders: ['wasm'], graphOptimizationLevel: 'all' })
      encoderSize[kind] = kind === 'small' ? studentSizeOf(s.files[file], file) : 224
      generation++
      bytes = null
      lastMs[`${kind}_load`] = Math.round(performance.now() - t)
    }
    await loadHead()
  }

  /** The runtime and the wing detector in memory (the first time from storage). */
  async function loadDetector(s: Stored) {
    ort ??= await importOrt(ORT_FILES[0])
    ort.env.wasm.numThreads = threads
    if (detector) return
    const n = storedName(s.files, 'detector')
    if (!n) throw new Error('not-downloaded')
    const t = performance.now()
    const bytes = await readModel(n)
    // The wing detector on the phone's GPU when it has one (faster), else WebAssembly like the encoder.
    if ('gpu' in navigator) {
      try {
        gpuOrt = await importOrt(ORT_GPU_FILES[0])
        gpuOrt.env.wasm.numThreads = 1
        detector = await gpuOrt.InferenceSession.create(bytes, { executionProviders: ['webgpu'], graphOptimizationLevel: 'all' })
        detectorOnGpu = true
      } catch {
        detector = null
      }
    }
    detector ??= await ort.InferenceSession.create(bytes, { executionProviders: ['wasm'], graphOptimizationLevel: 'all' })
    generation++
    lastMs.detector_load = Math.round(performance.now() - t)
  }

  /** The head alone (about 30 MB): enough to rank a feature again, e.g. a result restored after another tab. */
  async function loadHead(): Promise<Head> {
    if (headLoaded) return headLoaded
    const s = await stored()
    const name = (role: FileRole) => {
      const n = s && storedName(s.files, role)
      if (!n) throw new Error('not-downloaded')
      return n
    }
    const meta = JSON.parse(new TextDecoder().decode(await readModel(name('head-meta')))) as HeadMeta
    const w = await readModel(name('head'))
    headLoaded = makeHead(meta, w.buffer as ArrayBuffer)
    generation++
    return headLoaded
  }

  async function decode(file: Blob): Promise<Rgba> {
    const src = URL.createObjectURL(file)
    try {
      const img = new Image()
      img.decoding = 'async'
      img.src = src
      await img.decode()
      const canvas = document.createElement('canvas')
      canvas.width = img.naturalWidth
      canvas.height = img.naturalHeight
      const ctx = canvas.getContext('2d', { willReadFrequently: true })!
      ctx.drawImage(img, 0, 0)
      const data = ctx.getImageData(0, 0, canvas.width, canvas.height)
      canvas.width = canvas.height = 0
      return { width: data.width, height: data.height, data: data.data }
    } finally {
      URL.revokeObjectURL(src)
    }
  }

  async function sha256(buf: ArrayBuffer): Promise<string> {
    if (!crypto.subtle) return ''
    return [...new Uint8Array(await crypto.subtle.digest('SHA-256', buf))].map(b => b.toString(16).padStart(2, '0')).join('')
  }

  /** The detector's box and the 224 px square (the head's reviewed full-photo list applies when the head is loaded). */
  async function cut(photo: Photo): Promise<{ square: Uint8Array; status: string }> {
    // Taken before the first await: an update landing meanwhile replaces these (forgetLoaded), not this photo's run.
    const session = detector!, ortD = detectorOnGpu && gpuOrt ? gpuOrt : ort!, head = headLoaded
    const raw = photo.bytes
    const orientation = exifOrientation(raw)
    const override = !!head && head.meta.full_photo_overrides.includes(await sha256(raw))
    const img = await decode(photo.blob)
    const inp = detectorInput(img, orientation)
    const res = await session.run({ [session.inputNames[0]]: new ortD.Tensor('float32', inp.x, [1, 3, 640, 640]) })
    const det = res[session.outputNames[0]], proto = res[session.outputNames[1]]
    const masks = readMasks(det.data as Float32Array, det.dims[1], det.dims[2], proto.data as Float32Array, proto.dims[2], inp)
    const { box, status } = cropBox(selectMasks(masks), orientation, img.width, img.height, override)
    return { square: letterboxCrop(img, box), status }
  }

  async function run(photo: Photo, kind: ModelKind): Promise<RunResult> {
    await load(kind)
    const t0 = performance.now()
    // Taken before the first await (see cut): the encoder and its input size of this photo's run.
    const encoder = encoders[kind]!, size = encoderSize[kind]!
    const { square, status } = await cut(photo)
    const x =
      kind === 'small'
        ? new ort!.Tensor('float32', studentInput(square, 224, size), [1, 3, size, size])
        : new ort!.Tensor('float32', encoderInput(square), [1, 3, 224, 224])
    const enc = await encoder.run({ [encoder.inputNames[0]]: x })
    const feature = Float32Array.from(enc[encoder.outputNames[0]].data as Float32Array)
    const ms = Math.round(performance.now() - t0)
    lastMs.photo = ms
    return { feature, square, status, ms }
  }

  // ---------------------------------------------------------------- the sex estimate (team only, lib/sexModel.ts)

  let sexSession: Ort.InferenceSession | null = null
  let sexSessionInfo: SexModelInfo | null = null

  /**
   * A stored sex file's calibration: the one kept with it; a copy kept before calibrations were stored is model B
   * (the only one shipped then: lib/sexModel.ts SEX_DEFAULT). Any other file without one: null (not usable).
   */
  function calibrationOf(f: StoredFile): SexCalibration | null {
    return parseCalibration(f.calibration) ?? (!f.sha256 || f.sha256 === SEX_FILE.sha256 ? SEX_DEFAULT : null)
  }

  async function sexInfo(): Promise<SexModelInfo | null> {
    const s = await stored()
    const name = s && storedName(s.files, 'sex')
    if (!name) return null
    const f = s.files[name], calibration = calibrationOf(f)
    return calibration ? { name, bytes: f.bytes, version: f.version ?? null, sha256: f.sha256 ?? null, calibration } : null
  }

  /** The detector and a usable sex model on the phone, whatever its version (an older one works until its update is in). */
  async function sexReady() {
    const s = await stored()
    return rolesStored(SEX_ROLES, s?.files ?? null) && !!(await sexInfo())
  }

  async function square(photo: Photo) {
    const s = await stored()
    if (!s) throw new Error('not-downloaded')
    await loadDetector(s)
    return cut(photo)
  }

  async function sexLogit(sq: Uint8Array): Promise<number> {
    if (!sexSession) {
      const info = await sexInfo()
      if (!info) throw new Error('not-downloaded')
      ort ??= await importOrt(ORT_FILES[0])
      ort.env.wasm.numThreads = threads
      const t = performance.now()
      sexSession = await ort.InferenceSession.create(await readModel(info.name), { executionProviders: ['wasm'], graphOptimizationLevel: 'all' })
      sexSessionInfo = info
      generation++
      lastMs.sex_load = Math.round(performance.now() - t)
    }
    const t = performance.now()
    // Taken before the await: an update landing meanwhile clears sexSession, not this run.
    const session = sexSession
    const out = await session.run({ [session.inputNames[0]]: new ort!.Tensor('float32', sexInput(sq), [1, 3, 224, 224]) })
    lastMs.sex = Math.round(performance.now() - t)
    // The logit by name (model C also gives p_female), else the first output (model B has one).
    const name = session.outputNames.includes('logit') ? 'logit' : session.outputNames[0]
    return (out[name].data as Float32Array)[0]
  }

  async function forgetSex() {
    await cancel(SEX_ROLES.filter(r => r === 'sex'))
    await markBegun('sex', false)
    sexSession?.release().catch(() => {})
    sexSession = null
    sexSessionInfo = null
    const cache = await caches.open(CACHE)
    const s = await stored()
    const names = Object.keys(s?.files ?? {}).filter(name => roleOf(name, s!.files[name].role) === 'sex')
    for (const name of names) delete s!.files[name]
    if (names.length) await putStored(cache, s!)
    // The stored sex files by name, whatever the name.
    for (const name of names) await dropParts(cache, name)
    // Then every cached part of a sex model, by its file's role: also a download or an update cut halfway, whose file
    // stored.json never named (the team's model must not stay on a phone signed out).
    await dropRoles(cache, new Set<FileRole>(['sex']))
  }

  async function removeModel(kind: ModelKind) {
    await cancel([ENCODER_ROLE[kind]])
    await markBegun(kind, false)
    const s = await stored()
    if (!s) return
    const other = KINDS.find(k => k !== kind)!
    const has = (role: FileRole) => storedName(s.files, role) != null
    const otherKept = MODEL_ROLES[other].every(has)
    const keepRoles = new Set<FileRole>([...(otherKept ? MODEL_ROLES[other] : []), ...(has('sex') ? SEX_ROLES : [])])
    const cache = await caches.open(CACHE)
    const gone: string[] = []
    for (const [name, f] of Object.entries(s.files)) {
      const role = roleOf(name, f.role)
      if (!MODEL_ROLES[kind].includes(role) || keepRoles.has(role)) continue
      gone.push(name)
      delete s.files[name]
      if (role === 'detector') {
        detector?.release().catch(() => {})
        detector = null
      }
      if (role === 'head' || role === 'head-meta') headLoaded = null
    }
    encoders[kind]?.release().catch(() => {})
    delete encoders[kind]
    await putStored(cache, s)
    for (const name of gone) await dropParts(cache, name)
    // And the parts of this model's own files that stored.json does not name (a download or update cut halfway).
    await dropRoles(cache, new Set(MODEL_ROLES[kind].filter(r => !keepRoles.has(r))), new Set(Object.keys(s.files)))
  }

  async function remove() {
    await cancel([...MODEL_ROLES.full, ...MODEL_ROLES.small, ...SEX_ROLES])
    detector?.release().catch(() => {})
    sexSession?.release().catch(() => {})
    sexSession = null
    for (const e of Object.values(encoders)) e?.release().catch(() => {})
    detector = null
    for (const k of KINDS) delete encoders[k]
    headLoaded = null
    await caches.delete(CACHE)
  }

  // ---------------------------------------------------------------- the flash-frozen stock (server/frozen-stock.mjs)

  async function stock() {
    const r = await (await caches.open(CACHE)).match(stockKey)
    if (!r) return null
    try {
      const value = (await r.json()) as FrozenStock
      return { stock: value, asOf: r.headers.get('x-synced-at') || value.generatedAt }
    } catch {
      return null
    }
  }

  /** Asked again whenever online (cheap: the ETag answers 304 when nothing changed). Never throws. */
  async function refreshStock() {
    if (!navigator.onLine || !signedIn()) return
    try {
      const cache = await caches.open(CACHE)
      const have = await cache.match(stockKey)
      const etag = have?.headers.get('etag')
      const r = await fetch(new URL('../api/frozen-stock', base).href, {
        credentials: 'same-origin',
        cache: 'no-store',
        headers: etag ? { 'if-none-match': etag } : {},
        signal: AbortSignal.timeout(10_000),
      })
      const synced = r.headers.get('x-synced-at') || ''
      const keep = (body: string, tag: string) =>
        cache.put(stockKey, new Response(body, { headers: { 'content-type': 'application/json', etag: tag, 'x-synced-at': synced, 'x-checked-at': new Date().toISOString() } }))
      if (r.status === 304 && have) await keep(await have.text(), etag!)
      else if (r.ok) await keep(await r.text(), r.headers.get('etag') ?? '')
    } catch {
      /* the kept copy stays */
    }
  }

  return {
    base,
    list,
    stored,
    ready,
    offer,
    toDownload,
    download,
    sexReady,
    sexInfo,
    sexLoaded: () => sexSessionInfo,
    sexToDownload: (files, s) => roleSizes(SEX_ROLES, sexFiles(files), s),
    downloadSex: (files, progress) => downloadRoles('sex', SEX_ROLES, sexFiles(files), progress, async () => sexFiles(await relist())),
    busy: which => inflight.has(`${base}|${(which === 'sex' ? SEX_ROLES : MODEL_ROLES[which]).join(',')}`),
    saved,
    square,
    sexLogit,
    forgetSex,
    pageOffline,
    remove,
    removeModel,
    load,
    loaded: kind => !!encoders[kind] && !!detector && !!headLoaded,
    loadGeneration: () => generation,
    run,
    head: () => headLoaded,
    loadHead,
    stock,
    refreshStock,
    forgetStock: async () => {
      await (await caches.open(CACHE)).delete(stockKey).catch(() => false)
    },
    lastMs,
  }
}

export { chooseModel }
