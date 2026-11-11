/**
 * The offline identifier without its page: the models' list, download (Cache Storage, 32 MB parts, resumed after a
 * cut), the frozen-stock snapshot, loading the models and running a photo (wing detector -> crop -> encoder -> the
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
  modelReady,
  rolesReady,
  sameFile,
  readMasks,
  roleOf,
  selectMasks,
  storedName,
  STUDENT_SIZE,
  studentInput,
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
import { SEX_FILE, SEX_ROLES, sexInput } from './sexModel'

type OrtModule = typeof Ort
/** `url`: a public copy (Hugging Face) fetched in ranges; else the server's parts. `parts`: each part's SHA-256. */
export interface ModelFile { name: string; bytes: number; etag: string; role: FileRole; wire?: number; encoding?: string | null; url?: string; parts?: string[] }
export interface Stored { files: Record<string, StoredFile>; at: string }

export const CACHE = 'ithomiini-prueba-offline'
export const PART = 32 * 1024 * 1024
export const KINDS: ModelKind[] = ['small', 'full']
const ORT_FILES = ['ort.wasm.min.mjs', 'ort-wasm-simd-threaded.mjs', 'ort-wasm-simd-threaded.wasm']
const ORT_GPU_FILES = ['ort.webgpu.min.mjs', 'ort-wasm-simd-threaded.asyncify.mjs', 'ort-wasm-simd-threaded.asyncify.wasm']

/** What the list call found: the files and where from, or why there are none (no signal, the server's answer). */
export type Listing = { files: ModelFile[]; source: 'server' | 'public' } | { error: 'signin' | 'offline' | 'server'; status?: number }

/** The Hugging Face copies as a listing (no request: the list is part of the page). */
export const publicListing = (): ModelFile[] =>
  PUBLIC_FILES.map(f => ({ name: f.name, bytes: f.bytes, etag: `"sha256-${f.sha256}"`, role: f.role, wire: f.bytes, encoding: null, url: f.url, parts: f.parts }))

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
  ready(files: ModelFile[] | null): Promise<Record<ModelKind, boolean>>
  /** What a model still needs to download: plain bytes (space on the phone) and on the wire (mobile data). */
  toDownload(kind: ModelKind, files: ModelFile[], stored: Stored | null): { plain: number; wire: number; total: number }
  download(kind: ModelKind, files: ModelFile[], progress: (done: number, total: number, retrying?: string) => void): Promise<void>
  /** The team's sex estimate (lib/sexModel.ts): whether its files are on the phone, their size, the download. */
  sexReady(files: ModelFile[] | null): Promise<boolean>
  sexToDownload(files: ModelFile[], stored: Stored | null): { plain: number; wire: number; total: number }
  downloadSex(files: ModelFile[], progress: (done: number, total: number, retrying?: string) => void): Promise<void>
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
export function createEngine(base: string, pageFiles: () => string[] = () => [], { signedIn = () => true }: { signedIn?: () => boolean } = {}): Engine {
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
    const r = await fetch(at('models/list.json'), { credentials: 'same-origin', cache: 'no-store' }).catch(() => null)
    if (!r) return { error: 'offline' }
    // The session ended meanwhile: the public copies.
    if (r.status === 401) return { files: publicListing(), source: 'public' }
    if (!r.ok) return { error: 'server', status: r.status }
    const body = (await r.json()) as { files: ModelFile[]; partBytes?: number }
    if (body.partBytes && body.partBytes !== PART) return { error: 'server', status: 409 }
    return { files: body.files, source: 'server' }
  }

  async function ready(files: ModelFile[] | null) {
    const s = await stored()
    // Files kept before the lists said their part hashes: the hashes of the very version listed are added, so the
    // model also counts as the public copy later (signed out).
    if (s && files) {
      let added = false
      for (const f of files) {
        const have = s.files[f.name]
        if (have && f.parts?.length && !have.parts && have.etag === f.etag && have.bytes === f.bytes) {
          have.parts = f.parts
          added = true
        }
      }
      if (added) await (await caches.open(CACHE)).put(at('models/stored.json'), new Response(JSON.stringify(s), { headers: { 'content-type': 'application/json' } }))
    }
    return { full: modelReady('full', s?.files ?? null, files), small: modelReady('small', s?.files ?? null, files) }
  }

  function toDownload(kind: ModelKind, files: ModelFile[], s: Stored | null) {
    return roleSizes(MODEL_ROLES[kind], files, s)
  }

  function roleSizes(roles: FileRole[], files: ModelFile[], s: Stored | null) {
    const wanted = files.filter(f => roles.includes(f.role))
    const need = wanted.filter(f => !sameFile(s?.files[f.name], f))
    return {
      plain: need.reduce((a, f) => a + f.bytes, 0),
      wire: need.reduce((a, f) => a + (f.wire ?? f.bytes), 0),
      total: wanted.reduce((a, f) => a + f.bytes, 0),
    }
  }

  /** The page, its scripts and the runtime, for airplane mode (the service worker serves them from this cache). */
  async function keepPage(cache: Cache) {
    const ort = [...ORT_FILES, ...('gpu' in navigator ? ORT_GPU_FILES : [])].map(f => at(`ort/${f}`))
    for (const u of [...pageFiles(), ...ort]) {
      const r = await fetch(u, { cache: 'no-cache' })
      if (!r.ok) throw new Error(`${u}: HTTP ${r.status}`)
      await cache.put(u, r)
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

  async function download(kind: ModelKind, files: ModelFile[], progress: (done: number, total: number, retrying?: string) => void) {
    return downloadRoles(MODEL_ROLES[kind], files, progress)
  }

  /**
   * The sex model with the detector. Only the pinned file (lib/sexModel.ts SEX_FILE): a listed file with other bytes
   * is not taken, and each part is checked against the pinned hashes.
   */
  function sexFiles(files: ModelFile[]): ModelFile[] {
    return files.flatMap(f => (f.role !== 'sex' ? [f] : f.name === SEX_FILE.name && f.bytes === SEX_FILE.bytes ? [{ ...f, parts: SEX_FILE.parts }] : []))
  }

  async function downloadRoles(roles: FileRole[], files: ModelFile[], progress: (done: number, total: number, retrying?: string) => void) {
    const wanted = files.filter(f => roles.includes(f.role))
    if (wanted.length !== roles.length) throw new Error('missing-on-server')
    const cache = await caches.open(CACHE)
    if (roles.includes('head')) headLoaded = null // read again: its JSON may be a new one
    await keepPage(cache)
    if (signedIn()) await refreshStock()
    const total = wanted.reduce((a, f) => a + f.bytes, 0)
    let done = 0
    // The other model's files stay; files the server no longer lists go, with their parts.
    const before = await stored()
    const keep: Stored = { files: {}, at: new Date().toISOString() }
    for (const [name, f] of Object.entries(before?.files ?? {})) {
      if (files.some(l => l.name === name)) keep.files[name] = { ...f, role: roleOf(name, f.role) }
      else for (let i = 0; i * PART < f.bytes; i++) await cache.delete(at(`models/${name}?part=${i}`))
    }
    for (const f of wanted) {
      const parts = Math.ceil(f.bytes / PART)
      // Already kept from the other source (the same bytes): every part there, nothing to download.
      const old = before?.files[f.name]
      if (old && old.etag !== f.etag && sameFile(old, f)) {
        let all = true
        for (let i = 0; i < parts && all; i++) {
          const have = await cache.match(at(`models/${f.name}?part=${i}`))
          all = !!have && have.headers.get('x-etag') === old.etag && Number(have.headers.get('x-bytes')) === Math.min(f.bytes, (i + 1) * PART) - i * PART
        }
        if (all) {
          keep.files[f.name] = { ...old, role: f.role, parts: f.parts ?? old.parts }
          done += f.bytes
          progress(done, total)
          continue
        }
      }
      for (let i = 0; i < parts; i++) {
        const key = at(`models/${f.name}?part=${i}`)
        const start = i * PART, end = Math.min(f.bytes, start + PART) - 1
        const have = await cache.match(key)
        if (have && have.headers.get('x-etag') === f.etag && Number(have.headers.get('x-bytes')) === end - start + 1) {
          done += end - start + 1
          progress(done, total)
          continue
        }
        let chunks: Uint8Array[] = []
        for (let attempt = 1; ; attempt++) {
          chunks = []
          let got = 0
          try {
            let r: Response
            if (f.url) {
              // Hugging Face: a range of the file at a pinned commit (CORS; no cookies), checked by its hash below.
              r = await fetch(f.url, { headers: { range: `bytes=${start}-${end}` }, credentials: 'omit', cache: 'no-store', mode: 'cors' })
              const whole = r.status === 200 && start === 0 && end === f.bytes - 1
              if (r.status !== 206 && !whole) throw new Error(`HTTP ${r.status}`)
              const range = r.headers.get('content-range')
              if (r.status === 206 && range && range !== `bytes ${start}-${end}/${f.bytes}`) {
                r.body?.cancel().catch(() => {})
                throw Object.assign(new Error('changed-on-server'), { final: true })
              }
            } else {
              r = await fetch(key, { credentials: 'same-origin', cache: 'no-store' })
              if (r.status !== 200) throw new Error(`HTTP ${r.status}`)
              // The part's ETag is the listed file's: a model changed since the list is not mixed in.
              if (r.headers.get('etag') !== `W/${f.etag.slice(0, -1)}-p${i}"` || Number(r.headers.get('x-part-bytes')) !== end - start + 1) {
                r.body?.cancel().catch(() => {})
                throw Object.assign(new Error('changed-on-server'), { final: true })
              }
            }
            const reader = r.body!.getReader()
            for (;;) {
              const { done: finished, value } = await reader.read()
              if (finished) break
              chunks.push(value)
              got += value.length
              done += value.length
              progress(done, total)
            }
            if (got !== end - start + 1) throw new Error('incomplete')
            // The part's hash when the list gives it (Hugging Face's always, the server's when its parts are made).
            if (f.parts?.[i] && crypto.subtle) {
              const blob = new Blob(chunks as BlobPart[])
              if ((await sha256Hex(await blob.arrayBuffer())) !== f.parts[i]) throw new Error('corrupt')
            }
            break
          } catch (err) {
            done -= got
            if (attempt >= 4 || (err as { final?: boolean }).final) throw err
            progress(done, total, (err as Error).message)
            await new Promise(r => setTimeout(r, 2000 * attempt))
          }
        }
        await cache.put(key, new Response(new Blob(chunks as BlobPart[]), { headers: { 'x-etag': f.etag, 'x-bytes': String(end - start + 1) } }))
      }
      keep.files[f.name] = { etag: f.etag, bytes: f.bytes, role: f.role, ...(f.parts ? { parts: f.parts } : {}) }
    }
    // Another download may have finished meanwhile (the sex model beside an identifier model): its listed files stay.
    for (const [name, f] of Object.entries((await stored())?.files ?? {}))
      if (!keep.files[name] && !roles.includes(roleOf(name, f.role)) && files.some(l => l.name === name)) keep.files[name] = f
    await cache.put(at('models/stored.json'), new Response(JSON.stringify(keep), { headers: { 'content-type': 'application/json' } }))
  }

  async function readModel(name: string): Promise<Uint8Array> {
    const s = await stored()
    const f = s?.files[name]
    if (!f) throw new Error('not-downloaded')
    const cache = await caches.open(CACHE)
    const out = new Uint8Array(f.bytes)
    for (let i = 0, off = 0; off < f.bytes; i++) {
      const r = await cache.match(at(`models/${name}?part=${i}`))
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
  let headLoaded: Head | null = null

  async function importOrt(file: string): Promise<OrtModule> {
    const m = (await import(/* @vite-ignore */ at(`ort/${file}`))) as OrtModule
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
      let bytes: Uint8Array | null = await readModel(name(ENCODER_ROLE[kind]))
      encoders[kind] = await ort!.InferenceSession.create(bytes, { executionProviders: ['wasm'], graphOptimizationLevel: 'all' })
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
    const raw = photo.bytes
    const orientation = exifOrientation(raw)
    const override = !!headLoaded && headLoaded.meta.full_photo_overrides.includes(await sha256(raw))
    const img = await decode(photo.blob)
    const inp = detectorInput(img, orientation)
    const ortD = detectorOnGpu && gpuOrt ? gpuOrt : ort!
    const res = await detector!.run({ [detector!.inputNames[0]]: new ortD.Tensor('float32', inp.x, [1, 3, 640, 640]) })
    const det = res[detector!.outputNames[0]], proto = res[detector!.outputNames[1]]
    const masks = readMasks(det.data as Float32Array, det.dims[1], det.dims[2], proto.data as Float32Array, proto.dims[2], inp)
    const { box, status } = cropBox(selectMasks(masks), orientation, img.width, img.height, override)
    return { square: letterboxCrop(img, box), status }
  }

  async function run(photo: Photo, kind: ModelKind): Promise<RunResult> {
    await load(kind)
    const t0 = performance.now()
    const encoder = encoders[kind]!
    const { square, status } = await cut(photo)
    const x =
      kind === 'small'
        ? new ort!.Tensor('float32', studentInput(square), [1, 3, STUDENT_SIZE, STUDENT_SIZE])
        : new ort!.Tensor('float32', encoderInput(square), [1, 3, 224, 224])
    const enc = await encoder.run({ [encoder.inputNames[0]]: x })
    const feature = Float32Array.from(enc[encoder.outputNames[0]].data as Float32Array)
    const ms = Math.round(performance.now() - t0)
    lastMs.photo = ms
    return { feature, square, status, ms }
  }

  // ---------------------------------------------------------------- the sex estimate (team only, lib/sexModel.ts)

  let sexSession: Ort.InferenceSession | null = null

  async function sexReady(files: ModelFile[] | null) {
    const s = await stored()
    // Offline (no list): the pinned file by its name.
    if (!files) return !!s && storedName(s.files, 'sex') === SEX_FILE.name && storedName(s.files, 'detector') != null
    return rolesReady(SEX_ROLES, s?.files ?? null, sexFiles(files))
  }

  async function square(photo: Photo) {
    const s = await stored()
    if (!s) throw new Error('not-downloaded')
    await loadDetector(s)
    return cut(photo)
  }

  async function sexLogit(sq: Uint8Array): Promise<number> {
    if (!sexSession) {
      const s = await stored()
      if (!s || storedName(s.files, 'sex') !== SEX_FILE.name) throw new Error('not-downloaded')
      ort ??= await importOrt(ORT_FILES[0])
      ort.env.wasm.numThreads = threads
      const t = performance.now()
      sexSession = await ort.InferenceSession.create(await readModel(SEX_FILE.name), { executionProviders: ['wasm'], graphOptimizationLevel: 'all' })
      lastMs.sex_load = Math.round(performance.now() - t)
    }
    const t = performance.now()
    const out = await sexSession.run({ [sexSession.inputNames[0]]: new ort!.Tensor('float32', sexInput(sq), [1, 3, 224, 224]) })
    lastMs.sex = Math.round(performance.now() - t)
    return (out[sexSession.outputNames[0]].data as Float32Array)[0]
  }

  async function forgetSex() {
    sexSession?.release().catch(() => {})
    sexSession = null
    const cache = await caches.open(CACHE)
    const s = await stored()
    if (!s) return
    let changed = false
    for (const [name, f] of Object.entries(s.files)) {
      if (roleOf(name, f.role) !== 'sex') continue
      for (let i = 0; i * PART < f.bytes; i++) await cache.delete(at(`models/${name}?part=${i}`))
      delete s.files[name]
      changed = true
    }
    if (changed) await cache.put(at('models/stored.json'), new Response(JSON.stringify(s), { headers: { 'content-type': 'application/json' } }))
  }

  async function removeModel(kind: ModelKind) {
    const s = await stored()
    if (!s) return
    const other = KINDS.find(k => k !== kind)!
    const has = (role: FileRole) => storedName(s.files, role) != null
    const otherKept = MODEL_ROLES[other].every(has)
    const keepRoles = new Set<FileRole>([...(otherKept ? MODEL_ROLES[other] : []), ...(has('sex') ? SEX_ROLES : [])])
    const cache = await caches.open(CACHE)
    for (const [name, f] of Object.entries(s.files)) {
      const role = roleOf(name, f.role)
      if (!MODEL_ROLES[kind].includes(role) || keepRoles.has(role)) continue
      for (let i = 0; i * PART < f.bytes; i++) await cache.delete(at(`models/${name}?part=${i}`))
      delete s.files[name]
      if (role === 'detector') {
        detector?.release().catch(() => {})
        detector = null
      }
      if (role === 'head' || role === 'head-meta') headLoaded = null
    }
    encoders[kind]?.release().catch(() => {})
    delete encoders[kind]
    await cache.put(at('models/stored.json'), new Response(JSON.stringify(s), { headers: { 'content-type': 'application/json' } }))
  }

  async function remove() {
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
    toDownload,
    download,
    sexReady,
    sexToDownload: (files, s) => roleSizes(SEX_ROLES, sexFiles(files), s),
    downloadSex: (files, progress) => downloadRoles(SEX_ROLES, sexFiles(files), progress),
    square,
    sexLogit,
    forgetSex,
    pageOffline,
    remove,
    removeModel,
    load,
    loaded: kind => !!encoders[kind] && !!detector && !!headLoaded,
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
