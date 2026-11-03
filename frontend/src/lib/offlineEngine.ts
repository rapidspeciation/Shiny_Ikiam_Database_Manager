/**
 * The offline identifier without its page: the models' list, download (Cache Storage, 32 MB parts, resumed after a
 * cut), the frozen-stock snapshot, loading the models and running a photo (wing detector -> crop -> encoder -> the
 * feature). Used by the Identificar tab (views/IdentifyView.vue). It keeps the test page's cache and URLs
 * (/prueba-offline/models/…, src/offline/main.ts): a phone that downloaded there does not download again.
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

type OrtModule = typeof Ort
export interface ModelFile { name: string; bytes: number; etag: string; role: FileRole; wire?: number; encoding?: string | null }
export interface Stored { files: Record<string, StoredFile>; at: string }

export const CACHE = 'ithomiini-prueba-offline'
export const PART = 32 * 1024 * 1024
export const KINDS: ModelKind[] = ['small', 'full']
const ORT_FILES = ['ort.wasm.min.mjs', 'ort-wasm-simd-threaded.mjs', 'ort-wasm-simd-threaded.wasm']
const ORT_GPU_FILES = ['ort.webgpu.min.mjs', 'ort-wasm-simd-threaded.asyncify.mjs', 'ort-wasm-simd-threaded.asyncify.wasm']

/** What the list call found: the files, or why there are none (not signed in, no signal, the server's answer). */
export type Listing = { files: ModelFile[] } | { error: 'signin' | 'offline' | 'server'; status?: number }

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
  /** Whether this page, its scripts and the runtime are on the phone (it opens without signal); online, keeps them first. */
  pageOffline(): Promise<boolean>
  remove(): Promise<void>
  /** The models in memory for `kind`; the first time reads them from storage (the slow step). */
  load(kind: ModelKind): Promise<void>
  loaded(kind: ModelKind): boolean
  run(photo: Photo, kind: ModelKind): Promise<RunResult>
  head(): Head | null
  loadHead(): Promise<Head>
  stock(): Promise<{ stock: FrozenStock; asOf: string } | null>
  refreshStock(): Promise<void>
  lastMs: Record<string, number>
}

/**
 * `base`: the test page's folder (absolute, ending in /prueba-offline/), where the models, the runtime and the cache
 * keys live. `pageFiles`: the page's own files to keep for airplane mode (its document, scripts and styles).
 */
export function createEngine(base: string, pageFiles: () => string[] = () => []): Engine {
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
    const r = await fetch(at('models/list.json'), { credentials: 'same-origin', cache: 'no-store' }).catch(() => null)
    if (!r) return { error: 'offline' }
    if (r.status === 401) return { error: 'signin' }
    if (!r.ok) return { error: 'server', status: r.status }
    const body = (await r.json()) as { files: ModelFile[]; partBytes?: number }
    if (body.partBytes && body.partBytes !== PART) return { error: 'server', status: 409 }
    return { files: body.files }
  }

  async function ready(files: ModelFile[] | null) {
    const s = await stored()
    return { full: modelReady('full', s?.files ?? null, files), small: modelReady('small', s?.files ?? null, files) }
  }

  function toDownload(kind: ModelKind, files: ModelFile[], s: Stored | null) {
    const wanted = files.filter(f => MODEL_ROLES[kind].includes(f.role))
    const need = wanted.filter(f => s?.files[f.name]?.etag !== f.etag)
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
    const wanted = files.filter(f => MODEL_ROLES[kind].includes(f.role))
    if (wanted.length !== MODEL_ROLES[kind].length) throw new Error('missing-on-server')
    const cache = await caches.open(CACHE)
    await keepPage(cache)
    await refreshStock()
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
            const r = await fetch(key, { credentials: 'same-origin', cache: 'no-store' })
            if (r.status !== 200) throw new Error(`HTTP ${r.status}`)
            // The part's ETag is the listed file's: a model changed since the list is not mixed in.
            if (r.headers.get('etag') !== `W/${f.etag.slice(0, -1)}-p${i}"` || Number(r.headers.get('x-part-bytes')) !== end - start + 1) {
              r.body?.cancel().catch(() => {})
              throw Object.assign(new Error('changed-on-server'), { final: true })
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
      keep.files[f.name] = { etag: f.etag, bytes: f.bytes, role: f.role }
    }
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
    ort ??= await importOrt(ORT_FILES[0])
    ort.env.wasm.numThreads = threads
    if (!detector) {
      const t = performance.now()
      const bytes = await readModel(name('detector'))
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
    if (!encoders[kind]) {
      const t = performance.now()
      let bytes: Uint8Array | null = await readModel(name(ENCODER_ROLE[kind]))
      encoders[kind] = await ort.InferenceSession.create(bytes, { executionProviders: ['wasm'], graphOptimizationLevel: 'all' })
      bytes = null
      lastMs[`${kind}_load`] = Math.round(performance.now() - t)
    }
    await loadHead()
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

  async function run(photo: Photo, kind: ModelKind): Promise<RunResult> {
    await load(kind)
    const t0 = performance.now()
    const encoder = encoders[kind]!
    const raw = photo.bytes
    const orientation = exifOrientation(raw)
    const override = headLoaded!.meta.full_photo_overrides.includes(await sha256(raw))
    const img = await decode(photo.blob)
    const inp = detectorInput(img, orientation)
    const ortD = detectorOnGpu && gpuOrt ? gpuOrt : ort!
    const res = await detector!.run({ [detector!.inputNames[0]]: new ortD.Tensor('float32', inp.x, [1, 3, 640, 640]) })
    const det = res[detector!.outputNames[0]], proto = res[detector!.outputNames[1]]
    const masks = readMasks(det.data as Float32Array, det.dims[1], det.dims[2], proto.data as Float32Array, proto.dims[2], inp)
    const { box, status } = cropBox(selectMasks(masks), orientation, img.width, img.height, override)
    const square = letterboxCrop(img, box)
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

  async function remove() {
    detector?.release().catch(() => {})
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
    if (!navigator.onLine) return
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
    pageOffline,
    remove,
    load,
    loaded: kind => !!encoders[kind] && !!detector && !!headLoaded,
    run,
    head: () => headLoaded,
    loadHead,
    stock,
    refreshStock,
    lastMs,
  }
}

export { chooseModel }
