/**
 * The offline identifier test page (/prueba-offline/; server/offline-id.mjs serves it with cross-origin isolation
 * and its models behind the sign-in). Franz's phones: download the models once (Cache Storage, in parts of 32 MB so
 * a broken download resumes), then identify a photo in airplane mode: wing detector -> crop -> BioCLIP 2.5-H int8
 * encoder -> head, all in the browser (lib/offlineId.ts). «Copiar resultado» gives the device's numbers back.
 */
import type * as Ort from 'onnxruntime-web'
import {
  cropBox,
  detectorInput,
  encoderInput,
  exifOrientation,
  letterboxCrop,
  makeHead,
  readMasks,
  selectMasks,
  speciesTop,
  type Head,
  type HeadMeta,
  type Rgba,
  type Side,
} from '../lib/offlineId'
import { errorText, PHOTO_UNREADABLE, takePhoto, type Photo } from '../lib/offlinePhoto'
import { genusStock, speciesStock, stockDate, type FrozenStock } from '../lib/frozenStock'

type OrtModule = typeof Ort
interface ModelFile { name: string; bytes: number; etag: string; role: 'detector' | 'encoder' | 'head' | 'head-meta'; wire?: number; encoding?: string | null }
interface Stored { files: Record<string, { etag: string; bytes: number }>; at: string }

const CACHE = 'ithomiini-prueba-offline'
const PART = 32 * 1024 * 1024
const RUNNING = 'prueba-offline:running'
const LAST = 'prueba-offline:last'
const ORT_FILES = ['ort.wasm.min.mjs', 'ort-wasm-simd-threaded.mjs', 'ort-wasm-simd-threaded.wasm']
const ORT_GPU_FILES = ['ort.webgpu.min.mjs', 'ort-wasm-simd-threaded.asyncify.mjs', 'ort-wasm-simd-threaded.asyncify.wasm']

const $ = <T extends HTMLElement = HTMLElement>(id: string) => document.getElementById(id) as T
const url = (path: string) => new URL(path, location.href).href
const mb = (b: number) => Math.round(b / 1048576)
const show = (id: string, text: string | null) => {
  const el = $(id)
  el.hidden = text == null
  if (text != null) el.textContent = text
}

const diag: Record<string, unknown> = {
  page: 'prueba-offline/1',
  userAgent: navigator.userAgent,
  crossOriginIsolated: self.crossOriginIsolated,
  hardwareConcurrency: navigator.hardwareConcurrency ?? null,
  deviceMemoryGB: (navigator as Navigator & { deviceMemory?: number }).deviceMemory ?? null,
  standalone: matchMedia('(display-mode: standalone)').matches || (navigator as Navigator & { standalone?: boolean }).standalone === true,
  webgpu: 'gpu' in navigator,
}
const threads = self.crossOriginIsolated ? Math.max(1, Math.min(4, navigator.hardwareConcurrency || 4)) : 1
diag.threads = threads

function render() {
  $('diag').textContent = JSON.stringify(diag, null, 1)
}

// ---------------------------------------------------------------- a page closed in the middle of a step

const crashed = localStorage.getItem(RUNNING)
if (crashed) {
  localStorage.removeItem(RUNNING)
  let step = crashed
  try { step = JSON.parse(crashed).step } catch { /* old value */ }
  diag.previousCrash = JSON.parse(crashed.startsWith('{') ? crashed : JSON.stringify({ step }))
  show('crash', `La página se cerró durante la identificación (probablemente memoria). Paso: ${step}. Envíe «Copiar resultado».`)
}
const last = localStorage.getItem(LAST)
if (last) try { diag.previousResult = JSON.parse(last) } catch { /* ignore */ }

function running<T>(step: string, work: () => Promise<T>): Promise<T> {
  localStorage.setItem(RUNNING, JSON.stringify({ step, at: new Date().toISOString() }))
  show('running', `${step}…`)
  return work().finally(() => {
    localStorage.removeItem(RUNNING)
    show('running', null)
  })
}

// ---------------------------------------------------------------- service worker, install, sign-in

if ('serviceWorker' in navigator) navigator.serviceWorker.register(url('../sw.js')).catch(e => (diag.serviceWorkerError = String(e)))

let installPrompt: (Event & { prompt: () => Promise<void> }) | null = null
addEventListener('beforeinstallprompt', e => {
  e.preventDefault()
  installPrompt = e as typeof installPrompt
  $('install').hidden = false
})
$('install').addEventListener('click', () => installPrompt?.prompt())

let signedIn = false
async function checkSession() {
  if (!navigator.onLine) {
    show('status', 'Sin conexión.')
    return
  }
  try {
    const r = await fetch(url('../api/auth/session'), { credentials: 'same-origin', cache: 'no-store' })
    const s = await r.json()
    signedIn = !!s.user
    show('status', s.user ? `Sesión: ${s.user.displayName || s.user.username}` : 'Sin sesión.')
    $('login').hidden = signedIn
  } catch {
    show('status', 'Sin conexión.')
  }
}

$('loginForm').addEventListener('submit', async e => {
  e.preventDefault()
  show('loginErr', null)
  try {
    const r = await fetch(url('../api/auth/login'), {
      method: 'POST',
      credentials: 'same-origin',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({ username: $<HTMLInputElement>('user').value, password: $<HTMLInputElement>('pass').value }),
    })
    if (!r.ok) throw new Error((await r.json().catch(() => ({})))?.error?.message || `HTTP ${r.status}`)
    $<HTMLInputElement>('pass').value = ''
    await checkSession()
    await listFiles()
    await refreshStock()
  } catch (err) {
    show('loginErr', `No se pudo entrar: ${(err as Error).message}`)
  }
})

// ---------------------------------------------------------------- the models: list, download, storage

let files: ModelFile[] | null = null

async function stored(): Promise<Stored | null> {
  const r = await (await caches.open(CACHE)).match(url('./models/stored.json'))
  return r ? r.json() : null
}

async function storageInfo() {
  const est = await navigator.storage?.estimate?.().catch(() => null)
  const persisted = await navigator.storage?.persisted?.().catch(() => null)
  diag.storage = est ? { usageMB: mb(est.usage ?? 0), quotaMB: mb(est.quota ?? 0) } : null
  diag.persisted = persisted ?? null
  $('storage').textContent =
    `Almacenamiento: ${est ? `${mb(est.usage ?? 0)} MB usados de ${mb(est.quota ?? 0)} MB disponibles` : 'sin datos'}; ` +
    `persistente: ${persisted == null ? 'sin datos' : persisted ? 'sí' : 'no'}` +
    (diag.persistRequest != null ? ` (pedido: ${diag.persistRequest ? 'concedido' : 'denegado'})` : '')
  // Only once there is something to lose (models stored or asked for); the download goes ahead either way.
  $('notPersisted').hidden = persisted !== false || !(diag.modelsReady || diag.persistRequest != null)
  render()
}

async function refreshReady() {
  const s = await stored()
  const done = s && files ? files.every(f => s.files[f.name]?.etag === f.etag) : !!s
  const total = s ? Object.values(s.files).reduce((a, f) => a + f.bytes, 0) : 0
  diag.modelsReady = !!(s && done)
  show('ready', s && done ? `Listo sin conexión ✓ (${mb(total)} MB)` : s ? 'Hay modelos guardados, pero el servidor tiene otra versión: descargue de nuevo.' : null)
  $('remove').hidden = !s
  await storageInfo()
}

async function listFiles() {
  if (!navigator.onLine) return
  const r = await fetch(url('./models/list.json'), { credentials: 'same-origin', cache: 'no-store' }).catch(() => null)
  if (!r) return
  if (r.status === 401) {
    $('login').hidden = false
    return
  }
  if (!r.ok) {
    show('dlErr', `No se pudo leer la lista de modelos (HTTP ${r.status}).`)
    return
  }
  const list = (await r.json()) as { files: ModelFile[]; partBytes?: number }
  if (list.partBytes && list.partBytes !== PART) {
    show('dlErr', 'El servidor usa otro tamaño de parte: recargue la página.')
    return
  }
  files = list.files
  const wire = files.reduce((a, f) => a + (f.wire ?? f.bytes), 0), plain = files.reduce((a, f) => a + f.bytes, 0)
  diag.listWire = { plainMB: mb(plain), wireMB: mb(wire), encodings: [...new Set(files.map(f => f.encoding ?? 'none'))] }
  $('files').textContent =
    files.map(f => `${f.name} (${f.bytes < 1048576 ? '<1' : mb(f.bytes)} MB)`).join(' · ') + (wire < plain ? ` · ${mb(wire)} MB por la red (comprimido)` : '')
  await refreshReady()
}

// ---------------------------------------------------------------- the flash-frozen stock (server/frozen-stock.mjs)

/**
 * The snapshot as kept with the models: its ETag, when the server's copy last read the workbook (x-synced-at:
 * the inventory's date) and when this phone last asked (x-checked-at).
 */
const STOCK_KEY = () => url('./models/frozen-stock.json')
let stock: FrozenStock | null = null
let stockAsOf: string | null = null

function showStock(syncedAt: string | null, checkedAt: string | null) {
  stockAsOf = stock ? syncedAt || stock.generatedAt : null
  const withStock = stock ? Object.values(stock.species).filter(c => c.total).length : 0
  show('stockInfo', stock ? `${stockDate(stockAsOf!)} (${withStock} especies con congelados)` : null)
  diag.stock = stock ? { version: stock.version, generatedAt: stock.generatedAt, syncedAt, checkedAt } : null
}

async function loadStock() {
  const r = await (await caches.open(CACHE)).match(STOCK_KEY())
  try {
    stock = r ? ((await r.json()) as FrozenStock) : null
  } catch {
    stock = null
  }
  showStock(r?.headers.get('x-synced-at') || null, r?.headers.get('x-checked-at') || null)
}

/** Asked again whenever online (cheap: the ETag answers 304 when nothing changed). Never throws. */
async function refreshStock() {
  if (!navigator.onLine) return
  try {
    const cache = await caches.open(CACHE)
    const have = await cache.match(STOCK_KEY())
    const etag = have?.headers.get('etag')
    // A weak signal in the field must not hold the page up: given up after 10 s (the kept copy stays).
    const r = await fetch(url('../api/frozen-stock'), {
      credentials: 'same-origin',
      cache: 'no-store',
      headers: etag ? { 'if-none-match': etag } : {},
      signal: AbortSignal.timeout(10_000),
    })
    const now = new Date().toISOString()
    const synced = r.headers.get('x-synced-at') || ''
    const keep = (body: string, tag: string) =>
      cache.put(STOCK_KEY(), new Response(body, { headers: { 'content-type': 'application/json', etag: tag, 'x-synced-at': synced, 'x-checked-at': now } }))
    if (r.status === 304 && have) await keep(await have.text(), etag!)
    else if (r.ok) await keep(await r.text(), r.headers.get('etag') ?? '')
    else return
    await loadStock()
  } catch (err) {
    diag.stockError = errorText(err).slice(0, 200)
  }
}

/** The page itself, its scripts and the runtime: kept for airplane mode (the service worker serves them). */
async function keepPage(cache: Cache) {
  const own = [...document.querySelectorAll<HTMLScriptElement | HTMLLinkElement>('script[src], link[rel=stylesheet], link[rel=modulepreload], link[rel=manifest]')]
    .map(e => ('src' in e && e.src) || (e as HTMLLinkElement).href)
    .filter(Boolean)
  const ort = [...ORT_FILES, ...('gpu' in navigator ? ORT_GPU_FILES : [])].map(f => url(`./ort/${f}`))
  for (const u of [url('./'), ...own, ...ort]) {
    const r = await fetch(u, { cache: 'no-cache' })
    if (!r.ok) throw new Error(`${u}: HTTP ${r.status}`)
    await cache.put(u, r)
  }
}

async function download() {
  show('dlErr', null)
  const button = $<HTMLButtonElement>('download')
  button.disabled = true
  // Asked first, still inside the click (Chrome weighs the gesture and the installed app); shown again after the download.
  const persist = navigator.storage?.persist?.().catch(() => null) ?? Promise.resolve(null)
  try {
    diag.persistRequest = await persist
    await storageInfo()
    if (!files) await listFiles()
    if (!files) throw new Error(signedIn ? 'No se pudo leer la lista de modelos.' : 'Inicie sesión primero.')
    const cache = await caches.open(CACHE)
    $('progressBox').hidden = false
    const t0 = performance.now()
    show('progressText', 'La página y el motor…')
    await keepPage(cache)
    await refreshStock()
    // Counted in plain bytes: the browser decompresses the parts (Content-Encoding) and hands over plain bytes.
    const total = files.reduce((a, f) => a + f.bytes, 0)
    let done = 0
    const encodings: Record<string, number> = {}
    const progress = () => {
      $<HTMLProgressElement>('progress').value = done / total
      $('progressText').textContent = `${mb(done)} de ${mb(total)} MB`
    }
    const keep: Stored = { files: {}, at: new Date().toISOString() }
    for (const f of files) {
      const parts = Math.ceil(f.bytes / PART)
      for (let i = 0; i < parts; i++) {
        const key = url(`./models/${f.name}?part=${i}`)
        const start = i * PART, end = Math.min(f.bytes, start + PART) - 1
        const have = await cache.match(key)
        if (have && have.headers.get('x-etag') === f.etag && Number(have.headers.get('x-bytes')) === end - start + 1) {
          done += end - start + 1
          progress()
          continue
        }
        let chunks: Uint8Array[] = []
        for (let attempt = 1; ; attempt++) {
          chunks = []
          let got = 0
          try {
            // One part per request: the server sends it precompressed (zstd or gzip) when it can.
            const r = await fetch(key, { credentials: 'same-origin', cache: 'no-store' })
            if (r.status !== 200) throw new Error(`HTTP ${r.status}`)
            // The part's ETag is the listed file's (W/"<file etag>-p<i>"): a model changed since the list is not mixed in.
            if (r.headers.get('etag') !== `W/${f.etag.slice(0, -1)}-p${i}"` || Number(r.headers.get('x-part-bytes')) !== end - start + 1) {
              r.body?.cancel().catch(() => {})
              throw Object.assign(new Error('el modelo cambió en el servidor: recargue la página'), { final: true })
            }
            const enc = r.headers.get('content-encoding') || 'none'
            encodings[enc] = (encodings[enc] ?? 0) + 1
            const reader = r.body!.getReader()
            for (;;) {
              const { done: end_, value } = await reader.read()
              if (end_) break
              chunks.push(value)
              got += value.length
              done += value.length
              progress()
            }
            if (got !== end - start + 1) throw new Error('parte incompleta')
            break
          } catch (err) {
            done -= got
            if (attempt >= 4 || (err as { final?: boolean }).final) throw new Error(`${f.name} parte ${i + 1}/${parts}: ${(err as Error).message}`)
            show('progressText', `Reintentando (${(err as Error).message})…`)
            await new Promise(r => setTimeout(r, 2000 * attempt))
          }
        }
        await cache.put(key, new Response(new Blob(chunks as BlobPart[]), { headers: { 'x-etag': f.etag, 'x-bytes': String(end - start + 1) } }))
      }
      keep.files[f.name] = { etag: f.etag, bytes: f.bytes }
    }
    await cache.put(url('./models/stored.json'), new Response(JSON.stringify(keep), { headers: { 'content-type': 'application/json' } }))
    diag.downloadSeconds = Math.round((performance.now() - t0) / 1000)
    diag.downloadEncodings = encodings
    show('progressText', `Descargado en ${diag.downloadSeconds} s.`)
  } catch (err) {
    show('dlErr', `La descarga falló: ${(err as Error).message}`)
    diag.downloadError = String((err as Error).message)
  } finally {
    button.disabled = false
    await refreshReady()
  }
}

async function readModel(name: string): Promise<Uint8Array> {
  const s = await stored()
  const f = s?.files[name]
  if (!f) throw new Error(`Falta ${name}: descargue los modelos.`)
  const cache = await caches.open(CACHE)
  const out = new Uint8Array(f.bytes)
  for (let i = 0, off = 0; off < f.bytes; i++) {
    const r = await cache.match(url(`./models/${name}?part=${i}`))
    if (!r) throw new Error(`Falta la parte ${i + 1} de ${name}: descargue de nuevo.`)
    const part = new Uint8Array(await r.arrayBuffer())
    out.set(part, off)
    off += part.length
  }
  return out
}

$('download').addEventListener('click', download)
$('remove').addEventListener('click', async () => {
  if (!confirm('¿Borrar los modelos guardados en este teléfono?')) return
  dropSessions()
  await caches.delete(CACHE)
  await loadStock() // gone with the models
  await refreshReady()
})

// ---------------------------------------------------------------- identify

interface Loaded { ort: OrtModule; gpuOrt: OrtModule | null; detector: Ort.InferenceSession; encoder: Ort.InferenceSession; head: Head; detectorBackend: string }
let loaded: Loaded | null = null
const loadTimes: Record<string, number> = {}

function dropSessions() {
  loaded?.detector.release().catch(() => {})
  loaded?.encoder.release().catch(() => {})
  loaded = null
}

async function importOrt(file: string): Promise<OrtModule> {
  const m = (await import(/* @vite-ignore */ url(`./ort/${file}`))) as OrtModule
  m.env.wasm.wasmPaths = url('./ort/')
  m.env.logLevel = 'error'
  return m
}

async function load(useGpu: boolean): Promise<Loaded> {
  const wanted = useGpu ? 'webgpu' : 'wasm'
  if (loaded && loaded.detectorBackend.startsWith(wanted)) return loaded
  const s = await stored()
  if (!s) throw new Error('Primero descargue los modelos.')
  const name = (role: string) => Object.keys(s.files).find(n => (role === 'encoder' ? n.startsWith('bioclip') : role === 'detector' ? n.startsWith('wing_seg') : role === 'meta' ? n.endsWith('.json') : n.endsWith('.bin')))!
  let t = performance.now()
  const ort = loaded?.ort ?? (await importOrt(ORT_FILES[0]))
  ort.env.wasm.numThreads = threads
  let detector: Ort.InferenceSession | null = null, detectorBackend = 'wasm', gpuOrt: OrtModule | null = null
  const detBytes = await readModel(name('detector'))
  if (useGpu) {
    try {
      gpuOrt = await importOrt(ORT_GPU_FILES[0])
      gpuOrt.env.wasm.numThreads = 1
      detector = await gpuOrt.InferenceSession.create(detBytes, { executionProviders: ['webgpu'], graphOptimizationLevel: 'all' })
      detectorBackend = 'webgpu'
    } catch (err) {
      diag.webgpuError = String((err as Error).message).slice(0, 300)
      detectorBackend = 'wasm (WebGPU falló)'
    }
  }
  loaded?.detector.release().catch(() => {})
  detector ??= await ort.InferenceSession.create(detBytes, { executionProviders: ['wasm'], graphOptimizationLevel: 'all' })
  loadTimes.detector_load_ms = Math.round(performance.now() - t)
  let encoder = loaded?.encoder
  if (!encoder) {
    t = performance.now()
    encoder = await running('Cargando el codificador (638 MB) en memoria', async () => {
      let bytes: Uint8Array | null = await readModel(name('encoder'))
      loadTimes.encoder_read_ms = Math.round(performance.now() - t)
      const t1 = performance.now()
      const session = await ort.InferenceSession.create(bytes, { executionProviders: ['wasm'], graphOptimizationLevel: 'all' })
      bytes = null
      loadTimes.encoder_session_ms = Math.round(performance.now() - t1)
      return session
    })
  }
  let head = loaded?.head
  if (!head) {
    t = performance.now()
    const meta = JSON.parse(new TextDecoder().decode(await readModel(name('meta')))) as HeadMeta
    const w = await readModel(name('weights'))
    head = makeHead(meta, w.buffer as ArrayBuffer)
    loadTimes.head_load_ms = Math.round(performance.now() - t)
  }
  loaded = { ort, gpuOrt, detector, encoder, head, detectorBackend }
  diag.backend = { detector: detectorBackend, encoder: `wasm, ${threads} hilo(s)` }
  return loaded
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

async function identify(photo: Photo) {
  show('idErr', null)
  delete diag.identifyError
  const times: Record<string, number> = {}
  const tick = (key: string, t: number) => (times[key] = Math.round(performance.now() - t))
  const side = ($<HTMLSelectElement>('side').value || null) as Side
  try {
    const useGpu = $<HTMLInputElement>('useGpu').checked
    const m = await running('Cargando los modelos', () => load(useGpu))
    const out = await running('Identificando', async () => {
      let t = performance.now()
      const raw = photo.bytes
      const orientation = exifOrientation(raw)
      const override = m.head.meta.full_photo_overrides.includes(await sha256(raw))
      const img = await decode(photo.blob)
      tick('decode_ms', t)
      t = performance.now()
      const inp = detectorInput(img, orientation)
      tick('detector_prep_ms', t)
      t = performance.now()
      const ortD = m.detectorBackend === 'webgpu' && m.gpuOrt ? m.gpuOrt : m.ort
      const res = await m.detector.run({ [m.detector.inputNames[0]]: new ortD.Tensor('float32', inp.x, [1, 3, 640, 640]) })
      tick('detector_run_ms', t)
      t = performance.now()
      const det = res[m.detector.outputNames[0]], proto = res[m.detector.outputNames[1]]
      const masks = readMasks(det.data as Float32Array, det.dims[1], det.dims[2], proto.data as Float32Array, proto.dims[2], inp)
      const selected = selectMasks(masks)
      const { box, status } = cropBox(selected, orientation, img.width, img.height, override)
      const square = letterboxCrop(img, box)
      tick('crop_ms', t)
      t = performance.now()
      const enc = await m.encoder.run({ [m.encoder.inputNames[0]]: new m.ort.Tensor('float32', encoderInput(square), [1, 3, 224, 224]) })
      const feature = enc[m.encoder.outputNames[0]].data as Float32Array
      tick('encoder_run_ms', t)
      t = performance.now()
      const top = speciesTop(m.head, feature, side)
      tick('head_ms', t)
      return { orientation, img, box, status, square, top, masks: masks.length, selected: selected.length }
    })
    const total = Object.values(times).reduce((a, b) => a + b, 0)
    drawCrop(out.square)
    $('cropNote').textContent = out.status === 'success' ? `Alas encontradas (${out.selected})` : out.status === 'centre_crop_fallback' ? 'Sin alas: centro de la foto (90 %)' : 'Foto completa'
    $('top').innerHTML = ''
    for (const [name, p] of out.top) {
      const tr = document.createElement('tr')
      tr.innerHTML = `<td><i></i></td><td class="num">${(p * 100).toFixed(1)} %</td>`
      tr.querySelector('i')!.textContent = name
      if (stock) {
        const line = document.createElement('div')
        line.className = 'stock'
        for (const part of speciesStock(stock, name)) {
          const span = document.createElement('span')
          span.textContent = part.text
          if (part.low) span.className = 'low'
          line.append(span)
        }
        const genus = genusStock(stock, name)
        if (genus) line.append(Object.assign(document.createElement('div'), { className: 'genus', textContent: genus }))
        tr.firstElementChild!.append(line)
      }
      $('top').append(tr)
    }
    const labels: Record<string, string> = {
      encoder_read_ms: 'Leer codificador del almacenamiento', encoder_session_ms: 'Preparar codificador', detector_load_ms: 'Cargar detector', head_load_ms: 'Cargar cabeza',
      decode_ms: 'Abrir foto', detector_prep_ms: 'Preparar foto', detector_run_ms: `Detector de alas (${m.detectorBackend})`, crop_ms: 'Recorte', encoder_run_ms: `Codificador (wasm, ${threads} hilo(s))`, head_ms: 'Cabeza',
    }
    const rows = [...Object.entries(loadTimes).map(([k, v]) => [k, v, true] as const), ...Object.entries(times).map(([k, v]) => [k, v, false] as const)]
    $('times').innerHTML = ''
    for (const [k, v, once] of rows) {
      const tr = document.createElement('tr')
      tr.innerHTML = `<td></td><td class="num">${(v / 1000).toFixed(2)} s</td>`
      tr.firstElementChild!.textContent = labels[k] + (once ? ' (una vez)' : '')
      $('times').append(tr)
    }
    const tr = document.createElement('tr')
    tr.innerHTML = `<td><b>Total por foto</b></td><td class="num"><b>${(total / 1000).toFixed(2)} s</b></td>`
    $('times').append(tr)
    show('stockNote', stock ? stockDate(stockAsOf!) : 'Sin inventario de congelados: abra esta página una vez con conexión.')
    $('result').hidden = false
    const result = {
      at: new Date().toISOString(), file: { name: photo.name, bytes: photo.size, type: photo.type }, size: [out.img.width, out.img.height], orientation: out.orientation,
      side, crop: out.status, box: out.box, masks: out.masks, kept: out.selected, top5: out.top, times: { ...times, total_ms: total }, load: { ...loadTimes }, backend: diag.backend,
    }
    diag.lastResult = result
    ;(diag.results as unknown[] | undefined) ? (diag.results as unknown[]).push(result) : (diag.results = [result])
    localStorage.setItem(LAST, JSON.stringify(result))
    ;(window as unknown as { offlineResult: unknown }).offlineResult = result
  } catch (err) {
    const msg = errorText(err)
    diag.identifyError = msg.slice(0, 500)
    show('idErr', `Error: ${msg}${/memory|memoria|allocation|out of/i.test(msg) ? '\n(Parece falta de memoria.)' : ''}`)
    ;(window as unknown as { offlineResult: unknown }).offlineResult = { error: msg }
  } finally {
    await storageInfo()
  }
}

function drawCrop(rgb: Uint8Array) {
  const c = $<HTMLCanvasElement>('crop'), ctx = c.getContext('2d')!
  const im = ctx.createImageData(224, 224)
  for (let i = 0; i < 224 * 224; i++) {
    im.data[i * 4] = rgb[i * 3]
    im.data[i * 4 + 1] = rgb[i * 3 + 1]
    im.data[i * 4 + 2] = rgb[i * 3 + 2]
    im.data[i * 4 + 3] = 255
  }
  ctx.putImageData(im, 0, 0)
}

for (const id of ['camera', 'pick']) {
  // The photo's bytes are read here, before the input is cleared and before any model loads: on
  // Android the File's read grant is gone by then (NotReadableError). See lib/offlinePhoto.ts.
  $<HTMLInputElement>(id).addEventListener('change', e => {
    show('idErr', null)
    takePhoto(e.target as HTMLInputElement, identify, err => {
      const msg = errorText(err)
      diag.identifyError = `photo read (${id}): ${msg}`.slice(0, 500)
      show('idErr', `${PHOTO_UNREADABLE}.\n(${msg})`)
      $('result').hidden = true // the last photo's result is not this one's
      ;(window as unknown as { offlineResult: unknown }).offlineResult = { error: msg }
      render()
    })
  })
}

$('copy').addEventListener('click', async () => {
  diag.copiedAt = new Date().toISOString()
  diag.online = navigator.onLine
  await storageInfo()
  const text = JSON.stringify(diag, null, 1)
  try {
    await navigator.clipboard.writeText(text)
  } catch {
    const ta = document.createElement('textarea')
    ta.value = text
    document.body.append(ta)
    ta.select()
    document.execCommand('copy')
    ta.remove()
  }
  $('copied').hidden = false
  setTimeout(() => ($('copied').hidden = true), 3000)
})

$<HTMLInputElement>('useGpu').checked = 'gpu' in navigator
$<HTMLInputElement>('useGpu').disabled = !('gpu' in navigator)
addEventListener('online', () => { checkSession().then(listFiles); refreshStock() })
addEventListener('offline', () => show('status', 'Sin conexión.'))
render()
loadStock().then(refreshStock)
checkSession().then(listFiles).finally(refreshReady)
