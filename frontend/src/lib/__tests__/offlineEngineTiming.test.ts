import { afterEach, describe, expect, it, vi } from 'vitest'

// The timing line's load phase on the real engine (offlineEngine.ts) with a slow runtime import and a slow head read:
// the phase measured once (lib/identifyTiming.ts timed) covers both, and a reload is seen as a load even when it takes
// exactly as long as the first (Engine.loadGeneration, not the durations). The runtime and the server are stand-ins;
// no model, no AI.

vi.resetModules()
const { createEngine } = await import('../offlineEngine')
const { timed, phoneTiming, timingLine } = await import('../identifyTiming')

const BASE = 'https://x.test/prueba-offline/'
const RUNTIME_MS = 150
const HEAD_MS = 200
const sleep = (ms: number) => new Promise(r => setTimeout(r, ms))
const meta = { schema: 'ithomiini-offline-head/1', head_sha256: '', weights: 'head.bin', rows: 1, dim: 2, K: 1, s: 30, ln_eps: 1e-5, ln_w: [1, 1], ln_b: [0, 0], species: ['Aa bb'], leaf2sp: [0], full_photo_overrides: [] }
const META = new TextEncoder().encode(JSON.stringify(meta))
const files = [
  { name: 'wing_seg_s2.onnx', bytes: 4, etag: '"d"', role: 'detector' as const },
  { name: 'student.onnx', bytes: 4, etag: '"s"', role: 'student' as const },
  { name: 'head.json', bytes: META.length, etag: '"m"', role: 'head-meta' as const },
  { name: 'head.bin', bytes: 4, etag: '"h"', role: 'head' as const },
]
const bodyOf = (u: string) => (u.includes('head.json') ? META : new Uint8Array([9, 9, 9, 9]))

function setup() {
  const store = new Map<string, { body: ArrayBuffer; headers: Headers }>()
  let slowHead = false
  const cache = {
    match: async (k: string) => {
      if (slowHead && k.includes('head.bin')) await sleep(HEAD_MS)
      return store.has(k) ? new Response(store.get(k)!.body, { headers: store.get(k)!.headers }) : undefined
    },
    put: async (k: string, r: Response) => void store.set(k, { body: await r.arrayBuffer(), headers: r.headers }),
    delete: async (k: string | { url: string }) => store.delete(typeof k === 'string' ? k : k.url),
    keys: async () => [...store.keys()].map(url => ({ url })),
  }
  vi.stubGlobal('self', { crossOriginIsolated: false })
  vi.stubGlobal('caches', { open: async () => cache, delete: async () => true })
  vi.stubGlobal('fetch', async (url: string) => {
    const u = String(url)
    // the runtime's files (kept for airplane mode) and anything else: an empty answer
    const f = files.find(x => u.includes(`/models/${x.name}?`))
    if (!f) return new Response('')
    return new Response(bodyOf(u), { status: 200, headers: { etag: `W/${f.etag.slice(0, -1)}-p0"`, 'x-part-bytes': String(f.bytes) } })
  })
  const session = { inputNames: ['x'], outputNames: ['y'], release: async () => {}, run: async () => ({}) }
  const runtime = vi.fn(async () => {
    await sleep(RUNTIME_MS)
    return { env: { wasm: {}, logLevel: 'error' }, InferenceSession: { create: async () => session }, Tensor: class {} } as never
  })
  const engine = createEngine(BASE, () => [], { signedIn: () => false, runtime })
  return { engine, runtime, slow: (on: boolean) => (slowHead = on) }
}

afterEach(() => {
  vi.unstubAllGlobals()
  vi.restoreAllMocks()
})

describe('the first photo\'s load phase', () => {
  it('covers the runtime import and the head read, once', async () => {
    const { engine, runtime, slow } = setup()
    await engine.download('small', files, () => {})
    slow(true)
    const phase = await timed(engine.loadGeneration, () => engine.load('small'))
    expect(runtime).toHaveBeenCalledTimes(1)
    expect(engine.loaded('small')).toBe(true)
    expect(phase.fresh).toBe(true)
    // the runtime (150 ms) and the head (200 ms) are both inside; the engine's own *_load keys see neither
    expect(phase.ms).toBeGreaterThanOrEqual(RUNTIME_MS + HEAD_MS - 5)
    expect((engine.lastMs.small_load ?? 0) + (engine.lastMs.detector_load ?? 0)).toBeLessThan(RUNTIME_MS)
    // the line: the photo's 1,2 s plus what loading added
    const line = timingLine({ source: 'phone', ms: 1200, timing: phoneTiming({ ms: phase.ms + 1200, fresh: true }, 1200) }, s => s.replace('{s}', '…'), 'es')
    expect(line).toContain('(primera vez, cargando modelos)')
    expect(phoneTiming({ ms: phase.ms + 1200, fresh: true }, 1200).loadMs).toBe(phase.ms)
    // a second photo: nothing to load, no flag
    const again = await timed(engine.loadGeneration, () => engine.load('small'))
    expect(again.fresh).toBe(false)
    expect(runtime).toHaveBeenCalledTimes(1)
  })

  it('regression: a reload after removing the model counts as a load even when its durations equal the first', async () => {
    const { engine } = setup()
    // every duration the engine and timed measure is 0 ms: the first load and the reload "take" the same time
    vi.spyOn(performance, 'now').mockReturnValue(1000)
    await engine.download('small', files, () => {})
    const first = await timed(engine.loadGeneration, () => engine.load('small'))
    const before = { ...engine.lastMs }
    await engine.removeModel('small')
    expect(engine.loaded('small')).toBe(false)
    await engine.download('small', files, () => {})
    const reload = await timed(engine.loadGeneration, () => engine.load('small'))
    // the old rule (a changed *_load value) would see no load here: the values are equal
    expect(engine.lastMs.small_load).toBe(before.small_load)
    expect(engine.lastMs.detector_load).toBe(before.detector_load)
    expect(first.fresh).toBe(true)
    expect(reload.fresh).toBe(true)
    expect(timingLine({ source: 'phone', ms: 900, timing: phoneTiming(reload, 900) }, s => s, 'es')).toContain('(primera vez, cargando modelos)')
  })
})
