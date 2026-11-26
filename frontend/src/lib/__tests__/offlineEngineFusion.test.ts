import { createHash } from 'node:crypto'
import { afterEach, describe, expect, it, vi } from 'vitest'
import type { HeadMeta } from '../offlineId'

vi.resetModules()
const { createEngine, LIST_PATH } = await import('../offlineEngine')

const BASE = 'https://fusion.test/prueba-offline/'
const sha = (b: Uint8Array) => createHash('sha256').update(b).digest('hex')

function memoryCaches() {
  const store = new Map<string, { body: ArrayBuffer; headers: Headers }>()
  const cache = {
    match: async (k: string) => (store.has(k) ? new Response(store.get(k)!.body, { headers: store.get(k)!.headers }) : undefined),
    put: async (k: string, r: Response) => void store.set(k, { body: await r.arrayBuffer(), headers: r.headers }),
    delete: async (k: string | { url: string }) => store.delete(typeof k === 'string' ? k : k.url),
    keys: async () => [...store.keys()].map(url => ({ url })),
  }
  vi.stubGlobal('self', { crossOriginIsolated: false })
  vi.stubGlobal('caches', { open: async () => cache, delete: async () => true })
  vi.stubGlobal('navigator', { onLine: true })
  return store
}

const storedJson = (store: Map<string, { body: ArrayBuffer }>) => JSON.parse(new TextDecoder().decode(store.get(`${BASE}models/stored.json`)!.body))
const partKeys = (store: Map<string, unknown>) => [...store.keys()].filter(k => k.includes('?part=')).map(k => k.slice(BASE.length)).sort()

// Minimal valid legacy & fusion head fixtures
const legacyMeta: HeadMeta = {
  schema: 'ithomiini-offline-head/2', head_sha256: 'legacy', weights: 'head_6fa6ab78_fp16.bin',
  rows: 2, dim: 2, K: 1, s: 30, ln_eps: 1e-5, ln_w: [1, 1], ln_b: [0, 0],
  species: ['Sp a', 'Sp b'], leaf2sp: [0, 1], leaves: ['Sp a', 'Sp b'], is_ssp: '00', geo: null, full_photo_overrides: [],
}
const legacyBin = new Uint8Array(new Uint16Array([0x3c00, 0, 0, 0x3c00]).buffer)

// Fusion bin: 3 branches (bio dim=2 b=1, w1c dim=2 b=1, w1d dim=2 b=1), each: down_w(1x2=4B), up_w(2x1=4B), W(2x2=8B) => 16B/branch = 48B
const fusionBin = new Uint8Array(48)
{
  const u16 = new Uint16Array(fusionBin.buffer)
  for (const bIdx of [0, 1, 2]) {
    const o = bIdx * 8
    u16[o + 0] = 0x3c00; u16[o + 1] = 0
    u16[o + 2] = 0x3c00; u16[o + 3] = 0
    u16[o + 4] = 0x3c00; u16[o + 5] = 0; u16[o + 6] = 0; u16[o + 7] = 0x3c00
  }
}
const fusionMeta: HeadMeta = {
  schema: 'ithomiini-offline-fusion-head/1', slot: 'slot1', head_sha256: 'slot1test',
  weights: 'head_slot1_7d54b54f_fp16.bin', weights_bytes: fusionBin.byteLength,
  rows: 2, K: 1, blend_mode: 'prob', T_disp: 0.97,
  branches: [
    { name: 'bio', role: 'encoder', input_size: 224, norm: 'clip', l2_normalize_input: true, weight: 0.5, arch: 'res_adapter', dim: 2, bottleneck: 1, residual_scale: 0.2, s: 25, ln_eps: 1e-5, dtype: 'float16', offsets: { down_w: [0, 1, 2], up_w: [4, 2, 1], W: [8, 2, 2] }, down_b: [0], up_b: [0, 0], ln_w: [1, 1], ln_b: [0, 0] },
    { name: 'w1c', role: 'dino_w1c', input_size: 224, norm: 'imagenet', l2_normalize_input: false, weight: 0.25, arch: 'res_adapter', dim: 2, bottleneck: 1, residual_scale: 0.2, s: 25, ln_eps: 1e-5, dtype: 'float16', offsets: { down_w: [16, 1, 2], up_w: [20, 2, 1], W: [24, 2, 2] }, down_b: [0], up_b: [0, 0], ln_w: [1, 1], ln_b: [0, 0] },
    { name: 'w1d', role: 'dino_w1d', input_size: 208, norm: 'imagenet', l2_normalize_input: false, weight: 0.25, arch: 'res_adapter', dim: 2, bottleneck: 1, residual_scale: 0.2, s: 25, ln_eps: 1e-5, dtype: 'float16', offsets: { down_w: [32, 1, 2], up_w: [36, 2, 1], W: [40, 2, 2] }, down_b: [0], up_b: [0, 0], ln_w: [1, 1], ln_b: [0, 0] },
  ],
  species: ['Sp a', 'Sp b'], leaf2sp: [0, 1], leaves: ['Sp a', 'Sp b'], is_ssp: '00', geo: null, full_photo_overrides: [],
}

const BYTES: Record<string, Uint8Array> = {
  'wing_seg_s2.onnx': new Uint8Array([1, 2, 3, 4]),
  'bioclip25h_visual_int8.onnx': new Uint8Array([10, 11, 12, 13, 14, 15, 16, 17]),
  'student_W2_int8.onnx': new Uint8Array([20, 21, 22, 23]),
  'head_6fa6ab78.json': new TextEncoder().encode(JSON.stringify(legacyMeta)),
  'head_6fa6ab78_fp16.bin': legacyBin,
  'w1c_pre384_fp16.onnx': new Uint8Array([30, 31, 32, 33, 34, 35]),
  'w1d_pre384_fp16.onnx': new Uint8Array([40, 41, 42, 43, 44, 45, 46]),
  'head_slot1_7d54b54f.json': new TextEncoder().encode(JSON.stringify(fusionMeta)),
  'head_slot1_7d54b54f_fp16.bin': fusionBin,
}

const FUSION_NOTES = { es: '93,4 % vs 88,6 % (+4,8 pp).', en: '93.4 % vs 88.6 % (+4.8 pp).' }

function makeList(includeFusion: boolean) {
  const base = [
    { name: 'wing_seg_s2.onnx', role: 'detector' as const, etag: '"det"', bytes: BYTES['wing_seg_s2.onnx'].length, wire: BYTES['wing_seg_s2.onnx'].length, sha256: sha(BYTES['wing_seg_s2.onnx']), parts: [sha(BYTES['wing_seg_s2.onnx'])] },
    { name: 'bioclip25h_visual_int8.onnx', role: 'encoder' as const, etag: '"bio"', bytes: BYTES['bioclip25h_visual_int8.onnx'].length, wire: BYTES['bioclip25h_visual_int8.onnx'].length, sha256: sha(BYTES['bioclip25h_visual_int8.onnx']), parts: [sha(BYTES['bioclip25h_visual_int8.onnx'])] },
    { name: 'head_6fa6ab78.json', role: 'head-meta' as const, etag: '"hm"', bytes: BYTES['head_6fa6ab78.json'].length, wire: BYTES['head_6fa6ab78.json'].length, sha256: sha(BYTES['head_6fa6ab78.json']), parts: [sha(BYTES['head_6fa6ab78.json'])] },
    { name: 'head_6fa6ab78_fp16.bin', role: 'head' as const, etag: '"hb"', bytes: BYTES['head_6fa6ab78_fp16.bin'].length, wire: BYTES['head_6fa6ab78_fp16.bin'].length, sha256: sha(BYTES['head_6fa6ab78_fp16.bin']), parts: [sha(BYTES['head_6fa6ab78_fp16.bin'])] },
    { name: 'student_W2_int8.onnx', role: 'student' as const, etag: '"st"', bytes: BYTES['student_W2_int8.onnx'].length, wire: BYTES['student_W2_int8.onnx'].length, sha256: sha(BYTES['student_W2_int8.onnx']), parts: [sha(BYTES['student_W2_int8.onnx'])], input: { size: 182, values: 'clip' } },
  ]
  if (!includeFusion) return base
  return [
    ...base,
    { name: 'w1c_pre384_fp16.onnx', role: 'dino-w1c' as const, etag: '"w1c"', bytes: BYTES['w1c_pre384_fp16.onnx'].length, wire: 4, sha256: sha(BYTES['w1c_pre384_fp16.onnx']), parts: [sha(BYTES['w1c_pre384_fp16.onnx'])], version: 'slot1-3bb-20261126' },
    { name: 'w1d_pre384_fp16.onnx', role: 'dino-w1d' as const, etag: '"w1d"', bytes: BYTES['w1d_pre384_fp16.onnx'].length, wire: 5, sha256: sha(BYTES['w1d_pre384_fp16.onnx']), parts: [sha(BYTES['w1d_pre384_fp16.onnx'])], version: 'slot1-3bb-20261126' },
    { name: 'head_slot1_7d54b54f.json', role: 'fusion-head-meta' as const, etag: '"fhm"', bytes: BYTES['head_slot1_7d54b54f.json'].length, wire: 100, sha256: sha(BYTES['head_slot1_7d54b54f.json']), parts: [sha(BYTES['head_slot1_7d54b54f.json'])], version: 'slot1-3bb-20261126', notes: FUSION_NOTES },
    { name: 'head_slot1_7d54b54f_fp16.bin', role: 'fusion-head' as const, etag: '"fhb"', bytes: BYTES['head_slot1_7d54b54f_fp16.bin'].length, wire: 40, sha256: sha(BYTES['head_slot1_7d54b54f_fp16.bin']), parts: [sha(BYTES['head_slot1_7d54b54f_fp16.bin'])], version: 'slot1-3bb-20261126' },
  ]
}

function mockFetch(list: ReturnType<typeof makeList>) {
  const asked: string[] = []
  vi.stubGlobal('fetch', async (url: string) => {
    const u = String(url)
    asked.push(u)
    if (u.includes('/ort/')) return new Response('')
    if (u.includes('list.json')) return Response.json({ files: list, partBytes: 32 * 1024 * 1024 })
    const f = list.find(x => u.includes(`/models/${x.name}?`))
    if (!f) return new Response('', { status: 404 })
    const b = BYTES[f.name]
    return new Response(b.slice(), { status: 200, headers: { etag: `W/${f.etag.slice(0, -1)}-p0"`, 'x-part-bytes': String(b.length) } })
  })
  return asked
}

afterEach(() => {
  vi.unstubAllGlobals()
  vi.restoreAllMocks()
})

describe('offlineEngine Slot 1 fusion lifecycle', () => {
  it('requests caps=versions,fusion, upgrades legacy More accurate by downloading ONLY the 4 delta files (not BioCLIP), retains legacy head for older tabs/saved results, and reuses the loaded BioCLIP session', async () => {
    expect(LIST_PATH).toBe('models/list.json?caps=versions,fusion')
    const store = memoryCaches()
    const legacyList = makeList(false)
    const fusionList = makeList(true)
    mockFetch(legacyList)

    // Stub runtime to count InferenceSession.create calls per model byte signature
    const createdByTag: number[] = []
    const runtime = async () => ({
      env: { wasm: {}, logLevel: 'error' },
      InferenceSession: {
        create: async (b: Uint8Array) => {
          createdByTag.push(b[0])
          return { inputNames: ['x'], outputNames: ['y'], release: async () => {}, run: async () => ({}) }
        },
      },
      Tensor: class { constructor(public type: string, public data: Float32Array, public dims: number[]) {} },
    }) as never

    const engine = createEngine(BASE, () => [], { signedIn: () => false, runtime })
    await engine.download('full', legacyList, () => {})
    await engine.load('full')
    expect((await engine.ready(legacyList)).full).toBe(true)
    expect(createdByTag).toEqual([1, 10]) // detector + BioCLIP

    // Now server offers Slot 1 fusion: phone keeps working and sees a delta 'update' (not 'missing').
    const before = await engine.stored()
    const off = engine.offer('full', fusionList, before)
    expect(off.state).toBe('update')
    expect(off.to).toBe('slot1-3bb-20261126')
    expect(off.notes).toEqual(FUSION_NOTES)
    const expectedDeltaPlain = BYTES['w1c_pre384_fp16.onnx'].length + BYTES['w1d_pre384_fp16.onnx'].length + BYTES['head_slot1_7d54b54f.json'].length + BYTES['head_slot1_7d54b54f_fp16.bin'].length
    expect(off.plain).toBe(expectedDeltaPlain)
    expect(off.wire).toBe(4 + 5 + 100 + 40)
    expect((await engine.ready(fusionList)).full).toBe(true)

    // Download the upgrade: BioCLIP and wing_seg are NOT fetched again!
    const asked = mockFetch(fusionList)
    await engine.download('full', fusionList, () => {})
    expect(asked.some(u => u.includes('bioclip25h_visual_int8.onnx'))).toBe(false)
    expect(asked.some(u => u.includes('wing_seg_s2.onnx'))).toBe(false)
    expect(asked.some(u => u.includes('w1c_pre384_fp16.onnx'))).toBe(true)
    expect(asked.some(u => u.includes('w1d_pre384_fp16.onnx'))).toBe(true)
    expect(asked.some(u => u.includes('head_slot1_7d54b54f.json'))).toBe(true)
    expect(asked.some(u => u.includes('head_slot1_7d54b54f_fp16.bin'))).toBe(true)

    // Legacy head_6fa6ab78 is RETAINED so older open tabs sharing the cache and saved pre-fusion history entries stay readable!
    const after = storedJson(store)
    expect(Object.keys(after.files).sort()).toEqual([
      'bioclip25h_visual_int8.onnx',
      'head_6fa6ab78.json',
      'head_6fa6ab78_fp16.bin',
      'head_slot1_7d54b54f.json',
      'head_slot1_7d54b54f_fp16.bin',
      'w1c_pre384_fp16.onnx',
      'w1d_pre384_fp16.onnx',
      'wing_seg_s2.onnx',
    ])
    // A saved legacy result can still revive with loadHead('small') (legacy head) AND fusion loads via loadHead('full')!
    expect((await engine.loadHead('small')).fusion).toBeNull()
    expect((await engine.loadHead('full')).fusion).not.toBeNull()

    // Loading 'full' after the delta upgrade loads ONLY w1c (30) and w1d (40) — BioCLIP (10) & detector (1) are reused in memory!
    expect(engine.loaded('full')).toBe(false)
    await engine.load('full')
    expect(engine.loaded('full')).toBe(true)
    expect(createdByTag).toEqual([1, 10, 30, 40])

    expect(engine.offer('full', fusionList, after).state).toBe('current')
    // Viewing a non-fusion listing (e.g. signed-out HF) does NOT nag to downgrade!
    expect(engine.offer('full', legacyList, after).state).toBe('current')
    expect(engine.toDownload('full', legacyList, after).plain).toBe(0)

    // Installing Fast and then removing Fast must retain the legacy head while fusion More accurate stays installed!
    await engine.download('small', fusionList, () => {})
    await engine.removeModel('small')
    expect(storedJson(store).files['head_6fa6ab78.json']).toBeDefined()
    expect(storedJson(store).files['head_6fa6ab78_fp16.bin']).toBeDefined()
    expect((await engine.loadHead('small')).fusion).toBeNull()

    // Removing 'full' when 'small' is absent cleans up BOTH fusion files and the retained legacy head!
    await engine.removeModel('full')
    expect(Object.keys(storedJson(store).files)).toEqual([])
    expect(partKeys(store)).toEqual([])
  })

  it('preserves Fast (student + head_6fa6ab78) on fusion upgrade and when removeModel("full") removes Slot 1 files', async () => {
    const store = memoryCaches()
    const fusionList = makeList(true)
    mockFetch(fusionList)

    const engine = createEngine(BASE, () => [], { signedIn: () => false })
    await engine.download('small', fusionList, () => {})
    await engine.download('full', fusionList, () => {})
    expect(await engine.ready(fusionList)).toEqual({ full: true, small: true })
    // Both legacy head (for Fast) and fusion head (for More accurate) are stored together
    expect(storedJson(store).files['head_6fa6ab78_fp16.bin']).toBeDefined()
    expect(storedJson(store).files['head_slot1_7d54b54f_fp16.bin']).toBeDefined()

    // Removing 'full' deletes encoder + all 4 fusion files while leaving Fast 100% intact!
    await engine.removeModel('full')
    const left = storedJson(store)
    expect(Object.keys(left.files).sort()).toEqual([
      'head_6fa6ab78.json',
      'head_6fa6ab78_fp16.bin',
      'student_W2_int8.onnx',
      'wing_seg_s2.onnx',
    ])
    expect(await engine.ready(fusionList)).toEqual({ full: false, small: true })
    expect(partKeys(store).some(k => k.includes('w1c') || k.includes('w1d') || k.includes('head_slot1'))).toBe(false)
  })

  it('runs detector -> BioCLIP -> w1c (224) -> w1d (208) and returns feature (bio) + fusionFeatures, with fusion head loaded', async () => {
    memoryCaches()
    const fusionList = makeList(true)
    mockFetch(fusionList)

    // Stub Image + Canvas for decode()
    const W = 16, H = 16
    const rgba = new Uint8ClampedArray(W * H * 4).fill(128)
    vi.stubGlobal('URL', Object.assign(URL, { createObjectURL: () => 'blob:mock', revokeObjectURL: () => {} }))
    vi.stubGlobal('Image', class { naturalWidth = W; naturalHeight = H; decoding = 'async'; src = ''; async decode() {} })
    vi.stubGlobal('document', {
      createElement: () => ({ width: 0, height: 0, getContext: () => ({ drawImage: () => {}, getImageData: () => ({ width: W, height: H, data: rgba }) }) }),
    })

    const calls: { bytes0: number; dims: number[] }[] = []
    const makeSess = (bytes0: number) => ({
      inputNames: ['pixel_values'],
      outputNames: bytes0 === 1 ? ['output0', 'output1'] : ['out'],
      release: async () => {},
      run: async (feeds: Record<string, { dims: number[] }>) => {
        const dims = feeds.pixel_values.dims
        calls.push({ bytes0, dims })
        if (bytes0 === 1) {
          return {
            output0: { data: new Float32Array(38), dims: [1, 1, 38], dispose: () => {} },
            output1: { data: new Float32Array(32 * 160 * 160), dims: [1, 32, 160, 160], dispose: () => {} },
          }
        }
        return { out: { data: new Float32Array([bytes0, bytes0 + 1]), dims: [1, 2], dispose: () => {} } }
      },
    })
    const runtime = async () => ({
      env: { wasm: {}, logLevel: 'error' },
      InferenceSession: { create: async (b: Uint8Array) => makeSess(b[0]) },
      Tensor: class { constructor(public type: string, public data: Float32Array, public dims: number[]) {} },
    }) as never

    const engine = createEngine(BASE, () => [], { signedIn: () => false, runtime })
    await engine.download('full', fusionList, () => {})
    const out = await engine.run({ blob: new Blob([new Uint8Array([0x89, 0x50, 0x4e, 0x47])]), bytes: new Uint8Array([0x89, 0x50, 0x4e, 0x47]).buffer, name: 'x.png' } as never, 'full')

    expect(engine.loaded('full')).toBe(true)
    expect(engine.head('full')?.fusion).not.toBeNull()
    expect(calls).toEqual([
      { bytes0: 1, dims: [1, 3, 640, 640] },
      { bytes0: 10, dims: [1, 3, 224, 224] },
      { bytes0: 30, dims: [1, 3, 224, 224] },
      { bytes0: 40, dims: [1, 3, 208, 208] },
    ])
    expect(Array.from(out.feature)).toEqual([10, 11])
    expect(out.fusionFeatures).toBeDefined()
    expect(Array.from(out.fusionFeatures!.bio)).toEqual([10, 11])
    expect(Array.from(out.fusionFeatures!.w1c)).toEqual([30, 31])
    expect(Array.from(out.fusionFeatures!.w1d)).toEqual([40, 41])
  })
})
