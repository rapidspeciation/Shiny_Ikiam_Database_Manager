import { afterEach, describe, expect, it, vi } from 'vitest'
import { applyP3Roi, readDetectorMasks, readMasks, type DetectorInput, type Mask } from '../offlineId'

// End-to-end wiring & Stride-2 (P=320) / Stride-4 (P=160) tests for offlineEngine.ts cut()/square() and
// offlineId.ts readMasks()/readDetectorMasks(): proves P3 speck stripping, P1-A 0.70-IoU shifted-duplicate
// suppression, hoisted box-bounded vs full-map sum bit-identity on random prototypes, layout/shape
// validation (including N=0 and non-square/mismatched channel layouts), and output tensor disposal on throw.
// Zero real models, zero AI calls.

vi.resetModules()
const { createEngine } = await import('../offlineEngine')

const BASE = 'https://x.test/prueba-offline/'

afterEach(() => {
  vi.unstubAllGlobals()
  vi.restoreAllMocks()
})

function makeSyntheticFixture(P: number, overrideOutputs?: { detDims?: number[]; protoDims?: number[]; detData?: Float32Array; protoData?: Float32Array }) {
  const W = 640, H = 640
  const scale = P / 160
  // Paint [100..260, 100..260] red (main wing), [260..320, 100..260] green (shifted duplicate overhang),
  // and [500..600, 500..600] blue (distant speck region).
  const rgba = new Uint8ClampedArray(W * H * 4)
  for (let y = 0; y < H; y++) {
    for (let x = 0; x < W; x++) {
      const off = (y * W + x) * 4
      if (x >= 100 && x < 260 && y >= 100 && y < 260) {
        rgba[off] = 255; rgba[off + 1] = 0; rgba[off + 2] = 0
      } else if (x >= 260 && x < 320 && y >= 100 && y < 260) {
        rgba[off] = 0; rgba[off + 1] = 255; rgba[off + 2] = 0
      } else if (x >= 500 && x < 600 && y >= 500 && y < 600) {
        rgba[off] = 0; rgba[off + 1] = 0; rgba[off + 2] = 255
      }
      rgba[off + 3] = 255
    }
  }

  // Prototypes at stride 640 / P (160 -> stride 4, 320 -> stride 2):
  // ch 0 (conf 0.90): main red wing [100..260, 100..260] + distant blue speck [520..536, 520..536]
  // ch 1 (conf 0.70): shifted duplicate [128..288, 100..260] -> IoU = 0.7021 >= 0.65, containment = 0.825 < 0.95
  const proto = overrideOutputs?.protoData ?? new Float32Array(32 * P * P).fill(-4)
  if (!overrideOutputs?.protoData) {
    const fill = (ch: number, x0: number, y0: number, x1: number, y1: number) => {
      const base = ch * P * P
      for (let r = y0 * scale; r < y1 * scale; r++)
        for (let c = x0 * scale; c < x1 * scale; c++) proto[base + r * P + c] = 4
    }
    fill(0, 25, 25, 65, 65)
    fill(0, 130, 130, 134, 134)
    fill(1, 32, 25, 72, 65)
  }

  const det = overrideOutputs?.detData ?? new Float32Array(2 * 38)
  if (!overrideOutputs?.detData) {
    det[0] = 0; det[1] = 0; det[2] = 640; det[3] = 640; det[4] = 0.90; det[6 + 0] = 1
    det[38 + 0] = 0; det[38 + 1] = 0; det[38 + 2] = 640; det[38 + 3] = 640; det[38 + 4] = 0.70; det[38 + 6 + 1] = 1
  }

  const store = new Map<string, { body: ArrayBuffer; headers: Headers }>()
  const storedDoc = { at: '2026-11-26T00:00:00Z', files: { 'wing_seg_s2.onnx': { etag: '"d"', bytes: 4, role: 'detector' } } }
  store.set(`${BASE}models/stored.json`, { body: new TextEncoder().encode(JSON.stringify(storedDoc)).buffer, headers: new Headers() })
  store.set(`${BASE}models/wing_seg_s2.onnx?part=0`, { body: new Uint8Array([1, 2, 3, 4]).buffer, headers: new Headers({ 'x-bytes': '4' }) })
  const cache = {
    match: async (k: string) => (store.has(k) ? new Response(store.get(k)!.body, { headers: store.get(k)!.headers }) : undefined),
    put: async () => {},
    delete: async () => true,
    keys: async () => [],
  }

  vi.stubGlobal('self', { crossOriginIsolated: false })
  vi.stubGlobal('caches', { open: async () => cache, delete: async () => true })
  vi.stubGlobal('navigator', { onLine: false })
  vi.stubGlobal('URL', Object.assign(URL, { createObjectURL: () => 'blob:mock', revokeObjectURL: () => {} }))
  vi.stubGlobal('Image', class {
    naturalWidth = W
    naturalHeight = H
    decoding = 'async'
    src = ''
    async decode() {}
  })
  vi.stubGlobal('document', {
    createElement: () => ({
      width: 0,
      height: 0,
      getContext: () => ({
        drawImage: () => {},
        getImageData: () => ({ width: W, height: H, data: rgba }),
      }),
    }),
  })

  const detDispose = vi.fn()
  const protoDispose = vi.fn()
  const session = {
    inputNames: ['images'],
    outputNames: ['output0', 'output1'],
    release: async () => {},
    run: async () => ({
      output0: { data: det, dims: overrideOutputs?.detDims ?? [1, 2, 38], dispose: detDispose },
      output1: { data: proto, dims: overrideOutputs?.protoDims ?? [1, 32, P, P], dispose: protoDispose },
    }),
  }
  const runtime = async () => ({
    env: { wasm: {}, logLevel: 'error' },
    InferenceSession: { create: async () => session },
    Tensor: class {
      constructor(public type: string, public data: Float32Array, public dims: number[]) {}
    },
  }) as never

  return { W, H, proto, det, runtime, detDispose, protoDispose }
}

/** Naive full-prototype-map reference implementation of readMasks to assert bit-identity of box-bounded hoisting. */
function referenceFullSumReadMasks(
  det: Float32Array,
  N: number,
  D: number,
  proto: Float32Array,
  P: number,
  inp: DetectorInput,
  minConf = 0.25,
  S = 640,
): Mask[] {
  const out: Mask[] = []
  const n = P * P, k = P / S, nm = D - 6
  const { gain, padX, padY, rawW, rawH } = inp
  for (let i = 0; i < N; i++) {
    const score = det[i * D + 4]
    if (!(score >= minConf)) continue
    const x1 = det[i * D], y1 = det[i * D + 1], x2 = det[i * D + 2], y2 = det[i * D + 3]
    const pm = new Float32Array(n)
    for (let c = 0; c < nm; c++) {
      const w = det[i * D + 6 + c], off = c * n
      if (w === 0) continue
      for (let j = 0; j < n; j++) pm[j] += w * proto[off + j]
    }
    const c0 = Math.max(0, Math.ceil(x1)), c1 = Math.min(S - 1, Math.ceil(x2) - 1)
    const r0 = Math.max(0, Math.ceil(y1)), r1 = Math.min(S - 1, Math.ceil(y2) - 1)
    if (!(c1 >= c0 && r1 >= r0)) continue
    const W = c1 - c0 + 1, H = r1 - r0 + 1
    let m: Uint8Array = new Uint8Array(W * H)
    const gx0 = new Int32Array(W), gx1 = new Int32Array(W), gwx = new Float32Array(W)
    for (let c = 0; c < W; c++) {
      let g = (c0 + c + 0.5) * k - 0.5
      if (g < 0) g = 0
      const a = Math.min(Math.floor(g), P - 1)
      gx0[c] = a; gx1[c] = Math.min(a + 1, P - 1); gwx[c] = g - a
    }
    let count = 0
    for (let r = 0; r < H; r++) {
      let g = (r0 + r + 0.5) * k - 0.5
      if (g < 0) g = 0
      const a = Math.min(Math.floor(g), P - 1), b = Math.min(a + 1, P - 1), wy = g - a
      for (let c = 0; c < W; c++) {
        const wx = gwx[c]
        const v = (pm[a * P + gx0[c]] * (1 - wx) + pm[a * P + gx1[c]] * wx) * (1 - wy) + (pm[b * P + gx0[c]] * (1 - wx) + pm[b * P + gx1[c]] * wx) * wy
        if (v > 0) { m[r * W + c] = 1; count++ }
      }
    }
    if (count < 3) continue
    m = applyP3Roi(m, W, H)
    count = 0
    for (let j = 0; j < m.length; j++) count += m[j]
    if (count < 3) continue
    let mx0 = W, my0 = H, mx1 = -1, my1 = -1, edge = 0
    for (let r = 0; r < H; r++)
      for (let c = 0; c < W; c++) {
        if (!m[r * W + c]) continue
        if (c < mx0) mx0 = c
        if (c > mx1) mx1 = c
        if (r < my0) my0 = r
        if (r > my1) my1 = r
        if (c === 0 || r === 0 || c === W - 1 || r === H - 1 || !m[r * W + c - 1] || !m[r * W + c + 1] || !m[(r - 1) * W + c] || !m[(r + 1) * W + c]) edge++
      }
    const nx = (px: number) => Math.min(Math.max((px - padX) / gain, 0), rawW) / rawW
    const ny = (py: number) => Math.min(Math.max((py - padY) / gain, 0), rawH) / rawH
    const bbox: [number, number, number, number] = [nx(c0 + mx0), ny(r0 + my0), nx(c0 + mx1), ny(r0 + my1)]
    const area = Math.max(0, count - edge / 2 - 1) / (gain * gain * rawW * rawH)
    const boxArea = (bbox[2] - bbox[0]) * (bbox[3] - bbox[1])
    const RASTER = 64
    const raster = new Uint8Array(RASTER * RASTER)
    for (let row = 0; row < RASTER; row++) {
      const py = Math.round(((row + 0.5) / RASTER) * rawH * gain + padY) - r0
      if (py < 0 || py >= H) continue
      for (let col = 0; col < RASTER; col++) {
        const px = Math.round(((col + 0.5) / RASTER) * rawW * gain + padX) - c0
        if (px >= 0 && px < W && m[py * W + px]) raster[row * RASTER + col] = 1
      }
    }
    const rw = mx1 - mx0 + 1, rh = my1 - my0 + 1
    const tight = new Uint8Array(rw * rh)
    for (let r = 0; r < rh; r++) tight.set(m.subarray((my0 + r) * W + mx0, (my0 + r) * W + mx0 + rw), r * rw)
    out.push({
      uid: `p${String(i).padStart(3, '0')}`,
      confidence: score,
      bbox,
      raster,
      fill: boxArea ? area / boxArea : 0,
      clamp: 0,
      roi: { x0: c0 + mx0, y0: r0 + my0, w: rw, h: rh, data: tight, area: count },
    })
  }
  return out
}

function mulberry32(seed: number) {
  let a = seed >>> 0
  return () => {
    a = (a + 0x6d2b79f5) >>> 0
    let t = Math.imul(a ^ (a >>> 15), 1 | a)
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296
  }
}

describe('offlineEngine P3 + P1-A wiring (Stride-4 P=160 and Stride-2 P=320)', () => {
  for (const P of [160, 320]) {
    it(`strips distant specks (P3) and suppresses 0.70-IoU shifted duplicates (P1-A) inside square() at P=${P}`, async () => {
      const { runtime, detDispose, protoDispose } = makeSyntheticFixture(P)
      const engine = createEngine(BASE, () => [], { signedIn: () => false, runtime })
      const { square, status } = await engine.square({
        blob: new Blob([new Uint8Array([0x89, 0x50, 0x4e, 0x47])]),
        bytes: new Uint8Array([0x89, 0x50, 0x4e, 0x47]).buffer,
        name: 'mock.png',
      } as never)

      expect(status).toBe('success')
      expect(detDispose).toHaveBeenCalledTimes(1)
      expect(protoDispose).toHaveBeenCalledTimes(1)
      expect(square.length).toBe(224 * 224 * 3)
      let redPixels = 0, greenOrBlue = 0
      for (let i = 0; i < square.length; i += 3) {
        if (square[i] > 200 && square[i + 1] < 20 && square[i + 2] < 20) redPixels++
        if (square[i + 1] > 40 || square[i + 2] > 40) greenOrBlue++
      }
      expect(redPixels).toBeGreaterThan(224 * 224 * 0.95)
      expect(greenOrBlue).toBe(0)
    })
  }

  for (const P of [160, 320]) {
    it(`produces bit-identical masks between box-bounded hoisted sum and full-map sum on random prototypes at P=${P}`, () => {
      const rng = mulberry32(P === 160 ? 20261126 : 20261127)
      const nm = 32, D = 6 + nm, N = 6
      const proto = new Float32Array(nm * P * P)
      for (let i = 0; i < proto.length; i++) proto[i] = (rng() - 0.45) * 2

      const boxes: [number, number, number, number, number][] = [
        [0, 0, 640, 640, 0.95],
        [0.1, 0.2, 47.9, 63.4, 0.88],
        [580.3, 590.1, 639.8, 639.9, 0.82],
        [113.7, 205.2, 421.4, 512.8, 0.77],
        [310.0, 12.5, 326.0, 628.0, 0.61],
        [200.0, 200.0, 300.0, 300.0, 0.10], // below minConf
      ]
      const det = new Float32Array(N * D)
      for (let i = 0; i < N; i++) {
        const [x1, y1, x2, y2, sc] = boxes[i]
        det[i * D + 0] = x1
        det[i * D + 1] = y1
        det[i * D + 2] = x2
        det[i * D + 3] = y2
        det[i * D + 4] = sc
        for (let c = 0; c < nm; c++) {
          det[i * D + 6 + c] = c % 5 === 0 ? 0 : (rng() - 0.5) * 3
        }
      }
      const inp: DetectorInput = { x: new Float32Array(0), gain: 0.5, padX: 0, padY: 80, rawW: 1280, rawH: 960 }

      const got = readMasks(det, N, D, proto, P, inp, 0.25, 640)
      const ref = referenceFullSumReadMasks(det, N, D, proto, P, inp, 0.25, 640)
      expect(got.length).toBe(ref.length)
      expect(got.length).toBeGreaterThanOrEqual(4)
      for (let i = 0; i < got.length; i++) {
        expect(got[i].uid).toBe(ref[i].uid)
        expect(got[i].confidence).toBe(ref[i].confidence)
        expect(got[i].bbox).toEqual(ref[i].bbox)
        expect(got[i].fill).toBe(ref[i].fill)
        expect(Array.from(got[i].raster)).toEqual(Array.from(ref[i].raster))
        expect(got[i].roi?.x0).toBe(ref[i].roi?.x0)
        expect(got[i].roi?.y0).toBe(ref[i].roi?.y0)
        expect(got[i].roi?.w).toBe(ref[i].roi?.w)
        expect(got[i].roi?.h).toBe(ref[i].roi?.h)
        expect(got[i].roi?.area).toBe(ref[i].roi?.area)
        expect(Array.from(got[i].roi!.data)).toEqual(Array.from(ref[i].roi!.data))
      }
    })
  }

  it('allows N=0 and rejects incompatible tensor layouts, non-exact packed lengths, and non-integer dims', () => {
    const inp: DetectorInput = { x: new Float32Array(0), gain: 1, padX: 0, padY: 0, rawW: 640, rawH: 640 }
    const validProto = new Float32Array(32 * 160 * 160)
    expect(readMasks(new Float32Array(0), 0, 38, validProto, 160, inp)).toEqual([])
    expect(readDetectorMasks({ data: new Float32Array(0), dims: [1, 0, 38] }, { data: validProto, dims: [1, 32, 160, 160] }, inp)).toEqual([])

    // Exact length & integer invariants in readMasks
    expect(() => readMasks(new Float32Array(38), 1, 6, new Float32Array(0), 160, inp)).toThrow('detector-shape')
    expect(() => readMasks(new Float32Array(38), 1, 38, new Float32Array(32 * 160 * 160 - 1), 160, inp)).toThrow('detector-shape')
    expect(() => readMasks(new Float32Array(39), 1, 38, validProto, 160, inp)).toThrow('detector-shape')
    expect(() => readMasks(new Float32Array(38), 1, 38.5, validProto, 160, inp)).toThrow('detector-shape')
    expect(() => readMasks(new Float32Array(38), 1, 38, validProto, 160, inp, 0.25, 640.5)).toThrow('detector-shape')
    expect(() => readMasks(new Float32Array(38), 1, 38, new Float32Array(0), 0, inp)).toThrow('detector-shape')

    // Incompatible layout [1, 64, 160, 80] has identical element count (32 * 160 * 160) but wrong channels/shape
    expect(() =>
      readDetectorMasks(
        { data: new Float32Array(38), dims: [1, 1, 38] },
        { data: validProto, dims: [1, 64, 160, 80] },
        inp,
      ),
    ).toThrow('detector-shape')
    expect(() =>
      readDetectorMasks(
        { data: new Float32Array(38), dims: [2, 1, 38] },
        { data: validProto, dims: [1, 32, 160, 160] },
        inp,
      ),
    ).toThrow('detector-shape')
  })

  it('disposes both detector output tensors even when readDetectorMasks throws detector-shape', async () => {
    const { runtime, detDispose, protoDispose } = makeSyntheticFixture(160, {
      detDims: [1, 1, 38],
      detData: new Float32Array(38),
      protoDims: [1, 64, 160, 80],
      protoData: new Float32Array(32 * 160 * 160),
    })
    const engine = createEngine(BASE, () => [], { signedIn: () => false, runtime })
    await expect(
      engine.square({
        blob: new Blob([new Uint8Array([0x89, 0x50, 0x4e, 0x47])]),
        bytes: new Uint8Array([0x89, 0x50, 0x4e, 0x47]).buffer,
        name: 'mock.png',
      } as never),
    ).rejects.toThrow('detector-shape')
    expect(detDispose).toHaveBeenCalledTimes(1)
    expect(protoDispose).toHaveBeenCalledTimes(1)
  })
})
