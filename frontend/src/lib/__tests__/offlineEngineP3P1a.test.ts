import { afterEach, describe, expect, it, vi } from 'vitest'

// End-to-end wiring test for offlineEngine.ts cut()/square(): proves the live engine executes both
// P3 (stripping distant specks without bridge-chord expansion) and P1-A (greedy IoU >= 0.65 NMS
// before selectMasks, suppressing a 0.70-IoU shifted duplicate that selectMasks's 0.88/0.95 rules
// would otherwise keep). Zero real models, zero AI calls.

vi.resetModules()
const { createEngine } = await import('../offlineEngine')

const BASE = 'https://x.test/prueba-offline/'

afterEach(() => {
  vi.unstubAllGlobals()
  vi.restoreAllMocks()
})

describe('offlineEngine P3 + P1-A wiring', () => {
  it('strips distant specks (P3) and suppresses 0.70-IoU shifted duplicates (P1-A) inside square()', async () => {
    const W = 640, H = 640
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

    // 160x160 prototypes (stride 4 on 640x640):
    // ch 0 (conf 0.90): main red wing [25..65, 25..65] -> [100..260, 100..260] + distant blue speck [130..134, 130..134]
    // ch 1 (conf 0.70): shifted duplicate [32..72, 25..65] -> IoU = 1320/1880 = 0.7021 >= 0.65, containment = 0.825 < 0.95
    //                   (kept by selectMasks alone, suppressed ONLY when applyP1aNms is wired!)
    const P = 160
    const proto = new Float32Array(32 * P * P).fill(-4)
    const fill = (ch: number, x0: number, y0: number, x1: number, y1: number) => {
      const base = ch * P * P
      for (let r = y0; r < y1; r++) for (let c = x0; c < x1; c++) proto[base + r * P + c] = 4
    }
    fill(0, 25, 25, 65, 65)
    fill(0, 130, 130, 134, 134)
    fill(1, 32, 25, 72, 65)

    const det = new Float32Array(2 * 38)
    det[0] = 0; det[1] = 0; det[2] = 640; det[3] = 640; det[4] = 0.90; det[6 + 0] = 1
    det[38 + 0] = 0; det[38 + 1] = 0; det[38 + 2] = 640; det[38 + 3] = 640; det[38 + 4] = 0.70; det[38 + 6 + 1] = 1

    const store = new Map<string, { body: ArrayBuffer; headers: Headers }>()
    const storedDoc = { at: '2026-11-25T00:00:00Z', files: { 'wing_seg_s2.onnx': { etag: '"d"', bytes: 4, role: 'detector' } } }
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

    const session = {
      inputNames: ['images'],
      outputNames: ['output0', 'output1'],
      release: async () => {},
      run: async () => ({
        output0: { data: det, dims: [1, 2, 38] },
        output1: { data: proto, dims: [1, 32, P, P] },
      }),
    }
    const runtime = async () => ({
      env: { wasm: {}, logLevel: 'error' },
      InferenceSession: { create: async () => session },
      Tensor: class {
        constructor(public type: string, public data: Float32Array, public dims: number[]) {}
      },
    }) as never

    const engine = createEngine(BASE, () => [], { signedIn: () => false, runtime })
    const { square, status } = await engine.square({
      blob: new Blob([new Uint8Array([0x89, 0x50, 0x4e, 0x47])]),
      bytes: new Uint8Array([0x89, 0x50, 0x4e, 0x47]).buffer,
      name: 'mock.png',
    } as never)

    expect(status).toBe('success')
    expect(square.length).toBe(224 * 224 * 3)
    // Every pixel in the cropped 224x224 square must come exclusively from the red main wing [100..260, 100..260]:
    // zero green overhang from the 0.70-IoU duplicate and zero blue from the distant speck.
    let redPixels = 0, greenOrBlue = 0
    for (let i = 0; i < square.length; i += 3) {
      if (square[i] > 200 && square[i + 1] < 20 && square[i + 2] < 20) redPixels++
      if (square[i + 1] > 40 || square[i + 2] > 40) greenOrBlue++
    }
    expect(redPixels).toBeGreaterThan(224 * 224 * 0.95)
    expect(greenOrBlue).toBe(0)
  })
})
