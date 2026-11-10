import { createHash } from 'node:crypto'
import { readFileSync } from 'node:fs'
import { resolve } from 'node:path'
import { describe, expect, it } from 'vitest'
import { letterboxCrop, roleOf, rolesReady } from '../offlineId'
import { SEX_CUT, SEX_FILE, SEX_GENERA, sexApplies, sexCall, sexInput, sigmoid } from '../sexModel'
import { ITHOMIINI_GENERA } from '../ithomiiniGenera'

// The sex estimate's preprocessing and decision (lib/sexModel.ts; PLAN.md Amendment 8 in WingsClassificator).

/** A deterministic RGBA test image (the same formula as the Pillow fixture below). */
function pattern(W: number, H: number) {
  const data = new Uint8Array(W * H * 4)
  for (let y = 0; y < H; y++)
    for (let x = 0; x < W; x++) {
      for (let c = 0; c < 3; c++) data[(y * W + x) * 4 + c] = (x * 7 + y * 13 + c * 50 + ((x * y) % 11)) % 256
      data[(y * W + x) * 4 + 3] = 255
    }
  return { width: W, height: H, data }
}
const sha = (b: Uint8Array) => createHash('sha256').update(b).digest('hex')

describe('sex model preprocessing', () => {
  // Pillow 12 (the training pipeline's feats_v2.square_crop, then resize((224, 224), BICUBIC)) on the same pixels:
  // the app's square must be byte-identical (checked on 12 real team photos too: 0 px difference).
  it.each([
    [37, 23, [3, 2, 31, 21], '5e8f530aef2486e034c42d2b9dad9afc75905f4463b846c748d1297fcc6473c1', [51, 64, 114]],
    [400, 300, [50, 40, 390, 170], '41af9a218a507eb01c89371fd5ad33daee5b3689a161ad9d3d5f53782f6ce042', [97, 147, 197]],
  ] as const)('the 224 px square of a %ix%i photo is Pillow’s, byte for byte', (W, H, box, hash, centre) => {
    const sq = letterboxCrop(pattern(W, H), [...box] as [number, number, number, number])
    expect(sha(sq)).toBe(hash)
    expect(Array.from(sq.slice((112 * 224 + 112) * 3, (112 * 224 + 112) * 3 + 3))).toEqual([...centre])
    expect(Array.from(sq.slice(0, 3))).toEqual([128, 128, 128]) // the grey pad
  })

  it('feeds raw 0 to 255 values, channels first (the model normalises itself)', () => {
    const rgb = new Uint8Array(224 * 224 * 3)
    rgb.set([255, 10, 0], 0)
    rgb.set([1, 2, 3], (224 * 224 - 1) * 3)
    const x = sexInput(rgb)
    expect(x.length).toBe(3 * 224 * 224)
    const plane = 224 * 224
    expect([x[0], x[plane], x[2 * plane]]).toEqual([255, 10, 0])
    expect([x[plane - 1], x[2 * plane - 1], x[3 * plane - 1]]).toEqual([1, 2, 3])
    expect(x.reduce((a, b) => Math.max(a, b), 0)).toBe(255)
  })
})

describe('sex call: cutoff and «no seguro» band', () => {
  it('uses the Amendment 8 numbers: band around the cutoff, all far above P = 0.5', () => {
    expect(SEX_CUT).toEqual({ t: 2.9805, lo: 1.6157, hi: 4.8722 })
    expect(SEX_CUT.lo).toBeLessThan(SEX_CUT.t)
    expect(SEX_CUT.t).toBeLessThan(SEX_CUT.hi)
    expect(sigmoid(SEX_CUT.t)).toBeCloseTo(0.952, 3)
    expect(sigmoid(SEX_CUT.lo)).toBeCloseTo(0.834, 3)
    expect(sigmoid(SEX_CUT.hi)).toBeCloseTo(0.992, 3)
  })

  it('female at or above hi, male below lo, not sure in between (lo included, hi excluded)', () => {
    expect(sexCall(SEX_CUT.hi)).toBe('hembra')
    expect(sexCall(9)).toBe('hembra')
    expect(sexCall(SEX_CUT.hi - 1e-6)).toBe('no seguro')
    expect(sexCall(SEX_CUT.t)).toBe('no seguro')
    expect(sexCall(SEX_CUT.lo)).toBe('no seguro')
    expect(sexCall(SEX_CUT.lo - 1e-6)).toBe('macho')
    // P(female) 0.80 from the model is a male call on in-hand photos.
    expect(sexCall(Math.log(0.8 / 0.2))).toBe('macho')
    expect(sexCall(0)).toBe('macho')
  })

})

describe('where the estimate applies', () => {
  it('only the 13 genera of the training composites; other Ithomiini «no aplica»; the rest nothing', () => {
    expect(SEX_GENERA).toHaveLength(13)
    for (const g of SEX_GENERA) expect(ITHOMIINI_GENERA).toContain(g)
    expect(sexApplies('Mechanitis messenoides', ITHOMIINI_GENERA)).toBe('yes')
    expect(sexApplies('Hypothyris', ITHOMIINI_GENERA)).toBe('yes')
    expect(sexApplies('Pseudoscada florula', ITHOMIINI_GENERA)).toBe('genus')
    expect(sexApplies('Heliconius erato', ITHOMIINI_GENERA)).toBe('no')
    expect(sexApplies(undefined, ITHOMIINI_GENERA)).toBe('no')
    expect(sexApplies('', ITHOMIINI_GENERA)).toBe('no')
  })
})

describe('the sex file', () => {
  it('is pinned: the name and sha256 the server lists, the bytes in 32 MB parts', () => {
    expect(SEX_FILE.name).toBe('sex_v2s_ft_full_fp16.onnx')
    expect(roleOf(SEX_FILE.name)).toBe('sex')
    expect(SEX_FILE.parts).toHaveLength(Math.ceil(SEX_FILE.bytes / (32 * 1024 * 1024)))
    for (const h of [SEX_FILE.sha256, ...SEX_FILE.parts]) expect(h).toMatch(/^[0-9a-f]{64}$/)
    // The phone downloads the file the server lists: the server's pin (server/offline-id.mjs SEX) must be this one.
    const server = readFileSync(resolve(import.meta.dirname, '../../../../server/offline-id.mjs'), 'utf8')
    const pin = /export const SEX = \{ name: '([^']+)', sha256: '([0-9a-f]{64})' \}/.exec(server)
    expect(pin, 'server/offline-id.mjs SEX').not.toBeNull()
    expect([pin![1], pin![2]]).toEqual([SEX_FILE.name, SEX_FILE.sha256])
  })

  it('is ready with the detector and the very file listed (by ETag or by the pinned parts)', () => {
    const det = { name: 'wing_seg_s2.onnx', etag: '"d"', bytes: 10, role: 'detector' }
    const sex = { name: SEX_FILE.name, etag: '"s"', bytes: SEX_FILE.bytes, role: 'sex', parts: SEX_FILE.parts }
    const stored = { [det.name]: { etag: '"d"', bytes: 10, role: 'detector' }, [sex.name]: { etag: '"old"', bytes: SEX_FILE.bytes, role: 'sex', parts: SEX_FILE.parts } }
    expect(rolesReady(['detector', 'sex'], stored, [det, sex])).toBe(true)
    expect(rolesReady(['detector', 'sex'], stored, [det])).toBe(false) // not listed (another server, or signed out)
    expect(rolesReady(['detector', 'sex'], { [det.name]: stored[det.name] }, [det, sex])).toBe(false)
    expect(rolesReady(['detector', 'sex'], stored, null)).toBe(true) // offline: what is stored
  })
})
