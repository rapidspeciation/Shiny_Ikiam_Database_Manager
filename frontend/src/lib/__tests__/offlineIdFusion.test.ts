import { describe, expect, it } from 'vitest'
import {
  applyGeoPrior,
  dinoInput,
  dinoW1cInput,
  dinoW1dInput,
  LEAF_MERGES_6FA6AB78,
  leafScores,
  leafScoreStages,
  makeHead,
  resizeBilinear,
  speciesTop,
  subspeciesBySpecies,
  subspeciesTop,
  sumSpecies,
  type HeadMeta,
} from '../offlineId'
import { forHead } from '../taxonomy'

/** Encode a finite float32 number as IEEE-754 float16 (round-to-nearest-even). */
function f32ToF16(val: number): number {
  const f32 = new Float32Array([val])
  const u32 = new Uint32Array(f32.buffer)[0]
  const sign = (u32 >>> 16) & 0x8000
  const exp = ((u32 >>> 23) & 0xff) - 127 + 15
  const mant = u32 & 0x7fffff
  if (val === 0) return sign
  if (exp <= 0) return sign
  if (exp >= 31) return sign | 0x7c00
  return sign | (exp << 10) | ((mant + 0x1000) >>> 13)
}

function packFp16Matrices(mats: number[][][]): { buffer: ArrayBuffer; offsets: [number, number, number][] } {
  const chunks: Uint8Array[] = []
  const offsets: [number, number, number][] = []
  let byteLen = 0
  for (const m of mats) {
    while (byteLen % 4 !== 0) {
      chunks.push(new Uint8Array([0]))
      byteLen += 1
    }
    const r = m.length, c = m[0].length
    offsets.push([byteLen, r, c])
    const u16 = new Uint16Array(r * c)
    for (let i = 0; i < r; i++) for (let j = 0; j < c; j++) u16[i * c + j] = f32ToF16(m[i][j])
    const bytes = new Uint8Array(u16.buffer)
    chunks.push(bytes)
    byteLen += bytes.byteLength
  }
  const out = new Uint8Array(byteLen)
  let off = 0
  for (const ch of chunks) { out.set(ch, off); off += ch.byteLength }
  return { buffer: out.buffer, offsets }
}

/** Independent convergent Maclaurin series reference for erf(x) (|x| <= 5; exact to double precision). */
function erfSeriesRef(x: number): number {
  let term = x
  let sum = x
  const x2 = x * x
  for (let n = 1; n <= 60; n++) {
    term *= -x2 / n
    sum += term / (2 * n + 1)
  }
  return (2 / Math.sqrt(Math.PI)) * sum
}

function refBranchLogits(
  feat: number[],
  dw: number[][],
  db: number[],
  uw: number[][],
  ub: number[],
  lnw: number[],
  lnb: number[],
  W: number[][],
  rs: number,
  s: number,
  eps: number,
  l2NormIn: boolean,
): number[] {
  let x = [...feat]
  if (l2NormIn) {
    const n = Math.hypot(...x) || 1
    x = x.map(v => v / n)
  }
  const hid = dw.map((row, i) => {
    const v = db[i] + row.reduce((a, w, j) => a + w * x[j], 0)
    return 0.5 * v * (1 + erfSeriesRef(v / Math.SQRT2))
  })
  const h = uw.map((row, i) => x[i] + rs * (ub[i] + row.reduce((a, w, j) => a + w * hid[j], 0)))
  const mean = h.reduce((a, b) => a + b, 0) / h.length
  const var_ = h.reduce((a, b) => a + (b - mean) ** 2, 0) / h.length
  const inv = 1 / Math.sqrt(var_ + eps)
  let z = h.map((v, i) => (v - mean) * inv * lnw[i] + lnb[i])
  const zn = Math.hypot(...z) || 1
  z = z.map(v => v / zn)
  return W.map(row => Math.min(Math.max(row.reduce((a, w, i) => a + w * z[i], 0), -1 + 1e-6), 1 - 1e-6) * s)
}

function refSoftmax(z: number[]): number[] {
  const m = Math.max(...z)
  const ex = z.map(v => Math.exp(v - m))
  const sum = ex.reduce((a, b) => a + b, 0)
  return ex.map(v => v / sum)
}

describe('Slot 1 offline fusion preprocessing and head math', () => {
  it('dinoW1cInput (224) and dinoW1dInput (208 bilinear) match ImageNet mean/std and align_corners=False', () => {
    const rgb = new Uint8Array(224 * 224 * 3)
    for (let i = 0; i < rgb.length; i++) rgb[i] = (i * 53 + 17) % 256
    const x224 = dinoW1cInput(rgb)
    expect(x224.length).toBe(3 * 224 * 224)
    expect(x224[0]).toBeCloseTo((rgb[0] / 255 - 0.485) / 0.229, 5)
    expect(x224[224 * 224]).toBeCloseTo((rgb[1] / 255 - 0.456) / 0.224, 5)
    expect(x224[2 * 224 * 224]).toBeCloseTo((rgb[2] / 255 - 0.406) / 0.225, 5)

    const x208 = dinoW1dInput(rgb)
    expect(x208.length).toBe(3 * 208 * 208)
    // Independent reference: 0..1 CHW -> resizeBilinear 224->208 -> ImageNet norm
    const x01 = new Float32Array(3 * 224 * 224)
    for (let i = 0; i < 224 * 224; i++) for (let c = 0; c < 3; c++) x01[c * 224 * 224 + i] = rgb[i * 3 + c] / 255
    const r = resizeBilinear(x01, 3, 224, 224, 208, 208)
    const mean = [0.485, 0.456, 0.406], std = [0.229, 0.224, 0.225]
    for (const idx of [0, 1, 207, 208 * 100 + 55, 208 * 208 - 1]) {
      for (let c = 0; c < 3; c++) {
        expect(x208[c * 208 * 208 + idx]).toBeCloseTo((r[c * 208 * 208 + idx] - mean[c]) / std[c], 6)
      }
    }
    // Flat colour remains constant after 224 -> 208 resize
    const flat = dinoInput(new Uint8Array(224 * 224 * 3).fill(128), 208)
    expect(flat[0]).toBeCloseTo((128 / 255 - 0.485) / 0.229, 5)
    expect(flat[208 * 208 - 1]).toBeCloseTo((128 / 255 - 0.485) / 0.229, 5)
  })

  it('matches exact Slot 1 res_adapter + LayerNorm + L2 + prob blend [0.5, 0.25, 0.25] + T_disp=0.97 + Option 1A merge + geo prior', () => {
    // 4 raw leaves: 0="Alpha one" (merged into 1="Beta two"), 1="Beta two", 2="Beta two ssp", 3="Gamma three"
    const dwBio = [[0.5, -0.25, 0.125], [-0.5, 0.25, 0.5]]
    const uwBio = [[0.25, -0.5], [0.5, 0.25], [-0.25, 0.5]]
    const WBio = [[1, 0, 0], [0, 1, 0], [0, 0.5, 0.866025], [-1, 0, 0]]
    const dwC = [[0.25, 0.5], [-0.25, 0.25]]
    const uwC = [[0.5, -0.25], [0.25, 0.5]]
    const WC = [[0, 1], [1, 0], [0.7071, 0.7071], [-1, 0]]
    const dwD = [[-0.5, 0.25], [0.25, 0.5]]
    const uwD = [[0.25, 0.5], [-0.5, 0.25]]
    const WD = [[0.7071, -0.7071], [0, 1], [1, 0], [0, -1]]

    const { buffer, offsets } = packFp16Matrices([dwBio, uwBio, WBio, dwC, uwC, WC, dwD, uwD, WD])
    const dbBio = [0.1, -0.1], ubBio = [0.05, -0.05, 0.02], lnwBio = [1.1, 0.9, 1.0], lnbBio = [0.01, -0.02, 0.03]
    const dbC = [0.0, 0.2], ubC = [-0.1, 0.1], lnwC = [1.0, 1.2], lnbC = [0.0, -0.05]
    const dbD = [-0.1, 0.1], ubD = [0.05, 0.05], lnwD = [0.8, 1.1], lnbD = [0.02, 0.01]

    const meta: HeadMeta = {
      schema: 'ithomiini-offline-fusion-head/1',
      slot: 'slot1',
      head_sha256: '7d54b54f9853c9b6bd41df66138f3695190c3f47fe52880c03a031801712b6d1',
      compat_live_head_sha256: LEAF_MERGES_6FA6AB78.head,
      weights: 'head_slot1_test_fp16.bin',
      weights_bytes: buffer.byteLength,
      rows: 4,
      K: 1,
      blend_mode: 'prob',
      T_disp: 0.97,
      branches: [
        { name: 'bio', role: 'encoder', input_size: 224, norm: 'clip', l2_normalize_input: true, weight: 0.5, arch: 'res_adapter', dim: 3, bottleneck: 2, residual_scale: 0.2, s: 25, ln_eps: 1e-5, dtype: 'float16', offsets: { down_w: offsets[0], up_w: offsets[1], W: offsets[2] }, down_b: dbBio, up_b: ubBio, ln_w: lnwBio, ln_b: lnbBio },
        { name: 'w1c', role: 'dino_w1c', input_size: 224, norm: 'imagenet', l2_normalize_input: false, weight: 0.25, arch: 'res_adapter', dim: 2, bottleneck: 2, residual_scale: 0.2, s: 25, ln_eps: 1e-5, dtype: 'float16', offsets: { down_w: offsets[3], up_w: offsets[4], W: offsets[5] }, down_b: dbC, up_b: ubC, ln_w: lnwC, ln_b: lnbC },
        { name: 'w1d', role: 'dino_w1d', input_size: 208, norm: 'imagenet', l2_normalize_input: false, weight: 0.25, arch: 'res_adapter', dim: 2, bottleneck: 2, residual_scale: 0.2, s: 25, ln_eps: 1e-5, dtype: 'float16', offsets: { down_w: offsets[6], up_w: offsets[7], W: offsets[8] }, down_b: dbD, up_b: ubD, ln_w: lnwD, ln_b: lnbD },
      ],
      species: ['Alpha one', 'Beta two', 'Gamma three'],
      leaf2sp: [0, 1, 1, 2],
      leaves: ['Alpha one', 'Beta two', 'Beta two ssp', 'Gamma three'],
      is_ssp: '0010',
      geo: '777b', // leaves 0,1,2 in Ecuador East (7); leaf 3 in Ecuador West (b = 1|2|8)
      full_photo_overrides: [],
    }

    // Explicit Option 1A-style merge: "Alpha one" -> "Beta two"
    const head = makeHead(meta, buffer, { 'Alpha one': 'Beta two' })
    expect(head.fusion).not.toBeNull()
    expect(head.meta.species).toEqual(['Beta two', 'Gamma three'])
    expect(head.meta.leaves).toEqual(['Beta two', 'Beta two ssp', 'Gamma three'])

    const feats = {
      bio: new Float32Array([0.3, 0.8, 0.2]),
      w1c: new Float32Array([1.2, -0.4]),
      w1d: new Float32Array([0.4, 0.9]),
    }

    // Independent reference computation
    const zBio = refBranchLogits(Array.from(feats.bio), dwBio, dbBio, uwBio, ubBio, lnwBio, lnbBio, WBio, 0.2, 25, 1e-5, true)
    const zC = refBranchLogits(Array.from(feats.w1c), dwC, dbC, uwC, ubC, lnwC, lnbC, WC, 0.2, 25, 1e-5, false)
    const zD = refBranchLogits(Array.from(feats.w1d), dwD, dbD, uwD, ubD, lnwD, lnbD, WD, 0.2, 25, 1e-5, false)
    const pBio = refSoftmax(zBio), pC = refSoftmax(zC), pD = refSoftmax(zD)
    const pMix = [0, 1, 2, 3].map(i => 0.5 * pBio[i] + 0.25 * pC[i] + 0.25 * pD[i])
    const rawRef = refSoftmax(pMix.map(v => Math.log(Math.max(v, 1e-12)) / 0.97))
    const mergedRef = [rawRef[0] + rawRef[1], rawRef[2], rawRef[3]]

    const stages = leafScoreStages(head, feats, null)
    for (let i = 0; i < 4; i++) expect(stages.rawP[i]).toBeCloseTo(rawRef[i], 3)
    for (let i = 0; i < 3; i++) expect(stages.mergedP[i]).toBeCloseTo(mergedRef[i], 3)

    // Side of the Andes: applyGeoPrior on cached mergedP equals leafScores(head, feats, side) bit-for-bit!
    for (const side of ['East', 'West'] as const) {
      const fromStages = applyGeoPrior(stages.mergedP, head.geo, side)
      const direct = leafScores(head, feats, side)
      expect(Array.from(fromStages)).toEqual(Array.from(direct))
    }
    // West penalizes East-only leaves 0 and 1 (0.02x), boosting Gamma three (leaf 2)
    const eastSp = sumSpecies(head, leafScores(head, feats, 'East'))
    const westSp = sumSpecies(head, leafScores(head, feats, 'West'))
    expect(westSp[1]).toBeGreaterThan(eastSp[1])

    // Subspecies & speciesTop
    const sspTop = subspeciesTop(head, stages.p)
    expect(sspTop.map(([n]) => n)).toEqual(['Beta two ssp'])
    const bySp = subspeciesBySpecies(head, stages.p, [0])
    expect(bySp.get(0)!.map(x => x.name)).toEqual(['Beta two ssp'])
    const top = speciesTop(head, feats)
    expect(top[0][0]).toBe('Beta two')

    // compat_live_head_sha256 activates taxonomy forHead()
    const fakeTax = { head: LEAF_MERGES_6FA6AB78.head } as never
    expect(forHead(fakeTax, head.meta.head_sha256, head.meta.compat_live_head_sha256)).toBe(fakeTax)
    expect(forHead(fakeTax, head.meta.head_sha256)).toBeNull()
  })

  it('rejects unsupported blend_mode, malformed fusion head metadata, or missing branch features', () => {
    const dw = [[1, 0]], uw = [[1], [0]], W = [[1, 0], [0, 1]]
    const { buffer, offsets } = packFp16Matrices([dw, uw, W])
    const baseMeta: HeadMeta = {
      schema: 'ithomiini-offline-fusion-head/1',
      head_sha256: 'abc',
      weights: 'h.bin',
      weights_bytes: buffer.byteLength,
      rows: 2,
      K: 1,
      blend_mode: 'prob',
      T_disp: 0.97,
      branches: [
        { name: 'bio', role: 'encoder', input_size: 224, norm: 'clip', l2_normalize_input: true, weight: 1.0, arch: 'res_adapter', dim: 2, bottleneck: 1, residual_scale: 0.2, s: 25, ln_eps: 1e-5, dtype: 'float16', offsets: { down_w: offsets[0], up_w: offsets[1], W: offsets[2] }, down_b: [0], up_b: [0, 0], ln_w: [1, 1], ln_b: [0, 0] },
      ],
      species: ['Sp a', 'Sp b'],
      leaf2sp: [0, 1],
      leaves: ['Sp a', 'Sp b'],
      is_ssp: '00',
      geo: null,
      full_photo_overrides: [],
    }
    const h = makeHead(baseMeta, buffer)
    const p = leafScores(h, { bio: new Float32Array([1, 0]) })
    expect(p[0] + p[1]).toBeCloseTo(1, 6)
    expect(p[0]).toBeGreaterThan(p[1])

    // Error guards
    expect(() => leafScores(h, new Float32Array([1, 0]))).toThrow(/per-branch/)
    expect(() => leafScores(h, {})).toThrow(/missing feature/)
    expect(() => leafScores(h, { bio: new Float32Array([1, 2, 3]) })).toThrow(/expected dim 2/)
    expect(() => makeHead({ ...baseMeta, blend_mode: 'logit' as never }, buffer)).toThrow(/blend_mode/)
    expect(() => makeHead({ ...baseMeta, weights_bytes: buffer.byteLength + 4 }, buffer)).toThrow(/wrong size/)
    expect(() => makeHead({ ...baseMeta, T_disp: 0 }, buffer)).toThrow(/T_disp/)
  })
})
