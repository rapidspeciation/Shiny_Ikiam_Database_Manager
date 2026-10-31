import { describe, expect, it } from 'vitest'
import { cropBox, detectorInput, encoderInput, exifOrientation, letterboxCrop, makeHead, orientedSize, selectMasks, speciesTop, transformPoint, type HeadMeta, type Mask } from '../offlineId'

/** A minimal JPEG start with an EXIF block holding only the orientation tag. */
function jpegWithOrientation(o: number, little = true): ArrayBuffer {
  const tiff = new DataView(new ArrayBuffer(26))
  tiff.setUint16(0, little ? 0x4949 : 0x4d4d)
  tiff.setUint16(2, 42, little)
  tiff.setUint32(4, 8, little)
  tiff.setUint16(8, 1, little)
  tiff.setUint16(10, 274, little)
  tiff.setUint16(12, 3, little)
  tiff.setUint32(14, 1, little)
  tiff.setUint16(18, o, little)
  const app1 = new Uint8Array(4 + 6 + 26)
  const v = new DataView(app1.buffer)
  v.setUint16(0, 0xffe1)
  v.setUint16(2, 2 + 6 + 26)
  app1.set([0x45, 0x78, 0x69, 0x66, 0, 0], 4)
  app1.set(new Uint8Array(tiff.buffer), 10)
  const out = new Uint8Array(2 + app1.length + 2)
  out.set([0xff, 0xd8])
  out.set(app1, 2)
  out.set([0xff, 0xda], 2 + app1.length)
  return out.buffer
}

const solid = (w: number, h: number, f: (x: number, y: number) => [number, number, number]) => {
  const data = new Uint8Array(w * h * 4)
  for (let y = 0; y < h; y++) for (let x = 0; x < w; x++) data.set([...f(x, y), 255], (y * w + x) * 4)
  return { width: w, height: h, data }
}

const mask = (uid: string, confidence: number, bbox: [number, number, number, number], cells: number[] = []): Mask => {
  const raster = new Uint8Array(64 * 64)
  for (const c of cells) raster[c] = 1
  return { uid, confidence, bbox, raster, fill: 0.5, clamp: 0 }
}

describe('offline identifier chain', () => {
  it('reads the EXIF orientation of a JPEG, both byte orders; 1 for anything else', () => {
    expect(exifOrientation(jpegWithOrientation(6))).toBe(6)
    expect(exifOrientation(jpegWithOrientation(3, false))).toBe(3)
    expect(exifOrientation(new Uint8Array([0x89, 0x50, 0x4e, 0x47]).buffer)).toBe(1)
    expect(exifOrientation(jpegWithOrientation(12))).toBe(1)
  })

  it('maps stored points into the upright photo as oriented_tight_crop does', () => {
    expect(orientedSize(4000, 3000, 6)).toEqual([3000, 4000])
    expect(transformPoint(0.1, 0.2, 6)).toEqual([0.8, 0.1])
    expect(transformPoint(0.1, 0.2, 8)).toEqual([0.2, 0.9])
    expect(transformPoint(0.1, 0.2, 3)).toEqual([0.9, 0.8])
  })

  it('letterboxes the stored photo for the detector: centred, grey 114, the turn undone', () => {
    // upright 2 x 4 (orientation 6: stored 4 x 2), left column red, right column blue
    const up = solid(2, 4, x => (x === 0 ? [255, 0, 0] : [0, 0, 255]))
    const { x, padX, padY, gain, rawW, rawH } = detectorInput(up, 6, 8)
    expect([rawW, rawH, gain, padX, padY]).toEqual([4, 2, 2, 0, 2])
    const at = (c: number, row: number, col: number) => x[c * 64 + row * 8 + col]
    expect(at(0, 0, 0)).toBeCloseTo(114 / 255)
    // stored row 0 is the upright right column (blue), stored row 1 the left one (red)
    expect(at(2, 2, 3)).toBe(1)
    expect(at(0, 5, 3)).toBe(1)
  })

  it('crops the union of the kept masks, the central 90 % when none, the whole photo for a reviewed override', () => {
    const a = mask('p000', 0.9, [0.1, 0.2, 0.3, 0.4], [1])
    const b = mask('p001', 0.8, [0.5, 0.5, 0.6, 0.7], [2000])
    expect(cropBox(selectMasks([a, b]), 1, 1000, 1000)).toEqual({ box: [100, 200, 600, 700], status: 'success' })
    // (1 - 0.9) / 2 is 0.04999… in floating point, in Python too: 49, not 50
    expect(cropBox([], 1, 1000, 500)).toEqual({ box: [49, 24, 950, 475], status: 'centre_crop_fallback' })
    expect(cropBox([a], 1, 1000, 500, true)).toEqual({ box: [0, 0, 1000, 500], status: 'reviewed_full_image_override' })
    // the same box in a stored photo turned 90 degrees (orientation 6): upright x = 1 - stored y
    expect(cropBox([a], 6, 1000, 1000).box).toEqual([600, 100, 800, 300])
  })

  it('merges confident duplicates into their most confident mask and drops rectangle artefacts', () => {
    const cells = Array.from({ length: 100 }, (_, i) => i)
    const a = mask('p000', 0.6, [0.1, 0.1, 0.4, 0.4], cells)
    const b = mask('p001', 0.9, [0.1, 0.1, 0.41, 0.4], cells)
    expect(selectMasks([a, b]).map(m => m.uid)).toEqual(['p001'])
    const strip = { ...mask('p002', 0.95, [0, 0.45, 1, 0.5], [3000]), fill: 0.95 }
    expect(selectMasks([strip, a]).map(m => m.uid)).toEqual(['p000'])
  })

  it('pads the crop to a grey square and resizes it like Pillow (a flat colour stays flat)', () => {
    const img = solid(50, 40, () => [200, 100, 50])
    const sq = letterboxCrop(img, [0, 0, 50, 40], 10)
    expect(Array.from(letterboxCrop(img, [0, 0, 50, 10], 10).slice(0, 3))).toEqual([128, 128, 128])
    expect(Array.from(sq.slice((5 * 10 + 5) * 3, (5 * 10 + 5) * 3 + 3))).toEqual([200, 100, 50])
    const x = encoderInput(new Uint8Array(224 * 224 * 3).fill(255))
    expect(x[0]).toBeCloseTo((1 - 0.48145466) / 0.26862954, 5)
  })

  it('sums the head over leaves per species and applies the side of the Andes', () => {
    // 3 leaves x 2 dims: leaf 0 and 1 are species A, leaf 2 species B
    const meta: HeadMeta = {
      schema: 'ithomiini-offline-head/1', head_sha256: '', weights: '', rows: 3, dim: 2, K: 1, s: 30, ln_eps: 1e-5,
      ln_w: [1, 1], ln_b: [0, 0], species: ['A', 'B'], leaf2sp: [0, 0, 1], geo: '7d9', full_photo_overrides: [],
    }
    // float16: 1.0 = 0x3c00, 0 = 0, -1.0 = 0xbc00
    const w = new Uint16Array([0x3c00, 0xbc00, 0x3c00, 0xbc00, 0xbc00, 0x3c00])
    const head = makeHead(meta, w.buffer)
    const top = speciesTop(head, new Float32Array([1, 0]))
    expect(top[0][0]).toBe('A')
    expect(top[0][1] + top[1][1]).toBeCloseTo(1, 3)
    // flags: leaf 0 known, in Ecuador, East (7); leaf 1 East and West but not in Ecuador (d); leaf 2 West only (9).
    // Seen East, B (leaf 2) weighs 2 %: on a feature where A and B tie, A wins 50:1.
    const tie = new Float32Array([0, 0])
    const plain = speciesTop(head, tie), east = speciesTop(head, tie, 'East')
    expect(plain[0][1]).toBeCloseTo(east[0][0] === 'A' ? plain[0][1] : 0, 6)
    expect(east[0][0]).toBe('A')
  })
})
