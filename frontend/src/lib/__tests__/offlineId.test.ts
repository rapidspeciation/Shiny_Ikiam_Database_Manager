import { describe, expect, it } from 'vitest'
import { applyP1aNms, applyP3Roi, cropBox, detectorInput, encoderInput, exifOrientation, leafScores, letterboxCrop, makeHead, orientedSize, readMasks, selectMasks, speciesScores, speciesTop, subspeciesBySpecies, subspeciesTop, sumSpecies, transformPoint, type DetectorInput, type HeadMeta, type Mask, type MaskRoi } from '../offlineId'

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
    // Without a side the three leaves tie: A 2/3, B 1/3. Seen East, leaves 1 and 2 weigh 0.02: A (1 + 0.02) / 1.04.
    expect(plain.map(([s]) => s)).toEqual(['A', 'B'])
    expect(plain[0][1]).toBeCloseTo(2 / 3, 3)
    expect(plain[1][1]).toBeCloseTo(1 / 3, 3)
    expect(east.map(([s]) => s)).toEqual(['A', 'B'])
    expect(east[0][1]).toBeCloseTo(1.02 / 1.04, 3)
    expect(east[1][1]).toBeCloseTo(0.02 / 1.04, 3)
  })

  it('names subspecies with a schema 2 head as worker.py tops does, and none with schema 1', () => {
    // leaves: 0 "G a" (species leaf), 1 "G a x", 2 "G a y", 3 "G b z"
    const base = {
      head_sha256: '', weights: '', rows: 4, dim: 2, K: 1, s: 30, ln_eps: 1e-5, ln_w: [1, 1], ln_b: [0, 0],
      species: ['G a', 'G b'], leaf2sp: [0, 0, 0, 1], geo: null, full_photo_overrides: [],
    }
    // leaf directions: 1 and 2 near (1, 0), 0 at (0, 1), 3 at (-1, 0)
    const w = new Uint16Array([0, 0x3c00, 0x3c00, 0, 0x3c00, 0x2e66, 0xbc00, 0])
    const v1 = makeHead({ schema: 'ithomiini-offline-head/1', ...base } as HeadMeta, w.buffer)
    const v2 = makeHead({ schema: 'ithomiini-offline-head/2', ...base, leaves: ['G a', 'G a x', 'G a y', 'G b z'], is_ssp: '0111' } as HeadMeta, w.buffer)
    const f = new Float32Array([1, -0.2])
    const p = leafScores(v2, f)
    expect(Array.from(sumSpecies(v2, p))).toEqual(Array.from(speciesScores(v1, f)))
    expect(subspeciesTop(v1, leafScores(v1, f))).toEqual([])
    const top = subspeciesTop(v2, p)
    expect(top.map(t => t[0])).toEqual(['G a x', 'G a y', 'G b z'])
    expect(top[0][1]).toBeCloseTo(p[1], 4)
    const by = subspeciesBySpecies(v2, p, [0])
    expect(by.get(0)!.map(c => c.name)).toEqual(['G a x', 'G a y'])
    expect(by.has(1)).toBe(false)
  })
})

/** Build a W x H binary grid with given rectangular [x0, y0, x1, y1] (exclusive) blocks set to 1. */
function makeGrid(W: number, H: number, rects: [number, number, number, number][]): Uint8Array {
  const m = new Uint8Array(W * H)
  for (const [x0, y0, x1, y1] of rects)
    for (let r = y0; r < y1; r++)
      for (let c = x0; c < x1; c++) m[r * W + c] = 1
  return m
}

function roiMask(uid: string, confidence: number, x0: number, y0: number, w: number, h: number, data?: Uint8Array): Mask {
  const d = data ?? new Uint8Array(w * h).fill(1)
  let area = 0
  for (let i = 0; i < d.length; i++) area += d[i]
  const roi: MaskRoi = { x0, y0, w, h, data: d, area }
  return {
    uid,
    confidence,
    bbox: [x0 / 640, y0 / 640, (x0 + w - 1) / 640, (y0 + h - 1) / 640],
    raster: new Uint8Array(64 * 64),
    fill: 1,
    clamp: 0,
    roi,
  }
}

describe('P3 contour cleanup and P1-A mask-IoU NMS', () => {
  it('applyP3Roi passes single-component masks through unchanged (preserves 1-px open slit)', () => {
    const m = makeGrid(40, 40, [[10, 10, 26, 26]])
    for (let r = 10; r < 15; r++) m[r * 40 + 17] = 0 // 1-px vertical slit open to top edge
    const out = applyP3Roi(m, 40, 40)
    expect(out).toBe(m) // exact passthrough without MORPH_CLOSE altering the slit
  })

  it('applyP3Roi ignores nested speck inside a hole (matches OpenCV RETR_EXTERNAL count)', () => {
    // Hollow ring [4..24, 4..24] with hole [8..20, 8..20], a 1-px external notch on top edge,
    // and a floating 2x2 speck inside the hole: still 1 external contour -> passthrough unchanged.
    const m = makeGrid(28, 28, [[4, 4, 24, 24]])
    for (let r = 8; r < 20; r++) for (let c = 8; c < 20; c++) m[r * 28 + c] = 0
    for (let r = 13; r < 15; r++) for (let c = 13; c < 15; c++) m[r * 28 + c] = 1 // nested speck
    m[4 * 28 + 14] = 0 // 1-px external notch that MORPH_CLOSE would fill if wrongly triggered
    const out = applyP3Roi(m, 28, 28)
    expect(out).toBe(m)
  })

  it('applyP3Roi closes a border-touching 1-px gap with exact OpenCV border semantics', () => {
    // 5x3 ROI with two border-touching 2x2 blocks separated by a 1-px gap at col 2:
    // row 0: [1, 1, 0, 1, 1]
    // row 1: [1, 1, 0, 1, 1]
    // row 2: [0, 0, 0, 0, 0]
    // Dilate (0-padded) fills all 15 pixels; erode (1-padded) keeps all 15 -> exact match with cv2.MORPH_CLOSE!
    // Use 5x4 so row 3 stays 0 and tests both top/side borders (1-padded erode) and interior erosion:
    // row 0: [1,1,0,1,1], row 1: [1,1,0,1,1], row 2: [0,0,0,0,0], row 3: [0,0,0,0,0]
    // Dilate -> rows 0..2 all 1, row 3 all 0. Erode -> rows 0..1 all 1, rows 2..3 all 0.
    const m = makeGrid(5, 4, [[0, 0, 2, 2], [3, 0, 5, 2]])
    const out = applyP3Roi(m, 5, 4)
    expect(Array.from(out)).toEqual([
      1, 1, 1, 1, 1,
      1, 1, 1, 1, 1,
      0, 0, 0, 0, 0,
      0, 0, 0, 0, 0,
    ])
  })

  it('applyP3Roi prefers a 2D blob over a longer 1-px line of zero contour area', () => {
    // 1x10 horizontal line (10 px, 0 contour area) vs distant 3x3 solid square (9 px, 2D contour area = 4)
    const m = makeGrid(20, 20, [[2, 2, 12, 3], [14, 14, 17, 17]])
    const out = applyP3Roi(m, 20, 20)
    expect(out[15 * 20 + 15]).toBe(1)
    expect(out[2 * 20 + 5]).toBe(0)
  })

  it('applyP3Roi bridges a 1-px gap between two components via 3x3 MORPH_CLOSE', () => {
    // Two blocks separated by a 1-px column gap at c = 20
    const m = makeGrid(50, 40, [[8, 10, 20, 25], [21, 10, 35, 25]])
    const out = applyP3Roi(m, 50, 40)
    // Gap column 20 is bridged inside rows 10..24
    expect(out[15 * 50 + 20]).toBe(1)
    expect(out[10 * 50 + 8]).toBe(1)
    expect(out[24 * 50 + 34]).toBe(1)
  })

  it('applyP3Roi removes distant speck/island without bridge-chord expansion', () => {
    // 30x30 main wing blob + 6x6 distant speck at [80..85, 60..65]
    const m = makeGrid(100, 80, [[10, 10, 40, 40], [80, 60, 86, 66]])
    const out = applyP3Roi(m, 100, 80)
    let xMax = -1, yMax = -1, count = 0
    for (let r = 0; r < 80; r++)
      for (let c = 0; c < 100; c++)
        if (out[r * 100 + c]) {
          count++
          if (c > xMax) xMax = c
          if (r > yMax) yMax = r
        }
    expect(count).toBe(900)
    expect(xMax).toBe(39)
    expect(yMax).toBe(39)
    expect(out[62 * 100 + 82]).toBe(0)
  })

  it('applyP3Roi treats diagonal pinch (8-connected) as a single component without splitting', () => {
    // Two squares touching diagonally at (19,19)-(20,20): 8-connected single component, matches cv2.findContours
    const m = makeGrid(40, 40, [[10, 10, 20, 20], [20, 20, 28, 28]])
    const out = applyP3Roi(m, 40, 40)
    expect(out).toBe(m)
  })

  it('readMasks rejects masks that drop below 3 pixels after P3 island removal', () => {
    // Construct a 16x16 proto grid on S=16 (1:1 pixel mapping) with two disconnected 2-px specks (total 4 >= 3 pre-P3, 2 < 3 post-P3)
    const S = 16, P = 16
    const proto = new Float32Array(32 * P * P).fill(-1)
    // Speck 1: (2,2), (3,2); Speck 2: (12,12), (13,12)
    proto[2 * P + 2] = 1; proto[2 * P + 3] = 1
    proto[12 * P + 12] = 1; proto[12 * P + 13] = 1
    const det = new Float32Array(38)
    det[0] = 0; det[1] = 0; det[2] = 16; det[3] = 16; det[4] = 0.9; det[6] = 1
    const inp: DetectorInput = { x: new Float32Array(0), gain: 1, padX: 0, padY: 0, rawW: 16, rawH: 16 }
    expect(readMasks(det, 1, 38, proto, P, inp, 0.25, S)).toEqual([])
  })

  it('readMasks + applyP1aNms strips distant specks from cropBox and suppresses IoU >= 0.65 duplicates', () => {
    // 64x64 1:1 grid (S=64, P=64):
    // mask 0 (conf 0.90): main 20x20 blob [10..30, 10..30] + distant 4x4 speck [52..56, 52..56]
    // mask 1 (conf 0.70): overlapping duplicate [10..27, 10..27] (17x17 = 289 inside 20x20 = 400 -> IoU = 0.7225 >= 0.65)
    // mask 2 (conf 0.60): separate wing [36..50, 10..24] (0 overlap with mask 0)
    const S = 64, P = 64
    const proto = new Float32Array(32 * P * P).fill(-1)
    const setRect = (ch: number, x0: number, y0: number, x1: number, y1: number) => {
      const off = ch * P * P
      for (let r = y0; r < y1; r++) for (let c = x0; c < x1; c++) proto[off + r * P + c] = 1
    }
    setRect(0, 10, 10, 30, 30)
    setRect(0, 52, 52, 56, 56) // distant speck
    setRect(1, 10, 10, 27, 27) // 0.7225 IoU duplicate of main blob (below selectMasks's 0.88 threshold, caught by P1-A!)
    setRect(2, 36, 10, 50, 24) // distinct wing

    const det = new Float32Array(3 * 38)
    // row 0
    det[0] = 0; det[1] = 0; det[2] = 64; det[3] = 64; det[4] = 0.90; det[6 + 0] = 1
    // row 1
    det[38 + 0] = 0; det[38 + 1] = 0; det[38 + 2] = 64; det[38 + 3] = 64; det[38 + 4] = 0.70; det[38 + 6 + 1] = 1
    // row 2
    det[76 + 0] = 0; det[76 + 1] = 0; det[76 + 2] = 64; det[76 + 3] = 64; det[76 + 4] = 0.60; det[76 + 6 + 2] = 1

    const inp: DetectorInput = { x: new Float32Array(0), gain: 1, padX: 0, padY: 0, rawW: 64, rawH: 64 }
    const raw = readMasks(det, 3, 38, proto, P, inp, 0.25, S)
    expect(raw.map(m => m.uid)).toEqual(['p000', 'p001', 'p002'])
    // P3 stripped the distant speck [52..56] from p000 so its bbox max is 29/64, not 55/64
    expect(raw[0].bbox[2]).toBeCloseTo(29 / 64, 6)
    expect(raw[0].bbox[3]).toBeCloseTo(29 / 64, 6)
    // P1-A suppresses p001 (IoU = 289/400 = 0.7225 >= 0.65) while keeping p000 and p002
    const nms = applyP1aNms(raw, 0.65, 0.25)
    expect(nms.map(m => m.uid)).toEqual(['p000', 'p002'])
    // Crop box stays tightly within y <= 29 (speck at y=55 completely gone)
    const { box, status } = cropBox(selectMasks(nms), 1, 64, 64)
    expect(status).toBe('success')
    expect(box).toEqual([10, 10, 49, 29])
  })

  it('applyP1aNms enforces <=1 early return, empty-mask drop, exact 0.65 boundary, uid tie-break, and <0.25 passthrough', () => {
    // 1. <= 1 eligible early return (even empty mask passes through untouched)
    const emptyElig = roiMask('p000', 0.90, 0, 0, 10, 10, new Uint8Array(100))
    const below = roiMask('p001', 0.10, 0, 0, 10, 10)
    const singleRaw = [emptyElig, below]
    expect(applyP1aNms(singleRaw, 0.65, 0.25)).toBe(singleRaw)
    expect(applyP1aNms([], 0.65, 0.25)).toEqual([])

    // 2. Empty-mask drop when > 1 eligible
    const good = roiMask('p002', 0.80, 10, 10, 100, 10)
    expect(applyP1aNms([emptyElig, good], 0.65, 0.25).map(m => m.uid)).toEqual(['p002'])

    // 3. Exact 0.65 threshold vs 0.64 below threshold (100x10 vs 65x10 -> IoU = 0.65; 64x10 -> IoU = 0.64)
    const base = roiMask('p000', 0.90, 0, 0, 100, 10)
    const exact65 = roiMask('p001', 0.80, 0, 0, 65, 10)
    const below64 = roiMask('p002', 0.75, 0, 0, 64, 10)
    expect(applyP1aNms([exact65, base], 0.65, 0.25).map(m => m.uid)).toEqual(['p000'])
    expect(applyP1aNms([below64, base], 0.65, 0.25).map(m => m.uid)).toEqual(['p000', 'p002'])

    // 4. Confidence tie breaks by uid ascending
    const t0 = roiMask('p000', 0.80, 0, 0, 50, 50)
    const t1 = roiMask('p001', 0.80, 0, 0, 50, 50)
    expect(applyP1aNms([t1, t0], 0.65, 0.25).map(m => m.uid)).toEqual(['p000'])

    // 5. < minConf passes through untouched after kept eligible masks even at 100% overlap
    const lo1 = roiMask('p003', 0.24, 0, 0, 100, 10)
    const lo2 = roiMask('p004', 0.05, 0, 0, 10, 10, new Uint8Array(100))
    expect(applyP1aNms([lo1, exact65, base, lo2], 0.65, 0.25).map(m => m.uid)).toEqual(['p000', 'p003', 'p004'])

    // 6. Non-convex L-shapes sharing the same bounding box (bbox IoU = 1.0, mask IoU = 0.0) -> both kept
    const lA = new Uint8Array(100 * 100)
    const lB = new Uint8Array(100 * 100)
    for (let r = 0; r < 100; r++)
      for (let c = 0; c < 100; c++) {
        if (r < 20 || c < 20) lA[r * 100 + c] = 1
        else if (r >= 25 && c >= 25) lB[r * 100 + c] = 1
      }
    const mA = roiMask('p000', 0.90, 0, 0, 100, 100, lA)
    const mB = roiMask('p001', 0.80, 0, 0, 100, 100, lB)
    expect(applyP1aNms([mA, mB], 0.65, 0.25).map(m => m.uid)).toEqual(['p000', 'p001'])

    // 7. Fallback 64x64 raster IoU when `roi` is omitted (synthetic Mask mocks)
    const cells100 = Array.from({ length: 100 }, (_, i) => i)
    const cells65 = Array.from({ length: 65 }, (_, i) => i)
    const cells64 = Array.from({ length: 64 }, (_, i) => i)
    expect(applyP1aNms([mask('p001', 0.7, [0, 0, 0.5, 0.5], cells65), mask('p000', 0.9, [0, 0, 0.5, 0.5], cells100)]).map(m => m.uid)).toEqual(['p000'])
    expect(applyP1aNms([mask('p002', 0.7, [0, 0, 0.5, 0.5], cells64), mask('p000', 0.9, [0, 0, 0.5, 0.5], cells100)]).map(m => m.uid)).toEqual(['p000', 'p002'])
  })
})
