/**
 * The AI Identifier's chain in the browser, for the offline test page (/prueba-offline/, src/offline/main.ts):
 * the same steps as the server's worker (tools/identifier/worker.py, v6_feature.py and its oriented_tight_crop /
 * biological_object_dedup), without the models themselves (the page runs them with onnxruntime-web).
 *
 * 1. The wing detector (wing_seg_s2.onnx since 5 Nov 2026, end-to-end head) on the photo as stored, not turned by its EXIF tag
 *    (the server gives it the raw pixels): a 640 px square letterbox as Ultralytics makes it (bilinear like
 *    cv2.INTER_LINEAR, grey 114).
 * 2. Its masks (prototypes upsampled to 640, > 0, cut to the box), each mask's outline measures, the masks under
 *    0.25 left out, rectangle artefacts dropped, duplicates merged; the union box of what is left, in the upright
 *    photo. Nothing found: the central 90 %.
 * 3. That box cut from the upright photo, padded to a grey (128) square and resized to 224 px with Pillow's
 *    bicubic filter in its own integer arithmetic.
 * 4. The encoder's feature, normalised, through the cosine head (fp16 weights), the leaves summed per species.
 *    Or the small model's (studentInput: the same crop at 192 px), the same 1024-D feature and the same head.
 *
 * Where the server reads the mask's outline (cv2 contours) this reads the mask's pixels: same box, close shape
 * measures (they only decide rare cases: rectangle artefacts and overlapping duplicates).
 */

export interface Rgba { width: number; height: number; data: Uint8ClampedArray | Uint8Array }

// ---------------------------------------------------------------- EXIF orientation

/** The JPEG's EXIF orientation (tag 274), 1 when there is none (PNG, no EXIF, unreadable). */
export function exifOrientation(buf: ArrayBuffer): number {
  const v = new DataView(buf)
  if (v.byteLength < 4 || v.getUint16(0) !== 0xffd8) return 1
  let p = 2
  while (p + 4 <= v.byteLength) {
    const marker = v.getUint16(p)
    if ((marker & 0xff00) !== 0xff00) return 1
    const len = v.getUint16(p + 2)
    if (marker === 0xffe1 && p + 10 <= v.byteLength && v.getUint32(p + 4) === 0x45786966) {
      const t = p + 10
      const little = v.getUint16(t) === 0x4949
      const u16 = (o: number) => v.getUint16(o, little)
      const u32 = (o: number) => v.getUint32(o, little)
      const ifd = t + u32(t + 4)
      if (ifd + 2 > v.byteLength) return 1
      const n = u16(ifd)
      for (let i = 0; i < n; i++) {
        const e = ifd + 2 + i * 12
        if (e + 12 > v.byteLength) return 1
        if (u16(e) === 274) {
          const o = u16(e + 8)
          return o >= 1 && o <= 8 ? o : 1
        }
      }
      return 1
    }
    if (marker === 0xffda) return 1
    p += 2 + len
  }
  return 1
}

/** oriented_tight_crop.oriented_size */
export const orientedSize = (w: number, h: number, o: number): [number, number] => (o >= 5 ? [h, w] : [w, h])

/** oriented_tight_crop.transform_point: a normalised point of the stored photo in the upright photo. */
export function transformPoint(x: number, y: number, o: number): [number, number] {
  switch (o) {
    case 2: return [1 - x, y]
    case 3: return [1 - x, 1 - y]
    case 4: return [x, 1 - y]
    case 5: return [y, x]
    case 6: return [1 - y, x]
    case 7: return [1 - y, 1 - x]
    case 8: return [y, 1 - x]
    default: return [x, y]
  }
}

/** Python's round (half to even), as Ultralytics' letterbox and scale_coords use it. */
const pyRound = (x: number) => {
  const f = Math.floor(x), d = x - f
  return d > 0.5 ? f + 1 : d < 0.5 ? f : f % 2 === 0 ? f : f + 1
}

// ---------------------------------------------------------------- 1. detector input

export interface DetectorInput { x: Float32Array; gain: number; padX: number; padY: number; rawW: number; rawH: number }

/**
 * The 640 px letterbox of the stored (raw) photo, read from the upright pixels: Ultralytics' LetterBox
 * (resize with cv2.INTER_LINEAR, centred, grey 114), RGB, CHW, / 255.
 */
export function detectorInput(upright: Rgba, orientation: number, S = 640): DetectorInput {
  const [rawW, rawH] = orientedSize(upright.width, upright.height, orientation) // the inverse swap is the same swap
  const r = Math.min(S / rawH, S / rawW)
  const nw = pyRound(rawW * r), nh = pyRound(rawH * r)
  const dw = (S - nw) / 2, dh = (S - nh) / 2
  const padX = pyRound(dw - 0.1), padY = pyRound(dh - 0.1)
  const plane = S * S
  const x = new Float32Array(3 * plane).fill(114 / 255)
  const { width: UW, data } = upright
  // raw pixel (rx, ry) -> upright pixel index
  const at = (rx: number, ry: number) => {
    const [u, v] = transformPoint((rx + 0.5) / rawW, (ry + 0.5) / rawH, orientation)
    return (Math.floor(v * upright.height) * UW + Math.floor(u * UW)) * 4
  }
  const sx = rawW / nw, sy = rawH / nh
  const xs0 = new Int32Array(nw), xs1 = new Int32Array(nw), wx = new Float32Array(nw)
  for (let i = 0; i < nw; i++) {
    let f = (i + 0.5) * sx - 0.5
    if (f < 0) f = 0
    let i0 = Math.floor(f)
    if (i0 >= rawW - 1) { i0 = rawW - 1; f = i0 }
    xs0[i] = i0; xs1[i] = Math.min(i0 + 1, rawW - 1); wx[i] = f - i0
  }
  for (let j = 0; j < nh; j++) {
    let f = (j + 0.5) * sy - 0.5
    if (f < 0) f = 0
    let j0 = Math.floor(f)
    if (j0 >= rawH - 1) { j0 = rawH - 1; f = j0 }
    const j1 = Math.min(j0 + 1, rawH - 1), wy = f - j0
    for (let i = 0; i < nw; i++) {
      const a = at(xs0[i], j0), b = at(xs1[i], j0), c = at(xs0[i], j1), d = at(xs1[i], j1)
      const t = (j + padY) * S + i + padX, w = wx[i]
      for (let ch = 0; ch < 3; ch++) {
        const top = data[a + ch] * (1 - w) + data[b + ch] * w
        const bot = data[c + ch] * (1 - w) + data[d + ch] * w
        x[ch * plane + t] = Math.round(top * (1 - wy) + bot * wy) / 255
      }
    }
  }
  return { x, gain: r, padX, padY, rawW, rawH }
}

// ---------------------------------------------------------------- 2. masks and the crop's box

export interface Mask {
  uid: string
  confidence: number
  /** normalised in the stored photo: [x0, y0, x1, y1] of the mask's pixels */
  bbox: [number, number, number, number]
  /** 64 x 64 occupancy (biological_object_dedup._rasterize_polygon), row-major */
  raster: Uint8Array
  /** polygon area / bbox area, from the pixels */
  fill: number
  /** straight runs along its box (common.bbox_clamp_segments), from the pixels */
  clamp: number
}

const RASTER = 64

/**
 * The detector's masks of confidence >= `minConf` (end-to-end output: [N, 38] rows x1 y1 x2 y2 score class
 * coef[32], 640 px input; prototypes [32, 160, 160]), as Ultralytics process_mask (upsample=True) and
 * masks.xyn give them, measured from the pixels.
 */
export function readMasks(det: Float32Array, N: number, D: number, proto: Float32Array, P: number, inp: DetectorInput, minConf = 0.25, S = 640): Mask[] {
  const out: Mask[] = []
  const n = P * P, k = P / S
  const { gain, padX, padY, rawW, rawH } = inp
  for (let i = 0; i < N; i++) {
    const score = det[i * D + 4]
    if (!(score >= minConf)) continue
    const x1 = det[i * D], y1 = det[i * D + 1], x2 = det[i * D + 2], y2 = det[i * D + 3]
    const pm = new Float32Array(n)
    for (let c = 0; c < 32; c++) {
      const w = det[i * D + 6 + c], off = c * n
      if (w === 0) continue
      for (let j = 0; j < n; j++) pm[j] += w * proto[off + j]
    }
    // F.interpolate(bilinear, align_corners=False) to S x S, > 0, then crop_mask: columns r >= x1 and r < x2
    const c0 = Math.max(0, Math.ceil(x1)), c1 = Math.min(S - 1, Math.ceil(x2) - 1)
    const r0 = Math.max(0, Math.ceil(y1)), r1 = Math.min(S - 1, Math.ceil(y2) - 1)
    if (c1 < c0 || r1 < r0) continue
    const W = c1 - c0 + 1, H = r1 - r0 + 1
    const m = new Uint8Array(W * H)
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
    if (count < 3) continue // no outline of three points: v6_feature leaves it out
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
    // scale_coords: letterbox pixel -> stored photo, clipped, normalised
    const nx = (px: number) => Math.min(Math.max((px - padX) / gain, 0), rawW) / rawW
    const ny = (py: number) => Math.min(Math.max((py - padY) / gain, 0), rawH) / rawH
    const bbox: [number, number, number, number] = [nx(c0 + mx0), ny(r0 + my0), nx(c0 + mx1), ny(r0 + my1)]
    // the outline through the edge pixels' centres encloses about the pixels less half the edge
    const area = Math.max(0, count - edge / 2 - 1) / (gain * gain * rawW * rawH)
    const boxArea = (bbox[2] - bbox[0]) * (bbox[3] - bbox[1])
    const raster = new Uint8Array(RASTER * RASTER)
    for (let row = 0; row < RASTER; row++) {
      const py = Math.round(((row + 0.5) / RASTER) * rawH * gain + padY) - r0
      if (py < 0 || py >= H) continue
      for (let col = 0; col < RASTER; col++) {
        const px = Math.round(((col + 0.5) / RASTER) * rawW * gain + padX) - c0
        if (px >= 0 && px < W && m[py * W + px]) raster[row * RASTER + col] = 1
      }
    }
    out.push({ uid: `p${String(i).padStart(3, '0')}`, confidence: score, bbox, raster, fill: boxArea ? area / boxArea : 0, clamp: clampRuns(m, W, H, mx0, my0, mx1, my1, bbox) })
  }
  return out
}

/** Runs of the mask along its own box's sides at least 12 % of that side long, not on the photo's border. */
function clampRuns(m: Uint8Array, W: number, _H: number, x0: number, y0: number, x1: number, y1: number, bbox: number[]): number {
  const bw = x1 - x0 + 1, bh = y1 - y0 + 1
  let runs = 0
  const count = (len: number, get: (t: number) => number, min: number) => {
    let run = 0
    for (let t = 0; t <= len; t++) {
      if (t < len && get(t)) run++
      else { if (run >= min) runs++; run = 0 }
    }
  }
  const nearBorder = (v: number) => Math.min(Math.abs(v), Math.abs(1 - v)) <= 0.003
  if (!nearBorder(bbox[1])) count(bw, t => m[y0 * W + x0 + t], 0.12 * bw)
  if (!nearBorder(bbox[3])) count(bw, t => m[y1 * W + x0 + t], 0.12 * bw)
  if (!nearBorder(bbox[0])) count(bh, t => m[(y0 + t) * W + x0], 0.12 * bh)
  if (!nearBorder(bbox[2])) count(bh, t => m[(y0 + t) * W + x1], 0.12 * bh)
  return runs
}

/** oriented_tight_crop.artifact_reason_codes */
function artifact(mk: Mask): boolean {
  const w = Math.max(0, mk.bbox[2] - mk.bbox[0]), h = Math.max(0, mk.bbox[3] - mk.bbox[1])
  if (w && h && Math.max(w / h, h / w) >= 10 && mk.fill >= 0.8) return true
  if (mk.clamp >= 5 && mk.fill >= 0.85) return true
  return w * h >= 0.92 && mk.fill >= 0.9
}

/** biological_object_dedup.classify_pair, whole-insect rule: a confident duplicate. */
function duplicate(a: Mask, b: Mask): boolean {
  let inter = 0, ua = 0, ub = 0
  for (let i = 0; i < a.raster.length; i++) { inter += a.raster[i] & b.raster[i]; ua += a.raster[i]; ub += b.raster[i] }
  const union = ua + ub - inter
  const iou = union ? inter / union : 0
  const ca = ua ? inter / ua : 0, cb = ub ? inter / ub : 0
  const hi = Math.max(ca, cb), lo = Math.min(ca, cb)
  const cx = (m: Mask) => [(m.bbox[0] + m.bbox[2]) / 2, (m.bbox[1] + m.bbox[3]) / 2]
  const [ax, ay] = cx(a), [bx, by] = cx(b)
  const u = [Math.min(a.bbox[0], b.bbox[0]), Math.min(a.bbox[1], b.bbox[1]), Math.max(a.bbox[2], b.bbox[2]), Math.max(a.bbox[3], b.bbox[3])]
  const diag = Math.hypot(u[2] - u[0], u[3] - u[1])
  const centroid = diag ? Math.hypot(ax - bx, ay - by) / diag : 0
  const ratio = Math.min(ua, ub) > 0 ? Math.max(ua, ub) / Math.min(ua, ub) : Infinity
  return iou >= 0.88 || (hi >= 0.95 && lo >= 0.58 && centroid <= 0.24 && ratio <= 1.75)
}

/** The masks kept for the crop: biological_object_dedup.resolve_biological_objects' representatives. */
export function selectMasks(masks: Mask[]): Mask[] {
  if (!masks.length) return []
  let bio = masks.filter(m => !artifact(m))
  if (!bio.length) bio = [masks.reduce((a, b) => (b.confidence > a.confidence || (b.confidence === a.confidence && b.uid > a.uid) ? b : a))]
  let clusters = [...bio].sort((a, b) => (a.uid < b.uid ? -1 : 1)).map(m => [m])
  for (let changed = true; changed; ) {
    changed = false
    outer: for (let i = 0; i < clusters.length; i++)
      for (let j = i + 1; j < clusters.length; j++)
        if (clusters[i].every(a => clusters[j].every(b => duplicate(a, b)))) {
          clusters[i] = [...clusters[i], ...clusters[j]].sort((a, b) => (a.uid < b.uid ? -1 : 1))
          clusters = clusters.filter((_, k) => k !== j)
          changed = true
          break outer
        }
  }
  return clusters.map(cluster => {
    const ordered = [...cluster].sort((a, b) => b.confidence - a.confidence || (a.uid < b.uid ? -1 : 1))
    const winner = ordered[0]
    if (winner.clamp >= 5) {
      const clean = ordered.slice(1).find(m => m.clamp < 5)
      if (clean && winner.confidence - clean.confidence <= 0.15) return clean
    }
    return winner
  })
}

export type CropStatus = 'success' | 'centre_crop_fallback' | 'reviewed_full_image_override'

/** The crop's box in the upright photo's pixels [left, top, right, bottom] (oriented_tight_crop.tight_integer_bbox). */
export function cropBox(selected: Mask[], orientation: number, W: number, H: number, override = false): { box: [number, number, number, number]; status: CropStatus } {
  if (override) return { box: [0, 0, W, H], status: 'reviewed_full_image_override' }
  if (!selected.length) {
    const q = (1 - 90 / 100) / 2
    return { box: [Math.max(0, Math.floor(q * W)), Math.max(0, Math.floor(q * H)), Math.min(W, Math.ceil((1 - q) * W)), Math.min(H, Math.ceil((1 - q) * H))], status: 'centre_crop_fallback' }
  }
  let x0 = Infinity, y0 = Infinity, x1 = -Infinity, y1 = -Infinity
  for (const m of selected)
    for (const [px, py] of [[m.bbox[0], m.bbox[1]], [m.bbox[2], m.bbox[1]], [m.bbox[2], m.bbox[3]], [m.bbox[0], m.bbox[3]]]) {
      const [u, v] = transformPoint(px, py, orientation)
      x0 = Math.min(x0, u); y0 = Math.min(y0, v); x1 = Math.max(x1, u); y1 = Math.max(y1, v)
    }
  const box: [number, number, number, number] = [Math.max(0, Math.floor(x0 * W)), Math.max(0, Math.floor(y0 * H)), Math.min(W, Math.ceil(x1 * W)), Math.min(H, Math.ceil(y1 * H))]
  if (box[0] >= box[2] || box[1] >= box[3]) return cropBox([], orientation, W, H)
  return { box, status: 'success' }
}

// ---------------------------------------------------------------- 3. the 224 px square

const PRECISION = 22

/** Pillow's resample coefficients (Resample.c precompute_coeffs + normalize_coeffs_8bpc), bicubic a = -0.5. */
function coeffs(inSize: number, outSize: number): { bounds: Int32Array; k: Int32Array; ksize: number } {
  const bicubic = (x: number) => {
    const a = -0.5
    x = Math.abs(x)
    if (x < 1) return ((a + 2) * x - (a + 3)) * x * x + 1
    if (x < 2) return (((x - 5) * x + 8) * x - 4) * a
    return 0
  }
  const scale = inSize / outSize
  const filterscale = Math.max(scale, 1)
  const support = 2 * filterscale
  const ksize = Math.ceil(support) * 2 + 1
  const bounds = new Int32Array(outSize * 2)
  const k = new Int32Array(outSize * ksize)
  const ww = new Float64Array(ksize)
  for (let xx = 0; xx < outSize; xx++) {
    const center = (xx + 0.5) * scale
    const ss = 1 / filterscale
    let xmin = Math.trunc(center - support + 0.5)
    if (xmin < 0) xmin = 0
    let xmax = Math.trunc(center + support + 0.5)
    if (xmax > inSize) xmax = inSize
    xmax -= xmin
    let total = 0
    for (let x = 0; x < xmax; x++) { const w = bicubic((x + xmin - center + 0.5) * ss); ww[x] = w; total += w }
    for (let x = 0; x < ksize; x++) {
      const w = x < xmax && total !== 0 ? ww[x] / total : 0
      k[xx * ksize + x] = w < 0 ? Math.trunc(-0.5 + w * (1 << PRECISION)) : Math.trunc(0.5 + w * (1 << PRECISION))
    }
    bounds[xx * 2] = xmin; bounds[xx * 2 + 1] = xmax
  }
  return { bounds, k, ksize }
}

const clip8 = (ss: number) => { const v = Math.floor(ss / (1 << PRECISION)); return v < 0 ? 0 : v > 255 ? 255 : v }

/**
 * oriented_tight_crop.crop_and_letterbox: the box cut from the upright photo, centred on a grey (128) square,
 * resized to `size` with Pillow's BICUBIC (horizontal pass, then vertical, 8-bit between them). RGB bytes.
 */
export function letterboxCrop(img: Rgba, box: [number, number, number, number], size = 224): Uint8Array {
  const cw = box[2] - box[0], ch = box[3] - box[1], edge = Math.max(cw, ch)
  const ox = Math.floor((edge - cw) / 2), oy = Math.floor((edge - ch) / 2)
  // pixel of the square: grey outside the crop
  const px = (x: number, y: number, c: number) => {
    const sx = x - ox, sy = y - oy
    if (sx < 0 || sy < 0 || sx >= cw || sy >= ch) return 128
    return img.data[((sy + box[1]) * img.width + sx + box[0]) * 4 + c]
  }
  if (edge === size) {
    const out = new Uint8Array(size * size * 3)
    for (let y = 0; y < size; y++) for (let x = 0; x < size; x++) for (let c = 0; c < 3; c++) out[(y * size + x) * 3 + c] = px(x, y, c)
    return out
  }
  const h = coeffs(edge, size), v = coeffs(edge, size)
  // rows the vertical pass needs (Pillow's ybox)
  const ymin = v.bounds[0], ymax = v.bounds[(size - 1) * 2] + v.bounds[(size - 1) * 2 + 1]
  const rows = ymax - ymin
  const tmp = new Uint8Array(rows * size * 3)
  for (let yy = 0; yy < rows; yy++) {
    const y = yy + ymin
    for (let xx = 0; xx < size; xx++) {
      const xmin = h.bounds[xx * 2], xn = h.bounds[xx * 2 + 1], kk = xx * h.ksize
      for (let c = 0; c < 3; c++) {
        let ss = 1 << (PRECISION - 1)
        for (let x = 0; x < xn; x++) ss += px(x + xmin, y, c) * h.k[kk + x]
        tmp[(yy * size + xx) * 3 + c] = clip8(ss)
      }
    }
  }
  const out = new Uint8Array(size * size * 3)
  for (let yy = 0; yy < size; yy++) {
    const y0 = v.bounds[yy * 2] - ymin, yn = v.bounds[yy * 2 + 1], kk = yy * v.ksize
    for (let xx = 0; xx < size; xx++)
      for (let c = 0; c < 3; c++) {
        let ss = 1 << (PRECISION - 1)
        for (let y = 0; y < yn; y++) ss += tmp[((y + y0) * size + xx) * 3 + c] * v.k[kk + y]
        out[(yy * size + xx) * 3 + c] = clip8(ss)
      }
  }
  return out
}

const MEAN = [0.48145466, 0.4578275, 0.40821073]
const STD = [0.26862954, 0.26130258, 0.27577711]

/** The encoder's input (worker.py feature): RGB bytes / 255, the CLIP mean and std, CHW. */
export function encoderInput(rgb: Uint8Array, size = 224): Float32Array {
  const plane = size * size, x = new Float32Array(3 * plane)
  for (let i = 0; i < plane; i++) for (let c = 0; c < 3; c++) x[c * plane + i] = (rgb[i * 3 + c] / 255 - MEAN[c]) / STD[c]
  return x
}

/**
 * torch F.interpolate(mode='bilinear', align_corners=False, antialias=False) of a CHW image: half-pixel centres,
 * the source position clamped at 0, the next pixel clamped at the edge, no filtering when shrinking.
 */
export function resizeBilinear(x: Float32Array, channels: number, inH: number, inW: number, outH: number, outW: number): Float32Array {
  const out = new Float32Array(channels * outH * outW)
  const axis = (inSize: number, outSize: number) => {
    const i0 = new Int32Array(outSize), i1 = new Int32Array(outSize), w = new Float32Array(outSize)
    const scale = inSize / outSize
    for (let o = 0; o < outSize; o++) {
      const src = Math.max((o + 0.5) * scale - 0.5, 0)
      const a = Math.min(Math.floor(src), inSize - 1)
      i0[o] = a; i1[o] = Math.min(a + 1, inSize - 1); w[o] = src - a
    }
    return { i0, i1, w }
  }
  const ys = axis(inH, outH), xs = axis(inW, outW)
  for (let c = 0; c < channels; c++) {
    const src = c * inH * inW, dst = c * outH * outW
    for (let y = 0; y < outH; y++) {
      const r0 = src + ys.i0[y] * inW, r1 = src + ys.i1[y] * inW, wy = ys.w[y]
      for (let xx = 0; xx < outW; xx++) {
        const a = xs.i0[xx], b = xs.i1[xx], wx = xs.w[xx]
        const top = x[r0 + a] * (1 - wx) + x[r0 + b] * wx
        const bot = x[r1 + a] * (1 - wx) + x[r1 + b] * wx
        out[dst + y * outW + xx] = top * (1 - wy) + bot * wy
      }
    }
  }
  return out
}

/** The small model's input: the same 224 px crop, the CLIP mean and std, resized to 192 (bilinear, no antialias). */
export const STUDENT_SIZE = 192
export function studentInput(rgb: Uint8Array, size = 224, out = STUDENT_SIZE): Float32Array {
  return resizeBilinear(encoderInput(rgb, size), 3, size, size, out, out)
}

// ---------------------------------------------------------------- the two models of the page

/** «modelo completo» (BioCLIP 2.5-H int8) or «modelo pequeño» (the distilled ViT-S, server/offline-id.mjs STUDENT). */
export type ModelKind = 'full' | 'small'
/** `sex`: the team's experimental sex model (lib/sexModel.ts), not part of either identifier model. */
export type FileRole = 'detector' | 'encoder' | 'student' | 'head' | 'head-meta' | 'sex'

/** The files each model needs: the detector and the head serve both, each has its own encoder. */
export const MODEL_ROLES: Record<ModelKind, FileRole[]> = {
  full: ['detector', 'encoder', 'head-meta', 'head'],
  small: ['detector', 'student', 'head-meta', 'head'],
}
export const ENCODER_ROLE: Record<ModelKind, FileRole> = { full: 'encoder', small: 'student' }
export const MODEL_LABEL: Record<ModelKind, string> = { full: 'modelo completo', small: 'modelo pequeño' }

/** A stored file's role: as stored, else from its name (stored.json written before the small model had none). */
export function roleOf(name: string, role?: string | null): FileRole {
  if (role) return role as FileRole
  if (name.startsWith('wing_seg')) return 'detector'
  if (name.startsWith('student')) return 'student'
  if (name.startsWith('sex_')) return 'sex'
  if (name.startsWith('bioclip')) return 'encoder'
  return name.endsWith('.json') ? 'head-meta' : 'head'
}

/**
 * `parts`: the SHA-256 of each 32 MB part, when the source said (Hugging Face's list, the server's parts manifest).
 * `version` and `sha256`: the installed version and the whole file's SHA-256 (the list's, checked on the phone before
 * the file was taken); files kept before versions existed have neither. `slot`: the file's parts are kept under
 * partKey(name, i, slot) (a newer version of a file with the same name is downloaded beside the old one, so the old
 * one works until the new one is checked). `calibration`: the sex model's cutoff, band and display (lib/sexModel.ts).
 */
export interface StoredFile { etag: string; bytes: number; role?: string; parts?: string[]; version?: string; sha256?: string; slot?: string; calibration?: unknown }
/** `version` and `sha256`: the model version the server (or the public list) offers, and the file's SHA-256. */
export interface ListedFile { name: string; etag: string; bytes: number; role: string; parts?: string[]; version?: string; sha256?: string }

/** The cache key (relative to the page's folder) of part `i` of a stored model file. */
export const partKey = (name: string, i: number, slot?: string) => `models/${name}?part=${i}${slot ? `&v=${slot}` : ''}`

/**
 * Whether the stored file is the listed one: the same ETag (the same source), or the same part hashes (a model
 * downloaded from the server is the one on Hugging Face, and the other way round).
 */
export function sameFile(stored: StoredFile | undefined, listed: ListedFile): boolean {
  if (!stored) return false
  if (stored.etag === listed.etag) return true
  const a = stored.parts, b = listed.parts
  return !!a?.length && !!b?.length && stored.bytes === listed.bytes && a.length === b.length && a.every((h, i) => h === b[i])
}

/** The stored file of a role (its name), or null. */
export function storedName(files: Record<string, StoredFile>, role: FileRole): string | null {
  return Object.keys(files).find(n => roleOf(n, files[n].role) === role) ?? null
}

/**
 * Whether a model can run from what is stored: every file it needs, and when the server's list is known (online),
 * the very versions listed (a model missing from the list is not ready). Offline: the files stored.
 */
export function modelReady(kind: ModelKind, stored: Record<string, StoredFile> | null, listed: ListedFile[] | null): boolean {
  return rolesReady(MODEL_ROLES[kind], stored, listed)
}

/** modelReady for any set of roles (the sex estimate: the detector and the sex model). */
export function rolesReady(roles: FileRole[], stored: Record<string, StoredFile> | null, listed: ListedFile[] | null): boolean {
  if (!stored) return false
  return roles.every(role => {
    if (!listed) return storedName(stored, role) != null
    const f = listed.find(l => l.role === role)
    return !!f && sameFile(stored[f.name], f)
  })
}

/**
 * Whether the stored file IS the listed version: the same SHA-256 when both know it. A file kept before versions
 * existed (no SHA-256 stored) is an old version, unless it is provably the listed bytes (sameFile: the same ETag, or
 * the same part hashes); then it only lacks the label, and adoptVersions records it without a download.
 */
export function isCurrent(stored: StoredFile | undefined, listed: ListedFile): boolean {
  if (!stored) return false
  if (stored.sha256 && listed.sha256) return stored.sha256 === listed.sha256 && stored.bytes === listed.bytes
  return sameFile(stored, listed)
}

/**
 * Whether files can run from what is stored: one file of each role. The version does not matter: an older one keeps
 * working until its update is downloaded and checked (offlineEngine's download swaps then).
 */
export function rolesStored(roles: FileRole[], stored: Record<string, StoredFile> | null): boolean {
  return !!stored && roles.every(role => storedName(stored, role) != null)
}

/**
 * What the server offers for these roles against the phone: 'none' (a role not listed: nothing to offer), 'missing'
 * (a role not on the phone: a first download), 'update' (on the phone, but a listed file is another version),
 * 'current'. `files`: the listed files not current on the phone (what a download fetches).
 */
export function offerFor(roles: FileRole[], stored: Record<string, StoredFile> | null, listed: ListedFile[]): { state: 'none' | 'missing' | 'update' | 'current'; files: ListedFile[] } {
  const wanted = roles.map(role => listed.find(l => l.role === role))
  if (wanted.some(f => !f)) return { state: 'none', files: [] }
  const need = (wanted as ListedFile[]).filter(f => !isCurrent(stored?.[f.name], f))
  if (!rolesStored(roles, stored)) return { state: 'missing', files: need }
  return { state: need.length ? 'update' : 'current', files: need }
}

/**
 * Files kept before versions existed (no `sha256`) that are provably the listed bytes (sameFile): the listed version,
 * SHA-256 and part hashes are recorded on them (`extra` adds role-specific fields, the sex model's calibration).
 * Returns whether anything changed. Pure: the caller stores the result.
 */
export function adoptVersions(stored: Record<string, StoredFile>, listed: ListedFile[], extra: (f: ListedFile) => Partial<StoredFile> = () => ({})): boolean {
  let changed = false
  for (const f of listed) {
    const have = stored[f.name]
    if (!have || have.sha256 || !sameFile(have, f)) continue
    const before = JSON.stringify(have)
    if (f.sha256) have.sha256 = f.sha256
    if (f.version) have.version = f.version
    if (f.parts?.length && !have.parts && have.bytes === f.bytes) have.parts = f.parts
    if (f.sha256) Object.assign(have, extra(f))
    changed ||= JSON.stringify(have) !== before
  }
  return changed
}

/** The model to identify with: the one asked for when it is ready, else the one that is, else none. */
export function chooseModel(wanted: ModelKind | null, ready: Record<ModelKind, boolean>): ModelKind | null {
  if (wanted && ready[wanted]) return wanted
  return ready.full ? 'full' : ready.small ? 'small' : null
}

// ---------------------------------------------------------------- 4. the head

export interface HeadMeta {
  schema: string
  head_sha256: string
  weights: string
  rows: number
  dim: number
  K: number
  s: number
  ln_eps: number
  ln_w: number[]
  ln_b: number[]
  species: string[]
  leaf2sp: number[]
  /** Schema 2 (ithomiini-offline-head/2): each leaf's name, and "1" where the leaf is a subspecies (worker.py is_ssp). */
  leaves?: string[]
  is_ssp?: string
  geo: string | null
  full_photo_overrides: string[]
}

/* Schemas read: ithomiini-offline-head/1 (no leaf names: no subspecies shown) and /2, which adds them. */

/** `ssp`: per leaf 1 when it is a subspecies; null with a schema 1 head (a phone that kept the older file). */
export interface Head { meta: HeadMeta; w: Uint16Array; leaf2sp: Int32Array; geo: Uint8Array | null; ssp: Uint8Array | null }

let half: Float32Array | null = null
/** float16 bits -> float32, for every 16-bit value */
function halfTable(): Float32Array {
  if (half) return half
  half = new Float32Array(65536)
  for (let h = 0; h < 65536; h++) {
    const s = h & 0x8000 ? -1 : 1, e = (h >> 10) & 0x1f, f = h & 0x3ff
    half[h] = e === 0 ? s * 2 ** -14 * (f / 1024) : e === 31 ? (f ? NaN : s * Infinity) : s * 2 ** (e - 15) * (1 + f / 1024)
  }
  return half
}

export function makeHead(meta: HeadMeta, weights: ArrayBuffer): Head {
  if (weights.byteLength !== meta.rows * meta.dim * 2) throw new Error('head weights: wrong size')
  if (meta.K !== 1) throw new Error('head: K > 1 not supported here')
  const geo = meta.geo ? Uint8Array.from(meta.geo, c => parseInt(c, 16)) : null
  const ssp = meta.leaves?.length === meta.rows && meta.is_ssp?.length === meta.rows ? Uint8Array.from(meta.is_ssp, c => (c === '1' ? 1 : 0)) : null
  return { meta, w: new Uint16Array(weights), leaf2sp: Int32Array.from(meta.leaf2sp), geo, ssp }
}

export type Side = 'East' | 'West' | null

/**
 * worker.py tops (species): softmax of the cosine head, the geographic prior when a side of the Andes is
 * given (names off that side, or not recorded in Ecuador, weigh 2 %), summed per species, the first `top`.
 */
export function speciesTop(head: Head, feature: Float32Array, side: Side = null, top = 5): [string, number][] {
  const agg = speciesScores(head, feature, side)
  const order = Array.from(agg.keys()).sort((a, b) => agg[b] - agg[a] || a - b)
  return order.slice(0, top).map(j => [head.meta.species[j], Math.round(agg[j] * 1e4) / 1e4])
}

/** Every species' probability (the order of head.meta.species), as speciesTop sums them: the genus totals come from it. */
export function speciesScores(head: Head, feature: Float32Array, side: Side = null): Float64Array {
  return sumSpecies(head, leafScores(head, feature, side))
}

/** The leaves' probabilities summed per species. */
export function sumSpecies(head: Head, p: Float64Array): Float64Array {
  const agg = new Float64Array(head.meta.species.length)
  for (let r = 0; r < p.length; r++) agg[head.leaf2sp[r]] += p[r]
  return agg
}

/**
 * worker.py tops (subspecies): the subspecies leaves in probability order (after the prior), the first `top`, as
 * [name, p]. Empty with a schema 1 head.
 */
export function subspeciesTop(head: Head, p: Float64Array, top = 5): [string, number][] {
  const { ssp, meta } = head
  if (!ssp || !meta.leaves) return []
  const order = Array.from(p.keys()).filter(i => ssp[i]).sort((a, b) => p[b] - p[a] || a - b)
  return order.slice(0, top).map(i => [meta.leaves![i], Math.round(p[i] * 1e4) / 1e4])
}

/** Per species (its index in head.meta.species): its subspecies leaves, most probable first. Empty with schema 1. */
export function subspeciesBySpecies(head: Head, p: Float64Array, species: number[]): Map<number, { name: string; p: number }[]> {
  const out = new Map<number, { name: string; p: number }[]>()
  const { ssp, meta } = head
  if (!ssp || !meta.leaves) return out
  const want = new Set(species)
  for (let r = 0; r < p.length; r++) {
    const s = head.leaf2sp[r]
    if (!ssp[r] || !want.has(s)) continue
    const list = out.get(s) ?? []
    list.push({ name: meta.leaves[r], p: p[r] })
    out.set(s, list)
  }
  for (const list of out.values()) list.sort((a, b) => b.p - a.p)
  return out
}

/**
 * Every leaf's probability (worker.py tops): softmax of the cosine head, then the geographic prior when a side of
 * the Andes is given (names off that side, or not recorded in Ecuador, weigh 2 %), normalised again.
 */
export function leafScores(head: Head, feature: Float32Array, side: Side = null): Float64Array {
  const { meta, w, geo } = head
  const d = meta.dim, n = meta.rows
  let norm = 0
  for (let i = 0; i < d; i++) norm += feature[i] * feature[i]
  norm = Math.sqrt(norm) || 1
  const f = new Float64Array(d)
  let mean = 0
  for (let i = 0; i < d; i++) { f[i] = feature[i] / norm; mean += f[i] }
  mean /= d
  let v = 0
  for (let i = 0; i < d; i++) v += (f[i] - mean) ** 2
  const inv = 1 / Math.sqrt(v / d + meta.ln_eps)
  const x = new Float32Array(d)
  let xn = 0
  for (let i = 0; i < d; i++) { x[i] = (f[i] - mean) * inv * meta.ln_w[i] + meta.ln_b[i]; xn += x[i] * x[i] }
  xn = Math.sqrt(xn) || 1
  for (let i = 0; i < d; i++) x[i] /= xn
  const tab = halfTable()
  const logits = new Float64Array(n)
  let max = -Infinity
  for (let r = 0; r < n; r++) {
    let s = 0
    const o = r * d
    for (let i = 0; i < d; i++) s += tab[w[o + i]] * x[i]
    const c = Math.min(Math.max(s, -1 + 1e-6), 1 - 1e-6) * meta.s
    logits[r] = c
    if (c > max) max = c
  }
  let total = 0
  for (let r = 0; r < n; r++) { logits[r] = Math.exp(logits[r] - max); total += logits[r] }
  const p = logits
  for (let r = 0; r < n; r++) p[r] /= total
  if (geo && (side === 'East' || side === 'West')) {
    const sideBit = side === 'East' ? 4 : 8
    let sum = 0
    for (let r = 0; r < n; r++) {
      const g = geo[r]
      if (g & 1 && (!(g & sideBit) || !(g & 2))) p[r] *= 0.02
      sum += p[r]
    }
    if (sum > 0) for (let r = 0; r < n; r++) p[r] /= sum
  }
  return p
}

/**
 * The sides of the Andes the head records a species on in Ecuador (its leaves' geo bits: 1 known, 2 in Ecuador,
 * 4 East, 8 West), by name; species without a record are left out.
 */
export function speciesSides(head: Head): Map<string, { East: boolean; West: boolean }> {
  const out = new Map<string, { East: boolean; West: boolean }>()
  if (!head.geo) return out
  for (let r = 0; r < head.geo.length; r++) {
    const g = head.geo[r]
    if (!(g & 1) || !(g & 2)) continue
    const name = head.meta.species[head.leaf2sp[r]]
    const e = out.get(name) ?? { East: false, West: false }
    if (g & 4) e.East = true
    if (g & 8) e.West = true
    out.set(name, e)
  }
  return out
}
