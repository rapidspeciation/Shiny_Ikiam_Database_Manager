// Tube racks read from photos of their bottoms (docs/assistant-tools-rack-reader.md): every tube's Data Matrix
// (FluidX-style 2D codes, IDs like FS50849233), the rack's own code, and the 8 × 12 grid they sit in, from one
// or more photos merged by tube ID.
//
// Pixels come from ffmpeg (on the server already, as for the photo copies); the codes from zxing-cpp built to
// WebAssembly (zxing-wasm 3.1.6, vendored in server/vendor/zxing-wasm, MIT; zxing-cpp Apache 2.0). A whole
// photo decodes almost nothing (the codes are ~40 px): each tube's bright square is found, cut out, enlarged and
// decoded alone, which reads 92-93 of 96 on a phone photo of a full rack.
//
// Positions. A photo of the bottom is a mirror of the rack seen from above, so a photo position maps to A1..H12
// in one of two ways. Which one is decided only by evidence (`orient`): a tube the person says is in A1 (or, when
// that one was not read, in A12, H1 or H12), the
// rack's code with the corner the person confirmed it sits by, or, for a rack of pre-loaded sequential tubes, the
// IDs themselves (+1 from row A to B and +8 from a column to the next, as the Sanger-scanned FluidX racks of the
// workbook's tube locations show). Without any of them the positions stay photo positions.

import { execFile } from 'node:child_process';
import { readFileSync } from 'node:fs';
import { promisify } from 'node:util';
import { prepareZXingModule, readBarcodes } from './vendor/zxing-wasm/es/reader/index.js';

const run = promisify(execFile);
const WASM = new URL('./vendor/zxing-wasm/reader/zxing_reader.wasm', import.meta.url);
let ready = null;
function zxing() {
  ready ??= prepareZXingModule({ overrides: { wasmBinary: readFileSync(WASM) }, fireImmediately: true });
  return ready;
}

/**
 * Rack IDs (the prefixes of the workbook's Rack_ID and Tube_N_rack columns: FluidX FK, FH, FE and Sanger's SA;
 * no tube column holds them). A code like this is the rack's, never a tube's.
 */
export const RACK_ID = /^(?:FK|FH|FE|SA)\d{8}$/;

export const ROWS = 8;
export const COLS = 12;
export const ROW_LETTERS = 'ABCDEFGH';
/** Longest side the photos are read at (a 12 MP phone photo is kept whole). */
const MAX_SIDE = 4096;

// ---------------------------------------------------------------- pixels

/** Grey images from PGM bytes (ffmpeg's image2pipe; several frames back to back). */
export function parsePgm(buf) {
  const out = [];
  let at = 0;
  while (at < buf.length) {
    const head = buf.subarray(at, at + 64).toString('latin1');
    const m = head.match(/^P5\s+(\d+)\s+(\d+)\s+(\d+)\s/);
    if (!m) break;
    const width = +m[1];
    const height = +m[2];
    const start = at + m[0].length;
    out.push({ width, height, data: buf.subarray(start, start + width * height) });
    at = start + width * height;
  }
  return out;
}

const SCALE = `scale='if(gt(iw,ih),min(${MAX_SIDE},iw),-2)':'if(gt(iw,ih),-2,min(${MAX_SIDE},ih))'`;

/** A photo's pixels, grey, its longest side at most MAX_SIDE. */
export async function loadPhoto(path) {
  const { stdout } = await run('ffmpeg', ['-v', 'error', '-i', path, '-frames:v', '1', '-vf', SCALE, '-f', 'image2pipe', '-vcodec', 'pgm', '-'], {
    encoding: 'buffer',
    maxBuffer: 1 << 26,
  });
  const [img] = parsePgm(stdout);
  if (!img) throw new Error('not an image ffmpeg can read');
  return img;
}

function boxBlur(src, w, h, r) {
  const tmp = new Float32Array(w * h);
  const out = new Float32Array(w * h);
  const n = 2 * r + 1;
  for (let y = 0; y < h; y++) {
    const row = y * w;
    let acc = 0;
    for (let x = -r; x <= r; x++) acc += src[row + Math.min(w - 1, Math.max(0, x))];
    for (let x = 0; x < w; x++) {
      tmp[row + x] = acc / n;
      acc += src[row + Math.min(w - 1, x + r + 1)] - src[row + Math.max(0, x - r)];
    }
  }
  for (let x = 0; x < w; x++) {
    let acc = 0;
    for (let y = -r; y <= r; y++) acc += tmp[Math.min(h - 1, Math.max(0, y)) * w + x];
    for (let y = 0; y < h; y++) {
      out[y * w + x] = acc / n;
      acc += tmp[Math.min(h - 1, y + r + 1) * w + x] - tmp[Math.max(0, y - r) * w + x];
    }
  }
  return out;
}

function otsu(values) {
  const hist = new Float64Array(256);
  for (const v of values) hist[Math.max(0, Math.min(255, v | 0))]++;
  let sum = 0;
  for (let i = 0; i < 256; i++) sum += i * hist[i];
  let sb = 0;
  let wb = 0;
  let best = -1;
  let th = 128;
  for (let t = 0; t < 256; t++) {
    wb += hist[t];
    if (!wb) continue;
    const wf = values.length - wb;
    if (!wf) break;
    sb += t * hist[t];
    const between = wb * wf * (sb / wb - (sum - sb) / wf) ** 2;
    if (between > best) {
      best = between;
      th = t;
    }
  }
  return th;
}

function components(mask, w, h) {
  const label = new Uint8Array(w * h);
  const out = [];
  const stack = [];
  for (let i = 0; i < w * h; i++) {
    if (!mask[i] || label[i]) continue;
    label[i] = 1;
    stack.push(i);
    let n = 0;
    let sx = 0;
    let sy = 0;
    let x0 = w;
    let y0 = h;
    let x1 = 0;
    let y1 = 0;
    while (stack.length) {
      const j = stack.pop();
      const x = j % w;
      const y = (j - x) / w;
      n++;
      sx += x;
      sy += y;
      if (x < x0) x0 = x;
      if (x > x1) x1 = x;
      if (y < y0) y0 = y;
      if (y > y1) y1 = y;
      if (x > 0 && mask[j - 1] && !label[j - 1]) (label[j - 1] = 1), stack.push(j - 1);
      if (x < w - 1 && mask[j + 1] && !label[j + 1]) (label[j + 1] = 1), stack.push(j + 1);
      if (y > 0 && mask[j - w] && !label[j - w]) (label[j - w] = 1), stack.push(j - w);
      if (y < h - 1 && mask[j + w] && !label[j + w]) (label[j + w] = 1), stack.push(j + w);
    }
    out.push({ n, x: sx / n, y: sy / n, bw: x1 - x0 + 1, bh: y1 - y0 + 1 });
  }
  return out;
}

/**
 * The bright squares of the tubes' codes: blobs of a blurred, Otsu-thresholded copy about `side` px across.
 * Returns [{ x, y, size }] in the image's pixels.
 */
export function findSquares(img, side = 1000) {
  const { width: W, height: H } = img;
  const f = Math.max(1, Math.round(Math.max(W, H) / side));
  const w = Math.floor(W / f);
  const h = Math.floor(H / f);
  const small = new Float32Array(w * h);
  for (let y = 0; y < h; y++)
    for (let x = 0; x < w; x++) {
      let s = 0;
      for (let dy = 0; dy < f; dy++) for (let dx = 0; dx < f; dx++) s += img.data[(y * f + dy) * W + x * f + dx];
      small[y * w + x] = s / (f * f);
    }
  let blurred = small;
  for (let i = 0; i < 3; i++) blurred = boxBlur(blurred, w, h, 2);
  // Brighter than their surroundings (a bright bench around the rack must not count): the blurred photo less
  // its local mean over a few tubes, thresholded by Otsu.
  const L = Math.max(w, h);
  const around = boxBlur(blurred, w, h, Math.max(4, Math.round(L / 30)));
  const diff = new Float32Array(w * h);
  for (let i = 0; i < w * h; i++) diff[i] = Math.max(0, Math.min(255, 64 + blurred[i] - around[i]));
  const th = Math.max(otsu(diff), 80);
  const mask = new Uint8Array(w * h);
  for (let i = 0; i < w * h; i++) mask[i] = diff[i] > th ? 1 : 0;
  let found = components(mask, w, h).filter(
    c => c.n > (L / 150) ** 2 && c.n < (L / 15) ** 2 && c.bw < 2 * c.bh && c.bh < 2 * c.bw && c.n / (c.bw * c.bh) > 0.45,
  );
  if (!found.length) return [];
  const median = found.map(c => c.n).sort((a, b) => a - b)[found.length >> 1];
  found = found.filter(c => c.n > median * 0.3 && c.n < median * 3);
  return found.map(c => ({ x: c.x * f + f / 2, y: c.y * f + f / 2, size: Math.sqrt(c.n) * f }));
}

// ---------------------------------------------------------------- decoding

function crop(img, cx, cy, half) {
  const s = Math.max(8, 2 * Math.round(half));
  const x0 = Math.round(cx - s / 2);
  const y0 = Math.round(cy - s / 2);
  const data = new Uint8Array(s * s);
  for (let y = 0; y < s; y++) {
    const Y = Math.min(img.height - 1, Math.max(0, y0 + y)) * img.width;
    for (let x = 0; x < s; x++) data[y * s + x] = img.data[Y + Math.min(img.width - 1, Math.max(0, x0 + x))];
  }
  return { s, data, x0, y0 };
}

function stretch(c) {
  const sorted = Uint8Array.from(c.data).sort();
  const lo = sorted[Math.floor(sorted.length * 0.05)];
  const hi = sorted[Math.floor(sorted.length * 0.98)];
  const k = 255 / Math.max(1, hi - lo);
  const data = new Uint8Array(c.data.length);
  for (let i = 0; i < data.length; i++) data[i] = Math.max(0, Math.min(255, (c.data[i] - lo) * k));
  return { ...c, data };
}

/** Bilinear enlargement to S × S, as the RGBA pixels zxing-wasm takes. */
function enlarge(c, S) {
  const out = new Uint8ClampedArray(S * S * 4);
  const k = c.s / S;
  const last = c.s - 1.001;
  for (let y = 0; y < S; y++) {
    const fy = Math.min(last, Math.max(0, (y + 0.5) * k - 0.5));
    const y0 = fy | 0;
    const ay = fy - y0;
    for (let x = 0; x < S; x++) {
      const fx = Math.min(last, Math.max(0, (x + 0.5) * k - 0.5));
      const x0 = fx | 0;
      const ax = fx - x0;
      const i0 = y0 * c.s + x0;
      const v = (c.data[i0] * (1 - ax) + c.data[i0 + 1] * ax) * (1 - ay) + (c.data[i0 + c.s] * (1 - ax) + c.data[i0 + c.s + 1] * ax) * ay;
      const o = (y * S + x) * 4;
      out[o] = out[o + 1] = out[o + 2] = v;
      out[o + 3] = 255;
    }
  }
  return { width: S, height: S, data: out };
}

const OPTIONS = { formats: ['DataMatrix'], tryHarder: true, tryRotate: true, tryInvert: true, tryDownscale: false };

/**
 * Data Matrix codes in a square around (cx, cy), the square `half`·2 px wide, enlarged to each size in turn,
 * plain then contrast-stretched, with both of zxing's binarizers, until one reads. `many`: all codes in the
 * square (the rack's code beside tubes), with their centres in the image's pixels.
 */
export async function decodeAround(img, cx, cy, half, { sizes = [300, 450, 200, 600], halves = [0.8, 1, 1.4], many = false } = {}) {
  await zxing();
  for (const hk of halves) {
    const plain = crop(img, cx, cy, half * hk);
    for (const pre of [plain, null]) {
      const c = pre ?? stretch(plain);
      for (const S of sizes)
        for (const binarizer of ['LocalAverage', 'GlobalHistogram']) {
          const found = await readBarcodes(enlarge(c, S), { ...OPTIONS, binarizer, maxNumberOfSymbols: many ? 8 : 1 });
          const valid = found.filter(r => r.isValid && r.text);
          if (!valid.length) continue;
          const k = c.s / S;
          return valid.map(r => {
            const p = r.position;
            const mx = (p.topLeft.x + p.topRight.x + p.bottomLeft.x + p.bottomRight.x) / 4;
            const my = (p.topLeft.y + p.topRight.y + p.bottomLeft.y + p.bottomRight.y) / 4;
            return { text: r.text.trim(), x: c.x0 + mx * k, y: c.y0 + my * k };
          });
        }
    }
  }
  return [];
}

// ---------------------------------------------------------------- the grid

const key = (i, j) => `${i},${j}`;
const unkey = k => k.split(',').map(Number);
/** A lattice point turned by q quarter turns (never mirrored). */
export const rotate = ([i, j], q) => [[i, j], [-j, i], [-i, -j], [j, -i]][((q % 4) + 4) % 4];

/** Least-squares homography taking the points `from` to `to` ([[x, y]]), normalised; null with fewer than 4. */
export function homography(from, to) {
  if (from.length < 4) return null;
  const norm = pts => {
    const mx = pts.reduce((s, p) => s + p[0], 0) / pts.length;
    const my = pts.reduce((s, p) => s + p[1], 0) / pts.length;
    const d = pts.reduce((s, p) => s + Math.hypot(p[0] - mx, p[1] - my), 0) / pts.length || 1;
    const k = Math.SQRT2 / d;
    return { k, mx, my, pts: pts.map(p => [(p[0] - mx) * k, (p[1] - my) * k]) };
  };
  const a = norm(from);
  const b = norm(to);
  // h33 = 1: 8 unknowns, normal equations.
  const M = Array.from({ length: 8 }, () => new Float64Array(9));
  const add = (row, rhs) => {
    for (let i = 0; i < 8; i++) {
      for (let j = 0; j < 8; j++) M[i][j] += row[i] * row[j];
      M[i][8] += row[i] * rhs;
    }
  };
  a.pts.forEach(([x, y], i) => {
    const [u, v] = b.pts[i];
    add([x, y, 1, 0, 0, 0, -u * x, -u * y], u);
    add([0, 0, 0, x, y, 1, -v * x, -v * y], v);
  });
  for (let c = 0; c < 8; c++) {
    let p = c;
    for (let r = c + 1; r < 8; r++) if (Math.abs(M[r][c]) > Math.abs(M[p][c])) p = r;
    if (Math.abs(M[p][c]) < 1e-12) return null;
    [M[c], M[p]] = [M[p], M[c]];
    for (let r = 0; r < 8; r++) {
      if (r === c) continue;
      const f = M[r][c] / M[c][c];
      for (let j = c; j < 9; j++) M[r][j] -= f * M[c][j];
    }
  }
  const h = M.map((row, i) => row[8] / row[i]);
  // Undo the normalisations: H = Tb⁻¹ · Hn · Ta.
  const Hn = [
    [h[0], h[1], h[2]],
    [h[3], h[4], h[5]],
    [h[6], h[7], 1],
  ];
  const Ta = [
    [a.k, 0, -a.k * a.mx],
    [0, a.k, -a.k * a.my],
    [0, 0, 1],
  ];
  const TbInv = [
    [1 / b.k, 0, b.mx],
    [0, 1 / b.k, b.my],
    [0, 0, 1],
  ];
  const mul = (P, Q) => P.map((r, i) => Q[0].map((_, j) => r.reduce((s, _v, k) => s + P[i][k] * Q[k][j], 0)));
  return mul(mul(TbInv, Hn), Ta);
}

export const apply = (H, [x, y]) => {
  const w = H[2][0] * x + H[2][1] * y + H[2][2];
  return [(H[0][0] * x + H[0][1] * y + H[0][2]) / w, (H[1][0] * x + H[1][1] * y + H[1][2]) / w];
};

export function invert(H) {
  const [[a, b, c], [d, e, f], [g, h, i]] = H;
  const A = e * i - f * h;
  const B = -(d * i - f * g);
  const C = d * h - e * g;
  const det = a * A + b * B + c * C;
  return [
    [A / det, -(b * i - c * h) / det, (b * f - c * e) / det],
    [B / det, (a * i - c * g) / det, -(a * f - c * d) / det],
    [C / det, -(a * h - b * g) / det, (a * e - b * d) / det],
  ];
}

/**
 * Lattice coordinates for the squares: neighbours one pitch apart along the grid's two directions, walked from
 * the square nearest the middle, then a homography from lattice to image fitted on the decoded ones and every
 * square put on its nearest node. The frame is turned so its first axis is the longer one and is never a mirror
 * of the photo (first axis to the right means second axis down). Returns { nodes: Map("i,j" → square), H } or null.
 */
export function fitLattice(squares) {
  const read = squares.filter(s => s.text);
  if (read.length < 6) return null;
  const near = p =>
    squares
      .filter(q => q !== p)
      .map(q => ({ q, dx: q.x - p.x, dy: q.y - p.y, d: Math.hypot(q.x - p.x, q.y - p.y) }))
      .sort((a, b) => a.d - b.d);
  const pitch = read.map(p => near(p)[0]?.d ?? Infinity).sort((a, b) => a - b)[read.length >> 1];
  // The grid's directions: the angles (mod 180°) of the steps of about one pitch between read squares.
  const angles = [];
  for (const p of read)
    for (const n of near(p).slice(0, 6)) if (n.q.text && n.d > 0.7 * pitch && n.d < 1.3 * pitch) angles.push(((Math.atan2(n.dy, n.dx) % Math.PI) + Math.PI) % Math.PI);
  if (angles.length < 4) return null;
  const dominant = list => {
    let best = null;
    for (const a of list) {
      const close = list.filter(b => Math.abs(((b - a + Math.PI * 1.5) % Math.PI) - Math.PI / 2) < 0.2);
      if (!best || close.length > best.close.length) best = { a, close };
    }
    // The mean of the close ones, wrapped around a.
    const m = best.close.reduce((s, b) => s + (((b - best.a + Math.PI * 1.5) % Math.PI) - Math.PI / 2), 0) / best.close.length;
    return best.a + m;
  };
  const a1 = dominant(angles);
  const a2 = dominant(angles.filter(b => Math.abs(((b - a1 + Math.PI * 1.5) % Math.PI) - Math.PI / 2) > 0.6));
  let u = [Math.cos(a1) * pitch, Math.sin(a1) * pitch];
  let v = [Math.cos(a2) * pitch, Math.sin(a2) * pitch];
  if (u[0] * v[1] - u[1] * v[0] < 0) v = [-v[0], -v[1]];
  const det = u[0] * v[1] - u[1] * v[0];
  const toUV = (dx, dy) => [(dx * v[1] - dy * v[0]) / det, (u[0] * dy - u[1] * dx) / det];
  // Walk: each step from a placed square to one about ±u or ±v away.
  const mid = read.reduce((m, p) => [m[0] + p.x / read.length, m[1] + p.y / read.length], [0, 0]);
  const start = read.reduce((b, p) => (Math.hypot(p.x - mid[0], p.y - mid[1]) < Math.hypot(b.x - mid[0], b.y - mid[1]) ? p : b));
  const placed = new Map([[start, [0, 0]]]);
  const queue = [start];
  while (queue.length) {
    const p = queue.shift();
    const [i, j] = placed.get(p);
    for (const n of near(p).slice(0, 12)) {
      if (placed.has(n.q) || n.d > 1.45 * pitch) continue;
      const [a, b] = toUV(n.dx, n.dy);
      const ra = Math.round(a);
      const rb = Math.round(b);
      if (Math.abs(ra) + Math.abs(rb) !== 1 || Math.abs(a - ra) > 0.25 || Math.abs(b - rb) > 0.25) continue;
      placed.set(n.q, [i + ra, j + rb]);
      queue.push(n.q);
    }
  }
  // Lattice → image, refitted twice with every square on its nearest node.
  let H = null;
  let nodes = new Map();
  for (let pass = 0; pass < 3; pass++) {
    const pairs = [...placed].filter(([p]) => p.text);
    H = homography(
      pairs.map(([, c]) => c),
      pairs.map(([p]) => [p.x, p.y]),
    );
    if (!H) return null;
    const Hi = invert(H);
    nodes = new Map();
    placed.clear();
    for (const p of squares) {
      const [a, b] = apply(Hi, [p.x, p.y]);
      const c = [Math.round(a), Math.round(b)];
      if (Math.hypot(a - c[0], b - c[1]) > 0.3) continue;
      const key = c.join(',');
      const had = nodes.get(key);
      if (had && (had.text || !p.text)) continue;
      nodes.set(key, p);
      placed.set(p, c);
    }
  }
  // The longer side first (12 columns), the frame not mirrored.
  const span = k => {
    const vals = [...placed.values()].map(c => c[k]);
    return Math.max(...vals) - Math.min(...vals);
  };
  // ...and pointing right in the photo (down when the rack stands upright in it).
  const quarter = span(1) > span(0) ? 1 : 0;
  const refit = q => {
    const turned = new Map();
    for (const [p, c] of placed) turned.set(rotate(c, q).join(','), p);
    const pairs = [...turned].filter(([, p]) => p.text).map(([k, p]) => [k.split(',').map(Number), [p.x, p.y]]);
    return { nodes: turned, H: homography(pairs.map(([c]) => c), pairs.map(([, xy]) => xy)) };
  };
  let fit = refit(quarter);
  if (!fit.H) return null;
  const [x0, y0] = apply(fit.H, [0, 0]);
  const [x1, y1] = apply(fit.H, [1, 0]);
  if (Math.abs(x1 - x0) >= Math.abs(y1 - y0) ? x1 < x0 : y1 < y0) fit = refit(quarter + 2);
  return fit.H ? { ...fit, pitch } : null;
}


/**
 * One photo read: its tubes' codes on a lattice, the rack's code if seen, and the photo's window of 8 × 12
 * when its extent is certain. `img`: grey pixels. Returns { tubes: Map("i,j" → id), seen: Set("i,j" with a code
 * square not read), rack: { text, at: [i, j] } | null, codes: number }.
 */
export async function readOne(img) {
  let squares = findSquares(img, 1000);
  if (squares.length < 20) squares = findSquares(img, 2000);
  for (const s of squares) {
    const [hit] = await decodeAround(img, s.x, s.y, s.size);
    s.text = hit?.text ?? null;
  }
  // A code shaped like a rack's ID is the rack's, wherever it sits (a photo of part of the rack).
  const rackSquare = squares.find(s => RACK_ID.test(s.text ?? ''));
  if (rackSquare) squares = squares.filter(s => s !== rackSquare);
  const lattice = fitLattice(squares);
  if (!lattice) {
    // Too few for a grid (a photo of one tube, or of a few): the codes read, without places; the whole photo too.
    // A tube's side barcode (Code 128, the same ID) is read too.
    const loose = new Set(squares.filter(s => s.text).map(s => s.text));
    await zxing();
    const rgba = new Uint8ClampedArray(img.width * img.height * 4);
    for (let i = 0; i < img.data.length; i++) rgba[4 * i] = rgba[4 * i + 1] = rgba[4 * i + 2] = img.data[i];
    const whole = { ...OPTIONS, formats: ['DataMatrix', 'Code128'], tryDownscale: true, maxNumberOfSymbols: 8 };
    for (const r of await readBarcodes({ width: img.width, height: img.height, data: rgba }, whole)) if (r.isValid && r.text) loose.add(r.text.trim());
    if (rackSquare) loose.add(rackSquare.text);
    return { tubes: new Map(), seen: new Set(), rack: null, loose: [...loose], codes: loose.size, lattice: false };
  }
  const { nodes, H, pitch } = lattice;
  // The rack's own code sits one pitch beyond a corner tube, so it can land on the lattice as a 13th column (or a
  // 9th row) holding only it: taken off the grid as the rack's code.
  let rack = null;
  for (let pass = 0; pass < 2 && !rackSquare; pass++) {
    const all = [...nodes.keys()].map(unkey);
    const lo0 = [Math.min(...all.map(c => c[0])), Math.min(...all.map(c => c[1]))];
    const hi0 = [Math.max(...all.map(c => c[0])), Math.max(...all.map(c => c[1]))];
    for (const [axis, at, inward, limit] of [
      [0, lo0[0], 1, COLS],
      [0, hi0[0], -1, COLS],
      [1, lo0[1], 1, ROWS],
      [1, hi0[1], -1, ROWS],
    ]) {
      if (hi0[axis] - lo0[axis] + 1 <= limit) continue;
      const line = [...nodes].filter(([k]) => unkey(k)[axis] === at);
      if (line.length !== 1) continue;
      const [k, p] = line[0];
      const c = unkey(k);
      const corner = [...c];
      corner[axis] += inward;
      nodes.delete(k);
      rack ??= { text: p.text, at: c, corner, x: p.x, y: p.y, size: p.size };
      break;
    }
  }
  const tubes = new Map();
  const seen = new Set();
  for (const [k, p] of nodes)
    if (p.text) tubes.set(k, p.text);
    else seen.add(k);
  // The nodes inside the tubes' extent not read: a second, harder look where the grid says the code is (a
  // square seen first; nodes with nothing seen, a hole or glare, only when few: each costs ~60 decodes).
  const coords = [...nodes.keys()].map(unkey);
  const lo = [Math.min(...coords.map(c => c[0])), Math.min(...coords.map(c => c[1]))];
  const hi = [Math.max(...coords.map(c => c[0])), Math.max(...coords.map(c => c[1]))];
  const sizes = [...nodes.values()].map(p => p.size).sort((a, b) => a - b);
  const side = sizes[sizes.length >> 1] || pitch * 0.36;
  const holes = [];
  for (let i = lo[0]; i <= hi[0]; i++) for (let j = lo[1]; j <= hi[1]; j++) if (!tubes.has(key(i, j)) && !seen.has(key(i, j))) holes.push(key(i, j));
  for (const k of [...seen, ...(holes.length <= 12 ? holes : [])]) {
    const [x, y] = apply(H, unkey(k));
    const [hit] = await decodeAround(img, x, y, side, { sizes: [250, 350, 500, 700], halves: [0.7, 0.9, 1.2, 1.6] });
    if (!hit) continue;
    seen.delete(k);
    // The rack's code read only now, on a node of its own beyond the tubes.
    if (RACK_ID.test(hit.text)) {
      const c = unkey(k);
      rack ??= { text: hit.text, at: c };
    } else tubes.set(k, hit.text);
  }
  if (rack && seen.has(key(...rack.at))) seen.delete(key(...rack.at));
  // The tubes' extent without the rack's code, and the corner tube the code is beside.
  const held = [...tubes.keys(), ...seen].map(unkey);
  if (held.length) {
    lo[0] = Math.min(...held.map(c => c[0]));
    lo[1] = Math.min(...held.map(c => c[1]));
    hi[0] = Math.max(...held.map(c => c[0]));
    hi[1] = Math.max(...held.map(c => c[1]));
  }
  const cornerOf = at => at.map((v, a) => Math.max(lo[a], Math.min(hi[a], Math.round(v))));
  if (rack) rack.corner = cornerOf(rack.at);
  // The rack's code: beyond a corner of the extent, one pitch out along either side.
  const ids = new Set(tubes.values());
  if (rackSquare) rack = { text: rackSquare.text, at: apply(invert(H), [rackSquare.x, rackSquare.y]), corner: cornerOf(apply(invert(H), [rackSquare.x, rackSquare.y])) };
  // The rack's code where its square is (printed on the rack, often less sharp than the tubes').
  if (rack && !rack.text) {
    // Its print is blurrier than the tubes': the square's centre nudged a little each way too.
    // Where the grid puts it first: its square's own centre is pulled off by the label it is printed on.
    const d = pitch * 0.08;
    const [gx, gy] = apply(H, rack.at);
    const tries = [[gx, gy], [rack.x, rack.y], ...[[d, d], [-d, -d], [d, -d], [-d, d]].map(([dx, dy]) => [gx + dx, gy + dy])];
    for (const [x, y] of tries) {
      const [hit] = await decodeAround(img, x, y, side, { sizes: [250, 350, 500, 700], halves: [0.7, 0.9, 1.2, 1.6] });
      if (hit && !ids.has(hit.text)) {
        rack.text = hit.text;
        break;
      }
    }
  }
  if (rack) {
    for (const k of ['x', 'y', 'size']) delete rack[k];
    if (!rack.text) rack = null;
  }
  // Not seen yet: one pitch out from each corner, along the long side first (where it was on the racks seen),
  // the spot nudged a little each way.
  const corners = [
    [lo[0], lo[1], -1, 0],
    [hi[0], hi[1], 1, 0],
    [hi[0], lo[1], 1, 0],
    [lo[0], hi[1], -1, 0],
    [lo[0], lo[1], 0, -1],
    [hi[0], hi[1], 0, 1],
    [hi[0], lo[1], 0, -1],
    [lo[0], hi[1], 0, 1],
  ];
  const d = 0.08;
  if (!rack)
    search: for (const [i, j, di, dj] of corners)
      for (const [ni, nj] of [[0, 0], [d, d], [-d, -d], [d, -d], [-d, d]]) {
        const at = [i + di * 0.95 + ni, j + dj * 0.95 + nj];
        const [x, y] = apply(H, at);
        if (x < 0 || y < 0 || x >= img.width || y >= img.height) continue;
        const [hit] = await decodeAround(img, x, y, side, { sizes: [250, 350, 500, 700], halves: [0.7, 0.9, 1.2, 1.6] });
        if (!hit || ids.has(hit.text)) continue;
        rack = { text: hit.text, at, corner: [i, j] };
        break search;
      }
  return { tubes, seen, rack, codes: tubes.size, lattice: true, extent: { lo, hi } };
}

// ---------------------------------------------------------------- photos merged


/**
 * The photos' lattices put on the first one's: for each photo, the quarter turns and shift that make the
 * most of its tube IDs land where the merged photos have them. A photo sharing fewer than 2 IDs is left out.
 */
export function alignReadings(readings) {
  const placed = [];
  const merged = new Map(); // id → "i,j" in the reference frame
  const order = readings.map((r, n) => ({ r, n })).sort((a, b) => b.r.tubes.size - a.r.tubes.size);
  for (const { r, n } of order) {
    if (!r.lattice || !r.tubes.size) {
      placed[n] = null;
      continue;
    }
    if (!merged.size) {
      placed[n] = { turn: 0, shift: [0, 0], agree: r.tubes.size };
    } else {
      let best = null;
      for (const turn of [0, 1, 2, 3]) {
        const votes = new Map();
        for (const [k, id] of r.tubes) {
          const there = merged.get(id);
          if (!there) continue;
          const c = rotate(unkey(k), turn);
          const [ti, tj] = unkey(there);
          const s = key(ti - c[0], tj - c[1]);
          votes.set(s, (votes.get(s) ?? 0) + 1);
        }
        for (const [s, agree] of votes) if (!best || agree > best.agree) best = { turn, shift: unkey(s), agree };
      }
      placed[n] = best && best.agree >= 2 ? best : null;
    }
    const p = placed[n];
    if (!p) continue;
    for (const [k, id] of r.tubes) if (!merged.has(id)) merged.set(id, key(...move(p, unkey(k))));
  }
  return placed;
}

export function move(p, c) {
  const t = rotate(c, p.turn);
  return [t[0] + p.shift[0], t[1] + p.shift[1]];
}

/** "R3C5": a photo position in the merged frame, rows from the top, 1-based. */
export const photoName = ([i, j]) => `R${j + 1}C${i + 1}`;

/**
 * The two ways a photo position (frame column c 0..11, row r 0..7) can be a well, the photo being a mirror:
 * 'm1' row A at the frame's top and column 1 at its right; 'm2' row A at the bottom and column 1 at the left.
 */
export function wellOf(mapping, c, r) {
  return mapping === 'm1' ? `${ROW_LETTERS[r]}${COLS - c}` : `${ROW_LETTERS[ROWS - 1 - r]}${c + 1}`;
}

export const WELLS = [...ROW_LETTERS].flatMap(l => Array.from({ length: COLS }, (_, k) => `${l}${k + 1}`));

/** Column-major order of a rack's filling: A1, B1 … H1, A2 … */
export const fillOrder = well => (Number(well.slice(1)) - 1) * ROWS + ROW_LETTERS.indexOf(well[0]);

const serial = id => {
  const m = /^([A-Z]{2})(\d{6,})$/.exec(id ?? '');
  return m ? { prefix: m[1], n: Number(m[2]) } : null;
};

/**
 * How well a mapping fits pre-loaded sequential tubes: of the neighbour pairs with IDs of one series, how many
 * go +1 from a row to the next (A→B) and +8 from a column to the next. Returns { pairs, fit }.
 */
export function sequenceFit(byWell) {
  let pairs = 0;
  let fit = 0;
  for (const [well, id] of byWell) {
    const a = serial(id);
    if (!a) continue;
    const r = ROW_LETTERS.indexOf(well[0]);
    const c = Number(well.slice(1));
    for (const [other, step] of [
      [r + 1 < ROWS ? `${ROW_LETTERS[r + 1]}${c}` : null, 1],
      [c + 1 <= COLS ? `${well[0]}${c + 1}` : null, ROWS],
    ]) {
      const b = other && serial(byWell.get(other));
      if (!b || b.prefix !== a.prefix || Math.abs(b.n - a.n) > 200) continue;
      pairs++;
      if (b.n - a.n === step) fit++;
    }
  }
  return { pairs, fit };
}

/** The rack's corner wells, and the two photo corners each can be at in a photo of the bottom (a mirror). */
export const CORNER_WELLS = ['A1', 'A12', 'H1', 'H12'];

/**
 * The photos read (readOne's), merged into one rack. `a1Tube`: the tube the person says is in A1;
 * `cornerTubes`: [{ well: 'A1' | 'A12' | 'H1' | 'H12', id }], tubes the person names at corner wells (when the A1
 * tube was not read, the one at A12 or H1 does the same); `codeAt`: the well the person confirmed the rack's code
 * sits beside ('A12' or 'H12'). A named tube the photos did not read is noted, never guessed, and does not stop
 * other evidence. Without the person's word, the rack's code read beside a corner places A1 as the rack model the
 * team uses has it (`defaultCodeAt`, A12 on FK00274230: the tubes go into racks in any order, so their IDs seldom
 * tell): an assumption (`assumed`), said to the person to confirm; the IDs' sequence, when they happen to follow
 * one, only checks it (`checked_by_ids`), and alone never decides. No code read and nothing said: the person is asked.
 * Returns the merged positions, the orientation with what decided it, conflicts, and the rack's code.
 */
export function mergeReadings(readings, { a1Tube = null, codeAt = null, cornerTubes = [], defaultCodeAt = 'A12' } = {}) {
  const named = [...(a1Tube ? [{ well: 'A1', id: a1Tube }] : []), ...cornerTubes]
    .map(t => ({ well: String(t?.well ?? '').toUpperCase(), id: String(t?.id ?? '').trim().toUpperCase() }))
    .filter(t => CORNER_WELLS.includes(t.well) && t.id)
    .filter((t, k, all) => all.findIndex(o => o.well === t.well && o.id === t.id) === k);
  const placed = alignReadings(readings);
  const votes = new Map(); // "i,j" → Map(id → photos)
  const seen = new Set();
  const rackReads = new Map(); // text → { photos, corners: ["i,j" of the tube beside it], side: [di] }
  readings.forEach((r, n) => {
    const p = placed[n];
    if (!p) return;
    for (const [k, id] of r.tubes) {
      const at = key(...move(p, unkey(k)));
      const m = votes.get(at) ?? votes.set(at, new Map()).get(at);
      m.set(id, (m.get(id) ?? 0) + 1);
    }
    for (const k of r.seen) seen.add(key(...move(p, unkey(k))));
    if (r.rack) {
      const at = move(p, r.rack.at);
      const corner = move(p, r.rack.corner);
      const e = rackReads.get(r.rack.text) ?? rackReads.set(r.rack.text, { photos: 0, at, corner }).get(r.rack.text);
      e.photos++;
    }
  });
  // The merged tubes longer across than down: else a quarter turn (a reference photo of part of the rack).
  const coords0 = [...votes.keys(), ...seen].map(unkey);
  const spanOf = (list, k) => (list.length ? Math.max(...list.map(c => c[k])) - Math.min(...list.map(c => c[k])) : 0);
  if (spanOf(coords0, 1) > spanOf(coords0, 0)) {
    const turnKeys = m => new Map([...m].map(([k, v]) => [key(...rotate(unkey(k), 1)), v]));
    const turned = turnKeys(votes);
    votes.clear();
    for (const [k, v] of turned) votes.set(k, v);
    const seenTurned = [...seen].map(k => key(...rotate(unkey(k), 1)));
    seen.clear();
    for (const k of seenTurned) seen.add(k);
    for (const e of rackReads.values()) {
      e.at = rotate(e.at, 1);
      e.corner = rotate(e.corner, 1);
    }
  }
  // An ID read at two places: kept where most photos put it.
  const where = new Map();
  for (const [at, m] of votes) for (const [id, n] of m) if (!where.has(id) || where.get(id).n < n) where.set(id, { at, n });
  const conflicts = [];
  for (const [at, m] of votes)
    for (const [id] of m)
      if (where.get(id).at !== at) {
        conflicts.push({ tube_id: id, also_at: at, kept_at: where.get(id).at });
        m.delete(id);
      }
  // The extent from the tubes read (an unread speck beside the rack's code must not widen it).
  const coords = [...(votes.size ? votes.keys() : seen)].map(unkey);
  const rackCode = [...rackReads].sort((a, b) => b[1].photos - a[1].photos)[0] ?? null;
  const out = {
    photos_used: placed.filter(Boolean).length,
    photos_left_out: placed.map((p, n) => (p ? null : n)).filter(n => n !== null),
    rack_id: rackCode?.[0] ?? null,
    rack_id_photos: rackCode?.[1].photos ?? 0,
    other_rack_codes: [...rackReads.keys()].filter(t => t !== rackCode?.[0]),
  };
  if (!coords.length) return { ...out, frame: null, cells: new Map(), conflicts, notes: [], orientation: { mapping: null, basis: [], problems: [], note: 'No tubes read on a grid.' } };
  let lo = [Math.min(...coords.map(c => c[0])), Math.min(...coords.map(c => c[1]))];
  const hi = [Math.max(...coords.map(c => c[0])), Math.max(...coords.map(c => c[1]))];
  const span = [hi[0] - lo[0] + 1, hi[1] - lo[1] + 1];
  const notes = [];
  // The frame's origin: certain when the tubes fill 12 × 8; otherwise from the rack's code (beside a corner tube).
  let origin = span[0] === COLS && span[1] === ROWS ? lo : null;
  let turn = false;
  if (rackCode) {
    const { at } = rackCode[1];
    // Where the code can sit: a pitch beyond a corner well, along the long side or the short one. Each placement
    // gives a corner well and the rack's origin; one is taken only if a single origin keeps every tube inside the
    // rack and the code outside it (that corner well may be empty).
    const fits = ([oi, oj]) => oi <= lo[0] && oi + COLS - 1 >= hi[0] && oj <= lo[1] && oj + ROWS - 1 >= hi[1];
    const outside = ([oi, oj]) => at[0] < oi - 0.5 || at[0] > oi + COLS - 0.5 || at[1] < oj - 0.5 || at[1] > oj + ROWS - 0.5;
    const placements = [];
    for (const axis of [0, 1]) {
      const n = [COLS, ROWS];
      for (const toward of [1, -1]) {
        const corner = [Math.round(at[0]), Math.round(at[1])];
        corner[axis] = Math.round(at[axis] + toward * 0.95);
        const other = 1 - axis;
        // The corner is the rack's first or last along both axes: the code's side fixes the one, either for the other.
        const onAxis = toward > 0 ? corner[axis] : corner[axis] - (n[axis] - 1);
        for (const o of [corner[other], corner[other] - (n[other] - 1)]) {
          const origin = axis === 0 ? [onAxis, o] : [o, onAxis];
          if (fits(origin) && outside(origin)) placements.push({ origin, corner, turn: axis === 0 && toward < 0 });
        }
      }
    }
    const distinct = [...new Map(placements.map(pl => [pl.origin.join(','), pl])).values()];
    if (distinct.length === 1) {
      const [pl] = distinct;
      rackCode[1].corner = pl.corner;
      turn = pl.turn;
      if (!origin) origin = pl.origin;
    } else {
      rackCode[1].corner = null;
      notes.push('The rack code was read but where it sits does not place the rack: it is not used for positions.');
    }
  }
  if (span[0] > COLS || span[1] > ROWS) notes.push(`The tubes spread over ${span[0]} × ${span[1]} places, more than a rack's 12 × 8: one photo may be misplaced.`);
  // Or from a tube the person names at a corner well (that was read): A1 and H12 can only be at the photo's top
  // right or bottom left, A12 and H1 at its top left or bottom right (either mirror, either half turn); taken
  // when only one of the two leaves every tube inside the rack.
  for (const t of named) {
    if (origin || !where.has(t.id)) continue;
    const [i, j] = unkey(where.get(t.id).at);
    const fits = ([oi, oj]) => oi <= lo[0] && oi + COLS - 1 >= hi[0] && oj <= lo[1] && oj + ROWS - 1 >= hi[1];
    const options = (t.well === 'A1' || t.well === 'H12' ? [[i - (COLS - 1), j], [i, j - (ROWS - 1)]] : [[i, j], [i - (COLS - 1), j - (ROWS - 1)]]).filter(fits);
    if (options.length === 1) origin = options[0];
  }
  // Without the rack's edges no well can be named: a sequence of IDs proves the mirror, not where row A is.
  const placedEdges = !!origin;
  if (!origin) {
    notes.push('The tubes do not reach every edge of the rack and its code was not placed: photo positions are relative, no wells.');
    origin = lo;
  }
  // Frame cell (c 0..11, r 0..7), turned so the rack code is at the left when it was read.
  const toFrame = k => {
    const [i, j] = unkey(k);
    const c = i - origin[0];
    const r = j - origin[1];
    return turn ? [COLS - 1 - c, ROWS - 1 - r] : [c, r];
  };
  const cells = new Map(); // "c,r" → { id, photos, seen }
  for (const [at, m] of votes) {
    const [id, n] = [...m].sort((a, b) => b[1] - a[1])[0] ?? [];
    const f = toFrame(at);
    if (m.size > 1) conflicts.push({ photo_position: photoName(f), tube_ids: [...m.keys()] });
    if (id) cells.set(key(...f), { id, photos: n });
  }
  for (const s of seen) {
    const [c, r] = toFrame(s);
    if (c >= 0 && c < COLS && r >= 0 && r < ROWS && !cells.has(key(c, r))) cells.set(key(c, r), { id: null, photos: 0, seen: true });
  }
  for (const c of conflicts)
    if (c.also_at) {
      c.also_at = photoName(toFrame(c.also_at));
      c.kept_at = photoName(toFrame(c.kept_at));
    }
  const codeCorner = rackCode?.[1].corner ? toFrame(key(...rackCode[1].corner)) : null;
  // ---- orientation: which mirror the photo is.
  const byWell = mapping => new Map([...cells].filter(([, v]) => v.id).map(([k, v]) => [wellOf(mapping, ...unkey(k)), v.id]));
  const votesFor = new Map(); // mapping → [basis]
  const say = (mapping, basis) => votesFor.set(mapping, [...(votesFor.get(mapping) ?? []), basis]);
  const problems = [];
  // A named tube not read (glare, a smudged code) decides nothing: noted, and the person is asked for another corner.
  const notRead = [];
  for (const t of named) {
    const hit = [...cells].find(([, v]) => v.id === t.id);
    if (!hit) {
      notRead.push(t);
      notes.push(`${t.id} (given as ${t.well}) was not read in these photos: it does not place the rack.`);
      continue;
    }
    const f = unkey(hit[0]);
    const m = ['m1', 'm2'].find(mm => wellOf(mm, ...f) === t.well);
    if (m) say(m, t.well === 'A1' ? 'a1_tube' : 'corner_tube');
    else problems.push(`${t.id} (given as ${t.well}) is at photo position ${photoName(f)}, not a corner where ${t.well} can be in a photo of the bottom.`);
  }
  if (codeAt && codeCorner) {
    const m = ['m1', 'm2'].find(mm => wellOf(mm, ...codeCorner) === codeAt);
    if (m) say(m, 'rack_code');
    else problems.push(`The rack code is beside photo position ${photoName(codeCorner)}, which cannot be ${codeAt} in a photo of the bottom.`);
  }
  // The rack model's layout, when the person said nothing: the code beside `defaultCodeAt`.
  const personSaid = votesFor.size > 0;
  if (!personSaid && codeCorner && defaultCodeAt) {
    const m = ['m1', 'm2'].find(mm => wellOf(mm, ...codeCorner) === defaultCodeAt);
    if (m) say(m, 'rack_code_default');
  }
  const fits = Object.fromEntries(['m1', 'm2'].map(m => [m, sequenceFit(byWell(m))]));
  const strong = m => fits[m].pairs >= 10 && fits[m].fit >= 0.8 * fits[m].pairs;
  const sequence = strong('m1') !== strong('m2') ? (strong('m1') ? 'm1' : 'm2') : null;
  if (sequence) say(sequence, 'tube_sequence');
  let mapping = null;
  // The IDs' sequence alone does not decide (it is only a check of the code's corner or the person's word).
  const decided = [...votesFor].filter(([, b]) => b.some(x => x !== 'tube_sequence'));
  if (votesFor.size === 1 && decided.length === 1 && !problems.length && placedEdges) mapping = decided[0][0];
  if (!placedEdges)
    problems.push(
      'The rack is not filled edge to edge and its code was not read beside a corner: which well is which is not known. Send a photo where the rack code shows, or say which tube is in A1 (a1_tube), or in A12 or H1 (corner_tube).',
    );
  if (votesFor.size > 1) problems.push(`The evidence disagrees: ${[...votesFor].map(([m, b]) => `${b.join(' and ')} say A1 is at photo position ${photoName(m === 'm1' ? [COLS - 1, 0] : [0, ROWS - 1])}`).join('; ')}.`);
  const basis = mapping ? votesFor.get(mapping) : [];
  const orientation = {
    mapping,
    basis,
    // The code's usual corner taken without the person's word: the person confirms it (or says H12).
    assumed: basis.includes('rack_code_default'),
    checked_by_ids: basis.includes('tube_sequence'),
    sequence_suggests: !mapping && sequence ? photoName(sequence === 'm1' ? [COLS - 1, 0] : [0, ROWS - 1]) : null,
    a1_photo_position: mapping ? photoName(mapping === 'm1' ? [COLS - 1, 0] : [0, ROWS - 1]) : null,
    rack_code_beside: mapping && codeCorner ? wellOf(mapping, ...codeCorner) : null,
    tube_sequence: { m1: fits.m1, m2: fits.m2 },
    problems,
    not_read: notRead.map(t => ({ well: t.well, id: t.id })),
    note: mapping
      ? null
      : 'A1 is not known for these photos: a photo of the bottom is a mirror, and A1 is either at photo position ' +
        `${photoName([COLS - 1, 0])} or ${photoName([0, ROWS - 1])}. ` +
        (codeCorner ? '' : "The rack's code (the printed square at a corner) was not read beside a corner in any photo: ask for a photo where it shows, or ") +
        (sequence ? `(The tubes' IDs follow a sequence that puts A1 at ${photoName(sequence === 'm1' ? [COLS - 1, 0] : [0, ROWS - 1])}: a hint, not proof.) ` : '') +
        (notRead.length
          ? `${notRead.map(t => `${t.id} (given as ${t.well})`).join(' and ')} was not read in these photos. Ask the person for the ID of the tube in ${['A12', 'H1'].filter(w => !notRead.some(t => t.well === w)).join(' or ') || 'H12'} instead (look at the rack from above, where the wells are printed) and read it again with corner_tube.`
          : 'Ask the person which tube is in A1 (look at the rack from above, where A1 is printed) and read it again with a1_tube; if the A1 tube was not read, the tube in A12 or H1 with corner_tube' +
            (codeCorner ? `, or whether the rack code is beside A12 or H12 (here it would be ${wellOf('m1', ...codeCorner)} or ${wellOf('m2', ...codeCorner)}).` : '.')),
  };
  return { ...out, frame: { origin, turn, span, rack_code_photo_position: codeCorner ? photoName(codeCorner) : null, code_corner: codeCorner }, cells, conflicts, orientation, notes };
}
