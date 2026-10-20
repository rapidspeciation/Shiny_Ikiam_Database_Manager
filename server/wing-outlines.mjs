// Wing outlines and the tight zoom box of a photo, from the wing chain's masks
// (the Rename Photos app's chain: V5N segmentation + wing classifier). Shared by
// the Sanger backfill (photo_readings, 19 Oct 2026) and the app's photo-reading
// worker, so both give the same points. No dependencies; plain ES module (Node
// .mjs, or bundled into TS with esbuild).
//
// Agreed in /tmp/gallery-readings-spec.md:
// - per wing, the largest 4-connected part of its mask (logit > 0, inside the
//   wing's box, sampled every 1/800 of the photo's longer side), its outer outline
//   traced along the logit = 0 iso-line (marching squares, crossings interpolated
//   between samples), Douglas-Peucker with a tolerance of 0.1 % of the photo's
//   longer side, at most 64 points (the tolerance grows until it fits);
// - points quantized to integers 0..1000 of the photo AS SHOWN (EXIF applied, not
//   turned upright): x = 1000 * x / width, y = 1000 * y / height; the public file
//   writes them as relative SVG path data (outlinePath);
// - kind "fw" | "hw" | "x" (extra, or not a wing for the classifier), half "l" | "r" | "u";
// - the zoom box: union of the fw/hw outlines (photo px), then 1.5 % of the
//   photo's shorter side on each side, clamped to the photo; fractions, 4 decimals.

export const OUTLINE_VERSION = 2;
export const SIMPLIFY_FRACTION = 0.001;
export const MAX_POINTS = 64;
export const BOX_PADDING = 0.015;
/** grid step of the mask sampling, as a fraction of the photo's longer side (2 px on 1600 px) */
export const GRID_FRACTION = 1 / 800;

const r4 = n => Math.round(n * 1e4) / 1e4;

/**
 * Samples one wing's mask on a regular grid over its box and returns the outer
 * outline of the largest connected part, in photo pixels of the frame the box is in:
 * the logit = threshold iso-line by marching squares, its crossings linearly
 * interpolated between grid samples (sub-sample, no staircase), then Douglas-Peucker.
 * @param {(xs: number[], ys: number[]) => Float32Array} sample mask logits at photo points
 *   (row-major: ys rows of xs columns), -Infinity outside the wing's box; model.ts maskLogits
 * @param {[number, number, number, number]} box the wing's box in photo pixels
 * @param {{ width: number, height: number, threshold?: number, step?: number, tolerance?: number, maxPoints?: number }} photo
 *   tolerance: Douglas-Peucker tolerance in px (default SIMPLIFY_FRACTION of the longer side); 0 = unsimplified
 * @returns {Array<[number, number]> | null}
 */
export function traceWing(sample, box, { width, height, threshold = 0, step, tolerance, maxPoints = MAX_POINTS }) {
  const long = Math.max(width, height);
  const s = step ?? long * GRID_FRACTION;
  const [bx1, by1, bx2, by2] = box;
  if (!(bx2 > bx1 && by2 > by1)) return null;
  // samples at x0 + i * s; one sample of margin outside the box so every outline closes
  const x0 = bx1 - s,
    y0 = by1 - s;
  const gw = Math.ceil((bx2 - bx1) / s) + 3,
    gh = Math.ceil((by2 - by1) / s) + 3;
  const xs = Array.from({ length: gw }, (_, i) => x0 + i * s);
  const ys = Array.from({ length: gh }, (_, j) => y0 + j * s);
  const logits = sample(xs, ys);
  const f = new Float32Array(gw * gh);
  const mask = new Uint8Array(gw * gh);
  for (let j = 0; j < gh; j++)
    for (let i = 0; i < gw; i++) {
      const t = j * gw + i;
      const inside = xs[i] >= 0 && ys[j] >= 0 && xs[i] <= width && ys[j] <= height && i > 0 && j > 0 && i < gw - 1 && j < gh - 1;
      // -Infinity (outside the box) and samples outside the photo count as a clear "no"
      const v = inside && Number.isFinite(logits[t]) ? logits[t] - threshold : -1;
      f[t] = v;
      if (v > 0) mask[t] = 1;
    }
  const blob = largestBlob(mask, gw, gh);
  if (!blob) return null;
  // positive only on the largest part: other parts become a small "no"
  for (let t = 0; t < f.length; t++) if (f[t] > 0 && !blob[t]) f[t] = -1e-3;
  const loop = isoLoop(f, gw, gh);
  if (!loop || loop.length < 3) return null;
  const pts = loop.map(([u, v]) => [Math.min(width, Math.max(0, x0 + u * s)), Math.min(height, Math.max(0, y0 + v * s))]);
  const tol = tolerance ?? SIMPLIFY_FRACTION * long;
  return tol > 0 ? simplifyClosed(pts, tol, maxPoints) : pts;
}

/**
 * The longest closed iso-line (value 0) of a grid of values (positive inside) by
 * marching squares, as points [u, v] in grid units (sample (i, j) is at [i, j]),
 * crossings interpolated linearly along the cell edges; saddles are resolved by the
 * cell's mean. The outer loop is the one enclosing the largest area.
 */
export function isoLoop(f, gw, gh) {
  // crossing points on cell edges, keyed: horizontal edge (i,j)-(i+1,j) -> 2*(j*gw+i), vertical (i,j)-(i,j+1) -> +1
  const pt = new Map();
  const cross = (key, i0, j0, i1, j1) => {
    if (!pt.has(key)) {
      const a = f[j0 * gw + i0],
        b = f[j1 * gw + i1];
      const t = a / (a - b);
      pt.set(key, [i0 + (i1 - i0) * t, j0 + (j1 - j0) * t]);
    }
    return key;
  };
  const links = new Map(); // key -> [other keys]
  const link = (p, q) => {
    (links.get(p) || links.set(p, []).get(p)).push(q);
    (links.get(q) || links.set(q, []).get(q)).push(p);
  };
  for (let j = 0; j < gh - 1; j++)
    for (let i = 0; i < gw - 1; i++) {
      const a = f[j * gw + i] > 0,
        b = f[j * gw + i + 1] > 0,
        c = f[(j + 1) * gw + i + 1] > 0,
        d = f[(j + 1) * gw + i] > 0;
      const code = (a ? 1 : 0) | (b ? 2 : 0) | (c ? 4 : 0) | (d ? 8 : 0);
      if (code === 0 || code === 15) continue;
      const top = () => cross(2 * (j * gw + i), i, j, i + 1, j);
      const bottom = () => cross(2 * ((j + 1) * gw + i), i, j + 1, i + 1, j + 1);
      const left = () => cross(2 * (j * gw + i) + 1, i, j, i, j + 1);
      const right = () => cross(2 * (j * gw + i + 1) + 1, i + 1, j, i + 1, j + 1);
      switch (code) {
        case 1: case 14: link(left(), top()); break;
        case 2: case 13: link(top(), right()); break;
        case 3: case 12: link(left(), right()); break;
        case 4: case 11: link(right(), bottom()); break;
        case 6: case 9: link(top(), bottom()); break;
        case 7: case 8: link(left(), bottom()); break;
        case 5: case 10: {
          const mid = (f[j * gw + i] + f[j * gw + i + 1] + f[(j + 1) * gw + i + 1] + f[(j + 1) * gw + i]) / 4 > 0;
          // a, c positive (5) or b, d positive (10); joined through the centre when the mean is positive
          if ((code === 5) === mid) (link(left(), bottom()), link(top(), right()));
          else (link(left(), top()), link(right(), bottom()));
          break;
        }
      }
    }
  const used = new Set();
  let best = null,
    bestArea = 0;
  for (const start of links.keys()) {
    if (used.has(start)) continue;
    const loop = [];
    let prev = null,
      cur = start;
    while (cur !== undefined && !used.has(cur)) {
      used.add(cur);
      loop.push(pt.get(cur));
      const nb = links.get(cur);
      const nxt = nb.find(k => k !== prev && !used.has(k));
      prev = cur;
      cur = nxt;
    }
    if (loop.length < 3) continue;
    const area = Math.abs(polygonArea(loop));
    if (area > bestArea) ((bestArea = area), (best = loop));
  }
  return best;
}

/** The largest 4-connected part of a grid (null when empty). */
export function largestBlob(mask, w, h) {
  const lab = new Int32Array(w * h),
    stack = new Int32Array(w * h);
  let best = 0,
    bestSize = 0,
    cur = 0;
  for (let s = 0; s < w * h; s++) {
    if (!mask[s] || lab[s]) continue;
    cur++;
    let size = 0,
      sp = 0;
    stack[sp++] = s;
    lab[s] = cur;
    while (sp) {
      const p = stack[--sp];
      size++;
      const x = p % w,
        y = (p / w) | 0;
      if (x > 0 && mask[p - 1] && !lab[p - 1]) ((lab[p - 1] = cur), (stack[sp++] = p - 1));
      if (x < w - 1 && mask[p + 1] && !lab[p + 1]) ((lab[p + 1] = cur), (stack[sp++] = p + 1));
      if (y > 0 && mask[p - w] && !lab[p - w]) ((lab[p - w] = cur), (stack[sp++] = p - w));
      if (y < h - 1 && mask[p + w] && !lab[p + w]) ((lab[p + w] = cur), (stack[sp++] = p + w));
    }
    if (size > bestSize) ((bestSize = size), (best = cur));
  }
  if (!best) return null;
  const out = new Uint8Array(w * h);
  for (let s = 0; s < w * h; s++) out[s] = lab[s] === best ? 1 : 0;
  return out;
}

export function polygonArea(pts) {
  let a = 0;
  for (let n = 0; n < pts.length; n++) {
    const p = pts[n],
      q = pts[(n + 1) % pts.length];
    a += p[0] * q[1] - q[0] * p[1];
  }
  return a / 2;
}

function dpOpen(pts, tol) {
  if (pts.length <= 2) return pts.slice();
  const keep = new Uint8Array(pts.length);
  keep[0] = keep[pts.length - 1] = 1;
  const stack = [[0, pts.length - 1]];
  while (stack.length) {
    const [a, b] = stack.pop();
    const [ax, ay] = pts[a],
      [bx, by] = pts[b];
    const dx = bx - ax,
      dy = by - ay,
      len = Math.hypot(dx, dy);
    let far = -1,
      dmax = tol;
    for (let n = a + 1; n < b; n++) {
      const [px, py] = pts[n];
      const d = len ? Math.abs(dy * px - dx * py + bx * ay - by * ax) / len : Math.hypot(px - ax, py - ay);
      if (d > dmax) ((dmax = d), (far = n));
    }
    if (far >= 0) {
      keep[far] = 1;
      stack.push([a, far], [far, b]);
    }
  }
  return pts.filter((_, n) => keep[n]);
}

/**
 * Douglas-Peucker on a closed loop (split at the point farthest from the first),
 * the tolerance raised by 25 % steps until at most `maxPoints` remain.
 */
export function simplifyClosed(pts, tol, maxPoints = MAX_POINTS) {
  if (pts.length <= 3) return pts.slice();
  let far = 0,
    dmax = -1;
  for (let n = 1; n < pts.length; n++) {
    const d = Math.hypot(pts[n][0] - pts[0][0], pts[n][1] - pts[0][1]);
    if (d > dmax) ((dmax = d), (far = n));
  }
  let t = tol,
    out;
  for (let tries = 0; tries < 60; tries++) {
    const a = dpOpen(pts.slice(0, far + 1), t);
    const b = dpOpen([...pts.slice(far), pts[0]], t);
    out = [...a, ...b.slice(1, -1)];
    if (out.length <= maxPoints) break;
    t *= 1.25;
  }
  return out;
}

/**
 * Points of the frame the chain checked -> the photo as shown. The chain checks a
 * photo it finds quarter-turned again turned upright (model.ts analysePhoto:
 * rotate(img, turnK === 1 ? 270 : 90), counter-clockwise degrees); `checkedSize` is
 * that frame's [width, height]. Returns the points and the shown size.
 */
export function toShown(points, turnK, checkedUpright, checkedSize) {
  const [W, H] = checkedSize;
  if (!checkedUpright) return { points, size: [W, H] };
  // the checked frame = shown photo turned r degrees counter-clockwise; undo with (360 - r) more
  const r = turnK === 1 ? 270 : 90;
  const k = ((360 - r) / 90) % 4;
  const turn = ([x, y]) => {
    let w = W,
      h = H;
    for (let n = 0; n < k; n++) {
      // a counter-clockwise quarter turn of a w x h photo: (x, y) -> (y, w - x), size h x w
      [x, y] = [y, w - x];
      [w, h] = [h, w];
    }
    return [x, y];
  };
  return { points: points.map(turn), size: k % 2 ? [H, W] : [W, H] };
}

/** Photo pixels -> flat integers 0..1000 [x0, y0, x1, y1, …]. */
export function quantize(points, width, height) {
  const q = [];
  for (const [x, y] of points)
    q.push(Math.min(1000, Math.max(0, Math.round((1000 * x) / width))), Math.min(1000, Math.max(0, Math.round((1000 * y) / height))));
  // drop repeated points after rounding
  const out = [];
  for (let n = 0; n < q.length; n += 2) {
    const m = out.length;
    if (m && out[m - 2] === q[n] && out[m - 1] === q[n + 1]) continue;
    out.push(q[n], q[n + 1]);
  }
  if (out.length > 2 && out[0] === out[out.length - 2] && out[1] === out[out.length - 1]) out.length -= 2;
  return out;
}

/**
 * Flat 0..1000 points -> SVG path data with relative commands, the gallery's
 * wing_outlines.json (version 2): "m x0 y0" then "l" and the steps dx dy to each
 * next point (a minus sign separates without a space), closed with "z";
 * e.g. [108, 177, 152, 170, 150, 190] -> "m108 177l44-7-2 20z".
 */
export function outlinePath(flat) {
  if (flat.length < 2) return '';
  let d = `m${flat[0]} ${flat[1]}`;
  if (flat.length > 2) {
    const steps = [];
    for (let n = 2; n < flat.length; n++) steps.push(flat[n] - flat[n - 2]);
    d += 'l' + steps.join(' ').replace(/ -/g, '-');
  }
  return d + 'z';
}

/** SVG path data (absolute "M x y … Z" of version 1, or relative "m x y l dx dy … z") -> flat points. */
export function parsePath(path) {
  const p = (path || '').trim();
  const nums = s => (s.match(/-?\d+(?:\.\d+)?/g) || []).map(Number);
  if (p.startsWith('m')) {
    const [head, tail = ''] = p.replace(/z$/i, '').slice(1).split('l');
    const flat = nums(head);
    const steps = nums(tail);
    for (let n = 0; n < steps.length; n++) flat.push(flat[flat.length - 2] + steps[n]);
    return flat;
  }
  return nums(p.replace(/^M/, '').replace(/Z$/, ''));
}

export const kindCode = kind => (kind === 'forewing' ? 'fw' : kind === 'hindwing' ? 'hw' : 'x');
export const halfCode = half => (half === 'left' ? 'l' : half === 'right' ? 'r' : 'u');

/**
 * The zoom box: union of the fw/hw outlines (photo px of the photo as shown) +
 * `pad` x the shorter side per side, clamped; fractions 0..1 with 4 decimals.
 * @param {Array<{kind: string, points: Array<[number, number]>}>} wings
 */
export function zoomBox(wings, width, height, pad = BOX_PADDING) {
  let x1 = Infinity,
    y1 = Infinity,
    x2 = -Infinity,
    y2 = -Infinity;
  for (const w of wings) {
    const k = w.kind.length > 2 ? kindCode(w.kind) : w.kind;
    if (k !== 'fw' && k !== 'hw') continue;
    for (const [x, y] of w.points) {
      if (x < x1) x1 = x;
      if (y < y1) y1 = y;
      if (x > x2) x2 = x;
      if (y > y2) y2 = y;
    }
  }
  if (!(x2 > x1 && y2 > y1)) return null;
  const p = pad * Math.min(width, height);
  return [
    r4(Math.max(0, x1 - p) / width),
    r4(Math.max(0, y1 - p) / height),
    r4(Math.min(width, x2 + p) / width),
    r4(Math.min(height, y2 + p) / height),
  ];
}

/**
 * Everything for one photo, from the chain's wings with their outlines.
 * @param {{ wings: Array<{kind: string, half: string, score?: number, points: Array<[number, number]> | null}>,
 *           turnK: number, checkedUpright?: boolean, size: [number, number] }} reading
 *   `points` in the checked frame (traceWing); the chain's `size` is that frame's.
 * @returns {{ size: [number, number], w: Array<[string, string, number[]]>, box: number[] | null, conf: number | null }}
 *   `w`: [kind, half, flat 0..1000 points] per wing with an outline, in the chain's wing order, x included;
 *   `box` from fw/hw only; `conf` the lowest segmentation score of the fw/hw wings.
 */
export function photoOutlines(reading) {
  let size = reading.size;
  const shown = [];
  for (const wing of reading.wings) {
    if (!wing.points?.length) continue;
    const t = toShown(wing.points, reading.turnK, !!reading.checkedUpright, reading.size);
    size = t.size;
    shown.push({ kind: kindCode(wing.kind), half: halfCode(wing.half), score: wing.score, points: t.points });
  }
  if (reading.checkedUpright && !shown.length) size = toShown([], reading.turnK, true, reading.size).size;
  const [W, H] = size;
  const fwhw = shown.filter(w => w.kind !== 'x');
  const scores = fwhw.map(w => w.score).filter(Number.isFinite);
  return {
    size,
    w: shown.map(w => [w.kind, w.half, quantize(w.points, W, H)]).filter(x => x[2].length >= 6),
    box: zoomBox(fwhw, W, H),
    conf: scores.length ? Math.round(Math.min(...scores) * 1000) / 1000 : null,
  };
}
