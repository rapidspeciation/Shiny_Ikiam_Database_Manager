// Wing outlines and the tight zoom box of a photo, from the wing chain's masks
// (the Rename Photos app's chain: V5N segmentation + wing classifier). Shared by
// the Sanger backfill (photo_readings, 19 Oct 2026) and the app's photo-reading
// worker, so both give the same points. No dependencies; plain ES module (Node
// .mjs, or bundled into TS with esbuild).
//
// Agreed in /tmp/gallery-readings-spec.md:
// - per wing, the largest 4-connected part of its mask (logit > 0, inside the
//   wing's box), its outer outline, Douglas-Peucker with a tolerance of 0.4 % of
//   the photo's longer side, at most 32 points (the tolerance grows until it fits);
// - points quantized to integers 0..1000 of the photo AS SHOWN (EXIF applied, not
//   turned upright): x = 1000 * x / width, y = 1000 * y / height;
// - kind "fw" | "hw" | "x" (extra, or not a wing for the classifier), half "l" | "r" | "u";
// - the zoom box: union of the fw/hw outlines (photo px), then 1.5 % of the
//   photo's shorter side on each side, clamped to the photo; fractions, 4 decimals.

export const OUTLINE_VERSION = 1;
export const SIMPLIFY_FRACTION = 0.004;
export const MAX_POINTS = 32;
export const BOX_PADDING = 0.015;
/** grid step of the mask sampling, as a fraction of the photo's longer side (4 px on 1600 px) */
export const GRID_FRACTION = 1 / 400;

const r4 = n => Math.round(n * 1e4) / 1e4;

/**
 * Samples one wing's mask on a regular grid over its box and returns the outer
 * outline of the largest connected part, in photo pixels of the frame the box is in.
 * @param {(xs: number[], ys: number[]) => Float32Array} sample mask logits at photo points
 *   (row-major: ys rows of xs columns), -Infinity outside the wing's box; model.ts maskLogits
 * @param {[number, number, number, number]} box the wing's box in photo pixels
 * @param {{ width: number, height: number, threshold?: number, step?: number }} photo
 * @returns {Array<[number, number]> | null}
 */
export function traceWing(sample, box, { width, height, threshold = 0, step }) {
  const s = step ?? Math.max(width, height) * GRID_FRACTION;
  const [bx1, by1, bx2, by2] = box;
  if (!(bx2 > bx1 && by2 > by1)) return null;
  // one empty cell of margin around the box so every outline closes
  const x0 = bx1 - s,
    y0 = by1 - s;
  const gw = Math.ceil((bx2 - bx1) / s) + 2,
    gh = Math.ceil((by2 - by1) / s) + 2;
  const xs = Array.from({ length: gw }, (_, i) => x0 + (i + 0.5) * s);
  const ys = Array.from({ length: gh }, (_, j) => y0 + (j + 0.5) * s);
  const logits = sample(xs, ys);
  const mask = new Uint8Array(gw * gh);
  for (let j = 0; j < gh; j++)
    for (let i = 0; i < gw; i++) {
      const x = xs[i],
        y = ys[j];
      if (x < 0 || y < 0 || x > width || y > height) continue;
      if (logits[j * gw + i] > threshold) mask[j * gw + i] = 1;
    }
  const blob = largestBlob(mask, gw, gh);
  if (!blob) return null;
  const loop = outerLoop(blob, gw, gh);
  if (!loop || loop.length < 3) return null;
  const pts = loop.map(([i, j]) => [Math.min(width, Math.max(0, x0 + i * s)), Math.min(height, Math.max(0, y0 + j * s))]);
  return simplifyClosed(pts, SIMPLIFY_FRACTION * Math.max(width, height), MAX_POINTS);
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

/**
 * The outer boundary of a blob as a closed loop of grid-corner points [i, j]
 * (corner (i, j) is the top-left corner of cell (i, j)). Boundary edges are
 * directed with the blob on their left; at a pinch corner the walk turns left,
 * so the loops are simple; the outer loop is the one with the largest area.
 */
export function outerLoop(blob, w, h) {
  const at = (i, j) => (i >= 0 && j >= 0 && i < w && j < h ? blob[j * w + i] : 0);
  const W1 = w + 1;
  // out-edges per corner: up to 2; direction 0 right, 1 down, 2 left, 3 up (y grows downwards)
  const next = new Map();
  const add = (i, j, d) => {
    const k = j * W1 + i;
    const l = next.get(k);
    if (l) l.push(d);
    else next.set(k, [d]);
  };
  for (let j = 0; j < h; j++)
    for (let i = 0; i < w; i++) {
      if (!blob[j * w + i]) continue;
      // blob on the left of each directed edge (screen coordinates, y down): go around the cell counter-clockwise on screen
      if (!at(i, j - 1)) add(i + 1, j, 2); // top edge, right -> left
      if (!at(i - 1, j)) add(i, j, 1); // left edge, top -> bottom
      if (!at(i, j + 1)) add(i, j + 1, 0); // bottom edge, left -> right
      if (!at(i + 1, j)) add(i + 1, j + 1, 3); // right edge, bottom -> top
    }
  const D = [
    [1, 0],
    [0, 1],
    [-1, 0],
    [0, -1],
  ];
  let best = null,
    bestArea = 0;
  for (const [start, dirs] of next) {
    while (dirs.length) {
      let d = dirs.pop();
      let k = start;
      const loop = [];
      for (let guard = 0; guard < 4 * W1 * (h + 1) + 8; guard++) {
        const i = k % W1,
          j = (k / W1) | 0;
        loop.push([i, j]);
        const ni = i + D[d][0],
          nj = j + D[d][1];
        k = nj * W1 + ni;
        if (k === start) break;
        const outs = next.get(k);
        if (!outs || !outs.length) break;
        // turn left first (the blob is on the left: keeps parts that touch at a corner apart), then straight, then right
        let pick = -1;
        for (const p of [(d + 3) % 4, d, (d + 1) % 4]) {
          const idx = outs.indexOf(p);
          if (idx >= 0) {
            pick = p;
            outs.splice(idx, 1);
            break;
          }
        }
        if (pick < 0) pick = outs.pop();
        d = pick;
      }
      // drop collinear corners
      const pts = loop.filter((p, n) => {
        const a = loop[(n + loop.length - 1) % loop.length],
          b = loop[(n + 1) % loop.length];
        return (p[0] - a[0]) * (b[1] - p[1]) - (p[1] - a[1]) * (b[0] - p[0]) !== 0;
      });
      const area = Math.abs(polygonArea(pts));
      if (area > bestArea) ((bestArea = area), (best = pts));
    }
  }
  return best;
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

/** Flat 0..1000 points -> SVG path data "M x y x y … Z" (the gallery's wing_outlines.json). */
export function outlinePath(flat) {
  return flat.length ? `M${flat.join(' ')}Z` : '';
}

/** SVG path data -> flat points. */
export function parsePath(path) {
  return (path || '')
    .replace(/^M/, '')
    .replace(/Z$/, '')
    .trim()
    .split(/\s+/)
    .filter(Boolean)
    .map(Number);
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
