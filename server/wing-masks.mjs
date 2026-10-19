// The wing chain's masks outside the chain: its last segmentation pass, captured in the WingBackend of the background
// readings (server/readings-worker.mjs), gives each wing analysePhoto returned its detection (score, coefficients,
// box) and its mask logits, as the Rename Photos app's src/lib/wings/model.ts computes them (segment, protoMask,
// maskLogits: copied, not exported there). The outlines themselves are server/wing-outlines.mjs (shared with the
// Sanger backfill): traceWing, toShown, quantize.

import { halfCode, kindCode, quantize, toShown, traceWing } from './wing-outlines.mjs';

const iou = (a, b) => {
  const ix = Math.max(0, Math.min(a[2], b[2]) - Math.max(a[0], b[0]));
  const iy = Math.max(0, Math.min(a[3], b[3]) - Math.max(a[1], b[1]));
  const i = ix * iy;
  return i / ((a[2] - a[0]) * (a[3] - a[1]) + (b[2] - b[0]) * (b[3] - b[1]) - i + 1e-9);
};

/** The letterbox model.ts puts a W x H photo in (S x S). */
export function letterboxOf(W, H, S) {
  const r = Math.min(S / W, S / H);
  return { r, px: Math.round((S - Math.round(W * r)) / 2 - 0.1), py: Math.round((S - Math.round(H * r)) / 2 - 0.1), S };
}

/**
 * The detections the chain keeps (model.ts segment: score >= conf, best first, NMS, at most maxWings), boxes in
 * photo pixels. `seg`: { det, detDims, S, r, px, py, width, height }.
 */
export function keptDetections(seg, { conf, nmsIou, maxWings }) {
  const { det, detDims, r, px, py, width, height } = seg;
  const N = detDims[1];
  const D = detDims[2];
  const cand = [];
  for (let i = 0; i < N; i++) if (det[i * D + 4] >= conf) cand.push(i);
  cand.sort((a, b) => det[b * D + 4] - det[a * D + 4]);
  const kept = [];
  for (const i of cand) {
    const lbox = [det[i * D], det[i * D + 1], det[i * D + 2], det[i * D + 3]];
    if (kept.every(k => iou(k.lbox, lbox) < nmsIou)) kept.push({ i, lbox });
    if (kept.length === maxWings) break;
  }
  return kept.map(({ i, lbox }) => ({
    score: det[i * D + 4],
    coef: det.slice(i * D + 6, i * D + 38),
    box: [Math.max(0, (lbox[0] - px) / r), Math.max(0, (lbox[1] - py) / r), Math.min(width, (lbox[2] - px) / r), Math.min(height, (lbox[3] - py) / r)],
  }));
}

/** model.ts protoMask: P x P mask logits of a detection (coefficients . prototypes). */
function protoMask(d, seg) {
  const P = seg.protoDims[2];
  const n = P * P;
  const m = new Float32Array(n);
  for (let c = 0; c < 32; c++) {
    const w = d.coef[c];
    const off = c * n;
    for (let j = 0; j < n; j++) m[j] += w * seg.proto[off + j];
  }
  return m;
}

/** model.ts maskLogits: mask logits at photo points (xs columns, ys rows), -Infinity outside the detection's box. */
function maskLogits(d, seg, pm, xs, ys) {
  const P = seg.protoDims[2];
  const { S, r, px, py } = seg;
  const k = P / S;
  const [bx1, by1, bx2, by2] = d.box;
  const out = new Float32Array(xs.length * ys.length);
  const gx = xs.map(x => Math.min(P - 1, Math.max(0, (x * r + px) * k - 0.5)));
  for (let yi = 0; yi < ys.length; yi++) {
    const y = ys[yi];
    const gy = Math.min(P - 1, Math.max(0, (y * r + py) * k - 0.5));
    const y0 = Math.floor(gy);
    const y1 = Math.min(y0 + 1, P - 1);
    const wy = gy - y0;
    for (let xi = 0; xi < xs.length; xi++) {
      const x = xs[xi];
      const t = yi * xs.length + xi;
      if (x < bx1 || x > bx2 || y < by1 || y > by2) {
        out[t] = -Infinity;
        continue;
      }
      const g = gx[xi];
      const x0 = Math.floor(g);
      const x1 = Math.min(x0 + 1, P - 1);
      const wx = g - x0;
      out[t] = (pm[y0 * P + x0] * (1 - wx) + pm[y0 * P + x1] * wx) * (1 - wy) + (pm[y1 * P + x0] * (1 - wx) + pm[y1 * P + x1] * wx) * wy;
    }
  }
  return out;
}

/**
 * Each wing of analysePhoto's `result`, in the photo as shown: { box (fractions, 4 decimals), score, outline:
 * [kind, half, [x, y, …] 0..1000] or [kind, half, null] (no mask) }, in the order of result.wings. Each wing is
 * matched to its detection by its rounded box. `seg`: the last segmentation pass ({ det, detDims, proto,
 * protoDims, S }); `cfg`: wingside.json's segmentation section.
 */
export function wingShapes(seg, result, cfg) {
  const [W, H] = result.size;
  const frame = { ...seg, ...letterboxOf(W, H, seg.S), width: W, height: H };
  const dets = keptDetections(frame, cfg);
  const used = new Set();
  const upright = !!result.checkedUpright;
  const shown = toShown([], result.turnK, upright, result.size).size;
  const r4 = v => Math.round(v * 10000) / 10000;
  return result.wings.map(w => {
    const at = dets.findIndex((d, i) => !used.has(i) && d.box.every((v, j) => Math.round(v) === w.box[j]));
    const d = at >= 0 ? dets[at] : null;
    if (d) used.add(at);
    const corners = toShown(
      [
        [w.box[0], w.box[1]],
        [w.box[2], w.box[1]],
        [w.box[0], w.box[3]],
        [w.box[2], w.box[3]],
      ],
      result.turnK,
      upright,
      result.size,
    ).points;
    const xs = corners.map(p => p[0]);
    const ys = corners.map(p => p[1]);
    const box = [r4(Math.min(...xs) / shown[0]), r4(Math.min(...ys) / shown[1]), r4(Math.max(...xs) / shown[0]), r4(Math.max(...ys) / shown[1])];
    let flat = null;
    if (d) {
      const pm = protoMask(d, frame);
      const pts = traceWing((x, y) => maskLogits(d, frame, pm, x, y), d.box, { width: W, height: H, threshold: cfg.maskThreshold ?? 0 });
      if (pts?.length >= 3) {
        const q = quantize(toShown(pts, result.turnK, upright, result.size).points, shown[0], shown[1]);
        if (q.length >= 6) flat = q;
      }
    }
    return { box, score: d ? r4(d.score) : null, outline: [kindCode(w.kind), halfCode(w.half), flat] };
  });
}
