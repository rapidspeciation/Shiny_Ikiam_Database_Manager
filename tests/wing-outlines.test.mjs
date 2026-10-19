import { test } from 'node:test';
import assert from 'node:assert/strict';
import {
  traceWing,
  toShown,
  quantize,
  outlinePath,
  parsePath,
  zoomBox,
  photoOutlines,
  simplifyClosed,
  MAX_POINTS,
} from '../server/wing-outlines.mjs';

// an ellipse of mask logits inside a box, plus a far-away speck that must be dropped (largest part only)
const ellipse = (cx, cy, rx, ry, speck) => (xs, ys) => {
  const out = new Float32Array(xs.length * ys.length);
  for (let j = 0; j < ys.length; j++)
    for (let i = 0; i < xs.length; i++) {
      const e = ((xs[i] - cx) / rx) ** 2 + ((ys[j] - cy) / ry) ** 2;
      const s = speck && Math.hypot(xs[i] - speck[0], ys[j] - speck[1]) < 12;
      out[j * xs.length + i] = s ? 5 : 1 - e;
    }
  return out;
};

test('an ellipse gives a smooth closed outline of at most 64 points on the ellipse', () => {
  const W = 1600,
    H = 1200;
  const pts = traceWing(ellipse(500, 400, 200, 120, [320, 300]), [280, 270, 720, 530], { width: W, height: H });
  assert.ok(pts.length >= 8 && pts.length <= MAX_POINTS, `points ${pts.length}`);
  for (const [x, y] of pts) {
    const e = Math.sqrt(((x - 500) / 200) ** 2 + ((y - 400) / 120) ** 2);
    assert.ok(Math.abs(e - 1) < 0.015, `point ${x},${y} off the ellipse (${e})`);
  }
  const xs = pts.map(p => p[0]);
  assert.ok(Math.min(...xs) > 290, 'the speck is not part of the outline');
  // unsimplified: every point of the iso-line within a fraction of a sample of the ellipse (no staircase)
  const raw = traceWing(ellipse(500, 400, 200, 120), [280, 270, 720, 530], { width: W, height: H, tolerance: 0 });
  assert.ok(raw.length > 200);
  for (const [x, y] of raw) {
    // distance to the ellipse along the radius, in px
    const e = Math.sqrt(((x - 500) / 200) ** 2 + ((y - 400) / 120) ** 2);
    assert.ok(Math.abs(e - 1) * 120 < 0.6, `raw point ${x},${y} ${(Math.abs(e - 1) * 120).toFixed(2)} px off`);
  }
});

test('the iso-line keeps a notch the grid samples see', () => {
  // a rectangle with a 30 px deep notch from the top
  const sample = (xs, ys) => {
    const out = new Float32Array(xs.length * ys.length);
    for (let j = 0; j < ys.length; j++)
      for (let i = 0; i < xs.length; i++) {
        const x = xs[i], y = ys[j];
        const inRect = Math.min(x - 100, 400 - x, y - 100, 300 - y);
        const notch = Math.min(x - 240, 260 - x, 130 - y);
        out[j * xs.length + i] = Math.min(inRect, -notch) / 10;
      }
    return out;
  };
  const pts = traceWing(sample, [90, 90, 410, 310], { width: 1600, height: 1200 });
  const inNotch = pts.filter(([x, y]) => x > 238 && x < 262 && y > 125 && y < 135);
  assert.ok(inNotch.length >= 2, 'the notch bottom has points');
});

test('simplify keeps squares as 4 corners and caps the points', () => {
  const sq = [];
  for (let i = 0; i < 100; i++) sq.push([i, 0]);
  for (let i = 0; i < 100; i++) sq.push([100, i]);
  for (let i = 100; i > 0; i--) sq.push([i, 100]);
  for (let i = 100; i > 0; i--) sq.push([0, i]);
  assert.equal(simplifyClosed(sq, 1).length, 4);
  const circle = Array.from({ length: 1000 }, (_, n) => [Math.cos(n / 159.15) * 500, Math.sin(n / 159.15) * 500]);
  assert.ok(simplifyClosed(circle, 0.01).length <= MAX_POINTS);
});

test('points of a photo checked upright go back to the photo as shown', () => {
  // shown photo 1200 x 1600 (portrait) turned 90 deg counter-clockwise (turnK 1): the chain checks it
  // rotated 270 ccw (1600 x 1200). A shown point (x, y) is (H - y, x) in the checked frame.
  const W = 1200,
    H = 1600;
  const shownPt = [300, 100];
  const checked = [H - shownPt[1], shownPt[0]];
  const t = toShown([checked], 1, true, [H, W]);
  assert.deepEqual(t.size, [W, H]);
  assert.deepEqual(t.points[0].map(Math.round), shownPt);
  // turnK 3: checked rotate(img, 90): (x, y) -> (y, W - x)
  const c3 = [shownPt[1], W - shownPt[0]];
  const t3 = toShown([c3], 3, true, [H, W]);
  assert.deepEqual(t3.points[0].map(Math.round), shownPt);
  assert.deepEqual(toShown([[1, 2]], 2, false, [10, 20]).points, [[1, 2]]);
});

test('quantize, path and the zoom box', () => {
  const flat = quantize(
    [
      [0, 0],
      [800, 0],
      [800, 600],
      [800.2, 600.1],
    ],
    1600,
    1200,
  );
  assert.deepEqual(flat, [0, 0, 500, 0, 500, 500]);
  assert.equal(outlinePath(flat), 'm0 0l500 0 0 500z');
  assert.equal(outlinePath([108, 177, 152, 170, 150, 190]), 'm108 177l44-7-2 20z');
  assert.deepEqual(parsePath('m108 177l44-7-2 20z'), [108, 177, 152, 170, 150, 190]);
  assert.deepEqual(parsePath(outlinePath(flat)), flat);
  assert.deepEqual(parsePath('M0 0 500 0 500 500Z'), flat);
  const box = zoomBox(
    [
      { kind: 'fw', points: [[100, 100], [500, 300]] },
      { kind: 'x', points: [[0, 0], [1600, 1200]] },
    ],
    1600,
    1200,
  );
  // 1.5 % of 1200 = 18 px
  assert.deepEqual(box, [0.0513, 0.0683, 0.3238, 0.265]);
  const r = photoOutlines({
    turnK: 0,
    size: [1600, 1200],
    wings: [
      { kind: 'forewing', half: 'left', score: 0.91, points: [[100, 100], [500, 100], [500, 300]] },
      { kind: 'extra', half: 'unknown', score: 0.3, points: [[0, 0], [1600, 0], [1600, 1200]] },
      { kind: 'hindwing', half: 'right', score: 0.8, points: null },
    ],
  });
  assert.equal(r.w.length, 2);
  assert.deepEqual(r.w[0].slice(0, 2), ['fw', 'l']);
  assert.deepEqual(r.w[1].slice(0, 2), ['x', 'u']);
  assert.equal(r.conf, 0.91);
  assert.deepEqual(r.box, [0.0513, 0.0683, 0.3238, 0.265]);
});
