// The wing chain's masks outside the chain (server/wing-masks.mjs): each wing analysePhoto returns matched to its
// detection, its score and its outline (server/wing-outlines.mjs) in the photo as shown, on made-up model outputs.
import test from 'node:test';
import assert from 'node:assert/strict';
import { letterboxOf, wingShapes } from '../server/wing-masks.mjs';

/** A segmentation output: one wing whose mask (prototype 0) covers letterbox cells [cx0, cx1) x [cy0, cy1). */
function segmentation({ W = 128, H = 96, cells = [4, 3, 12, 9], conf = 0.9 } = {}) {
  const S = 64;
  const P = 16;
  const proto = new Float32Array(32 * P * P);
  for (let j = 0; j < P; j++) for (let i = 0; i < P; i++) proto[j * P + i] = i >= cells[0] && i < cells[2] && j >= cells[1] && j < cells[3] ? 5 : -5;
  const k = S / P;
  const { r, px, py } = letterboxOf(W, H, S);
  const det = new Float32Array(2 * 38);
  // Its box (a little wider than its mask) in the letterbox; a second detection under the threshold.
  det.set([cells[0] * k - 2, cells[1] * k - 2, cells[2] * k + 2, cells[3] * k + 2, conf, 0, 1], 0);
  det.set([0, 0, 10, 10, 0.05, 0, 1], 38);
  const box = [(cells[0] * k - 2 - px) / r, (cells[1] * k - 2 - py) / r, (cells[2] * k + 2 - px) / r, (cells[3] * k + 2 - py) / r].map(Math.round);
  const edge = [(cells[0] * k - px) / r, (cells[1] * k - py) / r, (cells[2] * k - px) / r, (cells[3] * k - py) / r];
  return { seg: { det, detDims: [1, 2, 38], proto, protoDims: [1, 32, P, P], S }, box, edge };
}
const CFG = { conf: 0.1, nmsIou: 0.6, maxWings: 8, maskThreshold: 0 };
const xsOf = flat => flat.filter((_, i) => i % 2 === 0);
const ysOf = flat => flat.filter((_, i) => i % 2 === 1);

test("a wing's outline: its mask's edge, in thousandths of the photo as shown, with its detection's score", () => {
  const { seg, box, edge } = segmentation();
  const [wing] = wingShapes(seg, { size: [128, 96], turnK: 0, wings: [{ box, kind: 'forewing', half: 'left' }] }, CFG);
  assert.deepEqual(wing.outline.slice(0, 2), ['fw', 'l']);
  assert.equal(wing.score, 0.9);
  const flat = wing.outline[2];
  const xs = xsOf(flat).map(v => (v / 1000) * 128);
  const ys = ysOf(flat).map(v => (v / 1000) * 96);
  // The mask's edge, within a grid cell (128 / 400 px) and the rounding to thousandths.
  for (const [got, want] of [[Math.min(...xs), edge[0]], [Math.max(...xs), edge[2]], [Math.min(...ys), edge[1]], [Math.max(...ys), edge[3]]])
    assert.ok(Math.abs(got - want) <= 1, `${got} vs ${want}`);
  assert.ok(flat.length / 2 <= 32 && flat.length / 2 >= 4);
  assert.deepEqual(wing.box, box.map((v, i) => Math.round((v / (i % 2 ? 96 : 128)) * 10000) / 10000));
  // A wing the detections do not have (another box): no outline, no score.
  const [other] = wingShapes(seg, { size: [128, 96], turnK: 0, wings: [{ box: [1, 1, 5, 5], kind: 'extra', half: 'unknown' }] }, CFG);
  assert.deepEqual([other.outline, other.score], [['x', 'u', null], null]);
});

test('a photo the chain checked again turned upright: box and outline turned back to the photo as shown', () => {
  // Shown 96 x 128 (portrait), found turned a quarter (turnK 1) and checked as rotate(img, 270): 128 x 96.
  const { seg, box, edge } = segmentation();
  const [wing] = wingShapes(seg, { size: [128, 96], turnK: 1, checkedUpright: true, wings: [{ box, kind: 'hindwing', half: 'right' }] }, CFG);
  // A point (x, y) of the checked frame is (y, 128 - x) of the photo as shown (96 wide, 128 high).
  const flat = wing.outline[2];
  const xs = xsOf(flat).map(v => (v / 1000) * 96);
  const ys = ysOf(flat).map(v => (v / 1000) * 128);
  assert.ok(Math.abs(Math.min(...xs) - edge[1]) <= 1 && Math.abs(Math.max(...xs) - edge[3]) <= 1);
  assert.ok(Math.abs(Math.min(...ys) - (128 - edge[2])) <= 1 && Math.abs(Math.max(...ys) - (128 - edge[0])) <= 1);
  assert.deepEqual(wing.box, [box[1] / 96, (128 - box[2]) / 128, box[3] / 96, (128 - box[0]) / 128].map(v => Math.round(v * 10000) / 10000));
});
