// What the wing chain's readings of a butterfly's photos say together (server/wing-reasons.mjs): the rows of
// photo_wing_sides the background readings write, as the 18 Oct 2026 bundles worked them out.
import test from 'node:test';
import assert from 'node:assert/strict';
import { wingRows } from '../server/wing-reasons.mjs';

test("a butterfly's photos together: the pair's check, a single photo, the turn trusted", () => {
  const reading = (side, p, extra = {}) => ({
    side,
    p,
    fore: { side, p },
    hind: { side, p },
    wings: [1, 2, 3, 4].map(i => ({ box: [0.1 * i, 0.1, 0.1 * i + 0.05, 0.2], kind: i < 3 ? 'forewing' : 'hindwing', half: i % 2 ? 'left' : 'right', pDorsal: p, pForewing: 0.5, score: 0.9 })),
    missing: [],
    turnK: 0,
    pTurn: 1,
    size: [1600, 1067],
    ...extra,
  });
  // Named d and v the other way round.
  const swapped = wingRows('CAM000301', [
    { name: 'CAM000301d', fileId: 'f1', reading: reading('ventral', 0.01) },
    { name: 'CAM000301v', fileId: 'f2', reading: reading('dorsal', 0.99, { turnK: 2, pTurn: 0.97 }) },
  ]);
  assert.deepEqual(swapped.rows.map(r => [r.pair, r.camReasons, r.review]), [
    ['suffix swapped', 'suffix-swapped', true],
    ['suffix swapped', 'suffix-swapped', true],
  ]);
  assert.equal(swapped.rows[1].reasons, 'side-disagrees-with-suffix(p=0.99); orientation:upside down(p=0.97)');
  assert.deepEqual([swapped.rows[1].turn, swapped.rows[1].degrees], ['upside down', 180]);
  assert.equal(swapped.boxes.length, 2);
  const fine = wingRows('CAM000302', [
    { name: 'CAM000302d', fileId: 'f3', reading: reading('dorsal', 0.99) },
    { name: 'CAM000302v', fileId: 'f4', reading: reading('ventral', 0.01) },
  ]);
  assert.deepEqual(fine.rows.map(r => [r.pair, r.reasons, r.camReasons, r.review]), [
    [undefined, undefined, undefined, false],
    [undefined, undefined, undefined, false],
  ]);
  const lone = wingRows('CAM000303', [{ name: 'CAM000303d', fileId: 'f5', reading: reading('dorsal', 0.99) }, { name: 'CAM000303v', fileId: 'f6', error: 'Drive answered 404' }]);
  assert.equal(lone.rows[0].camReasons, 'suffix-missing:no v; single-photo');
  assert.deepEqual(lone.rows[1], { name: 'CAM000303v', fileId: 'f6', cam: 'CAM000303', suffix: 'v', folder: null, source: undefined, model: undefined, error: 'Drive answered 404', review: false });
  // Whole-specimen photos (v1, v2…): no side judged.
  const lateral = wingRows('CAM000304', [{ name: 'CAM000304v1', fileId: 'f7', reading: reading('dorsal', 0.99) }]);
  assert.equal(lateral.rows[0].side, null);
});
