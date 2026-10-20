import test from 'node:test';
import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';

// The sex model's inputs (tools/identifier/sex_crops.py): the 19 Oct 2026 CNN's wing boxes from the nano chain and
// its 4 androconial strips as its training made them. Needs Python with Pillow and numpy.

const TOOLS = new URL('../tools/identifier', import.meta.url).pathname;
const python = process.env.PYTHON ?? 'python3';
const hasPillow = spawnSync(python, ['-I', '-c', 'import PIL'], { encoding: 'utf8' }).status === 0;

const CNN_CHECK = new URL('./fixtures/sex-cnn-check.py', import.meta.url).pathname;
const hasNumpy = hasPillow && spawnSync(python, ['-I', '-c', 'import numpy'], { encoding: 'utf8' }).status === 0;

test('the CNN sex model: wing boxes from the nano chain, the strips and their inputs as its training made them', { skip: !hasNumpy && 'no Python with Pillow and numpy' }, () => {
  const run = spawnSync(python, ['-I', CNN_CHECK, TOOLS], { encoding: 'utf8', timeout: 120_000 });
  assert.equal(run.status, 0, run.stderr);
  const out = JSON.parse(run.stdout.trim().split('\n').pop());
  // Per role the best-scoring wing of that kind and half; its outline's box, or its own box without an outline.
  assert.deepEqual(out.boxes, {
    vf_l: [0.09, 0.11, 0.44, 0.47],
    vf_r: [0.5, 0.1, 0.6, 0.2],
    dh_l: [0.13, 0.53, 0.43, 0.85],
    dh_r: [0.57, 0.54, 0.87, 0.84],
  });
  assert.deepEqual(out.errors, ['v:no_wing_reading', 'd:turned_photo', 'd:hindwing_halves_r', 'v:turned_photo']);
  // The released model's 4 androconial strips: the two tiles' pixels unsplit, letterboxed into 112 x 336 as in training.
  assert.equal(out.same_strips, true, 'cut as build_w07_strips.py cut the training strips');
  assert.equal(out.strip_tiles, true, 'a strip is its wing\'s two tiles side by side');
  assert.equal(out.same_strip_inputs, true);
  assert.deepEqual(out.strip_input, [[4, 112, 336, 3], 'uint8']);
  // The crop-dropout models: whatever wings are there; nothing only when no wing is.
  assert.deepEqual(out.partial, ['dh_r', 'vf_l', 'vf_r']);
  assert.deepEqual(out.partial_turned, ['vf_l', 'vf_r']);
  assert.equal(out.partial_none, 'no_wings (v:no_wing_reading, d:turned_photo)');
  assert.deepEqual(out.partial_roles, ['vf_l_s', 'vf_r_s', 'dh_r_s']);
  assert.equal(out.unknown_role, 'unknown sex crop vf_l_t0');
});

test('the readings name the sex model the worker says it ran and how many strips it had', async () => {
  const { rowsOfAnswer } = await import('../server/readings.mjs');
  const db = { prepare: () => ({ all: () => [] }) };
  const cam = { cam: 'CAM000201', side: 'East', species: 'Mechanitis polymnia', photos: [], pending: [] };
  const answer = sex => ({ ident: { photos: {}, combined: { species: [['Mechanitis polymnia', 0.9]], genus: [], subspecies: [], used: [] }, sex } });
  const arm = 'release_20261019 cnn_strips4: MobileNetV3-Large fine-tuned on the 4 androconial strips';
  const cnn = rowsOfAnswer(db, cam, answer({ pMale: 0.1, status: 'species_supported', supported: true, model: arm, kind: 'cnn', crops: 3 })).prediction;
  assert.equal(cnn.sexModel, 'collection sex release_20261019 cnn_strips4 (3 of 4 androconial strips of the nano wing boxes, MobileNetV3, cand_cv fold models)');
  assert.equal(cnn.sex, 'female');
  const unscored = rowsOfAnswer(db, cam, answer({ status: 'not_scored:no_wings' })).prediction;
  assert.match(unscored.sexModel, /^collection sex release_20261019 cnn_strips4 /);
  assert.equal(unscored.sexStatus, 'not_scored:no_wings');
});
