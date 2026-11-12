import test from 'node:test';
import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { readFileSync } from 'node:fs';

// The general sex line's server piece (tools/identifier/field_sex.py) and its pass-through
// (server/identify-photos.mjs fieldSexBlock). Made-up model folders, no model files, no AI. Needs Python with numpy.

const TOOLS = new URL('../tools/identifier', import.meta.url).pathname;
const CHECK = new URL('./fixtures/field-sex-check.py', import.meta.url).pathname;
const DISPLAY = new URL('./fixtures/field-sex-display.json', import.meta.url).pathname;
const python = process.env.PYTHON ?? 'python3';
const hasNumpy = spawnSync(python, ['-I', '-c', 'import numpy'], { encoding: 'utf8' }).status === 0;

let memo = null;
function check() {
  if (memo) return memo;
  const run = spawnSync(python, ['-I', CHECK, TOOLS, DISPLAY], { encoding: 'utf8', timeout: 120_000 });
  assert.equal(run.status, 0, run.stderr);
  return (memo = JSON.parse(run.stdout.trim().split('\n').pop()));
}
const skip = !hasNumpy && 'no Python with numpy';
const CHECK2 = new URL('./fixtures/field-sex-v2-check.py', import.meta.url).pathname;
const CALIBRATION = new URL('./fixtures/field-sex-calibration.json', import.meta.url).pathname;
let memo2 = null;
function check2() {
  if (memo2) return memo2;
  const run = spawnSync(python, ['-I', CHECK2, TOOLS, DISPLAY, CALIBRATION], { encoding: 'utf8', timeout: 120_000 });
  assert.equal(run.status, 0, run.stderr);
  return (memo2 = JSON.parse(run.stdout.trim().split('\n').pop()));
}

test('all species (field-sex/2): the verified species gives the same block as in a six-species folder', { skip }, () => {
  const out = check2();
  assert.deepEqual(out.verified_same_as_v1, [120, 120]);
  assert.equal(out.verified_flag, true);
});

test('all species: the kNN pool is the species, its genus under 5 photos, or none (the prior)', { skip }, () => {
  const out = check2();
  assert.ok(out.knn_species_diff < 1e-6, String(out.knn_species_diff));
  assert.ok(out.knn_genus_diff < 1e-6, String(out.knn_genus_diff));
  assert.equal(out.knn_empty, 0.3);
});

test('all species: an estimated species gets the level table\'s map, its level and a % inside the band too, never over its cap nor 95', { skip }, () => {
  const out = check2();
  const c = out.estimate_checks;
  assert.equal(c.estimates, 160);
  for (const k of ['over_cap', 'over_95', 'pct_mismatch', 'marker_mismatch', 'sure_mismatch']) assert.equal(c[k], 0, k);
  assert.equal(c.inside_band_with_pct, 160, 'made-up photos fall in the band and still get their %');
  assert.equal(out.estimate_block.status, 'estimate');
  assert.equal(out.estimate_block.verified, false);
  assert.equal(out.estimate_block.level, 'species');
  assert.equal(out.estimate_block.trainPhotos, 30);
  assert.equal(out.rare_block.level, 'family');
  assert.equal(out.solo_block.level, 'global');
  assert.equal(out.alias_block_species, 'Xx hh', 'the block names the species as the page does');
  assert.equal(out.cap_max, 0.95);
  assert.deepEqual(out.strong_capped, [[0.95, 'female', 0.95], [0.05, 'male', 0.95]], 'a cap of 0.99 in the table still shows 95');
  // species unsure or no wings: the bar, no %
  assert.equal(out.estimate_unsure.status, 'species_unsure');
  assert.equal(out.estimate_unsure.pct, undefined);
  assert.equal(out.estimate_no_wings.status, 'no_wings');
  assert.equal(out.estimate_no_wings.pct, undefined);
  assert.equal(out.outside, null, 'a species outside the folder (the 13 Ithomiini genera are left out): no block');
  assert.match(out.missing_row, /no calibration row/);
});

test('all species: the real level table keeps every % within 50 to 95, and rare species (1 to 4 photos) inside the band', { skip }, () => {
  const out = check2();
  assert.equal(out.real.species, 1261);
  assert.deepEqual(out.real.levels, { family: 918, species: 136, global: 28, genus: 179 });
  assert.equal(out.real.n_bad, 0, JSON.stringify(out.real.bad));
  assert.equal(out.real.b1_sure, 0);
  assert.ok(out.real.b1_max_pct <= 84, String(out.real.b1_max_pct));
});

test('field sex (six-species folder): only the passing species get a block; the alias maps the head\'s name to the model\'s', { skip }, () => {
  const out = check();
  assert.equal(out.outside, null);
  assert.equal(out.model_name_alias, 'Aa bb');
  assert.equal(out.model_name_plain, 'Aa bb');
  assert.equal(out.alias.status, 'call');
  assert.equal(out.alias.species, 'Xx bb', 'the block names the species as the page does');
  assert.equal(out.alias.sex, 'female');
});

test('field sex: every photo of the six gets a bar position; only a Supported answer outside the band gets a %', { skip }, () => {
  const out = check();
  // under the species gate: the bar and the side, the species note, no %
  assert.equal(out.gate_0799.status, 'species_unsure');
  assert.equal(out.gate_0799.supported, false);
  assert.equal(out.gate_0799.sex, 'female');
  assert.equal(out.gate_0799.sure, true);
  assert.ok(out.gate_0799.p > 0.85 && out.gate_0799.p < 1);
  assert.equal(out.gate_0799.pct, undefined);
  // at the gate: the call and its %
  assert.equal(out.gate_08.status, 'call');
  assert.equal(out.gate_08.supported, true);
  assert.equal(typeof out.gate_08.pct, 'number');
  assert.equal(out.gate_08.p, out.gate_0799.p, 'the same photo, the same bar position');
  // no wings: bar, no %
  assert.equal(out.no_wings.status, 'no_wings');
  assert.equal(out.no_wings.pct, undefined);
  // inside the «no seguro» band (confidence under 0.85): leaning side, no %
  assert.equal(out.not_supported.status, 'band');
  assert.equal(out.not_supported.sure, false);
  assert.equal(out.not_supported.supported, false);
  assert.ok(out.not_supported.p > 0.15 && out.not_supported.p < 0.85);
  assert.equal(out.not_supported.sex, out.not_supported.p >= 0.5 ? 'female' : 'male');
  assert.equal(out.not_supported.pct, undefined);
});

test('field sex: a species x sex cell under 90 % gets the bar and marker but no call and no %', { skip }, () => {
  const out = check();
  assert.equal(out.suppressed.status, 'cell_under_90');
  assert.equal(out.suppressed.supported, true);
  assert.equal(out.suppressed.sex, 'male', 'the side the marker is on');
  assert.equal(out.suppressed.pct, undefined);
  assert.equal(out.max_pct_per_row['Cc dd|male'], null);
});

test('field sex: the % shown never exceeds the table\'s cap nor 99, rounded down', { skip }, () => {
  const out = check();
  assert.equal(out.max_pct_per_row['Aa bb|female'], 96);
  assert.equal(out.max_pct_per_row['Aa bb|male'], 90);
  assert.equal(out.max_pct_per_row['Cc dd|female'], 99);
  assert.equal(out.pct_at_cap_099, 99, '0.99 shows 99, not 98');
  // the real table (WingsClassificator reports/general_sex_20261111/BUILD.md)
  assert.deepEqual(out.real_six, ['Danaus plexippus', 'Hypolimnas bolina', 'Hypolimnas misippus', 'Papilio polyxenes', 'Papilio troilus', 'Poanes zabulon']);
  const display = JSON.parse(readFileSync(DISPLAY, 'utf8'));
  assert.equal(display.table.length, 12);
  for (const [key, r] of Object.entries(out.real)) {
    if (!r.shown) {
      assert.equal(r.max_pct, null, key);
      continue;
    }
    assert.equal(r.any_none, false, key);
    assert.ok(r.max_pct <= Math.floor(100 * r.cap + 1e-9) && r.max_pct <= 99, `${key}: ${r.max_pct} over cap ${r.cap}`);
  }
  assert.ok(out.real['Papilio polyxenes|male'].max_pct <= 92);
  // under the 90 % floor: no call, whatever the row says
  assert.deepEqual(out.low_map, { '0.85': null, '0.9': null, '0.94': null, '0.95': 90, '1.0': 99 });
  assert.deepEqual(out.low_map_floor_05, { '0.85': null, '0.94': null, '0.95': 90 }, 'a table floor under 90 % does not lower it');
});

test('field sex: ADAPT + kNN equal the training code\'s formulas; a changed file is refused', { skip }, () => {
  const out = check();
  assert.ok(out.ens_max_diff < 1e-6, String(out.ens_max_diff));
  assert.match(out.tamper, /SHA-256 differs/);
});

test('the worker gates field sex on the species score WITHOUT the geographic prior, whatever side was chosen', { skip }, async () => {
  const { mkdtempSync, readFileSync: read } = await import('node:fs');
  const { tmpdir } = await import('node:os');
  const { join } = await import('node:path');
  const out = join(mkdtempSync(join(tmpdir(), 'field-sex-gate-')), 'out.json');
  const run = spawnSync(python, ['-I', new URL('./fixtures/field-sex-gate-check.py', import.meta.url).pathname, TOOLS, out], { encoding: 'utf8', timeout: 60_000 });
  assert.equal(run.status, 0, run.stderr);
  const r = JSON.parse(read(out, 'utf8'));
  // East: the prior takes the top species down (the page names Cc dd)
  assert.equal(r.east_top[0], 'Cc dd');
  assert.equal(r.none_top[0], 'Aa bb');
  assert.equal(r.east_prior, 'East');
  assert.deepEqual(r.noprior, [0.85, 0.1, 0]);
  // the block is for the species the page names (Cc dd with East), gated on ITS no-prior score (0.10, so unsure);
  // with no side, Aa bb at 0.85; the crop status goes with it
  assert.deepEqual(r.seen, [['Cc dd', 0.1, 'success'], ['Aa bb', 0.85, 'centre_crop_fallback']]);
  assert.equal(r.not_six, null, 'a species outside the six: no block');
  assert.equal(r.no_model_block, null, 'no folder: no block');
});

test('the worker starts and answers without field_sex.py, and loads no field sex model then', { skip }, async () => {
  const { mkdtempSync, copyFileSync, mkdirSync, writeFileSync } = await import('node:fs');
  const { tmpdir } = await import('node:os');
  const { join } = await import('node:path');
  const dir = mkdtempSync(join(tmpdir(), 'worker-no-field-sex-'));
  for (const f of ['worker.py', 'sex_crops.py']) copyFileSync(join(TOOLS, f), join(dir, f));
  const root = join(dir, 'ident');
  mkdirSync(join(root, 'field_sex'), { recursive: true });
  // the module loads (no import of field_sex at the top) and answers a ping on its protocol
  const ping = spawnSync(python, ['-I', '-B', join(dir, 'worker.py'), '--dir', root, '--no-pin', '--idle', '5'], { input: '{"id":"1","op":"ping"}\n', encoding: 'utf8', timeout: 60_000 });
  const lines = ping.stdout.trim().split('\n').map(l => JSON.parse(l));
  assert.deepEqual(lines.slice(0, 2), [{ ready: true, pid: lines[0].pid }, { id: '1', loaded: false }], ping.stderr);
  // a field_sex folder without field_sex.py: no model, a message, no failure
  const check = join(dir, 'check.py');
  writeFileSync(check, `import json, sys\nsys.path.insert(0, ${JSON.stringify(dir)})\nsys.argv = ["worker.py", "--dir", "/x", "--no-pin"]\nimport worker\nr = [worker.load_field_sex(${JSON.stringify(root)}) is None, worker.load_field_sex(${JSON.stringify(dir)}) is None]\nopen(${JSON.stringify(join(dir, 'out.json'))}, "w").write(json.dumps(r))\n`);
  const run = spawnSync(python, ['-I', '-B', check], { encoding: 'utf8', timeout: 60_000 });
  assert.equal(run.status, 0, run.stderr);
  assert.deepEqual(JSON.parse(readFileSync(join(dir, 'out.json'), 'utf8')), [true, true]);
  assert.match(run.stderr, /Field sex model not loaded: ModuleNotFoundError/);
});

test('identifyUpload passes a cleaned fieldSex only when the worker gives one', async () => {
  const { identifyUpload, fieldSexBlock } = await import('../server/identify-photos.mjs');
  const { mkdtempSync } = await import('node:fs');
  const { tmpdir } = await import('node:os');
  const { join } = await import('node:path');
  const scratch = mkdtempSync(join(tmpdir(), 'field-sex-upload-'));
  const jpeg = Buffer.from([0xff, 0xd8, 0xff, 0xe0, 1, 2, 3]).toString('base64');
  const base = { species: [['Danaus plexippus', 0.97]], subspecies: [], genus: [['Danaus', 0.99]], crop: 'success', wingBox: null, prior: null, ms: 9 };
  const call = { species: 'Danaus plexippus', status: 'call', sex: 'male', pct: 97, p: 0.021, pFemale: 0.02134, sure: true, supported: true, gateP: 0.97, version: 'field-sex/1' };
  const withBlock = await identifyUpload({ identifier: { identify: async () => ({ ...base, fieldSex: call }) }, body: { photo: jpeg }, scratch });
  assert.deepEqual(withBlock.body.fieldSex, { species: 'Danaus plexippus', status: 'call', p: 0.021, sex: 'male', sure: true, pct: 97, version: 'field-sex/1' });
  // an Ithomiini (no block, or null): the answer has no fieldSex key at all, as before
  for (const fieldSex of [undefined, null]) {
    const r = await identifyUpload({ identifier: { identify: async () => ({ ...base, species: [['Mechanitis messenoides', 0.9]], fieldSex }) }, body: { photo: jpeg }, scratch });
    assert.equal('fieldSex' in r.body, false);
  }
  // a block that is not a call carries the bar position and side but no %; an odd one is dropped or loses its %
  assert.deepEqual(fieldSexBlock({ species: 'Danaus plexippus', status: 'band', p: 0.4567, sex: 'male', sure: false, pct: 97 }),
    { species: 'Danaus plexippus', status: 'band', p: 0.457, sex: 'male', sure: false, version: null });
  assert.equal(fieldSexBlock({ species: 'Danaus plexippus', status: 'error:ValueError' }), null);
  assert.equal(fieldSexBlock({ species: 'Danaus plexippus', status: 'band', p: 1.2, sex: 'male' }), null);
  assert.equal(fieldSexBlock({ species: 'Danaus plexippus', status: 'band', p: 0.5, sex: 'other' }), null);
  assert.equal(fieldSexBlock({ species: 'Danaus plexippus', status: 'call', p: 0.02, sex: 'male', sure: true, pct: 100 }).status, 'cell_under_90');
  assert.equal(fieldSexBlock({ species: 'Danaus plexippus', status: 'call', p: 0.02, sex: 'male', sure: true, pct: 89 }).pct, undefined);
});

test('fieldSexBlock: an estimated block keeps its level and training photos, and a % of 50 to 95 only for an estimate', async () => {
  const { fieldSexBlock } = await import('../server/identify-photos.mjs');
  const est = { species: 'Danaus gilippus', status: 'estimate', p: 0.3123, pFemale: 0.2, sex: 'male', sure: false, pct: 68, verified: false, level: 'species', trainPhotos: 4489, gateP: 0.97, version: 'field-sex/2' };
  assert.deepEqual(fieldSexBlock(est), { species: 'Danaus gilippus', status: 'estimate', p: 0.312, sex: 'male', sure: false, version: 'field-sex/2', verified: false, level: 'species', trainPhotos: 4489, pct: 68 });
  // out of range: the bar stays, the % goes
  for (const pct of [96, 99, 49, 62.5, null]) assert.equal(fieldSexBlock({ ...est, pct }).pct, undefined, String(pct));
  assert.equal(fieldSexBlock({ ...est, pct: 96 }).status, 'estimate');
  // species unsure or no wings: never a %
  assert.equal(fieldSexBlock({ ...est, status: 'species_unsure' }).pct, undefined);
  assert.equal(fieldSexBlock({ ...est, status: 'no_wings' }).pct, undefined);
  // a verified status on an estimated block, an odd level: dropped
  assert.equal(fieldSexBlock({ ...est, status: 'call' }), null);
  assert.equal(fieldSexBlock({ ...est, level: 'order' }), null);
  assert.equal(fieldSexBlock({ ...est, trainPhotos: -3 }).trainPhotos, null);
  // a verified block is cleaned exactly as before (no new keys)
  assert.deepEqual(Object.keys(fieldSexBlock({ species: 'Danaus plexippus', status: 'call', p: 0.02, sex: 'male', sure: true, pct: 97, verified: true, level: 'species' })).sort(),
    ['p', 'pct', 'sex', 'species', 'status', 'sure', 'version']);
});
