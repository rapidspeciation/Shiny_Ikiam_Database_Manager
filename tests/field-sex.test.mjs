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

test('field sex: only the passing species get a block; the alias maps the head\'s name to the model\'s', { skip }, () => {
  const out = check();
  assert.equal(out.outside, null);
  assert.equal(out.model_name_alias, 'Aa bb');
  assert.equal(out.model_name_plain, 'Aa bb');
  assert.equal(out.alias.status, 'call');
  assert.equal(out.alias.species, 'Xx bb', 'the block names the species as the page does');
  assert.equal(out.alias.sex, 'female');
});

test('field sex: the gate is >= 0.8 on the given (no-prior) species score; wings must be found', { skip }, () => {
  const out = check();
  assert.equal(out.gate_0799.status, 'below_gate');
  assert.equal(out.gate_0799.sex, undefined);
  assert.equal(out.gate_08.status, 'call');
  assert.equal(out.no_wings.status, 'no_wings');
  assert.equal(out.no_wings.sex, undefined);
  assert.equal(out.not_supported.status, 'not_supported');
  assert.equal(out.not_supported.supported, false);
});

test('field sex: a suppressed species x called sex gives no call (not the other sex either)', { skip }, () => {
  const out = check();
  assert.equal(out.suppressed.status, 'suppressed');
  assert.equal(out.suppressed.sex, undefined);
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
  // East: the prior takes the top species down (the page would name Cc dd) ...
  assert.equal(r.east_top[0], 'Cc dd');
  assert.equal(r.east_prior, 'East');
  // ... but the sex line is given the no-prior top species and its own score
  assert.deepEqual(r.noprior, ['Aa bb', 0.85]);
  assert.deepEqual(r.seen_success, [['Aa bb', 0.85, 'success']]);
  assert.deepEqual(r.seen, [['Aa bb', 0.85, 'centre_crop_fallback']], 'the crop status goes with it');
  assert.equal(r.no_model_block, null, 'no folder: no block');
});

test('identifyUpload passes a cleaned fieldSex only when the worker gives one', async () => {
  const { identifyUpload, fieldSexBlock } = await import('../server/identify-photos.mjs');
  const { mkdtempSync } = await import('node:fs');
  const { tmpdir } = await import('node:os');
  const { join } = await import('node:path');
  const scratch = mkdtempSync(join(tmpdir(), 'field-sex-upload-'));
  const jpeg = Buffer.from([0xff, 0xd8, 0xff, 0xe0, 1, 2, 3]).toString('base64');
  const base = { species: [['Danaus plexippus', 0.97]], subspecies: [], genus: [['Danaus', 0.99]], crop: 'success', wingBox: null, prior: null, ms: 9 };
  const call = { species: 'Danaus plexippus', status: 'call', sex: 'male', pct: 97, pFemale: 0.02, conf: 0.98, gateP: 0.97, version: 'field-sex/1' };
  const withBlock = await identifyUpload({ identifier: { identify: async () => ({ ...base, fieldSex: call }) }, body: { photo: jpeg }, scratch });
  assert.deepEqual(withBlock.body.fieldSex, { species: 'Danaus plexippus', status: 'call', sex: 'male', pct: 97, version: 'field-sex/1' });
  // an Ithomiini (no block, or null): the answer has no fieldSex key at all, as before
  for (const fieldSex of [undefined, null]) {
    const r = await identifyUpload({ identifier: { identify: async () => ({ ...base, species: [['Mechanitis messenoides', 0.9]], fieldSex }) }, body: { photo: jpeg }, scratch });
    assert.equal('fieldSex' in r.body, false);
  }
  // a block that is not a call carries no sex; an odd one is dropped or made not supported
  assert.deepEqual(fieldSexBlock({ species: 'Danaus plexippus', status: 'suppressed', sex: 'male', pct: 97 }), { species: 'Danaus plexippus', status: 'suppressed', version: null });
  assert.equal(fieldSexBlock({ species: 'Danaus plexippus', status: 'error:ValueError' }), null);
  assert.equal(fieldSexBlock({ species: 'Danaus plexippus', status: 'call', sex: 'male', pct: 100 }).status, 'not_supported');
  assert.equal(fieldSexBlock({ species: 'Danaus plexippus', status: 'call', sex: 'other', pct: 90 }).status, 'not_supported');
});
