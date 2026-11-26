import test from 'node:test';
import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { mkdtempSync, readFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';

// Slot 1 3-backbone server fusion integration tests (tools/identifier/worker.py):
// numerical parity vs step_b1_claudeclaw_arm_bench.py, untouched 1024-D feat_bio for field_sex,
// graceful automatic fallback when fusion assets are absent/partial, Option 1A merges + geo prior,
// and multi-photo specimen() feature averaging.

const TOOLS = new URL('../tools/identifier', import.meta.url).pathname;
const CHECK = new URL('./fixtures/fusion-slot1-check.py', import.meta.url).pathname;
const python = process.env.PYTHON ?? 'python3';
const hasNumpy = spawnSync(python, ['-I', '-c', 'import numpy'], { encoding: 'utf8' }).status === 0;
const skip = !hasNumpy && 'no Python with numpy';

test('worker.py Slot 1 fusion math, field_sex feat_bio isolation, Option 1A + geo prior, specimen(), and fallback', { skip }, t => {
  const dir = mkdtempSync(join(tmpdir(), 'fusion-slot1-'));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  const outPath = join(dir, 'out.json');
  const run = spawnSync(python, ['-I', '-B', CHECK, TOOLS, outPath], { encoding: 'utf8', timeout: 60_000 });
  assert.equal(run.status, 0, run.stderr);
  const res = JSON.parse(readFileSync(outPath, 'utf8'));
  assert.equal(res.asset_discovery, 'ok');
  assert.ok(res.blend_stub_max_diff < 1e-14, String(res.blend_stub_max_diff));
  assert.equal(res.combine_features, 'ok');
  assert.equal(res.field_sex_bio_bit_identical, true);
  assert.equal(res.option1a_and_geo_prior, 'ok');
  if (res.has_torch) {
    assert.ok(res.torch_adapter_blend_max_diff < 1e-5, String(res.torch_adapter_blend_max_diff));
    assert.equal(res.encode_dino_exact_zero_diff, true);
    assert.equal(res.init_fusion_fallback_tamper, 'ok');
  } else {
    t.diagnostic(`torch checks skipped (torch not installed in ${python})`);
  }
});

test('tools/identifier/setup.sh passes bash -n and pins all three Slot 1 fusion SHA-256s', () => {
  const setupPath = join(TOOLS, 'setup.sh');
  const syntax = spawnSync('bash', ['-n', setupPath], { encoding: 'utf8' });
  assert.equal(syntax.status, 0, syntax.stderr);
  const text = readFileSync(setupPath, 'utf8');
  assert.match(text, /head_slot1_3bb_dist_rel_7d54b54f\.pt:7d54b54f9853c9b6bd41df66138f3695190c3f47fe52880c03a031801712b6d1/);
  assert.match(text, /w1c_pre384_fp32\.onnx:baddedde2f528b39f6f2a643c2630cb7f325101e9e12b4b2df66104cb1ce1366/);
  assert.match(text, /w1d_pre384_fp32\.onnx:6e5174daddfca36843fcbf9db52ce593469454ffc24ae946c38c12d41358aa27/);
});
