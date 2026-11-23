import { test } from 'node:test';
import assert from 'node:assert/strict';
import { checkPhoneManifest, STUDENT, STUDENT_L1, STUDENT_MANIFEST, STUDENT_SB, STUDENT_W2 } from '../server/offline-id.mjs';

// The small model W2 (small-W2-20261123, 182 px) is the server's built-in small model since 23 Nov 2026 (frozen S2
// fresh-set check passed: species top-1 84.72 % vs S_B's 82.70 %). Rollback needs no code change: a manifest naming
// S_B (182 px) or L1 (192 px) in offline/student-model.json (model-versions HANDOFF.md). These pin all three.

/** The rollback manifest to S_B (182 px). */
const SB_ROLLBACK = {
  schema: 'phone-model/1',
  role: 'student',
  version: STUDENT_SB.version,
  file: STUDENT_SB.name,
  bytes: STUDENT_SB.bytes,
  sha256: STUDENT_SB.sha256,
  input: { size: 182, values: 'clip' },
};

/** The rollback manifest to L1 (192 px), as HANDOFF.md gives it. */
const L1_ROLLBACK = {
  schema: 'phone-model/1',
  role: 'student',
  version: STUDENT_L1.version,
  file: STUDENT_L1.name,
  bytes: STUDENT_L1.bytes,
  sha256: STUDENT_L1.sha256,
  input: { size: 192, values: 'clip' },
};

test('the current small model is one of the supported built-in models (W2 default; rollback by code: STUDENT_SB or STUDENT_L1)', () => {
  assert.ok([STUDENT_W2, STUDENT_SB, STUDENT_L1].includes(STUDENT));
});

test('W2 at 182 px, its note short and without an em dash', () => {
  assert.equal(STUDENT_W2.name, 'student_W2_int8.onnx');
  assert.equal(STUDENT_W2.bytes, 26327311);
  assert.equal(STUDENT_W2.sha256, '52c76bd7de48fa228be885c9fa3d1d9af112cd838b25fd2e80d7dbc63ea6cd74');
  assert.equal(STUDENT_W2.version, 'small-W2-20261123');
  assert.deepEqual(STUDENT_W2.input, { size: 182, values: 'clip' });
  for (const v of Object.values(STUDENT_W2.notes)) {
    assert.ok(v.length <= 300 && !v.includes('—'), v);
  }
  assert.equal(STUDENT_MANIFEST, 'offline/student-model.json');
});

test('rollback to S_B: its manifest is valid, 182 px, the file name allowed', () => {
  assert.equal(STUDENT_SB.name, 'student_SB_int8.onnx');
  assert.equal(STUDENT_SB.bytes, 23380320);
  assert.equal(STUDENT_SB.sha256, '200343ee87918fb7b8e2d2ff8433d9aa54995c9921ab5b8f942ff3ab25385762');
  assert.equal(STUDENT_SB.version, 'small-SB-20261111');
  assert.deepEqual(STUDENT_SB.input, { size: 182, values: 'clip' });
  const r = checkPhoneManifest(SB_ROLLBACK);
  assert.equal(r.ok, true, r.why);
  assert.deepEqual(r.entry.input, { size: 182, values: 'clip' });
  assert.equal(r.entry.path, 'offline/student_SB_int8.onnx');
  for (const v of Object.values(STUDENT_SB.notes)) {
    assert.ok(v.length <= 300 && !v.includes('—'), v);
  }
});

test('rollback to L1: its manifest is valid, 192 px, the file name allowed', () => {
  const r = checkPhoneManifest(L1_ROLLBACK);
  assert.equal(r.ok, true, r.why);
  assert.deepEqual(r.entry.input, { size: 192, values: 'clip' });
  assert.equal(r.entry.path, 'offline/student_L1_int8.onnx');
  assert.equal(STUDENT_L1.sha256, 'd6bdc3e5f8721b6ff707b4f46625cc8d8c9af86c0df757f9b2619aed1cc2f5fb');
});
