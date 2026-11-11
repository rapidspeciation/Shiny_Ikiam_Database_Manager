import { test } from 'node:test';
import assert from 'node:assert/strict';
import { checkPhoneManifest, STUDENT, STUDENT_L1, STUDENT_MANIFEST } from '../server/offline-id.mjs';

// The small model S_B (small-SB-20261111, 182 px) is the server's built-in small model since 11 Nov 2026 (B12 fresh-set
// check passed: species top-1 81.75 % vs L1's 79.41 %). Its rollback needs no code change: a manifest naming L1
// (192 px) in offline/student-model.json (model-versions HANDOFF.md). These pin both.

/** The rollback manifest, as HANDOFF.md gives it. */
const L1_ROLLBACK = {
  schema: 'phone-model/1',
  role: 'student',
  version: STUDENT_L1.version,
  file: STUDENT_L1.name,
  bytes: STUDENT_L1.bytes,
  sha256: STUDENT_L1.sha256,
  input: { size: 192, values: 'clip' },
};

test('the built-in small model is S_B at 182 px, its note short and without an em dash', () => {
  assert.equal(STUDENT.name, 'student_SB_int8.onnx');
  assert.equal(STUDENT.sha256, '200343ee87918fb7b8e2d2ff8433d9aa54995c9921ab5b8f942ff3ab25385762');
  assert.equal(STUDENT.version, 'small-SB-20261111');
  assert.deepEqual(STUDENT.input, { size: 182, values: 'clip' });
  for (const v of Object.values(STUDENT.notes)) {
    assert.ok(v.length <= 300 && !v.includes('—'), v);
  }
  assert.equal(STUDENT_MANIFEST, 'offline/student-model.json');
});

test('rollback to L1: its manifest is valid, 192 px, the file name allowed', () => {
  const r = checkPhoneManifest(L1_ROLLBACK);
  assert.equal(r.ok, true, r.why);
  assert.deepEqual(r.entry.input, { size: 192, values: 'clip' });
  assert.equal(r.entry.path, 'offline/student_L1_int8.onnx');
  assert.equal(STUDENT_L1.sha256, 'd6bdc3e5f8721b6ff707b4f46625cc8d8c9af86c0df757f9b2619aed1cc2f5fb');
});
