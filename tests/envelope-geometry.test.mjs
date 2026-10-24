import test from 'node:test';
import assert from 'node:assert/strict';
import { fileURLToPath } from 'node:url';
import { join } from 'node:path';
import { Store } from '../server/store.mjs';
import { importBundle, readCuration } from '../server/envelope-import.mjs';
import { reviewData } from '../server/photodata.mjs';

test('envelope import preserves the OCR text-line detector geometry through storage', () => {
  const dir = fileURLToPath(new URL('./fixtures/envelope/', import.meta.url));
  const bundle = readCuration(dir, join(dir, 'manifest.csv'));
  const imported = bundle.readings.find(r => r.name === 'CAM000101d');
  assert.deepEqual(imported.camidLines[0], ['CAM000111', 1, [[0, 0], [1, 0], [1, 1], [0, 1]], 0]);
  const store = new Store({ localMode: true });
  try {
    importBundle(store.db, bundle);
    assert.deepEqual(reviewData(store).readings.get('CAM000101D').camidLines, imported.camidLines);
  } finally {
    store.close();
  }
});
