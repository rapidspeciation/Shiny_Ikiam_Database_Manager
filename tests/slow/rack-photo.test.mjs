// The slow suite (scripts/test-slow.mjs): the rack reader on Franz's real rack photo, with ffmpeg and zxing.
// The deploy's rack tests use the reading saved from this read (tests/helpers/rack-reading.mjs): it must stay exactly
// what the reader makes of the photo.
import test from 'node:test';
import assert from 'node:assert/strict';
import { photoSha256, readRackPhoto, readingToJson, savedReading } from '../helpers/rack-reading.mjs';

test("the saved reading of Franz's rack photo is the reader's real reading of it", async () => {
  const saved = savedReading();
  assert.equal(saved.photo_sha256, photoSha256(), 'the photo changed: node tests/slow/make-rack-reading.mjs');
  const real = readingToJson(await readRackPhoto());
  assert.ok(real.tubes.length >= 92, `${real.tubes.length} read`);
  assert.deepEqual(real, saved.reading, 'the reader reads the photo differently now: check why, then node tests/slow/make-rack-reading.mjs');
});
