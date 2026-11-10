// Franz's rack photo (tests/fixtures/rack-bottom-fk00274230.jpg, 28 Oct) as the rack reader reads it, for the tests of what
// is done with a reading (orientation, wells, reports, deposits). Reading the photo itself (ffmpeg, then zxing over every
// square) costs seconds of CPU, so the deploy's tests take the reading saved in tests/fixtures/rack-reading-fk00274230.json,
// made from the real read by `node tests/slow/make-rack-reading.mjs`. The slow suite (scripts/test-slow.mjs) reads the
// photo for real: it checks that the saved reading is still what the reader makes of the photo, and runs the rack tests
// again on the real read (ITHOMIINI_TEST_REAL_RACK=1).
import { createHash } from 'node:crypto';
import { readFileSync } from 'node:fs';
import { loadPhoto, readOne } from '../../server/rack-reader.mjs';

export const RACK_PHOTO = new URL('../fixtures/rack-bottom-fk00274230.jpg', import.meta.url).pathname;
export const RACK_READING = new URL('../fixtures/rack-reading-fk00274230.json', import.meta.url).pathname;
export const REAL = process.env.ITHOMIINI_TEST_REAL_RACK === '1';

/** readOne's result as JSON: the Map and Set as [key, value] lists and keys, in their order. */
export const readingToJson = r => ({ ...r, tubes: [...r.tubes], seen: [...r.seen] });
export const readingFromJson = j => ({ ...structuredClone(j), tubes: new Map(j.tubes), seen: new Set(j.seen) });

export const photoSha256 = () => createHash('sha256').update(readFileSync(RACK_PHOTO)).digest('hex');
/** The saved file: { photo, photo_sha256, made_by, reading }. */
export const savedReading = () => JSON.parse(readFileSync(RACK_READING, 'utf8'));

export const readRackPhoto = async () => readOne(await loadPhoto(RACK_PHOTO));

/** The rack photo's reading: a fresh copy of the saved one, or (slow suite) the photo read for real. */
export async function rackReading() {
  return REAL ? readRackPhoto() : readingFromJson(savedReading().reading);
}
