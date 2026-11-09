// Saves the rack photo's real reading for the deploy's tests (tests/helpers/rack-reading.mjs):
//   node tests/slow/make-rack-reading.mjs
// Run it when the photo or the reader (server/rack-reader.mjs, its zxing) changes on purpose; the slow suite's
// rack-photo test says when the saved reading no longer matches the reader.
import { writeFileSync } from 'node:fs';
import { RACK_READING, photoSha256, readRackPhoto, readingToJson } from '../helpers/rack-reading.mjs';

const reading = readingToJson(await readRackPhoto());
const saved = { photo: 'rack-bottom-fk00274230.jpg', photo_sha256: photoSha256(), made_by: 'node tests/slow/make-rack-reading.mjs', reading };
// One line per tube: arrays of plain values are kept on one line.
const json = JSON.stringify(saved, null, 1).replace(/\[\s+((?:[-\d.e]+|"[^"]*")(?:,\s+(?:[-\d.e]+|"[^"]*"))*)\s+\]/g, (_, items) => `[${items.split(/,\s+/).join(', ')}]`);
writeFileSync(RACK_READING, json + '\n');
console.log(`Saved ${reading.tubes.length} tubes, rack ${reading.rack?.text ?? '-'} to ${RACK_READING}`);
