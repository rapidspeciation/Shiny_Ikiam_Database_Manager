import test from 'node:test';
import assert from 'node:assert/strict';
import { LocalSheets } from '../server/sheets.mjs';
import { moduleMap } from '../server/schema.mjs';

// LocalSheets.readSheet copies a big sheet in pieces (the app answers others between them), yet as of one moment:
// a write or a deleted row landing between two pieces starts the copy again.

const SHEET = 'Insectary_data';
const sheetId = moduleMap.get(SHEET).sheetId ?? 0;
const seed = () => ({
  [SHEET]: Array.from({ length: 1600 }, (_, i) => ({ row: i + 2, values: { Insectary_ID: `A${i}B`, SPECIES: 'Mechanitis polymnia' } })),
});
const plain = rows => rows.map(r => ({ row: r.row, cells: r.cells }));
/** The sheet as one synchronous copy (what readSheet did before reading in pieces). */
const atOnce = sheets => structuredClone(sheets.rows.get(SHEET));
/** Runs `change` after the first piece has been copied (readSheet waits for setImmediate between pieces). */
const between = change => new Promise(resolve => setImmediate(() => resolve(change())));

test('a read in pieces gives the same rows and digest as before when nothing changes', async () => {
  const sheets = new LocalSheets(seed());
  const before = atOnce(sheets);
  const read = await sheets.readSheet(SHEET);
  assert.deepEqual(plain(read), plain(before));
  assert.equal(read.length, 1601);
});

test('a cell written between two pieces: the copy is of one moment, with the write', async () => {
  const sheets = new LocalSheets(seed());
  const reading = sheets.readSheet(SHEET);
  await between(() => sheets.externalEdit(SHEET, 1500, { SPECIES: 'Oleria onega' }));
  await between(() => sheets.externalEdit(SHEET, 3, { SPECIES: 'Oleria onega' }));
  const read = await reading;
  assert.deepEqual(plain(read), plain(atOnce(sheets)));
});

test('a row deleted between two pieces: no row number twice, the copy is of one moment', async () => {
  const sheets = new LocalSheets(seed());
  const reading = sheets.readSheet(SHEET);
  await between(() => sheets.batchUpdate([{ deleteDimension: { range: { sheetId, dimension: 'ROWS', startIndex: 9, endIndex: 10 } } }]));
  const read = await reading;
  const numbers = read.map(r => r.row);
  assert.equal(new Set(numbers).size, numbers.length);
  assert.deepEqual(plain(read), plain(atOnce(sheets)));
});

test('writes landing in three reads in a row: the sheet copied at once, still of one moment', async () => {
  const sheets = new LocalSheets(seed());
  const reading = sheets.readSheet(SHEET);
  // One write after the first piece of each of the three attempts.
  for (const row of [1500, 900, 1200]) await between(() => sheets.externalEdit(SHEET, row, { SPECIES: 'Oleria onega' }));
  const read = await reading;
  assert.deepEqual(plain(read), plain(atOnce(sheets)));
});
