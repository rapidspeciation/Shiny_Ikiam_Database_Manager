import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, rmSync, writeFileSync, utimesSync, existsSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { LocalSheets } from '../server/sheets.mjs';
import { FileSheets, buildRowsFile, openFileSheets, rowsFileFresh, rowsFileName, withBuildLock } from '../server/file-sheets.mjs';
import { moduleMap } from '../server/schema.mjs';

// FileSheets (the lab's rows in a SQLite file) must answer as LocalSheets (rows in memory) does: every test runs
// the same operations on both and compares every answer, the digests of whole-sheet reads included.

const SHEET = 'Insectary_data';
const OTHER = 'Collection_data';
const sheetId = name => moduleMap.get(name).sheetId ?? 0;
const col = (sheet, key) => moduleMap.get(sheet).fields.find(f => f.key === key).column;
const ID = col(SHEET, 'Insectary_ID');
const LET = r =>
  `=LET(p,LEFT(A${r},3),l,LEFT(p,1),n,VALUE(MID(p,2,1)),s,RIGHT(p,1),IF(n<9,l&(n+1)&s,IF(l<>"Z",CHAR(CODE(l)+1)&"0"&s,"A0"&CHAR(CODE(s)+1))))`;

/** A seed as the lab's snapshot has them: Google's cells (empty ones null, as JSON leaves them), some by field. */
function seed() {
  const header = { row: 1, cells: moduleMap.get(SHEET).fields.map(f => ({ userEnteredValue: { stringValue: f.key }, effectiveValue: { stringValue: f.key } })) };
  const rows = [header];
  for (let r = 2; r <= 1300; r++) {
    const cells = [];
    cells[ID] =
      r === 2
        ? { userEnteredValue: { stringValue: 'A1B' }, effectiveValue: { stringValue: 'A1B' } }
        : { userEnteredValue: { formulaValue: LET(r - 1) }, effectiveValue: { stringValue: `X${r}B` } };
    cells[col(SHEET, 'SPECIES')] = { userEnteredValue: { stringValue: r % 3 ? 'Mechanitis polymnia' : 'Oleria onega' }, userEnteredFormat: { numberFormat: { type: 'TEXT' } } };
    cells[col(SHEET, 'SPECIES') + 2] = { userEnteredValue: { numberValue: r }, effectiveValue: { numberValue: r }, dataValidation: { condition: { type: 'ONE_OF_LIST' } } };
    // JSON has no empty slots: the snapshot's gaps are nulls.
    rows.push(JSON.parse(JSON.stringify({ row: r, cells })));
  }
  return {
    [SHEET]: rows,
    // Out of row order, with a gap at 4, and no header (LocalSheets puts one first).
    [OTHER]: [
      { row: 3, values: { CAM_ID: 'CAM000002', Sex: 'female' } },
      { row: 2, values: { CAM_ID: 'CAM000001', Sex: 'male' } },
      { row: 5, values: { CAM_ID: 'CAM000005', Sex: 'male' } },
    ],
    Unlisted_sheet: [{ row: 1, cells: [{ userEnteredValue: { stringValue: 'x' } }] }],
  };
}

const protectedRanges = { [OTHER]: [{ startRowIndex: 0, endRowIndex: 1, startColumnIndex: 0, endColumnIndex: 30 }] };
const dirs = [];
test.after(() => {
  for (const dir of dirs) rmSync(dir, { recursive: true, force: true });
});
function pair(data = seed()) {
  const dir = mkdtempSync(join(tmpdir(), 'file-sheets-'));
  dirs.push(dir);
  const seedFile = join(dir, 'snapshot.json');
  writeFileSync(seedFile, JSON.stringify({ sheets: data }));
  buildRowsFile(seedFile, join(dir, 'rows.sqlite'));
  return {
    memory: new LocalSheets(structuredClone(data), { protectedRanges }),
    file: new FileSheets(join(dir, 'rows.sqlite'), { protectedRanges }),
    dir,
    seedFile,
  };
}

/** Everything both answer about the workbook. */
async function state(sheets) {
  const out = {};
  for (const sheet of [SHEET, OTHER, 'Unlisted_sheet', 'Photo_links']) {
    const rows = await sheets.readSheet(sheet);
    out[sheet] = { rows: [...rows], digest: rows.digest, info: await sheets.sheetInfo(sheet), count: sheets.rowCount(sheet) };
  }
  out.rows = [...(await sheets.readRows([{ sheet: SHEET, rows: [1, 2, 3, 700, 1299, 1300, 1301, 5000] }]))];
  out.grid = await sheets.readGrid(SHEET, 1298, 1303);
  out.names = sheets.sheetNames();
  return out;
}
/** Runs `step` on both and compares what it answered (or threw) and the whole workbook after it. */
async function same({ memory, file }, step) {
  const run = async sheets => {
    try {
      return { ok: await step(sheets) };
    } catch (error) {
      return { error: error.message, status: error.status };
    }
  };
  const [a, b] = [await run(memory), await run(file)];
  assert.deepStrictEqual(b, a);
  assert.deepStrictEqual(await state(file), await state(memory));
  return a;
}

test('the rows file reads as the seed in memory, the gaps of the cells and the digests included', async () => {
  const both = pair();
  const a = await state(both.memory);
  assert.equal(a[SHEET].rows.length, 1300);
  assert.equal(a[SHEET].rows[5].cells[col(SHEET, 'SPECIES') + 1], null);
  assert.deepStrictEqual(await state(both.file), a);
  // The headers the seed lacks are added, as LocalSheets does.
  assert.equal(a[OTHER].rows[0].row, moduleMap.get(OTHER).headerRow);
});

test('cell writes, new rows past the end and in gaps (kept last, as LocalSheets does), a new sheet, formulas worked out', async () => {
  const both = pair();
  await same(both, s => s.externalEdit(SHEET, 1310, { SPECIES: 'Hypothyris anastasia' }));
  // In the gap left by 1310: LocalSheets lists it after 1310.
  await same(both, s => s.externalEdit(SHEET, 1305, { SPECIES: 'Oleria onega' }));
  await same(both, s => s.externalEdit(SHEET, 1311, { SPECIES: 'Oleria onega' }));
  await same(both, s => s.externalEdit(OTHER, 4, { Sex: 'female' }));
  const order = (await both.file.readSheet(SHEET)).map(r => r.row).slice(-4);
  assert.deepStrictEqual(order, [1300, 1310, 1305, 1311]);
  await same(both, s => s.externalEdit(SHEET, 700, { SPECIES: 'Oleria onega', Insectary_ID: LET(699) }));
  await same(both, s => s.writeCells(OTHER, 2, { Sex: 'female' }, { Sex: 40 }));
  await same(both, s => s.writeCells('Photo_links', 4, { Name: 'IMG_1.JPG' }));
  await same(both, s => s.writeCells(SHEET, 5, { No_such_field: 1 }));
});

test('inserted and deleted rows move the rows below and the formulas that refer to them', async () => {
  const both = pair();
  const insert = at => ({
    sheet: SHEET,
    row: at,
    insert: { at, source: at - 1, width: 40, clear: [col(SHEET, 'SPECIES')], formulas: [{ column: ID, from: at - 1 }] },
    changes: { SPECIES: 'Mechanitis messenoides' },
    columns: { SPECIES: col(SHEET, 'SPECIES') },
  });
  await same(both, s => s.writeBatch([insert(10)]));
  await same(both, s => s.writeBatch([insert(1301)]));
  await same(both, s => s.writeBatch([{ sheet: SHEET, row: 20, deleteRow: { at: 20 }, changes: {} }]));
  await same(both, s => s.batchUpdate([{ deleteDimension: { range: { sheetId: sheetId(SHEET), dimension: 'ROWS', startIndex: 1, endIndex: 2 } } }]));
  await same(both, s => s.writeBatch([insert(400), { sheet: SHEET, row: 900, deleteRow: { at: 900 }, changes: {} }]));
});

test('copyPaste (tiled), updateCells and appendDimension, as one batch', async () => {
  const both = pair();
  const rect = (r0, r1, c0, c1) => ({ sheetId: sheetId(SHEET), startRowIndex: r0, endRowIndex: r1, startColumnIndex: c0, endColumnIndex: c1 });
  await same(both, s => s.batchUpdate([
    { appendDimension: { sheetId: sheetId(SHEET), dimension: 'ROWS', length: 5 } },
    { copyPaste: { source: rect(2, 3, 0, 40), destination: rect(1300, 1304, 0, 40), pasteType: 'PASTE_NORMAL' } },
    { copyPaste: { source: rect(3, 4, ID, ID + 1), destination: rect(50, 53, ID, ID + 1), pasteType: 'PASTE_FORMULA' } },
    { updateCells: { range: rect(60, 62, 3, 5), rows: [{ values: [{ userEnteredValue: { stringValue: 'a' } }, { userEnteredValue: { formulaValue: '=ROW()' } }] }], fields: 'userEnteredValue' } },
    { updateCells: { range: rect(70, 71, 2, 3), fields: 'userEnteredValue,userEnteredFormat.numberFormat' } },
  ]));
  await same(both, s => s.batchUpdate([{ copyPaste: { source: rect(2, 3, 0, 1), destination: rect(5, 6, 0, 1), pasteType: 'PASTE_VALUES' } }]));
});

test('rows out of order: a write keeps them so, a pasted new row or a moved row sorts the sheet, as LocalSheets does', async () => {
  const both = pair();
  const other = (r0, r1, c0, c1) => ({ sheetId: sheetId(OTHER), startRowIndex: r0, endRowIndex: r1, startColumnIndex: c0, endColumnIndex: c1 });
  await same(both, s => s.externalEdit(OTHER, 4, { Sex: 'female' }));
  assert.deepStrictEqual((await both.file.readSheet(OTHER)).map(r => r.row), [1, 3, 2, 5, 4]);
  await same(both, s => s.batchUpdate([{ updateCells: { range: other(7, 8, 0, 1), rows: [{ values: [{ userEnteredValue: { stringValue: 'CAM000008' } }] }], fields: 'userEnteredValue' } }]));
  assert.deepStrictEqual((await both.file.readSheet(OTHER)).map(r => r.row), [1, 2, 3, 4, 5, 8]);
  await same(both, s => s.externalEdit(OTHER, 6, { Sex: 'male' }));
  await same(both, s => s.batchUpdate([{ insertDimension: { range: { sheetId: sheetId(OTHER), dimension: 'ROWS', startIndex: 2, endIndex: 3 } } }]));
  await same(both, s => s.externalEdit(OTHER, 3, { Sex: 'male' }));
});

test('a protected range fails the whole batch before anything is written, as Google does', async () => {
  const both = pair();
  const r = await same(both, s => s.writeBatch([
    { sheet: OTHER, row: 3, changes: { Sex: 'male' }, columns: { Sex: 1 } },
    { sheet: OTHER, row: 1, changes: { Sex: 'x' }, columns: { Sex: 1 } },
  ]));
  assert.equal(r.status, 400);
  await same(both, s => s.batchUpdate([{ appendDimension: { sheetId: sheetId(OTHER), dimension: 'ROWS', length: 1 } }]));
});

test('a read in pieces with writes and a deleted row landing between pieces is of one moment', async () => {
  const between = change => new Promise(resolve => setImmediate(() => resolve(change())));
  for (const make of [
    s => [between(() => s.externalEdit(SHEET, 1200, { SPECIES: 'Oleria onega' }))],
    s => [between(() => s.batchUpdate([{ deleteDimension: { range: { sheetId: sheetId(SHEET), dimension: 'ROWS', startIndex: 9, endIndex: 10 } } }]))],
  ]) {
    const both = pair();
    const reads = [];
    for (const s of [both.memory, both.file]) {
      const reading = s.readSheet(SHEET);
      await Promise.all(make(s));
      const rows = await reading;
      reads.push({ rows: [...rows], digest: rows.digest });
    }
    assert.deepStrictEqual(reads[1], reads[0]);
  }
  // Three reads interrupted: the whole sheet at once.
  const both = pair();
  const reads = [];
  for (const s of [both.memory, both.file]) {
    const reading = s.readSheet(SHEET);
    for (const row of [1200, 900, 1100]) await new Promise(resolve => setImmediate(() => resolve(s.externalEdit(SHEET, row, { SPECIES: 'x' }))));
    const rows = await reading;
    reads.push({ rows: [...rows], digest: rows.digest });
  }
  assert.deepStrictEqual(reads[1], reads[0]);
});

test('openFileSheets makes the rows file once, again when the snapshot changes, and each start on a fresh copy', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'file-sheets-open-'));
  dirs.push(dir);
  const seedFile = join(dir, 'snapshot.json');
  writeFileSync(seedFile, JSON.stringify(seed()));
  const databasePath = join(dir, 'app', 'app.sqlite');
  rmSync(join(dir, 'app'), { recursive: true, force: true });
  (await import('node:fs')).mkdirSync(join(dir, 'app'));
  const logs = [];
  const log = line => logs.push(line);
  let sheets = await openFileSheets({ seedFile, databasePath, log });
  assert.ok(sheets instanceof FileSheets);
  assert.ok(rowsFileFresh(rowsFileName(seedFile), seedFile));
  assert.equal(logs.filter(l => l.startsWith('Workbook rows file made')).length, 1);
  await sheets.externalEdit(SHEET, 2, { SPECIES: 'changed' });
  sheets.close();
  // A restart: the rows file is not made again, and the change is gone.
  sheets = await openFileSheets({ seedFile, databasePath, log });
  assert.equal(logs.filter(l => l.startsWith('Workbook rows file made')).length, 1);
  const species = col(SHEET, 'SPECIES');
  assert.equal((await sheets.readRow(SHEET, 2)).cells[species].userEnteredValue.stringValue, 'Mechanitis polymnia');
  sheets.close();
  // A new snapshot: made again.
  const changed = seed();
  changed[SHEET][1].cells[species] = { userEnteredValue: { stringValue: 'Ithomia salapia' } };
  writeFileSync(seedFile, JSON.stringify(changed));
  utimesSync(seedFile, new Date(), new Date(Date.now() + 5000));
  sheets = await openFileSheets({ seedFile, databasePath, log });
  assert.equal(logs.filter(l => l.startsWith('Workbook rows file made')).length, 2);
  assert.equal((await sheets.readRow(SHEET, 2)).cells[species].userEnteredValue.stringValue, 'Ithomia salapia');
  sheets.close();
  // A row number twice: kept in memory instead (null), with the reason logged.
  const twice = seed();
  twice[OTHER].push({ row: 2, values: { CAM_ID: 'CAM000009' } });
  writeFileSync(seedFile, JSON.stringify(twice));
  utimesSync(seedFile, new Date(), new Date(Date.now() + 10000));
  assert.equal(await openFileSheets({ seedFile, databasePath, log }), null);
  assert.match(logs.at(-1), /kept in memory .*row 2 twice/);
  assert.equal(existsSync(join(dir, 'app', 'workbook-rows.sqlite')), true);
});

test('copies side by side share the rows file, made once, and each keeps its own writes', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'file-sheets-copies-'));
  dirs.push(dir);
  const seedFile = join(dir, 'snapshot.json');
  writeFileSync(seedFile, JSON.stringify(seed()));
  const { mkdirSync, statSync } = await import('node:fs');
  const logs = [];
  const log = line => logs.push(line);
  const open = name => {
    mkdirSync(join(dir, name));
    return openFileSheets({ seedFile, databasePath: join(dir, name, 'app.sqlite'), log });
  };
  // Two copies starting together: one rows file, made by one of them, used by both.
  const [a, b] = await Promise.all([open('a'), open('b')]);
  assert.ok(a instanceof FileSheets && b instanceof FileSheets);
  assert.equal(logs.filter(l => l.startsWith('Workbook rows file made')).length, 1, logs.join('\n'));
  const shared = rowsFileName(seedFile);
  const sharedBefore = statSync(shared);
  // A third copy later: not made again.
  const c = await open('c');
  assert.equal(logs.filter(l => l.startsWith('Workbook rows file made')).length, 1);
  const species = col(SHEET, 'SPECIES');
  const speciesOf = async (s, row) => (await s.readRow(SHEET, row)).cells[species]?.userEnteredValue?.stringValue;
  await a.externalEdit(SHEET, 2, { SPECIES: 'only in a' });
  await b.externalEdit(SHEET, 3, { SPECIES: 'only in b' });
  await b.batchUpdate([{ insertDimension: { range: { sheetId: sheetId(SHEET), dimension: 'ROWS', startIndex: 4, endIndex: 5 } } }]);
  assert.equal(await speciesOf(a, 2), 'only in a');
  assert.equal(await speciesOf(b, 2), 'Mechanitis polymnia');
  assert.equal(await speciesOf(c, 2), 'Mechanitis polymnia');
  assert.equal(await speciesOf(a, 3), 'Oleria onega');
  assert.equal(await speciesOf(b, 3), 'only in b');
  // b's inserted row moved its rows below; a's and c's did not move.
  assert.equal((await a.readSheet(SHEET)).length, (await c.readSheet(SHEET)).length);
  assert.equal((await b.readSheet(SHEET)).at(-1).row, (await a.readSheet(SHEET)).at(-1).row + 1);
  for (const s of [a, b, c]) s.close();
  // The shared file is as it was made: no copy wrote to it.
  assert.equal(statSync(shared).mtimeMs, sharedBefore.mtimeMs);
  assert.ok(rowsFileFresh(shared, seedFile));
  const fresh = new FileSheets(join(dir, 'c', 'workbook-rows.sqlite'));
  try {
    assert.equal(await speciesOf(fresh, 2), 'Mechanitis polymnia');
  } finally {
    fresh.close();
  }
});

test('the build lock: a holder that has ended is taken over, a live one is waited for, and it is gone after', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'file-sheets-lock-'));
  dirs.push(dir);
  const file = join(dir, 'rows.sqlite');
  const { spawnSync } = await import('node:child_process');
  const { readFileSync } = await import('node:fs');
  const ended = spawnSync(process.execPath, ['-e', 'process.stdout.write(String(process.pid))'], { encoding: 'utf8' });
  writeFileSync(`${file}.lock`, ended.stdout);
  assert.equal(await withBuildLock(file, () => readFileSync(`${file}.lock`, 'utf8')), String(process.pid));
  assert.equal(existsSync(`${file}.lock`), false);
  // Held by a live process (this one): waited for, until the holder lets go or the wait ends.
  writeFileSync(`${file}.lock`, String(process.pid));
  await assert.rejects(withBuildLock(file, () => 'never', { waitMs: 300 }), /waited 0 s/);
  setTimeout(() => rmSync(`${file}.lock`, { force: true }), 300);
  assert.equal(await withBuildLock(file, () => 'after', { waitMs: 5000 }), 'after');
  // A failing fn still lets go.
  await assert.rejects(withBuildLock(file, () => { throw new Error('boom'); }), /boom/);
  assert.equal(existsSync(`${file}.lock`), false);
});
