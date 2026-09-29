// Columns are found by their header name: moving, inserting or renaming columns
// in Google Sheets must never scramble what the app reads or writes.
import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { applyBatch } from '../server/batch.mjs';
import { tablePayload } from '../server/grid.mjs';
import { headerLayout } from '../server/columns.mjs';

const user = { id: 'editor-1', username: 'editor', role: 'editor' };
const SHEET = 'Insectary_data';
const formulaCell = (formula, value) => ({
  userEnteredValue: { formulaValue: formula },
  effectiveValue: { stringValue: value },
});

async function fixture() {
  const sheets = new LocalSheets({
    [SHEET]: [
      {
        row: 2,
        values: { Insectary_ID: 'A0A', SPECIES: 'Melinaea menophilus', Sex: 'female', Death_cause: 'Natural' },
      },
      { row: 3, values: { Insectary_ID: 'A1A', SPECIES: 'Melinaea menophilus', Sex: 'male' } },
      { row: 4, cells: [formulaCell('="A2A"', 'A2A')] },
      { row: 5, cells: [formulaCell('="A3A"', 'A3A')] },
    ],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: [SHEET] });
  return { sheets, store, at: row => store.getRecordBySheetRow(SHEET, row) };
}
/** Changes every row of the local sheet the same way (as moving columns in Google Sheets does). */
function rearrange(sheets, change) {
  for (const r of sheets.rows.get(SHEET)) r.cells = change([...r.cells]);
}
/** The cell under a header name, wherever that column is now. */
async function cellUnder(sheets, name, row) {
  const header = await sheets.readRow(SHEET, 1);
  const column = header.cells.findIndex(c => c?.userEnteredValue?.stringValue === name);
  assert.ok(column >= 0, `header ${name} present`);
  return (await sheets.readRow(SHEET, row)).cells[column];
}
const text = cell => cell?.userEnteredValue?.stringValue ?? cell?.userEnteredValue?.formulaValue ?? null;

test('columns moved in the sheet: reads stay right and writes land under their header', async () => {
  const { sheets, store, at } = await fixture();
  const width = (await sheets.readRow(SHEET, 1)).cells.length;
  // Reverse the order of every column (Insectary_ID ends up last).
  rearrange(sheets, cells => Array.from({ length: width }, (_, i) => cells[width - 1 - i]));
  const status = await store.sync({ sheets: [SHEET], force: true });
  assert.equal(status.skipped, 0);
  assert.equal(status.changed, 0, 'moving columns changes no value');
  assert.equal(at(2).values.Sex, 'female');
  assert.equal(at(3).values.Insectary_ID, 'A1A');
  assert.equal(at(4).formulas.Insectary_ID, '="A2A"');
  assert.deepEqual(store.headerProblems.get(SHEET), []);

  const result = await applyBatch(
    store,
    {
      requestId: randomUUID(),
      edits: [{ id: at(2).id, values: { Sex: 'male', Death_date: '2025-08-14' } }],
      creates: [{ module: SHEET, values: { Insectary_ID: 'A2A', Sex: 'female', 'CLUTCH NUMBER': '944' } }],
    },
    user,
  );
  assert.equal(result.status, 'verified');
  assert.equal(text(await cellUnder(sheets, 'Sex', 2)), 'male');
  assert.equal((await cellUnder(sheets, 'Death_date', 2)).userEnteredValue.numberValue, 45883);
  assert.equal(text(await cellUnder(sheets, 'Sex', 4)), 'female', 'the new row went to the pre-made row of A2A');
  assert.equal(text(await cellUnder(sheets, 'Insectary_ID', 4)), '="A2A"', 'its ID formula is untouched');
  assert.equal(text(await cellUnder(sheets, 'SPECIES', 2)), 'Melinaea menophilus');
  assert.equal(at(4).values.Sex, 'female');
  store.close();
});

test('a column inserted in the middle is ignored and never written; the rest keeps working', async () => {
  const { sheets, store, at } = await fixture();
  // A new "Wing_clip_date" column before Death_date (I), with a value in row 2.
  rearrange(sheets, cells => {
    cells.splice(8, 0, undefined);
    return cells;
  });
  sheets.rows.get(SHEET).find(r => r.row === 1).cells[8] = { userEnteredValue: { stringValue: 'Wing_clip_date' } };
  sheets.rows.get(SHEET).find(r => r.row === 2).cells[8] = { userEnteredValue: { stringValue: 'kept' } };
  const status = await store.sync({ sheets: [SHEET], force: true });
  assert.equal(status.skipped, 0);
  assert.deepEqual(status.headerProblems[SHEET], [{ kind: 'new', field: 'Wing_clip_date', column: 'I' }]);
  assert.equal(at(2).values.Death_cause, 'Natural');
  assert.equal(at(2).values.Wing_clip_date, undefined, 'an unknown column is not read into the app');

  const values = { Death_date: '2025-08-14', Death_cause: 'Unknown' };
  await applyBatch(store, { requestId: randomUUID(), edits: [{ id: at(2).id, values }] }, user);
  const row = await sheets.readRow(SHEET, 2);
  assert.equal(row.cells[8].userEnteredValue.stringValue, 'kept', 'the new column keeps its value');
  assert.equal(row.cells[9].userEnteredValue.numberValue, 45883, 'Death_date moved to J and was written there');
  assert.equal(text(await cellUnder(sheets, 'Death_cause', 2)), 'Unknown');
  assert.equal(tablePayload(store, SHEET).headerProblems[0].kind, 'new');
  store.close();
});

test('a renamed column: only that field is unavailable, with its last values kept', async () => {
  const { sheets, store, at } = await fixture();
  const header = sheets.rows.get(SHEET).find(r => r.row === 1);
  const column = header.cells.findIndex(c => c?.userEnteredValue?.stringValue === 'Death_cause');
  header.cells[column] = { userEnteredValue: { stringValue: 'Cause_of_death' } };
  sheets.rows.get(SHEET).find(r => r.row === 2).cells[column] = { userEnteredValue: { stringValue: 'Predation' } };
  const status = await store.sync({ sheets: [SHEET], force: true });
  assert.equal(status.skipped, 0);
  assert.deepEqual(
    status.headerProblems[SHEET].map(p => [p.kind, p.field, !!p.blocking]),
    [
      ['missing', 'Death_cause', false],
      ['new', 'Cause_of_death', false],
    ],
  );
  assert.equal(at(2).values.Death_cause, 'Natural', 'the last known value is kept, not blanked');
  assert.equal(status.changed, 0, 'no edit is recorded for a column that only lost its name');
  const payload = tablePayload(store, SHEET);
  assert.deepEqual(
    payload.columns.filter(c => c.unavailable).map(c => [c.key, c.readonly]),
    [['Death_cause', true]],
  );

  // Saving that field is refused, naming the column; the other fields still save.
  await assert.rejects(
    applyBatch(store, { requestId: randomUUID(), edits: [{ id: at(2).id, values: { Death_cause: 'Unknown' } }] }, user),
    e =>
      e.details.items[0].code === 'COLUMN_MISSING' &&
      /Falta la columna Death_cause en Insectary_data/.test(e.details.items[0].message),
  );
  const edits = [{ id: at(2).id, values: { Death_cause: 'Unknown', Sex: 'male' } }];
  const partial = await applyBatch(store, { requestId: randomUUID(), partial: true, edits }, user);
  assert.equal(partial.status, 'verified');
  assert.deepEqual(partial.skipped.map(s => [s.code, s.field]), [['COLUMN_MISSING', 'Death_cause']]);
  assert.equal(text(await cellUnder(sheets, 'Sex', 2)), 'male');
  assert.equal(text(await cellUnder(sheets, 'Cause_of_death', 2)), 'Predation', 'the renamed column is not written');
  assert.equal(at(2).values.Death_cause, 'Natural');
  store.close();
});

test('the identity column missing, or the header unreadable, blocks the sheet with the reason', async () => {
  const { sheets, store, at } = await fixture();
  const header = sheets.rows.get(SHEET).find(r => r.row === 1);
  const saved = structuredClone(header.cells);
  header.cells[0] = { userEnteredValue: { stringValue: 'Insectary ID' } };
  const status = await store.sync({ sheets: [SHEET], force: true });
  assert.equal(status.skipped, 1);
  assert.ok(status.headerProblems[SHEET].some(p => p.kind === 'missing' && p.field === 'Insectary_ID' && p.blocking));
  await assert.rejects(
    applyBatch(store, { requestId: randomUUID(), edits: [{ id: at(2).id, values: { Sex: 'male' } }] }, user),
    e =>
      e.details.items[0].code === 'HEADER_MISMATCH' &&
      /falta la columna Insectary_ID/.test(e.details.items[0].message),
  );
  // A row inserted above the header: nothing is recognized.
  assert.ok(headerLayout(SHEET, { cells: [{ userEnteredValue: { stringValue: 'Notes' } }] }).blocked);
  header.cells = saved;
  assert.equal((await store.sync({ sheets: [SHEET], force: true })).skipped, 0);
  store.close();
});

test('a direct edit reported by the trigger after columns moved asks for a sheet sync', async () => {
  const { sheets, store } = await fixture();
  rearrange(sheets, cells => [cells[1], cells[0], ...cells.slice(2)]);
  assert.deepEqual(await store.refreshRows(SHEET, [2]), { needsSync: true });
  await store.sync({ sheets: [SHEET], force: true });
  await sheets.externalEdit(SHEET, 2, { Sex: 'male' });
  const result = await store.refreshRows(SHEET, [2]);
  assert.equal(result.changed, 1);
  assert.equal(store.getRecordBySheetRow(SHEET, 2).values.Sex, 'male');
  store.close();
});
