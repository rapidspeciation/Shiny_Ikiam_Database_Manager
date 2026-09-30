import test from 'node:test';
import assert from 'node:assert/strict';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';

// The periodic sync reads every sheet; sheets and rows that read as last time are not compared again.

const SHEETS = ['Insectary_data', 'Collection_data'];

async function fixture() {
  const sheets = new LocalSheets({
    Insectary_data: [
      { row: 2, values: { Insectary_ID: 'A0A', SPECIES: 'Mechanitis polymnia', Sex: 'male' } },
      { row: 3, values: { Insectary_ID: 'A1A', SPECIES: 'Mechanitis polymnia', Sex: 'female' } },
      { row: 4, values: { Insectary_ID: 'A2A', SPECIES: 'Oleria onega', Sex: 'female' } },
    ],
    Collection_data: [{ row: 2, values: { CAM_ID: 'CAM000001', SPECIES: 'Oleria onega', Sex: 'male' } }],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: SHEETS });
  // Counts the rows written to the local copy.
  let writes = 0;
  const persist = store.persistRecord.bind(store);
  store.persistRecord = record => (writes++, persist(record));
  return { store, sheets, writes: () => writes };
}

test('a sheet that reads as at the last sync, with no local change since, is not compared again', async () => {
  const { store, writes } = await fixture();
  const status = await store.sync({ sheets: SHEETS });
  assert.equal(status.sheetsUnchanged, 2);
  assert.equal(status.state, 'offline_seed');
  assert.equal(writes(), 0);
  store.close();
});

test('an edit in one sheet reconciles that sheet, writing only the edited row', async () => {
  const { store, sheets, writes } = await fixture();
  const before = store.getRecordBySheetRow('Insectary_data', 3);
  await sheets.externalEdit('Insectary_data', 3, { Sex: 'NA' });
  const status = await store.sync({ sheets: SHEETS });
  assert.equal(status.sheetsUnchanged, 1);
  assert.equal(status.changed, 1);
  assert.equal(writes(), 1);
  const after = store.getRecordBySheetRow('Insectary_data', 3);
  assert.equal(after.id, before.id);
  assert.equal(after.values.Sex, 'NA');
  assert.equal(after.version, before.version + 1);
  // Unchanged rows keep their record, version and time.
  const other = store.getRecordBySheetRow('Insectary_data', 4);
  assert.equal(other.version, 1);
  // The edit is in the history as a change made in Google Sheets.
  const history = store.db
    .prepare("SELECT c.field, c.after_json FROM changes c JOIN actions a ON a.id=c.action_id WHERE a.source='sheet_reconciliation'")
    .all();
  assert.deepEqual(history.map(h => [h.field, h.after_json]), [['Sex', '"NA"']]);
  store.close();
});

test('a local copy changed since the last sync is compared again even when the sheet reads the same', async () => {
  const { store } = await fixture();
  const record = store.getRecordBySheetRow('Insectary_data', 2);
  // As after a save whose value Google did not keep: the local row differs from the sheet.
  store.placeRecord({ ...record, values: { ...record.values, Sex: 'female' }, version: record.version + 1, updatedAt: new Date().toISOString() });
  const status = await store.sync({ sheets: SHEETS });
  assert.equal(status.sheetsUnchanged, 1);
  assert.equal(store.getRecordBySheetRow('Insectary_data', 2).values.Sex, 'male');
  store.close();
});

test('a forced sync compares every sheet', async () => {
  const { store, writes } = await fixture();
  const status = await store.sync({ sheets: SHEETS, force: true });
  assert.equal(status.sheetsUnchanged, 0);
  assert.equal(status.changed, 0);
  // Rows that read as stored are still not rewritten.
  assert.equal(writes(), 0);
  store.close();
});

test('a moved row is still written at its new row number', async () => {
  const { store, sheets } = await fixture();
  const moving = store.getRecordBySheetRow('Insectary_data', 4);
  // A row inserted above A2A in Google Sheets.
  const rows = sheets.rows.get('Insectary_data');
  const target = rows.find(r => r.row === 4);
  target.row = 5;
  await sheets.externalEdit('Insectary_data', 4, { Insectary_ID: 'A9A', SPECIES: 'Oleria onega', Sex: 'male' });
  const status = await store.sync({ sheets: SHEETS });
  assert.equal(status.moved, 1);
  assert.equal(status.added, 1);
  assert.equal(store.getRecordBySheetRow('Insectary_data', 5).id, moving.id);
  assert.equal(store.getRecordBySheetRow('Insectary_data', 4).values.Insectary_ID, 'A9A');
  store.close();
});
