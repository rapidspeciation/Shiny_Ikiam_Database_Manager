import test from 'node:test';
import assert from 'node:assert/strict';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { tableChanges, tablePayload } from '../server/grid.mjs';
import { createSheetHook } from '../server/hooks.mjs';

async function fixture() {
  const sheets = new LocalSheets({
    Insectary_data: [
      { row: 2, values: { Insectary_ID: 'A0A', SPECIES: 'Mechanitis polymnia', Sex: 'male' } },
      { row: 3, values: { Insectary_ID: 'A1A', SPECIES: 'Mechanitis polymnia', Sex: 'female' } },
    ],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data'] });
  return { store, sheets };
}

test('an edit made in Google Sheets updates only the reported rows, with its history', async () => {
  const { store, sheets } = await fixture();
  const since = tablePayload(store, 'Insectary_data').latest;
  const before = store.getRecordBySheetRow('Insectary_data', 3);
  await sheets.externalEdit('Insectary_data', 3, { Sex: 'NA' });
  await sheets.externalEdit('Insectary_data', 4, { Insectary_ID: 'A2A', SPECIES: 'Oleria onega' });
  const result = await store.refreshRows('Insectary_data', [3, 4]);
  assert.deepEqual(result, { changed: 1, added: 1, removed: 0, needsSync: false });
  const after = store.getRecordBySheetRow('Insectary_data', 3);
  assert.equal(after.id, before.id);
  assert.equal(after.values.Sex, 'NA');
  assert.equal(after.version, before.version + 1);
  const history = store.db
    .prepare("SELECT c.field FROM changes c JOIN actions a ON a.id=c.action_id WHERE a.status='observed'")
    .all();
  assert.deepEqual(
    history.map(h => h.field),
    ['Sex'],
  );

  const delta = tableChanges(store, 'Insectary_data', since);
  assert.equal(delta.count, 3);
  // Changed rows come back (plus any written in the same instant as `since`).
  const changedRows = delta.rows.map(r => r.row);
  assert.ok(changedRows.includes(3) && changedRows.includes(4));
  assert.equal(delta.rows.find(r => r.row === 3).version, after.version);
  // Asking again from the reply's `latest` returns at most the rows written at that instant.
  assert.ok(tableChanges(store, 'Insectary_data', delta.latest).rows.length <= 2);

  // A cleared row disappears from the page.
  sheets.rows.get('Insectary_data').find(r => r.row === 4).cells = [];
  assert.equal((await store.refreshRows('Insectary_data', [4])).removed, 1);
  assert.equal(tableChanges(store, 'Insectary_data', delta.latest).removed.length, 1);
  store.close();
});

test('a row that matches no record above the end needs a sheet sync', async () => {
  const { store, sheets } = await fixture();
  store.db.prepare("DELETE FROM records WHERE sheet='Insectary_data' AND row_num=2").run();
  await sheets.externalEdit('Insectary_data', 2, { Sex: 'female' });
  assert.equal((await store.refreshRows('Insectary_data', [2])).needsSync, true);
  store.close();
});

test('the hook needs its secret, merges reports and syncs the sheet after row inserts', async () => {
  const { store, sheets } = await fixture();
  const hook = createSheetHook(store, { secret: 's3cret-value', delayMs: 5 });
  assert.throws(
    () => hook.receive({}, { sheet: 'Insectary_data', startRow: 3 }),
    e => e.code === 'HOOK_FORBIDDEN',
  );
  assert.throws(
    () => createSheetHook(store, {}).receive({ 'x-hook-secret': 'x' }, {}),
    e => e.code === 'HOOK_DISABLED',
  );
  const headers = { 'x-hook-secret': 's3cret-value' };
  await sheets.externalEdit('Insectary_data', 2, { Sex: 'NA' });
  assert.deepEqual(
    hook.receive(headers, {
      events: [
        { sheet: 'Insectary_data', startRow: 2, numRows: 1, change: 'EDIT' },
        { sheet: 'tube_locations', startRow: 5 },
      ],
    }),
    { accepted: 1 },
  );
  await new Promise(r => setTimeout(r, 30));
  await hook.flush();
  assert.equal(store.getRecordBySheetRow('Insectary_data', 2).values.Sex, 'NA');
  assert.equal(hook.status.lastResult, '1 rows');

  // A row inserted above shifts the rest: the whole sheet is read again.
  for (const row of sheets.rows.get('Insectary_data')) if (row.row >= 2) row.row++;
  sheets.rows.get('Insectary_data').push({ row: 2, cells: [] });
  await sheets.externalEdit('Insectary_data', 2, { Insectary_ID: 'Z9Z', SPECIES: 'Oleria onega' });
  sheets.rows.get('Insectary_data').sort((a, b) => a.row - b.row);
  hook.receive(headers, { sheet: 'Insectary_data', change: 'INSERT_ROW' });
  await new Promise(r => setTimeout(r, 30));
  await hook.flush();
  assert.equal(hook.status.lastResult, 'sheet');
  assert.equal(store.getRecordBySheetRow('Insectary_data', 3).values.Insectary_ID, 'A0A');
  assert.equal(store.getRecordBySheetRow('Insectary_data', 2).values.Insectary_ID, 'Z9Z');
  store.close();
});
