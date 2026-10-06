import test from 'node:test';
import assert from 'node:assert/strict';
import { performance } from 'node:perf_hooks';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';

// A sync of a big sheet gives the other requests turns, and writes many moved rows in several transactions.

const ROWS = 3500;
const id = i => `B${String(i).padStart(5, '0')}`;
const seed = () =>
  Array.from({ length: ROWS }, (_, i) => ({ row: i + 2, values: { Insectary_ID: id(i), SPECIES: 'Oleria onega', Sex: i % 2 ? 'male' : 'female' } }));

test('a row inserted at the top of a big sheet: every record moves down one row, with turns for others meanwhile', async () => {
  const sheets = new LocalSheets({ Insectary_data: seed() });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data'] });
  const before = new Map(
    store.db.prepare("SELECT id, row_num, version FROM records WHERE sheet='Insectary_data' AND missing=0").all().map(r => [r.row_num, r]),
  );
  const version = store.copyVersion();

  // The row inserted in the sheet: everything below row 1 moves down, one row is edited.
  const [header, ...rows] = sheets.rows.get('Insectary_data');
  sheets.rows.set('Insectary_data', [header, ...rows.map(r => ({ ...r, row: r.row + 1 }))]);
  await sheets.externalEdit('Insectary_data', 3000, { Sex: 'NA' });

  let ticks = 0;
  let longest = 0;
  let last = performance.now();
  const beat = setInterval(() => {
    ticks++;
    longest = Math.max(longest, performance.now() - last);
    last = performance.now();
  }, 1);
  const status = await store.sync({ sheets: ['Insectary_data'], force: true });
  clearInterval(beat);
  assert.equal(status.moved, ROWS);
  assert.equal(status.changed, 1);
  assert.equal(status.added + status.missing, 0);
  assert.ok(ticks > 1, 'the event loop had turns during the sync');

  for (const [row, r] of before) {
    const now = store.getRecordBySheetRow('Insectary_data', row + 1);
    assert.equal(now.id, r.id);
    assert.equal(now.version, row + 1 === 3000 ? r.version + 1 : r.version);
  }
  assert.equal(store.getRecordBySheetRow('Insectary_data', 2), null);
  const history = store.db
    .prepare("SELECT c.row_num, c.field, c.after_json FROM changes c JOIN actions a ON a.id=c.action_id WHERE a.source='sheet_reconciliation'")
    .all();
  assert.deepEqual(history.map(h => ({ ...h })), [{ row_num: 3000, field: 'Sex', after_json: '"NA"' }]);
  // The copy's version moved, and the triggers that move it are back for the next writes.
  const after = store.copyVersion();
  assert.notEqual(after, version);
  const record = store.getRecordBySheetRow('Insectary_data', 5);
  store.persistRecord({ ...record, version: record.version + 1 });
  assert.notEqual(store.copyVersion(), after);
  store.close();
});
