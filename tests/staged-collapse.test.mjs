// Entries kept in the app from before a cell set back to the sheet's value cancelled them
// (a clutch's count changed and changed back as two entries): when the entries are listed
// (and when the app starts) the cells whose latest entry is what the sheet holds come out,
// as Staged.dropWaiting does for a cell set back now. Cells being written are left alone.
import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { LocalSheets } from '../server/sheets.mjs';
import { Store } from '../server/store.mjs';

const SPECIES = 'Mechanitis messenoides';

test("waiting entries that net to the sheet's value are cleaned when listed; others and those being written stay", async () => {
  const sheets = new LocalSheets({
    Insectary_stocks: [
      { row: 2, values: { 'CLUTCH NUMBER': 990, SPECIES, 'DATE LAID': 46280, 'NUMBER OF EGGS': 20, 'NUMBER OF LARVAE': { formula: '=12+3' } } },
      { row: 3, values: { 'CLUTCH NUMBER': 991, SPECIES, 'DATE LAID': 46281, 'NUMBER OF EGGS': 12 } },
    ],
  });
  const store = new Store({ localMode: true }, { sheets });
  try {
    await store.sync({ sheets: ['Insectary_stocks'] });
    const idOf = n =>
      store.db.prepare("SELECT id FROM records WHERE sheet='Insectary_stocks' AND json_extract(values_json,'$.\"CLUTCH NUMBER\"')=?").get(n).id;
    const insert = store.db.prepare(
      `INSERT INTO staged(id, entry_id, request_id, purpose, kind, sheet, record_id, label, values_json, expected_json, base_json, actor, created_at, updated_at, status)
       VALUES(?, ?, ?, 'clutches', 'edit', 'Insectary_stocks', ?, ?, ?, '{}', '{}', 'ana', ?, ?, ?)`,
    );
    const entry = (n, values, status = 'staged') => {
      const at = new Date().toISOString();
      insert.run(randomUUID(), randomUUID(), randomUUID(), idOf(n), String(n), JSON.stringify(values), at, at, status);
    };
    // 990: its sum changed and changed back, and its eggs changed and changed back with another cell kept.
    entry(990, { 'NUMBER OF LARVAE': { formula: '=12+3+4' } });
    entry(990, { 'NUMBER OF LARVAE': { formula: '=12+3' }, 'NUMBER OF EGGS': 25 });
    entry(990, { 'NUMBER OF EGGS': 20, 'PUPA DATE': 46300 });
    // 991: one changed back while another entry on it is being written: left alone.
    entry(991, { 'NUMBER OF EGGS': 14 }, 'sent');
    entry(991, { 'NUMBER OF EGGS': 12 });
    const list = store.staged.list();
    const cells = list.items.flatMap(i => Object.keys(i.values).map(f => `${i.label}:${f}`)).sort();
    assert.deepEqual(cells, ['990:PUPA DATE', '991:NUMBER OF EGGS', '991:NUMBER OF EGGS']);
    assert.equal(store.staged.count().staged, 2);
    // Nothing more to clean the next time.
    assert.equal(store.staged.collapseUnchanged(), 0);
  } finally {
    store.close();
  }
});
