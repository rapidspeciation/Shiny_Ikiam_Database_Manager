import test from 'node:test';
import assert from 'node:assert/strict';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { cellText, searchAll, searchRange } from '../server/search.mjs';

// Insectary_data rows 2–41: A0B, A1B…; rows 20–23 have tubes typed FS58490… (a 0 missing), the rest FS508490….
const insectary = Array.from({ length: 40 }, (_, i) => {
  const row = i + 2;
  const id = `${'ABCD'[Math.floor(i / 10)]}${i % 10}B`;
  const tube = row >= 20 && row <= 23 ? `FS58490${row}` : `FS508490${String(row).padStart(2, '0')}`;
  return {
    row,
    values: {
      Insectary_ID: id,
      SPECIES: row % 2 ? 'Melinaea menophilus' : 'Mechanitis polymnia',
      CAM_ID: `CAM0800${String(row).padStart(2, '0')}`,
      Tube_1_id: tube,
      Preservation_date: 46168 + row, // 26-May-26 at row 0
    },
  };
});
const seed = {
  Insectary_data: insectary,
  Collection_data: [
    { row: 2, values: { CAM_ID: 'CAM080010', Insectary_ID: 'D9B', SPECIES: 'Oleria onega', Collection_time: 0.3958333333 } },
    { row: 3, values: { CAM_ID: 'CAM080099', SPECIES: 'Hypothyris anastasia', Notes_Collection_data: 'Wing clip of B5B lost' } },
  ],
  'F1/F2_MutationRate': [{ row: 2, values: { Insectary_ID: 'X1X', Mother_ID: 'B5B', SPECIES: 'Melinaea menophilus' } }],
};

async function fixture() {
  const sheets = new LocalSheets(structuredClone(seed));
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: Object.keys(seed) });
  return store;
}
const rowOf = (store, sheet, wire) => {
  const keys = store.listModules().find(m => m.id === sheet).fields.map(f => f.key);
  return Object.fromEntries(keys.map((k, i) => [k, wire.v[i]]));
};

test('an ID is found in every sheet that has it, the sheet where it is the row ID first', async () => {
  const store = await fixture();
  const result = searchAll(store, { q: ' b5b ', context: 3 });
  assert.equal(result.query, 'b5b');
  assert.deepEqual(
    result.sheets.map(s => [s.module, s.total, s.idExact, s.exact]),
    [
      ['Insectary_data', 1, 1, 1],
      // Exact in Mother_ID (not its ID column), then a note that mentions it.
      ['F1/F2_MutationRate', 1, 0, 1],
      ['Collection_data', 1, 0, 0],
    ],
  );
  const [ins] = result.sheets;
  assert.deepEqual(result.sheets.map(s => s.columns), [['Insectary_ID'], ['Mother_ID'], ['Notes_Collection_data']]);
  // B5B is the 16th butterfly: row 17. Three rows before and after it come along.
  assert.equal(ins.focus, 17);
  assert.deepEqual(ins.matches, [17]);
  assert.equal(ins.window.from, 14);
  assert.equal(ins.window.to, 20);
  assert.deepEqual(
    ins.window.rows.map(r => r.row),
    [14, 15, 16, 17, 18, 19, 20],
  );
  assert.equal(rowOf(store, 'Insectary_data', ins.window.rows[3]).Insectary_ID, 'B5B');
  assert.deepEqual({ first: ins.first, last: ins.last }, { first: 2, last: 41 });
  // The other sheets' windows are their own rows.
  assert.deepEqual(result.sheets[2].window.rows.map(r => r.row), [2, 3]);
  // A CAM ID is an ID in both sheets: the insectary's comes first, as in the sheet list.
  assert.deepEqual(
    searchAll(store, { q: 'CAM080010', context: 1 }).sheets.map(s => [s.module, s.idExact, s.focus, s.window.rows.length]),
    [
      ['Insectary_data', 1, 10, 3],
      ['Collection_data', 1, 2, 2],
    ],
  );
  store.close();
});

test('a part of a value matches; the sheet opens at the newest match, and every match is listed', async () => {
  const store = await fixture();
  const good = searchAll(store, { q: 'fs5084', context: 2 }).sheets;
  assert.deepEqual(good.map(s => [s.module, s.total, s.exact]), [['Insectary_data', 36, 0]]);
  // The tubes missing a 0 sit together: the window around the newest shows the row after them too.
  const [typo] = searchAll(store, { q: 'FS584', context: 2 }).sheets;
  assert.deepEqual(typo.matches, [20, 21, 22, 23]);
  assert.equal(typo.focus, 23);
  assert.deepEqual(
    typo.window.rows.map(r => r.row),
    [21, 22, 23, 24, 25],
  );
  // A whole tube ID: the exact match, though not in an ID column.
  const [one] = searchAll(store, { q: 'FS5849021' }).sheets;
  assert.deepEqual([one.total, one.exact, one.idExact, one.focus], [1, 1, 0, 21]);
  store.close();
});

test('the text is matched in the values as shown, never in column names; dates and times as the grid shows them', async () => {
  const store = await fixture();
  // Every row has a SPECIES column: only the values count.
  assert.deepEqual(searchAll(store, { q: 'species' }).sheets, []);
  assert.deepEqual(
    searchAll(store, { q: 'MELINAEA' }).sheets.map(s => [s.module, s.total]),
    [
      ['Insectary_data', 20],
      ['F1/F2_MutationRate', 1],
    ],
  );
  // Preservation_date 46168 + 2 is 28-May-26.
  assert.equal(cellText(46170, { key: 'Preservation_date', type: 'date' }), '28-May-26');
  const date = searchAll(store, { q: '28-may-26' }).sheets;
  assert.deepEqual(date.map(s => [s.module, s.matches]), [['Insectary_data', [2]]]);
  assert.equal(date[0].exact, 1);
  // 0.3958… of a day is 09:30.
  assert.deepEqual(searchAll(store, { q: '09:30' }).sheets.map(s => [s.module, s.matches]), [['Collection_data', [2]]]);
  // LIKE's wildcards are plain text here.
  assert.deepEqual(searchAll(store, { q: 'CAM_%' }).sheets, []);
  // Too short to search.
  assert.deepEqual(searchAll(store, { q: 'B' }).sheets, []);
  store.close();
});

test('a pinned sheet comes first; sheets past the first ones come without rows', async () => {
  const store = await fixture();
  const result = searchAll(store, { q: 'B5B', pin: 'Collection_data', sheets: 1 });
  assert.deepEqual(
    result.sheets.map(s => s.module),
    ['Collection_data', 'Insectary_data', 'F1/F2_MutationRate'],
  );
  assert.ok(result.sheets[0].window);
  assert.equal(result.sheets[1].window, null);
  assert.equal(result.sheets[1].focus, 17);
  store.close();
});

test('rows by range, for scrolling a result', async () => {
  const store = await fixture();
  const range = searchRange(store, { module: 'Insectary_data', from: '38', to: '60' });
  assert.deepEqual(
    range.rows.map(r => r.row),
    [38, 39, 40, 41],
  );
  assert.equal(range.to, 60);
  const wire = range.rows[0];
  assert.equal(typeof wire.id, 'string');
  assert.equal(typeof wire.version, 'number');
  assert.equal(rowOf(store, 'Insectary_data', wire).Insectary_ID, 'D6B');
  assert.throws(() => searchRange(store, { module: 'Insectary_data', from: 10, to: 5 }), { code: 'INVALID_RANGE' });
  assert.throws(() => searchRange(store, { module: 'Nope', from: 1, to: 5 }), { code: 'MODULE_NOT_FOUND' });
  store.close();
});
