// Pre-made rows: made as copies of the last one (formulas, formats, dropdowns),
// with Insectary_data's ID series going on, and made automatically before a
// save would write a bare row.
import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets, shiftFormula } from '../server/sheets.mjs';
import { applyBatch } from '../server/batch.mjs';
import { idSuggestions } from '../server/grid.mjs';
import { extendPremadeRows, insectaryIdRow, nextInSeries } from '../server/premade.mjs';
import { createApp } from '../server/index.mjs';
import { moduleMap } from '../server/schema.mjs';

const SHEET = 'Insectary_data';
const col = key => moduleMap.get(SHEET).fields.find(f => f.key === key).column;
const reviewer = { id: 'rev-1', username: 'rev', role: 'reviewer' };
const editor = { id: 'ed-1', username: 'ed', role: 'editor' };

// The workbook's ID formula: the ID of the row above, plus one.
const idFormula = row =>
  `=IF(MID(A${row - 1},2,1)="9", CHAR(CODE(LEFT(A${row - 1},1))+1) & "0D", LEFT(A${row - 1},1) & (MID(A${row - 1},2,1)+1) & "D")`;
/** What Google computes for the formulas these tests use. */
function evaluate(formula, { value }) {
  const ref = /^=IF\(MID\(A(\d+),2,1\)/.exec(formula);
  if (ref) return nextInSeries(value(Number(ref[1]), 0)) ?? '#VALUE!';
  return null;
}
/** As `evaluate`, with the round letter the formula writes (after Z9D it gives "[0D", as Sheets does). */
function roundEvaluate(formula, { value }) {
  const ref = /^=IF\(MID\(A(\d+),2,1\)/.exec(formula);
  if (!ref) return null;
  const round = /"0([A-Z])"/.exec(formula)[1];
  const previous = String(value(Number(ref[1]), 0) ?? '');
  const digit = Number(previous[1]);
  return digit === 9
    ? `${String.fromCharCode(previous.charCodeAt(0) + 1)}0${round}`
    : `${previous[0]}${digit + 1}${round}`;
}
const SEX_LIST = { condition: { type: 'ONE_OF_RANGE', values: [{ userEnteredValue: '=Lists!$E$2:$E' }] }, strict: true };
const DATE = { numberFormat: { type: 'DATE', pattern: 'd-mmm-yy' } };
const F = (formula, value = null) => ({
  userEnteredValue: { formulaValue: formula },
  ...(value === null ? {} : { effectiveValue: { stringValue: value } }),
});
const V = value => ({ userEnteredValue: { stringValue: value }, effectiveValue: { stringValue: value } });

/**
 * Rows as in the test workbook: used rows up to `used`, then pre-made rows with
 * the ID formula up to `withIds`, then pre-made rows whose ID column was not
 * filled down (up to `last`). Every row has Pedigree and DATE_OF_COLLECTION
 * formulas, a dropdown on Sex and a date format on Death_date.
 */
function workbook({ used, withIds, last, typedOver = [], firstId = 'Q0D', firstRow = 2 }) {
  const rows = [];
  let id = firstId;
  for (let row = firstRow; row <= last; row++) {
    const cells = [];
    if (row <= withIds) {
      cells[col('Insectary_ID')] = row === firstRow ? V(id) : F(idFormula(row), id);
      id = nextInSeries(id);
    }
    cells[col('SPECIES')] = typedOver.includes(row) ? V('Mechanitis messenoides') : F(`=IFS(C${row}="","",TRUE,"x")`);
    cells[col('Sex')] = { dataValidation: SEX_LIST, ...(row <= used ? V('male') : {}) };
    cells[col('Death_date')] = { userEnteredFormat: DATE };
    cells[col('Pedigree')] = F(`=IFS(K${row}="","",K${row}="NA","NA")`);
    cells[col('DATE_OF_COLLECTION')] = F(`=M${row}`);
    if (row <= used) cells[col('Wild_Reared')] = V('Wild-caught');
    rows.push({ row, cells });
  }
  return rows;
}

async function fixture(shape, options = {}) {
  const sheets = new LocalSheets({ [SHEET]: workbook(shape) }, { evaluate, ...options });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: [SHEET] });
  return { sheets, store, cell: (row, key) => sheets.cell(SHEET, row, col(key)) };
}

test('formulas pasted lower move their relative references, as in Sheets', () => {
  assert.equal(
    shiftFormula('=IFS($G5="","",TRUE,XLOOKUP($G5,Location_data!$A:$A,Lists!B$2:B))', 3),
    '=IFS($G8="","",TRUE,XLOOKUP($G8,Location_data!$A:$A,Lists!B$2:B))',
  );
  assert.equal(shiftFormula(`='F1/F2_MutationRate'!A2&"A12"&LOG10(4)`, 1), `='F1/F2_MutationRate'!A3&"A12"&LOG10(4)`);
  assert.equal(nextInSeries('Q5D'), 'Q6D');
  assert.equal(nextInSeries('Q9D'), 'R0D');
});

test('extending Insectary_data continues the ID series over the rows without IDs, with formulas and dropdowns', async () => {
  // Used up to row 7 (Q5D), IDs pre-made to row 7, pre-made rows without IDs 8–10.
  const { sheets, store, cell } = await fixture({ used: 7, withIds: 7, last: 10 });
  assert.equal(idSuggestions(store, { kind: 'insectary', count: 5000 }).freeAtEnd, 0);
  const result = await extendPremadeRows(store, SHEET, 5, reviewer);
  assert.equal(result.ok, true, JSON.stringify(result.problems));
  assert.equal(result.template, 10);
  assert.deepEqual(result.added, { from: 11, to: 15, count: 5 });
  assert.deepEqual(result.completed, { from: 8, to: 10, columns: ['A (Insectary_ID)'] });
  assert.equal(result.firstId, 'Q6D');
  assert.equal(result.lastId, 'R3D');
  assert.equal(result.ids, 8);
  assert.deepEqual(result.checks, { formulas: true, validation: true, formats: true, ids: true });
  assert.deepEqual(result.ownerColumns, []);
  for (let row = 11; row <= 15; row++) {
    assert.equal(cell(row, 'Pedigree').userEnteredValue.formulaValue, `=IFS(K${row}="","",K${row}="NA","NA")`);
    assert.deepEqual(cell(row, 'Sex').dataValidation, SEX_LIST);
    assert.equal(cell(row, 'Sex').userEnteredValue, undefined);
    assert.deepEqual(cell(row, 'Death_date').userEnteredFormat, DATE);
  }
  assert.equal(cell(8, 'Insectary_ID').userEnteredValue.formulaValue, idFormula(8));
  assert.equal(sheets.rowCount(SHEET), 15);
  // Nothing else changed: the used rows keep their values.
  assert.equal(cell(7, 'Sex').userEnteredValue.stringValue, 'male');
  // The app's copy has the new free IDs.
  const ids = idSuggestions(store, { kind: 'insectary', count: 5000 });
  assert.equal(ids.freeAtEnd, 8);
  assert.equal(ids.sequence[0], 'Q6D');
  assert.equal(ids.last, 'R3D');
  store.close();
});

test('when the last pre-made row is already used, its values are not copied and typed-over formulas come back', async () => {
  const { store, cell } = await fixture({ used: 6, withIds: 6, last: 6, typedOver: [6] });
  const result = await extendPremadeRows(store, SHEET, 3, reviewer);
  assert.equal(result.ok, true, JSON.stringify(result.problems));
  assert.deepEqual(result.added, { from: 7, to: 9, count: 3 });
  assert.equal(result.firstId, 'Q5D');
  for (let row = 7; row <= 9; row++) {
    assert.equal(cell(row, 'Sex').userEnteredValue, undefined, 'the used row’s Sex is not copied');
    assert.equal(cell(row, 'Wild_Reared')?.userEnteredValue, undefined);
    assert.equal(cell(row, 'SPECIES').userEnteredValue.formulaValue, `=IFS(C${row}="","",TRUE,"x")`);
  }
  store.close();
});

test('after Z9D a new round starts, typed, after the IDs of that round already in the sheet', async () => {
  // Rows 2–3 hold A0E and A1E (typed by mistake long ago); the D series runs Z5D–Z9D in rows 4–8.
  const rows = [
    { row: 2, cells: [V('A0E')] },
    { row: 3, cells: [F(idFormula(3).replaceAll('D"', 'E"'), 'A1E')] },
    ...workbook({ used: 5, withIds: 8, last: 8, firstId: 'Z5D', firstRow: 4 }),
  ];
  const sheets = new LocalSheets({ [SHEET]: rows }, { evaluate: roundEvaluate });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: [SHEET] });
  const result = await extendPremadeRows(store, SHEET, 4, reviewer);
  assert.equal(result.ok, true, JSON.stringify(result.problems));
  assert.deepEqual(result.newRounds, [{ row: 9, id: 'A2E' }]);
  assert.equal(result.firstId, 'A2E');
  assert.equal(result.lastId, 'A5E');
  const a = row => sheets.cell(SHEET, row, col('Insectary_ID')).userEnteredValue;
  assert.deepEqual(a(9), { stringValue: 'A2E' });
  assert.equal(a(10).formulaValue, idFormula(10).replaceAll('D"', 'E"'));
  store.close();
});

test('protected columns the credential cannot edit are left for the owner, and the rest is written', async () => {
  const at = col('DATE_OF_COLLECTION');
  const { store, cell } = await fixture(
    { used: 5, withIds: 7, last: 7 },
    { protectedRanges: { [SHEET]: [{ startColumnIndex: at, endColumnIndex: at + 1 }] } },
  );
  const result = await extendPremadeRows(store, SHEET, 4, reviewer);
  assert.deepEqual(result.ownerColumns, ['AS (DATE_OF_COLLECTION)']);
  assert.deepEqual(result.protectedRanges, [{ columns: 'AS–AS', rows: 'todas', canEdit: false }]);
  assert.equal(result.ok, true, JSON.stringify(result.problems));
  assert.equal(cell(9, 'DATE_OF_COLLECTION'), undefined);
  assert.equal(cell(9, 'Pedigree').userEnteredValue.formulaValue, '=IFS(K9="","",K9="NA","NA")');
  assert.equal(result.lastId, 'Q9D');
  store.close();
});

test('rows typed without formulas after the pre-made rows stop the extension', async () => {
  const { sheets, store } = await fixture({ used: 5, withIds: 5, last: 5 });
  await sheets.externalEdit(SHEET, 7, { Sex: 'female' });
  await store.sync({ sheets: [SHEET] });
  await assert.rejects(extendPremadeRows(store, SHEET, 5, reviewer), e => e.code === 'BARE_ROWS');
  await assert.rejects(extendPremadeRows(store, SHEET, 501, reviewer), e => e.code === 'INVALID_COUNT');
  store.close();
});

test('a save that needs a row past the pre-made ones makes a block of them first, never a bare row', async () => {
  const sheets = new LocalSheets(
    {
      Insectary_stocks: [
        {
          row: 2,
          cells: Object.assign([], {
            0: V('990'),
            1: F('=IFS(A2="","",TRUE,"Mechanitis")', 'Mechanitis'),
            4: { dataValidation: SEX_LIST },
          }),
        },
      ],
    },
    { evaluate },
  );
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_stocks'] });
  const mod = moduleMap.get('Insectary_stocks');
  const clutch = mod.fields[0].key;
  const result = await applyBatch(
    store,
    { requestId: randomUUID(), creates: [{ module: 'Insectary_stocks', values: { [clutch]: '991' } }] },
    editor,
  );
  assert.equal(result.status, 'verified');
  const row = result.records[0].row;
  assert.equal(row, 3);
  const cells = (await sheets.readRow('Insectary_stocks', 3)).cells;
  assert.equal(cells[0].userEnteredValue.numberValue, 991);
  assert.equal(cells[1].userEnteredValue.formulaValue, '=IFS(A3="","",TRUE,"Mechanitis")', 'the row has formulas');
  assert.deepEqual(cells[4].dataValidation, SEX_LIST, 'and the dropdowns');
  assert.equal(sheets.rowCount('Insectary_stocks'), 22, 'a block of 20 pre-made rows was made');
  assert.equal((await sheets.readRow('Insectary_stocks', 4)).cells[0]?.userEnteredValue, undefined);
  store.close();
});

test('a new row named by an Insectary ID past the pre-made rows gets its row: the rows are made up to it', async () => {
  // Used Q0D–Q1D (rows 2–3), free pre-made Q2D–Q3D (rows 4–5); R5D's row will be 17.
  const { store, cell } = await fixture({ used: 3, withIds: 5, last: 5 });
  assert.deepEqual(insectaryIdRow(store, 'Q2D'), { row: 4 });
  assert.deepEqual(insectaryIdRow(store, 'r5d'), { row: 17, ahead: true });
  assert.equal(insectaryIdRow(store, 'Q1D'), null, 'used');
  assert.equal(insectaryIdRow(store, 'Q4E'), null, 'not in the series');
  const result = await applyBatch(
    store,
    {
      requestId: randomUUID(),
      creates: [
        { module: SHEET, values: { Insectary_ID: 'R5D', Wild_Reared: 'Reared' } },
        { module: SHEET, values: { Insectary_ID: 'Q2D', Wild_Reared: 'Reared' } },
      ],
    },
    editor,
  );
  assert.equal(result.status, 'verified');
  assert.deepEqual(
    result.records.map(r => [r.row, r.values.Insectary_ID]),
    [
      [17, 'R5D'],
      [4, 'Q2D'],
    ],
  );
  // The ID stays the sheet's formula; only the typed values are written.
  assert.equal(cell(17, 'Insectary_ID').userEnteredValue.formulaValue, idFormula(17));
  assert.equal(cell(17, 'Wild_Reared').userEnteredValue.stringValue, 'Reared');
  assert.equal(cell(5, 'Wild_Reared')?.userEnteredValue, undefined, 'Q3D stays free');
  store.close();
});

test('POST /api/sheets/:sheet/extend is for reviewers and admins, at most 500 rows', async () => {
  const sheets = new LocalSheets({ [SHEET]: workbook({ used: 3, withIds: 4, last: 4 }) }, { evaluate });
  const config = { databasePath: ':memory:', localMode: true, secureCookies: false, setupToken: 'setup-secret' };
  const app = await createApp({ ...config, syncIntervalMs: 0 }, { sheets });
  await app.ready;
  const address = await app.listen(0, '127.0.0.1');
  try {
    await app.store.sync({ sheets: [SHEET] });
    const base = `http://127.0.0.1:${address.port}/ithomiini`;
    let cookie = '',
      csrf = '';
    const call = async (path, method = 'GET', body) => {
      const response = await fetch(base + path, {
        method,
        headers: { 'content-type': 'application/json', ...(cookie ? { cookie, 'x-csrf-token': csrf } : {}) },
        body: body ? JSON.stringify({ requestId: randomUUID(), ...body }) : undefined,
      });
      const data = await response.json();
      if (response.headers.get('set-cookie')) cookie = response.headers.get('set-cookie').split(';')[0];
      if (data.csrf) csrf = data.csrf;
      return { status: response.status, data };
    };
    await call('/api/auth/setup', 'POST', { token: 'setup-secret', username: 'admin1', password: 'test-admin-123' });
    await call('/api/admin/users', 'POST', { username: 'editor1', password: 'test-editor-123', role: 'editor' });
    assert.equal((await call('/api/sheets/Insectary_data/extend', 'POST', { count: 501 })).status, 400);
    const done = await call('/api/sheets/Insectary_data/extend', 'POST', { count: 3 });
    assert.equal(done.status, 200, JSON.stringify(done.data));
    assert.equal(done.data.firstId, 'Q3D');
    assert.equal(done.data.lastId, 'Q5D');
    assert.equal((await call('/api/ids?kind=insectary&count=5000')).data.freeAtEnd, 4);
    await call('/api/auth/logout', 'POST', {});
    await call('/api/auth/login', 'POST', { username: 'editor1', password: 'test-editor-123' });
    assert.equal((await call('/api/sheets/Insectary_data/extend', 'POST', { count: 3 })).status, 403);
  } finally {
    await app.close();
  }
});
