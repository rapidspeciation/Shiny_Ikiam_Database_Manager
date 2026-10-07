// Pre-made rows: made as copies of the last one (formulas, formats, dropdowns),
// and made automatically before a save would write a bare row. In Insectary_data
// they are the rows with an Insectary ID: the ID series goes on into the rows
// below, which mostly hold the other formulas already; rows are added only when
// none are left, and when Google refuses them PAS is asked to add them.
import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets, insectaryIdValue, shiftFormula } from '../server/sheets.mjs';
import { applyBatch } from '../server/batch.mjs';
import { idSuggestions } from '../server/grid.mjs';
import { extendPremadeRows, insectaryIdRow, nextInSeries } from '../server/premade.mjs';
import { moduleMap } from '../server/schema.mjs';

const SHEET = 'Insectary_data';
const col = key => moduleMap.get(SHEET).fields.find(f => f.key === key).column;
const reviewer = { id: 'rev-1', username: 'rev', role: 'reviewer' };
const editor = { id: 'ed-1', username: 'ed', role: 'editor' };

// The workbook's ID formula: the ID of the row above, plus one.
const idFormula = row =>
  `=IF(MID(A${row - 1},2,1)="9", CHAR(CODE(LEFT(A${row - 1},1))+1) & "0D", LEFT(A${row - 1},1) & (MID(A${row - 1},2,1)+1) & "D")`;
// Its newer form (since round F), which takes the round from the ID above.
const letFormula = row =>
  `=LET(p, LEFT(A${row - 1}, 3), l, LEFT(p, 1), n, VALUE(MID(p, 2, 1)), s, RIGHT(p, 1), IF(n < 9, l & (n + 1) & s, IF(l <> "Z", CHAR(CODE(l) + 1) & "0" & s, "A0" & CHAR(CODE(s) + 1))))`;
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
 * Rows as in the workbook: used rows up to `used`, then pre-made rows with the
 * ID formula up to `withIds`, then rows whose ID column was not filled down (up
 * to `last`; in the real workbook thousands). Every row has Pedigree and
 * DATE_OF_COLLECTION formulas, a dropdown on Sex and a date format on Death_date.
 * `let`: the IDs by the newer formula.
 */
function workbook({ used, withIds, last, typedOver = [], firstId = 'Q0D', firstRow = 2, let: byLet = false }) {
  const rows = [];
  let id = firstId;
  for (let row = firstRow; row <= last; row++) {
    const cells = [];
    if (row <= withIds) {
      cells[col('Insectary_ID')] = row === firstRow ? V(id) : F((byLet ? letFormula : idFormula)(row), id);
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

/** `requests`: what each batchUpdate sent (by kind); `sheet()`: the sheet's rows, to see nothing was written. */
async function fixture(shape, options = {}) {
  const sheets = new LocalSheets({ [SHEET]: workbook(shape) }, { evaluate, ...options });
  const requests = [];
  const batchUpdate = sheets.batchUpdate.bind(sheets);
  sheets.batchUpdate = list => {
    requests.push(...list.map(r => Object.keys(r)[0]));
    return batchUpdate(list);
  };
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: [SHEET] });
  return {
    sheets,
    store,
    requests,
    sheet: () => JSON.stringify(sheets.rows.get(SHEET)),
    cell: (row, key) => sheets.cell(SHEET, row, col(key)),
  };
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

test('the lab works out both forms of the ID formula, as Sheets does', () => {
  const above = id => () => id;
  assert.equal(insectaryIdValue(idFormula(9), above('Q5D')), 'Q6D');
  assert.equal(insectaryIdValue(idFormula(9), above('Q9D')), 'R0D');
  assert.equal(insectaryIdValue(idFormula(9), above('Z9D')), '[0D', 'the older form keeps its round');
  assert.equal(insectaryIdValue(idFormula(9), above('W2D.1')), 'W3D');
  assert.equal(insectaryIdValue(letFormula(9), above('B8F')), 'B9F');
  assert.equal(insectaryIdValue(letFormula(9), above('B9F')), 'C0F');
  assert.equal(insectaryIdValue(letFormula(9), above('Z9F')), 'A0G', 'the newer one starts the next round');
  assert.equal(insectaryIdValue(letFormula(9), above('W2B.1')), 'W3B');
  assert.equal(insectaryIdValue('=IFS(K9="","",K9="NA","NA")', above('Q5D')), undefined);
});

test('Insectary_data: more IDs go into the rows below the last ID, which hold the other formulas already; no row is added', async () => {
  // As the real workbook: used to row 5, IDs to row 8 (Q6D), the other formulas filled down to row 40.
  const { sheets, store, cell, requests } = await fixture({ used: 5, withIds: 8, last: 40, let: true }, { evaluate: undefined });
  assert.equal(idSuggestions(store, { kind: 'insectary', count: 5000 }).freeAtEnd, 3);
  const before = cell(14, 'Pedigree');
  const result = await extendPremadeRows(store, SHEET, 10, reviewer);
  assert.equal(result.ok, true, JSON.stringify(result.problems));
  assert.deepEqual(result.filled, { from: 9, to: 18, count: 10 });
  assert.equal(result.added, null);
  assert.equal(result.firstId, 'Q7D');
  assert.equal(result.lastId, 'R6D');
  assert.equal(result.ids, 10);
  assert.deepEqual(result.checks, { formulas: true, validation: true, formats: true, ids: true });
  // Only the ID cells were written: no row added, nothing pasted.
  assert.deepEqual(requests, ['updateCells']);
  assert.equal(sheets.rowCount(SHEET), 40);
  assert.equal(cell(9, 'Insectary_ID').userEnteredValue.formulaValue, letFormula(9));
  assert.equal(cell(18, 'Insectary_ID').effectiveValue.stringValue, 'R6D');
  assert.equal(cell(19, 'Insectary_ID'), undefined);
  assert.deepEqual(cell(14, 'Pedigree'), before);
  const ids = idSuggestions(store, { kind: 'insectary', count: 5000 });
  assert.equal(ids.freeAtEnd, 13);
  assert.equal(ids.last, 'R6D');
  store.close();
});

test('Insectary_data: rows without formulas below the last ID get the last formula row copied; rows are added only past the grid', async () => {
  // Used up to row 7 (Q5D), IDs pre-made to row 7, pre-made rows without IDs 8–10, the grid ends at 10.
  const { sheets, store, cell } = await fixture({ used: 7, withIds: 7, last: 10 });
  assert.equal(idSuggestions(store, { kind: 'insectary', count: 5000 }).freeAtEnd, 0);
  const result = await extendPremadeRows(store, SHEET, 5, reviewer);
  assert.equal(result.ok, true, JSON.stringify(result.problems));
  assert.equal(result.template, 10);
  assert.deepEqual(result.filled, { from: 8, to: 12, count: 5 });
  assert.deepEqual(result.added, { from: 11, to: 12, count: 2 });
  assert.equal(result.completed, null);
  assert.equal(result.firstId, 'Q6D');
  assert.equal(result.lastId, 'R0D');
  assert.equal(result.ids, 5);
  assert.deepEqual(result.checks, { formulas: true, validation: true, formats: true, ids: true });
  assert.deepEqual(result.ownerColumns, []);
  for (let row = 11; row <= 12; row++) {
    assert.equal(cell(row, 'Pedigree').userEnteredValue.formulaValue, `=IFS(K${row}="","",K${row}="NA","NA")`);
    assert.deepEqual(cell(row, 'Sex').dataValidation, SEX_LIST);
    assert.equal(cell(row, 'Sex').userEnteredValue, undefined);
    assert.deepEqual(cell(row, 'Death_date').userEnteredFormat, DATE);
  }
  assert.equal(cell(8, 'Insectary_ID').userEnteredValue.formulaValue, idFormula(8));
  assert.equal(cell(12, 'Insectary_ID').userEnteredValue.formulaValue, idFormula(12));
  assert.equal(sheets.rowCount(SHEET), 12);
  // Nothing else changed: the used rows keep their values.
  assert.equal(cell(7, 'Sex').userEnteredValue.stringValue, 'male');
  // The app's copy has the new free IDs.
  const ids = idSuggestions(store, { kind: 'insectary', count: 5000 });
  assert.equal(ids.freeAtEnd, 5);
  assert.equal(ids.sequence[0], 'Q6D');
  assert.equal(ids.last, 'R0D');
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

test('protected columns the credential cannot edit are left for the owner, and the rest is written into the empty rows of the grid', async () => {
  const at = col('DATE_OF_COLLECTION');
  const { sheets, store, cell, requests } = await fixture(
    { used: 5, withIds: 7, last: 7 },
    { protectedRanges: { [SHEET]: [{ startColumnIndex: at, endColumnIndex: at + 1 }] } },
  );
  // The grid goes on to row 20 with empty rows (no formulas).
  sheets.gridRows.set(SHEET, 20);
  const result = await extendPremadeRows(store, SHEET, 4, reviewer);
  assert.deepEqual(result.ownerColumns, ['AS (DATE_OF_COLLECTION)']);
  assert.deepEqual(result.protectedRanges, [{ columns: 'AS–AS', rows: 'todas', canEdit: false }]);
  assert.equal(result.ok, true, JSON.stringify(result.problems));
  assert.equal(result.added, null);
  assert.deepEqual(result.filled, { from: 8, to: 11, count: 4 });
  assert.ok(!requests.includes('appendDimension'));
  assert.equal(sheets.rowCount(SHEET), 20);
  assert.equal(cell(9, 'DATE_OF_COLLECTION'), undefined);
  assert.equal(cell(9, 'Pedigree').userEnteredValue.formulaValue, '=IFS(K9="","",K9="NA","NA")');
  assert.deepEqual(cell(11, 'Sex').dataValidation, SEX_LIST);
  assert.equal(cell(12, 'Pedigree'), undefined);
  assert.equal(result.lastId, 'Q9D');
  store.close();
});

test('Insectary_data: with no rows left and Google refusing to add them, PAS is asked and nothing is written', async () => {
  const at = col('DATE_OF_COLLECTION');
  const locked = { protectedRanges: { [SHEET]: [{ startColumnIndex: at, endColumnIndex: at + 1 }] } };
  // IDs to the grid's last row (8): no rows left.
  let f = await fixture({ used: 5, withIds: 8, last: 8 }, locked);
  let before = f.sheet();
  await assert.rejects(
    extendPremadeRows(f.store, SHEET, 5, reviewer),
    e =>
      e.code === 'NO_ROWS_LEFT' &&
      e.message === 'No quedan filas con fórmulas al final de Insectary_data: pide a PAS que añada filas',
  );
  assert.equal(f.sheet(), before);
  assert.equal(f.sheets.rowCount(SHEET), 8);
  // A save of an ID past them is refused the same way, whole.
  await assert.rejects(
    applyBatch(
      f.store,
      {
        requestId: randomUUID(),
        creates: [
          { module: SHEET, values: { Insectary_ID: 'Q8D', Wild_Reared: 'Reared' } },
          { module: SHEET, values: { Insectary_ID: 'Q4D', Wild_Reared: 'Reared' } },
        ],
      },
      editor,
    ),
    e => e.code === 'NO_ROWS_LEFT',
  );
  assert.equal(f.sheet(), before);
  f.store.close();
  // Two rows left, five asked: none is filled either.
  f = await fixture({ used: 5, withIds: 8, last: 10 }, locked);
  before = f.sheet();
  await assert.rejects(
    extendPremadeRows(f.store, SHEET, 5, reviewer),
    e =>
      e.code === 'NO_ROWS_LEFT' &&
      e.message === 'Quedan 2 filas al final de Insectary_data y hacen falta 5: pide a PAS que añada filas',
  );
  assert.equal(f.sheet(), before);
  // The two left can be filled.
  const result = await extendPremadeRows(f.store, SHEET, 2, reviewer);
  assert.equal(result.ok, true, JSON.stringify(result.problems));
  assert.deepEqual([result.firstId, result.lastId, result.added], ['Q7D', 'Q8D', null]);
  f.store.close();
});

test('a save with an Insectary ID ahead of the pre-made ones fills the IDs into the rows below, up to its row only', async () => {
  // Used Q0D–Q3D (rows 2–5), free Q4D–Q6D (rows 6–8), the other formulas down to row 40.
  const { sheets, store, cell, requests } = await fixture({ used: 5, withIds: 8, last: 40 });
  assert.deepEqual(insectaryIdRow(store, 'R5D'), { row: 17, ahead: true });
  const result = await applyBatch(
    store,
    { requestId: randomUUID(), creates: [{ module: SHEET, values: { Insectary_ID: 'R5D', Wild_Reared: 'Reared' } }] },
    editor,
  );
  assert.equal(result.status, 'verified');
  assert.deepEqual([result.records[0].row, result.records[0].values.Insectary_ID], [17, 'R5D']);
  assert.ok(!requests.includes('appendDimension') && !requests.includes('copyPaste'));
  assert.equal(sheets.rowCount(SHEET), 40);
  assert.equal(cell(9, 'Insectary_ID').effectiveValue.stringValue, 'Q7D');
  assert.equal(cell(17, 'Insectary_ID').userEnteredValue.formulaValue, idFormula(17));
  assert.equal(cell(17, 'Wild_Reared').userEnteredValue.stringValue, 'Reared');
  assert.equal(cell(18, 'Insectary_ID'), undefined, 'no ID past the row the save needs');
  // Q7D–R4D wait in their rows.
  assert.equal(idSuggestions(store, { kind: 'insectary', count: 5000 }).freeAtEnd, 11);
  store.close();
});

test('the row of a second butterfly with an ID (W2B.2) that Google does not let the app insert: PAS is asked, nothing is written', async () => {
  const at = col('DATE_OF_COLLECTION');
  const { store, sheet } = await fixture(
    { used: 5, withIds: 8, last: 8 },
    { protectedRanges: { [SHEET]: [{ startColumnIndex: at, endColumnIndex: at + 1 }] } },
  );
  const before = sheet();
  await assert.rejects(
    applyBatch(store, { requestId: randomUUID(), creates: [{ module: SHEET, values: { Insectary_ID: 'Q1D.1', Sex: 'female' } }] }, editor),
    e => e.code === 'ROWS_PROTECTED' && /pide a PAS que inserte la fila/.test(e.message) && /Q1D\.1/.test(e.message),
  );
  assert.equal(sheet(), before);
  assert.equal(store.getRecordBySheetRow(SHEET, 4).values.Insectary_ID, 'Q2D', 'the rows below did not move');
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
