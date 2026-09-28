import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { applyBatch } from '../server/batch.mjs';
import { simpleSum } from '../server/schema.mjs';

const user = { id: 'editor-1', username: 'editor', role: 'editor' };
const formula = (f, value) => ({ userEnteredValue: { formulaValue: f }, effectiveValue: { numberValue: value } });

async function fixture() {
  const sheets = new LocalSheets({
    Insectary_stocks: [
      { row: 2, values: { 'CLUTCH NUMBER': 120, SPECIES: 'Mechanitis polymnia proceriformis' } },
      // Clutch 121: eggs typed as a sum; a column holding a real formula next to it.
      { row: 3, cells: [] },
    ],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_stocks'] });
  return { store, sheets };
}

test('simple sums are recognised, anything else is not', () => {
  assert.equal(simpleSum('=12+15'), '=12+15');
  assert.equal(simpleSum('12 + 15 + 3'), '=12+15+3');
  assert.equal(simpleSum('=27'), '=27');
  assert.equal(simpleSum('27'), null);
  assert.equal(simpleSum('=A1+3'), null);
  assert.equal(simpleSum('=SUM(1,2)'), null);
});

test('counts are written as the notebook sums them, over an old sum, but never over a real formula', async () => {
  const { store, sheets } = await fixture();
  const mod = (await import('../server/schema.mjs')).moduleMap.get('Insectary_stocks');
  const col = key => mod.fields.find(f => f.key === key).column;
  // Row 3: clutch 121 with eggs =41+36 and larvae computed by a real formula.
  const cells = [];
  cells[col('CLUTCH NUMBER')] = { userEnteredValue: { numberValue: 121 } };
  cells[col('NUMBER OF EGGS')] = formula('=41+36', 77);
  cells[col('NUMBER OF LARVAE')] = formula('=COUNTIF(A:A,1)', 3);
  sheets.rows.get('Insectary_stocks').find(r => r.row === 3).cells = cells;
  await store.sync({ sheets: ['Insectary_stocks'] });
  const clutch = row => store.getRecordBySheetRow('Insectary_stocks', row);

  // An empty count takes the notebook's sum as a formula.
  let saved = await applyBatch(
    store,
    { requestId: randomUUID(), edits: [{ id: clutch(2).id, values: { 'NUMBER OF EGGS': '12+15' }, expected: { 'NUMBER OF EGGS': null } }] },
    user,
  );
  assert.equal(saved.status, 'verified');
  assert.equal(clutch(2).formulas['NUMBER OF EGGS'], '=12+15');

  // An old sum is replaced by the corrected one (the person saw "=41+36").
  saved = await applyBatch(
    store,
    { requestId: randomUUID(), edits: [{ id: clutch(3).id, values: { 'NUMBER OF EGGS': '=41+36+2' }, expected: { 'NUMBER OF EGGS': '=41+36' } }] },
    user,
  );
  assert.equal(saved.status, 'verified');
  assert.equal(clutch(3).formulas['NUMBER OF EGGS'], '=41+36+2');

  // A real formula stays the sheet's.
  await assert.rejects(
    applyBatch(store, { requestId: randomUUID(), edits: [{ id: clutch(3).id, values: { 'NUMBER OF LARVAE': '=3+1' } }] }, user),
    e => e.details?.items?.[0]?.code === 'FORMULA_CELL',
  );
  store.close();
});
