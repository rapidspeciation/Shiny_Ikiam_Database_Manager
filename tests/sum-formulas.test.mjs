import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { applyBatch } from '../server/batch.mjs';
import { moduleMap, simpleSum } from '../server/schema.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { signIn } from './helpers/assistant.mjs';

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
/** Row 3: clutch 121, its eggs kept as the sum =41+36, and `larvae`. */
async function clutch121(store, sheets, larvae) {
  const col = key => moduleMap.get('Insectary_stocks').fields.find(f => f.key === key).column;
  const cells = [];
  cells[col('CLUTCH NUMBER')] = { userEnteredValue: { numberValue: 121 } };
  cells[col('NUMBER OF EGGS')] = formula('=41+36', 77);
  cells[col('NUMBER OF LARVAE')] = larvae;
  sheets.rows.get('Insectary_stocks').find(r => r.row === 3).cells = cells;
  await store.sync({ sheets: ['Insectary_stocks'] });
}

test('simple sums are recognised, a count may subtract (27 larvae, 5 died); anything else is not', () => {
  assert.equal(simpleSum('=12+15'), '=12+15');
  assert.equal(simpleSum('12 + 15 + 3'), '=12+15+3');
  assert.equal(simpleSum('=27'), '=27');
  assert.equal(simpleSum('27-5'), '=27-5');
  assert.equal(simpleSum('= 4 + 6 - 10'), '=4+6-10');
  assert.equal(simpleSum('27'), null);
  assert.equal(simpleSum('=A1+3'), null);
  assert.equal(simpleSum('=SUM(1,2)'), null);
});

test('counts are written as the notebook sums them, over an old sum, but never over a real formula', async () => {
  const { store, sheets } = await fixture();
  // Row 3: clutch 121 with eggs =41+36 and larvae computed by a real formula.
  await clutch121(store, sheets, formula('=COUNTIF(A:A,1)', 3));
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

// The review table shows a count kept as a sum as its formula (=41+36), not the total it computes to (77):
// a person who types the sheet's sum back, or sets the cell back to the sheet, sees the sum, not a number.
test('a proposal shows the sheet sums of its counts as formulas', async () => {
  const { store, sheets } = await fixture();
  try {
    await clutch121(store, sheets, formula('=50', 50));
    const assistant = createAssistant({ store, config: {} });
    const { result, get } = signIn(store, assistant);
    const clutch = store.getRecordBySheetRow('Insectary_stocks', 3);
    const out = await result('propose_changes', { reason: 'Posturas', changes: [{ recordId: clutch.id, values: { 'NUMBER OF EGGS': '=41+30' } }] });
    assert.ok(!out.isError, out.content[0].text);
    const shown = (await get('/api/chat/proposals', { all: '1' })).body.proposals[0].changes[0];
    assert.equal(shown.rowValues['NUMBER OF EGGS'], '=41+36');
    // A single number typed as =50 is a sum too: the same as the person would type it.
    assert.equal(shown.rowValues['NUMBER OF LARVAE'], '=50');
    assert.equal(shown.rowValues['CLUTCH NUMBER'], 121);
  } finally {
    store.close();
  }
});
