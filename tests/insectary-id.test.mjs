import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { applyIdChange, planIdChange } from '../server/insectaryId.mjs';

const user = { id: 'editor-1', username: 'editor', role: 'editor' };
const formulaCell = (formula, value) => ({
  userEnteredValue: { formulaValue: formula },
  effectiveValue: { stringValue: value },
});

async function fixture() {
  const sheets = new LocalSheets({
    Insectary_data: [
      // Saved as N5D, but the wings carry N6D.
      {
        row: 2,
        values: {
          Insectary_ID: 'N5D',
          Wild_Reared: 'Wild-caught',
          SPECIES: 'Ithomia salapia salapia',
          Sex: 'female',
          Intro2Insectary_date: 46288,
          CAM_ID: 'CAM000010',
          Tube_1_id: 'FS00000010',
        },
      },
      // The pre-made row of N6D (its ID is a formula, as in the workbook).
      { row: 3, cells: [formulaCell('=(ROW()+3)&"D"', 'N6D')] },
      // Another butterfly, N7D.
      { row: 4, values: { Insectary_ID: 'N7D', Wild_Reared: 'Wild-caught', SPECIES: 'Mechanitis lysimnia', Sex: 'male' } },
    ],
    Collection_data: [
      { row: 2, values: { Insectary_ID: 'N5D', SPECIES: 'Ithomia salapia', Release_Collect: 'Collected_Sent2Insectary' } },
      { row: 3, values: { Insectary_ID: 'N7D', SPECIES: 'Mechanitis lysimnia', Release_Collect: 'Collected_Sent2Insectary' } },
    ],
    Stocks_Matings: [{ row: 2, values: { male_ID: 'N7D', female_ID: 'N5D' } }],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data', 'Collection_data', 'Stocks_Matings'] });
  return store;
}
const row = (store, sheet, n) => store.getRecordBySheetRow(sheet, n).values;

test('a butterfly saved under the wrong ID moves to the pre-made row of the right one, and every reference follows', async () => {
  const store = await fixture();
  const plan = planIdChange(store, 'n5d', 'N6D');
  assert.equal(plan.mode, 'move');
  assert.deepEqual(
    plan.references.map(r => `${r.sheet}:${r.field}:${r.before}→${r.after}`),
    ['Collection_data:Insectary_ID:N5D→N6D', 'Stocks_Matings:female_ID:N5D→N6D'],
  );
  const saved = await applyIdChange(store, { from: 'N5D', to: 'N6D', requestId: randomUUID() }, user);
  assert.equal(saved.status, 'verified');
  assert.equal(row(store, 'Insectary_data', 3).SPECIES, 'Ithomia salapia salapia');
  assert.equal(row(store, 'Insectary_data', 3).CAM_ID, 'CAM000010');
  assert.equal(row(store, 'Insectary_data', 3).Insectary_ID, 'N6D');
  // The old row is free again, with its ID.
  assert.equal(row(store, 'Insectary_data', 2).Insectary_ID, 'N5D');
  assert.equal(row(store, 'Insectary_data', 2).SPECIES ?? null, null);
  assert.equal(row(store, 'Insectary_data', 2).CAM_ID ?? null, null);
  assert.equal(row(store, 'Collection_data', 2).Insectary_ID, 'N6D');
  assert.equal(row(store, 'Stocks_Matings', 2).female_ID, 'N6D');

  // One undo puts everything back.
  await store.undo({ actionIds: [saved.action.id], requestId: randomUUID() }, user);
  assert.equal(row(store, 'Insectary_data', 2).SPECIES, 'Ithomia salapia salapia');
  assert.equal(row(store, 'Collection_data', 2).Insectary_ID, 'N5D');
  assert.equal(row(store, 'Stocks_Matings', 2).female_ID, 'N5D');
  store.close();
});

test('two butterflies with each other’s IDs are swapped', async () => {
  const store = await fixture();
  const saved = await applyIdChange(store, { from: 'N5D', to: 'N7D', requestId: randomUUID() }, user);
  assert.equal(saved.plan.mode, 'swap');
  assert.equal(row(store, 'Insectary_data', 2).SPECIES, 'Mechanitis lysimnia');
  assert.equal(row(store, 'Insectary_data', 4).SPECIES, 'Ithomia salapia salapia');
  assert.equal(row(store, 'Collection_data', 2).Insectary_ID, 'N7D');
  assert.equal(row(store, 'Collection_data', 3).Insectary_ID, 'N5D');
  const mating = row(store, 'Stocks_Matings', 2);
  assert.deepEqual([mating.male_ID, mating.female_ID], ['N5D', 'N7D']);
  store.close();
});

test('an ID that is not recorded, or has no pre-made row, is refused', async () => {
  const store = await fixture();
  assert.throws(() => planIdChange(store, 'N9D', 'N6D'), /N9D no está registrado/);
  assert.throws(() => planIdChange(store, 'N5D', 'Z9Z'), /Z9Z no tiene fila preparada/);
  assert.throws(() => planIdChange(store, 'N5D', 'n5d'), /igual al actual/);
  store.close();
});
