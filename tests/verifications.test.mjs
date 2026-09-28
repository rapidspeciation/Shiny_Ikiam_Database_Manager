import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { applyBatch } from '../server/batch.mjs';
import { listOptions, listProblem } from '../server/verify.mjs';

const user = { id: 'editor-1', username: 'editor', role: 'editor' };

async function fixture() {
  const sheets = new LocalSheets({
    Collection_data: [
      { row: 2, values: { Insectary_ID: 'N5D', SPECIES: 'Ithomia salapia', Sex: 'female', Collection_location: 'Ikiam' } },
      { row: 3, values: { Insectary_ID: 'NA', SPECIES: 'Oleria onega', Sex: 'male', Collection_location: 'Ikiam' } },
    ],
    Location_data: [{ row: 2, values: { Collection_location: 'Ikiam' } }, { row: 3, values: { Collection_location: 'Apuya Y' } }],
    Insectary_data: [{ row: 2, values: { Insectary_ID: 'N5D', Wild_Reared: 'Wild-caught', Sex: 'female' } }],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Collection_data', 'Location_data', 'Insectary_data'] });
  return store;
}
const create = (values, module = 'Collection_data') => ({ requestId: randomUUID(), creates: [{ module, clientId: 'c1', values }] });
const refused = async (store, body, code) => {
  await assert.rejects(applyBatch(store, body, user, { source: 'app' }), e => {
    assert.equal(e.details?.items?.[0]?.code ?? e.code, code);
    return true;
  });
};

test('the sheet’s checks apply to what the app writes: repeated IDs and values outside strict lists are refused', async () => {
  const store = await fixture();
  // Insectary_ID repeats in Collection_data (the sheet colours it); NA may repeat.
  await refused(store, create({ Insectary_ID: 'N5D', SPECIES: 'Oleria onega', Sex: 'male', Collection_location: 'Ikiam' }), 'DUPLICATE_ID');
  const ok = await applyBatch(store, create({ Insectary_ID: 'NA', SPECIES: 'Oleria onega', Sex: 'NOT_COLLECTED', Collection_location: 'Ikiam' }), user, { source: 'app' });
  assert.equal(ok.status, 'verified');
  // Sex in Collection_data: female, male, female ?, male ?, NOT_COLLECTED (not NA).
  await refused(store, create({ Insectary_ID: 'NA', SPECIES: 'Oleria onega', Sex: 'NA', Collection_location: 'Ikiam' }), 'NOT_IN_LIST');
  // Collection_location comes from Location_data.
  await refused(store, create({ Insectary_ID: 'NA', SPECIES: 'Oleria onega', Sex: 'male', Collection_location: 'Narnia' }), 'NOT_IN_LIST');
  store.close();
});

test('lists read from other sheets, with the reason a value is outside them', async () => {
  const store = await fixture();
  const options = listOptions(store, 'Collection_data');
  assert.ok(options.Collection_location.values.has('Apuya Y'));
  assert.equal(options.Sex.strict, true);
  assert.equal(listProblem(options, 'Sex', 'female ?'), null);
  assert.match(listProblem(options, 'Collection_location', 'Narnia'), /Location_data · Collection_location/);
  // Insectary_data SPECIES is not strict in the sheet: only flagged, never refused.
  assert.equal(listOptions(store, 'Insectary_data').SPECIES, undefined); // its source (Lists) is not loaded here
  store.close();
});
