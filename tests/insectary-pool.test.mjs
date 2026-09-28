import test from 'node:test';
import assert from 'node:assert/strict';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { idSuggestions } from '../server/grid.mjs';

// Pre-made rows as in the workbook: the round letter at the end (…B, then …D), rows used out of order.
async function fixture() {
  const sheets = new LocalSheets({
    Insectary_data: [
      { row: 2, values: { Insectary_ID: '85Y' } }, // an old form of ID, left empty years ago
      { row: 3, values: { Insectary_ID: 'H0B', SPECIES: 'Mechanitis polymnia eurydice', Sex: 'female' } },
      { row: 4, values: { Insectary_ID: 'H1B' } }, // an earlier empty row
      { row: 5, values: { Insectary_ID: 'H2B' } }, // empty, but a wild butterfly in Collection_data carries it
      { row: 6, values: { Insectary_ID: 'H3B' } }, // empty, named in a cross
      { row: 7, values: { Insectary_ID: 'H4B' } }, // empty, but its ID has two pre-made rows
      { row: 8, values: { Insectary_ID: 'H4B' } },
      { row: 9, values: { Insectary_ID: 'H5B' } },
      { row: 10, values: { Insectary_ID: 'M9D' } }, // an empty row of the current round
      { row: 11, values: { Insectary_ID: 'N1D', SPECIES: 'Ithomia salapia', Sex: 'male' } }, // the last row used
      { row: 12, values: { Insectary_ID: 'N2D' } },
      { row: 13, values: { Insectary_ID: 'N3D' } },
      { row: 14, values: {} },
    ],
    Collection_data: [{ row: 2, values: { Insectary_ID: 'H2B', SPECIES: 'Ithomia salapia' } }],
    'F1/F2_MutationRate': [{ row: 2, values: { SPECIES: 'Mechanitis polymnia', Mother_ID: 'h3b' } }],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data', 'Collection_data', 'F1/F2_MutationRate'] });
  return store;
}

test('free Insectary IDs: first the rows after the last one used, then the earlier empty rows nobody names', async () => {
  const store = await fixture();
  const ids = idSuggestions(store, { kind: 'insectary', count: 50 });
  // After the tail: the current round (D) first, then B, then the old forms.
  assert.deepEqual(ids.sequence, ['N2D', 'N3D', 'M9D', 'H1B', 'H5B', '85Y']);
  assert.equal(ids.tail, 2);
  assert.equal(ids.suggestions[0].value, 'N2D');
  assert.deepEqual(
    ids.rows.map(r => r.row),
    [12, 13, 10, 4, 9, 2],
  );
  store.close();
});

test('starting from an earlier empty row, the IDs follow in sheet order (H1B → H5B → M9D)', async () => {
  const store = await fixture();
  assert.deepEqual(idSuggestions(store, { kind: 'insectary', start: 'H1B', count: 3 }).sequence, ['H1B', 'H5B', 'M9D']);
  // Used by a butterfly, named in another sheet, or not unique: not offered.
  for (const id of ['H0B', 'H2B', 'H3B', 'H4B'])
    assert.throws(() => idSuggestions(store, { kind: 'insectary', start: id }), /not an unused pre-filled/);
  store.close();
});
