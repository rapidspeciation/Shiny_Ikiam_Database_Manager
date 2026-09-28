import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { applyBatch, withoutConflicts } from '../server/batch.mjs';
import { idSuggestions } from '../server/grid.mjs';

const user = { id: 'editor-1', username: 'editor', role: 'editor' };

async function fixture() {
  const sheets = new LocalSheets({
    Insectary_data: [
      { row: 2, values: { Insectary_ID: 'N2D', Sex: 'female', CAM_ID: 'CAM078274', Tube_1_id: 'FS00000010' } },
      { row: 3, values: { Insectary_ID: 'N3D', Sex: 'male' } },
      { row: 4, values: { Insectary_ID: 'N4D', Sex: 'male' } },
    ],
    Collection_data: [{ row: 2, values: { CAM_ID: 'CAM000001', SPECIES: 'Oleria onega', Tube_1_id: 'FS00000020' } }],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data', 'Collection_data'] });
  return { store, sheets, at: row => store.getRecordBySheetRow('Insectary_data', row) };
}

test('a partial save writes every change except the one that conflicts, and says why in Spanish', async () => {
  const { store, at } = await fixture();
  const n3 = at(3),
    n4 = at(4);
  const requestId = randomUUID();
  const body = {
      requestId,
      partial: true,
      edits: [
        // The CAM is N2D's; the death date of the same row is fine.
        { id: n3.id, values: { CAM_ID: 'CAM078274', Death_date: '2026-09-27' }, expected: { CAM_ID: null, Death_date: null } },
        { id: n4.id, values: { Death_cause: 'Unknown' }, expected: { Death_cause: null } },
      ],
    };
  const result = await applyBatch(store, body, user);
  assert.equal(result.status, 'verified');
  assert.equal(at(3).values.Death_date, 46292);
  assert.equal(at(3).values.CAM_ID ?? null, null);
  assert.equal(at(4).values.Death_cause, 'Unknown');
  assert.equal(result.skipped.length, 1);
  assert.equal(result.skipped[0].code, 'DUPLICATE_ID');
  assert.equal(result.skipped[0].field, 'CAM_ID');
  assert.equal(result.skipped[0].id, n3.id);
  assert.equal(result.skipped[0].message, 'CAM078274 ya está usado en Insectary_data fila 2 (N2D)');
  // Retrying the same request (an unclear answer) reports the same cells left out.
  assert.deepEqual((await applyBatch(store, body, user)).skipped, result.skipped);
  store.close();
});

test('without `partial` one conflict still keeps the whole batch from saving', async () => {
  const { store, at } = await fixture();
  await assert.rejects(
    applyBatch(
      store,
      {
        requestId: randomUUID(),
        edits: [
          { id: at(3).id, values: { CAM_ID: 'CAM078274' } },
          { id: at(4).id, values: { Death_cause: 'Unknown' } },
        ],
      },
      user,
    ),
    e => e.code === 'BATCH_CONFLICT' && e.details.items[0].code === 'DUPLICATE_ID',
  );
  assert.equal(at(4).values.Death_cause ?? null, null);
  store.close();
});

test('a partial save where every change conflicts saves nothing and lists them all', async () => {
  const { store, at } = await fixture();
  await assert.rejects(
    applyBatch(
      store,
      {
        requestId: randomUUID(),
        partial: true,
        edits: [
          { id: at(3).id, values: { Tube_1_id: 'FS00000020' } },
          { id: at(4).id, values: { Death_date: '21/09/92026' } },
        ],
      },
      user,
    ),
    e => {
      assert.equal(e.code, 'BATCH_CONFLICT');
      assert.deepEqual(e.details.items.map(i => i.code).sort(), ['DUPLICATE_ID', 'INVALID_DATE']);
      assert.match(e.details.items.find(i => i.code === 'INVALID_DATE').message, /Fecha no válida en Death_date/);
      return true;
    },
  );
  store.close();
});

test('a date with an impossible year is refused', async () => {
  const { store, at } = await fixture();
  await assert.rejects(
    applyBatch(store, { requestId: randomUUID(), edits: [{ id: at(3).id, values: { Death_date: 3_000_000 } }] }, user),
    e => e.details.items[0].code === 'INVALID_DATE' && e.details.items[0].field === 'Death_date',
  );
  store.close();
});

test('conflicting fields and rows are taken out of a batch', () => {
  const edits = [
    { id: 'a', values: { CAM_ID: 'X1', Death_date: 1 } },
    { id: 'b', values: { Sex: 'male' } },
  ];
  const creates = [{ clientId: 'c', module: 'Insectary_data', values: {} }];
  assert.deepEqual(
    withoutConflicts({ edits, creates }, [
      { id: 'a', field: 'CAM_ID' },
      { id: 'b', field: null },
      { id: null, clientId: 'c' },
    ]),
    { edits: [{ id: 'a', values: { Death_date: 1 } }], creates: [] },
  );
  // A conflict of no single change (the sheet's columns changed) cannot be isolated.
  assert.equal(withoutConflicts({ edits, creates }, [{ id: null, clientId: null }]), null);
});

test('a starting CAM or tube already in use is reported with where it is, and the next free one', async () => {
  const { store } = await fixture();
  const cam = idSuggestions(store, { kind: 'cam', start: 'CAM078274', count: 2 });
  assert.deepEqual(cam.startUsed, { value: 'CAM078274', sheet: 'Insectary_data', row: 2, label: 'N2D' });
  assert.equal(cam.nextFree, 'CAM078275');
  const tube = idSuggestions(store, { kind: 'tube', start: 'FS00000020', count: 1 });
  assert.equal(tube.startUsed.sheet, 'Collection_data');
  assert.equal(idSuggestions(store, { kind: 'cam', start: 'CAM078275', count: 1 }).startUsed, undefined);
  store.close();
});
