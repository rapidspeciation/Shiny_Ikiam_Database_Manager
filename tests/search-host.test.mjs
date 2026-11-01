import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { searchAll } from '../server/search.mjs';
import { createSearchHost } from '../server/search-host.mjs';

// The Buscador's search in its worker thread (server/search-host.mjs), on a database file: the same answer as
// the search in the app's thread, a save seen by the next search, and the app's thread when there is no worker.

const seed = () => ({
  Insectary_data: Array.from({ length: 30 }, (_, i) => ({
    row: i + 2,
    values: { Insectary_ID: `A${i}B`, SPECIES: i % 2 ? 'Melinaea menophilus' : 'Mechanitis polymnia', CAM_ID: `CAM0800${String(i).padStart(2, '0')}` },
  })),
  Collection_data: [
    { row: 2, values: { CAM_ID: 'CAM080010', Insectary_ID: 'A9B', SPECIES: 'Oleria onega', Collection_time: 0.3958333333 } },
    { row: 3, values: { CAM_ID: 'CAM080099', SPECIES: 'Hypothyris anastasia', Notes_Collection_data: 'Wing clip of A5B lost' } },
  ],
});
const took = answer => ({ ...answer, took: 0 });

async function fixture(t, config = {}) {
  const dir = mkdtempSync(join(tmpdir(), 'search-worker-'));
  const databasePath = join(dir, 'app.sqlite');
  const store = new Store({ databasePath, localMode: true }, { sheets: new LocalSheets(seed()) });
  await store.sync({ sheets: Object.keys(seed()) });
  const host = createSearchHost({ store, config: { databasePath, localMode: true, ...config } });
  t.after(() => {
    host.close();
    store.close();
    rmSync(dir, { recursive: true, force: true });
  });
  return { store, host };
}

test('the search in its worker: the same answer as in the app, and a save seen by the next search', async t => {
  const { store, host } = await fixture(t);
  assert.equal(host.mode, 'worker');
  for (const q of ['mechanitis', 'A5B', 'CAM0800', '09:30', 'onega', 'x'])
    for (const options of [{}, { context: 2, sheets: 1 }, { pin: 'Collection_data' }])
      assert.deepEqual(took(await host.all({ q, ...options })), took(searchAll(store, { q, ...options })), `${q} ${JSON.stringify(options)}`);
  assert.equal(host.status().running, true);
  const record = store.getRecordBySheetRow('Collection_data', 3);
  store.db
    .prepare('UPDATE records SET values_json=?, version=version+1 WHERE id=?')
    .run(JSON.stringify({ ...record.values, SPECIES: 'Ithomia salapia' }), record.id);
  const found = await host.all({ q: 'salapia' });
  assert.deepEqual(found.sheets.map(s => [s.module, s.total]), [['Collection_data', 1]]);
});

test('the search in the app when there is no worker (ASSISTANT_WORKER=0)', async t => {
  const { store, host } = await fixture(t, { assistantWorker: '0' });
  assert.equal(host.mode, 'inline');
  assert.deepEqual(took(await host.all({ q: 'onega' })), took(searchAll(store, { q: 'onega' })));
});

test('a search after the worker was closed (the app closing, its store still open): answered in the app', async t => {
  const { store, host } = await fixture(t);
  await host.all({ q: 'onega' });
  host.close();
  assert.deepEqual(took(await host.all({ q: 'onega' })), took(searchAll(store, { q: 'onega' })));
});
