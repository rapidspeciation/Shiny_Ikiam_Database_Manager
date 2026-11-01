// The Buscador's worker thread (server/search-host.mjs starts it): searchAll (server/search.mjs) on its own
// read-only connection to the app's database (server/store-reader.mjs), one search at a time, each in one read
// transaction, so a save landing meanwhile is in all of the answer or none of it.
import { parentPort, workerData } from 'node:worker_threads';
import { createStoreReader } from './store-reader.mjs';
import { searchAll } from './search.mjs';

const { path, localMode, spreadsheetId, layouts } = workerData;
const store = createStoreReader({ path, writable: [], localMode, spreadsheetId, layouts });

function answer(m) {
  if (m.type !== 'search') throw new Error(`The search's worker does not answer ${m.type}`);
  store.db.exec('BEGIN');
  try {
    return searchAll(store, m.query);
  } finally {
    store.db.exec('COMMIT');
  }
}

parentPort.on('message', m => {
  if (m?.type === 'layouts') return store.setLayouts(m.layouts);
  if (m?.id === undefined) return;
  try {
    parentPort.postMessage({ type: 'reply', id: m.id, out: answer(m) });
  } catch (e) {
    parentPort.postMessage({ type: 'reply', id: m.id, error: { message: e.message, code: e.code ?? null } });
  }
});
