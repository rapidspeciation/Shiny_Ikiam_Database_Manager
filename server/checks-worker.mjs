// The Revisión checks' worker thread (server/checks-host.mjs starts it): the whole scan of the
// local copy (server/checks.mjs, a second or two on the team's workbook) runs here, so the app
// answers people meanwhile. It reads the app's database on its own connection, where SQLite
// refuses any write (server/store-reader.mjs); the app keeps «Resueltos» with what it sends back.
import { parentPort, workerData } from 'node:worker_threads';
import { createStoreReader } from './store-reader.mjs';
import { allIssues } from './checks.mjs';

const { path, localMode, spreadsheetId, config } = workerData;
const store = createStoreReader({ path, writable: [], localMode, spreadsheetId, config });

/** The scan of the copy as it is now: one read transaction, so a save landing meanwhile is in all of it or none. */
function issues() {
  store.db.exec('BEGIN');
  try {
    const { stamp, issues, checkedAt, ms } = allIssues(store);
    return { stamp, issues, checkedAt, ms };
  } finally {
    store.db.exec('COMMIT');
  }
}

parentPort.on('message', m => {
  if (m?.id === undefined) return;
  try {
    if (m.type !== 'issues') throw new Error(`The checks' worker does not answer ${m.type}`);
    parentPort.postMessage({ type: 'reply', id: m.id, out: issues() });
  } catch (e) {
    parentPort.postMessage({ type: 'reply', id: m.id, error: { message: e.message, code: e.code ?? null } });
  }
});
