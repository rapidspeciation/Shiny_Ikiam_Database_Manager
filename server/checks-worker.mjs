// The Revisión worker thread (server/checks-host.mjs starts it): the whole scan of the local copy
// (server/checks.mjs), the suggested edits (server/suggestions/), the alerts (server/alerts.mjs),
// Inicio's summaries (server/summary.mjs) and Monitoreo's Wikiloc corrections
// (server/monitoring-export.mjs), up to a second or two each on the team's workbook, run here, so
// the app answers people meanwhile; what they read in common is read once. It reads the app's
// database on its own connection, where SQLite refuses any write (server/store-reader.mjs); the
// app keeps «Resueltos» with what it sends back.
import { parentPort, workerData } from 'node:worker_threads';
import { createStoreReader } from './store-reader.mjs';
import { allIssues } from './checks.mjs';
import { alertsEntry } from './alerts.mjs';
import { allSuggestions } from './suggestions/index.mjs';
import { summaryEntry } from './summary.mjs';
import { correctionsEntry } from './monitoring-export.mjs';

const { path, localMode, spreadsheetId, layouts, config } = workerData;
const store = createStoreReader({ path, writable: [], localMode, spreadsheetId, layouts, config });

const work = {
  issues() {
    const { stamp, issues, checkedAt, ms } = allIssues(store);
    return { stamp, issues, checkedAt, ms };
  },
  async suggestions() {
    const { stamp, items, computedAt, ms, timings } = await allSuggestions(store);
    return { stamp, items, computedAt, ms, timings };
  },
  alerts: () => alertsEntry(store),
  summary() {
    const started = Date.now();
    return { ...summaryEntry(store), workMs: Date.now() - started };
  },
  corrections: () => correctionsEntry(store),
};

/** The work on the copy as it is now: one read transaction, so a save landing meanwhile is in all of it or none. */
async function answer(m) {
  if (!Object.hasOwn(work, m.type)) throw new Error(`The checks' worker does not answer ${m.type}`);
  store.db.exec('BEGIN');
  try {
    return await work[m.type]();
  } finally {
    store.db.exec('COMMIT');
  }
}

// One at a time: each in its own transaction on the one connection.
let queue = Promise.resolve();
parentPort.on('message', m => {
  if (m?.type === 'layouts') return store.setLayouts(m.layouts);
  if (m?.id === undefined) return;
  queue = queue.then(async () => {
    try {
      parentPort.postMessage({ type: 'reply', id: m.id, out: await answer(m) });
    } catch (e) {
      parentPort.postMessage({ type: 'reply', id: m.id, error: { message: e.message, code: e.code ?? null } });
    }
  });
});
