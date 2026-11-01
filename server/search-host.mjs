// The Buscador's search (server/search.mjs searchAll) in a worker thread (server/search-worker.mjs): each search
// reads and checks every row that may hold the text, 0.1–0.5 s on the team's workbook, so in the app's thread it
// kept everyone else waiting meanwhile. The worker reads the app's database on its own connection (SQLite refuses
// any write there, server/store-reader.mjs), starts with the first search and stops after some idle minutes.
// No worker (the search in the app's thread, as before): an in-memory database (tests), ASSISTANT_WORKER=0, or a
// worker that failed three times in a minute (server/worker-slot.mjs).
import { searchAll } from './search.mjs';
import { layoutsFeed, workerMode, workerSlot } from './worker-slot.mjs';

const SEARCH_MS = 60_000;

/** { all(query) → the answer of searchAll, mode, status(), close() }. `workerUrl`: for tests. */
export function createSearchHost({ store, config = {}, workerUrl = new URL('./search-worker.mjs', import.meta.url) } = {}) {
  const decided = workerMode(store, config);
  if (decided.mode === 'inline')
    return { mode: 'inline', all: query => searchAll(store, query), status: () => ({ mode: 'inline', why: decided.why }), close() {} };
  // The columns missing from a live sheet are read from its layout: the worker gets them as the app has them.
  const layouts = layoutsFeed(store, changed => worker.post({ type: 'layouts', layouts: changed }));
  const worker = workerSlot({
    role: 'search',
    url: workerUrl,
    data: () => ({ path: decided.path, localMode: !!store.localMode, spreadsheetId: store.sheets?.spreadsheetId ?? null, layouts: layouts.current() }),
    timeoutMs: SEARCH_MS,
    ...(config.workerIdleMs !== undefined ? { idleMs: config.workerIdleMs } : {}),
    name: "The search's",
    log: 'Search worker',
  });
  return {
    mode: 'worker',
    async all(query) {
      try {
        return await worker.call({ type: 'search', query: { q: query?.q, context: query?.context, sheets: query?.sheets, pin: query?.pin } });
      } catch (e) {
        if (store.closed || e.code === 'WORKER_CLOSED') throw e;
        // A worker left aside, failing or too slow: the search in the app's thread, as before.
        if (e.code !== 'DEGRADED') console.error('Search worker:', e.message, '(searched in the app instead)');
        return searchAll(store, query);
      }
    },
    status: () => worker.status(),
    close() {
      layouts.stop();
      worker.close();
    },
  };
}
