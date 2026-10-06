// The Revisión checks as the app runs them (server/checks.mjs): the whole scan, a second or two on
// the team's workbook, in a worker thread (server/checks-worker.mjs), so the app answers people
// while it runs. The app keeps the last result and answers with it while the local copy is still
// as it was scanned (the same stamp as before); a request that needs the issues as of now
// (freshIssues: the Revisión tab, its verdicts, «Resueltos», the suggested edits) waits for a scan
// that starts after it, without holding up anyone else. Requests that arrive while a scan runs on
// an older copy share the next one. Pages that never wait (readyIssues: the proposals' marks) get
// the result when it is up to date, else nothing, and a scan is started.
// No worker (the scan in the app's thread, as before): an in-memory database (tests),
// ASSISTANT_WORKER=0, or a worker that failed three times in a minute (server/worker-slot.mjs).
import { allIssues, cachedIssues, issuesStamp, keepIssues, useChecksRunner } from './checks.mjs';
import { workerMode, workerSlot } from './worker-slot.mjs';

const SCAN_MS = 120_000;

/**
 * Hands the app's scans to the checks' worker where it can: { mode, status(), close() }, and
 * freshIssues / readyIssues (server/checks.mjs) for this store ask it. `workerUrl`, `scanMs`: for tests.
 */
export function createChecksHost({
  store,
  config = {},
  workerUrl = new URL('./checks-worker.mjs', import.meta.url),
  scanMs = SCAN_MS,
} = {}) {
  const decided = workerMode(store, config);
  if (decided.mode === 'inline')
    return { mode: 'inline', status: () => ({ mode: 'inline', why: decided.why }), close() {} };
  const worker = workerSlot({
    role: 'checks',
    url: workerUrl,
    data: () => ({
      path: decided.path,
      localMode: !!store.localMode,
      spreadsheetId: store.sheets?.spreadsheetId ?? null,
      config: structuredClone(config),
    }),
    timeoutMs: scanMs,
    name: "The checks'",
    log: 'Checks worker',
  });
  /** The scan running: { asked: the stamp when it was asked for, promise }; the one after it, shared. */
  let running = null;
  let queued = null;
  let scans = 0;
  let lastMs = null;

  function scan() {
    const asked = issuesStamp(store);
    const promise = worker.call({ type: 'issues' }).then(
      entry => {
        scans++;
        lastMs = entry.ms;
        return store.closed ? entry : keepIssues(store, entry);
      },
      e => {
        // A worker left aside or failing: the scan in the app's thread, as before. Too long or closing: said.
        if (store.closed || ['WORKER_TIMEOUT', 'WORKER_CLOSED'].includes(e.code)) throw e;
        if (e.code !== 'DEGRADED') console.error('Checks worker:', e.message, '(scanned in the app instead)');
        return allIssues(store);
      },
    );
    running = { asked, promise };
    const done = () => {
      if (running?.promise === promise) running = null;
    };
    promise.then(done, done);
    return promise;
  }

  const runner = {
    fresh() {
      const kept = cachedIssues(store);
      if (kept) return Promise.resolve(kept);
      // Asked for since the copy last changed: its answer is as of now.
      if (running && running.asked === issuesStamp(store)) return running.promise;
      if (!running) return scan();
      // A scan of an older copy runs: the next one starts when it ends, for everyone asking meanwhile.
      queued ??= running.promise
        .catch(() => {})
        .then(() => {
          queued = null;
          return scan();
        });
      return queued;
    },
    ready() {
      const kept = cachedIssues(store);
      if (kept) return kept;
      runner.fresh().catch(e => console.error('Checks:', e.message));
      return null;
    },
  };
  useChecksRunner(store, runner);
  return {
    mode: 'worker',
    status: () => ({ ...worker.status(), scans, lastMs, scanning: !!running }),
    close() {
      useChecksRunner(store, null);
      worker.close();
    },
  };
}
