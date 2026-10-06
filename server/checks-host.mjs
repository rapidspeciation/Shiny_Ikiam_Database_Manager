// What Revisión, Inicio and Monitoreo compute over the whole local copy, as the app runs it: the
// checks (server/checks.mjs), the suggested edits (server/suggestions/), the alerts
// (server/alerts.mjs), Inicio's summaries (server/summary.mjs) and the Wikiloc corrections
// (server/monitoring-export.mjs), each up to a second or two on the team's workbook, in one worker
// thread (server/checks-worker.mjs), so the app answers people while they run; what they read in
// common the worker reads once. For each, the app keeps the last result and answers with it while
// the local copy is still as it was computed from (the same stamp as before); a request that needs
// it as of now (freshIssues, allSuggestions, freshAlerts, freshSummary, freshCorrections) waits for
// a run that starts after it, without holding up anyone else. Requests that arrive while a run
// works on an older copy share the next one. Pages that never wait (readyIssues: the proposals'
// marks) get the issues when they are up to date, else nothing, and a scan is started. What the
// results write (first seen and solved, «Resueltos») the app writes from them.
// No worker (all in the app's thread, as before): an in-memory database (tests),
// ASSISTANT_WORKER=0, or a worker that failed three times in a minute (server/worker-slot.mjs).
import { allIssues, cachedIssues, issuesStamp, keepIssues, useChecksRunner } from './checks.mjs';
import { alertsEntry, alertsStamp, cachedAlerts, keepAlerts, useAlertsRunner } from './alerts.mjs';
import {
  cachedSuggestions,
  keepSuggestions,
  suggestionsHere,
  suggestionsStamp,
  useSuggestionsRunner,
} from './suggestions/index.mjs';
import { cachedSummary, keepSummary, summaryEntry, summaryStamp, useSummaryRunner } from './summary.mjs';
import {
  cachedCorrections,
  correctionsEntry,
  correctionsStamp,
  keepCorrections,
  useCorrectionsRunner,
} from './monitoring-export.mjs';
import { layoutsFeed, workerMode, workerSlot } from './worker-slot.mjs';

const SCAN_MS = 120_000;

/**
 * Hands the app's checks, suggested edits, alerts, summaries and Wikiloc corrections to the
 * Revisión worker where it can: { mode, status(), close() }; freshIssues / readyIssues
 * (server/checks.mjs), allSuggestions (server/suggestions/), freshAlerts (server/alerts.mjs),
 * freshSummary (server/summary.mjs) and freshCorrections (server/monitoring-export.mjs) for this
 * store ask it.
 * `workerUrl`, `scanMs`: for tests.
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
  // The suggested edits read each sheet's columns (formulas): the worker gets them as the app has them.
  const layouts = layoutsFeed(store, changed => worker.post({ type: 'layouts', layouts: changed }));
  const worker = workerSlot({
    role: 'checks',
    url: workerUrl,
    data: () => ({
      path: decided.path,
      localMode: !!store.localMode,
      spreadsheetId: store.sheets?.spreadsheetId ?? null,
      layouts: layouts.current(),
      config: structuredClone(config),
    }),
    timeoutMs: scanMs,
    name: "The checks'",
    log: 'Checks worker',
  });

  /**
   * One kind of work the worker does for the app's requests: { fresh() → promise, ready() → result
   * or null, status() }. `stamp()`: the state it depends on; `cached()`: the kept result while that
   * state is the same; `keep(out)`: what the app does with the worker's answer (writes «Resueltos»,
   * keeps it); `here()`: the same work in the app's thread, when the worker cannot.
   */
  function runner({ type, stamp, cached, keep, here }) {
    /** The run working: { asked: the stamp when it was asked for, promise }; the one after it, shared. */
    let running = null;
    let queued = null;
    let runs = 0;
    let lastMs = null;

    function run() {
      const asked = stamp();
      const promise = worker.call({ type }).then(
        out => {
          runs++;
          lastMs = out.ms ?? out.value?.ms ?? out.workMs ?? null;
          return store.closed ? out : keep(out);
        },
        e => {
          // A worker left aside or failing: the work in the app's thread, as before. Too long or closing: said.
          if (store.closed || ['WORKER_TIMEOUT', 'WORKER_CLOSED'].includes(e.code)) throw e;
          if (e.code !== 'DEGRADED') console.error(`Checks worker (${type}):`, e.message, '(done in the app instead)');
          return here();
        },
      );
      running = { asked, promise };
      const done = () => {
        if (running?.promise === promise) running = null;
      };
      promise.then(done, done);
      return promise;
    }

    const self = {
      fresh() {
        const kept = cached();
        if (kept) return Promise.resolve(kept);
        // Asked for since the copy last changed: its answer is as of now.
        if (running && running.asked === stamp()) return running.promise;
        if (!running) return run();
        // A run on an older copy works: the next one starts when it ends, for everyone asking meanwhile.
        queued ??= running.promise
          .catch(() => {})
          .then(() => {
            queued = null;
            return run();
          });
        return queued;
      },
      ready() {
        const kept = cached();
        if (kept) return kept;
        self.fresh().catch(e => console.error(`Checks worker (${type}):`, e.message));
        return null;
      },
      status: () => ({ runs, lastMs, running: !!running }),
    };
    return self;
  }

  const issues = runner({
    type: 'issues',
    stamp: () => issuesStamp(store),
    cached: () => cachedIssues(store),
    keep: out => keepIssues(store, out),
    here: () => allIssues(store),
  });
  const suggestions = runner({
    type: 'suggestions',
    stamp: () => suggestionsStamp(store),
    cached: () => cachedSuggestions(store),
    keep: out => keepSuggestions(store, out),
    here: () => suggestionsHere(store),
  });
  const alerts = runner({
    type: 'alerts',
    stamp: () => alertsStamp(store),
    cached: () => cachedAlerts(store),
    keep: out => keepAlerts(store, out),
    here: () => alertsEntry(store),
  });
  const summary = runner({
    type: 'summary',
    stamp: () => summaryStamp(store),
    cached: () => cachedSummary(store),
    keep: out => keepSummary(store, out),
    here: () => summaryEntry(store),
  });
  const corrections = runner({
    type: 'corrections',
    stamp: () => correctionsStamp(store),
    cached: () => cachedCorrections(store),
    keep: out => keepCorrections(store, out),
    here: () => correctionsEntry(store),
  });
  useChecksRunner(store, issues);
  useSuggestionsRunner(store, suggestions);
  useAlertsRunner(store, alerts);
  useSummaryRunner(store, summary);
  useCorrectionsRunner(store, corrections);
  return {
    mode: 'worker',
    status: () => {
      const scanned = issues.status();
      return {
        ...worker.status(),
        scans: scanned.runs,
        lastMs: scanned.lastMs,
        scanning: scanned.running,
        suggestions: suggestions.status(),
        alerts: alerts.status(),
        summary: summary.status(),
        corrections: corrections.status(),
      };
    },
    close() {
      useChecksRunner(store, null);
      useSuggestionsRunner(store, null);
      useAlertsRunner(store, null);
      useSummaryRunner(store, null);
      useCorrectionsRunner(store, null);
      layouts.stop();
      worker.close();
    },
  };
}
