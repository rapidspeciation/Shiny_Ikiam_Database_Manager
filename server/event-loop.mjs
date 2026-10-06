// How long the server keeps requests waiting: one process answers everyone, so
// a long piece of work (a big proposal's table, a scan) holds up every other
// request meanwhile. A timer asks every SAMPLE_MS and notes how late it runs;
// /health reports the last minute of it (eventLoop: { p99Ms, maxMs }).
// (perf_hooks' monitorEventLoopDelay misses a long block when other timers are
// waiting too, as they always are in the server.)
import { performance } from 'node:perf_hooks';

const SAMPLE_MS = 100;
const WINDOW_MS = 60_000;

/**
 * Starts watching the event loop: { stats() → { p99Ms, maxMs, windowS }, stop() }.
 * A block of 2 s also stands for the asks that fell inside it (each as late as it
 * would have been), so p99 reads as the wait of a request arriving at any moment.
 */
export function watchEventLoop({ sampleMs = SAMPLE_MS, windowMs = WINDOW_MS } = {}) {
  /** [when, how late] in ms, oldest first. */
  const samples = [];
  let due = performance.now() + sampleMs;
  const timer = setInterval(() => {
    const now = performance.now();
    const late = Math.max(0, now - due);
    for (let missed = late; ; missed -= sampleMs) {
      samples.push([now, missed]);
      if (missed < sampleMs || samples.length > (2 * windowMs) / sampleMs) break;
    }
    due = now + sampleMs;
    let old = 0;
    while (old < samples.length && samples[old][0] < now - windowMs) old++;
    if (old) samples.splice(0, old);
  }, sampleMs);
  timer.unref();
  return {
    stats() {
      const since = performance.now() - windowMs;
      const lags = samples
        .filter(([at]) => at >= since)
        .map(([, l]) => l)
        .sort((a, b) => a - b);
      const round = n => Math.round(n * 10) / 10;
      return {
        p99Ms: lags.length ? round(lags[Math.min(lags.length - 1, Math.floor(lags.length * 0.99))]) : 0,
        maxMs: lags.length ? round(lags[lags.length - 1]) : 0,
        windowS: windowMs / 1000,
      };
    },
    stop: () => clearInterval(timer),
  };
}

/**
 * For a long loop on the server: `if (turn.due()) await turn()` gives the other
 * requests a turn once the loop has worked `ms` since the last one.
 */
export function turns(ms = 20) {
  let since = performance.now();
  const turn = async () => {
    await new Promise(resolve => setImmediate(resolve));
    since = performance.now();
  };
  turn.due = () => performance.now() - since >= ms;
  return turn;
}
