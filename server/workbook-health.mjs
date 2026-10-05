// Whether the team's workbook answers. After an edit to Insectary_data the
// workbook recalculates for minutes (XLOOKUPs over whole columns, COUNTIFs);
// meanwhile Google answers every request to it slowly, or with 503 UNAVAILABLE,
// or not at all, while other spreadsheets answer normally (5 Oct 2026).
//
// Every request to Google reports how it went (record): its time, its HTTP
// status, a timeout. From those the workbook is
//   ok    answering normally;
//   slow  answering, but a request took longer than SLOW_MS;
//   busy  a request got a 5xx (503 UNAVAILABLE) or no answer in time.
// While it is not ok a cheap request (one cell) is sent every PROBE_MS until
// one answers within SLOW_MS: then it is ok again. Saves are kept in the app
// meanwhile (server/outbox.mjs) and written when it answers.

/** A request slower than this makes the workbook "slow". */
export const SLOW_MS = 20_000;
/** How often a workbook that is not ok is asked again. */
export const PROBE_MS = 60_000;

/** An error that says the workbook does not answer now (nothing was written by a request that got it). */
export const busyError = e => !!e && (e.busy === true || e.timeout === true || [500, 502, 503, 504].includes(Number(e.status)));

export class WorkbookHealth {
  /**
   * `probe`: an async function sending one cheap request (it reports itself through record).
   * `slowMs`, `probeMs`, `now`: for tests.
   */
  constructor({ probe = null, slowMs = SLOW_MS, probeMs = PROBE_MS, now = Date.now } = {}) {
    this.probe = probe;
    this.slowMs = slowMs;
    this.probeMs = probeMs;
    this.now = now;
    this.state = 'ok';
    this.since = new Date(now()).toISOString();
    this.lastError = null;
    this.lastMs = null;
    this.checkedAt = null;
    this.nextProbeAt = null;
    this.timer = null;
    this.listeners = new Set();
    this.stopped = false;
  }
  /** One request's outcome: `ms` it took, `status` (HTTP), `timeout` (no answer), `background` (a whole-sheet read: its time does not count). */
  record({ ms = 0, status = 200, timeout = false, background = false, error = null } = {}) {
    this.checkedAt = new Date(this.now()).toISOString();
    if (timeout || status >= 500) {
      this.lastError = timeout ? `No answer in ${Math.round(ms / 1000)} s` : `Google Sheets ${status}${error ? `: ${String(error).slice(0, 120)}` : ''}`;
      return this.set('busy');
    }
    // A refusal (400, 403, 429…) says nothing about the recalculation.
    if (status >= 400) return;
    this.lastMs = ms;
    if (background && ms > this.slowMs) return;
    if (ms > this.slowMs) return this.set('slow');
    this.lastError = null;
    this.set('ok');
  }
  set(state) {
    if (state === this.state) {
      if (state !== 'ok') this.schedule();
      return;
    }
    const before = this.state;
    this.state = state;
    this.since = new Date(this.now()).toISOString();
    if (state === 'ok') this.cancel();
    else this.schedule();
    for (const fn of this.listeners) {
      try {
        fn(state, before);
      } catch (e) {
        console.error('Workbook state watcher:', e.message);
      }
    }
  }
  /** Calls `fn(state, before)` when the state changes. Returns the call that stops it. */
  onChange(fn) {
    this.listeners.add(fn);
    return () => this.listeners.delete(fn);
  }
  get ok() {
    return this.state === 'ok';
  }
  /** What /health, the app's banner and the assistant's tools say. */
  snapshot() {
    return {
      state: this.state,
      since: this.since,
      ...(this.lastError ? { lastError: this.lastError } : {}),
      ...(this.lastMs !== null ? { lastMs: this.lastMs } : {}),
      checkedAt: this.checkedAt,
      ...(this.state !== 'ok' && this.nextProbeAt ? { nextProbeAt: this.nextProbeAt } : {}),
    };
  }
  schedule() {
    if (this.timer || this.stopped || !this.probe) return;
    this.nextProbeAt = new Date(this.now() + this.probeMs).toISOString();
    this.timer = setTimeout(() => {
      this.timer = null;
      this.runProbe();
    }, this.probeMs);
    this.timer.unref?.();
  }
  cancel() {
    if (this.timer) clearTimeout(this.timer);
    this.timer = null;
    this.nextProbeAt = null;
  }
  /** Asks the workbook once now (the probe reports through record); asks again later while it is not ok. */
  async runProbe() {
    if (!this.probe || this.stopped) return;
    try {
      await this.probe();
    } catch {
      /* Reported through record by the request itself. */
    }
    if (this.state !== 'ok') this.schedule();
  }
  stop() {
    this.stopped = true;
    this.cancel();
  }
}
