// A worker thread the app hands long work to (the assistant's tool calls and proposal views,
// server/assistant-host.mjs; the Revisión checks, server/checks-host.mjs), so people's requests
// are answered meanwhile. Each starts with the first call that needs it; a call that takes longer
// than its time is answered "call again" and its worker is replaced; a worker that fails three
// times in a minute is left aside (`degraded`: the caller does the work in the app's thread).
import { Worker } from 'node:worker_threads';

/** Failures in a minute that leave a worker aside. */
const CRASHES = 3;
const CRASH_WINDOW_MS = 60_000;
const fail = (code, message) => Object.assign(new Error(message), { code });

/**
 * Whether work can run in workers here: { mode: 'worker', path } or { mode: 'inline', why }.
 * Not with an in-memory database (tests), ASSISTANT_WORKER=0, or a configuration that cannot be
 * handed to a thread.
 */
export function workerMode(store, config) {
  if (String(config.assistantWorker ?? 'auto') === '0') return { mode: 'inline', why: 'ASSISTANT_WORKER=0' };
  const path = store.db.location?.() ?? null;
  if (!path) return { mode: 'inline', why: 'in-memory database' };
  try {
    structuredClone(config);
  } catch {
    return { mode: 'inline', why: 'configuration not transferable' };
  }
  return { mode: 'worker', path };
}

/**
 * One worker thread of a role, started when first needed: call(message) → its answer. `data()`:
 * what it starts with (workerData); `onMessage`: what it says besides its answers. `name`: how
 * the errors call it, `log`: the log.
 */
export function workerSlot({
  role,
  url,
  data,
  timeoutMs,
  onMessage,
  onDegraded,
  name = `The assistant's ${role}`,
  log = `Assistant ${role} worker`,
}) {
  let worker = null;
  let next = 0;
  let restarts = 0;
  let failures = 0;
  let backoffUntil = 0;
  let degraded = false;
  let closed = false;
  const crashes = [];
  const pending = new Map();

  const rejectAll = error => {
    for (const [id, call] of pending) {
      clearTimeout(call.timer);
      pending.delete(id);
      call.reject(error);
    }
  };
  /** Stops the worker we chose to stop (a call too long, closing): not a failure. */
  function stop(error) {
    const w = worker;
    worker = null;
    rejectAll(error);
    if (w) {
      w.stopping = true;
      void w.terminate();
    }
  }
  function crashed(error) {
    restarts++;
    failures++;
    const at = Date.now();
    crashes.push(at);
    while (crashes.length && crashes[0] < at - CRASH_WINDOW_MS) crashes.shift();
    // Started again after 1, 2, 4… s (at most 30 s), with the next call.
    backoffUntil = at + Math.min(30_000, 1000 * 2 ** (failures - 1));
    rejectAll(fail('WORKER_FAILED', `${name} stopped unexpectedly and is starting again: call again.`));
    if (crashes.length >= CRASHES && !degraded) {
      degraded = true;
      console.error(
        `${log} failed ${crashes.length} times in a minute: its work runs in the app's thread until the next restart`,
        error?.message ?? '',
      );
      onDegraded?.();
    }
  }
  function spawn() {
    const w = new Worker(url, { workerData: { role, ...data() } });
    w.unref();
    w.on('message', m => {
      if (m?.type !== 'reply') return onMessage?.(m);
      const call = pending.get(m.id);
      if (!call) return;
      pending.delete(m.id);
      clearTimeout(call.timer);
      failures = 0;
      if (m.error) call.reject(fail(m.error.code ?? 'WORKER_ERROR', m.error.message));
      else call.resolve(m.out);
    });
    w.on('error', e => {
      if (!w.stopping) console.error(`${log}:`, e.stack || e.message);
      w.lastError = e;
    });
    w.on('exit', code => {
      if (worker === w) worker = null;
      if (w.stopping || closed) return;
      console.error(`${log} exited (${code})`);
      crashed(w.lastError);
    });
    worker = w;
  }
  return {
    get degraded() {
      return degraded;
    },
    get alive() {
      return !!worker;
    },
    inFlight: () => pending.size,
    status: () => ({ mode: degraded ? 'degraded' : 'worker', inFlight: pending.size, restarts, running: !!worker }),
    /** Tells the running worker something (the sheets' columns, how Google answers); a new one starts with data(). */
    post(message) {
      try {
        worker?.postMessage(message);
      } catch (e) {
        console.error(`${log}:`, e.message);
      }
    },
    async call(message) {
      if (closed) throw fail('WORKER_CLOSED', 'The app is stopping: call again in a minute.');
      if (degraded) throw fail('DEGRADED', `${name} worker is left aside`);
      const wait = backoffUntil - Date.now();
      if (wait > 0) await new Promise(resolve => setTimeout(resolve, wait).unref?.());
      if (!worker) spawn();
      const id = ++next;
      return new Promise((resolve, reject) => {
        const timer = setTimeout(() => {
          // Too long: its answer is "call again", and the worker (busy with it, maybe for ever) is replaced.
          restarts++;
          console.error(`${log}: a call took over ${Math.round(timeoutMs / 1000)} s; replaced`);
          const call = pending.get(id);
          pending.delete(id);
          call?.reject(
            fail(
              'WORKER_TIMEOUT',
              `The call took over ${Math.round(timeoutMs / 1000)} s and was stopped: call again, asking for less if it is a big one.`,
            ),
          );
          stop(fail('WORKER_RESTARTED', `${name} worker was restarted while this call ran: call again.`));
        }, timeoutMs);
        timer.unref?.();
        pending.set(id, { resolve, reject, timer });
        try {
          worker.postMessage({ ...message, id });
        } catch (e) {
          clearTimeout(timer);
          pending.delete(id);
          reject(e);
        }
      });
    },
    close() {
      closed = true;
      stop(fail('WORKER_CLOSED', 'The app is stopping: call again in a minute.'));
    },
  };
}
