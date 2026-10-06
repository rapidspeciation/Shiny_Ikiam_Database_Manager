// The assistant as the app runs it: what people use, and everything that reaches Google, in the
// app's own thread; the AI's work in worker threads (server/assistant-worker.mjs), so a long tool
// call or a big proposal's table never keeps the app from answering people.
// - The tools' worker answers T3 Code's tool calls (MCP tools/call), all but MAIN_TOOLS: those
//   write to Google or the app's state (apply, undo, the Wikiloc queue, the documents' sync), or
//   run apart already (`query`, a child process).
// - The views' worker builds the proposals as Cambios propuestos shows them (the list the page
//   asks for, and the answer to a person's edit in the table).
// The workers start with the first call that needs them. A call that takes longer than callMs
// is answered "call again" and its worker is replaced; a worker that fails three times in a
// minute is left aside (`degraded`: the work runs in the app's thread, as before), shown on /health.
// No worker (the work in the app's thread): an in-memory database (tests), ASSISTANT_WORKER=0, or
// a configuration that cannot be handed to a thread.
import { Worker } from 'node:worker_threads';
import { createAssistant } from './assistant.mjs';

/** Tools that run in the app's thread. */
export const MAIN_TOOLS = new Set([
  'apply_proposal',
  'undo_edits',
  'preview_undo',
  'queue_wikiloc',
  'sync_documents',
  'query',
]);
const CALL_MS = 180_000;
const VIEWS_MS = 60_000;
/** Failures in a minute that leave a worker aside. */
const CRASHES = 3;
const CRASH_WINDOW_MS = 60_000;
const fail = (code, message) => Object.assign(new Error(message), { code });

/** Whether the assistant's work can run in workers here: { mode: 'worker', path } or { mode: 'inline', why }. */
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
 * what it starts with (workerData); `onMessage`: what it says besides its answers.
 */
function workerSlot({ role, url, data, timeoutMs, onMessage, onDegraded }) {
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
    rejectAll(fail('WORKER_FAILED', `The assistant's ${role} stopped unexpectedly and is starting again: call again.`));
    if (crashes.length >= CRASHES && !degraded) {
      degraded = true;
      console.error(
        `Assistant ${role} worker failed ${crashes.length} times in a minute: its work runs in the app's thread until the next restart`,
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
      if (!w.stopping) console.error(`Assistant ${role} worker:`, e.stack || e.message);
      w.lastError = e;
    });
    w.on('exit', code => {
      if (worker === w) worker = null;
      if (w.stopping || closed) return;
      console.error(`Assistant ${role} worker exited (${code})`);
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
        console.error(`Assistant ${role} worker:`, e.message);
      }
    },
    async call(message) {
      if (closed) throw fail('WORKER_CLOSED', 'The app is stopping: call again in a minute.');
      if (degraded) throw fail('DEGRADED', `The assistant's ${role} worker is left aside`);
      const wait = backoffUntil - Date.now();
      if (wait > 0) await new Promise(resolve => setTimeout(resolve, wait).unref?.());
      if (!worker) spawn();
      const id = ++next;
      return new Promise((resolve, reject) => {
        const timer = setTimeout(() => {
          // Too long: its answer is "call again", and the worker (busy with it, maybe for ever) is replaced.
          restarts++;
          console.error(`Assistant ${role} worker: a call took over ${Math.round(timeoutMs / 1000)} s; replaced`);
          const call = pending.get(id);
          pending.delete(id);
          call?.reject(
            fail(
              'WORKER_TIMEOUT',
              `The call took over ${Math.round(timeoutMs / 1000)} s and was stopped: call again, asking for less if it is a big one.`,
            ),
          );
          stop(fail('WORKER_RESTARTED', "The assistant's tools were restarted while this call ran: call again."));
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

/**
 * The assistant (server/assistant.mjs) with its tool calls and proposal views in workers where it
 * can: the same answers as createAssistant, plus status() for /health and inFlight() (tool calls
 * running in the worker, which a restart waits for). `workerUrl`, `callMs`, `viewsMs`: for tests.
 */
export function createAssistantHost({
  store,
  config = {},
  workerUrl = new URL('./assistant-worker.mjs', import.meta.url),
  callMs,
  viewsMs,
} = {}) {
  const main = createAssistant({ store, config });
  const decided = workerMode(store, config);
  if (decided.mode === 'inline') {
    return {
      ...main,
      mode: 'inline',
      inFlight: () => 0,
      status: () => ({ mode: 'inline', why: decided.why, inFlight: 0, restarts: 0 }),
    };
  }
  const cloned = structuredClone(config);
  // Each sheet's columns as the workers last got them (a key of its layout), so only changes are sent.
  const sentLayouts = new Map();
  const layoutKey = layout => JSON.stringify({ ...layout, columns: [...(layout?.columns ?? [])] });
  const data = () => {
    for (const [sheet, layout] of store.layouts ?? []) sentLayouts.set(sheet, layoutKey(layout));
    return {
      path: decided.path,
      localMode: !!store.localMode,
      spreadsheetId: store.sheets?.spreadsheetId ?? null,
      layouts: [...(store.layouts ?? [])],
      google: store.googleState?.() ?? null,
      config: cloned,
    };
  };
  const onMessage = m => {
    if (m?.type === 'changed') main.notifyChanged(m.ownerId, m.page ?? null);
  };
  const tools = workerSlot({
    role: 'tools',
    url: workerUrl,
    data,
    timeoutMs: callMs ?? config.assistantCallMs ?? CALL_MS,
    onMessage,
  });
  const views = workerSlot({
    role: 'views',
    url: workerUrl,
    data,
    timeoutMs: viewsMs ?? VIEWS_MS,
    onMessage,
    onDegraded: () => main.useViews(null),
  });
  main.useViews({
    list: (rows, titles, have) => views.call({ type: 'views', rows, titles: [...titles], have: [...have] }),
  });

  // What the workers read of the app's memory: the sheets' columns after each sync, how Google answers.
  const stopSyncs = store.watchSyncs?.(() => {
    const changed = [];
    for (const [sheet, layout] of store.layouts ?? []) {
      const key = layoutKey(layout);
      if (sentLayouts.get(sheet) === key) continue;
      sentLayouts.set(sheet, key);
      changed.push([sheet, layout]);
    }
    if (!changed.length) return;
    tools.post({ type: 'layouts', layouts: changed });
    views.post({ type: 'layouts', layouts: changed });
  });
  let googleQueued = false;
  const stopLive = store.watchLive?.(() => {
    if (googleQueued || (!tools.alive && !views.alive)) return;
    googleQueued = true;
    setImmediate(() => {
      googleQueued = false;
      if (store.closed) return;
      const state = store.googleState();
      tools.post({ type: 'google', state });
      views.post({ type: 'google', state });
    });
  });

  const rpcError = (id, message) => ({ status: 200, body: { jsonrpc: '2.0', id, error: { code: -32000, message } } });
  return {
    ...main,
    mode: 'worker',
    /** Tool calls running in the worker (a restart waits for them, server/index.mjs drain). */
    inFlight: () => tools.inFlight(),
    status: () => ({ ...tools.status(), views: views.status() }),
    async mcp(headers, body) {
      const name = body?.method === 'tools/call' ? String(body.params?.name ?? '') : null;
      if (name === null || MAIN_TOOLS.has(name) || tools.degraded) return main.mcp(headers, body);
      const id = body?.id ?? null;
      // Stopping (a deploy): no new calls; those running finish first.
      if (store.draining) return rpcError(id, 'The app is restarting: call again in a minute.');
      try {
        return await tools.call({ type: 'mcp', headers: { authorization: headers?.authorization }, body });
      } catch (e) {
        if (e.code === 'DEGRADED') return main.mcp(headers, body);
        return rpcError(id, e.message);
      }
    },
    close() {
      stopSyncs?.();
      stopLive?.();
      tools.close();
      views.close();
      main.close();
    },
  };
}
