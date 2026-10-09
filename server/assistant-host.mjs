// The assistant as the app runs it: what people use, and everything that reaches Google, in the
// app's own thread; the AI's work in worker threads (server/assistant-worker.mjs), so a long tool
// call or a big proposal's table never keeps the app from answering people.
// - The tools' worker answers T3 Code's tool calls (MCP tools/call), all but MAIN_TOOLS: those
//   write to Google or the app's state (apply, undo, the Wikiloc queue, the documents' sync, invitations), or
//   run apart already (`query`, a child process).
// - The views' worker builds the proposals as Cambios propuestos shows them (the list the page
//   asks for, and the answer to a person's edit in the table).
// The workers start with the first call that needs them. A call that takes longer than callMs
// is answered "call again" and its worker is replaced; a worker that fails three times in a
// minute is left aside (`degraded`: the work runs in the app's thread, as before), shown on /health
// (server/worker-slot.mjs).
// No worker (the work in the app's thread): an in-memory database (tests), ASSISTANT_WORKER=0, or
// a configuration that cannot be handed to a thread.
import { createAssistant, currentTool } from './assistant.mjs';
import { layoutsFeed, workerMode, workerSlot } from './worker-slot.mjs';

/** Tools that run in the app's thread. */
export const MAIN_TOOLS = new Set([
  'apply_proposal',
  'undo_edits',
  'preview_undo',
  'queue_walk',
  'sync_documents',
  'query',
  // They send email and write the invitations (server/invite-tool.mjs).
  'invite_person',
  'list_invitations',
]);
const CALL_MS = 180_000;
const VIEWS_MS = 60_000;

/**
 * The assistant (server/assistant.mjs) with its tool calls and proposal views in workers where it
 * can: the same answers as createAssistant, plus status() for /health and inFlight() (tool calls
 * running in the worker, which a restart waits for). `workerUrl`, `callMs`, `viewsMs`, `backoffMs`: for tests.
 */
export function createAssistantHost({
  store,
  config = {},
  invitations = null,
  workerUrl = new URL('./assistant-worker.mjs', import.meta.url),
  callMs,
  viewsMs,
  backoffMs,
} = {}) {
  const main = createAssistant({ store, config, invitations });
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
  // What the workers read of the app's memory: the sheets' columns after each sync (only changes are sent).
  const layouts = layoutsFeed(store, changed => {
    tools.post({ type: 'layouts', layouts: changed });
    views.post({ type: 'layouts', layouts: changed });
  });
  const data = () => ({
    path: decided.path,
    localMode: !!store.localMode,
    spreadsheetId: store.sheets?.spreadsheetId ?? null,
    layouts: layouts.current(),
    google: store.googleState?.() ?? null,
    config: cloned,
  });
  const onMessage = m => {
    if (m?.type === 'changed') main.notifyChanged(m.ownerId, m.page ?? null);
  };
  const tools = workerSlot({
    role: 'tools',
    url: workerUrl,
    data,
    timeoutMs: callMs ?? config.assistantCallMs ?? CALL_MS,
    backoffMs,
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

  // And how Google answers.
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
      // By its name now: an earlier name (TOOL_ALIASES) runs where its tool runs.
      const name = body?.method === 'tools/call' ? currentTool(String(body.params?.name ?? '')).name : null;
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
      layouts.stop();
      stopLive?.();
      tools.close();
      views.close();
      main.close();
    },
  };
}
