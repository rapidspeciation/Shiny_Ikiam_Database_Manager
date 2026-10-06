// A worker thread of the assistant (server/assistant-host.mjs starts it): the AI's work runs here,
// so a long tool call never keeps the app from answering people. It reads the app's database on its
// own connection (server/store-reader.mjs) and writes only what its role may:
// - 'tools': the assistant's MCP tool calls (all but those the app keeps, MAIN_TOOLS); it writes
//   its proposals and their conversations, and tells the app whose proposals changed.
// - 'views': the proposals as Cambios propuestos shows them (read-only).
// Nothing here reaches Google: applying a proposal, the outbox and the syncs stay with the app.
import { parentPort, workerData } from 'node:worker_threads';
import { PROPOSAL_TABLES, createStoreReader } from './store-reader.mjs';
import { createAssistant } from './assistant.mjs';

const { role, path, localMode, spreadsheetId, layouts, google, config } = workerData;
export const store = createStoreReader({
  path,
  writable: role === 'tools' ? PROPOSAL_TABLES : [],
  localMode,
  spreadsheetId,
  layouts,
  google,
  config,
});
const assistant = createAssistant({
  store,
  config,
  role,
  onChanged: (ownerId, page) => parentPort.postMessage({ type: 'changed', ownerId, page }),
});

async function answer(m) {
  if (m.type === 'mcp' && role === 'tools') return assistant.mcp(m.headers ?? {}, m.body);
  if (m.type === 'views' && role === 'views')
    return assistant.listedViews(m.rows, new Map(m.titles ?? []), new Set(m.have ?? []));
  throw new Error(`The ${role} worker does not answer ${m.type}`);
}

parentPort.on('message', m => {
  if (m?.type === 'layouts') return store.setLayouts(m.layouts);
  if (m?.type === 'google') return store.setGoogle(m.state);
  if (m?.id === undefined) return;
  answer(m).then(
    out => {
      try {
        parentPort.postMessage({ type: 'reply', id: m.id, out });
      } catch (e) {
        parentPort.postMessage({ type: 'reply', id: m.id, error: { message: e.message, code: 'NOT_SENT' } });
      }
    },
    e => parentPort.postMessage({ type: 'reply', id: m.id, error: { message: e.message, code: e.code ?? null } }),
  );
});
