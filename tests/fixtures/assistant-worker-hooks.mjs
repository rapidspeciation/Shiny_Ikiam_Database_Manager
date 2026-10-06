// The assistant's worker with three tools for tests (tests/assistant-worker.test.mjs): __hang
// never answers (the thread stays busy), __crash kills the thread, __write tries to change a
// sheet row on the worker's connection and says what SQLite answered.
import { parentPort } from 'node:worker_threads';
import { store } from '../../server/assistant-worker.mjs';

parentPort.on('message', m => {
  const name = m?.type === 'mcp' ? m.body?.params?.name : null;
  if (name === '__hang') for (;;);
  if (name === '__crash') throw new Error('crashed on purpose');
  if (name === '__write') {
    let body;
    try {
      store.db.prepare("UPDATE records SET label = 'changed by the worker'").run();
      body = { wrote: true };
    } catch (e) {
      body = { refused: e.message };
    }
    parentPort.postMessage({ type: 'reply', id: m.id, out: { status: 200, body } });
  }
});
