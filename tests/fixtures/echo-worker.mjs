// A worker for tests of server/worker-slot.mjs: answers each call with its message's `n` and its own thread id.
import { parentPort, threadId } from 'node:worker_threads';

parentPort.on('message', m => {
  if (m?.id) parentPort.postMessage({ type: 'reply', id: m.id, out: { n: m.n, thread: threadId } });
});
