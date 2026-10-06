// A checks worker that dies on its first call (tests/checks-worker.test.mjs).
import { parentPort } from 'node:worker_threads';

parentPort.on('message', () => {
  throw new Error('crashed on purpose');
});
