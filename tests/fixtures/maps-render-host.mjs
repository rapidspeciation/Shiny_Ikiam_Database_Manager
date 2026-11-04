// One render of server/maps-render.mjs as a tools thread runs it (a worker_threads Worker, options in workerData) or as
// a server process would (options as JSON in argv[2]), for tests/maps-render.test.mjs: { module, deadlineMs, url }.
// The answer goes to the parent thread, or to stdout as one JSON line.
import { isMainThread, parentPort, workerData } from 'node:worker_threads';
import { createMapRenderer } from '../../server/maps-render.mjs';

const o = isMainThread ? JSON.parse(process.argv[2]) : workerData;
const r = createMapRenderer({ env: { PLAYWRIGHT_MODULE: o.module, PATH: process.env.PATH }, memAvailable: () => 8 * 1024 ** 3, deadlineMs: o.deadlineMs, log: () => {} });
const out = await r.render(o.url);
const answer = { ok: out.ok, reason: out.reason ?? null };
if (parentPort) parentPort.postMessage(answer);
else process.stdout.write(`${JSON.stringify(answer)}\n`);
