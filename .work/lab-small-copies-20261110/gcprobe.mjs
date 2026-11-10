// Diagnostic preload (not shipped): every isolate runs a full GC every 10 s from 20 s on; main logs memory to stderr.
import v8 from 'node:v8';
import vm from 'node:vm';
import { isMainThread } from 'node:worker_threads';
v8.setFlagsFromString('--expose_gc');
const gc = vm.runInNewContext('gc');
const t0 = Date.now();
setTimeout(() => setInterval(() => gc(), 10000).unref(), 20000).unref();
if (isMainThread) {
  setInterval(() => {
    const m = process.memoryUsage();
    const mb = x => Math.round(x / 1048576);
    console.error(`MEM t=${((Date.now() - t0) / 1000).toFixed(0)} rss=${mb(m.rss)} heapUsed=${mb(m.heapUsed)} heapTotal=${mb(m.heapTotal)} external=${mb(m.external)}`);
  }, 5000).unref();
}
