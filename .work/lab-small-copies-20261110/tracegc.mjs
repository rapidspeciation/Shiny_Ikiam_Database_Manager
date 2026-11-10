// Diagnostic preload (not shipped): GC trace of every isolate of the app, and the main thread's memory every 5 s.
import v8 from 'node:v8';
import { isMainThread } from 'node:worker_threads';
if ((process.argv[1] || '').endsWith('server/index.mjs')) {
  v8.setFlagsFromString('--trace_gc');
  if (isMainThread) {
    const t0 = Date.now();
    setInterval(() => {
      const m = process.memoryUsage();
      const mb = x => Math.round(x / 1048576);
      console.log(`MEM t=${((Date.now() - t0) / 1000).toFixed(0)} rss=${mb(m.rss)} heapUsed=${mb(m.heapUsed)} heapTotal=${mb(m.heapTotal)} external=${mb(m.external)}`);
    }, 5000).unref();
  }
}
