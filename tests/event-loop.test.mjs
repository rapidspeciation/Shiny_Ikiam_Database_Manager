// /health says how late the server got to waiting requests over the last minute: a long
// piece of work (a big proposal's table) shows up there, other timers running or not.
import test from 'node:test';
import assert from 'node:assert/strict';
import { watchEventLoop } from '../server/event-loop.mjs';
import { createApp } from '../server/index.mjs';

const wait = ms => new Promise(resolve => setTimeout(resolve, ms));
const busy = ms => {
  const until = Date.now() + ms;
  while (Date.now() < until);
};

test('a block of the event loop shows in its p99 and longest wait', async () => {
  const other = setInterval(() => {}, 5);
  const watch = watchEventLoop({ sampleMs: 20, windowMs: 10_000 });
  try {
    await wait(100);
    assert.ok(watch.stats().maxMs < 300, JSON.stringify(watch.stats()));
    busy(400);
    await wait(60);
    const { p99Ms, maxMs, windowS } = watch.stats();
    assert.ok(maxMs >= 350, `maxMs ${maxMs}`);
    // The asks the block swallowed count too: p99 is about the block, not one late ask among many.
    assert.ok(p99Ms >= 300, `p99Ms ${p99Ms}`);
    assert.equal(windowS, 10);
  } finally {
    watch.stop();
    clearInterval(other);
  }
});

test('/health reports the event loop', async () => {
  const app = await createApp({ databasePath: ':memory:', localMode: true, secureCookies: false, syncIntervalMs: 0 }, { seed: {} });
  await app.ready;
  try {
    await new Promise(resolve => app.server.listen(0, '127.0.0.1', resolve));
    const { port } = app.server.address();
    const health = await (await fetch(`http://127.0.0.1:${port}/health`)).json();
    assert.equal(health.status, 'ok');
    assert.deepEqual(Object.keys(health.eventLoop), ['p99Ms', 'maxMs', 'windowS']);
    assert.equal(health.eventLoop.windowS, 60);
  } finally {
    await app.close();
  }
});
