// How late the server got to waiting requests: a long piece of work (a big proposal's table)
// shows up, other timers running or not. /health's report: tests/integration.test.mjs.
import test from 'node:test';
import assert from 'node:assert/strict';
import { watchEventLoop } from '../server/event-loop.mjs';

const wait = ms => new Promise(resolve => setTimeout(resolve, ms));
const busy = ms => {
  const until = Date.now() + ms;
  while (Date.now() < until);
};

test('a block of the event loop shows in its p99 and longest wait', async () => {
  const other = setInterval(() => {}, 5);
  const watch = watchEventLoop({ sampleMs: 10, windowMs: 10_000 });
  try {
    await wait(50);
    assert.ok(watch.stats().maxMs < 200, JSON.stringify(watch.stats()));
    busy(250);
    await wait(30);
    const { p99Ms, maxMs, windowS } = watch.stats();
    assert.ok(maxMs >= 220, `maxMs ${maxMs}`);
    // The asks the block swallowed count too: p99 is about the block, not one late ask among many.
    assert.ok(p99Ms >= 180, `p99Ms ${p99Ms}`);
    assert.equal(windowS, 10);
  } finally {
    watch.stop();
    clearInterval(other);
  }
});
