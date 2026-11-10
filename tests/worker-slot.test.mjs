import test from 'node:test';
import assert from 'node:assert/strict';
import { workerPool, workerSlot } from '../server/worker-slot.mjs';

const url = new URL('./fixtures/echo-worker.mjs', import.meta.url);
const sleep = ms => new Promise(resolve => setTimeout(resolve, ms));

test('an idle worker stops after idleMs and gives its memory back; the next call starts another', async t => {
  const slot = workerSlot({ role: 'test', url, data: () => ({}), timeoutMs: 5000, idleMs: 100 });
  t.after(() => slot.close());
  const first = await slot.call({ n: 1 });
  assert.equal(first.n, 1);
  assert.equal(slot.alive, true);
  await sleep(400);
  assert.equal(slot.alive, false, 'stopped while idle');
  assert.equal(slot.status().idleStops, 1);
  assert.equal(slot.status().restarts, 0, 'not counted as a failure');
  const second = await slot.call({ n: 2 });
  assert.equal(second.n, 2);
  assert.notEqual(second.thread, first.thread, 'a new worker answered');
  assert.equal(slot.degraded, false);
});

test('release stops an idle worker at once, not a busy one or none; the next call starts another', async t => {
  const slot = workerSlot({ role: 'test', url, data: () => ({}), timeoutMs: 5000, idleMs: 0 });
  t.after(() => slot.close());
  assert.equal(slot.release(), false, 'none runs yet');
  const answering = slot.call({ n: 1 });
  assert.equal(slot.release(), false, 'a call is waiting for its answer');
  const first = await answering;
  assert.equal(first.n, 1);
  assert.equal(slot.release(), true);
  assert.equal(slot.alive, false);
  assert.equal(slot.status().idleStops, 1);
  assert.equal(slot.status().restarts, 0, 'not counted as a failure');
  const second = await slot.call({ n: 2 });
  assert.equal(second.n, 2);
  assert.notEqual(second.thread, first.thread, 'a new worker answered');
  assert.equal(slot.degraded, false);
});

test('a worker kept busy does not stop; idleMs 0 never stops it', async t => {
  const slot = workerSlot({ role: 'test', url, data: () => ({}), timeoutMs: 5000, idleMs: 300 });
  const forever = workerSlot({ role: 'test', url, data: () => ({}), timeoutMs: 5000, idleMs: 0 });
  t.after(() => {
    slot.close();
    forever.close();
  });
  const { thread } = await slot.call({ n: 0 });
  // Calls every 100 ms for 600 ms: the 300 ms wait starts again after each.
  for (let i = 1; i <= 6; i++) {
    await sleep(100);
    assert.equal((await slot.call({ n: i })).thread, thread);
  }
  await forever.call({ n: 0 });
  await sleep(500);
  assert.equal(forever.alive, true);
});

test("a pool's idle workers stop too, and its status counts them", async t => {
  const pool = workerPool({ role: 'test', url, data: () => ({}), timeoutMs: 5000, idleMs: 100, size: 2 });
  t.after(() => pool.close());
  const [a, b] = await Promise.all([pool.call({ n: 1 }), pool.call({ n: 2 })]);
  // Two workers answered. Not "2 of 2 alive" now: on a busy machine the first can be idle for 100 ms (and stop)
  // before the second has started and answered.
  assert.notEqual(a.thread, b.thread);
  assert.match(pool.status().workers, /^[12] of 2$/);
  await sleep(400);
  assert.equal(pool.alive, false);
  assert.equal(pool.status().idleStops, 2);
  assert.equal((await pool.call({ n: 3 })).n, 3);
});
