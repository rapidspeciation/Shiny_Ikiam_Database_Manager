import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { createIdentifier } from '../server/identifier.mjs';

// The AI Identifier's process (server/identifier.mjs), with a stand-in that speaks its lines
// (tests/fixtures/fake-identifier.mjs) instead of tools/identifier/worker.py and its models.

const FAKE = new URL('./fixtures/fake-identifier.mjs', import.meta.url).pathname;

/**
 * Moves a mocked setTimeout clock on by `ms` until `promise` settles (its timer fires as soon as it is set): at most
 * 30 s of real time (Date is not mocked), then the mocked timers go back in any case.
 */
async function ticked(t, promise, ms) {
  let done = false;
  promise.then(() => (done = true), () => (done = true));
  const until = Date.now() + 30_000;
  try {
    while (!done) {
      if (Date.now() > until) throw new Error('the call did not settle within 30 s of real time');
      t.mock.timers.tick(ms);
      await new Promise(resolve => setImmediate(resolve));
    }
  } finally {
    t.mock.timers.reset();
  }
  return promise;
}

function setup(t, options = {}) {
  const dir = mkdtempSync(join(tmpdir(), 'identifier-'));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  const photo = (name, text) => {
    const path = join(dir, name);
    writeFileSync(path, text);
    return path;
  };
  const idle = options.idleSeconds ?? 60;
  const identifier = createIdentifier({ dir, python: process.execPath, args: [FAKE, '--idle', String(idle)], ...options });
  t.after(() => identifier.close());
  return { dir, photo, identifier };
}

test('not installed: says so, starts nothing', async () => {
  const identifier = createIdentifier({ dir: join(tmpdir(), 'no-such-identifier-dir') });
  assert.match((await identifier.identify({ path: '/x.jpg' })).error, /not installed/);
  assert.deepEqual(identifier.status(), { installed: false, running: false, loaded: false, photos: 0, lastMs: null });
  assert.match((await createIdentifier().identify({ path: '/x.jpg' })).error, /not installed/);
});

test('started with the first photo, one process for the following ones, one photo at a time', async t => {
  const { photo, identifier } = setup(t);
  assert.equal(identifier.status().running, false);
  const a = photo('a.jpg', 'Mechanitis polymnia 0.9');
  const b = photo('b.jpg', 'Oleria cyrene 0.3');
  const [first, second] = await Promise.all([identifier.identify({ path: a }), identifier.identify({ path: b, side: 'East', box: [0.1, 0.1, 0.9, 0.9] })]);
  assert.deepEqual(first.species[0], ['Mechanitis polymnia', 0.9]);
  assert.equal(first.loadMs, 10);
  assert.deepEqual(second.species[0], ['Oleria cyrene', 0.3]);
  assert.equal(second.prior, 'East');
  assert.equal(second.crop, 'manual_bbox_square_letterbox');
  assert.equal(second.loadMs, undefined);
  assert.equal(first.pid, second.pid);
  assert.deepEqual(identifier.status(), { installed: true, running: true, loaded: true, photos: 2, lastMs: identifier.status().lastMs });
});

test("a photo it cannot read is that photo's error; the next one is read", async t => {
  const { photo, identifier } = setup(t);
  assert.match((await identifier.identify({ path: photo('bad.jpg', 'not a picture') })).error, /cannot identify/);
  assert.deepEqual((await identifier.identify({ path: photo('ok.jpg', 'Melinaea isocomma 0.6') })).species[0], ['Melinaea isocomma', 0.6]);
});

test('ended after its idle time (memory given back); the next photo starts it again', async t => {
  const { photo, identifier } = setup(t, { idleSeconds: 0.2 });
  const first = await identifier.identify({ path: photo('a.jpg', 'Mechanitis polymnia 0.9') });
  // Stopped within 600 ms (looked at every 20 ms, not waited out in full).
  for (let i = 0; i < 30 && identifier.status().running; i++) await new Promise(resolve => setTimeout(resolve, 20));
  assert.equal(identifier.status().running, false);
  const again = await identifier.identify({ path: photo('b.jpg', 'Mechanitis polymnia 0.8') });
  assert.notEqual(again.pid, first.pid);
  assert.equal(again.loadMs, 10);
});

test('a process that dies answers its photo with an error; the next photo gets a new one', async t => {
  const { photo, identifier } = setup(t);
  assert.match((await identifier.identify({ path: photo('crash.jpg', 'crash') })).error, /stopped \(3\)/);
  assert.equal(identifier.status().running, false);
  assert.deepEqual((await identifier.identify({ path: photo('ok.jpg', 'Oleria cyrene 0.4') })).species[0], ['Oleria cyrene', 0.4]);
});

test('a photo that takes too long: the process is stopped and the call says to call again', async t => {
  // The slow photo never answers: the limit only has to leave a busy machine time to start the next process.
  const { photo, identifier } = setup(t, { firstMs: 2000, photoMs: 2000 });
  // The slow photo's limit on a mocked setTimeout (moved past 2 s at once); the next photo's limit is real.
  t.mock.timers.enable({ apis: ['setTimeout'] });
  const out = await ticked(t, identifier.identify({ path: photo('slow.jpg', 'slow') }), 2000);
  assert.match(out.error, /took more than/);
  assert.deepEqual((await identifier.identify({ path: photo('ok.jpg', 'Oleria cyrene 0.4') })).species[0], ['Oleria cyrene', 0.4]);
});

test('a photo a person waits for (first) goes ahead of the queued background photos', async t => {
  const { photo, identifier } = setup(t);
  const done = [];
  const ask = (name, first = false) => identifier.identify({ path: photo(`${name}.jpg`, `Oleria ${name} 0.5`), first }).then(() => done.push(name));
  await Promise.all([ask('w1'), ask('w2'), ask('w3'), ask('p1', true), ask('p2', true)]);
  // w1 was already running; the people's photos then, in their order; then the rest of the walk.
  assert.deepEqual(done, ['w1', 'p1', 'p2', 'w2', 'w3']);
});
