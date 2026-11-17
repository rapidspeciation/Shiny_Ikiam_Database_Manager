// Identificar's «En línea (servidor)» for visitors (ANON_SERVER_IDENTIFY; server/visitor-identify.mjs on top of the one
// identifier queue, server/identifier.mjs): the queue's order, one photo at a time, the cap, the per-address limit,
// leaving, the temp files gone whatever happens, and nothing new with the flag off. The identifier is the stand-in
// process (tests/fixtures/fake-identifier.mjs): no models, no AI.
import test from 'node:test';
import assert from 'node:assert/strict';
import { spawn } from 'node:child_process';
import { mkdtempSync, readdirSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { createApp } from '../server/index.mjs';
import { createIdentifier } from '../server/identifier.mjs';

const FAKE = new URL('./fixtures/fake-identifier.mjs', import.meta.url).pathname;
/** A "JPEG" the stand-in reads: FF D8, then what it should answer. */
const jpeg = text => Buffer.concat([Buffer.from([0xff, 0xd8]), Buffer.from(text)]);
const b64 = text => jpeg(text).toString('base64');

/** The stand-in identifier, counting the photos sent to its process and not answered yet (never more than one). */
function fakeIdentifier(t) {
  const dir = mkdtempSync(join(tmpdir(), 'visitor-ident-'));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  const seen = { now: 0, most: 0, sent: [] };
  const spawnImpl = (exe, argv, opts) => {
    const proc = spawn(exe, argv, opts);
    const write = proc.stdin.write.bind(proc.stdin);
    proc.stdin.write = (data, ...rest) => {
      seen.now += 1;
      seen.most = Math.max(seen.most, seen.now);
      seen.sent.push(JSON.parse(String(data)).path);
      return write(data, ...rest);
    };
    let buf = '';
    proc.stdout.on('data', d => {
      buf += d;
      const lines = buf.split('\n');
      buf = lines.pop();
      for (const line of lines) if (/"id":/.test(line)) seen.now -= 1;
    });
    return proc;
  };
  const identifier = createIdentifier({ dir, python: process.execPath, args: [FAKE, '--idle', '60'], spawnImpl });
  t.after(() => identifier.close());
  return { dir, identifier, seen };
}

async function setup(t, { on = true, anonIdentify = {}, anonLimits = {} } = {}) {
  const { identifier, seen } = fakeIdentifier(t);
  const scratch = mkdtempSync(join(tmpdir(), 'visitor-scratch-'));
  t.after(() => rmSync(scratch, { recursive: true, force: true }));
  const app = await createApp({
    databasePath: ':memory:',
    localMode: true,
    secureCookies: false,
    syncIntervalMs: 0,
    setupToken: 'visitor-identify-setup',
    identifier,
    identifyScratch: scratch,
    anonServerIdentify: on,
    anonIdentify,
    anonLimits,
  });
  await app.ready;
  const address = await app.listen(0, '127.0.0.1');
  t.after(async () => {
    await app.visitorIdentify?.idle();
    await app.close();
  });
  const base = `http://127.0.0.1:${address.port}/ithomiini/api`;
  const call = async (method, path, { body, ip } = {}) => {
    const r = await fetch(base + path, {
      method,
      headers: { 'content-type': 'application/json', ...(ip ? { 'x-forwarded-for': ip } : {}) },
      body: body && JSON.stringify(body),
    });
    return { status: r.status, headers: r.headers, body: await r.json() };
  };
  /** Polls a ticket until it has an answer (or 404). */
  const answer = async ticket => {
    for (let i = 0; i < 400; i++) {
      const r = await call('GET', `/visitor/identify/${ticket}`);
      if (r.status !== 200 || r.body.state === 'done' || r.body.state === 'failed') return r;
      await new Promise(resolve => setTimeout(resolve, 10));
    }
    throw new Error('no answer');
  };
  return { app, call, answer, identifier, seen, scratch };
}

test('the flag off: the visitor routes are not there (401 as before) and the team path stays signed in only', async t => {
  const { app, call } = await setup(t, { on: false });
  assert.equal(app.visitorIdentify, null);
  for (const [method, path, body] of [
    ['GET', '/visitor/identify'],
    ['POST', '/visitor/identify', { photo: b64('Oleria onega 0.9') }],
    ['GET', '/visitor/identify/00000000-0000-0000-0000-000000000000'],
    ['POST', '/ai/identify-photo', { photo: b64('Oleria onega 0.9') }],
  ]) {
    const r = await call(method, path, { body });
    assert.equal(r.status, 401, `${method} ${path}`);
    assert.equal(r.body.error.code, 'AUTH_REQUIRED');
  }
});

test('a visitor sends a photo, sees a ticket and gets the answer; no sex line, no temp file left', async t => {
  const { call, answer, scratch, app } = await setup(t);
  const info = await call('GET', '/visitor/identify');
  assert.equal(info.status, 200);
  assert.equal(info.body.on, true);
  assert.equal(info.headers.get('cache-control'), 'no-store');
  const sent = await call('POST', '/visitor/identify', { body: { photo: b64('Oleria onega 0.9'), side: 'East' } });
  assert.equal(sent.status, 202, JSON.stringify(sent.body));
  assert.match(sent.body.ticket, /^[0-9a-f-]{36}$/);
  assert.equal(sent.body.place, 1);
  // The models not in yet: their load is in the estimate (about 4 s, then about 1.5 s a photo before any was timed).
  assert.ok(sent.body.waitS >= 5, `cold estimate ${sent.body.waitS}`);
  const r = await answer(sent.body.ticket);
  assert.equal(r.body.state, 'done');
  assert.deepEqual(r.body.answer.species[0], ['Oleria onega', 0.9]);
  assert.equal(r.body.answer.prior, 'East');
  assert.equal('fieldSex' in r.body.answer, false);
  // Given once: the ticket is gone after.
  assert.equal((await call('GET', `/visitor/identify/${sent.body.ticket}`)).status, 404);
  // Loaded now: the next one waits about a photo's time.
  assert.ok((await call('GET', '/visitor/identify')).body.waitS <= 2);
  await app.visitorIdentify.idle();
  assert.deepEqual(readdirSync(scratch), []);
});

test('bad photos are refused before they take a place', async t => {
  const { call } = await setup(t);
  assert.equal((await call('POST', '/visitor/identify', { body: { photo: 'not base64!' } })).status, 400);
  assert.equal((await call('POST', '/visitor/identify', { body: { photo: Buffer.from('PNG...').toString('base64') } })).status, 400);
  assert.equal((await call('POST', '/visitor/identify', { body: {} })).status, 400);
  const big = (await call('POST', '/visitor/identify', { body: { photo: Buffer.concat([jpeg('x'), Buffer.alloc(7 * 1024 * 1024)]).toString('base64') } })).status;
  assert.equal(big, 400);
});

test('one photo at a time: places 1, 2, 3 in order, answers in order, never two in the identifier at once', async t => {
  const { call, answer, seen } = await setup(t);
  const a = await call('POST', '/visitor/identify', { body: { photo: b64('sleep 150 Oleria onega 0.9') } });
  const b = await call('POST', '/visitor/identify', { body: { photo: b64('sleep 50 Mechanitis polymnia 0.8') } });
  const c = await call('POST', '/visitor/identify', { body: { photo: b64('Hypothyris anastasia 0.7') } });
  assert.deepEqual([a.body.place, b.body.place, c.body.place], [1, 2, 3]);
  assert.equal(b.body.ahead, 1);
  assert.ok(c.body.waitS >= b.body.waitS, 'a later place waits at least as long');
  const order = [];
  const answers = await Promise.all([a, b, c].map(x => answer(x.body.ticket).then(r => (order.push(r.body.answer.species[0][0]), r))));
  assert.deepEqual(answers.map(r => r.body.state), ['done', 'done', 'done']);
  assert.deepEqual(order, ['Oleria onega', 'Mechanitis polymnia', 'Hypothyris anastasia']);
  assert.equal(seen.most, 1);
  assert.equal(seen.sent.length, 3);
});

test('the team and the visitors share the one identifier: a team photo goes before waiting visitors, never at the same time', async t => {
  const { identifier, seen, scratch } = await setup(t);
  const dir = mkdtempSync(join(tmpdir(), 'visitor-team-'));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  const file = (name, text) => (writeFileSync(join(dir, name), jpeg(text)), join(dir, name));
  const done = [];
  const ask = (name, opts) => identifier.identify({ path: file(`${name}.jpg`, `sleep 20 Oleria ${name} 0.5`), ...opts }).then(() => done.push(name));
  await Promise.all([ask('v1', { priority: 1 }), ask('w1', {}), ask('v2', { priority: 1 }), ask('team', { first: true }), ask('v3', { priority: 1 })]);
  // v1 was being read; the team's photo next, then the visitors in their order, then the background photo.
  assert.deepEqual(done, ['v1', 'team', 'v2', 'v3', 'w1']);
  assert.equal(seen.most, 1);
  assert.deepEqual(readdirSync(scratch), []);
});

test('the queue is capped: one more visitor gets 503 BUSY with Retry-After', async t => {
  const { call, answer } = await setup(t, { anonIdentify: { maxWaiting: 2 } });
  const a = await call('POST', '/visitor/identify', { body: { photo: b64('sleep 100 Oleria onega 0.9') } });
  const b = await call('POST', '/visitor/identify', { body: { photo: b64('Oleria onega 0.9') } });
  const c = await call('POST', '/visitor/identify', { body: { photo: b64('Oleria onega 0.9') } });
  assert.equal(c.status, 503);
  assert.equal(c.body.error.code, 'BUSY');
  assert.equal(c.headers.get('retry-after'), '60');
  assert.equal((await call('GET', '/visitor/identify')).body.full, true);
  await answer(a.body.ticket);
  await answer(b.body.ticket);
  assert.equal((await call('POST', '/visitor/identify', { body: { photo: b64('Oleria onega 0.9') } })).status, 202);
});

test('per address at most perWindow photos in 10 minutes; another address is not held up', async t => {
  const { call } = await setup(t, { anonIdentify: { perWindow: 2 } });
  const send = ip => call('POST', '/visitor/identify', { body: { photo: b64('Oleria onega 0.9') }, ip });
  assert.equal((await send('203.0.113.1')).status, 202);
  assert.equal((await send('203.0.113.1')).status, 202);
  const third = await send('203.0.113.1');
  assert.equal(third.status, 429);
  assert.equal(third.body.error.code, 'RATE_LIMITED');
  assert.equal((await send('203.0.113.2')).status, 202);
});

test('leaving: a waiting photo leaves the queue and is never read, and its temp file goes', async t => {
  const { call, answer, seen, scratch, app } = await setup(t);
  const a = await call('POST', '/visitor/identify', { body: { photo: b64('sleep 150 Oleria onega 0.9') } });
  const b = await call('POST', '/visitor/identify', { body: { photo: b64('Mechanitis polymnia 0.8') } });
  const c = await call('POST', '/visitor/identify', { body: { photo: b64('Hypothyris anastasia 0.7') } });
  assert.equal((await call('DELETE', `/visitor/identify/${b.body.ticket}`)).status, 200);
  assert.equal((await call('GET', `/visitor/identify/${b.body.ticket}`)).status, 404);
  // c moved up.
  assert.equal((await call('GET', `/visitor/identify/${c.body.ticket}`)).body.place, 2);
  assert.equal((await answer(a.body.ticket)).body.state, 'done');
  assert.deepEqual((await answer(c.body.ticket)).body.answer.species[0], ['Hypothyris anastasia', 0.7]);
  await app.visitorIdentify.idle();
  assert.equal(seen.sent.length, 2);
  assert.deepEqual(readdirSync(scratch), []);
});

test('a visitor who stops asking is dropped from the queue (a closed tab)', async t => {
  const { call, answer, seen, app } = await setup(t, { anonIdentify: { staleMs: 50 } });
  const a = await call('POST', '/visitor/identify', { body: { photo: b64('sleep 200 Oleria onega 0.9') } });
  await call('POST', '/visitor/identify', { body: { photo: b64('Mechanitis polymnia 0.8') } });
  // Only a is asked about; the other one goes stale while a is read.
  assert.equal((await answer(a.body.ticket)).body.state, 'done');
  await app.visitorIdentify.idle();
  assert.equal(seen.sent.length, 1);
});

test('an identifier error answers the visitor and leaves no temp file; a timeout too', async t => {
  const { call, answer, scratch, app } = await setup(t, { anonIdentify: { timeoutMs: 100 } });
  const bad = await call('POST', '/visitor/identify', { body: { photo: b64('garbage that is no name') } });
  const r = await answer(bad.body.ticket);
  assert.equal(r.body.state, 'failed');
  assert.equal(r.body.status, 503);
  const slow = await call('POST', '/visitor/identify', { body: { photo: b64('sleep 400 Oleria onega 0.9') } });
  const late = await answer(slow.body.ticket);
  assert.equal(late.body.state, 'failed');
  assert.equal(late.body.status, 504);
  await app.visitorIdentify.idle();
  assert.deepEqual(readdirSync(scratch), []);
});
