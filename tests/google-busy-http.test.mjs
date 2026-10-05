// The whole flow over HTTP, as the pages use it: the pulse says the workbook is busy, a save
// waits (records/batch → queued) and its page reads its outcome (/api/outbox/:id); Emergidos
// entries are kept in the app (/api/staged), seen by everyone, and «Guardar en Google
// Sheets» (/api/staged/flush) writes them once the lab's busy workbook (/api/local/busy) answers.
import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { createApp } from '../server/index.mjs';

test('busy workbook, waiting saves and staged entries over HTTP', async () => {
  const seed = {
    Insectary_data: [
      { row: 2, values: { Insectary_ID: 'A0E', Sex: 'female', SPECIES: 'Ithomia salapia' } },
      { row: 3, values: { Insectary_ID: 'A1E' } },
      { row: 4, values: { Insectary_ID: 'A2E' } },
    ],
  };
  const app = await createApp(
    { databasePath: ':memory:', localMode: true, secureCookies: false, syncIntervalMs: 0, setupToken: 'setup-token-123', pulseWaitMs: 200, flushWaitMs: 500 },
    { seed },
  );
  await app.ready;
  await new Promise(resolve => app.server.listen(0, '127.0.0.1', resolve));
  const base = `http://127.0.0.1:${app.server.address().port}`;
  const people = {};
  const call = async (who, method, path, body) => {
    const r = await fetch(`${base}/api/${path}`, {
      method,
      headers: { 'content-type': 'application/json', origin: base, cookie: people[who].cookie, 'x-csrf-token': people[who].csrf },
      body: body === undefined ? undefined : JSON.stringify(body),
    });
    return { status: r.status, body: await r.json() };
  };
  try {
    const setup = await fetch(`${base}/api/auth/setup`, {
      method: 'POST',
      headers: { 'content-type': 'application/json', origin: base },
      body: JSON.stringify({ token: 'setup-token-123', username: 'ana', displayName: 'Ana', password: 'secret12' }),
    });
    assert.equal(setup.status, 201);
    people.ana = { cookie: setup.headers.getSetCookie().map(c => c.split(';')[0]).join('; '), csrf: (await setup.json()).csrf };

    let pulse = await call('ana', 'GET', 'pulse');
    assert.equal(pulse.body.workbook.state, 'ok');
    const revision = pulse.body.revision;

    // The lab makes the workbook busy: the pulse changes at once.
    const busy = await call('ana', 'POST', 'local/busy', { minutes: 5, mode: 'unavailable', delayMs: 0 });
    assert.equal(busy.body.workbook.state, 'busy');
    pulse = await call('ana', 'GET', `pulse?wait=1&revision=${encodeURIComponent(revision)}`);
    assert.equal(pulse.body.workbook.state, 'busy');

    // A save from Buscador waits.
    const table = await call('ana', 'GET', 'table?module=Insectary_data');
    const a0e = table.body.rows.find(r => r.row === 2).id;
    const save = await call('ana', 'POST', 'records/batch', {
      requestId: randomUUID(),
      purpose: 'tablas',
      partial: true,
      edits: [{ id: a0e, values: { Sex: 'male' }, expected: { Sex: 'female' } }],
    });
    assert.equal(save.status, 200);
    assert.equal(save.body.status, 'queued');
    assert.equal((await call('ana', 'GET', `outbox/${save.body.outboxId}`)).body.outbox.status, 'queued');

    // An emerged butterfly is kept in the app, for everyone; its ID is taken.
    const staged = await call('ana', 'POST', 'staged', {
      requestId: randomUUID(),
      purpose: 'emergidos',
      partial: false,
      creates: [{ clientId: 'k1', module: 'Insectary_data', values: { Insectary_ID: 'A1E', Sex: 'male', Intro2Insectary_date: 46300 } }],
    });
    assert.equal(staged.body.status, 'staged');
    const list = await call('ana', 'GET', 'staged');
    assert.deepEqual(list.body.claims.map(c => c.value), ['A1E']);
    assert.equal((await call('ana', 'GET', 'ids?kind=insectary&count=5')).body.sequence[0], 'A2E');

    // «Guardar en Google Sheets» while busy: it waits too.
    const flush = await call('ana', 'POST', 'staged/flush', { requestId: randomUUID() });
    assert.equal(flush.body.status, 'queued');

    // Google answers again: both are written, in order.
    await call('ana', 'POST', 'local/busy', { minutes: 0 });
    for (let i = 0; i < 100 && (await call('ana', 'GET', 'pulse')).body.outbox.waiting; i++) await new Promise(r => setTimeout(r, 20));
    const done = await call('ana', 'GET', `outbox/${save.body.outboxId}`);
    assert.equal(done.body.outbox.status, 'done');
    assert.equal(done.body.records[0].values.Sex, 'male');
    assert.equal((await call('ana', 'GET', 'staged')).body.items.length, 0);
    const health = await (await fetch(`${base}/health`)).json();
    assert.deepEqual([health.google.workbook.state, health.google.outbox.waiting, health.google.staged.staged], ['ok', 0, 0]);
  } finally {
    await app.close();
  }
});
