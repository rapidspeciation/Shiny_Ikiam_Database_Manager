import test, { after, before } from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import http from 'node:http';
import { brotliDecompressSync, gunzipSync } from 'node:zlib';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createApp } from '../server/index.mjs';

test('HTTP workflow enforces access, saves exact fields, reverses a selected edit and persists operations', async t => {
  const app = await createApp(
    {
      databasePath: ':memory:',
      localMode: true,
      secureCookies: false,
      syncIntervalMs: 0,
      setupToken: 'private-integration-setup',
    },
    {
      seed: {
        Collection_data: [
          {
            row: 2,
            values: {
              CAM_ID: 'CAMTEST',
              SPECIES: 'Test species',
              Sex: 'male',
              Subspecies_Form: 'alpha',
              Notes_Collection_data: 'original',
            },
          },
        ],
        Lists: [
          {
            row: 2,
            values: { Name: 'Unused full name', Abbr_name: 'AB Test', 'InsectaryWild&Reared_CAMid': 'CAM9001' },
          },
        ],
        Location_data: [{ row: 2, values: { Collection_location: 'Test river' } }],
      },
    },
  );
  await app.ready;
  const address = await app.listen(0);
  t.after(() => app.close());
  const base = `http://127.0.0.1:${address.port}/ithomiini`;
  let cookie = '',
    csrf = '';
  async function call(path, method = 'GET', body, authenticate = true) {
    const response = await fetch(base + path, {
      method,
      headers: {
        ...(body ? { 'content-type': 'application/json' } : {}),
        ...(authenticate && cookie ? { cookie, 'x-csrf-token': csrf } : {}),
      },
      body: body ? JSON.stringify({ requestId: randomUUID(), ...body }) : undefined,
    });
    const data = await response.json();
    if (response.headers.get('set-cookie')) cookie = response.headers.get('set-cookie').split(';')[0];
    if (data.csrf) csrf = data.csrf;
    return { response, data };
  }
  assert.equal((await call('/api/records')).response.status, 401);
  const setup = await call('/api/auth/setup', 'POST', {
    token: 'private-integration-setup',
    username: 'integration_admin',
    password: 'test-admin-123',
  });
  assert.equal(setup.response.status, 201);
  const boot = await call('/api/bootstrap');
  // An offline copy (LOCAL_MODE) has no sheet of its own: no link to the team's workbook.
  assert.equal(boot.data.settings.sheetUrl, null);
  assert.equal(boot.data.settings.localMode, true);
  assert.ok(
    (await call('/api/options?module=Collection_data&field=Identifier')).data.options.some(o => o.value === 'AB Test'),
  );
  assert.ok(
    (await call('/api/options?module=Collection_data&field=Collection_location')).data.options.some(
      o => o.value === 'Test river',
    ),
  );
  assert.equal(
    (await call('/api/options?module=Collection_data&field=Subspecies_Form&species=Missing')).data.options.length,
    0,
  );
  assert.equal(
    (await call('/api/options?module=Collection_data&field=Subspecies_Form&species=Test%20species')).data.options[0]
      .value,
    'alpha',
  );
  assert.equal(
    (await call('/api/options?module=Collection_data&field=CAM_ID_insectary')).data.options[0].value,
    'CAM9001',
  );
  const list = await call('/api/records?module=Collection_data');
  assert.equal(list.data.total, 1);
  const record = list.data.records[0];
  const patch = await call(`/api/records/${record.id}`, 'PATCH', {
    values: { Notes_Collection_data: 'reviewed correction' },
    expectedVersion: record.version,
  });
  assert.equal(patch.response.status, 200, JSON.stringify(patch.data));
  assert.equal(patch.data.record.values.Sex, 'male');
  assert.equal(patch.data.status, 'verified');
  const preview = await call('/api/history/preview', 'POST', { actionIds: [patch.data.action.id] });
  assert.equal(preview.response.status, 200);
  assert.equal(preview.data.conflicts.length, 0);
  const undo = await call('/api/history/undo', 'POST', {
    actionIds: [patch.data.action.id],
    reason: 'Integration reversal',
  });
  assert.equal(undo.response.status, 200, JSON.stringify(undo.data));
  const restored = await call(`/api/records/${record.id}`);
  assert.equal(restored.data.record.values.Notes_Collection_data, 'original');
  assert.equal(restored.data.record.values.Sex, 'male');
  const id = randomUUID();
  const event = await call('/api/events', 'POST', {
    requestId: id,
    kind: 'stage_round',
    recordId: record.id,
    values: { observed: 4 },
  });
  const duplicate = await call('/api/events', 'POST', {
    requestId: id,
    kind: 'stage_round',
    recordId: record.id,
    values: { observed: 4 },
  });
  assert.equal(duplicate.data.event.id, event.data.event.id);
  assert.equal((await call('/api/events')).data.events.length, 1);
  const task = await call('/api/tasks', 'POST', { title: 'Check specimen', recordId: record.id });
  assert.equal(task.response.status, 201);
  assert.equal((await call(`/api/tasks/${task.data.task.id}`, 'PATCH', { status: 'done' })).data.task.status, 'done');
  const user = await call('/api/admin/users', 'POST', {
    username: 'integration_observer',
    password: 'test-reader-123',
    role: 'observer',
  });
  assert.equal(user.response.status, 201);
  await call('/api/auth/logout', 'POST', {});
  await call('/api/auth/login', 'POST', { username: 'integration_observer', password: 'test-reader-123' });
  assert.equal((await call('/api/records')).response.status, 200);
  assert.equal((await call('/api/events', 'POST', { kind: 'attempted_edit' })).response.status, 403);
  assert.equal((await call('/api/admin/users')).response.status, 403);
});

// How answers travel, on one app signed in once: compression, cache tags, /health.
let app, port, cookie;
before(async () => {
  const sheets = new LocalSheets({
    Lists: [{ row: 2, values: { Abbr_name: 'FC' } }],
    Collection_data: Array.from({ length: 200 }, (_, i) => ({
      row: i + 2,
      values: { CAM_ID: `CAM0${79000 + i}`, SPECIES: 'Oleria onega', Tube_1_id: `FS0${1000 + i}` },
    })),
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Lists', 'Collection_data'] });
  app = await createApp(
    { localMode: true, secureCookies: false, setupToken: 'test-setup-secret', syncIntervalMs: 0 },
    { store, skipInitialSync: true },
  );
  ({ port } = await app.listen(0, '127.0.0.1'));
  const setup = await fetch(`http://127.0.0.1:${port}/ithomiini/api/auth/setup`, {
    method: 'POST',
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify({ token: 'test-setup-secret', username: 'testadmin', password: 'test12', displayName: 'Test' }),
  });
  cookie = setup.headers.get('set-cookie').split(';')[0];
});
after(() => app?.close());

/** A GET with the bytes as sent (fetch would decompress them). */
const get = (path, headers) =>
  new Promise((resolve, reject) => {
    http
      .get({ host: '127.0.0.1', port, path, headers: { cookie, ...headers } }, res => {
        const parts = [];
        res.on('data', d => parts.push(d));
        res.on('end', () => resolve({ status: res.statusCode, headers: res.headers, body: Buffer.concat(parts) }));
      })
      .on('error', reject);
  });

// Large JSON answers (a proposal of hundreds of rows, a page of records) go compressed: brotli when the
// browser takes it, else gzip; small ones as they are.
test('a large JSON answer is sent with brotli or gzip, as the browser accepts', async () => {
  const path = '/ithomiini/api/records?module=Collection_data&limit=200';
  const plain = await get(path);
  assert.equal(plain.status, 200);
  assert.equal(plain.headers['content-encoding'], undefined);
  const text = plain.body.toString();
  assert.ok(text.length > 8192, String(text.length));

  const br = await get(path, { 'accept-encoding': 'gzip, deflate, br' });
  assert.equal(br.headers['content-encoding'], 'br');
  assert.equal(br.headers.vary, 'accept-encoding');
  assert.equal(brotliDecompressSync(br.body).toString(), text);
  assert.ok(br.body.length < text.length / 4, `${br.body.length} of ${text.length}`);

  const gzip = await get(path, { 'accept-encoding': 'gzip' });
  assert.equal(gzip.headers['content-encoding'], 'gzip');
  assert.equal(gunzipSync(gzip.body).toString(), text);

  // A small answer goes as it is.
  const small = await get('/ithomiini/api/records?module=Collection_data&limit=1', { 'accept-encoding': 'br' });
  assert.equal(small.headers['content-encoding'], undefined);
});

// A whole sheet is kept compressed until it changes: brotli for browsers, gzip only for a client that asks for it.
test('a whole sheet is sent with brotli or gzip, as the client accepts', async () => {
  const path = '/ithomiini/api/table?module=Collection_data';
  const plain = await get(path);
  assert.equal(plain.status, 200);
  assert.equal(plain.headers['content-encoding'], undefined);
  const text = plain.body.toString();
  assert.equal(JSON.parse(text).module, 'Collection_data');
  for (let i = 0; i < 2; i++) {
    const br = await get(path, { 'accept-encoding': 'gzip, deflate, br, zstd' });
    assert.equal(br.headers['content-encoding'], 'br');
    assert.equal(brotliDecompressSync(br.body).toString(), text);
    const gzip = await get(path, { 'accept-encoding': 'gzip, deflate' });
    assert.equal(gzip.headers['content-encoding'], 'gzip');
    assert.equal(gunzipSync(gzip.body).toString(), text);
  }
});

test('Monitoreo walks and tracks and a sheet’s lists answer 304 when unchanged', async () => {
  const api = `http://127.0.0.1:${port}/ithomiini/api`;
  const tags = new Set();
  for (const path of ['monitoring/wikiloc', 'monitoring/tracks', 'verifications?module=Collection_data', 'verifications?module=Lists']) {
    const first = await fetch(`${api}/${path}`, { headers: { cookie } });
    assert.equal(first.status, 200);
    const etag = first.headers.get('etag');
    assert.ok(etag, path);
    assert.equal(first.headers.get('cache-control'), 'private, no-cache');
    await first.json();
    const again = await fetch(`${api}/${path}`, { headers: { cookie, 'if-none-match': etag } });
    assert.equal(again.status, 304, path);
    assert.equal(await again.text(), '');
    tags.add(etag);
  }
  // Different answers have different tags.
  assert.equal(tags.size, 4);
  // A stale tag gets the whole answer.
  const stale = await fetch(`${api}/monitoring/tracks`, { headers: { cookie, 'if-none-match': '"old"' } });
  assert.equal(stale.status, 200);
  assert.deepEqual(await stale.json(), { tracks: [] });
});

// The watch itself: tests/event-loop.test.mjs.
test('/health reports the event loop of the last minute', async () => {
  const health = await (await fetch(`http://127.0.0.1:${port}/health`)).json();
  assert.equal(health.status, 'ok');
  assert.equal(typeof health.eventLoop.p99Ms, 'number');
  assert.equal(typeof health.eventLoop.maxMs, 'number');
  assert.equal(health.eventLoop.windowS, 60);
});
