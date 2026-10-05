import test from 'node:test';
import assert from 'node:assert/strict';
import http from 'node:http';
import { brotliDecompressSync, gunzipSync } from 'node:zlib';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createApp } from '../server/index.mjs';

// Large JSON answers (a proposal of hundreds of rows, a page of records) go compressed: brotli when the
// browser takes it, else gzip; small ones as they are.

const get = (port, path, headers) =>
  new Promise((resolve, reject) => {
    http
      .get({ host: '127.0.0.1', port, path, headers }, res => {
        const parts = [];
        res.on('data', d => parts.push(d));
        res.on('end', () => resolve({ status: res.statusCode, headers: res.headers, body: Buffer.concat(parts) }));
      })
      .on('error', reject);
  });

test('a large JSON answer is sent with brotli or gzip, as the browser accepts', async () => {
  const sheets = new LocalSheets({
    Collection_data: Array.from({ length: 200 }, (_, i) => ({
      row: i + 2,
      values: { CAM_ID: `CAM0${79000 + i}`, SPECIES: 'Oleria onega', Tube_1_id: `FS0${1000 + i}` },
    })),
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Collection_data'] });
  const app = await createApp(
    { localMode: true, secureCookies: false, setupToken: 'test-setup-secret', syncIntervalMs: 0 },
    { store, skipInitialSync: true },
  );
  const address = await app.listen(0, '127.0.0.1');
  try {
    const api = `http://127.0.0.1:${address.port}/ithomiini/api`;
    const setup = await fetch(`${api}/auth/setup`, {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({
        token: 'test-setup-secret',
        username: 'testadmin',
        password: 'test12',
        displayName: 'Test',
      }),
    });
    const cookie = setup.headers.get('set-cookie').split(';')[0];
    const path = '/ithomiini/api/records?module=Collection_data&limit=200';
    const plain = await get(address.port, path, { cookie });
    assert.equal(plain.status, 200);
    assert.equal(plain.headers['content-encoding'], undefined);
    const text = plain.body.toString();
    assert.ok(text.length > 8192, String(text.length));

    const br = await get(address.port, path, { cookie, 'accept-encoding': 'gzip, deflate, br' });
    assert.equal(br.headers['content-encoding'], 'br');
    assert.equal(br.headers.vary, 'accept-encoding');
    assert.equal(brotliDecompressSync(br.body).toString(), text);
    assert.ok(br.body.length < text.length / 4, `${br.body.length} of ${text.length}`);

    const gzip = await get(address.port, path, { cookie, 'accept-encoding': 'gzip' });
    assert.equal(gzip.headers['content-encoding'], 'gzip');
    assert.equal(gunzipSync(gzip.body).toString(), text);

    // A small answer goes as it is.
    const small = await get(address.port, '/ithomiini/api/records?module=Collection_data&limit=1', {
      cookie,
      'accept-encoding': 'br',
    });
    assert.equal(small.headers['content-encoding'], undefined);
  } finally {
    await app.close();
  }
});
