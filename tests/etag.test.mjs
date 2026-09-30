import test from 'node:test';
import assert from 'node:assert/strict';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createApp } from '../server/index.mjs';

test('Monitoreo walks and tracks and a sheet’s lists answer 304 when unchanged', async () => {
  const sheets = new LocalSheets({
    Lists: [{ row: 2, values: { Abbr_name: 'FC' } }],
    Collection_data: [{ row: 2, values: { CAM_ID: 'CAM000001', SPECIES: 'Oleria onega' } }],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Lists', 'Collection_data'] });
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
      body: JSON.stringify({ token: 'test-setup-secret', username: 'testadmin', password: 'test12', displayName: 'Test' }),
    });
    const cookie = setup.headers.get('set-cookie').split(';')[0];
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
  } finally {
    await app.close();
  }
});
