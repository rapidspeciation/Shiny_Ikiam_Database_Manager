import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdirSync, writeFileSync, rmSync, existsSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createApp } from '../server/index.mjs';

// The built files are served from web/ (built from frontend/, not committed).
const assets = fileURLToPath(new URL('../web/assets/', import.meta.url));

test('built scripts are cached for good; other files are checked again', async () => {
  const created = !existsSync(assets);
  mkdirSync(assets, { recursive: true });
  const name = `cache-test-${randomUUID().slice(0, 8)}.js`;
  writeFileSync(assets + name, 'export {}\n');
  const store = new Store({ localMode: true }, { sheets: new LocalSheets({}) });
  const app = await createApp({ localMode: true, secureCookies: false, syncIntervalMs: 0 }, { store, skipInitialSync: true });
  const address = await app.listen(0, '127.0.0.1');
  try {
    const base = `http://127.0.0.1:${address.port}/ithomiini`;
    const script = await fetch(`${base}/assets/${name}`);
    assert.equal(script.status, 200);
    assert.equal(script.headers.get('cache-control'), 'public, max-age=31536000, immutable');
    // A script of an older build is a 404, never cached.
    const missing = await fetch(`${base}/assets/gone-00000000.js`);
    assert.equal(missing.status, 404);
    assert.equal(missing.headers.get('cache-control'), 'no-store');
  } finally {
    await app.close();
    rmSync(created ? assets : assets + name, { recursive: true, force: true });
  }
});
