import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdirSync, mkdtempSync, writeFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createApp } from '../server/index.mjs';

test('built scripts are cached for good; other files are checked again', async () => {
  // A built page (web/ is built from frontend/, not committed) in a folder of its own.
  const webRoot = mkdtempSync(join(tmpdir(), 'static-cache-'));
  mkdirSync(join(webRoot, 'assets'));
  writeFileSync(join(webRoot, 'assets', 'index-abc123.js'), 'export {}\n');
  writeFileSync(join(webRoot, 'favicon.svg'), '<svg/>');
  const store = new Store({ localMode: true }, { sheets: new LocalSheets({}) });
  const app = await createApp({ localMode: true, secureCookies: false, syncIntervalMs: 0, webRoot }, { store, skipInitialSync: true });
  const address = await app.listen(0, '127.0.0.1');
  try {
    const base = `http://127.0.0.1:${address.port}/ithomiini`;
    const script = await fetch(`${base}/assets/index-abc123.js`);
    assert.equal(script.status, 200);
    assert.equal(script.headers.get('cache-control'), 'public, max-age=31536000, immutable');
    const other = await fetch(`${base}/favicon.svg`);
    assert.equal(other.status, 200);
    assert.equal(other.headers.get('cache-control'), 'public, max-age=3600');
    // A script of an older build is a 404, never cached.
    const missing = await fetch(`${base}/assets/gone-00000000.js`);
    assert.equal(missing.status, 404);
    assert.equal(missing.headers.get('cache-control'), 'no-store');
  } finally {
    await app.close();
    rmSync(webRoot, { recursive: true, force: true });
  }
});
