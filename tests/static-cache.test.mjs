import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdirSync, mkdtempSync, writeFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import http from 'node:http';
import { brotliCompressSync, brotliDecompressSync } from 'node:zlib';
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

test('a file with a brotli copy beside it is sent compressed to a browser that takes brotli', async () => {
  const webRoot = mkdtempSync(join(tmpdir(), 'static-br-'));
  mkdirSync(join(webRoot, 'assets'));
  mkdirSync(join(webRoot, 'fonts'));
  const script = 'export const words = "' + 'mariposa '.repeat(500) + '"\n';
  writeFileSync(join(webRoot, 'assets', 'index-abc123.js'), script);
  writeFileSync(join(webRoot, 'assets', 'index-abc123.js.br'), brotliCompressSync(script));
  writeFileSync(join(webRoot, 'fonts', 'sans.ttf'), 'font');
  const store = new Store({ localMode: true }, { sheets: new LocalSheets({}) });
  const app = await createApp({ localMode: true, secureCookies: false, syncIntervalMs: 0, webRoot }, { store, skipInitialSync: true });
  const { port } = await app.listen(0, '127.0.0.1');
  // The bytes as sent (fetch would decompress them).
  const get = (path, headers = {}) =>
    new Promise((resolve, reject) => {
      http
        .get({ host: '127.0.0.1', port, path: `/ithomiini${path}`, headers }, res => {
          const parts = [];
          res.on('data', d => parts.push(d));
          res.on('end', () => resolve({ status: res.statusCode, headers: res.headers, body: Buffer.concat(parts) }));
        })
        .on('error', reject);
    });
  try {
    const br = await get('/assets/index-abc123.js', { 'accept-encoding': 'gzip, deflate, br, zstd' });
    assert.equal(br.status, 200);
    assert.equal(br.headers['content-encoding'], 'br');
    assert.equal(br.headers['content-type'], 'text/javascript; charset=utf-8');
    assert.equal(br.headers.vary, 'accept-encoding');
    assert.equal(br.headers['cache-control'], 'public, max-age=31536000, immutable');
    assert.equal(brotliDecompressSync(br.body).toString(), script);
    // Without brotli: the file as it is (the proxy may compress it).
    const plain = await get('/assets/index-abc123.js', { 'accept-encoding': 'gzip' });
    assert.equal(plain.headers['content-encoding'], undefined);
    assert.equal(plain.headers.vary, 'accept-encoding');
    assert.equal(plain.body.toString(), script);
    // A font without a copy: as it is, typed as a font.
    const font = await get('/fonts/sans.ttf', { 'accept-encoding': 'br' });
    assert.equal(font.headers['content-encoding'], undefined);
    assert.equal(font.headers['content-type'], 'font/ttf');
    assert.equal(font.body.toString(), 'font');
  } finally {
    await app.close();
    rmSync(webRoot, { recursive: true, force: true });
  }
});
