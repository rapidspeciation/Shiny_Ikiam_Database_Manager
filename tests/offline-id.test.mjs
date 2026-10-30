import { test } from 'node:test';
import assert from 'node:assert/strict';
import http from 'node:http';
import { mkdirSync, mkdtempSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { serveOfflineId, MODELS } from '../server/offline-id.mjs';

function setup() {
  const dir = mkdtempSync(join(tmpdir(), 'offline-id-'));
  const web = join(dir, 'web'),
    ident = join(dir, 'identifier');
  mkdirSync(join(web, 'prueba-offline', 'ort'), { recursive: true });
  writeFileSync(join(web, 'prueba-offline', 'index.html'), '<!doctype html><title>t</title>');
  writeFileSync(join(web, 'secret.txt'), 'outside');
  for (const m of MODELS) {
    mkdirSync(join(ident, m.path, '..'), { recursive: true });
    writeFileSync(join(ident, m.path), Buffer.from(Array.from({ length: 1000 }, (_, i) => i % 256)));
  }
  let user = null;
  const server = http.createServer((req, res) => {
    const path = new URL(req.url, 'http://x').pathname.slice('/prueba-offline'.length);
    serveOfflineId(req, res, { path, webRoot: web, identifierDir: ident, user });
  });
  return new Promise(ok =>
    server.listen(0, '127.0.0.1', () => ok({ base: `http://127.0.0.1:${server.address().port}/prueba-offline`, server, signIn: () => (user = { username: 'u' }) })),
  );
}

test('offline test page: isolated page, models only signed in, ranges and ETags', async () => {
  const { base, server, signIn } = await setup();
  try {
    const page = await fetch(`${base}/`);
    assert.equal(page.status, 200);
    assert.equal(page.headers.get('cross-origin-opener-policy'), 'same-origin');
    assert.equal(page.headers.get('cross-origin-embedder-policy'), 'require-corp');
    assert.match(page.headers.get('content-security-policy'), /'wasm-unsafe-eval'/);
    const bare = await fetch(base, { redirect: 'manual' });
    assert.equal(bare.status, 301);
    assert.equal((await fetch(`${base}/..%2Fsecret.txt`)).status, 404);

    assert.equal((await fetch(`${base}/models/list.json`)).status, 401);
    assert.equal((await fetch(`${base}/models/bioclip25h_visual_int8.onnx`)).status, 401);
    signIn();
    const list = await (await fetch(`${base}/models/list.json`)).json();
    assert.deepEqual(list.files.map(f => [f.name, f.bytes]), MODELS.map(m => [m.name, 1000]));
    const { etag } = list.files[1];

    const part = await fetch(`${base}/models/bioclip25h_visual_int8.onnx`, { headers: { range: 'bytes=100-199', 'if-range': etag } });
    assert.equal(part.status, 206);
    assert.equal(part.headers.get('content-range'), 'bytes 100-199/1000');
    const bytes = new Uint8Array(await part.arrayBuffer());
    assert.equal(bytes.length, 100);
    assert.equal(bytes[0], 100);
    // another version of the file: the whole file, not a part of a different one
    assert.equal((await fetch(`${base}/models/bioclip25h_visual_int8.onnx`, { headers: { range: 'bytes=0-9', 'if-range': '"old"' } })).status, 200);
    assert.equal((await fetch(`${base}/models/bioclip25h_visual_int8.onnx`, { headers: { 'if-none-match': etag } })).status, 304);
    assert.equal((await fetch(`${base}/models/bioclip25h_visual_int8.onnx`, { headers: { range: 'bytes=5000-' } })).status, 416);
    assert.equal((await fetch(`${base}/models/other.onnx`)).status, 404);
  } finally {
    server.close();
  }
});
