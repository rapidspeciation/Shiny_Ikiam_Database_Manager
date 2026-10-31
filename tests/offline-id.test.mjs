import { test } from 'node:test';
import assert from 'node:assert/strict';
import http from 'node:http';
import { mkdirSync, mkdtempSync, readFileSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { request } from 'node:http';
import { gunzipSync, zstdDecompressSync } from 'node:zlib';
import { serveOfflineId, MODELS, PART_BYTES, modelsWithoutParts, pickEncoding, precompressParts } from '../server/offline-id.mjs';

function setup(sizes = {}) {
  const dir = mkdtempSync(join(tmpdir(), 'offline-id-'));
  const web = join(dir, 'web'),
    ident = join(dir, 'identifier');
  mkdirSync(join(web, 'prueba-offline', 'ort'), { recursive: true });
  writeFileSync(join(web, 'prueba-offline', 'index.html'), '<!doctype html><title>t</title>');
  writeFileSync(join(web, 'secret.txt'), 'outside');
  for (const m of MODELS) {
    mkdirSync(join(ident, m.path, '..'), { recursive: true });
    const n = sizes[m.name] ?? 1000;
    const b = Buffer.alloc(n);
    for (let i = 0; i < n; i++) b[i] = (i % 256) & (i % 7 ? 0xff : 0x0f);
    writeFileSync(join(ident, m.path), b);
  }
  let user = null;
  const server = http.createServer((req, res) => {
    const path = new URL(req.url, 'http://x').pathname.slice('/prueba-offline'.length);
    serveOfflineId(req, res, { path, webRoot: web, identifierDir: ident, user });
  });
  return new Promise(ok =>
    server.listen(0, '127.0.0.1', () => ok({ base: `http://127.0.0.1:${server.address().port}/prueba-offline`, port: server.address().port, ident, server, signIn: () => (user = { username: 'u' }) })),
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

/** A raw GET (fetch would decompress): status, headers and the bytes as sent. */
function raw(port, path, headers = {}) {
  return new Promise((ok, fail) => {
    const req = request({ host: '127.0.0.1', port, path, headers }, res => {
      const chunks = [];
      res.on('data', c => chunks.push(c));
      res.on('end', () => ok({ status: res.statusCode, headers: res.headers, body: Buffer.concat(chunks) }));
    });
    req.on('error', fail);
    req.end();
  });
}

test('pickEncoding: zstd first, then gzip, q=0 refuses', () => {
  assert.equal(pickEncoding('gzip, deflate, br, zstd'), 'zstd');
  assert.equal(pickEncoding('gzip, deflate, br'), 'gzip');
  assert.equal(pickEncoding('zstd;q=0, gzip;q=0.5'), 'gzip');
  assert.equal(pickEncoding('identity'), null);
  assert.equal(pickEncoding(''), null);
  assert.equal(pickEncoding('*'), 'zstd');
});

test('model parts: precompressed zstd/gzip per part, plain without, ETag tied to the plain content', async () => {
  const enc = 'bioclip25h_visual_int8.onnx';
  const big = PART_BYTES + 12345;
  const { port, ident, server, signIn } = await setup({ [enc]: big });
  const p = '/prueba-offline/models';
  try {
    signIn();
    const plainFile = readFileSync(join(ident, 'onnx', enc));
    // Before precompressing: plain parts, list says wire = bytes.
    let r = await raw(port, `${p}/${enc}?part=1`, { 'accept-encoding': 'gzip, zstd' });
    assert.equal(r.status, 200);
    assert.equal(r.headers['content-encoding'], undefined);
    assert.equal(r.headers['x-part-bytes'], '12345');
    assert.deepEqual(r.body, plainFile.subarray(PART_BYTES));
    const plainEtag = r.headers.etag;
    let list = JSON.parse((await raw(port, `${p}/list.json`, { 'accept-encoding': 'zstd' })).body);
    assert.equal(list.partBytes, PART_BYTES);
    assert.equal(list.files[1].wire, big);
    assert.equal(list.files[1].encoding, null);

    assert.equal((await modelsWithoutParts(ident)).length, MODELS.length);
    const { manifest } = await precompressParts(ident, { stamp: '20261031T000000Z' });
    assert.deepEqual(await modelsWithoutParts(ident), []);
    assert.equal(manifest.files[enc].parts.length, 2);
    assert.ok(manifest.files[enc].parts[0].zst < PART_BYTES / 2, 'compressible test data');

    for (const [accept, encoding, unpack] of [
      ['gzip, deflate, br, zstd', 'zstd', zstdDecompressSync],
      ['gzip, deflate, br', 'gzip', gunzipSync],
    ]) {
      for (const i of [0, 1]) {
        r = await raw(port, `${p}/${enc}?part=${i}`, { 'accept-encoding': accept });
        assert.equal(r.status, 200);
        assert.equal(r.headers['content-encoding'], encoding);
        assert.equal(r.headers.vary, 'accept-encoding');
        assert.match(r.headers['cache-control'], /no-transform/);
        assert.equal(Number(r.headers['content-length']), r.body.length);
        assert.equal(r.headers.etag, i === 1 ? plainEtag : plainEtag.replace(/-p1"$/, '-p0"'));
        assert.deepEqual(unpack(r.body), plainFile.subarray(i * PART_BYTES, (i + 1) * PART_BYTES));
      }
      list = JSON.parse((await raw(port, `${p}/list.json`, { 'accept-encoding': accept })).body);
      assert.equal(list.files[1].encoding, encoding);
      assert.ok(list.files[1].wire < big / 2);
      assert.equal(list.files[1].bytes, big);
    }
    // No Accept-Encoding: plain.
    r = await raw(port, `${p}/${enc}?part=0`);
    assert.equal(r.headers['content-encoding'], undefined);
    assert.equal(r.body.length, PART_BYTES);
    // Same ETag whatever the encoding: 304 for a weak match.
    assert.equal((await raw(port, `${p}/${enc}?part=1`, { 'if-none-match': plainEtag, 'accept-encoding': 'zstd' })).status, 304);
    assert.equal((await raw(port, `${p}/${enc}?part=2`)).status, 416);
    assert.equal((await raw(port, `${p}/${enc}?part=x`)).status, 416);

    // The model changes: the old compressed parts no longer match, plain bytes go.
    writeFileSync(join(ident, 'onnx', enc), Buffer.alloc(big, 7));
    assert.deepEqual(await modelsWithoutParts(ident), [enc]);
    r = await raw(port, `${p}/${enc}?part=0`, { 'accept-encoding': 'zstd' });
    assert.equal(r.headers['content-encoding'], undefined);
    assert.notEqual(r.headers.etag.replace(/-p0"$/, ''), plainEtag.replace(/-p1"$/, ''));
    assert.ok(r.body.every(b => b === 7));
  } finally {
    server.close();
  }
});
