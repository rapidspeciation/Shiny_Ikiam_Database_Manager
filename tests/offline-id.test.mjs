import { test } from 'node:test';
import assert from 'node:assert/strict';
import http from 'node:http';
import { existsSync, mkdirSync, mkdtempSync, readFileSync, readdirSync, renameSync, utimesSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { request } from 'node:http';
import { gunzipSync, zstdDecompressSync } from 'node:zlib';
import { createHash } from 'node:crypto';
import { serveOfflineId, MODELS, PART_BYTES, SEX, STUDENT, modelsWithoutParts, pickEncoding, precompressParts } from '../server/offline-id.mjs';

const student = MODELS.find(m => m.role === 'student');
const sex = MODELS.find(m => m.role === 'sex');
const REAL_STUDENT_SHA = STUDENT.sha256;
const REAL_SEX_SHA = SEX.sha256;

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
    // The test's small model is these bytes: its expected hash is theirs.
    if (m === student || m === sex) m.sha256 = createHash('sha256').update(b).digest('hex');
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
    // Models without an account come from Hugging Face and its CDN.
    assert.match(page.headers.get('content-security-policy'), /connect-src 'self' https:\/\/huggingface\.co https:\/\/\*\.hf\.co;/);
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
    server.closeAllConnections();
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

    // The sex model is sent plain: no parts are made for it.
    assert.equal((await modelsWithoutParts(ident)).length, MODELS.length - 1);
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
      // Each part's SHA-256 from the manifest: the page knows the same file on Hugging Face (publicModels.ts).
      assert.deepEqual(list.files[1].parts, manifest.files[enc].parts.map(x => x.sha256));
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
    utimesSync(join(ident, 'onnx', enc), new Date(), new Date(Date.now() + 5000)); // a different mtime for sure
    assert.deepEqual(await modelsWithoutParts(ident), [enc]);
    r = await raw(port, `${p}/${enc}?part=0`, { 'accept-encoding': 'zstd' });
    assert.equal(r.headers['content-encoding'], undefined);
    assert.notEqual(r.headers.etag.replace(/-p0"$/, ''), plainEtag.replace(/-p1"$/, ''));
    assert.ok(r.body.every(b => b === 7));
  } finally {
    server.close();
    server.closeAllConnections();
  }
});

test('model parts: a folder still being made is read again once its manifest appears; failed and older folders go', async () => {
  const { port, ident, server, signIn } = await setup();
  const p = '/prueba-offline/models';
  const name = 'head_6fa6ab78_fp16.bin';
  try {
    signIn();
    await precompressParts(ident, { stamp: '20261031T000000Z' });
    const second = await precompressParts(ident, { stamp: '20261031T000001Z' });
    // Only the newest folder is kept.
    assert.deepEqual(readdirSync(join(ident, 'offline')).filter(d => d.startsWith('parts-')), ['parts-20261031T000001Z']);
    // The same folder without its manifest yet (as while it is made): plain, and the memo must not keep that.
    const manifest = join(second.dir, 'manifest.json');
    renameSync(manifest, manifest + '.hold');
    let r = await raw(port, `${p}/${name}?part=0`, { 'accept-encoding': 'zstd' });
    assert.equal(r.headers['content-encoding'], undefined);
    renameSync(manifest + '.hold', manifest);
    r = await raw(port, `${p}/${name}?part=0`, { 'accept-encoding': 'zstd' });
    assert.equal(r.headers['content-encoding'], 'zstd');
    // A run that fails leaves nothing behind.
    await assert.rejects(precompressParts(ident, { stamp: '20261031T000001Z' }));
    assert.ok(existsSync(second.dir), 'an existing folder is never overwritten');
    const missing = join(ident, MODELS[0].path);
    renameSync(missing, missing + '.away');
    await assert.rejects(precompressParts(ident, { stamp: '20261031T000002Z' }));
    renameSync(missing + '.away', missing);
    assert.equal(existsSync(join(ident, 'offline', 'parts-20261031T000002Z')), false);
  } finally {
    server.close();
    server.closeAllConnections();
  }
});

test('the small model: one constant, listed with its role and hash, hidden when missing or not the expected bytes', async () => {
  assert.equal(student.name, STUDENT.name);
  assert.equal(student.path, `offline/${STUDENT.name}`);
  assert.match(REAL_STUDENT_SHA, /^[0-9a-f]{64}$/);
  assert.equal(student.optional, true);
  const { base, port, ident, server, signIn } = await setup();
  const p = '/prueba-offline/models';
  try {
    signIn();
    let list = await (await fetch(`${base}/models/list.json`)).json();
    const entry = list.files.find(f => f.name === student.name);
    assert.equal(entry.role, 'student');
    assert.equal(entry.sha256, student.sha256);
    assert.equal((await raw(port, `${p}/${student.name}?part=0`)).status, 200);

    // Other bytes than the constant says (a swap half done): not offered, not served; the rest unchanged.
    const good = student.sha256;
    student.sha256 = '0'.repeat(64);
    list = await (await fetch(`${base}/models/list.json`)).json();
    assert.deepEqual(list.files.map(f => f.role), ['detector', 'encoder', 'head-meta', 'head', 'sex']);
    assert.equal((await raw(port, `${p}/${student.name}?part=0`)).status, 404);
    assert.equal((await raw(port, `${p}/${student.name}`)).status, 404);
    student.sha256 = good;

    // Missing: the full model is still listed, parts are made for the rest, nothing waits for it.
    const file = join(ident, student.path);
    renameSync(file, file + '.away');
    list = await (await fetch(`${base}/models/list.json`)).json();
    assert.equal(list.files.length, MODELS.length - 1);
    const { manifest } = await precompressParts(ident, { stamp: '20261031T000000Z' });
    assert.equal(manifest.files[student.name], undefined);
    assert.deepEqual(await modelsWithoutParts(ident), []);
    // Put back: it needs parts now, and gets them compressed like the others.
    renameSync(file + '.away', file);
    assert.deepEqual(await modelsWithoutParts(ident), [student.name]);
    await precompressParts(ident, { stamp: '20261031T000001Z' });
    const r = await raw(port, `${p}/${student.name}?part=0`, { 'accept-encoding': 'zstd' });
    assert.equal(r.headers['content-encoding'], 'zstd');
    assert.deepEqual(zstdDecompressSync(r.body), readFileSync(file));
  } finally {
    server.close();
    server.closeAllConnections();
  }
});

test('the sex model: signed in only, pinned bytes, sent plain, never in the parts', async () => {
  assert.equal(sex.name, SEX.name);
  assert.equal(sex.path, `offline/${SEX.name}`);
  assert.match(REAL_SEX_SHA, /^[0-9a-f]{64}$/);
  assert.equal(sex.optional, true);
  const { base, port, ident, server, signIn } = await setup();
  const p = '/prueba-offline/models';
  try {
    // Signed out: neither the list nor the file nor a part.
    assert.equal((await fetch(`${base}/models/list.json`)).status, 401);
    assert.equal((await raw(port, `${p}/${sex.name}`)).status, 401);
    assert.equal((await raw(port, `${p}/${sex.name}?part=0`)).status, 401);
    assert.equal((await raw(port, `${p}/${sex.name}`, { range: 'bytes=0-9' })).status, 401);
    signIn();
    let list = await (await fetch(`${base}/models/list.json`)).json();
    const entry = list.files.find(f => f.name === sex.name);
    assert.equal(entry.role, 'sex');
    assert.equal(entry.sha256, sex.sha256);
    await precompressParts(ident, { stamp: '20261108T000000Z' });
    assert.equal(JSON.parse(readFileSync(join(ident, 'offline', 'parts-20261108T000000Z', 'manifest.json'), 'utf8')).files[sex.name], undefined);
    assert.deepEqual(await modelsWithoutParts(ident), []);
    const r = await raw(port, `${p}/${sex.name}?part=0`, { 'accept-encoding': 'zstd, gzip' });
    assert.equal(r.status, 200);
    assert.equal(r.headers['content-encoding'], undefined);
    assert.deepEqual(r.body, readFileSync(join(ident, sex.path)));
    list = await (await fetch(`${base}/models/list.json`, { headers: { 'accept-encoding': 'zstd' } })).json();
    assert.equal(list.files.find(f => f.name === sex.name).parts, undefined);
    // Other bytes than the constant: not offered, not served.
    const good = sex.sha256;
    sex.sha256 = '0'.repeat(64);
    list = await (await fetch(`${base}/models/list.json`)).json();
    assert.equal(list.files.some(f => f.role === 'sex'), false);
    assert.equal((await raw(port, `${p}/${sex.name}?part=0`)).status, 404);
    sex.sha256 = good;
  } finally {
    server.close();
    server.closeAllConnections();
  }
});
