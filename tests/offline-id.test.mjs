import { test } from 'node:test';
import assert from 'node:assert/strict';
import http from 'node:http';
import { existsSync, mkdirSync, mkdtempSync, readFileSync, readdirSync, renameSync, utimesSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { request } from 'node:http';
import { gunzipSync, zstdDecompressSync } from 'node:zlib';
import { createHash } from 'node:crypto';
import { serveOfflineId, MODELS, COMPAT_MODELS, PART_BYTES, SEX, SEX_B, SEX_MANIFEST, STUDENT, STUDENT_L1, STUDENT_MANIFEST, checkPhoneManifest, checkSexManifest, contentVersion, modelsWithoutParts, phoneModels, pickEncoding, precompressParts } from '../server/offline-id.mjs';

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
  // The current models, and the files only older pages get (L1 when the current small model is another).
  const compatOnly = COMPAT_MODELS.filter(c => !MODELS.some(m => m.name === c.name));
  for (const m of [...MODELS, ...compatOnly]) {
    mkdirSync(join(ident, m.path, '..'), { recursive: true });
    const n = sizes[m.name] ?? 1000;
    const b = Buffer.alloc(n);
    for (let i = 0; i < n; i++) b[i] = (i % 256) & (i % 7 ? 0xff : 0x0f);
    writeFileSync(join(ident, m.path), b);
    // The test's small model is these bytes: its expected hash is theirs.
    if (m === student || m === sex || compatOnly.includes(m)) m.sha256 = createHash('sha256').update(b).digest('hex');
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

    assert.equal((await fetch(`${base}/models/list.json?caps=versions`)).status, 401);
    assert.equal((await fetch(`${base}/models/bioclip25h_visual_int8.onnx`)).status, 401);
    signIn();
    const list = await (await fetch(`${base}/models/list.json?caps=versions`)).json();
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
    let list = JSON.parse((await raw(port, `${p}/list.json?caps=versions`, { 'accept-encoding': 'zstd' })).body);
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
      list = JSON.parse((await raw(port, `${p}/list.json?caps=versions`, { 'accept-encoding': accept })).body);
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
    let list = await (await fetch(`${base}/models/list.json?caps=versions`)).json();
    const entry = list.files.find(f => f.name === student.name);
    assert.equal(entry.role, 'student');
    assert.equal(entry.sha256, student.sha256);
    assert.equal((await raw(port, `${p}/${student.name}?part=0`)).status, 200);

    // Other bytes than the constant says (a swap half done): not offered, not served; the rest unchanged.
    const good = student.sha256;
    student.sha256 = '0'.repeat(64);
    list = await (await fetch(`${base}/models/list.json?caps=versions`)).json();
    assert.deepEqual(list.files.map(f => f.role), ['detector', 'encoder', 'head-meta', 'head', 'sex']);
    assert.equal((await raw(port, `${p}/${student.name}?part=0`)).status, 404);
    assert.equal((await raw(port, `${p}/${student.name}`)).status, 404);
    student.sha256 = good;

    // Missing: the full model is still listed, parts are made for the rest, nothing waits for it.
    const file = join(ident, student.path);
    renameSync(file, file + '.away');
    list = await (await fetch(`${base}/models/list.json?caps=versions`)).json();
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
    assert.equal((await fetch(`${base}/models/list.json?caps=versions`)).status, 401);
    assert.equal((await raw(port, `${p}/${sex.name}`)).status, 401);
    assert.equal((await raw(port, `${p}/${sex.name}?part=0`)).status, 401);
    assert.equal((await raw(port, `${p}/${sex.name}`, { range: 'bytes=0-9' })).status, 401);
    signIn();
    let list = await (await fetch(`${base}/models/list.json?caps=versions`)).json();
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
    list = await (await fetch(`${base}/models/list.json?caps=versions`, { headers: { 'accept-encoding': 'zstd' } })).json();
    assert.equal(list.files.find(f => f.name === sex.name).parts, undefined);
    // Other bytes than the constant: not offered, not served.
    const good = sex.sha256;
    sex.sha256 = '0'.repeat(64);
    list = await (await fetch(`${base}/models/list.json?caps=versions`)).json();
    assert.equal(list.files.some(f => f.role === 'sex'), false);
    assert.equal((await raw(port, `${p}/${sex.name}?part=0`)).status, 404);
    sex.sha256 = good;
  } finally {
    server.close();
    server.closeAllConnections();
  }
});

// ---------------------------------------------------------------- versions (model-versions, 11 Nov 2026)

const sha256Of = b => createHash('sha256').update(b).digest('hex');
/** A manifest for a new sex model file in offline/ (model C in the HANDOFF), valid unless changed. */
function cManifest(bytes, extra = {}) {
  return {
    schema: 'phone-model/1',
    role: 'sex',
    version: 'C 2026-11-20',
    file: 'sex_c_test.onnx',
    bytes: bytes.length,
    sha256: sha256Of(bytes),
    input: { size: 224, values: 'raw' },
    calibration: { cut: { t: 0.5, lo: -0.5, hi: 1.5 }, display: { a: 1, b: 0 }, genera: ['Mechanitis', 'Oleria'] },
    ...extra,
  };
}

test('every listed file carries a version and its SHA-256: the content for the identifier, S_B and B for the small and sex models', async () => {
  const { base, ident, server, signIn } = await setup();
  try {
    signIn();
    const list = await (await fetch(`${base}/models/list.json?caps=versions`)).json();
    for (const f of list.files) {
      const bytes = readFileSync(join(ident, MODELS.find(m => m.name === f.name).path));
      assert.equal(f.sha256, sha256Of(bytes), f.name);
      assert.equal(f.version, f.role === 'sex' ? SEX_B.version : f.role === 'student' ? 'small-SB-20261111' : contentVersion(f.sha256), f.name);
      assert.deepEqual(f.input, f.role === 'student' ? { size: 182, values: 'clip' } : undefined, f.name);
      // The small model's release note travels with it (Descargas shows it with the update).
      if (f.role === 'student') assert.match(f.notes.es, /81,8\u00a0% frente al 79,4\u00a0%/);
    }
    // Without a manifest the sex model is model B, with its calibration (the phone keeps it with the file).
    const b = list.files.find(f => f.role === 'sex');
    assert.equal(b.name, SEX.name);
    assert.deepEqual(b.calibration, SEX_B.calibration);
    assert.equal(list.files.filter(f => f.role !== 'sex').some(f => 'calibration' in f), false);
  } finally {
    server.close();
    server.closeAllConnections();
  }
});

test('a new sex model is a data change: its file and offline/sex-model.json; size or hash wrong, or a bad manifest, offers none', async () => {
  const { base, port, ident, server, signIn } = await setup();
  const p = '/prueba-offline/models';
  try {
    signIn();
    const c = Buffer.from(Array.from({ length: 3000 }, (_, i) => (i * 31) % 251));
    writeFileSync(join(ident, 'offline', 'sex_c_test.onnx'), c);
    const put = m => {
      writeFileSync(join(ident, SEX_MANIFEST), typeof m === 'string' ? m : JSON.stringify(m));
      // A new mtime each time (the manifest is read again when its ETag changes).
      const t = new Date(Date.now() + Math.floor(Math.random() * 1e6));
      utimesSync(join(ident, SEX_MANIFEST), t, t);
    };
    put(cManifest(c));
    let list = await (await fetch(`${base}/models/list.json?caps=versions`)).json();
    const sexes = list.files.filter(f => f.role === 'sex');
    assert.equal(sexes.length, 1, 'only the manifest\'s model: model B is not listed beside it');
    assert.deepEqual(
      { name: sexes[0].name, bytes: sexes[0].bytes, version: sexes[0].version, sha256: sexes[0].sha256 },
      { name: 'sex_c_test.onnx', bytes: 3000, version: 'C 2026-11-20', sha256: sha256Of(c) },
    );
    assert.deepEqual(sexes[0].calibration, cManifest(c).calibration);
    // Served, in parts, plain. Model B's file is no longer in the versioned list, but still served: pages from before
    // model versions get B (COMPAT_MODELS).
    const r = await raw(port, `${p}/sex_c_test.onnx?part=0`, { 'accept-encoding': 'zstd' });
    assert.equal(r.status, 200);
    assert.deepEqual(r.body, c);
    assert.equal(list.files.some(f => f.name === SEX.name), false);
    assert.equal((await raw(port, `${p}/${SEX.name}?part=0`)).status, 200);

    // The file is not the manifest's (another size, then other bytes of the same size): not offered, not served.
    put(cManifest(c, { bytes: 2999 }));
    list = await (await fetch(`${base}/models/list.json?caps=versions`)).json();
    assert.equal(list.files.some(f => f.role === 'sex'), false);
    put(cManifest(c, { sha256: '1'.repeat(64) }));
    list = await (await fetch(`${base}/models/list.json?caps=versions`)).json();
    assert.equal(list.files.some(f => f.role === 'sex'), false);
    assert.equal((await raw(port, `${p}/sex_c_test.onnx?part=0`)).status, 404);
    // An invalid manifest: nothing offered (never model B silently instead), the identifier still listed.
    const errors = [];
    const orig = console.error;
    console.error = (...a) => errors.push(a.join(' '));
    try {
      put('{ not json');
      list = await (await fetch(`${base}/models/list.json?caps=versions`)).json();
      assert.equal(list.files.some(f => f.role === 'sex'), false);
      assert.equal(list.files.length, MODELS.length - 1);
      assert.match(errors.join('\n'), /sex model is not offered: offline\/sex-model\.json: not JSON/);
      assert.deepEqual((await phoneModels(ident)).problems.length, 1);
    } finally {
      console.error = orig;
    }
  } finally {
    server.close();
    server.closeAllConnections();
  }
});

test('checkSexManifest: what the field-sex worker must supply (HANDOFF.md), and nothing that escapes offline/', () => {
  const c = Buffer.from('model C');
  const ok = checkSexManifest(cManifest(c));
  assert.equal(ok.ok, true);
  assert.equal(ok.entry.path, 'offline/sex_c_test.onnx');
  assert.equal(ok.entry.packed, false);
  assert.equal(checkSexManifest({ ...cManifest(c), input: undefined }).ok, true); // input defaults to 224 raw
  const bad = [
    [{ schema: 'phone-model/2' }, /schema/],
    [{ role: 'detector' }, /role/],
    [{ version: '' }, /version/],
    [{ version: '<script>' }, /version/],
    [{ version: 'x'.repeat(81) }, /version/],
    [{ file: '../app/x.onnx' }, /file/],
    [{ file: 'sub/x.onnx' }, /file/],
    [{ file: '.hidden.onnx' }, /file/],
    [{ file: 'x.bin' }, /file/],
    [{ file: 'bioclip25h_visual_int8.onnx' }, /identifier model/],
    [{ bytes: 0 }, /bytes/],
    [{ bytes: '3000' }, /bytes/],
    [{ sha256: 'ABC' }, /sha256/],
    [{ input: { size: 256, values: 'raw' } }, /input/],
    [{ input: { size: 224, values: 'imagenet' } }, /input/],
    [{ calibration: null }, /calibration/],
    [{ calibration: { cut: { t: 1, lo: 2, hi: 3 }, display: { a: 1, b: 0 }, genera: ['Oleria'] } }, /cut/],
    [{ calibration: { cut: { t: 1, lo: 0, hi: 3 }, display: { a: -1, b: 0 }, genera: ['Oleria'] } }, /display/],
    [{ calibration: { cut: { t: 1, lo: 0, hi: 3 }, display: { a: 1, b: 0 }, genera: [] } }, /genera/],
    [{ calibration: { cut: { t: 1, lo: 0, hi: 3 }, display: { a: 1, b: 0 }, genera: ['oleria'] } }, /genera/],
  ];
  for (const [change, why] of bad) {
    const r = checkSexManifest({ ...cManifest(c), ...change });
    assert.equal(r.ok, false, JSON.stringify(change));
    assert.match(r.why, why, JSON.stringify(change));
  }
  assert.equal(checkSexManifest(null).ok, false);
});

test('model C\'s manifest as delivered (field-sex-C-20261111) is valid; notes are optional plain text', () => {
  const c = {
    schema: 'phone-model/1',
    role: 'sex',
    version: 'field-sex-C-20261111',
    file: 'sex_c_dinov2b_sam_flip_fp16.onnx',
    bytes: 171606829,
    sha256: 'a9784a1c6b9988af638ca20cff2f41cb83951a38a00c1eafa7138efd69b53a3d',
    input: { size: 224, values: 'raw' },
    calibration: { cut: { t: 0.9918, lo: -0.1859, hi: 1.1956 }, display: { a: 1.0038, b: -0.5417 }, genera: SEX_B.calibration.genera },
  };
  const r = checkSexManifest(c);
  assert.equal(r.ok, true);
  assert.equal(r.entry.version, 'field-sex-C-20261111');
  assert.equal(r.entry.notes, undefined);
  const notes = { es: 'Más lento que el modelo B: 0,92 s por foto en una computadora portátil.', en: 'Slower than model B: 0.92 s per photo on a laptop.' };
  assert.deepEqual(checkSexManifest({ ...c, notes }).entry.notes, notes);
  assert.deepEqual(checkSexManifest({ ...c, notes: { es: notes.es } }).entry.notes, { es: notes.es });
  for (const bad of [{}, { fr: 'x' }, { es: '' }, { es: 'x'.repeat(301) }, { es: '<b>x</b>' }, { es: 'a\nb' }, ['x'], 'x'])
    assert.match(checkSexManifest({ ...c, notes: bad }).why ?? '', /notes/, JSON.stringify(bad));
});

test('the notes reach the phone\'s list with the sex model', async () => {
  const { base, ident, server, signIn } = await setup();
  try {
    signIn();
    const c = Buffer.from('a model with notes');
    writeFileSync(join(ident, 'offline', 'sex_c_test.onnx'), c);
    writeFileSync(join(ident, SEX_MANIFEST), JSON.stringify(cManifest(c, { notes: { es: 'Más lento.', en: 'Slower.' } })));
    const list = await (await fetch(`${base}/models/list.json?caps=versions`)).json();
    assert.deepEqual(list.files.find(f => f.role === 'sex').notes, { es: 'Más lento.', en: 'Slower.' });
  } finally {
    server.close();
    server.closeAllConnections();
  }
});

// ---------------------------------------------------------------- the small model by manifest (another input size)

const newManifest = (bytes, extra = {}) => ({
  schema: 'phone-model/1',
  role: 'student',
  version: 'small-new-test',
  file: 'student_new_test.onnx',
  bytes: bytes.length,
  sha256: sha256Of(bytes),
  input: { size: 182, values: 'clip' },
  notes: { es: 'Mejor en pruebas de desarrollo.' },
  ...extra,
});

test('a new small model is a data change: its file and offline/student-model.json, with its input size', async () => {
  const { base, port, ident, server, signIn } = await setup();
  const p = '/prueba-offline/models';
  try {
    signIn();
    const nb = Buffer.from(Array.from({ length: 2500 }, (_, i) => (i * 13) % 253));
    writeFileSync(join(ident, 'offline', 'student_new_test.onnx'), nb);
    writeFileSync(join(ident, STUDENT_MANIFEST), JSON.stringify(newManifest(nb)));
    const list = await (await fetch(`${base}/models/list.json?caps=versions`)).json();
    const students = list.files.filter(f => f.role === 'student');
    assert.equal(students.length, 1);
    assert.deepEqual(
      { name: students[0].name, bytes: students[0].bytes, version: students[0].version, sha256: students[0].sha256, input: students[0].input, notes: students[0].notes },
      { name: 'student_new_test.onnx', bytes: 2500, version: 'small-new-test', sha256: sha256Of(nb), input: { size: 182, values: 'clip' }, notes: { es: 'Mejor en pruebas de desarrollo.' } },
    );
    // The sex model and the rest are as before (no sex manifest: model B).
    assert.equal(list.files.find(f => f.role === 'sex').version, SEX_B.version);
    const r = await raw(port, `${p}/student_new_test.onnx?part=0`, { 'accept-encoding': 'zstd' });
    assert.equal(r.status, 200);
    assert.equal(r.headers['content-encoding'], undefined); // plain: no precompressed parts for a manifest's file
    assert.deepEqual(r.body, nb);
    // The built-in one is not served beside it (unless older pages still get it: L1 when it is the built-in).
    if (!COMPAT_MODELS.some(c => c.name === STUDENT.name)) assert.equal((await raw(port, `${p}/${STUDENT.name}?part=0`)).status, 404);
    // Not the manifest's bytes: not offered (the full model stays).
    writeFileSync(join(ident, STUDENT_MANIFEST), JSON.stringify(newManifest(nb, { sha256: '2'.repeat(64) })));
    const t = new Date(Date.now() + 5000);
    utimesSync(join(ident, STUDENT_MANIFEST), t, t);
    const again = await (await fetch(`${base}/models/list.json?caps=versions`)).json();
    assert.equal(again.files.some(f => f.role === 'student'), false);
    assert.equal(again.files.some(f => f.role === 'encoder'), true);
  } finally {
    server.close();
    server.closeAllConnections();
  }
});

test('checkPhoneManifest: a small model needs its input size; a sex manifest under the student name is refused', () => {
  const nb = Buffer.from('a small model');
  const ok = checkPhoneManifest(newManifest(nb));
  assert.equal(ok.ok, true);
  assert.deepEqual(ok.entry.input, { size: 182, values: 'clip' });
  assert.equal(ok.entry.packed, false);
  for (const [change, why] of [
    [{ input: undefined }, /input/],
    [{ input: { size: 182 } }, /input/],
    [{ input: { size: 182, values: 'raw' } }, /input/],
    [{ input: { size: 1000, values: 'clip' } }, /input/],
    [{ input: { size: 182.5, values: 'clip' } }, /input/],
    [{ calibration: SEX_B.calibration }, /calibration/],
    [{ role: 'encoder' }, /role/],
    [{ file: 'bioclip25h_visual_int8.onnx' }, /identifier model/],
  ]) {
    const r = checkPhoneManifest({ ...newManifest(nb), ...change });
    assert.equal(r.ok, false, JSON.stringify(change));
    assert.match(r.why, why, JSON.stringify(change));
  }
  // The student's own default L1 name is allowed for a manifest (the same file, a named version).
  assert.equal(checkPhoneManifest({ ...newManifest(nb), file: STUDENT.name }).ok, true);
  assert.equal(checkSexManifest(newManifest(nb)).ok, false);
});

test('a manifest whose role is not its file\'s is not used', async () => {
  const { base, ident, server, signIn } = await setup();
  const errors = [];
  const orig = console.error;
  console.error = (...a) => errors.push(a.join(' '));
  try {
    signIn();
    const nb = Buffer.from('a small model');
    writeFileSync(join(ident, 'offline', 'student_new_test.onnx'), nb);
    writeFileSync(join(ident, SEX_MANIFEST), JSON.stringify(newManifest(nb)));
    const list = await (await fetch(`${base}/models/list.json?caps=versions`)).json();
    assert.equal(list.files.some(f => f.role === 'sex'), false);
    assert.equal(list.files.find(f => f.role === 'student').name, STUDENT.name);
    assert.match(errors.join('\n'), /sex-model\.json: role must be "sex"/);
  } finally {
    console.error = orig;
    server.close();
    server.closeAllConnections();
  }
});

// ---------------------------------------------------------------- pages from before model versions (production 6366223e)

test('a page from before model versions gets L1 and B only; the versioned list (?caps=versions) offers S_B and a manifest\'s C', async () => {
  const { port, ident, server, signIn } = await setup();
  const p = '/prueba-offline/models';
  try {
    signIn();
    // A sex model C by manifest, as on the server after the drop.
    const c = Buffer.from(Array.from({ length: 1500 }, (_, i) => (i * 7) % 251));
    writeFileSync(join(ident, 'offline', 'sex_c_test.onnx'), c);
    writeFileSync(join(ident, SEX_MANIFEST), JSON.stringify(cManifest(c)));
    // The request of the 6366223e client (frontend/src/lib/offlineEngine.ts list(): models/list.json, no query; the
    // browser's usual Accept-Encoding), and the parts it then asks for (models/<name>?part=i).
    const old = JSON.parse((await raw(port, `${p}/list.json`, { 'accept-encoding': 'gzip, deflate, br, zstd' })).body);
    const small = old.files.find(f => f.role === 'student'), sexOld = old.files.find(f => f.role === 'sex');
    assert.equal(small.name, STUDENT_L1.name); // the 192 px model that client feeds
    assert.equal(sexOld.name, SEX.name); // the only sex model that client takes (by B's name)
    assert.equal(old.files.some(f => f.name === STUDENT.name && STUDENT.name !== STUDENT_L1.name), false);
    assert.equal(old.files.some(f => f.name === 'sex_c_test.onnx'), false);
    // That client's downloads still work: each part of L1 and B, with the part ETag it checks (W/"<file etag>-p<i>").
    for (const f of [small, sexOld]) {
      const r = await raw(port, `${p}/${f.name}?part=0`, { 'accept-encoding': 'gzip, deflate, br, zstd' });
      assert.equal(r.status, 200, f.name);
      assert.equal(r.headers.etag, `W/${f.etag.slice(0, -1)}-p0"`);
      assert.equal(r.headers['x-part-bytes'], String(f.bytes));
    }
    // The current client asks for the versioned list: the current small model and the manifest's sex model.
    const now = JSON.parse((await raw(port, `${p}/list.json?caps=versions`)).body);
    assert.equal(now.files.find(f => f.role === 'student').name, STUDENT.name);
    assert.equal(now.files.find(f => f.role === 'sex').name, 'sex_c_test.onnx');
    // Other capability names alone do not open it.
    const other = JSON.parse((await raw(port, `${p}/list.json?caps=other`)).body);
    assert.equal(other.files.find(f => f.role === 'sex').name, SEX.name);
  } finally {
    server.close();
    server.closeAllConnections();
  }
});
