import { createHash } from 'node:crypto';
import { createReadStream, createWriteStream } from 'node:fs';
import { mkdir, open, readdir, readFile, rename, rm, stat, writeFile } from 'node:fs/promises';
import { extname, join, resolve } from 'node:path';
import { PassThrough } from 'node:stream';
import { pipeline } from 'node:stream/promises';
import { constants as zlibConstants, createGunzip, createGzip, createZstdCompress, createZstdDecompress } from 'node:zlib';

/**
 * The offline identifier test page, /prueba-offline/ (frontend/prueba-offline/index.html, src/offline/main.ts):
 * the AI Identifier's models run in the phone's browser. Its files are sent cross-origin isolated (COOP/COEP, so
 * onnxruntime-web may use threads) and may compile WebAssembly; the rest of the app keeps its own headers.
 *
 * Its models come from the identifier's own files (IDENTIFIER_DIR, tools/identifier/setup.sh; the head as
 * tools/identifier/export_offline_head.py writes it), only to a signed-in person (BioCLIP's weights are not for
 * public download), streamed from disk with ranges and an ETag: the page downloads them in parts.
 *
 * Parts on the wire: the page asks for each 32 MB part (models/<name>?part=i). Each part is kept compressed
 * beside the models, zstd and gzip (precompressParts, tools/identifier/precompress_offline_parts.mjs, in a stamped
 * folder offline/parts-<stamp>/), and sent as it is with Content-Encoding: zstd when the request accepts it
 * (Chrome 123+), else gzip (Safari). The browser decompresses while downloading; the page stores the plain bytes.
 * A part's ETag stays the uncompressed file's (weak, the same for every encoding), so parts a phone already has
 * stay valid. Without a matching compressed part (none made, or the model changed since) the plain bytes go.
 */

export const MODELS = [
  { name: 'wing_seg_v5n.onnx', path: 'app/assets/wing_seg_v5n.onnx', role: 'detector' },
  { name: 'bioclip25h_visual_int8.onnx', path: 'onnx/bioclip25h_visual_int8.onnx', role: 'encoder' },
  { name: 'head_b8127dbf.json', path: 'offline/head_b8127dbf.json', role: 'head-meta' },
  { name: 'head_b8127dbf_fp16.bin', path: 'offline/head_b8127dbf_fp16.bin', role: 'head' },
];

const types = {
  '.html': 'text/html; charset=utf-8',
  '.js': 'text/javascript; charset=utf-8',
  '.mjs': 'text/javascript; charset=utf-8',
  '.wasm': 'application/wasm',
  '.json': 'application/json',
  '.webmanifest': 'application/manifest+json',
};

const isolated = {
  'cross-origin-opener-policy': 'same-origin',
  'cross-origin-embedder-policy': 'require-corp',
  'cross-origin-resource-policy': 'same-origin',
  'x-content-type-options': 'nosniff',
};

const etagOf = s => `"${s.size.toString(36)}-${Math.floor(s.mtimeMs).toString(36)}"`;

/** The page's part size (frontend/src/offline/main.ts PART). */
export const PART_BYTES = 32 * 1024 * 1024;
/** Content-Encoding -> the compressed part's file suffix, in the order preferred. */
export const ENCODINGS = [
  ['zstd', 'zst'],
  ['gzip', 'gz'],
];
const PARTS_DIR = /^parts-\d{8}T\d{6}Z$/;

/** The encoding to send for an Accept-Encoding header: zstd, gzip or null (plain). q=0 refuses one. */
export function pickEncoding(accept) {
  const q = {};
  for (const item of String(accept || '').toLowerCase().split(',')) {
    const [token, ...params] = item.trim().split(';');
    if (!token) continue;
    const qp = params.map(p => p.trim()).find(p => p.startsWith('q='));
    q[token] = qp ? Number(qp.slice(2)) || 0 : 1;
  }
  for (const [enc] of ENCODINGS) if ((q[enc] ?? q['*'] ?? 0) > 0) return enc;
  return null;
}

/**
 * The newest stamped parts folder's manifest entry for each model whose ETag still matches the model on disk:
 * { name: { dir, parts: [{ bytes, sha256, zst, gz }] } }. Read again when a folder is added or changed.
 */
let partsMemo = { key: '', value: {} };
async function compressedParts(identifierDir) {
  const root = join(identifierDir, 'offline');
  const dirs = (await readdir(root).catch(() => [])).filter(d => PARTS_DIR.test(d)).sort().reverse();
  const stats = await Promise.all(MODELS.map(m => stat(join(identifierDir, m.path)).catch(() => null)));
  // Each folder's manifest too: a folder being made gets its manifest last, and must be read again then.
  const manifests = await Promise.all(dirs.map(d => stat(join(root, d, 'manifest.json')).catch(() => null)));
  const key = [root, ...dirs.map((d, i) => `${d}:${manifests[i] ? etagOf(manifests[i]) : '-'}`), ...stats.map(s => (s ? etagOf(s) : '-'))].join('|');
  if (partsMemo.key === key) return partsMemo.value;
  const value = {};
  for (const d of dirs) {
    const manifest = await readFile(join(root, d, 'manifest.json'), 'utf8').then(JSON.parse).catch(() => null);
    if (!manifest || manifest.partBytes !== PART_BYTES) continue;
    MODELS.forEach((m, i) => {
      const entry = manifest.files?.[m.name];
      if (!value[m.name] && stats[i] && entry?.etag === etagOf(stats[i])) value[m.name] = { dir: join(root, d, m.name), etag: entry.etag, parts: entry.parts };
    });
  }
  partsMemo = { key, value };
  return value;
}

/** The models without compressed parts made from their current bytes (precompressParts needed when not empty). */
export async function modelsWithoutParts(identifierDir) {
  const packed = await compressedParts(identifierDir);
  return MODELS.filter(m => !packed[m.name]).map(m => m.name);
}

/** What one part costs on the wire with `encoding` (null: plain). */
const wireBytes = (part, encoding) => (encoding ? part[ENCODINGS.find(e => e[0] === encoding)[1]] : part.bytes);

class HttpError extends Error {
  constructor(status, message) {
    super(message);
    this.status = status;
  }
}

/**
 * `path`: the request's path after /prueba-offline. `webRoot`: the built page (web/); `identifierDir`: the
 * identifier's files; `user`: the signed-in person or null.
 */
export async function serveOfflineId(req, res, { path, webRoot, identifierDir, user }) {
  try {
    if (req.method !== 'GET' && req.method !== 'HEAD') throw new HttpError(405, 'Method not allowed');
    if (path === '') {
      const q = req.url.indexOf('?');
      res.writeHead(301, { location: `prueba-offline/${q >= 0 ? req.url.slice(q) : ''}` });
      return res.end();
    }
    if (path.startsWith('/models/')) {
      if (!user) throw new HttpError(401, 'Sign in first');
      const name = decodeURIComponent(path.slice('/models/'.length));
      if (!identifierDir) throw new HttpError(404, 'The identifier is not installed on this server');
      if (name === 'list.json') {
        const files = [];
        const encoding = pickEncoding(req.headers['accept-encoding']);
        const packed = await compressedParts(identifierDir);
        for (const m of MODELS) {
          const s = await stat(join(identifierDir, m.path)).catch(() => null);
          if (!s) throw new HttpError(404, `${m.name} is missing on the server`);
          const parts = packed[m.name]?.parts;
          // `wire`: the bytes the parts take on the wire for this browser's Accept-Encoding.
          const wire = parts && encoding ? parts.reduce((a, p) => a + wireBytes(p, encoding), 0) : s.size;
          files.push({ name: m.name, bytes: s.size, etag: etagOf(s), role: m.role, wire, encoding: parts ? encoding : null });
        }
        const body = JSON.stringify({ files, partBytes: PART_BYTES });
        res.writeHead(200, { ...isolated, 'content-type': 'application/json', 'cache-control': 'no-store', vary: 'accept-encoding', 'content-length': Buffer.byteLength(body) });
        return res.end(req.method === 'HEAD' ? undefined : body);
      }
      const model = MODELS.find(m => m.name === name);
      if (!model) throw new HttpError(404, 'File not found');
      const query = new URL(req.url, 'http://x').searchParams;
      if (query.has('part')) return await sendPart(req, res, identifierDir, model, query.get('part'));
      return await sendFile(req, res, join(identifierDir, model.path), { 'cache-control': 'private, no-store' });
    }
    const clean = decodeURIComponent(path).replace(/^\/+/, '');
    const root = resolve(webRoot, 'prueba-offline');
    const file = resolve(root, clean || 'index.html');
    if (!file.startsWith(root + '/')) throw new HttpError(404, 'File not found');
    const html = file.endsWith('.html');
    return await sendFile(req, res, file, {
      'content-security-policy':
        "default-src 'self'; object-src 'none'; base-uri 'self'; frame-ancestors 'none'; img-src 'self' data: blob:; connect-src 'self'; font-src 'self'; style-src 'self' 'unsafe-inline'; script-src 'self' 'wasm-unsafe-eval'; worker-src 'self' blob:",
      'cache-control': html || file.endsWith('.webmanifest') ? 'no-cache' : 'public, max-age=3600',
    });
  } catch (e) {
    const status = e.status || 500;
    if (res.headersSent) return res.destroy(e);
    const body = JSON.stringify({ error: { code: status === 401 ? 'UNAUTHENTICATED' : 'OFFLINE_ID', message: status === 500 ? 'Server error' : e.message } });
    res.writeHead(status, { ...isolated, 'content-type': 'application/json', 'cache-control': 'no-store' });
    res.end(body);
    if (status === 500) console.error('offline-id:', e);
  }
}

/**
 * Part `index` of a model (PART_BYTES of its plain bytes): its precompressed file with Content-Encoding when the
 * request accepts one and the part was made from the model on disk, else the plain range. A weak ETag tied to
 * the plain content; x-part-bytes is the plain length (what the page stores).
 */
async function sendPart(req, res, identifierDir, model, index) {
  const file = join(identifierDir, model.path);
  // Opened once: the ETag and the bytes come from the same file, even if the model is replaced meanwhile.
  const fh = await open(file, 'r').catch(() => null);
  if (!fh) throw new HttpError(404, 'File not found');
  try {
    return await sendPartOf(req, res, identifierDir, model, index, fh);
  } finally {
    await fh.close().catch(() => {});
  }
}

async function sendPartOf(req, res, identifierDir, model, index, fh) {
  const s = await fh.stat();
  if (!s.isFile()) throw new HttpError(404, 'File not found');
  const i = /^\d+$/.test(index) ? Number(index) : -1;
  const count = Math.ceil(s.size / PART_BYTES);
  if (i < 0 || i >= count) throw new HttpError(416, 'No such part');
  const start = i * PART_BYTES,
    end = Math.min(s.size, start + PART_BYTES) - 1;
  const etag = `W/${etagOf(s).slice(0, -1)}-p${i}"`;
  const headers = {
    ...isolated,
    etag,
    vary: 'accept-encoding',
    // no-transform: a proxy (Caddy's encode) must neither compress again nor undo the encoding.
    'cache-control': 'private, no-store, no-transform',
    'content-type': 'application/octet-stream',
    'x-part-bytes': String(end - start + 1),
  };
  if (req.headers['if-none-match'] && req.headers['if-none-match'].split(/\s*,\s*/).some(t => t.replace(/^W\//, '') === etag.slice(2))) {
    res.writeHead(304, headers);
    return res.end();
  }
  const encoding = pickEncoding(req.headers['accept-encoding']);
  let packed = encoding ? (await compressedParts(identifierDir))[model.name] : null;
  if (packed && packed.etag !== etagOf(s)) packed = null;
  const part = packed?.parts?.[i];
  const suffix = encoding && ENCODINGS.find(e => e[0] === encoding)[1];
  const packedFile = part && part.bytes === end - start + 1 ? join(packed.dir, `${i}.${suffix}`) : null;
  const ps = packedFile ? await stat(packedFile).catch(() => null) : null;
  if (ps && ps.size === part[suffix]) {
    res.writeHead(200, { ...headers, 'content-encoding': encoding, 'content-length': ps.size });
    if (req.method === 'HEAD') return res.end();
    return await pipeline(createReadStream(packedFile, { highWaterMark: 1 << 20 }), res).catch(() => {});
  }
  res.writeHead(200, { ...headers, 'content-length': end - start + 1 });
  if (req.method === 'HEAD') return res.end();
  await pipeline(fh.createReadStream({ start, end, highWaterMark: 1 << 20, autoClose: false }), res).catch(() => {});
}

/**
 * Makes offline/parts-<stamp>/: each model's PART_BYTES parts as <name>/<i>.zst (level 3) and <i>.gz (level 6),
 * each checked by decompressing it again, and manifest.json (written last: a folder without it is ignored).
 * Derived files only: the models are read, never changed. After a success the older parts-* folders are removed;
 * a failed run removes its own. Returns the folder and the manifest.
 */
export async function precompressParts(identifierDir, { stamp = new Date().toISOString().replace(/[-:]/g, '').replace(/\.\d+Z$/, 'Z'), log = () => {} } = {}) {
  const root = join(identifierDir, 'offline');
  const out = join(root, `parts-${stamp}`);
  await mkdir(out); // a fresh folder: fails if it exists
  let manifest;
  try {
    manifest = await compressInto(identifierDir, out, log);
    await writeFile(join(out, 'manifest.json'), JSON.stringify(manifest, null, 1));
  } catch (err) {
    await rm(out, { recursive: true, force: true }); // a folder without its manifest is only dead weight
    throw err;
  }
  // The new folder covers every model: older folders (and any left without a manifest) are not read any more.
  for (const d of await readdir(root)) if (PARTS_DIR.test(d) && d !== `parts-${stamp}`) await rm(join(root, d), { recursive: true, force: true });
  return { dir: out, manifest };
}

async function compressInto(identifierDir, out, log) {
  const manifest = { created: new Date().toISOString(), partBytes: PART_BYTES, levels: { zst: 3, gz: 6 }, files: {} };
  const sha = () => createHash('sha256');
  for (const m of MODELS) {
    const file = join(identifierDir, m.path);
    const s = await stat(file);
    await mkdir(join(out, m.name));
    const parts = [];
    for (let i = 0, start = 0; start < s.size; i++, start += PART_BYTES) {
      const end = Math.min(s.size, start + PART_BYTES) - 1;
      const plain = sha();
      const hashing = new PassThrough();
      hashing.on('data', b => plain.update(b));
      const zst = join(out, m.name, `${i}.zst`),
        gz = join(out, m.name, `${i}.gz`);
      const z = createZstdCompress({ params: { [zlibConstants.ZSTD_c_compressionLevel]: 3 } });
      const g = createGzip({ level: 6 });
      const read = createReadStream(file, { start, end, highWaterMark: 1 << 20 });
      hashing.pipe(z);
      hashing.pipe(g);
      // A read error ends every stream (pipeline), so nothing waits forever.
      const fail = err => [z, g].forEach(x => x.destroy(err));
      await Promise.all([
        pipeline(read, hashing).catch(err => (fail(err), Promise.reject(err))),
        pipeline(z, createWriteStream(zst + '.tmp')),
        pipeline(g, createWriteStream(gz + '.tmp')),
      ]);
      const want = plain.digest('hex');
      for (const [f, unpack] of [
        [zst, createZstdDecompress],
        [gz, createGunzip],
      ]) {
        const h = sha();
        await pipeline(createReadStream(f + '.tmp'), unpack(), async source => {
          for await (const b of source) h.update(b);
        });
        if (h.digest('hex') !== want) throw new Error(`${f}: does not decompress to the part`);
        await rename(f + '.tmp', f);
      }
      parts.push({ bytes: end - start + 1, sha256: want, zst: (await stat(zst)).size, gz: (await stat(gz)).size });
      log(`${m.name} part ${i}: ${end - start + 1} -> zst ${parts.at(-1).zst}, gz ${parts.at(-1).gz}`);
    }
    // The model must not have changed meanwhile.
    const after = await stat(file);
    if (etagOf(after) !== etagOf(s)) throw new Error(`${m.name} changed while it was compressed`);
    manifest.files[m.name] = { etag: etagOf(s), bytes: s.size, parts };
  }
  return manifest;
}

/** A file streamed from disk (never read whole), with an ETag, If-None-Match and one byte range (If-Range). */
async function sendFile(req, res, file, extra) {
  const s = await stat(file).catch(() => null);
  if (!s?.isFile()) throw new HttpError(404, 'File not found');
  const etag = etagOf(s);
  const headers = { ...isolated, ...extra, etag, 'last-modified': s.mtime.toUTCString(), 'accept-ranges': 'bytes', 'content-type': types[extname(file)] || 'application/octet-stream' };
  if (req.headers['if-none-match'] && req.headers['if-none-match'].split(/\s*,\s*/).includes(etag)) {
    res.writeHead(304, headers);
    return res.end();
  }
  let start = 0,
    end = s.size - 1,
    status = 200;
  const range = req.headers.range;
  const ifRange = req.headers['if-range'];
  if (range && (!ifRange || ifRange === etag)) {
    const m = /^bytes=(\d*)-(\d*)$/.exec(range.trim());
    if (m && (m[1] || m[2])) {
      if (m[1]) {
        start = Number(m[1]);
        end = m[2] ? Math.min(Number(m[2]), s.size - 1) : s.size - 1;
      } else {
        start = Math.max(0, s.size - Number(m[2]));
      }
      if (start > end || start >= s.size) {
        res.writeHead(416, { ...headers, 'content-range': `bytes */${s.size}` });
        return res.end();
      }
      status = 206;
      headers['content-range'] = `bytes ${start}-${end}/${s.size}`;
    }
  }
  headers['content-length'] = end - start + 1;
  res.writeHead(status, headers);
  if (req.method === 'HEAD') return res.end();
  // A stream: the event loop stays free and a slow phone only holds its own buffers.
  await pipeline(createReadStream(file, { start, end, highWaterMark: 1 << 20 }), res).catch(() => {});
}
