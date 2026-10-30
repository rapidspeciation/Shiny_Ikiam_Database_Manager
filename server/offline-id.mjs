import { createReadStream } from 'node:fs';
import { stat } from 'node:fs/promises';
import { extname, join, resolve } from 'node:path';
import { pipeline } from 'node:stream/promises';

/**
 * The offline identifier test page, /prueba-offline/ (frontend/prueba-offline/index.html, src/offline/main.ts):
 * the AI Identifier's models run in the phone's browser. Its files are sent cross-origin isolated (COOP/COEP, so
 * onnxruntime-web may use threads) and may compile WebAssembly; the rest of the app keeps its own headers.
 *
 * Its models come from the identifier's own files (IDENTIFIER_DIR, tools/identifier/setup.sh; the head as
 * tools/identifier/export_offline_head.py writes it), only to a signed-in person (BioCLIP's weights are not for
 * public download), streamed from disk with ranges and an ETag: the page downloads them in parts.
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
        for (const m of MODELS) {
          const s = await stat(join(identifierDir, m.path)).catch(() => null);
          if (!s) throw new HttpError(404, `${m.name} is missing on the server`);
          files.push({ name: m.name, bytes: s.size, etag: etagOf(s), role: m.role });
        }
        const body = JSON.stringify({ files });
        res.writeHead(200, { ...isolated, 'content-type': 'application/json', 'cache-control': 'no-store', 'content-length': Buffer.byteLength(body) });
        return res.end(req.method === 'HEAD' ? undefined : body);
      }
      const model = MODELS.find(m => m.name === name);
      if (!model) throw new HttpError(404, 'File not found');
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
