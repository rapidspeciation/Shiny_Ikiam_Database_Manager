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
 * tools/identifier/export_offline_head.py writes it), only to a signed-in person (the public copies are on Hugging Face), streamed from disk with ranges and an ETag: the page downloads them in parts.
 *
 * Parts on the wire: the page asks for each 32 MB part (models/<name>?part=i). Each part is kept compressed
 * beside the models, zstd and gzip (precompressParts, tools/identifier/precompress_offline_parts.mjs, in a stamped
 * folder offline/parts-<stamp>/), and sent as it is with Content-Encoding: zstd when the request accepts it
 * (Chrome 123+), else gzip (Safari). The browser decompresses while downloading; the page stores the plain bytes.
 * A part's ETag stays the uncompressed file's (weak, the same for every encoding), so parts a phone already has
 * stay valid. Without a matching compressed part (none made, or the model changed since) the plain bytes go.
 *
 * Without an account the page takes the same files from Hugging Face (frontend/src/lib/publicModels.ts; the CSP's
 * connect-src allows huggingface.co and its CDN, *.hf.co); this route stays for the team.
 *
 * Two models to choose from on the page: the full encoder (role encoder) and the small one (role student, STUDENT
 * below); the detector and the head serve both. The page downloads either or both.
 */

/**
 * The small model («Rápido y liviano», «modelo pequeño»): distilled from the encoder, the same 1024-D feature (so the
 * same detector, crop and head). Kept in offline/ beside the head. Which file it is, its version, SHA-256 and input
 * size come from the manifest offline/student-model.json (STUDENT_MANIFEST, checkPhoneManifest), so a new small model
 * is a data change even when its input size differs (the phone keeps the size with the file). Without a manifest,
 * model S_B (STUDENT: DINOv2-S/14 reg4 at 182 px, int8, 11 Nov 2026; WingsClassificator
 * reports/model_autoresearch_20261109/taxonomic EXPORT.md, B12 fresh-set check passed). A file whose SHA-256 is not
 * the expected one is not offered (checked once per file version), and a missing one only hides the option.
 * Rollback without a code change: a manifest naming L1 (STUDENT_L1; model-versions HANDOFF.md).
 */
export const STUDENT = {
  name: 'student_SB_int8.onnx',
  sha256: '200343ee87918fb7b8e2d2ff8433d9aa54995c9921ab5b8f942ff3ab25385762',
  version: 'small-SB-20261111',
  input: { size: 182, values: 'clip' },
  // Shown with the update in Descargas: B12, 1,156 iNaturalist photos never seen in training, species top-1.
  notes: { es: 'Mejor en fotos nuevas de iNaturalist (81,8\u00a0% vs 79,4\u00a0% de aciertos de especie).', en: 'Better on new iNaturalist photos (81.8\u00a0% vs 79.4\u00a0% of species right).' },
};
/** Model L1 (ViT-S/16, 192 px, 31 Oct 2026): the previous small model, kept in offline/ for the rollback. */
export const STUDENT_L1 = { name: 'student_L1_int8.onnx', sha256: 'd6bdc3e5f8721b6ff707b4f46625cc8d8c9af86c0df757f9b2619aed1cc2f5fb', bytes: 23565423, version: 'small-L1-20261031', input: { size: 192, values: 'clip' } };
export const STUDENT_MANIFEST = 'offline/student-model.json';

/**
 * The team's experimental sex model (Identificar, signed in only). Never public: it is sent only on this signed-in
 * route. Kept in offline/ beside the head. Which file it is, its version, SHA-256 and calibration come from the
 * manifest offline/sex-model.json (SEX_MANIFEST, checked by checkSexManifest; model-versions HANDOFF.md says what it
 * holds), so a new sex model is a data change: its .onnx and a new manifest in offline/. Without a manifest, model B
 * (SEX, SEX_B: the file and numbers shipped on 8 Nov 2026). A file whose size or SHA-256 is not the manifest's is not
 * offered, nor is anything when the manifest is invalid (logged once per version of it). Sent plain (`packed: false`):
 * fp16 weights barely compress, and adding it must not make every deploy compress the other models' parts again.
 * The phone (frontend/src/lib/offlineEngine.ts) checks the whole file against the SHA-256 listed before it replaces
 * the model it has, and keeps the calibration with it.
 */
export const SEX = { name: 'sex_v2s_ft_full_fp16.onnx', sha256: 'd6d5fb9d92af9a5b4d85a7fac3ead016e2764c5b1964a064fdaf0e98df7c412d' };
/** Model B's version and calibration (frontend/src/lib/sexModel.ts SEX_DEFAULT: the same numbers). */
export const SEX_B = {
  version: 'B 2026-11-08',
  bytes: 43409800,
  calibration: {
    cut: { t: 2.9805, lo: 1.6157, hi: 4.8722 },
    display: { a: 0.415, b: -1.0887 },
    genera: ['Dircenna', 'Forbestra', 'Godyris', 'Hyposcada', 'Hypothyris', 'Ithomia', 'Mechanitis', 'Methona', 'Napeogenes', 'Oleria', 'Pteronymia', 'Scada', 'Tithorea'],
  },
};
export const SEX_MANIFEST = 'offline/sex-model.json';
export const MANIFEST_SCHEMA = 'phone-model/1';
/** The roles a manifest in offline/ may define, and its file. */
export const MANIFESTS = { sex: SEX_MANIFEST, student: STUDENT_MANIFEST };

export const MODELS = [
  { name: 'wing_seg_s2.onnx', path: 'app/assets/wing_seg_s2.onnx', role: 'detector' },
  { name: 'bioclip25h_visual_int8.onnx', path: 'onnx/bioclip25h_visual_int8.onnx', role: 'encoder' },
  { name: 'head_6fa6ab78.json', path: 'offline/head_6fa6ab78.json', role: 'head-meta' },
  { name: 'head_6fa6ab78_fp16.bin', path: 'offline/head_6fa6ab78_fp16.bin', role: 'head' },
  { name: STUDENT.name, path: `offline/${STUDENT.name}`, role: 'student', sha256: STUDENT.sha256, optional: true, version: STUDENT.version, input: STUDENT.input, notes: STUDENT.notes },
  { name: SEX.name, path: `offline/${SEX.name}`, role: 'sex', sha256: SEX.sha256, optional: true, packed: false, version: SEX_B.version, calibration: SEX_B.calibration },
];
/** The models whose parts are precompressed. */
const PACKED = MODELS.filter(m => m.packed !== false);

const types = {
  '.html': 'text/html; charset=utf-8',
  '.js': 'text/javascript; charset=utf-8',
  '.mjs': 'text/javascript; charset=utf-8',
  '.wasm': 'application/wasm',
  '.json': 'application/json',
  '.webmanifest': 'application/manifest+json',
  // The Identificar page's field photos (identificar/field/).
  '.jpg': 'image/jpeg',
  '.jpeg': 'image/jpeg',
  '.png': 'image/png',
  '.webp': 'image/webp',
};

const isolated = {
  'cross-origin-opener-policy': 'same-origin',
  'cross-origin-embedder-policy': 'require-corp',
  'cross-origin-resource-policy': 'same-origin',
  'x-content-type-options': 'nosniff',
};

const etagOf = s => `"${s.size.toString(36)}-${Math.floor(s.mtimeMs).toString(36)}"`;

/**
 * A phone-model manifest checked: { ok: true, entry } (the MODELS-style entry the list uses) or { ok: false, why }.
 * Common to both roles:
 *   schema 'phone-model/1', role 'sex' or 'student', version (1 to 80 characters: letters, digits, space . _ : + ( ) -),
 *   file (a bare *.onnx name in offline/), bytes (> 0), sha256 (64 lowercase hex),
 *   notes (optional): { es, en }, one plain sentence or two each (at most 300 characters) shown with the download in
 *                 Descargas: what was measured about the model (cost, tests). Only measured facts.
 * role 'sex' (offline/sex-model.json):
 *   input (optional; only { size: 224, values: 'raw' }: the 224 px square as raw 0 to 255 RGB, channels first),
 *   calibration { cut: { t, lo, hi } logits with lo < t < hi, display: { a > 0, b } (P(female) = sigmoid(a z + b)),
 *                 genera: [Genus, …] }.
 * role 'student' (offline/student-model.json, «Rápido y liviano»):
 *   input { size: n, values: 'clip' } (required): the 224 px crop with the CLIP mean and std, resized to n x n
 *                 (bilinear, no antialias: frontend/src/lib/offlineId.ts studentInput); n 32 to 512. The output stays
 *                 the 1024-D feature for the identifier's head. The phone keeps n with the file.
 */
export function checkPhoneManifest(m) {
  const fail = why => ({ ok: false, why });
  if (!m || typeof m !== 'object') return fail('not a JSON object');
  if (m.schema !== MANIFEST_SCHEMA) return fail(`schema must be "${MANIFEST_SCHEMA}"`);
  if (!Object.hasOwn(MANIFESTS, m.role)) return fail('role must be "sex" or "student"');
  if (typeof m.version !== 'string' || !/^[\w .:+()-]{1,80}$/.test(m.version) || !m.version.trim()) return fail('version: 1 to 80 letters, digits, spaces or . _ : + ( ) -');
  if (typeof m.file !== 'string' || !/^[A-Za-z0-9][A-Za-z0-9._-]*\.onnx$/.test(m.file) || m.file.includes('..')) return fail('file: a bare .onnx file name (in offline/)');
  if (MODELS.some(x => x.role !== m.role && x.name === m.file)) return fail('file: the name of an identifier model');
  if (!Number.isSafeInteger(m.bytes) || m.bytes <= 0) return fail('bytes: the file size, a positive integer');
  if (typeof m.sha256 !== 'string' || !/^[0-9a-f]{64}$/.test(m.sha256)) return fail('sha256: 64 lowercase hex characters');
  const n = m.notes;
  const text = v => typeof v === 'string' && v.trim().length > 0 && v.length <= 300 && !/[<>\u0000-\u001f]/.test(v);
  if (n !== undefined && !(n && typeof n === 'object' && !Array.isArray(n) && Object.keys(n).length && Object.entries(n).every(([k, v]) => (k === 'es' || k === 'en') && text(v))))
    return fail('notes: { "es": "…", "en": "…" }, plain text of at most 300 characters each');
  const entry = {
    name: m.file,
    path: `offline/${m.file}`,
    role: m.role,
    sha256: m.sha256,
    bytes: m.bytes,
    optional: true,
    // Sent plain: the precompressed parts (precompressParts) cover the built-in files only.
    packed: false,
    version: m.version.trim(),
    ...(n ? { notes: { ...n } } : {}),
  };
  if (m.role === 'student') {
    const i = m.input;
    if (!(i && Number.isInteger(i.size) && i.size >= 32 && i.size <= 512 && i.values === 'clip' && Object.keys(i).length === 2))
      return fail('input: { "size": <32 to 512>, "values": "clip" } (the 224 px crop, CLIP mean and std, resized to size)');
    if (m.calibration !== undefined) return fail('calibration: only for the sex model');
    return { ok: true, entry: { ...entry, input: { size: i.size, values: 'clip' } } };
  }
  if (m.input !== undefined && !(m.input && m.input.size === 224 && m.input.values === 'raw' && Object.keys(m.input).length === 2))
    return fail('input: only { "size": 224, "values": "raw" } is supported (another input needs a code change)');
  const c = m.calibration, num = v => typeof v === 'number' && Number.isFinite(v);
  if (!c || typeof c !== 'object' || !c.cut || !c.display) return fail('calibration: { cut, display, genera }');
  const { t, lo, hi } = c.cut, { a, b } = c.display;
  if (![t, lo, hi].every(num) || !(lo < t && t < hi)) return fail('calibration.cut: numbers with lo < t < hi');
  if (!num(a) || !num(b) || !(a > 0)) return fail('calibration.display: numbers a > 0 and b');
  if (!Array.isArray(c.genera) || !c.genera.length || !c.genera.every(g => typeof g === 'string' && /^[A-Z][a-z]+$/.test(g))) return fail('calibration.genera: a list of genus names');
  return { ok: true, entry: { ...entry, calibration: { cut: { t, lo, hi }, display: { a, b }, genera: [...c.genera] } } };
}

/** checkPhoneManifest for the sex model's manifest only. */
export const checkSexManifest = m => (m && typeof m === 'object' && m.role !== 'sex' ? { ok: false, why: 'role must be "sex"' } : checkPhoneManifest(m));

/** Each manifest read once per version of it (ETag): { entry } from it, { entry: null, why } invalid, null without one. */
const manifestMemo = new Map();
async function fromManifest(identifierDir, role) {
  const rel = MANIFESTS[role];
  const file = join(identifierDir, rel);
  const s = await stat(file).catch(() => null);
  const key = s ? etagOf(s) : '-';
  if (manifestMemo.get(file)?.key === key) return manifestMemo.get(file).value;
  let value = null;
  if (s) {
    const json = await readFile(file, 'utf8')
      .then(JSON.parse)
      .catch(e => ({ __error: e.message }));
    const checked = json?.__error ? { ok: false, why: `not JSON (${json.__error})` } : json?.role !== role ? { ok: false, why: `role must be "${role}"` } : checkPhoneManifest(json);
    value = checked.ok ? { entry: checked.entry } : { entry: null, why: `${rel}: ${checked.why}` };
    if (!checked.ok) console.error(`offline-id: the ${role === 'sex' ? 'sex' : 'small'} model is not offered: ${value.why}`);
  }
  manifestMemo.set(file, { key, value });
  return value;
}

/**
 * The phone's models on this server: MODELS, with the sex model and the small model from their manifests when there
 * are any (left out when a manifest is invalid). `problems`: why something is not offered (a manifest), for
 * tools/identifier/check_phone_models.mjs.
 */
export async function phoneModels(identifierDir) {
  const manifests = Object.fromEntries(await Promise.all(Object.keys(MANIFESTS).map(async role => [role, await fromManifest(identifierDir, role)])));
  const models = MODELS.flatMap(m => {
    const man = manifests[m.role];
    return !man ? [m] : man.entry ? [man.entry] : [];
  });
  return { models, problems: Object.values(manifests).flatMap(v => (v?.why ? [v.why] : [])) };
}

/** A file's version when its metadata names none: its content (frontend/src/lib/offlineEngine.ts contentVersion). */
export const contentVersion = sha256 => `sha256-${sha256.slice(0, 12)}`;

/**
 * The SHA-256 of a model file, hashed once per file version (ETag); '' when it cannot be read. A model with a sha256
 * (and bytes) in its entry is offered only when its bytes are those (usable).
 */
const hashMemo = new Map();
async function usable(identifierDir, m, s) {
  if (!s?.isFile()) return false;
  if (m.bytes && s.size !== m.bytes) return false;
  if (!m.sha256) return true;
  return (await fileSha256(identifierDir, m, s)) === m.sha256;
}
async function fileSha256(identifierDir, m, s) {
  const file = join(identifierDir, m.path);
  const key = `${file}|${etagOf(s)}`;
  if (!hashMemo.has(key)) {
    const h = createHash('sha256');
    hashMemo.set(
      key,
      pipeline(createReadStream(file, { highWaterMark: 1 << 20 }), async source => {
        for await (const b of source) h.update(b);
      }).then(
        () => h.digest('hex'),
        () => (hashMemo.delete(key), ''),
      ),
    );
  }
  return hashMemo.get(key);
}

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
  const stats = await Promise.all(PACKED.map(m => stat(join(identifierDir, m.path)).catch(() => null)));
  // Each folder's manifest too: a folder being made gets its manifest last, and must be read again then.
  const manifests = await Promise.all(dirs.map(d => stat(join(root, d, 'manifest.json')).catch(() => null)));
  const key = [root, ...dirs.map((d, i) => `${d}:${manifests[i] ? etagOf(manifests[i]) : '-'}`), ...stats.map(s => (s ? etagOf(s) : '-'))].join('|');
  if (partsMemo.key === key) return partsMemo.value;
  const value = {};
  for (const d of dirs) {
    const manifest = await readFile(join(root, d, 'manifest.json'), 'utf8').then(JSON.parse).catch(() => null);
    if (!manifest || manifest.partBytes !== PART_BYTES) continue;
    PACKED.forEach((m, i) => {
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
  // An optional model (the small one) that is not on this server needs no parts.
  const present = await Promise.all(PACKED.map(m => stat(join(identifierDir, m.path)).then(() => true, () => !m.optional)));
  return PACKED.filter((m, i) => present[i] && !packed[m.name]).map(m => m.name);
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
 * `path`: the request's path after /prueba-offline (or after /identificar with `page: 'identificar'`, the app's
 * Identificar tab: the same headers, no models of its own). `webRoot`: the built page (web/); `identifierDir`: the
 * identifier's files; `user`: the signed-in person or null.
 */
export async function serveOfflineId(req, res, { page = 'prueba-offline', path, webRoot, identifierDir, user, packsDir = null }) {
  try {
    if (req.method !== 'GET' && req.method !== 'HEAD') throw new HttpError(405, 'Method not allowed');
    if (path === '') {
      const q = req.url.indexOf('?');
      res.writeHead(301, { location: `${page}/${q >= 0 ? req.url.slice(q) : ''}` });
      return res.end();
    }
    if (page === 'prueba-offline' && path.startsWith('/models/')) {
      if (!user) throw new HttpError(401, 'Sign in first');
      const name = decodeURIComponent(path.slice('/models/'.length));
      if (!identifierDir) throw new HttpError(404, 'The identifier is not installed on this server');
      if (name === 'list.json') {
        const files = [];
        const encoding = pickEncoding(req.headers['accept-encoding']);
        const packed = await compressedParts(identifierDir);
        for (const m of (await phoneModels(identifierDir)).models) {
          const s = await stat(join(identifierDir, m.path)).catch(() => null);
          if (!(await usable(identifierDir, m, s))) {
            if (m.optional) continue; // the small model or the sex model: not offered
            throw new HttpError(404, `${m.name} is missing on the server`);
          }
          const parts = packed[m.name]?.parts;
          // `wire`: the bytes the parts take on the wire for this browser's Accept-Encoding.
          const wire = parts && encoding ? parts.reduce((a, p) => a + wireBytes(p, encoding), 0) : s.size;
          // `parts`: each part's SHA-256 (the parts manifest's): the page then knows the public copy on Hugging Face
          // (frontend/src/lib/publicModels.ts, the same bytes) for the same model.
          // `version` and `sha256`: the model folder's metadata for the sex model (its manifest, or model B's), the
          // content for the others; the phone compares them with what it has (Descargas offers the update).
          const sha256 = m.sha256 ?? (await fileSha256(identifierDir, m, s));
          if (!sha256) throw new Error(`${m.name}: could not be read`);
          files.push({
            name: m.name,
            bytes: s.size,
            etag: etagOf(s),
            role: m.role,
            wire,
            encoding: parts ? encoding : null,
            version: m.version ?? contentVersion(sha256),
            sha256,
            ...(m.calibration ? { calibration: m.calibration } : {}),
            ...(m.notes ? { notes: m.notes } : {}),
            // The small model's input size (the phone resizes the crop to it): L1 192, a manifest's own.
            ...(m.role === 'student' && m.input ? { input: m.input } : {}),
            ...(parts ? { parts: parts.map(p => p.sha256) } : {}),
          });
        }
        const body = JSON.stringify({ files, partBytes: PART_BYTES });
        res.writeHead(200, { ...isolated, 'content-type': 'application/json', 'cache-control': 'no-store', vary: 'accept-encoding', 'content-length': Buffer.byteLength(body) });
        return res.end(req.method === 'HEAD' ? undefined : body);
      }
      const model = (await phoneModels(identifierDir)).models.find(m => m.name === name);
      if (!model) throw new HttpError(404, 'File not found');
      if (model.sha256 && !(await usable(identifierDir, model, await stat(join(identifierDir, model.path)).catch(() => null)))) throw new HttpError(404, 'File not found');
      const query = new URL(req.url, 'http://x').searchParams;
      if (query.has('part')) return await sendPart(req, res, identifierDir, model, query.get('part'));
      return await sendFile(req, res, join(identifierDir, model.path), { 'cache-control': 'private, no-store' });
    }
    const clean = decodeURIComponent(path).replace(/^\/+/, '');
    // The photo packs (scripts/photo-packs.py, frontend/src/lib/photoPacks.ts): from PACKS_DIR when set (a set too
    // large for Git), else beside the page (identificar/packs/).
    const inPacks = page === 'identificar' && packsDir && clean.startsWith('packs/');
    const root = inPacks ? resolve(packsDir) : resolve(webRoot, page);
    const file = resolve(root, inPacks ? clean.slice('packs/'.length) : clean || 'index.html');
    if (!file.startsWith(root + '/')) throw new HttpError(404, 'File not found');
    const html = file.endsWith('.html');
    // A pack list made with its .gz beside it (the builder writes both): sent compressed when the browser takes gzip.
    if (file.endsWith('/index.json') && clean.includes('packs/') && /\bgzip\b/.test(String(req.headers['accept-encoding'] || ''))) {
      const [plain, gz] = await Promise.all([stat(file).catch(() => null), stat(`${file}.gz`).catch(() => null)]);
      if (plain?.isFile() && gz?.isFile() && gz.mtimeMs >= plain.mtimeMs)
        return await sendFile(req, res, `${file}.gz`, { 'content-type': 'application/json', 'content-encoding': 'gzip', vary: 'accept-encoding', 'cache-control': 'no-cache' });
    }
    return await sendFile(req, res, file, {
      'content-security-policy':
        "default-src 'self'; object-src 'none'; base-uri 'self'; frame-ancestors 'none'; img-src 'self' data: blob:; connect-src 'self' https://huggingface.co https://*.hf.co; font-src 'self'; style-src 'self' 'unsafe-inline'; script-src 'self' 'wasm-unsafe-eval'; worker-src 'self' blob:",
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
  for (const m of PACKED) {
    const file = join(identifierDir, m.path);
    const s = await stat(file).catch(err => (m.optional && err.code === 'ENOENT' ? null : Promise.reject(err)));
    if (!s) continue; // the small model, not on this server
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
  const headers = { ...isolated, 'content-type': types[extname(file)] || 'application/octet-stream', ...extra, etag, 'last-modified': s.mtime.toUTCString(), 'accept-ranges': 'bytes' };
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
