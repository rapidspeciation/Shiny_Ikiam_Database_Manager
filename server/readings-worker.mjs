// The background photo readings' worker (server/readings.mjs): a child process of the app, started for one batch
// at the lowest priority (nice 19, set by the app), that reads a butterfly's photos at a time and answers what they
// show; the app writes the answers. Nothing here runs in the app's thread or in the assistant's workers.
//
//   node server/readings-worker.mjs --reader <photo-reader dir> --identifier <identifier dir> [--threads 2]
//        [--python <exe> --script <worker.py> | --python-args <json array>] [--source <dir of <fileId>.jpg>]
//        [--db <database> --batch <photos> --now <ms>]
//
// With --db (the app): it plans the batch itself (server/readings.mjs nextBatch, on its own read-only connection:
// the scan of Photo_links stays out of the app's thread), says it ({"plan": batch | null}), reads its butterflies
// one by one, answers each and ends. Without: one butterfly per line on stdin (tests, timing).
//
// Each photo: Drive's 1600 px copy (as the Wings Gallery and the app's photo cache load it; read only), or
// <source>/<fileId>.jpg (tests, the lab). Its wings: the Rename Photos app's chain (photo-reader/wing-chain.mjs with
// the reader's onnxruntime-node and sharp and its models: wingseg_v5n_640.onnx + wing_side_cls.onnx + wingside.json),
// with each wing's outline (server/wing-masks.mjs, server/wing-outlines.mjs: the Sanger backfill's code). Its species, and the butterfly's species and sex: the
// identifier's Python worker (tools/identifier/worker.py, op "specimen"), started here with 2 threads at nice 19.
//
// stdin, one JSON line per butterfly: { id, cam, side, ecuador, species, photos: [{ fileId, name, dup, wings }],
// sex: { d, v, wings?: { fileId: reading read before } } | null }. stdout, one line each, in order: { id, cam, wings: { fileId: reading | { error } },
// ident: the Python worker's answer | { error }, downloads: { fileId: error }, ms }. {"ready": true, models} first.

import { spawn } from 'node:child_process';
import { createHash } from 'node:crypto';
import { copyFileSync, existsSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { createRequire } from 'node:module';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { createInterface } from 'node:readline';
import { pathToFileURL } from 'node:url';
import { driveUrls } from './photos.mjs';
import { wingShapes } from './wing-masks.mjs';
import { nextBatch } from './readings.mjs';

const arg = (name, fallback = null) => {
  const i = process.argv.indexOf(`--${name}`);
  return i >= 0 ? process.argv[i + 1] : fallback;
};
const readerDir = arg('reader');
const identifierDir = arg('identifier');
const threads = Math.max(1, Number(arg('threads', '2')) || 2);
const source = arg('source');
const SCRIPT = new URL('../tools/identifier/worker.py', import.meta.url).pathname;
const DOWNLOAD_MS = 30_000;
/** A butterfly's longest wait for the Python worker (the first also loads the models). */
const SPECIMEN_MS = 15 * 60_000;
const work = mkdtempSync(join(tmpdir(), 'ithomiini-readings-'));
const say = o => process.stdout.write(`${JSON.stringify(o)}\n`);
const sha8 = path => createHash('sha256').update(readFileSync(path)).digest('hex').slice(0, 8);

// ------------------------------------------------------------ the wing chain
let chain = null;
async function loadChain() {
  if (!readerDir || !existsSync(join(readerDir, 'wing-chain.mjs'))) throw new Error('no wing chain (photo-reader/wing-chain.mjs: scripts/build-rename-app.sh)');
  const require = createRequire(join(readerDir, 'photo-reader.mjs'));
  const ort = require('onnxruntime-node');
  const sharp = require('sharp');
  sharp.concurrency(1);
  sharp.cache(false);
  const { analysePhoto } = await import(pathToFileURL(join(readerDir, 'wing-chain.mjs')).href);
  const models = join(readerDir, 'models');
  const cfg = JSON.parse(readFileSync(join(models, 'wingside.json'), 'utf8'));
  ort.env.logLevel = 'error';
  const options = { intraOpNumThreads: threads, interOpNumThreads: 1, graphOptimizationLevel: 'all', logSeverityLevel: 3 };
  const seg = await ort.InferenceSession.create(join(models, cfg.segmentation.file), options);
  const cls = await ort.InferenceSession.create(join(models, cfg.classifier.file), options);
  const model = `${cfg.segmentation.file.replace(/\.onnx$/, '')} ${sha8(join(models, cfg.segmentation.file))} + ${cfg.classifier.file.replace(/\.onnx$/, '')} ${sha8(join(models, cfg.classifier.file))} + wingside.json ${sha8(join(models, 'wingside.json'))}`;
  return { ort, sharp, analysePhoto, cfg, seg, cls, model };
}

/** The wings of a photo (as shown, longest side <= 1600 px): analysePhoto's answer, shown boxes, outlines. */
async function wingsOf(path) {
  chain ??= await loadChain();
  const { ort, sharp, analysePhoto, cfg, seg, cls } = chain;
  const { data, info } = await sharp(path).rotate().resize(1600, 1600, { fit: 'inside', withoutEnlargement: true }).ensureAlpha().raw().toBuffer({ resolveWithObject: true });
  const img = { width: info.width, height: info.height, data: new Uint8ClampedArray(data.buffer, data.byteOffset, data.length) };
  let last = null;
  const backend = {
    async segment(x, S) {
      const o = await seg.run({ [seg.inputNames[0]]: new ort.Tensor('float32', x, [1, 3, S, S]) });
      const [det, proto] = seg.outputNames.map(n => o[n]);
      last = { det: det.data, detDims: det.dims, proto: proto.data, protoDims: proto.dims, S };
      return { det: det.data, detDims: det.dims, proto: proto.data, protoDims: proto.dims };
    },
    async classify(x, n, size) {
      const o = await cls.run({ [cls.inputNames[0]]: new ort.Tensor('float32', x, [n, 3, size, size]) });
      return o[cls.outputNames[0]].data;
    },
  };
  const result = await analysePhoto(backend, cfg, img);
  // The last pass's masks (the photo turned upright when the chain checked it again).
  const shapes = wingShapes(last, result, cfg.segmentation);
  const [W, H] = result.size;
  return {
    side: result.side,
    p: result.p,
    fore: result.fore,
    hind: result.hind,
    missing: result.missing,
    turnK: result.turnK,
    pTurn: result.pTurn,
    size: result.checkedUpright ? [H, W] : [W, H],
    wings: result.wings.map((w, i) => ({
      box: shapes[i].box,
      kind: w.kind,
      half: w.half,
      pDorsal: Math.round(w.pDorsal * 10000) / 10000,
      pForewing: Math.round(w.pForewing * 10000) / 10000,
      ...(shapes[i].score !== null ? { score: shapes[i].score } : {}),
    })),
    outlines: shapes.map(s => s.outline),
  };
}

// ------------------------------------------------------------ the identifier (species, sex)
let python = null;
function startPython() {
  const exe = arg('python') ?? (identifierDir ? join(identifierDir, 'venv', 'bin', 'python') : null);
  const given = arg('python-args');
  const argv = given
    ? JSON.parse(given)
    : ['-I', '-B', arg('script') ?? SCRIPT, '--dir', identifierDir, '--threads', String(threads), '--nice', '19', '--idle', '300'];
  if (!exe || (!given && (!identifierDir || !existsSync(exe)))) throw new Error('the identifier is not installed (tools/identifier/setup.sh)');
  const scratch = join(work, 'py');
  const proc = spawn(exe, argv, { stdio: ['pipe', 'pipe', 'pipe'], env: { ...process.env, YOLO_CONFIG_DIR: scratch, MPLCONFIGDIR: scratch, YOLO_OFFLINE: '1' } });
  const state = { proc, waiting: new Map(), ended: false };
  proc.stderr.on('data', d => {
    const line = String(d).trim();
    if (/error|traceback|not loaded/i.test(line)) process.stderr.write(`Identifier: ${line.slice(0, 300)}\n`);
  });
  const end = why => {
    state.ended = true;
    if (python === state) python = null;
    for (const [, w] of state.waiting) w.resolve({ error: why });
    state.waiting.clear();
  };
  proc.on('error', e => end(`the identifier could not start (${e.code ?? e.message})`));
  proc.on('exit', code => end(`the identifier stopped (${code ?? 'killed'})`));
  proc.stdin.on('error', () => {});
  createInterface({ input: proc.stdout }).on('line', line => {
    let out;
    try {
      out = JSON.parse(line);
    } catch {
      return;
    }
    const w = out.id !== undefined && state.waiting.get(out.id);
    if (!w) return;
    state.waiting.delete(out.id);
    clearTimeout(w.timer);
    w.resolve(out);
  });
  return state;
}
let seq = 0;
function specimen(job) {
  python ??= startPython();
  const state = python;
  const id = String(++seq);
  return new Promise(resolve => {
    const timer = setTimeout(() => {
      state.waiting.delete(id);
      resolve({ error: `the identifier took more than ${SPECIMEN_MS / 60_000} minutes` });
      if (python === state) python = null;
      state.proc.kill('SIGKILL');
    }, SPECIMEN_MS);
    state.waiting.set(id, { resolve, timer });
    state.proc.stdin.write(`${JSON.stringify({ ...job, id, op: 'specimen' })}\n`);
  });
}

// ------------------------------------------------------------ photos
async function download(fileId) {
  const path = join(work, `${fileId.replace(/[^\w-]/g, '')}.jpg`);
  if (source) {
    copyFileSync(join(source, `${fileId}.jpg`), path);
    return path;
  }
  let why = 'no answer';
  for (const url of driveUrls(fileId, 1600)) {
    try {
      const response = await fetch(url, { redirect: 'follow', signal: AbortSignal.timeout(DOWNLOAD_MS) });
      // A file not shared by link answers with Google's sign-in page.
      if (!response.ok || !/^image\//.test(response.headers.get('content-type') ?? '')) {
        why = response.ok ? "Drive gave no resized copy (the file is not shared by link)" : `Drive answered ${response.status}`;
        continue;
      }
      writeFileSync(path, Buffer.from(await response.arrayBuffer()), { mode: 0o600 });
      return path;
    } catch (e) {
      why = String(e.message || e).slice(0, 120);
    }
  }
  throw new Error(why);
}

async function butterfly(job) {
  const started = Date.now();
  const paths = new Map();
  const downloads = {};
  for (const p of job.photos)
    try {
      paths.set(p.fileId, await download(p.fileId));
    } catch (e) {
      downloads[p.fileId] = String(e.message || e).slice(0, 200);
    }
  const wings = {};
  for (const p of job.photos) {
    if (!p.wings) continue;
    if (!paths.has(p.fileId)) {
      wings[p.fileId] = { error: `not downloaded: ${downloads[p.fileId]}` };
      continue;
    }
    try {
      wings[p.fileId] = await wingsOf(paths.get(p.fileId));
    } catch (e) {
      wings[p.fileId] = { error: String(e.message || e).slice(0, 300) };
    }
  }
  const photos = job.photos.filter(p => paths.has(p.fileId)).map(p => ({ key: p.fileId, path: paths.get(p.fileId), dup: p.dup ?? p.fileId }));
  let ident;
  try {
    // The sex model's wing boxes: the nano chain's readings of its d and v photos, read now or before (the plan's).
    const wingOf = key => (wings[key] && !wings[key].error ? wings[key] : (job.sex?.wings?.[key] ?? null));
    const sex =
      job.sex && paths.has(job.sex.d) && paths.has(job.sex.v)
        ? { cam: job.cam, d: job.sex.d, v: job.sex.v, species: job.species ?? null, wings: { d: wingOf(job.sex.d), v: wingOf(job.sex.v) } }
        : null;
    ident = photos.length ? await specimen({ photos, side: job.side ?? null, ecuador: job.ecuador ?? null, sex }) : { error: 'no photo downloaded' };
    if (job.sex && !sex && !ident.error) ident.sex = { status: 'not_scored:photo_not_downloaded' };
  } catch (e) {
    ident = { error: String(e.message || e).slice(0, 300) };
  }
  for (const path of paths.values()) rmSync(path, { force: true });
  return { id: job.id, cam: job.cam, wings, ident, downloads, ms: Date.now() - started };
}

let wingModel = null;
try {
  wingModel = existsSync(join(readerDir ?? '', 'models', 'wingside.json'))
    ? (() => {
        const models = join(readerDir, 'models');
        const cfg = JSON.parse(readFileSync(join(models, 'wingside.json'), 'utf8'));
        return `${cfg.segmentation.file.replace(/\.onnx$/, '')} ${sha8(join(models, cfg.segmentation.file))} + ${cfg.classifier.file.replace(/\.onnx$/, '')} ${sha8(join(models, cfg.classifier.file))} + wingside.json ${sha8(join(models, 'wingside.json'))}`;
      })()
    : null;
} catch {
  wingModel = null;
}
say({ ready: true, pid: process.pid, models: { wings: wingModel } });
/** A butterfly of the plan as the reading takes it. */
const jobOf = cam => ({
  id: cam.cam,
  cam: cam.cam,
  side: cam.side,
  ecuador: cam.ecuador,
  species: cam.species,
  photos: cam.photos.map(p => ({ fileId: p.fileId, name: p.name, dup: `${p.suffix}|${p.sizeKb ?? ''}`, wings: p.wings })),
  sex: cam.sex?.d ? cam.sex : null,
});
const run = async job => say(await butterfly(job).catch(e => ({ id: job.id, cam: job.cam, error: String(e.message || e).slice(0, 300) })));
if (arg('db')) {
  const { DatabaseSync } = await import('node:sqlite');
  const db = new DatabaseSync(arg('db'), { readOnly: true, timeout: 5000 });
  const plan = nextBatch(db, { size: Number(arg('batch', '40')) || 40, now: Number(arg('now')) || Date.now() });
  db.close();
  say({ plan });
  for (const cam of plan?.cams ?? []) await run(jobOf(cam));
} else
  for await (const line of createInterface({ input: process.stdin })) {
    if (!line.trim()) continue;
    let job;
    try {
      job = JSON.parse(line);
    } catch {
      continue;
    }
    await run(job);
  }
python?.proc.stdin.end();
rmSync(work, { recursive: true, force: true });
