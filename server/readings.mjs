// The background photo readings: every photo Photo_links lists under a CAM/M name gets the readings the 18 Oct
// 2026 backfill gave the others (/tmp/photo-readings-schema.md): its wings (the Rename Photos app's nano chain,
// with each wing's outline), its species alone, and per butterfly the species of its photos together and its sex
// (the identifier and the collection sex model). Then the Wings Gallery's files are made again
// (server/gallery-public.mjs) and the gallery's update is started (its second: the first comes with the photos).
//
// Checked after each sync that changed Photo_links, when the app starts, and on demand (POST /api/readings/run).
// The photos not read yet are put in batches (a few dozen photos; a butterfly's photos together, because its
// species is the mean of all of them); one batch at a time, read by a child process at the lowest priority
// (server/readings-worker.mjs, nice 19, 2 threads), so the app and the team's chats are not slowed. Each
// butterfly's answer is written as it comes (photo_reading_files says which photos were read, with what), so a
// restart goes on where it was. How it goes: /health `readings`, GET /api/readings.

import { spawn } from 'node:child_process';
import { randomUUID } from 'node:crypto';
import { existsSync } from 'node:fs';
import { setPriority } from 'node:os';
import { join } from 'node:path';
import { createInterface } from 'node:readline';
import { upsertReadings } from './envelope-import.mjs';
import { galleryData, writeGalleryFiles } from './gallery-public.mjs';
import { bumpReviewRevision, driveId, initPhotoTables, reviewRevision } from './photodata.mjs';
import { nameParts, readingOfRow, wingRows } from './wing-reasons.mjs';
import { trainingCams } from './oof.mjs';

const WORKER = new URL('./readings-worker.mjs', import.meta.url).pathname;
const BUILDER = new URL('./gallery-public.mjs', import.meta.url).pathname;
export const SOURCE = 'app_readings';
const ID_MODEL = 'bioclip-2.5-h int8 + head_hier 749fbffd + wing_seg_v5n d74f9c13 (tools/identifier)';
// The sex model's name: the arm the worker says it ran (the 19 Oct 2026 CNN on the androconial strips; its name
// before the colon) and how many of the 4 strips the butterfly had.
export const sexModelName = sex =>
  `collection sex ${String(sex?.model ?? 'release_20261019 cnn_strips4').split(':')[0]} (${Number.isFinite(sex?.crops) ? `${sex.crops} of 4 ` : ''}androconial strips of the nano wing boxes, MobileNetV3, cand_cv fold models)`;
/** New photos a batch reads (with the other photos of their butterflies). */
const BATCH_PHOTOS = 40;
/** A photo that could not be read is tried again after this long, three times at most. */
const RETRY_MS = 6 * 60 * 60_000;
const ATTEMPTS = 3;
/** The gallery's update waits this long after a batch for the next one, and at most MAX_WAIT_MS in all. */
const DEBOUNCE_MS = 10 * 60_000;
const MAX_WAIT_MS = 60 * 60_000;
/** Started again later when the gallery's update was already running (it may have read the files before). */
const BUSY_RETRY_MS = 5 * 60_000;
const PHOTO = /\.(jpe?g|heic|heif|jfif)$/i;
const TO_DELETE = /_todelete$/i;

const text = v => String(v ?? '').trim();
const parse = t => {
  try {
    return t ? JSON.parse(t) : null;
  } catch {
    return null;
  }
};
const binomial = s => {
  const t = text(s);
  return t && t !== 'NA' ? t.split(/\s+/).slice(0, 2).join(' ') : null;
};

export function initReadingTables(db) {
  db.exec(`CREATE TABLE IF NOT EXISTS photo_reading_files(
      file_id TEXT PRIMARY KEY, name TEXT, cam TEXT, stamp TEXT, status TEXT NOT NULL, attempts INTEGER NOT NULL DEFAULT 0,
      error TEXT, batch_id TEXT, read_at TEXT);
    CREATE TABLE IF NOT EXISTS photo_reading_batches(
      id TEXT PRIMARY KEY, started_at TEXT NOT NULL, ended_at TEXT, cams INTEGER, photos INTEGER, done INTEGER DEFAULT 0,
      readings INTEGER DEFAULT 0, errors INTEGER DEFAULT 0, state TEXT NOT NULL, error TEXT);`);
}

/**
 * The photos Photo_links lists under a butterfly's name (jpg, jpeg, heic, jfif; not …_todelete), one per Drive
 * file: [{ fileId, name (stem), cam, suffix, folder, sizeKb, stamp }]. `stamp`: the name and size, so a file
 * renamed or replaced (turned) since its reading is read again.
 */
export function listedPhotos(db) {
  const out = new Map();
  const rows = db
    .prepare(
      `SELECT json_extract(values_json, '$.Name') AS name, json_extract(values_json, '$.URL') AS url, json_extract(values_json, '$."Full Path"') AS path,
        json_extract(values_json, '$."Size KB"') AS size FROM records WHERE sheet='Photo_links' AND missing=0 AND observed=1 AND row_num>1 ORDER BY row_num`,
    )
    .all();
  for (const r of rows) {
    const file = text(r.name);
    if (!PHOTO.test(file)) continue;
    const stem = file.replace(/\.[^.]+$/, '');
    if (TO_DELETE.test(stem)) continue;
    const parts = nameParts(stem);
    const fileId = driveId(r.url);
    if (!parts || !fileId || out.has(fileId)) continue;
    const path = text(r.path);
    const folder = path.endsWith(file) ? path.slice(0, -file.length).replace(/\/+$/, '') : path.replace(/\/[^/]*$/, '');
    const sizeKb = Number(r.size) || null;
    out.set(fileId, { fileId, name: stem, cam: parts.cam, suffix: parts.suffix, folder: folder || null, sizeKb, stamp: `${stem}|${sizeKb ?? ''}` });
  }
  return [...out.values()];
}

/** The workbook's species, side of the Andes and country of some butterflies (Collection_data last: it wins). */
function specimenFacts(db, cams) {
  const wanted = new Set(cams);
  const out = new Map();
  const read = (sheet, fields) => {
    const sql = `SELECT ${fields.map((f, i) => `json_extract(values_json, '$."${f}"') AS c${i}`).join(', ')} FROM records WHERE sheet=? AND missing=0 AND observed=1`;
    for (const r of db.prepare(sql).all(sheet)) {
      const cam = text(r.c0).toUpperCase();
      if (!wanted.has(cam)) continue;
      const m = out.get(cam) ?? out.set(cam, {}).get(cam);
      const sp = binomial(r.c1);
      if (sp) m.species = sp;
      if (fields.length > 2) {
        m.side = r.c2 === 'East' || r.c2 === 'West' ? r.c2 : null;
        m.country = text(r.c3) || null;
      }
    }
  };
  read('CRISPR', ['CAM_ID', 'Species']);
  read('Insectary_data', ['CAM_ID', 'SPECIES']);
  read('Collection_data', ['CAM_ID', 'SPECIES', 'Side_Andes', 'Country']);
  return out;
}

/** The photos to read: listed, and not read with their current name and size (or failed, tried again later). */
export function pendingPhotos(db, { now = Date.now(), listed = null } = {}) {
  initPhotoTables(db);
  initReadingTables(db);
  listed ??= listedPhotos(db);
  const log = new Map(db.prepare('SELECT file_id, stamp, status, attempts, read_at FROM photo_reading_files').all().map(r => [r.file_id, r]));
  // Read by the backfill (its import): the wings with the nano model, and the identifier on the photo alone.
  const imported = new Map(db.prepare('SELECT file_id, name FROM photo_wing_sides WHERE file_id IS NOT NULL AND (wings_json IS NOT NULL OR error IS NOT NULL)').all().map(r => [r.file_id, r.name]));
  const predicted = new Set(db.prepare('SELECT file_id FROM photo_file_predictions').all().map(r => r.file_id));
  return listed.filter(p => {
    const l = log.get(p.fileId);
    if (l) {
      if (l.stamp !== p.stamp) return true;
      if (l.status === 'ok') return false;
      return l.attempts < ATTEMPTS && now - Date.parse(l.read_at ?? 0) > RETRY_MS;
    }
    return !(imported.get(p.fileId) === p.name && predicted.has(p.fileId));
  });
}

/**
 * The next batch: butterflies (in name order) with photos to read, until about `size` photos; each with all its
 * listed photos (`wings`: the ones to read the wings of) and what the workbook says of it.
 */
export function nextBatch(db, { size = BATCH_PHOTOS, now = Date.now() } = {}) {
  const listed = listedPhotos(db);
  const pending = pendingPhotos(db, { now, listed });
  if (!pending.length) return null;
  const byCam = new Map();
  for (const p of listed) (byCam.get(p.cam) ?? byCam.set(p.cam, []).get(p.cam)).push(p);
  const todo = new Set(pending.map(p => p.fileId));
  const read = new Set(db.prepare('SELECT file_id FROM photo_wing_sides WHERE file_id IS NOT NULL AND wings_json IS NOT NULL').all().map(r => r.file_id));
  const cams = [...new Set(pending.map(p => p.cam))].sort();
  const chosen = [];
  let count = 0;
  for (const cam of cams) {
    if (chosen.length && count >= size) break;
    const photos = byCam.get(cam).map(p => ({ ...p, wings: todo.has(p.fileId) || !read.has(p.fileId) }));
    count += photos.filter(p => todo.has(p.fileId)).length;
    chosen.push({ cam, photos, pending: photos.filter(p => todo.has(p.fileId)).map(p => p.fileId) });
  }
  const facts = specimenFacts(db, chosen.map(c => c.cam));
  const wingRowOf = db.prepare('SELECT * FROM photo_wing_sides WHERE file_id = ? AND wings_json IS NOT NULL ORDER BY imported_at DESC LIMIT 1');
  const wingRow = { get: fileId => wingRowOf.get(fileId) };
  for (const c of chosen) {
    const f = facts.get(c.cam) ?? {};
    c.side = f.side ?? null;
    c.ecuador = f.country ? /^ecuador$/i.test(f.country) : null;
    c.species = f.species ?? null;
    // The sex model's pair: one d and one v photo (the largest file, then the folder's name, as the backfill).
    const pick = v =>
      c.photos
        .filter(p => p.suffix === v)
        .sort((a, b) => (b.sizeKb ?? 0) - (a.sizeKb ?? 0) || String(a.folder ?? '').localeCompare(String(b.folder ?? '')) || a.fileId.localeCompare(b.fileId))[0];
    const d = pick('d');
    const v = pick('v');
    c.sex = d && v ? { d: d.fileId, v: v.fileId } : { missing: !d ? 'no_d_photo' : 'no_v_photo' };
    // The sex model's wing boxes come from the nano chain's readings of that pair (19 Oct 2026 CNN model): those
    // read before go with the butterfly; the worker reads the others now.
    if (d && v) {
      const stored = {};
      for (const p of [d, v]) {
        if (todo.has(p.fileId) || !read.has(p.fileId)) continue;
        const r = readingOfRow(wingRow.get(p.fileId));
        if (r) stored[p.fileId] = { wings: r.wings, outlines: r.outlines, turnK: r.turnK, size: r.size };
      }
      if (Object.keys(stored).length) c.sex.wings = stored;
    }
  }
  return { cams: chosen, photos: count, pending: pending.length };
}

/** The rows a butterfly's answer gives (importBundle's shapes), with the photos read before taken from the table. */
export function rowsOfAnswer(db, cam, answer, { wingModel = null, now = new Date().toISOString() } = {}) {
  const stored = new Map(db.prepare('SELECT * FROM photo_wing_sides WHERE cam = ?').all(cam.cam).map(r => [r.file_id, r]));
  const photos = cam.photos.map(p => {
    const fresh = answer.wings?.[p.fileId];
    if (fresh) return { name: p.name, fileId: p.fileId, folder: p.folder, ...(fresh.error ? { error: fresh.error } : { reading: fresh }) };
    const before = readingOfRow(stored.get(p.fileId));
    return { name: p.name, fileId: p.fileId, folder: p.folder, ...(before ? { reading: before } : { error: stored.get(p.fileId)?.error ?? 'not read' }) };
  });
  const model = wingModel ?? [...stored.values()].find(r => r.model)?.model ?? null;
  const { rows, boxes } = wingRows(cam.cam, photos, { source: SOURCE, model });
  // Rows of photos read before keep where they came from.
  for (const r of rows) {
    const s = stored.get(r.fileId);
    if (!answer.wings?.[r.fileId] && s) Object.assign(r, { source: s.source, model: s.model ?? r.model });
  }
  const ident = answer.ident && !answer.ident.error ? answer.ident : null;
  // A butterfly the identifier's head was trained on: its answer would be in-sample, so its species stay those it
  // has (out-of-fold, from the import; server/oof.mjs) and only its wings and sex are new.
  const inTraining = trainingCams().has(String(cam.cam).toUpperCase());
  const viewOf = p => (p.suffix[0] === 'd' || p.suffix[0] === 'v' ? p.suffix[0] : null);
  const photoPredictions = [];
  if (ident && !inTraining)
    for (const p of cam.photos) {
      const r = ident.photos?.[p.fileId];
      if (!r || r.error) continue;
      photoPredictions.push({
        fileId: p.fileId,
        name: p.name,
        cam: cam.cam,
        view: viewOf(p),
        species: r.species,
        genus: r.genus,
        subspecies: r.subspecies,
        andes: cam.side,
        cropStatus: r.crop ?? null,
        cropBox: r.wingBox ?? null,
        model: `${ID_MODEL} single photo`,
        source: SOURCE,
      });
    }
  let prediction = null;
  if (ident?.combined) {
    const used = ident.combined.used ?? [];
    const byId = new Map(cam.photos.map(p => [p.fileId, p]));
    const kept = inTraining ? db.prepare('SELECT * FROM photo_predictions WHERE cam = ?').get(cam.cam) : null;
    const json = text => {
      try {
        return text ? JSON.parse(text) : [];
      } catch {
        return [];
      }
    };
    prediction = inTraining
      ? {
          cam: cam.cam,
          source: kept?.source ?? SOURCE,
          species: json(kept?.species_json),
          genus: json(kept?.genus_json),
          subspecies: json(kept?.subspecies_json),
          andes: kept ? kept.andes : cam.side,
          model: kept?.model ?? 'none: the butterfly is in the training of head_hier 749fbffd (in-sample)',
          photos: json(kept?.photos_json),
          sexModel: sexModelName(ident.sex),
        }
      : {
          cam: cam.cam,
          source: SOURCE,
          species: ident.combined.species,
          genus: ident.combined.genus,
          subspecies: ident.combined.subspecies,
          andes: cam.side,
          model: `${ID_MODEL} (feature mean of ${used.length} photo${used.length === 1 ? '' : 's'})`,
          photos: used.map(f => ({ fileId: f, name: byId.get(f)?.name ?? null, view: byId.get(f) ? viewOf(byId.get(f)) : null })),
          sexModel: sexModelName(ident.sex),
        };
    const sex = ident.sex ?? (cam.sex?.missing ? { status: `not_scored:${cam.sex.missing}` } : null);
    if (sex && Number.isFinite(sex.pMale)) {
      const conf = Math.max(sex.pMale, 1 - sex.pMale);
      Object.assign(prediction, {
        sex: sex.pMale >= 0.5 ? 'male' : 'female',
        sexConf: Math.round(conf * 10000) / 10000,
        sexSupported: !!sex.supported,
        sexSpecies: cam.species ?? null,
        sexStatus: sex.status,
        sexPMale: sex.pMale,
      });
    } else if (sex) prediction.sexStatus = sex.status;
  }
  // Which photos were read, and how.
  const files = cam.pending.map(fileId => {
    const p = cam.photos.find(x => x.fileId === fileId);
    const why = answer.error ?? answer.wings?.[fileId]?.error ?? (ident ? ident.photos?.[fileId]?.error : answer.ident?.error) ?? answer.downloads?.[fileId] ?? null;
    return { fileId, name: p.name, cam: cam.cam, stamp: p.stamp, status: why ? 'error' : 'ok', error: why, at: now };
  });
  return { wings: rows, wingBoxes: boxes, photoPredictions, prediction, files };
}

/**
 * The background readings for one app. `readerDir`, `identifierDir`: the photo reader (with wing-chain.mjs) and the
 * identifier (with its sex/ folder); `publicDir`: where the gallery's files go; `siteUpdates`: the sites' updates
 * (server/site-updates.mjs: `start('gallery')`). For tests: `workerArgs` (the child's command after node),
 * `build` (the files made in the app's thread), `debounceMs`, `maxWaitMs`, `retryMs`, `batchPhotos`, `now`.
 */
export function createReadings({
  store,
  readerDir = null,
  identifierDir = null,
  publicDir = null,
  siteUpdates = null,
  threads = 2,
  workerArgs = null,
  spawnImpl = spawn,
  build = null,
  batchPhotos = BATCH_PHOTOS,
  debounceMs = DEBOUNCE_MS,
  maxWaitMs = MAX_WAIT_MS,
  retryMs = BUSY_RETRY_MS,
  enabled = true,
  log = (...a) => console.log(...a),
  now = () => Date.now(),
}) {
  const db = store.db;
  initPhotoTables(db);
  initReadingTables(db);
  // A batch cut by a restart: what it had read is kept; the rest is read again.
  db.prepare("UPDATE photo_reading_batches SET state='stopped', ended_at=? WHERE state='reading'").run(new Date(now()).toISOString());
  let running = null;
  let current = null;
  let again = false;
  let closed = false;
  let lastStamp = null;
  let building = null;
  let worker = null;
  let lastPlan = null;
  const update = { pendingSince: null, timer: null, last: null, batches: 0, failures: 0 };

  const missing = () => {
    if (!enabled) return 'turned off (READINGS=0)';
    if (!db.location?.()) return 'in-memory database';
    if (workerArgs) return null;
    if (!readerDir || !existsSync(join(readerDir, 'wing-chain.mjs'))) return 'no photo reader with the wing chain (scripts/build-rename-app.sh)';
    if (!identifierDir || !existsSync(join(identifierDir, 'venv', 'bin', 'python'))) return 'no identifier (tools/identifier/setup.sh)';
    return null;
  };

  function record(files, batchId) {
    const upsert = db.prepare(`INSERT INTO photo_reading_files(file_id,name,cam,stamp,status,attempts,error,batch_id,read_at) VALUES(?,?,?,?,?,?,?,?,?)
      ON CONFLICT(file_id) DO UPDATE SET name=excluded.name, cam=excluded.cam, stamp=excluded.stamp, status=excluded.status,
        attempts=CASE WHEN excluded.status='ok' THEN 0 WHEN photo_reading_files.stamp=excluded.stamp THEN photo_reading_files.attempts+1 ELSE 1 END,
        error=excluded.error, batch_id=excluded.batch_id, read_at=excluded.read_at`);
    for (const f of files) upsert.run(f.fileId, f.name, f.cam, f.stamp, f.status, f.status === 'ok' ? 0 : 1, f.error, batchId, f.at);
  }

  /** A butterfly's answer written: its rows (those read before kept when nothing new could be read) and its photos' log. */
  function write(cam, answer, models, batchId) {
    const out = rowsOfAnswer(db, cam, answer, { wingModel: models.wings ?? null, now: new Date(now()).toISOString() });
    const wingsRead = out.wings.some(w => !w.error);
    if (wingsRead || out.prediction || out.photoPredictions.length)
      upsertReadings(
        db,
        {
          cams: wingsRead ? [cam.cam] : [],
          wings: wingsRead ? out.wings : [],
          wingBoxes: wingsRead ? out.wingBoxes : [],
          predictions: out.prediction ? [out.prediction] : [],
          photoPredictions: out.photoPredictions,
          source: SOURCE,
        },
        { bump: false },
      );
    record(out.files, batchId);
    return out.files;
  }

  /**
   * One batch: the child process plans it (the photos waiting, from its own read-only connection: the scan of
   * Photo_links takes a fraction of a second, kept out of the app's thread) and reads it, a butterfly at a time;
   * each answer is written as it comes. Returns null when nothing waits.
   */
  async function readBatch() {
    const path = db.location?.() ?? null;
    const argv = [...(workerArgs ?? [WORKER, '--reader', readerDir, '--identifier', identifierDir, '--threads', String(threads)]), '--db', path, '--batch', String(batchPhotos), '--now', String(now())];
    const child = spawnImpl(process.execPath, argv, { stdio: ['ignore', 'pipe', 'pipe'] });
    try {
      // The lowest priority: the app, the chats and their identifier go first.
      if (child.pid) setPriority(child.pid, 19);
    } catch {
      // not allowed here: it still runs
    }
    worker = child;
    const exited = new Promise(resolve => {
      child.on('exit', code => resolve(code));
      child.on('error', e => resolve(e.code ?? e.message));
    });
    child.stderr.on('data', d => console.error('Readings:', String(d).trim().slice(0, 300)));
    let models = {};
    let batch = null;
    let id = null;
    const answered = new Set();
    const started = now();
    for await (const line of createInterface({ input: child.stdout })) {
      let out;
      try {
        out = JSON.parse(line);
      } catch {
        continue;
      }
      if (out.ready) models = out.models ?? {};
      else if ('plan' in out) {
        batch = out.plan;
        lastPlan = { at: new Date(now()).toISOString(), waiting: batch?.pending ?? 0 };
        if (!batch) continue;
        id = randomUUID();
        current = { id, startedAt: new Date(started).toISOString(), cams: batch.cams.length, photos: batch.photos, done: 0, readings: 0, errors: 0, waiting: batch.pending };
        db.prepare("INSERT INTO photo_reading_batches(id,started_at,cams,photos,state) VALUES(?,?,?,?,'reading')").run(id, current.startedAt, batch.cams.length, batch.photos);
      } else if (batch && out.cam && !closed) {
        const cam = batch.cams.find(c => c.cam === out.cam);
        if (!cam || answered.has(cam.cam)) continue;
        answered.add(cam.cam);
        const files = write(cam, out, models, id);
        current.done += files.length;
        current.readings += files.filter(f => f.status === 'ok').length;
        current.errors += files.filter(f => f.status !== 'ok').length;
        db.prepare('UPDATE photo_reading_batches SET done=?, readings=?, errors=? WHERE id=?').run(current.done, current.readings, current.errors, id);
      }
    }
    const code = await exited;
    worker = null;
    if (!batch) return null;
    // Stopped in the middle (not by the app): the butterfly it was reading waits like a photo that failed; the
    // others go to the next batch.
    const next = batch.cams.find(c => !answered.has(c.cam));
    if (next && !closed) {
      const files = write(next, { error: `the readings' worker stopped (${code ?? 'killed'})` }, models, id);
      current.done += files.length;
      current.errors += files.length;
    }
    const ended = new Date(now()).toISOString();
    db.prepare('UPDATE photo_reading_batches SET state=?, ended_at=?, done=?, readings=?, errors=? WHERE id=?').run(closed ? 'stopped' : next ? 'cut' : 'done', ended, current.done, current.readings, current.errors, id);
    const result = { id, photos: batch.photos, readings: current.readings, errors: current.errors, endedAt: ended, ms: now() - started, cut: !!next };
    current = null;
    if (result.readings) {
      bumpReviewRevision(db);
      log(`Readings: ${result.readings} photos read (${result.errors} could not be), in ${Math.round(result.ms / 1000)} s`);
    }
    return result;
  }

  /** The gallery's files made again (a child process reading the database; in the app's thread for tests). */
  function rebuild() {
    building ??= (async () => {
      const revision = reviewRevision(db);
      const path = db.location?.() ?? null;
      if (build) await build(db, publicDir);
      else if (!path || !publicDir) {
        if (publicDir) writeGalleryFiles(publicDir, galleryData(db));
      } else
        await new Promise((resolve, reject) => {
          const child = spawnImpl(process.execPath, [BUILDER, '--db', path, '--out', publicDir], { stdio: ['ignore', 'ignore', 'pipe'] });
          try {
            if (child.pid) setPriority(child.pid, 19);
          } catch {
            // it still runs
          }
          let said = '';
          child.stderr?.on('data', d => (said += d));
          child.on('error', reject);
          child.on('exit', code => (code === 0 ? resolve() : reject(new Error(`the gallery's files: ${said.trim().slice(0, 200) || code}`))));
        });
      db.prepare("INSERT INTO review_meta(key,value) VALUES('gallery_files',?) ON CONFLICT(key) DO UPDATE SET value=excluded.value").run(revision);
    })().finally(() => (building = null));
    return building;
  }

  /** The gallery's update after readings: once for the batches ending within the wait, after their files. */
  function scheduleUpdate() {
    if (!siteUpdates?.start) return;
    if (!siteUpdates.ready?.()) {
      update.last = { at: new Date(now()).toISOString(), state: 'not started', why: 'no password for the update server (SITE_UPDATE_PASSWORD_FILE)' };
      return;
    }
    update.batches++;
    update.pendingSince ??= now();
    clearTimeout(update.timer);
    const wait = Math.max(0, Math.min(debounceMs, update.pendingSince + maxWaitMs - now()));
    update.timer = setTimeout(fireUpdate, wait);
    update.timer.unref?.();
  }
  async function fireUpdate() {
    update.timer = null;
    let out;
    try {
      out = await siteUpdates.start('gallery');
    } catch (e) {
      out = { error: String(e.message || e) };
    }
    const at = new Date(now()).toISOString();
    if (out?.update?.note && /already running/.test(out.update.note)) {
      // It may have taken the files before this batch's: started again in a while.
      update.last = { at, state: 'waiting', why: 'an update was running' };
      update.timer = setTimeout(fireUpdate, retryMs);
      update.timer.unref?.();
      return;
    }
    update.last = out?.error ? { at, state: 'failed', why: out.error } : { at, state: out?.update?.state ?? 'started', batches: update.batches, link: out?.update?.link };
    log(`Readings: the gallery's update ${out?.error ? `failed: ${out.error}` : 'started'} (${update.batches} batch${update.batches === 1 ? '' : 'es'})`);
    // GitHub or the update server not answering: tried again twice, a while apart.
    if (out?.error && ++update.failures < 3) {
      update.timer = setTimeout(fireUpdate, retryMs * update.failures);
      update.timer.unref?.();
      return;
    }
    update.failures = 0;
    update.pendingSince = null;
    update.batches = 0;
  }

  async function loop() {
    let cuts = 0;
    for (;;) {
      if (closed) return;
      again = false;
      const result = await readBatch();
      if (!result || closed) break;
      // A worker that keeps stopping (twice in a row): left until the next sync or request.
      cuts = result.cut ? cuts + 1 : 0;
      if (cuts >= 2) {
        console.error("Readings: the readings' worker stopped twice in a row; tried again after the next sync");
        break;
      }
      if (result.readings) {
        await rebuild().catch(e => console.error('Readings:', e.message));
        scheduleUpdate();
      } else if (!result.errors) break;
      // Photos that could not be read wait RETRY_MS: the next batch has others.
    }
    // Files out of date with the tables (an import since): made again.
    if (!closed && publicDir && reviewRevision(db) !== db.prepare("SELECT value FROM review_meta WHERE key='gallery_files'").get()?.value)
      await rebuild().catch(e => console.error('Readings:', e.message));
  }

  /** Reads what is not read yet, one batch after another; a call while it runs makes it look again at the end. */
  function run(reason = 'asked') {
    const why = missing();
    if (why) return { started: false, why };
    if (running) {
      again = true;
      return { started: false, running: true };
    }
    running = loop()
      .catch(e => console.error('Readings:', e.stack || e.message))
      .finally(() => {
        running = null;
        if (again && !closed) run('again');
      });
    return { started: true, reason };
  }

  /** After a sync: a look only when Photo_links changed since the last one (count and latest change). */
  function afterSync() {
    const s = db.prepare("SELECT count(*) n, max(updated_at) u FROM records WHERE sheet='Photo_links'").get();
    const stamp = `${s.n}:${s.u}`;
    if (stamp === lastStamp) return null;
    lastStamp = stamp;
    return run('sync');
  }
  const stopWatching = store.watchSyncs?.(status => {
    if (status?.state === 'error' || closed) return;
    try {
      afterSync();
    } catch (e) {
      console.error('Readings:', e.message);
    }
  });

  return {
    run,
    afterSync,
    rebuild,
    status() {
      const why = missing();
      const last = db.prepare("SELECT * FROM photo_reading_batches WHERE state != 'reading' ORDER BY started_at DESC LIMIT 1").get();
      return {
        state: why ? 'off' : current ? 'reading' : building ? 'building' : 'idle',
        ...(why ? { why } : {}),
        ...(current ? { batch: { ...current } } : {}),
        ...(lastPlan ? { waiting: lastPlan.waiting, waitingAt: lastPlan.at } : {}),
        ...(last ? { last: { endedAt: last.ended_at, photos: last.photos, readings: last.readings, errors: last.errors, state: last.state } } : {}),
        ...(update.pendingSince || update.last ? { galleryUpdate: { ...(update.pendingSince ? { due: new Date(update.pendingSince + Math.min(maxWaitMs, debounceMs)).toISOString() } : {}), ...(update.last ? { last: update.last } : {}) } } : {}),
      };
    },
    /** The photos waiting when the last batch was planned. */
    waiting: () => lastPlan,
    idle: () => (running ?? Promise.resolve()).then(() => building),
    close() {
      closed = true;
      worker?.kill('SIGKILL');
      clearTimeout(update.timer);
      stopWatching?.();
    },
  };
}
