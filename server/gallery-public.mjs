// The photo readings the Wings Gallery takes from the app (its update_data.yml downloads them; formats agreed with
// the gallery in /tmp/gallery-readings-spec.md, "Decisions"): the species and sex of each butterfly and each
// photo's zoom box and wing outlines, from the tables the photo readings fill (the backfill's import and the
// background readings, server/readings.mjs). Only the models' readings: no notes, people or addresses. They are
// the collection's predictions, public on the gallery already.
//
// Built into a folder (PUBLIC_GALLERY_DIR, beside the database by default) by a child process after each batch
// of readings (`node server/gallery-public.mjs --db <database> --out <folder>`), so the app's thread only serves
// the files: /api/public/gallery/<file>, without login, gzip and an ETag (server/index.mjs).

import { createHash } from 'node:crypto';
import { existsSync, mkdirSync, readFileSync, renameSync, statSync, writeFileSync } from 'node:fs';
import { join, resolve } from 'node:path';
import { pathToFileURL } from 'node:url';
import { gzipSync } from 'node:zlib';
import { oofSource } from './oof.mjs';
import { initPhotoTables, photoIndex } from './photodata.mjs';
import { outlinePath, zoomBox } from './wing-outlines.mjs';

export const FILES = ['predictions_app.json', 'sex_predictions_app.json', 'wing_boxes_app.json', 'wing_outlines.json'];
export const INDEX = 'index.json';
const MODE = 'app_readings';
const WING_MODEL = 'wingseg_v5n_640 f3c4f755 + wing_side_cls 838b9371';

const parse = text => {
  try {
    return text ? JSON.parse(text) : null;
  } catch {
    return null;
  }
};
const r4 = v => (v === null || v === undefined ? null : Math.round(Number(v) * 10000) / 10000);
const pairs = list => (Array.isArray(list) ? list.filter(p => Array.isArray(p) && p.length >= 2).map(([n, p]) => [String(n), r4(p)]) : []);
const byP = list => [...list].sort((a, b) => b[1] - a[1]);
/** The gallery's key of a photo: its name without extension; Drive's "(2)" of a second copy becomes "_2". */
export const photoKey = name =>
  String(name ?? '')
    .trim()
    .replace(/\.(jpe?g|heic|heif|png|jfif|tiff?)$/i, '')
    .replace(/\s*\((\d+)\)$/, '_$1');

/** The four files' contents (objects) from a database. */
export function galleryData(db) {
  initPhotoTables(db);
  const predictions = {};
  const sex = {};
  for (const p of db.prepare('SELECT * FROM photo_predictions WHERE model IS NOT NULL ORDER BY cam').all()) {
    const cam = String(p.cam).toUpperCase();
    const species = byP(pairs(parse(p.species_json)));
    // Out-of-fold predictions only (server/oof.mjs): the identifier's head on a butterfly it was trained on is left out.
    const oof = oofSource({ cam, source: p.source });
    if (species.length && oof) {
      const photos = (parse(p.photos_json) ?? []).map(f => ({ name: f.name ?? null, file: f.fileId ?? null }));
      predictions[cam] = {
        species,
        subspecies: byP(pairs(parse(p.subspecies_json))),
        genus: byP(pairs(parse(p.genus_json))),
        side: p.andes === 'East' || p.andes === 'West' ? p.andes : '',
        n_views: photos.length,
        mode: MODE,
        model_meta: { source: p.source ?? null, model: p.model, oof: true, oof_source: oof, photos },
      };
    }
    if (p.sex_p_male !== null && p.sex_p_male !== undefined && p.sex && !String(p.sex_status ?? '').startsWith('not_scored')) {
      sex[cam] = {
        sex: p.sex,
        confidence: r4(p.sex_conf ?? Math.max(p.sex_p_male, 1 - p.sex_p_male)),
        male_probability: r4(p.sex_p_male),
        supported: !!p.sex_supported,
        species: p.sex_species ?? null,
        screen_status: p.sex_status ?? null,
        model_arm: p.sex_model ?? null,
        source: p.source ?? null,
      };
    }
  }
  // A name listed twice (copies in two folders): the file Photo_links finds by that name.
  const listed = photoIndex({ db }).byName;
  const rows = db.prepare('SELECT name, file_id, wings_json, outlines_json, size_json, source, model FROM photo_wing_sides WHERE outlines_json IS NOT NULL AND file_id IS NOT NULL ORDER BY name, file_id').all();
  const chosen = new Map();
  for (const r of rows) {
    const key = photoKey(r.name);
    const had = chosen.get(key);
    if (!had || (had.file_id !== listed.get(String(r.name).toUpperCase()) && r.file_id === listed.get(String(r.name).toUpperCase()))) chosen.set(key, r);
  }
  const boxes = {};
  const outlines = {};
  let source = null;
  for (const [key, r] of chosen) {
    const lines = parse(r.outlines_json);
    const wings = parse(r.wings_json) ?? [];
    if (!Array.isArray(lines)) continue;
    // The zoom box (server/wing-outlines.mjs zoomBox) from the outlines stored, in the photo's pixels.
    const size = parse(r.size_json);
    const [W, H] = Array.isArray(size) && size[0] > 0 && size[1] > 0 ? size : [1000, 1000];
    const px = flat => Array.from({ length: flat.length >> 1 }, (_, i) => [(flat[2 * i] / 1000) * W, (flat[2 * i + 1] / 1000) * H]);
    const box = zoomBox(lines.filter(l => Array.isArray(l?.[2]) && l[2].length >= 6).map(([kind, , flat]) => ({ kind, points: px(flat) })), W, H);
    if (!box) continue;
    const scores = lines.map((l, i) => (l?.[0] === 'fw' || l?.[0] === 'hw' ? wings[i]?.score : null)).filter(Number.isFinite);
    boxes[key] = [{ box, conf: scores.length ? Math.round(Math.min(...scores) * 1000) / 1000 : null, model: 'v5n', union: true, file: r.file_id }];
    outlines[key] = {
      file: r.file_id,
      w: lines.filter(l => (l?.[0] === 'fw' || l?.[0] === 'hw') && Array.isArray(l[2]) && l[2].length >= 6).map(([kind, half, pts]) => [kind, half, outlinePath(pts)]),
    };
    source ??= r.source ?? null;
  }
  return {
    'predictions_app.json': predictions,
    'sex_predictions_app.json': sex,
    'wing_boxes_app.json': boxes,
    'wing_outlines.json': { version: 2, source, model: WING_MODEL, photos: outlines },
  };
}

const countOf = (name, value) => Object.keys(name === 'wing_outlines.json' ? value.photos : value).length;

/**
 * Writes the files into `dir` (each with its .gz beside it, then index.json): written whole under another name
 * and renamed, so a download never gets half a file. Returns the index.
 */
export function writeGalleryFiles(dir, data, { now = new Date() } = {}) {
  mkdirSync(dir, { recursive: true });
  const index = { generated: now.toISOString(), files: {} };
  for (const name of FILES) {
    const text = JSON.stringify(data[name]);
    const sha256 = createHash('sha256').update(text).digest('hex');
    index.files[name] = { count: countOf(name, data[name]), sha256, bytes: Buffer.byteLength(text) };
    for (const [file, body] of [
      [name, text],
      [`${name}.gz`, gzipSync(text, { level: 9 })],
    ]) {
      writeFileSync(join(dir, `${file}.part`), body);
      renameSync(join(dir, `${file}.part`), join(dir, file));
    }
  }
  const text = JSON.stringify(index);
  writeFileSync(join(dir, `${INDEX}.part`), text);
  renameSync(join(dir, `${INDEX}.part`), join(dir, INDEX));
  return index;
}

/**
 * The files as the app serves them, read again when index.json changes: get(name) → { body, gz, etag,
 * modified } or null (not built). `dir`: where they are.
 */
export function galleryFiles(dir) {
  let kept = null;
  function load() {
    const path = join(dir ?? '', INDEX);
    if (!dir || !existsSync(path)) return (kept = null);
    const mtime = statSync(path).mtimeMs;
    if (kept?.mtime === mtime) return kept;
    const index = parse(readFileSync(path, 'utf8'));
    if (!index?.files) return (kept = null);
    const modified = new Date(index.generated ?? mtime).toUTCString();
    const files = new Map();
    for (const name of FILES) {
      const meta = index.files[name];
      if (!meta || !existsSync(join(dir, name))) continue;
      files.set(name, { path: join(dir, name), gzPath: join(dir, `${name}.gz`), etag: `"${meta.sha256.slice(0, 32)}"`, modified, body: null, gz: null });
    }
    const indexText = JSON.stringify(index);
    files.set(INDEX, { etag: `"${createHash('sha256').update(indexText).digest('hex').slice(0, 32)}"`, modified, body: Buffer.from(indexText), gz: gzipSync(indexText) });
    return (kept = { mtime, files });
  }
  return {
    get(name) {
      const f = load()?.files.get(name);
      if (!f) return null;
      // Read once per build; the files are a few MB.
      if (!f.body) f.body = readFileSync(f.path);
      if (!f.gz && f.gzPath && existsSync(f.gzPath)) f.gz = readFileSync(f.gzPath);
      return f;
    },
    names: () => [...FILES, INDEX],
  };
}

// node server/gallery-public.mjs --db <database> --out <folder>: built from the database (read only).
if (process.argv[1] && import.meta.url === pathToFileURL(resolve(process.argv[1])).href) {
  const arg = name => {
    const i = process.argv.indexOf(`--${name}`);
    return i >= 0 ? process.argv[i + 1] : null;
  };
  const { DatabaseSync } = await import('node:sqlite');
  const db = new DatabaseSync(arg('db'), { readOnly: true, timeout: 5000 });
  const index = writeGalleryFiles(arg('out'), galleryData(db));
  db.close();
  process.stdout.write(`${JSON.stringify(index)}\n`);
}
