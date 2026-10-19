// Reads the envelope curation of the photo archive (made on the home PC: OCR
// readings, full-text lines, flags, the decisions already taken) and the Wings
// Gallery's wing boxes and predictions into one small bundle, and writes a
// bundle into the tables of server/photodata.mjs. Used by
// scripts/import-envelope-curation.mjs; importing the same bundle twice
// leaves the same rows.

import { readFileSync, existsSync, realpathSync } from 'node:fs';
import { basename, join } from 'node:path';
import { bumpReviewRevision, initPhotoTables, photoName } from './photodata.mjs';

export const GALLERY_URL = 'https://rapidspeciation.github.io/Shiny_Ikiam_Wings_Gallery/data/';
const round = (n, d = 4) => (typeof n === 'number' ? Math.round(n * 10 ** d) / 10 ** d : null);
const jsonl = path =>
  existsSync(path)
    ? readFileSync(path, 'utf8')
        .split('\n')
        .filter(l => l.trim())
        .map(l => JSON.parse(l))
    : [];

/** A small CSV reader (quoted fields, doubled quotes, CRLF). */
export function parseCsv(text) {
  const rows = [];
  let row = [],
    field = '',
    quoted = false;
  for (let i = 0; i < text.length; i++) {
    const c = text[i];
    if (quoted) {
      if (c === '"' && text[i + 1] === '"') ((field += '"'), i++);
      else if (c === '"') quoted = false;
      else field += c;
    } else if (c === '"') quoted = true;
    else if (c === ',') (row.push(field), (field = ''));
    else if (c === '\n' || c === '\r') {
      if (c === '\r' && text[i + 1] === '\n') i++;
      row.push(field);
      rows.push(row);
      row = [];
      field = '';
    } else field += c;
  }
  if (field || row.length) (row.push(field), rows.push(row));
  const [head, ...body] = rows.filter(r => r.some(Boolean));
  return body.map(r => Object.fromEntries(head.map((h, i) => [h.trim(), r[i] ?? ''])));
}

const STRENGTH = {
  'envelope-not-file:all-photos': 'fuerte',
  'envelope-not-file:high': 'media',
  'envelope-not-file:low': 'baja',
  'dv-disagree:confident': 'baja',
  'duplicate:own-folder': 'baja',
  'sex:both-photos': 'fuerte',
  'sex:one-photo': 'media',
  'species:both-photos': 'fuerte',
  'species:one-photo': 'media',
};

/**
 * The curation directory (readings-*.jsonl, fulltext.jsonl, flags.jsonl,
 * corrections.csv, decided.json, decisions.json) and the photo manifest
 * (Name, google_id) as a bundle.
 */
export function readCuration(dir, manifestPath) {
  const ids = new Map();
  for (const r of parseCsv(readFileSync(manifestPath, 'utf8'))) {
    const parsed = photoName(r.Name);
    if (parsed && r.google_id) ids.set(parsed.stem.toUpperCase(), r.google_id);
  }
  const fulltext = new Map(jsonl(join(dir, 'fulltext.jsonl')).map(r => [r.file, r.lines]));
  const files = ['readings-0.jsonl', 'readings-1.jsonl', 'readings-2.jsonl', 'readings-single.jsonl'].filter(f =>
    existsSync(join(dir, f)),
  );
  const readings = [];
  const seen = new Set();
  for (const file of files)
    for (const r of jsonl(join(dir, file))) {
      if (seen.has(r.name)) continue;
      seen.add(r.name);
      const parsed = photoName(r.name);
      readings.push({
        name: r.name,
        fileId: ids.get(String(r.name).toUpperCase()) ?? null,
        cam: String(r.folder || parsed?.cam || '').toUpperCase(),
        view: parsed?.view ?? null,
        bbox: Array.isArray(r.envelope) ? r.envelope.map(n => Math.round(n)) : null,
        turned: r.turned === 180 ? 180 : 0,
        size: r.size ?? null,
        camid: r.camid ?? null,
        conf: round(r.conf),
        candidates: (r.candidates ?? []).slice(0, 5).map(c => [c.id, round(c.conf)]),
        camidLines: (r.lines ?? []).map(l => [l.t, round(l.c, 3)]),
        text: (fulltext.get(r.file) ?? []).map(([t, s]) => [t, round(s, 3)]),
        source: r.source ?? null,
      });
    }

  // Decisions: corrections.csv (final, with who decided), then decisions.json (Franz), then decided.json.
  const corrections = existsSync(join(dir, 'corrections.csv'))
    ? parseCsv(readFileSync(join(dir, 'corrections.csv'), 'utf8'))
    : [];
  const decided = existsSync(join(dir, 'decided.json'))
    ? JSON.parse(readFileSync(join(dir, 'decided.json'), 'utf8'))
    : {};
  const franz = existsSync(join(dir, 'decisions.json'))
    ? JSON.parse(readFileSync(join(dir, 'decisions.json'), 'utf8'))
    : {};
  const final = new Map(corrections.map(c => [`${c.specimen}|${c.issue}`, c]));
  const used = new Set();
  const flags = [];
  for (const f of jsonl(join(dir, 'flags.jsonl'))) {
    const key = `${f.folder}|${f.type}`;
    const c = final.get(key);
    if (c) used.add(key);
    const earlier = decided[`${f.folder}|${f.stratum}`];
    const own = f.type === 'envelope-not-file' ? franz[f.folder] : null;
    const { type, stratum, folder, photos, ...data } = f;
    flags.push({
      id: `${type}|${folder}|${stratum}`,
      cam: folder,
      type,
      stratum,
      strength: STRENGTH[stratum] ?? null,
      photos: photos ?? [],
      data,
      decision: c?.decision || own?.decision || earlier?.[0] || null,
      target: c?.target || own?.envelope || null,
      database: c?.database || null,
      action: c?.action || own?.note || earlier?.[1] || null,
      decidedBy: c?.decided_by || own?.by || earlier?.[2] || null,
    });
  }
  // Decisions about a specimen no flag names (the other side of a duplicate): kept as their own flag.
  for (const c of corrections) {
    if (used.has(`${c.specimen}|${c.issue}`)) continue;
    flags.push({
      id: `${c.issue}|${c.specimen}|decision`,
      cam: c.specimen,
      type: c.issue,
      stratum: null,
      strength: null,
      photos: c.photos.split(/\s+/).filter(Boolean),
      data: {},
      decision: c.decision || null,
      target: c.target || null,
      database: c.database || null,
      action: c.action || null,
      decidedBy: c.decided_by || null,
    });
  }
  return { version: 1, source: basename(dir), createdAt: new Date().toISOString(), readings, flags };
}

/**
 * All the wings of a photo in one box (the gallery's unionBox: a photo can have
 * one box per wing), with a little margin, inside the photo.
 */
export function unionBox(list, padding = 0.03) {
  const valid = list.filter(
    b =>
      Array.isArray(b?.box) &&
      b.box.length === 4 &&
      b.box.every(Number.isFinite) &&
      b.box[0] < b.box[2] &&
      b.box[1] < b.box[3],
  );
  if (!valid.length) return null;
  let [x1, y1, x2, y2] = [0, 1, 2, 3].map(i => (i < 2 ? Math.min : Math.max)(...valid.map(b => b.box[i])));
  const [dx, dy] = [(x2 - x1) * padding, (y2 - y1) * padding];
  const clamp = n => Math.min(1, Math.max(0, n));
  [x1, y1, x2, y2] = [clamp(x1 - dx), clamp(y1 - dy), clamp(x2 + dx), clamp(y2 + dy)];
  return { box: [x1, y1, x2, y2].map(n => round(n)), conf: round(Math.min(...valid.map(b => b.conf ?? 1)), 3) };
}

/** The gallery's data files, from a folder or its public URL. */
async function galleryFile(from, name) {
  if (/^https?:\/\//.test(from)) {
    const response = await fetch(new URL(`${name}.json`, from.endsWith('/') ? from : `${from}/`), {
      signal: AbortSignal.timeout(120000),
    });
    if (!response.ok) throw new Error(`${name}.json: HTTP ${response.status}`);
    return response.json();
  }
  const path = join(from, `${name}.json`);
  return existsSync(path) ? JSON.parse(readFileSync(path, 'utf8')) : {};
}

/** Wing boxes (one per photo) and per-CAM species/sex predictions of the Wings Gallery, slimmed. */
export async function readGallery(from = GALLERY_URL) {
  const boxes = await galleryFile(from, 'wing_boxes_v6');
  const wingBoxes = [];
  for (const [name, list] of Object.entries(boxes)) {
    const union = unionBox(Array.isArray(list) ? list : []);
    if (union) wingBoxes.push([name, union.box, union.conf]);
  }
  // The gallery's current predictions: live, then coverage, then the paired dorsal/ventral model (which wins).
  const [expanded, coverage, live] = await Promise.all(
    ['predictions_expanded_concat_dv', 'predictions_coverage_current', 'predictions_live_real'].map(n =>
      galleryFile(from, n).catch(() => ({})),
    ),
  );
  const merged = { ...live, ...coverage, ...expanded };
  const sexes = await galleryFile(from, 'sex_predictions').catch(() => ({}));
  const top = (list, n) => (Array.isArray(list) ? list.slice(0, n).map(r => [r[0], round(r[1], 4)]) : []);
  const predictions = [];
  for (const cam of new Set([...Object.keys(merged), ...Object.keys(sexes)])) {
    const p = merged[cam] ?? {};
    const s = sexes[cam];
    const sex =
      s && ['male', 'female'].includes(String(s.sex).toLowerCase()) && Number.isFinite(s.confidence) ? s : null;
    predictions.push({
      cam: cam.toUpperCase(),
      species: top(p.species, 5),
      genus: top(p.genus, 3),
      subspecies: top(p.subspecies, 3),
      // The side of the Andes the model's geographic prior took (East / West).
      ...(/^(east|west)$/i.test(String(p.side ?? '')) ? { andes: String(p.side) } : {}),
      ...(sex
        ? {
            sex: String(sex.sex).toLowerCase(),
            sexConf: round(sex.confidence, 4),
            // As the gallery: supported only with the model's support and at least 80 %.
            sexSupported: sex.supported === true && sex.confidence >= 0.8,
            sexSpecies: sex.species ?? null,
          }
        : {}),
    });
  }
  return { wingBoxes, predictions, gallery: String(from) };
}

/** How far a photo is turned (the wing model's words) → the clockwise turn that puts it upright. */
const UPRIGHT_BY = { 'upside down': 180, 'turned 90 deg counter-clockwise': 90, 'turned 90 deg clockwise': 270 };
const num = v => (v === '' || v === undefined || v === null || !Number.isFinite(Number(v)) ? null : Number(v));
/** "CAM036744/CAM036744v__Unknown_species.jpg", "CAM076802v.JPG" → "CAM036744v", "CAM076802v". */
const stemOf = photo =>
  basename(String(photo ?? ''))
    .split('__')[0]
    .replace(/\.(jpe?g|heic|png|jfif|tiff?)$/i, '');

/**
 * The wing models' results (ithomiini_maps models/dorsal_ventral/eval: triage.csv, one line per
 * photo, and suspected_problems.csv, the side problems of each CAM's pair), from one or more of
 * those folders (the archive's, a later run on newer Drive photos), as bundle rows. The archive's
 * photos get their Drive file ID from the photo manifest (Name, google_id); a later run's carry it.
 */
export function readWingSides(dirs, manifestPath = null) {
  const ids = new Map();
  if (manifestPath)
    for (const r of parseCsv(readFileSync(manifestPath, 'utf8')))
      if (r.Name && r.google_id) ids.set(stemOf(r.Name).toUpperCase(), r.google_id);
  const wings = [];
  const sources = [];
  for (const dir of dirs) {
    const folder = basename(realpathSync(dir));
    const source = folder === 'eval' ? 'archive' : folder;
    sources.push(source);
    const pairs = new Map();
    const suspected = join(dir, 'suspected_problems.csv');
    if (existsSync(suspected))
      for (const p of parseCsv(readFileSync(suspected, 'utf8'))) pairs.set(stemOf(p.file).toUpperCase(), p.kind);
    for (const r of parseCsv(readFileSync(join(dir, 'triage.csv'), 'utf8'))) {
      const name = stemOf(r.photo);
      if (!name) continue;
      const lateral = /not checked/i.test(r.format ?? '');
      // Turned: the archive's reason "orientation:…" (only photos with enough wings get one); a later run says whether the wings disagree with how the photo is stored.
      const turn =
        r.turn_disagrees_with_stored !== undefined
          ? r.turn_disagrees_with_stored === 'yes'
            ? r.wing_based_turn
            : null
          : (/orientation:\s*([^(;]+?)\s*\(/.exec(r.photo_reasons ?? '')?.[1] ?? null);
      const reasons = String(r.photo_reasons ?? '').split(';').map(s => s.trim()).filter(Boolean).join('; ');
      const camReasons = String(r.cam_reasons ?? '').split(';').map(s => s.trim()).filter(Boolean).join('; ');
      wings.push({
        name,
        fileId: r.drive_file_id || ids.get(name.toUpperCase()) || null,
        cam: /^(?:CAM\d{5,7}|M\d{4,5})$/i.test(r.cam ?? '') ? r.cam.toUpperCase() : null,
        suffix: r.suffix || null,
        side: lateral ? null : r.side || null,
        pDorsal: num(r.p_dorsal),
        pForewings: num(r.p_dorsal_forewings),
        pHindwings: num(r.p_dorsal_hindwings),
        wings: num(r.n_wings),
        ...(turn && UPRIGHT_BY[turn] ? { turn, degrees: UPRIGHT_BY[turn] } : {}),
        ...(pairs.has(name.toUpperCase()) ? { pair: pairs.get(name.toUpperCase()) } : {}),
        ...(reasons ? { reasons } : {}),
        ...(camReasons ? { camReasons } : {}),
        review: r.review === 'yes',
        ...(r.folder_path ? { folder: r.folder_path } : {}),
        source,
      });
    }
  }
  return { wings, wingSources: sources };
}

/**
 * Writes a bundle; rows of an earlier import of the same kind are replaced. Kinds: readings, flags, wingBoxes,
 * predictions (one per CAM: species, sex), photoPredictions (the identifier on each photo alone, by Drive ID) and
 * wings (one per photo: the wing models' reading; `wingList`, `outlines`, `missing`, `size`, `turnK`/`pTurn` and the fore/hind
 * sides come with the nano model's bundles).
 */
export function importBundle(db, bundle) {
  initPhotoTables(db);
  const at = new Date().toISOString();
  const json = v => (v === null || v === undefined ? null : JSON.stringify(v));
  const counts = {};
  db.exec('BEGIN');
  try {
    if (bundle.readings) {
      db.exec('DELETE FROM envelope_readings');
      const insert = db.prepare(
        'INSERT INTO envelope_readings(name,file_id,cam,view,bbox_json,turned,size_json,camid,camid_conf,candidates_json,camid_lines_json,text_json,model,source,imported_at) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)',
      );
      for (const r of bundle.readings)
        insert.run(
          r.name,
          r.fileId,
          r.cam,
          r.view,
          json(r.bbox),
          r.turned || 0,
          json(r.size),
          r.camid,
          r.conf,
          json(r.candidates),
          json(r.camidLines),
          json(r.text),
          bundle.source ?? null,
          r.source,
          at,
        );
      counts.readings = bundle.readings.length;
    }
    if (bundle.flags) {
      db.exec('DELETE FROM photo_flags');
      const insert = db.prepare(
        'INSERT OR REPLACE INTO photo_flags(id,cam,type,stratum,strength,photos_json,data_json,decision,target,database_value,action,decided_by,source,imported_at) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?)',
      );
      for (const f of bundle.flags)
        insert.run(
          f.id,
          f.cam,
          f.type,
          f.stratum,
          f.strength,
          json(f.photos),
          json(f.data),
          f.decision,
          f.target,
          f.database,
          f.action,
          f.decidedBy,
          bundle.source ?? null,
          at,
        );
      counts.flags = bundle.flags.length;
    }
    if (bundle.wingBoxes) {
      db.exec('DELETE FROM photo_wing_boxes');
      const insert = db.prepare('INSERT OR REPLACE INTO photo_wing_boxes(name,box_json,conf) VALUES(?,?,?)');
      for (const [name, box, conf] of bundle.wingBoxes) insert.run(name, json(box), conf);
      counts.wingBoxes = bundle.wingBoxes.length;
    }
    if (bundle.predictions) {
      db.exec('DELETE FROM photo_predictions');
      const insert = db.prepare(
        'INSERT OR REPLACE INTO photo_predictions(cam,species_json,genus_json,subspecies_json,sex,sex_conf,sex_supported,sex_species,source,imported_at,andes,model,photos_json,sex_status,sex_p_male,sex_model) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)',
      );
      for (const p of bundle.predictions)
        insert.run(
          p.cam,
          json(p.species),
          json(p.genus),
          json(p.subspecies),
          p.sex ?? null,
          p.sexConf ?? null,
          p.sexSupported ? 1 : 0,
          p.sexSpecies ?? null,
          p.source ?? bundle.gallery ?? null,
          at,
          p.andes ?? null,
          p.model ?? null,
          json(p.photos),
          p.sexStatus ?? null,
          p.sexPMale ?? null,
          p.sexModel ?? null,
        );
      counts.predictions = bundle.predictions.length;
    }
    if (bundle.photoPredictions) {
      db.exec('DELETE FROM photo_file_predictions');
      const insert = db.prepare(
        'INSERT OR REPLACE INTO photo_file_predictions(file_id,name,cam,view,species_json,genus_json,subspecies_json,andes,crop_status,crop_box_json,model,source,imported_at) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?)',
      );
      for (const p of bundle.photoPredictions)
        insert.run(
          p.fileId,
          p.name ?? null,
          p.cam ?? null,
          p.view ?? null,
          json(p.species),
          json(p.genus),
          json(p.subspecies),
          p.andes ?? null,
          p.cropStatus ?? null,
          json(p.cropBox),
          p.model ?? null,
          p.source ?? bundle.source ?? null,
          at,
        );
      counts.photoPredictions = bundle.photoPredictions.length;
    }
    if (bundle.wings) {
      db.exec('DELETE FROM photo_wing_sides');
      const insert = db.prepare(
        'INSERT INTO photo_wing_sides(name,file_id,cam,suffix,side,p_dorsal,p_forewings,p_hindwings,wings,turn,degrees,pair,reasons,cam_reasons,review,folder,source,imported_at,model,fore_side,hind_side,turn_k,p_turn,missing_json,wings_json,size_json,error,outlines_json) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)',
      );
      for (const w of bundle.wings)
        insert.run(
          w.name,
          w.fileId ?? null,
          w.cam ?? null,
          w.suffix ?? null,
          w.side ?? null,
          w.pDorsal ?? null,
          w.pForewings ?? null,
          w.pHindwings ?? null,
          w.wings ?? null,
          w.turn ?? null,
          w.degrees ?? null,
          w.pair ?? null,
          w.reasons ?? null,
          w.camReasons ?? null,
          w.review ? 1 : 0,
          w.folder ?? null,
          w.source ?? null,
          at,
          w.model ?? null,
          w.foreSide ?? null,
          w.hindSide ?? null,
          w.turnK ?? null,
          w.pTurn ?? null,
          json(w.missing),
          json(w.wingList),
          json(w.size),
          w.error ?? null,
          json(w.outlines),
        );
      counts.wings = bundle.wings.length;
    }
    db.exec('COMMIT');
  } catch (e) {
    db.exec('ROLLBACK');
    throw e;
  }
  bumpReviewRevision(db);
  return counts;
}
