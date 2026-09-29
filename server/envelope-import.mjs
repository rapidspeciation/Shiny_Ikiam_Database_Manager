// Reads the envelope curation of the photo archive (made on the home PC: OCR
// readings, full-text lines, flags, the decisions already taken) and the Wings
// Gallery's wing boxes and predictions into one small bundle, and writes a
// bundle into the tables of server/photodata.mjs. Used by
// scripts/import-envelope-curation.mjs; importing the same bundle twice
// leaves the same rows.

import { readFileSync, existsSync } from 'node:fs';
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
      if (c === '"' && text[i + 1] === '"') (field += '"'), i++;
      else if (c === '"') quoted = false;
      else field += c;
    } else if (c === '"') quoted = true;
    else if (c === ',') row.push(field), (field = '');
    else if (c === '\n' || c === '\r') {
      if (c === '\r' && text[i + 1] === '\n') i++;
      row.push(field);
      rows.push(row);
      row = [];
      field = '';
    } else field += c;
  }
  if (field || row.length) row.push(field), rows.push(row);
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
  const files = ['readings-0.jsonl', 'readings-1.jsonl', 'readings-2.jsonl', 'readings-single.jsonl'].filter(f => existsSync(join(dir, f)));
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
  const corrections = existsSync(join(dir, 'corrections.csv')) ? parseCsv(readFileSync(join(dir, 'corrections.csv'), 'utf8')) : [];
  const decided = existsSync(join(dir, 'decided.json')) ? JSON.parse(readFileSync(join(dir, 'decided.json'), 'utf8')) : {};
  const franz = existsSync(join(dir, 'decisions.json')) ? JSON.parse(readFileSync(join(dir, 'decisions.json'), 'utf8')) : {};
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

/** The gallery's data files, from a folder or its public URL. */
async function galleryFile(from, name) {
  if (/^https?:\/\//.test(from)) {
    const response = await fetch(new URL(`${name}.json`, from.endsWith('/') ? from : `${from}/`), { signal: AbortSignal.timeout(120000) });
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
    const best = (Array.isArray(list) ? list : []).filter(b => Array.isArray(b?.box)).sort((a, b) => !!b.union - !!a.union || (b.conf ?? 0) - (a.conf ?? 0))[0];
    if (best) wingBoxes.push([name, best.box.map(n => round(n)), round(best.conf, 3)]);
  }
  // The gallery's current predictions: live, then coverage, then the paired dorsal/ventral model (which wins).
  const [expanded, coverage, live] = await Promise.all(
    ['predictions_expanded_concat_dv', 'predictions_coverage_current', 'predictions_live_real'].map(n => galleryFile(from, n).catch(() => ({}))),
  );
  const merged = { ...live, ...coverage, ...expanded };
  const sexes = await galleryFile(from, 'sex_predictions').catch(() => ({}));
  const top = (list, n) => (Array.isArray(list) ? list.slice(0, n).map(r => [r[0], round(r[1], 4)]) : []);
  const predictions = [];
  for (const cam of new Set([...Object.keys(merged), ...Object.keys(sexes)])) {
    const p = merged[cam] ?? {};
    const s = sexes[cam];
    const sex = s && ['male', 'female'].includes(String(s.sex).toLowerCase()) && Number.isFinite(s.confidence) ? s : null;
    predictions.push({
      cam: cam.toUpperCase(),
      species: top(p.species, 5),
      genus: top(p.genus, 3),
      subspecies: top(p.subspecies, 3),
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

/** Writes a bundle; rows of an earlier import of the same kind are replaced. */
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
        insert.run(r.name, r.fileId, r.cam, r.view, json(r.bbox), r.turned || 0, json(r.size), r.camid, r.conf, json(r.candidates), json(r.camidLines), json(r.text), bundle.source ?? null, r.source, at);
      counts.readings = bundle.readings.length;
    }
    if (bundle.flags) {
      db.exec('DELETE FROM photo_flags');
      const insert = db.prepare(
        'INSERT OR REPLACE INTO photo_flags(id,cam,type,stratum,strength,photos_json,data_json,decision,target,database_value,action,decided_by,source,imported_at) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?)',
      );
      for (const f of bundle.flags)
        insert.run(f.id, f.cam, f.type, f.stratum, f.strength, json(f.photos), json(f.data), f.decision, f.target, f.database, f.action, f.decidedBy, bundle.source ?? null, at);
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
        'INSERT OR REPLACE INTO photo_predictions(cam,species_json,genus_json,subspecies_json,sex,sex_conf,sex_supported,sex_species,source,imported_at) VALUES(?,?,?,?,?,?,?,?,?,?)',
      );
      for (const p of bundle.predictions)
        insert.run(p.cam, json(p.species), json(p.genus), json(p.subspecies), p.sex ?? null, p.sexConf ?? null, p.sexSupported ? 1 : 0, p.sexSpecies ?? null, bundle.gallery ?? null, at);
      counts.predictions = bundle.predictions.length;
    }
    db.exec('COMMIT');
  } catch (e) {
    db.exec('ROLLBACK');
    throw e;
  }
  bumpReviewRevision(db);
  return counts;
}
