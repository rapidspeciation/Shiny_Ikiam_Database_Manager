// Photos of preserved butterflies and what was read from them away from the app:
// the Photo_links sheet (file names → Drive files), the envelope readings (OCR
// of the handwritten envelope photographed with the wings), the curation flags
// with the decisions already taken, the wing boxes and the Wings Gallery's
// species/sex predictions. The tables are filled by
// scripts/import-envelope-curation.mjs; the checks (server/checks.mjs) and the
// Revisión tab read them here.

const ready = new WeakSet();

export function initPhotoTables(db) {
  if (ready.has(db)) return;
  db.exec(`CREATE TABLE IF NOT EXISTS envelope_readings(
      name TEXT PRIMARY KEY, file_id TEXT, cam TEXT NOT NULL, view TEXT, bbox_json TEXT, turned INTEGER NOT NULL DEFAULT 0,
      size_json TEXT, camid TEXT, camid_conf REAL, candidates_json TEXT, camid_lines_json TEXT, text_json TEXT,
      model TEXT, source TEXT, imported_at TEXT NOT NULL);
    CREATE INDEX IF NOT EXISTS envelope_readings_cam ON envelope_readings(cam);
    CREATE TABLE IF NOT EXISTS photo_flags(
      id TEXT PRIMARY KEY, cam TEXT NOT NULL, type TEXT NOT NULL, stratum TEXT, strength TEXT, photos_json TEXT,
      data_json TEXT, decision TEXT, target TEXT, database_value TEXT, action TEXT, decided_by TEXT, source TEXT,
      imported_at TEXT NOT NULL);
    CREATE TABLE IF NOT EXISTS photo_wing_boxes(name TEXT PRIMARY KEY, box_json TEXT NOT NULL, conf REAL);
    CREATE TABLE IF NOT EXISTS photo_predictions(
      cam TEXT PRIMARY KEY, species_json TEXT, genus_json TEXT, subspecies_json TEXT, sex TEXT, sex_conf REAL,
      sex_supported INTEGER, sex_species TEXT, source TEXT, imported_at TEXT NOT NULL, andes TEXT);
    CREATE TABLE IF NOT EXISTS review_meta(key TEXT PRIMARY KEY, value TEXT NOT NULL);
    CREATE TABLE IF NOT EXISTS photo_wing_sides(
      id INTEGER PRIMARY KEY, name TEXT NOT NULL, file_id TEXT, cam TEXT, suffix TEXT, side TEXT, p_dorsal REAL,
      p_forewings REAL, p_hindwings REAL, wings INTEGER, turn TEXT, degrees INTEGER, pair TEXT, reasons TEXT,
      cam_reasons TEXT, review INTEGER, folder TEXT, source TEXT, imported_at TEXT NOT NULL);
    CREATE INDEX IF NOT EXISTS photo_wing_sides_cam ON photo_wing_sides(cam);
    CREATE INDEX IF NOT EXISTS photo_wing_sides_file ON photo_wing_sides(file_id);`);
  // The side of the Andes the gallery's prediction assumed, added after the first imports: by the app (or an
  // import); a worker's connection may not alter tables, and reads the rows without it until then.
  if (!db.prepare('PRAGMA table_info(photo_predictions)').all().some(c => c.name === 'andes'))
    try {
      db.exec('ALTER TABLE photo_predictions ADD COLUMN andes TEXT');
    } catch {
      // a reader: the column comes with the app's next start
    }
  ready.add(db);
}

/** Changes whenever an import changes the tables above (the checks are cached on it). */
export function reviewRevision(db) {
  initPhotoTables(db);
  return db.prepare("SELECT value FROM review_meta WHERE key='revision'").get()?.value ?? '0';
}
export function bumpReviewRevision(db) {
  initPhotoTables(db);
  db.prepare(
    "INSERT INTO review_meta(key,value) VALUES('revision',?) ON CONFLICT(key) DO UPDATE SET value=excluded.value",
  ).run(new Date().toISOString());
}

/** "CAM070046d.JPG", "CAM070046v (2).jpg", "CAM074081v2", "M6681d2" (the M series of Collection_data) → { cam, view, stem }; null for other photos. */
export function photoName(name) {
  const stem = String(name ?? '')
    .trim()
    .replace(/\.(jpe?g|heic|png|jfif|tiff?)$/i, '');
  const m = /^(CAM\d{5,7}|M\d{4,5}(?=[\s_-]*[dv]?\d*(?:\s*\(\d+\))?$))\s*[_-]?\s*([dv])?/i.exec(stem);
  if (!m) return null;
  const view = m[2] ? (m[2].toLowerCase() === 'd' ? 'dorsal' : 'ventral') : 'other';
  return { cam: m[1].toUpperCase(), view, stem };
}
/** The Drive file ID in a Photo_links URL (…/file/d/<id>/view, …?id=<id>). */
export function driveId(url) {
  const m = /\/d\/([\w-]{20,})|[?&]id=([\w-]{20,})/.exec(String(url ?? ''));
  return m ? m[1] || m[2] : null;
}
export const isFileId = id => /^[\w-]{20,100}$/.test(String(id ?? ''));

const parse = text => {
  if (text === null || text === undefined) return null;
  try {
    return JSON.parse(text);
  } catch {
    return null;
  }
};

const indexCache = new WeakMap();
/**
 * CAM → its dorsal and ventral photos (Drive file IDs), from the synced
 * Photo_links rows; photos known only from the imported manifest (envelope
 * readings) fill the gaps. Rebuilt only when Photo_links or an import changes.
 */
export function photoIndex(store) {
  const db = store.db;
  initPhotoTables(db);
  const state = db
    .prepare("SELECT count(*) n, max(updated_at) u FROM records WHERE sheet='Photo_links' AND missing=0")
    .get();
  const stamp = `${state.n}:${state.u}:${reviewRevision(db)}`;
  const hit = indexCache.get(store);
  if (hit?.stamp === stamp) return hit;
  const byCam = new Map();
  const files = new Map();
  let linked = 0;
  const add = (name, id, source) => {
    const parsed = photoName(name);
    if (!parsed || !isFileId(id) || files.has(id)) return;
    // The same file name twice (a copy in another folder): the first listed wins.
    if ([...(byCam.get(parsed.cam)?.[parsed.view] ?? [])].some(f => files.get(f)?.name === parsed.stem)) return;
    files.set(id, { name: parsed.stem, cam: parsed.cam, view: parsed.view, source });
    const entry =
      byCam.get(parsed.cam) ?? byCam.set(parsed.cam, { dorsal: [], ventral: [], other: [] }).get(parsed.cam);
    entry[parsed.view].push(id);
  };
  for (const r of db
    .prepare("SELECT values_json FROM records WHERE sheet='Photo_links' AND missing=0 AND observed=1 AND row_num>1")
    .all()) {
    const values = parse(r.values_json) ?? {};
    const before = files.size;
    add(values.Name, driveId(values.URL), 'Photo_links');
    if (files.size > before) linked++;
  }
  for (const r of db.prepare('SELECT name, file_id FROM envelope_readings WHERE file_id IS NOT NULL').all())
    add(r.name, r.file_id, 'manifest');
  const byName = new Map([...files].map(([id, f]) => [f.name.toUpperCase(), id]));
  for (const entry of byCam.values())
    for (const view of ['dorsal', 'ventral', 'other'])
      entry[view].sort((a, b) => files.get(a).name.localeCompare(files.get(b).name, 'en', { numeric: true }));
  const out = { stamp, byCam, files, byName, linked };
  indexCache.set(store, out);
  return out;
}

const dataCache = new WeakMap();
/** Everything imported, keyed for the checks; rebuilt after each import. */
export function reviewData(store) {
  const db = store.db;
  const stamp = reviewRevision(db);
  const hit = dataCache.get(store);
  if (hit?.stamp === stamp) return hit;
  const readings = new Map();
  const readingsByCam = new Map();
  for (const r of db.prepare('SELECT * FROM envelope_readings').all()) {
    const reading = {
      name: r.name,
      fileId: r.file_id,
      cam: r.cam,
      view: r.view,
      bbox: parse(r.bbox_json),
      turned: r.turned,
      size: parse(r.size_json),
      camid: r.camid,
      conf: r.camid_conf,
      candidates: parse(r.candidates_json) ?? [],
      text: parse(r.text_json) ?? [],
      model: r.model,
      source: r.source,
    };
    readings.set(r.name.toUpperCase(), reading);
    (readingsByCam.get(r.cam) ?? readingsByCam.set(r.cam, []).get(r.cam)).push(reading);
  }
  const flags = db
    .prepare('SELECT * FROM photo_flags ORDER BY cam, type')
    .all()
    .map(f => ({
      id: f.id,
      cam: f.cam,
      type: f.type,
      stratum: f.stratum,
      strength: f.strength,
      photos: parse(f.photos_json) ?? [],
      data: parse(f.data_json) ?? {},
      decision: f.decision,
      target: f.target,
      database: f.database_value,
      action: f.action,
      decidedBy: f.decided_by,
    }));
  const boxes = new Map(
    db
      .prepare('SELECT name, box_json, conf FROM photo_wing_boxes')
      .all()
      .map(b => [b.name.toUpperCase(), parse(b.box_json)]),
  );
  const predictions = new Map(
    db
      .prepare('SELECT * FROM photo_predictions')
      .all()
      .map(p => [
        p.cam,
        {
          species: parse(p.species_json) ?? [],
          genus: parse(p.genus_json) ?? [],
          subspecies: parse(p.subspecies_json) ?? [],
          sex: p.sex
            ? { sex: p.sex, confidence: p.sex_conf, supported: !!p.sex_supported, species: p.sex_species }
            : null,
          ...(p.andes ? { andes: p.andes } : {}),
        },
      ]),
  );
  const out = { stamp, readings, readingsByCam, flags, boxes, predictions, wings: wingSides(store) };
  dataCache.set(store, out);
  return out;
}

const wingCache = new WeakMap();
/** The wing models' reading of the photos alone (a Photo_links reading needs only these), kept until an import. */
export function wingSides(store) {
  const stamp = reviewRevision(store.db);
  const hit = wingCache.get(store);
  if (hit?.stamp === stamp) return hit.index;
  const index = wingIndex(store.db);
  wingCache.set(store, { stamp, index });
  return index;
}

/**
 * The wing models' reading of each photo (photo_wing_sides, scripts/import-wing-sides.mjs): which
 * side its wings show, how sure, whether the photo is turned, and what the pair of a CAM's photos
 * says. { byCam: Map CAM → rows, byFile: Map Drive ID → row, byName: Map NAME → rows }.
 */
function wingIndex(db) {
  const byCam = new Map();
  const byFile = new Map();
  const byName = new Map();
  for (const r of db.prepare('SELECT * FROM photo_wing_sides ORDER BY name').all()) {
    const row = {
      name: r.name,
      fileId: r.file_id,
      cam: r.cam,
      suffix: r.suffix,
      side: r.side,
      pDorsal: r.p_dorsal,
      pForewings: r.p_forewings,
      pHindwings: r.p_hindwings,
      wings: r.wings,
      turn: r.turn,
      degrees: r.degrees,
      pair: r.pair,
      reasons: r.reasons,
      camReasons: r.cam_reasons,
      review: !!r.review,
      folder: r.folder,
      source: r.source,
    };
    if (row.cam) (byCam.get(row.cam) ?? byCam.set(row.cam, []).get(row.cam)).push(row);
    if (row.fileId) byFile.set(row.fileId, row);
    const key = row.name.toUpperCase();
    (byName.get(key) ?? byName.set(key, []).get(key)).push(row);
  }
  return { byCam, byFile, byName };
}

/** A photo's view by the end of its name: "CAM070046d2" → 'd', "CAM070046v" → 'v', else null. */
export const nameView = name => /^[\w-]*?\d[_\s-]?([dv])\d*(?:\s*\(\d+\))?$/i.exec(String(name ?? '').replace(/\.[a-z0-9]+$/i, '').trim())?.[1]?.toLowerCase() ?? null;

/**
 * What the wings of a photo say against its name now (`name`, e.g. after a rename): `side` when its
 * wings show the other side than its d/v (decided: P(dorsal) ≥ 0.95 or ≤ 0.05), `turn` when the photo
 * is turned; null when neither. The pair's verdict (`pair`: "suffix swapped", "both photos same side")
 * goes with it.
 */
export function wingProblem(row, name = row?.name) {
  if (!row) return null;
  const view = nameView(name);
  const says = row.side === 'dorsal' || row.side === 'ventral' ? row.side[0] : null;
  const side = view && says && view !== says;
  if (!side && !row.turn) return null;
  return {
    ...(side ? { side: row.side, pDorsal: row.pDorsal } : {}),
    ...(row.turn ? { turn: row.turn, degrees: row.degrees } : {}),
    ...(row.pair ? { pair: row.pair } : {}),
  };
}

/** The envelope of a CAM as a crop of one photo: the photo named first, else the clearest reading. */
export function envelopeOf(data, index, cam, preferNames = []) {
  const list = (data.readingsByCam.get(cam) ?? []).filter(r => r.bbox && r.size);
  const preferred = preferNames.map(n => data.readings.get(String(n).toUpperCase())).find(r => r?.bbox && r.size);
  const reading = preferred ?? [...list].sort((a, b) => (b.conf ?? 0) - (a.conf ?? 0))[0];
  if (!reading) return null;
  const fileId = index.byName.get(reading.name.toUpperCase()) ?? reading.fileId;
  if (!fileId) return null;
  const [w, h] = reading.size;
  const [x0, y0, x1, y1] = reading.bbox;
  const round = n => Math.round(Math.min(Math.max(n, 0), 1) * 10000) / 10000;
  return {
    reading,
    envelope: {
      fileId,
      name: reading.name,
      bbox: [round(x0 / w), round(y0 / h), round(x1 / w), round(y1 / h)],
      turned: reading.turned || 0,
      aspect: Math.round((w / h) * 10000) / 10000,
    },
  };
}

/**
 * What an issue about a CAM shows: its photos (with the wing box of each, for
 * the thumbnails), the envelope crop, the envelope text and CAM read, and the
 * gallery's prediction. Null when nothing is known about the CAM.
 */
export function photoContext(data, index, cam, preferNames = []) {
  const entry = index.byCam.get(cam);
  const found = envelopeOf(data, index, cam, preferNames);
  const prediction = data.predictions.get(cam);
  if (!entry && !found && !prediction) return null;
  const files = {};
  for (const id of [...(entry?.dorsal ?? []), ...(entry?.ventral ?? []), ...(entry?.other ?? [])]) {
    const f = index.files.get(id);
    const wings = data.boxes.get(f.name.toUpperCase());
    files[id] = { name: f.name, ...(wings ? { wings } : {}) };
  }
  const out = {
    photos: {
      dorsal: entry?.dorsal ?? [],
      ventral: entry?.ventral ?? [],
      ...(entry?.other?.length ? { other: entry.other } : {}),
      files,
      ...(found ? { envelope: found.envelope } : {}),
    },
  };
  if (found) {
    const lines = found.reading.text.map(l => (Array.isArray(l) ? l[0] : l)).filter(Boolean);
    if (lines.length) out.envelopeText = lines.join(' / ');
    if (found.reading.camid) out.envelopeCamid = found.reading.camid;
  }
  if (prediction)
    out.prediction = {
      species: prediction.species.slice(0, 3),
      ...(prediction.genus[0] ? { genus: prediction.genus[0] } : {}),
      ...(prediction.sex ? { sex: prediction.sex } : {}),
    };
  return out;
}
