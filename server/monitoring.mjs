import { createHash, randomUUID } from 'node:crypto';
// The pairing of walk points with sheet rows is shared with the browser (Node strips the types).
import * as lib from '../frontend/src/lib/monitoring.ts';

/**
 * GPS tracks and capture points of the Ikiam monitoring walks (Wikiloc GPX
 * exports). They are kept in the app database, not in the workbook: the
 * Collection_data sheet has no columns for per-capture coordinates.
 */

const MAX_POINTS = 100_000;
const MAX_CAPTURES = 1000;
const fail = (code, message, status = 400) => Object.assign(new Error(message), { code, status });

export function initMonitoring(db) {
  db.exec(`CREATE TABLE IF NOT EXISTS monitoring_tracks(id TEXT PRIMARY KEY, request_id TEXT UNIQUE, fingerprint TEXT UNIQUE NOT NULL,
    date TEXT NOT NULL, collector TEXT, name TEXT NOT NULL, data_json TEXT NOT NULL, created_by TEXT NOT NULL, created_at TEXT NOT NULL);
    CREATE TABLE IF NOT EXISTS wikiloc_walks(id TEXT PRIMARY KEY, wikiloc_id TEXT UNIQUE NOT NULL, url TEXT NOT NULL, name TEXT NOT NULL,
      date TEXT, data_json TEXT NOT NULL, status TEXT NOT NULL, track_id TEXT, created_by TEXT NOT NULL, created_at TEXT NOT NULL);
    CREATE TABLE IF NOT EXISTS monitoring_photos(id TEXT PRIMARY KEY, walk_id TEXT NOT NULL, mime_type TEXT NOT NULL, data BLOB NOT NULL,
      source_url TEXT NOT NULL, created_at TEXT NOT NULL);
    CREATE TABLE IF NOT EXISTS wikiloc_profiles(id TEXT PRIMARY KEY, wikiloc_user TEXT UNIQUE NOT NULL, name TEXT, pattern TEXT NOT NULL,
      added_by TEXT NOT NULL, created_at TEXT NOT NULL, last_checked TEXT);
    CREATE TABLE IF NOT EXISTS wikiloc_jobs(id TEXT PRIMARY KEY, kind TEXT NOT NULL, target TEXT NOT NULL, status TEXT NOT NULL,
      message TEXT, requested_by TEXT NOT NULL, created_at TEXT NOT NULL, updated_at TEXT NOT NULL);`);
  // Added later: the collector whose walks a followed profile holds (e.g. "AA - Alex Arias").
  if (!db.prepare('PRAGMA table_info(wikiloc_profiles)').all().some(c => c.name === 'collector'))
    db.exec('ALTER TABLE wikiloc_profiles ADD COLUMN collector TEXT');
}

const text = (value, max = 300) => {
  if (value === null || value === undefined || value === '') return null;
  if (typeof value !== 'string' && typeof value !== 'number') throw fail('INVALID_TRACK', 'Invalid text value');
  return String(value).slice(0, max);
};
const number = (value, min = -Infinity, max = Infinity) => {
  if (value === null || value === undefined || value === '') return null;
  const n = Number(value);
  if (!Number.isFinite(n) || n < min || n > max) throw fail('INVALID_TRACK', 'Invalid number');
  return n;
};
const coordinate = (lat, lon) => {
  const a = number(lat, -90, 90);
  const b = number(lon, -180, 180);
  if (a === null || b === null) throw fail('INVALID_TRACK', 'Every point needs a latitude and longitude');
  return [a, b];
};

function cleanTrack(points) {
  if (!Array.isArray(points) || points.length > MAX_POINTS) throw fail('INVALID_TRACK', 'Invalid track');
  return points.map(p => {
    if (!Array.isArray(p)) throw fail('INVALID_TRACK', 'Invalid track point');
    const time = text(p[3], 40);
    if (time && !Number.isFinite(Date.parse(time))) throw fail('INVALID_TRACK', 'Invalid track time');
    return [...coordinate(p[0], p[1]), number(p[2], -1000, 10000), time];
  });
}

function cleanCapture(c) {
  if (!c || typeof c !== 'object') throw fail('INVALID_TRACK', 'Invalid capture');
  const [lat, lon] = coordinate(c.lat, c.lon);
  const sex = c.sex === 'female' || c.sex === 'male' ? c.sex : null;
  return {
    lat,
    lon,
    ele: number(c.ele, -1000, 10000),
    text: text(c.text, 500) || '',
    seq: number(c.seq, 0, 100000),
    species: text(c.species, 120),
    subspecies: text(c.subspecies, 120),
    sex,
    minutes: number(c.minutes, 0, 1440),
    height: number(c.height, 0, 100),
    cloud: text(c.cloud, 60),
    markId: text(c.markId, 20),
    recapture: !!c.recapture,
    section: number(c.section, 1, 4),
    photos: cleanPhotoIds(c.photos),
    // The Collection_data row the capture was matched to when stored (for the map popup).
    row: number(c.row, 1, 10_000_000),
    // Its record, which keeps pointing at the butterfly when rows above it are removed.
    recordId: text(c.recordId, 80),
    // Chosen by a person (a row, or "none": no row): matching again never changes it.
    link: c.link === 'manual' || c.link === 'none' ? c.link : null,
  };
}

function cleanPhotoIds(ids) {
  if (ids === undefined || ids === null) return [];
  if (!Array.isArray(ids) || ids.length > 20 || ids.some(id => typeof id !== 'string' || !/^\d{1,20}$/.test(id)))
    throw fail('INVALID_TRACK', 'Invalid photo list');
  return ids;
}

const fromRow = row => ({
  id: row.id,
  date: row.date,
  collector: row.collector,
  name: row.name,
  createdBy: row.created_by,
  createdAt: row.created_at,
  ...JSON.parse(row.data_json),
});

/** Stored tracks with their captures linked to the current sheet rows; `canDelete` for the person asking. */
export function listTracks(store, user = null) {
  const tracks = store.db
    .prepare('SELECT * FROM monitoring_tracks ORDER BY date DESC, created_at DESC')
    .all()
    .map(row => ({ ...fromRow(row), canDelete: canDeleteTrack(store, row, user) }));
  return relinkCaptures(store, tracks);
}

// ------------------------------------------------ captures and their sheet rows

const EPOCH = Date.UTC(1899, 11, 30);
const serialOf = iso => Math.round((Date.parse(`${iso}T00:00:00Z`) - EPOCH) / 864e5);
const clean = v => (v === null || v === undefined ? '' : String(v).trim());
const initialsOf = collector => clean(collector).split(' - ')[0].trim().toUpperCase();
const sexOf = v => {
  const s = clean(v).toLowerCase().replace(/[_\s]*\?$/, '');
  return s === 'female' || s === 'male' ? s : null;
};

/** Collection_data rows by date serial, rebuilt only when the local copy changes. */
const dayIndexes = new WeakMap();
function rowsByDay(store) {
  const state = store.db.prepare("SELECT count(*) n, max(updated_at) u FROM records WHERE sheet='Collection_data'").get();
  const stamp = `${state.n}:${state.u}`;
  const hit = dayIndexes.get(store);
  if (hit?.stamp === stamp) return hit;
  const byDay = new Map();
  const byId = new Map();
  for (const r of store.db
    .prepare("SELECT id,row_num,values_json FROM records WHERE sheet='Collection_data' AND observed=1 AND missing=0 AND row_num<2000000000")
    .all()) {
    const v = JSON.parse(r.values_json);
    const row = { id: r.id, row: r.row_num, version: 0, observed: true, values: v, formulas: [] };
    byId.set(r.id, row);
    if (typeof v.Collection_date === 'number') (byDay.get(v.Collection_date) || byDay.set(v.Collection_date, []).get(v.Collection_date)).push(row);
  }
  // Names to read the notes with (built once per state of the sheet, when first needed).
  let names = null;
  const taxa = () => {
    if (!names) {
      const all = [...byId.values()];
      names = { taxa: lib.taxaFrom(all), local: lib.taxaFrom(all.filter(r => /^ikiam$/i.test(clean(r.values.Collection_location)))) };
    }
    return names;
  };
  const entry = { stamp, byDay, byId, taxa };
  dayIndexes.set(store, entry);
  return entry;
}

/** The rows of a walk's day, of its collector (every row of the day when the walk has none). */
function dayRows(index, date, collector) {
  const who = initialsOf(collector);
  return (index.byDay.get(serialOf(date)) || []).filter(r => !who || initialsOf(r.values.Collector) === who);
}

/** Whether a sheet row is still the capture's butterfly: same day, and mark, minute, species and sex that agree. */
function holds(row, capture, serial) {
  const v = row.values;
  if (v.Collection_date !== serial) return false;
  if (capture.markId && clean(v.FieldMark_ID).toUpperCase() !== capture.markId.toUpperCase()) return false;
  const minute = typeof v.Collection_time === 'number' ? Math.round(v.Collection_time * 1440) : null;
  // The same mark that day is the butterfly even with another hour written (a common slip).
  if (!capture.markId && capture.minutes !== null && minute !== null && Math.abs(minute - capture.minutes) > lib.TIME_TOLERANCE)
    return false;
  const species = clean(v.SPECIES).toLowerCase();
  if (capture.species && species && species !== capture.species.toLowerCase()) return false;
  const sex = sexOf(v.Sex);
  return !(capture.sex && sex && sex !== capture.sex);
}

/**
 * A link by record still holds while the record is of that day and keeps the
 * capture's mark: the record is the butterfly even when its minute, species or
 * sex are corrected later (a row number, instead, may now be another butterfly).
 */
function recordHolds(row, capture, serial) {
  if (row.values.Collection_date !== serial) return false;
  return !capture.markId || !lib.hasMark(row) || clean(row.values.FieldMark_ID).toUpperCase() === capture.markId.toUpperCase();
}

/** A capture shown with its row's curated species, sex, mark and section (the note may lack them). */
function withRow(c, row) {
  if (!row) return { ...c, row: null, recordId: null };
  const v = row.values;
  const section = Number(clean(v.Transect_section));
  return {
    ...c,
    species: clean(v.SPECIES) || c.species,
    subspecies: clean(v.Subspecies_Form) || c.subspecies,
    sex: sexOf(v.Sex) || c.sex,
    markId: lib.hasMark(row) ? clean(v.FieldMark_ID).toUpperCase() : c.markId,
    section: section >= 1 && section <= 4 ? section : c.section,
    row: row.row,
    recordId: row.id,
  };
}

/**
 * The stored captures point at Collection_data rows (for the map popup and the
 * recapture photos). When rows are removed or moved in the sheet, a stored row
 * number points at another butterfly: each capture is checked against its row
 * and, if it no longer holds it, found again with the shared matcher
 * (lib.matchWalk), taking only pairings by mark or sure ones. Captures stored
 * before their rows were saved (an import) are found the same way. A link
 * chosen by a person stays while its record exists. Nothing is written; the
 * links are fixed on every read.
 */
export function relinkCaptures(store, tracks) {
  const index = rowsByDay(store);
  const { byId } = index;
  const byRow = new Map();
  for (const r of byId.values()) byRow.set(r.row, r);
  for (const t of tracks) {
    const serial = serialOf(t.date);
    const taken = new Set();
    const open = [];
    t.captures = t.captures.map((c, i) => {
      if (c.link === 'none') return { ...c, row: null, recordId: null };
      const record = c.recordId && byId.get(c.recordId);
      const linked = record || (c.row && byRow.get(c.row));
      const kept =
        linked &&
        !taken.has(linked.id) &&
        (c.link === 'manual' ? linked === record : record ? recordHolds(record, c, serial) : holds(linked, c, serial));
      if (kept) {
        taken.add(linked.id);
        return withRow(c, linked);
      }
      open.push(i);
      return { ...c, row: null, recordId: null };
    });
    const free = dayRows(index, t.date, t.collector).filter(r => !taken.has(r.id));
    if (!open.length || !free.length) continue;
    const { taxa, local } = index.taxa();
    const { points, groups } = lib.storedPoints(
      open.map(i => t.captures[i]),
      taxa,
      local,
    );
    const match = lib.matchWalk(free, t.date, t.collector || '', points, { shift: false });
    groups.forEach((g, p) => {
      const m = match.matches[p];
      // Gone from the sheet, or not sure: no row rather than the wrong one.
      if (m.confidence !== 'mark' && m.confidence !== 'sure') return;
      g.forEach((k, n) => {
        if (m.rows[n]) t.captures[open[k]] = withRow(t.captures[open[k]], m.rows[n]);
      });
    });
  }
  return tracks;
}

// ------------------------------------------------ matching again, and the doubts

const rowInfo = row =>
  row && {
    recordId: row.id,
    row: row.row,
    species: clean(row.values.SPECIES) || null,
    subspecies: clean(row.values.Subspecies_Form) || null,
    sex: sexOf(row.values.Sex),
    minutes: typeof row.values.Collection_time === 'number' ? Math.round(row.values.Collection_time * 1440) : null,
    markId: lib.hasMark(row) ? clean(row.values.FieldMark_ID).toUpperCase() : null,
    kind: clean(row.values.Release_Collect) || null,
    section: clean(row.values.Transect_section) || null,
  };

/**
 * Rows of the day within ten minutes of the point (or of its row), so a person
 * can pick another one when the note and its row disagree (e.g. a mark written
 * on the next row of the sheet).
 */
function nearby(day, point, match) {
  const minute = r => (typeof r.values.Collection_time === 'number' ? Math.round(r.values.Collection_time * 1440) : null);
  const at = point.timeFromTrack ? null : point.minutes ?? (match.rows[0] ? minute(match.rows[0]) : null);
  if (at === null) return [];
  return day
    .filter(r => minute(r) !== null && Math.abs(minute(r) - at) <= 10)
    .sort((a, b) => Math.abs(minute(a) - at) - Math.abs(minute(b) - at))
    .slice(0, 6);
}

/** Public (wklcdn) addresses of the photos, for the message to a collector. */
function photoLinks(store, ids) {
  if (!ids?.length) return [];
  const get = store.db.prepare('SELECT source_url FROM monitoring_photos WHERE id=?');
  return ids.map(id => get.get(String(id))?.source_url).filter(Boolean);
}

/**
 * Every stored walk paired again with the shared matcher (links chosen by a
 * person are kept), compared with its links as they are read now. Returns, per
 * stored capture, what would change, and the doubtful points (ties, points
 * placed by order, notes that disagree with their row, and the changes). Also
 * the walks still waiting in "por revisar" that have rows that day, with each
 * point's proposed row, to be paired by a person before going on the map.
 * Nothing is written.
 */
export function rematchTracks(store, user = null) {
  const index = rowsByDay(store);
  const { taxa, local } = index.taxa();
  const tracks = listTracks(store, user);
  const changes = [];
  const doubts = [];
  const walks = [];
  for (const t of tracks) {
    const day = dayRows(index, t.date, t.collector);
    const { points, groups } = lib.storedPoints(t.captures, taxa, local);
    // Kept as a person chose them: the whole point was chosen (every stored copy).
    const fixed = new Map();
    groups.forEach((g, p) => {
      if (g.every(i => t.captures[i].link === 'manual' || t.captures[i].link === 'none'))
        fixed.set(p, g.flatMap(i => (t.captures[i].link === 'manual' && t.captures[i].recordId ? [t.captures[i].recordId] : [])));
    });
    const match = lib.matchWalk(day, t.date, t.collector || '', points, { fixed, shift: false });
    let count = 0;
    groups.forEach((g, p) => {
      const m = match.matches[p];
      const next = m.rows.map(r => r.id);
      const current = g.map(i => t.captures[i].recordId || null);
      // Copies keep the row they have when it is still one of the point's rows.
      const spare = next.filter(id => !current.includes(id));
      const proposed = current.map(id => (id && next.includes(id) ? id : (spare.shift() ?? null)));
      const changed = !m.manual && proposed.some((id, k) => id !== current[k]);
      g.forEach((i, k) => {
        if (!m.manual && proposed[k] !== current[k])
          changes.push({
            trackId: t.id,
            index: i,
            from: current[k],
            to: proposed[k],
            date: t.date,
            collector: t.collector,
            name: t.name,
            text: t.captures[i].text,
            confidence: m.confidence,
            before: rowInfo(current[k] && index.byId.get(current[k])),
            after: rowInfo(proposed[k] && index.byId.get(proposed[k])),
          });
      });
      if (!changed && !lib.doubtfulMatch(m)) return;
      count++;
      const c = t.captures[g[0]];
      doubts.push({
        source: 'track',
        trackId: t.id,
        indexes: g,
        date: t.date,
        collector: t.collector,
        name: t.name,
        wikiloc: t.wikiloc?.url || null,
        text: c.text,
        minutes: points[p].timeFromTrack ? null : points[p].minutes,
        photos: c.photos || [],
        photoLinks: photoLinks(store, c.photos),
        confidence: m.confidence,
        conflicts: m.conflicts,
        changed,
        current: current.map(id => rowInfo(id && index.byId.get(id))),
        proposed: proposed.map(id => rowInfo(id && index.byId.get(id))),
        candidates: [...new Map([...m.rows, ...m.candidates, ...nearby(day, points[p], m)].map(r => [r.id, r])).values()].map(rowInfo),
      });
    });
    if (count) walks.push({ source: 'track', id: t.id, date: t.date, collector: t.collector, name: t.name, doubts: count });
  }
  // Walks waiting for review whose points do not all pair surely (e.g. old notes without times).
  for (const w of listWalks(store).filter(w => w.status === 'waiting' && w.date && w.collector)) {
    const captures = w.waypoints.map(p => lib.locateCapture({ ...p, time: null }, taxa, local));
    const match = lib.matchWalk([...index.byId.values()], w.date, w.collector, captures);
    const day = dayRows(index, match.date, w.collector);
    if (!day.length || !match.matches.some(m => lib.doubtfulMatch(m) || m.confidence === 'none')) continue;
    if (!match.matches.some(m => m.rows.length || m.candidates.length)) continue;
    match.matches.forEach((m, i) => {
      const used = new Set(match.matches.flatMap((o, j) => (j === i ? [] : o.rows.map(r => r.id))));
      doubts.push({
        source: 'walk',
        walkId: w.id,
        indexes: [i],
        date: match.date,
        collector: w.collector,
        name: w.name,
        wikiloc: w.url,
        text: captures[i].text,
        minutes: captures[i].minutes,
        photos: captures[i].photos,
        photoLinks: photoLinks(store, captures[i].photos),
        confidence: m.confidence,
        conflicts: m.conflicts,
        changed: false,
        current: [],
        proposed: m.rows.map(rowInfo),
        // Any row of that day not proposed for another point.
        candidates: [...new Map([...m.rows, ...m.candidates, ...day.filter(r => !used.has(r.id))].map(r => [r.id, r])).values()]
          .sort((a, b) => (rowInfo(a).minutes ?? 1e6) - (rowInfo(b).minutes ?? 1e6) || a.row - b.row)
          .map(rowInfo),
      });
    });
    walks.push({ source: 'walk', id: w.id, date: match.date, collector: w.collector, name: w.name, doubts: captures.length });
  }
  return { changes, doubts, walks };
}

/** Applies the changes of rematchTracks that are still the same now (each named by track, capture, from and to). */
export function applyRematch(store, body) {
  const wanted = Array.isArray(body.changes) ? body.changes : [];
  if (!wanted.length || wanted.length > 5000) throw fail('INVALID_CHANGES', 'Give the changes to apply');
  const key = c => `${c.trackId}|${c.index}|${c.from ?? ''}|${c.to ?? ''}`;
  const asked = new Set(wanted.map(key));
  const { changes } = rematchTracks(store);
  const index = rowsByDay(store);
  const byTrack = new Map();
  for (const c of changes) if (asked.has(key(c))) byTrack.set(c.trackId, [...(byTrack.get(c.trackId) || []), c]);
  let applied = 0;
  for (const [trackId, list] of byTrack) {
    const row = store.db.prepare('SELECT * FROM monitoring_tracks WHERE id=?').get(trackId);
    if (!row) continue;
    const data = JSON.parse(row.data_json);
    for (const c of list) {
      const capture = data.captures[c.index];
      if (!capture) continue;
      data.captures[c.index] = { ...withRow(capture, c.to ? index.byId.get(c.to) : null), link: null };
      applied++;
    }
    store.db.prepare('UPDATE monitoring_tracks SET data_json=? WHERE id=?').run(JSON.stringify(data), trackId);
  }
  return { applied, skipped: wanted.length - applied };
}

/**
 * A person pairs one stored capture with a row (or says it is none of them,
 * `recordId` null). Kept as chosen: matching again never changes it.
 */
export function linkCapture(store, trackId, body) {
  const row = store.db.prepare('SELECT * FROM monitoring_tracks WHERE id=?').get(trackId);
  if (!row) throw fail('TRACK_NOT_FOUND', 'Track not found', 404);
  const data = JSON.parse(row.data_json);
  const i = Number(body.index);
  if (!Number.isInteger(i) || !data.captures[i]) throw fail('INVALID_LINK', 'Unknown capture');
  const index = rowsByDay(store);
  let target = null;
  if (body.recordId) {
    target = index.byId.get(String(body.recordId));
    if (!target || target.values.Collection_date !== serialOf(row.date)) throw fail('INVALID_LINK', 'That row is not of the walk’s day');
    // The row leaves any other capture of the walk that had it.
    data.captures.forEach((c, k) => {
      if (k !== i && c.recordId === target.id) data.captures[k] = { ...c, row: null, recordId: null, link: null };
    });
  }
  data.captures[i] = { ...withRow(data.captures[i], target), link: target ? 'manual' : 'none' };
  store.db.prepare('UPDATE monitoring_tracks SET data_json=? WHERE id=?').run(JSON.stringify(data), trackId);
  return { track: relinkCaptures(store, [fromRow(store.db.prepare('SELECT * FROM monitoring_tracks WHERE id=?').get(trackId))])[0] };
}

/**
 * A waiting Wikiloc walk reviewed in "Dudas": each point with the rows a person
 * chose (none for a point that is not a butterfly of the sheet), stored on the
 * map with those links kept as chosen.
 */
export function storeReviewedWalk(store, walkId, body, user) {
  const walk = store.db.prepare('SELECT * FROM wikiloc_walks WHERE id=?').get(walkId);
  if (!walk) throw fail('WALK_NOT_FOUND', 'Wikiloc walk not found', 404);
  const data = JSON.parse(walk.data_json);
  const date = text(body.date, 10) || walk.date;
  const collector = text(body.collector, 120) || data.collector;
  if (!date || !collector) throw fail('INVALID_WALK', 'The walk needs a date and a collector');
  const links = Array.isArray(body.links) ? body.links : [];
  if (links.length !== data.waypoints.length) throw fail('INVALID_LINK', 'Give the rows of every point');
  const index = rowsByDay(store);
  const { taxa, local } = index.taxa();
  const seen = new Set();
  const points = data.waypoints.map(p => lib.locateCapture({ ...p, time: null }, taxa, local));
  // New mark or recapture, from the marks before the walk (as Importar recorrido does).
  const roles = lib.walkMarkRoles([...index.byId.values()], date, points);
  const captures = data.waypoints.flatMap((p, i) => {
    const c = points[i];
    const ids = Array.isArray(links[i]) ? links[i].map(String) : [];
    const rows = ids.map(id => {
      const r = index.byId.get(id);
      if (!r || r.values.Collection_date !== serialOf(date) || seen.has(id)) throw fail('INVALID_LINK', 'A chosen row is not of that day, or chosen twice');
      seen.add(id);
      return r;
    });
    const base = { ...c, recapture: roles[i]?.role === 'recapture' };
    return rows.length ? rows.map(r => ({ ...withRow(base, r), link: 'manual' })) : [{ ...base, row: null, recordId: null, link: 'none' }];
  });
  return saveTrack(store, { requestId: text(body.requestId, 80) || randomUUID(), date, collector, name: walk.name, track: data.track, wikilocWalkId: walk.id, captures }, user);
}

/**
 * Saves a track once: the same file uploaded again returns the stored copy,
 * with its captures' row links refreshed. A Wikiloc walk reviewed again
 * replaces its earlier track.
 */
export function saveTrack(store, body, user) {
  const date = text(body.date, 10);
  if (!date || !/^\d{4}-\d{2}-\d{2}$/.test(date) || !Number.isFinite(Date.parse(date)))
    throw fail('INVALID_TRACK', 'A date (YYYY-MM-DD) is required');
  const track = cleanTrack(body.track);
  if (!Array.isArray(body.captures) || body.captures.length > MAX_CAPTURES)
    throw fail('INVALID_TRACK', 'Invalid capture list');
  const captures = body.captures.map(cleanCapture);
  if (!track.length && !captures.length) throw fail('INVALID_TRACK', 'The file has no track or waypoints');
  const name = text(body.name, 200) || `Monitoreo ${date}`;
  const collector = text(body.collector, 120);
  const walk = body.wikilocWalkId ? store.db.prepare('SELECT * FROM wikiloc_walks WHERE id=?').get(String(body.wikilocWalkId)) : null;
  if (body.wikilocWalkId && !walk) throw fail('WALK_NOT_FOUND', 'Wikiloc walk not found', 404);
  const fingerprint = createHash('sha256')
    .update(JSON.stringify([date, track, captures.map(c => [c.lat, c.lon, c.text])]))
    .digest('hex');
  const replay = store.db.prepare('SELECT * FROM monitoring_tracks WHERE request_id=?').get(body.requestId);
  if (replay) return { track: fromRow(replay), duplicate: true };
  const existing =
    (walk?.track_id && store.db.prepare('SELECT * FROM monitoring_tracks WHERE id=?').get(walk.track_id)) ||
    store.db.prepare('SELECT * FROM monitoring_tracks WHERE fingerprint=?').get(fingerprint);
  if (existing) {
    // Reviewed again: the captures (rows found, recaptures) are the new ones. A GPX
    // track with GPS times is not replaced by the Wikiloc page's track, which has none.
    const old = JSON.parse(existing.data_json);
    const timed = points => points.some(p => p[3]);
    // A point a person paired by hand keeps that pairing (same note at the same place).
    const chosen = new Map(
      (old.captures || []).filter(c => c.link).map(c => [`${c.text}|${c.lat}|${c.lon}`, c]),
    );
    const kept = captures.map(c => {
      const was = !c.link && chosen.get(`${c.text}|${c.lat}|${c.lon}`);
      if (!was) return c;
      chosen.delete(`${c.text}|${c.lat}|${c.lon}`);
      return { ...c, row: was.row, recordId: was.recordId, link: was.link };
    });
    const data = { ...old, track: timed(old.track || []) && !timed(track) ? old.track : track, captures: kept };
    if (walk) data.wikiloc = { id: walk.wikiloc_id, url: walk.url };
    const clash = store.db.prepare('SELECT id FROM monitoring_tracks WHERE fingerprint=? AND id<>?').get(fingerprint, existing.id);
    store.db
      // The latest request id, so a retry of this request is recognised.
      .prepare('UPDATE monitoring_tracks SET request_id=?, date=?, collector=?, name=?, data_json=?, fingerprint=? WHERE id=?')
      .run(body.requestId, date, collector, name, JSON.stringify(data), clash ? existing.fingerprint : fingerprint, existing.id);
    if (walk) markImported(store, walk.id, existing.id);
    return { track: fromRow(store.db.prepare('SELECT * FROM monitoring_tracks WHERE id=?').get(existing.id)), duplicate: true };
  }
  const row = {
    id: randomUUID(),
    request_id: body.requestId,
    fingerprint,
    date,
    collector,
    name,
    data_json: JSON.stringify({ track, captures, wikiloc: walk ? { id: walk.wikiloc_id, url: walk.url } : null }),
    created_by: user.username,
    created_at: new Date().toISOString(),
  };
  store.db
    .prepare(
      'INSERT INTO monitoring_tracks(id,request_id,fingerprint,date,collector,name,data_json,created_by,created_at) VALUES(?,?,?,?,?,?,?,?,?)',
    )
    .run(...Object.values(row));
  if (walk) markImported(store, walk.id, row.id);
  return { track: fromRow(row), duplicate: false };
}

/**
 * Who may remove a stored track: the person who uploaded it or brought its
 * Wikiloc walk into the app, a reviewer or an administrator. Not every editor:
 * a GPX track (with its GPS times) cannot be brought back from Wikiloc, and the
 * track is someone else's walk on the map and in the report's distances.
 */
export function canDeleteTrack(store, track, user) {
  if (!user || !['editor', 'reviewer', 'admin'].includes(user.role)) return false;
  if (['reviewer', 'admin'].includes(user.role) || track.created_by === user.username) return true;
  return !!store.db.prepare('SELECT 1 FROM wikiloc_walks WHERE track_id=? AND created_by=?').get(track.id, user.username);
}

/** Removes a track from the map; its Wikiloc walk goes back to "por revisar". */
export function deleteTrack(store, id, user) {
  const row = store.db.prepare('SELECT * FROM monitoring_tracks WHERE id=?').get(id);
  if (!row) throw fail('TRACK_NOT_FOUND', 'Track not found', 404);
  if (!canDeleteTrack(store, row, user))
    throw fail('FORBIDDEN', 'Only the uploader, a reviewer or an administrator can remove this track', 403);
  store.db.prepare('DELETE FROM monitoring_tracks WHERE id=?').run(id);
  store.db.prepare("UPDATE wikiloc_walks SET status='waiting', track_id=NULL WHERE track_id=?").run(id);
  return { ok: true };
}

// ------------------------------------------------------------------ Wikiloc

/*
 * Walks read from public Wikiloc trail pages by the helper in tools/wikiloc
 * (Wikiloc has no API and blocks servers with Cloudflare, so the page is read
 * by a browser on a home connection). They wait here, with their photos,
 * until someone reviews them in "Importar recorrido".
 */
const PHOTO_HOST = /^https:\/\/s\d*\.wklcdn\.com\/image_\d+\/[\w/]+\.jpe?g$/i;
const MAX_PHOTO = 8 * 1024 * 1024;
const walkRow = row => {
  const data = JSON.parse(row.data_json);
  return {
    id: row.id,
    wikilocId: row.wikiloc_id,
    url: row.url,
    name: row.name,
    date: row.date,
    status: row.status,
    trackId: row.track_id,
    createdBy: row.created_by,
    createdAt: row.created_at,
    ...data,
  };
};
function markImported(store, walkId, trackId) {
  store.db.prepare("UPDATE wikiloc_walks SET status='imported', track_id=? WHERE id=?").run(trackId, walkId);
}

export function listWalks(store) {
  return store.db.prepare('SELECT * FROM wikiloc_walks ORDER BY date DESC, created_at DESC').all().map(walkRow);
}

async function fetchPhoto(url) {
  const response = await fetch(url, { signal: AbortSignal.timeout(30_000), redirect: 'error' });
  if (!response.ok) throw new Error(`photo ${response.status}`);
  const type = response.headers.get('content-type') || '';
  if (!/^image\/(jpeg|png|webp)$/.test(type.split(';')[0].trim())) throw new Error('not an image');
  const data = Buffer.from(await response.arrayBuffer());
  if (data.length > MAX_PHOTO) throw new Error('photo too large');
  return { type: type.split(';')[0].trim(), data };
}

/**
 * Stores (or refreshes) a walk read from Wikiloc and downloads its photos.
 * The same trail sent again replaces the waiting copy; photos already stored
 * are not downloaded twice.
 */
export async function saveWalk(store, body, user, { fetchImage = fetchPhoto } = {}) {
  const url = text(body.url, 500);
  const wikilocId = /(\d{6,12})\/?(?:[?#].*)?$/.exec(url || '')?.[1];
  if (!url || !/^https:\/\/([a-z]{2}\.)?wikiloc\.com\//.test(url) || !wikilocId)
    throw fail('INVALID_WALK', 'A Wikiloc trail URL is required');
  const date = text(body.date, 10);
  if (date && (!/^\d{4}-\d{2}-\d{2}$/.test(date) || !Number.isFinite(Date.parse(date))))
    throw fail('INVALID_WALK', 'Invalid date');
  const track = cleanTrack(body.track || []);
  if (!Array.isArray(body.waypoints) || body.waypoints.length > MAX_CAPTURES)
    throw fail('INVALID_WALK', 'Invalid waypoint list');
  const waypoints = [];
  const failed = [];
  for (const w of body.waypoints) {
    if (!w || typeof w !== 'object') throw fail('INVALID_WALK', 'Invalid waypoint');
    const [lat, lon] = coordinate(w.lat, w.lon);
    const urls = Array.isArray(w.photos) ? w.photos.slice(0, 20) : [];
    const photos = [];
    for (const photoUrl of urls) {
      const id = /\/(\d+)(?:Master)?\.jpe?g$/i.exec(String(photoUrl))?.[1];
      if (!PHOTO_HOST.test(String(photoUrl)) || !id) throw fail('INVALID_WALK', 'Photos must come from wklcdn.com');
      if (!store.db.prepare('SELECT 1 FROM monitoring_photos WHERE id=?').get(id)) {
        try {
          const image = await fetchImage(String(photoUrl));
          store.db
            .prepare('INSERT INTO monitoring_photos(id,walk_id,mime_type,data,source_url,created_at) VALUES(?,?,?,?,?,?)')
            .run(id, wikilocId, image.type, image.data, String(photoUrl), new Date().toISOString());
        } catch (e) {
          failed.push({ url: String(photoUrl), error: String(e.message).slice(0, 120) });
          continue;
        }
      }
      photos.push(id);
    }
    waypoints.push({ lat, lon, ele: number(w.ele, -1000, 10000), text: text(w.text, 500) || '', photos });
  }
  const name = text(body.name, 200) || `Wikiloc ${wikilocId}`;
  // The collector: given by the profile being checked, or known from the trail's author.
  const author = /^\d{3,12}$/.test(String(body.author || '')) ? String(body.author) : null;
  const collector =
    text(body.collector, 120) ||
    (author && store.db.prepare('SELECT collector FROM wikiloc_profiles WHERE wikiloc_user=?').get(author)?.collector) ||
    null;
  const data = JSON.stringify({ track, waypoints, collector, recorded: text(body.recorded, 60), author });
  const existing = store.db.prepare('SELECT * FROM wikiloc_walks WHERE wikiloc_id=?').get(wikilocId);
  if (existing) {
    store.db.prepare('UPDATE wikiloc_walks SET url=?, name=?, date=?, data_json=? WHERE id=?').run(url, name, date, data, existing.id);
    return { walk: walkRow(store.db.prepare('SELECT * FROM wikiloc_walks WHERE id=?').get(existing.id)), failed, updated: true };
  }
  const id = randomUUID();
  store.db
    .prepare(
      "INSERT INTO wikiloc_walks(id,wikiloc_id,url,name,date,data_json,status,track_id,created_by,created_at) VALUES(?,?,?,?,?,?,'waiting',NULL,?,?)",
    )
    .run(id, wikilocId, url, name, date, data, jobRequester(store, body.jobId) || user.username, new Date().toISOString());
  return { walk: walkRow(store.db.prepare('SELECT * FROM wikiloc_walks WHERE id=?').get(id)), failed, updated: false };
}

/**
 * An imported walk back to "por revisar", e.g. after its rows were removed from
 * the sheet or its notes corrected in Wikiloc. Its track stays on the map until
 * the walk is imported again, which then replaces it (saveTrack).
 */
export function reopenWalk(store, id) {
  const row = store.db.prepare('SELECT * FROM wikiloc_walks WHERE id=?').get(id);
  if (!row) throw fail('WALK_NOT_FOUND', 'Wikiloc walk not found', 404);
  store.db.prepare("UPDATE wikiloc_walks SET status='waiting' WHERE id=?").run(id);
  return { walk: walkRow(store.db.prepare('SELECT * FROM wikiloc_walks WHERE id=?').get(id)) };
}

export function deleteWalk(store, id, user) {
  const row = store.db.prepare('SELECT * FROM wikiloc_walks WHERE id=?').get(id);
  if (!row) throw fail('WALK_NOT_FOUND', 'Wikiloc walk not found', 404);
  if (row.created_by !== user.username && !['reviewer', 'admin'].includes(user.role))
    throw fail('FORBIDDEN', 'Only the uploader, a reviewer or an administrator can remove this walk', 403);
  store.db.prepare('DELETE FROM wikiloc_walks WHERE id=?').run(id);
  return { ok: true };
}

export function getPhoto(store, id) {
  return store.db.prepare('SELECT id, mime_type, data FROM monitoring_photos WHERE id=?').get(String(id)) || null;
}

/**
 * Adds the photos of a Wikiloc walk to a track already stored from a GPX of
 * the same walk (matching waypoints by their note), so the GPS times are kept.
 */
export function attachWalkPhotos(store, trackId, walkId) {
  const trackRow = store.db.prepare('SELECT * FROM monitoring_tracks WHERE id=?').get(trackId);
  if (!trackRow) throw fail('TRACK_NOT_FOUND', 'Track not found', 404);
  const walk = store.db.prepare('SELECT * FROM wikiloc_walks WHERE id=?').get(walkId);
  if (!walk) throw fail('WALK_NOT_FOUND', 'Wikiloc walk not found', 404);
  const key = t => String(t || '').toLowerCase().replace(/\s+/g, ' ').trim();
  const photos = new Map(JSON.parse(walk.data_json).waypoints.map(w => [key(w.text), w.photos]));
  const data = JSON.parse(trackRow.data_json);
  let matched = 0;
  for (const c of data.captures) {
    const found = photos.get(key(c.text));
    if (found?.length) {
      c.photos = [...new Set([...(c.photos || []), ...found])];
      matched++;
    }
  }
  data.wikiloc = { id: walk.wikiloc_id, url: walk.url };
  store.db.prepare('UPDATE monitoring_tracks SET data_json=? WHERE id=?').run(JSON.stringify(data), trackId);
  markImported(store, walk.id, trackId);
  return { track: fromRow(store.db.prepare('SELECT * FROM monitoring_tracks WHERE id=?').get(trackId)), matched };
}

// ------------------------------------------------ links and followed profiles

/*
 * People paste Wikiloc links (or share them from the phone), or ask to look
 * for new monitoring trails on followed profiles. The server cannot open
 * Wikiloc, so these become jobs that a computer on a home connection
 * (tools/wikiloc/worker.mjs) claims, runs and reports back.
 */
const now = () => new Date().toISOString();
const STALE_MS = 15 * 60_000;
const jobRow = r => ({
  id: r.id,
  kind: r.kind,
  target: r.target,
  status: r.status,
  message: r.message,
  requestedBy: r.requested_by,
  createdAt: r.created_at,
  updatedAt: r.updated_at,
});
const profileRow = r => ({
  id: r.id,
  wikilocUser: r.wikiloc_user,
  name: r.name,
  pattern: r.pattern,
  addedBy: r.added_by,
  createdAt: r.created_at,
  lastChecked: r.last_checked,
  collector: r.collector || null,
});

/** A Wikiloc trail link, cleaned: https, Wikiloc host, ending in the trail number. */
export function trailUrl(value) {
  const text = String(value || '').trim();
  const match = /https:\/\/(?:[a-z]{2,3}\.)?wikiloc\.com\/[^\s?#]*?-(\d{6,12})(?=[/?#\s]|$)/i.exec(text);
  return match ? { url: match[0], wikilocId: match[1] } : null;
}

function addJob(store, kind, target, user) {
  const open = store.db
    .prepare("SELECT * FROM wikiloc_jobs WHERE kind=? AND target=? AND status IN ('queued','running')")
    .get(kind, target);
  if (open) return jobRow(open);
  const row = { id: randomUUID(), kind, target, status: 'queued', message: null, requested_by: user.username, created_at: now(), updated_at: now() };
  store.db
    .prepare('INSERT INTO wikiloc_jobs(id,kind,target,status,message,requested_by,created_at,updated_at) VALUES(?,?,?,?,?,?,?,?)')
    .run(...Object.values(row));
  return jobRow(row);
}

/**
 * Queues a pasted or shared link (any text containing one is accepted). A walk
 * already in the app is read again; `known` and `walkId` let the screen offer
 * to review an imported one again (reopenWalk).
 */
export function queueLink(store, body, user) {
  const found = trailUrl(body.url || body.text);
  if (!found) throw fail('INVALID_WALK', 'No Wikiloc trail link found');
  const known = store.db.prepare('SELECT id, status, name, date FROM wikiloc_walks WHERE wikiloc_id=?').get(found.wikilocId);
  return {
    job: addJob(store, 'trail', found.url, user),
    known: known?.status || null,
    walkId: known?.id || null,
    name: known?.name || null,
    date: known?.date || null,
  };
}

/** One job per followed profile, to look for monitoring trails not yet in the app. */
export function queueSync(store, user) {
  const profiles = store.db.prepare('SELECT * FROM wikiloc_profiles ORDER BY created_at').all();
  if (!profiles.length) throw fail('NO_PROFILES', 'Add a Wikiloc profile to follow first');
  return { jobs: profiles.map(p => addJob(store, 'profile', p.wikiloc_user, user)) };
}

export function listJobs(store) {
  const jobs = store.db.prepare('SELECT * FROM wikiloc_jobs ORDER BY created_at DESC LIMIT 30').all().map(jobRow);
  return { jobs, workerSeen: store.getSetting('wikilocWorkerSeen') };
}

/** The worker takes the oldest waiting job; jobs stuck "running" for 15 minutes are retried. */
export function claimJob(store) {
  store.setSetting('wikilocWorkerSeen', now());
  store.db
    .prepare("UPDATE wikiloc_jobs SET status='queued', updated_at=? WHERE status='running' AND updated_at < ?")
    .run(now(), new Date(Date.now() - STALE_MS).toISOString());
  const job = store.db.prepare("SELECT * FROM wikiloc_jobs WHERE status='queued' ORDER BY created_at LIMIT 1").get();
  if (!job) return { job: null };
  store.db.prepare("UPDATE wikiloc_jobs SET status='running', updated_at=? WHERE id=?").run(now(), job.id);
  const out = jobRow({ ...job, status: 'running' });
  if (job.kind === 'profile') {
    const profile = store.db.prepare('SELECT * FROM wikiloc_profiles WHERE wikiloc_user=?').get(job.target);
    out.profile = profile ? profileRow(profile) : null;
    out.knownIds = [
      ...new Set([
        ...store.db.prepare('SELECT wikiloc_id FROM wikiloc_walks').all().map(r => r.wikiloc_id),
        ...store.db
          .prepare("SELECT json_extract(data_json,'$.wikiloc.id') AS id FROM monitoring_tracks WHERE id IS NOT NULL")
          .all()
          .map(r => r.id)
          .filter(Boolean),
      ]),
    ];
  }
  return { job: out };
}

export function finishJob(store, id, body) {
  const job = store.db.prepare('SELECT * FROM wikiloc_jobs WHERE id=?').get(id);
  if (!job) throw fail('JOB_NOT_FOUND', 'Job not found', 404);
  const status = body.status === 'done' ? 'done' : 'failed';
  store.db
    .prepare('UPDATE wikiloc_jobs SET status=?, message=?, updated_at=? WHERE id=?')
    .run(status, text(body.message, 1000), now(), id);
  if (job.kind === 'profile' && status === 'done')
    store.db.prepare('UPDATE wikiloc_profiles SET last_checked=?, name=COALESCE(?, name) WHERE wikiloc_user=?').run(now(), text(body.profileName, 120), job.target);
  return { job: jobRow(store.db.prepare('SELECT * FROM wikiloc_jobs WHERE id=?').get(id)) };
}

/** Who requested the job a walk comes from: the walk is theirs, not the worker's. */
export function jobRequester(store, jobId) {
  return jobId ? store.db.prepare('SELECT requested_by FROM wikiloc_jobs WHERE id=?').get(String(jobId))?.requested_by || null : null;
}

export function listProfiles(store) {
  return store.db.prepare('SELECT * FROM wikiloc_profiles ORDER BY created_at').all().map(profileRow);
}

/** Follows a profile, given its page link (…/wikiloc/user.do?id=13756119) or number. */
export function addProfile(store, body, user) {
  const id = /user\.do\?id=(\d{3,12})/.exec(String(body.url || ''))?.[1] || /^\d{3,12}$/.exec(String(body.url || '').trim())?.[0];
  if (!id) throw fail('INVALID_PROFILE', 'Paste the link of a Wikiloc profile (…/wikiloc/user.do?id=…)');
  // "monitor" also catches typos such as "Monitoro 12/7".
  const pattern = text(body.pattern, 100) || 'monitor';
  const collector = text(body.collector, 120);
  const existing = store.db.prepare('SELECT * FROM wikiloc_profiles WHERE wikiloc_user=?').get(id);
  if (existing) {
    store.db
      .prepare('UPDATE wikiloc_profiles SET pattern=?, collector=COALESCE(?, collector) WHERE id=?')
      .run(text(body.pattern, 100) || existing.pattern, collector, existing.id);
    return { profile: profileRow(store.db.prepare('SELECT * FROM wikiloc_profiles WHERE id=?').get(existing.id)) };
  }
  const row = {
    id: randomUUID(),
    wikiloc_user: id,
    name: text(body.name, 120),
    pattern,
    added_by: user.username,
    created_at: now(),
    last_checked: null,
    collector,
  };
  store.db
    .prepare('INSERT INTO wikiloc_profiles(id,wikiloc_user,name,pattern,added_by,created_at,last_checked,collector) VALUES(?,?,?,?,?,?,?,?)')
    .run(...Object.values(row));
  return { profile: profileRow(row) };
}

export function removeProfile(store, id) {
  if (!store.db.prepare('DELETE FROM wikiloc_profiles WHERE id=?').run(id).changes) throw fail('PROFILE_NOT_FOUND', 'Profile not found', 404);
  return { ok: true };
}
