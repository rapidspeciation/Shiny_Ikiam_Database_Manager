import { createHash, randomUUID } from 'node:crypto';

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
    date TEXT NOT NULL, collector TEXT, name TEXT NOT NULL, data_json TEXT NOT NULL, created_by TEXT NOT NULL, created_at TEXT NOT NULL)`);
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
  };
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

export function listTracks(store) {
  return store.db.prepare('SELECT * FROM monitoring_tracks ORDER BY date DESC, created_at DESC').all().map(fromRow);
}

/** Saves a track once: the same file uploaded again returns the stored copy. */
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
  const fingerprint = createHash('sha256')
    .update(JSON.stringify([date, track, captures.map(c => [c.lat, c.lon, c.text])]))
    .digest('hex');
  const existing =
    store.db.prepare('SELECT * FROM monitoring_tracks WHERE request_id=?').get(body.requestId) ||
    store.db.prepare('SELECT * FROM monitoring_tracks WHERE fingerprint=?').get(fingerprint);
  if (existing) return { track: fromRow(existing), duplicate: true };
  const row = {
    id: randomUUID(),
    request_id: body.requestId,
    fingerprint,
    date,
    collector,
    name,
    data_json: JSON.stringify({ track, captures }),
    created_by: user.username,
    created_at: new Date().toISOString(),
  };
  store.db
    .prepare(
      'INSERT INTO monitoring_tracks(id,request_id,fingerprint,date,collector,name,data_json,created_by,created_at) VALUES(?,?,?,?,?,?,?,?,?)',
    )
    .run(...Object.values(row));
  return { track: fromRow(row), duplicate: false };
}

/** Only the person who uploaded a track, a reviewer or an administrator can remove it. */
export function deleteTrack(store, id, user) {
  const row = store.db.prepare('SELECT * FROM monitoring_tracks WHERE id=?').get(id);
  if (!row) throw fail('TRACK_NOT_FOUND', 'Track not found', 404);
  if (row.created_by !== user.username && !['reviewer', 'admin'].includes(user.role))
    throw fail('FORBIDDEN', 'Only the uploader, a reviewer or an administrator can remove this track', 403);
  store.db.prepare('DELETE FROM monitoring_tracks WHERE id=?').run(id);
  return { ok: true };
}
