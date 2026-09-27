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
  const walk = body.wikilocWalkId ? store.db.prepare('SELECT * FROM wikiloc_walks WHERE id=?').get(String(body.wikilocWalkId)) : null;
  if (body.wikilocWalkId && !walk) throw fail('WALK_NOT_FOUND', 'Wikiloc walk not found', 404);
  const fingerprint = createHash('sha256')
    .update(JSON.stringify([date, track, captures.map(c => [c.lat, c.lon, c.text])]))
    .digest('hex');
  const existing =
    store.db.prepare('SELECT * FROM monitoring_tracks WHERE request_id=?').get(body.requestId) ||
    store.db.prepare('SELECT * FROM monitoring_tracks WHERE fingerprint=?').get(fingerprint);
  if (existing) {
    if (walk) markImported(store, walk.id, existing.id);
    return { track: fromRow(existing), duplicate: true };
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

/** Only the person who uploaded a track, a reviewer or an administrator can remove it. */
export function deleteTrack(store, id, user) {
  const row = store.db.prepare('SELECT * FROM monitoring_tracks WHERE id=?').get(id);
  if (!row) throw fail('TRACK_NOT_FOUND', 'Track not found', 404);
  if (row.created_by !== user.username && !['reviewer', 'admin'].includes(user.role))
    throw fail('FORBIDDEN', 'Only the uploader, a reviewer or an administrator can remove this track', 403);
  store.db.prepare('DELETE FROM monitoring_tracks WHERE id=?').run(id);
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

/** Queues a pasted or shared link (any text containing one is accepted). */
export function queueLink(store, body, user) {
  const found = trailUrl(body.url || body.text);
  if (!found) throw fail('INVALID_WALK', 'No Wikiloc trail link found');
  const known = store.db.prepare("SELECT status FROM wikiloc_walks WHERE wikiloc_id=?").get(found.wikilocId);
  return { job: addJob(store, 'trail', found.url, user), known: known?.status || null };
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
