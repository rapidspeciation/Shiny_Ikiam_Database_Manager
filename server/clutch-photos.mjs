// Photos of clutches (the Clutches tab's cards), only in the app: several per
// clutch and day, taken with the phone's camera or chosen from its gallery, each
// linked to one of the clutch's events of that day (the "+5 hatched", the "−1
// disappeared": server/clutches.mjs clutch_events) or just to the day, with a
// short caption ("Dead larva"). The next person sees them in the clutch's
// timeline. They never go to Google Sheets.
//
// The phone resizes a photo before sending it (longest side 2560 px, JPEG
// quality 85, the camera's turn applied, nothing else of its EXIF kept: the
// canvas writes none) and makes its thumbnail; both travel as one upload in
// small chunks (PUT …/photo-uploads/<id>?offset=N), so a slow or dropped
// connection resumes where it stopped instead of starting again, and the photo
// is stored once all of it has arrived (POST …/photos, idempotent by its id).
// The server checks both are JPEGs, drops any metadata segment left (EXIF with
// GPS, XMP, IPTC, comments) and, when a photo still comes larger or turned by
// its EXIF (a browser that did not resize), makes the copies with Pillow
// (python3, as server/proposal-photos.mjs) or refuses it.
//
// Files: <dir>/<yyyy-mm>/<id>.jpg and <id>.thumb.jpg, by default in
// clutch-photos next to the database (scripts/backup.mjs copies them); the
// records (clutch, day, event, caption, who, when, sizes) in SQLite. In memory
// for an in-memory database (tests).

import { execFile } from 'node:child_process';
import { randomUUID } from 'node:crypto';
import { appendFileSync, mkdirSync, readFileSync, readdirSync, rmSync, statSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join } from 'node:path';
import { ecuadorDay } from './clutches.mjs';

/** Longest side of a stored photo and of its thumbnail, in pixels; the JPEG quality the phone and Pillow use. */
export const PHOTO_EDGE = 2560;
export const THUMB_EDGE = 480;
export const PHOTO_QUALITY = 85;
/** The most a photo (with its thumbnail) may weigh, and one chunk of its upload. */
export const UPLOAD_MAX = 12 * 1024 * 1024;
export const CHUNK_MAX = 1024 * 1024;
const THUMB_MAX = 400 * 1024;
const NOTE_MAX = 200;
const ID = /^[A-Za-z0-9-]{8,64}$/;
/** Uploads left unfinished longer than this are dropped. */
const STALE_MS = 3 * 86_400_000;
const fail = (code, message, status = 400) => Object.assign(new Error(message), { code, status });

/** Where the photos are kept: CLUTCH_PHOTO_DIR, else clutch-photos next to the database; null in memory. */
export function clutchPhotoDir(env, databasePath) {
  if (env?.CLUTCH_PHOTO_DIR) return env.CLUTCH_PHOTO_DIR;
  return !databasePath || databasePath === ':memory:' ? null : join(dirname(databasePath), 'clutch-photos');
}

export function initClutchPhotos(db) {
  db.exec(`CREATE TABLE IF NOT EXISTS clutch_photos(id TEXT PRIMARY KEY, request_id TEXT UNIQUE, record_id TEXT NOT NULL, clutch TEXT,
      day TEXT NOT NULL, event_id TEXT, note TEXT, actor TEXT NOT NULL, width INTEGER NOT NULL, height INTEGER NOT NULL,
      bytes INTEGER NOT NULL, thumb_bytes INTEGER NOT NULL, file TEXT NOT NULL, created_at TEXT NOT NULL);
    CREATE INDEX IF NOT EXISTS clutch_photos_record ON clutch_photos(record_id, day);
    CREATE INDEX IF NOT EXISTS clutch_photos_day ON clutch_photos(day);
    CREATE TABLE IF NOT EXISTS clutch_photo_uploads(id TEXT PRIMARY KEY, actor TEXT NOT NULL, created_at TEXT NOT NULL);`);
  // A photo of one group of the clutch (box A's larvae: server/clutches.mjs clutch_groups), older databases.
  if (!new Set(db.prepare('PRAGMA table_info(clutch_photos)').all().map(c => c.name)).has('group_id')) db.exec('ALTER TABLE clutch_photos ADD COLUMN group_id TEXT');
}

// --- JPEG: size, EXIF turn, and the metadata segments taken out (no image library needed)

const SOF = new Set([0xc0, 0xc1, 0xc2, 0xc3, 0xc5, 0xc6, 0xc7, 0xc9, 0xca, 0xcb, 0xcd, 0xce, 0xcf]);
/**
 * A JPEG's segments up to the image data: { width, height, orientation, clean }
 * where `clean` is the file without its APP1–APP15 (EXIF, GPS, XMP, IPTC…) and
 * comment segments; null when it is not a JPEG.
 */
export function readJpeg(buf) {
  if (!Buffer.isBuffer(buf) || buf.length < 4 || buf[0] !== 0xff || buf[1] !== 0xd8) return null;
  const keep = [buf.subarray(0, 2)];
  let i = 2;
  let width = 0;
  let height = 0;
  let orientation = 1;
  while (i < buf.length) {
    if (buf[i] !== 0xff) return null;
    let m = buf[i + 1];
    // Fill bytes before a marker.
    while (m === 0xff && i + 2 < buf.length) m = buf[++i + 1];
    if (m === 0xd9) {
      keep.push(buf.subarray(i, i + 2));
      break;
    }
    if (m === 0x01 || (m >= 0xd0 && m <= 0xd7)) {
      keep.push(buf.subarray(i, i + 2));
      i += 2;
      continue;
    }
    if (i + 4 > buf.length) return null;
    const len = buf.readUInt16BE(i + 2);
    if (len < 2 || i + 2 + len > buf.length) return null;
    const segment = buf.subarray(i, i + 2 + len);
    if (SOF.has(m) && len >= 7) {
      height = buf.readUInt16BE(i + 5);
      width = buf.readUInt16BE(i + 7);
    }
    if (m === 0xe1) orientation = exifOrientation(buf.subarray(i + 4, i + 2 + len)) ?? orientation;
    if (m === 0xda) {
      // The image data runs to the end.
      keep.push(buf.subarray(i));
      break;
    }
    if (!((m >= 0xe1 && m <= 0xef) || m === 0xfe)) keep.push(segment);
    i += 2 + len;
  }
  if (!width || !height) return null;
  return { width, height, orientation, clean: Buffer.concat(keep) };
}
/** The Orientation tag (1–8) of an APP1 EXIF payload, or null. */
function exifOrientation(data) {
  if (data.length < 14 || data.toString('latin1', 0, 6) !== 'Exif\0\0') return null;
  const tiff = data.subarray(6);
  const little = tiff.toString('latin1', 0, 2) === 'II';
  const u16 = at => (little ? tiff.readUInt16LE(at) : tiff.readUInt16BE(at));
  const u32 = at => (little ? tiff.readUInt32LE(at) : tiff.readUInt32BE(at));
  try {
    const ifd = u32(4);
    const n = u16(ifd);
    for (let k = 0; k < n; k++) {
      const entry = ifd + 2 + k * 12;
      if (u16(entry) === 0x0112) {
        const v = u16(entry + 8);
        return v >= 1 && v <= 8 ? v : null;
      }
    }
  } catch {
    return null;
  }
  return null;
}

const PILLOW = `
import sys
from PIL import Image, ImageOps
src, out, edge, quality = sys.argv[1], sys.argv[2], int(sys.argv[3]), int(sys.argv[4])
im = ImageOps.exif_transpose(Image.open(src))
im.thumbnail((edge, edge))
im.convert('RGB').save(out, 'JPEG', quality=quality, optimize=True)
`;
/** A copy turned upright and fitted in `edge` px, by Pillow; null without it. */
function pillowCopy(data, edge, quality, python = 'python3') {
  const base = join(tmpdir(), `ithomiini-clutch-${randomUUID()}`);
  writeFileSync(`${base}.in`, data, { mode: 0o600 });
  return new Promise(resolve =>
    execFile(python, ['-c', PILLOW, `${base}.in`, `${base}.out`, String(edge), String(quality)], { timeout: 60_000 }, error => {
      let out = null;
      if (!error)
        try {
          out = readFileSync(`${base}.out`);
        } catch {
          out = null;
        }
      rmSync(`${base}.in`, { force: true });
      rmSync(`${base}.out`, { force: true });
      resolve(out);
    }),
  );
}

/**
 * A photo as it is stored: upright, at most `edge` px on its longest side,
 * without metadata. A JPEG already within the size and upright is only
 * cleaned; anything else goes through Pillow (refused without it).
 */
async function prepared(data, edge, quality, python) {
  const info = readJpeg(data);
  if (info && info.orientation === 1 && Math.max(info.width, info.height) <= edge) return info;
  const copy = await pillowCopy(data, edge, quality, python);
  const again = copy ? readJpeg(copy) : null;
  if (!again) {
    if (!info) throw fail('INVALID_PHOTO', 'The photo must be a JPEG image');
    throw fail('PHOTO_TOO_LARGE', `The photo must be at most ${edge} px on its longest side`);
  }
  return again;
}

const shape = r => ({
  id: r.id,
  recordId: r.record_id,
  clutch: r.clutch,
  day: r.day,
  eventId: r.event_id ?? null,
  groupId: r.group_id ?? null,
  note: r.note ?? null,
  actor: r.actor,
  username: r.username ?? null,
  name: r.name ?? null,
  width: r.width,
  height: r.height,
  bytes: r.bytes,
  thumbBytes: r.thumb_bytes,
  createdAt: r.created_at,
});
const SELECT = 'SELECT p.*, u.username, u.display_name name FROM clutch_photos p LEFT JOIN users u ON u.id = p.actor';

function cleanNote(value) {
  if (value === undefined || value === null) return null;
  if (typeof value !== 'string') throw fail('INVALID_NOTE', 'Invalid caption');
  const text = value.trim().replace(/\s+/g, ' ');
  if (text.length > NOTE_MAX) throw fail('INVALID_NOTE', `The caption is too long (${NOTE_MAX} characters at most)`);
  return text || null;
}
function cleanDay(value) {
  const today = ecuadorDay();
  const day = value === undefined || value === null || value === '' ? today : String(value);
  if (!/^\d{4}-\d{2}-\d{2}$/.test(day) || Number.isNaN(Date.parse(day)) || day > today || day < '2020-01-01') throw fail('INVALID_DAY', 'Invalid day');
  return day;
}

/**
 * The clutches' photos. `dir`: where the files go (null: in memory); `python`:
 * the interpreter with Pillow, for photos that come too large or turned.
 */
export function createClutchPhotos(store, { dir = null, python = 'python3' } = {}) {
  const db = store.db;
  const memory = new Map();
  if (dir) mkdirSync(join(dir, 'uploads'), { recursive: true, mode: 0o700 });
  const partPath = id => join(dir, 'uploads', `${id}.part`);
  const partSize = id => {
    if (!dir) return memory.get(`part:${id}`)?.length ?? 0;
    try {
      return statSync(partPath(id)).size;
    } catch {
      return 0;
    }
  };
  const dropPart = id => {
    if (dir) rmSync(partPath(id), { force: true });
    else memory.delete(`part:${id}`);
    db.prepare('DELETE FROM clutch_photo_uploads WHERE id = ?').run(id);
  };
  const write = (name, data) => {
    if (!dir) return memory.set(name, data);
    mkdirSync(dirname(join(dir, name)), { recursive: true, mode: 0o700 });
    writeFileSync(join(dir, name), data, { mode: 0o600 });
  };
  const read = name => (dir ? readFileSync(join(dir, name)) : memory.get(name));
  const remove = name => (dir ? rmSync(join(dir, name), { force: true }) : memory.delete(name));

  /** Unfinished uploads older than three days: their bytes go. */
  function sweep() {
    const before = new Date(Date.now() - STALE_MS).toISOString();
    for (const r of db.prepare('SELECT id FROM clutch_photo_uploads WHERE created_at < ?').all(before)) dropPart(r.id);
    if (!dir) return;
    const known = new Set(db.prepare('SELECT id FROM clutch_photo_uploads').all().map(r => r.id));
    for (const name of readdirSync(join(dir, 'uploads'))) {
      const id = name.replace(/\.part$/, '');
      if (!known.has(id)) rmSync(join(dir, 'uploads', name), { force: true });
    }
  }
  sweep();

  /**
   * One chunk of an upload, at `offset` (what has arrived so far). Another
   * offset is answered with what the server has (409), so the phone sends on
   * from there; a chunk already received is not appended twice.
   */
  function receiveChunk(uploadId, offsetText, chunk, user) {
    if (!ID.test(uploadId)) throw fail('INVALID_UPLOAD', 'Invalid upload');
    const owner = db.prepare('SELECT actor FROM clutch_photo_uploads WHERE id = ?').get(uploadId);
    if (owner && owner.actor !== user.id) throw fail('FORBIDDEN', 'Not your upload', 403);
    if (db.prepare('SELECT 1 FROM clutch_photos WHERE request_id = ?').get(uploadId)) return { received: null, done: true };
    const offset = Number(offsetText);
    if (!Number.isInteger(offset) || offset < 0) throw fail('INVALID_UPLOAD', 'Invalid offset');
    if (!Buffer.isBuffer(chunk) || !chunk.length || chunk.length > CHUNK_MAX) throw fail('INVALID_UPLOAD', 'Invalid chunk');
    const have = partSize(uploadId);
    if (offset !== have) return { received: have, status: 409 };
    if (have + chunk.length > UPLOAD_MAX) throw fail('PHOTO_TOO_LARGE', 'The photo is too large');
    if (!owner) db.prepare('INSERT INTO clutch_photo_uploads(id, actor, created_at) VALUES(?,?,?)').run(uploadId, user.id, new Date().toISOString());
    if (dir) appendFileSync(partPath(uploadId), chunk, { mode: 0o600 });
    else memory.set(`part:${uploadId}`, Buffer.concat([memory.get(`part:${uploadId}`) ?? Buffer.alloc(0), chunk]));
    return { received: have + chunk.length };
  }

  /** How much of an upload has arrived (to resume it), or that it is stored already. */
  function uploadState(uploadId, user) {
    if (!ID.test(uploadId)) throw fail('INVALID_UPLOAD', 'Invalid upload');
    const done = db.prepare(`${SELECT} WHERE p.request_id = ?`).get(uploadId);
    if (done) return { received: null, photo: shape(done) };
    const owner = db.prepare('SELECT actor FROM clutch_photo_uploads WHERE id = ?').get(uploadId);
    if (owner && owner.actor !== user.id) throw fail('FORBIDDEN', 'Not your upload', 403);
    return { received: partSize(uploadId) };
  }

  /**
   * The upload complete: `thumbBytes` its first bytes are the thumbnail, the
   * rest the photo; stored for the clutch (`recordId`), its `day`, an event of
   * that clutch (`eventId`), one of its groups (`groupId`) or none (the clutch
   * that day), and a caption (`note`).
   */
  async function finish(body, user) {
    const uploadId = String(body.requestId ?? '');
    if (!ID.test(uploadId)) throw fail('REQUEST_ID_REQUIRED', 'A unique requestId is required');
    const prior = db.prepare(`${SELECT} WHERE p.request_id = ?`).get(uploadId);
    if (prior) return { photo: shape(prior), duplicate: true };
    const record = db.prepare("SELECT id, values_json FROM records WHERE id = ? AND sheet = 'Insectary_stocks' AND missing = 0").get(String(body.recordId ?? ''));
    if (!record) throw fail('RECORD_NOT_FOUND', 'Clutch not found', 404);
    const day = cleanDay(body.day);
    const eventId = body.eventId ? String(body.eventId) : null;
    if (eventId && !db.prepare('SELECT 1 FROM clutch_events WHERE id = ? AND record_id = ?').get(eventId, record.id))
      throw fail('EVENT_NOT_FOUND', 'Event not found for this clutch', 404);
    const groupId = groupOf(body.groupId, record.id);
    const note = cleanNote(body.note);
    const owner = db.prepare('SELECT actor FROM clutch_photo_uploads WHERE id = ?').get(uploadId);
    if (!owner) throw fail('UPLOAD_NOT_FOUND', 'Nothing has arrived for this photo', 404);
    if (owner.actor !== user.id) throw fail('FORBIDDEN', 'Not your upload', 403);
    let all;
    try {
      all = dir ? readFileSync(partPath(uploadId)) : memory.get(`part:${uploadId}`);
    } catch {
      all = null;
    }
    if (!all?.length) throw fail('UPLOAD_NOT_FOUND', 'Nothing has arrived for this photo', 404);
    if (body.totalBytes !== undefined && Number(body.totalBytes) !== all.length) throw fail('UPLOAD_INCOMPLETE', 'The photo has not arrived whole yet', 409);
    const thumbBytes = Number(body.thumbBytes ?? 0);
    if (!Number.isInteger(thumbBytes) || thumbBytes < 0 || thumbBytes > Math.min(THUMB_MAX, all.length - 4))
      throw fail('INVALID_PHOTO', 'Invalid thumbnail size');
    const photo = await prepared(all.subarray(thumbBytes), PHOTO_EDGE, PHOTO_QUALITY, python);
    let thumb = thumbBytes ? readJpeg(all.subarray(0, thumbBytes)) : null;
    if (!thumb || thumb.orientation !== 1 || Math.max(thumb.width, thumb.height) > THUMB_EDGE * 1.5) {
      const copy = await pillowCopy(photo.clean, THUMB_EDGE, 80, python);
      thumb = (copy && readJpeg(copy)) || null;
    }
    // Without a thumbnail (no Pillow, none sent): the photo stands for it.
    const thumbData = thumb?.clean ?? photo.clean;
    const id = randomUUID();
    const file = `${day.slice(0, 7)}/${id}`;
    write(`${file}.jpg`, photo.clean);
    write(`${file}.thumb.jpg`, thumbData);
    const values = JSON.parse(record.values_json || '{}');
    const clutch = values['CLUTCH NUMBER'] === null || values['CLUTCH NUMBER'] === undefined ? null : String(values['CLUTCH NUMBER']).trim();
    db.prepare(
      `INSERT INTO clutch_photos(id, request_id, record_id, clutch, day, event_id, group_id, note, actor, width, height, bytes, thumb_bytes, file, created_at)
       VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)`,
    ).run(id, uploadId, record.id, clutch, day, eventId, groupId, note, user.id, photo.width, photo.height, photo.clean.length, thumbData.length, file, new Date().toISOString());
    dropPart(uploadId);
    return { photo: shape(db.prepare(`${SELECT} WHERE p.id = ?`).get(id)), duplicate: false };
  }

  /** A clutch's photos (oldest first), or one day's of every clutch (`day`). */
  function list(query = {}) {
    if (query.recordId) return { photos: db.prepare(`${SELECT} WHERE p.record_id = ? ORDER BY p.day, p.created_at`).all(String(query.recordId)).map(shape) };
    const day = cleanDay(query.day);
    return { photos: db.prepare(`${SELECT} WHERE p.day = ? ORDER BY p.created_at`).all(day).map(shape) };
  }

  /** A photo's bytes: `thumb` or the whole photo. */
  function file(id, size = 'full') {
    const row = db.prepare('SELECT file FROM clutch_photos WHERE id = ?').get(String(id));
    if (!row) return null;
    try {
      return read(`${row.file}${size === 'thumb' ? '.thumb' : ''}.jpg`) ?? null;
    } catch {
      return null;
    }
  }

  /** A group of the clutch a photo shows, or none. */
  const groupOf = (value, recordId) => {
    if (!value) return null;
    const g = db.prepare('SELECT id FROM clutch_groups WHERE id = ? AND record_id = ?').get(String(value), recordId);
    if (!g) throw fail('GROUP_NOT_FOUND', 'Group not found for this clutch', 404);
    return g.id;
  };
  const mayChange = (row, user) => row.actor === user.id || ['reviewer', 'admin'].includes(user.role);
  /** What a photo shows, changed: the event it goes with (or none: the day) and its caption. */
  function update(id, body, user) {
    const row = db.prepare('SELECT * FROM clutch_photos WHERE id = ?').get(String(id));
    if (!row) throw fail('PHOTO_NOT_FOUND', 'Photo not found', 404);
    if (!mayChange(row, user)) throw fail('FORBIDDEN', 'Only your own photos', 403);
    let eventId = row.event_id;
    if (body.eventId !== undefined) {
      eventId = body.eventId ? String(body.eventId) : null;
      if (eventId && !db.prepare('SELECT 1 FROM clutch_events WHERE id = ? AND record_id = ?').get(eventId, row.record_id))
        throw fail('EVENT_NOT_FOUND', 'Event not found for this clutch', 404);
    }
    const groupId = body.groupId !== undefined ? groupOf(body.groupId, row.record_id) : row.group_id;
    const note = body.note !== undefined ? cleanNote(body.note) : row.note;
    db.prepare('UPDATE clutch_photos SET event_id = ?, group_id = ?, note = ? WHERE id = ?').run(eventId, groupId, note, row.id);
    return { photo: shape(db.prepare(`${SELECT} WHERE p.id = ?`).get(row.id)) };
  }

  /** Takes a photo away (its files too): one's own, or anyone's for reviewers and admins. */
  function removePhoto(id, user) {
    const row = db.prepare('SELECT * FROM clutch_photos WHERE id = ?').get(String(id));
    if (!row) throw fail('PHOTO_NOT_FOUND', 'Photo not found', 404);
    if (!mayChange(row, user)) throw fail('FORBIDDEN', 'Only your own photos', 403);
    db.prepare('DELETE FROM clutch_photos WHERE id = ?').run(row.id);
    remove(`${row.file}.jpg`);
    remove(`${row.file}.thumb.jpg`);
    return { removed: row.id };
  }

  return { receiveChunk, uploadState, finish, list, file, update, remove: removePhoto, sweep, dir };
}
