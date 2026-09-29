// Specimen photos for the Revisión tab, fetched from Google Drive by the server
// and cached, so the browser never talks to Google and signed-out visitors see
// nothing. Only files listed in Photo_links (or the imported photo manifest)
// are served: the app is not an open proxy for Drive.
//
// Drive gives resized copies of files shared by link without an API key:
// lh3.googleusercontent.com/d/<id>=w<N>, else drive.google.com/thumbnail
// (the Wings Gallery's fallbacks). Two sizes are kept: 400 px for thumbnails
// and 1600 px for the envelope crop and the full view. Crops are done by the
// browser on these images (CSS, from the envelope and wing boxes), so the
// server needs no image library (sharp needs native arm64 binaries).
//
// Bytes are cached as files under PHOTO_CACHE_DIR (default: photo-cache next
// to the database), bounded by PHOTO_CACHE_MB (default 1024) with the least
// recently used files dropped first; their list is in SQLite (photo_cache).

import { mkdirSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { createHash } from 'node:crypto';
import { isFileId, photoIndex } from './photodata.mjs';

export const PHOTO_WIDTHS = [400, 1600];
const MAX_BYTES = 12_000_000;
const RETRY_AFTER_MS = 10 * 60_000;
const fail = (code, message, status) => Object.assign(new Error(message), { code, status });

export const driveUrls = (id, width) => [
  `https://lh3.googleusercontent.com/d/${id}=w${width}`,
  `https://drive.google.com/thumbnail?id=${id}&sz=w${width}`,
];

/**
 * @param store the app's store (photo_cache lives in its database)
 * @param options.dir cache directory; null keeps the bytes in memory (tests, :memory: databases)
 * @param options.maxBytes cache bound
 * @param options.fetchImpl fetch (tests pass a fake one)
 */
export function createPhotoService(
  store,
  { dir = null, maxBytes = 1024 * 1024 * 1024, fetchImpl = fetch, concurrency = 4 } = {},
) {
  const db = store.db;
  db.exec(`CREATE TABLE IF NOT EXISTS photo_cache(key TEXT PRIMARY KEY, file_id TEXT NOT NULL, width INTEGER NOT NULL,
    mime TEXT NOT NULL, bytes INTEGER NOT NULL, fetched_at TEXT NOT NULL, used_at INTEGER NOT NULL)`);
  if (dir) mkdirSync(dir, { recursive: true, mode: 0o700 });
  const memory = new Map();
  const inFlight = new Map();
  const failed = new Map();
  let running = 0;
  const queue = [];
  const slot = () =>
    running < concurrency
      ? (running++, Promise.resolve())
      : new Promise(resolve => queue.push(resolve)).then(() => running++);
  const release = () => {
    running--;
    queue.shift()?.();
  };
  const pathOf = key => join(dir, `${key}.img`);

  function read(key) {
    const row = db.prepare('SELECT mime, bytes, used_at FROM photo_cache WHERE key=?').get(key);
    if (!row) return null;
    let data;
    try {
      data = dir ? readFileSync(pathOf(key)) : memory.get(key);
    } catch {
      data = null;
    }
    if (!data) {
      db.prepare('DELETE FROM photo_cache WHERE key=?').run(key);
      return null;
    }
    // Used times are coarse (an hour) so browsing does not write to the database on every image.
    if (Date.now() - row.used_at > 3600_000)
      db.prepare('UPDATE photo_cache SET used_at=? WHERE key=?').run(Date.now(), key);
    return { mime: row.mime, data };
  }

  function write(key, id, width, mime, data) {
    if (dir) writeFileSync(pathOf(key), data, { mode: 0o600 });
    else memory.set(key, data);
    db.prepare(
      'INSERT OR REPLACE INTO photo_cache(key,file_id,width,mime,bytes,fetched_at,used_at) VALUES(?,?,?,?,?,?,?)',
    ).run(key, id, width, mime, data.length, new Date().toISOString(), Date.now());
    let total = db.prepare('SELECT coalesce(sum(bytes),0) n FROM photo_cache').get().n;
    if (total <= maxBytes) return;
    for (const old of db.prepare('SELECT key, bytes FROM photo_cache WHERE key<>? ORDER BY used_at').all(key)) {
      if (total <= maxBytes * 0.9) break;
      if (dir) rmSync(pathOf(old.key), { force: true });
      else memory.delete(old.key);
      db.prepare('DELETE FROM photo_cache WHERE key=?').run(old.key);
      total -= old.bytes;
    }
  }

  async function download(id, width) {
    let last = 'no answer';
    for (const url of driveUrls(id, width)) {
      try {
        const response = await fetchImpl(url, { redirect: 'follow', signal: AbortSignal.timeout(20000) });
        const mime = String(response.headers.get('content-type') || '')
          .split(';')[0]
          .trim();
        if (!response.ok || !/^image\/(jpeg|png|webp|gif)$/.test(mime)) {
          last = `${response.status} ${mime}`;
          continue;
        }
        const data = Buffer.from(await response.arrayBuffer());
        if (!data.length || data.length > MAX_BYTES) {
          last = 'empty or too large';
          continue;
        }
        return { mime, data };
      } catch (e) {
        last = e.message;
      }
    }
    // Not shared by link, or a format Drive does not preview (some HEIC): the card says "Sin foto".
    throw fail('PHOTO_UNAVAILABLE', `Drive did not give the photo (${last}); is it shared by link?`, 404);
  }

  /** The photo as { mime, data, etag }, from the cache or Drive. */
  async function get(id, width) {
    if (!isFileId(id)) throw fail('PHOTO_NOT_FOUND', 'Photo not found', 404);
    if (!PHOTO_WIDTHS.includes(width)) throw fail('INVALID_WIDTH', `w must be ${PHOTO_WIDTHS.join(' or ')}`, 400);
    if (!photoIndex(store).files.has(id)) throw fail('PHOTO_NOT_FOUND', 'Photo not in Photo_links', 404);
    const key = `${id}_w${width}`;
    const etag = `"${createHash('sha1').update(key).digest('base64url').slice(0, 16)}"`;
    const cached = read(key);
    if (cached) return { ...cached, etag };
    if ((failed.get(key) ?? 0) > Date.now())
      throw fail('PHOTO_UNAVAILABLE', 'Drive did not give the photo; try again later', 404);
    // Several cards asking for the same photo share one download.
    let pending = inFlight.get(key);
    if (!pending) {
      pending = slot()
        .then(() => download(id, width))
        .then(
          photo => {
            write(key, id, width, photo.mime, photo.data);
            return photo;
          },
          e => {
            failed.set(key, Date.now() + RETRY_AFTER_MS);
            throw e;
          },
        )
        .finally(() => {
          release();
          inFlight.delete(key);
        });
      inFlight.set(key, pending);
    }
    const photo = await pending;
    return { ...photo, etag };
  }

  const stats = () => db.prepare('SELECT count(*) files, coalesce(sum(bytes),0) bytes FROM photo_cache').get();
  return { get, stats };
}

/** The cache directory for a database path (next to it), or null for an in-memory database. */
export function photoCacheDir(env, databasePath) {
  if (env.PHOTO_CACHE_DIR) return env.PHOTO_CACHE_DIR;
  return !databasePath || databasePath === ':memory:' ? null : join(dirname(databasePath), 'photo-cache');
}
