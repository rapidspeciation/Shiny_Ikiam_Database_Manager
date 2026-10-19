// The species in each photo of a Wikiloc walk, read in the background by the AI Identifier
// (server/identifier.mjs: its own process, one photo at a time; this thread only waits on its
// pipe), for the walk's proposal in Monitoreo → Importar: the review table shows each photo's
// progress and, once read, the identifier's species beside the one typed (server/assistant.mjs
// marks the rows). Field photos of a living butterfly are what the identifier reads best.
//
// One row per photo in walk_photo_ids (status queued | reading | done | failed), kept: a walk
// opened again shows its readings at once, and only photos never read (or failed) are read again.

import { mkdirSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';

/** East of the Andes: Ikiam's side, for the identifier's geographic prior. */
const SIDE = 'East';
/** The lists kept per photo: the first names above 1 %, at most this many. */
const KEEP = 5;

export function initWalkIds(db) {
  db.exec(`CREATE TABLE IF NOT EXISTS walk_photo_ids(photo_id TEXT PRIMARY KEY, walk_id TEXT NOT NULL, status TEXT NOT NULL,
    species_json TEXT, subspecies_json TEXT, crop TEXT, error TEXT, ms INTEGER, updated_at TEXT NOT NULL);
    CREATE INDEX IF NOT EXISTS walk_photo_ids_walk ON walk_photo_ids(walk_id);`);
}

const kept = list =>
  (Array.isArray(list) ? list : [])
    .filter(([, p], i) => i === 0 || p >= 0.01)
    .slice(0, KEEP)
    .map(([name, p]) => [String(name), Math.round(Number(p) * 1000) / 1000]);

/**
 * `identifier`: { identify({ path, side }) → { species: [[name, p]…], subspecies, crop } | { error } }
 * (null when not set up: start() says so). `onRead(walkId)`: after each photo is read.
 * `scratch`: where a photo is put for the identifier while it reads it.
 */
export function createWalkIds({ store, identifier, onRead = () => {}, scratch = join(tmpdir(), 'ithomiini-walk-ids') }) {
  const db = store.db;
  initWalkIds(db);
  const now = () => new Date().toISOString();
  // A reading cut by a restart is read again.
  db.prepare("UPDATE walk_photo_ids SET status='queued', updated_at=? WHERE status='reading'").run(now());
  let running = null;

  /** The walk's photos in point order: [{ id, point }]. */
  function photosOf(walkId) {
    const row = db.prepare('SELECT data_json FROM wikiloc_walks WHERE id=?').get(String(walkId));
    if (!row) return null;
    const data = JSON.parse(row.data_json);
    return data.waypoints.flatMap((p, point) => (p.photos || []).map(id => ({ id: String(id), point })));
  }

  async function readOne(job) {
    const photo = db.prepare('SELECT data FROM monitoring_photos WHERE id=?').get(job.photo_id);
    db.prepare("UPDATE walk_photo_ids SET status='reading', updated_at=? WHERE photo_id=?").run(now(), job.photo_id);
    const started = Date.now();
    let out;
    if (!photo) out = { error: 'The photo is not in the app' };
    else {
      mkdirSync(scratch, { recursive: true, mode: 0o700 });
      const path = join(scratch, `${job.photo_id.replace(/[^\w-]/g, '')}.jpg`);
      try {
        writeFileSync(path, photo.data, { mode: 0o600 });
        out = await identifier.identify({ path, side: SIDE });
      } catch (e) {
        out = { error: String(e?.message || e).slice(0, 200) };
      } finally {
        rmSync(path, { force: true });
      }
    }
    if (out?.error)
      db.prepare("UPDATE walk_photo_ids SET status='failed', error=?, ms=?, updated_at=? WHERE photo_id=?").run(
        String(out.error).slice(0, 300),
        Date.now() - started,
        now(),
        job.photo_id,
      );
    else
      db.prepare(
        "UPDATE walk_photo_ids SET status='done', species_json=?, subspecies_json=?, crop=?, error=NULL, ms=?, updated_at=? WHERE photo_id=?",
      ).run(JSON.stringify(kept(out?.species)), JSON.stringify(kept(out?.subspecies)), out?.crop ?? null, Date.now() - started, now(), job.photo_id);
    try {
      onRead(job.walk_id);
    } catch (e) {
      console.error('Walk identification:', e.message);
    }
  }

  /** Reads the queued photos one after the other, in the background. */
  function drain() {
    running ??= (async () => {
      try {
        for (;;) {
          const job = db.prepare("SELECT * FROM walk_photo_ids WHERE status='queued' ORDER BY updated_at, rowid LIMIT 1").get();
          if (!job) break;
          await readOne(job);
        }
      } finally {
        running = null;
      }
    })();
    return running;
  }

  return {
    /** Queues the walk's photos not read yet (and those that failed); returns its status. */
    start(walkId) {
      const photos = photosOf(walkId);
      if (!photos) return { error: 'Walk not found', status: 404 };
      if (!identifier || identifier.status?.().installed === false)
        return { ...this.status(walkId), unavailable: 'The identifier is not set up on this server.' };
      const at = now();
      for (const p of photos) {
        const old = db.prepare('SELECT status FROM walk_photo_ids WHERE photo_id=?').get(p.id);
        if (!old)
          db.prepare("INSERT INTO walk_photo_ids(photo_id,walk_id,status,updated_at) VALUES(?,?,'queued',?)").run(p.id, String(walkId), at);
        else if (old.status === 'failed')
          db.prepare("UPDATE walk_photo_ids SET status='queued', error=NULL, updated_at=? WHERE photo_id=?").run(at, p.id);
      }
      drain();
      return this.status(walkId);
    },
    /** Each photo of the walk: its point, status and, once read, the identifier's species and subspecies. */
    status(walkId) {
      const photos = photosOf(walkId);
      if (!photos) return { error: 'Walk not found', status: 404 };
      const rows = new Map(
        db
          .prepare('SELECT * FROM walk_photo_ids WHERE photo_id IN (SELECT value FROM json_each(?))')
          .all(JSON.stringify(photos.map(p => p.id)))
          .map(r => [r.photo_id, r]),
      );
      const list = photos.map(p => {
        const r = rows.get(p.id);
        return {
          photo: p.id,
          point: p.point,
          status: r?.status ?? 'not_started',
          ...(r?.species_json ? { species: JSON.parse(r.species_json), subspecies: JSON.parse(r.subspecies_json || '[]') } : {}),
          ...(r?.crop && r.crop !== 'success' ? { crop: r.crop } : {}),
          ...(r?.error ? { error: r.error } : {}),
        };
      });
      return {
        walkId: String(walkId),
        installed: !!identifier && (identifier.status?.().installed ?? true),
        photos: list,
        read: list.filter(p => p.status === 'done' || p.status === 'failed').length,
        total: list.length,
      };
    },
    /** For tests: resolves when the queue is empty. */
    idle: () => running ?? Promise.resolve(),
  };
}

/** The identifier's reading of a row's photos: the photo whose first species is surest. */
export function bestReading(photos) {
  const read = photos.filter(p => p.status === 'done' && p.species?.length);
  if (!read.length) return null;
  return read.sort((a, b) => b.species[0][1] - a.species[0][1])[0];
}
