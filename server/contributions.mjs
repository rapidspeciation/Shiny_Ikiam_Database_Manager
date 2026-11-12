// Photos sent from Identificar to train the AI (reports/contributions_20261111/DESIGN.md in WingsClassificator,
// lane C): after a result, a signed-in person marks the photo Hembra / Macho and/or confirms the species (or gives
// another one) and sends it. The phone keeps unsent items in an outbox and sends each with an id it made once
// (crypto.randomUUID()), so a retry never makes a second copy.
//
// What the server keeps, per item: the photo (at most PHOTO_EDGE px, at most PHOTO_MAX bytes, a JPEG with its
// APP1-APP15 and comment segments taken out: EXIF, GPS, XMP), an optional thumbnail, the person's labels, what the AI
// answered (prediction_json, as the phone sent it), the day the photo was taken, the consent version and the
// licence. No location: a `gps`, `lat` or `lon` in the request is ignored and never stored (Franz, 12 Nov 2026), and
// the photo's EXIF (GPS included) is taken out here even though the phone already took it out. Photos stay on this
// server (claudeclaw): nothing goes to Drive or the workbook, and nothing goes into training from here. Every item
// starts `received`; a reviewer or admin accepts or rejects it (review()), and an unsure or unrecognised name stays
// «sin identificar» (`unidentified`) in the pool. The training export with its leak gate (lane E) comes later.
//
// Deleting an item (its owner or an admin) removes its files and its row and keeps a tombstone {id, user, sha256,
// deleted_at}, so a phone that still holds it cannot send it again under its id (410). The same photo under a new id
// is a new item: that is how a wrong label is corrected («Deshacer», then Enviar again).
//
// Files: <dir>/<yyyy-mm>/<id>.jpg and <id>.thumb.jpg, by default in `contributions` next to the database
// (CONTRIB_DIR; scripts/backup.mjs copies them and drops the copies of deleted items). In memory for an in-memory
// database (tests).

import { createHash } from 'node:crypto';
import { mkdirSync, readFileSync, rmSync, statfsSync, writeFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { readJpeg } from './clutch-photos.mjs';

/**
 * Franz's answers (12 Nov 2026, DESIGN.md §8), in one place:
 * - `roles`: who may send (Q1: the team, i.e. anyone signed in; the routes sit after the sign-in check);
 * - `reviewRoles`: who accepts or rejects an item (Q4: any reviewer or admin; no expert step, nobody named);
 * - `keepTakenDay`: the day the photo was taken is kept (Q5). No location is kept at all (Q5);
 * - `sexForAnySpecies`: sex labels are taken for every species, not only those with a sex model (Q7).
 */
export const POLICY = Object.freeze({
  roles: Object.freeze(['observer', 'editor', 'reviewer', 'admin']),
  reviewRoles: Object.freeze(['reviewer', 'admin']),
  keepTakenDay: true,
  sexForAnySpecies: true,
});
/**
 * The consent the phone showed (DESIGN.md §6): its version. The text is the phone's (frontend/src/lib/contribConfig.ts
 * CONSENT, with the same version). v2 (12 Nov 2026) replaced the draft v1: no location, Franz Chandi responsible.
 * Only v2 is taken: an item queued under v1 waits on the phone until its person accepts v2 (lib/contribOutbox.ts).
 */
export const CONSENT_VERSION = 'v2';
export const CONSENTS = new Set([CONSENT_VERSION]);
/** Who the consent names as responsible for the data. The contact email is the server's config (CONTRIB_CONTACT_EMAIL), never the repo's. */
export const CONTROLLER = 'Franz Chandi';
/** A contact address as config gives it: kept when it reads as one address, else null (the consent then says how to ask the team). */
export function contactEmail(value) {
  const v = typeof value === 'string' ? value.trim() : '';
  return v.length <= 254 && /^[^\s@<>"',;]+@[^\s@<>"',;]+\.[^\s@<>"',;]+$/.test(v) ? v : null;
}
/**
 * A review's verdicts and the status each leaves (the last verdict counts; all are kept):
 * accept → accepted; reject → rejected; unsure → unidentified («sin identificar»: kept in the pool without a species
 * label). Accepting an item whose name the app did not recognise (free text) leaves it unidentified too: its sex
 * label is accepted, the name is not taken as a label.
 */
export const VERDICTS = Object.freeze(['accept', 'reject', 'unsure']);
const REASON_MAX = 200;
/** The licence recorded with each item (Q2's default). */
export const LICENCE = 'training-grant+CC-BY-4.0';

/** The photo as the phone sends it: longest side, bytes (before base64); the thumbnail. */
export const PHOTO_EDGE = 1600;
export const PHOTO_MAX = 4 * 1024 * 1024;
export const THUMB_EDGE = 480;
export const THUMB_MAX = 256 * 1024;
const PREDICTION_MAX = 32 * 1024;
const NAME_MAX = 120;
/** Per person: 30 items in 10 minutes and 300 in a day. */
export const LIMITS = Object.freeze([
  { count: 30, windowMs: 10 * 60_000 },
  { count: 300, windowMs: 24 * 3_600_000 },
]);
const GB = 1024 ** 3;
/** New items are refused (507) while the disk has less than this free, or the store holds `maxBytes`. */
export const MIN_FREE_BYTES = 5 * GB;
export const MAX_BYTES = 20 * GB;

const UUID = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/;
const SHA = /^[0-9a-f]{64}$/;
const BASE64 = /^[A-Za-z0-9+/]*={0,2}$/;
const SEXES = new Set(['female', 'male']);
const KINDS = new Set(['hand', 'live', 'collection']);
const RANKS = new Set(['genus', 'species', 'subspecies']);
const fail = (code, message, status = 400, headers) => Object.assign(new Error(message), { code, status, ...(headers ? { headers } : {}) });

/** Where the photos are kept: CONTRIB_DIR, else `contributions` next to the database; null in memory. */
export function contributionsDir(env, databasePath) {
  if (env?.CONTRIB_DIR) return env.CONTRIB_DIR;
  return !databasePath || databasePath === ':memory:' ? null : join(dirname(databasePath), 'contributions');
}

export function initContributions(db) {
  // phash: filled by the training export (lane E), which decodes the image; null until then.
  db.exec(`CREATE TABLE IF NOT EXISTS contributions(id TEXT PRIMARY KEY, user_id TEXT NOT NULL, created_at TEXT, received_at TEXT NOT NULL,
      sha256 TEXT NOT NULL, phash TEXT, width INTEGER NOT NULL, height INTEGER NOT NULL, bytes INTEGER NOT NULL, thumb_bytes INTEGER NOT NULL,
      file TEXT NOT NULL, photo_kind TEXT, sex_label TEXT, species_label TEXT, species_rank TEXT, species_confirmed INTEGER,
      free_text_name INTEGER, prediction_json TEXT, taken_at TEXT,
      consent_version TEXT NOT NULL, licence TEXT NOT NULL, status TEXT NOT NULL DEFAULT 'received');
    CREATE INDEX IF NOT EXISTS contributions_user ON contributions(user_id, received_at);
    CREATE UNIQUE INDEX IF NOT EXISTS contributions_user_sha ON contributions(user_id, sha256);
    CREATE TABLE IF NOT EXISTS contribution_tombstones(id TEXT PRIMARY KEY, user_id TEXT, sha256 TEXT, received_at TEXT,
      deleted_at TEXT NOT NULL, deleted_by TEXT);
    CREATE INDEX IF NOT EXISTS contribution_tombstones_user ON contribution_tombstones(user_id, sha256);
    CREATE TABLE IF NOT EXISTS contribution_verdicts(id INTEGER PRIMARY KEY AUTOINCREMENT, contribution_id TEXT NOT NULL,
      reviewer_id TEXT NOT NULL, verdict TEXT NOT NULL, reason TEXT, status TEXT NOT NULL, at TEXT NOT NULL);
    CREATE INDEX IF NOT EXISTS contribution_verdicts_item ON contribution_verdicts(contribution_id, id);`);
  // v1 kept an opt-in location: the columns go, with whatever a v1 phone sent (no location is kept from v2 on); the
  // predictions kept then are cut to the fields v2 keeps. When a location was held, VACUUM rewrites the file so its
  // bytes do not stay in free pages (backups made before keep them: delete those by hand).
  const columns = new Set(db.prepare('PRAGMA table_info(contributions)').all().map(c => c.name));
  if (columns.has('gps_lat')) {
    const held = db.prepare('SELECT count(*) n FROM contributions WHERE gps_lat IS NOT NULL OR gps_lon IS NOT NULL').get().n;
    for (const row of db.prepare('SELECT id, prediction_json p FROM contributions WHERE prediction_json IS NOT NULL').all()) {
      let clean = null;
      try {
        clean = cleanPrediction(JSON.parse(row.p));
      } catch {
        clean = null;
      }
      db.prepare('UPDATE contributions SET prediction_json = ? WHERE id = ?').run(clean, row.id);
    }
    for (const c of ['gps_lat', 'gps_lon', 'gps_acc']) if (columns.has(c)) db.exec(`ALTER TABLE contributions DROP COLUMN ${c}`);
    if (held) db.exec('VACUUM');
  }
}

/** A base64 JPEG (a data: URL too) as bytes, refused past `max` before it is decoded. */
function decoded(value, max, what) {
  if (typeof value !== 'string' || !value) throw fail('INVALID_PHOTO', `The ${what} must be a base64 JPEG`);
  const text = value.replace(/^data:image\/jpeg;base64,/, '');
  if (Math.floor((text.length * 3) / 4) - (text.endsWith('==') ? 2 : text.endsWith('=') ? 1 : 0) > max)
    throw fail('PHOTO_TOO_LARGE', `The ${what} must be at most ${Math.round(max / 1024)} kB`, 413);
  if (!BASE64.test(text) || text.length % 4) throw fail('INVALID_PHOTO', `The ${what} must be a base64 JPEG`);
  return Buffer.from(text, 'base64');
}
/** A JPEG within `edge` and upright, without its metadata segments; refused otherwise (the phone's canvas makes such). */
function cleanJpeg(data, edge, what) {
  const info = readJpeg(data);
  if (!info) throw fail('INVALID_PHOTO', `The ${what} must be a JPEG image`);
  if (Math.max(info.width, info.height) > edge) throw fail('PHOTO_TOO_BIG', `The ${what} must be at most ${edge} px on its longest side`, 422);
  if (info.orientation !== 1) throw fail('PHOTO_TURNED', `The ${what} must be upright (no EXIF turn)`, 422);
  return info;
}

const isBlank = v => v === undefined || v === null || v === '';
function cleanLabels(labels) {
  if (!labels || typeof labels !== 'object' || Array.isArray(labels)) throw fail('INVALID_LABELS', 'labels must be an object');
  const sex = isBlank(labels.sex) ? null : String(labels.sex);
  if (sex !== null && !SEXES.has(sex)) throw fail('INVALID_LABELS', 'labels.sex must be female, male or empty');
  const kind = isBlank(labels.photoKind) ? null : String(labels.photoKind);
  if (kind !== null && !KINDS.has(kind)) throw fail('INVALID_LABELS', 'labels.photoKind must be hand, live or collection');
  let species = null;
  if (!isBlank(labels.species)) {
    const s = labels.species;
    if (typeof s !== 'object' || Array.isArray(s)) throw fail('INVALID_LABELS', 'labels.species must be an object');
    const name = typeof s.name === 'string' ? s.name.trim().replace(/\s+/g, ' ') : '';
    if (!name || name.length > NAME_MAX || !/^[\p{L}][\p{L}\p{M}\d .,()'?/×-]*$/u.test(name)) throw fail('INVALID_LABELS', 'labels.species.name is not a taxon name');
    const rank = isBlank(s.rank) ? 'species' : String(s.rank);
    if (!RANKS.has(rank)) throw fail('INVALID_LABELS', 'labels.species.rank must be genus, species or subspecies');
    species = { name, rank, confirmed: s.confirmed === true, freeText: s.freeText === true };
  }
  if (!sex && !species) throw fail('NO_LABEL', 'A sex or a species label is needed', 422);
  if (sex && !POLICY.sexForAnySpecies && !species) throw fail('NO_LABEL', 'A sex label needs the species', 422);
  return { sex, kind, species };
}
/**
 * The day the photo was taken (YYYY-MM-DD), from a day, a date-time or EXIF's 2026:11:10; null when unknown, not
 * kept, or not believable (a camera clock reset to 1970, a day ahead): it is optional, so it never refuses the item.
 */
function cleanTaken(value, now) {
  if (isBlank(value) || !POLICY.keepTakenDay) return null;
  const day = String(value).slice(0, 10).replace(/^(\d{4}):(\d{2}):(\d{2})$/, '$1-$2-$3');
  const latest = new Date(now + 86_400_000).toISOString().slice(0, 10);
  if (!/^\d{4}-\d{2}-\d{2}$/.test(day) || Number.isNaN(Date.parse(day)) || day < '1990-01-01' || day > latest) return null;
  return day;
}
/** When the person pressed Enviar (the phone's clock): kept if it reads as a date, else null. */
function cleanCreated(value) {
  if (isBlank(value) || typeof value !== 'string' || value.length > 40 || Number.isNaN(Date.parse(value))) return null;
  return new Date(value).toISOString();
}
/**
 * What the AI said, as IdentifyView.vue prediction() sends it: only these fields, each of its own kind (names as taxon
 * names, the rest short tokens, numbers and an ISO time), so nothing else (a location, a person's name) can be kept
 * here by a request made by hand.
 */
const TOKEN = /^[A-Za-z0-9_.:+-]{0,120}$/;
const TAXON = /^[\p{L}][\p{L}\p{M}\d .,()'?/×_-]*$/u;
const token = v => (typeof v === 'string' && TOKEN.test(v) ? v : null);
const numeric = v => (typeof v === 'number' && Number.isFinite(v) ? v : null);
const taxon = v => (typeof v === 'string' && v.length <= NAME_MAX && TAXON.test(v) ? v : null);
const scored = (list, n) =>
  Array.isArray(list)
    ? list
        .slice(0, n)
        .map(x => (x && typeof x === 'object' ? { name: taxon(x.name), p: numeric(x.p) } : null))
        .filter(x => x?.name)
    : [];
export function cleanPrediction(value) {
  if (isBlank(value)) return null;
  if (typeof value !== 'object' || Array.isArray(value)) throw fail('INVALID_PREDICTION', 'prediction must be an object');
  const v = value;
  const sex = v.sex && typeof v.sex === 'object' && !Array.isArray(v.sex) ? { z: numeric(v.sex.z), model: token(v.sex.model), sha256: token(v.sex.sha256), version: token(v.sex.version) ?? numeric(v.sex.version) } : null;
  const wingBox = Array.isArray(v.wingBox) && v.wingBox.length === 4 && v.wingBox.every(x => numeric(x) !== null) ? v.wingBox : null;
  const at = typeof v.at === 'string' && v.at.length <= 40 && !Number.isNaN(Date.parse(v.at)) ? new Date(v.at).toISOString() : null;
  const text = JSON.stringify({
    source: token(v.source),
    model: token(v.model),
    head: token(v.head),
    headFile: token(v.headFile),
    status: token(v.status),
    wingBox,
    prior: token(v.prior),
    build: token(v.build),
    side: token(v.side),
    species: scored(v.species, 5),
    genus: scored(v.genus, 3),
    subspecies: scored(v.subspecies, 3),
    sex,
    pick: taxon(v.pick),
    at,
  });
  if (Buffer.byteLength(text) > PREDICTION_MAX) throw fail('INVALID_PREDICTION', 'prediction is too large', 413);
  return text;
}

const shape = r => ({
  id: r.id,
  createdAt: r.created_at,
  receivedAt: r.received_at,
  status: r.status,
  sha256: r.sha256,
  photoKind: r.photo_kind,
  sex: r.sex_label,
  species: r.species_label ? { name: r.species_label, rank: r.species_rank, confirmed: !!r.species_confirmed, freeText: !!r.free_text_name } : null,
  takenAt: r.taken_at,
  width: r.width,
  height: r.height,
  consentVersion: r.consent_version,
});

/**
 * The store. `dir`: where the files go (null: in memory); `freeBytes()`: free space on its disk (statfs by
 * default; Infinity in memory); `minFreeBytes`, `maxBytes`: the low-disk and store caps; `now()`: ms (tests).
 */
export function createContributions(store, { dir = null, freeBytes = null, minFreeBytes = MIN_FREE_BYTES, maxBytes = MAX_BYTES, limits = LIMITS, now = Date.now, contact = null } = {}) {
  const db = store.db;
  initContributions(db);
  const memory = new Map();
  if (dir) mkdirSync(dir, { recursive: true, mode: 0o700 });
  const free =
    freeBytes ??
    (() => {
      if (!dir) return Infinity;
      const fs = statfsSync(dir);
      return fs.bavail * fs.bsize;
    });
  const write = (name, data) => {
    if (!dir) return memory.set(name, data);
    mkdirSync(dirname(join(dir, name)), { recursive: true, mode: 0o700 });
    writeFileSync(join(dir, name), data, { mode: 0o600 });
  };
  const remove = name => (dir ? rmSync(join(dir, name), { force: true }) : memory.delete(name));
  const read = name => (dir ? readFileSync(join(dir, name)) : memory.get(name));

  /** Too many from this person lately: 429 with the seconds until one more fits. Deleted items count too. */
  function checkRate(userId, t) {
    for (const { count, windowMs } of limits) {
      const since = new Date(t - windowMs).toISOString();
      const times = db
        .prepare(
          `SELECT received_at t FROM contributions WHERE user_id = ? AND received_at > ?
           UNION ALL SELECT received_at t FROM contribution_tombstones WHERE user_id = ? AND received_at > ? ORDER BY t`,
        )
        .all(userId, since, userId, since)
        .map(r => Date.parse(r.t));
      if (times.length >= count) {
        const wait = Math.max(1, Math.ceil((times[times.length - count] + windowMs - t) / 1000));
        throw fail('RATE_LIMITED', `Too many photos sent; try again in ${wait} s`, 429, { 'retry-after': String(wait) });
      }
    }
  }
  /** Room for one more: the disk's free space and the store's own cap (507: the phone keeps the item and tries later). */
  function checkRoom(adding) {
    let left;
    try {
      left = free();
    } catch {
      left = 0;
    }
    const held = db.prepare('SELECT coalesce(sum(bytes + thumb_bytes), 0) n FROM contributions').get().n;
    if (left - adding < minFreeBytes || held + adding > maxBytes)
      throw fail('STORE_FULL', 'The server has no room for photos now; the phone keeps it and tries later', 507, { 'retry-after': '3600' });
  }

  /**
   * One item from the phone: `{ id, photo, thumb?, sha256, createdAt?, takenAt?, labels, prediction?, consent }`
   * (anything else, a `gps` from an old phone too, is ignored).
   * 201 stored; 200 `{ already: true }` for the same id with the same photo, or the same person's same photo under
   * another id: then `id` is the first one's, and the phone keeps that id (to list or delete it); 409 the same id with
   * other bytes; 410 an id deleted before (the phone drops the item).
   * `sha256` is of the bytes the phone sent; the stored file is those bytes without their metadata segments, so its
   * own hash can differ (lanes D and E hash the file when they need it).
   */
  function accept(body, user) {
    if (!user?.id) throw fail('AUTH_REQUIRED', 'Sign in required', 401);
    if (!POLICY.roles.includes(user.role)) throw fail('FORBIDDEN', 'Your account cannot send photos', 403);
    if (!body || typeof body !== 'object') throw fail('INVALID_BODY', 'A JSON object is required');
    const id = String(body.id ?? '').toLowerCase();
    if (!UUID.test(id)) throw fail('INVALID_ID', 'id must be a UUID');
    const sha = String(body.sha256 ?? '').toLowerCase();
    if (!SHA.test(sha)) throw fail('INVALID_SHA', 'sha256 must be the photo bytes’ SHA-256, in hex');
    if (db.prepare('SELECT 1 FROM contribution_tombstones WHERE id = ?').get(id))
      throw fail('CONTRIBUTION_DELETED', 'This photo was deleted and is not taken again', 410);
    const prior = db.prepare('SELECT * FROM contributions WHERE id = ?').get(id);
    if (prior) {
      if (prior.user_id !== user.id || prior.sha256 !== sha) throw fail('ID_CONFLICT', 'Another photo was sent under this id', 409);
      return { status: 200, body: { already: true, contribution: shape(prior) } };
    }
    const same = db.prepare('SELECT * FROM contributions WHERE user_id = ? AND sha256 = ?').get(user.id, sha);
    if (same) return { status: 200, body: { already: true, id: same.id, contribution: shape(same) } };

    const photoBytes = decoded(body.photo, PHOTO_MAX, 'photo');
    if (createHash('sha256').update(photoBytes).digest('hex') !== sha) throw fail('SHA_MISMATCH', 'sha256 does not match the photo', 400);
    const photo = cleanJpeg(photoBytes, PHOTO_EDGE, 'photo');
    const thumb = isBlank(body.thumb) ? null : cleanJpeg(decoded(body.thumb, THUMB_MAX, 'thumbnail'), THUMB_EDGE, 'thumbnail');
    const consent = String(body.consent?.version ?? body.consent ?? '');
    // An app from before v2 (its cached copy, until it updates): 503, which it takes as «try later», so it keeps the
    // photo; the updated app holds the item until its person accepts v2. Never stored under v1.
    if (consent === 'v1-draft') throw fail('CONSENT_OUTDATED', 'The consent changed: update the app and accept the new one', 503, { 'retry-after': '3600' });
    if (!CONSENTS.has(consent)) throw fail('CONSENT_REQUIRED', 'The consent shown must be accepted first', 422);
    const labels = cleanLabels(body.labels);
    const t = now();
    const takenAt = cleanTaken(body.takenAt, t);
    const prediction = cleanPrediction(body.prediction);
    checkRate(user.id, t);
    const thumbData = thumb?.clean ?? null;
    checkRoom(photo.clean.length + (thumbData?.length ?? 0));

    const receivedAt = new Date(t).toISOString();
    const file = `${receivedAt.slice(0, 7)}/${id}`;
    write(`${file}.jpg`, photo.clean);
    if (thumbData) write(`${file}.thumb.jpg`, thumbData);
    try {
      db.prepare(
        `INSERT INTO contributions(id, user_id, created_at, received_at, sha256, width, height, bytes, thumb_bytes, file, photo_kind, sex_label,
           species_label, species_rank, species_confirmed, free_text_name, prediction_json, taken_at, consent_version, licence)
         VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)`,
      ).run(
        id,
        user.id,
        cleanCreated(body.createdAt),
        receivedAt,
        sha,
        photo.width,
        photo.height,
        photo.clean.length,
        thumbData?.length ?? 0,
        file,
        labels.kind,
        labels.sex,
        labels.species?.name ?? null,
        labels.species?.rank ?? null,
        labels.species ? Number(labels.species.confirmed) : null,
        labels.species ? Number(labels.species.freeText) : null,
        prediction,
        takenAt,
        consent,
        LICENCE,
      );
    } catch (e) {
      remove(`${file}.jpg`);
      remove(`${file}.thumb.jpg`);
      throw e;
    }
    return { status: 201, body: { already: false, contribution: shape(db.prepare('SELECT * FROM contributions WHERE id = ?').get(id)) } };
  }

  /** The person's own items, newest first (all of them: each can be deleted from «Mis envíos»). */
  function mine(user) {
    return { contributions: db.prepare('SELECT * FROM contributions WHERE user_id = ? ORDER BY received_at DESC').all(user.id).map(shape) };
  }

  /**
   * Deletes an item: its owner, or an admin. Its files and row go; a tombstone stays. An id the server does not
   * have yet (a send still on its way) gets a tombstone too, so it is refused when it arrives (`removed: false`: no
   * photo was held under this id). Deleting again is fine.
   */
  function removeItem(rawId, user) {
    const id = String(rawId ?? '').toLowerCase();
    if (!UUID.test(id)) throw fail('INVALID_ID', 'id must be a UUID');
    if (db.prepare('SELECT 1 FROM contribution_tombstones WHERE id = ?').get(id)) return { deleted: id, already: true, removed: false };
    const row = db.prepare('SELECT * FROM contributions WHERE id = ?').get(id);
    if (row && row.user_id !== user.id && user.role !== 'admin') throw fail('FORBIDDEN', 'Only your own photos', 403);
    const deletedAt = new Date(now()).toISOString();
    db.exec('BEGIN IMMEDIATE');
    try {
      db.prepare('INSERT INTO contribution_tombstones(id, user_id, sha256, received_at, deleted_at, deleted_by) VALUES(?,?,?,?,?,?)').run(
        id,
        row?.user_id ?? user.id,
        row?.sha256 ?? null,
        row?.received_at ?? null,
        deletedAt,
        user.id,
      );
      if (row) db.prepare('DELETE FROM contributions WHERE id = ?').run(id);
      db.prepare('DELETE FROM contribution_verdicts WHERE contribution_id = ?').run(id);
      db.exec('COMMIT');
    } catch (e) {
      db.exec('ROLLBACK');
      throw e;
    }
    if (row) {
      remove(`${row.file}.jpg`);
      remove(`${row.file}.thumb.jpg`);
    }
    return { deleted: id, already: false, removed: !!row };
  }

  /** An item's bytes (`thumb` or the photo), for the review page (lane D, no route yet); null when gone. */
  function file(id, size = 'full') {
    const row = db.prepare('SELECT file, thumb_bytes FROM contributions WHERE id = ?').get(String(id));
    if (!row) return null;
    try {
      return read(`${row.file}${size === 'thumb' && row.thumb_bytes ? '.thumb' : ''}.jpg`) ?? null;
    } catch {
      return null;
    }
  }

  /** What the consent sheet shows from the server: its version, who is responsible, and the contact address (null when unset). */
  function consentInfo() {
    return { version: CONSENT_VERSION, controller: CONTROLLER, contact: contactEmail(contact) };
  }

  /**
   * The items waiting for review (`received`, oldest first), or those of another status, for a reviewer or admin.
   * Nobody's name or account is in the list: reviewers judge the photo and its labels only.
   */
  function queue(user, { status = 'received', limit = 100 } = {}) {
    if (!POLICY.reviewRoles.includes(user?.role)) throw fail('FORBIDDEN', 'Reviewer role required', 403);
    const s = ['received', 'accepted', 'rejected', 'unidentified'].includes(status) ? status : 'received';
    const n = Math.min(Math.max(Number(limit) || 100, 1), 500);
    const rows = db.prepare('SELECT * FROM contributions WHERE status = ? ORDER BY received_at LIMIT ?').all(s, n);
    return { status: s, contributions: rows.map(r => ({ ...shape(r), prediction: r.prediction_json ? JSON.parse(r.prediction_json) : null })) };
  }

  /**
   * A reviewer's or admin's verdict on one item: `{ verdict: accept | reject | unsure, reason? }`. Every verdict is
   * kept (contribution_verdicts) and the last one sets the status (VERDICTS). A deleted item: 410.
   */
  function review(rawId, body, user) {
    if (!POLICY.reviewRoles.includes(user?.role)) throw fail('FORBIDDEN', 'Reviewer role required', 403);
    const id = String(rawId ?? '').toLowerCase();
    if (!UUID.test(id)) throw fail('INVALID_ID', 'id must be a UUID');
    const verdict = String(body?.verdict ?? '');
    if (!VERDICTS.includes(verdict)) throw fail('INVALID_VERDICT', 'verdict must be accept, reject or unsure');
    const reason = isBlank(body?.reason) ? null : String(body.reason).trim().slice(0, REASON_MAX) || null;
    if (db.prepare('SELECT 1 FROM contribution_tombstones WHERE id = ?').get(id)) throw fail('CONTRIBUTION_DELETED', 'This photo was deleted', 410);
    const row = db.prepare('SELECT * FROM contributions WHERE id = ?').get(id);
    if (!row) throw fail('NOT_FOUND', 'No such photo', 404);
    const status = verdict === 'reject' ? 'rejected' : verdict === 'unsure' || (row.species_label && row.free_text_name) ? 'unidentified' : 'accepted';
    const at = new Date(now()).toISOString();
    db.exec('BEGIN IMMEDIATE');
    try {
      db.prepare('INSERT INTO contribution_verdicts(contribution_id, reviewer_id, verdict, reason, status, at) VALUES(?,?,?,?,?,?)').run(id, user.id, verdict, reason, status, at);
      db.prepare('UPDATE contributions SET status = ? WHERE id = ?').run(status, id);
      db.exec('COMMIT');
    } catch (e) {
      db.exec('ROLLBACK');
      throw e;
    }
    return { contribution: shape(db.prepare('SELECT * FROM contributions WHERE id = ?').get(id)), verdict, status };
  }

  return { accept, mine, remove: removeItem, file, dir, consentInfo, queue, review };
}
