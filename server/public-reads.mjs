// What Identificar, Buscar, Descargas and Compare read that is public, answered without a sign-in (Franz, 13 Nov 2026:
// «if all info we are showing is public»). Public: what the Wings Gallery publishes (its data/collection.json and
// data/photo_lookup.json: each butterfly's CAM_ID, Sex and its Photo_links photos by Drive file, JPEG/HEIC/PNG, not
// the raw camera files), and iNaturalist's photos under a Creative Commons licence with their credit. Writes,
// Monitoreo, the team's frozen stock, the sex model and the online identifier stay behind the sign-in
// (.work/public-gates-20261113/GATES.md).
//
// The anonymous answers have their own limits (signed-in people keep theirs):
// - a per-address limit per route (the address the local proxy saw, clientAddress);
// - /api/species-photos: at most ANON_MAX_NAMES names a request, each at most ANON_MAX_NAME_LENGTH characters; a name
//   the identifier's head or the curated list knows is read as their exact spelling (case variants are one name), and
//   only such a name goes to the existing background queue (one request a second) when not cached yet: a visitor
//   cannot fill the queue with made-up or re-cased names. The answer keeps only the fields listed below (anything
//   added to the server's answer later is not public until listed here);
// - /api/identify/ref-photo: only photos the server already lists (a CC-licensed iNaturalist photo id in its species
//   cache, or an id in the curated list): no open proxy;
// - /api/photo/<id>: only files Photo_links lists under a CAM name, not raw camera files (what the gallery's
//   photo_lookup.json lists), 400 px or 1600 px as for the team; a photo card's ?card= is the team's. A photo not
//   cached yet is fetched from Drive within one budget shared by every visitor (DriveBudget).

import { readFile, stat } from 'node:fs/promises';
import { join } from 'node:path';
import { MODELS } from './offline-id.mjs';
import { photoIndex } from './photodata.mjs';

export const ANON_MAX_NAMES = 20;
export const ANON_MAX_NAME_LENGTH = 120;
const fail = (code, message, status = 400) => Object.assign(new Error(message), { code, status });

/**
 * The client's address: the last X-Forwarded-For entry when the connection is local (the one the local reverse proxy
 * added: Caddy replaces a visitor's own header, tailscale serve appends to it; deploy/Caddyfile.fragment), else the
 * connection's own. A visitor's spoofed first entries are never the key.
 */
export function clientAddress(req) {
  const socket = req.socket?.remoteAddress || 'unknown';
  const local = /^(::1|127\.|::ffff:127\.)/.test(socket);
  const forwarded = local
    ? String(req.headers['x-forwarded-for'] || '')
        .split(',')
        .map(s => s.trim())
        .filter(Boolean)
        .at(-1) ?? ''
    : '';
  return forwarded || socket;
}

/**
 * At most `limit` requests per address in each window of `windowMs` (a fixed window: one count per address, so a
 * flood costs no memory beyond its address). check() counts the request and throws 429 past the limit. Expired
 * windows are swept at most once a minute; past `maxEntries` addresses the oldest windows go first.
 */
export class AddressLimiter {
  constructor({ limit, windowMs = 15 * 60_000, maxEntries = 50_000, now = () => Date.now() } = {}) {
    this.limit = limit;
    this.windowMs = windowMs;
    this.maxEntries = maxEntries;
    this.now = now;
    this.counts = new Map();
    this.swept = now();
  }
  check(req) {
    const t = this.now();
    const key = clientAddress(req);
    if (t - this.swept >= 60_000) {
      this.swept = t;
      for (const [k, v] of this.counts) if (t - v.start >= this.windowMs) this.counts.delete(k);
    }
    let c = this.counts.get(key);
    if (!c || t - c.start >= this.windowMs) {
      // A new window goes to the end: the map stays in the order the windows started.
      this.counts.delete(key);
      c = { start: t, n: 0 };
      this.counts.set(key, c);
      for (const k of this.counts.keys()) {
        if (this.counts.size <= this.maxEntries) break;
        this.counts.delete(k);
      }
    }
    if (++c.n > this.limit) {
      const left = c.start + this.windowMs - t;
      throw Object.assign(fail('RATE_LIMITED', `Too many requests without signing in; wait ${Math.ceil(left / 60_000)} min or sign in`, 429), {
        headers: { 'retry-after': String(Math.ceil(left / 1000)) },
      });
    }
  }
}

/** A token bucket: `perSecond` on average, `burst` at once. tryTake(): whether one was there (and takes it). */
export class TokenBucket {
  constructor({ perSecond, burst, now = () => Date.now() } = {}) {
    this.perSecond = perSecond;
    this.burst = burst;
    this.now = now;
    this.tokens = burst;
    this.at = now();
  }
  tryTake() {
    const t = this.now();
    this.tokens = Math.min(this.burst, this.tokens + ((t - this.at) / 1000) * this.perSecond);
    this.at = t;
    if (this.tokens < 1) return false;
    this.tokens -= 1;
    return true;
  }
}

/**
 * One budget shared by every visitor for the photos that are not cached yet (a Drive fetch each): 2 a second on
 * average, 20 at once. take() throws 503 when it is spent; cached photos never ask it.
 */
export class DriveBudget extends TokenBucket {
  constructor({ perSecond = 2, burst = 20, now } = {}) {
    super({ perSecond, burst, now });
  }
  take() {
    if (!this.tryTake())
      throw Object.assign(fail('BUSY', 'Many photos asked for right now; try again in a moment or sign in', 503), { headers: { 'retry-after': '2' } });
  }
}

/**
 * Which of a visitor's names may join the background iNaturalist queue (the head is all of Lepidoptera, 11,965
 * names: a visitor must not queue it for everyone): only while fewer than `waiting` names wait, and at most `perDay`
 * a day for all visitors together (a bucket refilled over the day). Past either, the cache alone answers.
 */
export function queueAdmission(speciesPhotos, { waiting = 20, perDay = 300, now } = {}) {
  const day = new TokenBucket({ perSecond: perDay / 86_400, burst: perDay, now });
  return () => speciesPhotos.queued() < waiting && day.tryTake();
}

const text = v => (v === null || v === undefined ? '' : String(v));
const str = v => (typeof v === 'string' ? v : null);
/** A raw camera file (Photo_links lists them beside the JPEG; the gallery does not publish them). */
const RAW = /\.(cr2|orf|nef|arw|raf|rw2|dng)$/i;

/** An iNaturalist photo's fields as /api/species-photos gives them, only those that are public (CC photo, credit). */
const publicInatPhoto = p => ({
  id: p.id,
  thumb: str(p.thumb),
  url: str(p.url),
  attribution: text(p.attribution),
  license: text(p.license),
  ...(str(p.observation) ? { observation: p.observation } : {}),
  ...(p.country !== undefined ? { country: str(p.country) } : {}),
});
/** A cached iNaturalist answer, with only its public fields (the taxon's page and names, its CC photos). */
function publicInat(inat) {
  if (!inat || typeof inat !== 'object') return null;
  const t = inat.taxon ?? {};
  return {
    taxon: { id: t.id ?? null, name: str(t.name), rank: str(t.rank), common: str(t.common), url: str(t.url) },
    level: str(inat.level),
    ...(str(inat.synonymOf) ? { synonymOf: inat.synonymOf } : {}),
    photos: (Array.isArray(inat.photos) ? inat.photos : []).filter(p => p && p.license).map(publicInatPhoto),
  };
}

/** Whether a visitor may have a Drive photo: a file Photo_links lists under a CAM name, not a raw camera file. */
export function publicPhotoFile(store, id) {
  const f = typeof id === 'string' ? photoIndex(store).files.get(id) : null;
  return f?.source === 'Photo_links' && !RAW.test(f.name);
}

const spaced = name => text(name).trim().replace(/\s+/g, ' ');

/**
 * /api/species-photos for a visitor: `names` (at most ANON_MAX_NAMES, each at most ANON_MAX_NAME_LENGTH characters)
 * → { name: { inat, pending, specimens } } with only public fields, keyed by each name exactly as sent. `canonical(name)`:
 * the head's or curated list's exact spelling of a name (any case), or null; only those are looked up and may go to
 * the background queue, when `admit()` agrees (queueAdmission), so case variants share one cache entry and one job. Specimens only with a public photo
 * (publicPhotoFile: the gallery's photo_lookup.json), the first public dorsal and ventral of the CAM, as
 * { cam, sex, dorsal, ventral } (collection.json's CAM_ID and Sex).
 */
export function anonymousSpeciesPhotos({ speciesPhotos, store, admit = () => true }, names, canonical) {
  if (!Array.isArray(names) || names.length > ANON_MAX_NAMES)
    throw fail('INVALID_NAMES', `names: a list of at most ${ANON_MAX_NAMES} without signing in`);
  if (names.some(n => typeof n !== 'string' || n.length > ANON_MAX_NAME_LENGTH))
    throw fail('INVALID_NAMES', `names: each a text of at most ${ANON_MAX_NAME_LENGTH} characters`);
  const asked = names.map(n => ({ sent: n, key: spaced(n), name: canonical(n) }));
  const fetchable = new Set(asked.map(a => a.name).filter(Boolean));
  const found = speciesPhotos.photosOf(
    [...new Set(asked.map(a => a.name ?? a.key))],
    { mayFetch: n => fetchable.has(n) && admit() },
  );
  const index = photoIndex(store);
  const first = list => (list ?? []).find(id => publicPhotoFile(store, id)) ?? null;
  const out = {};
  for (const { sent, key, name } of asked) {
    const e = found[name ?? key];
    if (!key || !e || out[sent]) continue;
    out[sent] = {
      inat: publicInat(e.inat),
      pending: e.pending === true,
      specimens: (e.specimens ?? []).flatMap(s => {
        const photos = index.byCam.get(text(s.cam).toUpperCase());
        const dorsal = publicPhotoFile(store, s.dorsal) ? s.dorsal : first(photos?.dorsal);
        if (!dorsal) return [];
        const ventral = publicPhotoFile(store, s.ventral) ? s.ventral : first(photos?.ventral);
        return [{ cam: text(s.cam), sex: str(s.sex), dorsal, ventral }];
      }),
    };
  }
  return out;
}

const INAT_ID = /\/photos\/(\d+)\//;
/** The iNaturalist photo id of a photo URL, or null. */
export const inatPhotoId = url => INAT_ID.exec(text(url))?.[1] ?? null;

/**
 * The iNaturalist photos the server lists (the CC-licensed ids in its species cache, and the ids of the curated list),
 * for the anonymous photo proxy; and the exact spelling of the names a visitor's request may look up (the head's
 * species, the curated list's). The curated list is read again every 10 minutes; the head again when its file
 * changes (a new identifier release), and a failed or empty read is tried again after `retryMs`.
 */
export function createPublicLists(store, { webRoot, identifierDir = null, retryMs = 60_000, curatedMs = 600_000, now = () => Date.now() } = {}) {
  let ids = { stamp: null, set: new Set() };
  let curated = { at: -Infinity, ids: new Set(), names: [] };
  let head = { at: -Infinity, stamp: null, names: [] };
  let spellings = { from: null, map: new Map() };
  const headPath = identifierDir ? join(identifierDir, MODELS.find(m => m.role === 'head-meta').path) : null;
  // One read at a time: requests that come during a read wait for it (none sees an empty list meanwhile).
  let reading = { curated: null, head: null };
  const once = (k, fn) => (reading[k] ??= fn().finally(() => (reading[k] = null)));
  const loadCurated = () => once('curated', readCurated);
  const loadHead = () => once('head', readHead);
  async function readCurated() {
    if (now() - curated.at < (curated.names.length ? curatedMs : retryMs)) return curated;
    const next = { at: now(), ids: new Set(), names: [] };
    try {
      const d = JSON.parse(await readFile(join(webRoot, 'identificar', 'curated.json'), 'utf8'));
      for (const [name, taxon] of Object.entries(d?.taxa ?? {})) {
        next.names.push(name);
        for (const p of taxon?.field ?? []) if (/^\d+$/.test(text(p.id))) next.ids.add(text(p.id));
      }
    } catch {
      // not built here: the species cache alone, and tried again after retryMs
    }
    curated = next;
    return curated;
  }
  async function readHead() {
    if (!headPath || now() - head.at < retryMs) return head;
    try {
      const s = await stat(headPath);
      const stamp = `${s.size}:${s.mtimeMs}`;
      if (stamp === head.stamp && head.names.length) return (head = { ...head, at: now() });
      const names = JSON.parse(await readFile(headPath, 'utf8'))?.species;
      head = { at: now(), stamp, names: Array.isArray(names) ? names.filter(n => typeof n === 'string') : [] };
    } catch {
      head = { at: now(), stamp: null, names: [] };
    }
    return head;
  }
  function cacheIds() {
    const s = store.db.prepare('SELECT count(*) n, max(fetched_at) f FROM species_photo_cache').get();
    const stamp = `${s.n}:${s.f}`;
    if (ids.stamp === stamp) return ids.set;
    const set = new Set();
    for (const r of store.db.prepare('SELECT data_json FROM species_photo_cache WHERE data_json IS NOT NULL').all()) {
      try {
        // Only the photos a visitor's answer lists: those with a licence.
        for (const p of JSON.parse(r.data_json)?.photos ?? []) {
          if (!p?.license) continue;
          if (p.id != null) set.add(String(p.id));
          for (const u of [p.thumb, p.url]) {
            const id = inatPhotoId(u);
            if (id) set.add(id);
          }
        }
      } catch {
        // a broken row lists nothing
      }
    }
    ids = { stamp, set };
    return set;
  }
  return {
    /** Whether the iNaturalist photo at `url` is one the server lists. */
    async knownPhoto(url) {
      const id = inatPhotoId(url);
      if (!id) return false;
      return cacheIds().has(id) || (await loadCurated()).ids.has(id);
    },
    /** name → its exact spelling in the head or the curated list (any case, spaces as one), or null. */
    async canonicalNames() {
      const [h, c] = await Promise.all([loadHead(), loadCurated()]);
      if (spellings.from?.[0] !== h.names || spellings.from?.[1] !== c.names) {
        const map = new Map();
        for (const n of [...h.names, ...c.names]) {
          const k = spaced(n).toLowerCase();
          if (k && !map.has(k)) map.set(k, spaced(n));
        }
        spellings = { from: [h.names, c.names], map };
      }
      const map = spellings.map;
      return name => map.get(spaced(name).toLowerCase()) ?? null;
    },
  };
}
