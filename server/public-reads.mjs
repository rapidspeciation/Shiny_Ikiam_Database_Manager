// What Identificar, Buscar, Descargas and Compare read that is public, answered without a sign-in (Franz, 13 Nov 2026:
// «if all info we are showing is public»). Public: what the Wings Gallery publishes (its data/collection.json and
// data/photo_lookup.json: each butterfly's CAM_ID, Sex and its Photo_links photos by Drive file), and iNaturalist's
// photos under a Creative Commons licence with their credit. Writes, Monitoreo, the team's frozen stock, the sex model
// and the online identifier stay behind the sign-in (.work/public-gates-20261113/GATES.md).
//
// The anonymous answers have their own limits (signed-in people keep theirs):
// - a per-address limit per route (the same address as the sign-in limits: the proxy's forwarded one);
// - /api/species-photos: at most ANON_MAX_NAMES names a request; a name iNaturalist was not asked about yet is
//   looked up by the existing background queue (one request a second) only when the identifier's head or the
//   curated list names it, so a visitor cannot fill the queue with made-up names; the answer keeps only the fields
//   listed below (anything added to the server's answer later is not public until listed here);
// - /api/identify/ref-photo: only photos the server already lists (an iNaturalist photo id in its species cache or in
//   the curated list): no open proxy;
// - /api/photo/<id>: only files Photo_links lists under a CAM name (what the gallery's photo_lookup.json lists),
//   400 px or 1600 px as for the team; a photo card's ?card= is the team's.

import { readFile } from 'node:fs/promises';
import { join } from 'node:path';
import { photoIndex } from './photodata.mjs';

export const ANON_MAX_NAMES = 20;
const fail = (code, message, status = 400) => Object.assign(new Error(message), { code, status });

/** The client's address: the connection's, or the proxy's forwarded one when the connection is local (the reverse proxy). */
export function clientAddress(req) {
  const socket = req.socket?.remoteAddress || 'unknown';
  const local = /^(::1|127\.|::ffff:127\.)/.test(socket);
  const forwarded = local ? String(req.headers['x-forwarded-for'] || '').split(',')[0].trim() : '';
  return forwarded || socket;
}

/**
 * At most `limit` requests per address in each window of `windowMs` (a fixed window: one count per address, so a
 * flood costs no memory beyond its address). check() counts the request and throws 429 past the limit.
 */
export class AddressLimiter {
  constructor({ limit, windowMs = 15 * 60_000, now = () => Date.now() } = {}) {
    this.limit = limit;
    this.windowMs = windowMs;
    this.now = now;
    this.counts = new Map();
  }
  check(req) {
    const t = this.now();
    const key = clientAddress(req);
    let c = this.counts.get(key);
    if (!c || t - c.start >= this.windowMs) {
      // Old windows go when a new one starts somewhere (bounded by the addresses of one window).
      if (this.counts.size > 10_000) for (const [k, v] of this.counts) if (t - v.start >= this.windowMs) this.counts.delete(k);
      c = { start: t, n: 0 };
      this.counts.set(key, c);
    }
    if (++c.n > this.limit) {
      const left = c.start + this.windowMs - t;
      throw Object.assign(fail('RATE_LIMITED', `Too many requests without signing in; wait ${Math.ceil(left / 60_000)} min or sign in`, 429), {
        headers: { 'retry-after': String(Math.ceil(left / 1000)) },
      });
    }
  }
}

const text = v => (v === null || v === undefined ? '' : String(v));
const str = v => (typeof v === 'string' ? v : null);

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

/**
 * /api/species-photos for a visitor: `names` (at most ANON_MAX_NAMES) → { name: { inat, pending, specimens } } with
 * only public fields; specimens only with a photo Photo_links lists (the gallery's photo_lookup.json), as
 * { cam, sex, dorsal, ventral } (collection.json's CAM_ID and Sex, photo_lookup.json's files).
 * `known(name)`: whether a name not cached yet may go to the background queue.
 */
export function anonymousSpeciesPhotos({ speciesPhotos, store }, names, known) {
  if (!Array.isArray(names) || names.length > ANON_MAX_NAMES)
    throw fail('INVALID_NAMES', `names: a list of at most ${ANON_MAX_NAMES} without signing in`);
  const found = speciesPhotos.photosOf(names, { mayFetch: known });
  const files = photoIndex(store).files;
  const listed = id => typeof id === 'string' && files.get(id)?.source === 'Photo_links';
  const out = {};
  for (const [name, e] of Object.entries(found))
    out[name] = {
      inat: publicInat(e.inat),
      pending: e.pending === true,
      specimens: (e.specimens ?? [])
        .filter(s => listed(s.dorsal))
        .map(s => ({ cam: text(s.cam), sex: str(s.sex), dorsal: s.dorsal, ventral: listed(s.ventral) ? s.ventral : null })),
    };
  return out;
}

/** Whether a visitor may have a Drive photo: a file Photo_links lists under a CAM name (not one from elsewhere). */
export function publicPhotoFile(store, id) {
  return photoIndex(store).files.get(id)?.source === 'Photo_links';
}

const INAT_ID = /\/photos\/(\d+)\//;
/** The iNaturalist photo id of a photo URL, or null. */
export const inatPhotoId = url => INAT_ID.exec(text(url))?.[1] ?? null;

/**
 * The iNaturalist photos the server lists (the ids in its species cache and in the curated list), for the anonymous
 * photo proxy; and the names a visitor's request may send to the background queue (the head's species, the curated
 * list's). Both read again when their source changed.
 */
export function createPublicLists(store, { webRoot, identifierDir = null, headSpecies = null } = {}) {
  let ids = { stamp: null, set: new Set() };
  let curated = { at: 0, ids: new Set(), names: new Set() };
  let head = new Set();
  let headTried = false;
  async function loadCurated() {
    // Read again at most every 10 minutes (a deploy replaces the file).
    if (Date.now() - curated.at < 600_000) return curated;
    const next = { at: Date.now(), ids: new Set(), names: new Set() };
    try {
      const d = JSON.parse(await readFile(join(webRoot, 'identificar', 'curated.json'), 'utf8'));
      for (const [name, taxon] of Object.entries(d?.taxa ?? {})) {
        next.names.add(name);
        for (const p of taxon?.field ?? []) if (/^\d+$/.test(text(p.id))) next.ids.add(text(p.id));
      }
    } catch {
      // not built here: the species cache alone
    }
    curated = next;
    return curated;
  }
  function cacheIds() {
    const s = store.db.prepare('SELECT count(*) n, max(fetched_at) f FROM species_photo_cache').get();
    const stamp = `${s.n}:${s.f}`;
    if (ids.stamp === stamp) return ids.set;
    const set = new Set();
    for (const r of store.db.prepare('SELECT data_json FROM species_photo_cache WHERE data_json IS NOT NULL').all()) {
      try {
        for (const p of JSON.parse(r.data_json)?.photos ?? []) {
          if (p?.id != null) set.add(String(p.id));
          for (const u of [p?.thumb, p?.url]) {
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
    /** A predicate: the names a visitor may send to the background queue. */
    async knownNames() {
      if (!headTried && identifierDir && headSpecies) {
        headTried = true;
        try {
          head = new Set((await headSpecies(identifierDir)).map(n => text(n).toLowerCase()));
        } catch {
          head = new Set();
        }
      }
      const c = await loadCurated();
      const lower = new Set([...c.names].map(n => n.toLowerCase()));
      return name => {
        const n = text(name).trim().replace(/\s+/g, ' ').toLowerCase();
        return head.has(n) || lower.has(n);
      };
    },
  };
}
