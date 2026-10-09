// What a species looks like, for the Monitoreo report's species table: photos
// of live butterflies from iNaturalist and the team's own specimen photos.
//
// iNaturalist: the taxon's curated photos (taxon_photos), only those under a
// Creative Commons licence (shown with their attribution), topped up with
// research-grade observation photos with a licence when there are fewer than
// 3. A name is looked up as written (subspecies), then as its species, then
// its genus; a synonym iNaturalist knows is followed (matched_term). Answers
// are kept in SQLite (species_photo_cache) for 30 days (7 when nothing was
// found) and an old one is still shown while it is fetched again. Requests to
// iNaturalist go one at a time, at most one a second (their guidance), from a
// queue in the background: a request to the app never waits on iNaturalist,
// it answers with what is cached and "pending" for the rest.
//
// Specimen photos: the Collection_data rows of the species with photos in
// Photo_links (server/photodata.mjs); the browser gets them from /api/photo.

import { photoIndex } from './photodata.mjs';

const API = 'https://api.inaturalist.org/v1';
const DAY = 864e5;
const LEPIDOPTERA = 47157;
const SPECIES_OR_LOWER = new Set(['species', 'subspecies', 'variety', 'form', 'hybrid', 'infrahybrid']);
const MAX_PHOTOS = 6;
const MAX_SPECIMENS = 4;
export const MAX_NAMES = 300;

const text = v => (v === null || v === undefined ? '' : String(v).trim());
const fail = (code, message, status = 400) => Object.assign(new Error(message), { code, status });

/**
 * A table's name as the names to try on iNaturalist, most precise first:
 * "Hyposcada illinissa ida" → [trinomial, binomial, genus]. Forms ("f. x"),
 * "sp.", "cf." and notes are dropped; null when there is no genus.
 */
export function lookupNames(name) {
  const words = text(name)
    .replace(/\(.*?\)/g, ' ')
    .split(/\s+/)
    .filter(w => w && !/^(cf|aff|nr)\.?$/i.test(w));
  const genus = words[0];
  // The report's placeholder for rows without a species.
  if (/^sin$/i.test(genus) || !genus || !/^[A-Z][a-z]+$/.test(genus)) return null;
  const out = [];
  const species = words[1];
  if (species && /^[a-z][a-z-]+$/.test(species) && !/^spp?$/.test(species)) {
    const ssp = words[2];
    if (ssp && /^[a-z][a-z-]{2,}$/.test(ssp) && !words[3]) out.push({ name: `${genus} ${species} ${ssp}`, level: 'subspecies' });
    out.push({ name: `${genus} ${species}`, level: 'species' });
  }
  out.push({ name: genus, level: 'genus' });
  return out;
}

/** iNat photo URLs end in /square.jpg; the other sizes swap that segment. */
export const photoSize = (url, size) =>
  text(url).replace(/\/(square|thumb|small|medium|large|original)\.(jpe?g|png|gif|webp)/i, `/${size}.$2`);

const photoOf = (photo, extra = {}) => ({
  id: photo.id,
  thumb: photoSize(photo.url, 'small'),
  url: photoSize(photo.url, 'large'),
  attribution: text(photo.attribution),
  license: text(photo.license_code).toUpperCase(),
  ...extra,
});

/** The search result that is the asked-for taxon (or a synonym of it), among Lepidoptera. */
export function pickTaxon(results, wanted) {
  const want = wanted.name.toLowerCase();
  const fits = r =>
    r?.id &&
    r.is_active !== false &&
    (!Array.isArray(r.ancestor_ids) || r.ancestor_ids.includes(LEPIDOPTERA)) &&
    (wanted.level === 'genus' ? r.rank === 'genus' : SPECIES_OR_LOWER.has(r.rank));
  const list = (results ?? []).filter(fits);
  const exact = list.find(r => text(r.name).toLowerCase() === want);
  if (exact) return exact;
  // A synonym: iNaturalist found it under the old name; species-level names only
  // (a subspecies not in iNaturalist falls back to its species instead).
  if (wanted.level === 'subspecies') return null;
  return list.find(r => text(r.matched_term).toLowerCase() === want && text(r.name).split(' ').length <= 2) ?? null;
}

/**
 * @param store the app's store (the cache table lives in its database)
 * @param options.fetchImpl fetch (tests pass a fake one)
 * @param options.intervalMs time between two requests to iNaturalist
 * @param options.enabled false: no requests to iNaturalist (specimen photos only)
 */
export function createSpeciesPhotos(
  store,
  {
    fetchImpl = fetch,
    intervalMs = 1100,
    ttlMs = 30 * DAY,
    missTtlMs = 7 * DAY,
    retryMs = 15 * 60_000,
    now = () => Date.now(),
    sleep = ms => new Promise(resolve => setTimeout(resolve, ms)),
    enabled = true,
  } = {},
) {
  const db = store.db;
  db.exec(`CREATE TABLE IF NOT EXISTS species_photo_cache(name TEXT PRIMARY KEY, data_json TEXT, fetched_at INTEGER NOT NULL)`);
  const queue = new Set();
  const failed = new Map();
  let running = null;
  let nextAt = 0;
  let pausedUntil = 0;

  async function get(path) {
    const wait = Math.max(nextAt, pausedUntil) - now();
    if (wait > 0) await sleep(wait);
    nextAt = now() + intervalMs;
    const response = await fetchImpl(`${API}${path}`, {
      headers: { accept: 'application/json', 'user-agent': 'Ithomiini-database (Ikiam monitoring)' },
      signal: AbortSignal.timeout(15000),
    });
    if (response.status === 429 || response.status >= 500) {
      // Too many requests (or iNaturalist down): everyone waits a minute.
      pausedUntil = now() + 60_000;
      throw fail('INAT_BUSY', `iNaturalist answered ${response.status}`, 503);
    }
    if (!response.ok) throw fail('INAT_ERROR', `iNaturalist answered ${response.status}`, 502);
    return response.json();
  }

  /** The photos of one name, or null when iNaturalist has none for it nor its species or genus. */
  async function lookup(name) {
    for (const wanted of lookupNames(name) ?? []) {
      const rank = wanted.level === 'genus' ? 'genus' : 'species,subspecies';
      const found = await get(`/taxa?q=${encodeURIComponent(wanted.name)}&rank=${rank}&per_page=10`);
      const hit = pickTaxon(found.results, wanted);
      if (!hit) continue;
      const taxon = (await get(`/taxa/${hit.id}`)).results?.[0] ?? hit;
      let photos = (taxon.taxon_photos ?? [])
        .map(tp => tp.photo)
        .filter(p => p?.url && p.license_code)
        .slice(0, MAX_PHOTOS)
        .map(p => photoOf(p));
      // Fewer than 3 curated photos with a licence: research-grade observations fill the gallery.
      if (photos.length < 3 && wanted.level !== 'genus') {
        const obs = await get(
          `/observations?taxon_id=${hit.id}&quality_grade=research&photo_licensed=true&photos=true&order_by=votes&per_page=${MAX_PHOTOS}`,
        );
        const have = new Set(photos.map(p => p.id));
        for (const o of obs.results ?? []) {
          const p = (o.photos ?? []).find(p => p?.url && p.license_code && !have.has(p.id));
          if (p && photos.length < MAX_PHOTOS)
            photos.push(photoOf(p, { observation: `https://www.inaturalist.org/observations/${o.id}` }));
        }
      }
      // A subspecies or species without licensed photos: try the next, broader name.
      if (!photos.length) continue;
      return {
        taxon: {
          id: taxon.id,
          name: taxon.name,
          rank: taxon.rank,
          common: text(taxon.preferred_common_name) || null,
          url: `https://www.inaturalist.org/taxa/${taxon.id}`,
        },
        level: wanted.level,
        ...(text(taxon.name).toLowerCase() !== wanted.name.toLowerCase() ? { synonymOf: taxon.name } : {}),
        photos,
      };
    }
    return null;
  }

  function work() {
    running ??= (async () => {
      while (queue.size) {
        const name = queue.values().next().value;
        try {
          const data = await lookup(name);
          db.prepare('INSERT OR REPLACE INTO species_photo_cache(name,data_json,fetched_at) VALUES(?,?,?)').run(
            name,
            data ? JSON.stringify(data) : null,
            now(),
          );
          failed.delete(name);
        } catch (e) {
          failed.set(name, now() + retryMs);
          console.warn(`species photos: ${name}: ${e.message}`);
        }
        queue.delete(name);
      }
    })().finally(() => {
      running = null;
      // A name asked for while the loop was ending.
      if (queue.size) work();
    });
    return running;
  }

  /** name → { inat, pending, specimens }; names not cached (or old) are fetched in the background. */
  function photosOf(names) {
    if (!Array.isArray(names) || names.length > MAX_NAMES) throw fail('INVALID_NAMES', `names: a list of at most ${MAX_NAMES}`);
    const read = db.prepare('SELECT data_json, fetched_at FROM species_photo_cache WHERE name=?');
    const specimens = specimenIndex(store);
    const out = {};
    for (const raw of names) {
      const name = text(raw).replace(/\s+/g, ' ');
      if (!name || out[name]) continue;
      const row = read.get(name);
      const data = row?.data_json ? JSON.parse(row.data_json) : null;
      const old = !row || now() - row.fetched_at > (data ? ttlMs : missTtlMs);
      const canFetch = enabled && lookupNames(name) && (failed.get(name) ?? 0) <= now();
      if (old && canFetch) queue.add(name);
      out[name] = {
        inat: data,
        // Still to be asked (no answer yet); an old answer is shown meanwhile.
        pending: !row && queue.has(name),
        specimens: specimensOf(specimens, name),
      };
    }
    if (queue.size) work();
    return out;
  }

  return { photosOf, lookup, idle: () => running ?? Promise.resolve() };
}

const specimenCache = new WeakMap();
/** "genus species" → its collected individuals with photos, newest CAM first ({ cam, sex, subspecies, dorsal, ventral }). */
export function specimenIndex(store) {
  const index = photoIndex(store);
  const db = store.db;
  const state = db
    .prepare("SELECT count(*) n, max(updated_at) u FROM records WHERE sheet='Collection_data' AND missing=0")
    .get();
  const stamp = `${state.n}:${state.u}:${index.stamp}`;
  const hit = specimenCache.get(store);
  if (hit?.stamp === stamp) return hit;
  const bySpecies = new Map();
  const seen = new Set();
  for (const r of db
    .prepare("SELECT values_json FROM records WHERE sheet='Collection_data' AND missing=0 AND observed=1 AND row_num>1")
    .all()) {
    let values;
    try {
      values = JSON.parse(r.values_json) ?? {};
    } catch {
      continue;
    }
    const words = text(values.SPECIES).split(/\s+/);
    if (words.length < 2) continue;
    for (const cam of [text(values.CAM_ID).toUpperCase(), text(values.CAM_ID_insectary).toUpperCase()]) {
      const photos = index.byCam.get(cam);
      if (!photos?.dorsal.length || seen.has(cam)) continue;
      seen.add(cam);
      const key = `${words[0]} ${words[1]}`.toLowerCase();
      (bySpecies.get(key) ?? bySpecies.set(key, []).get(key)).push({
        cam,
        sex: text(values.Sex).toLowerCase() || null,
        subspecies: (words[2] ?? text(values.Subspecies_Form)).toLowerCase(),
        dorsal: photos.dorsal[0],
        ventral: photos.ventral[0] ?? null,
      });
    }
  }
  for (const list of bySpecies.values()) list.sort((a, b) => b.cam.localeCompare(a.cam));
  const out = { stamp, bySpecies };
  specimenCache.set(store, out);
  return out;
}

/** A few specimens of the name: its subspecies first, then the rest of the species; both sexes when there are. */
export function specimensOf(index, name) {
  const words = text(name).split(/\s+/);
  if (words.length < 2) return [];
  const list = index.bySpecies.get(`${words[0]} ${words[1]}`.toLowerCase()) ?? [];
  const ssp = (words[2] ?? '').toLowerCase();
  const same = ssp ? list.filter(s => s.subspecies === ssp) : [];
  const ranked = same.length ? [...same, ...list.filter(s => s.subspecies !== ssp)] : list;
  const picked = [];
  for (const sex of ['female', 'male']) {
    const one = (same.length ? same : list).find(s => s.sex === sex);
    if (one) picked.push(one);
  }
  for (const s of ranked) if (picked.length < MAX_SPECIMENS && !picked.includes(s)) picked.push(s);
  return picked
    .slice(0, MAX_SPECIMENS)
    .map(({ cam, sex, dorsal, ventral }) => ({ cam, sex, dorsal, ventral }));
}
