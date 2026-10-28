// Read-only tools on the project's public sites (docs/assistant-tools-maps-gallery.md):
//   maps_localities    places with the most records of some taxa (the maps' occurrence records), with map links
//   maps_species       the taxa recorded in a region
//   host_plants        host plants of a butterfly, or the Ithomiini on a plant, with their sources
//   gallery_specimens  collection specimens with wing photos (the workbook and Photo_links), gallery links, thumbnails
// The maps' data files are read over HTTPS from the published site, at most every 6 hours (and parsed again only
// when their ETag changed), and kept in memory as compact lists. The workbook is read with SELECT only. Nothing is
// written anywhere.

import { driveIdOf, specimenName } from './photo-links.mjs';

export const MAPS_URL = 'https://rapidspeciation.github.io/ithomiini_maps/';
export const GALLERY_URL = 'https://rapidspeciation.github.io/Shiny_Ikiam_Wings_Gallery/';
const ELEVATION_API = 'https://api.open-meteo.com/v1/elevation';

/** The maps' sources: the key the tools take, and the name the maps app shows (its `source` URL parameter). */
export const SOURCES = {
  sanger: 'Sanger Institute',
  gbif_unam: 'GBIF (UNAM)',
  gbif_other: 'GBIF (Other Institutions)',
  dore: 'Dore et al. (2022)',
  inaturalist: 'iNaturalist',
};
const SOURCE_KEYS = Object.keys(SOURCES);
const KEY_OF = Object.fromEntries(Object.entries(SOURCES).map(([k, v]) => [v, k]));

const REFRESH_MS = 6 * 3600_000;
const DEM_POINTS = 1000;
const DEM_BATCH = 100;
const CONFIDENCE = { low: 1, medium: 2, high: 3 };
const URL_CAMS = 150;

const region = {
  country: { type: 'array', items: { type: 'string' } },
  bbox: { type: 'array', items: { type: 'number' }, description: '[west, south, east, north] in degrees' },
  near: {
    type: 'object',
    properties: { lat: { type: 'number' }, lng: { type: 'number' }, km: { type: 'number' } },
    required: ['lat', 'lng'],
  },
  elevation_min: { type: 'number' },
  elevation_max: { type: 'number' },
  sources: { type: 'array', items: { type: 'string', enum: SOURCE_KEYS } },
  years: { type: 'object', properties: { from: { type: 'integer' }, to: { type: 'integer' } } },
};

export const MAPS_TOOLS = [
  {
    type: 'function',
    function: {
      name: 'maps_localities',
      description: [
        "Where to find some taxa: the places with the most occurrence records of the Ithomiini maps (the project's own collection records, GBIF museum records, Dore et al. 2022, iNaturalist), best first, each with a link that opens the maps app on it with the filters set. For questions like \"where are the best places to find X?\".",
        '- `taxa`: species ("Oleria onega"), subspecies ("Oleria onega janarilla") or genus ("Oleria"); synonyms are resolved.',
        '- Region (optional): `country`, `bbox` [west, south, east, north], `near` {lat, lng, km}. `elevation_min`/`elevation_max` in metres (the workbook\'s elevation for the project\'s records, the ground\'s from a 90 m DEM for the rest). `sources` (default all), `years` {from, to}.',
        '- `site_km` (default 3): records closer than this are one site. `limit` (default 15).',
        "Ranked by records, then by the number of years seen. Dore et al. records are 0.25° grid cells, not sites: counted apart (`grid_cells`). Cite the data's dates (`freshness`) and give the links; a site's name is its records' commonest locality name.",
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          taxa: { type: 'array', items: { type: 'string' } },
          ...region,
          site_km: { type: 'number' },
          limit: { type: 'integer' },
        },
        required: ['taxa'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'maps_species',
      description: [
        'The Ithomiini recorded in a region of the maps (the same records as maps_localities), most records first: for "what can we find around X?" or "which species live above 1500 m in Napo?".',
        '- A region is needed: `country`, `bbox` or `near` {lat, lng, km}; with `elevation_min`/`elevation_max`, `sources`, `years` as in maps_localities. `genus` narrows it. `limit` (default 30).',
        'Each taxon with its records, sites, sources and the last year seen, and a map link.',
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: { ...region, genus: { type: 'array', items: { type: 'string' } }, limit: { type: 'integer' } },
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'host_plants',
      description: [
        "Host plants of Ithomiini from the maps' literature review (Beccaloni et al. 2008 catalogue and direct observations), each with its source.",
        '- `butterfly`: a species, subspecies or genus: its host plants. Or `plant`: a plant family, genus or species: the Ithomiini recorded on it.',
        '- `min_confidence`: low, medium (default) or high. `include_unchecked`: also the rows the review marked needs_check.',
        'Give the citation and link of each claim. A catalogue row is a published record, not observed use at a given place; a family-level host says little.',
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          butterfly: { type: 'string' },
          plant: { type: 'string' },
          min_confidence: { type: 'string', enum: ['low', 'medium', 'high'] },
          include_unchecked: { type: 'boolean' },
        },
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'gallery_specimens',
      description: [
        "Collection specimens with wing photos (the Wings Gallery's Collection tab, read from the workbook and Photo_links): by CAM ID, species, sex, locality or elevation, with a link to the gallery showing them and photo URLs.",
        '- Any of: `cam_ids`, `species` ["Genus species"], `subspecies`, `genus`, `sex` (male, female), `locality` (words of Collection_location), `country`, `elevation_min`/`elevation_max`. `with_photos` (default true). `limit` (default 12).',
        '- `markdown`: the first `thumbnails` specimens (default 6) as small photos linked to the full ones, ready to put in the reply: put it in as it is to show them.',
        'The gallery site is rebuilt when its update runs (update_websites): a specimen added since may not be there yet.',
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          cam_ids: { type: 'array', items: { type: 'string' } },
          species: { type: 'array', items: { type: 'string' } },
          subspecies: { type: 'array', items: { type: 'string' } },
          genus: { type: 'array', items: { type: 'string' } },
          sex: { type: 'string', enum: ['male', 'female'] },
          locality: { type: 'string' },
          country: { type: 'array', items: { type: 'string' } },
          elevation_min: { type: 'number' },
          elevation_max: { type: 'number' },
          with_photos: { type: 'boolean' },
          limit: { type: 'integer' },
          thumbnails: { type: 'integer' },
        },
      },
    },
  },
];
export const MAPS_TOOL_NAMES = new Set(MAPS_TOOLS.map(t => t.function.name));

const text = v => (v === null || v === undefined ? '' : String(v).trim());
/** Lower case, accents and repeated spaces away: how names and places are compared. */
export const fold = v =>
  text(v)
    .normalize('NFD')
    .replace(/\p{M}/gu, '')
    .toLowerCase()
    .replace(/\s+/g, ' ');
const num = v => {
  const n = Number(String(v ?? '').replace(',', '.').replace(/[^\d.+-]/g, ''));
  return text(v) !== '' && Number.isFinite(n) ? n : null;
};
const clamp = (v, low, high, fallback) => (Number.isFinite(Number(v)) ? Math.min(high, Math.max(low, Math.round(Number(v)))) : fallback);
const list = v => (Array.isArray(v) ? v : v === undefined || v === null || v === '' ? [] : [v]).map(text).filter(Boolean);
const count = (map, key, by = 1) => map.set(key, (map.get(key) ?? 0) + by);
const top = map => [...map].sort((a, b) => b[1] - a[1])[0]?.[0] ?? null;
const median = values => {
  if (!values.length) return null;
  const s = [...values].sort((a, b) => a - b);
  return Math.round(s.length % 2 ? s[(s.length - 1) / 2] : (s[s.length / 2 - 1] + s[s.length / 2]) / 2);
};
const round = (v, d) => Number(v.toFixed(d));

/** The year of a record's date ("2019-08-12T13:15", "18-Jan-22", "12/03/2015"), or null. */
export function yearOf(date) {
  const s = text(date);
  const four = /(?:^|\D)((?:18|19|20)\d{2})(?:\D|$)/.exec(s);
  if (four) return Number(four[1]);
  const two = /^\d{1,2}[-/ ][A-Za-z]{3,9}[-/ ](\d{2})$/.exec(s) ?? /^\d{1,2}\/\d{1,2}\/(\d{2})$/.exec(s);
  if (!two) return null;
  const yy = Number(two[1]);
  return yy <= new Date().getUTCFullYear() % 100 ? 2000 + yy : 1900 + yy;
}

/** A workbook date as YYYY-MM-DD: the sheet's day number (45914) made a date; any other text as it is. */
export function dateOf(value) {
  const s = text(value);
  if (!/^\d{5}(\.\d+)?$/.test(s)) return s || null;
  return new Date(Date.UTC(1899, 11, 30) + Math.floor(Number(s)) * 86400_000).toISOString().slice(0, 10);
}

function km(a, b) {
  const rad = Math.PI / 180;
  const dLat = (b.lat - a.lat) * rad;
  const dLng = (b.lng - a.lng) * rad;
  const h = Math.sin(dLat / 2) ** 2 + Math.cos(a.lat * rad) * Math.cos(b.lat * rad) * Math.sin(dLng / 2) ** 2;
  return 12742 * Math.asin(Math.sqrt(h));
}

/** The maps app with these filters: `sp`/`ssp`/`genus`, `source` (always: left out it shows only the project's), `map_view`. */
export function mapsUrl({ species = [], subspecies = [], genus = [], sources = SOURCE_KEYS, country = [], view = null } = {}, base = MAPS_URL) {
  const params = new URLSearchParams();
  if (genus.length) params.set('genus', genus.join(','));
  if (species.length) params.set('sp', species.join(','));
  if (subspecies.length) params.set('ssp', subspecies.join(','));
  params.set('source', sources.map(k => SOURCES[k]).join(','));
  if (country.length) params.set('country', country.join(','));
  if (view) params.set('map_view', [view.lng.toFixed(5), view.lat.toFixed(5), view.zoom.toFixed(2)].join(','));
  return `${base}?${params.toString().replace(/\+/g, '%20')}`;
}

/** A map view holding these points (centre and a zoom that fits their span). */
export function viewOf(points, minZoom = 3, maxZoom = 12) {
  if (!points.length) return null;
  const lats = points.map(p => p.lat);
  const lngs = points.map(p => p.lng);
  const [s, n, w, e] = [Math.min(...lats), Math.max(...lats), Math.min(...lngs), Math.max(...lngs)];
  const span = Math.max(n - s, e - w, 0.01);
  const zoom = Math.min(maxZoom, Math.max(minZoom, Math.floor(Math.log2(360 / span)) - 1));
  return { lng: (w + e) / 2, lat: (s + n) / 2, zoom };
}

/** The Wings Gallery's Collection tab with these filters, or its search tab with these CAM IDs. */
export function galleryUrl({ species = [], subspecies = [], sex = null, camids = null } = {}, base = GALLERY_URL) {
  const params = new URLSearchParams({ view: '1' });
  if (camids) {
    params.set('camids', camids.slice(0, URL_CAMS).join(' '));
    return `${base}search?${params.toString().replace(/\+/g, '%20')}`;
  }
  for (const s of species) params.append('species', s);
  for (const s of subspecies) params.append('subspecies', s);
  if (sex) params.set('sex', sex);
  return `${base}?${params.toString().replace(/\+/g, '%20')}`;
}

const thumbUrl = (file, width = 160) =>
  `https://wsrv.nl/?url=${encodeURIComponent(`https://drive.google.com/uc?id=${file}`)}&w=${width}&output=webp`;
const fullUrl = file => `https://lh3.googleusercontent.com/d/${file}=w1600`;

/** One record of the maps, kept small: strings shared (`intern`). */
function compactPoint(p, intern) {
  const lat = Number(p.lat);
  const lng = Number(p.lng);
  if (!Number.isFinite(lat) || !Number.isFinite(lng)) return null;
  return {
    id: intern(text(p.id)),
    name: intern(text(p.scientific_name)),
    genus: intern(text(p.genus)),
    ssp: intern(text(p.subspecies)),
    lat,
    lng,
    place: intern(text(p.collection_location)),
    country: intern(text(p.country)),
    year: yearOf(p.observation_date),
    source: KEY_OF[p.source] ?? intern(text(p.source)),
    sex: intern(text(p.sex)),
  };
}

/**
 * The tools for one app. `store`: the app's database (read with SELECT only); `fetchImpl`, `now`, `mapsBase`,
 * `galleryBase`, `elevationApi`: for tests.
 */
export function createMapsTools({
  store,
  fetchImpl = fetch,
  now = Date.now,
  mapsBase = MAPS_URL,
  galleryBase = GALLERY_URL,
  elevationApi = ELEVATION_API,
} = {}) {
  const files = new Map();
  const dem = new Map();
  const strings = new Map();
  const intern = s => strings.get(s) ?? (strings.set(s, s), s);

  /** A data file of the maps site, made into `shape(json)`; read again after REFRESH_MS, parsed again when it changed. */
  async function remote(path, shape) {
    const kept = files.get(path);
    if (kept?.loading) return kept.loading;
    if (kept?.value && now() - kept.checked < REFRESH_MS) return kept;
    const loading = (async () => {
      const response = await fetchImpl(new URL(`data/${path}`, mapsBase).href, {
        headers: { 'user-agent': 'ithomiini-database', ...(kept?.etag ? { 'if-none-match': kept.etag } : {}) },
        signal: AbortSignal.timeout(60_000),
      });
      if (response.status === 304 && kept?.value) return { ...kept, checked: now(), loading: null };
      if (!response.ok) throw new Error(`The maps site answered ${response.status} for ${path}`);
      const value = shape(await response.json());
      return { value, etag: response.headers.get('etag'), modified: response.headers.get('last-modified'), checked: now(), loading: null };
    })();
    files.set(path, { ...kept, loading });
    try {
      const done = await loading;
      files.set(path, done);
      return done;
    } catch (e) {
      // The copy read before, if any, while the site does not answer.
      if (kept?.value) {
        files.set(path, { ...kept, loading: null });
        return kept;
      }
      files.delete(path);
      throw e;
    }
  }

  const manifest = () => remote('data_manifest.json', json => json);
  async function points(keys) {
    const { value: m, modified } = await manifest();
    const out = { records: [], freshness: { generated_at: m.generated_at ?? null, manifest_modified: modified ?? null, files: {} } };
    // One file at a time: each is parsed and made small before the next.
    for (const key of keys) {
      const file = m.sources?.[SOURCES[key]]?.file;
      if (!file) continue;
      const got = await remote(file, json => (Array.isArray(json) ? json : []).map(p => compactPoint(p, intern)).filter(Boolean));
      out.freshness.files[SOURCES[key]] = got.modified ?? null;
      for (const r of got.value) out.records.push(r);
    }
    return out;
  }
  const synonyms = async () =>
    (
      await remote('taxonomy_synonyms.json', json => {
        const map = new Map();
        for (const s of json?.synonyms ?? []) if (s.synonym && s.accepted) map.set(fold(s.synonym), text(s.accepted));
        return map;
      }).catch(() => ({ value: new Map() }))
    ).value;

  /** The workbook's elevation of each CAM ID (metres), rebuilt when Collection_data changes. */
  let workbook = null;
  function collection() {
    const db = store?.db;
    if (!db) return { rows: [], elevation: new Map(), photos: new Map() };
    const state = db
      .prepare("SELECT count(*) n, max(updated_at) u FROM records WHERE sheet IN ('Collection_data','Photo_links') AND missing=0")
      .get();
    const stamp = `${state.n}:${state.u}`;
    if (workbook?.stamp === stamp) return workbook;
    const rows = [];
    const elevation = new Map();
    for (const r of db
      .prepare("SELECT values_json FROM records WHERE sheet='Collection_data' AND missing=0 AND observed=1 AND row_num>1 ORDER BY row_num")
      .all()) {
      let v;
      try {
        v = JSON.parse(r.values_json) ?? {};
      } catch {
        continue;
      }
      const cam = text(v.CAM_ID).toUpperCase();
      if (!cam) continue;
      const elev = num(v.ELEVATION);
      if (elev !== null) elevation.set(cam, elev);
      rows.push({ cam, v, elev });
    }
    const photos = new Map();
    for (const r of db
      .prepare("SELECT values_json FROM records WHERE sheet='Photo_links' AND missing=0 AND observed=1 AND row_num>1 ORDER BY row_num")
      .all()) {
      let v;
      try {
        v = JSON.parse(r.values_json) ?? {};
      } catch {
        continue;
      }
      if (text(v.Type) && text(v.Type) !== 'File') continue;
      const parsed = specimenName(text(v.Name));
      const file = driveIdOf(v.URL);
      if (!parsed || !file || !/\.(jpe?g|png)$/i.test(text(v.Name))) continue;
      const cam = parsed.id.replace(/[\s_-]+/g, '').toUpperCase();
      const entry = photos.get(cam) ?? photos.set(cam, {}).get(cam);
      const view = parsed.view === 'd' ? 'dorsal' : 'ventral';
      // The plain name (CAM…d.JPG) before its extra views (CAM…d2.JPG).
      if (!entry[view] || !parsed.extra) entry[view] = { name: text(v.Name), file };
    }
    workbook = { stamp, rows, elevation, photos };
    return workbook;
  }

  /** Elevations of these records: the workbook's for the project's, else the DEM's (at most DEM_POINTS new points). */
  async function elevations(records) {
    const { elevation } = collection();
    const out = new Map();
    const wanted = new Map();
    for (const r of records) {
      const own = r.source === 'sanger' ? elevation.get(r.id.toUpperCase()) : undefined;
      if (own !== undefined) {
        out.set(r, { m: own, from: 'workbook' });
        continue;
      }
      const key = `${r.lat.toFixed(3)},${r.lng.toFixed(3)}`;
      if (dem.has(key)) out.set(r, { m: dem.get(key), from: 'dem' });
      else (wanted.get(key) ?? wanted.set(key, []).get(key)).push(r);
    }
    const keys = [...wanted.keys()];
    const asked = keys.slice(0, DEM_POINTS);
    for (let i = 0; i < asked.length; i += DEM_BATCH) {
      const batch = asked.slice(i, i + DEM_BATCH);
      try {
        const url = `${elevationApi}?latitude=${batch.map(k => k.split(',')[0]).join(',')}&longitude=${batch.map(k => k.split(',')[1]).join(',')}`;
        const response = await fetchImpl(url, { signal: AbortSignal.timeout(15_000) });
        if (!response.ok) break;
        const got = (await response.json()).elevation ?? [];
        batch.forEach((k, j) => {
          if (Number.isFinite(got[j])) dem.set(k, got[j]);
        });
      } catch {
        break;
      }
    }
    for (const [key, rs] of wanted) if (dem.has(key)) for (const r of rs) out.set(r, { m: dem.get(key), from: 'dem' });
    return { out, unknownPoints: keys.filter(k => !dem.has(k)).length };
  }

  /** The records within the region and years asked. */
  function inRegion(records, args) {
    const countries = new Set(list(args.country).map(fold));
    const bbox = Array.isArray(args.bbox) && args.bbox.length === 4 && args.bbox.every(Number.isFinite) ? args.bbox : null;
    const near = args.near && Number.isFinite(args.near.lat) && Number.isFinite(args.near.lng) ? { ...args.near, km: Number(args.near.km) || 25 } : null;
    const from = Number.isFinite(args.years?.from) ? args.years.from : null;
    const to = Number.isFinite(args.years?.to) ? args.years.to : null;
    return records.filter(r => {
      if (countries.size && !countries.has(fold(r.country))) return false;
      if (bbox && (r.lng < bbox[0] || r.lat < bbox[1] || r.lng > bbox[2] || r.lat > bbox[3])) return false;
      if (near && km(near, r) > near.km) return false;
      if ((from !== null || to !== null) && (r.year === null || (from !== null && r.year < from) || (to !== null && r.year > to))) return false;
      return true;
    });
  }
  const sourcesOf = args => {
    const asked = list(args.sources).filter(k => SOURCES[k]);
    return asked.length ? asked : SOURCE_KEYS;
  };
  const hasElevation = args => Number.isFinite(args.elevation_min) || Number.isFinite(args.elevation_max);
  async function byElevation(records, args) {
    if (!hasElevation(args)) return { kept: records, elev: null, unknown: 0, unknownPoints: 0 };
    const { out, unknownPoints } = await elevations(records);
    const low = Number.isFinite(args.elevation_min) ? args.elevation_min : -Infinity;
    const high = Number.isFinite(args.elevation_max) ? args.elevation_max : Infinity;
    let unknown = 0;
    const kept = records.filter(r => {
      const e = out.get(r);
      if (!e) return void unknown++;
      return e.m >= low && e.m <= high;
    });
    return { kept, elev: out, unknown, unknownPoints };
  }

  /** The maps' names for what was asked: { species, subspecies, genus, match(record), unknown }. */
  async function taxaOf(asked, records) {
    const names = new Set(records.map(r => fold(r.name)));
    const genera = new Set(records.map(r => fold(r.genus)));
    const syn = await synonyms();
    const species = new Map();
    const subspecies = new Map();
    const genus = new Map();
    const unknown = [];
    const resolved = {};
    for (const raw of asked) {
      const words = fold(raw).replace(/\b(cf|aff)\.?\s/g, '').split(' ');
      if (words.length === 1 && genera.has(words[0])) {
        genus.set(words[0], records.find(r => fold(r.genus) === words[0]).genus);
        continue;
      }
      let name = words.slice(0, 2).join(' ');
      if (!names.has(name) && syn.has(name)) {
        resolved[raw] = syn.get(name);
        name = fold(syn.get(name));
      }
      if (words.length < 2 || !names.has(name)) {
        unknown.push(raw);
        continue;
      }
      const shown = records.find(r => fold(r.name) === name).name;
      if (words[2]) subspecies.set(`${name} ${words[2]}`, { name, ssp: words[2], shown: `${shown} ${words[2]}` });
      else species.set(name, shown);
    }
    const match = r => {
      const n = fold(r.name);
      return species.has(n) || genus.has(fold(r.genus)) || subspecies.has(`${n} ${fold(r.ssp)}`);
    };
    return {
      species: [...species.values(), ...[...subspecies.values()].map(s => records.find(r => fold(r.name) === s.name).name)],
      subspecies: [...subspecies.values()].map(s => s.ssp),
      genus: [...genus.values()],
      match,
      unknown,
      resolved,
    };
  }

  async function localities(args) {
    const asked = list(args.taxa).slice(0, 20);
    if (!asked.length) return { error: 'Name at least one taxon in `taxa`.' };
    const sources = sourcesOf(args);
    const { records, freshness } = await points(sources);
    const taxa = await taxaOf(asked, records);
    if (!taxa.species.length && !taxa.genus.length)
      return { error: `The maps have no records of ${asked.join(', ')}.`, unknown: taxa.unknown, freshness };
    const ofTaxa = records.filter(taxa.match);
    const regional = inRegion(ofTaxa, args);
    const { kept, elev: fromBand, unknown, unknownPoints } = await byElevation(regional, args);
    // Without an elevation asked: the workbook's for the project's records (no DEM look-ups).
    const own = collection().elevation;
    const elev = fromBand ?? new Map(kept.filter(r => r.source === 'sanger' && own.has(r.id.toUpperCase())).map(r => [r, { m: own.get(r.id.toUpperCase()), from: 'workbook' }]));
    const siteKm = Math.min(50, Math.max(0.5, Number(args.site_km) || 3));
    const deg = siteKm / 111;
    const sites = new Map();
    const grid = new Map();
    for (const r of kept) {
      if (r.source === 'dore') {
        const key = `${r.lat},${r.lng}`;
        const cell = grid.get(key) ?? grid.set(key, { lat: r.lat, lng: r.lng, country: r.country, records: 0 }).get(key);
        cell.records++;
        continue;
      }
      const row = Math.floor(r.lat / deg);
      const key = `${row}:${Math.floor(r.lng / (deg / Math.max(0.2, Math.cos(((row + 0.5) * deg * Math.PI) / 180))))}`;
      const s =
        sites.get(key) ??
        sites.set(key, { n: 0, lat: 0, lng: 0, names: new Map(), countries: new Map(), sources: new Map(), years: new Set(), elev: [], elevFrom: new Set(), taxa: new Map() }).get(key);
      s.n++;
      s.lat += r.lat;
      s.lng += r.lng;
      if (r.place) count(s.names, r.place);
      if (r.country) count(s.countries, r.country);
      count(s.sources, SOURCES[r.source] ?? r.source);
      count(s.taxa, r.name);
      if (r.year) s.years.add(r.year);
      const e = elev?.get(r);
      if (e) {
        s.elev.push(e.m);
        s.elevFrom.add(e.from);
      }
    }
    const limit = clamp(args.limit, 1, 50, 15);
    const ranked = [...sites.values()]
      .map(s => ({ ...s, lat: s.lat / s.n, lng: s.lng / s.n }))
      .sort((a, b) => b.n - a.n || b.years.size - a.years.size);
    const urlTaxa = { species: taxa.species, subspecies: taxa.subspecies, genus: taxa.genus, sources };
    const shown = ranked.slice(0, limit).map((s, i) => {
      const years = [...s.years].sort();
      return {
        rank: i + 1,
        site: top(s.names) ?? `${s.lat.toFixed(3)}, ${s.lng.toFixed(3)}`,
        country: top(s.countries),
        lat: round(s.lat, 4),
        lng: round(s.lng, 4),
        records: s.n,
        by_source: Object.fromEntries(s.sources),
        ...(taxa.genus.length || taxa.species.length > 1 ? { taxa: Object.fromEntries([...s.taxa].sort((a, b) => b[1] - a[1]).slice(0, 8)) } : {}),
        years: years.length ? { first: years[0], last: years.at(-1), distinct: years.length } : null,
        ...(s.elev.length ? { elevation_m: median(s.elev), elevation_from: [...s.elevFrom].join('+') } : {}),
        other_names: [...s.names.keys()].filter(n => n !== top(s.names)).slice(0, 4),
        map: mapsUrl({ ...urlTaxa, view: { lat: s.lat, lng: s.lng, zoom: siteKm <= 3 ? 12 : 10 } }, mapsBase),
      };
    });
    const cells = [...grid.values()].sort((a, b) => b.records - a.records);
    const view = viewOf(ranked.length ? ranked.slice(0, limit) : cells.slice(0, 5));
    return {
      taxa: { asked, species: taxa.species, subspecies: taxa.subspecies, genus: taxa.genus, ...(Object.keys(taxa.resolved).length ? { synonyms: taxa.resolved } : {}), ...(taxa.unknown.length ? { not_in_maps: taxa.unknown } : {}) },
      records: {
        of_taxa: ofTaxa.length,
        in_region: regional.length,
        kept: kept.length,
        ...(hasElevation(args) ? { elevation_unknown: unknown, ...(unknownPoints ? { note: `${unknownPoints} points had no DEM elevation (over the ${DEM_POINTS}-point budget or not answered): left out` } : {}) } : {}),
      },
      sites_total: ranked.length,
      sites: shown,
      ...(cells.length ? { grid_cells: { note: 'Dore et al. (2022): 0.25° cells, not collecting sites', cells: cells.length, top: cells.slice(0, 5).map(c => ({ lat: c.lat, lng: c.lng, country: c.country, records: c.records })) } } : {}),
      map: mapsUrl({ ...urlTaxa, country: list(args.country), view }, mapsBase),
      freshness,
    };
  }

  async function species(args) {
    if (!list(args.country).length && !Array.isArray(args.bbox) && !args.near) return { error: 'Give a region: `country`, `bbox` or `near`.' };
    const sources = sourcesOf(args);
    const { records, freshness } = await points(sources);
    const genera = new Set(list(args.genus).map(fold));
    const regional = inRegion(genera.size ? records.filter(r => genera.has(fold(r.genus))) : records, args);
    const { kept, unknown, unknownPoints } = await byElevation(regional, args);
    const byName = new Map();
    let unidentified = 0;
    for (const r of kept) {
      // "Unknown species" and the like: counted, not ranked.
      if (!r.name || /^unknown\b/i.test(r.name) || r.name.split(' ').length < 2) {
        unidentified++;
        continue;
      }
      const t = byName.get(r.name) ?? byName.set(r.name, { records: 0, sites: new Set(), sources: new Map(), last: null }).get(r.name);
      t.records++;
      t.sites.add(r.source === 'dore' ? `g${r.lat},${r.lng}` : `${r.lat.toFixed(2)},${r.lng.toFixed(2)}`);
      count(t.sources, SOURCES[r.source] ?? r.source);
      if (r.year && (!t.last || r.year > t.last)) t.last = r.year;
    }
    const limit = clamp(args.limit, 1, 100, 30);
    const ranked = [...byName].sort((a, b) => b[1].records - a[1].records);
    const view = args.near
      ? { lat: args.near.lat, lng: args.near.lng, zoom: Math.max(5, Math.min(12, Math.round(Math.log2(40000 / (Number(args.near.km) || 25))) - 1)) }
      : Array.isArray(args.bbox) && args.bbox.length === 4
        ? viewOf([{ lat: args.bbox[1], lng: args.bbox[0] }, { lat: args.bbox[3], lng: args.bbox[2] }])
        : viewOf(kept);
    return {
      records: { in_region: regional.length, kept: kept.length, ...(hasElevation(args) ? { elevation_unknown: unknown, ...(unknownPoints ? { note: `${unknownPoints} points had no DEM elevation: left out` } : {}) } : {}) },
      taxa_total: ranked.length,
      ...(unidentified ? { unidentified_records: unidentified } : {}),
      taxa: ranked.slice(0, limit).map(([name, t]) => ({ name, records: t.records, sites: t.sites.size, by_source: Object.fromEntries(t.sources), last_year: t.last })),
      map: mapsUrl({ species: ranked.slice(0, 20).map(([n]) => n), sources, country: list(args.country), view }, mapsBase),
      freshness,
    };
  }

  async function hostPlants(args) {
    const butterfly = fold(args.butterfly);
    const plant = fold(args.plant);
    if (!butterfly && !plant) return { error: 'Name a `butterfly` or a `plant`.' };
    const got = await remote('host_plants/host_plant_associations.json', json => json);
    const all = got.value.associations ?? [];
    const least = CONFIDENCE[args.min_confidence] ?? CONFIDENCE.medium;
    const okConfidence = a => (a.confidence === 'needs_check' ? args.include_unchecked === true : (CONFIDENCE[a.confidence] ?? 0) >= least);
    const words = butterfly.split(' ');
    const ofButterfly = a => {
      const t = fold(a.butterfly_taxon);
      if (words.length === 1) return t.split(' ')[0] === butterfly;
      const sp = words.slice(0, 2).join(' ');
      // A subspecies asked: its own rows and its species' (rank used: species).
      return words.length > 2 ? t === butterfly || t === sp : t === sp || t.startsWith(`${sp} `);
    };
    const ofPlant = a => [a.host_plant_family, a.host_plant_genus, a.host_plant_species, a.host_taxon_name].some(v => fold(v) === plant);
    const matched = all.filter(a => (butterfly ? ofButterfly(a) : true) && (plant ? ofPlant(a) : true));
    const kept = matched.filter(okConfidence);
    const groups = new Map();
    for (const a of kept) count(groups, butterfly ? a.host_plant_genus ?? `(${a.host_plant_family}, genus not given)` : a.butterfly_taxon.split(' ')[0]);
    // Direct observations and species-level hosts first; each source's link and caveat once, in `sources`.
    const rank = a => (a.evidence_level === 'direct' ? 0 : 2) + (a.host_id_level === 'species' ? 0 : 1) - (CONFIDENCE[a.confidence] ?? 0) / 10;
    const sources = {};
    const associations = [...kept]
      .sort((a, b) => rank(a) - rank(b))
      .map(a => {
        const citation = text(a.citation_for_ui ?? a.source_citation) || 'not given';
        sources[citation] ??= { link: a.doi_or_url ?? null, ...(a.caveats ? { caveat: text(a.caveats).slice(0, 200) } : {}) };
        return {
          butterfly: a.butterfly_taxon,
          plant: a.host_taxon_name,
          plant_family: a.host_plant_family,
          ...(a.host_plant_name_verbatim && a.host_plant_name_verbatim !== a.host_taxon_name ? { as_written: a.host_plant_name_verbatim } : {}),
          host_level: a.host_id_level,
          evidence: a.evidence_level,
          confidence: a.confidence,
          source: citation,
          ...(text(a.source_refs) ? { references: text(a.source_refs).slice(0, 300) } : {}),
        };
      });
    const onlyCoarse = kept.length > 0 && kept.every(a => a.host_id_level !== 'species');
    return {
      asked: { butterfly: args.butterfly ?? null, plant: args.plant ?? null, min_confidence: args.min_confidence ?? 'medium' },
      found: kept.length,
      ...(matched.length > kept.length ? { left_out: { count: matched.length - kept.length, by_confidence: Object.fromEntries([...matched.filter(a => !okConfidence(a)).reduce((m, a) => count(m, a.confidence), new Map())]) } } : {}),
      [butterfly ? 'by_plant_genus' : 'by_butterfly_genus']: Object.fromEntries([...groups].sort((a, b) => b[1] - a[1])),
      ...(onlyCoarse ? { note: 'Only genus- or family-level hosts: the plant species is not known.' } : {}),
      associations,
      sources,
      data: { file: new URL('data/host_plants/host_plant_associations.json', mapsBase).href, generated_at: got.value.metadata?.generated_at ?? null, modified: got.modified ?? null, policy: got.value.metadata?.excluded_policy ?? null },
    };
  }

  function specimens(args) {
    const { rows, photos } = collection();
    const cams = new Set(list(args.cam_ids).map(c => c.replace(/[\s_-]+/g, '').toUpperCase()));
    const sp = new Set(list(args.species).map(s => fold(s).split(' ').slice(0, 2).join(' ')));
    const ssp = new Set(list(args.subspecies).map(s => fold(s).split(' ').at(-1)));
    const genera = new Set(list(args.genus).map(fold));
    const sex = fold(args.sex);
    const place = fold(args.locality).split(' ').filter(Boolean);
    const countries = new Set(list(args.country).map(fold));
    const low = Number.isFinite(args.elevation_min) ? args.elevation_min : null;
    const high = Number.isFinite(args.elevation_max) ? args.elevation_max : null;
    if (!cams.size && !sp.size && !ssp.size && !genera.size && !sex && !place.length && !countries.size && low === null && high === null)
      return { error: 'Give at least one of cam_ids, species, subspecies, genus, sex, locality, country, elevation.' };
    const withPhotos = args.with_photos !== false;
    const found = [];
    for (const { cam, v, elev } of rows) {
      const name = fold(v.SPECIES).split(' ');
      const words = name.slice(0, 2).join(' ');
      if (cams.size && !cams.has(cam)) continue;
      if (sp.size && !sp.has(words)) continue;
      if (ssp.size && !ssp.has(fold(v.Subspecies_Form)) && !ssp.has(name[2] ?? '')) continue;
      if (genera.size && !genera.has(name[0])) continue;
      if (sex && fold(v.Sex) !== sex) continue;
      if (place.length && !place.every(w => fold(v.Collection_location).includes(w))) continue;
      if (countries.size && !countries.has(fold(v.Country))) continue;
      if ((low !== null || high !== null) && (elev === null || (low !== null && elev < low) || (high !== null && elev > high))) continue;
      const p = photos.get(cam) ?? {};
      if (withPhotos && !p.dorsal && !p.ventral) continue;
      found.push({ cam, v, elev, p });
    }
    found.sort((a, b) => b.cam.localeCompare(a.cam, 'en', { numeric: true }));
    const limit = clamp(args.limit, 1, 60, 12);
    const thumbs = clamp(args.thumbnails, 0, 24, 6);
    const photo = x => x && { name: x.name, file: x.file, thumbnail: thumbUrl(x.file), full: fullUrl(x.file) };
    const shown = found.slice(0, limit).map(({ cam, v, elev, p }) => ({
      cam_id: cam,
      species: text(v.SPECIES) || null,
      subspecies: text(v.Subspecies_Form) || null,
      sex: text(v.Sex) || null,
      locality: text(v.Collection_location) || null,
      country: text(v.Country) || null,
      date: dateOf(v.Collection_date),
      elevation_m: elev,
      photos: { dorsal: photo(p.dorsal) ?? null, ventral: photo(p.ventral) ?? null },
    }));
    // The Collection tab filters by species, subspecies and sex only; anything else is the search tab with the IDs.
    const byFilters = (sp.size || ssp.size) && !cams.size && !genera.size && !place.length && !countries.size && low === null && high === null && withPhotos;
    const speciesNames = [...new Set(found.map(f => text(f.v.SPECIES)))];
    const url = byFilters
      ? galleryUrl({ species: speciesNames, subspecies: [...new Set(found.map(f => text(f.v.Subspecies_Form)).filter(s => ssp.has(fold(s))))], sex: sex || null }, galleryBase)
      : found.length
        ? galleryUrl({ camids: found.map(f => f.cam) }, galleryBase)
        : null;
    const lines = shown
      .slice(0, thumbs)
      .filter(s => s.photos.dorsal || s.photos.ventral)
      .map(s => {
        const pics = ['dorsal', 'ventral']
          .filter(k => s.photos[k])
          .map(k => `[![${s.cam_id} ${k[0]}](${s.photos[k].thumbnail})](${s.photos[k].full})`)
          .join(' ');
        const about = [s.species, s.subspecies, s.sex, s.locality, s.elevation_m !== null ? `${s.elevation_m} m` : null].filter(Boolean).join(', ');
        return `${pics}  \n**${s.cam_id}** ${about}`;
      });
    return {
      found: found.length,
      shown: shown.length,
      specimens: shown,
      gallery: url,
      ...(url && !byFilters && found.length > URL_CAMS ? { gallery_note: `The link holds the first ${URL_CAMS} of ${found.length} CAM IDs.` } : {}),
      markdown: lines.join('\n\n'),
      note: 'From the workbook and Photo_links as this app has them now; the gallery site shows a new specimen after its next update.',
    };
  }

  return {
    async run(name, args = {}) {
      try {
        if (name === 'maps_localities') return await localities(args);
        if (name === 'maps_species') return await species(args);
        if (name === 'host_plants') return await hostPlants(args);
        if (name === 'gallery_specimens') return specimens(args);
      } catch (e) {
        return { error: e.message };
      }
      return { error: `Unknown tool ${name}` };
    },
  };
}
