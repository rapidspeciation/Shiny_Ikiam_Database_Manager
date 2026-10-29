// Read-only tools on the project's public sites (docs/assistant-tools-maps-gallery.md):
//   maps_localities    places with the most records of some taxa (the maps' occurrence records), with map links
//   maps_species       the taxa recorded in a region
//   host_plants        host plants of a butterfly, or the Ithomiini on a plant, with their sources; with an area, where
//                      each host plant grows there (the maps' GBIF plant records), its photos and a map link
//   host_plant_sites   where to go first for some host plants near a point: the project's collection localities (or grid
//                      cells) ranked by the plant records around them, weighted by distance
//   gallery_specimens  collection specimens with wing photos (the workbook and Photo_links), gallery links, photos
//                      with wing and envelope crops, and the models' readings stored for them (labelled model output)
// The maps' data files are read over HTTPS from the published site, at most every 6 hours (and parsed again only
// when their ETag changed), and kept in memory as compact lists. The workbook is read with SELECT only. Nothing is
// written anywhere.

import { driveIdOf, specimenName } from './photo-links.mjs';
import { COAST_TOLERANCE_DEG, countryOfPoint, inBox } from './country-outlines.mjs';
import { unreliableFor } from './sex-check.mjs';

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
// Host plants with an area: at most this many plants, gallery pages read per plant, and the named places a site's
// name may come from (km).
const PLANT_MAX = 12;
const PHOTO_PAGES = 4;
const PLACE_KM = 10;
const PLACE_SOURCES = ['sanger', 'gbif_unam', 'gbif_other', 'inaturalist'];
// Site scores: plant records within these distances (km), the kernel's reach, photo pages read per plant for the
// photos nearest a site, and how many sites get photos.
const SCORE_RINGS = [2, 5, 10];
const KERNEL_KM = 10;
const NEAR_PAGES = 10;
const PHOTO_SITES = 3;
/** A record's weight in a site's score: quartic kernel, 1 at the site, 0 at KERNEL_KM (0.85 at 2 km, 0.56 at 5 km). */
export const kernel = d => (d >= KERNEL_KM ? 0 : (1 - (d / KERNEL_KM) ** 2) ** 2);

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
        "Host plants of Ithomiini from the maps' literature review (Beccaloni et al. 2008 catalogue and direct observations), each with its source; with an area, also where each host plant grows there, its photos and a map link. One call answers \"photos of X's host plants in Ecuador\" or \"a map of X's host plants\": use it before any outside API (GBIF, iNaturalist).",
        '- `butterfly`: a species, subspecies or genus: its host plants. Or `plant`: a plant family, genus or species: the Ithomiini recorded on it.',
        '- `min_confidence`: low, medium (default) or high. `include_unchecked`: also the rows the review marked needs_check.',
        "- Area (optional): `country` (list), `bbox` [west, south, east, north] or `near` {lat, lng, km}; `records: true` for the whole range. Then `plants`: each host plant's records there (the maps' GBIF plant records, placed in countries by their coordinates), years and top sites; `sites`: the best sites for the species-level hosts together, with coordinates, years and the nearest named collecting place; `markdown`: `photos` (default 3, at most 6) thumbnails per plant, each linked to its full photo, with credit; `map_url`: the Wings Atlas with the host-plant layer, these plants and the country on. `site_km` (default 5), `limit` (sites, default 8).",
        'Give the citation and link of each claim. A catalogue row is a published record, not observed use at a given place; a family-level host says little. Plant records show where the plant grows, never that the butterfly uses it there: say so.',
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          butterfly: { type: 'string' },
          plant: { type: 'string' },
          min_confidence: { type: 'string', enum: ['low', 'medium', 'high'] },
          include_unchecked: { type: 'boolean' },
          country: region.country,
          bbox: region.bbox,
          near: region.near,
          records: { type: 'boolean' },
          photos: { type: 'integer' },
          site_km: { type: 'number' },
          limit: { type: 'integer' },
        },
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'host_plant_sites',
      description: [
        "Where to go first to collect a butterfly's host plants (or a plant) near a point: the project's own collection localities (the Sanger records' localities in the maps), or grid cells, ranked by the host plants' GBIF records around each, weighted by distance. For \"near Ikiam (-0.95, -77.86), which site first for X's host plants?\" or \"where around here?\".",
        '- `butterfly` or `plant`, `min_confidence`, `include_unchecked` as in host_plants. `near` {lat, lng, km} (required): the sites within `km` (default 25, at most 100) of the point.',
        '- `rank`: `localities` (default) or `grid` (cells of `cell_km`, default 1.5, named after the nearest named place). `limit` (default 8, at most 20). `photos` (default 2, at most 4): the plant photos nearest each of the first 3 sites, as `markdown` thumbnails with credit.',
        "Per site: `score` (each plant record within 10 km weighs (1 - (d/10)²)²: 1 at the site, 0.56 at 5 km), records within 2, 5 and 10 km, the plants there, the nearest record (plant, km, year), the years of the records, the elevation, the project's records of the butterfly there. `map_url`: the Wings Atlas on the first site with the host-plant layer on.",
        'Say in the answer: GBIF and iNaturalist records follow roads, stations and observers, so a high score can mean a well-sampled place rather than more plants (give the records and years behind each score); a plant record is not evidence that the butterfly uses the plant there.',
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          butterfly: { type: 'string' },
          plant: { type: 'string' },
          min_confidence: { type: 'string', enum: ['low', 'medium', 'high'] },
          include_unchecked: { type: 'boolean' },
          near: region.near,
          rank: { type: 'string', enum: ['localities', 'grid'] },
          cell_km: { type: 'number' },
          limit: { type: 'integer' },
          photos: { type: 'integer' },
        },
        required: ['near'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'gallery_specimens',
      description: [
        "Collection specimens with wing photos (the Wings Gallery's Collection tab, read from the workbook and Photo_links): by CAM ID, species, sex, locality or elevation, with a link to the gallery showing them, photo URLs, and what the models read from the photos.",
        '- Any of: `cam_ids`, `species` ["Genus species"], `subspecies`, `genus`, `sex` (male, female), `locality` (words of Collection_location), `country`, `elevation_min`/`elevation_max`. `with_photos` (default true). `limit` (default 12).',
        "- Each specimen: the database's species, subspecies, sex; `model` (the Wings Gallery's species/subspecies top 3 and the sex model's call, with `sex_trusted` false and `sex_not_trusted` saying why when the model is not trusted for it: not a Supported call, or a species on the model's unreliable list); `envelope` (the envelope's text lines as the reader read them, and its model); `differs` (where the model or envelope disagree with the database).",
        '- `markdown`: ready to put in the reply as it is. The first `thumbnails` specimens (default 4, at most 8) in full: a 600 px photo, the wings and envelope crops (each linked to the full photo), the model output and the envelope lines; the rest one plain line each.',
        'Model output and envelope readings are not checked: say so, never give them as facts. The database is the record. The gallery site is rebuilt when its update runs (update_websites): a specimen added since may not be there yet.',
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
  const s = String(v ?? '').replace(',', '.').replace(/[^\d.+-]/g, '');
  const n = Number(s);
  return /\d/.test(s) && Number.isFinite(n) ? n : null;
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

/** The countries asked, as the records spell them (the maps app compares them exactly); one they lack as asked. */
const countriesIn = (asked, records) => {
  const spelled = new Map(records.map(r => [fold(r.country), r.country]));
  return [...new Set(list(asked).map(c => spelled.get(fold(c)) ?? c))];
};

/**
 * The maps app with these filters: `sp`/`ssp`/`genus`, `source` (always: left out it shows only the project's),
 * `country`, `map_view`; `hostPlants` {butterflies, taxa (slugs), levels, evidence}: the host-plant layer on
 * (`hp_bf`, `hp_taxa`, `hp_level`, `hp_evidence`; ithomiini_maps docs/host-plant-links.md).
 */
export function mapsUrl({ species = [], subspecies = [], genus = [], sources = SOURCE_KEYS, country = [], view = null, hostPlants = null } = {}, base = MAPS_URL) {
  const params = new URLSearchParams();
  if (genus.length) params.set('genus', genus.join(','));
  if (species.length) params.set('sp', species.join(','));
  if (subspecies.length) params.set('ssp', subspecies.join(','));
  params.set('source', sources.map(k => SOURCES[k]).join(','));
  if (country.length) params.set('country', country.join(','));
  if (hostPlants?.butterflies?.length) params.set('hp_bf', hostPlants.butterflies.join(','));
  if (hostPlants?.taxa?.length) params.set('hp_taxa', hostPlants.taxa.join(','));
  if (hostPlants?.levels?.length) params.set('hp_level', hostPlants.levels.join(','));
  if (hostPlants?.evidence?.length) params.set('hp_evidence', hostPlants.evidence.join(','));
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

const lh3 = (file, width = 1600) => `https://lh3.googleusercontent.com/d/${file}=w${width}`;
const fullUrl = file => lh3(file);
/** A photo `width` px wide, made by the image proxy from the 1600 px photo. */
export const previewUrl = (file, width = 600) => `https://wsrv.nl/?url=${encodeURIComponent(lh3(file))}&w=${width}&output=jpg`;
/**
 * A crop of a photo made by the image proxy, nothing stored. `box` [x0, y0, x1, y1] in pixels of the photo as
 * `size` [w, h] says (the frame the boxes were read in); the proxy is given the photo at that width (Drive's
 * =w1600 of an upright 1200x1600 photo is 1536x2048, not that frame). `precrop`: crop, then shrink to `width`
 * (without it the proxy shrinks first and a crop outside the small image is dropped). Not turned: an envelope
 * reading's `turned` 180 is how the reader found the photo then, and the Drive photos checked on 29 Oct 2026
 * (6 of 6) show the envelope upright as they are now. Null when the box or size is unusable.
 */
export function cropUrl(file, box, size, { width = 300, pad = 0 } = {}) {
  if (!file || !Array.isArray(box) || box.length !== 4 || !Array.isArray(size) || size.length !== 2) return null;
  const [w, h] = size.map(Number);
  if (!(w > 0 && h > 0) || !box.every(Number.isFinite)) return null;
  const x0 = Math.max(0, Math.floor(Math.min(box[0], box[2]) - pad));
  const y0 = Math.max(0, Math.floor(Math.min(box[1], box[3]) - pad));
  const x1 = Math.min(w, Math.ceil(Math.max(box[0], box[2]) + pad));
  const y1 = Math.min(h, Math.ceil(Math.max(box[1], box[3]) + pad));
  if (x1 - x0 < 8 || y1 - y0 < 8) return null;
  const out = Math.min(width, x1 - x0);
  return `https://wsrv.nl/?url=${encodeURIComponent(lh3(file, w))}&cx=${x0}&cy=${y0}&cw=${x1 - x0}&ch=${y1 - y0}&w=${out}&precrop&output=jpg`;
}
const parseJson = v => {
  try {
    return v ? JSON.parse(v) : null;
  } catch {
    return null;
  }
};
/** A probability as a percentage: 85 %, 1.8 %, 0.4 %, 99.8 % (never 100 % unless it is 1). */
export const percent = p =>
  Number.isFinite(p) ? `${p < 0.1 ? Math.round(p * 1000) / 10 : p < 0.995 || p >= 1 ? Math.round(p * 100) : Math.floor(p * 1000) / 10} %` : '?';
const binomialOf = name => fold(name).split(' ').slice(0, 2).join(' ');
const MODEL_TOP = 3;
const DETAILED_MAX = 8;
const PLAIN_ROWS = 20;

/**
 * What was read from a specimen's photos and kept in this app's tables (server/photodata.mjs): the Wings
 * Gallery's predictions, the envelope readings, the wing boxes and the photos' sizes. SELECT only; a table the
 * app has not made yet is read as empty. -> Map(cam -> { prediction, envelopes, boxes, sizes })
 */
export function photoReadings(db, wanted) {
  const out = new Map();
  if (!db || !wanted.length) return out;
  const tables = new Set(db.prepare("SELECT name FROM sqlite_master WHERE type='table'").all().map(r => r.name));
  const columns = t => (tables.has(t) ? new Set(db.prepare(`PRAGMA table_info(${t})`).all().map(c => c.name)) : new Set());
  const pc = columns('photo_predictions');
  const has = c => (pc.has(c) ? c : `NULL ${c}`);
  const prediction = pc.size
    ? db.prepare(`SELECT species_json, subspecies_json, sex, sex_conf, sex_supported, sex_species, ${has('sex_status')}, ${has('model')}, ${has('sex_model')} FROM photo_predictions WHERE cam=?`)
    : null;
  const envelopes = tables.has('envelope_readings')
    ? db.prepare('SELECT name, file_id, view, bbox_json, size_json, camid, camid_conf, text_json, model FROM envelope_readings WHERE cam=? ORDER BY name')
    : null;
  const box = tables.has('photo_wing_boxes') ? db.prepare('SELECT box_json, conf FROM photo_wing_boxes WHERE name=?') : null;
  const sideSize = columns('photo_wing_sides').has('size_json') ? db.prepare('SELECT size_json FROM photo_wing_sides WHERE file_id=? AND size_json IS NOT NULL LIMIT 1') : null;
  for (const { cam, photos } of wanted) {
    const p = prediction?.get(cam);
    const env = (envelopes?.all(cam) ?? []).map(r => ({
      name: r.name,
      file: r.file_id,
      view: r.view,
      bbox: parseJson(r.bbox_json),
      size: parseJson(r.size_json),
      camid: r.camid,
      camid_conf: r.camid_conf,
      lines: (parseJson(r.text_json) ?? []).map(l => (Array.isArray(l) ? text(l[0]) : text(l))).filter(Boolean),
      model: r.model,
    }));
    const boxes = {};
    const sizes = {};
    for (const [view, photo] of Object.entries(photos)) {
      if (!photo) continue;
      const stem = photo.name.replace(/\.[^.]+$/, '');
      const b = box?.get(stem);
      const frac = parseJson(b?.box_json);
      if (Array.isArray(frac) && frac.length === 4) boxes[view] = frac;
      const size = env.find(e => e.file === photo.file || fold(e.name) === fold(stem))?.size ?? parseJson(sideSize?.get(photo.file)?.size_json);
      if (Array.isArray(size) && size.length === 2) sizes[view] = size;
    }
    out.set(cam, { prediction: p ?? null, envelopes: env, boxes, sizes });
  }
  return out;
}

/** The models' output for a specimen, as the answer gives it (labelled model output, never facts). */
export function modelOf(prediction, databaseSpecies) {
  if (!prediction) return null;
  const top = v => (parseJson(v) ?? []).slice(0, MODEL_TOP).map(([name, p]) => ({ name, p }));
  const species = top(prediction.species_json);
  const subspecies = top(prediction.subspecies_json);
  const scored = prediction.sex === 'male' || prediction.sex === 'female';
  const unreliable = unreliableFor(databaseSpecies) ?? unreliableFor(prediction.sex_species) ?? unreliableFor(species[0]?.name);
  const supported = prediction.sex_supported === 1 || prediction.sex_supported === true;
  // Short, for the Markdown; the list's reason goes in `sex_not_trusted_why`.
  const why = !scored
    ? null
    : unreliable
      ? 'the sex model is not trusted for this species'
      : !supported
        ? `not a Supported call (${text(prediction.sex_status) || 'no status'}): a leaning only`
        : null;
  return {
    species,
    subspecies,
    ...(scored
      ? {
          sex: prediction.sex,
          sex_confidence: prediction.sex_conf,
          sex_trusted: !why,
          ...(why ? { sex_not_trusted: why } : {}),
          ...(unreliable ? { sex_not_trusted_why: text(unreliable.why) } : {}),
        }
      : { sex: null, sex_status: text(prediction.sex_status) || 'not scored' }),
    species_model: prediction.model ?? null,
    sex_model: prediction.sex_model ?? null,
  };
}

const sexWord = v => {
  const s = fold(v).replace(/\s*\?$/, '');
  return s === 'male' || s === 'female' ? s : null;
};
const NO_SUBSPECIES = /^(na|unknown|-|)$/;

/**
 * Where the models' top answer or the envelope's CAM read is not the database's: 'species', 'subspecies' (only
 * when the species agree and the database names one), 'sex' (whether or not the sex model is trusted for it:
 * the answer says which), 'envelope_cam'.
 */
export function differences({ cam, species, subspecies, sex, model, envelope }) {
  const out = [];
  const sp = model?.species[0]?.name;
  if (sp && species && binomialOf(sp) !== binomialOf(species)) out.push('species');
  const ssp = model?.subspecies[0]?.name;
  if (ssp && species && binomialOf(ssp) === binomialOf(species) && !NO_SUBSPECIES.test(fold(subspecies)) && fold(ssp).split(' ')[2] !== fold(subspecies))
    out.push('subspecies');
  if (model?.sex && sexWord(sex) && sexWord(sex) !== model.sex) out.push('sex');
  if (envelope?.camid && text(envelope.camid).toUpperCase() !== cam) out.push('envelope_cam');
  return out;
}

const shortName = (name, genus) => {
  const w = text(name).split(' ');
  return genus && w[0] === genus && w.length > 2 ? `${w[0][0]}. ${w.slice(1).join(' ')}` : text(name);
};
const flagged = (s, key) => (s.differs?.includes(key) ? ' **(not the database\'s)**' : '');
const stemOf = name => text(name).replace(/\.[^.]+$/, '');
const factsOf = (s, extra = true) =>
  [s.species, s.subspecies, s.sex, s.locality, extra ? s.country : null, extra ? s.date : null, s.elevation_m !== null ? `${s.elevation_m} m` : null]
    .filter(Boolean)
    .join(', ');

/** One specimen in full: the database's line, a 600 px photo, the wings and envelope crops, the models' output. */
function detailedMarkdown(s) {
  const out = [`**${s.cam_id}** in the database: ${factsOf(s) || 'no species or sex recorded'}`];
  const main = s.photos.dorsal ?? s.photos.ventral;
  out.push(`[![${s.cam_id} ${s.photos.dorsal ? 'dorsal' : 'ventral'}](${main.preview})](${main.full})`);
  const crops = ['dorsal', 'ventral'].filter(k => s.photos[k]?.wings).map(k => `[![${s.cam_id} wings ${k}](${s.photos[k].wings})](${s.photos[k].full})`);
  if (s.envelope?.crop) {
    const from = ['dorsal', 'ventral'].map(k => s.photos[k]).find(x => x && stemOf(x.name).toUpperCase() === text(s.envelope.photo).toUpperCase()) ?? main;
    crops.push(`[![${s.cam_id} envelope](${s.envelope.crop})](${from.full})`);
  }
  if (crops.length) out.push(crops.join(' '));
  out.push(`Full photos: ${['dorsal', 'ventral'].filter(k => s.photos[k]).map(k => `[${k}](${s.photos[k].full})`).join(', ')}`);
  const m = s.model;
  if (m) {
    const genus = m.species[0]?.name.split(' ')[0];
    const parts = [];
    if (m.species.length) parts.push(`species ${m.species.map(x => `${x.name} ${percent(x.p)}`).join(', ')}${flagged(s, 'species')}`);
    if (m.subspecies.length) parts.push(`subspecies ${m.subspecies.map(x => `${shortName(x.name, genus)} ${percent(x.p)}`).join(', ')}${flagged(s, 'subspecies')}`);
    parts.push(m.sex ? `sex ${m.sex} ${percent(m.sex_confidence)}, ${m.sex_trusted ? 'a Supported call' : m.sex_not_trusted}${flagged(s, 'sex')}` : `sex not scored (${m.sex_status})`);
    out.push(`Model output, not checked: ${parts.join('; ')}.`);
  } else out.push('Model output: none stored for this specimen.');
  if (s.envelope?.lines.length)
    out.push(
      `Envelope as read by the model ${s.envelope.model ?? '(not named)'}, not checked: ${s.envelope.lines.join(' / ')}${flagged(s, 'envelope_cam')}${s.envelope.cam_read ? '' : ' (no CAM ID read from it: the reading may be poor)'}`,
    );
  return out.join('  \n');
}

/** One specimen on one plain line. */
function plainMarkdown(s) {
  const m = s.model;
  const model = m?.species[0] ? `; model: ${m.species[0].name} ${percent(m.species[0].p)}${m.sex ? `, ${m.sex} ${percent(m.sex_confidence)}${m.sex_trusted ? '' : ' (not trusted)'}` : ''}` : '';
  const flag = s.differs?.length ? ` (not the database's: ${s.differs.join(', ')})` : '';
  const links = ['dorsal', 'ventral'].filter(k => s.photos[k]).map(k => `[${k[0]}](${s.photos[k].full})`).join(' ');
  return `- **${s.cam_id}** ${factsOf(s, false)}${model}${flag}${links ? ` ${links}` : ''}`;
}

/**
 * A specimen as the answer's JSON gives it: the preview and crop URLs only in the Markdown, the models' names in
 * `models`; past the detailed ones, the models' first answer only.
 */
function compactSpecimen(s, detailed) {
  const { species_model, sex_model, ...model } = s.model ?? {};
  const first = list => list?.[0] ?? null;
  const photo = x => x && (({ preview, wings, ...rest }) => rest)(x);
  return {
    ...s,
    photos: { dorsal: photo(s.photos.dorsal), ventral: photo(s.photos.ventral) },
    model: !s.model ? null : detailed ? model : { species: first(model.species), subspecies: first(model.subspecies), ...Object.fromEntries(Object.entries(model).filter(([k]) => k.startsWith('sex'))) },
    envelope: s.envelope ? (({ model: _, crop, ...e }) => e)(s.envelope) : null,
  };
}

/**
 * The answer's Markdown: the first `detailed` specimens with photos in full, the next PLAIN_ROWS one plain line
 * each (the rest are counted: the gallery link has them).
 */
export function specimensMarkdown(shown, detailed) {
  const full = shown.slice(0, detailed).filter(s => s.photos.dorsal || s.photos.ventral);
  const rest = shown.filter(s => !full.includes(s));
  const rows = rest.slice(0, PLAIN_ROWS).map(plainMarkdown);
  if (rest.length > PLAIN_ROWS) rows.push(`- and ${rest.length - PLAIN_ROWS} more (in \`specimens\` and the gallery link)`);
  return [...full.map(detailedMarkdown), ...(rows.length ? [rows.join('\n')] : [])].join('\n\n');
}

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
    // The app ANDs its genus and species filters: a genus asked with species is given as its species.
    const mixed = taxa.genus.length && taxa.species.length;
    const urlTaxa = {
      species: mixed ? [...new Set([...taxa.species, ...ofTaxa.filter(r => taxa.genus.includes(r.genus)).map(r => r.name)])] : taxa.species,
      // A subspecies filter would hide the rest of a genus asked.
      subspecies: taxa.genus.length ? [] : taxa.subspecies,
      genus: mixed ? [] : taxa.genus,
      sources,
    };
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
      map: mapsUrl({ ...urlTaxa, country: countriesIn(args.country, records), view }, mapsBase),
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
    const country = countriesIn(args.country, records);
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
      taxa: ranked.slice(0, limit).map(([name, t]) => ({ name, records: t.records, sites: t.sites.size, by_source: Object.fromEntries(t.sources), last_year: t.last, map: mapsUrl({ species: [name], sources, country, view }, mapsBase) })),
      map: mapsUrl({ species: ranked.slice(0, 20).map(([n]) => n), sources, country, view }, mapsBase),
      map_note: 'The region\'s map holds the first 20 taxa; each taxon has its own link.',
      freshness,
    };
  }


  // Host plants in an area: the maps' plant layers (data/host_plants/, ithomiini_maps docs/host-plant-links.md).
  // Each taxon's refs file names the records it has in the 24 occurrence chunks; only the chunks a call needs are
  // read, each kept as typed arrays (about 0.4 MB for its 20,000 records).
  const HP = 'host_plants/';
  const plantLayers = () =>
    remote(`${HP}host_plant_layers_manifest.json`, json => {
      const m = json?.metadata ?? {};
      return {
        generated_at: m.generated_at ?? null,
        refs: m.occurrence_refs_by_taxon ?? {},
        galleries: m.gallery_indexes_by_taxon ?? {},
        caveat: text(m.scientific_caveat) || null,
        taxa: new Map(
          (json?.taxa ?? []).map(t => [
            t.slug,
            { slug: t.slug, name: text(t.canonical_name), rank: t.rank, family: t.family ?? null, gbif_name: text(t.gbif_resolution?.canonical_name) || null },
          ]),
        ),
      };
    });
  const outlines = () => remote('country_boundaries.json', json => ({ ...json, countries: json?.countries ?? [] }));
  const plantRefs = path => remote(`${HP}${path}`, json => ({ total: json?.metadata?.total_records ?? null, chunks: Object.entries(json?.chunks ?? {}) }));
  const plantChunk = path =>
    remote(`${HP}${path}`, json => {
      const records = json?.records ?? [];
      const n = records.length;
      const out = { lat: new Float64Array(n), lng: new Float64Array(n), year: new Int16Array(n), basis: new Array(n) };
      records.forEach((r, i) => {
        const [lat, lng] = [Number(r?.lat), Number(r?.lng)];
        out.lat[i] = Number.isFinite(lat) ? lat : NaN;
        out.lng[i] = Number.isFinite(lng) ? lng : NaN;
        out.year[i] = yearOf(r?.eventDate) ?? 0;
        out.basis[i] = intern(text(r?.basisOfRecord).toLowerCase());
      });
      return out;
    });
  const galleryIndex = path => remote(`${HP}${path}`, json => ({ total: json?.total_images ?? 0, pages: json?.pages ?? [] }));
  const galleryPage = path =>
    remote(`${HP}${path}`, json =>
      (json?.items ?? [])
        .filter(p => /^https:\/\//.test(text(p?.image_url)))
        .map(p => ({
          img: text(p.image_url),
          record: text(p.observation_url) || null,
          lat: Number(p.lat),
          lng: Number(p.lng),
          year: yearOf(p.observation_date),
          field: p.photo_context === 'field',
          source: intern(text(p.source)),
        })),
    );

  /** The area asked of the plant records: { test(lng, lat), countries, unknown, bbox, near }, null for none, or { error }. */
  async function plantArea(args) {
    const asked = list(args.country);
    const bbox = Array.isArray(args.bbox) && args.bbox.length === 4 && args.bbox.every(Number.isFinite) ? args.bbox : null;
    const near = args.near && Number.isFinite(args.near.lat) && Number.isFinite(args.near.lng) ? { lat: args.near.lat, lng: args.near.lng, km: Number(args.near.km) || 25 } : null;
    if (!asked.length && !bbox && !near) return null;
    let o = null;
    const countries = [];
    const unknown = [];
    if (asked.length) {
      o = (await outlines()).value;
      const byName = new Map(o.countries.map(c => [fold(c.name), c]));
      for (const c of asked) (byName.has(fold(c)) ? countries.push(byName.get(fold(c)).name) : unknown.push(c));
      if (!countries.length) return { error: `The country outlines do not know ${unknown.join(', ')}: give a country of the Americas as the maps spell it (Ecuador, Peru, Costa Rica…).` };
    }
    const wanted = new Set(countries);
    const boxes = o ? o.countries.filter(c => wanted.has(c.name)).map(c => c.bbox) : [];
    const test = (lng, lat) => {
      if (!Number.isFinite(lng) || !Number.isFinite(lat)) return false;
      if (bbox && (lng < bbox[0] || lat < bbox[1] || lng > bbox[2] || lat > bbox[3])) return false;
      if (near && km(near, { lat, lng }) > near.km) return false;
      if (!wanted.size) return true;
      // Cheap reject first: outside every asked country's box (with the coast margin).
      return boxes.some(b => inBox(lng, lat, b, COAST_TOLERANCE_DEG)) && wanted.has(countryOfPoint(lng, lat, o));
    };
    return { test, countries, unknown, bbox, near, outlines: o };
  }

  /** The named places of the maps' butterfly records (not Dore et al.'s grid cells), on a 0.1° grid; built once per file. */
  const placeGrids = new WeakMap();
  async function namedPlaces() {
    const { value: m } = await manifest();
    const grids = [];
    for (const key of PLACE_SOURCES) {
      const file = m.sources?.[SOURCES[key]]?.file;
      if (!file) continue;
      const { value } = await remote(file, json => (Array.isArray(json) ? json : []).map(p => compactPoint(p, intern)).filter(Boolean));
      if (!placeGrids.has(value)) {
        const grid = new Map();
        for (const r of value) {
          if (!r.place || /^(na|unknown|-)$/i.test(r.place)) continue;
          const cell = `${Math.floor(r.lat * 10)}:${Math.floor(r.lng * 10)}`;
          (grid.get(cell) ?? grid.set(cell, []).get(cell)).push(r);
        }
        placeGrids.set(value, grid);
      }
      grids.push(placeGrids.get(value));
    }
    /** The nearest named place within PLACE_KM: "Tena, Hollín (2.1 km)", or null. */
    return point => {
      let best = null;
      const [row, col] = [Math.floor(point.lat * 10), Math.floor(point.lng * 10)];
      for (const grid of grids)
        for (let dr = -1; dr <= 1; dr++)
          for (let dc = -1; dc <= 1; dc++)
            for (const r of grid.get(`${row + dr}:${col + dc}`) ?? []) {
              const d = km(point, r);
              if (d <= PLACE_KM && (!best || d < best.d)) best = { d, r };
            }
      if (!best) return null;
      const { place, country } = best.r;
      const named = country && !fold(place).includes(fold(country)) ? `${place}, ${country}` : place;
      return `${named} (${best.d < 1 ? '<1' : Math.round(best.d)} km)`;
    };
  }

  /** Records on a grid of `siteKm` cells, busiest first: { lat, lng, n, years, plants, keys }. */
  function plantSites(points, siteKm) {
    const deg = siteKm / 111;
    const sites = new Map();
    for (const p of points) {
      const row = Math.floor(p.lat / deg);
      const key = `${row}:${Math.floor(p.lng / (deg / Math.max(0.2, Math.cos(((row + 0.5) * deg * Math.PI) / 180))))}`;
      const s = sites.get(key) ?? sites.set(key, { n: 0, lat: 0, lng: 0, years: new Set(), plants: new Map() }).get(key);
      s.n++;
      s.lat += p.lat;
      s.lng += p.lng;
      if (p.year) s.years.add(p.year);
      for (const name of p.plants) count(s.plants, name);
    }
    return [...sites.values()]
      .map(s => ({ ...s, lat: s.lat / s.n, lng: s.lng / s.n }))
      .sort((a, b) => b.n - a.n || b.years.size - a.years.size);
  }
  const yearsOf = set => {
    const y = [...set].sort();
    return y.length ? { first: y[0], last: y.at(-1), distinct: y.length } : null;
  };
  const thumbOf = img => `https://wsrv.nl/?url=${encodeURIComponent(img.replace(/(inaturalist-open-data\.s3\.amazonaws\.com\/photos\/\d+\/)original\./, '$1medium.'))}&w=240&h=180&fit=cover&output=jpg`;

  /** A plant's photos, those taken in the area first (field photos before herbarium sheets), then elsewhere. */
  async function plantPhotos(path, area, want) {
    if (!path || want <= 0) return [];
    const index = (await galleryIndex(path)).value;
    const here = [];
    const away = [];
    for (const page of index.pages.slice(0, PHOTO_PAGES)) {
      for (const p of (await galleryPage(page)).value) (!area || area.test(p.lng, p.lat) ? here : away).push(p);
      if (here.filter(p => p.field).length >= want) break;
    }
    // Field photos first; one photo per GBIF record before a second of the same record.
    const ordered = [...here.sort((a, b) => b.field - a.field), ...away.sort((a, b) => b.field - a.field).map(p => ({ ...p, away: true }))];
    const seen = new Set();
    const first = ordered.filter(p => !p.record || (!seen.has(p.record) && seen.add(p.record)));
    return [...first, ...ordered.filter(p => !first.includes(p))].slice(0, want);
  }

  /**
   * Where the host plants of `kept` grow in the area: per plant its records, years, top sites; the best sites of the
   * species-level hosts together; their photos as Markdown; the maps link with the host-plant layer on.
   */
  /**
   * The host plants of `kept` (ranked associations) with records layers, their refs and the chunks those name:
   * { m, modified, shown, mapped, withRecords, withoutRecords, refs, chunks }. `mapped`: the plants that make sites,
   * photos and the map (species-level hosts; genus-level ones only when there are none, or that genus was asked).
   */
  async function hostPlantLayers(kept, args) {
    const { value: m, modified } = await plantLayers();
    // The plants in the order the associations were ranked (observed, species-level first).
    const bySlug = new Map();
    for (const a of kept) {
      const slug = text(a.host_taxon_slug);
      if (!slug) continue;
      const e = bySlug.get(slug) ?? bySlug.set(slug, { slug, butterflies: new Set(), evidence: new Set(), confidence: 0 }).get(slug);
      e.butterflies.add(text(a.butterfly_taxon).split(' ').slice(0, 2).join(' '));
      e.evidence.add(a.evidence_level);
      e.confidence = Math.max(e.confidence, CONFIDENCE[a.confidence] ?? 0);
    }
    const all = [...bySlug.values()].map(e => ({ ...e, taxon: m.taxa.get(e.slug) ?? { slug: e.slug, name: e.slug, rank: null } }));
    const withRecords = all.filter(e => m.refs[e.slug] && ['species', 'genus'].includes(e.taxon.rank));
    const withoutRecords = all.filter(e => !withRecords.includes(e)).map(e => e.taxon.name);
    const shown = withRecords.slice(0, PLANT_MAX);
    // Species-level hosts make the sites, photos and map; genus-level ones only when there are none, or that genus was asked.
    const genusAsked = shown.some(e => e.taxon.rank === 'genus' && fold(e.taxon.name) === fold(args.plant));
    const mapped = shown.some(e => e.taxon.rank === 'species') ? shown.filter(e => e.taxon.rank === 'species' || genusAsked) : shown;
    const refs = await Promise.all(shown.map(e => plantRefs(m.refs[e.slug]).then(r => r.value)));
    const chunks = new Map();
    await Promise.all([...new Set(refs.flatMap(r => r.chunks.map(([c]) => c)))].map(async c => chunks.set(c, (await plantChunk(c)).value)));
    return { m, modified, shown, mapped, withRecords, withoutRecords, refs, chunks };
  }

  /** The maps link's filters for these plants: the butterflies asked, the host-plant layer with the mapped plants. */
  function plantUrlTaxa(shown, mapped, asked, country = []) {
    const evidence = [...new Set(mapped.flatMap(e => [...e.evidence]))].filter(Boolean);
    const levels = [...new Set(mapped.map(e => e.taxon.rank))];
    const butterflies = [...new Set(shown.flatMap(e => [...e.butterflies]))].filter(Boolean);
    const asButterfly = !!asked.butterfly;
    return {
      species: asButterfly && fold(asked.butterfly).split(' ').length > 1 ? butterflies.slice(0, 10) : [],
      genus: asButterfly && fold(asked.butterfly).split(' ').length === 1 ? [butterflies[0]?.split(' ')[0]].filter(Boolean) : [],
      sources: SOURCE_KEYS.filter(k => k !== 'dore'),
      country,
      hostPlants: {
        butterflies: asButterfly ? butterflies.slice(0, 10) : [],
        taxa: mapped.map(e => e.slug),
        levels: levels.length === 1 && levels[0] === 'species' ? [] : levels,
        evidence: evidence.length === 2 && evidence.includes('direct') && evidence.includes('literature') ? [] : evidence,
      },
    };
  }

  async function plantsInArea(kept, args, area, asked) {
    const { m, modified, shown, mapped, withRecords, withoutRecords, refs, chunks } = await hostPlantLayers(kept, args);
    const siteKm = Math.min(50, Math.max(0.5, Number(args.site_km) || 5));
    const points = new Map();
    const plants = shown.map((e, i) => {
      const own = [];
      const years = new Set();
      const basis = new Map();
      for (const [c, indexes] of refs[i].chunks) {
        const chunk = chunks.get(c);
        for (const j of indexes ?? []) {
          const [lat, lng] = [chunk.lat[j], chunk.lng[j]];
          if (lat === undefined || (area && !area.test(lng, lat))) continue;
          const key = `${c}:${j}`;
          const p = points.get(key) ?? { lat, lng, year: chunk.year[j] || null, plants: new Set() };
          own.push(p);
          if (mapped.includes(e)) {
            p.plants.add(e.taxon.name);
            points.set(key, p);
          }
          if (p.year) years.add(p.year);
          count(basis, chunk.basis[j] || 'unknown');
        }
      }
      return { e, own, years, basis };
    });
    const named = await namedPlaces().catch(() => () => null);
    const sitesAll = plantSites([...points.values()], siteKm);
    const limit = clamp(args.limit, 1, 30, 8);
    const urlTaxa = plantUrlTaxa(shown, mapped, asked, area?.countries ?? []);
    const view = area?.near
      ? { lat: area.near.lat, lng: area.near.lng, zoom: Math.max(5, Math.min(12, Math.round(Math.log2(40000 / area.near.km)) - 1)) }
      : sitesAll.length
        ? viewOf(sitesAll)
        : area?.bbox
          ? viewOf([{ lat: area.bbox[1], lng: area.bbox[0] }, { lat: area.bbox[3], lng: area.bbox[2] }])
          : null;
    const where = area ? (area.countries.length ? area.countries.join(', ') : area.near ? `${area.near.km} km around ${area.near.lat}, ${area.near.lng}` : 'the box asked') : 'their whole range';
    const want = clamp(args.photos, 0, 6, 3);
    const photos = await Promise.all(
      plants.map(({ e, own }) => (mapped.includes(e) && own.length ? plantPhotos(m.galleries[e.slug], area, want) : [])),
    );
    const placeOf = p => (area?.outlines ? countryOfPoint(p.lng, p.lat, area.outlines) : null) ?? `${p.lat.toFixed(2)}, ${p.lng.toFixed(2)}`;
    const markdown = plants
      .map(({ e, own, years }, i) => {
        if (!photos[i].length) return null;
        const y = yearsOf(years);
        const gbif = e.taxon.gbif_name && e.taxon.gbif_name !== e.taxon.name ? ` (GBIF: ${e.taxon.gbif_name})` : '';
        const head = `**${e.taxon.name}**${gbif}: ${own.length} record${own.length === 1 ? '' : 's'} in ${where}${y ? `, ${y.first}–${y.last}` : ''}`;
        const thumbs = photos[i].map(p => `[![${e.taxon.name}${p.year ? ` ${p.year}` : ''}${p.away ? `, ${placeOf(p)}` : ''}](${thumbOf(p.img)})](${p.img})`).join(' ');
        const credit = `Photos (GBIF records): ${photos[i].map((p, k) => (p.record ? `[${k + 1}](${p.record})` : `${k + 1}`)).join(' ')}; from ${[...new Set(photos[i].map(p => p.source))].join('; ')}. Licence and author on each record's page.${photos[i].some(p => p.away) ? ` Some photos are from outside ${where} (their place in the caption).` : ''}`;
        return [head, thumbs, credit].join('  \n');
      })
      .filter(Boolean)
      .join('\n\n');
    return {
      area: {
        where,
        ...(area?.unknown.length ? { countries_not_known: area.unknown } : {}),
        ...(area?.bbox ? { bbox: area.bbox, bbox_note: 'The map link frames the box; the maps app limits plant records by country, not by this box.' } : {}),
      },
      plants: plants.map(({ e, own, years, basis }, i) => ({
        plant: e.taxon.name,
        ...(e.taxon.gbif_name && e.taxon.gbif_name !== e.taxon.name ? { gbif_name: e.taxon.gbif_name } : {}),
        rank: e.taxon.rank,
        host_of: [...e.butterflies],
        evidence: [...e.evidence].join(', '),
        records: own.length,
        records_total: refs[i].total,
        years: yearsOf(years),
        by_basis: Object.fromEntries([...basis].sort((a, b) => b[1] - a[1])),
        ...(own.length
          ? {
              top_sites: plantSites(own.map(p => ({ ...p, plants: [] })), siteKm)
                .slice(0, 3)
                .map(s => ({ lat: round(s.lat, 4), lng: round(s.lng, 4), records: s.n, years: yearsOf(s.years), near: named(s) })),
            }
          : {}),
        ...(mapped.includes(e) ? {} : { note: 'genus-level host: its records are of any species of the genus; not in `sites` or the map link' }),
      })),
      ...(withoutRecords.length ? { plants_without_records: withoutRecords } : {}),
      ...(withRecords.length > shown.length ? { plants_left_out: withRecords.slice(PLANT_MAX).map(e => e.taxon.name) } : {}),
      sites_total: sitesAll.length,
      sites: sitesAll.slice(0, limit).map((s, i) => ({
        rank: i + 1,
        lat: round(s.lat, 4),
        lng: round(s.lng, 4),
        near: named(s),
        records: s.n,
        plants: Object.fromEntries([...s.plants].sort((a, b) => b[1] - a[1])),
        years: yearsOf(s.years),
        map: mapsUrl({ ...urlTaxa, view: { lat: s.lat, lng: s.lng, zoom: siteKm <= 3 ? 12 : 11 } }, mapsBase),
      })),
      map_url: mapsUrl({ ...urlTaxa, view }, mapsBase),
      markdown,
      records_note: `Plant records: the maps' GBIF occurrences of each plant (generated ${text(m.generated_at).slice(0, 10) || 'date unknown'}), placed in a country by their coordinates (Natural Earth 1:50m outlines: a record within a few km of a border can fall on the wrong side). They show where the plant has been collected or photographed, not that the butterfly uses it there. ${m.caveat ?? ''}`.trim(),
      plant_data: { manifest: new URL(`data/${HP}host_plant_layers_manifest.json`, mapsBase).href, modified: modified ?? null },
    };
  }

  /** The associations asked (`butterfly` or `plant`, confidence): { got, matched, kept, okConfidence, rank, butterfly } or { error }. */
  async function associationsFor(args) {
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
    // Direct observations and species-level hosts first.
    const rank = a => (a.evidence_level === 'direct' ? 0 : 2) + (a.host_id_level === 'species' ? 0 : 1) - (CONFIDENCE[a.confidence] ?? 0) / 10;
    const kept = matched.filter(okConfidence).sort((a, b) => rank(a) - rank(b));
    return { got, matched, kept, okConfidence, butterfly };
  }

  /** Every photo of these plants in their first NEAR_PAGES gallery pages (field and herbarium), with its plant. */
  async function plantGallery(plants, m) {
    const out = [];
    await Promise.all(
      plants.map(async e => {
        const path = m.galleries[e.slug];
        if (!path) return;
        const index = (await galleryIndex(path)).value;
        const pages = await Promise.all(index.pages.slice(0, NEAR_PAGES).map(page => galleryPage(page).then(r => r.value)));
        for (const p of pages.flat()) if (Number.isFinite(p.lat) && Number.isFinite(p.lng)) out.push({ ...p, plant: e.taxon.name });
      }),
    );
    return out;
  }

  /**
   * The sites near a point ranked for collecting the host plants asked: the project's collection localities, or
   * grid cells, each scored by the plant records around it (kernel()), with photos of the records nearest it.
   */
  async function plantSitesNear(args) {
    const near = args.near && Number.isFinite(args.near.lat) && Number.isFinite(args.near.lng) ? { lat: args.near.lat, lng: args.near.lng, km: Math.min(100, Math.max(1, Number(args.near.km) || 25)) } : null;
    if (!near) return { error: 'Give `near` {lat, lng, km}: the point to start from (Ikiam is about -0.95, -77.86).' };
    const found = await associationsFor(args);
    if (found.error) return found;
    const { m, shown, mapped, withoutRecords, refs, chunks } = await hostPlantLayers(found.kept, args);
    if (!mapped.length) return { error: 'None of these host plants has a records layer on the maps.', ...(withoutRecords.length ? { plants_without_records: withoutRecords } : {}) };
    // The mapped plants' records that can count for a site within near.km: one per GBIF record (synonymous plants share it).
    const reach = near.km + KERNEL_KM;
    const records = new Map();
    shown.forEach((e, i) => {
      if (!mapped.includes(e)) return;
      for (const [c, indexes] of refs[i].chunks) {
        const chunk = chunks.get(c);
        for (const j of indexes ?? []) {
          const [lat, lng] = [chunk.lat[j], chunk.lng[j]];
          if (!Number.isFinite(lat) || km(near, { lat, lng }) > reach) continue;
          const key = `${c}:${j}`;
          const r = records.get(key) ?? records.set(key, { lat, lng, year: chunk.year[j] || null, plants: new Set() }).get(key);
          r.plants.add(e.taxon.name);
        }
      }
    });
    const plantRecords = [...records.values()];
    const scoreAt = site => {
      const out = { score: 0, rings: SCORE_RINGS.map(() => 0), plants: new Map(), years: [], undated: 0, nearest: null };
      for (const r of plantRecords) {
        const d = km(site, r);
        if (!out.nearest || d < out.nearest.d) out.nearest = { d, r };
        if (d >= KERNEL_KM) continue;
        out.score += kernel(d);
        SCORE_RINGS.forEach((ring, k) => d <= ring && out.rings[k]++);
        for (const name of r.plants) count(out.plants, name);
        if (r.year) out.years.push(r.year);
        else out.undated++;
      }
      return out;
    };
    const asked = { butterfly: args.butterfly, plant: args.plant };
    const words = fold(args.butterfly).split(' ').filter(Boolean);
    const isButterfly = r => (words.length === 1 ? fold(r.genus) === words[0] : words.length > 1 && fold(r.name) === words.slice(0, 2).join(' '));
    const mode = args.rank === 'grid' ? 'grid' : 'localities';
    let candidates;
    if (mode === 'localities') {
      // The project's localities: its Sanger records in the maps, by locality name, split where its records are over ~5 km apart.
      const { records: own } = await points(['sanger']);
      const { elevation } = collection();
      const byPlace = new Map();
      for (const r of own) {
        if (!r.place || /^(na|unknown|-)$/i.test(r.place)) continue;
        const key = `${fold(r.place)}|${Math.round(r.lat / 0.05)}|${Math.round(r.lng / 0.05)}`;
        (byPlace.get(key) ?? byPlace.set(key, []).get(key)).push(r);
      }
      candidates = [...byPlace.values()]
        .map(rs => {
          const lat = median(rs.map(r => r.lat * 1e5)) / 1e5;
          const lng = median(rs.map(r => r.lng * 1e5)) / 1e5;
          const elev = rs.map(r => elevation.get(r.id.toUpperCase())).filter(v => v !== undefined);
          const names = new Map();
          rs.forEach(r => count(names, r.place));
          return {
            name: top(names),
            lat,
            lng,
            ...(elev.length ? { elevation_m: median(elev), elevation_from: 'workbook' } : {}),
            project_records: rs.length,
            ...(words.length ? { project_records_of_butterfly: rs.filter(isButterfly).length } : {}),
          };
        })
        .filter(c => km(near, c) <= near.km);
    } else {
      const cellKm = Math.min(5, Math.max(0.5, Number(args.cell_km) || 1.5));
      const deg = cellKm / 111;
      const cells = new Map();
      for (const r of plantRecords) {
        if (km(near, r) > near.km) continue;
        const row = Math.floor(r.lat / deg);
        const kLng = deg / Math.max(0.2, Math.cos(((row + 0.5) * deg * Math.PI) / 180));
        const col = Math.floor(r.lng / kLng);
        cells.set(`${row}:${col}`, { lat: (row + 0.5) * deg, lng: (col + 0.5) * kLng, cell_km: cellKm });
      }
      candidates = [...cells.values()];
    }
    const limit = clamp(args.limit, 1, 20, 8);
    const scored = candidates
      .map(c => ({ c, s: scoreAt(c), from: km(near, c) }))
      .sort((a, b) => b.s.score - a.s.score || a.from - b.from);
    // A site with no plant record within KERNEL_KM is counted, not listed.
    const kept = scored.filter(x => x.s.score > 0).slice(0, limit);
    const named = await namedPlaces().catch(() => () => null);
    if (mode === 'grid' && kept.length) {
      const asked = kept.map(({ c }) => ({ lat: c.lat, lng: c.lng, source: 'grid', id: '' }));
      const { out } = await elevations(asked);
      kept.forEach(({ c }, i) => {
        const e = out.get(asked[i]);
        if (e) Object.assign(c, { elevation_m: Math.round(e.m), elevation_from: 'dem' });
      });
    }
    const want = clamp(args.photos, 0, 4, 2);
    const nearPlants = mapped.filter(e => plantRecords.some(r => r.plants.has(e.taxon.name)));
    const gallery = want && kept.length ? await plantGallery(nearPlants, m) : [];
    const markdown = kept
      .slice(0, PHOTO_SITES)
      .map(({ c }, i) => {
        const label = c.name ?? named(c) ?? `${c.lat.toFixed(3)}, ${c.lng.toFixed(3)}`;
        const seen = new Set();
        const photos = gallery
          .map(p => ({ ...p, d: km(c, p) }))
          .sort((a, b) => a.d - b.d)
          .filter(p => !p.record || (!seen.has(p.record) && seen.add(p.record)))
          .slice(0, want);
        if (!photos.length) return null;
        const dist = d => (d < 1 ? '<1 km' : `${d < 10 ? d.toFixed(1) : Math.round(d)} km`);
        return [
          `**${i + 1}. ${label}**: the plant photos nearest it`,
          photos.map(p => `[![${p.plant}${p.year ? ` ${p.year}` : ''}, ${dist(p.d)} away](${thumbOf(p.img)})](${p.img})`).join(' '),
          `${photos.map((p, k) => `${p.record ? `[${k + 1}](${p.record})` : k + 1} ${p.plant}, ${dist(p.d)}`).join('; ')}. From ${[...new Set(photos.map(p => p.source))].join('; ')}; licence and author on each GBIF record's page.`,
        ].join('  \n');
      })
      .filter(Boolean)
      .join('\n\n');
    const urlTaxa = plantUrlTaxa(shown, mapped, asked);
    const sites = kept.map(({ c, s, from }, i) => {
      const years = s.years.sort((a, b) => a - b);
      return {
        rank: i + 1,
        ...(c.name ? { locality: c.name } : { near: named(c) }),
        lat: round(c.lat, 4),
        lng: round(c.lng, 4),
        km_from_start: round(from, 1),
        score: round(s.score, 1),
        records_within_km: Object.fromEntries(SCORE_RINGS.map((ring, k) => [ring, s.rings[k]])),
        plants_within_10_km: Object.fromEntries([...s.plants].sort((a, b) => b[1] - a[1])),
        nearest_record: s.nearest ? { plant: [...s.nearest.r.plants].join(' / '), km: round(s.nearest.d, 1), year: s.nearest.r.year } : null,
        record_years: years.length || s.undated ? { oldest: years[0] ?? null, newest: years.at(-1) ?? null, undated: s.undated } : null,
        ...(c.elevation_m !== undefined ? { elevation_m: c.elevation_m, elevation_from: c.elevation_from } : {}),
        ...(c.project_records !== undefined ? { project_records: c.project_records } : {}),
        ...(c.project_records_of_butterfly !== undefined ? { project_records_of_butterfly: c.project_records_of_butterfly } : {}),
        map: mapsUrl({ ...urlTaxa, view: { lat: c.lat, lng: c.lng, zoom: 12 } }, mapsBase),
      };
    });
    return {
      asked: { ...asked, near, rank: mode, min_confidence: args.min_confidence ?? 'medium' },
      plants: mapped.map(e => e.taxon.name),
      ...(shown.length > mapped.length ? { plants_not_scored: shown.filter(e => !mapped.includes(e)).map(e => `${e.taxon.name} (genus-level host)`) } : {}),
      ...(withoutRecords.length ? { plants_without_records: withoutRecords } : {}),
      plant_records_in_reach: plantRecords.length,
      candidates: candidates.length,
      ...(mode === 'localities' ? { localities_without_records_within_10_km: scored.filter(x => x.s.score === 0).length } : {}),
      sites,
      ...(sites.length ? { map_url: sites[0].map } : {}),
      markdown,
      method: `Score: each plant record within ${KERNEL_KM} km weighs (1 - (d/${KERNEL_KM})²)², 1 at the site, 0.85 at 2 km, 0.56 at 5 km; a GBIF record of two synonymous plants counts once. ${mode === 'localities' ? "Sites: the project's collection localities (its Sanger records in the maps; elevation from the workbook)." : 'Sites: grid cells with plant records, scored at their centre, named after the nearest named collecting place; elevation from a 90 m DEM.'} Plant records: the maps' GBIF download (generated ${text(m.generated_at).slice(0, 10) || 'date unknown'}).`,
      caveats: [
        'GBIF and iNaturalist records follow roads, field stations and observers: a high score can mean a well-sampled place rather than more plants. Each site gives the records and years behind its score.',
        'A plant record shows where the plant grows, not that the butterfly uses it there.',
      ],
    };
  }

  async function hostPlants(args) {
    const found = await associationsFor(args);
    if (found.error) return found;
    const { got, matched, kept, okConfidence, butterfly } = found;
    const groups = new Map();
    for (const a of kept) count(groups, butterfly ? a.host_plant_genus ?? `(${a.host_plant_family}, genus not given)` : a.butterfly_taxon.split(' ')[0]);
    // Each source's link and caveat once, in `sources`.
    const sources = {};
    const associations = kept
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
    const area = await plantArea(args);
    if (area?.error) return area;
    const where = area || args.records === true ? await plantsInArea(kept, args, area, { butterfly: args.butterfly, plant: args.plant }) : null;
    return {
      asked: {
        butterfly: args.butterfly ?? null,
        plant: args.plant ?? null,
        min_confidence: args.min_confidence ?? 'medium',
        ...(area ? { country: area.countries, bbox: area.bbox, near: area.near } : {}),
      },
      found: kept.length,
      ...(matched.length > kept.length ? { left_out: { count: matched.length - kept.length, by_confidence: Object.fromEntries([...matched.filter(a => !okConfidence(a)).reduce((m, a) => count(m, a.confidence), new Map())]) } } : {}),
      [butterfly ? 'by_plant_genus' : 'by_butterfly_genus']: Object.fromEntries([...groups].sort((a, b) => b[1] - a[1])),
      ...(onlyCoarse ? { note: 'Only genus- or family-level hosts: the plant species is not known.' } : {}),
      associations,
      sources,
      data: { file: new URL('data/host_plants/host_plant_associations.json', mapsBase).href, generated_at: got.value.metadata?.generated_at ?? null, modified: got.modified ?? null, policy: got.value.metadata?.excluded_policy ?? null },
      ...(where ?? {}),
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
    const thumbs = clamp(args.thumbnails, 0, DETAILED_MAX, 4);
    const kept = found.slice(0, limit);
    const read = photoReadings(store?.db, kept.map(({ cam, p }) => ({ cam, photos: { dorsal: p.dorsal ?? null, ventral: p.ventral ?? null } })));
    const shown = kept.map(({ cam, v, elev, p }, i) => {
      const detailed = i < thumbs;
      const r = read.get(cam) ?? { prediction: null, envelopes: [], boxes: {}, sizes: {} };
      const photo = view => {
        const x = p[view];
        if (!x) return null;
        const size = r.sizes[view] ?? null;
        const f = r.boxes[view];
        // The wing box as fractions of the photo, made pixels of its real size, with a small margin.
        const wings = detailed && f && size ? cropUrl(x.file, [f[0] * size[0], f[1] * size[1], f[2] * size[0], f[3] * size[1]], size, { pad: 0.02 * size[0] }) : null;
        return {
          name: x.name,
          file: x.file,
          full: fullUrl(x.file),
          ...(detailed ? { preview: previewUrl(x.file), ...(size ? { size } : {}), ...(wings ? { wings } : {}) } : {}),
        };
      };
      // The envelope: the reading that read this CAM ID best, then one that read another (shown as differing),
      // then one that read none (an upside-down reading); the dorsal photo's on a tie.
      const camConf = e => (!e.camid ? -2 : text(e.camid).toUpperCase() === cam ? (e.camid_conf ?? 0) : -1);
      const env = [...r.envelopes]
        .sort((a, b) => camConf(b) - camConf(a) || (b.view === 'dorsal') - (a.view === 'dorsal'))
        .find(e => e.lines.length || e.bbox);
      const envCrop = detailed && env?.file && env.bbox ? cropUrl(env.file, env.bbox, env.size, { width: 360, pad: 6 }) : null;
      const species = text(v.SPECIES) || null;
      const subspecies = text(v.Subspecies_Form) || null;
      const sex = text(v.Sex) || null;
      const model = modelOf(r.prediction, species);
      const differs = differences({ cam, species, subspecies, sex, model, envelope: env });
      return {
        cam_id: cam,
        species,
        subspecies,
        sex,
        locality: text(v.Collection_location) || null,
        country: text(v.Country) || null,
        date: dateOf(v.Collection_date),
        elevation_m: elev,
        photos: { dorsal: photo('dorsal'), ventral: photo('ventral') },
        model,
        envelope: env
          ? { photo: env.name, lines: env.lines, cam_read: env.camid ?? null, model: env.model ?? null, ...(envCrop ? { crop: envCrop } : {}) }
          : null,
        ...(differs.length ? { differs } : {}),
      };
    });
    // The Collection tab filters by species, subspecies and sex only; anything else is the search tab with the IDs.
    const byFilters = (sp.size || ssp.size) && !cams.size && !genera.size && !place.length && !countries.size && low === null && high === null && withPhotos;
    const speciesNames = [...new Set(found.map(f => text(f.v.SPECIES)))];
    const url = byFilters
      ? galleryUrl({ species: speciesNames, subspecies: [...new Set(found.map(f => text(f.v.Subspecies_Form)).filter(s => ssp.has(fold(s))))], sex: sex || null }, galleryBase)
      : found.length
        ? galleryUrl({ camids: found.map(f => f.cam) }, galleryBase)
        : null;
    // The models' names once for the answer, not in every specimen; the rows past `thumbnails` with only the
    // models' first answer: the answer stays well within the tools' budget (server/tool-budget.mjs).
    const named = key => [...new Set(shown.map(key).filter(Boolean))];
    const models = { species: named(s => s.model?.species_model), sex: named(s => s.model?.sex_model), envelope: named(s => s.envelope?.model) };
    return {
      found: found.length,
      shown: shown.length,
      specimens: shown.map((s, i) => compactSpecimen(s, i < thumbs)),
      models,
      gallery: url,
      ...(url && !byFilters && found.length > URL_CAMS ? { gallery_note: `The link holds the first ${URL_CAMS} of ${found.length} CAM IDs.` } : {}),
      markdown: specimensMarkdown(shown, thumbs),
      note: 'From the workbook and Photo_links as this app has them now; the gallery site shows a new specimen after its next update. `model` and `envelope` are model output, not checked: the database is the record.',
    };
  }

  return {
    async run(name, args = {}) {
      try {
        if (name === 'maps_localities') return await localities(args);
        if (name === 'maps_species') return await species(args);
        if (name === 'host_plants') return await hostPlants(args);
        if (name === 'host_plant_sites') return await plantSitesNear(args);
        if (name === 'gallery_specimens') return specimens(args);
      } catch (e) {
        return { error: e.message };
      }
      return { error: `Unknown tool ${name}` };
    },
  };
}
