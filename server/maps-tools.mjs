// Read-only tools on the project's public sites (docs/assistant-tools-maps-gallery.md):
//   maps_localities    places with the most records of some taxa (the maps' occurrence records), with map links
//   maps_species       the taxa recorded in a region
//   host_plants        host plants of a butterfly, or the Ithomiini on a plant, with their sources; with an area, where
//                      each host plant grows there (the maps' GBIF plant records), its photos and a map link
//   host_plant_sites   where to go first for some host plants near a point: the project's collection localities (or grid
//                      cells) ranked by the plant records around them, weighted by distance
//   gallery_specimens  collection specimens with wing photos (the workbook and Photo_links), gallery links, photos
//                      with wing and envelope crops, and the models' readings stored for them (labelled model output)
//   maps_snapshot      a picture of the maps with some taxa (one sex too) framed on the best sites (server/map-snapshot.mjs)
// `area` (every tool with a region): a province or canton of Ecuador by its INEC outline (server/admin-areas.mjs).
// The maps' data files are read over HTTPS from the published site, at most every 6 hours (and parsed again only
// when their ETag changed), and kept in memory as compact lists. The workbook is read with SELECT only. Nothing is
// written anywhere.

import { readFileSync, statSync } from 'node:fs';
import { join } from 'node:path';
import { writeCacheFile } from './cache-write.mjs';
import { driveIdOf, specimenName } from './photo-links.mjs';
import { COAST_TOLERANCE_DEG, countryOfPoint, inBox, lngPadAt } from './country-outlines.mjs';
import { unreliableFor } from './sex-check.mjs';
import { PALETTE, createSnapshots } from './map-snapshot.mjs';
import { cantonOf, createAdminAreas } from './admin-areas.mjs';
import { createPlantPhotos, creditOf, licenceName, proxyUrl } from './plant-photos.mjs';
import { PER_SHEET, TILE, createPhotoSheets } from './photo-sheets.mjs';

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
// An answer with an area keeps itself within the tools' budget (server/tool-budget.mjs): its Markdown at most
// MARKDOWN_CHARS, map links on the first SITE_LINKS sites (map_url has them all), and the whole at most AREA_BUDGET.
const MARKDOWN_CHARS = 12000;
const SITE_LINKS = 3;
const AREA_BUDGET = 36000;
const CHUNKS_AT_ONCE = 2;
// Host plants with an area: the default reach (km) of `nearby_km`, the farthest a nearest record is looked for, and
// the gallery pages read per plant for `send_photos`.
const NEARBY_KM = 50;
const NEAREST_MAX_KM = 300;
const SEND_PAGES = 12;
// taxon_photos: iNaturalist's Life Stage annotation (term 1) and its values; GBIF's lifeStage words; the
// iNaturalist dataset on GBIF (its records come from iNaturalist itself).
const INAT_API = 'https://api.inaturalist.org/v1/observations';
const GBIF_SEARCH = 'https://api.gbif.org/v1/occurrence/search';
const LIFE_STAGES = { egg: { inat: 7, gbif: 'Egg' }, larva: { inat: 6, gbif: 'Larva' }, pupa: { inat: 4, gbif: 'Pupa' }, adult: { inat: 2, gbif: 'Adult' } };
const INAT_DATASET = '50c9509d-22c7-4a22-a47d-8c48425ef4a7';
const IKIAM = { lat: -0.95, lng: -77.86 };
const STAGE_WORDS = { en: { egg: 'eggs', larva: 'larva', pupa: 'pupa', adult: 'adult' }, es: { egg: 'huevos', larva: 'larva', pupa: 'pupa', adult: 'adulto' } };
// The photo tags file in the cache folder, and the probability of "leaves or whole plant" over which a photo counts
// as showing them (tools/plant-photo-tags/CALIBRATION.md: over 80 photos checked by eye, 26 of 27 so tagged showed
// them and 1 of 33 flower close-ups was so tagged; a small sample: no errors expected, not ruled out).
export const TAGS_FILE = 'plant-photo-tags-v1.json';
export const TAG_LEAVES = 0.8;
const SEND_WORDS = {
  en: {
    area: 'the area',
    as: 'recorded as',
    inArea: (where, n) => `recorded in ${where} (${n} record${n === 1 ? '' : 's'})`,
    candidate: (where, n, km) => `CANDIDATE: no record in ${where}, ${n} within ${km} km`,
    photoOutside: where => `photo outside ${where}`,
    photo: 'Photo',
    unknown: 'author not given',
    herbariumTag: '[Herbarium specimen]',
    candidateShort: 'candidate',
    herbariumShort: 'herbarium',
    views: { leaves: 'leaves', habit: 'whole plant', flower: 'flower', fruit: 'fruit' },
    herbarium: 'herbarium specimen (no photo of the living plant)',
  },
  es: {
    area: 'el área',
    as: 'registrada como',
    inArea: (where, n) => `registrada en ${where} (${n} registro${n === 1 ? '' : 's'})`,
    candidate: (where, n, km) => `CANDIDATA: sin registros en ${where}, ${n} a menos de ${km} km`,
    photoOutside: where => `foto fuera de ${where}`,
    photo: 'Foto',
    unknown: 'autor no indicado',
    herbariumTag: '[Ejemplar de herbario]',
    candidateShort: 'candidata',
    herbariumShort: 'herbario',
    views: { leaves: 'hojas', habit: 'planta entera', flower: 'flor', fruit: 'fruto' },
    herbarium: 'ejemplar de herbario (no hay foto de la planta viva)',
  },
};
/** A record's weight in a site's score: quartic kernel, 1 at the site, 0 at KERNEL_KM (0.92 at 2 km, 0.56 at 5 km). */
export const kernel = d => (d >= KERNEL_KM ? 0 : (1 - (d / KERNEL_KM) ** 2) ** 2);
const CLUSTER_KM = 150;

const AREA = {
  anyOf: [{ type: 'string' }, { type: 'object', properties: { province: { type: 'string' }, canton: { type: 'string' } } }],
  description: 'A province or canton of Ecuador, by its INEC outline: {province: "Napo"}, {canton: "Tena"} (with `province` when two share the name), or "Napo"',
};
const region = {
  area: AREA,
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
  sex: { type: 'string', enum: ['male', 'female'] },
};

export const MAPS_TOOLS = [
  {
    type: 'function',
    function: {
      name: 'maps_localities',
      description: [
        "Where to find some taxa: the places with the most occurrence records of the Ithomiini maps (the project's own collection records, GBIF museum records, Dore et al. 2022, iNaturalist), best first, each with a link that opens the maps app on it with the filters set. For questions like \"where are the best places to find X?\".",
        '- `taxa`: species ("Oleria onega"), subspecies ("Oleria onega janarilla") or genus ("Oleria"); synonyms are resolved.',
        '- Region (optional): `area` (a province or canton of Ecuador by its outline: {province: "Napo"}, {canton: "Tena"}), `country`, `bbox` [west, south, east, north], `near` {lat, lng, km}. `elevation_min`/`elevation_max` in metres (the workbook\'s elevation for the project\'s records, the ground\'s from a 90 m DEM for the rest). `sources` (default all), `years` {from, to}.',
        '- `sex` (male, female): only the records of that sex. The project\'s records nearly all have it, GBIF\'s about four in ten, iNaturalist\'s and Dore et al.\'s almost never, so with it a site is ranked by its records of that sex; every site gives `by_sex` (all its records: male, female, unknown) and `own_by_sex` (the project\'s).',
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
        '- A region is needed: `area` ({province} or {canton} of Ecuador), `country`, `bbox` or `near` {lat, lng, km}; with `elevation_min`/`elevation_max`, `sources`, `years`, `sex` as in maps_localities. `genus` narrows it. `limit` (default 30).',
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
        "- Area (optional): `area` (a province or canton of Ecuador by its outline: {province: \"Napo\"}, {canton: \"Tena\"}), `country` (list), `bbox` [west, south, east, north] or `near` {lat, lng, km}; `records: true` for the whole range. Then `plants`: each host plant's records there (the maps' GBIF plant records, placed in countries by their coordinates), years and top sites; `sites`: the best sites for the species-level hosts together, with coordinates, years and the nearest named collecting place; `markdown`: `photos` (default 3, at most 6) thumbnails per plant, each linked to its full photo, with credit; `map_url`: the Wings Atlas with the host-plant layer, these plants and the country on. `site_km` (default 5), `limit` (sites, default 8).",
        "- With `area`, `bbox` or `near`: each plant's `status` (in_area, nearby, far) and `nearby` (records outside but within `nearby_km`, default 50 km: count, years, by province or country, and the nearest record's km, year and place); `in_area`, `nearby_only` and `not_near` list them. Plants only nearby are candidates, not records from the area: say so.",
        '- `photo_sheet: true` (with an area): `photo_sheets`, 1 or 2 contact sheets (JPEG, 3 by 3 numbered tiles of 512 px with the plant and its place) of the plants\' candidate photos (living plants first, those in the area or nearest). Look at them and pick the tiles that show the leaves or the whole plant, and a flower, then send_sheet_photos. This is how photos to send are chosen.',
        '- `send_photos` (a number): the same as `photo_sheet: true` (photos to send are always chosen by looking at a contact sheet first).',
        'Give the citation and link of each claim. A catalogue row is a published record, not observed use at a given place; a family-level host says little. Plant records show where the plant grows, never that the butterfly uses it there: say so.',
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          butterfly: { type: 'string' },
          plant: { type: 'string' },
          min_confidence: { type: 'string', enum: ['low', 'medium', 'high'] },
          include_unchecked: { type: 'boolean' },
          area: AREA,
          nearby_km: { type: 'number' },
          send_photos: { type: 'integer' },
          photo_sheet: { type: 'boolean' },
          lang: { type: 'string', enum: ['en', 'es'] },
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
      name: 'taxon_photos',
      description: [
        'Photos of a butterfly (or any taxon) at a life stage, to show or send: eggs, larvae, pupae or adults. For "photos of the eggs and larvae of X". Without an area, the photos nearest Ikiam come first.',
        '- `taxon`: species or genus. `life_stage`: egg, larva, pupa, adult (a list, or one). Optional `area` ({province}/{canton} of Ecuador) or `near` {lat, lng, km}: photos there first, then the nearest.',
        "- Sources: the project's own clutch photos (Clutches tab: eggs and larvae of the insectary's clutches), iNaturalist observations annotated with that life stage (research grade first), and GBIF records of other datasets whose lifeStage says it. Unannotated iNaturalist photos are not found: say so when few come back.",
        'Gives `counts` per source and stage, and `photo_sheets` (1–2 contact sheets of numbered tiles with the stage, place and year): look at them, pick the tiles that show the stage asked clearly (at most 5), then send_sheet_photos.',
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          taxon: { type: 'string' },
          life_stage: { anyOf: [{ type: 'string', enum: ['egg', 'larva', 'pupa', 'adult'] }, { type: 'array', items: { type: 'string', enum: ['egg', 'larva', 'pupa', 'adult'] } }] },
          area: AREA,
          near: region.near,
          lang: { type: 'string', enum: ['en', 'es'] },
        },
        required: ['taxon'],
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'send_sheet_photos',
      description: [
        'The photos chosen on contact sheets (host_plants or gallery_specimens with `photo_sheet`), ready to send: each downloaded at about 2048 px and credited (plant, in the area or candidate, place, year, photographer, licence, link; a specimen\'s CAM ID and data).',
        '- `sheets`: the sheet ids of one call; `tiles`: the numbers chosen, in the order to send: at most 5 in all for an answer (the WhatsApp limit). `lang`: "es" or "en" for the captions.',
        'Gives `images` [{path, caption, …}] and `images_block` (on WhatsApp, end the answer with it as it is). A tile whose photo or credit cannot be had is in `failed`: pick another of the same plant.',
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: { sheets: { type: 'array', items: { type: 'string' } }, tiles: { type: 'array', items: { type: 'integer' } }, lang: { type: 'string', enum: ['en', 'es'] } },
        required: ['sheets', 'tiles'],
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
        "Per site: `score` (each plant record within 10 km weighs (1 - (d/10)²)²: 1 at the site, 0.92 at 2 km, 0.56 at 5 km), records within 2, 5 and 10 km, the plants there, the nearest record (plant, km, year), the years of the records, the elevation, the project's records of the butterfly there. `map_url`: the Wings Atlas on the first site with the host-plant layer on.",
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
        '- `life_stage`: the collection photos are of adults; for eggs, larvae or pupae use taxon_photos.',
        '- `photo_sheet: true`: `photo_sheets`, contact sheets (3 by 3 numbered tiles) of the specimens\' photos to look at and pick from; then send_sheet_photos sends those chosen.',
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
          life_stage: { type: 'string', enum: ['egg', 'larva', 'pupa', 'adult'] },
          photo_sheet: { type: 'boolean' },
          thumbnails: { type: 'integer' },
        },
      },
    },
  },
  {
    type: 'function',
    function: {
      name: 'maps_snapshot',
      description: [
        'A picture of the Ithomiini maps (a JPEG of about 200 KB, small enough for WhatsApp) with some taxa, one sex if asked, framed on the best sites to find them; with the ranked sites (as maps_localities gives them) and `map_url`, the maps app on the same view and filters. For "where should we collect males of X and Y?" when a map helps.',
        '- `species`: as maps_localities\' `taxa` (species, subspecies or genus). `sex` (male, female). `area`, `country`, `sources`, `years`, `elevation_min`/`elevation_max`, `site_km` as there.',
        '- `bbox` [west, south, east, north] frames the picture (and limits the sites); left out ("auto"), it frames the best site and the next best within 150 km of it, up to `sites` (default 8); the best ones further away are in `elsewhere`. Give `country` to keep it to one country.',
        '- `lang`: "es" or "en" (default) for the words on the picture.',
        'Send `image` (a file on this server) as the picture and give `map_url` to open it live; the sites are numbered on the picture as their `on_map` (their `rank` is among all sites). `best_by_taxon`: each taxon\'s own best sites (of the sex asked), for a taxon the shared ranking puts low.',
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          species: { type: 'array', items: { type: 'string' } },
          sex: region.sex,
          area: AREA,
          bbox: { anyOf: [region.bbox, { type: 'string', enum: ['auto'] }] },
          country: region.country,
          sources: region.sources,
          years: region.years,
          elevation_min: { type: 'number' },
          elevation_max: { type: 'number' },
          site_km: { type: 'number' },
          sites: { type: 'integer' },
          lang: { type: 'string', enum: ['en', 'es'] },
        },
        required: ['species'],
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
 * `country`, `sex`, `map_view`; `hostPlants` {butterflies, taxa (slugs), levels, evidence}: the host-plant layer on
 * (`hp_bf`, `hp_taxa`, `hp_level`, `hp_evidence`; ithomiini_maps docs/host-plant-links.md).
 */
export function mapsUrl({ species = [], subspecies = [], genus = [], sources = SOURCE_KEYS, country = [], sex = null, view = null, hostPlants = null } = {}, base = MAPS_URL) {
  const params = new URLSearchParams();
  if (genus.length) params.set('genus', genus.join(','));
  if (species.length) params.set('sp', species.join(','));
  if (subspecies.length) params.set('ssp', subspecies.join(','));
  params.set('source', sources.map(k => SOURCES[k]).join(','));
  if (country.length) params.set('country', country.join(','));
  if (sex) params.set('sex', sex);
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

/**
 * A plant's tiles for a contact sheet: `k` of its photos (already by place), one per GBIF record before a second of
 * the same; then the nearest leaves-or-plant photo and the nearest flower (photo tags) are made sure of, in place of
 * the last tiles that are neither the nearest photo nor the one leaves photo and one flower kept.
 */
export function sheetPicks(sorted, k) {
  const records = new Set();
  const firstOfRecord = sorted.filter(ph => !ph.record || (!records.has(ph.record) && records.add(ph.record)));
  const list = [...firstOfRecord, ...sorted.filter(ph => !firstOfRecord.includes(ph))];
  const best = list.slice(0, k);
  const isLeaves = ph => ph.live === 0 && (ph.view === 'leaves' || ph.view === 'habit');
  const isFlower = ph => ph.live === 0 && ph.view === 'flower';
  for (const want of [isLeaves, isFlower]) {
    // The nearest such photo, whatever its record's other photos (the record order only orders the plain tiles).
    const extra = sorted.find(want);
    if (!extra || best.some(want) || best.length < k || k < 3) continue;
    const keptLeaves = best.find(isLeaves);
    const keptFlower = best.find(isFlower);
    const at = best.findLastIndex((ph, j) => j > 0 && ph !== keptLeaves && ph !== keptFlower);
    if (at > 0) best[at] = extra;
  }
  return best;
}

const viewOfBox = b => viewOf([{ lat: b[1], lng: b[0] }, { lat: b[3], lng: b[2] }]);
/** "Napo (province of Ecuador)", "Tena (canton of Napo, Ecuador)". */
const adminWhere = a => (a.level === 'province' ? `${a.name} (province of Ecuador)` : `${a.name} (canton of ${a.province}, Ecuador)`);
const adminOf = a => ({ level: a.level, name: a.name, province: a.province, cantons: a.cantons, bbox: a.bbox, outline: a.source, ...(a.also ? { also: a.also } : {}) });

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

/** "male" or "female" from the maps' and the workbook's ways of writing it, else null. */
export function sexOf(value) {
  const s = fold(value);
  if (/^(m|male|males|macho|♂)$/.test(s)) return 'male';
  if (/^(f|female|females|hembra|♀)$/.test(s)) return 'female';
  return null;
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
    sex: sexOf(p.sex),
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
  snapshotDir = null,
  snapshotOptions = {},
  cacheDir = null,
  sheetOptions = null,
  clutchPhotoDir = null,
  inatApi = INAT_API,
  gbifApi = GBIF_SEARCH,
} = {}) {
  const files = new Map();
  const dem = new Map();
  const strings = new Map();
  const intern = s => strings.get(s) ?? (strings.set(s, s), s);
  // Provinces and cantons (downloaded once into the cache folder), photos to send and their credits, plant records.
  const adminAreas = createAdminAreas({ dir: cacheDir ? join(cacheDir, 'admin') : null, fetchImpl });
  const photoFiles = createPlantPhotos({ dir: cacheDir ? join(cacheDir, 'plant-photos') : null, fetchImpl });
  const sheets = cacheDir ? createPhotoSheets({ dir: join(cacheDir, 'photo-sheets'), fetchImpl, ...(sheetOptions ?? {}) }) : null;
  // What each gallery photo shows (scripts/plant-photo-tags: leaves, habit, flower, fruit…), by iNaturalist photo
  // number, from the cache folder; read again when the file changes. Without it, photos are not told apart by view.
  let tagsKept = null;
  const photoTags = () => {
    if (!cacheDir) return null;
    const file = join(cacheDir, TAGS_FILE);
    try {
      const mtime = statSync(file).mtimeMs;
      if (tagsKept?.mtime !== mtime) {
        // A file that cannot be read is remembered as none until it changes.
        let map = null;
        try {
          map = new Map(Object.entries(JSON.parse(readFileSync(file, 'utf8')).photos ?? {}));
        } catch {
          /* unreadable */
        }
        tagsKept = { mtime, map };
      }
      return tagsKept.map;
    } catch {
      return null;
    }
  };
  /** The `area` asked (a province or canton of Ecuador), null without one, or { error }. */
  const areaOf = args => (args.area && (typeof args.area === 'object' || text(args.area)) ? adminAreas.area(args.area) : Promise.resolve(null));

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
    if (!db) return { rows: [], elevation: new Map(), sexes: new Map(), photos: new Map() };
    const state = db
      .prepare("SELECT count(*) n, max(updated_at) u FROM records WHERE sheet IN ('Collection_data','Photo_links') AND missing=0")
      .get();
    const stamp = `${state.n}:${state.u}`;
    if (workbook?.stamp === stamp) return workbook;
    const rows = [];
    const elevation = new Map();
    const sexes = new Map();
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
      const sex = sexOf(v.Sex);
      if (sex) sexes.set(cam, sex);
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
    workbook = { stamp, rows, elevation, sexes, photos };
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

  /** The records within the region (`admin`: the area asked, areaOf) and years asked. */
  function inRegion(records, args, admin = null) {
    const countries = new Set(list(args.country).map(fold));
    const bbox = Array.isArray(args.bbox) && args.bbox.length === 4 && args.bbox.every(Number.isFinite) ? args.bbox : null;
    const near = args.near && Number.isFinite(args.near.lat) && Number.isFinite(args.near.lng) ? { ...args.near, km: Number(args.near.km) || 25 } : null;
    const from = Number.isFinite(args.years?.from) ? args.years.from : null;
    const to = Number.isFinite(args.years?.to) ? args.years.to : null;
    return records.filter(r => {
      if (countries.size && !countries.has(fold(r.country))) return false;
      if (bbox && (r.lng < bbox[0] || r.lat < bbox[1] || r.lng > bbox[2] || r.lat > bbox[3])) return false;
      if (near && km(near, r) > near.km) return false;
      if (admin && !admin.test(r.lng, r.lat)) return false;
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

  /** A record's sex: the maps', else the workbook's for the project's records, else null. */
  const sexOfRecord = (r, sexes) => r.sex ?? (r.source === 'sanger' ? sexes.get(r.id.toUpperCase()) ?? null : null);
  const sexAsked = args => sexOf(args.sex);

  /** `inner`, when given, gets what maps_snapshot draws: the records shown, the link's taxa and sources. */
  async function localities(args, inner = null) {
    const asked = list(args.taxa).slice(0, 20);
    if (!asked.length) return { error: 'Name at least one taxon in `taxa`.' };
    const sources = sourcesOf(args);
    const [admin, { records, freshness }] = await Promise.all([areaOf(args), points(sources)]);
    if (admin?.error) return admin;
    const taxa = await taxaOf(asked, records);
    if (!taxa.species.length && !taxa.genus.length)
      return { error: `The maps have no records of ${asked.join(', ')}.`, unknown: taxa.unknown, freshness };
    const ofTaxa = records.filter(taxa.match);
    const regional = inRegion(ofTaxa, args, admin);
    const { kept, elev: fromBand, unknown, unknownPoints } = await byElevation(regional, args);
    // Without an elevation asked: the workbook's for the project's records (no DEM look-ups).
    const own = collection().elevation;
    const elev = fromBand ?? new Map(kept.filter(r => r.source === 'sanger' && own.has(r.id.toUpperCase())).map(r => [r, { m: own.get(r.id.toUpperCase()), from: 'workbook' }]));
    const { sexes } = collection();
    const sex = sexAsked(args);
    const sexOfKept = new Map(kept.map(r => [r, sexOfRecord(r, sexes)]));
    const siteKm = Math.min(50, Math.max(0.5, Number(args.site_km) || 3));
    const deg = siteKm / 111;
    const sites = new Map();
    const grid = new Map();
    for (const r of kept) {
      // Dore et al. cells have no sex: left out when one is asked, as the maps app's sex filter does.
      if (r.source === 'dore' && sex) continue;
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
        sites.set(key, { n: 0, lat: 0, lng: 0, names: new Map(), countries: new Map(), sources: new Map(), years: new Set(), elev: [], elevFrom: new Set(), taxa: new Map(), sex: { male: 0, female: 0, unknown: 0 }, own: { male: 0, female: 0, unknown: 0 } }).get(key);
      s.n++;
      const rs = sexOfKept.get(r) ?? 'unknown';
      s.sex[rs]++;
      if (r.source === 'sanger') s.own[rs]++;
      s.lat += r.lat;
      s.lng += r.lng;
      if (r.place) count(s.names, r.place);
      if (r.country) count(s.countries, r.country);
      count(s.sources, SOURCES[r.source] ?? r.source);
      if (!sex || rs === sex) count(s.taxa, r.name);
      if (r.year) s.years.add(r.year);
      const e = elev?.get(r);
      if (e) {
        s.elev.push(e.m);
        s.elevFrom.add(e.from);
      }
    }
    const limit = clamp(args.limit, 1, 50, 15);
    // With a sex asked: the sites with records of it, by those records, then all records and years.
    const ranked = [...sites.values()]
      .filter(s => !sex || s.sex[sex] > 0)
      .map(s => ({ ...s, lat: s.lat / s.n, lng: s.lng / s.n }))
      .sort((a, b) => (sex ? b.sex[sex] - a.sex[sex] : 0) || b.n - a.n || b.years.size - a.years.size);
    // The app ANDs its genus and species filters: a genus asked with species is given as its species.
    const mixed = taxa.genus.length && taxa.species.length;
    const urlTaxa = {
      species: mixed ? [...new Set([...taxa.species, ...ofTaxa.filter(r => taxa.genus.includes(r.genus)).map(r => r.name)])] : taxa.species,
      // A subspecies filter would hide the rest of a genus asked.
      subspecies: taxa.genus.length ? [] : taxa.subspecies,
      genus: mixed ? [] : taxa.genus,
      sources,
      sex,
    };
    if (inner) Object.assign(inner, { records: sex ? kept.filter(r => sexOfKept.get(r) === sex) : kept, urlTaxa, taxa, countries: countriesIn(admin ? [...list(args.country), 'Ecuador'] : args.country, records), ranked });
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
        by_sex: s.sex,
        ...(s.own.male + s.own.female + s.own.unknown ? { own_by_sex: s.own } : {}),
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
        ...(sex ? { [sex]: [...sexOfKept.values()].filter(v => v === sex).length, sex_unknown: [...sexOfKept.values()].filter(v => !v).length, sex_note: `Ranked by ${sex} records; records without a sex (most of iNaturalist's, all of Dore et al.'s) are in each site's by_sex but not on the map's ${sex} filter.` } : {}),
        ...(hasElevation(args) ? { elevation_unknown: unknown, ...(unknownPoints ? { note: `${unknownPoints} points had no DEM elevation (over the ${DEM_POINTS}-point budget or not answered): left out` } : {}) } : {}),
      },
      sites_total: ranked.length,
      sites: shown,
      ...(cells.length ? { grid_cells: { note: 'Dore et al. (2022): 0.25° cells, not collecting sites', cells: cells.length, top: cells.slice(0, 5).map(c => ({ lat: c.lat, lng: c.lng, country: c.country, records: c.records })) } } : {}),
      map: mapsUrl({ ...urlTaxa, country: countriesIn(admin ? [...list(args.country), 'Ecuador'] : args.country, records), view }, mapsBase),
      freshness,
    };
  }

  async function species(args) {
    if (!list(args.country).length && !Array.isArray(args.bbox) && !args.near && !(args.area && (typeof args.area === 'object' || text(args.area)))) return { error: 'Give a region: `area`, `country`, `bbox` or `near`.' };
    const sources = sourcesOf(args);
    const [admin, { records, freshness }] = await Promise.all([areaOf(args), points(sources)]);
    if (admin?.error) return admin;
    const genera = new Set(list(args.genus).map(fold));
    const regional = inRegion(genera.size ? records.filter(r => genera.has(fold(r.genus))) : records, args, admin);
    const { kept: banded, unknown, unknownPoints } = await byElevation(regional, args);
    const sex = sexAsked(args);
    const { sexes } = collection();
    const kept = sex ? banded.filter(r => sexOfRecord(r, sexes) === sex) : banded;
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
    const country = countriesIn(admin ? [...list(args.country), 'Ecuador'] : args.country, records);
    const ranked = [...byName].sort((a, b) => b[1].records - a[1].records);
    const view = args.near
      ? { lat: args.near.lat, lng: args.near.lng, zoom: Math.max(5, Math.min(12, Math.round(Math.log2(40000 / (Number(args.near.km) || 25))) - 1)) }
      : Array.isArray(args.bbox) && args.bbox.length === 4
        ? viewOf([{ lat: args.bbox[1], lng: args.bbox[0] }, { lat: args.bbox[3], lng: args.bbox[2] }])
        : admin
          ? viewOfBox(admin.bbox)
          : viewOf(kept);
    return {
      records: { in_region: regional.length, kept: kept.length, ...(sex ? { sex, sex_note: `Only records with ${sex} as their sex (most of iNaturalist's and all of Dore et al.'s have none).` } : {}), ...(hasElevation(args) ? { elevation_unknown: unknown, ...(unknownPoints ? { note: `${unknownPoints} points had no DEM elevation: left out` } : {}) } : {}) },
      taxa_total: ranked.length,
      ...(unidentified ? { unidentified_records: unidentified } : {}),
      taxa: ranked.slice(0, limit).map(([name, t]) => ({ name, records: t.records, sites: t.sites.size, by_source: Object.fromEntries(t.sources), last_year: t.last, map: mapsUrl({ species: [name], sources, country, sex, view }, mapsBase) })),
      map: mapsUrl({ species: ranked.slice(0, 20).map(([n]) => n), sources, country, sex, view }, mapsBase),
      map_note: 'The region\'s map holds the first 20 taxa; each taxon has its own link.',
      freshness,
    };
  }

  const snapshots = snapshotDir ? createSnapshots({ dir: snapshotDir, fetchImpl, ...snapshotOptions }) : null;
  const WORDS = {
    en: { male: 'Males', female: 'Females', null: 'Records', title: 'best sites', sites: n => (n > 1 ? `1–${n}: best sites here, in order` : '1: the best site'), count: (n, sex) => `${n.toLocaleString('en')} ${sex ?? 'record'}${n === 1 ? '' : 's'}`, other: 'other taxa' },
    es: { male: 'Machos', female: 'Hembras', null: 'Registros', title: 'mejores sitios', sites: n => (n > 1 ? `1–${n}: mejores sitios aquí, en orden` : '1: el mejor sitio'), count: (n, sex) => `${n.toLocaleString('es')} ${sex === 'male' ? 'macho' : sex === 'female' ? 'hembra' : 'registro'}${n === 1 ? '' : 's'}`, other: 'otros taxones' },
  };

  /** A picture of the maps with the taxa asked (one sex too), framed on the best sites, and the link to the same view. */
  async function snapshot(args) {
    if (!snapshots) return { error: 'Map pictures are off on this server (no folder for them is set).' };
    const started = Date.now();
    const bbox = Array.isArray(args.bbox) && args.bbox.length === 4 && args.bbox.every(Number.isFinite) ? args.bbox : null;
    const wanted = clamp(args.sites, 1, 15, 8);
    const inner = {};
    const found = await localities({ ...args, taxa: args.species ?? args.taxa, bbox, limit: 50 }, inner);
    if (found.error) return found;
    const dataMs = Date.now() - started;
    // Auto: the best site and the next best near it, so the picture is a place to go to, not a continent.
    const near = s => km(found.sites[0], s) <= CLUSTER_KM;
    // Numbered on the picture in this order (`on_map`); `rank` stays the site's place among all.
    const best = (bbox ? found.sites : found.sites.filter(near)).slice(0, wanted).map((s, i) => ({ on_map: i + 1, ...s }));
    const elsewhere = bbox ? [] : found.sites.filter(s => !near(s)).slice(0, 5);
    let frame = bbox;
    if (!frame) {
      const at = best.length ? best : inner.records;
      if (!at.length) return { ...found, error: 'None of these records can go on a map.' };
      const lats = at.map(p => p.lat);
      const lngs = at.map(p => p.lng);
      const [w, s, e, n] = [Math.min(...lngs), Math.min(...lats), Math.max(...lngs), Math.max(...lats)];
      const padX = Math.max(0.04, (e - w) * 0.12);
      const padY = Math.max(0.03, (n - s) * 0.12);
      frame = [w - padX, s - padY, e + padX, n + padY];
    }
    // Colours by taxon, the commonest in the frame first; past the palette, grey.
    const shownIn = r => r.lng >= frame[0] && r.lng <= frame[2] && r.lat >= frame[1] && r.lat <= frame[3];
    const counts = new Map();
    for (const r of inner.records) if (shownIn(r)) count(counts, r.name);
    const order = [...counts].sort((a, b) => b[1] - a[1]);
    const colour = new Map(order.slice(0, PALETTE.length - 1).map(([name], i) => [name, i]));
    const grey = PALETTE.length - 1;
    const lang = WORDS[args.lang] ? args.lang : 'en';
    const words = WORDS[lang];
    const sex = inner.urlTaxa.sex;
    const legend = order.slice(0, PALETTE.length - 1).map(([name, n]) => ({ text: `${name} · ${words.count(n, sex)}`, color: colour.get(name) }));
    if (order.length > PALETTE.length - 1)
      legend.push({ text: `${words.other} · ${words.count(order.slice(PALETTE.length - 1).reduce((t, [, n]) => t + n, 0), sex)}`, color: grey });
    if (best.length) legend.push({ text: words.sites(best.length), color: null });
    const picture = await snapshots.render({
      bbox: frame,
      // Grey first, then the commonest taxon to the rarest, so the rarer ones are on top.
      points: inner.records
        .map(r => ({ lat: r.lat, lng: r.lng, color: colour.get(r.name) ?? grey }))
        .sort((a, b) => (a.color === grey ? -1 : a.color) - (b.color === grey ? -1 : b.color)),
      sites: best.map(s => ({ lat: s.lat, lng: s.lng, rank: s.on_map })),
      legend,
      title: `${words[sex]} · ${words.title}`,
    });
    return {
      image: picture.path,
      image_bytes: picture.bytes,
      image_size: `${picture.width}x${picture.height}`,
      map_url: mapsUrl({ ...inner.urlTaxa, country: inner.countries, view: picture.view }, mapsBase),
      framed: { bbox: picture.bbox, by: bbox ? 'bbox asked' : `the first ${best.length || 'records'} sites` },
      taxa: found.taxa,
      records: { ...found.records, on_picture: picture.points_drawn },
      sites_total: found.sites_total,
      sites: best,
      ...(elsewhere.length ? { elsewhere: elsewhere.map(({ rank, site, country, lat, lng, records, by_sex, map }) => ({ rank, site, country, lat, lng, records, by_sex, map })) } : {}),
      best_by_taxon: Object.fromEntries(
        found.taxa.species.map(name => [
          name,
          inner.ranked
            .map((s, i) => ({ rank: i + 1, site: top(s.names) ?? `${s.lat.toFixed(3)}, ${s.lng.toFixed(3)}`, country: top(s.countries), [sex ?? 'records']: s.taxa.get(name) ?? 0 }))
            .filter(s => s[sex ?? 'records'] > 0)
            .sort((a, b) => b[sex ?? 'records'] - a[sex ?? 'records'] || a.rank - b.rank)
            .slice(0, 3),
        ]),
      ),
      freshness: found.freshness,
      timing_ms: { data: dataMs, tiles: picture.ms.tiles, picture: picture.ms.total - picture.ms.tiles, total: Date.now() - started, tiles_fetched: picture.tiles.fetched, tiles_failed: picture.tiles.failed },
    };
  }


  // Host plants in an area: the maps' plant layers (data/host_plants/, ithomiini_maps docs/host-plant-links.md).
  // Each taxon's refs file names the records it has in the 24 occurrence chunks; only the chunks a call needs are
  // read, each kept as typed arrays (about 0.4 MB for its 20,000 records).
  const HP = 'host_plants/';
  // The plant files read for one version of the layers (the manifest's generated_at): a refs file indexes into its
  // own download's chunks, so after the maps republish, every plant file is read again and the old ones dropped.
  let plantVersion = null;
  const versioned = path => `${HP}${path}${plantVersion ? `?v=${plantVersion}` : ''}`;
  function usePlantVersion(generatedAt) {
    const v = encodeURIComponent(text(generatedAt) || 'none');
    if (v === plantVersion) return;
    for (const key of files.keys()) if (key.startsWith(HP) && key.includes('?v=')) files.delete(key);
    for (const key of plantPoints.keys()) if (!key.startsWith(`${v}|`)) plantPoints.delete(key);
    plantVersion = v;
  }
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
  const plantRefs = path => remote(versioned(path), json => ({ total: json?.metadata?.total_records ?? null, chunks: Object.entries(json?.chunks ?? {}) }));
  const plantChunk = path =>
    remote(versioned(path), json => {
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
  const galleryIndex = path => remote(versioned(path), json => ({ total: json?.total_images ?? 0, pages: json?.pages ?? [] }));
  const galleryPage = path =>
    remote(versioned(path), json =>
      (json?.items ?? [])
        .filter(p => /^https:\/\//.test(text(p?.image_url)))
        .map(p => ({
          img: text(p.image_url),
          record: text(p.observation_url) || null,
          lat: Number(p.lat),
          lng: Number(p.lng),
          year: yearOf(p.observation_date),
          field: p.photo_context === 'field',
          // A pressed museum specimen: its record says so, or the gallery's context.
          herbarium: /preserved|fossil|material/i.test(text(p.basisOfRecord)) || p.photo_context === 'preserved',
          // A record without a photo shown with another record's photo of the plant: its place and credit are not the photo's.
          fallback: p.image_is_fallback === true,
          source: intern(text(p.source)),
        })),
    );

  /** The area asked of the plant records: { test(lng, lat), countries, unknown, bbox, near }, null for none, or { error }. */
  async function plantArea(args) {
    const asked = list(args.country);
    const bbox = Array.isArray(args.bbox) && args.bbox.length === 4 && args.bbox.every(Number.isFinite) ? args.bbox : null;
    const near = args.near && Number.isFinite(args.near.lat) && Number.isFinite(args.near.lng) ? { lat: args.near.lat, lng: args.near.lng, km: Number(args.near.km) || 25 } : null;
    const admin = await areaOf(args);
    if (admin?.error) return admin;
    if (!asked.length && !bbox && !near && !admin) return null;
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
      if (admin && !admin.test(lng, lat)) return false;
      if (!wanted.size) return true;
      // Cheap reject first: outside every asked country's box (with the coast margin).
      return boxes.some(b => inBox(lng, lat, b, COAST_TOLERANCE_DEG, lngPadAt(lat))) && wanted.has(countryOfPoint(lng, lat, o));
    };
    // How far a point outside is (km; Infinity past NEAREST_MAX_KM): from a province's or canton's edge, a box, a
    // circle's rim. A lower bound first (the distance to the area's box), so most points cost one haversine.
    const box = admin?.bbox ?? bbox;
    const toBox = (lng, lat) => (box ? km({ lat: Math.min(box[3], Math.max(box[1], lat)), lng: Math.min(box[2], Math.max(box[0], lng)) }, { lat, lng }) : 0);
    // One kind of area only: with two (a province and a box), "how far" has no single answer.
    // Ecuador asked beside a province or canton adds nothing: not a second kind of area.
    const countryKind = countries.length && !(admin && countries.every(c => c === 'Ecuador'));
    const distance = [admin, bbox, near, countryKind].filter(Boolean).length !== 1
      ? null
      : admin
      ? (lng, lat) => (toBox(lng, lat) > NEAREST_MAX_KM ? Infinity : admin.distanceKm(lng, lat))
      : near && !bbox && !countries.length
        ? (lng, lat) => Math.max(0, km(near, { lat, lng }) - near.km)
        : bbox && !near && !countries.length
          ? toBox
          : null;
    // Where a point outside is: its province (Ecuador), else its country.
    const adminData = admin ? await adminAreas.data() : null;
    const world = distance ? (o ?? (await outlines().then(r => r.value).catch(() => null))) : null;
    const regionOf = (lng, lat) => cantonOf(lng, lat, adminData)?.province ?? (world ? countryOfPoint(lng, lat, world) : null);
    // A province or canton is in Ecuador: the map link's country.
    return { test, distance, regionOf, countries: admin && !countries.length ? ['Ecuador'] : countries, unknown, bbox, near, admin, outlines: o };
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
   * The host plants of `kept` (ranked associations) with records layers and their records (pointsOf):
   * { m, modified, shown, mapped, withRecords, withoutRecords, points, from }. `mapped`: the plants that make sites,
   * photos and the map (species-level hosts; genus-level ones only when there are none, or that genus was asked).
   */
  async function hostPlantLayers(kept, args) {
    const { value: m, modified } = await plantLayers();
    usePlantVersion(m.generated_at);
    const { shown, mapped, withRecords, withoutRecords } = hostPlantList(kept, args, m);
    const { points, from } = await pointsOf(m, shown.map(e => e.slug));
    return { m, modified, shown, mapped, withRecords, withoutRecords, points, from };
  }

  // Each plant's records as small lists { total, lat, lng, year, basis, key } (key: the record's place in the
  // download, shared by synonymous plants), per layers version, in memory and on disk: a new worker or a restart
  // reads them there instead of the 7.5 MB chunks.
  const plantPoints = new Map();
  async function pointsOf(m, slugs) {
    const out = new Map();
    const missing = [];
    let from = 'memory';
    for (const slug of slugs) {
      const kept = plantPoints.get(`${plantVersion}|${slug}`);
      if (kept) out.set(slug, kept);
      else missing.push(slug);
    }
    const dir = cacheDir ? join(cacheDir, 'plant-records', plantVersion ?? 'none') : null;
    const fromChunks = [];
    for (const slug of missing) {
      try {
        if (!dir) throw new Error('no folder');
        const kept = JSON.parse(readFileSync(join(dir, `${slug.replace(/[^\w.-]/g, '_')}.json`), 'utf8'));
        plantPoints.set(`${plantVersion}|${slug}`, kept);
        out.set(slug, kept);
        from = 'disk';
      } catch {
        fromChunks.push(slug);
      }
    }
    if (fromChunks.length) {
      from = 'download';
      const refs = await Promise.all(fromChunks.map(slug => plantRefs(m.refs[slug]).then(r => r.value)));
      const lists = refs.map(r => ({ total: r.total, lat: [], lng: [], year: [], basis: [], key: [] }));
      const wanted = [...new Set(refs.flatMap(r => r.chunks.map(([c]) => c)))];
      // Two chunks at a time (7.5 MB of JSON each): each made small before the next ones.
      for (let i = 0; i < wanted.length; i += CHUNKS_AT_ONCE)
        await Promise.all(
          wanted.slice(i, i + CHUNKS_AT_ONCE).map(async c => {
            const chunk = (await plantChunk(c)).value;
            const tag = c.replace(/^.*\/|\.json$/g, '');
            refs.forEach((r, k) => {
              for (const [name, indexes] of r.chunks) {
                if (name !== c) continue;
                for (const j of indexes ?? []) {
                  if (!Number.isFinite(chunk.lat[j]) || !Number.isFinite(chunk.lng[j])) continue;
                  const l = lists[k];
                  l.lat.push(round(chunk.lat[j], 5));
                  l.lng.push(round(chunk.lng[j], 5));
                  l.year.push(chunk.year[j] || 0);
                  l.basis.push(chunk.basis[j] || 'unknown');
                  l.key.push(`${tag}:${j}`);
                }
              }
            });
          }),
        );
      fromChunks.forEach((slug, k) => {
        plantPoints.set(`${plantVersion}|${slug}`, lists[k]);
        out.set(slug, lists[k]);
        if (dir) writeCacheFile(join(dir, `${slug.replace(/[^\w.-]/g, '_')}.json`), JSON.stringify(lists[k]));
      });
    }
    return { points: out, from };
  }

  /** The host plants of `kept` with a records layer in `m`, ranked as the associations: { shown, mapped, withRecords, withoutRecords }. */
  function hostPlantList(kept, args, m) {
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
    return { shown, mapped, withRecords, withoutRecords };
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
    const started = Date.now();
    // The places that name the sites come from the maps' butterfly records (the largest files, read one at a time):
    // started now, so they load while the plants are counted and the photos fetched.
    const namedLoading = namedPlaces().catch(() => () => null);
    const { m, modified, shown, mapped, withRecords, withoutRecords, points: recs, from } = await hostPlantLayers(kept, args);
    const recordsMs = Date.now() - started;
    const siteKm = Math.min(50, Math.max(0.5, Number(args.site_km) || 5));
    // Records outside the area but within `nearby_km` of it (a province, canton, box or circle; not a country).
    const nearbyKm = area?.distance ? clamp(args.nearby_km, 0, 200, NEARBY_KM) : 0;
    const points = new Map();
    const plants = shown.map(e => {
      const r = recs.get(e.slug) ?? { total: null, lat: [], lng: [], year: [], basis: [], key: [] };
      const own = [];
      const years = new Set();
      const basis = new Map();
      const near = { records: 0, years: new Set(), regions: new Map(), nearest: null };
      for (let j = 0; j < r.lat.length; j++) {
        const [lat, lng] = [r.lat[j], r.lng[j]];
        if (area && !area.test(lng, lat)) {
          if (!area.distance) continue;
          const d = area.distance(lng, lat);
          if (d < (near.nearest?.d ?? Infinity)) near.nearest = { d, year: r.year[j] || null, lat, lng };
          if (d <= nearbyKm) {
            near.records++;
            if (r.year[j]) near.years.add(r.year[j]);
            count(near.regions, area.regionOf(lng, lat) ?? 'unknown');
          }
          continue;
        }
        const p = points.get(r.key[j]) ?? { lat, lng, year: r.year[j] || null, plants: new Set() };
        own.push(p);
        if (mapped.includes(e)) {
          p.plants.add(e.taxon.name);
          points.set(r.key[j], p);
        }
        if (p.year) years.add(p.year);
        count(basis, r.basis[j] || 'unknown');
      }
      const status = own.length ? 'in_area' : near.records ? 'nearby' : 'far';
      // Two plants with the same GBIF records (a synonym the review kept apart): said, and one photo for both.
      const sig = r.key.length ? `${r.key.length}|${r.key[0]}|${r.key.at(-1)}` : null;
      return { e, own, years, basis, near, total: r.total ?? r.lat.length, status, sig };
    });
    const firstOf = new Map();
    for (const p of plants) if (p.sig && mapped.includes(p.e)) (firstOf.get(p.sig) ? (p.sameAs = firstOf.get(p.sig).e.taxon.name) : firstOf.set(p.sig, p));
    for (const p of plants) if (p.sameAs) (firstOf.get(p.sig).same ??= []).push(p.e.taxon.name);
    // The photo files, fetched while the thumbnails and site names are made.
    // Photos to send are chosen by looking: `send_photos` gives the contact sheets (as `photo_sheet`), so a chat
    // that asks for photos the older way still sees them before any is sent. `auto_pick` (tests, scripts) picks
    // without looking. The WhatsApp bridge sends at most 5 pictures with an answer.
    const wantsPhotos = clamp(args.send_photos, 0, 5, 0);
    const sendPhotos = args.auto_pick === true ? wantsPhotos : 0;
    const asSheet = args.photo_sheet === true || (wantsPhotos > 0 && args.auto_pick !== true);
    const photosStarted = Date.now();
    const sheetLoading = asSheet ? plantSheets(plants.filter(p => mapped.includes(p.e) && !p.sameAs), area, nearbyKm, m, args.lang === 'es' ? 'es' : 'en').catch(e => ({ photo_sheet_error: e.message })) : Promise.resolve(null);
    const filesLoading = sendPhotos
      ? photosToSend(plants.filter(p => mapped.includes(p.e) && !p.sameAs), sendPhotos, area, nearbyKm, m, args.lang === 'es' ? 'es' : 'en').then(f => ({ ...f, ms: Date.now() - photosStarted }))
      : Promise.resolve(null);
    const sitesAll = plantSites([...points.values()], siteKm);
    const limit = clamp(args.limit, 1, 15, 8);
    const urlTaxa = plantUrlTaxa(shown, mapped, asked, area?.countries ?? []);
    const view = area?.near
      ? { lat: area.near.lat, lng: area.near.lng, zoom: Math.max(5, Math.min(12, Math.round(Math.log2(40000 / area.near.km)) - 1)) }
      : area?.admin
        ? viewOfBox(area.admin.bbox)
        : sitesAll.length
        ? viewOf(sitesAll)
        : area?.bbox
          ? viewOf([{ lat: area.bbox[1], lng: area.bbox[0] }, { lat: area.bbox[3], lng: area.bbox[2] }])
          : null;
    const where = area ? (area.admin ? adminWhere(area.admin) : area.countries.length ? area.countries.join(', ') : area.near ? `${area.near.km} km around ${area.near.lat}, ${area.near.lng}` : 'the box asked') : 'their whole range';
    // With photo files asked, no thumbnails unless asked too: a small answer.
    const want = clamp(args.photos, 0, 6, sendPhotos || asSheet ? 0 : 3);
    // A photo page that cannot be read now leaves that plant without photos, not the answer without its records.
    let photoFailed = false;
    const photos = await Promise.all(
      plants.map(({ e, own }) =>
        mapped.includes(e) && own.length
          ? plantPhotos(m.galleries[e.slug], area, want).catch(() => {
              photoFailed = true;
              return [];
            })
          : [],
      ),
    );
    const placeOf = p => (area?.outlines ? countryOfPoint(p.lng, p.lat, area.outlines) : null) ?? `${p.lat.toFixed(2)}, ${p.lng.toFixed(2)}`;
    const blocks = plants
      .map(({ e, own, years }, i) => {
        if (!photos[i].length) return null;
        const y = yearsOf(years);
        const gbif = e.taxon.gbif_name && e.taxon.gbif_name !== e.taxon.name ? ` (GBIF: ${e.taxon.gbif_name})` : '';
        const head = `**${e.taxon.name}**${gbif}: ${own.length} record${own.length === 1 ? '' : 's'} in ${where}${y ? `, ${y.first}–${y.last}` : ''}`;
        const thumbs = photos[i].map(p => `[![${e.taxon.name}${p.year ? ` ${p.year}` : ''}${p.away ? `, ${placeOf(p)}` : ''}](${thumbOf(p.img)})](${p.img})`).join(' ');
        const credit = `Photos (GBIF records): ${photos[i].map((p, k) => (p.record ? `[${k + 1}](${p.record})` : `${k + 1}`)).join(' ')}; from ${[...new Set(photos[i].map(p => p.source))].join('; ')}. Licence and author on each record's page.${photos[i].some(p => p.away) ? ` Some photos are from outside ${where} (their place in the caption).` : ''}`;
        return { plant: e.taxon.name, text: [head, thumbs, credit].join('  \n') };
      })
      .filter(Boolean);
    const [files, named, sheet] = await Promise.all([filesLoading, namedLoading, sheetLoading]);
    const shownBlocks = [];
    for (const b of blocks) if (shownBlocks.reduce((n, x) => n + x.text.length + 2, 0) + b.text.length <= MARKDOWN_CHARS) shownBlocks.push(b);
    const markdown = shownBlocks.map(b => b.text).join('\n\n');
    const photosLeftOut = blocks.filter(b => !shownBlocks.includes(b)).map(b => b.plant);
    return {
      area: {
        where,
        ...(area?.unknown.length ? { countries_not_known: area.unknown } : {}),
        ...(area?.admin ? { admin: adminOf(area.admin) } : {}),
        ...(area?.bbox ? { bbox: area.bbox, bbox_note: 'The map link frames the box; the maps app limits plant records by country, not by this box.' } : {}),
      },
      plants: plants.map(({ e, own, years, basis }, i) => ({
        plant: e.taxon.name,
        ...(e.taxon.gbif_name && e.taxon.gbif_name !== e.taxon.name ? { gbif_name: e.taxon.gbif_name } : {}),
        rank: e.taxon.rank,
        host_of: [...e.butterflies],
        evidence: [...e.evidence].join(', '),
        records: own.length,
        records_total: plants[i].total,
        ...(plants[i].sameAs ? { same_records_as: plants[i].sameAs } : plants[i].same ? { same_records_as: plants[i].same.join(', ') } : {}),
        ...(area?.distance
          ? {
              status: plants[i].status,
              nearby: {
                within_km: nearbyKm,
                records: plants[i].near.records,
                years: yearsOf(plants[i].near.years),
                by_region: Object.fromEntries([...plants[i].near.regions].sort((x, y) => y[1] - x[1]).slice(0, 5)),
                nearest: plants[i].near.nearest
                  ? { km: Math.round(plants[i].near.nearest.d * 10) / 10, year: plants[i].near.nearest.year, region: area.regionOf(plants[i].near.nearest.lng, plants[i].near.nearest.lat) }
                  : plants[i].own.length
                    ? null
                    : `none within ${NEAREST_MAX_KM} km`,
              },
            }
          : {}),
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
        ...(i < SITE_LINKS ? { map: mapsUrl({ ...urlTaxa, view: { lat: s.lat, lng: s.lng, zoom: siteKm <= 3 ? 12 : 11 } }, mapsBase) } : {}),
      })),
      map_url: mapsUrl({ ...urlTaxa, view }, mapsBase),
      ...(area?.distance
        ? {
            in_area: plants.filter(p => p.status === 'in_area' && mapped.includes(p.e) && !p.sameAs).map(p => `${p.e.taxon.name}${p.same ? ` (= ${p.same.join(', ')})` : ''} (${p.own.length})`),
            nearby_only: plants.filter(p => p.status === 'nearby' && mapped.includes(p.e) && !p.sameAs).map(p => `${p.e.taxon.name}${p.same ? ` (= ${p.same.join(', ')})` : ''} (${p.near.records} within ${nearbyKm} km)`),
            not_near: plants.filter(p => p.status === 'far' && mapped.includes(p.e) && !p.sameAs).map(p => p.e.taxon.name),
          }
        : {}),
      ...(sheet ?? {}),
      ...(files
        ? {
            images: files.images,
            // The WhatsApp bridge's picture block (tools/whatsapp/bridge-skill): one line per photo, `path | caption`.
            images_block: imagesBlock(files.images),
            ...(files.note ? { images_note: files.note } : {}),
          }
        : {}),
      markdown,
      ...(photosLeftOut.length ? { photos_left_out: { plants: photosLeftOut, note: 'Not in `markdown` (its size): ask again with `plant` for these.' } } : {}),
      ...(photoFailed ? { photos_note: 'Some photo pages of the maps could not be read now: those plants have no photos in this answer.' } : {}),
      records_note: `Plant records: the maps' GBIF occurrences of each plant (generated ${text(m.generated_at).slice(0, 10) || 'date unknown'}), placed in a country by their coordinates (Natural Earth 1:50m outlines: a record within a few km of a border can fall on the wrong side). They show where the plant has been collected or photographed, not that the butterfly uses it there. ${m.caveat ?? ''}`.trim(),
      plant_data: { manifest: new URL(`data/${HP}host_plant_layers_manifest.json`, mapsBase).href, modified: modified ?? null, records_from: from },
      timing_ms: { records: recordsMs, ...(files ? { photos: files.ms } : {}), area_total: Date.now() - started },
    };
  }

  /**
   * Each plant's gallery photos (its first SEND_PAGES pages, no fallback items), ranked: a living plant before
   * anything else, a herbarium sheet last; within that, leaves or the whole plant (photo tags), untagged, then flowers
   * and fruits; then in the area, within `nearbyKm`, nearer, newer. Each photo with inside, away, live, view.
   */
  async function rankedFor(picked, area, nearbyKm, m) {
    const tags = photoTags();
    const galleries = await Promise.all(
      picked.map(async p => {
        const path = m.galleries[p.e.slug];
        if (!path) return [];
        const index = (await galleryIndex(path)).value;
        const pages = await Promise.all(index.pages.slice(0, SEND_PAGES).map(page => galleryPage(page).then(r => r.value)));
        return pages.flat().filter(ph => !ph.fallback && Number.isFinite(ph.lat) && Number.isFinite(ph.lng));
      }).map(x => x.catch(() => [])),
    );
    const viewOf = ph => {
      const t = tags?.get(/inaturalist[^/]*\/photos\/(\d+)\//.exec(ph.img)?.[1] ?? '');
      // Leaves or the whole plant only over the calibrated threshold; under it, a leaves/habit guess is no view.
      if (!t) return null;
      if (t[1] >= TAG_LEAVES) return t[0] === 'habit' ? 'habit' : 'leaves';
      return t[0] === 'leaves' || t[0] === 'habit' ? null : t[0];
    };
    const viewRank = ph => (ph.view === 'leaves' || ph.view === 'habit' ? 0 : ph.view === null ? 1 : 2);
    // Past the measured reach, distance to the area's middle orders the far photos.
    const box = area?.admin?.bbox ?? area?.bbox ?? null;
    const middle = box ? { lat: (box[1] + box[3]) / 2, lng: (box[0] + box[2]) / 2 } : area?.near ?? null;
    return galleries.map(gallery =>
      gallery
        .map(ph => {
          const inside = !area || area.test(ph.lng, ph.lat);
          const measured = inside ? 0 : area.distance ? area.distance(ph.lng, ph.lat) : Infinity;
          const away = Number.isFinite(measured) ? measured : middle ? NEAREST_MAX_KM + km(middle, ph) : Infinity;
          const live = ph.field && !ph.herbarium ? 0 : !ph.herbarium ? 1 : 2;
          return { ...ph, inside, away, live, view: viewOf(ph), loc: inside ? 0 : away <= nearbyKm ? 1 : 2 };
        })
        .sort((a, b) => a.live - b.live || viewRank(a) - viewRank(b) || a.loc - b.loc || a.away - b.away || (b.year ?? 0) - (a.year ?? 0)),
    );
  }

  /**
   * Photo files to send (WhatsApp), at most `n`: first one per plant (the plants recorded in the area, then those
   * only nearby, most nearby records first), then a flower of the first plants as a second photo. A plant's photos
   * from its first SEND_PAGES gallery pages are ranked: a living plant before anything else, a herbarium sheet last;
   * within that, leaves or the whole plant (photo tags), untagged, then flowers and fruits; then in the area, within
   * `nearbyKm`, nearer, newer. A photo goes only with its own credit, else the next; downloaded and credited together
   * (server/plant-photos.mjs). -> { images, note }
   */
  async function photosToSend(plants, n, area, nearbyKm, m, lang) {
    const words = SEND_WORDS[lang];
    const short = area?.admin?.name ?? (area?.countries.length ? area.countries.join(', ') : words.area);
    const picked = [...plants.filter(p => p.status === 'in_area'), ...plants.filter(p => p.status === 'nearby').sort((a, b) => b.near.records - a.near.records)].slice(0, n);
    const ranked = await rankedFor(picked, area, nearbyKm, m);
    const used = new Set();
    let failed = 0;
    const viewRank = ph => (ph.view === 'leaves' || ph.view === 'habit' ? 0 : ph.view === null ? 1 : 2);
    // The first photos: living plant first (its tier decides), within it leaves or whole plant, untagged, then the
    // rest; the seconds (flowers) are chosen after, from what the firsts did not use.
    const firsts = picked.map((p, i) => ({ p, list: [...ranked[i]].sort((a, b) => a.live - b.live || viewRank(a) - viewRank(b)), second: false }));
    const fetchOne = async ({ p, list, second }) => {
      {
        // Taken in order, before any await, so two photos are never the same.
        const tries = [];
        for (const ph of list) {
          if (tries.length >= 4) break;
          if (used.has(ph.img)) continue;
          used.add(ph.img);
          tries.push(ph);
        }
        // A photo goes only with its own credit (its GBIF record's media entry): otherwise the next one.
        for (const ph of tries) {
          try {
            const id = /(\d+)\/?$/.exec(ph.record ?? '')?.[1];
            const [credit, file] = await Promise.all([photoFiles.credit(id, ph.img), photoFiles.photo(ph.img)]);
            if (!credit.matched || !(credit.photographer || credit.rights_holder)) throw new Error('no credit');
            return { p, ph, credit, file, second };
          } catch {
            failed++;
          }
        }
        return null;
      }
    };
    const firstGot = await Promise.all(firsts.map(fetchOne));
    // Spares the firsts did not need are free again for the seconds.
    used.clear();
    for (const g of firstGot) if (g) used.add(g.ph.img);
    const seconds = [];
    for (const [i, p] of picked.entries()) {
      if (firstGot.filter(Boolean).length + seconds.length >= n) break;
      const flowers = ranked[i].filter(ph => ph.live === 0 && ph.view === 'flower' && !used.has(ph.img));
      if (flowers.length && firstGot[i]?.ph.view !== 'flower') seconds.push({ p, list: flowers, second: true });
    }
    const got = [...firstGot, ...(await Promise.all(seconds.map(fetchOne)))];
    const adminData = area?.admin ? await adminAreas.data() : null;
    const images = got.filter(Boolean).map(({ p, ph, credit, file, second }) => plantImage(plantTile(p, ph, short, nearbyKm, adminData), credit, file, lang, second));
    // Plants that had photos to try and got none credited (not those with no photo in the gallery).
    const missing = picked.filter((_, i) => !firstGot[i] && firsts[i].list.length).map(p => p.e.taxon.name);
    // Seconds that found nothing are not said.
    const note = [
      failed ? `${failed} photo(s) could not be fetched or credited (GBIF's record of the photo): the next best was used.` : '',
      missing.length ? `No photo with its credit could be had for ${missing.join(', ')}.` : '',
    ].filter(Boolean).join(' ');
    return { images, note: note || null };
  }

  /** What a sent plant photo's caption needs, kept with a contact sheet's tile too. */
  function plantTile(p, ph, short, nearbyKm, adminData) {
    const canton = ph.inside || !adminData ? null : cantonOf(ph.lng, ph.lat, adminData);
    return {
      kind: 'plant',
      name: p.e.taxon.name,
      gbif_name: p.e.taxon.gbif_name ?? null,
      same: p.same ?? null,
      status: p.status,
      here: p.own.length,
      nearby: p.near.records,
      nearby_km: nearbyKm,
      where: short,
      inside: ph.inside,
      away_km: ph.away <= NEAREST_MAX_KM ? Math.round(ph.away) : null,
      canton,
      region: canton ? canton.province : null,
      herbarium: ph.herbarium,
      field: ph.field,
      view: ph.view ?? null,
      lat: ph.lat,
      lng: ph.lng,
      year: ph.year ?? null,
      record: ph.record ?? null,
      img: ph.img,
    };
  }

  /** A plant photo ready to send: its file and its caption (plant, in the area or candidate, place, credit). */
  function plantImage(t, credit, file, lang, second = false) {
    const words = SEND_WORDS[lang];
    const stem = v => fold(v).split(' ').slice(0, 2).map(w => w.slice(0, -2)).join(' ');
    // "Mesechites trifidus" for "Mesechites trifida" is the same name: not said.
    const asRecorded = credit?.as_recorded && ![t.name, t.gbif_name, ...(t.same ?? [])].filter(Boolean).map(stem).includes(stem(credit.as_recorded)) ? credit.as_recorded : null;
    const place = credit?.place ?? `${t.lat.toFixed(3)}, ${t.lng.toFixed(3)}`;
    const year = credit?.year ?? t.year;
    const by = credit?.photographer ?? credit?.rights_holder ?? null;
    const link = credit?.link ?? t.record ?? null;
    // Outside the area: where the outlines put it (a GBIF locality may name the area across a changed border).
    const at = t.inside ? '' : ` (${words.photoOutside(t.where)}${t.canton ? `: ${t.canton.name}, ${t.canton.province}` : ''})`;
    const status = t.status === 'in_area' ? words.inArea(t.where, t.here) : t.status === 'nearby' ? words.candidate(t.where, t.nearby, t.nearby_km) : null;
    return {
      path: file.path,
      caption: [
        `${t.herbarium ? `${words.herbariumTag} ` : ''}_${t.name}_${t.same ? ` (= ${t.same.join(', ')})` : ''}${asRecorded ? ` (${words.as} ${asRecorded})` : ''}${status ? ` — ${status}` : ''}${words.views[t.view] ? ` · ${words.views[t.view]}` : ''}`,
        `${[place, year].filter(Boolean).join(', ')}${at}`,
        `${words.photo}: ${by ?? words.unknown}${credit?.licence ? ` (${credit.licence})` : ''}${t.herbarium ? `, ${words.herbarium}` : ''}`,
        link,
      ]
        .filter(Boolean)
        .join('\n'),
      plant: t.name,
      status: t.status,
      view: t.view,
      ...(second ? { second: true } : {}),
      photo_in_area: t.inside,
      field_photo: t.field && !t.herbarium,
      herbarium: t.herbarium,
      width: file.width,
      height: file.height,
      bytes: file.bytes,
      url: file.url,
      source_url: t.img,
      credit: { photographer: by, licence: credit?.licence ?? null, licence_url: credit?.licence_url ?? null, place, year: year ?? null, link, ...(asRecorded ? { as_recorded: asRecorded } : {}) },
    };
  }

  /** The `images_block` of the WhatsApp bridge: one line per photo, `path | caption` (its lines joined by « — »). */
  const imagesBlock = images => (images.some(im => im.path) ? `[images]\n${images.filter(im => im.path).map(im => `${im.path} | ${im.caption.split('\n').join(' — ')}`).join('\n')}\n[/images]` : '');

  /**
   * Contact sheets of the plants' candidate photos (server/photo-sheets.mjs) for the chat to look at and pick from:
   * the plants recorded in the area, then those only nearby; per plant its photos ranked as photosToSend ranks them
   * (living plant, view tags when there are, place), one flower among them when it has one. At most 2 sheets of 9.
   */
  async function plantSheets(plants, area, nearbyKm, m, lang) {
    if (!sheets) return { photo_sheet_error: 'Contact sheets are off on this server (no folder for them).' };
    const words = SEND_WORDS[lang];
    const short = area?.admin?.name ?? (area?.countries.length ? area.countries.join(', ') : words.area);
    const ordered = [...plants.filter(p => p.status === 'in_area'), ...plants.filter(p => p.status === 'nearby').sort((a, b) => b.near.records - a.near.records)];
    const picked = ordered.slice(0, 2 * PER_SHEET);
    if (!picked.length) return { photo_sheets: [], photo_sheet_note: `No host plant recorded in or near ${short}: no photos to choose from.` };
    const ranked = await rankedFor(picked, area, nearbyKm, m);
    const adminData = area?.admin ? await adminAreas.data() : null;
    // Up to 2 sheets of 9: one plant fills a sheet, two get a sheet each, more share them.
    const k = Math.min(PER_SHEET, Math.max(2, Math.floor((2 * PER_SHEET) / picked.length)));
    const seen = new Set();
    const tiles = [];
    // By place, as the chat will judge them: the living plant first, then in the area, nearer, newer.
    const byPlace = (a, b) => a.live - b.live || a.loc - b.loc || a.away - b.away || (b.year ?? 0) - (a.year ?? 0);
    for (const [i, p] of picked.entries()) {
      const best = sheetPicks(ranked[i].filter(ph => !seen.has(ph.img)).sort(byPlace), k);
      for (const ph of best) {
        seen.add(ph.img);
        const t = plantTile(p, ph, short, nearbyKm, adminData);
        const region = cantonOf(ph.lng, ph.lat, adminData)?.province ?? area?.regionOf?.(ph.lng, ph.lat) ?? null;
        const where = ph.inside
          ? (t.canton?.name ?? cantonOf(ph.lng, ph.lat, adminData)?.name ?? short)
          : `${ph.away <= NEAREST_MAX_KM ? `${Math.round(ph.away)} km` : `>${NEAREST_MAX_KM} km`}${region ? `, ${region}` : ''}`;
        tiles.push({
          ...t,
          // The chat judges what a tile shows; a model's guess is not put in the caption.
          view: null,
          thumb: /inaturalist[^/]*\/photos\/\d+\//.test(ph.img) ? ph.img.replace(/\/(original|large)\./, '/medium.') : proxyUrl(ph.img).replace(/w=\d+&h=\d+/, `w=${TILE}&h=${TILE}`),
          line1: `${t.name}${p.status === 'nearby' ? ` (${words.candidateShort})` : ''}`,
          line2: [where, ph.year, ph.herbarium ? words.herbariumShort : null].filter(Boolean).join(' · '),
        });
      }
    }
    const started = Date.now();
    const made = [];
    for (let i = 0; i < tiles.length && made.length < 2; i += PER_SHEET) made.push(sheets.make(tiles.slice(i, i + PER_SHEET), { first: i + 1 }));
    const out = await Promise.all(made);
    const onSheets = new Set(tiles.slice(0, 2 * PER_SHEET).map(t => t.name));
    // Plants with photos that did not fit (those with none in the gallery are not said to have been left out).
    const leftOut = ordered.filter((p, i) => (i >= picked.length || ranked[i].length) && !onSheets.has(p.e.taxon.name)).map(p => p.e.taxon.name);
    return {
      photo_sheets: out.map(sh => ({ id: sh.id, path: sh.path, tiles: sh.tiles.map(t => `${t.n}: ${t.line1} — ${t.line2}`) })),
      ...(leftOut.length ? { photo_sheet_left_out: leftOut, photo_sheet_left_out_note: 'Not on the sheets (2 sheets at most): ask again with `plant` for these.' } : {}),
      photo_sheet_note: 'No photo is sent yet. Look at each sheet (read its path) and choose for every plant a tile that shows its leaves or the whole plant (how it is found in the field), then a flower where there is one; then send_sheet_photos with `sheets` (their ids) and `tiles` (the numbers, at most 5). A plant with no such tile: say that none of its photos on the sheet shows its leaves (host_plants with `plant` and `photo_sheet` shows more of its photos).',
      photo_sheet_ms: Date.now() - started,
    };
  }

  /** send_sheet_photos: the tiles chosen on contact sheets, downloaded (about 2048 px) and credited, ready to send. */
  async function sendSheetPhotos(args) {
    if (!sheets) return { error: 'Contact sheets are off on this server.' };
    const ids = [...new Set(list(args.sheets ?? args.sheet))];
    const all = ids.flatMap(id => sheets.tilesOf(id) ?? []);
    if (!all.length) return { error: 'No such contact sheet: give the `id` of a sheet from photo_sheets.' };
    const numbers = [...new Set((Array.isArray(args.tiles) ? args.tiles : [args.tiles]).map(Number).filter(Number.isInteger))].slice(0, 5);
    // Sheets of one call are numbered on from each other; sheets of two calls both start at 1.
    const twice = numbers.filter(n => all.filter(t => t.n === n).length > 1);
    if (twice.length) return { error: `Tiles ${twice.join(', ')} are on more than one of these sheets (from different calls): call send_sheet_photos once per call's sheets.` };
    const chosen = numbers.map(n => all.find(t => t.n === n)).filter(Boolean);
    if (!chosen.length) return { error: `None of tiles ${numbers.join(', ')} is on these sheets (1–${all.at(-1).n}).` };
    const lang = args.lang === 'es' ? 'es' : 'en';
    const started = Date.now();
    // A plant's second tile in the order chosen is its second photo.
    const secondOf = new Set(chosen.filter((t, i) => t.kind === 'plant' && chosen.slice(0, i).some(u => u.kind === 'plant' && u.name === t.name)));
    const got = await Promise.all(
      chosen.map(async t => {
        try {
          if (t.kind === 'local') return { path: t.path, caption: t.caption, stage: t.stage, source: t.source };
          if (t.kind === 'remote') {
            const file = await photoFiles.photo(t.img);
            const c = t.credit ?? {};
            if (!(c.photographer || c.rights_holder)) throw new Error('no credit');
            const stageWord = STAGE_WORDS[lang][t.stage];
            return {
              path: file.path,
              caption: [`_${t.name}_ — ${stageWord}`, [t.place, t.year].filter(Boolean).join(', '), `${lang === 'es' ? 'Foto' : 'Photo'}: ${c.photographer ?? c.rights_holder}${c.licence ? ` (${c.licence})` : ''}`, c.link ?? t.record].filter(Boolean).join('\n'),
              stage: t.stage,
              source: t.source,
              width: file.width,
              height: file.height,
              bytes: file.bytes,
              url: file.url,
              source_url: t.img,
              credit: c,
            };
          }
          if (t.kind === 'specimen') {
            const file = await photoFiles.photo(t.full);
            return { path: file.path, caption: t.caption, cam_id: t.cam_id, width: file.width, height: file.height, bytes: file.bytes, url: file.url };
          }
          const id = /(\d+)\/?$/.exec(t.record ?? '')?.[1];
          const [credit, file] = await Promise.all([photoFiles.credit(id, t.img), photoFiles.photo(t.img)]);
          if (!credit.matched || !(credit.photographer || credit.rights_holder)) throw new Error('no credit');
          return plantImage(t, credit, file, lang, secondOf.has(t));
        } catch (e) {
          return { tile: t.n, error: e.message === 'no credit' ? 'its credit (GBIF record of the photo) could not be had' : e.message };
        }
      }),
    );
    const images = got.filter(g => !g.error);
    const failed = got.filter(g => g.error).map(g => ({ tile: g.tile, error: g.error }));
    return {
      images,
      images_block: imagesBlock(images),
      ...(failed.length ? { failed, failed_note: 'Pick another tile of the same plant for these.' } : {}),
      timing_ms: Date.now() - started,
    };
  }

  /** A JSON answer of an outside API, kept in memory for REFRESH_MS. */
  const apiKept = new Map();
  async function getJson(url) {
    const kept = apiKept.get(url);
    if (kept && now() - kept.at < REFRESH_MS) return kept.value;
    const r = await fetchImpl(url, { headers: { 'user-agent': 'ithomiini-database' }, signal: AbortSignal.timeout(20_000) });
    if (!r.ok) throw new Error(`${new URL(url).host} answered ${r.status}`);
    const value = await r.json();
    apiKept.set(url, { at: now(), value });
    return value;
  }

  /**
   * taxon_photos: photos of a taxon at a life stage, from the project's clutch photos, iNaturalist's Life Stage
   * annotation and GBIF's lifeStage (other datasets), on contact sheets to pick from (send_sheet_photos).
   */
  async function taxonPhotos(args) {
    const taxon = text(args.taxon);
    if (!/^[A-Za-z]+( [a-z-]+)?$/.test(taxon)) return { error: 'Give `taxon` as a genus or "Genus species".' };
    const stages = [...new Set(list(args.life_stage).map(fold).filter(s => LIFE_STAGES[s]))];
    if (!stages.length) return { error: 'Give `life_stage`: egg, larva, pupa or adult (one or a list).' };
    const lang = args.lang === 'es' ? 'es' : 'en';
    const admin = await areaOf(args);
    if (admin?.error) return admin;
    const near = args.near && Number.isFinite(args.near.lat) && Number.isFinite(args.near.lng) ? { lat: args.near.lat, lng: args.near.lng, km: Number(args.near.km) || 25 } : null;
    const box = admin?.bbox ?? null;
    // Without an area, nearest to Ikiam first (the team's base).
    const middle = box ? { lat: (box[1] + box[3]) / 2, lng: (box[0] + box[2]) / 2 } : near ?? IKIAM;
    const placeOf = (lat, lng) => {
      if (!Number.isFinite(lat) || !Number.isFinite(lng)) return { inside: false, away: Infinity };
      const inside = admin ? admin.test(lng, lat) : near ? km(near, { lat, lng }) <= near.km : false;
      return { inside, away: inside ? 0 : middle ? km(middle, { lat, lng }) : Infinity };
    };
    const counts = {};
    const errors = [];
    const candidates = [];
    const fold2 = v => fold(v).split(' ').slice(0, taxon.includes(' ') ? 2 : 1).join(' ');
    // The project's own: clutch photos of this taxon (eggs and larvae of the insectary's clutches).
    if (stages.some(s => s === 'egg' || s === 'larva') && store?.db) {
      const dir = clutchPhotoDir;
      let rows = [];
      try {
        rows = store.db.prepare("SELECT p.id, p.day, p.note, p.file, p.actor, r.values_json FROM clutch_photos p JOIN records r ON r.id = p.record_id WHERE r.sheet = 'Insectary_stocks'").all();
      } catch {
        rows = [];
      }
      const own = rows.filter(r => {
        try {
          return fold2(JSON.parse(r.values_json).SPECIES) === fold(taxon);
        } catch {
          return false;
        }
      });
      counts.project_clutch_photos = own.length;
      for (const r of own.slice(0, 2 * PER_SHEET)) {
        const v = JSON.parse(r.values_json);
        const path = dir ? join(dir, `${r.file}.jpg`) : null;
        if (!path) continue;
        candidates.push({ kind: 'local', source: 'project', stage: /larva|oruga/i.test(r.note ?? '') ? 'larva' : stages.includes('egg') ? 'egg' : 'larva', path, thumb: dir ? join(dir, `${r.file}.thumb.jpg`) : path, year: Number(String(r.day).slice(0, 4)) || null, place: 'Insectario Ikiam', inside: false, away: -1, caption: [`_${text(v.SPECIES)}_ — ${lang === 'es' ? 'puesta del insectario Ikiam' : 'clutch of the Ikiam insectary'} ${text(v['CLUTCH NUMBER'] ?? '')}`.trim(), [r.note, r.day].filter(Boolean).join(', '), `${lang === 'es' ? 'Foto' : 'Photo'}: ${r.actor} (Ithomiini Ikiam)`].join('\n') });
      }
    }
    // iNaturalist: research-grade observations annotated with the stage, with photos.
    for (const stage of stages) {
      try {
        const url = `${inatApi}?taxon_name=${encodeURIComponent(taxon)}&term_id=1&term_value_id=${LIFE_STAGES[stage].inat}&photos=true&quality_grade=research,needs_id&per_page=200&order=desc&order_by=votes`;
        const got = await getJson(url);
        counts[`inaturalist_${stage}`] = got.total_results ?? (got.results ?? []).length;
        for (const o of got.results ?? []) {
          const [lng, lat] = o.geojson?.coordinates ?? [];
          const at = placeOf(lat, lng);
          for (const [i, ph] of (o.photos ?? []).slice(0, 3).entries()) {
            const square = text(ph.url);
            if (!/^https:\/\//.test(square)) continue;
            candidates.push({
              kind: 'remote',
              source: 'inaturalist',
              stage,
              record: `inat:${o.id}`,
              second: i > 0,
              img: square.replace(/\/square\./, '/original.'),
              thumb: square.replace(/\/square\./, '/medium.'),
              year: Number(String(o.observed_on ?? '').slice(0, 4)) || null,
              place: text(o.place_guess) || null,
              research: o.quality_grade === 'research',
              ...at,
              credit: { photographer: text(ph.attribution).replace(/^\(c\)\s*/i, '').replace(/,.*$/, '') || null, licence: ph.license_code ? licenceName(`http://creativecommons.org/licenses/${ph.license_code.replace(/^cc-/, '')}/4.0/`) : null, link: `https://www.inaturalist.org/observations/${o.id}`, attribution: text(ph.attribution) || null },
            });
          }
        }
      } catch (e) {
        errors.push(`iNaturalist (${stage}): ${e.message}`);
      }
      // GBIF: other datasets whose records say the stage (iNaturalist's are above already).
      try {
        const got = await getJson(`${gbifApi}?scientificName=${encodeURIComponent(taxon)}&lifeStage=${LIFE_STAGES[stage].gbif}&mediaType=StillImage&limit=100`);
        const other = (got.results ?? []).filter(r => r.datasetKey !== INAT_DATASET);
        counts[`gbif_other_${stage}`] = other.length;
        for (const r of other) {
          const at = placeOf(r.decimalLatitude, r.decimalLongitude);
          for (const m of (r.media ?? []).filter(m => m.type === 'StillImage' && /^https?:\/\//.test(m.identifier ?? '')).slice(0, 2)) {
            candidates.push({ kind: 'remote', source: 'gbif', stage, record: `https://www.gbif.org/occurrence/${r.key}`, img: m.identifier, thumb: proxyUrl(m.identifier).replace(/w=\d+&h=\d+/, `w=${TILE}&h=${TILE}`), year: r.year ?? null, place: [r.locality, r.stateProvince, r.country].filter(Boolean).join(', ') || null, research: true, ...at, credit: creditOf(r, m.identifier) });
          }
        }
      } catch (e) {
        errors.push(`GBIF (${stage}): ${e.message}`);
      }
    }
    // The project's first, then in the area, nearer, research grade, newer; one photo per observation before a second.
    candidates.sort((a, b) => (a.source === 'project' ? -1 : 0) - (b.source === 'project' ? -1 : 0) || b.inside - a.inside || a.away - b.away || (b.research ?? 0) - (a.research ?? 0) || (b.year ?? 0) - (a.year ?? 0));
    const ordered = [...candidates.filter(c => !c.second), ...candidates.filter(c => c.second)];
    // Each stage asked gets its share of the 18 tiles.
    // Each stage asked gets its share of the 18 tiles; a stage with fewer photos leaves its share to the others.
    const per = Math.max(3, Math.floor((2 * PER_SHEET) / stages.length));
    const first = stages.flatMap(stage => ordered.filter(c => c.stage === stage).slice(0, per));
    const tiles = [...first, ...ordered.filter(c => !first.includes(c))].slice(0, 2 * PER_SHEET);
    tiles.sort((a, b) => stages.indexOf(a.stage) - stages.indexOf(b.stage));
    const words = STAGE_WORDS[lang];
    // A stage asked with no photo anywhere is said plainly (the chat must not imply there are some).
    const noneFor = stages.filter(stage => !candidates.some(c => c.stage === stage));
    const answer = {
      taxon,
      life_stage: stages,
      counts,
      ...(noneFor.length ? { none_for: noneFor, none_note: `No photo of ${noneFor.join(', ')} of ${taxon}: none in the project's clutch photos (${counts.project_clutch_photos ?? 0} of this taxon), none annotated so on iNaturalist, none in GBIF's lifeStage. Say so plainly.` } : {}),
      ...(errors.length ? { errors } : {}),
    };
    if (!tiles.length)
      return { ...answer, photo_sheets: [], note: `No photo of ${taxon} at ${stages.map(s => words[s]).join(' / ')} in the project's clutch photos, iNaturalist's Life Stage annotations or GBIF's lifeStage. Unannotated photos may exist on iNaturalist: say so.` };
    if (!sheets) return { ...answer, photo_sheet_error: 'Contact sheets are off on this server.' };
    const made = [];
    const shaped = tiles.map(c => ({
      ...c,
      name: taxon,
      line1: `${taxon} · ${words[c.stage]}`,
      line2: [(c.source === 'project' ? 'Insectario Ikiam' : c.inside ? admin?.name ?? (lang === 'es' ? 'en el área' : 'in the area') : c.place)?.slice(0, 38), c.year].filter(Boolean).join(' · '),
    }));
    for (let i = 0; i < shaped.length && made.length < 2; i += PER_SHEET) made.push(sheets.make(shaped.slice(i, i + PER_SHEET), { first: i + 1 }));
    const out = await Promise.all(made);
    return {
      ...answer,
      photo_sheets: out.map(sh => ({ id: sh.id, path: sh.path, tiles: sh.tiles.map(t => `${t.n}: ${t.line1} — ${t.line2}`) })),
      photo_sheet_note: 'Look at each sheet (read its path), pick the tiles that clearly show the stage asked (an annotation can be wrong: a tile showing an adult or a plant is not a larva), at most 5, then send_sheet_photos with `sheets` and `tiles`.',
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
    const { m, shown, mapped, withoutRecords, points: plantRecs } = await hostPlantLayers(found.kept, args);
    if (!mapped.length) return { error: 'None of these host plants has a records layer on the maps.', ...(withoutRecords.length ? { plants_without_records: withoutRecords } : {}) };
    // The mapped plants' records that can count for a site within near.km: one per GBIF record (synonymous plants share it).
    const reach = near.km + KERNEL_KM;
    const records = new Map();
    for (const e of shown) {
      if (!mapped.includes(e)) continue;
      const p = plantRecs.get(e.slug);
      for (let j = 0; j < (p?.lat.length ?? 0); j++) {
        const [lat, lng] = [p.lat[j], p.lng[j]];
        if (km(near, { lat, lng }) > reach) continue;
        const r = records.get(p.key[j]) ?? records.set(p.key[j], { lat, lng, year: p.year[j] || null, plants: new Set() }).get(p.key[j]);
        r.plants.add(e.taxon.name);
      }
    }
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
    let photoFailed = false;
    const gallery =
      want && kept.length
        ? await plantGallery(nearPlants, m).catch(() => {
            photoFailed = true;
            return [];
          })
        : [];
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
        ...(i < SITE_LINKS ? { map: mapsUrl({ ...urlTaxa, view: { lat: c.lat, lng: c.lng, zoom: 12 } }, mapsBase) } : {}),
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
      ...(photoFailed ? { photos_note: "The maps' photo pages could not be read now: no photos in this answer." } : {}),
      method: `Score: each plant record within ${KERNEL_KM} km weighs (1 - (d/${KERNEL_KM})²)², 1 at the site, 0.92 at 2 km, 0.56 at 5 km; a GBIF record of two synonymous plants counts once. ${mode === 'localities' ? "Sites: the project's collection localities (its Sanger records in the maps; elevation from the workbook)." : 'Sites: grid cells with plant records, scored at their centre, named after the nearest named collecting place; elevation from a 90 m DEM.'} Plant records: the maps' GBIF download (generated ${text(m.generated_at).slice(0, 10) || 'date unknown'}).`,
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
    const out = {
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
    return where ? fitArea(out) : out;
  }

  /**
   * An answer with an area within AREA_BUDGET, cut where it says least: the associations' references first (`sources`
   * keeps the citations), then the associations of plants with no records in the area, then the last associations,
   * the plants' top sites, the last photo blocks and the last sites; each cut said in `cut`.
   */
  function fitArea(answer) {
    const size = v => JSON.stringify(v).length;
    let out = answer;
    const cut = [];
    const over = () => size(out) > AREA_BUDGET;
    if (over()) {
      out = { ...out, associations: out.associations.map(({ references, ...a }) => a) };
      cut.push('the associations\' references');
    }
    if (over()) {
      const here = new Set(out.plants.filter(p => p.records > 0).map(p => fold(p.plant)));
      const kept = out.associations.filter(a => here.has(fold(a.plant)));
      if (kept.length < out.associations.length) {
        cut.push(`${out.associations.length - kept.length} associations of plants without records in \`plants\``);
        out = { ...out, associations: kept };
      }
    }
    const trim = (key, label, min) => {
      if (!over() || !Array.isArray(out[key]) || out[key].length <= min) return;
      let n = out[key].length;
      while (n > min && size({ ...out, [key]: out[key].slice(0, n) }) > AREA_BUDGET - 200) n--;
      cut.push(`${label}: the first ${n} of ${out[key].length}`);
      out = { ...out, [key]: out[key].slice(0, n) };
    };
    trim('associations', 'associations', 10);
    if (over()) {
      out = { ...out, plants: out.plants.map(({ top_sites, ...p }) => p) };
      cut.push("the plants' top sites (see `sites`)");
    }
    if (over() && out.markdown) {
      const blocks = out.markdown.split('\n\n');
      let n = blocks.length;
      while (n > 0 && size({ ...out, markdown: blocks.slice(0, n).join('\n\n') }) > AREA_BUDGET - 200) n--;
      cut.push(`photos: the first ${n} of ${blocks.length} plants`);
      out = { ...out, markdown: blocks.slice(0, n).join('\n\n') };
    }
    trim('sites', 'sites', 1);
    trim('associations', 'associations', 0);
    return cut.length ? { ...out, cut: { left_out: cut, note: 'To keep the answer within its size: ask without an area for all the associations, or with `plant` or a smaller area for the rest.' } } : out;
  }

  async function specimens(args) {
    if (args.life_stage && args.life_stage !== 'adult')
      return { error: `The collection's wing photos are of adults only: for ${args.life_stage} photos use taxon_photos (the project's clutch photos, iNaturalist, GBIF).` };
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
    // A contact sheet of the specimens' photos (dorsal first) for the chat to look at and pick from (send_sheet_photos).
    let sheet = null;
    if (args.photo_sheet === true) {
      if (!sheets) sheet = { photo_sheet_error: 'Contact sheets are off on this server.' };
      else {
        const tiles = shown
          .map(sp => ({ sp, ph: sp.photos.dorsal ?? sp.photos.ventral }))
          .filter(x => x.ph)
          .slice(0, 2 * PER_SHEET)
          .map(({ sp, ph }) => {
            const facts = [sp.sex, sp.locality, sp.country, sp.date, sp.elevation_m !== null ? `${sp.elevation_m} m` : null].filter(Boolean);
            return {
              kind: 'specimen',
              cam_id: sp.cam_id,
              thumb: lh3(ph.file, TILE),
              full: lh3(ph.file, 2048),
              caption: [`_${sp.species ?? '?'}_${sp.subspecies ? ` ${sp.subspecies}` : ''} — ${sp.cam_id}`, facts.join(', '), 'Ithomiini Ikiam (project collection)'].join('\n'),
              line1: `${sp.cam_id} ${sp.species ?? ''}`.trim(),
              line2: [sp.sex, sp.locality].filter(Boolean).join(', '),
            };
          });
        const out = [];
        for (let i = 0; i < tiles.length; i += PER_SHEET) out.push(sheets.make(tiles.slice(i, i + PER_SHEET), { first: i + 1 }));
        sheet = await Promise.all(out)
          .then(made => ({ photo_sheets: made.map(sh => ({ id: sh.id, path: sh.path, tiles: sh.tiles.map(t => `${t.n}: ${t.line1} — ${t.line2}`) })), photo_sheet_note: 'Look at each sheet (read its path), choose the tiles to send, then send_sheet_photos with `sheets` and `tiles`.' }))
          .catch(e => ({ photo_sheet_error: e.message }));
      }
    }
    return {
      ...(sheet ?? {}),
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
        if (name === 'send_sheet_photos') return await sendSheetPhotos(args);
        if (name === 'taxon_photos') return await taxonPhotos(args);
        if (name === 'gallery_specimens') return await specimens(args);
        if (name === 'maps_snapshot') return await snapshot(args);
      } catch (e) {
        return { error: e.message };
      }
      return { error: `Unknown tool ${name}` };
    },
  };
}
