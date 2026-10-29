import test from 'node:test';
import assert from 'node:assert/strict';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { mcpTools } from '../server/assistant.mjs';
import { countryOfPoint } from '../server/country-outlines.mjs';
import { MAPS_TOOL_NAMES, createMapsTools, kernel, cropUrl, dateOf, differences, galleryUrl, mapsUrl, modelOf, percent, previewUrl, specimensMarkdown, yearOf } from '../server/maps-tools.mjs';
import { initPhotoTables } from '../server/photodata.mjs';

const MAPS = 'https://maps.test/ithomiini_maps/';
const GALLERY = 'https://gallery.test/Gallery/';
const DEM = 'https://dem.test/v1/elevation';

const point = (id, name, ssp, lat, lng, source, place = null, date = null, country = 'Ecuador') => ({
  id,
  scientific_name: name,
  genus: name.split(' ')[0],
  species: name.split(' ')[1],
  subspecies: ssp,
  lat,
  lng,
  source,
  collection_location: place,
  observation_date: date,
  country,
});
const FILES = {
  'data_manifest.json': {
    generated_at: '2026-09-26T04:34:23Z',
    sources: {
      'Sanger Institute': { file: 'map_points_sanger.json' },
      iNaturalist: { file: 'map_points_inaturalist.json' },
      'Dore et al. (2022)': { file: 'map_points_dore.json' },
    },
  },
  'map_points_sanger.json': [
    // Tena: three records over two years; Mindo: one.
    point('CAM000001', 'Oleria onega', 'janarilla', -0.99, -77.81, 'Sanger Institute', 'Tena, Hollín', '18-Jan-22'),
    point('CAM000002', 'Oleria onega', 'janarilla', -0.991, -77.812, 'Sanger Institute', 'Tena, Hollín', '2023-02-01'),
    point('CAM000003', 'Oleria onega', 'lota', -0.992, -77.811, 'Sanger Institute', 'Tena', '2023-03-01'),
    point('CAM000004', 'Oleria onega', null, -0.05, -78.77, 'Sanger Institute', 'Mindo', '2024-05-01'),
    point('CAM000005', 'Mechanitis menapis', null, -0.05, -78.771, 'Sanger Institute', 'Mindo', '2024-05-02'),
    point('CAM000006', 'Unknown species', null, -0.05, -78.771, 'Sanger Institute', 'Mindo', '2024-05-02'),
  ],
  'map_points_inaturalist.json': [
    point('9001', 'Oleria onega', null, -0.051, -78.772, 'iNaturalist', 'Mindo Valley, Ecuador', '2019-08-12T13:15'),
    point('9002', 'Oleria onega', null, 4.6, -74.1, 'iNaturalist', 'Bogotá', '2020-01-01', 'Colombia'),
  ],
  'map_points_dore.json': [point('DORE_1', 'Oleria onega', 'onega', -1.125, -77.875, 'Dore et al. (2022)')],
  'taxonomy_synonyms.json': { synonyms: [{ synonym: 'Ithomia onega', accepted: 'Oleria onega' }] },
  'host_plants/host_plant_associations.json': {
    metadata: { generated_at: '2026-06-16T14:02:20+00:00', excluded_policy: 'confidence=exclude omitted' },
    associations: [
      { butterfly_taxon: 'Oleria onega', host_taxon_slug: 'genus_solanum', host_taxon_name: 'Solanum', host_plant_family: 'Solanaceae', host_plant_genus: 'Solanum', host_plant_species: null, host_plant_name_verbatim: 'Solanum', host_id_level: 'genus', evidence_level: 'literature', confidence: 'medium', citation_for_ui: 'Beccaloni et al. 2008', doi_or_url: 'https://example.org/beccaloni', source_refs: 'Ecuador: [Drummond, 1986]' },
      { butterfly_taxon: 'Oleria onega janarilla', host_taxon_slug: 'species_solanum_mite', host_taxon_name: 'Solanum mite', host_plant_family: 'Solanaceae', host_plant_genus: 'Solanum', host_plant_species: 'Solanum mite', host_plant_name_verbatim: 'Solanum mite', host_id_level: 'species', evidence_level: 'direct', confidence: 'high', citation_for_ui: 'Willmott & Mallet 2004', doi_or_url: 'https://doi.org/10.1/x' },
      { butterfly_taxon: 'Oleria onega', host_taxon_slug: 'genus_cestrum', host_taxon_name: 'Cestrum', host_plant_family: 'Solanaceae', host_plant_genus: 'Cestrum', host_plant_species: null, host_plant_name_verbatim: 'Cestrum', host_id_level: 'genus', evidence_level: 'literature', confidence: 'low', citation_for_ui: 'Beccaloni et al. 2008' },
      { butterfly_taxon: 'Mechanitis menapis', host_taxon_name: 'Solanum', host_plant_family: 'Solanaceae', host_plant_genus: 'Solanum', host_plant_species: null, host_plant_name_verbatim: 'Solanum', host_id_level: 'genus', evidence_level: 'literature', confidence: 'high', citation_for_ui: 'Beccaloni et al. 2008' },
      { butterfly_taxon: 'Mechanitis menapis', host_taxon_name: 'Solanum x', host_plant_family: 'Solanaceae', host_plant_genus: 'Solanum', host_plant_species: 'Solanum x', host_plant_name_verbatim: 'Solanum x', host_id_level: 'species', evidence_level: 'needs_check', confidence: 'needs_check', citation_for_ui: 'Someone' },
    ],
  },
  // Host-plant layers: Solanum mite's records in chunks 1 and 2 (chunk 3 is another plant's, never read for it).
  'host_plants/host_plant_layers_manifest.json': {
    metadata: {
      generated_at: '2026-06-13T03:47:42+00:00',
      scientific_caveat: 'Plant layers do not demonstrate local host use.',
      occurrence_refs_by_taxon: { species_solanum_mite: 'taxon_occurrence_refs/species_solanum_mite.json', genus_solanum: 'taxon_occurrence_refs/genus_solanum.json' },
      gallery_indexes_by_taxon: { species_solanum_mite: 'gallery_taxa/species_solanum_mite/index.json' },
    },
    taxa: [
      { slug: 'species_solanum_mite', canonical_name: 'Solanum mite', rank: 'species', family: 'Solanaceae', gbif_resolution: { canonical_name: 'Solanum mitis' } },
      { slug: 'genus_solanum', canonical_name: 'Solanum', rank: 'genus', family: 'Solanaceae' },
      { slug: 'genus_cestrum', canonical_name: 'Cestrum', rank: 'genus', family: 'Solanaceae' },
    ],
  },
  'host_plants/taxon_occurrence_refs/species_solanum_mite.json': {
    metadata: { total_records: 5 },
    chunks: { 'occurrence_chunks/chunk_001.json': [0, 1, 2], 'occurrence_chunks/chunk_002.json': [0, 1] },
  },
  'host_plants/taxon_occurrence_refs/genus_solanum.json': { metadata: { total_records: 2 }, chunks: { 'occurrence_chunks/chunk_002.json': [0, 1] } },
  'host_plants/occurrence_chunks/chunk_001.json': {
    records: [
      // Near Tena (twice, two years) and in Colombia.
      { lat: -0.995, lng: -77.805, eventDate: '2019-05-01', basisOfRecord: 'HUMAN_OBSERVATION' },
      { lat: -0.996, lng: -77.806, eventDate: '2021-07-02', basisOfRecord: 'PRESERVED_SPECIMEN' },
      { lat: 4.6, lng: -74.1, eventDate: '1999-01-01', basisOfRecord: 'PRESERVED_SPECIMEN' },
    ],
  },
  'host_plants/occurrence_chunks/chunk_002.json': {
    records: [
      { lat: -0.06, lng: -78.77, eventDate: '2010-01-01', basisOfRecord: 'PRESERVED_SPECIMEN' },
      // Just off the outline's western edge (the coast): counted in Ecuador.
      { lat: -2, lng: -80.02, eventDate: null, basisOfRecord: 'PRESERVED_SPECIMEN' },
    ],
  },
  'host_plants/occurrence_chunks/chunk_003.json': { records: [{ lat: 0, lng: 0 }] },
  'host_plants/gallery_taxa/species_solanum_mite/index.json': { total_images: 3, pages: ['gallery_taxa/species_solanum_mite/page_001.json'] },
  'host_plants/gallery_taxa/species_solanum_mite/page_001.json': {
    items: [
      { image_url: 'https://herbarium.test/sheet.jpg', observation_url: 'https://www.gbif.org/occurrence/3', lat: -0.06, lng: -78.77, observation_date: '2010-01-01', photo_context: 'preserved', source: 'Tropicos' },
      { image_url: 'https://inaturalist-open-data.s3.amazonaws.com/photos/11/original.jpg', observation_url: 'https://www.gbif.org/occurrence/1', lat: -0.995, lng: -77.805, observation_date: '2019-05-01', photo_context: 'field', source: 'iNaturalist research-grade observations' },
      { image_url: 'https://inaturalist-open-data.s3.amazonaws.com/photos/12/original.jpg', observation_url: 'https://www.gbif.org/occurrence/9', lat: 4.6, lng: -74.1, observation_date: '1999-01-01', photo_context: 'field', source: 'iNaturalist research-grade observations' },
    ],
  },
  // Two square countries, Ecuador south of the equator and Colombia north of it.
  'country_boundaries.json': {
    countries: [
      { name: 'Ecuador', bbox: [-80, -4, -75, 0], polygons: [[[[-80, -4], [-75, -4], [-75, 0], [-80, 0], [-80, -4]]]] },
      { name: 'Colombia', bbox: [-80, 0, -70, 8], polygons: [[[[-80, 0], [-70, 0], [-70, 8], [-80, 8], [-80, 0]]]] },
    ],
  },
};

/** The maps site, Open-Meteo's elevations (1500 m at Mindo, 500 m elsewhere), and what was asked of them. */
function fakeWeb() {
  const calls = [];
  let clock = 0;
  const fetchImpl = async (url, options = {}) => {
    const u = new URL(url);
    calls.push(u.pathname + (options.headers?.['if-none-match'] ? ' (etag)' : ''));
    if (u.origin + '/v1/elevation' === DEM) {
      const lats = u.searchParams.get('latitude').split(',').map(Number);
      return Response.json({ elevation: lats.map(lat => (Math.abs(lat + 0.05) < 0.01 ? 1500 : 500)) });
    }
    const path = u.pathname.replace('/ithomiini_maps/data/', '');
    if (!(path in FILES)) return new Response('missing', { status: 404 });
    if (options.headers?.['if-none-match'] === `"${path}"`) return new Response(null, { status: 304 });
    return Response.json(FILES[path], { headers: { etag: `"${path}"`, 'last-modified': 'Wed, 14 Oct 2026 15:15:59 GMT' } });
  };
  return { calls, fetchImpl, now: () => clock, advance: ms => void (clock += ms) };
}

const seed = {
  Collection_data: [
    ['CAM000001', 'Oleria onega', 'janarilla', 'female', 'Tena, Hollín', 'Ecuador', '600'],
    ['CAM000002', 'Oleria onega', 'janarilla', 'male', 'Tena, Hollín', 'Ecuador', '610'],
    ['CAM000003', 'Oleria onega', 'lota', 'male', 'Tena', 'Ecuador', ''],
    ['CAM000004', 'Oleria onega', '', 'female', 'Mindo', 'Ecuador', '1450'],
    ['CAM000005', 'Mechanitis menapis', '', 'male', 'Mindo', 'Ecuador', '1460'],
    ['CAM000007', 'Mechanitis menapis', '', 'male', 'Mindo', 'Ecuador', 'NA'],
  ].map(([cam, SPECIES, Subspecies_Form, Sex, Collection_location, Country, ELEVATION], i) => ({
    row: i + 2,
    values: { CAM_ID: cam, SPECIES, Subspecies_Form, Sex, Collection_location, Country, ELEVATION, Release_Collect: 'Collected_Preserved' },
  })),
  Photo_links: ['CAM000001d', 'CAM000001v', 'CAM000002d', 'CAM000004v', 'CAM000005d', 'CAM000005d2', 'CAM000007d'].map((name, i) => ({
    row: i + 2,
    values: { Name: `${name}.JPG`, Type: 'File', URL: `https://drive.google.com/file/d/1${name.padEnd(30, 'x')}/view` },
  })),
};
async function setup() {
  const store = new Store({ localMode: true }, { sheets: new LocalSheets(seed) });
  await store.sync({ sheets: Object.keys(seed) });
  const web = fakeWeb();
  const tools = createMapsTools({ store, fetchImpl: web.fetchImpl, now: web.now, mapsBase: MAPS, galleryBase: GALLERY, elevationApi: DEM });
  return { store, web, tools };
}

test('the five tools are offered to the chats', () => {
  const names = new Set(mcpTools().map(t => t.name));
  for (const n of MAPS_TOOL_NAMES) assert.ok(names.has(n), n);
});

test('years of the maps\' dates', () => {
  assert.equal(yearOf('18-Jan-22'), 2022);
  assert.equal(yearOf('2019-08-12T13:15'), 2019);
  assert.equal(yearOf('12/03/1998'), 1998);
  assert.equal(yearOf(null), null);
  // The workbook's day numbers.
  assert.equal(dateOf('45914'), '2025-09-14');
  assert.equal(dateOf('2025-09-13'), '2025-09-13');
  assert.equal(dateOf(''), null);
});

test('URLs: the maps keep their sources and view; the gallery its filters or CAM IDs', () => {
  assert.equal(
    mapsUrl({ species: ['Oleria onega'], sources: ['sanger', 'inaturalist'], view: { lng: -77.81, lat: -0.99, zoom: 12 } }, MAPS),
    `${MAPS}?sp=Oleria%20onega&source=Sanger%20Institute%2CiNaturalist&map_view=-77.81000%2C-0.99000%2C12.00`,
  );
  assert.equal(galleryUrl({ species: ['Oleria onega'], sex: 'male' }, GALLERY), `${GALLERY}?view=1&species=Oleria%20onega&sex=male`);
  assert.equal(galleryUrl({ camids: ['CAM1', 'CAM2'] }, GALLERY), `${GALLERY}search?view=1&camids=CAM1%20CAM2`);
});

test('maps_localities: sites ranked by records, Dore cells apart, synonyms, links', async () => {
  const { tools } = await setup();
  const out = await tools.run('maps_localities', { taxa: ['Ithomia onega', 'Nonexistent thing'] });
  assert.deepEqual(out.taxa.species, ['Oleria onega']);
  assert.deepEqual(out.taxa.synonyms, { 'Ithomia onega': 'Oleria onega' });
  assert.deepEqual(out.taxa.not_in_maps, ['Nonexistent thing']);
  assert.deepEqual(
    out.sites.map(s => [s.site, s.records]),
    [['Tena, Hollín', 3], ['Mindo', 2], ['Bogotá', 1]],
  );
  assert.deepEqual(out.sites[0].years, { first: 2022, last: 2023, distinct: 2 });
  assert.deepEqual(out.sites[1].by_source, { 'Sanger Institute': 1, iNaturalist: 1 });
  assert.equal(out.grid_cells.cells, 1);
  assert.match(out.sites[0].map, /sp=Oleria%20onega&source=Sanger%20Institute%2CGBIF%20%28UNAM%29%2C/);
  assert.match(out.sites[0].map, /map_view=-77\.81\d+%2C-0\.99\d+%2C12\.00/);
  assert.equal(out.freshness.generated_at, '2026-09-26T04:34:23Z');
  assert.equal(out.freshness.files['Sanger Institute'], 'Wed, 14 Oct 2026 15:15:59 GMT');

  const colombia = await tools.run('maps_localities', { taxa: ['Oleria'], country: ['colombia'] });
  assert.deepEqual(colombia.taxa.genus, ['Oleria']);
  assert.deepEqual(colombia.sites.map(s => s.site), ['Bogotá']);
  // The maps' own spelling of the country.
  assert.match(colombia.map, /genus=Oleria&source=.*&country=Colombia&/);
  // A genus with a species: the app ANDs those filters, so the genus goes as its species.
  const mixed = await tools.run('maps_localities', { taxa: ['Oleria', 'Mechanitis menapis'] });
  assert.doesNotMatch(mixed.map, /genus=/);
  assert.match(mixed.map, /sp=Mechanitis%20menapis%2COleria%20onega&/);
  const withSubspecies = await tools.run('maps_localities', { taxa: ['Oleria', 'Oleria onega janarilla'] });
  assert.doesNotMatch(withSubspecies.map, /ssp=/);
  // A country with no records of them keeps its filter.
  const peru = await tools.run('maps_localities', { taxa: ['Oleria onega'], country: ['Peru'] });
  assert.match(peru.map, /&country=Peru/);
});

test('maps_localities by elevation: the workbook for the project\'s records, the DEM for the rest', async () => {
  const { tools, web } = await setup();
  const out = await tools.run('maps_localities', { taxa: ['Oleria onega'], elevation_min: 1000 });
  // CAM000004 (1450 m in the workbook) and the iNaturalist record at Mindo (1500 m DEM).
  assert.deepEqual(out.sites.map(s => [s.site, s.records, s.elevation_m, s.elevation_from]), [['Mindo', 2, 1475, 'workbook+dem']]);
  assert.equal(out.records.elevation_unknown, 0);
  const asked = web.calls.filter(c => c === '/v1/elevation').length;
  assert.equal(asked, 1);
  // The DEM's answers are kept: not asked again.
  const low = await tools.run('maps_localities', { taxa: ['Oleria onega'], elevation_max: 700 });
  assert.deepEqual(low.sites.map(s => [s.site, s.records]), [['Tena, Hollín', 3], ['Bogotá', 1]]);
  assert.equal(web.calls.filter(c => c === '/v1/elevation').length, asked);
});

test('the site is read at most every 6 hours, and parsed again only when it changed', async () => {
  const { tools, web } = await setup();
  await tools.run('maps_localities', { taxa: ['Oleria onega'] });
  const first = web.calls.length;
  await tools.run('maps_localities', { taxa: ['Oleria onega'] });
  assert.equal(web.calls.length, first);
  web.advance(6 * 3600_000 + 1);
  const again = await tools.run('maps_localities', { taxa: ['Oleria onega'] });
  assert.ok(web.calls.slice(first).every(c => c.endsWith('(etag)')));
  assert.equal(again.sites[0].records, 3);
});

test('maps_species: a region is needed; taxa by records', async () => {
  const { tools } = await setup();
  assert.match((await tools.run('maps_species', {})).error, /region/);
  const out = await tools.run('maps_species', { near: { lat: -0.05, lng: -78.77, km: 5 } });
  assert.deepEqual(out.taxa.map(t => [t.name, t.records]), [['Oleria onega', 2], ['Mechanitis menapis', 1]]);
  assert.equal(out.unidentified_records, 1);
  assert.equal(out.taxa[0].last_year, 2024);
  assert.match(out.map, /sp=Oleria%20onega%2CMechanitis%20menapis/);
  assert.match(out.taxa[1].map, /sp=Mechanitis%20menapis&source=/);
});

test('host_plants: a species with its subspecies\' rows, confidence, sources; a plant\'s butterflies', async () => {
  const { tools } = await setup();
  const out = await tools.run('host_plants', { butterfly: 'Oleria onega' });
  // The direct observation first.
  assert.deepEqual(out.associations.map(a => [a.butterfly, a.plant, a.confidence, a.source]), [
    ['Oleria onega janarilla', 'Solanum mite', 'high', 'Willmott & Mallet 2004'],
    ['Oleria onega', 'Solanum', 'medium', 'Beccaloni et al. 2008'],
  ]);
  assert.deepEqual(out.left_out, { count: 1, by_confidence: { low: 1 } });
  assert.deepEqual(out.by_plant_genus, { Solanum: 2 });
  assert.equal(out.sources['Willmott & Mallet 2004'].link, 'https://doi.org/10.1/x');
  const solanum = await tools.run('host_plants', { plant: 'Solanum', min_confidence: 'high' });
  assert.deepEqual(solanum.associations.map(a => a.butterfly), ['Oleria onega janarilla', 'Mechanitis menapis']);
  const unchecked = await tools.run('host_plants', { plant: 'solanum x', include_unchecked: true });
  assert.equal(unchecked.found, 1);
  assert.match((await tools.run('host_plants', {})).error, /butterfly/);
});

test('host_plants with an area: plants\' records there, sites, photos as Markdown, the map with the plant layer', async () => {
  const { tools, web } = await setup();
  const out = await tools.run('host_plants', { butterfly: 'Oleria onega', country: ['ecuador'], photos: 2 });
  // The associations as before, and the area.
  assert.equal(out.found, 2);
  assert.deepEqual(out.asked.country, ['Ecuador']);
  const mite = out.plants.find(p => p.plant === 'Solanum mite');
  assert.equal(mite.gbif_name, 'Solanum mitis');
  // Tena twice, Mindo and the coastal record in Ecuador; Bogotá is in Colombia.
  assert.equal(mite.records, 4);
  assert.equal(mite.records_total, 5);
  assert.deepEqual(mite.years, { first: 2010, last: 2021, distinct: 3 });
  assert.deepEqual(mite.by_basis, { preserved_specimen: 3, human_observation: 1 });
  assert.equal(mite.top_sites[0].records, 2);
  assert.equal(mite.top_sites[0].near, 'Tena, Ecuador (<1 km)');
  // The genus-level host is counted, but the sites, photos and map are the species-level host's.
  const genus = out.plants.find(p => p.plant === 'Solanum');
  assert.equal(genus.records, 2);
  assert.match(genus.note, /genus-level/);
  assert.equal(out.sites[0].records, 2);
  assert.deepEqual(out.sites[0].plants, { 'Solanum mite': 2 });
  assert.deepEqual(out.sites[0].years, { first: 2019, last: 2021, distinct: 2 });
  // Only the chunks the plants' refs name were read.
  assert.ok(web.calls.some(c => c.includes('chunk_001')) && !web.calls.some(c => c.includes('chunk_003')));
  const url = new URL(out.map_url);
  assert.equal(url.searchParams.get('hp_bf'), 'Oleria onega');
  assert.equal(url.searchParams.get('hp_taxa'), 'species_solanum_mite');
  assert.equal(url.searchParams.get('country'), 'Ecuador');
  assert.equal(url.searchParams.get('sp'), 'Oleria onega');
  assert.ok(url.searchParams.get('map_view'));
  assert.ok(!url.searchParams.get('source').includes('Dore'));
  // Photos taken in Ecuador, field photo first, linked to the full photo, with the GBIF record and credit.
  assert.match(out.markdown, /^\*\*Solanum mite\*\* \(GBIF: Solanum mitis\): 4 records in Ecuador, 2010–2021/);
  assert.match(out.markdown, /\[!\[Solanum mite 2019\]\(https:\/\/wsrv\.nl\/\?url=https%3A%2F%2Finaturalist-open-data\.s3\.amazonaws\.com%2Fphotos%2F11%2Fmedium\.jpg&w=240&h=180&fit=cover&output=jpg\)\]\(https:\/\/inaturalist-open-data\.s3\.amazonaws\.com\/photos\/11\/original\.jpg\)/);
  assert.ok(out.markdown.indexOf('photos%2F11') < out.markdown.indexOf('sheet.jpg'));
  // Bogotá's photo is not in Ecuador: left out while the area has enough.
  assert.ok(!out.markdown.includes('photos%2F12'));
  assert.match(out.markdown, /\[1\]\(https:\/\/www\.gbif\.org\/occurrence\/1\)/);
  assert.match(out.markdown, /iNaturalist research-grade observations; Tropicos\. Licence and author on each record's page/);
  assert.match(out.records_note, /not that the butterfly uses it there/);
  // Cestrum is a low-confidence row: not asked.
  assert.equal(out.plants_without_records, undefined);
  // A second call reads nothing new.
  const calls = web.calls.length;
  await tools.run('host_plants', { butterfly: 'Oleria onega', country: ['Ecuador'] });
  assert.equal(web.calls.length, calls);
});

test('host_plants with an area: photos from elsewhere when the area has too few, near, an unknown country', async () => {
  const { tools } = await setup();
  const three = await tools.run('host_plants', { butterfly: 'Oleria onega', country: ['Ecuador'], photos: 3 });
  assert.match(three.markdown, /photos%2F12.*\]\(.*\)/);
  assert.match(three.markdown, /Solanum mite 1999, Colombia\]/);
  assert.match(three.markdown, /Some photos are from outside Ecuador/);
  const none = await tools.run('host_plants', { butterfly: 'Oleria onega', country: ['Ecuador'], photos: 0 });
  assert.equal(none.markdown, '');
  const near = await tools.run('host_plants', { butterfly: 'Oleria onega', near: { lat: -0.99, lng: -77.81, km: 5 } });
  assert.equal(near.plants.find(p => p.plant === 'Solanum mite').records, 2);
  assert.equal(new URL(near.map_url).searchParams.get('country'), null);
  const everywhere = await tools.run('host_plants', { butterfly: 'Oleria onega janarilla', records: true });
  assert.equal(everywhere.plants.find(p => p.plant === 'Solanum mite').records, 5);
  assert.equal(everywhere.area.where, 'their whole range');
  assert.match((await tools.run('host_plants', { butterfly: 'Oleria onega', country: ['Atlantis'] })).error, /do not know Atlantis/);
  // Without an area: the associations only, as before.
  const plain = await tools.run('host_plants', { butterfly: 'Oleria onega' });
  assert.equal(plain.plants, undefined);
  assert.equal(plain.map_url, undefined);
});

test('host_plant_sites: the project\'s localities near a point, scored by the plant records around them', async () => {
  const { tools } = await setup();
  assert.equal(kernel(0), 1);
  assert.equal(kernel(5), 0.5625);
  assert.equal(kernel(10), 0);
  assert.match((await tools.run('host_plant_sites', { butterfly: 'Oleria onega' })).error, /near/);
  // Between Tena and Mindo (about 75 km from each); a radius over 100 km is cut to 100.
  const out = await tools.run('host_plant_sites', { butterfly: 'Oleria onega', near: { lat: -0.5, lng: -78.3, km: 500 } });
  assert.equal(out.asked.near.km, 100);
  // Only the species-level host is scored; the genus-level one is named.
  assert.deepEqual(out.plants, ['Solanum mite']);
  assert.deepEqual(out.plants_not_scored, ['Solanum (genus-level host)']);
  // Tena's two localities (two plant records under 1 km away), then Mindo (one, 1 km away).
  assert.deepEqual(out.sites.slice(0, 2).map(s => s.locality).sort(), ['Tena', 'Tena, Hollín']);
  const hollin = out.sites.find(s => s.locality === 'Tena, Hollín');
  assert.deepEqual(hollin.records_within_km, { 2: 2, 5: 2, 10: 2 });
  assert.ok(hollin.score > 1.9 && hollin.score <= 2);
  assert.deepEqual(hollin.record_years, { oldest: 2019, newest: 2021, undated: 0 });
  assert.equal(hollin.nearest_record.plant, 'Solanum mite');
  assert.equal(hollin.nearest_record.year, 2021);
  // The workbook's elevation, and the project's records there (of the butterfly asked, and all).
  assert.equal(hollin.elevation_m, 605);
  assert.equal(hollin.project_records, 2);
  assert.equal(hollin.project_records_of_butterfly, 2);
  const mindo = out.sites[2];
  assert.equal(mindo.locality, 'Mindo');
  assert.equal(mindo.project_records, 3);
  assert.equal(mindo.project_records_of_butterfly, 1);
  assert.equal(mindo.elevation_m, 1455);
  assert.equal(out.sites.length, 3);
  // The map on the first site with the host-plant layer on.
  const url = new URL(out.map_url);
  assert.equal(url.searchParams.get('hp_taxa'), 'species_solanum_mite');
  assert.equal(url.searchParams.get('hp_bf'), 'Oleria onega');
  assert.match(url.searchParams.get('map_view'), /,12\.00$/);
  // The photos nearest each site, with their distance and credit.
  assert.match(out.markdown, /^\*\*1\. Tena(, Hollín)?\*\*: the plant photos nearest it/);
  assert.match(out.markdown, /\[!\[Solanum mite 2019, <1 km away\]\(https:\/\/wsrv\.nl\/\?url=[^)]+photos%2F11%2Fmedium\.jpg[^)]*\)\]\(https:\/\/inaturalist-open-data\.s3\.amazonaws\.com\/photos\/11\/original\.jpg\)/);
  assert.match(out.markdown, /\[1\]\(https:\/\/www\.gbif\.org\/occurrence\/1\) Solanum mite, <1 km/);
  assert.match(out.markdown, /licence and author on each GBIF record's page/);
  assert.match(out.caveats.join(' '), /well-sampled place rather than more plants/);
  assert.match(out.caveats.join(' '), /not that the butterfly uses it there/);
  // A small radius around Tena: Mindo is out of reach.
  const close = await tools.run('host_plant_sites', { butterfly: 'Oleria onega', near: { lat: -0.99, lng: -77.81, km: 5 } });
  assert.equal(close.sites.length, 2);
});

test('host_plant_sites by grid cells: named after the nearest place, DEM elevation', async () => {
  const { tools } = await setup();
  const out = await tools.run('host_plant_sites', { butterfly: 'Oleria onega', near: { lat: -0.5, lng: -78.3, km: 100 }, rank: 'grid', cell_km: 1 });
  // Cells holding plant records within reach: Tena's (both records) first, then Mindo's.
  assert.equal(out.sites[0].records_within_km['2'], 2);
  assert.equal(out.sites[0].near, 'Tena, Ecuador (<1 km)');
  assert.equal(out.sites[0].elevation_m, 500);
  assert.equal(out.sites[0].elevation_from, 'dem');
  // The iNaturalist record's name is nearer this cell's centre than the project's.
  assert.equal(out.sites[1].near, 'Mindo Valley, Ecuador (<1 km)');
  assert.equal(out.sites[1].elevation_m, 1500);
  assert.equal(out.sites[0].locality, undefined);
  assert.equal(out.localities_without_records_within_10_km, undefined);
});

test('country outlines: inside, on the coast, nowhere', () => {
  const outlines = FILES['country_boundaries.json'];
  assert.equal(countryOfPoint(-78, -1, outlines), 'Ecuador');
  assert.equal(countryOfPoint(-78, 1, outlines), 'Colombia');
  assert.equal(countryOfPoint(-80.02, -2, outlines), 'Ecuador');
  assert.equal(countryOfPoint(-81, -2, outlines), null);
});

test('maps URL with the host-plant layer', () => {
  const url = mapsUrl({ species: ['Tithorea harmonia'], sources: ['sanger'], country: ['Ecuador'], hostPlants: { butterflies: ['Tithorea harmonia'], taxa: ['species_mesechites_trifida'], evidence: ['direct'] }, view: { lng: -78.4, lat: -1.5, zoom: 6.5 } }, MAPS);
  assert.equal(url, `${MAPS}?sp=Tithorea%20harmonia&source=Sanger%20Institute&country=Ecuador&hp_bf=Tithorea%20harmonia&hp_taxa=species_mesechites_trifida&hp_evidence=direct&map_view=-78.40000%2C-1.50000%2C6.50`);
});

test('gallery_specimens: filters, gallery links, photo URLs and thumbnails; nothing written', async () => {
  const { tools, store } = await setup();
  const changes = () => store.db.prepare('SELECT total_changes() n').get().n;
  const before = changes();
  const males = (await tools.run('gallery_specimens', { species: ['oleria onega'], sex: 'male' }));
  // CAM000003 has no photo.
  assert.deepEqual(males.specimens.map(s => s.cam_id), ['CAM000002']);
  assert.equal(males.gallery, `${GALLERY}?view=1&species=Oleria%20onega&sex=male`);
  assert.equal(males.specimens[0].photos.dorsal.file, '1CAM000002d'.padEnd(31, 'x'));
  assert.equal(males.specimens[0].photos.dorsal.full, `https://lh3.googleusercontent.com/d/${'1CAM000002d'.padEnd(31, 'x')}=w1600`);
  // Without the photo tables (an app that has not imported them): the photo and no model output.
  assert.equal(males.specimens[0].model, null);
  assert.match(males.markdown, /^\*\*CAM000002\*\* in the database: Oleria onega, janarilla, male/);
  assert.match(males.markdown, /\[!\[CAM000002 dorsal\]\(https:\/\/wsrv\.nl\/\?url=https%3A%2F%2Flh3\.googleusercontent\.com%2Fd%2F1CAM000002d\w+%3Dw1600&w=600&output=jpg\)\]\(https:\/\/lh3\.googleusercontent\.com\/d\/1CAM000002d\w+=w1600\)/);
  assert.match(males.markdown, /Model output: none stored/);
  assert.equal((await tools.run('gallery_specimens', { species: ['Oleria onega'], sex: 'male', with_photos: false })).found, 2);

  const mindo = (await tools.run('gallery_specimens', { locality: 'mindo', elevation_min: 1400 }));
  assert.deepEqual(mindo.specimens.map(s => s.cam_id), ['CAM000005', 'CAM000004']);
  assert.equal(mindo.gallery, `${GALLERY}search?view=1&camids=CAM000005%20CAM000004`);
  // The plain photo, not its extra view.
  assert.equal(mindo.specimens[0].photos.dorsal.name, 'CAM000005d.JPG');
  assert.equal(mindo.specimens[1].photos.dorsal, null);

  // "NA" is no elevation, not 0 m.
  assert.equal((await tools.run('gallery_specimens', { genus: ['Mechanitis'], elevation_max: 100 })).found, 0);
  const byId = (await tools.run('gallery_specimens', { cam_ids: ['cam000001'], thumbnails: 1 }));
  // One 600 px photo (no wing boxes or envelope here), and both full photos linked.
  assert.equal(byId.markdown.match(/!\[/g).length, 1);
  assert.match(byId.markdown, /Full photos: \[dorsal\]\(.*\), \[ventral\]\(/);
  assert.match((await tools.run('gallery_specimens', {})).error, /at least one/);
  assert.equal(changes(), before);
});

test('a site that does not answer: an error the first time, the copy read before later', async () => {
  const { store } = await setup();
  let down = true;
  let clock = 0;
  const web = fakeWeb();
  const tools = createMapsTools({
    store,
    now: () => clock,
    mapsBase: MAPS,
    fetchImpl: (url, o) => (down ? Promise.reject(new Error('offline')) : web.fetchImpl(url, o)),
  });
  assert.match((await tools.run('host_plants', { butterfly: 'Oleria' })).error, /offline/);
  down = false;
  assert.equal((await tools.run('host_plants', { butterfly: 'Oleria' })).found, 2);
  down = true;
  clock += 7 * 3600_000;
  assert.equal((await tools.run('host_plants', { butterfly: 'Oleria' })).found, 2);
});

/** The photo tables of server/photodata.mjs with readings of CAM000002 (landscape) and CAM000001 (upright). */
function addReadings(db) {
  initPhotoTables(db);
  const now = '2026-10-29T00:00:00Z';
  const env = db.prepare('INSERT INTO envelope_readings(name,file_id,cam,view,bbox_json,turned,size_json,camid,camid_conf,text_json,model,imported_at) VALUES(?,?,?,?,?,?,?,?,?,?,?,?)');
  env.run('CAM000002d', '1CAM000002d'.padEnd(31, 'x'), 'CAM000002', 'dorsal', '[0,263,547,656]', 0, '[1600,1067]', 'CAM000002', 1, '[["CAM000002",1],["Oleria onega",0.97],["♂",1]]', 'reader-v1', now);
  // An upside-down reading of the upright photo: no CAM ID read, so the other photo's reading is shown.
  env.run('CAM000001d', '1CAM000001d'.padEnd(31, 'x'), 'CAM000001', 'dorsal', '[10,20,410,620]', 180, '[1200,1600]', null, null, '[["99EEL0",0.5]]', 'reader-v1', now);
  env.run('CAM000001v', '1CAM000001v'.padEnd(31, 'x'), 'CAM000001', 'ventral', '[10,20,410,620]', 0, '[1200,1600]', 'CAM000009', 0.9, '[["CAM000009",0.9],["♀",1]]', 'reader-v1', now);
  const box = db.prepare('INSERT INTO photo_wing_boxes(name,box_json,conf) VALUES(?,?,?)');
  box.run('CAM000002d', '[0.32,0.28,0.6,0.56]', 0.94);
  box.run('CAM000001d', '[0.25,0.5,0.75,0.75]', 0.9);
  const pred = db.prepare('INSERT INTO photo_predictions(cam,species_json,subspecies_json,sex,sex_conf,sex_supported,sex_species,sex_status,model,sex_model,imported_at) VALUES(?,?,?,?,?,?,?,?,?,?,?)');
  pred.run('CAM000002', '[["Oleria onega",0.849],["Oleria baizana",0.0181],["Hypothyris anastasia",0.004],["Oleria x",0.001]]', '[["Oleria onega lota",0.61],["Oleria onega janarilla",0.18]]', 'female', 0.9936, 1, 'Oleria onega', 'species_supported', 'species-model-D', 'sex-model-1019', now);
  pred.run('CAM000001', '[["Oleria onega",0.9]]', '[["Oleria onega janarilla",0.7]]', 'female', 0.71, 0, 'Oleria onega', 'below_confidence', 'species-model-D', 'sex-model-1019', now);
}

test('gallery_specimens with the photo tables: 600 px photo, wing and envelope crops, model output marked as such', async () => {
  const { tools, store } = await setup();
  addReadings(store.db);
  const changes = () => store.db.prepare('SELECT total_changes() n').get().n;
  const before = changes();
  const out = await tools.run('gallery_specimens', { cam_ids: ['CAM000002', 'CAM000001'], thumbnails: 1 });
  assert.deepEqual(out.specimens.map(s => s.cam_id), ['CAM000002', 'CAM000001']);
  const [two, one] = out.specimens;
  const file2 = '1CAM000002d'.padEnd(31, 'x');

  // The wing box (fractions) made pixels of the photo's 1600x1067, with a 2 % margin; the envelope's pixels.
  const wings = `https://wsrv.nl/?url=${encodeURIComponent(`https://lh3.googleusercontent.com/d/${file2}=w1600`)}&cx=480&cy=266&cw=512&ch=364&w=300&precrop&output=jpg`;
  assert.ok(out.markdown.includes(`[![CAM000002 wings dorsal](${wings})](https://lh3.googleusercontent.com/d/${file2}=w1600)`));
  assert.ok(out.markdown.includes(`&cx=0&cy=257&cw=553&ch=405&w=360&precrop&output=jpg)`));
  assert.ok(out.markdown.includes(`[![CAM000002 dorsal](${previewUrl(file2)})]`));
  // Top 3 with probabilities, as model output; the sex model's Supported call against the database's male.
  assert.deepEqual(two.model.species.map(x => x.name), ['Oleria onega', 'Oleria baizana', 'Hypothyris anastasia']);
  assert.equal(two.model.sex_trusted, true);
  assert.deepEqual(two.differs, ['subspecies', 'sex']);
  assert.match(
    out.markdown,
    /Model output, not checked: species Oleria onega 85 %, Oleria baizana 1\.8 %, Hypothyris anastasia 0\.4 %; subspecies O\. onega lota 61 %, O\. onega janarilla 18 % \*\*\(not the database's\)\*\*; sex female 99 %, a Supported call \*\*\(not the database's\)\*\*\./,
  );
  assert.match(out.markdown, /Envelope as read by the model reader-v1, not checked: CAM000002 \/ Oleria onega \/ ♂/);
  // The models' names once; the JSON without the Markdown's URLs.
  assert.deepEqual(out.models, { species: ['species-model-D'], sex: ['sex-model-1019'], envelope: ['reader-v1'] });
  assert.equal(two.model.species_model, undefined);
  assert.equal(two.photos.dorsal.wings, undefined);
  assert.deepEqual(two.photos.dorsal.size, [1600, 1067]);

  // Past `thumbnails`: one plain line, the models' first answer only, the sex call not trusted.
  assert.deepEqual(one.model.species, { name: 'Oleria onega', p: 0.9 });
  assert.equal(one.model.sex_trusted, false);
  assert.match(one.model.sex_not_trusted, /not a Supported call \(below_confidence\)/);
  // The reading that read a CAM ID, though not this one: flagged.
  assert.equal(one.envelope.photo, 'CAM000001v');
  assert.deepEqual(one.differs, ['envelope_cam']);
  assert.match(out.markdown, /\n\n- \*\*CAM000001\*\* Oleria onega, janarilla, female, Tena, Hollín, 600 m; model: Oleria onega 90 %, female 71 % \(not trusted\) \(not the database's: envelope_cam\) \[d\]\(.*\) \[v\]\(.*\)$/);

  // In full, the upright photo's crop asks Drive for the photo at its own width (1200), not 1600.
  const full = await tools.run('gallery_specimens', { cam_ids: ['CAM000001'] });
  assert.ok(full.markdown.includes(`${encodeURIComponent('=w1200')}&cx=276&cy=776&cw=648&ch=448&w=300&precrop&output=jpg`));
  assert.match(full.markdown, /\(not the database's\)/);
  assert.equal(changes(), before);
});

test('crop URLs: clamped to the photo, made from its own width, none from a bad box or size', () => {
  const url = cropUrl('F1', [-20, 10, 300, 2000], [1600, 1067], { width: 300 });
  assert.equal(url, `https://wsrv.nl/?url=${encodeURIComponent('https://lh3.googleusercontent.com/d/F1=w1600')}&cx=0&cy=10&cw=300&ch=1057&w=300&precrop&output=jpg`);
  // A crop narrower than the width asked is not enlarged.
  assert.match(cropUrl('F1', [0, 0, 100, 100], [1600, 1067], { width: 300 }), /&w=100&/);
  assert.equal(cropUrl('F1', [0, 0, 100, 100], null), null);
  assert.equal(cropUrl('F1', [0, 0, 4, 4], [1600, 1067]), null);
  assert.equal(cropUrl(null, [0, 0, 100, 100], [1600, 1067]), null);
  assert.deepEqual([percent(0.849), percent(0.0181), percent(0.004), percent(0.0435), percent(0.9989), percent(1), percent(undefined)], ['85 %', '1.8 %', '0.4 %', '4.4 %', '99.8 %', '100 %', '?']);
});

test('the sex model is not trusted for a species on its unreliable list, even for a Supported call', () => {
  const prediction = { species_json: '[["Methona confusa",0.76]]', subspecies_json: '[]', sex: 'male', sex_conf: 0.96, sex_supported: 1, sex_species: 'Methona confusa', sex_status: 'species_supported' };
  const m = modelOf(prediction, 'Methona confusa');
  assert.equal(m.sex_trusted, false);
  assert.equal(m.sex_not_trusted, 'the sex model is not trusted for this species');
  assert.match(m.sex_not_trusted_why, /Training labels/);
  // The database's species on the list is enough.
  assert.equal(modelOf({ ...prediction, species_json: '[["Methona grandior",0.6]]', sex_species: null }, 'Methona confusa psamathe').sex_trusted, false);
  assert.equal(modelOf({ ...prediction, species_json: '[["Oleria onega",0.9]]', sex_species: 'Oleria onega' }, 'Oleria onega').sex_trusted, true);
  // Not scored: no sex, its status.
  assert.deepEqual(modelOf({ species_json: '[]', subspecies_json: null, sex: null, sex_status: 'not_scored:no_v_photo' }, 'Oleria onega').sex_status, 'not_scored:no_v_photo');
  // A database sex with a question mark is still that sex; no subspecies in the database is no disagreement.
  const model = { species: [{ name: 'Oleria onega' }], subspecies: [{ name: 'Oleria onega lota' }], sex: 'male' };
  assert.deepEqual(differences({ cam: 'CAM1', species: 'Oleria onega', subspecies: 'NA', sex: 'male?', model, envelope: null }), []);
  assert.deepEqual(differences({ cam: 'CAM1', species: 'Oleria baizana', subspecies: '', sex: 'female', model, envelope: { camid: 'cam1' } }), ['species', 'sex']);
});

test('the Markdown stays compact with many specimens: plain lines past the detailed ones, at most 20', () => {
  const s = i => ({ cam_id: `CAM${String(i).padStart(6, '0')}`, species: 'Oleria onega', subspecies: null, sex: 'male', locality: 'Tena', elevation_m: null, photos: { dorsal: { full: `https://f/${i}`, preview: `https://p/${i}` }, ventral: null }, model: null, envelope: null });
  const md = specimensMarkdown(Array.from({ length: 30 }, (_, i) => s(i)), 2);
  assert.equal(md.match(/!\[/g).length, 2);
  assert.equal(md.match(/^- \*\*CAM/gm).length, 20);
  assert.match(md, /- and 8 more \(in `specimens` and the gallery link\)$/);
});
