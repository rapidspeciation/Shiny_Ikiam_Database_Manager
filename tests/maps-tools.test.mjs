import test from 'node:test';
import assert from 'node:assert/strict';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { mcpTools } from '../server/assistant.mjs';
import { MAPS_TOOL_NAMES, createMapsTools, dateOf, galleryUrl, mapsUrl, yearOf } from '../server/maps-tools.mjs';

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
      { butterfly_taxon: 'Oleria onega', host_taxon_name: 'Solanum', host_plant_family: 'Solanaceae', host_plant_genus: 'Solanum', host_plant_species: null, host_plant_name_verbatim: 'Solanum', host_id_level: 'genus', evidence_level: 'literature', confidence: 'medium', citation_for_ui: 'Beccaloni et al. 2008', doi_or_url: 'https://example.org/beccaloni', source_refs: 'Ecuador: [Drummond, 1986]' },
      { butterfly_taxon: 'Oleria onega janarilla', host_taxon_name: 'Solanum mite', host_plant_family: 'Solanaceae', host_plant_genus: 'Solanum', host_plant_species: 'Solanum mite', host_plant_name_verbatim: 'Solanum mite', host_id_level: 'species', evidence_level: 'direct', confidence: 'high', citation_for_ui: 'Willmott & Mallet 2004', doi_or_url: 'https://doi.org/10.1/x' },
      { butterfly_taxon: 'Oleria onega', host_taxon_name: 'Cestrum', host_plant_family: 'Solanaceae', host_plant_genus: 'Cestrum', host_plant_species: null, host_plant_name_verbatim: 'Cestrum', host_id_level: 'genus', evidence_level: 'literature', confidence: 'low', citation_for_ui: 'Beccaloni et al. 2008' },
      { butterfly_taxon: 'Mechanitis menapis', host_taxon_name: 'Solanum', host_plant_family: 'Solanaceae', host_plant_genus: 'Solanum', host_plant_species: null, host_plant_name_verbatim: 'Solanum', host_id_level: 'genus', evidence_level: 'literature', confidence: 'high', citation_for_ui: 'Beccaloni et al. 2008' },
      { butterfly_taxon: 'Mechanitis menapis', host_taxon_name: 'Solanum x', host_plant_family: 'Solanaceae', host_plant_genus: 'Solanum', host_plant_species: 'Solanum x', host_plant_name_verbatim: 'Solanum x', host_id_level: 'species', evidence_level: 'needs_check', confidence: 'needs_check', citation_for_ui: 'Someone' },
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
  ].map(([cam, SPECIES, Subspecies_Form, Sex, Collection_location, Country, ELEVATION], i) => ({
    row: i + 2,
    values: { CAM_ID: cam, SPECIES, Subspecies_Form, Sex, Collection_location, Country, ELEVATION, Release_Collect: 'Collected_Preserved' },
  })),
  Photo_links: ['CAM000001d', 'CAM000001v', 'CAM000002d', 'CAM000004v', 'CAM000005d', 'CAM000005d2'].map((name, i) => ({
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

test('the four tools are offered to the chats', () => {
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
  assert.match(colombia.map, /genus=Oleria/);
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

test('gallery_specimens: filters, gallery links, photo URLs and thumbnails; nothing written', async () => {
  const { tools, store } = await setup();
  const changes = () => store.db.prepare('SELECT total_changes() n').get().n;
  const before = changes();
  const males = (await tools.run('gallery_specimens', { species: ['oleria onega'], sex: 'male' }));
  // CAM000003 has no photo.
  assert.deepEqual(males.specimens.map(s => s.cam_id), ['CAM000002']);
  assert.equal(males.gallery, `${GALLERY}?view=1&species=Oleria%20onega&sex=male`);
  assert.equal(males.specimens[0].photos.dorsal.file, '1CAM000002d'.padEnd(31, 'x'));
  assert.match(males.specimens[0].photos.dorsal.thumbnail, /^https:\/\/wsrv\.nl\/\?url=https%3A%2F%2Fdrive\.google\.com%2Fuc%3Fid%3D1CAM000002d/);
  assert.match(males.markdown, /^\[!\[CAM000002 d\]\(https:\/\/wsrv\.nl\/.*\)\]\(https:\/\/lh3\.googleusercontent\.com\/d\/1CAM000002d\w+=w1600\)/);
  assert.equal((await tools.run('gallery_specimens', { species: ['Oleria onega'], sex: 'male', with_photos: false })).found, 2);

  const mindo = (await tools.run('gallery_specimens', { locality: 'mindo', elevation_min: 1400 }));
  assert.deepEqual(mindo.specimens.map(s => s.cam_id), ['CAM000005', 'CAM000004']);
  assert.equal(mindo.gallery, `${GALLERY}search?view=1&camids=CAM000005%20CAM000004`);
  // The plain photo, not its extra view.
  assert.equal(mindo.specimens[0].photos.dorsal.name, 'CAM000005d.JPG');
  assert.equal(mindo.specimens[1].photos.dorsal, null);

  const byId = (await tools.run('gallery_specimens', { cam_ids: ['cam000001'], thumbnails: 1 }));
  assert.equal(byId.markdown.match(/!\[/g).length, 2);
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
