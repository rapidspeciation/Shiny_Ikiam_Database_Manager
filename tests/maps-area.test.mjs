import test from 'node:test';
import assert from 'node:assert/strict';
import { existsSync, mkdtempSync, readFileSync, readdirSync, statSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { BOUNDARY_URLS, createAdminAreas } from '../server/admin-areas.mjs';
import { TAGS_FILE, createMapsTools, sheetPicks } from '../server/maps-tools.mjs';
import { MAX_PX, creditOf, jpegSize, licenceName, proxyUrl, sizedUrl } from '../server/plant-photos.mjs';

const MAPS = 'https://maps.test/ithomiini_maps/';
// geoBoundaries' simplified ECU files, cut to Napo, Sucumbíos, Pastaza and Orellana (coordinates to 0.001°).
const BOUNDARIES = {
  [BOUNDARY_URLS.ADM2]: JSON.parse(readFileSync(new URL('./fixtures/ecuador-admin/ADM2.geojson', import.meta.url), 'utf8')),
  [BOUNDARY_URLS.ADM1]: JSON.parse(readFileSync(new URL('./fixtures/ecuador-admin/ADM1.geojson', import.meta.url), 'utf8')),
};
// About 2 km either side of Napo's borders.
const NAPO_SOUTH = { lat: -1.18, lng: -77.8 };
const PASTAZA = { lat: -1.22, lng: -77.8 };
const NAPO_NORTH = { lat: -0.11, lng: -77.6 };
const SUCUMBIOS = { lat: -0.07, lng: -77.6 };
const TENA = { lat: -0.993, lng: -77.814 };
// About 30 km south of Napo, in Pastaza; and Bogotá.
const PUYO = { lat: -1.49, lng: -78.0 };
const BOGOTA = { lat: 4.6, lng: -74.1 };

/** A JPEG header with its frame size: all jpegSize reads. */
function jpeg(width, height) {
  const b = Buffer.alloc(30);
  b.set([0xff, 0xd8, 0xff, 0xe0, 0x00, 0x04, 0x00, 0x00, 0xff, 0xc0, 0x00, 0x11, 0x08]);
  b.writeUInt16BE(height, 13);
  b.writeUInt16BE(width, 15);
  return b;
}

const GBIF = {
  1: { key: 1, species: 'Solanum mitis', locality: 'Tena', stateProvince: 'Napo', country: 'Ecuador', year: 2019, recordedBy: 'Ana Pérez', license: 'http://creativecommons.org/licenses/by/4.0/legalcode', references: 'https://www.inaturalist.org/observations/101', institutionCode: 'iNaturalist', media: [{ identifier: 'https://inaturalist-open-data.s3.amazonaws.com/photos/11/original.jpg', creator: 'Ana Pérez', rightsHolder: 'Ana Pérez', license: 'http://creativecommons.org/licenses/by/4.0/' }] },
  2: { key: 2, species: 'Solanum mitis', verbatimLocality: 'Lumbaqui, Sucumbíos, EC', country: 'Ecuador', year: 2022, media: [{ identifier: 'https://inaturalist-open-data.s3.amazonaws.com/photos/12/original.jpg', creator: 'Luis', license: 'http://creativecommons.org/licenses/by-nc/4.0/' }] },
  3: { key: 3, species: 'Cestrum megalophyllum', locality: 'Puyo', stateProvince: 'Pastaza', country: 'Ecuador', year: 2005, media: [{ identifier: 'https://inaturalist-open-data.s3.amazonaws.com/photos/13/original.jpg', creator: 'Rosa', license: 'http://creativecommons.org/licenses/by-nc/4.0/' }] },
};
const point = (id, name, { lat, lng }, date) => ({ id, scientific_name: name, genus: name.split(' ')[0], lat, lng, source: 'iNaturalist', collection_location: id, observation_date: date, country: 'Ecuador' });
const FILES = {
  'data_manifest.json': { generated_at: '2026-09-26T04:34:23Z', sources: { iNaturalist: { file: 'map_points_inaturalist.json' } } },
  'map_points_inaturalist.json': [
    point('Tena', 'Oleria onega', TENA, '2020-01-01'),
    point('Napo south', 'Oleria onega', NAPO_SOUTH, '2021-01-01'),
    point('Sucumbíos', 'Oleria onega', SUCUMBIOS, '2022-01-01'),
    point('Pastaza', 'Mechanitis menapis', PASTAZA, '2022-01-01'),
  ],
  'host_plants/host_plant_associations.json': {
    metadata: { generated_at: '2026-06-16T14:02:20+00:00' },
    associations: [
      { butterfly_taxon: 'Oleria onega', host_taxon_slug: 'species_solanum_mite', host_taxon_name: 'Solanum mite', host_plant_family: 'Solanaceae', host_plant_genus: 'Solanum', host_plant_species: 'Solanum mite', host_id_level: 'species', evidence_level: 'direct', confidence: 'high', citation_for_ui: 'Willmott & Mallet 2004', doi_or_url: 'https://doi.org/10.1/x' },
      { butterfly_taxon: 'Oleria onega', host_taxon_slug: 'species_solanum_mitis', host_taxon_name: 'Solanum mitis', host_plant_family: 'Solanaceae', host_plant_genus: 'Solanum', host_plant_species: 'Solanum mitis', host_id_level: 'species', evidence_level: 'literature', confidence: 'medium', citation_for_ui: 'Beccaloni et al. 2008' },
      { butterfly_taxon: 'Oleria onega', host_taxon_slug: 'species_cestrum_megalophyllum', host_taxon_name: 'Cestrum megalophyllum', host_plant_family: 'Solanaceae', host_plant_genus: 'Cestrum', host_plant_species: 'Cestrum megalophyllum', host_id_level: 'species', evidence_level: 'literature', confidence: 'high', citation_for_ui: 'Beccaloni et al. 2008' },
      { butterfly_taxon: 'Oleria onega', host_taxon_slug: 'species_far_away', host_taxon_name: 'Far away', host_plant_family: 'Solanaceae', host_plant_genus: 'Far', host_plant_species: 'Far away', host_id_level: 'species', evidence_level: 'direct', confidence: 'high', citation_for_ui: 'Beccaloni et al. 2008' },
    ],
  },
  'host_plants/host_plant_layers_manifest.json': {
    metadata: {
      generated_at: '2026-06-13T03:47:42+00:00',
      occurrence_refs_by_taxon: {
        species_solanum_mite: 'refs/mite.json',
        // A synonym kept apart by the review: the same GBIF records.
        species_solanum_mitis: 'refs/mitis.json',
        species_cestrum_megalophyllum: 'refs/cestrum.json',
        species_far_away: 'refs/far.json',
      },
      gallery_indexes_by_taxon: { species_solanum_mite: 'gallery/mite/index.json', species_solanum_mitis: 'gallery/mite/index.json', species_cestrum_megalophyllum: 'gallery/cestrum/index.json' },
    },
    taxa: [
      { slug: 'species_solanum_mite', canonical_name: 'Solanum mite', rank: 'species', gbif_resolution: { canonical_name: 'Solanum mitis' } },
      { slug: 'species_solanum_mitis', canonical_name: 'Solanum mitis', rank: 'species' },
      { slug: 'species_cestrum_megalophyllum', canonical_name: 'Cestrum megalophyllum', rank: 'species' },
      { slug: 'species_far_away', canonical_name: 'Far away', rank: 'species' },
    ],
  },
  'host_plants/refs/mite.json': { metadata: { total_records: 4 }, chunks: { 'chunks/c1.json': [0, 1, 2, 3] } },
  'host_plants/refs/mitis.json': { metadata: { total_records: 4 }, chunks: { 'chunks/c1.json': [0, 1, 2, 3] } },
  'host_plants/refs/cestrum.json': { metadata: { total_records: 2 }, chunks: { 'chunks/c2.json': [0, 1] } },
  'host_plants/refs/far.json': { metadata: { total_records: 1 }, chunks: { 'chunks/c2.json': [2] } },
  'host_plants/chunks/c1.json': {
    records: [
      { ...NAPO_SOUTH, eventDate: '2019-05-01', basisOfRecord: 'HUMAN_OBSERVATION' },
      { ...TENA, eventDate: '2021-07-02', basisOfRecord: 'PRESERVED_SPECIMEN' },
      { ...SUCUMBIOS, eventDate: '2022-01-01', basisOfRecord: 'HUMAN_OBSERVATION' },
      { ...PASTAZA, eventDate: '2005-01-01', basisOfRecord: 'PRESERVED_SPECIMEN' },
    ],
  },
  'host_plants/chunks/c2.json': {
    records: [
      // Cestrum: only outside Napo (2 and about 30 km away, in Pastaza); Far away: in Colombia.
      { ...PASTAZA, eventDate: '2005-01-01' },
      { ...PUYO, eventDate: '2011-01-01' },
      { ...BOGOTA, eventDate: '1999-01-01' },
    ],
  },
  'host_plants/gallery/mite/index.json': { pages: ['gallery/mite/p1.json'] },
  'host_plants/gallery/mite/p1.json': {
    items: [
      // Outside Napo (field), then inside (field): the inside one is chosen.
      { image_url: 'https://inaturalist-open-data.s3.amazonaws.com/photos/12/original.jpg', observation_url: 'https://www.gbif.org/occurrence/2', ...SUCUMBIOS, observation_date: '2022-01-01', photo_context: 'field', source: 'iNaturalist' },
      { image_url: 'https://inaturalist-open-data.s3.amazonaws.com/photos/11/original.jpg', observation_url: 'https://www.gbif.org/occurrence/1', ...TENA, observation_date: '2019-05-01', photo_context: 'field', source: 'iNaturalist' },
    ],
  },
  'host_plants/gallery/cestrum/index.json': { pages: ['gallery/cestrum/p1.json'] },
  'host_plants/gallery/cestrum/p1.json': {
    items: [{ image_url: 'https://inaturalist-open-data.s3.amazonaws.com/photos/13/original.jpg', observation_url: 'https://www.gbif.org/occurrence/3', ...PUYO, observation_date: '2011-01-01', photo_context: 'field', source: 'iNaturalist' }],
  },
  'country_boundaries.json': {
    countries: [
      { name: 'Ecuador', bbox: [-81, -5, -75, 1.5], polygons: [[[[-81, -5], [-75, -5], [-75, 1.5], [-81, 1.5], [-81, -5]]]] },
      { name: 'Colombia', bbox: [-80, 1.5, -66, 12], polygons: [[[[-80, 1.5], [-66, 1.5], [-66, 12], [-80, 12], [-80, 1.5]]]] },
    ],
  },
};

/** The maps site, the boundary files, GBIF's API and the photos (photo 13 too big: given again by the proxy). */
function fakeWeb() {
  const calls = [];
  const fetchImpl = async url => {
    calls.push(url);
    if (BOUNDARIES[url]) return Response.json(BOUNDARIES[url]);
    const u = new URL(url);
    if (u.host === 'api.gbif.org') {
      const r = GBIF[u.pathname.split('/').pop()];
      return r ? Response.json(r) : new Response('no', { status: 404 });
    }
    if (u.host === 'inaturalist-open-data.s3.amazonaws.com') return new Response(u.pathname.includes('/13/') ? jpeg(4000, 3000) : jpeg(2048, 1536));
    if (u.host === 'wsrv.nl') return new Response(jpeg(2048, 1536));
    const path = u.pathname.replace('/ithomiini_maps/data/', '');
    return path in FILES ? Response.json(FILES[path]) : new Response('missing', { status: 404 });
  };
  return { calls, fetchImpl };
}

test('Napo by its outline: a point just inside, one just outside in Pastaza or Sucumbíos; downloaded once', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'admin-areas-'));
  const web = fakeWeb();
  const areas = createAdminAreas({ dir, fetchImpl: web.fetchImpl });
  const napo = await areas.area({ province: 'Napo' });
  assert.equal(napo.error, undefined, napo.error);
  assert.deepEqual([napo.level, napo.name], ['province', 'Napo']);
  assert.deepEqual(napo.cantons, ['Archidona', 'Carlos Julio Arosemena Tola', 'El Chaco', 'Quijos', 'Tena']);
  for (const p of [NAPO_SOUTH, NAPO_NORTH, TENA]) assert.equal(napo.test(p.lng, p.lat), true, JSON.stringify(p));
  assert.equal(napo.test(PASTAZA.lng, PASTAZA.lat), false);
  assert.equal(napo.test(SUCUMBIOS.lng, SUCUMBIOS.lat), false);
  // Napo's box holds both: the outline does not.
  const [w, s, e, n] = napo.bbox;
  for (const p of [PASTAZA, SUCUMBIOS]) assert.ok(p.lng >= w && p.lng <= e && p.lat >= s && p.lat <= n);
  // How far: 0 inside, about 2 km just across, about 30 km at Puyo.
  assert.equal(napo.distanceKm(TENA.lng, TENA.lat), 0);
  assert.ok(Math.abs(napo.distanceKm(PASTAZA.lng, PASTAZA.lat) - 2) < 0.5);
  assert.ok(Math.abs(napo.distanceKm(SUCUMBIOS.lng, SUCUMBIOS.lat) - 2) < 0.5);
  assert.ok(napo.distanceKm(PUYO.lng, PUYO.lat) > 20 && napo.distanceKm(PUYO.lng, PUYO.lat) < 40);
  assert.equal(web.calls.length, 2);
  assert.ok(existsSync(join(dir, 'ecuador-admin-v1.json')));
  // Asked again, and by another process: no download.
  await areas.area('Napo');
  const again = createAdminAreas({ dir, fetchImpl: () => assert.fail('downloaded again') });
  assert.equal((await again.area('cantón Tena')).test(TENA.lng, TENA.lat), true);
  assert.equal(web.calls.length, 2);
});

test('area names: province before canton, a canton by name, unknown names, a download that fails', async () => {
  const areas = createAdminAreas({ fetchImpl: fakeWeb().fetchImpl });
  assert.equal((await areas.area('Pastaza')).level, 'province');
  assert.match((await areas.area('Pastaza')).also, /also a canton/);
  assert.deepEqual(Object.values(await areas.area({ canton: 'Pastaza' })).slice(0, 3), ['canton', 'Pastaza', 'Pastaza']);
  assert.equal((await areas.area({ canton: 'Tena' })).test(NAPO_NORTH.lng, NAPO_NORTH.lat), false);
  assert.equal((await areas.area('provincia de Sucumbios')).name, 'Sucumbíos');
  // Names with «de» in them.
  assert.deepEqual(Object.values(await areas.area({ canton: 'La Joya de los Sachas' })).slice(0, 3), ['canton', 'La Joya de los Sachas', 'Orellana']);
  assert.equal((await areas.area('cantón La Joya de los Sachas, provincia de Orellana')).name, 'La Joya de los Sachas');
  assert.match((await areas.area('Narnia')).error, /No province or canton/);
  const down = createAdminAreas({ fetchImpl: async () => new Response('down', { status: 503 }) });
  assert.match((await down.area('Napo')).error, /outlines could not be read: .*503/);
});

test('photo helpers: about 2048 px, licences, JPEG size, the credit from a GBIF record', () => {
  assert.equal(MAX_PX, 2048);
  assert.equal(sizedUrl('https://inaturalist-open-data.s3.amazonaws.com/photos/11/medium.jpg'), 'https://inaturalist-open-data.s3.amazonaws.com/photos/11/original.jpg');
  assert.equal(sizedUrl('https://herbarium.test/sheet.jpg'), proxyUrl('https://herbarium.test/sheet.jpg'));
  assert.match(proxyUrl('https://herbarium.test/sheet.jpg'), /&w=2048&h=2048&fit=inside/);
  assert.equal(licenceName('http://creativecommons.org/licenses/by-nc/4.0/'), 'CC BY-NC 4.0');
  assert.equal(licenceName('http://creativecommons.org/publicdomain/zero/1.0/legalcode'), 'CC0');
  assert.deepEqual(jpegSize(jpeg(1024, 768)), { width: 1024, height: 768 });
  assert.equal(jpegSize(Buffer.from('not a jpeg')), null);
  const c = creditOf(GBIF[1], 'https://inaturalist-open-data.s3.amazonaws.com/photos/11/original.jpg');
  assert.deepEqual([c.photographer, c.licence, c.place, c.year, c.link], ['Ana Pérez', 'CC BY 4.0', 'Tena, Napo, Ecuador', 2019, 'https://www.inaturalist.org/observations/101']);
  // Another size of the same iNaturalist photo is the same photo; a photo the record does not list gets no
  // photographer or licence (never the record's or another photo's).
  assert.equal(creditOf(GBIF[1], 'https://inaturalist-open-data.s3.amazonaws.com/photos/11/large.jpg').photographer, 'Ana Pérez');
  const other = creditOf(GBIF[1], 'https://inaturalist-open-data.s3.amazonaws.com/photos/99/original.jpg');
  assert.deepEqual([other.matched, other.photographer, other.licence], [false, null, null]);
  // iNaturalist's ", EC" is dropped.
  assert.equal(creditOf(GBIF[2], '').place, 'Lumbaqui, Sucumbíos, Ecuador');
});

test('maps_localities and maps_species with an area: the records inside the outline only', async () => {
  const web = fakeWeb();
  const tools = createMapsTools({ fetchImpl: web.fetchImpl, mapsBase: MAPS });
  const sites = await tools.run('maps_localities', { taxa: ['Oleria onega'], area: { province: 'Napo' }, site_km: 1 });
  assert.equal(sites.records.in_region, 2);
  assert.deepEqual(sites.sites.map(s => s.site).sort(), ['Napo south', 'Tena']);
  assert.equal(new URL(sites.map).searchParams.get('country'), 'Ecuador');
  const sp = await tools.run('maps_species', { area: 'Pastaza' });
  assert.deepEqual(sp.taxa.map(t => t.name), ['Mechanitis menapis']);
  assert.match((await tools.run('maps_species', { area: 'Narnia' })).error, /No province or canton/);
});

test('host_plants with an area: in the area, only nearby (candidates) or far, and photo files labelled so', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'maps-cache-'));
  const web = fakeWeb();
  const tools = createMapsTools({ fetchImpl: web.fetchImpl, mapsBase: MAPS, cacheDir: dir });
  const out = await tools.run('host_plants', { butterfly: 'Oleria onega', area: { province: 'Napo' }, send_photos: 5, auto_pick: true, lang: 'es' });
  assert.equal(out.error, undefined, out.error);
  const plant = name => out.plants.find(p => p.plant === name);
  // Solanum mite: 2 records inside (the Pastaza and Sucumbíos ones 2 km out are nearby, not inside).
  assert.equal(plant('Solanum mite').records, 2);
  assert.equal(plant('Solanum mite').status, 'in_area');
  assert.equal(plant('Solanum mite').nearby.records, 2);
  assert.deepEqual(plant('Solanum mite').nearby.by_region, { Sucumbíos: 1, Pastaza: 1 });
  assert.equal(plant('Solanum mite').same_records_as, 'Solanum mitis');
  assert.equal(plant('Solanum mitis').same_records_as, 'Solanum mite');
  // Cestrum: none inside, 2 within 50 km, the nearest about 2 km away in Pastaza.
  const cestrum = plant('Cestrum megalophyllum');
  assert.deepEqual([cestrum.records, cestrum.status, cestrum.nearby.records], [0, 'nearby', 2]);
  assert.deepEqual(cestrum.nearby.years, { first: 2005, last: 2011, distinct: 2 });
  assert.equal(cestrum.nearby.nearest.region, 'Pastaza');
  assert.ok(cestrum.nearby.nearest.km < 3);
  assert.equal(plant('Far away').status, 'far');
  assert.equal(plant('Far away').nearby.nearest, 'none within 300 km');
  assert.deepEqual(out.in_area, ['Solanum mite (= Solanum mitis) (2)']);
  assert.deepEqual(out.nearby_only, ['Cestrum megalophyllum (2 within 50 km)']);
  assert.deepEqual(out.not_near, ['Far away']);
  // Ecuador asked beside the province changes nothing.
  const withCountry = await tools.run('host_plants', { butterfly: 'Oleria onega', area: { province: 'Napo' }, country: ['Ecuador'] });
  assert.deepEqual(withCountry.nearby_only, ['Cestrum megalophyllum (2 within 50 km)']);
  // A smaller reach: Puyo (about 30 km) drops out.
  const near10 = await tools.run('host_plants', { butterfly: 'Oleria onega', area: 'Napo', nearby_km: 10 });
  assert.equal(near10.plants.find(p => p.plant === 'Cestrum megalophyllum').nearby.records, 1);

  // The photos: the plant in Napo first, then the candidate, each labelled; about 2048 px; one for the synonyms.
  assert.equal(out.images.length, 2);
  const [inNapo, candidate] = out.images;
  assert.deepEqual(Object.keys(inNapo).slice(0, 4), ['path', 'caption', 'plant', 'status']);
  for (const im of out.images) {
    assert.ok(existsSync(im.path) && im.path.startsWith(dir));
    assert.ok(Math.max(im.width, im.height) <= MAX_PX);
  }
  assert.equal(inNapo.caption, '_Solanum mite_ (= Solanum mitis) — registrada en Napo (2 registros)\nTena, Napo, Ecuador, 2019\nFoto: Ana Pérez (CC BY 4.0)\nhttps://www.inaturalist.org/observations/101');
  assert.equal(inNapo.url, 'https://inaturalist-open-data.s3.amazonaws.com/photos/11/original.jpg');
  assert.equal(candidate.status, 'nearby');
  assert.match(candidate.caption, /^_Cestrum megalophyllum_ — CANDIDATA: sin registros en Napo, 2 a menos de 50 km\nPuyo, Pastaza, Ecuador, 2005 \(foto fuera de Napo: Pastaza, Pastaza\)\nFoto: Rosa \(CC BY-NC 4\.0\)\n/);
  // Its original was 4000 px: the proxy's 2048 px copy instead.
  assert.match(candidate.url, /^https:\/\/wsrv\.nl\/.*w=2048/);
  assert.equal(out.images_block, `[images]\n${inNapo.path} | ${inNapo.caption.split('\n').join(' — ')}\n${candidate.path} | ${candidate.caption.split('\n').join(' — ')}\n[/images]`);
  // Photo files asked: no thumbnails, a small answer.
  assert.equal(out.markdown, '');
  assert.ok(JSON.stringify(out).length < 12000, JSON.stringify(out).length);

  // Another process with the same folder: records, outlines, photos and credits from the disk.
  const web2 = fakeWeb();
  const again = await createMapsTools({ fetchImpl: web2.fetchImpl, mapsBase: MAPS, cacheDir: dir }).run('host_plants', { butterfly: 'Oleria onega', area: 'Napo', send_photos: 5, auto_pick: true });
  assert.equal(again.plant_data.records_from, 'disk');
  assert.ok(!web2.calls.some(u => /chunks|api\.gbif|amazonaws|wsrv|geoBoundaries/.test(u)), web2.calls.join('\n'));
  assert.match(again.images[1].caption, /CANDIDATE: no record in Napo, 2 within 50 km/);
  assert.equal(readdirSync(join(dir, 'plant-photos', 'photos')).length, 2);
});

test('host_plants with send_photos: a fallback photo is never sent; a photo without its credit gives way to the next', async () => {
  const web = fakeWeb();
  const page = 'host_plants/gallery/mite/p1.json';
  const saved = FILES[page];
  // A record without a photo shown with a photo from elsewhere, placed in Tena: skipped.
  FILES[page] = { items: [{ ...saved.items[1], image_url: 'https://inaturalist-open-data.s3.amazonaws.com/photos/50/original.jpg', image_is_fallback: true }, ...saved.items] };
  try {
    // GBIF does not answer for the Tena photo's record: the Sucumbíos photo, with its credit, goes instead.
    const noGbif1 = async url => (/occurrence\/1$/.test(url) ? new Response('down', { status: 503 }) : web.fetchImpl(url));
    const out = await createMapsTools({ fetchImpl: noGbif1, mapsBase: MAPS }).run('host_plants', { butterfly: 'Oleria onega', area: 'Napo', send_photos: 1, auto_pick: true });
    assert.equal(out.images.length, 1);
    assert.equal(out.images[0].source_url, 'https://inaturalist-open-data.s3.amazonaws.com/photos/12/original.jpg');
    assert.equal(out.images[0].credit.photographer, 'Luis');
    assert.match(out.images_note, /could not be fetched or credited/);
  } finally {
    FILES[page] = saved;
  }
});

test('host_plants with send_photos: a photo that cannot be had gives way to the next', async () => {
  const web = fakeWeb();
  const broken = async url => (/photos\/11\//.test(url) ? new Response('gone', { status: 404 }) : web.fetchImpl(url));
  const out = await createMapsTools({ fetchImpl: broken, mapsBase: MAPS }).run('host_plants', { butterfly: 'Oleria onega', area: 'Napo', send_photos: 1, auto_pick: true });
  assert.equal(out.images.length, 1);
  assert.equal(out.images[0].photo_in_area, false);
  assert.match(out.images[0].caption, /photo outside Napo: Gonzalo Pizarro, Sucumbíos/);
  assert.match(out.images_note, /1 photo/);
});

test('host_plants with send_photos: a living plant before a herbarium sheet, even one taken in the area; a sheet labelled', async () => {
  const web = fakeWeb();
  const mite = 'host_plants/gallery/mite/p1.json';
  const cestrum = 'host_plants/gallery/cestrum/p1.json';
  const saved = [FILES[mite], FILES[cestrum]];
  FILES[mite] = {
    items: [
      { image_url: 'https://inaturalist-open-data.s3.amazonaws.com/photos/11/original.jpg', observation_url: 'https://www.gbif.org/occurrence/1', ...TENA, observation_date: '2019-05-01', photo_context: 'preserved', basisOfRecord: 'PRESERVED_SPECIMEN', source: 'Herbario' },
      saved[0].items[0],
    ],
  };
  FILES[cestrum] = { items: [{ ...saved[1].items[0], photo_context: 'preserved', basisOfRecord: 'PRESERVED_SPECIMEN' }] };
  try {
    const out = await createMapsTools({ fetchImpl: web.fetchImpl, mapsBase: MAPS }).run('host_plants', { butterfly: 'Oleria onega', area: 'Napo', send_photos: 2, auto_pick: true, lang: 'es' });
    // The Sucumbíos field photo, not the herbarium sheet from Tena.
    assert.equal(out.images[0].source_url, 'https://inaturalist-open-data.s3.amazonaws.com/photos/12/original.jpg');
    assert.equal(out.images[0].herbarium, false);
    // Cestrum has only a sheet: sent, and said so.
    assert.equal(out.images[1].herbarium, true);
    assert.match(out.images[1].caption, /^\[Ejemplar de herbario\] _Cestrum megalophyllum_/);
    assert.match(out.images[1].caption, /ejemplar de herbario \(no hay foto de la planta viva\)/);
  } finally {
    [FILES[mite], FILES[cestrum]] = saved;
  }
});

test('host_plants with send_photos and photo tags: leaves or the whole plant first, a flower as a second photo', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'maps-cache-'));
  writeFileSync(join(dir, TAGS_FILE), JSON.stringify({ model: 'test', photos: { 11: ['flower', 0.02, 0.95], 12: ['leaves', 0.97, 0.01], 13: ['habit', 0.9, 0.05] } }));
  const out = await createMapsTools({ fetchImpl: fakeWeb().fetchImpl, mapsBase: MAPS, cacheDir: dir }).run('host_plants', { butterfly: 'Oleria onega', area: 'Napo', send_photos: 3, auto_pick: true, lang: 'es' });
  // Solanum mite: its leaves (Sucumbíos) before its flower taken in Tena; Cestrum: the whole plant; then the flower.
  assert.deepEqual(out.images.map(im => [im.plant, im.view, im.second ?? false]), [
    ['Solanum mite', 'leaves', false],
    ['Cestrum megalophyllum', 'habit', false],
    ['Solanum mite', 'flower', true],
  ]);
  assert.match(out.images[0].caption, /^_Solanum mite_ \(= Solanum mitis\) — registrada en Napo \(2 registros\) · hojas\n/);
  assert.match(out.images[2].caption, / · flor\n/);
  // Fewer photos than plants and flowers: the flower waits.
  const two = await createMapsTools({ fetchImpl: fakeWeb().fetchImpl, mapsBase: MAPS, cacheDir: dir }).run('host_plants', { butterfly: 'Oleria onega', area: 'Napo', send_photos: 2, auto_pick: true });
  assert.deepEqual(two.images.map(im => im.view), ['leaves', 'habit']);
});

test('photo tags: a leaves guess under the threshold is no view; a living flower before a herbarium sheet; a failed leaf photo gives way to a flower', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'maps-cache-'));
  const mite = 'host_plants/gallery/mite/p1.json';
  const saved = FILES[mite];
  const run = (fetchImpl = fakeWeb().fetchImpl) => createMapsTools({ fetchImpl, mapsBase: MAPS, cacheDir: dir }).run('host_plants', { butterfly: 'Oleria onega', area: 'Napo', send_photos: 1, auto_pick: true });
  try {
    // 11 (Tena) a weak «leaves» (0.4), 12 (Sucumbíos) a sure «habit»: the sure one first.
    writeFileSync(join(dir, TAGS_FILE), JSON.stringify({ photos: { 11: ['leaves', 0.4, 0.3], 12: ['habit', 0.95, 0.01] } }));
    assert.equal((await run()).images[0].source_url, 'https://inaturalist-open-data.s3.amazonaws.com/photos/12/original.jpg');
    // A living plant's flower (12) against an untagged herbarium sheet (11): the flower.
    FILES[mite] = { items: [{ ...saved.items[1], photo_context: 'preserved', basisOfRecord: 'PRESERVED_SPECIMEN' }, saved.items[0]] };
    writeFileSync(join(dir, TAGS_FILE), JSON.stringify({ photos: { 12: ['flower', 0.01, 0.97] } }));
    const living = (await run()).images[0];
    assert.deepEqual([living.source_url.includes('/12/'), living.herbarium, living.view], [true, false, 'flower']);
    // The leaf photo (11, field) cannot be had: the flower (12) is sent instead of nothing.
    FILES[mite] = saved;
    writeFileSync(join(dir, TAGS_FILE), JSON.stringify({ photos: { 11: ['leaves', 0.97, 0.01], 12: ['flower', 0.01, 0.97] } }));
    const web = fakeWeb();
    const out = await run(async url => (/photos\/11\//.test(url) ? new Response('gone', { status: 404 }) : web.fetchImpl(url)));
    assert.equal(out.images[0].view, 'flower');
  } finally {
    FILES[mite] = saved;
  }
});

const pillow = (await import('node:child_process')).spawnSync('python3', ['-c', 'import PIL'], { stdio: 'ignore' }).status === 0;
test('contact sheets: host_plants gives numbered sheets of candidate photos; send_sheet_photos sends the tiles chosen', { skip: !pillow && 'no python3 with Pillow' }, async () => {
  const dir = mkdtempSync(join(tmpdir(), 'maps-cache-'));
  const tools = createMapsTools({ fetchImpl: fakeWeb().fetchImpl, mapsBase: MAPS, cacheDir: dir });
  const out = await tools.run('host_plants', { butterfly: 'Oleria onega', area: 'Napo', photo_sheet: true, lang: 'es' });
  assert.equal(out.photo_sheets.length, 1);
  const [sheet] = out.photo_sheets;
  assert.ok(existsSync(sheet.path) && statSync(sheet.path).size > 1000);
  // The plant in Napo first, its photo in Tena before the one in Sucumbíos; the candidate said so.
  assert.deepEqual(sheet.tiles, ['1: Solanum mite — Tena · 2019', '2: Solanum mite — 2 km, Sucumbíos · 2022', '3: Cestrum megalophyllum (candidata) — 28 km, Pastaza · 2011']);
  assert.match(out.photo_sheet_note, /send_sheet_photos/);
  assert.equal(out.markdown, '');
  // Two photos of one GBIF record: the record's second photo comes after the other records' first.
  const page = 'host_plants/gallery/mite/p1.json';
  const saved = FILES[page];
  // A second photo of the Tena record (nearer than Sucumbíos): it waits behind the Sucumbíos record's first.
  FILES[page] = { items: [...saved.items, { ...saved.items[1], image_url: 'https://inaturalist-open-data.s3.amazonaws.com/photos/14/original.jpg' }] };
  try {
    const dup = await createMapsTools({ fetchImpl: fakeWeb().fetchImpl, mapsBase: MAPS, cacheDir: mkdtempSync(join(tmpdir(), 'maps-cache-')) }).run('host_plants', { butterfly: 'Oleria onega', area: 'Napo', photo_sheet: true });
    assert.deepEqual(dup.photo_sheets[0].tiles.slice(0, 3).map(t => t.replace(/^\d+: Solanum mite — /, '')), ['Tena · 2019', '2 km, Sucumbíos · 2022', 'Tena · 2019']);
  } finally {
    FILES[page] = saved;
  }
  // Asked the older way (send_photos), the answer is the same sheet: nothing is picked without looking.
  const older = await tools.run('host_plants', { butterfly: 'Oleria onega', area: 'Napo', send_photos: 5, lang: 'es' });
  assert.equal(older.images, undefined);
  assert.equal(older.photo_sheets[0].id, sheet.id);
  const sent = await tools.run('send_sheet_photos', { sheets: [sheet.id], tiles: [2, 3], lang: 'es' });
  assert.deepEqual(sent.images.map(im => [im.plant, im.status, im.photo_in_area]), [['Solanum mite', 'in_area', false], ['Cestrum megalophyllum', 'nearby', false]]);
  assert.match(sent.images[1].caption, /^_Cestrum megalophyllum_ — CANDIDATA: sin registros en Napo, 2 a menos de 50 km\n/);
  assert.match(sent.images_block, /^\[images\]\n.+ \| _Solanum mite_/);
  // Another worker finds the sheet on disk; a wrong sheet or tile is said.
  const other = createMapsTools({ fetchImpl: fakeWeb().fetchImpl, mapsBase: MAPS, cacheDir: dir });
  assert.equal((await other.run('send_sheet_photos', { sheets: [sheet.id], tiles: [1] })).images[0].plant, 'Solanum mite');
  assert.match((await other.run('send_sheet_photos', { sheets: ['0123456789abcdef'], tiles: [1] })).error, /No such contact sheet/);
  assert.match((await other.run('send_sheet_photos', { sheets: [sheet.id], tiles: [9] })).error, /None of tiles 9/);
});

test('contact sheets, review fixes: another reach is another sheet; a tile number on two calls\' sheets is refused', { skip: !pillow && 'no python3 with Pillow' }, async () => {
  const dir = mkdtempSync(join(tmpdir(), 'maps-cache-'));
  const tools = createMapsTools({ fetchImpl: fakeWeb().fetchImpl, mapsBase: MAPS, cacheDir: dir });
  const a = await tools.run('host_plants', { butterfly: 'Oleria onega', area: 'Napo', photo_sheet: true, lang: 'es' });
  const b = await tools.run('host_plants', { butterfly: 'Oleria onega', area: 'Napo', photo_sheet: true, lang: 'es', nearby_km: 80 });
  // The same photos with another reach: another sheet, so its captions say 80 km.
  assert.notEqual(a.photo_sheets[0].id, b.photo_sheets[0].id);
  const fresh = createMapsTools({ fetchImpl: fakeWeb().fetchImpl, mapsBase: MAPS, cacheDir: dir });
  assert.match((await fresh.run('send_sheet_photos', { sheets: [b.photo_sheets[0].id], tiles: [3], lang: 'es' })).images[0].caption, /a menos de 80 km/);
  // Two calls' sheets both start at 1.
  assert.match((await fresh.run('send_sheet_photos', { sheets: [a.photo_sheets[0].id, b.photo_sheets[0].id], tiles: [1] })).error, /more than one of these sheets/);
});

test('send_photos never sends unseen, Pillow or not', async () => {
  const out = await createMapsTools({ fetchImpl: fakeWeb().fetchImpl, mapsBase: MAPS }).run('host_plants', { butterfly: 'Oleria onega', area: 'Napo', send_photos: 5 });
  assert.equal(out.images, undefined);
  assert.ok(out.photo_sheets || out.photo_sheet_error);
});

test('a plant\'s sheet tiles: one per record first; its leaves photo and a flower made sure of, the nearest kept', () => {
  const ph = (img, record, view) => ({ img, record, view, live: 0 });
  // Only flowers in the first three (one record twice): the leaves photo replaces a second flower, one flower stays.
  const picks = sheetPicks([ph('A1', 'A', 'flower'), ph('A2', 'A', 'leaves'), ph('B', 'B', 'flower'), ph('C', 'C', 'flower')], 3);
  assert.deepEqual(picks.map(p => p.img), ['A1', 'B', 'A2']);
  // The nearest leaves photo, even a second photo of a record, before a far record's first.
  const near = sheetPicks([ph('A1', 'A', 'flower'), ph('A2', 'A', 'leaves'), ph('B', 'B', 'flower'), ph('C', 'C', 'flower'), ph('D', 'D', 'leaves')], 3);
  assert.deepEqual(near.map(p => p.img), ['A1', 'B', 'A2']);
  // Untagged photos: the leaves and the flower come in for the last two, the nearest stays.
  const untagged = sheetPicks([ph('1', '1', null), ph('2', '2', null), ph('3', '3', null), ph('4', '4', 'leaves'), ph('5', '5', 'flower')], 3);
  assert.deepEqual(untagged.map(p => p.img), ['1', '5', '4']);
  // Two photos of one record wait behind the other records' first.
  assert.deepEqual(sheetPicks([ph('a1', 'a', null), ph('a2', 'a', null), ph('b', 'b', null)], 2).map(p => p.img), ['a1', 'b']);
});
