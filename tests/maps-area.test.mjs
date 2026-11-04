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

test('taxon_photos: a life stage from the project\'s clutch photos, iNaturalist\'s annotation and GBIF\'s lifeStage, on a sheet to pick from', { skip: !pillow && 'no python3 with Pillow' }, async () => {
  const { DatabaseSync } = await import('node:sqlite');
  const dir = mkdtempSync(join(tmpdir(), 'maps-cache-'));
  const clutchDir = mkdtempSync(join(tmpdir(), 'clutch-photos-'));
  // The project's own: one clutch photo of the species (Insectary_stocks), one of another species.
  const db = new DatabaseSync(':memory:');
  db.exec(`CREATE TABLE records(id TEXT, sheet TEXT, values_json TEXT); CREATE TABLE clutch_photos(id TEXT, record_id TEXT, day TEXT, note TEXT, file TEXT, actor TEXT); CREATE TABLE users(id TEXT, display_name TEXT);`);
  db.prepare('INSERT INTO users VALUES (?,?)').run('u-ana', 'Ana Pérez');
  db.prepare('INSERT INTO records VALUES (?,?,?)').run('r1', 'Insectary_stocks', JSON.stringify({ SPECIES: 'Oleria onega', 'CLUTCH NUMBER': 'P12' }));
  db.prepare('INSERT INTO records VALUES (?,?,?)').run('r2', 'Insectary_stocks', JSON.stringify({ SPECIES: 'Mechanitis menapis' }));
  db.prepare('INSERT INTO clutch_photos VALUES (?,?,?,?,?,?)').run('p1', 'r1', '2026-10-20', 'Larvas día 3', '2026-10/p1', 'u-ana');
  db.prepare('INSERT INTO clutch_photos VALUES (?,?,?,?,?,?)').run('p2', 'r2', '2026-10-21', null, '2026-10/p2', 'Luis');
  const { mkdirSync } = await import('node:fs');
  mkdirSync(join(clutchDir, '2026-10'));
  writeFileSync(join(clutchDir, '2026-10', 'p1.jpg'), jpeg(1200, 900));
  writeFileSync(join(clutchDir, '2026-10', 'p1.thumb.jpg'), jpeg(480, 360));
  const asked = [];
  const web = fakeWeb();
  const fetchImpl = async url => {
    asked.push(url);
    const u = new URL(url);
    if (u.host === 'inat.test') {
      assert.equal(u.searchParams.get('term_id'), '1');
      const larva = u.searchParams.get('term_value_id') === '6';
      return Response.json({
        total_results: larva ? 2 : 0,
        results: larva
          ? [
              { id: 501, quality_grade: 'research', observed_on: '2021-03-01', place_guess: 'Tena', geojson: { coordinates: [TENA.lng, TENA.lat] }, photos: [{ url: 'https://inaturalist-open-data.s3.amazonaws.com/photos/21/square.jpg', attribution: '(c) Ana, some rights reserved (CC BY-NC)', license_code: 'cc-by-nc' }, { url: 'https://inaturalist-open-data.s3.amazonaws.com/photos/22/square.jpg', attribution: '(c) Ana', license_code: 'cc-by-nc' }] },
              { id: 502, quality_grade: 'research', observed_on: '2019-01-01', place_guess: 'Bogotá', geojson: { coordinates: [BOGOTA.lng, BOGOTA.lat] }, photos: [{ url: 'https://inaturalist-open-data.s3.amazonaws.com/photos/23/square.jpg', attribution: '(c) Luis', license_code: 'cc-by' }] },
            ]
          : [],
      });
    }
    if (u.host === 'gbif.test') {
      const larva = u.searchParams.get('lifeStage') === 'Larva';
      return Response.json({ results: larva ? [{ key: 9, datasetKey: '50c9509d-22c7-4a22-a47d-8c48425ef4a7', media: [{ type: 'StillImage', identifier: 'https://x/inat.jpg' }] }, { key: 10, datasetKey: 'museum', year: 1990, country: 'Ecuador', decimalLatitude: PUYO.lat, decimalLongitude: PUYO.lng, media: [{ type: 'StillImage', identifier: 'https://museum.test/l.jpg', creator: 'Museo', license: 'http://creativecommons.org/licenses/by/4.0/' }] }] : [] });
    }
    return web.fetchImpl(url);
  };
  const tools = createMapsTools({ store: { db }, fetchImpl, mapsBase: MAPS, cacheDir: dir, clutchPhotoDir: clutchDir, inatApi: 'https://inat.test/v1/observations', gbifApi: 'https://gbif.test/v1/occurrence/search' });
  const out = await tools.run('taxon_photos', { taxon: 'Oleria onega', life_stage: ['egg', 'larva'], area: 'Napo', lang: 'es' });
  assert.deepEqual(out.counts, { project_clutch_photos: 1, inaturalist_egg: 0, gbif_other_egg: 0, inaturalist_larva: 2, gbif_other_larva: 1 });
  // The project's photo first, then Tena (in Napo), then Puyo (GBIF, a museum), Bogotá; the Tena observation's second photo last.
  const tiles = out.photo_sheets[0].tiles;
  assert.match(tiles[0], /^1: Oleria onega · larva — Insectario Ikiam · 2026$/);
  assert.match(tiles[1], /^2: Oleria onega · larva — Napo · 2021$/);
  assert.match(tiles[2], /Puyo|Ecuador/);
  assert.match(tiles.at(-1), /Napo · 2021$/);
  // The iNaturalist record GBIF holds is not shown twice.
  assert.ok(!tiles.some(t => /inat\.jpg/.test(t)));
  const sent = await tools.run('send_sheet_photos', { sheets: [out.photo_sheets[0].id], tiles: [1, 2], lang: 'es' });
  assert.equal(sent.images[0].path, join(clutchDir, '2026-10', 'p1.jpg'));
  assert.match(sent.images[0].caption, /^_Oleria onega_ — puesta del insectario Ikiam P12\nLarvas día 3, 2026-10-20\nFoto: Ana Pérez \(Ithomiini Ikiam\)$/);
  assert.match(sent.images[1].caption, /^_Oleria onega_ — larva\nTena, 2021\nFoto: Ana \(CC BY-NC\)\nhttps:\/\/www\.inaturalist\.org\/observations\/501$/);
  assert.match(sent.images[1].url, /photos\/21\/original\.jpg$/);
  assert.match((await tools.run('taxon_photos', { taxon: 'Oleria onega' })).error, /life_stage/);
  const none = await tools.run('taxon_photos', { taxon: 'Oleria onega', life_stage: 'pupa' });
  assert.match(none.note, /Unannotated photos may exist/);
  // Eggs asked with larvae: the eggs said to be missing.
  assert.deepEqual(out.none_for, ['egg']);
  assert.match(out.none_note, /No photo of egg of Oleria onega: none in the project's clutch photos \(1 of this taxon, stages by their notes\)/);
  assert.match((await tools.run('gallery_specimens', { species: ['Oleria onega'], life_stage: 'larva' })).error, /adults only.*taxon_photos/);
  // iNaturalist down: eggs are not said to be missing, only not checked.
  const down = createMapsTools({ store: { db }, fetchImpl: async url => (/inat\.test/.test(url) ? new Response('down', { status: 503 }) : fetchImpl(url)), mapsBase: MAPS, cacheDir: dir, clutchPhotoDir: clutchDir, inatApi: 'https://inat.test/v1/observations', gbifApi: 'https://gbif.test/v1/occurrence/search' });
  const partial = await down.run('taxon_photos', { taxon: 'Oleria onega', life_stage: ['egg', 'larva'] });
  assert.equal(partial.none_for, undefined);
  assert.deepEqual(partial.not_checked, ['egg']);
  // A clutch photo with no stage in its note: eggs are not said to be missing; an unlicensed annotated egg photo
  // is not "none" either.
  db.prepare('INSERT INTO clutch_photos VALUES (?,?,?,?,?,?)').run('p3', 'r1', '2026-10-22', null, '2026-10/p3', 'u-ana');
  writeFileSync(join(clutchDir, '2026-10', 'p3.jpg'), jpeg(1200, 900));
  writeFileSync(join(clutchDir, '2026-10', 'p3.thumb.jpg'), jpeg(480, 360));
  const withUnnoted = await tools.run('taxon_photos', { taxon: 'Oleria onega', life_stage: ['egg', 'larva'] });
  assert.equal(withUnnoted.none_for, undefined);
  assert.match(withUnnoted.unnoted_note, /1 clutch photo\(s\) of Oleria onega have no stage/);
  const unlicensed = createMapsTools({ fetchImpl: async url => (/inat\.test/.test(url) && /term_value_id=7/.test(url) ? Response.json({ total_results: 1, results: [{ id: 9, geojson: { coordinates: [TENA.lng, TENA.lat] }, photos: [{ url: 'https://inaturalist-open-data.s3.amazonaws.com/photos/31/square.jpg' }] }] }) : fetchImpl(url)), mapsBase: MAPS, cacheDir: dir, inatApi: 'https://inat.test/v1/observations', gbifApi: 'https://gbif.test/v1/occurrence/search' });
  const eggs = await unlicensed.run('taxon_photos', { taxon: 'Oleria onega', life_stage: 'egg' });
  assert.equal(eggs.none_for, undefined);
  assert.deepEqual(eggs.no_usable_photo, ['egg']);
});

// ---- taxon_photos: the Wings Atlas records' photos, GBIF tried twice, egg-laying asks; host-plant tags fallback ----

test('atlasPhotoOf: the photo and its page only when http(s); the project\'s wsrv thumbnails unwrapped to their file', async () => {
  const { atlasPhotoOf } = await import('../server/maps-tools.mjs');
  assert.deepEqual(atlasPhotoOf({ image_url: 'https://wsrv.nl/?url=https://drive.google.com/uc?id=AbC&w=400&output=webp' }), { img: 'https://drive.google.com/uc?id=AbC' });
  assert.deepEqual(atlasPhotoOf({ image_url: 'http://www.boldsystems.org/pics/X.jpg', observation_url: 'https://boldsystems.org/r?id=1' }), { img: 'http://www.boldsystems.org/pics/X.jpg', obs: 'https://boldsystems.org/r?id=1' });
  assert.deepEqual(atlasPhotoOf({ image_url: 'javascript:alert(1)' }), {});
  assert.deepEqual(atlasPhotoOf({ image_url: null }), {});
  assert.deepEqual(atlasPhotoOf({ image_url: 'https://x.test/a.jpg', observation_url: 'javascript:x' }), { img: 'https://x.test/a.jpg' });
});

const atlasPoint = (id, name, at, extra) => ({ id, scientific_name: name, genus: name.split(' ')[0], lat: at.lat, lng: at.lng, country: 'Ecuador', observation_date: '2020-05-01', sex: null, ...extra });
const ATLAS_FILES = {
  'data_manifest.json': { generated_at: '2026-09-26T04:34:23Z', sources: { 'Sanger Institute': { file: 'map_points_sanger.json' }, 'GBIF (Other Institutions)': { file: 'map_points_gbif_other.json' }, iNaturalist: { file: 'map_points_inaturalist.json' } } },
  'map_points_sanger.json': [
    atlasPoint('CAM001', 'Tithorea harmonia', TENA, { source: 'Sanger Institute', collection_location: 'Tena CAM', sex: 'female', image_url: 'https://wsrv.nl/?url=https://drive.google.com/uc?id=F1&w=400&output=webp' }),
    atlasPoint('CAM002', 'Tithorea harmonia', TENA, { source: 'Sanger Institute', collection_location: 'Tena CAM male', sex: 'male', image_url: 'https://wsrv.nl/?url=https://drive.google.com/uc?id=F2&w=400&output=webp' }),
    // No photo: not on offer.
    atlasPoint('CAM003', 'Tithorea harmonia', TENA, { source: 'Sanger Institute', collection_location: 'no photo', sex: 'female', image_url: null }),
  ],
  'map_points_gbif_other.json': [
    // A BOLD photo over http, female, in Pastaza (outside Napo).
    atlasPoint('7001', 'Tithorea harmonia', PUYO, { source: 'GBIF (Other Institutions)', collection_location: 'Puyo BOLD', sex: 'female', image_url: 'http://www.boldsystems.org/pics/th1.jpg', observation_url: 'https://boldsystems.org/r?id=th1' }),
    // Its GBIF record has no licence on the photo.
    atlasPoint('7002', 'Tithorea harmonia', BOGOTA, { source: 'GBIF (Other Institutions)', collection_location: 'Bogota museum', image_url: 'https://museum.test/th2.jpg' }),
    // The GBIF API gives this record already: not twice.
    atlasPoint('7003', 'Tithorea harmonia', PUYO, { source: 'GBIF (Other Institutions)', collection_location: 'dup gbif', image_url: 'https://museum.test/th3.jpg' }),
  ],
  'map_points_inaturalist.json': [
    atlasPoint('8001', 'Tithorea harmonia', NAPO_SOUTH, { source: 'iNaturalist', collection_location: 'Napo inat', image_url: 'https://inaturalist-open-data.s3.amazonaws.com/photos/801/original.jpeg', observation_url: 'https://www.inaturalist.org/observations/9801' }),
    // The iNaturalist API gives this observation already: not twice.
    atlasPoint('8002', 'Tithorea harmonia', NAPO_SOUTH, { source: 'iNaturalist', collection_location: 'dup inat', image_url: 'https://inaturalist-open-data.s3.amazonaws.com/photos/802/original.jpeg', observation_url: 'https://www.inaturalist.org/observations/9802' }),
    // Another species and a sexless record of another genus.
    atlasPoint('8003', 'Tithorea tarricina', TENA, { source: 'iNaturalist', collection_location: 'other species', image_url: 'https://inaturalist-open-data.s3.amazonaws.com/photos/803/original.jpeg' }),
    atlasPoint('8004', 'Oleria onega', TENA, { source: 'iNaturalist', collection_location: 'other genus', image_url: 'https://inaturalist-open-data.s3.amazonaws.com/photos/804/original.jpeg' }),
  ],
};
const ATLAS_GBIF_RECORDS = {
  8001: { key: 8001, species: 'Tithorea harmonia', locality: 'Napo south', country: 'Ecuador', year: 2020, references: 'https://www.inaturalist.org/observations/9801', media: [{ identifier: 'https://inaturalist-open-data.s3.amazonaws.com/photos/801/original.jpeg', creator: 'Rosa Q', license: 'http://creativecommons.org/licenses/by-nc/4.0/' }] },
  7001: { key: 7001, species: 'Tithorea harmonia', locality: 'Puyo', country: 'Ecuador', year: 2011, media: [{ identifier: 'http://www.boldsystems.org/pics/th1.jpg', creator: 'BOLD lab', license: 'http://creativecommons.org/licenses/by/4.0/' }] },
  7002: { key: 7002, species: 'Tithorea harmonia', country: 'Colombia', year: 1990, media: [{ identifier: 'https://museum.test/th2.jpg', creator: 'Museo X' }] },
};

/**
 * The maps site with the atlas files, Napo's outlines, GBIF's record API (credits), the photos, and the iNaturalist
 * and GBIF search APIs answered by `inat(url)` and `gbif(url, n)` (n: that URL's call count).
 */
function atlasWeb({ inat = () => Response.json({ total_results: 0, results: [] }), gbif = () => Response.json({ results: [] }) } = {}) {
  const calls = [];
  const counted = new Map();
  const fetchImpl = async url => {
    calls.push(url);
    if (BOUNDARIES[url]) return Response.json(BOUNDARIES[url]);
    const u = new URL(url);
    if (u.host === 'inat.test') return inat(url);
    if (u.host === 'gbif.test') {
      counted.set(url, (counted.get(url) ?? 0) + 1);
      return gbif(url, counted.get(url));
    }
    if (u.host === 'api.gbif.org') {
      const r = ATLAS_GBIF_RECORDS[u.pathname.split('/').pop()];
      return r ? Response.json(r) : new Response('no', { status: 404 });
    }
    if (u.host === 'inaturalist-open-data.s3.amazonaws.com' || u.host === 'wsrv.nl' || u.host === 'museum.test') return new Response(jpeg(1600, 1200));
    const path = u.pathname.replace('/ithomiini_maps/data/', '');
    return path in ATLAS_FILES ? Response.json(ATLAS_FILES[path]) : new Response('missing', { status: 404 });
  };
  return { calls, fetchImpl };
}
const atlasTools = (web, dir) => createMapsTools({ fetchImpl: web.fetchImpl, mapsBase: MAPS, cacheDir: dir, inatApi: 'https://inat.test/v1/observations', gbifApi: 'https://gbif.test/v1/occurrence/search', retryDelayMs: 0 });
const inatAdults = () =>
  Response.json({
    total_results: 1,
    results: [{ id: 9802, quality_grade: 'research', observed_on: '2022-01-01', place_guess: 'Napo', geojson: { coordinates: [NAPO_SOUTH.lng, NAPO_SOUTH.lat] }, annotations: [{ controlled_attribute_id: 9, controlled_value_id: 11 }], photos: [{ url: 'https://inaturalist-open-data.s3.amazonaws.com/photos/9802/square.jpg', attribution: '(c) Ana, some rights reserved (CC BY)', license_code: 'cc-by' }] }],
  });
const gbifAdults = () => Response.json({ results: [{ key: 7003, datasetKey: 'museum', year: 2001, country: 'Ecuador', decimalLatitude: PUYO.lat, decimalLongitude: PUYO.lng, media: [{ type: 'StillImage', identifier: 'https://museum.test/th3.jpg', creator: 'Museo', license: 'http://creativecommons.org/licenses/by/4.0/' }] }] });

test('taxon_photos, Wings Atlas: adults there while the iNaturalist and GBIF APIs are down; said not checked, never none', { skip: !pillow && 'no python3 with Pillow' }, async () => {
  const dir = mkdtempSync(join(tmpdir(), 'maps-cache-'));
  const web = atlasWeb({ inat: () => new Response('down', { status: 503 }), gbif: () => new Response('down', { status: 503 }) });
  const out = await atlasTools(web, dir).run('taxon_photos', { taxon: 'Tithorea harmonia', life_stage: 'adult', area: 'Napo' });
  assert.equal(out.error, undefined, out.error);
  // 2 project specimens with photos, 3 GBIF (other), 2 iNaturalist: 7; none already shown (the APIs gave nothing).
  assert.equal(out.counts.atlas_adult, 7);
  assert.ok(out.photo_sheets[0].tiles.length === 7);
  assert.ok(out.photo_sheets[0].tiles.every(t => /Atlas/.test(t)));
  // In Napo first (Tena, Napo south), Puyo and Bogotá after.
  assert.ok(out.photo_sheets[0].tiles.slice(0, 4).every(t => /Napo/.test(t)), out.photo_sheets[0].tiles.join('\n'));
  assert.equal(out.none_for, undefined);
  assert.deepEqual(out.not_checked, undefined, 'adults were found, so nothing is missing');
  assert.deepEqual(out.sources_not_checked.sort(), ['gbif:adult', 'inat:adult']);
  assert.deepEqual(out.retried, ['gbif.test']);
  assert.match(out.photo_sheet_note, /Wings Atlas records: no life stage is recorded/);
  // GBIF tried twice, iNaturalist once.
  assert.equal(web.calls.filter(u => u.includes('gbif.test')).length, 2);
  assert.equal(web.calls.filter(u => u.includes('inat.test')).length, 1);
});

test('taxon_photos, Wings Atlas: sex and taxon filters, records the APIs gave not shown twice, credits and licences on sending', { skip: !pillow && 'no python3 with Pillow' }, async () => {
  const dir = mkdtempSync(join(tmpdir(), 'maps-cache-'));
  const web = atlasWeb({ inat: inatAdults, gbif: gbifAdults });
  const tools = atlasTools(web, dir);
  const all = await tools.run('taxon_photos', { taxon: 'Tithorea harmonia', life_stage: 'adult', area: 'Napo', lang: 'es' });
  // The observation 9802 and GBIF record 7003 came from the APIs: their atlas copies are left out.
  assert.equal(all.counts.atlas_adult, 5);
  assert.equal(all.counts.atlas_adult_already_shown, 2);
  const lines = all.photo_sheets[0].tiles;
  assert.ok(!lines.some(t => /dup /.test(t)));
  assert.ok(!lines.some(t => /other species|other genus/.test(t)));
  // Female only: the project's female specimen and the BOLD female; the male and the sexless records are not.
  const fem = await tools.run('taxon_photos', { taxon: 'Tithorea harmonia', life_stage: 'adult', area: 'Napo', sex: 'female', lang: 'es' });
  assert.equal(fem.counts.atlas_adult, 2);
  // The iNaturalist adult is annotated male: not with female asked; GBIF asked for females.
  assert.equal(fem.counts.inaturalist_adult, 0);
  assert.ok(web.calls.some(u => u.includes('gbif.test') && u.includes('sex=FEMALE')));
  // The GBIF API's adult (asked with sex=FEMALE) and the two atlas females, all marked ♀.
  assert.deepEqual(fem.photo_sheets[0].tiles.map(t => /♀/.test(t)), [true, true, true]);
  assert.match((await tools.run('taxon_photos', { taxon: 'Tithorea harmonia', life_stage: 'adult', sex: 'x' })).error, /male or female/);
  // Genus: every Tithorea photo (T. tarricina too), not Oleria; the 2 the APIs gave still not twice.
  const genus = await tools.run('taxon_photos', { taxon: 'Tithorea', life_stage: 'adult' });
  assert.equal(genus.counts.atlas_adult, 6);
  assert.ok(genus.photo_sheets[0].tiles.some(t => /Tithorea tarricina/.test(t) && /other species/.test(t)));
  assert.ok(!genus.photo_sheets[0].tiles.some(t => /other genus/.test(t)));
  // Sending: the project's specimen with its CAM ID; the BOLD http photo through the proxy, credited from GBIF.
  const sheet = fem.photo_sheets[0];
  const tileOf = re => Number(sheet.tiles.find(t => re.test(t)).split(':')[0]);
  const sent = await tools.run('send_sheet_photos', { sheets: [sheet.id], tiles: [tileOf(/Napo · 2020 · Atlas/), tileOf(/Puyo BOLD/)], lang: 'es' });
  assert.equal(sent.failed, undefined, JSON.stringify(sent.failed));
  assert.match(sent.images[0].caption, /^_Tithorea harmonia_ — adulto, hembra\nTena CAM, Ecuador, 2020\nCAM001, Ithomiini Ikiam \(project collection\)\nRegistro del Wings Atlas, sin etapa anotada$/);
  assert.match(sent.images[0].url, /^https:\/\/wsrv\.nl\/\?url=https%3A%2F%2Fdrive\.google\.com/);
  assert.match(sent.images[1].caption, /Foto: BOLD lab \(CC BY 4\.0\)/);
  assert.match(sent.images[1].caption, /Registro del Wings Atlas, sin etapa anotada/);
  assert.match(sent.images[1].url, /^https:\/\/wsrv\.nl\/\?url=http%3A%2F%2Fwww\.boldsystems\.org/);
  assert.equal(sent.images[1].stage_recorded, false);
  assert.match(sent.images_block, /^\[images\]\n/);
  // A photo whose GBIF record gives no licence is not sent; the iNaturalist one is, with its photographer.
  const sheetAll = all.photo_sheets[0];
  const n = re => Number(sheetAll.tiles.find(t => re.test(t)).split(':')[0]);
  // Tile 4: the sexless iNaturalist record in Napo (credited from its GBIF record).
  assert.match(sheetAll.tiles[3], /^4: Tithorea harmonia · adulto — Napo · 2020 · Atlas$/);
  const two = await tools.run('send_sheet_photos', { sheets: [sheetAll.id], tiles: [n(/Bogota/), 4], lang: 'en' });
  assert.deepEqual(two.failed.map(f => f.error), ['its photo has no licence on its GBIF record: not sent']);
  assert.equal(two.images.length, 1);
  assert.match(two.images[0].caption, /^_Tithorea harmonia_ — adult\nNapo south, Ecuador\nPhoto: Rosa Q \(CC BY-NC 4\.0\)\nhttps:\/\/www\.inaturalist\.org\/observations\/9801\nWings Atlas record, no life stage recorded$/);
  assert.match(two.images[0].url, /photos\/801\/original\.jpeg$/);
});

test('taxon_photos: GBIF failing once is tried again and checked; twice, or a passing failure of a 4xx, not checked', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'maps-cache-'));
  // Sheets off (no folder): the answer without sheets still says what was checked.
  const once = atlasWeb({ gbif: (url, n) => (n === 1 ? new Response('busy', { status: 503 }) : Response.json({ results: [] })) });
  const a = await createMapsTools({ fetchImpl: once.fetchImpl, mapsBase: MAPS, inatApi: 'https://inat.test/v1/observations', gbifApi: 'https://gbif.test/v1/occurrence/search', retryDelayMs: 0 }).run('taxon_photos', { taxon: 'Tithorea harmonia', life_stage: 'egg' });
  assert.deepEqual(a.retried, ['gbif.test']);
  assert.deepEqual(a.sources_not_checked, []);
  assert.deepEqual(a.none_for, ['egg']);
  assert.equal(a.counts.gbif_other_egg, 0);
  const twice = atlasWeb({ gbif: () => new Response('busy', { status: 503 }) });
  const b = await createMapsTools({ fetchImpl: twice.fetchImpl, mapsBase: MAPS, inatApi: 'https://inat.test/v1/observations', gbifApi: 'https://gbif.test/v1/occurrence/search', retryDelayMs: 0 }).run('taxon_photos', { taxon: 'Tithorea harmonia', life_stage: 'egg' });
  assert.equal(b.none_for, undefined);
  assert.deepEqual(b.not_checked, ['egg']);
  assert.deepEqual(b.sources_not_checked, ['gbif:egg']);
  assert.match(b.errors.join(' '), /tried twice/);
  assert.equal(twice.calls.filter(u => u.includes('gbif.test')).length, 2);
  // A network failure (no answer) is tried again too; a 400 is not.
  const net = atlasWeb({ gbif: (url, n) => { if (n === 1) throw new TypeError('fetch failed'); return Response.json({ results: [] }); } });
  const c = await createMapsTools({ fetchImpl: net.fetchImpl, mapsBase: MAPS, inatApi: 'https://inat.test/v1/observations', gbifApi: 'https://gbif.test/v1/occurrence/search', retryDelayMs: 0 }).run('taxon_photos', { taxon: 'Tithorea harmonia', life_stage: 'egg' });
  assert.deepEqual(c.none_for, ['egg']);
  const bad = atlasWeb({ gbif: () => new Response('bad', { status: 400 }) });
  const d = await createMapsTools({ fetchImpl: bad.fetchImpl, mapsBase: MAPS, inatApi: 'https://inat.test/v1/observations', gbifApi: 'https://gbif.test/v1/occurrence/search', retryDelayMs: 0 }).run('taxon_photos', { taxon: 'Tithorea harmonia', life_stage: 'egg' });
  assert.deepEqual(d.not_checked, ['egg']);
  assert.equal(bad.calls.filter(u => u.includes('gbif.test')).length, 1);
});

test('taxon_photos, egg-laying: adult atlas photos never stand for eggs; the answer points to host plants as context only', async () => {
  const web = atlasWeb();
  const tools = createMapsTools({ fetchImpl: web.fetchImpl, mapsBase: MAPS, inatApi: 'https://inat.test/v1/observations', gbifApi: 'https://gbif.test/v1/occurrence/search', retryDelayMs: 0 });
  const eggs = await tools.run('taxon_photos', { taxon: 'Tithorea harmonia', life_stage: 'egg', area: 'Napo' });
  // The atlas is not read for eggs: its adults are no evidence of eggs, and eggs stay "none".
  assert.equal(eggs.counts.atlas_adult, undefined);
  assert.ok(!web.calls.some(u => u.includes('map_points_')));
  assert.deepEqual(eggs.none_for, ['egg']);
  assert.match(eggs.oviposition_note, /No source here records egg-laying/);
  assert.match(eggs.oviposition_note, /host_plants \(butterfly: "Tithorea harmonia"/);
  assert.match(eggs.oviposition_note, /never as photos of oviposition/);
  assert.match(eggs.oviposition_note, /else country: "Ecuador"/);
  // Eggs and adults: adults from the atlas, eggs still none; sex is said to be for adults only.
  const both = await tools.run('taxon_photos', { taxon: 'Tithorea harmonia', life_stage: ['egg', 'adult'], sex: 'female' });
  assert.equal(both.counts.atlas_adult, 2);
  assert.deepEqual(both.none_for, ['egg']);
  assert.match(both.sex_note, /egg photos are of either sex/);
  // The maps site down: adults are not checked (not "none").
  const down = createMapsTools({ fetchImpl: async url => (url.includes('/ithomiini_maps/') ? new Response('down', { status: 503 }) : web.fetchImpl(url)), mapsBase: MAPS, inatApi: 'https://inat.test/v1/observations', gbifApi: 'https://gbif.test/v1/occurrence/search', retryDelayMs: 0 });
  const adults = await down.run('taxon_photos', { taxon: 'Tithorea harmonia', life_stage: 'adult' });
  assert.equal(adults.none_for, undefined);
  assert.deepEqual(adults.not_checked, ['adult']);
  assert.deepEqual(adults.sources_not_checked.sort(), ['atlas:gbif_other', 'atlas:inaturalist', 'atlas:sanger']);
});

test('host_plants tags_fallback: photos picked by the tags sent with captions saying so; a failed sheet points to it', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'maps-cache-'));
  const tools = createMapsTools({ fetchImpl: fakeWeb().fetchImpl, mapsBase: MAPS, cacheDir: dir });
  const out = await tools.run('host_plants', { butterfly: 'Oleria onega', area: 'Napo', send_photos: 2, tags_fallback: true, lang: 'es' });
  assert.equal(out.error, undefined, out.error);
  assert.equal(out.photo_sheets, undefined);
  assert.equal(out.unchecked, true);
  assert.ok(out.images.length >= 1);
  assert.ok(out.images.every(im => im.looked_at === false && im.picked_by === 'photo tags' && im.credit?.photographer));
  assert.ok(out.images.every(im => im.caption.endsWith('elegida automáticamente por etiqueta, no revisada')));
  assert.match(out.images_block, /^\[images\]\n/);
  assert.equal(out.images_block.match(/no revisada/g).length, out.images.length);
  assert.match(out.unchecked_note, /not looked at by anyone/);
  // No tags file in this cache folder: said.
  assert.equal(out.photo_tags_missing, true);
  // A sheet that cannot be made (no python): the error and the fallback named, nothing sent.
  const broken = createMapsTools({ fetchImpl: fakeWeb().fetchImpl, mapsBase: MAPS, cacheDir: mkdtempSync(join(tmpdir(), 'maps-cache-')), sheetOptions: { python: '/nonexistent/python3' } });
  const sheetless = await broken.run('host_plants', { butterfly: 'Oleria onega', area: 'Napo', photo_sheet: true });
  assert.ok(sheetless.photo_sheet_error);
  assert.match(sheetless.photo_sheet_fallback, /tags_fallback: true/);
  assert.equal(sheetless.images_block, undefined);
});

test('taxon_photos retains successful Atlas files when another source fails', async () => {
  const web = atlasWeb();
  const partial = { fetchImpl: url => url.endsWith('map_points_gbif_other.json') ? Promise.resolve(new Response('down', { status: 503 })) : web.fetchImpl(url) };
  const tools = atlasTools(partial, mkdtempSync(join(tmpdir(), 'maps-cache-')));
  const out = await tools.run('taxon_photos', { taxon: 'Tithorea harmonia', life_stage: 'adult' });
  assert.ok(out.counts.atlas_adult > 0);
  assert.deepEqual(out.sources_not_checked, ['atlas:gbif_other']);
  assert.equal(out.none_for, undefined);
  assert.match(out.photo_sheet_note ?? out.photo_sheet_error, /egg-laying is not recorded|Contact sheets are off/);
});

test('GBIF retry diagnostics belong to the stage that was retried', async () => {
  const web = atlasWeb({ gbif: (url, n) => url.includes('lifeStage=Egg') ? new Response('down', { status: 503 }) : new Response('bad', { status: 400 }) });
  const out = await atlasTools(web).run('taxon_photos', { taxon: 'Tithorea harmonia', life_stage: ['egg', 'larva'] });
  assert.match(out.errors.find(e => e.startsWith('GBIF (egg)')), /tried twice/);
  assert.doesNotMatch(out.errors.find(e => e.startsWith('GBIF (larva)')), /tried twice/);
});
