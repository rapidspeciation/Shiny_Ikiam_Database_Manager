import test from 'node:test';
import assert from 'node:assert/strict';
import { existsSync, mkdtempSync, readFileSync, readdirSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { cantonOf, findArea } from '../server/admin-areas.mjs';
import { createMapsTools } from '../server/maps-tools.mjs';
import { creditOf, jpegSize, licenceName, sizedUrl } from '../server/plant-photos.mjs';

const MAPS = 'https://maps.test/ithomiini_maps/';
// About 2 km either side of Napo's borders, on the INEC outlines (server/ecuador-admin.json).
const NAPO_SOUTH = { lat: -1.18, lng: -77.8 };
const PASTAZA = { lat: -1.22, lng: -77.8 };
const NAPO_NORTH = { lat: -0.11, lng: -77.6 };
const SUCUMBIOS = { lat: -0.07, lng: -77.6 };
const TENA = { lat: -0.993, lng: -77.814 };

test('Napo by its outline: a point just inside, one just outside in Pastaza or Sucumbíos', () => {
  const napo = findArea('provincia de Napo');
  assert.equal(napo.level, 'province');
  assert.equal(napo.name, 'Napo');
  assert.deepEqual(napo.cantons, ['Archidona', 'Carlos Julio Arosemena Tola', 'El Chaco', 'Quijos', 'Tena']);
  for (const p of [NAPO_SOUTH, NAPO_NORTH, TENA]) assert.equal(napo.test(p.lng, p.lat), true, JSON.stringify(p));
  assert.equal(napo.test(PASTAZA.lng, PASTAZA.lat), false);
  assert.equal(napo.test(SUCUMBIOS.lng, SUCUMBIOS.lat), false);
  // The box would take both: the outline does not.
  const [w, s, e, n] = napo.bbox;
  for (const p of [PASTAZA, SUCUMBIOS]) assert.ok(p.lng >= w && p.lng <= e && (p.lat >= s || p.lat <= n));
  assert.equal(cantonOf(PASTAZA.lng, PASTAZA.lat).province, 'Pastaza');
  assert.equal(cantonOf(SUCUMBIOS.lng, SUCUMBIOS.lat).province, 'Sucumbíos');
  assert.deepEqual(cantonOf(TENA.lng, TENA.lat), { name: 'Tena', province: 'Napo' });
  assert.equal(cantonOf(-74.1, 4.6), null);
});

test('area names: province before canton, a canton by its province, unknown names', () => {
  assert.equal(findArea('Pastaza').level, 'province');
  assert.match(findArea('Pastaza').also, /also a canton/);
  assert.deepEqual([findArea('cantón Pastaza').level, findArea('cantón Pastaza').province], ['canton', 'Pastaza']);
  assert.equal(findArea('Tena').level, 'canton');
  assert.equal(findArea('Tena').test(TENA.lng, TENA.lat), true);
  assert.equal(findArea('Tena').test(NAPO_NORTH.lng, NAPO_NORTH.lat), false);
  assert.deepEqual([findArea('Bolívar, Manabí').level, findArea('Bolívar, Manabí').province], ['canton', 'Manabí']);
  assert.match(findArea('cantón Bolívar').error, /Several cantons/);
  assert.match(findArea('Narnia').error, /No province or canton/);
  assert.equal(findArea('Sucumbios').name, 'Sucumbíos');
});

test('photo helpers: 1280 px at most, licences, JPEG size, the credit from a GBIF record', () => {
  assert.equal(sizedUrl('https://inaturalist-open-data.s3.amazonaws.com/photos/11/original.jpg'), 'https://inaturalist-open-data.s3.amazonaws.com/photos/11/large.jpg');
  assert.match(sizedUrl('https://herbarium.test/sheet.jpg'), /^https:\/\/wsrv\.nl\/\?url=https%3A%2F%2Fherbarium\.test%2Fsheet\.jpg&w=1280&h=1280&fit=inside/);
  assert.equal(licenceName('http://creativecommons.org/licenses/by-nc/4.0/'), 'CC BY-NC 4.0');
  assert.equal(licenceName('http://creativecommons.org/publicdomain/zero/1.0/legalcode'), 'CC0');
  assert.equal(licenceName(''), null);
  assert.deepEqual(jpegSize(jpeg(1024, 768)), { width: 1024, height: 768 });
  assert.equal(jpegSize(Buffer.from('not a jpeg')), null);
  const credit = creditOf(GBIF[1], 'https://inaturalist-open-data.s3.amazonaws.com/photos/11/original.jpg');
  assert.deepEqual(credit, {
    photographer: 'Ana Pérez',
    licence: 'CC BY 4.0',
    licence_url: 'http://creativecommons.org/licenses/by/4.0/',
    rights_holder: 'Ana Pérez',
    place: 'Tena, Napo, Ecuador',
    year: 2019,
    as_recorded: 'Solanum mitis',
    link: 'https://www.inaturalist.org/observations/101',
    gbif: 'https://www.gbif.org/occurrence/1',
    institution: 'iNaturalist',
  });
});

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
  2: { key: 2, species: 'Solanum mitis', verbatimLocality: 'Lago Agrio, Sucumbíos, EC', country: 'Ecuador', year: 2022, media: [{ identifier: 'https://inaturalist-open-data.s3.amazonaws.com/photos/12/original.jpg', creator: 'Luis', license: 'http://creativecommons.org/licenses/by-nc/4.0/' }] },
  3: { key: 3, species: 'Cestrum megalophyllum', locality: 'Puyo', stateProvince: 'Pastaza', country: 'Ecuador', year: 2005, media: [{ identifier: 'https://herbarium.test/cestrum.jpg', rightsHolder: 'Herbario Q', license: 'http://creativecommons.org/licenses/by-nc-sa/4.0/' }] },
};
const FILES = {
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
  'host_plants/refs/cestrum.json': { metadata: { total_records: 1 }, chunks: { 'chunks/c2.json': [0] } },
  'host_plants/refs/far.json': { metadata: { total_records: 1 }, chunks: { 'chunks/c2.json': [1] } },
  'host_plants/chunks/c1.json': {
    records: [
      { ...NAPO_SOUTH, eventDate: '2019-05-01' },
      { ...TENA, eventDate: '2021-07-02' },
      { ...SUCUMBIOS, eventDate: '2022-01-01' },
      { ...PASTAZA, eventDate: '2005-01-01' },
    ],
  },
  'host_plants/chunks/c2.json': {
    records: [
      // Cestrum: only just outside Napo (Pastaza); Far away: in Colombia.
      { ...PASTAZA, eventDate: '2005-01-01' },
      { lat: 4.6, lng: -74.1, eventDate: '1999-01-01' },
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
    items: [{ image_url: 'https://herbarium.test/cestrum.jpg', observation_url: 'https://www.gbif.org/occurrence/3', ...PASTAZA, observation_date: '2005-01-01', photo_context: 'preserved', source: 'Herbario Q' }],
  },
};

function fakeWeb() {
  const calls = [];
  const fetchImpl = async url => {
    const u = new URL(url);
    calls.push(u.href);
    if (u.host === 'api.gbif.org') {
      const r = GBIF[u.pathname.split('/').pop()];
      return r ? Response.json(r) : new Response('no', { status: 404 });
    }
    if (u.host === 'inaturalist-open-data.s3.amazonaws.com' || u.host === 'wsrv.nl') return new Response(jpeg(1024, 768));
    const path = u.pathname.replace('/ithomiini_maps/data/', '');
    return path in FILES ? Response.json(FILES[path]) : new Response('missing', { status: 404 });
  };
  return { calls, fetchImpl };
}

test('host_plant_photos: the plants recorded in Napo, one photo each ready to send with its credit, the shape the WhatsApp bridge reads', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'host-plant-photos-'));
  const web = fakeWeb();
  const tools = createMapsTools({ fetchImpl: web.fetchImpl, mapsBase: MAPS, hostPlantDir: dir });
  const out = await tools.run('host_plant_photos', { butterfly: 'Oleria onega', area: 'provincia de Napo', lang: 'es' });
  assert.equal(out.error, undefined, out.error);
  assert.equal(out.area.admin.name, 'Napo');
  // Solanum mite and its synonym: one plant, 2 records in Napo (Tena and the southern edge), not the Pastaza or
  // Sucumbíos ones just outside.
  const mite = out.plants.find(p => p.plant === 'Solanum mite');
  assert.equal(mite.records_in_area, 2);
  assert.deepEqual(mite.same_records_as, ['Solanum mitis']);
  assert.deepEqual(mite.by_canton, { 'Carlos Julio Arosemena Tola (Napo)': 1, 'Tena (Napo)': 1 });
  assert.deepEqual(mite.years, { first: 2019, last: 2021, distinct: 2 });
  assert.ok(!out.plants.some(p => p.plant === 'Solanum mitis'));
  // Cestrum is just across the border: nearby, with a photo after the plants in Napo; Far away: neither.
  assert.deepEqual(out.nearby, ['Cestrum megalophyllum']);
  assert.deepEqual(out.not_in_area, ['Far away']);
  // images[]: what the bridge sends, in order.
  assert.equal(out.images.length, 2);
  for (const im of out.images) {
    assert.deepEqual(Object.keys(im).slice(0, 4), ['path', 'caption', 'plant', 'in_area']);
    assert.ok(existsSync(im.path) && im.path.startsWith(dir));
    assert.ok(Math.max(im.width, im.height) <= 1280);
    assert.ok(im.credit.link && im.credit.licence);
  }
  const [first, second] = out.images;
  assert.equal(first.plant, 'Solanum mite');
  assert.equal(first.in_area, true);
  assert.equal(first.url, 'https://inaturalist-open-data.s3.amazonaws.com/photos/11/large.jpg');
  assert.equal(first.caption, '_Solanum mite_ (= Solanum mitis)\nTena, Napo, Ecuador, 2019\nFoto: Ana Pérez (CC BY 4.0)\nhttps://www.inaturalist.org/observations/101');
  assert.equal(second.plant, 'Cestrum megalophyllum');
  assert.equal(second.in_area, false);
  assert.match(second.caption, /^_Cestrum megalophyllum_ — ninguno en Napo, 1 a menos de 60 km\nPuyo, Pastaza, Ecuador, 2005 \(fuera de Napo: Santa Clara, Pastaza\)\nFoto: Herbario Q \(CC BY-NC-SA 4\.0\), pliego de herbario\nhttps:\/\/www\.gbif\.org\/occurrence\/3$/);
  assert.match(second.url, /^https:\/\/wsrv\.nl\//);
  assert.match(out.summary, /^Plantas hospederas de Oleria onega registradas en Napo \(1\):\n- _Solanum mite_ \(= Solanum mitis\): 2 registros GBIF en Napo, 2019–2021 \(Carlos Julio Arosemena Tola 1, Tena 1\)/);
  assert.match(out.summary, /Sin registros en Napo pero a menos de 60 km \(registros\): Cestrum megalophyllum \(1\)/);
  const url = new URL(out.map_url);
  assert.equal(url.searchParams.get('country'), 'Ecuador');
  assert.equal(url.searchParams.get('hp_bf'), 'Oleria onega');
  assert.ok(url.searchParams.get('map_view'));
  assert.match(out.markdown, /\[!\[Solanum mite\]\(https:\/\/inaturalist-open-data\.s3\.amazonaws\.com\/photos\/11\/large\.jpg\)\]/);
  assert.equal(out.sources['Willmott & Mallet 2004'], 'https://doi.org/10.1/x');
  assert.equal(out.timing_ms.records_from, 'download');

  // Again: nothing fetched. A new process (new tools, same folder): records, photos and credits from the disk.
  const n = web.calls.length;
  assert.deepEqual((await tools.run('host_plant_photos', { butterfly: 'Oleria onega', area: 'Napo', lang: 'es' })).images.map(i => i.path), out.images.map(i => i.path));
  assert.equal(web.calls.length, n);
  const web2 = fakeWeb();
  const fresh = await createMapsTools({ fetchImpl: web2.fetchImpl, mapsBase: MAPS, hostPlantDir: dir }).run('host_plant_photos', { butterfly: 'Oleria onega', area: 'Napo' });
  assert.equal(fresh.timing_ms.records_from, 'disk');
  assert.ok(!web2.calls.some(u => /chunks|api\.gbif|amazonaws|wsrv/.test(u)), web2.calls.join('\n'));
  assert.match(fresh.images[0].caption, /\nPhoto: Ana Pérez \(CC BY 4\.0\)\n/);
  assert.equal(readdirSync(join(dir, 'photos')).length, 2);
  assert.ok(JSON.parse(readFileSync(join(dir, 'gbif', '1.json'), 'utf8')).media);
});

test('host_plant_photos: errors said, a photo that cannot be had replaced by the next, host_plants by area', async () => {
  const web = fakeWeb();
  const broken = async (url, o) => (/photos\/11\//.test(url) ? new Response('gone', { status: 404 }) : web.fetchImpl(url, o));
  const tools = createMapsTools({ fetchImpl: broken, mapsBase: MAPS });
  assert.match((await tools.run('host_plant_photos', { area: 'Napo' })).error, /butterfly/);
  assert.match((await tools.run('host_plant_photos', { butterfly: 'Oleria onega' })).error, /area/);
  assert.match((await tools.run('host_plant_photos', { butterfly: 'Oleria onega', area: 'Narnia' })).error, /No province or canton/);
  const out = await tools.run('host_plant_photos', { butterfly: 'Oleria onega', area: 'Napo', photos: 1 });
  // Without a folder the photo is not kept (path null); the Tena photo is gone, so the next (Sucumbíos) is sent.
  assert.equal(out.images.length, 1);
  assert.equal(out.images[0].in_area, false);
  assert.match(out.images[0].caption, /outside Napo: Gonzalo Pizarro, Sucumbíos/);
  assert.match(out.photos_note, /1 photo/);
  assert.deepEqual(out.photos_left_out, ['Cestrum megalophyllum']);
  // host_plants takes the same area.
  const hp = await createMapsTools({ fetchImpl: web.fetchImpl, mapsBase: MAPS }).run('host_plants', { butterfly: 'Oleria onega', area: 'Napo', photos: 0 });
  assert.equal(hp.area.admin.name, 'Napo');
  assert.equal(hp.plants.find(p => p.plant === 'Solanum mite').records, 2);
  assert.match(hp.area.where, /Napo \(province of Ecuador\)/);
});
