import test from 'node:test';
import assert from 'node:assert/strict';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createSpeciesPhotos, lookupNames, pickTaxon } from '../server/species-photos.mjs';

const LEP = [48460, 1, 47120, 47157];
const photo = (id, license = 'cc-by-nc') => ({
  id,
  url: `https://inaturalist-open-data.s3.amazonaws.com/photos/${id}/square.jpg`,
  license_code: license,
  attribution: `(c) Person ${id}, some rights reserved`,
});
// A small iNaturalist: search results by query, taxa by id, observations by taxon.
const SEARCH = {
  'Oleria onega janarilla': [],
  'Oleria onega': [{ id: 10, name: 'Oleria onega', rank: 'species', matched_term: 'Oleria onega', ancestor_ids: LEP }],
  // Known to iNaturalist under its new name.
  'Hyposcada anchiala': [{ id: 20, name: 'Hyposcada kezia', rank: 'species', matched_term: 'Hyposcada anchiala', ancestor_ids: LEP }],
  'Godyris zavaleta': [{ id: 30, name: 'Godyris zavaleta', rank: 'species', matched_term: 'Godyris zavaleta', ancestor_ids: LEP }],
  // A plant genus of the same name first: not a butterfly.
  Napeogenes: [
    { id: 40, name: 'Napeogenes', rank: 'genus', matched_term: 'Napeogenes', ancestor_ids: [47126] },
    { id: 41, name: 'Napeogenes', rank: 'genus', matched_term: 'Napeogenes', ancestor_ids: LEP },
  ],
  'Napeogenes inachia': [],
};
const TAXA = {
  10: { id: 10, name: 'Oleria onega', rank: 'species', taxon_photos: [{ photo: photo(1) }, { photo: photo(2, null) }] },
  20: { id: 20, name: 'Hyposcada kezia', rank: 'species', taxon_photos: [{ photo: photo(3) }] },
  // Only photos with all rights reserved: observations instead.
  30: { id: 30, name: 'Godyris zavaleta', rank: 'species', taxon_photos: [{ photo: photo(4, null) }] },
  41: { id: 41, name: 'Napeogenes', rank: 'genus', taxon_photos: [{ photo: photo(5) }] },
};
const OBS = { 30: [{ id: 900, photos: [photo(6, null), photo(7)] }] };

function fakeInat({ busy = false } = {}) {
  const calls = [];
  let clock = 0;
  const fetchImpl = async url => {
    const u = new URL(url);
    calls.push({ at: clock, path: u.pathname + u.search });
    if (busy) return new Response('slow down', { status: 429 });
    let body;
    if (u.pathname === '/v1/taxa') body = { results: SEARCH[u.searchParams.get('q')] ?? [] };
    else if (u.pathname.startsWith('/v1/taxa/')) body = { results: [TAXA[u.pathname.split('/')[3]]] };
    else if (u.pathname === '/v1/observations') body = { results: OBS[u.searchParams.get('taxon_id')] ?? [] };
    return new Response(JSON.stringify(body), { status: 200, headers: { 'content-type': 'application/json' } });
  };
  return {
    calls,
    options: {
      fetchImpl,
      now: () => clock,
      sleep: async ms => void (clock += ms),
    },
    advance: ms => void (clock += ms),
  };
}

const seed = {
  Collection_data: [
    ['CAM000001', 'Oleria onega', 'janarilla', 'female'],
    ['CAM000002', 'Oleria onega', 'janarilla', 'male'],
    ['CAM000003', 'Oleria onega', 'lota', 'female'],
    ['CAM000004', 'Oleria onega', '', 'female'],
  ].map(([cam, SPECIES, Subspecies_Form, Sex], i) => ({
    row: i + 2,
    values: { CAM_ID: cam, SPECIES, Subspecies_Form, Sex, Release_Collect: 'Collected_Preserved' },
  })),
  Photo_links: ['CAM000001d', 'CAM000001v', 'CAM000002d', 'CAM000003d', 'CAM000004v'].map((name, i) => ({
    row: i + 2,
    values: { Name: `${name}.JPG`, URL: `https://drive.google.com/file/d/1${name.padEnd(30, 'x')}/view` },
  })),
};
async function storeWithData() {
  const store = new Store({ localMode: true }, { sheets: new LocalSheets(seed) });
  await store.sync({ sheets: Object.keys(seed) });
  return store;
}

test('names to look up: subspecies, then species, then genus; forms and notes dropped', () => {
  const names = n => lookupNames(n)?.map(x => x.name);
  assert.deepEqual(names('Hyposcada illinissa ida'), ['Hyposcada illinissa ida', 'Hyposcada illinissa', 'Hyposcada']);
  assert.deepEqual(names('Eresia pelonia f. ithomiola'), ['Eresia pelonia', 'Eresia']);
  assert.deepEqual(names('Castilia perilla (no subspecies described)'), ['Castilia perilla', 'Castilia']);
  assert.deepEqual(names('Hypothyris sp.'), ['Hypothyris']);
  assert.deepEqual(names('Oleria cf. onega'), ['Oleria onega', 'Oleria']);
  assert.equal(lookupNames('Sin especie'), null);
  assert.equal(lookupNames(''), null);
  // A subspecies iNaturalist does not have is not swapped for a synonym: the species is tried next.
  const results = [{ id: 1, name: 'Oleria onega', rank: 'species', matched_term: 'Oleria onega x', ancestor_ids: LEP }];
  assert.equal(pickTaxon(results, { name: 'Oleria onega x', level: 'subspecies' }), null);
});

test('iNaturalist photos: licensed taxon photos, synonyms, observation and genus fallbacks', async () => {
  const inat = fakeInat();
  const service = createSpeciesPhotos({ db: (await storeWithData()).db }, inat.options);
  const onega = await service.lookup('Oleria onega janarilla');
  assert.equal(onega.level, 'species');
  assert.equal(onega.taxon.url, 'https://www.inaturalist.org/taxa/10');
  // The photo with all rights reserved is left out.
  assert.deepEqual(
    onega.photos.map(p => [p.id, p.license, p.thumb.split('/').at(-1), p.url.split('/').at(-1)]),
    [[1, 'CC-BY-NC', 'small.jpg', 'large.jpg']],
  );
  assert.match(onega.photos[0].attribution, /Person 1/);
  const anchiala = await service.lookup('Hyposcada anchiala');
  assert.equal(anchiala.synonymOf, 'Hyposcada kezia');
  const godyris = await service.lookup('Godyris zavaleta');
  assert.deepEqual(
    godyris.photos.map(p => [p.id, p.observation]),
    [[7, 'https://www.inaturalist.org/observations/900']],
  );
  const napeogenes = await service.lookup('Napeogenes inachia');
  assert.equal(napeogenes.level, 'genus');
  assert.equal(napeogenes.taxon.id, 41);
  assert.equal(await service.lookup('Melinaea satevis'), null);
  // One request at a time, at least 1.1 s apart.
  const gaps = inat.calls.slice(1).map((c, i) => c.at - inat.calls[i].at);
  assert.ok(gaps.every(g => g >= 1100), String(gaps));
});

test('the page gets the cache now and pending names later; specimens come from Collection_data and Photo_links', async () => {
  const inat = fakeInat();
  const store = await storeWithData();
  const service = createSpeciesPhotos(store, inat.options);
  const first = service.photosOf(['Oleria onega janarilla', 'Sin especie', 'Oleria onega janarilla']);
  assert.deepEqual(Object.keys(first), ['Oleria onega janarilla', 'Sin especie']);
  assert.equal(first['Oleria onega janarilla'].pending, true);
  assert.equal(first['Sin especie'].pending, false);
  // Both sexes of the subspecies, then the others of the species with a dorsal photo (CAM000004 has only a ventral).
  assert.deepEqual(
    first['Oleria onega janarilla'].specimens.map(s => [s.cam, s.sex, !!s.ventral]),
    [
      ['CAM000001', 'female', true],
      ['CAM000002', 'male', false],
      ['CAM000003', 'female', false],
    ],
  );
  await service.idle();
  const requests = inat.calls.length;
  const second = service.photosOf(['Oleria onega janarilla']);
  assert.equal(second['Oleria onega janarilla'].pending, false);
  assert.equal(second['Oleria onega janarilla'].inat.taxon.name, 'Oleria onega');
  await service.idle();
  assert.equal(inat.calls.length, requests, 'cached: iNaturalist is not asked again');
  // After 30 days the old answer is still given while it is fetched again.
  inat.advance(31 * 864e5);
  const later = service.photosOf(['Oleria onega janarilla']);
  assert.equal(later['Oleria onega janarilla'].inat.taxon.name, 'Oleria onega');
  await service.idle();
  assert.ok(inat.calls.length > requests);
  assert.throws(() => service.photosOf('Oleria onega'), { code: 'INVALID_NAMES' });
});

test('iNaturalist busy: nothing is cached, the name is retried later, and turned off it is never asked', async () => {
  const busy = fakeInat({ busy: true });
  const store = await storeWithData();
  const service = createSpeciesPhotos(store, busy.options);
  service.photosOf(['Oleria onega']);
  await service.idle();
  assert.equal(busy.calls.length, 1);
  const again = service.photosOf(['Oleria onega']);
  assert.deepEqual([again['Oleria onega'].pending, again['Oleria onega'].inat], [false, null]);
  await service.idle();
  assert.equal(busy.calls.length, 1, 'not asked again before the retry time');
  assert.equal(store.db.prepare('SELECT count(*) n FROM species_photo_cache').get().n, 0);

  const off = fakeInat();
  const offline = createSpeciesPhotos(store, { ...off.options, enabled: false });
  const out = offline.photosOf(['Oleria onega']);
  assert.equal(out['Oleria onega'].pending, false);
  assert.equal(out['Oleria onega'].specimens.length, 3);
  await offline.idle();
  assert.equal(off.calls.length, 0);
});
