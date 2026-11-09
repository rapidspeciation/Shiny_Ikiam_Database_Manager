// maps_snapshot's app picture (server/maps-render.mjs, server/maps-render-worker.mjs): the checks of what the app
// returns, the worker process (with a stand-in Playwright module: tests/fixtures/fake-playwright.mjs, so the real
// worker runs, but no browser), and maps_snapshot choosing the app or Pillow. MAPS_RENDER_LIVE=1: one render of the
// deployed app (needs PLAYWRIGHT_MODULE, a deployed export interface and network).
import test from 'node:test';
import assert from 'node:assert/strict';
import { spawn, spawnSync } from 'node:child_process';
import { existsSync, mkdirSync, mkdtempSync, readFileSync, readdirSync, rmSync, utimesSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { Worker } from 'node:worker_threads';
import { deflateSync } from 'node:zlib';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createMapsTools, mapsUrl, sexOf, yearOf } from '../server/maps-tools.mjs';
import { EXPORT, OFFICIAL_MAPS_URL, checkExport, clearingFrame, createMapRenderer, decodePng, fitBoundsView, pixelChecks, procStat, procTable, processTree, projectPoint, renderUrlProblem, treeRss, unsupportedFilter, visibleBounds } from '../server/maps-render.mjs';
import { LEGEND_BOTTOM_LEFT, OVERLAYS, withGeometry } from './fixtures/export-geometry.mjs';
import { BBOX, EXPECT, GREEN, PNG_OK, REC, S, URL_OK, alive, answer, chunk, crc, dirOf, drawPng, encodePng, fakeModule, fakes, made, mapPng, started, waitFor } from './helpers/maps-render.mjs';

const pillow = spawnSync('python3', ['-c', 'import PIL'], { stdio: 'ignore' }).status === 0;

// ---------- maps_snapshot: the app picture or Pillow ----------
const MAPS = 'https://maps.test/ithomiini_maps/';
const pt = (id, name, ssp, sex, lat, lng, source, place, date) => ({ id, scientific_name: name, genus: name.split(' ')[0], species: name.split(' ')[1], subspecies: ssp, sex, lat, lng, source, collection_location: place, observation_date: date, country: 'Ecuador' });
const POINTS = {
  'map_points_sanger.json': [
    pt('CAM000011', 'Tithorea harmonia', 'hermias', 'female', -0.99, -77.81, 'Sanger Institute', 'Tena', '2022-01-02'),
    pt('CAM000012', 'Tithorea harmonia', null, 'female', -0.991, -77.812, 'Sanger Institute', 'Tena', '2023-05-01'),
    pt('CAM000013', 'Tithorea harmonia', 'hermias', 'female', -0.71, -77.19, 'Sanger Institute', 'Loreto', '2022-07-01'),
    pt('CAM000014', 'Tithorea harmonia', 'hermias', 'male', -0.99, -77.81, 'Sanger Institute', 'Tena', '2022-01-02'),
    // Loja: over 150 km from Tena.
    pt('CAM000015', 'Tithorea harmonia', 'hermias', 'female', -3.99, -79.2, 'Sanger Institute', 'Loja', '2021-01-01'),
    // No sex in the maps; the workbook says female.
    pt('CAM000016', 'Oleria onega', null, null, -0.99, -77.81, 'Sanger Institute', 'Tena', '2022-01-02'),
  ],
  'map_points_inaturalist.json': [pt('9001', 'Tithorea harmonia', null, null, -0.95, -77.86, 'iNaturalist', 'Ikiam', '2019-08-12T13:15')],
};
const FILES = {
  'data_manifest.json': { generated_at: '2026-10-01T00:00:00Z', sources: { 'Sanger Institute': { file: 'map_points_sanger.json' }, iNaturalist: { file: 'map_points_inaturalist.json' } } },
  ...POINTS,
  'taxonomy_synonyms.json': { synonyms: [] },
};
const APP_RECORDS = Object.values(POINTS)
  .flat()
  .map(p => ({ source: p.source, id: p.id, name: p.scientific_name, ssp: p.subspecies, sex: sexOf(p.sex), country: p.country, year: yearOf(p.observation_date), lng: p.lng, lat: p.lat, place: p.collection_location }));
const fetchImpl = async url => {
  if (url.startsWith('https://tiles.test/')) return new Response(encodePng(1, 1, () => [200, 200, 200]));
  const path = new URL(url).pathname.replace('/ithomiini_maps/data/', '');
  return path in FILES ? Response.json(FILES[path]) : new Response('missing', { status: 404 });
};
const appPngFile = (() => {
  let file = null;
  return () => {
    if (!file) writeFileSync((file = join(dirOf('png'), 'app.png')), mapPng());
    return file;
  };
})();
async function snapshotTools(app = {}, env = null) {
  // CAM000011: the workbook says male, the maps female; the maps' sex wins, so the app's filter agrees with the tool.
  const seed = { Collection_data: [{ row: 2, values: { CAM_ID: 'CAM000016', SPECIES: 'Oleria onega', Sex: 'female', Country: 'Ecuador' } }, { row: 3, values: { CAM_ID: 'CAM000014', SPECIES: 'Tithorea harmonia', Sex: 'male', Country: 'Ecuador' } }, { row: 4, values: { CAM_ID: 'CAM000011', SPECIES: 'Tithorea harmonia', Sex: 'male', Country: 'Ecuador' } }] };
  const store = new Store({ localMode: true }, { sheets: new LocalSheets(seed) });
  await store.sync({ sheets: Object.keys(seed) });
  const fake = fakeModule({ app: { records: APP_RECORDS, png: appPngFile(), ...app } });
  const renderOptions = env ?? { env: { PLAYWRIGHT_MODULE: fake.module }, memAvailable: () => 8 * 1024 ** 3, log: () => {} };
  const tools = createMapsTools({ store, fetchImpl, mapsBase: MAPS, snapshotDir: dirOf('snap'), snapshotOptions: { tileUrl: 'https://tiles.test/{z}/{x}/{y}' }, renderOptions });
  return { tools, fake };
}

test('maps_snapshot, app picture: best site + 150 km, parity checked, labels and caption from what the frame shows', async () => {
  const { tools, fake } = await snapshotTools();
  const out = await tools.run('maps_snapshot', { species: ['Tithorea harmonia'], sex: 'female', country: ['Ecuador'], lang: 'es' });
  assert.equal(out.renderer, 'app', JSON.stringify(out.degraded));
  assert.equal(out.degraded, null);
  assert.equal(out.image_type, 'png');
  assert.equal(out.image_size, '1600x1200');
  assert.ok(existsSync(out.image) && readFileSync(out.image).length === out.image_bytes);
  // The app was asked the tool's frame (Tena and Loreto, not Loja) by bbox=, with export=1, the sites layer and settings.
  const asked = new URL(fake.get('url')[0]);
  assert.equal(fake.get('url').length, 1, 'a picture that checks out is drawn once (no padded retry)');
  assert.equal(out.framed.padded, undefined);
  assert.equal(`${asked.origin}${asked.pathname}`, OFFICIAL_MAPS_URL);
  assert.equal(asked.searchParams.get('export'), '1');
  assert.deepEqual([asked.searchParams.get('style'), asked.searchParams.get('color_by'), asked.searchParams.get('sex')], ['light', 'subspecies', 'female']);
  const box = asked.searchParams.get('bbox').split(',').map(Number);
  assert.ok(box[0] < -77.81 && box[2] > -77.19 && box[1] > -3.99, box);
  assert.deepEqual(out.framed.asked, box);
  // Four females in all (Loja's too), three in the frame; one without a subspecies.
  assert.equal(out.records.on_picture, 3);
  assert.equal(out.records.app_total, 4);
  assert.equal(out.legend_unknown, 1);
  assert.deepEqual(out.picture_labels.map(l => [l.rank, l.site, l.label_on_picture]), [[1, 'Tena', 'Tena'], [2, 'Loreto', 'Loreto']]);
  assert.deepEqual(out.sites.map(s => s.site), ['Tena', 'Loreto']);
  assert.ok(out.sites.every(s => !('on_map' in s)), 'no numbers promised on the app picture');
  assert.deepEqual(out.elsewhere.map(s => s.site), ['Loja']);
  assert.equal(out.caption, 'Tithorea harmonia · hembras · Ecuador; 4 registros en 3 sitios, 3 en el mapa; 1 sin subespecie; 2021–2023');
  // The link: the same filters, sites layer, basemap, colour and frame, without export mode.
  const link = new URL(out.map_url);
  assert.equal(link.searchParams.has('export'), false);
  for (const k of ['sp', 'source', 'country', 'sex', 'sites', 'site_labels', 'site_min', 'style', 'color_by', 'bbox']) assert.equal(link.searchParams.get(k), asked.searchParams.get(k), k);
});

test('maps_snapshot, app picture: a bbox asked frames the app\'s whole filtered set; the tool\'s records are the ones inside it', async () => {
  const { tools } = await snapshotTools();
  const out = await tools.run('maps_snapshot', { species: ['Tithorea harmonia'], sex: 'female', bbox: [-77.9, -1.05, -77.7, -0.95] });
  assert.equal(out.renderer, 'app', JSON.stringify(out.degraded));
  assert.equal(out.records.female, 2);
  assert.equal(out.records.app_total, 4);
  assert.equal(out.framed.by, 'bbox asked');
  // frame: 'all' frames every record, Loja too.
  const all = await tools.run('maps_snapshot', { species: ['Tithorea harmonia'], sex: 'female', frame: 'all' });
  assert.equal(all.renderer, 'app', JSON.stringify(all.degraded));
  assert.equal(all.records.on_picture, 4);
  assert.equal(all.framed.by, 'all records');
});

test('maps_snapshot, app picture: a named site under the legend is drawn once more with a frame that clears it; an app without geometry falls back', async () => {
  // The legend left in its default bottom-left corner, over Tena (rank 1) in the first frame; the padded frame moves Tena inward.
  const { tools, fake } = await snapshotTools({ geometry: { overlays: [OVERLAYS[0], LEGEND_BOTTOM_LEFT, OVERLAYS[2], OVERLAYS[3]] } });
  const covered = await tools.run('maps_snapshot', { species: ['Tithorea harmonia'], sex: 'female', country: ['Ecuador'] });
  assert.equal(covered.renderer, 'app', JSON.stringify(covered.degraded));
  assert.ok(covered.framed.padded);
  assert.equal(covered.framed.redrawn, 1, 'the frame worked out from the geometry clears it at the first redraw');
  assert.equal(fake.get('url').length, 2);
  const [first, second] = fake.get('url').map(u => new URL(u).searchParams.get('bbox').split(',').map(Number));
  assert.ok(second[0] <= first[0] && second[1] <= first[1] && second[2] >= first[2] && second[3] >= first[3], 'the second frame holds the first');
  assert.ok(second.some((x, i) => x !== first[i]), 'and is wider');
  assert.deepEqual(covered.picture_labels.map(l => l.site), ['Tena', 'Loreto']);
  // The app interface of 16bd1f2 (no picture geometry): not trusted.
  const legacy = await (await snapshotTools({ geometry: false })).tools.run('maps_snapshot', { species: ['Tithorea harmonia'], sex: 'female', country: ['Ecuador'] });
  assert.equal(legacy.renderer, 'pillow');
  assert.equal(legacy.degraded.reason, 'geometry_missing');
});

test('maps_snapshot, app picture with a bbox: the caption separates the area asked from what the frame shows around it', async () => {
  const { tools } = await snapshotTools();
  // One female record (Loreto) in the box; the 4:3 frame is wider than the box.
  const one = await tools.run('maps_snapshot', { species: ['Tithorea harmonia'], sex: 'female', bbox: [-77.25, -0.75, -77.15, -0.68], lang: 'es' });
  assert.equal(one.renderer, 'app', JSON.stringify(one.degraded));
  assert.equal(one.records.asked_on_picture, 1);
  assert.match(one.caption, /; 1 registro en 1 sitio en la zona pedida, 1 visible en la imagen; /);
  // Tena's two in the box; the frame also shows records around it, counted apart.
  const box = [-77.9, -1.05, -77.7, -0.95];
  const two = await tools.run('maps_snapshot', { species: ['Tithorea harmonia'], sex: 'female', bbox: box, lang: 'en' });
  assert.equal(two.renderer, 'app', JSON.stringify(two.degraded));
  assert.equal(two.records.asked_on_picture, 2);
  const around = two.records.on_picture - two.records.asked_on_picture;
  assert.match(two.caption, /; 2 records at 1 site in the area asked, 2 visible in the picture(; |$)/);
  if (around > 0) assert.match(two.caption, new RegExp(`the picture also shows ${around} records? just outside the area`));
  else assert.doesNotMatch(two.caption, /just outside/);
});

test('maps_snapshot, no sex asked: map-native sexes compared, the workbook not consulted for a fallback', async () => {
  const { tools } = await snapshotTools();
  const out = await tools.run('maps_snapshot', { species: ['Tithorea harmonia', 'Oleria onega'] });
  assert.equal(out.renderer, 'app', JSON.stringify(out.degraded));
  assert.equal(new URL(out.map_url).searchParams.get('color_by'), 'species');
});

test('maps_snapshot falls back to Pillow with the reason and the same records, frame and ranking', { skip: !pillow && 'no python3 with Pillow' }, async () => {
  const pillowOnly = (await snapshotTools({}, false)).tools;
  const plain = await pillowOnly.run('maps_snapshot', { species: ['Tithorea harmonia'], sex: 'female' });
  assert.equal(plain.renderer, 'pillow');
  assert.equal(plain.degraded.reason, 'renderer_unavailable');
  assert.deepEqual(plain.degraded.not_kept, ['basemap', 'color_by', 'site_labels', 'legend_style']);
  assert.deepEqual(plain.picture_labels.map(l => l.label_on_picture), ['1', '2']);
  const same = out => {
    assert.deepEqual(out.sites.map(s => [s.on_map, s.rank, s.site, s.records]), plain.sites.map(s => [s.on_map, s.rank, s.site, s.records]));
    assert.deepEqual(out.records, plain.records);
    assert.deepEqual(out.framed, plain.framed);
  };
  // Coordinates moved by the app: parity mismatch.
  const moved = await (await snapshotTools({ shift: 0.001 })).tools.run('maps_snapshot', { species: ['Tithorea harmonia'], sex: 'female' });
  assert.equal(moved.renderer, 'pillow');
  assert.equal(moved.degraded.reason, 'parity_mismatch');
  assert.equal(moved.degraded.detail.fields.coords, 4);
  same(moved);
  // The app ignored bbox=: wrong view.
  const ignored = await (await snapshotTools({ ignoreBbox: true })).tools.run('maps_snapshot', { species: ['Tithorea harmonia'], sex: 'female' });
  assert.equal(ignored.degraded.reason, 'view_mismatch');
  same(ignored);
  // A filter the app does not have: no browser started.
  const { tools, fake } = await snapshotTools();
  const years = await tools.run('maps_snapshot', { species: ['Tithorea harmonia'], sex: 'female', years: { from: 2022 } });
  assert.equal(years.degraded.reason, 'unsupported_filter:years');
  assert.equal(years.records.female, (await tools.run('maps_localities', { taxa: ['Tithorea harmonia'], sex: 'female', years: { from: 2022 } })).records.female);
  // A sex asked that the workbook fills in for one of the project's records.
  const filled = await tools.run('maps_snapshot', { species: ['Oleria onega'], sex: 'female' });
  assert.equal(filled.degraded.reason, 'sex_from_workbook');
  assert.equal(filled.degraded.detail.records_affected, 1);
  assert.deepEqual(fake.get('url'), []);
});

test('live: one render of the deployed maps app', { skip: process.env.MAPS_RENDER_LIVE !== '1' && 'MAPS_RENDER_LIVE=1 to run' }, async () => {
  const r = createMapRenderer({});
  const url = mapsUrl({ species: ['Tithorea harmonia'], sources: ['sanger', 'gbif_unam', 'gbif_other', 'dore', 'inaturalist'], country: ['Ecuador'], sex: 'female', sites: true, style: 'light', colorBy: 'subspecies', bbox: [-77.95, -1.2, -77.1, -0.6], exportMode: true }, OFFICIAL_MAPS_URL);
  const out = await r.render(url);
  console.log(JSON.stringify({ ok: out.ok, reason: out.reason, detail: out.detail, timing: out.timing, version: out.result?.version, records: out.result?.records?.length }));
  assert.equal(out.ok, true);
  assert.equal(out.result.version, 1);
});


test('process tree revalidates newly discovered identities before stopping them', async () => {
  const proc = dirOf('rescan-reuse');
  const stat = (pid, ppid, start) => {
    mkdirSync(join(proc, String(pid)), { recursive: true });
    writeFileSync(join(proc, String(pid), 'stat'), `${pid} (fake) S ${ppid} ${pid} ${'0 '.repeat(16)}${start} 0 0\n`);
  };
  stat(100, 1, 10);
  stat(200, 100, 20);
  const sent = [];
  const tree = processTree({ pid: 100, alive: () => true, group: true }, { proc, sig: (pid, signal) => {
    sent.push([pid, signal]);
    if (pid === 200 && signal === 'SIGSTOP') {
      stat(300, 200, 30);
      stat(400, 200, 40);
    }
    // Both new children were in the rescan snapshot. Reuse the second PID
    // between its discovery and the callback which would send SIGSTOP.
    if (pid === 300 && signal === 'SIGSTOP') stat(400, 1, 99);
    if (pid > 0 && signal === 'SIGKILL') rmSync(join(proc, String(pid)), { recursive: true, force: true });
  } });
  tree.observe();
  await tree.reap({ waitMs: 100 });
  assert.ok(sent.some(([pid, signal]) => pid === 300 && signal === 'SIGSTOP'));
  assert.ok(!sent.some(([pid]) => Math.abs(pid) === 400), 'replacement must receive no signal');
  assert.equal(procStat(400, proc).start, 99);
});
