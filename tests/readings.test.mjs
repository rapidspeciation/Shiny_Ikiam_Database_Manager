// The background photo readings (server/readings.mjs) with a stand-in for their worker process
// (tests/fixtures/fake-readings-worker.mjs) and for the identifier's Python worker (tests/fixtures/fake-specimen.mjs):
// which photos wait, a batch written to the tables, a restart going on where it was, one gallery update per batch
// (debounced), and the Wings Gallery's files in the formats agreed with it (/tmp/gallery-readings-spec.md).
import test from 'node:test';
import assert from 'node:assert/strict';
import http from 'node:http';
import { spawnSync } from 'node:child_process';
import { existsSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { gunzipSync } from 'node:zlib';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createApp } from '../server/index.mjs';
import { importBundle } from '../server/envelope-import.mjs';
import { createReadings, initReadingTables, listedPhotos, nextBatch, pendingPhotos } from '../server/readings.mjs';
import { FILES, galleryData, photoKey, writeGalleryFiles } from '../server/gallery-public.mjs';

const FAKE = new URL('./fixtures/fake-readings-worker.mjs', import.meta.url).pathname;
const FAKE_PY = new URL('./fixtures/fake-specimen.mjs', import.meta.url).pathname;
const WORKER = new URL('../server/readings-worker.mjs', import.meta.url).pathname;
const id = (name, n = 0) => `${name.replace(/\W/g, '')}_${'0'.repeat(20)}${n}`.slice(0, 33);

const link = (row, name, fileId, size = 812, folder = 'Photos wings of collected butterflies/JPG_photos') => ({
  row,
  values: { 'Full Path': `${folder}/${name}`, Name: name, Type: 'File', URL: `https://drive.google.com/file/d/${fileId}/view?usp=drivesdk`, 'Size KB': size },
});
const collected = (row, cam, SPECIES, more = {}) => ({ row, values: { Release_Collect: 'Collected_Preserved', CAM_ID: cam, SPECIES, Collection_location: 'Ikiam', ...more } });

function seedOf(extra = []) {
  const names = ['CAM000201d.JPG', 'CAM000201v.JPG', 'CAM000202d.JPG', 'CAM000202v.JPG', 'CAM000203d.JPG', ...extra];
  return {
    Collection_data: [
      collected(2, 'CAM000201', 'Mechanitis polymnia proceriformis', { Side_Andes: 'East', Country: 'Ecuador' }),
      collected(3, 'CAM000202', 'Oleria gunilla', { Side_Andes: 'West', Country: 'Ecuador' }),
      collected(4, 'CAM000203', 'Ithomia salapia', { Country: 'Peru' }),
    ],
    Photo_links: [
      { row: 2, values: { 'Full Path': 'Photos wings of collected butterflies', Name: 'Photos wings of collected butterflies', Type: 'Folder', URL: `https://drive.google.com/drive/folders/${id('folder')}` } },
      ...names.map((name, i) => link(i + 3, name, id(name))),
      link(20, 'CAM000204d_todelete.JPG', id('todelete')),
      link(21, 'IMG_0001.JPG', id('img')),
      link(22, 'CAM000201d.CR2', id('raw')),
    ],
  };
}
async function storeOf(t, extra = []) {
  const dir = mkdtempSync(join(tmpdir(), 'readings-'));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  const seed = seedOf(extra);
  const store = new Store({ localMode: true, databasePath: join(dir, 'app.sqlite') }, { sheets: new LocalSheets(seed) });
  await store.sync({ sheets: Object.keys(seed) });
  t.after(() => store.close());
  return { store, dir };
}
/** What the backfill's import gives a photo (read already). */
const backfilled = (name, fileId) => ({
  wings: [{ name, fileId, cam: name.slice(0, 9), suffix: name.slice(9), source: 'v5n_20261018', side: 'dorsal', wings: 4, wingList: [], size: [1600, 1067] }],
  photoPredictions: [{ fileId, name, cam: name.slice(0, 9), view: name[9], species: [['Oleria gunilla', 0.9]] }],
});
function service(store, dir, options = {}) {
  const calls = [];
  const siteUpdates = options.siteUpdates ?? {
    ready: () => true,
    start: async key => {
      calls.push(key);
      return options.answer?.(calls.length) ?? { site: 'Wings Gallery', update: { state: 'running', link: 'https://github.com/x/actions/runs/1' } };
    },
  };
  const logFile = join(dir, 'jobs.jsonl');
  const readings = createReadings({
    store,
    publicDir: join(dir, 'public'),
    siteUpdates,
    workerArgs: [FAKE, '--log', logFile],
    build: async (db, out) => writeGalleryFiles(out, galleryData(db)),
    debounceMs: 40,
    maxWaitMs: 1000,
    retryMs: 40,
    log: () => {},
    ...options,
  });
  return { readings, calls, jobs: () => (existsSync(logFile) ? readFileSync(logFile, 'utf8').trim().split('\n').filter(Boolean).map(l => JSON.parse(l)) : []) };
}
const sleep = ms => new Promise(resolve => setTimeout(resolve, ms));

test('the photos to read: each Drive file listed under a butterfly, not read with that name and size', async t => {
  const { store } = await storeOf(t);
  const listed = listedPhotos(store.db);
  assert.deepEqual(
    listed.map(p => p.name),
    ['CAM000201d', 'CAM000201v', 'CAM000202d', 'CAM000202v', 'CAM000203d'],
    'not the folder, the RAW file, a …_todelete photo or a camera name',
  );
  assert.deepEqual(listed[0], {
    fileId: id('CAM000201d.JPG'),
    name: 'CAM000201d',
    cam: 'CAM000201',
    suffix: 'd',
    folder: 'Photos wings of collected butterflies/JPG_photos',
    sizeKb: 812,
    stamp: 'CAM000201d|812',
  });
  // The backfill read CAM000202's photos: they wait no more (its d under another name: read again).
  importBundle(store.db, {
    wings: [...backfilled('CAM000202v', id('CAM000202v.JPG')).wings, ...backfilled('CAM000202x', id('CAM000202d.JPG')).wings],
    photoPredictions: [...backfilled('CAM000202v', id('CAM000202v.JPG')).photoPredictions, ...backfilled('CAM000202d', id('CAM000202d.JPG')).photoPredictions],
  });
  assert.deepEqual(
    pendingPhotos(store.db).map(p => p.name),
    ['CAM000201d', 'CAM000201v', 'CAM000202d', 'CAM000203d'],
  );
  // A batch: whole butterflies, what the workbook says of each, the sex model's d and v.
  const batch = nextBatch(store.db, { size: 2 });
  assert.equal(batch.pending, 4);
  assert.deepEqual(
    batch.cams.map(c => [c.cam, c.side, c.ecuador, c.species, c.photos.map(p => `${p.name}${p.wings ? '+wings' : ''}`), c.pending.length]),
    [['CAM000201', 'East', true, 'Mechanitis polymnia', ['CAM000201d+wings', 'CAM000201v+wings'], 2]],
  );
  assert.deepEqual(batch.cams[0].sex, { d: id('CAM000201d.JPG'), v: id('CAM000201v.JPG') });
  const all = nextBatch(store.db, { size: 40 });
  assert.deepEqual(
    all.cams.map(c => [c.cam, c.photos.map(p => `${p.name}${p.wings ? '+wings' : ''}`), c.sex]),
    [
      ['CAM000201', ['CAM000201d+wings', 'CAM000201v+wings'], { d: id('CAM000201d.JPG'), v: id('CAM000201v.JPG') }],
      // All its photos (the species is their mean), the wings of the one not read.
      ['CAM000202', ['CAM000202d+wings', 'CAM000202v'], { d: id('CAM000202d.JPG'), v: id('CAM000202v.JPG') }],
      ['CAM000203', ['CAM000203d+wings'], { missing: 'no_v_photo' }],
    ],
  );
  assert.equal(all.cams[2].ecuador, false);
  assert.equal(all.cams[2].side, null);
});

test('a batch: read by the worker, written to the tables butterfly by butterfly, the files made, the gallery updated once', async t => {
  const { store, dir } = await storeOf(t);
  // The update's wait longer than the batch takes on a busy machine (a process for it, another to see nothing is left).
  const { readings, calls, jobs } = service(store, dir, { debounceMs: 1500 });
  t.after(() => readings.close());
  assert.deepEqual(readings.run('test'), { started: true, reason: 'test' });
  assert.equal(readings.run('again').running, true, 'one run at a time');
  await readings.idle();
  const db = store.db;
  // What the worker was asked: each butterfly once, its photos with the size that tells copies apart.
  const asked = jobs();
  assert.deepEqual(asked.map(j => j.cam), ['CAM000201', 'CAM000202', 'CAM000203']);
  assert.deepEqual(asked[0].photos.map(p => p.dup), ['d|812', 'v|812']);
  assert.equal(asked[0].side, 'East');
  assert.equal(asked[0].species, 'Mechanitis polymnia');
  assert.equal(asked[2].sex, null, 'no v photo: no sex');
  // Wings: one row per photo, the pair checked, the outlines kept.
  const w = db.prepare("SELECT * FROM photo_wing_sides WHERE name='CAM000201v'").get();
  assert.equal(w.source, 'app_readings');
  assert.equal(w.model, 'wingseg_v5n_640 f3c4f755 + wing_side_cls 838b9371 + wingside.json 48a70be6');
  assert.equal(w.side, 'ventral');
  assert.equal(w.wings, 5);
  assert.equal(JSON.parse(w.outlines_json).length, 5);
  assert.deepEqual(JSON.parse(w.outlines_json)[0], ['fw', 'l', [100, 100, 300, 100, 300, 300, 100, 300]]);
  assert.equal(db.prepare("SELECT cam_reasons FROM photo_wing_sides WHERE name='CAM000203d'").get().cam_reasons, 'suffix-missing:no v; single-photo');
  assert.ok(db.prepare("SELECT box_json FROM photo_wing_boxes WHERE name='CAM000201d'").get());
  // Species per photo and per butterfly, sex.
  const p = db.prepare("SELECT * FROM photo_predictions WHERE cam='CAM000201'").get();
  assert.deepEqual(JSON.parse(p.species_json)[0], ['Genus CAM000201', 0.8]);
  assert.match(p.model, /feature mean of 2 photos/);
  assert.deepEqual([p.sex, p.sex_conf, p.sex_supported, p.sex_p_male, p.sex_status, p.sex_species, p.andes], ['male', 0.93, 1, 0.93, 'species_supported', 'Mechanitis polymnia', 'East']);
  assert.equal(db.prepare("SELECT sex_status FROM photo_predictions WHERE cam='CAM000203'").get().sex_status, 'not_scored:no_v_photo');
  assert.equal(db.prepare('SELECT count(*) n FROM photo_file_predictions').get().n, 5);
  assert.deepEqual(db.prepare("SELECT status, count(*) n FROM photo_reading_files GROUP BY status").all().map(r => [r.status, r.n]), [['ok', 5]]);
  assert.deepEqual(pendingPhotos(db), []);
  // The files made, then the gallery's update once the wait is over.
  assert.ok(existsSync(join(dir, 'public', 'index.json')));
  assert.deepEqual(calls, []);
  assert.equal(readings.status().state, 'idle');
  assert.ok(readings.status().galleryUpdate.due);
  await sleep(1600);
  assert.deepEqual(calls, ['gallery']);
  assert.equal(readings.status().galleryUpdate.last.state, 'running');
  assert.deepEqual(readings.status().last, { endedAt: readings.status().last.endedAt, photos: 5, readings: 5, errors: 0, state: 'done' });
  // Nothing new: no batch, no update.
  readings.run('again');
  await readings.idle();
  await sleep(80);
  assert.deepEqual(calls, ['gallery']);
  assert.equal(jobs().length, 3);
});

test('batches ending within the wait start one update; one already running is started again later', async t => {
  const { store, dir } = await storeOf(t);
  const { readings, calls } = service(store, dir, {
    batchPhotos: 1,
    // Longer than a batch takes here (a process each).
    debounceMs: 600,
    retryMs: 200,
    answer: n => (n === 1 ? { update: { state: 'running', note: 'an update was already running: this one was not started' } } : { update: { state: 'queued' } }),
  });
  t.after(() => readings.close());
  readings.run();
  await readings.idle();
  assert.equal(store.db.prepare('SELECT count(*) n FROM photo_reading_batches').get().n, 3, 'three batches');
  assert.deepEqual(calls, []);
  await sleep(650);
  assert.deepEqual(calls, ['gallery'], 'one for the three');
  assert.equal(readings.status().galleryUpdate.last.state, 'waiting');
  await sleep(250);
  assert.deepEqual(calls, ['gallery', 'gallery'], 'again after the running one');
  assert.equal(readings.status().galleryUpdate.last.state, 'queued');
  await sleep(250);
  assert.equal(calls.length, 2);
});

test('the wait for the update is at most maxWaitMs while batches keep ending', async t => {
  const { store, dir } = await storeOf(t);
  const { readings, calls } = service(store, dir, { debounceMs: 2000, maxWaitMs: 500 });
  t.after(() => readings.close());
  // Batches one after another (the same photo listed again, renamed, each time): each pushes the wait, but not
  // past half a second after the first.
  let first = null;
  for (let i = 0; i < 40 && !calls.length; i++) {
    store.db.prepare("UPDATE records SET values_json = json_set(values_json, '$.\"Size KB\"', ?) WHERE sheet='Photo_links' AND json_extract(values_json, '$.Name') = 'CAM000203d.JPG'").run(900 + i);
    readings.run();
    await readings.idle();
    first ??= Date.now();
    await sleep(20);
  }
  assert.deepEqual(calls, ['gallery']);
  assert.ok(Date.now() - first < 1500, `${Date.now() - first} ms`);
});

test('no password for the update server: the readings and files are made, the update is not started', async t => {
  const { store, dir } = await storeOf(t);
  const { readings } = service(store, dir, { siteUpdates: { ready: () => false, start: () => assert.fail('not started') } });
  t.after(() => readings.close());
  readings.run();
  await readings.idle();
  assert.match(readings.status().galleryUpdate.last.why, /SITE_UPDATE_PASSWORD_FILE/);
  assert.ok(existsSync(join(dir, 'public', 'wing_outlines.json')));
});

test('a restart goes on where it was; photos that fail are tried again later, three times at most', async t => {
  const { store, dir } = await storeOf(t);
  // A batch the app was reading when it stopped.
  initReadingTables(store.db);
  store.db.prepare("INSERT INTO photo_reading_batches(id, started_at, state) VALUES ('old', '2026-10-19T00:00:00Z', 'reading')").run();
  // CAM000201 was read before the stop; CAM000202's d cannot be downloaded; CAM000203's worker crashes.
  store.db.prepare("INSERT INTO photo_reading_files(file_id,name,cam,stamp,status,attempts) VALUES (?, 'CAM000201d', 'CAM000201', 'CAM000201d|812', 'ok', 0), (?, 'CAM000201v', 'CAM000201', 'CAM000201v|812', 'ok', 0)").run(id('CAM000201d.JPG'), id('CAM000201v.JPG'));
  let clock = Date.parse('2026-10-19T12:00:00Z');
  const first = service(store, dir, { now: () => clock });
  assert.equal(store.db.prepare("SELECT state FROM photo_reading_batches WHERE id='old'").get().state, 'stopped');
  t.after(() => first.readings.close());
  store.db.prepare("UPDATE records SET values_json = json_set(values_json, '$.URL', ?) WHERE sheet='Photo_links' AND json_extract(values_json, '$.Name') = 'CAM000202d.JPG'").run(`https://drive.google.com/file/d/${id('BAD202d')}/view`);
  store.db.prepare("UPDATE records SET values_json = json_set(values_json, '$.URL', ?) WHERE sheet='Photo_links' AND json_extract(values_json, '$.Name') = 'CAM000203d.JPG'").run(`https://drive.google.com/file/d/${id('CRASH203')}/view`);
  first.readings.run();
  await first.readings.idle();
  assert.deepEqual(first.jobs().map(j => j.cam), ['CAM000202', 'CAM000203'], 'CAM000201 not again');
  const files = Object.fromEntries(store.db.prepare('SELECT name, status, attempts, error FROM photo_reading_files').all().map(r => [r.name, [r.status, r.attempts, r.error]]));
  assert.deepEqual(files.CAM000202d, ['error', 1, 'Drive answered 404']);
  assert.deepEqual(files.CAM000202v, ['ok', 0, null]);
  assert.equal(files.CAM000203d[0], 'error');
  assert.match(files.CAM000203d[2], /stopped/);
  // The photo read keeps its butterfly's rows; the one that failed has an error row.
  assert.equal(store.db.prepare("SELECT error FROM photo_wing_sides WHERE name='CAM000202d'").get().error, 'Drive answered 404');
  // Not again at once; again after the wait, as a new app (a restart) would.
  assert.deepEqual(pendingPhotos(store.db, { now: clock }).map(p => p.name), []);
  first.readings.close();
  clock += 7 * 60 * 60_000;
  const second = service(store, dir, { now: () => clock });
  t.after(() => second.readings.close());
  second.readings.run();
  await second.readings.idle();
  assert.deepEqual(second.jobs().slice(2).map(j => j.cam), ['CAM000202', 'CAM000203']);
  assert.equal(store.db.prepare("SELECT attempts FROM photo_reading_files WHERE name='CAM000202d'").get().attempts, 2);
  clock += 7 * 60 * 60_000;
  second.readings.run();
  await second.readings.idle();
  clock += 7 * 60 * 60_000;
  assert.deepEqual(pendingPhotos(store.db, { now: clock }), [], 'three attempts: left until its file changes');
});

test("the gallery's files: the formats the gallery reads, nothing else", async t => {
  const { store, dir } = await storeOf(t, ['CAM000205d (2).JPG']);
  const { readings } = service(store, dir, { siteUpdates: { ready: () => false } });
  t.after(() => readings.close());
  readings.run();
  await readings.idle();
  // The gallery's own predictions (an earlier import, no model column) are not the app's to publish.
  store.db.prepare("INSERT INTO photo_predictions(cam, species_json, imported_at, source) VALUES ('CAM009999', '[[\"Oleria gunilla\", 0.9]]', 'x', 'gallery')").run();
  const data = galleryData(store.db);
  assert.deepEqual(Object.keys(data), FILES);
  const pred = data['predictions_app.json'];
  assert.deepEqual(Object.keys(pred), ['CAM000201', 'CAM000202', 'CAM000203', 'CAM000205']);
  assert.deepEqual(pred.CAM000201, {
    species: [
      ['Genus CAM000201', 0.8],
      ['Genus other', 0.1],
    ],
    subspecies: [['Genus CAM000201 sub', 0.7]],
    genus: [['Genus', 0.95]],
    side: 'East',
    n_views: 2,
    mode: 'app_readings',
    model_meta: {
      source: 'app_readings',
      model: 'bioclip-2.5-h int8 + head_hier 749fbffd + wing_seg_v5n d74f9c13 (tools/identifier) (feature mean of 2 photos)',
      photos: [
        { name: 'CAM000201d', file: id('CAM000201d.JPG') },
        { name: 'CAM000201v', file: id('CAM000201v.JPG') },
      ],
    },
  });
  assert.equal(pred.CAM000203.side, '');
  const sex = data['sex_predictions_app.json'];
  assert.deepEqual(Object.keys(sex), ['CAM000201', 'CAM000202'], 'only the butterflies the sex model scored');
  assert.deepEqual(sex.CAM000201, {
    sex: 'male',
    confidence: 0.93,
    male_probability: 0.93,
    supported: true,
    species: 'Mechanitis polymnia',
    screen_status: 'species_supported',
    model_arm: 'collection sex release_20261014 (VF_DH_PARTS+ANDRO_TILES role heads, 12 BioCLIP crops, cand_cv fold models, int8 encoder)',
    source: 'app_readings',
  });
  const boxes = data['wing_boxes_app.json'];
  assert.deepEqual(Object.keys(boxes).sort(), ['CAM000201d', 'CAM000201v', 'CAM000202d', 'CAM000202v', 'CAM000203d', 'CAM000205d_2']);
  // The fore- and hindwing outlines (not the extra one at 0.8–0.95), then 1.5 % of 1,067 px on each side.
  assert.deepEqual(boxes.CAM000201d, [{ box: [0.09, 0.085, 0.61, 0.515], conf: 0.92, model: 'v5n', union: true, file: id('CAM000201d.JPG') }]);
  const outlines = data['wing_outlines.json'];
  assert.deepEqual(Object.keys(outlines), ['version', 'source', 'model', 'photos']);
  assert.equal(outlines.version, 1);
  assert.deepEqual(outlines.photos.CAM000201d, {
    file: id('CAM000201d.JPG'),
    w: [
      ['fw', 'l', 'M100 100 300 100 300 300 100 300Z'],
      ['fw', 'r', 'M400 100 600 100 600 300 400 300Z'],
      ['hw', 'l', 'M120 350 280 350 280 500 120 500Z'],
      ['hw', 'r', 'M420 350 580 350 580 500 420 500Z'],
    ],
  });
  assert.equal(photoKey('CAM036744d (2).JPG'), 'CAM036744d_2');
  // Nothing about people: no notes, names of people or addresses anywhere.
  const all = JSON.stringify(data);
  assert.doesNotMatch(all, /@|Notes|Collector|Ikiam/);
  // The files and their index (counts, sha256).
  const index = JSON.parse(readFileSync(join(dir, 'public', 'index.json'), 'utf8'));
  assert.deepEqual(Object.keys(index.files), FILES);
  assert.equal(index.files['wing_outlines.json'].count, 6);
  assert.deepEqual(JSON.parse(gunzipSync(readFileSync(join(dir, 'public', 'predictions_app.json.gz')))), JSON.parse(readFileSync(join(dir, 'public', 'predictions_app.json'), 'utf8')));
});

test('/api/public/gallery/<file>: no login, gzip, ETag and Last-Modified, 404 before the files are made', async t => {
  const dir = mkdtempSync(join(tmpdir(), 'gallery-route-'));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  const store = new Store({ localMode: true }, { sheets: new LocalSheets({}) });
  const app = await createApp({ localMode: true, secureCookies: false, syncIntervalMs: 0, publicGalleryDir: join(dir, 'public') }, { store, skipInitialSync: true });
  const { port } = await app.listen(0, '127.0.0.1');
  t.after(() => app.close());
  const base = `http://127.0.0.1:${port}/ithomiini/api/public/gallery`;
  assert.equal((await fetch(`${base}/predictions_app.json`)).status, 404);
  writeGalleryFiles(join(dir, 'public'), { 'predictions_app.json': { CAM000201: { species: [['A b', 0.9]] } }, 'sex_predictions_app.json': {}, 'wing_boxes_app.json': {}, 'wing_outlines.json': { version: 1, photos: {} } });
  const raw = await new Promise((resolve, reject) =>
    http.get(`${base}/predictions_app.json`, { headers: { 'accept-encoding': 'gzip' } }, res => {
      const chunks = [];
      res.on('data', c => chunks.push(c));
      res.on('end', () => resolve({ res, body: Buffer.concat(chunks) }));
    }).on('error', reject),
  );
  assert.equal(raw.res.statusCode, 200);
  assert.equal(raw.res.headers['content-encoding'], 'gzip');
  assert.equal(raw.res.headers['cache-control'], 'public, max-age=300');
  assert.ok(raw.res.headers['last-modified']);
  assert.deepEqual(JSON.parse(gunzipSync(raw.body)), { CAM000201: { species: [['A b', 0.9]] } });
  const etag = raw.res.headers.etag;
  assert.equal((await fetch(`${base}/predictions_app.json`, { headers: { 'if-none-match': etag } })).status, 304);
  const plain = await fetch(`${base}/index.json`);
  assert.equal(plain.status, 200);
  assert.deepEqual(Object.keys((await plain.json()).files), FILES);
  assert.equal((await fetch(`${base}/other.json`)).status, 404);
  assert.equal((await fetch(`${base}/predictions_app.json`, { method: 'HEAD' })).status, 200);
  // How the readings go, in /health (off here: no reader, no identifier).
  const health = await (await fetch(`http://127.0.0.1:${port}/ithomiini/health`)).json();
  assert.equal(health.readings.state, 'off');
});

test('the readings worker: photos from a folder, the species and sex from the Python worker, the wings said missing without the reader', async t => {
  const dir = mkdtempSync(join(tmpdir(), 'readings-worker-'));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  writeFileSync(join(dir, `${id('a')}.jpg`), 'Mechanitis polymnia 0.9');
  writeFileSync(join(dir, `${id('b')}.jpg`), 'Mechanitis lysimnia 0.6');
  const job = { id: '7', cam: 'CAM000201', side: 'East', ecuador: true, species: 'Mechanitis polymnia', photos: [{ fileId: id('a'), name: 'CAM000201d', wings: true }, { fileId: id('b'), name: 'CAM000201v', wings: false }, { fileId: id('gone'), name: 'CAM000201d2', wings: true }], sex: { d: id('a'), v: id('b') } };
  const out = spawnSync(process.execPath, [WORKER, '--source', dir, '--python', process.execPath, '--python-args', JSON.stringify([FAKE_PY])], { input: `${JSON.stringify(job)}\n`, encoding: 'utf8', timeout: 20_000 });
  const [ready, answer] = out.stdout.trim().split('\n').map(l => JSON.parse(l));
  assert.equal(ready.ready, true);
  assert.equal(answer.id, '7');
  assert.match(answer.wings[id('a')].error, /no wing chain/);
  assert.equal(answer.wings[id('b')], undefined, 'not asked');
  assert.match(answer.wings[id('gone')].error, /not downloaded/);
  assert.match(answer.downloads[id('gone')], /ENOENT/);
  assert.deepEqual(answer.ident.photos[id('a')].species, [['Mechanitis polymnia', 0.9]]);
  assert.deepEqual(answer.ident.combined.used, [id('a'), id('b')]);
  assert.deepEqual(answer.ident.sex, { pMale: 0.2, status: 'below_confidence', supported: false, fold: 1, side: 'East' });
});
