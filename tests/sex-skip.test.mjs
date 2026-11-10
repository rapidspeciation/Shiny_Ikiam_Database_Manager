// The collection sex model's locked test portion (COLLECTION_SEX_SKIP_FILE, server/sex-skip.mjs) everywhere: a
// locked CAM's sex is not published (the Wings Gallery's files), not scored (the readings worker: the Python worker
// never gets it), not stored (the readings' store step) and not shown (Dudas, the photo review's data, the
// assistant's specimen answers); without a valid list (missing, empty, a CSV, commas, quotes) none is, while the
// species and the other files stay. Made-up CAMs only (the real list never goes in git).
import test from 'node:test';
import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { DatabaseSync } from 'node:sqlite';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { sexClosedStatus, sexOpen, sexSkipList } from '../server/sex-skip.mjs';
import { galleryData } from '../server/gallery-public.mjs';
import { upsertReadings } from '../server/envelope-import.mjs';
import { initPhotoTables, reviewData } from '../server/photodata.mjs';
import { modelOf, photoReadings } from '../server/maps-tools.mjs';
import { projectInfo } from '../server/monitoring.mjs';

const LOCKED = 'CAM999902';
const OPEN = 'CAM999901';
const WORKER = new URL('../server/readings-worker.mjs', import.meta.url).pathname;
const FAKE_PY = new URL('./fixtures/fake-specimen.mjs', import.meta.url).pathname;

const dir = mkdtempSync(join(tmpdir(), 'sex-skip-test-'));
test.after(() => rmSync(dir, { recursive: true, force: true }));
/** A list file (a new name each time: the list is read once per file name). */
let n = 0;
const listOf = text => {
  const file = join(dir, `list-${++n}.txt`);
  writeFileSync(file, text);
  return file;
};
const VALID = listOf(`${LOCKED}\n`);
/** Every list that must close the collection sex: [why, file name or undefined]. */
const CLOSED = () => [
  ['unset', undefined],
  ['missing', join(dir, 'missing.txt')],
  ['empty', listOf('\n')],
  ['split CSV', listOf(`cam_id,split\n${LOCKED},locked\n`)],
  ['commas', listOf(`${LOCKED},CAM999903\n`)],
  ['quotes', listOf(`"${LOCKED}"\n`)],
  ['a short CAM', listOf(`${LOCKED}\nCAM99990\n`)],
];
const withList = (file, fn) => {
  const before = process.env.COLLECTION_SEX_SKIP_FILE;
  if (file === undefined) delete process.env.COLLECTION_SEX_SKIP_FILE;
  else process.env.COLLECTION_SEX_SKIP_FILE = file;
  try {
    return fn();
  } finally {
    if (before === undefined) delete process.env.COLLECTION_SEX_SKIP_FILE;
    else process.env.COLLECTION_SEX_SKIP_FILE = before;
  }
};

/** The app's database with both CAMs read by the photo readings (species and a scored sex each). */
async function readingsStore() {
  const store = new Store({ localMode: true }, { sheets: new LocalSheets({ Photo_links: [] }) });
  await store.sync({ sheets: ['Photo_links'] });
  const db = store.db;
  initPhotoTables(db);
  const insert = db.prepare(
    "INSERT INTO photo_predictions(cam, species_json, genus_json, subspecies_json, sex, sex_conf, sex_supported, sex_species, sex_status, sex_p_male, model, source, imported_at) VALUES(?, ?, '[]', '[]', ?, ?, 1, 'Oleria onega', 'species_supported', ?, 'm', 'app_readings', 'x')",
  );
  insert.run(OPEN, '[["Oleria onega", 0.9]]', 'male', 0.93, 0.93);
  insert.run(LOCKED, '[["Oleria onega", 0.8]]', 'female', 0.96, 0.04);
  return store;
}

test('the list: CAM IDs only, one per line or space, in any case; anything else closes the collection sex', () => {
  withList(listOf(`cam999902  CAM999903\n\n`), () => {
    assert.deepEqual([...sexSkipList()], [LOCKED, 'CAM999903']);
    assert.equal(sexOpen(OPEN), true);
    assert.equal(sexOpen(LOCKED.toLowerCase()), false);
    assert.equal(sexClosedStatus(OPEN), null);
    assert.equal(sexClosedStatus(LOCKED), 'not_scored:locked_test_split');
  });
  for (const [why, file] of CLOSED())
    withList(file, () => {
      assert.equal(sexSkipList(), null, why);
      assert.equal(sexOpen(OPEN), false, why);
      assert.equal(sexClosedStatus(OPEN), 'not_scored:no_valid_skip_list', why);
    });
});

test("the Wings Gallery's files: a locked CAM gets no sex entry; without a valid list no sex is published, the rest unchanged", async t => {
  const store = await readingsStore();
  t.after(() => store.close());
  const db = store.db;
  const open = withList(VALID, () => galleryData(db));
  assert.deepEqual(Object.keys(open['sex_predictions_app.json']), [OPEN]);
  assert.equal(open['sex_predictions_app.json'][OPEN].male_probability, 0.93);
  assert.deepEqual(Object.keys(open['predictions_app.json']), [OPEN, LOCKED], 'the species stay');
  for (const [why, file] of CLOSED()) {
    const closed = withList(file, () => galleryData(db));
    assert.deepEqual(closed['sex_predictions_app.json'], {}, why);
    assert.deepEqual({ ...closed, 'sex_predictions_app.json': null }, { ...open, 'sex_predictions_app.json': null }, why);
  }
});

test('the readings worker: a locked CAM, or any CAM without a valid list, is not scored; the Python worker never gets its sex', t => {
  const photos = mkdtempSync(join(tmpdir(), 'sex-skip-worker-'));
  t.after(() => rmSync(photos, { recursive: true, force: true }));
  const id = (cam, v) => `${cam}${v}_${'0'.repeat(20)}`;
  const jobs = [OPEN, LOCKED].flatMap(cam => {
    for (const v of ['d', 'v']) writeFileSync(join(photos, `${id(cam, v)}.jpg`), 'Oleria onega 0.9');
    const base = { cam, side: 'East', ecuador: true, species: 'Oleria onega', photos: ['d', 'v'].map(v => ({ fileId: id(cam, v), name: `${cam}${v}`, wings: false })), sex: { d: id(cam, 'd'), v: id(cam, 'v') } };
    // The butterfly's readings, and the «Fotos» tab's sex check (only: 'sex').
    return [
      { ...base, id: `${cam}-all` },
      { ...base, id: `${cam}-sex`, only: 'sex' },
    ];
  });
  const run = file => {
    const env = { ...process.env };
    if (file === undefined) delete env.COLLECTION_SEX_SKIP_FILE;
    else env.COLLECTION_SEX_SKIP_FILE = file;
    const out = spawnSync(process.execPath, [WORKER, '--source', photos, '--python', process.execPath, '--python-args', JSON.stringify([FAKE_PY])], {
      input: jobs.map(j => `${JSON.stringify(j)}\n`).join(''),
      encoding: 'utf8',
      timeout: 20_000,
      env,
    });
    const answers = out.stdout.trim().split('\n').map(l => JSON.parse(l)).filter(a => a.id);
    return Object.fromEntries(answers.map(a => [a.id, a.ident]));
  };
  const open = run(VALID);
  // The stand-in Python worker answers a sex only when it is sent one (tests/fixtures/fake-specimen.mjs).
  assert.equal(open[`${OPEN}-all`].sex.pMale, 0.2);
  assert.deepEqual(open[`${LOCKED}-all`].sex, { status: 'not_scored:locked_test_split' });
  assert.deepEqual(open[`${LOCKED}-all`].combined.species, [['Oleria onega', 0.9]], 'its species are read');
  assert.equal(open[`${OPEN}-sex`].sex.pMale, 0.2);
  assert.deepEqual(open[`${LOCKED}-sex`], { sex: { status: 'not_scored:locked_test_split' } });
  for (const file of [undefined, listOf(`cam_id,split\n${LOCKED},locked\n`)]) {
    const closed = run(file);
    for (const key of [`${OPEN}-all`, `${LOCKED}-all`]) {
      assert.deepEqual(closed[key].sex, { status: 'not_scored:no_valid_skip_list' }, `${file} ${key}`);
      assert.deepEqual(closed[key].combined.species, [['Oleria onega', 0.9]]);
    }
    assert.deepEqual(closed[`${OPEN}-sex`], { sex: { status: 'not_scored:no_valid_skip_list' } });
  }
});

test("the readings' store step: a locked CAM's sex, or every sex without a valid list, is never stored", () => {
  const prediction = cam => ({ cam, species: [['Oleria onega', 0.9]], genus: [], subspecies: [], sex: 'male', sexConf: 0.93, sexSupported: true, sexSpecies: 'Oleria onega', sexStatus: 'species_supported', sexPMale: 0.93, model: 'm', source: 'app_readings' });
  const stored = file => {
    const db = new DatabaseSync(':memory:');
    withList(file, () => upsertReadings(db, { predictions: [prediction(OPEN), prediction(LOCKED)] }, { bump: false }));
    return Object.fromEntries(db.prepare('SELECT cam, species_json, sex, sex_conf, sex_supported, sex_species, sex_status, sex_p_male FROM photo_predictions').all().map(r => [r.cam, { ...r }]));
  };
  const nothing = status => ({ sex: null, sex_conf: null, sex_supported: 0, sex_species: null, sex_status: status, sex_p_male: null });
  const open = stored(VALID);
  assert.deepEqual(open[OPEN], { cam: OPEN, species_json: '[["Oleria onega",0.9]]', sex: 'male', sex_conf: 0.93, sex_supported: 1, sex_species: 'Oleria onega', sex_status: 'species_supported', sex_p_male: 0.93 });
  assert.deepEqual(open[LOCKED], { cam: LOCKED, species_json: '[["Oleria onega",0.9]]', ...nothing('not_scored:locked_test_split') });
  for (const [why, file] of CLOSED()) {
    const closed = stored(file);
    for (const cam of [OPEN, LOCKED]) assert.deepEqual(closed[cam], { cam, species_json: '[["Oleria onega",0.9]]', ...nothing('not_scored:no_valid_skip_list') }, `${why} ${cam}`);
  }
});

test("the app's own views of the sex: the photo review's data, the assistant's specimen answers and Dudas", async () => {
  // Rows stored before the list (as in the live database): shown with no sex.
  const store = await readingsStore();
  const views = file =>
    withList(file, () => {
      // reviewData is kept per store object and database revision: a new object reads it afresh for each list.
      const review = reviewData({ db: store.db });
      const readings = photoReadings(store.db, [OPEN, LOCKED].map(cam => ({ cam, photos: {} })));
      const dudas = projectInfo(store, new Set([OPEN, LOCKED]));
      return Object.fromEntries(
        [OPEN, LOCKED].map(cam => [cam, { review: review.predictions.get(cam).sex?.sex ?? null, assistant: modelOf(readings.get(cam).prediction)?.sex ?? null, dudas: dudas[cam].sex?.pMale ?? null }]),
      );
    });
  assert.deepEqual(views(VALID), { [OPEN]: { review: 'male', assistant: 'male', dudas: 0.93 }, [LOCKED]: { review: null, assistant: null, dudas: null } });
  for (const [why, file] of CLOSED()) assert.deepEqual(views(file), { [OPEN]: { review: null, assistant: null, dudas: null }, [LOCKED]: { review: null, assistant: null, dudas: null } }, why);
  store.close();
});
