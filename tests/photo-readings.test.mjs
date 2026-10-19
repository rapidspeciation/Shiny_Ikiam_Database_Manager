// The nano wing model's and the identifier/sex model's readings of every Photo_links photo (18 Oct 2026 bundles):
// imported into photo_wing_sides / photo_wing_boxes / photo_predictions / photo_file_predictions with their new
// columns, again the same, and read back by the app's indexes. An older database gets the new columns.
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { DatabaseSync } from 'node:sqlite';
import { execFileSync } from 'node:child_process';
import { mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { gzipSync } from 'node:zlib';
import { importBundle } from '../server/envelope-import.mjs';
import { initPhotoTables, reviewData, wingProblem, wingSides } from '../server/photodata.mjs';

const ROOT = fileURLToPath(new URL('..', import.meta.url));
const MODEL = 'wingseg_v5n_640 f3c4f755 + wing_side_cls 838b9371 + wingside.json 48a70be6';

const wing = (name, fileId, extra = {}) => ({
  name,
  fileId,
  cam: name.slice(0, 9),
  suffix: name.slice(9),
  folder: 'Photos wings of collected butterflies/JPG_photos',
  source: 'v5n_20261018',
  model: MODEL,
  side: 'dorsal',
  pDorsal: 0.9981,
  pForewings: 0.9993,
  pHindwings: 0.9902,
  wings: 4,
  review: false,
  foreSide: 'dorsal',
  hindSide: 'dorsal',
  turnK: 0,
  pTurn: 0.9999,
  missing: [],
  wingList: [
    { box: [0.05, 0.1, 0.33, 0.4], kind: 'forewing', half: 'left', pDorsal: 0.999, pForewing: 0.998, score: 0.93 },
    { box: [0.36, 0.1, 0.66, 0.41], kind: 'forewing', half: 'right', pDorsal: 0.999, pForewing: 0.997, score: 0.92 },
    { box: [0.07, 0.42, 0.3, 0.65], kind: 'hindwing', half: 'left', pDorsal: 0.99, pForewing: 0.01, score: 0.9 },
    { box: [0.38, 0.43, 0.63, 0.66], kind: 'hindwing', half: 'right', pDorsal: 0.99, pForewing: 0.02, score: 0.88 },
  ],
  size: [1600, 1067],
  ...extra,
});

const wingsBundle = () => ({
  version: 2,
  source: 'v5n_20261018',
  wings: [
    wing('CAM000101d', 'FILE_d_0000000000000001'),
    // named v, but the wings show their upper side; upside down and one hindwing missing
    wing('CAM000101v', 'FILE_v_0000000000000001', {
      wings: 3,
      turnK: 2,
      pTurn: 0.97,
      turn: 'upside down',
      degrees: 180,
      missing: ['hindwing_right'],
      pair: 'both photos same side',
      reasons: 'side-disagrees-with-suffix(p=1.00); wings=3(absent:hindwing_right); orientation:upside down(p=0.97)',
      camReasons: 'both-photos-dorsal',
      review: true,
      // one outline per wing of wingList, same order (server/wing-outlines.mjs); the third wing has no mask
      outlines: [
        ['fw', 'l', [50, 100, 330, 100, 330, 400]],
        ['fw', 'r', [360, 100, 660, 100, 660, 410]],
        null,
        ['x', 'u', [380, 430, 630, 430, 630, 660]],
      ],
    }),
    { name: 'CAM000102d', fileId: 'FILE_d_0000000000000002', cam: 'CAM000102', suffix: 'd', source: 'v5n_20261018', model: MODEL, error: 'Error: SOI not found', review: false },
  ],
  wingBoxes: [
    ['CAM000101d', [0.0416, 0.0832, 0.6684, 0.6768], 0.88],
    ['CAM000101v', [0.0416, 0.0832, 0.6684, 0.6768], 0.88],
  ],
});

const predictionsBundle = () => ({
  version: 1,
  source: 'photo_readings_20261018',
  predictions: [
    {
      cam: 'CAM000101',
      source: 'photo_readings_20261018',
      species: [['Mechanitis messenoides', 0.9312], ['Mechanitis mazaeus', 0.0412]],
      genus: [['Mechanitis', 0.9871]],
      subspecies: [['Mechanitis messenoides deceptus', 0.9001]],
      andes: 'East',
      model: 'bioclip-2.5-h + head_hier 749fbffd + wing_seg_v5n d74f9c13 (feature mean of 2 photos)',
      photos: [
        { fileId: 'FILE_d_0000000000000001', name: 'CAM000101d', view: 'd' },
        { fileId: 'FILE_v_0000000000000001', name: 'CAM000101v', view: 'v' },
      ],
      sex: 'male',
      sexConf: 0.9712,
      sexSupported: true,
      sexSpecies: 'Mechanitis messenoides',
      sexStatus: 'species_supported',
      sexPMale: 0.9712,
      sexModel: 'collection sex release_20261014',
    },
    { cam: 'M01234', source: 'photo_readings_20261018', species: [['Melinaea menophilus', 0.51]], genus: [['Melinaea', 0.8]], subspecies: [], sexStatus: 'not_scored:no_v_photo', sexModel: 'collection sex release_20261014' },
  ],
  photoPredictions: [
    { fileId: 'FILE_d_0000000000000001', name: 'CAM000101d', cam: 'CAM000101', view: 'd', species: [['Mechanitis messenoides', 0.95]], genus: [['Mechanitis', 0.99]], subspecies: [], andes: 'East', cropStatus: 'selected_object', cropBox: [0.04, 0.08, 0.67, 0.68], model: 'single photo' },
    { fileId: 'FILE_v_0000000000000001', name: 'CAM000101v', cam: 'CAM000101', view: 'v', species: [['Mechanitis messenoides', 0.88]], genus: [['Mechanitis', 0.97]], subspecies: [], andes: 'East', cropStatus: 'centre_crop_fallback', cropBox: [0.05, 0.05, 0.95, 0.95], model: 'single photo' },
  ],
});

test('the nano wing readings go in with every wing, the missing positions and the raw turn, and come out the same', () => {
  const db = new DatabaseSync(':memory:');
  const bundle = wingsBundle();
  assert.deepEqual(importBundle(db, { wings: bundle.wings, wingBoxes: bundle.wingBoxes }), { wings: 3, wingBoxes: 2 });
  const row = db.prepare("SELECT * FROM photo_wing_sides WHERE name='CAM000101v'").get();
  assert.equal(row.model, MODEL);
  assert.equal(row.turn_k, 2);
  assert.equal(row.p_turn, 0.97);
  assert.equal(row.degrees, 180);
  assert.deepEqual(JSON.parse(row.missing_json), ['hindwing_right']);
  assert.equal(JSON.parse(row.wings_json).length, 4);
  assert.deepEqual(JSON.parse(row.size_json), [1600, 1067]);
  assert.deepEqual(JSON.parse(row.outlines_json)[0], ['fw', 'l', [50, 100, 330, 100, 330, 400]]);
  assert.equal(JSON.parse(row.outlines_json)[2], null);
  assert.equal(db.prepare("SELECT outlines_json FROM photo_wing_sides WHERE name='CAM000101d'").get().outlines_json, null);
  assert.equal(db.prepare("SELECT error FROM photo_wing_sides WHERE name='CAM000102d'").get().error, 'Error: SOI not found');
  assert.equal(JSON.parse(db.prepare("SELECT box_json FROM photo_wing_boxes WHERE name='CAM000101d'").get().box_json)[2], 0.6684);

  const index = wingSides({ db });
  const v = index.byFile.get('FILE_v_0000000000000001');
  assert.equal(v.turnK, 2);
  assert.equal(v.foreSide, 'dorsal');
  assert.deepEqual(v.missing, ['hindwing_right']);
  assert.equal(JSON.parse(v.wingsJson)[0].kind, 'forewing');
  assert.equal(JSON.parse(v.outlinesJson).length, 4);
  assert.equal(index.byFile.get('FILE_d_0000000000000001').outlinesJson, undefined);
  assert.deepEqual(wingProblem(v), { side: 'dorsal', pDorsal: 0.9981, turn: 'upside down', degrees: 180, pair: 'both photos same side' });
  assert.equal(index.byFile.get('FILE_d_0000000000000002').error, 'Error: SOI not found');

  // the same bundle again: the same rows
  importBundle(db, { wings: bundle.wings, wingBoxes: bundle.wingBoxes });
  assert.equal(db.prepare('SELECT count(*) n FROM photo_wing_sides').get().n, 3);
  assert.equal(db.prepare('SELECT count(*) n FROM photo_wing_boxes').get().n, 2);
});

test('per-CAM and per-photo predictions keep the model, the photos combined and the sex screen', () => {
  const db = new DatabaseSync(':memory:');
  const b = predictionsBundle();
  assert.deepEqual(importBundle(db, { predictions: b.predictions, photoPredictions: b.photoPredictions, gallery: b.source, source: b.source }), {
    predictions: 2,
    photoPredictions: 2,
  });
  const p = db.prepare("SELECT * FROM photo_predictions WHERE cam='CAM000101'").get();
  assert.equal(p.source, 'photo_readings_20261018');
  assert.equal(p.sex_status, 'species_supported');
  assert.equal(p.sex_p_male, 0.9712);
  assert.equal(p.sex_supported, 1);
  assert.equal(JSON.parse(p.photos_json).length, 2);
  const m = db.prepare("SELECT * FROM photo_predictions WHERE cam='M01234'").get();
  assert.equal(m.sex, null);
  assert.equal(m.sex_status, 'not_scored:no_v_photo');
  const f = db.prepare("SELECT * FROM photo_file_predictions WHERE file_id='FILE_v_0000000000000001'").get();
  assert.equal(f.cam, 'CAM000101');
  assert.equal(f.crop_status, 'centre_crop_fallback');
  assert.equal(f.source, 'photo_readings_20261018');
  assert.equal(JSON.parse(f.species_json)[0][0], 'Mechanitis messenoides');

  const data = reviewData({ db });
  const cam = data.predictions.get('CAM000101');
  assert.equal(cam.species[0][0], 'Mechanitis messenoides');
  assert.equal(cam.sex.status, 'species_supported');
  assert.equal(cam.sex.pMale, 0.9712);
  assert.equal(cam.photos.length, 2);
  assert.match(cam.model, /head_hier 749fbffd/);
  assert.equal(data.predictions.get('M01234').sex, null);
  assert.equal(data.predictions.get('M01234').sexStatus, 'not_scored:no_v_photo');

  importBundle(db, { predictions: b.predictions, photoPredictions: b.photoPredictions.slice(0, 1) });
  assert.equal(db.prepare('SELECT count(*) n FROM photo_file_predictions').get().n, 1, 'an import replaces the earlier one');
});

test('a database made before these readings gets their columns and keeps its rows', () => {
  const db = new DatabaseSync(':memory:');
  db.exec(`CREATE TABLE photo_predictions(
      cam TEXT PRIMARY KEY, species_json TEXT, genus_json TEXT, subspecies_json TEXT, sex TEXT, sex_conf REAL,
      sex_supported INTEGER, sex_species TEXT, source TEXT, imported_at TEXT NOT NULL);
    CREATE TABLE photo_wing_sides(
      id INTEGER PRIMARY KEY, name TEXT NOT NULL, file_id TEXT, cam TEXT, suffix TEXT, side TEXT, p_dorsal REAL,
      p_forewings REAL, p_hindwings REAL, wings INTEGER, turn TEXT, degrees INTEGER, pair TEXT, reasons TEXT,
      cam_reasons TEXT, review INTEGER, folder TEXT, source TEXT, imported_at TEXT NOT NULL);
    INSERT INTO photo_predictions(cam, species_json, source, imported_at) VALUES('CAM000001', '[["Oleria onega",0.9]]', 'gallery', 'then');
    INSERT INTO photo_wing_sides(name, cam, side, wings, source, imported_at) VALUES('CAM000001d', 'CAM000001', 'dorsal', 4, 'archive', 'then');`);
  initPhotoTables(db);
  const cols = t => db.prepare(`PRAGMA table_info(${t})`).all().map(c => c.name);
  for (const c of ['andes', 'model', 'photos_json', 'sex_status', 'sex_p_male', 'sex_model']) assert.ok(cols('photo_predictions').includes(c), c);
  for (const c of ['model', 'fore_side', 'hind_side', 'turn_k', 'p_turn', 'missing_json', 'wings_json', 'size_json', 'error', 'outlines_json'])
    assert.ok(cols('photo_wing_sides').includes(c), c);
  assert.ok(cols('photo_file_predictions').includes('crop_box_json'));
  const index = wingSides({ db });
  assert.deepEqual(Object.keys(index.byName.get('CAM000001D')[0]).filter(k => ['turnK', 'wingsJson', 'model'].includes(k)), [], 'an archive row has no nano fields');
  assert.equal(reviewData({ db }).predictions.get('CAM000001').species[0][0], 'Oleria onega');
});

test('the import scripts take the bundles as files', () => {
  const dir = mkdtempSync(join(tmpdir(), 'photo-readings-'));
  try {
    const dbPath = join(dir, 'app.sqlite');
    writeFileSync(join(dir, 'wings.json.gz'), gzipSync(JSON.stringify(wingsBundle())));
    writeFileSync(join(dir, 'predictions.json.gz'), gzipSync(JSON.stringify(predictionsBundle())));
    const run = (script, file) =>
      execFileSync(process.execPath, [join(ROOT, 'scripts', script), '--db', dbPath, join(dir, file)], { encoding: 'utf8' });
    assert.match(run('import-wing-sides.mjs', 'wings.json.gz'), /3 photos \(3 v5n_20261018\), 2 wing boxes/);
    assert.match(run('import-photo-predictions.mjs', 'predictions.json.gz'), /2 specimens, 2 photos \(photo_readings_20261018\)/);
    const db = new DatabaseSync(dbPath);
    assert.equal(db.prepare('SELECT count(*) n FROM photo_file_predictions').get().n, 2);
    assert.equal(db.prepare('SELECT count(*) n FROM photo_wing_boxes').get().n, 2);
    db.close();
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});
