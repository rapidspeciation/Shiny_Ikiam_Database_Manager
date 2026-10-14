import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, rmSync, writeFileSync, existsSync, readdirSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { FOLDER, LocalDrive } from '../server/drive.mjs';
import { arrange, createDrivePhotos, viewsFor } from '../server/drive-photos.mjs';
import { IMAGES } from '../server/rename-photos.mjs';

// Photos uploaded to Drive without being renamed, read on the server (server/drive-photos.mjs): the
// batch in the order taken, two photos per ID, one turn per session, what to look at; corrections;
// the changes it becomes. The reader is a stand-in here (the real one: photo-reader/, its own tests).

const id = n => `drive-id-${String(n).padStart(20, '0')}`;
const kim = { id: 'kim', username: 'kim', role: 'editor' };

test('views in the order taken: d, v, then d2, v2', () => {
  assert.deepEqual(viewsFor(1), ['d']);
  assert.deepEqual(viewsFor(4), ['d', 'v', 'd2', 'v2']);
});

test('arrange: views by ID, one turn per session, the marked rows', () => {
  const t = s => `2026-10-14T10:${s}Z`;
  const rows = [
    { file: 'a', camid: 'CAM000001', taken: t('00:00'), readRotation: 90, setBy: 'reader', conf: 1 },
    { file: 'b', camid: 'CAM000001', taken: t('00:30'), readRotation: 90, setBy: 'reader', conf: 1 },
    { file: 'c', camid: 'CAM000002', taken: t('01:00'), readRotation: 0, setBy: 'reader', conf: 0.5 },
    { file: 'd', camid: 'CAM000002', taken: t('01:30'), readRotation: 90, setBy: 'reader', conf: 1 },
    { file: 'e', camid: null, taken: t('02:00'), readRotation: null, setBy: 'reader' },
  ];
  const out = arrange(rows, { known: new Set(['CAM000001', 'CAM000002']), listed: new Set() });
  assert.deepEqual(rows.map(r => [r.file, r.view, r.rotation]), [
    ['a', 'd', 90],
    ['b', 'v', 90],
    ['c', 'd', 90],
    ['d', 'v', 90],
    ['e', '', 90],
  ]);
  // c: read unsure and turned otherwise than its session; e: no ID.
  assert.deepEqual(rows.filter(r => r.marked).map(r => r.file), ['c', 'e']);
  assert.deepEqual(rows.find(r => r.file === 'c').review, ['unsure reading', 'rotation-outlier']);
  assert.match(out.order, /CAM000001×2 CAM000002×2! \?×1/);
});

async function fixture(t) {
  const dir = mkdtempSync(join(tmpdir(), 'drive-photos-'));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  // A folder with eight camera-named photos and their RAW files, beside a listed folder.
  const files = [
    { id: id(1), name: 'Photos', mimeType: FOLDER, parents: [] },
    { id: id(2), name: 'Upload', mimeType: FOLDER, parents: [id(1)], webViewLink: 'https://drive.google.com/drive/folders/x' },
  ];
  for (let i = 1; i <= 8; i++) {
    const path = join(dir, `IMG_${5000 + i}.JPG`);
    writeFileSync(path, 'jpeg');
    files.push({
      id: id(10 + i),
      name: `IMG_${5000 + i}.JPG`,
      mimeType: 'image/jpeg',
      parents: [id(2)],
      size: '5000000',
      localPath: path,
      imageMediaMetadata: { time: `2026:10:14 10:0${i}:00` },
      webViewLink: `https://drive.google.com/file/d/${id(10 + i)}/view`,
    });
    files.push({ id: id(30 + i), name: `IMG_${5000 + i}.CR2`, mimeType: 'image/x-canon-cr2', parents: [id(2)], size: '25000000' });
  }
  const drive = new LocalDrive(files, { me: 'project@example.org' });
  const sheets = new LocalSheets({
    Collection_data: ['CAM000001', 'CAM000002', 'CAM000003'].map((cam, i) => ({ row: i + 2, values: { CAM_ID: cam, FieldMark_ID: `F${i}` } })),
    Photo_links: [{ row: 2, values: { 'Full Path': 'Upload', Name: 'Upload', Type: 'Folder', URL: 'https://drive.google.com/drive/folders/' + id(2) } }],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Collection_data', 'Photo_links'] });
  t.after(() => store.close());
  // What the reader says of each photo (by its name): two photos per ID, one ID read wrong, one not read, turned a quarter.
  const said = {
    'IMG_5001.JPG': { camid: 'CAM000001', conf: 1, rotation: 90 },
    'IMG_5002.JPG': { camid: 'CAM000001', conf: 1, rotation: 90 },
    'IMG_5003.JPG': { camid: 'CAM000002', conf: 0.98, rotation: 90 },
    'IMG_5004.JPG': { camid: 'CAM000092', conf: 0.6, rotation: 90, inWorkbook: false },
    'IMG_5005.JPG': { camid: 'CAM000003', conf: 1, rotation: 90 },
    'IMG_5006.JPG': { camid: 'CAM000003', conf: 1, rotation: 90 },
    'IMG_5007.JPG': { camid: null, conf: 0, rotation: null },
    'IMG_5008.JPG': { camid: 'CAM000003', conf: 1, rotation: 90 },
  };
  const byFile = new Map(files.map(f => [f.id, f.name]));
  const reader = {
    read: async job => {
      writeFileSync(job.preview, 'p');
      writeFileSync(job.crop, 'c');
      return { id: job.id, orientation: 1, inWorkbook: true, candidates: [], ...said[byFile.get(job.id)] };
    },
  };
  const planned = [];
  const cards = [];
  const photos = createDrivePhotos({
    store,
    config: { drive },
    cacheDir: join(dir, 'cache'),
    reader,
    organize: async (operations, user) => (planned.push({ operations, user }), { plan: 'plan-1', link: 'https://app/#/propuestas/plan-1' }),
    show: batch => (cards.push(['show', batch.status]), { link: 'https://app/#/propuestas/' + batch.id }),
    tell: batch => cards.push(['tell', batch.status]),
  });
  return { dir, drive, photos, planned, cards };
}

test('a folder read on the server: the batch in order, what to look at, corrections, and its changes on Drive', async t => {
  const { dir, photos, planned, cards } = await fixture(t);
  assert.match((await photos.run('drive_photos', { folder: 'Upload' }, { role: 'viewer' })).error, /role/);
  const started = await photos.run('drive_photos', { folder: 'Upload' }, kim, {});
  assert.equal(started.photos, 8);
  assert.equal(started.withRawFile, 8);
  assert.match(started.link, /propuestas/);
  await photos.idle();
  // The photos are deleted once read; their pictures are kept.
  assert.deepEqual(readdirSync(tmpdir()).filter(f => f.startsWith(`ithomiini-drive-photos-${started.batch}`)), []);
  assert.equal(readdirSync(join(dir, 'cache', started.batch)).length, 16);

  const seen = await photos.run('drive_photos', { batch: started.batch }, kim);
  assert.equal(seen.status, 'ready');
  assert.equal(seen.order, "CAM000001×2 CAM000002×1 CAM000092×1! CAM000003×2 ?×1! CAM000003×1");
  assert.equal(seen.checks.notTwoPhotos.count, 3);
  assert.equal(seen.checks.notInWorkbook.count, 1);
  // Only what does not add up is shown: not CAM000001's two photos.
  assert.deepEqual(seen.toLook.map(r => r.name), ['IMG_5003.JPG', 'IMG_5004.JPG', 'IMG_5005.JPG', 'IMG_5006.JPG', 'IMG_5007.JPG', 'IMG_5008.JPG']);
  assert.deepEqual(seen.toLook[1].look, ['unsure reading']);

  // The pictures, as the assistant sees them.
  const looked = await photos.run('drive_photos_look', { batch: started.batch, files: [id(14), 'nothing'] }, kim);
  assert.equal(looked[IMAGES].length, 1);
  assert.equal(looked.pictures[1].missing, 'no such row');

  // Corrected: the misread ID, the photo without one, a third photo not wanted.
  const edited = await photos.run(
    'drive_photos_edit',
    {
      batch: started.batch,
      changes: [
        { file: id(14), id: 'CAM000002' },
        { file: id(17), remove: true, note: 'no envelope in the photo' },
        { file: id(18), view: 'v', id: 'CAM000003', remove: false },
        { file: id(16), remove: true },
      ],
    },
    kim,
  );
  assert.equal(edited.changed, 4);
  assert.equal(edited.order, 'CAM000001×2 CAM000002×2 CAM000003×2');
  assert.equal(edited.checks, undefined);

  // «Aplicar»: renames (each RAW file follows its photo there), the quarter turn of the session, the two removed.
  const applied = await photos.apply(started.batch, kim);
  assert.equal(applied.plan, 'plan-1');
  const ops = planned[0].operations.map(o => [o.action, o.file.match(/d\/(.+?)\//)[1], o.name ?? o.degrees ?? '']);
  const kinds = ops.reduce((n, [a]) => ({ ...n, [a]: (n[a] ?? 0) + 1 }), {});
  assert.deepEqual(kinds, { rename: 6, rotate: 6, remove: 2 });
  assert.deepEqual(
    ops.filter(([a]) => a === 'rename').map(([, f, name]) => name),
    ['CAM000001d.JPG', 'CAM000001v.JPG', 'CAM000002d.JPG', 'CAM000002v.JPG', 'CAM000003d.JPG', 'CAM000003v.JPG'],
  );
  assert.equal(photos.batch(started.batch).status, 'applied');
  assert.match((await photos.apply(started.batch, kim)).error, /applied/);
  assert.deepEqual(cards.at(0), ['show', 'reading']);
  assert.deepEqual(cards.at(-1), ['tell', 'applied']);
});

test('a folder without camera-named photos, or none such', async t => {
  const { photos } = await fixture(t);
  assert.match((await photos.run('drive_photos', { folder: 'Nowhere' }, kim)).error, /No folder/);
  const all = await photos.run('drive_photos', { folder: 'Upload', all: true }, kim, {});
  assert.equal(all.photos, 8);
  await photos.idle();
  assert.ok(photos.discard(all.batch));
  assert.equal(photos.picture(all.batch, id(11), 'photo'), null);
  assert.equal(existsSync(join(tmpdir(), `ithomiini-drive-photos-${all.batch}`)), false);
});
