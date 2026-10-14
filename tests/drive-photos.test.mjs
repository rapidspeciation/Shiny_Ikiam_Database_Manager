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

test('arrange: views by ID, one turn for the batch in the camera frame, the marked rows', () => {
  const t = s => `2026-10-14T10:${s}Z`;
  const row = (file, camid, s, readRotation, extra = {}) => ({ file, camid, taken: t(s), readRotation, setBy: 'reader', conf: 1, wide: true, orientation: 1, ...extra });
  const rows = [
    row('a', 'CAM000001', '00:00', 180),
    row('b', 'CAM000001', '00:30', 180),
    row('c', 'CAM000002', '01:00', 0, { conf: 0.5 }),
    row('d', 'CAM000002', '01:30', 180),
    row('e', null, '02:00', null),
    // Its tag flipped (it shows standing): the same turn of the camera frame is a quarter less from how it shows.
    row('f', 'CAM000003', '02:30', 90, { wide: false, orientation: 6 }),
  ];
  const out = arrange(rows, { known: new Set(['CAM000001', 'CAM000002', 'CAM000003']), listed: new Set() });
  assert.deepEqual(rows.map(r => [r.file, r.view, r.rotation]), [
    ['a', 'd', 180],
    ['b', 'v', 180],
    ['c', 'd', 180],
    ['d', 'v', 180],
    ['e', '', 180],
    ['f', 'd', 90],
  ]);
  // c: read unsure; e: no ID; f: one photo of its ID.
  assert.deepEqual(rows.filter(r => r.marked).map(r => r.file), ['c', 'e', 'f']);
  assert.deepEqual(rows.find(r => r.file === 'c').review, ['unsure reading']);
  assert.match(out.order, /CAM000001×2 CAM000002×2! \?×1! CAM000003×1/);
});

test('arrange: envelopes laid sideways decide no turn; the photos stay landscape and two of them are to look at', () => {
  const rows = ['a', 'b', 'c', 'd'].map((file, i) => ({ file, camid: `CAM00000${Math.floor(i / 2) + 1}`, taken: `2026-10-14T10:0${i}:00Z`, readRotation: 270, setBy: 'reader', conf: 1, wide: true, orientation: 1 }));
  const out = arrange(rows, { known: new Set(['CAM000001', 'CAM000002']), listed: new Set() });
  assert.deepEqual(rows.map(r => r.rotation), [0, 0, 0, 0]);
  assert.equal(out.checks.turnUnsure.count, 1);
  assert.equal(out.checks.turnUnsure.examples[0].photos, 4);
  assert.deepEqual(rows.filter(r => r.marked).map(r => r.file), ['a', 'c']);
});

async function fixture(t, { originalsGb } = {}) {
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
    'IMG_5001.JPG': { camid: 'CAM000001', conf: 1, rotation: 180 },
    'IMG_5002.JPG': { camid: 'CAM000001', conf: 1, rotation: 180 },
    'IMG_5003.JPG': { camid: 'CAM000002', conf: 0.98, rotation: 180 },
    'IMG_5004.JPG': { camid: 'CAM000092', conf: 0.6, rotation: 180, inWorkbook: false },
    'IMG_5005.JPG': { camid: 'CAM000003', conf: 1, rotation: 180 },
    'IMG_5006.JPG': { camid: 'CAM000003', conf: 1, rotation: 180 },
    'IMG_5007.JPG': { camid: null, conf: 0, rotation: null },
    'IMG_5008.JPG': { camid: 'CAM000003', conf: 1, rotation: 180 },
  };
  const byFile = new Map(files.map(f => [f.id, f.name]));
  const reader = {
    read: async job => {
      if (job.op === 'cut') {
        writeFileSync(job.out, `cut of ${job.path.split('/').at(-1)} ${JSON.stringify(job.box)}`);
        return { id: job.id, out: job.out, width: 800, height: 600 };
      }
      writeFileSync(job.preview, 'p');
      writeFileSync(job.crop, 'c');
      return { id: job.id, orientation: 1, width: 6000, height: 4000, inWorkbook: true, candidates: [], ...said[byFile.get(job.id)] };
    },
  };
  const planned = [];
  const cards = [];
  const photos = createDrivePhotos({
    store,
    config: { drive, ...(originalsGb !== undefined ? { drivePhotosOriginalsGb: originalsGb } : {}) },
    cacheDir: join(dir, 'cache'),
    // Whatever room this computer's temporary folder has.
    freeBytes: 0,
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
  // The pictures are kept, and the whole photos, for a closer look.
  assert.deepEqual(readdirSync(tmpdir()).filter(f => f.startsWith(`ithomiini-drive-photos-${started.batch}`)), []);
  assert.equal(readdirSync(join(dir, 'cache', started.batch)).length, 16);
  assert.equal(readdirSync(join(dir, 'cache', 'originals')).length, 8);

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
  // A part of a whole photo, cut from the one kept; a photo no longer kept is downloaded again first.
  rmSync(join(dir, 'cache', 'originals', `${id(15)}.jpg`));
  const closer = await photos.run('drive_photos_look', { batch: started.batch, files: [id(14), id(15)], box: [0, 0, 0.5, 0.4] }, kim);
  assert.equal(closer[IMAGES].length, 2);
  assert.match(Buffer.from(closer[IMAGES][1].data, 'base64').toString(), /^cut of drive-id-0+15\.jpg \[0,0,0.5,0.4\]$/);
  assert.ok(existsSync(join(dir, 'cache', 'originals', `${id(15)}.jpg`)));

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
  // One turn for all the photos.
  assert.equal((await photos.run('drive_photos_edit', { batch: started.batch, turnAll: 0 }, kim)).changed, 8);
  assert.equal((await photos.run('drive_photos_edit', { batch: started.batch, turnAll: 180 }, kim)).changed, 8);
  assert.equal(edited.order, 'CAM000001×2 CAM000002×2 CAM000003×2');
  assert.equal(edited.checks, undefined);

  // «Aplicar»: renames (each RAW file follows its photo there), the batch's half turn, the two removed.
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

test("show_photos: each ID's listed photos side by side, a view missing said so, notes kept", async t => {
  const { galleryOf } = await import('../server/photo-gallery.mjs');
  const file = (n, path) => ({ 'Full Path': path, Name: path.split('/').at(-1), Type: 'File', URL: `https://drive.google.com/file/d/${id(n)}/view` });
  const sheets = new LocalSheets({
    Collection_data: ['CAM000001', 'CAM000002'].map((cam, i) => ({ row: i + 2, values: { CAM_ID: cam, FieldMark_ID: `F${i}` } })),
    Photo_links: [file(1, 'Peru/CAM000001v.JPG'), file(2, 'Peru/CAM000001d.JPG'), file(3, 'Peru/CAM000002d.JPG'), file(4, 'Peru/CAM000002d.CR2')].map((values, i) => ({ row: i + 2, values })),
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Collection_data', 'Photo_links'] });
  t.after(() => store.close());
  const items = galleryOf(store, ['cam000001', 'CAM000002', 'CAM000009'], { cam000001: 'both views ventral?' });
  assert.deepEqual(items.map(i => [i.id, i.photos.map(p => p.view), i.missing, i.note ?? null, i.inWorkbook]), [
    ['CAM000001', ['d', 'v'], [], 'both views ventral?', true],
    ['CAM000002', ['d'], ['v'], null, true],
    ['CAM000009', [], ['d', 'v'], null, false],
  ]);
  assert.equal(items[0].photos[0].file, id(2));
});

test('asked about a batch being read, the answer waits for it to end', async t => {
  const { photos } = await fixture(t);
  const started = await photos.run('drive_photos', { folder: 'Upload' }, kim, {});
  // At once: no polling needed.
  const seen = await photos.run('drive_photos', { batch: started.batch }, kim);
  assert.equal(seen.status, 'ready');
  assert.ok(seen.order);
});

test('no whole photos kept when the cache allows none', async t => {
  const { dir, photos } = await fixture(t, { originalsGb: 0 });
  const started = await photos.run('drive_photos', { folder: 'Upload' }, kim, {});
  await photos.run('drive_photos', { batch: started.batch }, kim);
  assert.equal(readdirSync(join(dir, 'cache', 'originals')).length, 0);
  assert.equal(readdirSync(join(dir, 'cache', started.batch)).length, 16);
});
