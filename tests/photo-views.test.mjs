import test from 'node:test';
import assert from 'node:assert/strict';
import { existsSync, mkdtempSync, readFileSync, rmSync, statSync, writeFileSync } from 'node:fs';
import { createRequire } from 'node:module';
import { tmpdir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { importBundle } from '../server/envelope-import.mjs';
import { bumpReviewRevision } from '../server/photodata.mjs';
import { VIEW_SIZES, photoBoxes, startViewer, viewPlan, wingBoxOf } from '../server/photo-views.mjs';
import { createDrivePhotos } from '../server/drive-photos.mjs';
import { createGallery } from '../server/photo-gallery.mjs';
import { IMAGES } from '../server/rename-photos.mjs';

// Views of a specimen's photo (server/photo-views.mjs, photo_files_look `view`): the boxes read from
// the archive turned into the parts of a picture, the pictures made by the views' process (with the
// photo reader's sharp, when this computer has it), and the whole photo when a box is not known.

const repo = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const kim = { id: 'kim', username: 'kim', role: 'editor' };
const id = n => `drive-id-${String(n).padStart(20, '0')}`;

/** The photo reader's folder (its sharp), here or in the main checkout of a worktree; null without. */
function readerDir() {
  const places = [process.env.PHOTO_READER_DIR, join(repo, 'photo-reader')];
  const git = join(repo, '.git');
  if (existsSync(git) && statSync(git).isFile()) {
    const m = /gitdir:\s*(.+)/.exec(readFileSync(git, 'utf8'));
    if (m) places.push(join(resolve(repo, m[1].trim()), '..', '..', '..', 'photo-reader'));
  }
  return places.find(p => p && existsSync(join(p, 'node_modules', 'sharp'))) ?? null;
}

async function photoStore() {
  const sheets = new LocalSheets({
    Photo_links: ['CAM000001d', 'CAM000001v', 'CAM000002d'].map((name, i) => ({
      row: i + 2,
      values: { 'Full Path': `Wild/${name}.JPG`, Name: `${name}.JPG`, Type: 'File', URL: `https://drive.google.com/file/d/${id(i + 1)}/view` },
    })),
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Photo_links'] });
  importBundle(store.db, {
    readings: [
      // Read on a 1600 × 1200 copy; the second envelope lay upside down.
      { name: 'CAM000001d', fileId: id(1), cam: 'CAM000001', view: 'dorsal', bbox: [16, 240, 512, 960], size: [1600, 1200], turned: 0, camid: null, conf: null, text: [], source: 'test' },
      { name: 'CAM000001v', fileId: id(2), cam: 'CAM000001', view: 'ventral', bbox: [0, 0, 400, 600], size: [1600, 1200], turned: 180, camid: null, conf: null, text: [], source: 'test' },
    ],
    // One box for the photo, and (the newer readings) a box per wing.
    wingBoxes: [
      ['CAM000001d', [0.4, 0.1, 0.9, 0.7], 0.98],
      ['CAM000002d', [
        [0.5, 0.2, 0.7, 0.4],
        [0.7, 0.2, 0.9, 0.45],
        [0.5, 0.45, 0.68, 0.6],
        [0.7, 0.45, 0.88, 0.62],
      ], 0.9],
    ],
  });
  return store;
}

test('a wing box is one box, or the box around each wing', () => {
  assert.deepEqual(wingBoxOf([0.1, 0.2, 0.3, 0.4]), [0.1, 0.2, 0.3, 0.4]);
  assert.deepEqual(wingBoxOf([{ box: [0.5, 0.2, 0.7, 0.4] }, { box: [0.6, 0.3, 0.9, 0.6] }]), [0.5, 0.2, 0.9, 0.6]);
  assert.equal(wingBoxOf([0.5, 0.5, 0.4, 0.6]), null);
  assert.equal(wingBoxOf(null), null);
});

test('the boxes of a photo by its Drive ID or name; each view its parts, a margin around them, the envelope upright', async () => {
  const store = await photoStore();
  const d = photoBoxes(store, { id: id(1) });
  assert.deepEqual(d, { name: 'CAM000001d', wings: [0.4, 0.1, 0.9, 0.7], envelope: { box: [0.01, 0.2, 0.32, 0.8], turned: 0 }, wide: true });
  assert.deepEqual(photoBoxes(store, { id: 'another-id-not-listed-00', name: 'CAM000001d.JPG' }).envelope, d.envelope);

  assert.deepEqual(viewPlan('wings', d), { parts: [{ box: [0.38, 0.08, 0.92, 0.72], turn: 0 }], side: VIEW_SIZES.wings, wide: true });
  assert.deepEqual(viewPlan('envelope', d), { parts: [{ box: [0, 0.19, 0.33, 0.81], turn: 0 }], side: VIEW_SIZES.envelope, wide: true });
  const both = viewPlan('both', d);
  assert.deepEqual(both.parts.map(p => p.box), [
    [0.38, 0.08, 0.92, 0.72],
    [0, 0.19, 0.33, 0.81],
  ]);
  assert.equal(both.height, VIEW_SIZES.both);

  // Upside down: turned upright. No wing box: said, for the whole photo instead.
  const v = photoBoxes(store, { id: id(2) });
  assert.deepEqual(viewPlan('envelope', v).parts, [{ box: [0, 0, 0.26, 0.51], turn: 180 }]);
  assert.deepEqual(viewPlan('wings', v), { missing: 'no wing box for this photo' });
  assert.deepEqual(viewPlan('both', v), { missing: 'no wing box for this photo' });

  // Per-wing boxes taken together; no envelope read.
  const two = photoBoxes(store, { id: id(3) });
  assert.deepEqual(two.wings, [0.5, 0.2, 0.9, 0.62]);
  assert.match(viewPlan('envelope', two).missing, /envelope was not read/);
  assert.deepEqual(photoBoxes(store, { name: 'CAM999999d' }), { name: 'CAM999999d', wings: null, envelope: null, wide: null });

  // No wing box, but the wing models' box of each wing (the photo readings' column): those, an extra object left out.
  importBundle(store.db, { wings: [{ name: 'CAM000001v', fileId: id(2), cam: 'CAM000001', suffix: 'v', side: 'ventral', pDorsal: 0.01, wings: 2, review: false, source: 'test' }] });
  store.db.prepare("UPDATE photo_wing_sides SET wings_json=? WHERE name='CAM000001v'").run(
    JSON.stringify([
      { box: [0.5, 0.2, 0.7, 0.4], kind: 'forewing' },
      { box: [0.5, 0.45, 0.68, 0.6], kind: 'hindwing' },
      { box: [0.1, 0.1, 0.2, 0.2], kind: 'extra' },
    ]),
  );
  bumpReviewRevision(store.db);
  assert.deepEqual(photoBoxes(store, { id: id(2) }).wings, [0.5, 0.2, 0.7, 0.6]);
  store.close();
});

test('photo_files_look `view`: the parts cut from the 1600 px copy; the whole photo with a note when a box is not known', async t => {
  const store = await photoStore();
  t.after(() => store.close());
  const dir = mkdtempSync(join(tmpdir(), 'photo-views-'));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  const jobs = [];
  const viewer = {
    make: async job => {
      jobs.push(job);
      writeFileSync(job.out, `view ${JSON.stringify(job.parts.map(p => [p.src.split('/').at(-1), p.box, p.turn]))}`);
      return { out: job.out, width: 600, height: 400 };
    },
  };
  const fetchImpl = async url => new Response(`copy ${url}`, { status: 200, headers: { 'content-type': 'image/jpeg' } });
  const photos = createDrivePhotos({ store, cacheDir: join(dir, 'cache'), viewer, fetchImpl, reader: { read: async () => ({}) } });
  const shown = await photos.run('photo_files_look', { files: ['CAM000001d', 'CAM000001v'], view: 'both' }, kim);
  assert.equal(shown[IMAGES].length, 2);
  assert.equal(shown.pictures[0].view, 'both');
  assert.equal(jobs.length, 1);
  assert.deepEqual(jobs[0].parts.map(p => [p.src.split('/').at(-1), p.box, p.turn]), [
    [`${id(1)}-1600.jpg`, [0.38, 0.08, 0.92, 0.72], 0],
    [`${id(1)}-1600.jpg`, [0, 0.19, 0.33, 0.81], 0],
  ]);
  assert.equal(jobs[0].height, VIEW_SIZES.both);
  // No wing box: the 800 px copy, and why.
  assert.equal(shown.pictures[1].view, undefined);
  assert.equal(shown.pictures[1].note, 'no wing box for this photo: the whole photo');
  assert.match(Buffer.from(shown[IMAGES][1].data, 'base64').toString(), /sz=w800$/);

  const envelope = await photos.run('photo_files_look', { files: ['CAM000001v'], view: 'envelope' }, kim);
  assert.match(Buffer.from(envelope[IMAGES][0].data, 'base64').toString(), /^view \[\["drive-id-0+2-1600\.jpg",\[0,0,0\.26,0\.51\],180\]\]$/);
  // Kept: asked again, nothing is made.
  await photos.run('photo_files_look', { files: ['CAM000001v'], view: 'envelope' }, kim);
  assert.equal(jobs.length, 2);
  // A `box` takes the view's place.
  const part = await photos.run('photo_files_look', { files: ['CAM000001d'], view: 'wings', box: [0, 0, 0.5, 0.5] }, kim);
  assert.equal(part.pictures[0].view, undefined);
  assert.deepEqual(part.pictures[0].box, [0, 0, 0.5, 0.5]);
});

test('show_photos `view`: each photo with its wing box, the butterfly with its envelope; the card keeps the view', async t => {
  const store = await photoStore();
  t.after(() => store.close());
  const cards = [];
  const gallery = createGallery({ store, show: card => (cards.push(card), { card: 'c1' }) });
  const out = gallery.run({ ids: ['CAM000001', 'CAM000002'], view: 'both' }, kim, {});
  assert.deepEqual(out.withoutEnvelope, ['CAM000002']);
  const [one, two] = cards[0].items;
  assert.equal(cards[0].view, 'both');
  assert.deepEqual(one.photos.map(p => [p.name, p.wings ?? null]), [
    ['CAM000001d.JPG', [0.4, 0.1, 0.9, 0.7]],
    ['CAM000001v.JPG', null],
  ]);
  // The clearest reading's photo (here the first of the two, both read alike).
  assert.deepEqual(one.envelope, { fileId: id(1), name: 'CAM000001d', bbox: [0.01, 0.2, 0.32, 0.8], turned: 0, aspect: 1.3333 });
  assert.equal(two.envelope, undefined);
  // Without a view: as before, no boxes.
  gallery.run({ ids: ['CAM000001'] }, kim, {});
  assert.equal(cards[1].view, undefined);
  assert.equal(cards[1].items[0].photos[0].wings, undefined);
  assert.equal(cards[1].items[0].envelope, undefined);
});

const sharpDir = readerDir();
test('the views process cuts, turns and joins the parts (the photo reader’s sharp)', { skip: !sharpDir && 'no photo-reader/ with sharp on this computer' }, async t => {
  const sharp = createRequire(join(sharpDir, 'photo-reader.mjs'))('sharp');
  const dir = mkdtempSync(join(tmpdir(), 'photo-views-real-'));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  // A 1600 × 1200 photo: grey, the wings red at [0.4, 0.1, 0.9, 0.7], the envelope blue at [0, 0.2, 0.3, 0.8] with a white mark at its top.
  const src = join(dir, 'photo.jpg');
  const block = (w, h, c) => ({ create: { width: w, height: h, channels: 3, background: c } });
  await sharp(block(1600, 1200, '#808080'))
    .composite([
      { input: await sharp(block(800, 720, '#ff0000')).png().toBuffer(), left: 640, top: 120 },
      { input: await sharp(block(480, 720, '#0000ff')).png().toBuffer(), left: 0, top: 240 },
      { input: await sharp(block(480, 60, '#ffffff')).png().toBuffer(), left: 0, top: 240 },
    ])
    .jpeg()
    .toFile(src);
  const viewer = startViewer(sharpDir);
  t.after(() => viewer.close());
  const colour = async (file, x, y) => {
    const { data, info } = await sharp(file).raw().toBuffer({ resolveWithObject: true });
    const i = (Math.round(y) * info.width + Math.round(x)) * info.channels;
    return [data[i], data[i + 1], data[i + 2]].map(v => (v > 200 ? 1 : v < 60 ? 0 : 0.5));
  };
  const boxes = { wings: [0.4, 0.1, 0.9, 0.7], envelope: { box: [0, 0.2, 0.3, 0.8], turned: 180 }, wide: true };
  const wings = viewPlan('wings', boxes);
  const w = await viewer.make({ out: join(dir, 'w.jpg'), parts: wings.parts.map(p => ({ ...p, src })), side: wings.side, wide: true });
  assert.deepEqual([w.width, w.height], [640, Math.round(((0.64 * 1200) / (0.54 * 1600)) * 640)]);
  assert.deepEqual(await colour(w.out, w.width / 2, w.height / 2), [1, 0, 0]);
  // The envelope turned: its white mark at the bottom now.
  const e = viewPlan('envelope', { ...boxes, wings: null });
  const env = await viewer.make({ out: join(dir, 'e.jpg'), parts: e.parts.map(p => ({ ...p, src })), side: e.side });
  assert.equal(env.height, VIEW_SIZES.envelope);
  assert.deepEqual(await colour(env.out, env.width / 2, env.height - 18), [1, 1, 1]);
  assert.deepEqual(await colour(env.out, env.width / 2, 18), [0, 0, 1]);
  // Both: side by side, one height.
  const b = viewPlan('both', boxes);
  const both = await viewer.make({ out: join(dir, 'b.jpg'), parts: b.parts.map(p => ({ ...p, src })), height: b.height });
  assert.equal(both.height, VIEW_SIZES.both);
  assert.deepEqual(await colour(both.out, 40, both.height / 2), [1, 0, 0]);
  assert.deepEqual(await colour(both.out, both.width - 20, both.height / 2), [0, 0, 1]);
  // A photo of the other shape than its boxes were read on (turned since).
  assert.deepEqual(await viewer.make({ out: join(dir, 'x.jpg'), parts: [{ src, box: [0, 0, 1, 1] }], side: 400, wide: false }), { id: '4', error: 'shape' });
});
