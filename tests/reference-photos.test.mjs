import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { importBundle } from '../server/envelope-import.mjs';
import { createDrivePhotos } from '../server/drive-photos.mjs';
import { IMAGES } from '../server/rename-photos.mjs';
import { referenceLabel, sexOf, speciesOf } from '../server/reference-photos.mjs';

// reference_photos (server/reference-photos.mjs, run by server/drive-photos.mjs): the collection's
// butterflies of a species (those the model agrees with first) and the field and specimen photos of a
// small reference set, a page at a time, each picture with a line saying what it is.

const id = name => `drive-id-${name.padStart(20, '0')}`;
const viewer = { role: 'viewer' };

test('names and sexes as the workbook writes them', () => {
  assert.deepEqual(speciesOf('mechanitis  Lysimnia'), { binomial: 'Mechanitis lysimnia', subspecies: null });
  assert.deepEqual(speciesOf('Mechanitis polymnia werneri x proceriformis'), { binomial: 'Mechanitis polymnia', subspecies: 'werneri' });
  assert.equal(speciesOf('Mechanitis'), null);
  assert.equal(speciesOf('Chetone sp.'), null);
  assert.deepEqual(['female ?', 'male_?', 'Male', 'NOT_COLLECTED', null].map(sexOf), ['female', 'male', 'male', null, null]);
  assert.equal(
    referenceLabel({ type: 'field', source: 'sangay', species: 'Melinaea mothone', siteName: 'Melinaea marsaeus mothone', sex: 'male', place: 'Sucua, Morona Santiago' }),
    'field photo, sangay.eu: Melinaea mothone (Melinaea marsaeus mothone there) (male, Sucua, Morona Santiago)',
  );
});

async function fixture(t, { withSet = true } = {}) {
  const dir = mkdtempSync(join(tmpdir(), 'reference-photos-'));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  const row = (n, cam, species, sex, extra = {}) => ({ row: n, values: { CAM_ID: cam, SPECIES: species, Sex: sex, ...extra } });
  const photographed = ['CAM000011', 'CAM000012', 'CAM000013', 'CAM000015', 'CAM000016'];
  const sheets = new LocalSheets({
    Collection_data: [
      row(2, 'CAM000011', 'Mechanitis lysimnia', 'female'),
      row(3, 'CAM000012', 'Mechanitis lysimnia', 'male'),
      row(4, 'CAM000013', 'Mechanitis lysimnia', 'female ?'),
      // No photos.
      row(5, 'CAM000014', 'Mechanitis lysimnia', 'female'),
      // Not read by the model.
      row(6, 'CAM000015', 'Mechanitis lysimnia roqueensis', 'female'),
      // The butterfly in question.
      row(7, 'CAM000016', 'Mechanitis lysimnia', 'female'),
      row(8, 'CAM000017', 'Mechanitis lysimniafake', 'female'),
    ],
    Photo_links: photographed.flatMap((cam, i) =>
      ['d', 'v'].map((view, j) => ({
        row: 2 + i * 2 + j,
        values: { 'Full Path': `Wild/${cam}${view}.JPG`, Name: `${cam}${view}.JPG`, Type: 'File', URL: `https://drive.google.com/file/d/${id(cam + view)}/view` },
      })),
    ),
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Collection_data', 'Photo_links'] });
  t.after(() => store.close());
  importBundle(store.db, {
    wingBoxes: [['CAM000011d', [0.4, 0.1, 0.9, 0.7], 0.98]],
    predictions: [
      { cam: 'CAM000011', species: [['Mechanitis lysimnia', 0.81]], genus: [], subspecies: [] },
      { cam: 'CAM000012', species: [['Mechanitis lysimnia', 0.95]], genus: [], subspecies: [] },
      { cam: 'CAM000013', species: [['Melinaea mothone', 0.7]], genus: [], subspecies: [] },
      { cam: 'CAM000016', species: [['Mechanitis lysimnia', 0.99]], genus: [], subspecies: [] },
    ],
  });
  // A tiny reference set.
  const set = join(dir, 'reference-photos');
  const entry = (file, source, type, extra = {}) => {
    mkdirSync(join(set, 'images', 'x'), { recursive: true });
    writeFileSync(join(set, 'images', 'x', file), `jpeg ${file}`);
    return { file: `images/x/${file}`, source, type, ...extra };
  };
  if (withSet)
    writeFileSync(
      join(set, 'index.json'),
      JSON.stringify({
        version: 1,
        species: {
          'Mechanitis lysimnia': [
            entry('f1.jpg', 'sangay', 'field', { species: 'Mechanitis lysimnia', subspecies: 'ocona', place: 'Topo, Tungurahua' }),
            entry('f2.jpg', 'noreste', 'field', { species: 'Mechanitis lysimnia', subspecies: 'ocona', sex: 'male' }),
            entry('f3.jpg', 'cotacachi', 'field', { species: 'Mechanitis lysimnia', subspecies: 'roqueensis' }),
            entry('f4.jpg', 'loja-zamora', 'field', { species: 'Mechanitis lysimnia', sex: 'female' }),
            entry('s1.jpg', 'sangay', 'specimen', { species: 'Mechanitis lysimnia', sex: 'male', view: 'dorsal' }),
            entry('s2.jpg', 'sangay', 'specimen', { species: 'Mechanitis lysimnia', sex: 'female', view: 'dorsal' }),
            entry('s3.jpg', 'noreste', 'specimen', { species: 'Mechanitis lysimnia', sex: 'female', view: 'ventral' }),
          ],
          'Melinaea mothone': [entry('m1.jpg', 'sangay', 'field', { species: 'Melinaea mothone', siteName: 'Melinaea marsaeus mothone' })],
          // A file outside the set's folder is never read.
          'Melinaea menophilus': [{ file: '../../etc/passwd', source: 'sangay', type: 'field', species: 'Melinaea menophilus' }],
        },
      }),
    );
  const jobs = [];
  const views = {
    make: async job => {
      jobs.push(job);
      writeFileSync(job.out, `view ${JSON.stringify(job.parts.map(p => [p.src.split('/').at(-1), p.box]))}`);
      return { out: job.out, width: 700, height: job.height };
    },
  };
  const fetchImpl = async url => new Response(`copy ${url}`, { status: 200, headers: { 'content-type': 'image/jpeg' } });
  const photos = createDrivePhotos({ store, config: { referencePhotosDir: withSet ? set : join(dir, 'none') }, cacheDir: join(dir, 'cache'), viewer: views, fetchImpl, reader: { read: async () => ({}) } });
  return { photos, jobs };
}

const text = image => Buffer.from(image.data, 'base64').toString();

test('a page: the collection butterflies the model agrees with first (dorsal | ventral wings), then field and specimen photos, the sex asked first', async t => {
  const { photos, jobs } = await fixture(t);
  // Anyone in a chat: it only reads.
  const out = await photos.run('reference_photos', { species: 'Mechanitis lysimnia', sex: 'female', cam: 'cam000016' }, viewer);
  assert.equal(out.error, undefined);
  assert.deepEqual(out.pictures, [
    '1. collection CAM000011: Mechanitis lysimnia, female in the workbook; the model agrees (0.81); dorsal | ventral',
    '2. collection CAM000015: Mechanitis lysimnia roqueensis, female in the workbook; not read by the model; dorsal | ventral',
    '3. field photo, loja-zamora.eu: Mechanitis lysimnia (female)',
    '4. field photo, sangay.eu: Mechanitis lysimnia ocona (Topo, Tungurahua)',
    '5. field photo, cotacachi.eu: Mechanitis lysimnia roqueensis',
    '6. specimen photo, sangay.eu: Mechanitis lysimnia (female, dorsal)',
    '7. specimen photo, noreste.eu: Mechanitis lysimnia (female, ventral)',
  ]);
  assert.equal(out[IMAGES].length, 7);
  assert.deepEqual(out.counts, { collection: 3, field: 4, specimen: 3 });
  assert.equal(out.more, 'page 2');
  // CAM000011: its dorsal's wings (a box known), its ventral whole; from Drive's 800 px copies, 300 px high.
  assert.deepEqual(JSON.parse(text(out[IMAGES][0]).slice(5)), [
    [`${id('CAM000011d')}-800.jpg`, [0.38, 0.08, 0.92, 0.72]],
    [`${id('CAM000011v')}-800.jpg`, [0, 0, 1, 1]],
  ]);
  assert.equal(jobs[0].height, 300);
  assert.equal(text(out[IMAGES][2]), 'jpeg f4.jpg');

  // The next page: the butterfly the model gives another species, said so.
  const next = await photos.run('reference_photos', { species: 'Mechanitis lysimnia', sex: 'female', cam: 'CAM000016', page: 2 }, viewer);
  assert.deepEqual(next.pictures, [
    '1. collection CAM000013: Mechanitis lysimnia, female in the workbook; the model says Melinaea mothone (0.7); dorsal | ventral',
    '2. field photo, noreste.eu: Mechanitis lysimnia ocona (male)',
    '3. specimen photo, sangay.eu: Mechanitis lysimnia (male, dorsal)',
  ]);
  assert.equal(next.more, undefined);
});

test('a subspecies asked comes first; a species the set has under another name says so', async t => {
  const { photos } = await fixture(t);
  const sub = await photos.run('reference_photos', { species: 'Mechanitis lysimnia roqueensis' }, viewer);
  assert.equal(sub.species, 'Mechanitis lysimnia roqueensis');
  assert.match(sub.pictures[0], /collection CAM000015/);
  assert.match(sub.pictures.find(p => p.includes('field photo')), /cotacachi\.eu: Mechanitis lysimnia roqueensis$/);
  const raised = await photos.run('reference_photos', { species: 'Melinaea mothone' }, viewer);
  assert.deepEqual(raised.pictures, ['1. field photo, sangay.eu: Melinaea mothone (Melinaea marsaeus mothone there)']);
  assert.match(raised.collection, /No butterfly of Melinaea mothone in the workbook/);
  const outside = await photos.run('reference_photos', { species: 'Melinaea menophilus' }, viewer);
  assert.equal(outside[IMAGES].length, 0);
});

test('a species without reference photos: said, with those of its genus; without the set installed: said', async t => {
  const { photos } = await fixture(t);
  const none = await photos.run('reference_photos', { species: 'Mechanitis polymnia' }, viewer);
  assert.match(none.reference, /^No field or specimen photos of Mechanitis polymnia: .*other countries \(Colombia, Brazil…\) have none yet\.$/);
  assert.deepEqual(none.sameGenus, ['Mechanitis lysimnia']);
  assert.equal(none[IMAGES].length, 0);
  assert.match((await photos.run('reference_photos', { species: 'Mechanitis' }, viewer)).error, /genus and its species/);

  const bare = await fixture(t, { withSet: false });
  const out = await bare.photos.run('reference_photos', { species: 'Mechanitis lysimnia', sex: 'male' }, viewer);
  assert.match(out.reference, /not installed/);
  assert.deepEqual(out.pictures, ['1. collection CAM000012: Mechanitis lysimnia, male in the workbook; the model agrees (0.95); dorsal | ventral']);
});
