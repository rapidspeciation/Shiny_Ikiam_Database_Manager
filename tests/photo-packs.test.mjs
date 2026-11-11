import assert from 'node:assert/strict';
import test from 'node:test';
import { spawnSync } from 'node:child_process';
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { gzipSync } from 'node:zlib';

// scripts/photo-packs.py taxa mode, schema 2: several photos per taxon, each with the country it was taken in (the
// taxon photo, then one per --obs country), and a preserved specimen for a taxon with no live photo. Local files only:
// no network.
const python = process.env.PYTHON ?? 'python3';
const hasPillow = spawnSync(python, ['-I', '-c', 'import PIL'], { encoding: 'utf8' }).status === 0;
const SCRIPT = new URL('../scripts/photo-packs.py', import.meta.url).pathname;

test('photo-packs.py taxa: photos by country and a preserved specimen', { skip: !hasPillow && 'Pillow not installed' }, () => {
  const dir = mkdtempSync(join(tmpdir(), 'photo-packs-'));
  try {
    const img = name => join(dir, `${name}.png`);
    const made = spawnSync(
      python,
      ['-I', '-c', 'import sys\nfrom PIL import Image\nfor i, p in enumerate(sys.argv[1:]): Image.new("RGB", (600, 400), (40 * i, 90, 160)).save(p)', ...['t1', 'ec', 'co', 'pe', 'wing'].map(img)],
      { encoding: 'utf8' },
    );
    assert.equal(made.status, 0, made.stderr);
    const taxa = [
      { name: 'Mechanitis polymnia', rank: 'species', genus: 'Mechanitis', family: 'Nymphalidae', group: 'ithomiini', countries: ['BR', 'EC'], inatId: 148675,
        photo: { url: img('t1'), inatPhotoId: 59300165, attribution: '(c) A, some rights reserved (CC BY)', license: 'CC-BY', taxonUrl: 'https://www.inaturalist.org/taxa/148675', source: 'https://www.inaturalist.org/photos/59300165' } },
      { name: 'Hyposcada kena', rank: 'species', genus: 'Hyposcada', family: 'Nymphalidae', group: 'ithomiini', countries: ['EC'], inatId: null, photo: null },
    ];
    writeFileSync(join(dir, 'taxa.json'), JSON.stringify(taxa));
    const obs = (rows) => gzipSync(rows.map(r => JSON.stringify(r)).join('\n') + '\n');
    const o = (id, faves, photoId, file, license = 'cc-by-nc') => ({ id, faves, taxon: { id: 148675, name: 'Mechanitis polymnia', rank: 'species', anc: [] }, photos: [{ id: photoId, url: file, license, attribution: `(c) obs ${id}` }] });
    // Ecuador: the most faved wins; a photo without a reusable licence is passed over.
    // The most faved of all (6) is annotated as a caterpillar (--young): left out.
    writeFileSync(join(dir, 'obs-EC.jsonl.gz'), obs([o(1, 0, 101, img('pe')), o(2, 5, 102, img('ec')), o(3, 9, 103, img('co'), null), o(6, 50, 106, img('pe'))]));
    writeFileSync(join(dir, 'young-EC.json'), JSON.stringify([6]));
    // Colombia: also has the taxon photo itself, so that one is known to be from Colombia.
    writeFileSync(join(dir, 'obs-CO.jsonl.gz'), obs([o(4, 1, 104, img('co')), o(5, 0, 59300165, img('t1'))]));
    writeFileSync(join(dir, 'preserved.json'), JSON.stringify({ schema: 'ithomiini-preserved/1', photos: [{ name: 'Hyposcada kena', file: 'wing.png', specimen: 'CAM040123', country: 'EC', attribution: '(c) Wellcome Sanger Institute', license: 'CC-BY' }] }));
    const out = join(dir, 'packs');
    const run = spawnSync(
      python,
      ['-I', SCRIPT, 'taxa', '--taxa', join(dir, 'taxa.json'), '--out', out, '--cell', 'EC:all', '--pause', '0', '--obs', `EC=${join(dir, 'obs-EC.jsonl.gz')}`, '--obs', `CO=${join(dir, 'obs-CO.jsonl.gz')}`, '--young', join(dir, 'young-EC.json'), '--preserved', join(dir, 'preserved.json')],
      { encoding: 'utf8', timeout: 120_000 },
    );
    assert.equal(run.status, 0, run.stderr + run.stdout);
    const index = JSON.parse(readFileSync(join(out, 'index.json'), 'utf8'));
    assert.equal(index.schema, 'ithomiini-photo-packs/2');
    const mp = index.taxa.find(t => t.name === 'Mechanitis polymnia');
    assert.deepEqual(mp.photos.map(p => [p.country, p.source, p.license]), [
      ['CO', 'https://www.inaturalist.org/photos/59300165', 'CC-BY'],
      ['EC', 'https://www.inaturalist.org/photos/102', 'CC-BY-NC'],
      ['CO', 'https://www.inaturalist.org/photos/104', 'CC-BY-NC'],
    ]);
    assert.ok(mp.photos.every(p => p.bytes > 0 && /^\d+-[0-9a-f]{8}\.webp$/.test(p.file) && p.attribution && p.taxonUrl));
    // The taxon photo is marked curated (a pack keeps it second), the observation photos are not.
    assert.deepEqual(mp.photos.map(p => !!p.curated), [true, false, false]);
    const hk = index.taxa.find(t => t.name === 'Hyposcada kena');
    assert.equal(hk.photos.length, 1);
    assert.deepEqual({ kind: hk.photos[0].kind, specimen: hk.photos[0].specimen, country: hk.photos[0].country, license: hk.photos[0].license }, { kind: 'preserved', specimen: 'CAM040123', country: 'EC', license: 'CC-BY' });
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});

test('photo-packs.py taxa: the Sanger crops manifest (poor ones left out, WebP kept as is) and bundles with offsets', { skip: !hasPillow && 'Pillow not installed' }, () => {
  const dir = mkdtempSync(join(tmpdir(), 'photo-packs-'));
  try {
    const made = spawnSync(
      python,
      ['-I', '-c', 'import sys\nfrom PIL import Image\nImage.new("RGB", (600, 400), (10, 90, 160)).save(sys.argv[1])\nfor i, p in enumerate(sys.argv[2:]): Image.new("RGB", (380, 260), (90, 30 * i, 60)).save(p, "WEBP", quality=60)', join(dir, 't1.png'), join(dir, 'CAM1d.webp'), join(dir, 'CAM2d.webp')],
      { encoding: 'utf8' },
    );
    assert.equal(made.status, 0, made.stderr);
    const t = (name, photo) => ({ name, rank: name.split(' ').length > 2 ? 'subspecies' : 'species', genus: name.split(' ')[0], family: 'Nymphalidae', group: 'ithomiini', countries: ['EC'], inatId: null, photo });
    writeFileSync(join(dir, 'taxa.json'), JSON.stringify([
      t('Mechanitis polymnia', { url: join(dir, 't1.png'), inatPhotoId: 5, attribution: '(c) A', license: 'CC-BY', taxonUrl: null, source: 'https://www.inaturalist.org/photos/5' }),
      t('Mechanitis polymnia werneri', null),
      t('Mechanitis polymnia eurydice', null),
    ]));
    writeFileSync(join(dir, 'manifest.json'), JSON.stringify({ label: 'Preserved specimen', items: [
      { taxon: 'Mechanitis polymnia werneri', rank: 'subspecies', specimen_id: 'CAM000001', file: 'CAM1d.webp', poor: false },
      { taxon: 'Mechanitis polymnia eurydice', rank: 'subspecies', specimen_id: 'CAM000002', file: 'CAM2d.webp', poor: true },
      // A taxon with a live photo: no preserved one.
      { taxon: 'Mechanitis polymnia', rank: 'species', specimen_id: 'CAM000003', file: 'CAM2d.webp', poor: false },
    ] }));
    const out = join(dir, 'packs');
    const run = spawnSync(python, ['-I', SCRIPT, 'taxa', '--taxa', join(dir, 'taxa.json'), '--out', out, '--cell', 'EC:all', '--pause', '0', '--preserved', join(dir, 'manifest.json'), '--bundle-mb', '0.001'], { encoding: 'utf8', timeout: 120_000 });
    assert.equal(run.status, 0, run.stderr + run.stdout);
    const index = JSON.parse(readFileSync(join(out, 'index.json'), 'utf8'));
    const of = name => index.taxa.find(x => x.name === name).photos;
    assert.deepEqual(of('Mechanitis polymnia werneri').map(p => [p.kind, p.specimen, p.attribution, p.license]), [['preserved', 'CAM000001', 'Ikiam / Wellcome Sanger Institute', '']]);
    // Schema 1's fields beside (r4-sol-F1): the first photo as `photo`, its size as `bytes`, for pages of release 1.
    for (const x of index.taxa) {
      assert.equal(x.photo?.file ?? null, x.photos[0]?.file ?? null);
      assert.equal(x.bytes, x.photos[0]?.bytes ?? 0);
    }
    assert.deepEqual(of('Mechanitis polymnia eurydice'), []);
    assert.equal(of('Mechanitis polymnia').length, 1);
    assert.equal(of('Mechanitis polymnia')[0].kind, undefined);
    // The crop's bytes unchanged (made at the pack's settings already).
    const crop = of('Mechanitis polymnia werneri')[0];
    assert.deepEqual(readFileSync(join(out, 'photos', crop.file)), readFileSync(join(dir, 'CAM1d.webp')));
    // Bundles: each photo's bytes at its place (a tiny bundle size).
    assert.ok(index.bundles.length >= 1);
    for (const p of [crop, of('Mechanitis polymnia')[0]]) {
      const [b, offset] = p.at;
      const bundle = readFileSync(join(out, index.bundles[b].file));
      assert.equal(bundle.length, index.bundles[b].bytes);
      assert.deepEqual(bundle.subarray(offset, offset + p.bytes), readFileSync(join(out, 'photos', p.file)));
      assert.match(index.bundles[b].file, /^bundles\/b\d{3}-[0-9a-f]{10}\.bin$/);
    }
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});
