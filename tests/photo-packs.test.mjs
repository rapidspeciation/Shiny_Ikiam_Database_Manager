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
    writeFileSync(join(dir, 'obs-EC.jsonl.gz'), obs([o(1, 0, 101, img('pe')), o(2, 5, 102, img('ec')), o(3, 9, 103, img('co'), null)]));
    // Colombia: also has the taxon photo itself, so that one is known to be from Colombia.
    writeFileSync(join(dir, 'obs-CO.jsonl.gz'), obs([o(4, 1, 104, img('co')), o(5, 0, 59300165, img('t1'))]));
    writeFileSync(join(dir, 'preserved.json'), JSON.stringify({ schema: 'ithomiini-preserved/1', photos: [{ name: 'Hyposcada kena', file: 'wing.png', specimen: 'CAM040123', country: 'EC', attribution: '(c) Wellcome Sanger Institute', license: 'CC-BY' }] }));
    const out = join(dir, 'packs');
    const run = spawnSync(
      python,
      ['-I', SCRIPT, 'taxa', '--taxa', join(dir, 'taxa.json'), '--out', out, '--cell', 'EC:all', '--pause', '0', '--obs', `EC=${join(dir, 'obs-EC.jsonl.gz')}`, '--obs', `CO=${join(dir, 'obs-CO.jsonl.gz')}`, '--preserved', join(dir, 'preserved.json')],
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
    const hk = index.taxa.find(t => t.name === 'Hyposcada kena');
    assert.equal(hk.photos.length, 1);
    assert.deepEqual({ kind: hk.photos[0].kind, specimen: hk.photos[0].specimen, country: hk.photos[0].country, license: hk.photos[0].license }, { kind: 'preserved', specimen: 'CAM040123', country: 'EC', license: 'CC-BY' });
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});
