import { test } from 'node:test';
import assert from 'node:assert/strict';
import http from 'node:http';
import { createHash } from 'node:crypto';
import { mkdirSync, mkdtempSync, rmSync, utimesSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fieldPhotos, ithomiiniSpecies, refPhotoUrl, sendRefPhoto } from '../server/identify-photos.mjs';
import { serveOfflineId } from '../server/offline-id.mjs';

test('refPhotoUrl: only iNaturalist photo hosts and sizes', () => {
  assert.equal(refPhotoUrl('https://inaturalist-open-data.s3.amazonaws.com/photos/123/small.jpg'), 'https://inaturalist-open-data.s3.amazonaws.com/photos/123/small.jpg');
  assert.equal(refPhotoUrl('https://static.inaturalist.org/photos/9/medium.jpeg'), 'https://static.inaturalist.org/photos/9/medium.jpeg');
  // «large»: the full-screen viewer's size (Identificar's zoom)
  assert.equal(refPhotoUrl('https://static.inaturalist.org/photos/9/large.jpg'), 'https://static.inaturalist.org/photos/9/large.jpg');
  for (const bad of [
    'http://static.inaturalist.org/photos/9/small.jpg',
    'https://static.inaturalist.org.evil.com/photos/9/small.jpg',
    'https://static.inaturalist.org/photos/9/original.jpg',
    'https://static.inaturalist.org/photos/9/small.jpg?x=1',
    'https://127.0.0.1/photos/9/small.jpg',
    '',
    undefined,
  ])
    assert.equal(refPhotoUrl(bad), null, String(bad));
});

test('sendRefPhoto: streams an allowed image, refuses others and non-images', async () => {
  const image = Buffer.from([0xff, 0xd8, 0xff, 1, 2, 3]);
  const fakeFetch = async url => (url.includes('/1/') ? new Response(image, { headers: { 'content-type': 'image/jpeg' } }) : new Response('<html>', { headers: { 'content-type': 'text/html' } }));
  const server = http.createServer((req, res) => sendRefPhoto(req, res, new URL(req.url, 'http://x').searchParams.get('u'), { fetchImpl: fakeFetch }));
  await new Promise(ok => server.listen(0, '127.0.0.1', ok));
  const base = `http://127.0.0.1:${server.address().port}/?u=`;
  try {
    const ok = await fetch(base + encodeURIComponent('https://static.inaturalist.org/photos/1/small.jpg'));
    assert.equal(ok.status, 200);
    assert.equal(ok.headers.get('cross-origin-resource-policy'), 'same-origin');
    assert.deepEqual(Buffer.from(await ok.arrayBuffer()), image);
    assert.equal((await fetch(base + encodeURIComponent('https://static.inaturalist.org/photos/2/small.jpg'))).status, 502);
    assert.equal((await fetch(base + encodeURIComponent('https://example.com/a.jpg'))).status, 404);
  } finally {
    server.close();
  }
});

test('fieldPhotos: one species-level photo per Ithomiini species, genus fallbacks left out, pending counted', () => {
  const names = ithomiiniSpecies(['Oleria onega', 'Heliconius erato', 'Mechanitis polymnia', 'Mechanitis lysimnia'], ['Mechanitis', 'Oleria']);
  assert.deepEqual(names, ['Mechanitis lysimnia', 'Mechanitis polymnia', 'Oleria onega']);
  const photo = id => ({ id, thumb: `https://static.inaturalist.org/photos/${id}/small.jpg`, attribution: '(c) A', license: 'CC-BY' });
  const photosOf = list =>
    Object.fromEntries(
      list.map(n => [
        n,
        n === 'Mechanitis polymnia'
          ? { inat: { level: 'species', taxon: { url: 'https://www.inaturalist.org/taxa/1' }, photos: [photo(1)] }, pending: false }
          : n === 'Oleria onega'
            ? { inat: { level: 'genus', photos: [photo(2)] }, pending: false }
            : { inat: null, pending: true },
      ]),
    );
  const out = fieldPhotos(photosOf, names, { batch: 2 });
  assert.deepEqual(out, {
    species: [{ name: 'Mechanitis polymnia', thumb: photo(1).thumb, attribution: '(c) A', license: 'CC-BY', taxonUrl: 'https://www.inaturalist.org/taxa/1' }],
    pending: 1,
    total: 3,
  });
});

test('the Identificar page: isolated like the test page, no models of its own', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'identify-page-'));
  const web = join(dir, 'web');
  mkdirSync(join(web, 'identificar'), { recursive: true });
  writeFileSync(join(web, 'identificar', 'index.html'), '<!doctype html><title>t</title>');
  const server = http.createServer((req, res) => {
    const path = new URL(req.url, 'http://x').pathname.slice('/identificar'.length);
    serveOfflineId(req, res, { page: 'identificar', path, webRoot: web, identifierDir: dir, user: { username: 'u' } });
  });
  await new Promise(ok => server.listen(0, '127.0.0.1', ok));
  const base = `http://127.0.0.1:${server.address().port}/identificar`;
  try {
    const page = await fetch(`${base}/`);
    assert.equal(page.status, 200);
    assert.equal(page.headers.get('cross-origin-embedder-policy'), 'require-corp');
    const bare = await fetch(base, { redirect: 'manual' });
    assert.equal(bare.status, 301);
    assert.equal(bare.headers.get('location'), 'identificar/');
    assert.equal((await fetch(`${base}/models/list.json`)).status, 404);
  } finally {
    server.close();
  }
});

test('identifyUpload: a JPEG to the identifier with the side, the file removed after; bad input refused', async () => {
  const { identifyUpload } = await import('../server/identify-photos.mjs');
  const { existsSync, readdirSync } = await import('node:fs');
  const scratch = mkdtempSync(join(tmpdir(), 'identify-upload-'));
  const jpeg = Buffer.from([0xff, 0xd8, 0xff, 0xe0, 1, 2, 3]).toString('base64');
  const seen = [];
  const identifier = {
    identify: async ({ path, side }) => {
      seen.push({ side, there: existsSync(path) });
      return { species: [['Mechanitis messenoides', 0.8]], subspecies: [['Mechanitis messenoides deceptus', 0.5]], genus: [['Mechanitis', 0.9]], tribe: [['Ithomiini', 0.95]], subfamily: [['Danainae', 0.95]], crop: 'success', wingBox: [0.1, 0.2, 0.8, 0.9], prior: 'East', ms: 900, head: 'head_slot1_3bb_dist_rel_7d54b54f', rssMb: 1 };
    },
  };
  const ok = await identifyUpload({ identifier, body: { photo: jpeg, side: 'East' }, scratch });
  assert.equal(ok.status, 200);
  assert.deepEqual(ok.body.species, [['Mechanitis messenoides', 0.8]]);
  assert.deepEqual(ok.body.wingBox, [0.1, 0.2, 0.8, 0.9]);
  // The tribes' and subfamilies' totals pass through (the page's tribe / subfamily answer, a lab flag).
  assert.deepEqual(ok.body.tribe, [['Ithomiini', 0.95]]);
  assert.deepEqual(ok.body.subfamily, [['Danainae', 0.95]]);
  assert.equal(ok.body.head, 'head_slot1_3bb_dist_rel_7d54b54f');
  assert.equal(ok.body.rssMb, undefined);
  assert.deepEqual(seen, [{ side: 'East', there: true }]);
  assert.deepEqual(readdirSync(scratch), []);
  // Any other side is no side.
  await identifyUpload({ identifier, body: { photo: jpeg, side: 'North' }, scratch });
  assert.equal(seen[1].side, null);
  assert.equal((await identifyUpload({ identifier, body: { photo: Buffer.from('GIF89a').toString('base64') }, scratch })).status, 400);
  assert.equal((await identifyUpload({ identifier, body: { photo: '<script>' }, scratch })).status, 400);
  assert.equal((await identifyUpload({ identifier, body: {}, scratch })).status, 400);
  assert.equal((await identifyUpload({ identifier: null, body: { photo: jpeg }, scratch })).status, 503);
  const failing = { identify: async () => ({ error: 'boom' }) };
  assert.equal((await identifyUpload({ identifier: failing, body: { photo: jpeg }, scratch })).status, 503);
  assert.deepEqual(readdirSync(scratch), []);
});

test('headSides: the sides of the Andes the head records per species; identifyUpload lists them for its species', async () => {
  const { headSides, identifyUpload } = await import('../server/identify-photos.mjs');
  const dir = mkdtempSync(join(tmpdir(), 'head-sides-'));
  const path = join(dir, 'head.json');
  // leaves: A east (1|2|4=7), A west (1|2|8=b), B known but not in Ecuador (1), C east (7)
  writeFileSync(path, JSON.stringify({ species: ['G a', 'G b', 'G c'], leaf2sp: [0, 0, 1, 2], geo: '7b17' }));
  const sides = await headSides(path);
  assert.deepEqual(sides.get('G a'), { East: true, West: true });
  assert.equal(sides.has('G b'), false);
  assert.deepEqual(sides.get('G c'), { East: true, West: false });
  const jpeg = Buffer.from([0xff, 0xd8, 0xff, 0xe0]).toString('base64');
  const identifier = { identify: async ({ first }) => (assert.equal(first, true), { species: [['G c', 0.6], ['G b', 0.2]], subspecies: [], genus: [['G', 0.8]], crop: 'success' }) };
  const out = await identifyUpload({ identifier, body: { photo: jpeg }, scratch: join(dir, 's'), sides: () => headSides(path) });
  assert.deepEqual(out.body.recorded, { 'G c': { East: true, West: false } });
});

test('the Identificar page: photo packs from PACKS_DIR, the list gzipped when its .gz is there', async () => {
  const { gzipSync, gunzipSync } = await import('node:zlib');
  const dir = mkdtempSync(join(tmpdir(), 'identify-packs-'));
  const web = join(dir, 'web');
  const packs = join(dir, 'packs');
  mkdirSync(join(web, 'identificar', 'packs'), { recursive: true });
  mkdirSync(join(packs, 'photos'), { recursive: true });
  writeFileSync(join(web, 'identificar', 'packs', 'index.json'), '{"beside":"page"}');
  const list = JSON.stringify({ schema: 'ithomiini-photo-packs/1', version: 'v1', taxa: [] });
  writeFileSync(join(packs, 'index.json'), list);
  writeFileSync(join(packs, 'photos', '1.webp'), Buffer.from('RIFF0000WEBP'));
  const serve = packsDir => http.createServer((req, res) => {
    const path = new URL(req.url, 'http://x').pathname.slice('/identificar'.length);
    serveOfflineId(req, res, { page: 'identificar', path, webRoot: web, identifierDir: null, user: null, packsDir });
  });
  const run = async (packsDir, fn) => {
    const server = serve(packsDir);
    await new Promise(ok => server.listen(0, '127.0.0.1', ok));
    try {
      await fn(`http://127.0.0.1:${server.address().port}/identificar`);
    } finally {
      server.close();
    }
  };
  await run(packs, async base => {
    const plain = await fetch(`${base}/packs/index.json`);
    assert.equal(plain.status, 200);
    assert.equal(await plain.text(), list);
    // The list is checked every time, also without gzip (it used to be kept an hour then: a phone showed old sizes).
    assert.equal(plain.headers.get('cache-control'), 'no-cache');
    const photo = await fetch(`${base}/packs/photos/1.webp`);
    assert.equal(photo.headers.get('content-type'), 'image/webp');
    assert.equal(photo.headers.get('cross-origin-resource-policy'), 'same-origin');
    assert.equal(photo.headers.get('cache-control'), 'public, max-age=31536000, immutable');
    // A bundle: a byte range of it, as a phone saving a pack asks (scripts/photo-packs.py make_bundles).
    mkdirSync(join(packs, 'bundles'), { recursive: true });
    writeFileSync(join(packs, 'bundles', 'b000-0123456789.bin'), Buffer.from('AAAABBBBCCCC'));
    const part = await fetch(`${base}/packs/bundles/b000-0123456789.bin`, { headers: { range: 'bytes=4-7' } });
    assert.equal(part.status, 206);
    assert.equal(part.headers.get('content-range'), 'bytes 4-7/12');
    assert.equal(part.headers.get('cache-control'), 'public, max-age=31536000, immutable');
    assert.equal(await part.text(), 'BBBB');
    assert.equal((await fetch(`${base}/packs/../../web/identificar/index.html`)).status, 404);
    // With its .gz (as new as the list): compressed for a browser that takes gzip.
    writeFileSync(join(packs, 'index.json.gz'), gzipSync(list));
    const later = new Date(Date.now() + 5000);
    utimesSync(join(packs, 'index.json.gz'), later, later);
    const gz = await new Promise((ok, fail) =>
      http.get(`${base}/packs/index.json`, { headers: { 'accept-encoding': 'gzip' } }, r => {
        const chunks = [];
        r.on('data', c => chunks.push(c));
        r.on('end', () => ok({ headers: r.headers, body: Buffer.concat(chunks) }));
      }).on('error', fail),
    );
    assert.equal(gz.headers['content-encoding'], 'gzip');
    assert.equal(gz.headers['content-type'], 'application/json');
    assert.equal(gunzipSync(gz.body).toString(), list);
  });
  // Without PACKS_DIR: the list beside the page.
  await run(null, async base => assert.deepEqual(await (await fetch(`${base}/packs/index.json`)).json(), { beside: 'page' }));
});

test('/identificar/confirmar/: a guide card asked by its version in the list is cached for good, any other way an hour', async () => {
  const web = mkdtempSync(join(tmpdir(), 'confirmar-cards-'));
  const cards = join(web, 'identificar', 'confirmar', 'cards');
  mkdirSync(join(cards, 'es'), { recursive: true });
  mkdirSync(join(cards, 'en'), { recursive: true });
  const bytes = Buffer.from('RIFF0000WEBPcard');
  writeFileSync(join(cards, 'es', 'p01_west.webp'), bytes);
  writeFileSync(join(cards, 'en', 'p01_west.webp'), bytes);
  writeFileSync(join(cards, 'es', 'unlisted.webp'), bytes);
  const sha = createHash('sha256').update(bytes).digest('hex');
  const list = join(web, 'identificar', 'confirmar', 'confirmar_visual_manifest.json');
  const listOf = hash =>
    JSON.stringify({ pairs: { p01: { routes: [{ card_id: null }, { assets: { es: { path: 'identificar/confirmar/cards/es/p01_west.webp', sha256: hash } } }] } } });
  writeFileSync(list, listOf(sha));
  const server = http.createServer((req, res) => {
    const path = new URL(req.url, 'http://x').pathname.slice('/identificar'.length);
    serveOfflineId(req, res, { page: 'identificar', path, webRoot: web, identifierDir: null, user: null });
  });
  await new Promise(ok => server.listen(0, '127.0.0.1', ok));
  const base = `http://127.0.0.1:${server.address().port}/identificar/confirmar`;
  const cache = async path => {
    const r = await fetch(`${base}/${path}`);
    assert.equal(r.status, 200, path);
    return r.headers.get('cache-control');
  };
  const forGood = 'public, max-age=31536000, immutable';
  const anHour = 'public, max-age=3600';
  try {
    const v = sha.slice(0, 10);
    const card = await fetch(`${base}/cards/es/p01_west.webp?v=${v}`);
    assert.equal(card.headers.get('cache-control'), forGood);
    assert.equal(card.headers.get('content-type'), 'image/webp');
    assert.deepEqual(Buffer.from(await card.arrayBuffer()), bytes);
    // No version, another release's version, a longer one, a card the list does not name (the same bytes in another language).
    assert.equal(await cache('cards/es/p01_west.webp'), anHour);
    assert.equal(await cache('cards/es/p01_west.webp?v=0123456789'), anHour);
    assert.equal(await cache(`cards/es/p01_west.webp?v=${sha}`), anHour);
    assert.equal(await cache(`cards/en/p01_west.webp?v=${v}`), anHour);
    assert.equal(await cache(`cards/es/unlisted.webp?v=${v}`), anHour);
    // The list itself is never kept for good, and a card that is not there is a 404.
    assert.equal(await cache(`confirmar_visual_manifest.json?v=${v}`), anHour);
    assert.equal((await fetch(`${base}/cards/es/gone.webp?v=${v}`)).status, 404);
    // A new list (a card rebuilt): the old version is no longer this release's.
    const other = 'f'.repeat(64);
    writeFileSync(list, listOf(other) + ' ');
    const later = new Date(Date.now() + 5000);
    utimesSync(list, later, later);
    assert.equal(await cache(`cards/es/p01_west.webp?v=${v}`), anHour);
    assert.equal(await cache(`cards/es/p01_west.webp?v=${other.slice(0, 10)}`), forGood);
    // No list (a build without the guides): nothing is kept for good.
    rmSync(list);
    assert.equal(await cache(`cards/es/p01_west.webp?v=${v}`), anHour);
  } finally {
    server.close();
    rmSync(web, { recursive: true, force: true });
  }
});
