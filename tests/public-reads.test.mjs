// Identificar's and Buscar's public reads without a sign-in (server/public-reads.mjs; .work/public-gates-20261113/GATES.md):
// each opened route answers a visitor with only its public fields and its own limits; writes and the team's data
// keep their 401. No AI and no live iNaturalist: iNaturalist, its photos and Drive are fakes.
import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { createApp } from '../server/index.mjs';
import { setPasswordHashCost } from '../server/auth.mjs';
import { AddressLimiter, ANON_MAX_NAME_LENGTH, ANON_MAX_NAMES, clientAddress, createPublicLists, DriveBudget } from '../server/public-reads.mjs';

setPasswordHashCost(16);

const fileId = name => `1${name.replace(/\W/g, '_').padEnd(30, 'x')}`;
const seed = {
  Collection_data: [
    ['CAM000001', 'Oleria onega', 'female'],
    ['CAM000002', 'Oleria onega', 'male'],
    ['CAM000003', 'Oleria onega', 'female'],
    // Raw camera files in Photo_links (the gallery publishes only the JPEGs): one CAM with a JPEG too, one without.
    ['CAM000004', 'Godyris zavaleta', 'female'],
    ['CAM000005', 'Godyris zavaleta', 'male'],
  ].map(([cam, SPECIES, Sex], i) => ({ row: i + 2, values: { CAM_ID: cam, SPECIES, Sex, Release_Collect: 'Collected_Preserved', Collector: 'A Person' } })),
  Photo_links: ['CAM000001d.JPG', 'CAM000001v.JPG', 'CAM000002d.JPG', 'CAM000004d.CR2', 'CAM000004d_1.JPG', 'CAM000005d.orf'].map((name, i) => ({
    row: i + 2,
    values: { Name: name, URL: `https://drive.google.com/file/d/${fileId(name.replace(/\.JPG$/, ''))}/view` },
  })),
};
const INAT = id => `https://inaturalist-open-data.s3.amazonaws.com/photos/${id}/small.jpg`;

async function setup(t, { anonLimits } = {}) {
  const web = mkdtempSync(join(tmpdir(), 'public-reads-'));
  t.after(() => rmSync(web, { recursive: true, force: true }));
  mkdirSync(join(web, 'identificar'));
  // The curated list names Oleria onega and one of its photos (id 222).
  writeFileSync(join(web, 'identificar', 'curated.json'), JSON.stringify({ schema: 'ithomiini-curated/1', taxa: { 'Oleria onega': { field: [{ id: '222' }] }, 'Godyris zavaleta': { field: [] } } }));
  const inatCalls = [];
  const fetchInat = async url => {
    inatCalls.push(url);
    return new Response(JSON.stringify({ results: [] }), { headers: { 'content-type': 'application/json' } });
  };
  const photoCalls = [];
  const fetchRefPhoto = async url => {
    photoCalls.push(url);
    return new Response(Buffer.alloc(20, 7), { headers: { 'content-type': 'image/jpeg', 'content-length': '20' } });
  };
  const app = await createApp(
    { databasePath: ':memory:', localMode: true, secureCookies: false, syncIntervalMs: 0, setupToken: 'public-reads-setup', webRoot: web, anonLimits },
    { seed, fetchInat, fetchRefPhoto, fetchPhoto: async () => new Response(Buffer.alloc(10, 1), { headers: { 'content-type': 'image/jpeg' } }) },
  );
  await app.ready;
  // A specimen photo from the photo archive's manifest, not listed in Photo_links: the team's only.
  app.store.db.prepare("INSERT INTO envelope_readings(name, file_id, cam, imported_at) VALUES('CAM000003d', ?, 'CAM000003', 'now')").run(fileId('CAM000003d'));
  // Oleria onega cached, with a field a newer server might add (not public until listed).
  app.store.db
    .prepare('INSERT INTO species_photo_cache(name,data_json,fetched_at) VALUES(?,?,?)')
    .run(
      'Oleria onega',
      JSON.stringify({
        taxon: { id: 10, name: 'Oleria onega', rank: 'species', common: null, url: 'https://www.inaturalist.org/taxa/10', internal: 'x' },
        level: 'species',
        reviewer: 'someone@example.org',
        photos: [{ id: 111, thumb: INAT(111), url: INAT(111).replace('small', 'large'), attribution: '(c) Ana, some rights reserved (CC BY-NC)', license: 'CC-BY-NC', observerEmail: 'ana@example.org' }],
      }),
      Date.now(),
    );
  const address = await app.listen(0, '127.0.0.1');
  // The background lookups end before the database closes.
  t.after(async () => {
    await app.speciesPhotos.idle();
    await app.close();
  });
  const base = `http://127.0.0.1:${address.port}/ithomiini/api`;
  const call = async (method, path, { body, session, ip } = {}) => {
    const r = await fetch(base + path, {
      method,
      headers: {
        'content-type': 'application/json',
        ...(ip ? { 'x-forwarded-for': ip } : {}),
        ...(session ? { cookie: session.cookie, 'x-csrf-token': session.csrf } : {}),
      },
      body: body && JSON.stringify(body),
    });
    const type = r.headers.get('content-type') ?? '';
    return { status: r.status, headers: r.headers, body: type.startsWith('application/json') ? await r.json() : Buffer.from(await r.arrayBuffer()), cookie: r.headers.get('set-cookie')?.split(';')[0] };
  };
  return { app, call, inatCalls, photoCalls };
}

test('a visitor gets the reference photos of a species with only the public fields', async t => {
  const { app, call, inatCalls } = await setup(t);
  const r = await call('POST', '/species-photos', { body: { names: ['Oleria onega', 'Godyris zavaleta', 'Madeup thing'] } });
  assert.equal(r.status, 200, JSON.stringify(r.body));
  const e = r.body.species['Oleria onega'];
  assert.deepEqual(e.inat.photos, [{ id: 111, thumb: INAT(111), url: INAT(111).replace('small', 'large'), attribution: '(c) Ana, some rights reserved (CC BY-NC)', license: 'CC-BY-NC' }]);
  assert.deepEqual(e.inat.taxon, { id: 10, name: 'Oleria onega', rank: 'species', common: null, url: 'https://www.inaturalist.org/taxa/10' });
  // The specimens with a Photo_links photo: CAM_ID, Sex and the files, as the Wings Gallery publishes them.
  assert.deepEqual(e.specimens, [
    { cam: 'CAM000002', sex: 'male', dorsal: fileId('CAM000002d'), ventral: null },
    { cam: 'CAM000001', sex: 'female', dorsal: fileId('CAM000001d'), ventral: fileId('CAM000001v') },
  ]);
  const raw = JSON.stringify(r.body);
  for (const word of ['reviewer', 'someone@example.org', 'observerEmail', 'ana@example.org', 'internal', 'CAM000003', 'A Person', 'Collector'])
    assert.ok(!raw.includes(word), `${word} is not in a visitor's answer`);
  // A name the curated list knows goes to the background queue; a made-up one is answered from the cache alone.
  assert.equal(r.body.species['Godyris zavaleta'].pending, true);
  assert.equal(r.body.species['Madeup thing'].pending, false);
  await app.speciesPhotos.idle();
  assert.ok(inatCalls.some(u => u.includes('Godyris')), 'the known name was looked up');
  assert.ok(!inatCalls.some(u => u.includes('Madeup')), 'the made-up name was not');
});

test('case variants of a known name are that name: one cache entry, one lookup, never a job of their own', async t => {
  const { app, call, inatCalls } = await setup(t);
  // Cached under its exact spelling: variants are answered from it, iNaturalist is not asked.
  const cached = await call('POST', '/species-photos', { body: { names: ['Oleria Onega', 'Oleria oNega', 'OLERIA  ONEGA', 'oleria onega'] } });
  assert.equal(cached.status, 200);
  assert.deepEqual(Object.keys(cached.body.species), ['Oleria Onega', 'Oleria oNega', 'OLERIA ONEGA', 'oleria onega']);
  for (const e of Object.values(cached.body.species)) assert.deepEqual(e.inat.photos.map(p => p.id), [111]);
  await app.speciesPhotos.idle();
  assert.deepEqual(inatCalls, []);
  // Not cached: the variants share one job under the curated spelling.
  const r = await call('POST', '/species-photos', { body: { names: ['Godyris Zavaleta', 'godyris zavaleta', 'GODYRIS ZAVALETA'] } });
  assert.equal(r.status, 200);
  for (const e of Object.values(r.body.species)) assert.equal(e.pending, true);
  await app.speciesPhotos.idle();
  const searched = inatCalls.map(u => new URL(u).searchParams.get('q')).filter(Boolean);
  assert.deepEqual(searched, ['Godyris zavaleta', 'Godyris'], 'one job: the species, then its genus');
  // Asked again (now cached as nothing found): no new call.
  await call('POST', '/species-photos', { body: { names: ['GoDyRiS zavaleta'] } });
  await app.speciesPhotos.idle();
  assert.equal(inatCalls.length, 2);
});

test('a visitor asks for at most 20 names; the team keeps 300', async t => {
  const { call } = await setup(t);
  const names = Array.from({ length: ANON_MAX_NAMES + 1 }, (_, i) => `sin ${i}`);
  const r = await call('POST', '/species-photos', { body: { names } });
  assert.equal(r.status, 400);
  assert.equal(r.body.error.code, 'INVALID_NAMES');
  assert.equal((await call('POST', '/species-photos', { body: { names: names.slice(0, ANON_MAX_NAMES) } })).status, 200);
  // Each name at most 120 characters (a long one would only be echoed back).
  const long = await call('POST', '/species-photos', { body: { names: ['Oleria onega', 'x'.repeat(ANON_MAX_NAME_LENGTH + 1)] } });
  assert.equal(long.status, 400);
  assert.equal((await call('POST', '/species-photos', { body: { names: ['x'.repeat(ANON_MAX_NAME_LENGTH)] } })).status, 200);
  assert.equal((await call('POST', '/species-photos', { body: { names: [{ name: 'Oleria onega' }] } })).status, 400);
  const setupDone = await call('POST', '/auth/setup', { body: { token: 'public-reads-setup', username: 'boss', password: 'secret1', displayName: 'Boss' } });
  const session = { cookie: setupDone.cookie, csrf: setupDone.body.csrf };
  assert.equal((await call('POST', '/species-photos', { session, body: { names } })).status, 200);
});

test('the per-address limit: past it a visitor gets 429 with Retry-After; another address and the team are not held', async t => {
  const { call } = await setup(t, { anonLimits: { speciesPhotos: { limit: 3 }, refPhoto: { limit: 2 }, photo: { limit: 2 } } });
  for (let i = 0; i < 3; i++) assert.equal((await call('POST', '/species-photos', { ip: '203.0.113.5', body: { names: ['Oleria onega'] } })).status, 200);
  const over = await call('POST', '/species-photos', { ip: '203.0.113.5', body: { names: ['Oleria onega'] } });
  assert.equal(over.status, 429);
  assert.equal(over.body.error.code, 'RATE_LIMITED');
  assert.ok(Number(over.headers.get('retry-after')) > 0);
  assert.equal((await call('POST', '/species-photos', { ip: '198.51.100.7', body: { names: ['Oleria onega'] } })).status, 200);
  for (let i = 0; i < 2; i++) assert.equal((await call('GET', `/identify/ref-photo?u=${encodeURIComponent(INAT(111))}`, { ip: '203.0.113.5' })).status, 200);
  assert.equal((await call('GET', `/identify/ref-photo?u=${encodeURIComponent(INAT(111))}`, { ip: '203.0.113.5' })).status, 429);
  for (let i = 0; i < 2; i++) assert.equal((await call('GET', `/photo/${fileId('CAM000001d')}?w=400`, { ip: '203.0.113.5' })).status, 200);
  assert.equal((await call('GET', `/photo/${fileId('CAM000001d')}?w=400`, { ip: '203.0.113.5' })).status, 429);
  // Spoofed first X-Forwarded-For entries do not make another address: the last one (the proxy's) is the key.
  const spoofed = await call('POST', '/species-photos', { ip: '192.0.2.77, 203.0.113.5', body: { names: ['Oleria onega'] } });
  assert.equal(spoofed.status, 429);
  const setupDone = await call('POST', '/auth/setup', { ip: '203.0.113.5', body: { token: 'public-reads-setup', username: 'boss', password: 'secret1', displayName: 'Boss' } });
  const session = { cookie: setupDone.cookie, csrf: setupDone.body.csrf };
  assert.equal((await call('POST', '/species-photos', { ip: '203.0.113.5', session, body: { names: ['Oleria onega'] } })).status, 200);
});

test("a visitor gets the iNaturalist photos the server lists, not any URL", async t => {
  const { call, photoCalls } = await setup(t);
  // In the species cache (small and large) and in the curated list.
  for (const u of [INAT(111), INAT(111).replace('small', 'large'), INAT(222).replace('small', 'large')]) {
    const r = await call('GET', `/identify/ref-photo?u=${encodeURIComponent(u)}`);
    assert.equal(r.status, 200, u);
    assert.equal(r.headers.get('content-type'), 'image/jpeg');
  }
  // Another iNaturalist photo, and a URL of another host: 404, nothing fetched.
  const before = photoCalls.length;
  assert.equal((await call('GET', `/identify/ref-photo?u=${encodeURIComponent(INAT(999))}`)).status, 404);
  assert.equal((await call('GET', `/identify/ref-photo?u=${encodeURIComponent('https://example.org/photos/111/small.jpg')}`)).status, 404);
  assert.equal(photoCalls.length, before);
});

test('raw camera files are not public: 404 for a visitor, and a specimen shows its JPEG or is left out', async t => {
  const { call } = await setup(t);
  assert.equal((await call('GET', `/photo/${fileId('CAM000004d.CR2')}?w=400`)).status, 404);
  assert.equal((await call('GET', `/photo/${fileId('CAM000005d.orf')}?w=400`)).status, 404);
  assert.equal((await call('GET', `/photo/${fileId('CAM000004d_1')}?w=400`)).status, 200);
  const r = await call('POST', '/species-photos', { body: { names: ['Godyris zavaleta'] } });
  assert.deepEqual(r.body.species['Godyris zavaleta'].specimens, [{ cam: 'CAM000004', sex: 'female', dorsal: fileId('CAM000004d_1'), ventral: null }]);
  assert.ok(!JSON.stringify(r.body).includes('CAM000005'));
});

test("one Drive budget for every visitor's photos not cached yet; cached ones and the team's are not held", async t => {
  const { call } = await setup(t, { anonLimits: { drive: { perSecond: 0.001, burst: 2 } } });
  assert.equal((await call('GET', `/photo/${fileId('CAM000001d')}?w=400`, { ip: '203.0.113.1' })).status, 200);
  assert.equal((await call('GET', `/photo/${fileId('CAM000001v')}?w=400`, { ip: '203.0.113.2' })).status, 200);
  const busy = await call('GET', `/photo/${fileId('CAM000002d')}?w=400`, { ip: '203.0.113.3' });
  assert.equal(busy.status, 503);
  assert.equal(busy.body.error.code, 'BUSY');
  assert.ok(Number(busy.headers.get('retry-after')) > 0);
  // Cached: no Drive fetch, so no budget.
  assert.equal((await call('GET', `/photo/${fileId('CAM000001d')}?w=400`, { ip: '203.0.113.4' })).status, 200);
  const setupDone = await call('POST', '/auth/setup', { body: { token: 'public-reads-setup', username: 'boss', password: 'secret1', displayName: 'Boss' } });
  const session = { cookie: setupDone.cookie, csrf: setupDone.body.csrf };
  assert.equal((await call('GET', `/photo/${fileId('CAM000002d')}?w=400`, { session })).status, 200);
});

test("a visitor gets the specimen photos Photo_links lists, not the archive's others nor a card's", async t => {
  const { call } = await setup(t);
  const ok = await call('GET', `/photo/${fileId('CAM000001d')}?w=1600`);
  assert.equal(ok.status, 200);
  assert.equal(ok.headers.get('content-security-policy'), 'sandbox; default-src none');
  assert.equal((await call('GET', `/photo/${fileId('CAM000003d')}?w=400`)).status, 404);
  assert.equal((await call('GET', `/photo/${fileId('CAM000001d')}?w=400&card=abc`)).status, 404);
  assert.equal((await call('GET', `/photo/${fileId('CAM000001d')}?w=999`)).status, 400);
  assert.equal((await call('GET', `/photo/${'2'.repeat(31)}?w=400`)).status, 404);
});

test('writes and the team-only reads still need a sign-in', async t => {
  const { call } = await setup(t);
  for (const [method, path, body] of [
    ['POST', '/identify/contributions', { id: 'x', photo: 'AAAA' }],
    ['GET', '/identify/contributions/mine'],
    ['GET', '/identify/contributions/consent'],
    ['DELETE', '/identify/contributions/x'],
    ['GET', '/frozen-stock'],
    ['GET', '/identify/field-photos'],
    ['GET', '/sex-check'],
    ['POST', '/ai/identify-photo', { photo: 'AAAA' }],
    ['GET', '/monitoring/wikiloc-data'],
  ]) {
    const r = await call(method, path, { body });
    assert.equal(r.status, 401, `${method} ${path}`);
    assert.equal(r.body.error.code, 'AUTH_REQUIRED');
  }
  // The models' files, the sex model's among them, stay behind the sign-in (/prueba-offline/models/).
  const models = await call('GET', '/../prueba-offline/models/sex_v2s_ft_full_fp16.onnx');
  assert.equal(models.status, 401);
});

test('the address limiter counts per address in fixed windows', () => {
  let now = 0;
  const limiter = new AddressLimiter({ limit: 2, windowMs: 1000, now: () => now });
  const req = ip => ({ socket: { remoteAddress: '127.0.0.1' }, headers: { 'x-forwarded-for': `10.9.9.9, ${ip}` } });
  // The last entry (the local proxy's), whatever a visitor wrote before it.
  assert.equal(clientAddress(req('203.0.113.9')), '203.0.113.9');
  assert.equal(clientAddress({ socket: { remoteAddress: '::1' }, headers: {} }), '::1');
  // Not behind the proxy: the forwarded header is not trusted.
  assert.equal(clientAddress({ socket: { remoteAddress: '198.51.100.1' }, headers: { 'x-forwarded-for': '1.2.3.4' } }), '198.51.100.1');
  limiter.check(req('a'));
  limiter.check(req('a'));
  assert.throws(() => limiter.check(req('a')), { code: 'RATE_LIMITED', status: 429 });
  limiter.check(req('b'));
  now = 1000;
  limiter.check(req('a'));
});

test('the address limiter sweeps at most once a minute and keeps at most maxEntries addresses, the oldest going first', () => {
  let now = 0;
  const limiter = new AddressLimiter({ limit: 5, windowMs: 1000, maxEntries: 3, now: () => now });
  const req = ip => ({ socket: { remoteAddress: ip }, headers: {} });
  for (const ip of ['a', 'b', 'c', 'd', 'e']) {
    limiter.check(req(ip));
    now += 1;
  }
  assert.deepEqual([...limiter.counts.keys()], ['c', 'd', 'e']);
  // Expired windows stay until the next sweep, a minute after the last.
  now = 30_000;
  limiter.check(req('f'));
  assert.equal(limiter.counts.size, 3);
  now = 60_000;
  limiter.check(req('g'));
  assert.deepEqual([...limiter.counts.keys()], ['g']);
});

test('the Drive budget: a burst, then its rate', () => {
  let now = 0;
  const budget = new DriveBudget({ perSecond: 2, burst: 3, now: () => now });
  for (let i = 0; i < 3; i++) budget.take();
  assert.throws(() => budget.take(), { code: 'BUSY', status: 503 });
  now = 500;
  budget.take();
  assert.throws(() => budget.take(), { code: 'BUSY' });
});

test("the head's names are read again after a failed or empty read and when a new release replaces the file", async t => {
  const dir = mkdtempSync(join(tmpdir(), 'public-head-'));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  mkdirSync(join(dir, 'offline'));
  let now = 0;
  const lists = createPublicLists(null, { webRoot: dir, identifierDir: dir, retryMs: 1000, now: () => now });
  // No head yet: nothing known.
  assert.equal((await lists.canonicalNames())('Mechanitis polymnia'), null);
  writeFileSync(join(dir, 'offline', 'head_6fa6ab78.json'), JSON.stringify({ species: ['Mechanitis polymnia'] }));
  assert.equal((await lists.canonicalNames())('mechanitis POLYMNIA'), null, 'not before retryMs');
  now = 1000;
  assert.equal((await lists.canonicalNames())('mechanitis POLYMNIA'), 'Mechanitis polymnia');
  // A new release: the file changes, its names are read again.
  writeFileSync(join(dir, 'offline', 'head_6fa6ab78.json'), JSON.stringify({ species: ['Mechanitis polymnia', 'Oleria baizana'] }));
  now = 2000;
  assert.equal((await lists.canonicalNames())('oleria baizana'), 'Oleria baizana');
  // Requests during a read share it.
  now = 3000;
  const [a, b] = await Promise.all([lists.canonicalNames(), lists.canonicalNames()]);
  assert.equal(a('Oleria baizana'), 'Oleria baizana');
  assert.equal(b('Oleria baizana'), 'Oleria baizana');
});
