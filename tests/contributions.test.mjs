import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash, randomUUID } from 'node:crypto';
import { existsSync, mkdtempSync, readFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { Store } from '../server/store.mjs';
import { createApp } from '../server/index.mjs';
import { CONSENT_VERSION, LIMITS, MAX_BYTES, MIN_FREE_BYTES, PHOTO_MAX, POLICY, createContributions } from '../server/contributions.mjs';
import { readJpeg } from '../server/clutch-photos.mjs';

const ana = { id: 'u-ana', username: 'ana', role: 'editor' };
const bob = { id: 'u-bob', username: 'bob', role: 'observer' };
const admin = { id: 'u-admin', username: 'root', role: 'admin' };

/** A JPEG's segments as a phone camera writes them: JFIF, EXIF with a GPS block, a comment, the frame, the scan. */
function jpeg({ width = 1600, height = 1200, orientation = 1, seed = 7 } = {}) {
  const seg = (marker, payload) => {
    const head = Buffer.alloc(4);
    head.writeUInt16BE(marker, 0);
    head.writeUInt16BE(payload.length + 2, 2);
    return Buffer.concat([head, payload]);
  };
  const jfif = seg(0xffe0, Buffer.from([0x4a, 0x46, 0x49, 0x46, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0]));
  const ifd = Buffer.alloc(2 + 12 + 4);
  ifd.writeUInt16BE(1, 0);
  ifd.writeUInt16BE(0x0112, 2);
  ifd.writeUInt16BE(3, 4);
  ifd.writeUInt32BE(1, 6);
  ifd.writeUInt16BE(orientation, 10);
  const tiff = Buffer.concat([Buffer.from([0x4d, 0x4d, 0, 0x2a, 0, 0, 0, 8]), ifd, Buffer.from('GPS -0.951234,-77.861234')]);
  const exif = seg(0xffe1, Buffer.concat([Buffer.from('Exif\0\0', 'latin1'), tiff]));
  const com = seg(0xfffe, Buffer.from('secret comment'));
  const sof = Buffer.alloc(15);
  sof[0] = 8;
  sof.writeUInt16BE(height, 1);
  sof.writeUInt16BE(width, 3);
  sof[5] = 3;
  const frame = seg(0xffc0, sof);
  const scan = seg(0xffda, Buffer.from([3, 1, 0, 2, 0x11, 3, 0x11, 0, 0x3f, 0]));
  const data = Buffer.from(Array.from({ length: 3000 }, (_, i) => (i * seed) % 255));
  return Buffer.concat([Buffer.from([0xff, 0xd8]), jfif, exif, com, frame, scan, data, Buffer.from([0xff, 0xd9])]);
}
const sha = buf => createHash('sha256').update(buf).digest('hex');

/** What the phone's outbox sends for one item. */
function item({ id = randomUUID(), photo = jpeg(), thumb = jpeg({ width: 224, height: 224, seed: 3 }), ...rest } = {}) {
  return {
    id,
    photo: photo.toString('base64'),
    thumb: thumb ? thumb.toString('base64') : undefined,
    sha256: sha(photo),
    createdAt: '2026-11-11T10:00:00.000Z',
    takenAt: '2026-11-10T08:31:00',
    labels: { sex: 'female', photoKind: 'hand', species: { name: 'Mechanitis  polymnia', rank: 'species', confirmed: true } },
    prediction: { source: 'online', head: 'head_6fa6ab78', species: [{ name: 'Mechanitis polymnia', p: 0.93 }] },
    gps: { lat: -0.951234, lon: -77.861234, acc: 12.4 },
    consent: { version: CONSENT_VERSION },
    ...rest,
  };
}

function fixture(options = {}) {
  const store = new Store({ localMode: true }, { sheets: null });
  const clock = { t: Date.parse('2026-11-11T12:00:00Z') };
  const contrib = createContributions(store, { now: () => clock.t, ...options });
  return { store, contrib, clock, count: () => store.db.prepare('SELECT count(*) n FROM contributions').get().n };
}
/** The error a call throws, as { status, code, headers }. */
function caught(fn) {
  try {
    fn();
  } catch (e) {
    return { status: e.status, code: e.code, headers: e.headers };
  }
  assert.fail('expected an error');
}

test('the defaults: team only, 0.01° location, the day kept, sex for any species; 30 in 10 min and 300 a day; 4 MB; 20 GB and 5 GB', () => {
  assert.deepEqual([...POLICY.roles].sort(), ['admin', 'editor', 'observer', 'reviewer']);
  assert.equal(POLICY.gpsDecimals, 2);
  assert.equal(POLICY.keepTakenDay, true);
  assert.equal(POLICY.sexForAnySpecies, true);
  assert.deepEqual(
    LIMITS.map(l => [l.count, l.windowMs]),
    [
      [30, 600_000],
      [300, 86_400_000],
    ],
  );
  assert.equal(PHOTO_MAX, 4 * 1024 * 1024);
  assert.equal(MAX_BYTES, 20 * 1024 ** 3);
  assert.equal(MIN_FREE_BYTES, 5 * 1024 ** 3);
});

test('accepted: stored without its metadata, location rounded, the day kept, status received', () => {
  const dir = mkdtempSync(join(tmpdir(), 'contrib-'));
  try {
    const { contrib, store } = fixture({ dir });
    const body = item();
    const out = contrib.accept(body, ana);
    assert.equal(out.status, 201);
    const c = out.body.contribution;
    assert.equal(c.id, body.id);
    assert.equal(c.status, 'received');
    assert.deepEqual(c.species, { name: 'Mechanitis polymnia', rank: 'species', confirmed: true, freeText: false });
    assert.equal(c.sex, 'female');
    assert.equal(c.photoKind, 'hand');
    assert.equal(c.takenAt, '2026-11-10');
    assert.deepEqual(c.gps, { lat: -0.95, lon: -77.86, acc: 12 });
    assert.deepEqual([c.width, c.height], [1600, 1200]);
    const row = store.db.prepare('SELECT * FROM contributions WHERE id = ?').get(body.id);
    assert.equal(row.user_id, ana.id);
    assert.equal(row.licence, 'training-grant+CC-BY-4.0');
    assert.equal(row.consent_version, CONSENT_VERSION);
    assert.equal(JSON.parse(row.prediction_json).head, 'head_6fa6ab78');
    // Files under <yyyy-mm>/<id>: the photo without EXIF/GPS or comment, still a JPEG of the same size.
    const file = join(dir, '2026-11', `${body.id}.jpg`);
    const stored = readFileSync(file);
    assert.ok(!stored.includes('GPS') && !stored.includes('secret comment') && !stored.includes('Exif'));
    assert.deepEqual([readJpeg(stored).width, readJpeg(stored).height], [1600, 1200]);
    assert.ok(existsSync(join(dir, '2026-11', `${body.id}.thumb.jpg`)));
    assert.equal(contrib.file(body.id).length, stored.length);
    // Without location, date or thumbnail; an observer may send; a sex label without a species is taken.
    const bare = contrib.accept(item({ gps: undefined, takenAt: undefined, thumb: null, labels: { sex: 'male' } }), bob).body.contribution;
    assert.deepEqual([bare.gps, bare.takenAt, bare.species, bare.sex], [null, null, null, 'male']);
    assert.equal(contrib.file(bare.id, 'thumb').length, contrib.file(bare.id).length);
    assert.deepEqual(
      contrib.mine(ana).contributions.map(x => x.id),
      [body.id],
    );
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});

test('the same id is idempotent; other bytes under it are a conflict; the same photo under a new id is not stored twice', () => {
  const { contrib, count } = fixture();
  const body = item();
  assert.equal(contrib.accept(body, ana).status, 201);
  const again = contrib.accept(body, ana);
  assert.deepEqual([again.status, again.body.already, again.body.contribution.id], [200, true, body.id]);
  const other = jpeg({ seed: 11 });
  assert.equal(caught(() => contrib.accept({ ...body, photo: other.toString('base64'), sha256: sha(other) }, ana)).status, 409);
  // Another person under the same id: a conflict too (nothing of the first item is told).
  assert.equal(caught(() => contrib.accept(body, bob)).status, 409);
  const copy = contrib.accept({ ...body, id: randomUUID() }, ana);
  assert.deepEqual([copy.status, copy.body.already, copy.body.id], [200, true, body.id]);
  assert.equal(count(), 1);
  // Another person's same photo is their own item.
  assert.equal(contrib.accept({ ...body, id: randomUUID() }, bob).status, 201);
  assert.equal(count(), 2);
});

test('rate limit per person: 429 with Retry-After; repeats do not count; another person is not held up', () => {
  const { contrib, clock } = fixture({ limits: [{ count: 3, windowMs: 600_000 }] });
  const first = item({ photo: jpeg({ seed: 2 }) });
  contrib.accept(first, ana);
  clock.t += 60_000;
  contrib.accept(item({ photo: jpeg({ seed: 3 }) }), ana);
  contrib.accept(item({ photo: jpeg({ seed: 4 }) }), ana);
  // The same item again is answered, not counted.
  assert.equal(contrib.accept(first, ana).status, 200);
  const held = caught(() => contrib.accept(item({ photo: jpeg({ seed: 5 }) }), ana));
  assert.deepEqual([held.status, held.code, held.headers['retry-after']], [429, 'RATE_LIMITED', '540']);
  assert.equal(contrib.accept(item({ photo: jpeg({ seed: 5 }) }), bob).status, 201);
  clock.t += 540_000;
  assert.equal(contrib.accept(item({ photo: jpeg({ seed: 5 }) }), ana).status, 201);
});

test('the default limits: the 31st in 10 minutes is refused', () => {
  const { contrib } = fixture();
  for (let k = 0; k < 30; k++) contrib.accept(item({ photo: jpeg({ seed: 20 + k }) }), ana);
  assert.equal(caught(() => contrib.accept(item({ photo: jpeg({ seed: 60 }) }), ana)).status, 429);
});

test('size and shape: over 4 MB (413), over 1600 px or turned (422), not a JPEG or a wrong sha256 (400), no label or consent (422)', () => {
  const { contrib, count } = fixture();
  const big = Buffer.alloc(PHOTO_MAX + 1, 1);
  assert.deepEqual(caught(() => contrib.accept({ ...item(), photo: big.toString('base64'), sha256: sha(big) }, ana)), {
    status: 413,
    code: 'PHOTO_TOO_LARGE',
    headers: undefined,
  });
  assert.equal(caught(() => contrib.accept(item({ thumb: Buffer.alloc(300 * 1024, 1) }), ana)).status, 413);
  assert.equal(caught(() => contrib.accept(item({ photo: jpeg({ width: 2000, height: 1500 }) }), ana)).code, 'PHOTO_TOO_BIG');
  assert.equal(caught(() => contrib.accept(item({ photo: jpeg({ orientation: 6 }) }), ana)).status, 422);
  assert.equal(caught(() => contrib.accept(item({ thumb: jpeg({ width: 1200, height: 900 }) }), ana)).status, 422);
  const png = Buffer.from('89504e470d0a1a0a0000', 'hex');
  assert.equal(caught(() => contrib.accept({ ...item(), photo: png.toString('base64'), sha256: sha(png) }, ana)).code, 'INVALID_PHOTO');
  assert.equal(caught(() => contrib.accept({ ...item(), sha256: sha(Buffer.from('x')) }, ana)).code, 'SHA_MISMATCH');
  assert.equal(caught(() => contrib.accept({ ...item(), photo: '!!!!' }, ana)).code, 'INVALID_PHOTO');
  assert.equal(caught(() => contrib.accept(item({ id: 'not-a-uuid' }), ana)).code, 'INVALID_ID');
  assert.equal(caught(() => contrib.accept(item({ labels: { sex: null, species: null } }), ana)).code, 'NO_LABEL');
  assert.equal(caught(() => contrib.accept(item({ labels: { sex: 'hermaphrodite' } }), ana)).code, 'INVALID_LABELS');
  assert.equal(caught(() => contrib.accept(item({ labels: { species: { name: '<script>' } } }), ana)).code, 'INVALID_LABELS');
  assert.equal(caught(() => contrib.accept(item({ consent: undefined }), ana)).code, 'CONSENT_REQUIRED');
  assert.equal(caught(() => contrib.accept(item({ consent: 'v0' }), ana)).status, 422);
  assert.equal(caught(() => contrib.accept(item({ gps: { lat: 200, lon: 0 } }), ana)).code, 'INVALID_GPS');
  assert.equal(caught(() => contrib.accept(item({ gps: { lat: null, lon: null } }), ana)).code, 'INVALID_GPS');
  assert.equal(caught(() => contrib.accept(item(), { id: 'u-x', role: 'guest' })).status, 403);
  assert.equal(caught(() => contrib.accept(item(), null)).status, 401);
  assert.equal(count(), 0);
  // The optional day taken never refuses an item: EXIF's form is read, a reset clock or a day ahead is dropped.
  const day = takenAt => contrib.accept(item({ photo: jpeg({ seed: 40 + count() }), takenAt }), ana).body.contribution.takenAt;
  assert.deepEqual([day('2026:11:10 08:31:00'), day('1970-01-01T00:00:00'), day('2030-01-01'), day('ayer')], ['2026-11-10', null, null, null]);
  // A typed name with a number or a question mark («sp. 2», «cf.?»).
  const named = contrib.accept(item({ photo: jpeg({ seed: 90 }), labels: { species: { name: 'Napeogenes sp. 2?', freeText: true } } }), ana).body.contribution;
  assert.deepEqual(named.species, { name: 'Napeogenes sp. 2?', rank: 'species', confirmed: false, freeText: true });
});

test('low disk or a full store: 507 with Retry-After, and nothing written', () => {
  const dir = mkdtempSync(join(tmpdir(), 'contrib-'));
  try {
    let free = 4 * 1024 ** 3;
    const { contrib, count } = fixture({ dir, freeBytes: () => free });
    const body = item();
    const full = caught(() => contrib.accept(body, ana));
    assert.deepEqual([full.status, full.code, full.headers['retry-after']], [507, 'STORE_FULL', '3600']);
    assert.ok(!existsSync(join(dir, '2026-11', `${body.id}.jpg`)));
    // A disk whose free space cannot be read counts as full.
    const broken = fixture({ freeBytes: () => {
      throw new Error('EIO');
    } });
    assert.equal(caught(() => broken.contrib.accept(item(), ana)).status, 507);
    free = 50 * 1024 ** 3;
    assert.equal(contrib.accept(body, ana).status, 201);
    assert.equal(count(), 1);
    // The store's own cap.
    const capped = fixture({ maxBytes: 9000 });
    assert.equal(capped.contrib.accept(item(), ana).status, 201);
    assert.equal(caught(() => capped.contrib.accept(item({ photo: jpeg({ seed: 9 }) }), ana)).status, 507);
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});

test('deleted: files and row gone, a tombstone kept; the same id or photo is refused (410); owner or admin only', () => {
  const dir = mkdtempSync(join(tmpdir(), 'contrib-'));
  try {
    const { contrib, store, count } = fixture({ dir });
    const body = item();
    contrib.accept(body, ana);
    assert.equal(caught(() => contrib.remove(body.id, bob)).status, 403);
    assert.deepEqual(contrib.remove(body.id.toUpperCase(), ana), { deleted: body.id, already: false, removed: true });
    assert.equal(count(), 0);
    assert.ok(!existsSync(join(dir, '2026-11', `${body.id}.jpg`)) && !existsSync(join(dir, '2026-11', `${body.id}.thumb.jpg`)));
    assert.equal(contrib.file(body.id), null);
    const stone = store.db.prepare('SELECT * FROM contribution_tombstones WHERE id = ?').get(body.id);
    assert.deepEqual([stone.user_id, stone.sha256, stone.deleted_by], [ana.id, body.sha256, ana.id]);
    assert.ok(stone.deleted_at);
    // A stale phone sends it again under its id: refused.
    assert.deepEqual(Object.values(caught(() => contrib.accept(body, ana))).slice(0, 2), [410, 'CONTRIBUTION_DELETED']);
    assert.deepEqual(contrib.remove(body.id, ana), { deleted: body.id, already: true, removed: false });
    assert.deepEqual(contrib.mine(ana).contributions, []);
    // «Deshacer» and Enviar again with the right label: the same photo under a new id is a new item.
    const fixed = contrib.accept({ ...body, id: randomUUID(), labels: { sex: 'male' } }, ana);
    assert.deepEqual([fixed.status, fixed.body.contribution.sex], [201, 'male']);
    contrib.remove(fixed.body.contribution.id, ana);
    // An admin deletes anyone's; the tombstone names the owner.
    const bobs = item({ photo: jpeg({ seed: 13 }) });
    contrib.accept(bobs, bob);
    contrib.remove(bobs.id, admin);
    assert.deepEqual(Object.values(store.db.prepare('SELECT user_id, deleted_by FROM contribution_tombstones WHERE id = ?').get(bobs.id)), [bob.id, admin.id]);
    // Deleted before it arrived (a send still on its way): refused when it comes.
    const late = item({ photo: jpeg({ seed: 17 }) });
    // No photo held under that id: said, so the phone does not take it for a deletion of a photo.
    assert.deepEqual(contrib.remove(late.id, ana), { deleted: late.id, already: false, removed: false });
    assert.equal(caught(() => contrib.accept(late, ana)).status, 410);
    assert.equal(caught(() => contrib.remove('nope', ana)).code, 'INVALID_ID');
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});

test('deleted items still count against the rate limit', () => {
  const { contrib } = fixture({ limits: [{ count: 2, windowMs: 600_000 }] });
  const a = item({ photo: jpeg({ seed: 2 }) });
  contrib.accept(a, ana);
  contrib.remove(a.id, ana);
  contrib.accept(item({ photo: jpeg({ seed: 3 }) }), ana);
  assert.equal(caught(() => contrib.accept(item({ photo: jpeg({ seed: 4 }) }), ana)).status, 429);
});

test('HTTP: signed in only; sent, sent again, listed, limited (Retry-After), too large, full, deleted and refused again', async t => {
  let free = Infinity;
  const app = await createApp(
    { databasePath: ':memory:', localMode: true, secureCookies: false, syncIntervalMs: 0, setupToken: 'contrib-setup' },
    { seed: {}, contributions: { limits: [{ count: 3, windowMs: 600_000 }], freeBytes: () => free } },
  );
  await app.ready;
  const address = await app.listen(0);
  t.after(() => app.close());
  const base = `http://127.0.0.1:${address.port}/ithomiini/api`;
  let cookie = '';
  let csrf = '';
  const call = async (path, method = 'GET', body) => {
    const response = await fetch(base + path, {
      method,
      headers: { ...(body ? { 'content-type': 'application/json' } : {}), ...(cookie ? { cookie, 'x-csrf-token': csrf } : {}) },
      body: body ? JSON.stringify(body) : undefined,
    });
    if (response.headers.get('set-cookie')) cookie = response.headers.get('set-cookie').split(';')[0];
    const data = await response.json();
    if (data?.csrf) csrf = data.csrf;
    return { status: response.status, data, retryAfter: response.headers.get('retry-after') };
  };
  const body = item();
  const anonymous = await call('/identify/contributions', 'POST', body);
  assert.deepEqual([anonymous.status, anonymous.data.error.code], [401, 'AUTH_REQUIRED']);
  assert.equal((await call('/identify/contributions/mine')).status, 401);
  assert.equal((await call(`/identify/contributions/${body.id}`, 'DELETE')).status, 401);
  const setup = await call('/auth/setup', 'POST', { token: 'contrib-setup', username: 'contrib_admin', password: 'contrib-123', requestId: randomUUID() });
  const sent = await call('/identify/contributions', 'POST', body);
  assert.equal(setup.status, 201);
  assert.deepEqual([sent.status, sent.data.already, sent.data.contribution.status], [201, false, 'received']);
  const again = await call('/identify/contributions', 'POST', body);
  assert.deepEqual([again.status, again.data.already], [200, true]);
  assert.deepEqual(
    (await call('/identify/contributions/mine')).data.contributions.map(c => c.id),
    [body.id],
  );
  const big = Buffer.alloc(PHOTO_MAX + 10, 1);
  assert.equal((await call('/identify/contributions', 'POST', { ...item(), photo: big.toString('base64'), sha256: sha(big) })).status, 413);
  free = 1024 ** 3;
  const full = await call('/identify/contributions', 'POST', item({ photo: jpeg({ seed: 5 }) }));
  assert.deepEqual([full.status, full.data.error.code, full.retryAfter], [507, 'STORE_FULL', '3600']);
  free = Infinity;
  await call('/identify/contributions', 'POST', item({ photo: jpeg({ seed: 16 }) }));
  await call('/identify/contributions', 'POST', item({ photo: jpeg({ seed: 17 }) }));
  const limited = await call('/identify/contributions', 'POST', item({ photo: jpeg({ seed: 18 }) }));
  assert.equal(limited.status, 429);
  assert.ok(Number(limited.retryAfter) > 500 && Number(limited.retryAfter) <= 600);
  const deleted = await call(`/identify/contributions/${body.id}`, 'DELETE');
  assert.deepEqual([deleted.status, deleted.data.deleted], [200, body.id]);
  const stale = await call('/identify/contributions', 'POST', body);
  assert.deepEqual([stale.status, stale.data.error.code], [410, 'CONTRIBUTION_DELETED']);
  assert.equal((await call('/identify/contributions/mine')).data.contributions.length, 2);
});

test('backups copy the contributed photos and drop the copies of deleted ones', async () => {
  const { DatabaseSync } = await import('node:sqlite');
  const { execFileSync } = await import('node:child_process');
  const { mkdirSync, writeFileSync } = await import('node:fs');
  const root = mkdtempSync(join(tmpdir(), 'contrib-backup-'));
  try {
    const databasePath = join(root, 'database.sqlite');
    const db = new DatabaseSync(databasePath);
    db.exec('CREATE TABLE contribution_tombstones(id TEXT PRIMARY KEY, user_id TEXT, sha256 TEXT, received_at TEXT, deleted_at TEXT NOT NULL, deleted_by TEXT)');
    db.close();
    const photos = join(root, 'contributions', '2026-11');
    mkdirSync(photos, { recursive: true });
    const [a, b] = [randomUUID(), randomUUID()];
    for (const name of [`${a}.jpg`, `${a}.thumb.jpg`, `${b}.jpg`]) writeFileSync(join(photos, name), jpeg());
    const backups = join(root, 'backups');
    const run = () =>
      execFileSync(process.execPath, [new URL('../scripts/backup.mjs', import.meta.url).pathname], {
        env: { PATH: process.env.PATH, DATABASE_PATH: databasePath, BACKUP_DIR: backups },
        encoding: 'utf8',
      });
    assert.match(run(), /Contributed photos copied: 3 new file/);
    const copy = join(backups, 'contributions', '2026-11');
    assert.ok(existsSync(join(copy, `${a}.jpg`)) && existsSync(join(copy, `${b}.jpg`)));
    // Deleted in the app (its files gone, a tombstone): the next backup drops its copies.
    rmSync(join(photos, `${a}.jpg`));
    rmSync(join(photos, `${a}.thumb.jpg`));
    const again = new DatabaseSync(databasePath);
    again.prepare("INSERT INTO contribution_tombstones(id, deleted_at) VALUES(?, '2026-11-11')").run(a);
    again.close();
    assert.match(run(), /removed from the copy: 2 file/);
    assert.ok(!existsSync(join(copy, `${a}.jpg`)) && !existsSync(join(copy, `${a}.thumb.jpg`)) && existsSync(join(copy, `${b}.jpg`)));
  } finally {
    rmSync(root, { recursive: true, force: true });
  }
});
