import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { existsSync, mkdtempSync, readdirSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createApp } from '../server/index.mjs';
import { addClutchEvent, clutchDay, clutchEvents, ecuadorDay, removeClutchEvent } from '../server/clutches.mjs';
import { createClutchPhotos, readJpeg } from '../server/clutch-photos.mjs';
import { setPasswordHashCost } from '../server/auth.mjs';

setPasswordHashCost(16);

const ana = { id: 'u-ana', username: 'ana', displayName: 'Ana Pérez', role: 'editor' };
const bob = { id: 'u-bob', username: 'bob', displayName: 'Bob Díaz', role: 'editor' };

/**
 * A JPEG's segments as a phone writes them (no image data worth decoding: the
 * server only reads the segments): JFIF, EXIF with the turn and a GPS block,
 * a comment, the frame with its size, the scan.
 */
function jpeg({ width = 2560, height = 1920, orientation = 1, gps = true, comment = 'secret comment' } = {}) {
  const seg = (marker, payload) => {
    const head = Buffer.alloc(4);
    head.writeUInt16BE(marker, 0);
    head.writeUInt16BE(payload.length + 2, 2);
    return Buffer.concat([head, payload]);
  };
  const jfif = seg(0xffe0, Buffer.from([0x4a, 0x46, 0x49, 0x46, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0]));
  // TIFF big-endian: one IFD with Orientation (and a GPS pointer), then the "GPS" bytes.
  const entries = gps ? 2 : 1;
  const ifd = Buffer.alloc(2 + entries * 12 + 4);
  ifd.writeUInt16BE(entries, 0);
  ifd.writeUInt16BE(0x0112, 2);
  ifd.writeUInt16BE(3, 4);
  ifd.writeUInt32BE(1, 6);
  ifd.writeUInt16BE(orientation, 10);
  if (gps) {
    ifd.writeUInt16BE(0x8825, 14);
    ifd.writeUInt16BE(4, 16);
    ifd.writeUInt32BE(1, 18);
    ifd.writeUInt32BE(8 + ifd.length, 22);
  }
  const tiff = Buffer.concat([Buffer.from([0x4d, 0x4d, 0, 0x2a, 0, 0, 0, 8]), ifd, Buffer.from(gps ? 'GPS -0.95,-77.86' : '')]);
  const exif = seg(0xffe1, Buffer.concat([Buffer.from('Exif\0\0', 'latin1'), tiff]));
  const com = seg(0xfffe, Buffer.from(comment));
  const sof = Buffer.alloc(15);
  sof[0] = 8;
  sof.writeUInt16BE(height, 1);
  sof.writeUInt16BE(width, 3);
  sof[5] = 3;
  const frame = seg(0xffc0, sof);
  const scan = seg(0xffda, Buffer.from([3, 1, 0, 2, 0x11, 3, 0x11, 0, 0x3f, 0]));
  const data = Buffer.from(Array.from({ length: 4000 }, (_, i) => (i * 7) % 255));
  return Buffer.concat([Buffer.from([0xff, 0xd8]), jfif, exif, com, frame, scan, data, Buffer.from([0xff, 0xd9])]);
}

async function fixture(dir) {
  const sheets = new LocalSheets({
    Insectary_stocks: [
      { row: 10, values: { 'CLUTCH NUMBER': 1012, SPECIES: 'Mechanitis lysimnia', 'NUMBER OF LARVAE': { formula: '=3+5' } } },
      { row: 11, values: { 'CLUTCH NUMBER': 1013, SPECIES: 'Mechanitis lysimnia' } },
    ],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_stocks'] });
  for (const u of [ana, bob])
    store.db
      .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES(?,?,?,?,'s','h',1,'2026-01-01')")
      .run(u.id, u.username, u.displayName, u.role);
  // No Pillow (a path that does not exist): photos too large or turned are refused.
  const photos = createClutchPhotos(store, { dir, python: join(dir ?? tmpdir(), 'no-python') });
  return { store, photos, row: r => store.getRecordBySheetRow('Insectary_stocks', r) };
}

/** Sends thumbnail + photo in chunks, as the phone does, then stores it. */
function upload(photos, user, { thumb, full, chunk = 1500, ...meta }) {
  const id = randomUUID();
  const all = Buffer.concat([thumb, full]);
  for (let at = 0; at < all.length; at += chunk) photos.receiveChunk(id, String(at), all.subarray(at, at + chunk), user);
  return { id, done: photos.finish({ requestId: id, thumbBytes: thumb.length, totalBytes: all.length, ...meta }, user) };
}

test('JPEG segments: size, EXIF turn, and every metadata segment left out', () => {
  const info = readJpeg(jpeg({ width: 3000, height: 4000, orientation: 6 }));
  assert.deepEqual([info.width, info.height, info.orientation], [3000, 4000, 6]);
  const clean = info.clean.toString('latin1');
  assert.ok(!clean.includes('Exif') && !clean.includes('GPS') && !clean.includes('secret comment'), 'EXIF, GPS and comments gone');
  assert.ok(clean.includes('JFIF'), 'the JFIF header stays');
  assert.deepEqual(readJpeg(info.clean).orientation, 1);
  assert.equal(readJpeg(Buffer.from('not a jpeg')), null);
  assert.equal(readJpeg(Buffer.from([0x89, 0x50, 0x4e, 0x47])), null);
});

test('photos: uploaded in chunks that resume, stored without metadata, linked to an event or the day, listed with the clutch', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'clutch-photos-'));
  try {
    const { store, photos, row } = await fixture(dir);
    const c1012 = row(10).id;
    const event = addClutchEvent(store, { requestId: randomUUID(), recordId: c1012, stage: 'larva', kind: 'disappeared', count: 1 }, ana).event;

    // A chunk at the wrong offset is answered with what has arrived: the phone sends on from there.
    const id = randomUUID();
    const thumb = jpeg({ width: 480, height: 360, gps: false });
    const full = jpeg();
    const all = Buffer.concat([thumb, full]);
    assert.deepEqual(photos.receiveChunk(id, '0', all.subarray(0, 3000), ana), { received: 3000 });
    assert.deepEqual(photos.receiveChunk(id, '0', all.subarray(0, 3000), ana), { received: 3000, status: 409 }, 'the same chunk again is not appended twice');
    assert.deepEqual(photos.uploadState(id, ana), { received: 3000 });
    assert.throws(() => photos.receiveChunk(id, '3000', all.subarray(3000, 4000), bob), { code: 'FORBIDDEN' }, "someone else's upload");
    await assert.rejects(photos.finish({ requestId: id, recordId: c1012, thumbBytes: thumb.length, totalBytes: all.length }, ana), { code: 'UPLOAD_INCOMPLETE' });
    photos.receiveChunk(id, '3000', all.subarray(3000), ana);
    const saved = await photos.finish({ requestId: id, recordId: c1012, eventId: event.id, note: '  missing   larva ', thumbBytes: thumb.length, totalBytes: all.length }, ana);
    assert.equal(saved.duplicate, false);
    const p = saved.photo;
    assert.deepEqual(
      [p.clutch, p.day, p.eventId, p.note, p.name, p.width, p.height],
      ['1012', ecuadorDay(), event.id, 'missing larva', 'Ana Pérez', 2560, 1920],
    );
    // Stored once: the same request again gives the same photo; its chunks are done.
    const again = await photos.finish({ requestId: id, recordId: c1012, thumbBytes: thumb.length }, ana);
    assert.deepEqual([again.duplicate, again.photo.id], [true, p.id]);
    assert.deepEqual(photos.receiveChunk(id, '0', all.subarray(0, 10), ana), { received: null, done: true });
    assert.equal(photos.uploadState(id, ana).photo.id, p.id);

    // The files: the photo and its thumbnail, without EXIF, GPS or comments; nothing left in uploads.
    const stored = photos.file(p.id).toString('latin1');
    assert.ok(!stored.includes('GPS') && !stored.includes('Exif') && !stored.includes('secret comment'));
    assert.equal(p.bytes, photos.file(p.id).length);
    assert.equal(readJpeg(photos.file(p.id, 'thumb')).width, 480);
    assert.deepEqual(readdirSync(join(dir, 'uploads')), []);
    assert.ok(existsSync(join(dir, ecuadorDay().slice(0, 7), `${p.id}.jpg`)));
    assert.ok(existsSync(join(dir, ecuadorDay().slice(0, 7), `${p.id}.thumb.jpg`)));

    // A second photo of the same day, of the day only; listed with the clutch's timeline and the day.
    const day = await upload(photos, bob, { thumb, full: jpeg({ width: 1920, height: 2560 }), recordId: c1012, note: 'Plant with fungi' }).done;
    assert.equal(day.photo.eventId, null);
    assert.deepEqual(clutchEvents(store, { recordId: c1012 }).photos.map(x => [x.id, x.eventId]), [
      [p.id, event.id],
      [day.photo.id, null],
    ]);
    assert.equal(clutchDay(store).photos.length, 2);
    assert.equal(photos.list({ recordId: row(11).id }).photos.length, 0);

    // Linked again (the event, the caption) by its author; not by someone else.
    assert.equal(photos.update(day.photo.id, { eventId: event.id }, bob).photo.eventId, event.id);
    assert.throws(() => photos.update(day.photo.id, { note: 'x' }, ana), { code: 'FORBIDDEN' });
    assert.throws(() => photos.update(day.photo.id, { eventId: 'nope' }, bob), { code: 'EVENT_NOT_FOUND' });
    // An event taken back: its photos stay, as the day's.
    removeClutchEvent(store, event.id, ana);
    assert.deepEqual(clutchEvents(store, { recordId: c1012 }).photos.map(x => x.eventId), [null, null]);
    // Removed: the record and its files.
    photos.remove(p.id, ana);
    assert.equal(photos.file(p.id), null);
    assert.ok(!existsSync(join(dir, ecuadorDay().slice(0, 7), `${p.id}.jpg`)));
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});

test('photos refused: not a JPEG, larger than 2560 px or turned without Pillow, another clutch’s event, a day to come', async () => {
  const { store, photos, row } = await fixture(null);
  const [c1012, c1013] = [row(10).id, row(11).id];
  const thumb = jpeg({ width: 400, height: 300 });
  await assert.rejects(upload(photos, ana, { thumb, full: Buffer.alloc(5000, 7), recordId: c1012 }).done, { code: 'INVALID_PHOTO' });
  await assert.rejects(upload(photos, ana, { thumb, full: jpeg({ width: 4080, height: 3060 }), recordId: c1012 }).done, { code: 'PHOTO_TOO_LARGE' });
  await assert.rejects(upload(photos, ana, { thumb, full: jpeg({ orientation: 6 }), recordId: c1012 }).done, { code: 'PHOTO_TOO_LARGE' });
  const other = addClutchEvent(store, { requestId: randomUUID(), recordId: c1013, stage: 'larva', kind: 'died', count: 1 }, ana).event;
  await assert.rejects(upload(photos, ana, { thumb, full: jpeg(), recordId: c1012, eventId: other.id }).done, { code: 'EVENT_NOT_FOUND' });
  await assert.rejects(upload(photos, ana, { thumb, full: jpeg(), recordId: c1012, day: '2999-01-01' }).done, { code: 'INVALID_DAY' });
  await assert.rejects(upload(photos, ana, { thumb, full: jpeg(), recordId: 'nope' }).done, { code: 'RECORD_NOT_FOUND' });
  await assert.rejects(photos.finish({ requestId: randomUUID(), recordId: c1012 }, ana), { code: 'UPLOAD_NOT_FOUND' });
  assert.throws(() => photos.receiveChunk('../../etc', '0', Buffer.from([1]), ana), { code: 'INVALID_UPLOAD' });
  // An earlier day is fine (yesterday's photos sent today); without a thumbnail the photo stands for it.
  const id = randomUUID();
  const full = jpeg();
  photos.receiveChunk(id, '0', full, ana);
  const saved = await photos.finish({ requestId: id, recordId: c1012, day: '2026-10-01', thumbBytes: 0 }, ana);
  assert.equal(saved.photo.day, '2026-10-01');
  assert.equal(photos.file(saved.photo.id, 'thumb').length, saved.photo.bytes);
});

test('HTTP: a photo uploaded in chunks with the session, served to people signed in only', async t => {
  const app = await createApp(
    { databasePath: ':memory:', localMode: true, secureCookies: false, syncIntervalMs: 0, setupToken: 'photo-setup' },
    { seed: { Insectary_stocks: [{ row: 10, values: { 'CLUTCH NUMBER': 1012, SPECIES: 'Mechanitis lysimnia' } }] } },
  );
  await app.ready;
  const address = await app.listen(0);
  t.after(() => app.close());
  const base = `http://127.0.0.1:${address.port}/ithomiini/api`;
  let cookie = '';
  let csrf = '';
  const call = async (path, method = 'GET', body, headers = {}) => {
    const raw = Buffer.isBuffer(body);
    const response = await fetch(base + path, {
      method,
      headers: { ...(body && !raw ? { 'content-type': 'application/json' } : {}), ...(cookie ? { cookie, 'x-csrf-token': csrf } : {}), ...headers },
      body: raw ? body : body ? JSON.stringify(body) : undefined,
    });
    if (response.headers.get('set-cookie')) cookie = response.headers.get('set-cookie').split(';')[0];
    const type = response.headers.get('content-type') ?? '';
    const data = type.includes('json') ? await response.json() : Buffer.from(await response.arrayBuffer());
    if (data?.csrf) csrf = data.csrf;
    return { status: response.status, data, type };
  };
  await call('/auth/setup', 'POST', { token: 'photo-setup', username: 'photo_admin', password: 'photo-admin-123', requestId: randomUUID() });
  const { data: state } = await call('/clutches/state');
  assert.ok(state);
  const recordId = (await call('/records?module=Insectary_stocks')).data.records[0].id;
  const id = randomUUID();
  const thumb = jpeg({ width: 400, height: 300 });
  const all = Buffer.concat([thumb, jpeg()]);
  assert.deepEqual((await call(`/clutches/photo-uploads/${id}?offset=0`, 'PUT', all.subarray(0, 5000), { 'content-type': 'application/octet-stream' })).data, {
    received: 5000,
    done: false,
  });
  // Resumed after a dropped connection: the server says where it stands.
  const wrong = await call(`/clutches/photo-uploads/${id}?offset=0`, 'PUT', all.subarray(0, 5000), { 'content-type': 'application/octet-stream' });
  assert.deepEqual([wrong.status, wrong.data.received], [409, 5000]);
  assert.equal((await call(`/clutches/photo-uploads/${id}`)).data.received, 5000);
  await call(`/clutches/photo-uploads/${id}?offset=5000`, 'PUT', all.subarray(5000), { 'content-type': 'application/octet-stream' });
  const saved = await call('/clutches/photos', 'POST', { requestId: id, recordId, thumbBytes: thumb.length, totalBytes: all.length, note: 'Sick larva' });
  assert.equal(saved.status, 201);
  const photo = saved.data.photo;
  const thumbFile = await call(`/clutches/photos/${photo.id}?size=thumb`);
  assert.deepEqual([thumbFile.status, thumbFile.type, thumbFile.data.length], [200, 'image/jpeg', photo.thumbBytes]);
  assert.equal((await call(`/clutches/photos?recordId=${encodeURIComponent(recordId)}`)).data.photos[0].note, 'Sick larva');
  assert.equal((await call('/clutches/day')).data.photos.length, 1);
  cookie = '';
  assert.equal((await call(`/clutches/photos/${photo.id}`)).status, 401);
});
