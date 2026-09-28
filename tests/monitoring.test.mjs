import test from 'node:test';
import assert from 'node:assert/strict';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import {
  addProfile,
  attachWalkPhotos,
  claimJob,
  finishJob,
  listJobs,
  queueLink,
  queueSync,
  trailUrl,
  deleteTrack,
  getPhoto,
  listTracks,
  listWalks,
  saveTrack,
  saveWalk,
  reopenWalk,
} from '../server/monitoring.mjs';

const editor = { id: 'e1', username: 'editor', role: 'editor' };
const other = { id: 'e2', username: 'other', role: 'editor' };
const reviewer = { id: 'r1', username: 'reviewer', role: 'reviewer' };
const body = requestId => ({
  requestId,
  date: '2026-09-26',
  collector: 'FCH - Franz Chandi',
  name: 'Monitoreo ithomidos FCH 26 SEP 2026',
  track: [
    [-0.950528, -77.869962, 601.4, '2026-09-26T14:11:55Z'],
    [-0.950562, -77.869942, null, null],
  ],
  captures: [
    { lat: -0.950925, lon: -77.869495, text: 'M1 Hyposcada illinissa ida hembra 9:20 0.5m NO id: B69', seq: 1, species: 'Hyposcada illinissa', sex: 'female', minutes: 560, height: 0.5, markId: 'B69', section: 4 },
  ],
});
const store = () => new Store({ localMode: true }, { sheets: new LocalSheets({}) });

test('monitoring tracks are stored once and listed with their captures', () => {
  const s = store();
  const first = saveTrack(s, body('request-0001'), editor);
  assert.equal(first.duplicate, false);
  assert.equal(first.track.captures[0].markId, 'B69');
  assert.equal(first.track.captures[0].recapture, false);
  // The same file again (another request) returns the stored copy.
  const again = saveTrack(s, body('request-0002'), editor);
  assert.equal(again.duplicate, true);
  assert.equal(again.track.id, first.track.id);
  assert.equal(listTracks(s).length, 1);
});

test('monitoring tracks reject malformed points', () => {
  const s = store();
  const bad = body('request-0003');
  bad.track[0][0] = 123;
  assert.throws(() => saveTrack(s, bad, editor), { code: 'INVALID_TRACK' });
  assert.throws(() => saveTrack(s, { ...body('request-0004'), date: '26/9/2026' }, editor), { code: 'INVALID_TRACK' });
  const badCapture = body('request-0005');
  badCapture.captures[0].lat = 'north';
  assert.throws(() => saveTrack(s, badCapture, editor), { code: 'INVALID_TRACK' });
});

test('only the uploader, a reviewer or an admin removes a track', () => {
  const s = store();
  const { track } = saveTrack(s, body('request-0006'), editor);
  assert.throws(() => deleteTrack(s, track.id, other), { code: 'FORBIDDEN' });
  deleteTrack(s, track.id, reviewer);
  assert.equal(listTracks(s).length, 0);
  assert.throws(() => deleteTrack(s, track.id, editor), { code: 'TRACK_NOT_FOUND' });
});

const walkBody = () => ({
  url: 'https://es.wikiloc.com/rutas-senderismo/monitoreo-ithomidos-sendero-ikiam-fch-14-mayo-2025-213523060',
  name: 'Monitoreo ithomidos sendero Ikiam FCH 14 mayo 2025',
  date: '2025-05-14',
  track: [[-0.951408, -77.864418, 600], [-0.951263, -77.863767, null]],
  waypoints: [
    {
      lat: -0.952753,
      lon: -77.864755,
      ele: 593,
      text: 'M1 Oleria tigilla macho 9:30 1m NC',
      photos: ['https://s2.wklcdn.com/image_458/13756119/213523061/129692483Master.jpg'],
    },
  ],
});
const fakeImage = calls => async url => {
  calls.push(url);
  return { type: 'image/jpeg', data: Buffer.from('jpeg') };
};

test('a Wikiloc walk is stored once with its photos, and refreshed when sent again', async () => {
  const s = store();
  const calls = [];
  const first = await saveWalk(s, walkBody(), editor, { fetchImage: fakeImage(calls) });
  assert.equal(first.walk.status, 'waiting');
  assert.equal(first.walk.wikilocId, '213523060');
  assert.deepEqual(first.walk.waypoints[0].photos, ['129692483']);
  assert.equal(getPhoto(s, '129692483').mime_type, 'image/jpeg');
  const again = await saveWalk(s, walkBody(), editor, { fetchImage: fakeImage(calls) });
  assert.equal(again.updated, true);
  assert.equal(calls.length, 1, 'a stored photo is not downloaded again');
  assert.equal(listWalks(s).length, 1);
});

test('a Wikiloc walk only accepts Wikiloc pages and wklcdn photos', async () => {
  const s = store();
  await assert.rejects(saveWalk(s, { ...walkBody(), url: 'https://example.com/123456789' }, editor), { code: 'INVALID_WALK' });
  const body = walkBody();
  body.waypoints[0].photos = ['https://evil.example/image_458/1/2/3.jpg'];
  await assert.rejects(saveWalk(s, body, editor, { fetchImage: fakeImage([]) }), { code: 'INVALID_WALK' });
});

test('a failed photo download is reported without losing the walk', async () => {
  const s = store();
  const saved = await saveWalk(s, walkBody(), editor, {
    fetchImage: async () => {
      throw new Error('photo 404');
    },
  });
  assert.equal(saved.failed.length, 1);
  assert.deepEqual(saved.walk.waypoints[0].photos, []);
});

test('reviewing a Wikiloc walk marks it imported; its photos can join a GPX track of the same walk', async () => {
  const s = store();
  const { walk } = await saveWalk(s, walkBody(), editor, { fetchImage: fakeImage([]) });
  const gpx = body('request-0100');
  gpx.captures[0].text = 'M1 Oleria tigilla macho 9:30 1m NC';
  const { track } = saveTrack(s, gpx, editor);
  const joined = attachWalkPhotos(s, track.id, walk.id);
  assert.equal(joined.matched, 1);
  assert.deepEqual(joined.track.captures[0].photos, ['129692483']);
  assert.equal(listWalks(s)[0].status, 'imported');
  const s2 = store();
  const { walk: w2 } = await saveWalk(s2, walkBody(), editor, { fetchImage: fakeImage([]) });
  const saved = saveTrack(s2, { ...body('request-0101'), wikilocWalkId: w2.id, captures: [{ ...body('x').captures[0], photos: ['129692483'] }] }, editor);
  assert.equal(saved.track.wikiloc.id, '213523060');
  assert.equal(listWalks(s2)[0].trackId, saved.track.id);
});

test('shared or pasted text yields the Wikiloc trail link', () => {
  assert.deepEqual(trailUrl('Mira mi ruta en Wikiloc: https://es.wikiloc.com/rutas-senderismo/monitoreo-ithomidos-fch-26-sep-2026-288058472 #wikiloc'), {
    url: 'https://es.wikiloc.com/rutas-senderismo/monitoreo-ithomidos-fch-26-sep-2026-288058472',
    wikilocId: '288058472',
  });
  assert.equal(trailUrl('https://example.com/ruta-288058472'), null);
});

test('links and profile checks become jobs for the home computer, which claims and finishes them', async () => {
  const s = store();
  const worker = { id: 'w', username: 'wikiloc-worker', role: 'editor' };
  assert.throws(() => queueSync(s, editor), { code: 'NO_PROFILES' });
  addProfile(s, { url: 'https://es.wikiloc.com/wikiloc/user.do?id=13756119' }, editor);
  assert.throws(() => addProfile(s, { url: 'https://example.com' }, editor), { code: 'INVALID_PROFILE' });
  const link = queueLink(s, { text: 'https://es.wikiloc.com/rutas-senderismo/monitoreo-ithomidos-fch-26-sep-2026-288058472' }, editor);
  // The same link twice is one job.
  assert.equal(queueLink(s, { url: link.job.target }, editor).job.id, link.job.id);
  queueSync(s, editor);
  const first = claimJob(s).job;
  assert.equal(first.kind, 'trail');
  const second = claimJob(s).job;
  assert.equal(second.kind, 'profile');
  assert.equal(second.profile.wikilocUser, '13756119');
  assert.ok(Array.isArray(second.knownIds));
  assert.equal(claimJob(s).job, null);
  // A walk sent for a job belongs to whoever asked for it.
  const { walk } = await saveWalk(s, { ...walkBody(), jobId: first.id }, worker, { fetchImage: fakeImage([]) });
  assert.equal(walk.createdBy, 'editor');
  finishJob(s, first.id, { status: 'done', message: 'ok' });
  finishJob(s, second.id, { status: 'done', message: '1 nueva', profileName: 'Franz Chandi 1' });
  const { jobs, workerSeen } = listJobs(s);
  assert.deepEqual(jobs.map(j => j.status), ['done', 'done']);
  assert.ok(workerSeen);
});

test('a walk takes its collector from the followed profile of its author', async () => {
  const s = store();
  addProfile(s, { url: '11910166', collector: 'AA - Alex Arias' }, editor);
  const { walk } = await saveWalk(s, { ...walkBody(), author: '11910166', recorded: 'mayo 2025' }, editor, { fetchImage: fakeImage([]) });
  assert.equal(walk.collector, 'AA - Alex Arias');
  assert.equal(walk.recorded, 'mayo 2025');
  // Following again updates the pattern and collector instead of failing.
  const again = addProfile(s, { url: '11910166', pattern: 'monitor', collector: 'AA - Alex Arias' }, editor);
  assert.equal(again.profile.pattern, 'monitor');
});

test('an imported walk can be reviewed again: re-pasting its link says so, and importing it again replaces its track', async () => {
  const s = store();
  const { walk } = await saveWalk(s, walkBody(), editor, { fetchImage: fakeImage([]) });
  const first = saveTrack(s, { ...body('request-0200'), date: '2025-05-14', wikilocWalkId: walk.id }, editor);
  assert.equal(listWalks(s)[0].status, 'imported');
  // Pasting the link again: the screen learns the walk was imported and can reopen it.
  const pasted = queueLink(s, { text: walk.url }, editor);
  assert.equal(pasted.known, 'imported');
  assert.equal(pasted.walkId, walk.id);
  assert.equal(reopenWalk(s, walk.id).walk.status, 'waiting');
  // Reviewed and imported again: the same track, with the new captures.
  const again = saveTrack(
    s,
    { ...body('request-0201'), date: '2025-05-14', wikilocWalkId: walk.id, captures: [{ ...body('x').captures[0], markId: 'B70', row: 7 }] },
    editor,
  );
  assert.equal(again.track.id, first.track.id);
  assert.equal(again.track.captures[0].markId, 'B70');
  assert.equal(listTracks(s).length, 1);
  assert.equal(listWalks(s)[0].status, 'imported');
  // A replayed request changes nothing.
  assert.equal(saveTrack(s, { ...body('request-0201'), captures: [] }, editor).track.captures[0].markId, 'B70');
  assert.throws(() => reopenWalk(s, 'nope'), { code: 'WALK_NOT_FOUND' });
});

test('whoever brought the walk in may remove its track; the walk then waits for review again', async () => {
  const s = store();
  const { walk } = await saveWalk(s, walkBody(), editor, { fetchImage: fakeImage([]) });
  // Another editor imported it; the editor who asked for the walk may still remove it.
  const { track } = saveTrack(s, { ...body('request-0300'), wikilocWalkId: walk.id }, other);
  const third = { id: 'e3', username: 'third', role: 'editor' };
  assert.deepEqual(
    [editor, other, third, reviewer].map(u => listTracks(s, u)[0].canDelete),
    [true, true, false, true],
  );
  assert.throws(() => deleteTrack(s, track.id, third), { code: 'FORBIDDEN' });
  deleteTrack(s, track.id, editor);
  assert.equal(listTracks(s).length, 0);
  assert.deepEqual([listWalks(s)[0].status, listWalks(s)[0].trackId], ['waiting', null]);
});

test('stored captures follow their butterfly when sheet rows are removed', async () => {
  const EPOCH = Date.UTC(1899, 11, 30);
  const serial = iso => Math.round((Date.parse(`${iso}T00:00:00Z`) - EPOCH) / 864e5);
  const day = { Collection_date: serial('2026-09-26'), Collector: 'FCH - Franz Chandi', Purpose: 'Monitoring', Collection_location: 'Ikiam' };
  // Row 2 was removed from the sheet: B69 moved up from row 3 to row 2, and row 3 is now another butterfly.
  const sheets = new LocalSheets({
    Collection_data: [
      { row: 2, values: { ...day, FieldMark_ID: 'B69', SPECIES: 'Hyposcada illinissa', Sex: 'female', Collection_time: 560 / 1440 } },
      { row: 3, values: { ...day, FieldMark_ID: 'NA', SPECIES: 'Oleria tigilla', Sex: 'male', Collection_time: 600 / 1440 } },
    ],
  });
  const s = new Store({ localMode: true }, { sheets });
  await s.sync({ sheets: ['Collection_data'] });
  const stored = body('request-0400');
  const b69 = stored.captures[0];
  stored.captures = [
    { ...b69, row: 3 },
    // Its row was removed and nothing else matches it.
    { ...b69, text: 'M2 Oleria onega macho 9:50', markId: null, species: 'Oleria onega', sex: 'male', minutes: 590, row: 4 },
    // Stored when the walk was imported, before its row was saved.
    { ...b69, text: 'M3 Oleria tigilla macho 10:00', markId: null, species: 'Oleria tigilla', sex: 'male', minutes: 600, row: null },
  ];
  saveTrack(s, stored, editor);
  const [track] = listTracks(s);
  assert.deepEqual(
    track.captures.map(c => c.row),
    [2, null, 3],
  );
  assert.ok(track.captures[0].recordId);
  s.close();
});
