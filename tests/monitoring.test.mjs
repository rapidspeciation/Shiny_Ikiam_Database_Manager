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
  rematchTracks,
  applyRematch,
  linkCapture,
  storeReviewedWalk,
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

// ---------------------------------------------------- matching again, and the doubts

const serialOf = iso => Math.round((Date.parse(`${iso}T00:00:00Z`) - Date.UTC(1899, 11, 30)) / 864e5);
/** Part of Monitoreo 8/9/2025 (AA) as it was on the map: each point one row too early. */
async function misPaired() {
  const at = (row, time, SPECIES, Sex, extra = {}) => {
    const [h, m] = time.split(':').map(Number);
    return {
      row,
      values: {
        Collection_date: serialOf('2025-09-08'),
        Collection_time: (h * 60 + m) / 1440,
        Collector: 'AA - Alex Arias',
        Purpose: 'Monitoring',
        Collection_location: 'Ikiam',
        FieldMark_ID: 'NA',
        SPECIES,
        Sex,
        ...extra,
      },
    };
  };
  const sheets = new LocalSheets({
    Collection_data: [
      at(2, '9:12', 'Hypothyris euclea', 'female'),
      at(3, '9:14', 'Eresia eunice', 'female'),
      at(4, '9:15', 'Methona confusa', 'male'),
      at(5, '9:16', 'Mechanitis polymnia', 'male', { Subspecies_Form: 'proceriformis' }),
      at(6, '9:12', 'Hypothyris euclea', 'male', { Collection_date: serialOf('2025-09-10') }),
    ],
  });
  const s = new Store({ localMode: true }, { sheets });
  await s.sync({ sheets: ['Collection_data'] });
  const id = row => s.db.prepare("SELECT id FROM records WHERE sheet='Collection_data' AND row_num=?").get(row).id;
  const point = (text, minutes, row) => ({ lat: -0.95, lon: -77.86 - minutes / 1e5, text, minutes, row, recordId: id(row) });
  const { track } = saveTrack(
    s,
    {
      requestId: 'request-0500',
      date: '2025-09-08',
      collector: 'AA - Alex Arias',
      name: 'Monitoreo 8/9/2025',
      track: [],
      captures: [point('9:14 sol female 0.3m', 554, 2), point('9:15 sol 0.4m', 555, 3), point('Pol p male 9:16 sol 2m', 556, 4)],
    },
    editor,
  );
  return { s, id, track };
}

test('matching the stored walks again lists what would change, and applies it only when asked', async () => {
  const { s, id, track } = await misPaired();
  const first = rematchTracks(s, reviewer);
  assert.deepEqual(
    first.changes.map(c => [c.text, c.before.row, c.after.row]),
    [
      ['9:14 sol female 0.3m', 2, 3],
      ['9:15 sol 0.4m', 3, 4],
      ['Pol p male 9:16 sol 2m', 4, 5],
    ],
  );
  // Nothing is written until the changes are applied.
  assert.deepEqual(listTracks(s)[0].captures.map(c => c.row), [2, 3, 4]);
  assert.ok(first.doubts.every(d => d.changed));
  assert.deepEqual(first.walks.map(w => [w.id, w.doubts]), [[track.id, 3]]);
  // A change that is no longer the same (another from) is skipped.
  assert.deepEqual(applyRematch(s, { changes: [{ ...first.changes[0], from: id(4) }] }), { applied: 0, skipped: 1 });
  assert.deepEqual(applyRematch(s, { changes: first.changes }), { applied: 3, skipped: 0 });
  const [after] = listTracks(s);
  assert.deepEqual(
    after.captures.map(c => [c.row, c.species]),
    [
      [3, 'Eresia eunice'],
      [4, 'Methona confusa'],
      [5, 'Mechanitis polymnia'],
    ],
  );
  assert.equal(rematchTracks(s).changes.length, 0);
  s.close();
});

test('a pairing chosen by a person is kept when matching again, and "none" leaves the point without a row', async () => {
  const { s, id, track } = await misPaired();
  linkCapture(s, track.id, { index: 0, recordId: id(2) });
  linkCapture(s, track.id, { index: 1, recordId: null });
  assert.throws(() => linkCapture(s, track.id, { index: 2, recordId: id(6) }), { code: 'INVALID_LINK' });
  const { changes } = rematchTracks(s);
  assert.deepEqual(
    changes.map(c => c.text),
    ['Pol p male 9:16 sol 2m'],
  );
  applyRematch(s, { changes });
  const [t] = listTracks(s);
  assert.deepEqual(
    t.captures.map(c => [c.row, c.link]),
    [
      [2, 'manual'],
      [null, 'none'],
      [5, null],
    ],
  );
  // Reviewed again (the same walk imported again): the pairings made by hand stay.
  saveTrack(
    s,
    {
      requestId: 'request-0501',
      date: t.date,
      collector: t.collector,
      name: t.name,
      track: [],
      captures: t.captures.map(c => ({ ...c, row: null, recordId: null, link: null })),
    },
    editor,
  );
  assert.deepEqual(
    listTracks(s)[0].captures.map(c => c.link),
    ['manual', 'none', null],
  );
  s.close();
});

test('a waiting walk whose points do not pair surely is listed to be paired by hand, then stored with those rows', async () => {
  const { s, id } = await misPaired();
  const { walk } = await saveWalk(
    s,
    {
      ...walkBody(),
      date: '2025-09-10',
      collector: 'AA - Alex Arias',
      waypoints: [
        { lat: -0.95, lon: -77.86, text: 'Marip 1', photos: [] },
        { lat: -0.95, lon: -77.87, text: 'Planta', photos: [] },
      ],
    },
    editor,
  );
  const listed = rematchTracks(s).doubts.filter(d => d.walkId === walk.id);
  assert.deepEqual(
    listed.map(d => [d.text, d.confidence]),
    [
      ['Marip 1', 'order'],
      ['Planta', 'none'],
    ],
  );
  // Its GPS position, for the section it lies in.
  assert.deepEqual([listed[1].lat, listed[1].lon], [-0.95, -77.87]);
  // The rows it could be, although the matcher gave that one to "Marip 1".
  assert.deepEqual(
    listed[1].candidates.map(r => r.row),
    [6],
  );
  assert.throws(() => storeReviewedWalk(s, walk.id, { links: [[id(6)]] }, editor), { code: 'INVALID_LINK' });
  const { track } = storeReviewedWalk(s, walk.id, { links: [[id(6)], []] }, editor);
  assert.deepEqual(
    track.captures.map(c => [c.row, c.link]),
    [
      [6, 'manual'],
      [null, 'none'],
    ],
  );
  assert.equal(listWalks(s)[0].status, 'imported');
  assert.equal(rematchTracks(s).doubts.filter(d => d.walkId === walk.id).length, 0);
  s.close();
});

test('the pairing board stores a point whose recapture row is being added, and rows without a point', async () => {
  const { s, id } = await misPaired();
  const { walk } = await saveWalk(
    s,
    {
      ...walkBody(),
      date: '2025-09-10',
      collector: 'AA - Alex Arias',
      waypoints: [
        { lat: -0.95, lon: -77.86, text: 'Hypothyris euclea macho 9:12', photos: [] },
        { lat: -0.95, lon: -77.87, text: 'Planta', photos: [] },
      ],
    },
    editor,
  );
  // Row 6 has no point: it cannot also be the first point's row, nor be of another day.
  assert.throws(() => storeReviewedWalk(s, walk.id, { links: [[id(6)], []], rowsWithoutPoint: [id(6)] }, editor), {
    code: 'INVALID_LINK',
  });
  assert.throws(() => storeReviewedWalk(s, walk.id, { links: [[], []], rowsWithoutPoint: [id(2)] }, editor), {
    code: 'INVALID_LINK',
  });
  storeReviewedWalk(s, walk.id, { links: [[], []], rowsWithoutPoint: [id(6)] }, editor);
  assert.deepEqual(listTracks(s)[0].rowsWithoutPoint, [id(6)]);
  assert.deepEqual(listTracks(s)[0].captures.map(c => c.link), ['none', 'none']);
  // Decided again on the board of the imported walk: 'new' points wait without a link (no earlier
  // "none" kept), and are paired with their row once it is in the sheet (row 6 stands for it here).
  storeReviewedWalk(s, walk.id, { links: ['new', 'new'], rowsWithoutPoint: [] }, editor);
  const [track] = listTracks(s);
  assert.deepEqual(track.rowsWithoutPoint, []);
  assert.deepEqual(
    track.captures.map(c => [c.row, c.link ?? null, !!c.doubt]),
    [
      [6, null, false],
      [null, null, false],
    ],
  );
  assert.equal(listWalks(s)[0].status, 'imported');
  s.close();
});

// ---------------------------------------------------- Pasar al mapa with doubtful points

/**
 * A waiting walk stored as the bulk "Pasar al mapa" does (frontend capturesToStore):
 * two identical 9:14 notes tie for two rows, "Planta" has none, the rest are sure.
 */
async function storedWithDoubts() {
  const lib = await import('../frontend/src/lib/monitoring.ts');
  const at = (row, time, SPECIES, Sex) => {
    const [h, m] = time.split(':').map(Number);
    const values = { Collection_date: serialOf('2025-09-08'), Collection_time: (h * 60 + m) / 1440, Collector: 'AA - Alex Arias' };
    return { row, values: { ...values, Purpose: 'Monitoring', Collection_location: 'Ikiam', FieldMark_ID: 'NA', SPECIES, Sex } };
  };
  const sheets = new LocalSheets({
    Collection_data: [
      at(2, '9:14', 'Hypothyris euclea', 'female'),
      at(3, '9:14', 'Eresia eunice', 'female'),
      at(4, '9:15', 'Methona confusa', 'male'),
      at(5, '9:16', 'Mechanitis polymnia', 'male'),
    ],
  });
  const s = new Store({ localMode: true }, { sheets });
  await s.sync({ sheets: ['Collection_data'] });
  const rows = s.db
    .prepare("SELECT id,row_num,values_json FROM records WHERE sheet='Collection_data' AND row_num>1")
    .all()
    .map(r => ({ id: r.id, row: r.row_num, version: 0, observed: true, values: JSON.parse(r.values_json), formulas: [] }));
  const id = row => rows.find(r => r.row === row).id;
  const taxa = lib.taxaFrom(rows);
  const notes = ['9:14 sol female 0.3m', '9:14 sol female 1m', '9:15 sol 0.4m', 'Pol p male 9:16 sol 2m', 'Planta'];
  const points = notes.map((text, i) =>
    lib.locateCapture({ lat: -0.95, lon: -77.86 - i / 1e4, ele: null, text, time: null, photos: [`${100 + i}`] }, taxa, taxa),
  );
  const match = lib.matchWalk(rows, '2025-09-08', 'AA - Alex Arias', points);
  const captures = lib.capturesToStore(points, match.matches);
  const { track } = saveTrack(
    s,
    { requestId: 'request-0600', date: match.date, collector: 'AA - Alex Arias', name: 'Monitoreo 8/9/2025', track: [], captures },
    editor,
  );
  return { s, id, track, match };
}

test('Pasar al mapa stores a walk with doubtful points: sure ones linked, doubtful ones without a row, listed in Dudas', async () => {
  const { s, id, track, match } = await storedWithDoubts();
  assert.deepEqual(
    match.matches.map(m => m.confidence),
    ['tie', 'tie', 'sure', 'sure', 'none'],
  );
  // Read again: the doubtful points stay without a row (the tie is not settled by reading).
  const [read] = listTracks(s);
  assert.deepEqual(
    read.captures.map(c => [c.text, c.row, !!c.doubt]),
    [
      ['9:14 sol female 0.3m', null, true],
      ['9:14 sol female 1m', null, true],
      ['9:15 sol 0.4m', 4, false],
      ['Pol p male 9:16 sol 2m', 5, false],
      ['Planta', null, true],
    ],
  );
  const found = rematchTracks(s);
  // The banner does not pair them: they are doubts for a person.
  assert.equal(found.changes.length, 0);
  const doubts = found.doubts.filter(d => d.trackId === track.id);
  assert.deepEqual(
    doubts.map(d => [d.text, d.confidence, d.pending, d.photos]),
    [
      ['9:14 sol female 0.3m', 'tie', true, ['100']],
      ['9:14 sol female 1m', 'tie', true, ['101']],
      ['Planta', 'none', true, ['104']],
    ],
  );
  // Any free row of the day can be picked, even for a note with nothing to match.
  assert.deepEqual(
    doubts[2].candidates.map(r => r.row),
    [2, 3],
  );
  assert.deepEqual(found.walks.map(w => [w.id, w.doubts]), [[track.id, 3]]);
  // Picking a row links the point, and it leaves Dudas; "No es ninguna" too.
  linkCapture(s, track.id, { index: 4, recordId: null });
  assert.deepEqual(
    rematchTracks(s).doubts.map(d => d.text),
    ['9:14 sol female 0.3m', '9:14 sol female 1m'],
  );
  linkCapture(s, track.id, { index: 0, recordId: id(3) });
  // The other 9:14 note now fits the row left surely: linked on reading, no longer a doubt.
  const [after] = listTracks(s);
  assert.deepEqual(
    after.captures.map(c => [c.row, c.link, !!c.doubt]),
    [
      [3, 'manual', false],
      [2, null, false],
      [4, null, false],
      [5, null, false],
      [null, 'none', false],
    ],
  );
  assert.deepEqual(rematchTracks(s), { changes: [], doubts: [], walks: [] });
  s.close();
});

test('a point stored as doubtful is linked on reading only when its note agrees with the row', async () => {
  const { s } = await storedWithDoubts();
  // A female Mechanitis polymnia at 9:16, whose row is a male: paired, but the sex disagrees.
  const walk = (requestId, lat, doubt) => ({
    requestId,
    date: '2025-09-08',
    collector: 'AA - Alex Arias',
    name: requestId,
    track: [],
    captures: [{ lat, lon: -77.9, text: 'Mechanitis polymnia female 9:16 sol', minutes: 556, sex: 'female', doubt }],
  });
  saveTrack(s, walk('request-0601', -0.951, true), editor);
  saveTrack(s, walk('request-0602', -0.952, false), editor);
  const rowOf = name => listTracks(s).find(t => t.name === name).captures[0].row;
  assert.equal(rowOf('request-0601'), null);
  assert.equal(rowOf('request-0602'), 5);
  s.close();
});
