import test from 'node:test';
import { mkdtempSync, readFileSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
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
  deleteWalk,
  getPhoto,
  listTracks,
  listWalks,
  saveTrack,
  saveWalk,
  reopenWalk,
  rematchTracks,
  projectInfo,
  applyRematch,
  linkCapture,
  storeReviewedWalk,
  wikilocHealth,
  PROFILE_STALE_DAYS,
  dailySync,
  startDailySync,
  DAILY_SYNC_HOUR_EC,
} from '../server/monitoring.mjs';
import { retryable } from '../tools/wikiloc/lib.mjs';
import { createWalkIds } from '../server/walk-ids.mjs';
import { initPhotoTables } from '../server/photodata.mjs';

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
  // Wikiloc's «Share»: a short link to open-trail-link.do, kept as that (the browser follows it to the trail).
  const short = { url: 'https://www.wikiloc.com/wikiloc/open-trail-link.do?id=291256112', wikilocId: '291256112' };
  assert.deepEqual(trailUrl('¡Mira esta ruta de @Wikiloc! https://loc.wiki/t/291256112?wa=share&og=ZQMNF_k5EP0 (Monitoreo 14 octubre)'), short);
  assert.deepEqual(trailUrl('loc.wiki/t/291256112'), short);
  assert.deepEqual(trailUrl('https://www.wikiloc.com/wikiloc/open-trail-link.do?id=291256112&wa=share'), short);
  assert.deepEqual(trailUrl('https://es.wikiloc.com/wikiloc/view.do?id=291256112'), short);
  assert.equal(trailUrl('https://loc.wiki/t/12'), null);
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

test("a job Cloudflare's check stopped waits and is tried again, then fails saying why", t => {
  const s = store();
  const link = queueLink(s, { text: 'https://es.wikiloc.com/rutas-senderismo/monitoreo-14-octubre-291256112' }, editor).job;
  assert.equal(claimJob(s).job.id, link.id);
  const retried = finishJob(s, link.id, { status: 'retry', message: 'Wikiloc browser: TimeoutError: Cloudflare challenge did not clear' }).job;
  assert.equal(retried.status, 'queued');
  assert.equal(retried.attempts, 1);
  assert.match(retried.message, /se intenta de nuevo hacia las \d\d:\d\d \(intento 2 de 3\)/);
  assert.ok(Date.parse(retried.notBefore) - Date.now() > 4 * 60_000);
  // Not taken before its retry time.
  assert.equal(claimJob(s).job, null);
  // Pasted again while it waits: tried at once.
  queueLink(s, { url: link.target }, editor);
  assert.equal(claimJob(s).job.id, link.id);
  finishJob(s, link.id, { status: 'retry', message: 'Cloudflare challenge did not clear' });
  queueLink(s, { url: link.target }, editor);
  assert.equal(claimJob(s).job.id, link.id);
  // The last try: then it fails, saying why (in the app's log too, as its third try).
  const logged = [];
  t.mock.method(console, 'error', line => logged.push(line));
  const last = finishJob(s, link.id, { status: 'retry', message: 'Cloudflare challenge did not clear' }).job;
  assert.match(logged.join('\n'), /\(try 3\): Cloudflare challenge did not clear/);
  assert.equal(last.status, 'failed');
  assert.equal(last.attempts, 3);
  // Any other error fails at once.
  const other = queueLink(s, { text: 'https://es.wikiloc.com/rutas-senderismo/otra-291000002' }, editor).job;
  claimJob(s);
  assert.equal(finishJob(s, other.id, { status: 'failed', message: 'App answered 502' }).job.status, 'failed');
});

// The worker's errors as it reported them on the live server (tests/fixtures/wikiloc-job-errors.json).
const jobErrors = JSON.parse(readFileSync(new URL('./fixtures/wikiloc-job-errors.json', import.meta.url), 'utf8'));

test('a walk page that did not load in time is tried again like a Cloudflare check; the app\'s own errors fail at once', t => {
  for (const e of jobErrors.retry) assert.equal(retryable(new Error(e.message)), true, e.seen);
  for (const e of jobErrors.failed) assert.equal(retryable(new Error(e.message)), false, e.seen);
  // What the app then does with the worker's answer: queued again for later (10 Nov 2026: this one failed for good).
  const s = store();
  const job = queueLink(s, { text: 'https://es.wikiloc.com/rutas-senderismo/monitoreo-14-octubre-291256112' }, editor).job;
  claimJob(s);
  const timeout = new Error(jobErrors.retry[1].message);
  const out = finishJob(s, job.id, { status: retryable(timeout) ? 'retry' : 'failed', message: timeout.message }).job;
  assert.equal(out.status, 'queued');
  assert.equal(out.attempts, 1);
});

test("/health's wikiloc: failing after a failed import until one works, stale profiles, a silent worker; failures logged", t => {
  const s = store();
  const logged = [];
  t.mock.method(console, 'error', line => logged.push(line));
  const day = 864e5;
  assert.equal(wikilocHealth(s).status, 'unused');
  addProfile(s, { url: '13756119' }, editor);
  assert.equal(wikilocHealth(s).status, 'ok');
  // Followed a month ago and never looked at: stale.
  assert.equal(wikilocHealth(s, Date.now() + (PROFILE_STALE_DAYS + 1) * day).status, 'stale');
  // A link that fails: failing, and said in the app's log.
  const a = queueLink(s, { text: 'https://es.wikiloc.com/rutas-senderismo/monitoreo-14-octubre-291256112' }, editor).job;
  claimJob(s);
  finishJob(s, a.id, { status: 'failed', message: 'Wikiloc browser: TimeoutError: Cloudflare challenge did not clear' });
  const failing = wikilocHealth(s);
  assert.equal(failing.status, 'failing');
  assert.equal(failing.reason, 'failed');
  assert.equal(failing.failedSinceOk, 1);
  assert.equal(failing.lastOk, null);
  assert.match(logged.join('\n'), /Wikiloc import failed: trail job .+ \(try 1\): Wikiloc browser: TimeoutError: Cloudflare challenge did not clear/);
  // Public: counts and times, no links, names or messages.
  assert.doesNotMatch(JSON.stringify(failing), /wikiloc\.com|Cloudflare|editor|13756119/);
  // A retry waiting is not a failure yet.
  const b = queueLink(s, { text: 'https://es.wikiloc.com/rutas-senderismo/otra-291000002' }, editor).job;
  claimJob(s);
  finishJob(s, b.id, { status: 'retry', message: 'Cloudflare challenge did not clear' });
  assert.equal(wikilocHealth(s).failedSinceOk, 1);
  assert.equal(wikilocHealth(s).waiting, 1);
  // One that works afterwards clears it (the profile check also counts as looked at).
  queueSync(s, editor);
  s.db.prepare("UPDATE wikiloc_jobs SET not_before=NULL").run();
  for (let job; (job = claimJob(s).job); ) finishJob(s, job.id, { status: 'done', message: 'ok' });
  const ok = wikilocHealth(s);
  assert.equal(ok.status, 'ok');
  assert.equal(ok.failedSinceOk, 0);
  assert.ok(ok.lastOk);
  // The worker silent for over an hour with nothing running: failing.
  const silent = wikilocHealth(s, Date.now() + 2 * 3600_000);
  assert.equal(silent.status, 'failing');
  assert.equal(silent.reason, 'worker');
  // A worker cut in the middle of a job: busy for the hour, then silent (only its next claim requeues the job).
  queueLink(s, { text: 'https://es.wikiloc.com/rutas-senderismo/tercera-291000003' }, editor);
  claimJob(s);
  assert.equal(wikilocHealth(s, Date.now() + 30 * 60_000).status, 'ok');
  assert.equal(wikilocHealth(s, Date.now() + 2 * 3600_000).reason, 'worker');
});

// Ecuador's clock (UTC-5) as the server's: `ec('2026-11-11', 21, 0)` is 02:00 UTC on 12 Nov.
const ec = (day, h, m = 0) => Date.parse(`${day}T00:00:00Z`) + (h + 5) * 3600_000 + m * 60_000;

test('the daily check queues «Buscar nuevos» once a day from 21:00 Ecuador, never twice at once; a fake worker runs it', async t => {
  assert.equal(DAILY_SYNC_HOUR_EC, 21);
  const s = store();
  t.mock.method(console, 'log', () => {});
  // No profile followed: nothing to do.
  assert.equal(dailySync(s, ec('2026-11-11', 21)), null);
  addProfile(s, { url: '13756119' }, editor);
  addProfile(s, { url: '23021371', collector: 'AO - Ángel Ojeda' }, editor);
  assert.equal(dailySync(s, ec('2026-11-11', 20, 59)), null);
  const first = dailySync(s, ec('2026-11-11', 21, 0));
  assert.deepEqual(first.jobs.map(j => [j.kind, j.target, j.requestedBy]), [
    ['profile', '13756119', 'wikiloc-daily'],
    ['profile', '23021371', 'wikiloc-daily'],
  ]);
  // Once that day, also after midnight UTC.
  assert.equal(dailySync(s, ec('2026-11-11', 23, 50)), null);
  assert.equal(dailySync(s, ec('2026-11-12', 20)), null);
  // The next day while the first look still waits: no second look of a profile.
  assert.deepEqual(dailySync(s, ec('2026-11-12', 21, 10)).jobs, []);
  assert.equal(s.db.prepare("SELECT count(*) AS n FROM wikiloc_jobs WHERE status IN ('queued','running')").get().n, 2);
  assert.equal(wikilocHealth(s).dailyQueued, '2026-11-12');
  // The worker (fake): Cloudflare stops the first profile once, so it waits for its retry like any job; then both work.
  const job1 = claimJob(s).job;
  finishJob(s, job1.id, { status: 'retry', message: 'Wikiloc browser: TimeoutError: Cloudflare challenge did not clear' });
  const job2 = claimJob(s).job;
  assert.equal(job2.target, '23021371');
  assert.equal(claimJob(s).job, null, 'the retry waits its 5 minutes');
  finishJob(s, job2.id, { status: 'done', message: 'Sin rutas nuevas (6 rutas revisadas)' });
  // The next day's look while that retry waits: only the other profile is queued, the retry keeps its wait.
  const wait = s.db.prepare('SELECT not_before FROM wikiloc_jobs WHERE id=?').get(job1.id).not_before;
  assert.deepEqual(dailySync(s, ec('2026-11-13', 21, 0)).jobs.map(j => j.target), ['23021371']);
  assert.equal(s.db.prepare('SELECT not_before FROM wikiloc_jobs WHERE id=?').get(job1.id).not_before, wait);
  s.db.prepare('UPDATE wikiloc_jobs SET not_before=NULL').run();
  for (let job; (job = claimJob(s).job); ) finishJob(s, job.id, { status: 'done', message: '1 rutas nuevas de 37' });
  const health = wikilocHealth(s);
  assert.equal(health.status, 'ok');
  assert.equal(health.failedSinceOk, 0);
  assert.equal(health.dailyQueued, '2026-11-13');
  // A walk the daily look found can be removed from the list by any editor, as one they brought in.
  const { walk } = await saveWalk(s, { ...walkBody(), jobId: job2.id }, editor, { fetchImage: fakeImage([]) });
  assert.equal(walk.createdBy, 'wikiloc-daily');
  assert.deepEqual(deleteWalk(s, walk.id, other), { ok: true });
  // A profile whose check fails early in the night, before others that work, still shows: failing.
  t.mock.method(console, 'error', () => {});
  dailySync(s, ec('2026-11-14', 21, 0));
  finishJob(s, claimJob(s).job.id, { status: 'failed', message: 'App answered 502' });
  finishJob(s, claimJob(s).job.id, { status: 'done', message: 'Sin rutas nuevas (6 rutas revisadas)' });
  const failing = wikilocHealth(s);
  assert.equal(failing.status, 'failing');
  assert.equal(failing.failedSinceOk, 0);
  assert.equal(failing.profilesFailing, 1);
  // That profile's next check works: ok again.
  dailySync(s, ec('2026-11-15', 21, 0));
  for (let job; (job = claimJob(s).job); ) finishJob(s, job.id, { status: 'done', message: 'ok' });
  assert.equal(wikilocHealth(s).status, 'ok');
});

test('a walk removed from the list stays out of profile checks, daily or by hand; its pasted link brings it back', async t => {
  t.mock.method(console, 'log', () => {});
  const s = store();
  addProfile(s, { url: '13756119' }, editor);
  dailySync(s, ec('2026-11-11', 21, 0));
  const daily = claimJob(s).job;
  const { walk } = await saveWalk(s, { ...walkBody(), jobId: daily.id }, editor, { fetchImage: fakeImage([]) });
  finishJob(s, daily.id, { status: 'done', message: '1 rutas nuevas de 1' });
  // Removed by an editor who did not bring it in (the daily check did).
  deleteWalk(s, walk.id, other);
  // The next profile check, daily or «Buscar nuevos», counts it as known: the worker does not fetch it again.
  dailySync(s, ec('2026-11-12', 21, 0));
  assert.ok(claimJob(s).job.knownIds.includes(walk.wikilocId));
  s.db.prepare("UPDATE wikiloc_jobs SET status='done' WHERE status IN ('queued','running')").run();
  queueSync(s, editor);
  assert.ok(claimJob(s).job.knownIds.includes(walk.wikilocId));
  s.db.prepare("UPDATE wikiloc_jobs SET status='done' WHERE status IN ('queued','running')").run();
  // Its link pasted: a trail job, which brings it back (and it is known as a walk again, not as removed).
  const link = queueLink(s, { url: walk.url }, editor).job;
  assert.equal(claimJob(s).job.id, link.id);
  const back = await saveWalk(s, { ...walkBody(), jobId: link.id }, { id: 'w', username: 'wikiloc-worker', role: 'editor' }, { fetchImage: fakeImage([]) });
  assert.equal(back.walk.status, 'waiting');
  assert.equal(back.walk.createdBy, 'editor');
  assert.equal(s.db.prepare('SELECT count(*) AS n FROM wikiloc_removed').get().n, 0);
});

test('an editor removes a waiting walk the daily check found, not one already imported', async t => {
  t.mock.method(console, 'log', () => {});
  const s = store();
  addProfile(s, { url: '13756119' }, editor);
  dailySync(s, ec('2026-11-11', 21, 0));
  const daily = claimJob(s).job;
  const { walk } = await saveWalk(s, { ...walkBody(), jobId: daily.id }, editor, { fetchImage: fakeImage([]) });
  s.db.prepare("UPDATE wikiloc_walks SET status='imported' WHERE id=?").run(walk.id);
  assert.throws(() => deleteWalk(s, walk.id, other), { code: 'FORBIDDEN' });
  assert.deepEqual(deleteWalk(s, walk.id, reviewer), { ok: true });
});

test('the daily check looks every 10 minutes on its timer and stops on close', t => {
  t.mock.timers.enable({ apis: ['setInterval'] });
  t.mock.method(console, 'log', () => {});
  const s = store();
  addProfile(s, { url: '13756119' }, editor);
  let clock = ec('2026-11-11', 20, 55);
  const daily = startDailySync(s, { now: () => clock });
  t.mock.timers.tick(10 * 60_000);
  assert.equal(s.db.prepare('SELECT count(*) AS n FROM wikiloc_jobs').get().n, 0, 'still 20:55');
  clock = ec('2026-11-11', 21, 5);
  t.mock.timers.tick(10 * 60_000);
  assert.equal(s.db.prepare('SELECT count(*) AS n FROM wikiloc_jobs').get().n, 1);
  daily.stop();
  clock = ec('2026-11-12', 21, 5);
  t.mock.timers.tick(30 * 60_000);
  assert.equal(s.db.prepare('SELECT count(*) AS n FROM wikiloc_jobs').get().n, 1, 'stopped');
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

test('a walk imported again from Wikiloc (no GPS times) keeps the timed GPX track; its captures are the new ones', async () => {
  const s = store();
  const { walk } = await saveWalk(s, walkBody(), editor, { fetchImage: fakeImage([]) });
  const gpx = saveTrack(s, { ...body('request-0300'), wikilocWalkId: walk.id }, editor);
  reopenWalk(s, walk.id);
  const untimed = [
    [-0.950528, -77.869962, 601, null],
    [-0.950562, -77.869942, 600, null],
  ];
  const again = saveTrack(
    s,
    { ...body('request-0301'), track: untimed, wikilocWalkId: walk.id, captures: [{ ...body('x').captures[0], markId: 'B71' }] },
    editor,
  );
  assert.equal(again.track.id, gpx.track.id);
  // The points and their times as the GPX gave them (body()'s track), not the Wikiloc page's.
  assert.deepEqual(listTracks(s)[0].track, [
    [-0.950528, -77.869962, 601.4, '2026-09-26T14:11:55Z'],
    [-0.950562, -77.869942, null, null],
  ]);
  assert.equal(listTracks(s)[0].captures[0].markId, 'B71');
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

test("a doubt carries its point's GPS time and the identifier's reading of its photo (species, wing box)", async () => {
  const { s } = await misPaired();
  const { walk } = await saveWalk(
    s,
    {
      ...walkBody(),
      date: '2025-09-10',
      collector: 'AA - Alex Arias',
      waypoints: [
        // Saved in Wikiloc at 14:47 UTC: 9:47 in Ecuador.
        { lat: -0.95, lon: -77.86, text: 'Marip 1', photos: ['https://s2.wklcdn.com/image_458/1/2/555Master.jpg'], time: '2025-09-10T14:47:00Z' },
        { lat: -0.95, lon: -77.87, text: 'Planta', photos: [] },
      ],
    },
    editor,
    { fetchImage: async () => ({ type: 'image/jpeg', data: Buffer.from([0xff, 0xd8, 0xff, 0xd9]) }) },
  );
  // Another visit to the same place, another day: its time is not this point's.
  await saveWalk(
    s,
    {
      ...walkBody(),
      url: 'https://es.wikiloc.com/rutas-senderismo/otra-visita-291000777',
      date: '2025-09-11',
      collector: 'AA - Alex Arias',
      waypoints: [{ lat: -0.95, lon: -77.87, text: 'Planta', photos: [], time: '2025-09-11T15:00:00Z' }],
    },
    editor,
  );
  const read = createWalkIds({
    store: s,
    identifier: { status: () => ({ installed: true }), identify: async () => ({ species: [['Oleria onega', 0.61], ['Oleria ilerdina', 0.11]], subspecies: [], crop: 'success', wingBox: [0.1, 0.2, 0.8, 0.9] }) },
  });
  read.start(walk.id);
  await read.idle();
  const [marip, planta] = rematchTracks(s).doubts.filter(d => d.walkId === walk.id);
  assert.equal(marip.gps, 587);
  assert.deepEqual(marip.readings, [{ photo: '555', species: [['Oleria onega', 0.61], ['Oleria ilerdina', 0.11]], box: [0.1, 0.2, 0.8, 0.9] }]);
  // No time of its own (the same place on another day does not count), no photo: nothing made up.
  assert.equal(planta.gps, null);
  assert.deepEqual(planta.readings, []);
  s.close();
});

test("a row's project photos (dorsal, ventral, wing box) and its collection sex call; a skipped CAM gets no sex", async () => {
  const id1 = '1AbCdEfGhIjKlMnOpQrStUvWx01';
  const id2 = '1AbCdEfGhIjKlMnOpQrStUvWx02';
  const sheets = new LocalSheets({
    Photo_links: [
      { row: 2, values: { 'Full Path': 'Peru/CAM000001d.JPG', Name: 'CAM000001d.JPG', Type: 'File', URL: `https://drive.google.com/file/d/${id1}/view` } },
      { row: 3, values: { 'Full Path': 'Peru/CAM000001v.JPG', Name: 'CAM000001v.JPG', Type: 'File', URL: `https://drive.google.com/file/d/${id2}/view` } },
    ],
  });
  const s = new Store({ localMode: true }, { sheets });
  await s.sync({ sheets: ['Photo_links'] });
  initPhotoTables(s.db);
  s.db.prepare('INSERT INTO photo_wing_boxes(name, box_json, conf) VALUES(?,?,?)').run('CAM000001d', '[0.1,0.2,0.6,0.7]', 0.9);
  s.db
    .prepare("INSERT INTO photo_predictions(cam, sex, sex_p_male, sex_supported, sex_species, sex_status, imported_at) VALUES(?,?,?,?,?,?,'x')")
    .run('CAM000001', 'male', 0.93, 1, 'Oleria onega', 'species_supported');
  s.db
    .prepare("INSERT INTO photo_predictions(cam, sex, sex_p_male, sex_supported, sex_species, sex_status, imported_at) VALUES(?,?,?,?,?,?,'x')")
    .run('CAM000003', 'female', 0.02, 1, 'Oleria onega', 'species_supported');
  // The locked test portion of the collection sex model: listed, never scored here.
  const dir = mkdtempSync(join(tmpdir(), 'skip-'));
  writeFileSync(join(dir, 'skip.txt'), 'CAM000003\n');
  process.env.COLLECTION_SEX_SKIP_FILE = join(dir, 'skip.txt');
  const got = projectInfo(s, new Set(['CAM000001', 'CAM000002', 'CAM000003']));
  // Without the list (unset, or unreadable), no collection sex at all: the locked portion never leaks.
  delete process.env.COLLECTION_SEX_SKIP_FILE;
  assert.equal(projectInfo(s, new Set(['CAM000001'])).CAM000001.sex, null);
  process.env.COLLECTION_SEX_SKIP_FILE = join(dir, 'missing.txt');
  assert.equal(projectInfo(s, new Set(['CAM000001'])).CAM000001.sex, null);
  // A list that is not CAM IDs only (the split CSV, commas, quotes, empty) matches no CAM: closed too.
  const lists = {
    'split.csv': 'cam_id,split\nCAM000003,locked\n',
    'commas.txt': 'CAM000003,CAM000004\n',
    'quoted.txt': '"CAM000003"\n',
    'mixed.txt': 'CAM000003\nCAM00004\n',
    'empty.txt': '\n',
  };
  for (const [name, text] of Object.entries(lists)) {
    writeFileSync(join(dir, name), text);
    process.env.COLLECTION_SEX_SKIP_FILE = join(dir, name);
    assert.equal(projectInfo(s, new Set(['CAM000001'])).CAM000001.sex, null, name);
  }
  // CAM IDs in any case, one per line or space: open.
  writeFileSync(join(dir, 'spaced.txt'), 'cam000003  CAM000004\n');
  process.env.COLLECTION_SEX_SKIP_FILE = join(dir, 'spaced.txt');
  const spaced = projectInfo(s, new Set(['CAM000001', 'CAM000003']));
  assert.equal(spaced.CAM000001.sex.pMale, 0.93);
  assert.equal(spaced.CAM000003.sex, null);
  delete process.env.COLLECTION_SEX_SKIP_FILE;
  assert.deepEqual(got.CAM000003, { photos: [], sex: null });
  assert.deepEqual(got.CAM000001, {
    photos: [
      { id: id1, view: 'dorsal', box: [0.1, 0.2, 0.6, 0.7] },
      { id: id2, view: 'ventral', box: null },
    ],
    sex: { pMale: 0.93, sure: true, supported: true, unreliable: false },
  });
  assert.deepEqual(got.CAM000002, { photos: [], sex: null });
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
  assert.deepEqual(rematchTracks(s), { changes: [], doubts: [], walks: [], projects: {} });
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
