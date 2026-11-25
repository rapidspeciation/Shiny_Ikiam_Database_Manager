import test from 'node:test';
import assert from 'node:assert/strict';
import { performance } from 'node:perf_hooks';
import { DatabaseSync } from 'node:sqlite';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import {
  addProfile,
  claimJob,
  deleteWalk,
  finishJob,
  initMonitoring,
  listProfiles,
  queryTrailIndex,
  queueSync,
  saveWalk,
  upsertTrailIndex,
} from '../server/monitoring.mjs';
import { findWalk } from '../server/walks.mjs';
import { createAssistant, mcpTools } from '../server/assistant.mjs';
import {
  bracketIdRange,
  filterTrailsByIdRange,
  normalizeTrailMeta,
  parseDistanceKm,
  parseIsoDate,
  parseRecordedMonth,
  readProfile,
  readTrail,
  trackDistanceKm,
} from '../tools/wikiloc/lib.mjs';
import { signIn } from './helpers/assistant.mjs';

const editor = { id: 'e1', username: 'franz', displayName: 'Franz Chandi', role: 'editor' };
const viewer = { id: 'v1', username: 'guest', displayName: 'Guest', role: 'viewer' };

const makeStore = () => new Store({ localMode: true }, { sheets: new LocalSheets({}) });

test('date, month, distance and trail metadata normalization handle Spanish, English and edge cases', () => {
  // ISO dates & timestamps
  assert.equal(parseIsoDate('2022-12-08'), '2022-12-08');
  assert.equal(parseIsoDate('2022-12-09T14:22:10+0100'), '2022-12-09');
  assert.equal(parseIsoDate('8 de diciembre de 2022'), '2022-12-08');
  assert.equal(parseIsoDate('08/12/2022'), '2022-12-08');
  assert.equal(parseIsoDate('8 Dec 2022'), '2022-12-08');
  // Invalid calendar dates rejected
  assert.equal(parseIsoDate('2025-02-30'), null);
  assert.equal(parseIsoDate('2022-13-01'), null);
  assert.equal(parseIsoDate('not a date'), null);
  assert.equal(parseIsoDate('1800-01-01'), null);

  // Recorded month parsing
  assert.equal(parseRecordedMonth('diciembre 2022'), '2022-12');
  assert.equal(parseRecordedMonth('diciembre de 2022'), '2022-12');
  assert.equal(parseRecordedMonth('Dec 2022'), '2022-12');
  assert.equal(parseRecordedMonth('2022-12'), '2022-12');
  assert.equal(parseRecordedMonth('2022-12-08'), '2022-12');
  assert.equal(parseRecordedMonth('2025-02-30'), null);
  assert.equal(parseRecordedMonth('2022-13'), null);
  assert.equal(parseRecordedMonth(''), null);

  // Distance parsing & track haversine fallback
  assert.equal(parseDistanceKm('1,11 km'), 1.11);
  assert.equal(parseDistanceKm('5.42 km'), 5.42);
  assert.equal(parseDistanceKm('850 m'), 0.85);
  assert.equal(parseDistanceKm(3.456), 3.46);
  assert.equal(parseDistanceKm(-1), null);
  assert.equal(parseDistanceKm('abc'), null);
  const trackKm = trackDistanceKm([
    [-0.9505, -77.8699, 600],
    [-0.9595, -77.8699, 600],
  ]);
  assert.ok(trackKm > 0.9 && trackKm < 1.1, `expected ~1 km, got ${trackKm}`);
  assert.equal(trackDistanceKm([[-0.95, -77.86]]), null);

  // Full trail normalization: upload date in different month never overrides recorded month
  const meta = normalizeTrailMeta({
    url: 'https://es.wikiloc.com/rutas-senderismo/colecta-venecia-misahualli-120902281',
    title: 'Colecta venecia misahualli',
    author: '11910200',
    recorded: 'diciembre 2022',
    created: '2023-01-03T10:00+0100',
    place: 'Misahuallí, Napo',
    distance: '3,45 km',
  });
  assert.deepEqual(meta, {
    wikilocId: '120902281',
    profileId: '11910200',
    trailUrl: 'https://es.wikiloc.com/rutas-senderismo/colecta-venecia-misahualli-120902281',
    title: 'Colecta venecia misahualli',
    place: 'Misahuallí, Napo',
    recordedDate: null,
    recordedMonth: '2022-12',
    uploadedDate: '2023-01-03',
    distanceKm: 3.45,
  });

  // Same-month upload stays in uploadedDate and never fabricates recordedDate when title lacks a day
  const sameMonth = normalizeTrailMeta({
    url: 'https://es.wikiloc.com/rutas-senderismo/colecta-venecia-misahualli-120902281',
    title: 'Colecta venecia misahualli',
    profileId: '11910200',
    recorded: 'diciembre 2022',
    created: '2022-12-08T18:10+0100',
  });
  assert.equal(sameMonth.recordedDate, null);
  assert.equal(sameMonth.recordedMonth, '2022-12');
  assert.equal(sameMonth.uploadedDate, '2022-12-08');

  // Non-Wikiloc URL rejected even if wikilocId is provided
  assert.equal(normalizeTrailMeta({ url: 'https://example.com/nope', title: 'Bad' }), null);
  assert.equal(
    normalizeTrailMeta({ url: 'https://other.example/monitoreo-120902999', wikilocId: '120902999', title: 'Monitoreo' }),
    null,
  );
});

test('ID-range filtering and date bracketing narrow candidate trails cleanly', () => {
  const trails = [
    { wikilocId: '100000001', recordedDate: '2022-06-10', uploadedDate: '2022-06-11', recordedMonth: '2022-06' },
    { wikilocId: '115000000', recordedDate: '2022-11-20', uploadedDate: '2022-11-21', recordedMonth: '2022-11' },
    { wikilocId: '120902281', recordedDate: '2022-12-08', uploadedDate: '2022-12-08', recordedMonth: '2022-12' },
    { wikilocId: '130000000', recordedDate: '2023-02-14', uploadedDate: '2023-02-15', recordedMonth: '2023-02' },
    { wikilocId: '213523060', recordedDate: '2025-05-14', uploadedDate: '2025-05-14', recordedMonth: '2025-05' },
  ];
  assert.deepEqual(
    filterTrailsByIdRange(trails, { minId: 115000000, maxId: 130000000 }).map(t => t.wikilocId),
    ['115000000', '120902281', '130000000'],
  );
  assert.deepEqual(
    filterTrailsByIdRange(trails, { minId: 115000000, maxId: 130000000, month: '2022-12' }).map(t => t.wikilocId),
    ['120902281'],
  );
  // Bracket for target date 2022-12-08 using known anchors (excluding target itself)
  const anchors = trails.filter(t => t.wikilocId !== '120902281');
  assert.deepEqual(bracketIdRange(anchors, { dateFrom: '2022-12-01', dateTo: '2022-12-31' }), {
    minId: 115000000,
    maxId: 130000000,
  });
});

test('backward-compatible SQLite migration adds columns, indexes and backfills legacy wikiloc_walks idempotently', () => {
  const db = new DatabaseSync(':memory:');
  try {
    // Simulate a pre-migration legacy database without the new columns or wikiloc_trail_index.
    db.exec(`CREATE TABLE wikiloc_walks(
      id TEXT PRIMARY KEY, wikiloc_id TEXT UNIQUE NOT NULL, url TEXT NOT NULL, name TEXT NOT NULL,
      date TEXT, data_json TEXT NOT NULL, status TEXT NOT NULL, track_id TEXT, created_by TEXT NOT NULL, created_at TEXT NOT NULL
    );`);
    db.prepare(
      `INSERT INTO wikiloc_walks(id, wikiloc_id, url, name, date, data_json, status, track_id, created_by, created_at)
       VALUES(?,?,?,?,?,?,?,?,?,?)`,
    ).run(
      'w-legacy-1',
      '213523060',
      'https://es.wikiloc.com/rutas-senderismo/monitoreo-ikiam-14-mayo-2025-213523060',
      'Monitoreo Ikiam FCH 14 mayo 2025',
      '2025-05-14',
      JSON.stringify({ author: '13756119', recorded: 'mayo 2025', track: [[-0.95, -77.86, 600], [-0.96, -77.86, 600]], waypoints: [] }),
      'imported',
      null,
      'franz',
      '2025-05-15T00:00:00.000Z',
    );
    db.prepare(
      `INSERT INTO wikiloc_walks(id, wikiloc_id, url, name, date, data_json, status, track_id, created_by, created_at)
       VALUES(?,?,?,?,?,?,?,?,?,?)`,
    ).run(
      'w-legacy-2',
      '120902281',
      'https://es.wikiloc.com/rutas-senderismo/colecta-venecia-misahualli-120902281',
      'Colecta venecia misahualli',
      null,
      JSON.stringify({ author: '11910200', recorded: 'diciembre 2022', track: [], waypoints: [] }),
      'waiting',
      null,
      'franz',
      '2025-05-15T00:00:00.000Z',
    );

    // Run migration twice to prove strict idempotency.
    initMonitoring(db);
    initMonitoring(db);

    const walkCols = new Set(db.prepare('PRAGMA table_info(wikiloc_walks)').all().map(c => c.name));
    for (const col of ['profile_id', 'recorded_date', 'recorded_month', 'uploaded_date', 'place', 'distance_km']) {
      assert.ok(walkCols.has(col), `missing migrated column ${col}`);
    }
    const indexNames = new Set(db.prepare("SELECT name FROM sqlite_master WHERE type='index'").all().map(r => r.name));
    for (const idx of [
      'wikiloc_walks_profile_date',
      'wikiloc_walks_recorded_month',
      'wikiloc_trail_index_profile_date',
      'wikiloc_trail_index_recorded_month',
    ]) {
      assert.ok(indexNames.has(idx), `missing index ${idx}`);
    }

    const w1 = db.prepare('SELECT profile_id, recorded_date, recorded_month, distance_km FROM wikiloc_walks WHERE id=?').get('w-legacy-1');
    assert.equal(w1.profile_id, '13756119');
    assert.equal(w1.recorded_date, '2025-05-14');
    assert.equal(w1.recorded_month, '2025-05');
    assert.ok(w1.distance_km > 1 && w1.distance_km < 1.2);

    const w2 = db.prepare('SELECT profile_id, recorded_date, recorded_month FROM wikiloc_walks WHERE id=?').get('w-legacy-2');
    assert.equal(w2.profile_id, '11910200');
    assert.equal(w2.recorded_date, null);
    assert.equal(w2.recorded_month, '2022-12');

    // Backfilled into wikiloc_trail_index too.
    const indexed = db
      .prepare('SELECT wikiloc_id, profile_id, recorded_month, walk_id FROM wikiloc_trail_index ORDER BY wikiloc_id_num')
      .all()
      .map(r => ({ ...r }));
    assert.deepEqual(indexed, [
      { wikiloc_id: '120902281', profile_id: '11910200', recorded_month: '2022-12', walk_id: 'w-legacy-2' },
      { wikiloc_id: '213523060', profile_id: '13756119', recorded_month: '2025-05', walk_id: 'w-legacy-1' },
    ]);
  } finally {
    db.close();
  }
});

test('multi-profile sync (13756119 + 11910200) indexes trails via finishJob and preserves richer walk metadata', async () => {
  const s = makeStore();
  try {
    addProfile(s, { url: '13756119', name: 'Franz Chandi 1', collector: 'FCH - Franz Chandi', pattern: 'monitor' }, editor);
    addProfile(s, { url: 'https://es.wikiloc.com/wikiloc/user.do?id=11910200', name: 'Franz Chandi', collector: 'FCH - Franz Chandi', pattern: 'monitor' }, editor);
    assert.deepEqual(
      listProfiles(s).map(p => [p.wikilocUser, p.name]),
      [
        ['13756119', 'Franz Chandi 1'],
        ['11910200', 'Franz Chandi'],
      ],
    );

    // Save a full walk on 11910200 with exact recorded_date & uploaded_date.
    const saved = await saveWalk(
      s,
      {
        url: 'https://es.wikiloc.com/rutas-senderismo/colecta-venecia-misahualli-120902281',
        name: 'Colecta venecia misahualli',
        date: '2022-12-08',
        recorded: 'diciembre 2022',
        uploadedDate: '2022-12-09',
        place: 'Misahuallí, Napo',
        distanceKm: 3.45,
        author: '11910200',
        track: [],
        waypoints: [],
      },
      editor,
    );
    assert.equal(saved.walk.author, '11910200');
    assert.equal(saved.walk.uploadedDate, '2022-12-09');
    assert.equal(saved.walk.distanceKm, 3.45);

    // Save two walks whose titles have misleading dates corrected by "Fecha de realización" (Sol F1 & Fable F1).
    await saveWalk(
      s,
      {
        url: 'https://es.wikiloc.com/rutas-senderismo/monitoreo-27-4-2024-220000001',
        name: 'Monitoreo 27/4/2024',
        date: '2025-04-27',
        recorded: 'abril 2025',
        place: 'Ikiam, Tena',
        distanceKm: 1.15,
        author: '13756119',
        track: [],
        waypoints: [],
      },
      editor,
    );
    await saveWalk(
      s,
      {
        url: 'https://es.wikiloc.com/rutas-senderismo/t2-8-9-2025-220000002',
        name: 'T2 8/9/2025',
        date: '2025-08-09',
        recorded: 'agosto 2025',
        author: '13756119',
        track: [],
        waypoints: [],
      },
      editor,
    );

    // Profile sync reports list-page trails (must NEVER overwrite saved walk date/month/place/distance!).
    queueSync(s, editor);
    const job1 = claimJob(s).job;
    assert.equal(job1.target, '13756119');
    finishJob(s, job1.id, {
      status: 'done',
      profileName: 'Franz Chandi 1',
      message: 'Sin rutas nuevas (4 rutas revisadas)',
      trails: [
        {
          url: 'https://es.wikiloc.com/rutas-senderismo/monitoreo-ithomidos-14-mayo-2025-213523060',
          title: 'Monitoreo ithomidos sendero Ikiam FCH 14 mayo 2025',
        },
        {
          url: 'https://es.wikiloc.com/rutas-senderismo/monitoreo-27-4-2024-220000001',
          title: 'Monitoreo 27/4/2024',
          distanceKm: 9.99,
          place: 'Wrong Place',
        },
        {
          url: 'https://es.wikiloc.com/rutas-senderismo/t2-8-9-2025-220000002',
          title: 'T2 8/9/2025',
        },
        // Undated trail (no date in title)
        {
          url: 'https://es.wikiloc.com/rutas-senderismo/monitoreo-t3-220000003',
          title: 'Monitoreo T3',
        },
      ],
    });
    const job2 = claimJob(s).job;
    assert.equal(job2.target, '11910200');
    finishJob(s, job2.id, {
      status: 'done',
      profileName: 'Franz Chandi',
      message: 'Sin rutas nuevas (2 rutas revisadas)',
      trails: [
        // Sparse list-card re-index must NOT clobber exact recorded_date, place or walk_id!
        {
          url: 'https://es.wikiloc.com/rutas-senderismo/colecta-venecia-misahualli-120902281',
          title: 'Colecta venecia misahualli',
        },
        // Non-monitoring historical walk indexed with month-only precision.
        {
          url: 'https://es.wikiloc.com/rutas-senderismo/apuya-sendero-120901100',
          title: 'Apuya sendero',
          recorded: 'diciembre 2022',
          place: 'Apuya, Napo',
          distance: '4,2 km',
        },
      ],
    });

    // Verify saved walks kept their corrected dates, months, place & distance after profile sync.
    const correctedYear = queryTrailIndex(s, { profileId: '13756119', dateFrom: '2025-04-27', dateTo: '2025-04-27' });
    assert.equal(correctedYear.walks.length, 1);
    assert.equal(correctedYear.walks[0].wikiloc_id, '220000001');
    assert.equal(correctedYear.walks[0].recorded_date, '2025-04-27');
    assert.equal(correctedYear.walks[0].recorded_month, '2025-04');
    assert.equal(correctedYear.walks[0].place, 'Ikiam, Tena');
    assert.equal(correctedYear.walks[0].distance_km, 1.15);
    assert.equal(correctedYear.undated, 1, 'undated Monitoreo T3 reported on date search');

    const correctedSwap = queryTrailIndex(s, { profileId: '13756119', dateFrom: '2025-08-09', dateTo: '2025-08-09' });
    assert.equal(correctedSwap.walks.length, 1);
    assert.equal(correctedSwap.walks[0].wikiloc_id, '220000002');
    assert.equal(correctedSwap.walks[0].recorded_month, '2025-08');
    assert.equal(queryTrailIndex(s, { profileId: '13756119', dateFrom: '2025-09-08', dateTo: '2025-09-08' }).total, 0);

    // Undated trail still findable by place/title keywords.
    const undatedHit = queryTrailIndex(s, { profileId: '13756119', place: 'T3' });
    assert.equal(undatedHit.total, 1);
    assert.equal(undatedHit.walks[0].wikiloc_id, '220000003');

    const hit = queryTrailIndex(s, { profileId: '11910200', dateFrom: '2022-12-08', dateTo: '2022-12-08' });
    assert.equal(hit.walks.length, 1);
    assert.equal(hit.walks[0].wikiloc_id, '120902281');
    assert.equal(hit.walks[0].recorded_date, '2022-12-08');
    assert.equal(hit.walks[0].place, 'Misahuallí, Napo');
    assert.equal(hit.walks[0].walk_id, saved.walk.id);
    assert.equal(hit.walks[0].walk_status, 'waiting');
    // Month-only trail from Dec 2022 surfaced cleanly in month_candidates (both with and without matching `month`).
    assert.equal(hit.month_candidates.length, 1);
    assert.equal(hit.month_candidates[0].wikiloc_id, '120901100');
    const hitWithMonth = queryTrailIndex(s, { profileId: '11910200', dateFrom: '2022-12-01', dateTo: '2022-12-10', month: '2022-12' });
    assert.equal(hitWithMonth.month_candidates.length, 1);
    assert.equal(hitWithMonth.month_candidates[0].wikiloc_id, '120901100');
    const hitOtherMonth = queryTrailIndex(s, { profileId: '11910200', dateFrom: '2022-12-01', dateTo: '2022-12-10', month: '2022-11' });
    assert.equal(hitOtherMonth.month_candidates, undefined);

    // Deleting the walk clears walk_id in the index without deleting the index row.
    deleteWalk(s, saved.walk.id, editor);
    const afterDelete = queryTrailIndex(s, { profileId: '11910200', place: 'misahualli' });
    assert.equal(afterDelete.walks.length, 1);
    assert.equal(afterDelete.walks[0].walk_id, null);
    assert.equal(afterDelete.walks[0].walk_status, null);
  } finally {
    s.close();
  }
});

test('find_walk MCP tool is registered as deferred, strictly read-only, filters accurately and rejects bad inputs', async () => {
  const s = makeStore();
  try {
    addProfile(s, { url: '13756119', name: 'Franz Chandi 1', collector: 'FCH - Franz Chandi' }, editor);
    addProfile(s, { url: '11910200', name: 'Franz Chandi', collector: 'FCH - Franz Chandi' }, editor);
    upsertTrailIndex(s, [
      {
        wikilocId: '120902281',
        profileId: '11910200',
        url: 'https://es.wikiloc.com/rutas-senderismo/colecta-venecia-misahualli-120902281',
        title: 'Colecta venecia misahualli',
        place: 'Misahuallí, Napo (Ecuador)',
        recordedDate: '2022-12-08',
        uploadedDate: '2022-12-08',
        distanceKm: 3.45,
      },
      {
        wikilocId: '121100500',
        profileId: '11910200',
        url: 'https://es.wikiloc.com/rutas-senderismo/colecta-jatun-sacha-121100500',
        title: 'Colecta Jatun Sacha 100%_test',
        place: 'Jatun Sacha, Napo',
        recordedDate: '2022-12-15',
        uploadedDate: '2022-12-16',
        distanceKm: 5.1,
      },
      {
        wikilocId: '213523060',
        profileId: '13756119',
        url: 'https://es.wikiloc.com/rutas-senderismo/monitoreo-ikiam-14-mayo-2025-213523060',
        title: 'Monitoreo ithomidos sendero Ikiam FCH 14 mayo 2025',
        place: 'Tena, Napo',
        recordedDate: '2025-05-14',
        uploadedDate: '2025-05-14',
        distanceKm: 1.11,
      },
    ]);

    // Deferred MCP tool registration check.
    const tool = mcpTools().find(t => t.name === 'find_walk');
    assert.ok(tool, 'find_walk registered in mcpTools');
    assert.equal(tool._meta?.['anthropic/alwaysLoad'], undefined, 'find_walk stays deferred');

    const assistant = createAssistant({ store: s, config: {} });
    // Read-only: even a viewer account can call find_walk.
    const { call } = signIn(s, assistant, viewer);

    // Filter by date_from + date_to
    const byDate = await call('find_walk', { date_from: '2022-12-01', date_to: '2022-12-10' });
    assert.equal(byDate.total, 1);
    assert.equal(byDate.walks[0].wikiloc_id, '120902281');
    assert.equal(byDate.walks[0].profile_name, 'Franz Chandi');

    // Filter by month
    const byMonth = await call('find_walk', { month: '2022-12' });
    assert.equal(byMonth.total, 2);
    assert.deepEqual(
      byMonth.walks.map(w => w.wikiloc_id),
      ['121100500', '120902281'],
    );

    // Filter by place (case- and accent-insensitive + multi-word)
    const byPlace = await call('find_walk', { place: 'Misahuallí venecia' });
    assert.equal(byPlace.total, 1);
    assert.equal(byPlace.walks[0].wikiloc_id, '120902281');

    // LIKE wildcard escaping: literal '%' only matches row containing '%'
    const byWildcard = await call('find_walk', { place: '100%_' });
    assert.equal(byWildcard.total, 1);
    assert.equal(byWildcard.walks[0].wikiloc_id, '121100500');

    // Filter by profile_id (bare ID and URL)
    const byProfile = await call('find_walk', { profile_id: '13756119' });
    assert.equal(byProfile.total, 1);
    assert.equal(byProfile.walks[0].wikiloc_id, '213523060');
    const byProfileUrl = await call('find_walk', { profile_id: 'https://es.wikiloc.com/wikiloc/user.do?id=11910200' });
    assert.equal(byProfileUrl.total, 2);

    // Empty input returns recent indexed walks + configured profiles
    const all = await call('find_walk', {});
    assert.equal(all.total, 3);
    assert.equal(all.profiles.length, 2);

    // Limit bounds
    const limited = await call('find_walk', { limit: 1 });
    assert.equal(limited.returned, 1);
    assert.equal(limited.total, 3);
    assert.equal(limited.truncated, true);
    const overCapped = await call('find_walk', { limit: 9999 });
    assert.equal(overCapped.returned, 3);
    assert.equal(overCapped.error, undefined);

    // Invalid inputs return clean errors (never throw)
    assert.match((await call('find_walk', { date_from: '2022-02-30' })).error, /date_from/);
    assert.match((await call('find_walk', { date_to: 'not-a-date' })).error, /date_to/);
    assert.match((await call('find_walk', { date_from: '2022-12-10', date_to: '2022-12-01' })).error, /on or before/);
    assert.match((await call('find_walk', { month: '2022-13' })).error, /month/);
    assert.match((await call('find_walk', { profile_id: "11910200'; DROP TABLE wikiloc_trail_index;--" })).error, /profile_id/);
    assert.match((await call('find_walk', { limit: 0 })).error, /limit/);
    assert.match((await call('find_walk', { limit: -5 })).error, /limit/);

    // Enforce SQLite query_only=ON to prove findWalk executes strictly read-only SQL!
    s.db.exec('PRAGMA query_only = ON');
    const readOnlyRes = findWalk(s, { month: '2022-12', profile_id: '11910200' });
    assert.equal(readOnlyRes.total, 2);
    s.db.exec('PRAGMA query_only = OFF');
  } finally {
    s.close();
  }
});

test('indexed query on 1,000 synthetic walks uses SQLite indexes and runs in < 100 ms', () => {
  const s = makeStore();
  try {
    addProfile(s, { url: '11910200', name: 'Franz Chandi', collector: 'FCH - Franz Chandi' }, editor);
    addProfile(s, { url: '13756119', name: 'Franz Chandi 1', collector: 'FCH - Franz Chandi' }, editor);

    const synthetic = [];
    const baseMs = Date.UTC(2020, 0, 1);
    for (let i = 0; i < 1000; i++) {
      const profileId = i % 2 === 0 ? '11910200' : '13756119';
      const day = new Date(baseMs + i * 2 * 86400_000).toISOString().slice(0, 10);
      const wikilocId = String(100_000_000 + i * 150_000);
      synthetic.push({
        wikilocId,
        profileId,
        url: `https://es.wikiloc.com/rutas-senderismo/ruta-${i}-${wikilocId}`,
        title: i === 536 ? 'Colecta venecia misahualli' : `Monitoreo tramo ${(i % 4) + 1} #${i}`,
        place: i === 536 ? 'Misahuallí, Napo' : i % 3 === 0 ? 'Ikiam, Tena' : 'Apuya, Napo',
        recordedDate: i === 536 ? '2022-12-08' : day,
        uploadedDate: i === 536 ? '2022-12-08' : day,
        distanceKm: 1.1 + (i % 50) * 0.1,
      });
    }
    const inserted = upsertTrailIndex(s, synthetic);
    assert.equal(inserted.indexed, 1000);

    // Verify SQLite query planner uses the new indexes.
    const planProfileDate = s.db
      .prepare('EXPLAIN QUERY PLAN SELECT * FROM wikiloc_trail_index WHERE profile_id = ? AND recorded_date >= ? AND recorded_date <= ?')
      .all('11910200', '2022-12-01', '2022-12-31')
      .map(r => r.detail)
      .join(' ');
    assert.match(planProfileDate, /wikiloc_trail_index_profile_date/, `expected index scan, got: ${planProfileDate}`);

    const planMonth = s.db
      .prepare('EXPLAIN QUERY PLAN SELECT * FROM wikiloc_trail_index WHERE recorded_month = ?')
      .all('2022-12')
      .map(r => r.detail)
      .join(' ');
    assert.match(planMonth, /wikiloc_trail_index_recorded_month/, `expected index scan, got: ${planMonth}`);

    // Timed read-only lookups across 1,000 rows must complete well under 100 ms.
    const t0 = performance.now();
    const res1 = findWalk(s, { profile_id: '11910200', date_from: '2022-12-01', date_to: '2022-12-31', place: 'misahualli' });
    const res2 = findWalk(s, { month: '2022-12' });
    const res3 = queryTrailIndex(s, { profileId: '11910200', idMin: 180_000_000, idMax: 181_000_000 });
    const elapsedMs = performance.now() - t0;

    assert.equal(res1.total, 1);
    assert.equal(res1.walks[0].title, 'Colecta venecia misahualli');
    assert.ok(res2.total >= 1);
    assert.ok(res3.total >= 1);
    assert.ok(elapsedMs < 100, `expected < 100 ms on 1,000 rows, took ${elapsedMs.toFixed(2)} ms`);
  } finally {
    s.close();
  }
});

test('readProfile stops pagination early at minId and rejects non-Wikiloc links', async () => {
  const visited = [];
  const pages = {
    0: {
      name: 'Franz Chandi',
      links: [
        {
          url: 'https://es.wikiloc.com/rutas-senderismo/monitoreo-14-mayo-2025-213523060',
          title: 'Monitoreo 14 mayo 2025',
        },
        {
          url: 'https://other.example/monitoreo-120902999',
          title: 'Monitoreo unrelated host',
        },
        {
          url: 'https://es.wikiloc.com/rutas-senderismo/colecta-venecia-misahualli-120902281',
          title: 'Colecta venecia misahualli',
        },
      ],
    },
    10: {
      name: 'Franz Chandi',
      links: [
        {
          url: 'https://es.wikiloc.com/rutas-senderismo/old-trail-90000000',
          title: 'Old trail',
        },
      ],
    },
  };
  let currentFrom = 0;
  const fakePage = {
    goto: async url => {
      visited.push(url);
      currentFrom = Number(/from=(\d+)/.exec(url)?.[1] || 0);
    },
    title: async () => 'Rutas de Franz Chandi | Wikiloc',
    waitForTimeout: async () => {},
    evaluate: async () => pages[currentFrom] || { name: 'Franz Chandi', links: [] },
  };

  // With minId=120000000: page 0 has both valid Wikiloc trails >= 120M (other.example ignored);
  // page 10 has only 90M (< minId) -> stops without fetching page 20!
  const result = await readProfile(fakePage, '11910200', { minId: 120_000_000 });
  assert.equal(result.name, 'Franz Chandi');
  assert.equal(visited.length, 2, 'stopped immediately after page with IDs below minId');
  assert.deepEqual(
    result.trails.map(t => ({ id: t.wikilocId, profileId: t.profileId, recordedDate: t.recordedDate, recordedMonth: t.recordedMonth })),
    [
      { id: '213523060', profileId: '11910200', recordedDate: '2025-05-14', recordedMonth: '2025-05' },
      { id: '120902281', profileId: '11910200', recordedDate: null, recordedMonth: null },
    ],
  );
});

test('readTrail keeps upload-day date fallback for import while leaving recordedDate null for month-only index lookups', async () => {
  const fakePage = {
    goto: async () => {},
    title: async () => 'Colecta venecia misahualli | Wikiloc',
    waitForFunction: async () => {},
    waitForTimeout: async () => {},
    evaluate: async () => ({
      name: 'Colecta venecia misahualli',
      track: [[-0.95, -77.86, 600, null], [-0.96, -77.86, 600, null]],
      waypoints: [],
      done: 'diciembre 2022',
      author: '11910200',
      created: '2022-12-08T19:00:00+0100',
      distance: '3,45 km',
      href: 'https://es.wikiloc.com/rutas-senderismo/colecta-venecia-misahualli-120902281',
      started: null,
      took: '1 hora 30 minutos',
      description: '',
    }),
  };
  const walk = await readTrail(fakePage, 'https://es.wikiloc.com/rutas-senderismo/colecta-venecia-misahualli-120902281');
  assert.equal(walk.author, '11910200');
  // Existing import workflow still gets upload-day fallback in `date`.
  assert.equal(walk.date, '2022-12-08');
  // Index metadata does NOT treat an upload day as a confirmed exact recording day.
  assert.equal(walk.recordedDate, null);
  assert.equal(walk.recordedMonth, '2022-12');
  assert.equal(walk.uploadedDate, '2022-12-08');
  assert.equal(walk.distanceKm, 3.45);
  assert.equal(walk.minutes, 90);

  // Saving this walk keeps it out of exact-day hits and surfaces it under month / month_candidates, even across re-init.
  const s = makeStore();
  try {
    await saveWalk(s, walk, editor);
    initMonitoring(s.db);
    const exactDay = findWalk(s, { date_from: '2022-12-08', date_to: '2022-12-08', profile_id: '11910200' });
    assert.equal(exactDay.total, 0);
    assert.equal(exactDay.month_candidates.length, 1);
    assert.equal(exactDay.month_candidates[0].wikiloc_id, '120902281');
    const byMonth = findWalk(s, { month: '2022-12', profile_id: '11910200' });
    assert.equal(byMonth.total, 1);
    assert.equal(byMonth.walks[0].recorded_date, null);
    assert.equal(byMonth.walks[0].uploaded_date, '2022-12-08');
  } finally {
    s.close();
  }
});
