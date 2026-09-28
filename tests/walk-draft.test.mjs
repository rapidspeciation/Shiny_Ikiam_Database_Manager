import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash, randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { addProfile, claimJob, finishJob, saveWalk } from '../server/monitoring.mjs';
import { createAssistant } from '../server/assistant.mjs';

const EPOCH = Date.UTC(1899, 11, 30);
const serial = iso => Math.round((Date.parse(`${iso}T00:00:00Z`) - EPOCH) / 864e5);
const URL_ = 'https://es.wikiloc.com/rutas-senderismo/monitoreo-ithomidos-26-sep-2026-288058472';
const FCH = 'FCH - Franz Chandi';
const T4 = { lat: -0.950925, lon: -77.869495 };
const monitoring = { Purpose: 'Monitoring', Collection_location: 'Ikiam', Collector: FCH, Tribe: 'Ithomiini' };

async function fixture() {
  const sheets = new LocalSheets({
    Collection_data: [
      // B40 marked in May: seen again on this walk.
      {
        row: 2,
        values: { ...monitoring, Release_Collect: 'Mark_Released', FieldMark_ID: 'B40', SPECIES: 'Oleria gunilla', Collection_date: serial('2026-05-10') },
      },
      // The first point of the walk was already typed in the sheet.
      {
        row: 3,
        values: {
          ...monitoring,
          Release_Collect: 'Mark_Released',
          FieldMark_ID: 'B68',
          SPECIES: 'Hyposcada illinissa',
          Subspecies_Form: 'ida',
          Collection_date: serial('2026-09-26'),
          Collection_time: 545 / 1440,
        },
      },
      { row: 4, values: { ...monitoring, Release_Collect: 'Collected_Preserved', SPECIES: 'Oleria gunilla', CAM_ID: 'CAM000001' } },
      // The next pre-made row, whose Tribe is a formula.
      { row: 5, values: { Tribe: { formula: '=VLOOKUP(S5,Taxonomy_v18Jun25!J:G,1,0)' } } },
    ],
    Taxonomy_v18Jun25: [
      { row: 2, values: { species: 'Oleria gunilla', tribe: 'Ithomiini' } },
      { row: 3, values: { species: 'Hyposcada illinissa', subspecies: 'Hyposcada illinissa ida', tribe: 'Ithomiini' } },
      // Only in Taxonomy, never collected yet.
      { row: 4, values: { species: 'Hypothyris anastasia', subspecies: 'Hypothyris anastasia honesta', tribe: 'Ithomiini' } },
    ],
    Location_data: [{ row: 2, values: { Collection_location: 'Ikiam' } }],
    SamplingDay_data: [],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Collection_data', 'Taxonomy_v18Jun25', 'Location_data', 'SamplingDay_data'] });
  const assistant = createAssistant({ store, config: { claude: {} } });
  store.db
    .prepare(
      "INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u1','franz','Franz','editor','s','h',1,'2026-01-01')",
    )
    .run();
  store.db
    .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
    .run(createHash('sha256').update('franz-token').digest('hex'), 'u1');
  const call = async (name, args) => {
    const out = await assistant.mcp(
      { authorization: 'Bearer franz-token' },
      { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } },
    );
    return JSON.parse(out.body.result.content[0].text);
  };
  return { store, assistant, call, user: { id: 'u1', username: 'franz', role: 'editor' } };
}

/** What tools/wikiloc/worker.mjs does on the home computer. */
async function worker(store, user) {
  const { job } = claimJob(store);
  await saveWalk(
    store,
    {
      url: job.target,
      jobId: job.id,
      date: '2026-09-26',
      name: 'Monitoreo ithomidos 26 sep 2026',
      author: '13756119',
      track: [],
      waypoints: [
        { ...T4, text: 'M1 Hyposcada ilinissa ida hembra 9:05 0.5m NO id: B68' },
        { ...T4, text: 'M2 Oleria gunilla macho 9:40 1m parches id B40' },
        { ...T4, text: 'M3 Hypothyris anastasia honesta hembra 10:02 1,5m NC llovizna ala rota' },
        { ...T4, text: 'M4 Heliconius numata macho 10:30 2m sol' },
      ],
    },
    user,
  );
  finishJob(store, job.id, { status: 'done' });
}

test('a Wikiloc link becomes preliminary Collection_data rows the person confirms', async () => {
  const { store, assistant, call, user } = await fixture();
  addProfile(store, { url: '13756119', collector: FCH }, user);

  // The server cannot open Wikiloc: the link is queued for the home computer.
  const queued = await call('queue_wikiloc', { url: `Mira este: ${URL_}` });
  assert.equal(queued.status, 'queued');
  assert.equal(queued.workerOnline, false);
  assert.equal((await call('get_walk', { url: URL_ })).status, 'queued');

  await worker(store, user);
  const ready = await call('queue_wikiloc', { url: URL_ });
  assert.equal(ready.status, 'ready');

  const draft = await call('get_walk', { walkId: ready.walkId });
  assert.equal(draft.walk.collector, FCH);
  assert.equal(draft.walk.collectorFrom, 'profile');
  assert.equal(draft.walk.date, '2026-09-26');
  const [m1, m2, m3, m4] = draft.points;
  // Already in the sheet (same day and mark): nothing to propose for it.
  assert.equal(m1.inSheet.row, 3);
  assert.equal(m1.species, 'Hyposcada illinissa');
  // A recapture of the May butterfly.
  assert.equal(m2.recapture.row, 2);
  assert.ok(m2.checks.some(c => /Recaptura de B40/.test(c)));
  assert.equal(m2.cloud, 'S&C_(sun_&_cloud_patches)');
  // A species known only from Taxonomy, with its subspecies.
  assert.equal(m3.species, 'Hypothyris anastasia');
  assert.equal(m3.subspecies, 'honesta');
  assert.equal(m3.section, 4);
  assert.ok(m4.checks.some(c => /no está en Taxonomy/.test(c)));
  assert.deepEqual(
    draft.newRows.map(r => r.point),
    [1, 2, 3],
  );
  const [recapture, preserved] = draft.newRows;
  assert.deepEqual(
    {
      Release_Collect: recapture.values.Release_Collect,
      FieldMark_ID: recapture.values.FieldMark_ID,
      Collection_time: recapture.values.Collection_time,
      Cloud_cover: recapture.values.Cloud_cover,
      Rainfall: recapture.values.Rainfall,
      Purpose: recapture.values.Purpose,
      Collection_location: recapture.values.Collection_location,
      Collector: recapture.values.Collector,
      Collection_date: recapture.values.Collection_date,
    },
    {
      Release_Collect: 'Mark_Released',
      FieldMark_ID: 'B40',
      Collection_time: '9:40',
      Cloud_cover: 'S&C_(sun_&_cloud_patches)',
      Rainfall: 'DY_(dry)',
      Purpose: 'Monitoring',
      Collection_location: 'Ikiam',
      Collector: FCH,
      Collection_date: '2026-09-26',
    },
  );
  assert.equal(preserved.values.Release_Collect, 'Collected_Preserved');
  assert.equal(preserved.values.Rainfall, 'DZ_(drizzle)');
  assert.equal(preserved.values.Cloud_cover, 'CL_(cloudy_light)');
  assert.equal(preserved.values.Notes_Collection_data, '26/9/2026 FCH: ala rota');
  // The formula column of the next pre-made row is never filled in.
  assert.equal('Tribe' in preserved.values, false);

  // A species the sheet's list would refuse is caught before the person sees it.
  const refused = await call('propose_changes', { reason: 'Recorrido 26 sep', newRows: draft.newRows });
  assert.match(refused.error, /newRows\[2\]: SPECIES: «Heliconius numata» no está en la lista/);

  // The Asistente tab waits for the proposal and shows it as soon as it is made.
  const first = await assistant.handle({ method: 'GET', path: '/api/chat/proposals', user, query: {} });
  const waiting = assistant.handle({
    method: 'GET',
    path: '/api/chat/proposals',
    user,
    query: { wait: '1', revision: first.body.revision },
  });
  const proposed = await call('propose_changes', {
    reason: 'Recorrido del 26 sep 2026',
    newRows: draft.newRows.slice(0, 2).map((r, i) => (i ? { ...r, values: { ...r.values, Tribe: 'Ithomiini' } } : r)),
  });
  assert.equal(proposed.rows, 2);
  assert.match(proposed.leftOut, /Tribe/);
  const live = await waiting;
  assert.notEqual(live.body.revision, first.body.revision);
  const shown = live.body.proposals[0];
  assert.equal(shown.id, proposed.proposalId);
  assert.equal(shown.changes[0].create, true);
  assert.equal(shown.changes[0].row, null);

  const applied = await assistant.handle({
    method: 'POST',
    path: `/api/chat/proposals/${proposed.proposalId}/apply`,
    user,
    body: { requestId: randomUUID() },
  });
  assert.equal(applied.body.status, 'applied');
  const row5 = store.getRecordBySheetRow('Collection_data', 5).values;
  assert.equal(row5.FieldMark_ID, 'B40');
  assert.equal(row5.Collection_date, serial('2026-09-26'));
  assert.equal(row5.Collection_time, (9 * 60 + 40) / 1440);
  assert.equal(row5.Transect_section, 4);
  assert.equal(store.getRecordBySheetRow('Collection_data', 6).values.SPECIES, 'Hypothyris anastasia');
  // Once written, the proposal shows the rows it created.
  const after = await assistant.handle({ method: 'GET', path: '/api/chat/proposals', user, query: { all: '1' } });
  assert.deepEqual(
    after.body.proposals[0].changes.map(c => c.row),
    [5, 6],
  );
  // And the walk now finds them in the sheet.
  const again = await call('get_walk', { url: URL_ });
  assert.deepEqual(
    again.points.map(p => p.inSheet?.row ?? null),
    [3, 5, 6, null],
  );
  store.close();
});

test('a walk without day or collector says what to ask, and takes them as arguments', async () => {
  const { store, call, user } = await fixture();
  await call('queue_wikiloc', { url: URL_ });
  const { job } = claimJob(store);
  // A followed profile without its collector, and a title without the day.
  await saveWalk(
    store,
    {
      url: job.target,
      jobId: job.id,
      name: 'Monitoreo',
      recorded: 'septiembre 2026',
      track: [],
      waypoints: [{ ...T4, text: 'M1 Oleria gunilla macho 9:40 1m NC' }],
    },
    user,
  );
  const draft = await call('get_walk', { url: URL_ });
  assert.equal(draft.newRows.length, 0);
  assert.equal(draft.problems.length, 2);
  assert.match(draft.problems.join(' '), /pregunta la fecha/);
  assert.match(draft.problems.join(' '), /pregunta el colector/);
  const given = await call('get_walk', { url: URL_, date: '2026-09-27', collector: FCH });
  assert.equal(given.newRows.length, 1);
  assert.equal(given.newRows[0].values.Collection_date, '2026-09-27');
  assert.match((await call('queue_wikiloc', { url: 'https://example.com/123' })).error, /No Wikiloc trail link/);
  store.close();
});
