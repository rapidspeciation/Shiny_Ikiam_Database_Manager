// Monitoreo → Importar: a Wikiloc walk as a proposal in the review table (server/walks.mjs
// walkRows, the from-walk route in server/assistant.mjs), its photos read by the identifier in
// the background (server/walk-ids.mjs), and what the Wikiloc reader keeps for the day's row.
import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { addProfile, listWalks, saveWalk } from '../server/monitoring.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { bestReading, createWalkIds } from '../server/walk-ids.mjs';
import { durationMinutes } from '../tools/wikiloc/lib.mjs';
import { signIn } from './helpers/assistant.mjs';

const EPOCH = Date.UTC(1899, 11, 30);
const serial = iso => Math.round((Date.parse(`${iso}T00:00:00Z`) - EPOCH) / 864e5);
const AO = 'AO - Collector Two';
const FCH = 'FCH - Franz Chandi';
const T4 = { lat: -0.950925, lon: -77.869495 };
const monitoring = { Purpose: 'Monitoring', Collection_location: 'Ikiam', Tribe: 'Ithomiini' };
const JPEG = Buffer.from([0xff, 0xd8, 0xff, 0xd9]);

async function fixture({ identifier = null } = {}) {
  const sheets = new LocalSheets({
    Collection_data: [
      // B69 marked on 26 Sep: recaptured on the walk.
      {
        row: 2,
        values: { ...monitoring, Collector: FCH, Release_Collect: 'Mark_Released', FieldMark_ID: 'B69', SPECIES: 'Hyposcada illinissa', Subspecies_Form: 'ida', Sex: 'female', Collection_date: serial('2026-09-26') },
      },
      { row: 3, values: { ...monitoring, Collector: AO, Release_Collect: 'Collected_Preserved', SPECIES: 'Oleria gunilla', Subspecies_Form: 'lota', CAM_ID: 'CAM079943', Collection_date: serial('2026-10-05') } },
      // The same morning, a teammate's row of the butterfly marked B90.
      {
        row: 4,
        values: { ...monitoring, Collector: FCH, Release_Collect: 'Mark_Released', FieldMark_ID: 'B90', SPECIES: 'Oleria gunilla', Sex: 'male', Collection_date: serial('2026-10-14'), Collection_time: 600 / 1440 },
      },
      { row: 5, values: { Tribe: { formula: '=VLOOKUP(S5,Taxonomy_v18Jun25!J:G,1,0)' } } },
    ],
    Taxonomy_v18Jun25: [
      { row: 2, values: { species: 'Oleria gunilla', subspecies: 'Oleria gunilla lota', tribe: 'Ithomiini' } },
      { row: 3, values: { species: 'Hyposcada illinissa', subspecies: 'Hyposcada illinissa ida', tribe: 'Ithomiini' } },
      { row: 4, values: { species: 'Methona confusa', subspecies: 'Methona confusa psamathe', tribe: 'Ithomiini' } },
      { row: 5, values: { species: 'Hypothyris euclea', subspecies: 'Hypothyris euclea intermedia', tribe: 'Ithomiini' } },
    ],
    Location_data: [{ row: 2, values: { Collection_location: 'Ikiam' } }],
    SamplingDay_data: [],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Collection_data', 'Taxonomy_v18Jun25', 'Location_data', 'SamplingDay_data'] });
  const assistant = createAssistant({ store, config: { identifier } });
  const { call } = signIn(store, assistant);
  const user = { id: 'u1', username: 'franz', role: 'editor' };
  addProfile(store, { url: '23021371', collector: AO }, user);
  return { store, assistant, call, user };
}

/** The 14 Oct 2026 walk as the Wikiloc reader sends it (notes as written, photos stubbed). */
async function walk(store, user, waypoints) {
  const fetchImage = async () => ({ type: 'image/jpeg', data: JPEG });
  const { walk } = await saveWalk(
    store,
    {
      url: 'https://es.wikiloc.com/rutas-senderismo/monitoreo-14-octubre-291256112',
      date: '2026-10-14',
      name: 'Monitoreo 14 octubre, en el tramo 3 4 hay árboles caídos hacia el río. Parches',
      author: '23021371',
      started: '2026-10-14T14:18:51.000Z',
      minutes: 111,
      description: 'Árboles caídos en el tramo 3-4',
      track: [],
      waypoints,
    },
    user,
    { fetchImage },
  );
  return walk;
}
const photo = n => `https://s2.wklcdn.com/image_767/23021371/1/${n}.jpg`;
/** A proposal as the table gets it, its cells' hints read from the proposal's hint table. */
async function shown(assistant, user, id) {
  const p = (await assistant.handle({ method: 'GET', path: '/api/chat/proposals', user, query: { only: id } })).body.proposals[0];
  for (const c of p.changes)
    if (c.hints)
      c.hints = Object.fromEntries(
        Object.entries(c.hints).map(([f, h]) => {
          const entry = typeof h === 'number' ? p.hintTable[h] : h;
          // A hint comes as its descriptor: its Spanish text filled in, as the table shows it.
          const text = entry.text ?? entry.msg.key.replace(/\{(\w+)\}/g, (_, k) => [entry.msg.vars?.[k]].flat().join(', '));
          return [f, { ...entry, text }];
        }),
      );
  return p;
}

const POINTS = [
  { ...T4, text: '10:04 B75 Hembra Hyposcada illinissa ida 0.3m parches', photos: [photo(101)], time: '2026-10-14T15:05:00.000Z' },
  { ...T4, text: '10:08 B76 hembra Hyposcada illinissa ida 0.3m parches', photos: [photo(102)] },
  { ...T4, text: '10:15 Methona confusa? Hembra 1.8m parches', photos: [photo(103)] },
  { ...T4, text: 'B69 Recaptura hembra Hyposcada illinissa ida 1m parches', photos: [photo(104)], time: '2026-10-14T15:20:30.000Z' },
  { ...T4, text: '10:28 B76 macho Hypothyris euclea intermedia 1.2m parches', photos: [photo(105)] },
  { ...T4, text: '10:00 B90 macho Oleria gunilla 0.5m parches', photos: [] },
  { ...T4, text: '10:50 Oleria gunilla Male Parches 0.1m', photos: [photo(107)] },
];

test('the Wikiloc reader keeps when the walk started, how long it took and when each point was saved', async () => {
  assert.equal(durationMinutes('una hora 51 minutos'), 111);
  assert.equal(durationMinutes('31 minutos'), 31);
  assert.equal(durationMinutes('2 horas 5 minutos'), 125);
  assert.equal(durationMinutes(''), null);
  const { store, user } = await fixture();
  const saved = await walk(store, user, POINTS.slice(0, 1));
  assert.equal(saved.started, '2026-10-14T14:18:51.000Z');
  assert.equal(saved.minutes, 111);
  assert.equal(saved.description, 'Árboles caídos en el tramo 3-4');
  assert.equal(saved.waypoints[0].time, '2026-10-14T15:05:00.000Z');
  assert.equal(saved.collector, AO);
  assert.equal(listWalks(store)[0].proposalStatus, null);
  store.close();
});

test('a walk becomes one proposal: rows with their photos, notes on the cells, CAMs and tubes in walk order, the day', async () => {
  const { store, assistant, user } = await fixture();
  const w = await walk(store, user, POINTS);
  const made = await assistant.handle({
    method: 'POST',
    path: '/api/chat/proposals/from-walk',
    user,
    body: { walkId: w.id, firstCam: 'CAM079944', firstTube: 'FS90415351' },
  });
  assert.equal(made.status, 201, JSON.stringify(made.body));
  // The point already in the sheet under the teammate is still proposed, but doubtful.
  const proposal = await shown(assistant, user, made.body.proposalId);
  assert.equal(proposal.status, 'pending');
  const rows = proposal.changes.filter(c => c.sheet === 'Collection_data');
  const day = proposal.changes.find(c => c.sheet === 'SamplingDay_data');
  // In walk order (by time): 10:00 B90, 10:04 B75, 10:08 B76, 10:15 Methona, 10:20 B69, 10:28 B76, 10:50 Oleria.
  assert.deepEqual(
    rows.map(c => c.values.FieldMark_ID),
    ['B90', 'B75', 'B76', 'NA', 'B69', 'B76', 'NA'],
  );
  const byMark = mark => rows.filter(c => c.values.FieldMark_ID === mark);
  // "ida 0.3m": the subspecies and the height, not a mark A0.
  const [b75] = byMark('B75');
  assert.equal(b75.values.Flight_height, 0.3);
  assert.equal(b75.rowPhotos.length, 1);
  assert.match(b75.rowPhotos[0].url, /^api\/monitoring\/photos\/101$/);
  assert.equal(b75.hints.Transect_section.msg.key, 'T{section}: de la posición GPS del punto (a {m} m del sendero)');
  // One mark on two butterflies: both doubtful, to ask the team.
  for (const c of byMark('B76')) assert.match(c.doubts.FieldMark_ID.reason, /también está en/);
  // The recapture: a new Mark_Released row, its time from when the point was saved.
  const [b69] = byMark('B69');
  assert.equal(b69.values.Collection_time, (10 * 60 + 20) / 1440);
  assert.match(b69.hints.FieldMark_ID.text, /Recaptura de B69/);
  assert.match(b69.hints.Collection_time.text, /cuando se guardó el punto/);
  // "Methona confusa?": to verify; the preserved butterflies get the CAMs and tubes in walk order.
  const preserved = rows.filter(c => c.values.Release_Collect === 'Collected_Preserved');
  assert.deepEqual(
    preserved.map(c => [c.values.SPECIES, c.values.CAM_ID, c.values.Tube_1_id]),
    [
      ['Methona confusa', 'CAM079944', 'FS90415351'],
      ['Oleria gunilla', 'CAM079945', 'FS90415352'],
    ],
  );
  const methona = preserved[0];
  assert.equal(methona.values.ID_status, 'Complete_but_verify');
  assert.match(methona.doubts.SPECIES.reason, /duda de la especie/);
  // A butterfly the teammate already entered that morning.
  const [b90] = byMark('B90');
  assert.match(b90.doubts.Collector.reason, /Ya en la hoja ese día: B90 Oleria gunilla male, FCH \(4\)/);
  // Every note says the sex: nothing to type there.
  assert.equal(rows.some(c => c.unreadable?.Sex), false);
  // The day's row: start and end from the recording, the note left to write in English.
  assert.equal(day.values.Date, serial('2026-10-14'));
  assert.equal(day.values.Collectors_initials, 'AO');
  assert.equal(Math.round(day.values.Start_time * 1440), 9 * 60 + 18);
  assert.equal(Math.round(day.values.End_time * 1440), 9 * 60 + 18 + 111);
  assert.match(day.unreadable.Notes.reason, /en inglés/);
  assert.equal(listWalks(store).find(x => x.id === w.id).proposalStatus, 'pending');

  // Made again with other first IDs while nobody touched it: the old one is discarded.
  const again = await assistant.handle({ method: 'POST', path: '/api/chat/proposals/from-walk', user, body: { walkId: w.id, firstCam: 'CAM079950' } });
  assert.equal(again.status, 201);
  assert.notEqual(again.body.proposalId, made.body.proposalId);
  assert.equal(store.db.prepare('SELECT status FROM ai_proposals WHERE id = ?').get(made.body.proposalId).status, 'discarded');

  // Once the person typed in it, it is not made again: their table is kept.
  const edit = await assistant.handle({
    method: 'POST',
    path: `/api/chat/proposals/${again.body.proposalId}/edit`,
    user,
    body: { cells: [{ key: (await assistant.handle({ method: 'GET', path: '/api/chat/proposals', user, query: { only: again.body.proposalId } })).body.proposals[0].changes[0].key, field: 'Flight_height', value: 0.4 }] },
  });
  assert.equal(edit.status, 200, JSON.stringify(edit.body));
  const kept = await assistant.handle({ method: 'POST', path: '/api/chat/proposals/from-walk', user, body: { walkId: w.id, firstCam: 'CAM079960' } });
  assert.equal(kept.status, 409);
  assert.equal(kept.body.error.code, 'walk_proposal_edited');
  assert.equal(kept.body.error.details.proposalId, again.body.proposalId);

  // Applied: the rows are written, the recapture as a new row with the same mark.
  const applied = await assistant.handle({ method: 'POST', path: `/api/chat/proposals/${again.body.proposalId}/apply`, user, body: { requestId: randomUUID(), doubtful: 'confirm' } });
  assert.equal(applied.body.status, 'applied', JSON.stringify(applied.body));
  const written = store.db
    .prepare("SELECT values_json FROM records WHERE sheet='Collection_data' AND missing=0")
    .all()
    .map(r => JSON.parse(r.values_json));
  assert.equal(written.filter(v => v.FieldMark_ID === 'B69').length, 2);
  const sampling = store.db.prepare("SELECT values_json FROM records WHERE sheet='SamplingDay_data' AND missing=0").all();
  assert.equal(sampling.length, 1);
  store.close();
});

test('a walk without butterflies is still a day of effort', async () => {
  const { store, assistant, user } = await fixture();
  const w = await walk(store, user, []);
  const made = await assistant.handle({ method: 'POST', path: '/api/chat/proposals/from-walk', user, body: { walkId: w.id } });
  assert.equal(made.status, 201, JSON.stringify(made.body));
  const listed = await assistant.handle({ method: 'GET', path: '/api/chat/proposals', user, query: { only: made.body.proposalId } });
  assert.deepEqual(
    listed.body.proposals[0].changes.map(c => c.sheet),
    ['SamplingDay_data'],
  );
  store.close();
});

test("the identifier reads each photo in the background; its species goes beside the one typed, the person decides", async () => {
  const species = {
    101: [['Hyposcada illinissa', 0.92], ['Oleria agarista', 0.03]],
    102: [['Hyposcada illinissa', 0.95]],
    103: [['Methona confusa', 0.9]],
    104: [['Hyposcada illinissa', 0.88]],
    // A tiger look-alike: the identifier disagrees with the note, sure enough to say so.
    105: [['Mechanitis polymnia', 0.63], ['Hypothyris euclea', 0.2]],
    107: [['Oleria gunilla', 0.3], ['Oleria agarista', 0.25]],
  };
  const read = [];
  const identifier = {
    status: () => ({ installed: true }),
    identify: async ({ path, side }) => {
      const id = /(\d+)\.jpg$/.exec(path)[1];
      read.push([id, side]);
      return id === '104' ? { error: 'The identifier stopped (killed).' } : { species: species[id], subspecies: [], crop: 'success' };
    },
  };
  const { store, assistant, user } = await fixture({ identifier });
  const w = await walk(store, user, POINTS);
  const made = await assistant.handle({ method: 'POST', path: '/api/chat/proposals/from-walk', user, body: { walkId: w.id } });
  assert.equal(made.body.identification.total, 6);
  // Progress, photo by photo, while it reads.
  let status;
  for (let i = 0; i < 50; i++) {
    status = (await assistant.handle({ method: 'GET', path: `/api/chat/walks/${w.id}/identify`, user, query: {} })).body;
    if (status.read === status.total) break;
    await new Promise(r => setTimeout(r, 10));
  }
  assert.equal(status.read, 6);
  assert.ok(read.every(([, side]) => side === 'East'));
  assert.equal(status.photos.find(p => p.photo === '104').status, 'failed');
  assert.deepEqual(status.photos.find(p => p.photo === '101').species[0], ['Hyposcada illinissa', 0.92]);
  const rows = (await shown(assistant, user, made.body.proposalId)).changes;
  const at = text => rows.find(c => c.note?.includes(text));
  const hintText = (c, f) => c.hints?.[f]?.text ?? '';
  // Agrees: said how sure.
  assert.match(hintText(at('10:04 B75'), 'SPECIES'), /coincide: Hyposcada illinissa \(92 %\)/);
  // Disagrees: doubtful, with the identifier's species as the other reading; the typed one stays.
  const b76 = at('10:28 B76');
  assert.equal(b76.values.SPECIES, 'Hypothyris euclea');
  assert.deepEqual(b76.doubts.SPECIES.alternatives, ['Mechanitis polymnia']);
  assert.match(b76.doubts.SPECIES.reason, /dice Mechanitis polymnia \(63 %\), no Hypothyris euclea/);
  // Unsure: only a hint.
  assert.match(hintText(at('10:50 Oleria'), 'SPECIES'), /duda: Oleria gunilla \(30 %\), luego Oleria agarista 25 %/);
  // Asked again: only the failed photo is read again.
  read.length = 0;
  await assistant.handle({ method: 'POST', path: `/api/chat/walks/${w.id}/identify`, user, body: {} });
  for (let i = 0; i < 50; i++) {
    const now = (await assistant.handle({ method: 'GET', path: `/api/chat/walks/${w.id}/identify`, user, query: {} })).body;
    if (now.read === now.total && read.length) break;
    await new Promise(r => setTimeout(r, 10));
  }
  await new Promise(r => setTimeout(r, 20));
  assert.deepEqual(read.map(([id]) => id), ['104']);
  store.close();
});

test('the walk photos queue: one photo at a time, a restart reads again what was being read', async () => {
  const sheets = new LocalSheets({ Collection_data: [] });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Collection_data'] });
  const user = { id: 'u1', username: 'franz', role: 'editor' };
  const fetchImage = async () => ({ type: 'image/jpeg', data: JPEG });
  const { walk: w } = await saveWalk(
    store,
    { url: 'https://es.wikiloc.com/rutas-senderismo/monitoreo-1-291000001', name: 'Monitoreo', track: [], waypoints: [{ ...T4, text: 'x', photos: [photo(1), photo(2)] }] },
    user,
    { fetchImage },
  );
  let running = 0;
  let most = 0;
  const identifier = {
    identify: async () => {
      running++;
      most = Math.max(most, running);
      await new Promise(r => setTimeout(r, 5));
      running--;
      return { species: [['Oleria gunilla', 0.8]] };
    },
  };
  const seen = [];
  const ids = createWalkIds({ store, identifier, onRead: id => seen.push(id) });
  assert.equal(ids.start(w.id).total, 2);
  await ids.idle();
  assert.equal(most, 1);
  assert.deepEqual(seen, [w.id, w.id]);
  assert.equal(bestReading(ids.status(w.id).photos).species[0][0], 'Oleria gunilla');
  // Without an identifier: said, nothing queued.
  const none = createWalkIds({ store, identifier: null });
  assert.match(none.start(w.id).unavailable, /not set up/);
  // A reading cut by a restart is queued again.
  store.db.prepare("UPDATE walk_photo_ids SET status='reading'").run();
  createWalkIds({ store, identifier });
  assert.deepEqual(
    store.db.prepare('SELECT DISTINCT status FROM walk_photo_ids').all().map(r => r.status),
    ['queued'],
  );
  store.close();
});

test('a name the sheet would refuse is left to fill, with what the note wrote offered', async () => {
  const { store, assistant, user } = await fixture();
  const w = await walk(store, user, [{ ...T4, text: '11:13 1m Hipotiris unchela B63 Female', photos: [] }]);
  const made = await assistant.handle({ method: 'POST', path: '/api/chat/proposals/from-walk', user, body: { walkId: w.id } });
  assert.equal(made.status, 201, JSON.stringify(made.body));
  const [row] = (await shown(assistant, user, made.body.proposalId)).changes.filter(c => c.sheet === 'Collection_data');
  assert.equal(row.values.SPECIES, undefined);
  assert.equal(row.values.ID_status, 'To_identify');
  assert.deepEqual(row.unreadable.SPECIES.partial, ['Hipotiris unchela']);
  assert.match(row.unreadable.SPECIES.reason, /no está en la lista de la hoja/);
  store.close();
});
