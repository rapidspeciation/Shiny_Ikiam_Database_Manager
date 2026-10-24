import test from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { Store } from '../server/store.mjs';
import { applyBatch } from '../server/batch.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { importBundle, readCuration, readGallery, readWingSides } from '../server/envelope-import.mjs';
import { bumpReviewRevision } from '../server/photodata.mjs';
import { runPhotoFacts } from '../server/photo-facts.mjs';
import { CURATION_TOOLS, createCuration, curationEntries, curationSummary, runPhotoFactsTool } from '../server/photo-curation.mjs';
import { latestVerdicts, trainingLabels } from '../server/review.mjs';

const ENVELOPES = fileURLToPath(new URL('./fixtures/envelope/', import.meta.url));
const WINGS = fileURLToPath(new URL('./fixtures/wings/', import.meta.url));

// The small archive of tests/photo-facts.test.mjs: its workbook, envelopes, gallery and wing readings.
const EPOCH = Date.UTC(1899, 11, 30);
const serial = iso => Math.round((Date.parse(`${iso}T00:00:00Z`) - EPOCH) / 864e5);
const ids = JSON.parse(readFileSync(join(ENVELOPES, 'ids.json'), 'utf8'));
const preserved = (row, cam, SPECIES, more = {}) => ({
  row,
  values: { Release_Collect: 'Collected_Preserved', CAM_ID: cam, SPECIES, Collection_location: 'Ikiam', Collection_date: serial('2023-03-01'), Collector: 'AB', ...more },
});
const seed = {
  Collection_data: [
    preserved(2, 'CAM000101', 'Oleria gunilla'),
    preserved(3, 'CAM000111', 'Hypothyris anastasia'),
    preserved(4, 'CAM000105', 'Mechanitis polymnia', { Sex: 'female' }),
    preserved(5, 'CAM000106', 'Mechanitis polymnia', { Sex: 'male' }),
    // Its note says the species was changed from what the envelope shows.
    preserved(6, 'CAM000107', 'Ithomia salapia', { Subspecies_Form: 'salapia', Notes_Collection_data: '31Jan25 AB Species ID changed from Episcada sulphurea (identified by AB) to Ithomia salapia' }),
    preserved(7, 'CAM000108', 'Ithomia salapia', { Subspecies_Form: 'salapia' }),
    preserved(8, 'CAM000110', 'Oleria gunilla'),
  ],
  Taxonomy_v18Jun25: ['Episcada sulphurea', 'Ithomia salapia', 'Oleria gunilla', 'Hypothyris anastasia', 'Mechanitis polymnia'].map((species, i) => ({ row: i + 2, values: { species } })),
  Photo_links: ['CAM000105d', 'CAM000105v', 'CAM000107d', 'CAM000107v'].map((name, i) => ({
    row: i + 2,
    values: { 'Full Path': `Wild/${name}.JPG`, Name: `${name}.JPG`, Type: 'File', URL: `https://drive.google.com/file/d/${ids[name]}/view` },
  })),
};
async function fixture() {
  const store = new Store({ localMode: true }, { sheets: new LocalSheets(seed) });
  await store.sync({ sheets: Object.keys(seed) });
  importBundle(store.db, { ...readCuration(ENVELOPES, join(ENVELOPES, 'manifest.csv')), ...(await readGallery(join(ENVELOPES, 'gallery'))) });
  const wings = readWingSides([join(WINGS, 'eval'), join(WINGS, 'eval', 'new_photos_20261015')], join(ENVELOPES, 'manifest.csv'));
  importBundle(store.db, { wings: wings.wings });
  return { store, wings };
}


const kim = { id: 'kim', username: 'kim', role: 'editor' };
const byId = entries => Object.fromEntries(entries.map(e => [e.id, e]));

test('each disagreement sorted: clear when independent sources agree against one, the fix proposed', async () => {
  const { store } = await fixture();
  const e = byId(curationEntries(store));
  // The envelope's sex and the sex model agree against the database: written from them.
  const sex = e['envelope_sex:CAM000105:Collection_data'];
  assert.equal(sex.tier, 'clear');
  assert.deepEqual(sex.why, ['agree']);
  assert.deepEqual(sex.fix, { field: 'Sex', from: 'female', to: 'male' });
  assert.deepEqual(sex.evidence, { database: 'female', envelope: ['male', 0.97, 1], model: ['male', 0.93] });
  // Both envelopes show another CAM, which has its row and no photos: the files renamed to it.
  const camid = e['photo_camid:CAM000101'];
  assert.equal(camid.tier, 'clear');
  assert.deepEqual(camid.fix.drive.map(o => [o.action, o.from, o.name]), [['rename', 'CAM000101d', 'CAM000111d'], ['rename', 'CAM000101v', 'CAM000111v']]);
  // A CAM without a row in the database: proposed, for a look at the ID line.
  assert.equal(e['photo_camid:CAM000103'].tier, 'look');
  assert.deepEqual(e['photo_camid:CAM000103'].why, ['targetNoRow']);
  // The wings: a pair the wrong way round is swapped, a photo upside down turned.
  assert.deepEqual(e['photo_side:CAM000105'].fix.drive.map(o => [o.from, o.name]), [['CAM000105d', 'CAM000105v'], ['CAM000105v', 'CAM000105d']]);
  assert.deepEqual(e['photo_turn:CAM000107D'].fix.drive, [{ action: 'rotate', file: e['photo_turn:CAM000107D'].fix.drive[0].file, degrees: 180, from: 'CAM000107d' }]);
  // The model alone against the database: a person looks; its species proposed, as the list writes it.
  const gallery = e['ai_species:CAM000110'];
  assert.equal(gallery.tier, 'look');
  assert.deepEqual(gallery.fix, { field: 'SPECIES', from: 'Oleria gunilla', to: 'Hypothyris anastasia' });
  assert.deepEqual([e['photo_side:CAM000105'].folder, sex.year], ['Wild', 2023]);
});

test('the model agreeing with the database sends the envelope for a look; the sex model alone is its own kind', async () => {
  const { store } = await fixture();
  store.db.prepare("UPDATE photo_predictions SET sex='female', sex_conf=0.95, sex_supported=1 WHERE cam='CAM000105'").run();
  store.db.prepare("INSERT OR REPLACE INTO photo_predictions(cam,species_json,sex,sex_conf,sex_supported,source,imported_at) VALUES('CAM000106',?,'female',0.96,1,'test','now')").run(JSON.stringify([['Mechanitis polymnia', 0.9]]));
  bumpReviewRevision(store.db);
  const e = byId(curationEntries(store));
  assert.equal(e['envelope_sex:CAM000105:Collection_data'].tier, 'look');
  assert.deepEqual(e['envelope_sex:CAM000105:Collection_data'].why, ['modelForDatabase']);
  // CAM000106: no envelope sex, the model sure of another sex than the database.
  const aisex = e['ai_sex:CAM000106'];
  assert.deepEqual([aisex.kind, aisex.tier, aisex.fix.to], ['aisex', 'look', 'female']);
  assert.equal(runPhotoFacts(store, { kind: 'aisex' }).items[0].cam, 'CAM000106');
});

test('photo_facts summary: by tier, kind, folder and year; a section lists tiers and fixes', async () => {
  const { store } = await fixture();
  const s = runPhotoFactsTool(store, { summary: true });
  assert.equal(s.total, 7);
  assert.deepEqual(s.tiers, { clear: 5, reason: 0, look: 2 });
  assert.deepEqual(s.kinds.turn, { n: 2, clear: 2, reason: 0, look: 0 });
  assert.deepEqual(s.folders.map(f => f.name), ['not in Photo_links', 'Wild']);
  assert.ok(JSON.stringify(s).length < 2000, 'a short answer');
  const section = runPhotoFactsTool(store, { tier: 'look' });
  assert.deepEqual(section.items.map(i => i.id).sort(), ['ai_species:CAM000110', 'photo_camid:CAM000103']);
  assert.equal(section.items[0].recordId, undefined);
  assert.equal(runPhotoFactsTool(store, { folder: 'wild', kind: 'wings' }).total, 2);
  assert.match(runPhotoFactsTool(store, { tier: 'maybe' }).error, /^tier is/);
  // Without a section's filters, photo_facts answers as before.
  assert.deepEqual(Object.keys(runPhotoFactsTool(store, {}).counts), ['species', 'sex', 'gallery', 'aisex', 'camid', 'wings']);
  assert.equal(curationSummary(store, { year: '1999' }).total, 0);
});

test('show_photo_fixes: the sure ones ticked, exact ID box, partial choices and retry-safe Drive plans', async () => {
  const { store } = await fixture();
  assert.equal(CURATION_TOOLS[0].function.name, 'show_photo_fixes');
  const cards = [];
  const planned = [];
  const updates = [];
  const curation = createCuration({
    store,
    show: (card, context) => (cards.push({ card, context }), { card: `card-${cards.length}`, link: 'https://app/#/c' }),
    organize: async (ops, user, proposal) => (planned.push({ ops, user, proposal }), { plan: 'plan-1', link: 'https://app/#/p' }),
    update: (id, card, status) => updates.push({ id, card, status }),
  });
  const out = curation.run({ tier: 'clear' }, kim, { threadId: 't1' });
  assert.deepEqual([out.shown, out.ticked, out.card], [5, 5, 'card-1']);
  const { card } = cards[0];
  assert.equal(card.type, 'curation');
  const sex = card.entries.find(e => e.kind === 'sex');
  assert.equal(sex.photos.envelope.name.startsWith('CAM000105'), true);
  assert.ok(sex.photos.files.length >= 2);
  assert.ok(card.entries.find(e => e.kind === 'camid').idLines.every(l => l.read === 'CAM000111'));
  assert.ok(card.entries.find(e => e.kind === 'camid').idLines.every(l => l.box?.length === 4));
  // Chosen entries: the AI's value and note; a kind with nothing now is said.
  const chosen = curation.run({ items: [{ cam: 'CAM000110', kind: 'gallery', value: 'Oleria gunilla', note: 'wings look like Oleria', check: true }, { cam: 'CAM000106', kind: 'sex' }] }, kim, { threadId: 't1' });
  assert.equal(chosen.shown, 1);
  assert.match(chosen.notFound[0], /^CAM000106 sex: no such disagreement/);
  assert.deepEqual([cards[1].card.entries[0].fix.to, cards[1].card.entries[0].note, cards[1].card.entries[0].checked], ['Oleria gunilla', 'wings look like Oleria', true]);

  // «Aplicar»: the sex written, the turn and the rename planned, the swap rejected.
  const proposal = { id: 'p1', thread_id: 't1', sync_json: JSON.stringify(card) };
  const ids = card.entries.map(e => e.id);
  const turn = ids.find(id => id.startsWith('photo_turn:CAM000107'));
  const result = await curation.apply(proposal, { accept: ['envelope_sex:CAM000105:Collection_data', turn, 'photo_camid:CAM000101'], keep: ['photo_side:CAM000105'] }, kim);
  assert.equal(result.status, 'partial');
  assert.equal(result.written, 1);
  assert.equal(store.getRecord(sex.recordId).values.Sex, 'male');
  assert.deepEqual(planned.flatMap(p => p.ops).map(o => [o.action, o.degrees ?? o.name]), [['rotate', 180], ['rename', 'CAM000111d'], ['rename', 'CAM000111v']]);
  assert.equal(planned[0].ops[0].from, undefined, 'the operations as organize_photos takes them');
  assert.equal(result.drive.length, 2);
  const verdicts = latestVerdicts(store.db);
  assert.deepEqual(
    ['envelope_sex:CAM000105:Collection_data', turn, 'photo_camid:CAM000101', 'photo_side:CAM000105'].map(id => verdicts.get(id)?.verdict),
    ['applied', 'pending', 'pending', 'rejected'],
  );
  assert.equal(updates[0].status, 'pending');
  assert.deepEqual(updates[0].card.entries.filter(e => e.decision).map(e => e.decision).sort(), ['accepted', 'kept', 'planned', 'planned']);
  // A plan is pending until its Drive changes finish. Keeping current is a training label.
  const left = curationEntries(store).map(e => e.id);
  assert.ok(!left.includes('photo_side:CAM000105') && left.includes(turn) && left.includes('photo_camid:CAM000101'));
  assert.ok(!left.includes('envelope_sex:CAM000105:Collection_data'));
  assert.match(trainingLabels(store), /"kind":"photo_side","verdict":"rejected"/);
  assert.doesNotMatch(trainingLabels(store), /"kind":"photo_turn","verdict":"applied"/);
  const retry = await curation.apply({ ...proposal, sync_json: JSON.stringify(updates[0].card) }, { accept: [turn] }, kim);
  assert.equal(retry.planned, 1);
  assert.equal(planned.length, 1, 'retry reuses the existing plan');
  // The database's value chosen: nothing written, the model's reading rejected.
  const again = await curation.apply({ id: 'p2', thread_id: 't1', sync_json: JSON.stringify(cards[1].card) }, { accept: [cards[1].card.entries[0].id] }, kim);
  assert.deepEqual([again.written, again.kept], [0, 1]);
  assert.deepEqual([latestVerdicts(store.db).get('ai_species:CAM000110').verdict, latestVerdicts(store.db).get('ai_species:CAM000110').value], ['rejected', 'Oleria gunilla']);
  // A cell changed since the card was made is left out.
  const stale = await curation.apply(proposal, { accept: ['envelope_sex:CAM000105:Collection_data'] }, kim);
  assert.equal(stale.written, 0);
  assert.equal((await curation.apply(proposal, {}, kim)).code, 'nothing');
});

test('defer is not a verdict; stale evidence and invalid choices cannot write or settle an issue', async () => {
  const { store } = await fixture();
  const cards = [];
  const curation = createCuration({ store, show: card => (cards.push(card), { card: 'shown' }), update: () => {} });
  curation.run({ tier: 'clear' }, kim, { threadId: 't1' });
  const card = cards[0];
  const sex = card.entries.find(e => e.kind === 'sex');
  const proposal = { id: 'p-stale', thread_id: 't1', sync_json: JSON.stringify(card) };
  const later = await curation.apply(proposal, { defer: [sex.id] }, kim);
  assert.equal(later.deferred, 1);
  assert.equal(later.entries.find(e => e.id === sex.id).decision, 'deferred');
  assert.equal(latestVerdicts(store.db).has(sex.id), false);
  assert.ok(curationEntries(store).some(e => e.id === sex.id));

  store.db.prepare("UPDATE photo_predictions SET sex='female', sex_conf=0.95, sex_supported=1 WHERE cam='CAM000105'").run();
  bumpReviewRevision(store.db);
  const stale = await curation.apply(proposal, { accept: [sex.id] }, kim);
  assert.equal(stale.written, 0);
  assert.match(stale.skipped[0], /evidence changed/);
  assert.equal(store.getRecord(sex.recordId).values.Sex, 'female');
  const duplicate = await curation.apply(proposal, { accept: [sex.id], keep: [sex.id] }, kim);
  assert.equal(duplicate.code, 'invalid_selection');
  assert.equal(latestVerdicts(store.db).has(sex.id), false);
});

test('queued sheet writes stay pending, then the same request confirms once; failures remain retryable', async () => {
  const { store } = await fixture();
  const cards = [];
  const updates = [];
  let failPlan = true;
  const curation = createCuration({
    store,
    show: card => (cards.push(card), { card: 'shown' }),
    update: (_id, card) => updates.push(card),
    organize: async () => failPlan ? { error: 'Drive unavailable' } : { status: 'planned', plan: 'later-plan', link: '#plan' },
  });
  curation.run({ tier: 'clear' }, kim, { threadId: 't1' });
  const card = cards[0];
  const sex = card.entries.find(e => e.kind === 'sex');
  const turn = card.entries.find(e => e.kind === 'turn');
  let queued = true;
  store.outbox = {
    byRequest: () => null,
    shouldQueue: () => queued,
    enqueue: () => ({ status: 'queued', outboxId: 'waiting-1' }),
  };
  const first = await curation.apply({ id: 'p-queue', sync_json: JSON.stringify(card) }, { accept: [sex.id, turn.id] }, kim);
  assert.deepEqual([first.queued, first.written, first.planned, first.failed.length], [1, 0, 0, 1]);
  assert.equal(store.getRecord(sex.recordId).values.Sex, 'female');
  assert.equal(latestVerdicts(store.db).get(sex.id).verdict, 'pending');
  assert.equal(latestVerdicts(store.db).has(turn.id), false);
  assert.equal(first.entries.find(e => e.id === turn.id).decision, undefined);

  const cannotKeep = await curation.apply({ id: 'p-queue', sync_json: JSON.stringify(updates.at(-1)) }, { keep: [sex.id] }, kim);
  assert.match(cannotKeep.skipped[0], /queued/);
  assert.equal(latestVerdicts(store.db).get(sex.id).verdict, 'pending');
  queued = false;
  store.outbox = null;
  failPlan = false;
  const retry = await curation.apply({ id: 'p-queue', sync_json: JSON.stringify(updates.at(-1)) }, { accept: [sex.id, turn.id] }, kim);
  assert.deepEqual([retry.written, retry.planned, retry.failed.length], [1, 1, 0]);
  assert.equal(latestVerdicts(store.db).get(sex.id).verdict, 'applied');
  assert.equal(store.getRecord(sex.recordId).values.Sex, 'male');
  assert.equal(trainingLabels(store).includes('"kind":"envelope_sex","verdict":"pending"'), false);
});

test('Drive verdict waits for every file operation; terminal progress settles only a complete plan', async () => {
  const { store } = await fixture();
  store.db.exec("CREATE TABLE drive_changes(id TEXT PRIMARY KEY, ops_json TEXT NOT NULL, undone_at TEXT)");
  const proposals = new Map();
  let planNumber = 0;
  const curation = createCuration({
    store,
    show: card => { proposals.set('p-drive', { sync_json: JSON.stringify(card) }); return { card: 'shown' }; },
    getProposal: id => proposals.get(id),
    update: (id, card) => proposals.set(id, { sync_json: JSON.stringify(card) }),
    organize: async () => ({ status: 'planned', plan: `plan-${++planNumber}`, link: '#plan' }),
  });
  curation.run({ tier: 'clear' }, kim, { threadId: 't1' });
  const initial = JSON.parse(proposals.get('p-drive').sync_json);
  const turn = initial.entries.find(e => e.kind === 'turn');
  const side = initial.entries.find(e => e.kind === 'side');
  const proposal = () => ({ id: 'p-drive', sync_json: proposals.get('p-drive').sync_json });
  const planned = await curation.apply(proposal(), { accept: [turn.id, side.id] }, kim);
  assert.equal(planned.planned, 2);
  assert.deepEqual(new Set(planned.drive.map(d => d.plan)), new Set(['plan-1']), 'one card for the selected file fixes');
  const turnOp = turn.fix.drive[0];
  store.db.prepare('INSERT INTO drive_changes(id,ops_json) VALUES(?,?)').run('plan-1', JSON.stringify([{ id: turnOp.file, action: 'rotate', degrees: turnOp.degrees }]));
  assert.deepEqual(curation.reconcileDrive('plan-1', { state: 'done', total: 3, written: 1, failed: 2 }), { settled: 1, unresolved: 1 });
  assert.equal(latestVerdicts(store.db).get(turn.id).verdict, 'applied');
  assert.equal(JSON.parse(proposals.get('p-drive').sync_json).entries.find(e => e.id === turn.id).decision, 'accepted');
  assert.deepEqual(curation.reconcileDrive('plan-1', { state: 'done', total: 1, written: 1, failed: 0 }), { settled: 0, unresolved: 0 });
  assert.equal(latestVerdicts(store.db).get(side.id).verdict, 'pending');
  assert.equal(JSON.parse(proposals.get('p-drive').sync_json).entries.find(e => e.id === side.id).decision, null);
  assert.ok(curationEntries(store).some(e => e.id === side.id));
});

test('failed outbox item can be retried with fresh evidence and a new request ID', async () => {
  const { store } = await fixture();
  let card;
  let updated;
  const curation = createCuration({ store, show: value => (card = value, { card: 'shown' }), update: (_id, value) => (updated = value) });
  curation.run({ tier: 'clear' }, kim, { threadId: 't1' });
  const sex = card.entries.find(e => e.kind === 'sex');
  store.outbox = { byRequest: () => null, shouldQueue: () => true, enqueue: () => ({ status: 'queued', outboxId: 'failed-1' }) };
  await curation.apply({ id: 'p-failed-outbox', sync_json: JSON.stringify(card) }, { accept: [sex.id] }, kim);
  const oldId = JSON.parse(latestVerdicts(store.db).get(sex.id).snapshot_json).progress.requestId;
  store.outbox = {
    byRequest: requestId => requestId === oldId ? { id: 'failed-1', status: 'failed', updated_at: '2026-10-21T00:00:00Z' } : null,
    shouldQueue: () => false,
    lookEveryMs: 10,
  };
  const retried = await curation.apply({ id: 'p-failed-outbox', sync_json: JSON.stringify(updated) }, { accept: [sex.id] }, kim);
  assert.equal(retried.written, 1);
  assert.equal(latestVerdicts(store.db).get(sex.id).verdict, 'applied');
  assert.equal(store.getRecord(sex.recordId).values.Sex, 'male');
});

test('queue refresh confirms or reports terminal outcomes without writing again', async () => {
  const { store } = await fixture();
  let card;
  let updated;
  const curation = createCuration({ store, show: value => (card = value, { card: 'shown' }), update: (_id, value) => (updated = value) });
  curation.run({ tier: 'clear' }, kim, { threadId: 't1' });
  const sex = card.entries.find(e => e.kind === 'sex');
  store.outbox = { byRequest: () => null, shouldQueue: () => true, enqueue: () => ({ status: 'queued', outboxId: 'waiting-1' }) };
  const proposal = () => ({ id: 'p-refresh', sync_json: JSON.stringify(updated ?? card) });
  await curation.apply(proposal(), { accept: [sex.id] }, kim);
  const requestId = JSON.parse(latestVerdicts(store.db).get(sex.id).snapshot_json).progress.requestId;
  store.outbox = null;
  await applyBatch(store, { requestId, edits: [{ id: sex.recordId, values: { Sex: 'male' }, expected: { Sex: 'female' } }], reason: 'queued save now completed' }, kim, { source: 'ai_approved' });
  const actions = store.db.prepare('SELECT count(*) n FROM actions').get().n;
  store.outbox = { byRequest: id => id === requestId ? { status: 'done', result_json: JSON.stringify({ status: 'verified' }) } : null };
  const refreshed = curation.refreshQueued(proposal());
  assert.deepEqual(refreshed.result, { confirmed: 1, failed: 0, queued: 0 });
  assert.equal(refreshed.entries.find(e => e.id === sex.id).decision, 'accepted');
  assert.equal(latestVerdicts(store.db).get(sex.id).verdict, 'applied');
  assert.equal(store.db.prepare('SELECT count(*) n FROM actions').get().n, actions, 'refresh is status-only');
  assert.deepEqual(curation.refreshQueued(proposal()).result, { confirmed: 0, failed: 0, queued: 0 });

  const second = await fixture();
  let failedCard;
  let failedUpdate;
  const failedCuration = createCuration({ store: second.store, show: value => (failedCard = value, { card: 'shown' }), update: (_id, value) => (failedUpdate = value) });
  failedCuration.run({ tier: 'clear' }, kim, { threadId: 't1' });
  const failedSex = failedCard.entries.find(e => e.kind === 'sex');
  second.store.outbox = { byRequest: () => null, shouldQueue: () => true, enqueue: () => ({ status: 'queued', outboxId: 'failed-1' }) };
  await failedCuration.apply({ id: 'p-refresh-fail', sync_json: JSON.stringify(failedCard) }, { accept: [failedSex.id] }, kim);
  const failedId = JSON.parse(latestVerdicts(second.store.db).get(failedSex.id).snapshot_json).progress.requestId;
  second.store.outbox = { byRequest: id => id === failedId ? { id: 'failed-1', status: 'failed', updated_at: '2026-10-21T00:00:00Z', error_json: JSON.stringify({ message: 'sheet conflict' }) } : null };
  const failed = failedCuration.refreshQueued({ id: 'p-refresh-fail', sync_json: JSON.stringify(failedUpdate) });
  assert.deepEqual(failed.result, { confirmed: 0, failed: 1, queued: 0 });
  assert.equal(failed.entries.find(e => e.id === failedSex.id).decision, null);
  assert.equal(latestVerdicts(second.store.db).get(failedSex.id).verdict, 'pending');
  assert.equal(second.store.getRecord(failedSex.recordId).values.Sex, 'female');
});

test('refresh clears an expired, unapplied Drive plan so its entry can be planned again', async () => {
  const { store } = await fixture();
  const proposals = new Map();
  let plans = 0;
  const curation = createCuration({
    store,
    show: card => (proposals.set('p-expired', { sync_json: JSON.stringify(card) }), { card: 'shown' }),
    getProposal: id => proposals.get(id),
    planAvailable: () => false,
    update: (id, card) => proposals.set(id, { sync_json: JSON.stringify(card) }),
    organize: async () => ({ status: 'planned', plan: `expired-${++plans}`, link: '#expired' }),
  });
  curation.run({ tier: 'clear' }, kim, { threadId: 't1' });
  const turn = JSON.parse(proposals.get('p-expired').sync_json).entries.find(e => e.kind === 'turn');
  const proposal = () => ({ id: 'p-expired', sync_json: proposals.get('p-expired').sync_json });
  await curation.apply(proposal(), { accept: [turn.id] }, kim);
  const refreshed = curation.refreshQueued(proposal());
  assert.equal(refreshed.result.failed, 1);
  assert.equal(refreshed.entries.find(e => e.id === turn.id).decision, null);
  assert.equal(JSON.parse(latestVerdicts(store.db).get(turn.id).snapshot_json).progress.type, 'drive_failed');
  assert.equal((await curation.apply(proposal(), { accept: [turn.id] }, kim)).planned, 1);
  assert.equal(plans, 2);
});

test('a card case carries the envelope as read and the database rows with their notes, for the card to show beside it', async () => {
  const { store } = await fixture();
  const cards = [];
  const curation = createCuration({ store, show: card => (cards.push(card), { card: 'c' }) });
  curation.run({ items: [{ cam: 'CAM000105', kind: 'sex' }, { cam: 'CAM000101', kind: 'camid' }, { cam: 'CAM000107', kind: 'turn' }] }, kim, { threadId: 't' });
  const by = Object.fromEntries(cards[0].entries.map(e => [e.kind, e]));
  const [sex] = by.sex.records;
  assert.deepEqual([sex.cam, sex.envelope.sex, sex.database[0].sex], ['CAM000105', ['male', 0.97], 'female']);
  // A CAMID case: the envelope against the butterfly it names, then the file's own row.
  assert.deepEqual(by.camid.records.map(r => [r.cam, !!r.envelope, !!r.named]), [['CAM000111', true, true], ['CAM000101', false, false]]);
  // The whole note, and the species change it records.
  const [turned] = by.turn.records;
  assert.match(turned.database[0].note, /Species ID changed from Episcada sulphurea/);
  assert.equal(turned.database[0].notes[0].from, 'Episcada sulphurea');
});
