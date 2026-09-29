import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash, randomUUID } from 'node:crypto';
import { mkdtempSync, readFileSync, readdirSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { checkData } from '../server/checks.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { importBundle, parseCsv, readCuration, readGallery } from '../server/envelope-import.mjs';
import { agreedFixes, latestVerdicts, reviewPage, setVerdicts, trainingLabels } from '../server/review.mjs';
import { createPhotoService } from '../server/photos.mjs';
import { photoIndex, photoName } from '../server/photodata.mjs';
import { predictionDiffers } from '../server/taxonomy.mjs';
import { createApp } from '../server/index.mjs';

const DIR = fileURLToPath(new URL('./fixtures/envelope/', import.meta.url));
const ids = JSON.parse(readFileSync(join(DIR, 'ids.json'), 'utf8'));
const EPOCH = Date.UTC(1899, 11, 30);
const serial = iso => Math.round((Date.parse(`${iso}T00:00:00Z`) - EPOCH) / 864e5);
const fileId = name => ids[name] ?? `1${createHash('sha1').update(name).digest('hex').slice(0, 32)}`;
const link = name => ({ values: { Name: `${name}.JPG`, Type: 'image/jpeg', URL: `https://drive.google.com/file/d/${fileId(name)}/view` } });
const preserved = (row, cam, SPECIES, more = {}) => ({
  row,
  values: {
    Release_Collect: 'Collected_Preserved',
    CAM_ID: cam,
    Tube_1_id: `FS0000${row}${row}`,
    SPECIES,
    Collection_date: serial('2023-03-01'),
    Collector: 'FCH - Franz Chandi',
    ...more,
  },
});
const seed = {
  Collection_data: [
    preserved(2, 'CAM000101', 'Oleria gunilla'),
    preserved(3, 'CAM000111', 'Hypothyris anastasia'),
    preserved(4, 'CAM000105', 'Mechanitis polymnia', { Sex: 'female' }),
    preserved(5, 'CAM000106', 'Mechanitis polymnia', { Sex: 'male' }),
    preserved(6, 'CAM000107', 'Ithomia salapia salapia', { Collector: 'AA - Ana Andrade' }),
    preserved(7, 'CAM000108', 'Ithomia salapia salapia', { Collector: 'AA - Ana Andrade' }),
    preserved(8, 'CAM000109', 'Oleria gunilla', { Photo_dorsal: 'Not Found', Photo_ventral: 'Not Found' }),
    // Its tube is the one of CAM000104: a repeat issue, shown with its photos.
    preserved(9, 'CAM000110', 'Oleria gunilla', { Tube_1_id: 'FS00001010' }),
    preserved(10, 'CAM000104', 'Oleria gunilla'),
  ],
  Taxonomy_v18Jun25: ['Episcada sulphurea', 'Ithomia salapia salapia', 'Oleria gunilla', 'Hypothyris anastasia', 'Mechanitis polymnia'].map(
    (species, i) => ({ row: i + 2, values: { species } }),
  ),
  // CAM000102, 103 and 106 are known only from the manifest (as photos renamed or not yet listed).
  Photo_links: [
    'CAM000101d',
    'CAM000101v',
    'CAM000111d',
    'CAM000111v',
    'CAM000105d',
    'CAM000105v',
    'CAM000107d',
    'CAM000107v',
    'CAM000108d',
    'CAM000108v',
    'CAM000110d',
    'CAM000110v',
    'CAM000104d',
    'CAM000104v',
  ].map((name, i) => ({ row: i + 2, ...link(name) })),
};

async function fixture() {
  const store = new Store({ localMode: true }, { sheets: new LocalSheets(seed) });
  await store.sync({ sheets: Object.keys(seed) });
  const bundle = { ...readCuration(DIR, join(DIR, 'manifest.csv')), ...(await readGallery(join(DIR, 'gallery'))) };
  importBundle(store.db, bundle);
  return { store, bundle };
}
const ana = { id: 'u1', username: 'ana', displayName: 'Ana', role: 'editor' };
const issueOf = (store, kind, cam) => checkData(store, { kind, limit: 500 }).issues.find(i => i.cam === cam);

test('the curation import reads readings, full text, flags with their decisions, wing boxes and predictions, twice the same', async () => {
  assert.deepEqual(parseCsv('a,b\n"x, ""y""",2\r\n'), [{ a: 'x, "y"', b: '2' }]);
  assert.deepEqual(photoName('CAM070046v (2).JPG'), { cam: 'CAM070046', view: 'ventral', stem: 'CAM070046v (2)' });
  assert.equal(photoName('CAM074081v2').view, 'ventral');
  assert.equal(photoName('IMG_2231.jpg'), null);
  const { store, bundle } = await fixture();
  assert.equal(bundle.readings.length, 12);
  const read = bundle.readings.find(r => r.name === 'CAM000101d');
  assert.equal(read.fileId, ids.CAM000101d);
  assert.deepEqual(read.text[1], ['Oleria gunilla', 0.98]);
  // Decisions from corrections.csv, with who took them; the flag the dorsal/ventral reader raised stays undecided.
  const flag = id => bundle.flags.find(f => f.id === id);
  assert.equal(flag('envelope-not-file|CAM000101|envelope-not-file:all-photos').decision, 'file-name-wrong');
  assert.equal(flag('envelope-not-file|CAM000101|envelope-not-file:all-photos').target, 'CAM000111');
  assert.equal(flag('envelope-not-file|CAM000102|envelope-not-file:high').decidedBy, 'Franz');
  assert.equal(flag('species|CAM000108|species:both-photos').decidedBy, 'batch of 2 reviewed');
  assert.equal(flag('dv-disagree|CAM000110|dv-disagree:confident').decision, null);
  assert.deepEqual(bundle.wingBoxes.find(b => b[0] === 'CAM000105d')[1], [0.1, 0.1, 0.7, 0.6]);
  const prediction = bundle.predictions.find(p => p.cam === 'CAM000105');
  assert.deepEqual(prediction.species[0], ['Mechanitis polymnia', 0.9]);
  assert.equal(prediction.sexSupported, true);
  assert.equal(JSON.stringify(bundle).includes('model_meta'), false);
  const before = checkData(store, { limit: 500 }).total;
  // Importing the same data again leaves the same rows (and the same issues).
  assert.deepEqual(importBundle(store.db, bundle), { readings: 12, flags: 8, wingBoxes: 2, predictions: 2 });
  assert.equal(store.db.prepare('SELECT count(*) n FROM photo_flags').get().n, 8);
  assert.equal(checkData(store, { limit: 500 }).total, before);
  // Photo_links first; the manifest fills the photos it does not list.
  const index = photoIndex(store);
  assert.deepEqual(index.byCam.get('CAM000101').dorsal, [ids.CAM000101d]);
  assert.equal(index.files.get(ids.CAM000106d).source, 'manifest');
  store.close();
});

test('photo checks: envelope CAM, extra photos, envelope sex and species (by batch), photos missing, the gallery AI', async () => {
  const { store } = await fixture();
  const out = checkData(store, { limit: 500 });
  assert.deepEqual(
    Object.fromEntries(['photo_camid', 'photo_extra', 'envelope_sex', 'envelope_species', 'photo_missing', 'ai_species'].map(k => [k, out.counts[k]])),
    { photo_camid: 1, photo_extra: 1, envelope_sex: 1, envelope_species: 2, photo_missing: 2, ai_species: 1 },
  );
  // A rename in Drive, not a sheet change: a task, with the envelope crop of the photo that shows the other CAM.
  const rename = issueOf(store, 'photo_camid', 'CAM000101');
  assert.equal(rename.id, 'photo_camid:CAM000101');
  assert.equal(rename.fix, undefined);
  assert.deepEqual([rename.task.type, rename.task.from, rename.task.to], ['rename', 'CAM000101', 'CAM000111']);
  assert.equal(rename.strength, 'fuerte');
  assert.equal(rename.curation.decidedBy, 'blind review');
  assert.deepEqual(rename.photos.dorsal, [ids.CAM000101d]);
  assert.deepEqual(rename.photos.envelope, { fileId: ids.CAM000101d, name: 'CAM000101d', bbox: [0.0625, 0.1667, 0.3125, 0.6667], turned: 0, aspect: 1.3333 });
  assert.match(rename.envelopeText, /Oleria gunilla/);
  assert.equal(rename.envelopeCamid, 'CAM000111');
  assert.equal(rename.related[0].row, 3);
  assert.deepEqual(rename.relatedPhotos.dorsal, [fileId('CAM000111d')]);
  // Reading errors already decided are not raised again (CAM000102, CAM000106).
  assert.equal(out.issues.some(i => i.cam === 'CAM000102' || (i.cam === 'CAM000106' && i.kind === 'envelope_sex')), false);
  // Photos of CAM000104 filed as CAM000103, which has no row.
  const extra = issueOf(store, 'photo_extra', 'CAM000103');
  assert.equal(extra.row, null);
  assert.equal(extra.sheet, 'Photo_links');
  assert.equal(extra.task.to, 'CAM000104');
  // Sex: a strong reading, so a fix ready for propose_changes; the wing box goes with each photo.
  const sex = issueOf(store, 'envelope_sex', 'CAM000105');
  assert.deepEqual(sex.fix, { recordId: sex.recordId, values: { Sex: 'male' } });
  assert.equal(sex.ocr.read, 'male');
  assert.deepEqual(sex.photos.files[ids.CAM000105d].wings, [0.1, 0.1, 0.7, 0.6]);
  assert.equal(sex.prediction.sex.sex, 'male');
  assert.deepEqual(sex.who, ['FCH - Franz Chandi']);
  assert.equal(sex.date, '2023-03-01');
  // Species: the doubled reading of both photos read once, both envelopes of the batch in one group.
  const species = issueOf(store, 'envelope_species', 'CAM000107');
  assert.equal(species.ocr.read, 'Episcada sulphurea');
  assert.deepEqual(species.fix.values, { SPECIES: 'Episcada sulphurea' });
  assert.equal(species.group.size, 2);
  assert.equal(issueOf(store, 'envelope_species', 'CAM000108').group.key, species.group.key);
  // No photos in Photo_links (nor in the manifest) for one view or both.
  assert.match(issueOf(store, 'photo_missing', 'CAM000109').problem, /sin foto dorsal ni ventral/);
  assert.match(issueOf(store, 'photo_missing', 'CAM000106').problem, /sin foto ventral/);
  // The gallery's rule: the recorded binomial against the model's top species.
  const ai = issueOf(store, 'ai_species', 'CAM000110');
  assert.deepEqual(ai.ai, { recorded: 'Oleria gunilla', predicted: 'Hypothyris anastasia', confidence: 0.95 });
  assert.equal(ai.strength, 'fuerte');
  assert.equal(predictionDiffers({ SPECIES: 'Mechanitis polymnia polymnia' }, { species: [['Mechanitis polymnia', 0.9]] }), false);
  // Issues of the other kinds about a butterfly with photos show them too.
  const repeat = out.issues.find(i => i.kind === 'repeat' && i.row === 9);
  assert.equal(repeat.cam, 'CAM000110');
  assert.deepEqual(repeat.photos.ventral, [fileId('CAM000110v')]);
  assert.equal(repeat.prediction.species[0][0], 'Hypothyris anastasia');
  store.close();
});

test('verdicts: last one counts, batches, other values, training labels, agreed fixes applied from one proposal', async () => {
  const { store } = await fixture();
  const assistant = createAssistant({ store, config: { claude: {} } });
  store.db
    .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u1','ana','Ana','editor','s','h',1,'2026-01-01')")
    .run();
  const token = 'token-for-ana';
  store.db
    .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
    .run(createHash('sha256').update(token).digest('hex'), 'u1');
  const call = async (name, args) => {
    const out = await assistant.mcp(
      { authorization: `Bearer ${token}` },
      { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } },
    );
    return JSON.parse(out.body.result.content[0].text);
  };
  const sex = issueOf(store, 'envelope_sex', 'CAM000105');
  const species = issueOf(store, 'envelope_species', 'CAM000107');
  const ai = issueOf(store, 'ai_species', 'CAM000110');
  // Rejected first, then accepted: the last verdict counts, both are kept.
  setVerdicts(store, { ids: [sex.id], verdict: 'rejected' }, ana);
  setVerdicts(store, { ids: [sex.id], verdict: 'accepted', comment: 'se ve ♂ en la foto' }, ana);
  assert.equal(latestVerdicts(store.db).get(sex.id).verdict, 'accepted');
  assert.equal(store.db.prepare('SELECT count(*) n FROM issue_verdicts WHERE issue_id=?').get(sex.id).n, 2);
  assert.equal(setVerdicts(store, { group: species.group.key, verdict: 'accepted' }, ana).saved, 2);
  setVerdicts(store, { ids: [ai.id], verdict: 'other', value: 'Hypothyris anastasia' }, ana);
  setVerdicts(store, { ids: ['photo_camid:CAM000101'], verdict: 'rejected', comment: 'el sobre está corregido' }, ana);
  setVerdicts(store, { ids: ['photo_extra:CAM000103'], verdict: 'accepted' }, ana);
  assert.throws(() => setVerdicts(store, { ids: [ai.id], verdict: 'other' }, ana), { code: 'VALUE_REQUIRED' });
  assert.throws(() => setVerdicts(store, { ids: [sex.id], verdict: 'applied' }, ana), { code: 'NOT_A_TASK' });
  assert.throws(() => setVerdicts(store, { ids: ['nope'], verdict: 'accepted' }, ana), { code: 'ISSUE_NOT_FOUND' });
  assert.throws(() => setVerdicts(store, { ids: [sex.id], verdict: 'maybe' }, ana), { code: 'INVALID_VERDICT' });

  // The tab's page: filters by status, person and date; counts; what waits to be applied.
  const accepted = reviewPage(store, { status: 'accepted' });
  assert.equal(accepted.total, 4);
  assert.deepEqual(accepted.agreed, { fixes: 4, tasks: 1 });
  assert.equal(accepted.issues.find(i => i.id === sex.id).verdict.user, 'Ana');
  const table = accepted.issues.find(i => i.id === sex.id).table;
  assert.deepEqual(table.compare, ['Sex']);
  assert.equal(table.rows[0].values.Sex, 'female');
  assert.equal(reviewPage(store, { status: 'accepted', person: 'AA' }).total, 2);
  assert.equal(reviewPage(store, { status: 'accepted', from: '2024-01-01' }).total, 0);
  assert.equal(reviewPage(store, { kind: 'envelope_species', status: 'all' }).issues[0].group.size, 2);
  assert.throws(() => reviewPage(store, { status: 'x' }), { code: 'INVALID_STATUS' });

  // Rejected readings are training labels.
  const labels = trainingLabels(store).trim().split('\n').map(l => JSON.parse(l));
  const rejected = labels.find(l => l.kind === 'photo_camid');
  assert.equal(rejected.verdict, 'rejected');
  assert.equal(rejected.envelopeCamid, 'CAM000111');
  assert.equal(rejected.envelope.fileId, ids.CAM000101d);

  // T3: "aplica las correcciones acordadas".
  const tools = (await assistant.mcp({ authorization: `Bearer ${token}` }, { jsonrpc: '2.0', id: 1, method: 'tools/list' })).body.result.tools;
  assert.ok(tools.some(t => t.name === 'list_agreed_fixes'));
  const agreed = await call('list_agreed_fixes', {});
  assert.equal(agreed.fixes.length, 4);
  assert.deepEqual(agreed.fixes.find(f => f.issueId === ai.id).values, { SPECIES: 'Hypothyris anastasia' });
  assert.match(agreed.fixes.find(f => f.issueId === sex.id).note, /aceptado por Ana: se ve ♂/);
  assert.equal(agreed.tasks.length, 1);
  assert.match(agreed.tasks[0].task, /CAM000104/);
  assert.equal((await call('list_agreed_fixes', { kind: 'envelope_sex' })).fixes.length, 1);
  const proposed = await call('propose_changes', {
    reason: 'Correcciones acordadas',
    changes: agreed.fixes.map(f => ({ recordId: f.recordId, values: f.values, note: f.note })),
    issueIds: agreed.fixes.map(f => f.issueId),
  });
  assert.equal(proposed.rows, 4);
  // Nothing is written until the person applies it.
  assert.equal(store.getRecordBySheetRow('Collection_data', 4).values.Sex, 'female');
  const applied = await assistant.handle({
    method: 'POST',
    path: `/api/chat/proposals/${proposed.proposalId}/apply`,
    user: ana,
    body: { requestId: randomUUID() },
  });
  assert.equal(applied.body.status, 'applied');
  assert.equal(store.getRecordBySheetRow('Collection_data', 4).values.Sex, 'male');
  assert.equal(store.getRecordBySheetRow('Collection_data', 6).values.SPECIES, 'Episcada sulphurea');
  const verdicts = latestVerdicts(store.db);
  assert.deepEqual([sex.id, species.id, ai.id].map(id => verdicts.get(id).verdict), ['applied', 'applied', 'applied']);
  assert.equal(verdicts.get(sex.id).proposal_id, proposed.proposalId);
  // The fixed issues left the checks; the tab still shows them as applied.
  const done = reviewPage(store, { status: 'applied' });
  assert.equal(done.total, 4);
  assert.ok(done.issues.every(i => i.resolved && i.verdict.status === 'aplicado'));
  assert.equal((await call('list_agreed_fixes', {})).fixes.length, 0);
  // The Drive task is marked done by hand.
  setVerdicts(store, { ids: ['photo_extra:CAM000103'], verdict: 'applied' }, ana);
  assert.equal(agreedFixes(store).tasks.length, 0);
  store.close();
});

test('"Preparar propuesta" makes one proposal of the agreed fixes; a fix changed since the verdict is held back', async () => {
  const { store } = await fixture();
  const assistant = createAssistant({ store, config: { claude: {} } });
  const sex = issueOf(store, 'envelope_sex', 'CAM000105');
  const species = issueOf(store, 'envelope_species', 'CAM000107');
  assert.equal((await assistant.handle({ method: 'POST', path: '/api/chat/proposals/from-review', user: ana, body: {} })).status, 409);
  setVerdicts(store, { ids: [sex.id, species.id], verdict: 'accepted' }, ana);
  // The accepted fix was another one: shown as stale, not proposed.
  store.db
    .prepare('UPDATE issue_verdicts SET snapshot_json=? WHERE issue_id=?')
    .run(JSON.stringify({ fix: { recordId: species.recordId, values: { SPECIES: 'Oleria gunilla' } } }), species.id);
  const agreed = agreedFixes(store);
  assert.equal(agreed.fixes.length, 1);
  assert.equal(agreed.stale[0].issueId, species.id);
  const made = await assistant.handle({ method: 'POST', path: '/api/chat/proposals/from-review', user: ana, body: {} });
  assert.equal(made.status, 201);
  const pending = await assistant.handle({ method: 'GET', path: '/api/chat/proposals', user: ana, query: {} });
  assert.equal(pending.body.proposals[0].source, 'Revisión de datos');
  assert.deepEqual(pending.body.proposals[0].changes[0].values, { Sex: 'male' });
  store.close();
});

test('photos come from Drive once (lh3, else the thumbnail), are cached within a bound and only for listed files', async () => {
  const { store } = await fixture();
  const dir = mkdtempSync(join(tmpdir(), 'photos-'));
  const asked = [];
  const jpeg = size => new Response(Buffer.alloc(size, 7), { headers: { 'content-type': 'image/jpeg' } });
  const fetchImpl = async url => {
    asked.push(url);
    return url.includes('lh3') ? new Response('no', { status: 403, headers: { 'content-type': 'text/html' } }) : jpeg(3000);
  };
  const photos = createPhotoService(store, { dir, maxBytes: 7000, fetchImpl });
  const first = await photos.get(ids.CAM000101d, 400);
  assert.equal(first.mime, 'image/jpeg');
  assert.equal(first.data.length, 3000);
  assert.deepEqual(asked.map(u => new URL(u).host), ['lh3.googleusercontent.com', 'drive.google.com']);
  // Two cards at once share a download; the next time it comes from the cache.
  await Promise.all([photos.get(ids.CAM000101d, 400), photos.get(ids.CAM000101d, 400)]);
  assert.equal(asked.length, 2);
  await assert.rejects(photos.get('1'.repeat(33), 400), { status: 404 });
  await assert.rejects(photos.get(ids.CAM000101d, 999), { status: 400 });
  // Over the bound, the least recently used photos go.
  await photos.get(ids.CAM000101v, 400);
  await photos.get(ids.CAM000105d, 400);
  assert.ok(photos.stats().bytes <= 7000);
  assert.ok(readdirSync(dir).length <= 2);
  rmSync(dir, { recursive: true, force: true });
  store.close();
});

test('the Revisión API needs an editor; photos are served behind login with a sandbox policy', async t => {
  const app = await createApp(
    { databasePath: ':memory:', localMode: true, secureCookies: false, syncIntervalMs: 0, setupToken: 'private-review-setup' },
    { seed, fetchPhoto: async () => new Response(Buffer.alloc(10, 1), { headers: { 'content-type': 'image/jpeg' } }) },
  );
  await app.ready;
  importBundle(app.store.db, { ...readCuration(DIR, join(DIR, 'manifest.csv')), ...(await readGallery(join(DIR, 'gallery'))) });
  const address = await app.listen(0);
  t.after(() => app.close());
  const base = `http://127.0.0.1:${address.port}/ithomiini`;
  let cookie = '',
    csrf = '';
  const call = async (path, method = 'GET', body) => {
    const response = await fetch(base + path, {
      method,
      headers: { ...(body ? { 'content-type': 'application/json' } : {}), ...(cookie ? { cookie, 'x-csrf-token': csrf } : {}) },
      body: body ? JSON.stringify(body) : undefined,
    });
    if (response.headers.get('set-cookie')) cookie = response.headers.get('set-cookie').split(';')[0];
    return response;
  };
  assert.equal((await call(`/api/photo/${ids.CAM000101d}?w=400`)).status, 401);
  const setup = await (await call('/api/auth/setup', 'POST', { token: 'private-review-setup', username: 'rev_admin', password: 'test-admin-123' })).json();
  csrf = setup.csrf;
  const photo = await call(`/api/photo/${ids.CAM000101d}?w=1600`);
  assert.equal(photo.status, 200);
  assert.equal(photo.headers.get('content-security-policy'), 'sandbox; default-src none');
  assert.match(photo.headers.get('cache-control'), /private, max-age=\d+/);
  assert.equal((await call(`/api/photo/${ids.CAM000101d}?w=1600`, 'GET')).headers.get('etag'), photo.headers.get('etag'));
  const page = await (await call('/api/review?kind=envelope_sex')).json();
  assert.equal(page.total, 1);
  const saved = await call('/api/review/verdicts', 'POST', { ids: [page.issues[0].id], verdict: 'accepted' });
  assert.equal(saved.status, 200);
  const history = await (await call(`/api/review/verdicts?issueId=${encodeURIComponent(page.issues[0].id)}`)).json();
  assert.equal(history.history[0].verdict, 'accepted');
  assert.equal((await call('/api/review/labels')).headers.get('content-type'), 'application/x-ndjson; charset=utf-8');
});
