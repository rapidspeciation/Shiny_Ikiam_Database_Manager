import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createApp } from '../server/index.mjs';
import { createAssistant, mcpTools } from '../server/assistant.mjs';
import { IMAGES, REVIEW_PROMPT, checkBatch, cleanState, createRenamePhotos, listedPlaces, uploadPlace } from '../server/rename-photos.mjs';
import { SITES, createSiteUpdates } from '../server/site-updates.mjs';

const minute = n => `2026-07-20T10:${String(n).padStart(2, '0')}:00.000Z`;
const photo = (n, id, extra = {}) => ({ file: `IMG_${n}.JPG`, n, id, view: '', to: '', taken: minute(n), status: 'Original', review: [], suggest: [], rotation: 0, rotationPending: 0, rotationFrom: 'ocr', ...extra });
// A dorsal and a ventral photo each; one ID misread (out of sequence), one photographed three times,
// one ID used again later, one photo unread, one skipped.
const batch = () => ({
  folder: 'batch july 20',
  canWrite: true,
  files: 13,
  folderBytes: 5 * 2 ** 30,
  reading: { on: false, percent: 100, message: '' },
  rows: [
    photo(1, 'CAM077001'),
    photo(2, 'CAM077001'),
    photo(3, 'CAM077002'),
    photo(4, 'CAM077008', { review: ['out-of-sequence'], suggest: ['CAM077002'] }),
    photo(5, 'CAM077003'),
    photo(6, 'CAM077003'),
    photo(7, 'CAM077003'),
    photo(8, 'CAM077004'),
    photo(9, 'CAM077004', { review: ['rotation-outlier'] }),
    photo(10, '', { review: ['no-reading'] }),
    photo(11, 'CAM077001'),
    photo(12, 'CAM077999'),
    photo(13, 'CAM077005', { skip: true }),
  ],
});
const known = new Set(['CAM077001', 'CAM077002', 'CAM077003', 'CAM077004', 'CAM077005', 'CAM077008', 'CAM077909']);

test('what does not add up in a batch: photos per ID, IDs apart, IDs the workbook lacks or already has photos of', () => {
  const { order, counts, checks, flagged } = checkBatch(cleanState(batch()).rows, { known, listed: new Set(['CAM077004']) });
  assert.equal(order, 'CAM077001×2 CAM077002×1 CAM077008×1! CAM077003×3 CAM077004×2! ?×1! CAM077001×1 CAM077999×1');
  assert.deepEqual(counts, { photos: 13, ids: 6, waitingForAPerson: 3, withoutId: 1, skipped: 1 });
  assert.deepEqual(
    checks.notTwoPhotos.examples.map(x => [x.id, x.photos.length]),
    [['CAM077001', 3], ['CAM077002', 1], ['CAM077008', 1], ['CAM077003', 3], ['CAM077999', 1]],
  );
  assert.deepEqual(checks.apart.examples.map(x => x.id), ['CAM077001']);
  assert.deepEqual(checks.apart.examples[0].photos[2], { n: 11, file: 'IMG_11.JPG', taken: '10:11:00' });
  // An ID one digit away from one the workbook has.
  assert.deepEqual(checks.notInWorkbook, { count: 1, examples: [{ id: 'CAM077999', photos: 1, maybe: ['CAM077909'] }] });
  assert.deepEqual(checks.alreadyPhotographed, { count: 1, examples: [{ id: 'CAM077004', photos: 2 }] });
  assert.deepEqual(checks.turned.examples.map(x => x.n), [9]);
  // To look at: the photos waiting, those without an ID and those of the IDs named; not the skipped one.
  assert.deepEqual(flagged.map(r => r.n), [1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12]);
  // Nothing to say of a clean batch.
  const clean = checkBatch(cleanState({ rows: [photo(1, 'CAM077001'), photo(2, 'CAM077001')] }).rows, { known });
  assert.deepEqual([clean.order, clean.checks, clean.flagged], ['CAM077001×2', {}, []]);
});

test("the app's wing check: a photo's wings in a few words, its notes among the checks, the photo to look at", () => {
  const wings = { side: 'ventral', pDorsal: 0.012, forewings: 'ventral', hindwings: 'ventral', count: 3, missing: ['hindwing_left'], turn: 0 };
  const says = ['Named d but the wings show their underside'];
  const state = cleanState({
    ...batch(),
    wingCheck: { on: false, done: 0, total: 0, checked: 12 },
    rows: batch().rows.map(r => (r.n === 3 ? { ...r, view: 'd', review: ['wing-side'], wings, wingNotes: says } : r)),
  });
  assert.deepEqual(state.wingCheck, { on: false, done: 0, total: 0, checked: 12 });
  assert.equal(state.rows[2].wings, 'ventral p=0.01, 3 wings (no left hindwing), upright');
  assert.equal(cleanState({ rows: [{ file: 'a.JPG', wings: { side: 'sideways' } }] }).rows[0].wings, undefined);
  const { checks, flagged } = checkBatch(state.rows);
  assert.deepEqual(checks.wings, { count: 1, examples: [{ n: 3, file: 'IMG_3.JPG', view: 'd', taken: '10:03:00', id: 'CAM077002', says }] });
  assert.ok(flagged.some(r => r.n === 3));
});

test('where a batch goes on Drive: the folders of the photos of the IDs nearest its own', () => {
  const link = n => `https://drive.google.com/drive/folders/folder-${n}`;
  const fileRow = path => ({ values: { 'Full Path': path, Name: path.split('/').at(-1), Type: 'File' } });
  const places = listedPlaces([
    { values: { 'Full Path': 'Wild/PERU_2024/JPG', Name: 'JPG', Type: 'Folder', URL: link(1) } },
    { values: { 'Full Path': 'Wild/PERU_2024/RAW', Name: 'RAW', Type: 'Folder', URL: link(2) } },
    { values: { 'Full Path': 'CRISPR', Name: 'CRISPR', Type: 'Folder', URL: link(3) } },
    ...['CAM076990', 'CAM076995', 'CAM076999'].flatMap(id => [fileRow(`Wild/PERU_2024/JPG/${id}d.JPG`), fileRow(`Wild/PERU_2024/JPG/${id}v.JPG`), fileRow(`Wild/PERU_2024/RAW/${id}d.CR2`)]),
    fileRow('CRISPR/CAM077003d1.JPG'),
    fileRow('CRISPR/CAM090000d.JPG'),
  ]);
  const rows = cleanState(batch()).rows;
  const where = uploadPlace(rows, places);
  assert.deepEqual(where.photos, {
    folder: 'Wild/PERU_2024/JPG',
    link: link(1),
    inDriveForDesktop: 'Ithomiini_IKIAM › Photos › Wild › PERU_2024 › JPG',
    because: 'the photos of the IDs nearest 5 of these 6 IDs are there',
    // A file of another folder is near them too.
    alsoNear: 'CRISPR',
  });
  assert.deepEqual([where.rawFiles.folder, where.rawFiles.link], ['Wild/PERU_2024/RAW', link(2)]);
  // IDs far from every listed one: a new collection, named after its butterflies.
  const far = uploadPlace([{ id: 'CAM099001' }, { id: 'CAM099002' }, { id: 'M6285' }], places, new Map([['CAM099001', { country: 'Guyana', date: 46200 }], ['CAM099002', { country: 'Guyana', date: 46201 }]]));
  assert.match(far.noFolderYet, /new collection/);
  assert.deepEqual(far.butterflies, { countries: { Guyana: 2 }, years: { 2026: 2 } });
  assert.equal(uploadPlace([{ id: '', skip: false }], places), null);
});

/** A «Fotos» tab: it tells its batch, asks for work, and answers it as the frame's app would. */
function fakeTab(rename, user, { tab = 'tab-1', state = batch() } = {}) {
  const done = [];
  let open = true;
  const http = (method, what, body = {}) => rename.handle({ method, path: `/api/rename/${what}`, user, body: { tab, ...body }, query: { tab } });
  const answer = ({ name, args }) => {
    if (name === 'view') return { images: [{ files: args.files, jpeg: Buffer.from(`jpeg of ${args.files.join('+')} (${args.part}${args.sheet ? ', sheet' : ''})`).toString('base64') }], missing: [] };
    if (name === 'edit') {
      for (const e of args.edits) Object.assign(state.rows.find(r => r.file === e.file) ?? {}, e.id ? { id: e.id, review: [], byHost: [`ID → ${e.id}`] } : {});
      return { changed: args.edits.map(e => ({ file: e.file, what: ['changed'] })), problems: [], rows: state.rows.filter(r => args.edits.some(e => e.file === r.file)) };
    }
    return { rowsRestored: 2 };
  };
  const loop = (async () => {
    await http('POST', 'state', { state });
    while (open) {
      const { commands } = (await http('GET', 'commands')).body;
      for (const c of commands) {
        done.push([c.name, c.args]);
        await http('POST', 'result', { id: c.id, ok: true, data: answer(c) });
        await http('POST', 'state', { state });
      }
    }
  })();
  return { done, close: () => ((open = false), loop) };
}

test('the tools reach the batch through the open tab: read, pictures, corrections', async () => {
  const sheets = new LocalSheets({ Insectary_data: [...known].map((cam, i) => ({ row: i + 2, values: { Insectary_ID: `A${i}B`, Sex: 'female', CAM_ID: cam } })) });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data'] });
  const rename = createRenamePhotos({ store, pollMs: 40, answerMs: 2000, space: async () => ({ freeGb: 4.2, percent: 96, nearlyFull: true }) });
  const kim = { id: 'kim', username: 'kim', role: 'editor' };
  try {
    // No tab open: the tools say what the person has to do; a visitor's role cannot use them.
    assert.match((await rename.run('rename_photos_read', {}, kim)).error, /tab is not open/);
    assert.match((await rename.run('rename_photos_read', {}, { id: 'v', role: 'viewer' })).error, /role/);
    assert.equal((await rename.handle({ method: 'GET', path: '/api/rename/ids', user: { id: 'v', role: 'viewer' } })).status, 403);
    assert.equal(await rename.handle({ method: 'GET', path: '/api/chat/proposals', user: kim }), null);
    assert.deepEqual((await rename.handle({ method: 'GET', path: '/api/rename/ids', user: kim })).body.ids.sort(), [...known].sort());

    const tab = fakeTab(rename, kim);
    await new Promise(r => setTimeout(r, 20));
    assert.equal(rename.active(kim), true);
    const read = await rename.run('rename_photos_read', {}, kim);
    assert.equal(read.folder, 'batch july 20');
    assert.match(read.order, /^CAM077001×2 CAM077002×1 CAM077008×1!/);
    assert.equal(read.checks.notInWorkbook.examples[0].id, 'CAM077999');
    assert.deepEqual(read.rows[3], { n: 4, file: 'IMG_4.JPG', id: 'CAM077008', taken: '10:04:00', review: ['out-of-sequence'], suggest: ['CAM077002'] });
    // What comes after the renaming: no folder near (the sheet lists no photos here), the folder's size against the space left.
    assert.match(read.upload.noFolderYet, /new collection/);
    assert.deepEqual([read.upload.folderOnTheComputerGb, read.upload.projectDrive], [5, { freeGb: 4.2, usedPercent: 96, nearlyFull: true, fits: 'no: the folder is larger than the space left' }]);
    assert.match(read.upload.then, /sync_photo_links.*update_websites/);
    // The same for the tab's own bar.
    const bar = await rename.handle({ method: 'GET', path: '/api/rename/upload', user: kim, query: { tab: 'tab-1' } });
    assert.match(bar.body.upload.noFolderYet, /new collection/);
    // Every photo, a page at a time; or some by name.
    const all = await rename.run('rename_photos_read', { show: 'all', limit: 5 }, kim);
    assert.deepEqual([all.rows.length, all.more], [5, { from: 5, of: 13 }]);
    assert.deepEqual((await rename.run('rename_photos_read', { files: ['img_13.jpg'] }, kim)).rows, [{ n: 13, file: 'IMG_13.JPG', id: 'CAM077005', taken: '10:13:00', skip: true }]);

    // Pictures: made by the tab, answered beside the text.
    const look = await rename.run('rename_photos_look', { files: ['IMG_3.JPG', 'IMG_4.JPG'] }, kim);
    assert.deepEqual(look.pictures, [{ picture: 1, shows: ['IMG_3.JPG', 'IMG_4.JPG'] }]);
    assert.equal(Buffer.from(look[IMAGES][0].data, 'base64').toString(), 'jpeg of IMG_3.JPG+IMG_4.JPG (envelope, sheet)');
    assert.deepEqual(tab.done.at(-1), ['view', { files: ['IMG_3.JPG', 'IMG_4.JPG'], part: 'envelope', sheet: true }]);
    assert.equal(JSON.stringify(look).includes('jpeg'), false);
    assert.match((await rename.run('rename_photos_look', {}, kim)).error, /Give the `files`/);

    // A correction: sent to the tab, and the batch read again shows it.
    const edit = await rename.run('rename_photos_edit', { edits: [{ file: 'IMG_4.JPG', id: 'CAM077002' }] }, kim);
    assert.equal(edit.changed, 1);
    assert.deepEqual(edit.rows, [{ n: 4, file: 'IMG_4.JPG', id: 'CAM077002', taken: '10:04:00', suggest: ['CAM077002'], changedByYou: ['ID → CAM077002'] }]);
    assert.match((await rename.run('rename_photos_read', {}, kim)).order, /^CAM077001×2 CAM077002×2 CAM077003×3/);
    assert.deepEqual(await rename.run('rename_photos_edit', { undo: true }, kim), { status: 'undone', rowsAsTheyWere: 2 });
    assert.match((await rename.run('rename_photos_edit', {}, kim)).error, /Give `edits`/);
    await tab.close();

    // A tab that does not answer: the call ends by itself.
    const slow = createRenamePhotos({ store, pollMs: 40, answerMs: 60 });
    await slow.handle({ method: 'POST', path: '/api/rename/state', user: kim, body: { tab: 't', state: batch() } });
    assert.match((await slow.run('rename_photos_look', { files: ['IMG_1.JPG'] }, kim)).error, /did not answer in time/);
    slow.close();
  } finally {
    rename.close();
    store.close();
  }
});

test('the assistant offers the tools, and a picture comes with the answer', async () => {
  const store = new Store({ localMode: true }, { sheets: new LocalSheets({}) });
  await store.sync({ sheets: [] });
  store.db.prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('kim','kim','Kim','editor','s','h',1,'2026-01-01')").run();
  const assistant = createAssistant({ store, config: { renamePhotos: { pollMs: 40, answerMs: 2000 } } });
  store.db.prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')").run(createHash('sha256').update('kim-token').digest('hex'), 'kim');
  const kim = { id: 'kim', username: 'kim', role: 'editor' };
  const call = async (name, args) => (await assistant.mcp({ authorization: 'Bearer kim-token' }, { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } })).body.result;
  try {
    for (const name of ['rename_photos_read', 'rename_photos_look', 'rename_photos_edit', 'update_websites']) assert.ok(mcpTools().some(t => t.name === name), name);
    // The tab's requests go through the assistant's own HTTP.
    const tab = fakeTab(assistant.renamePhotos, kim);
    await new Promise(r => setTimeout(r, 20));
    assert.equal((await assistant.handle({ method: 'GET', path: '/api/rename/ids', user: kim })).status, 200);
    const out = await call('rename_photos_look', { files: ['IMG_4.JPG'], part: 'photo' });
    assert.deepEqual(out.content.map(c => c.type), ['text', 'image']);
    assert.deepEqual(JSON.parse(out.content[0].text), { pictures: [{ picture: 1, shows: ['IMG_4.JPG'] }] });
    assert.equal(out.content[1].mimeType, 'image/jpeg');
    assert.equal(Buffer.from(out.content[1].data, 'base64').toString(), 'jpeg of IMG_4.JPG (photo)');
    assert.equal(JSON.parse((await call('rename_photos_read', {})).content[0].text).counts.photos, 13);
    await tab.close();
  } finally {
    assistant.close();
    store.close();
  }
});

test('the Rename Photos app is served for the tab\'s frame, and the review asks for a chat', async () => {
  const renameAppRoot = mkdtempSync(join(tmpdir(), 'rename-app-'));
  mkdirSync(join(renameAppRoot, 'assets'));
  mkdirSync(join(renameAppRoot, 'models'));
  writeFileSync(join(renameAppRoot, 'index.html'), '<!doctype html><title>Rename</title>');
  writeFileSync(join(renameAppRoot, 'assets', 'ort-abc123.wasm'), 'wasm');
  writeFileSync(join(renameAppRoot, 'models', 'camid_rec.onnx'), 'model');
  const store = new Store({ localMode: true }, { sheets: new LocalSheets({}) });
  const app = await createApp({ localMode: true, secureCookies: false, syncIntervalMs: 0, renameAppRoot }, { store, skipInitialSync: true });
  const { port } = await app.listen(0, '127.0.0.1');
  const base = `http://127.0.0.1:${port}/ithomiini`;
  try {
    const page = await fetch(`${base}/rename-app/index.html?embed`);
    assert.equal(page.status, 200);
    assert.equal(page.headers.get('cache-control'), 'no-cache');
    // Framed by this site only; its reader's WebAssembly and workers allowed.
    const csp = page.headers.get('content-security-policy');
    assert.match(csp, /frame-ancestors 'self'/);
    assert.match(csp, /script-src 'self' 'wasm-unsafe-eval'/);
    assert.match(csp, /worker-src 'self' blob:/);
    assert.equal((await fetch(`${base}/rename-app/`)).status, 200);
    const wasm = await fetch(`${base}/rename-app/assets/ort-abc123.wasm`);
    assert.deepEqual([wasm.headers.get('content-type'), wasm.headers.get('cache-control')], ['application/wasm', 'public, max-age=31536000, immutable']);
    assert.equal((await fetch(`${base}/rename-app/models/camid_rec.onnx`)).headers.get('cache-control'), 'public, max-age=86400');
    assert.equal((await fetch(`${base}/rename-app/models/none.onnx`)).status, 404);
    assert.equal((await fetch(`${base}/rename-app/..%2Fpackage.json`)).status, 404);
  } finally {
    await app.close();
    rmSync(renameAppRoot, { recursive: true, force: true });
  }
  assert.match(REVIEW_PROMPT, /«Fotos»/);
});

test('update_websites: when each site was last updated, an update started through its worker and followed, its publishing', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'site-updates-'));
  const passwordFile = join(dir, 'password');
  const kim = { id: 'kim', role: 'editor' };
  // GitHub and the two Cloudflare Workers, as far as the tool uses them.
  const hub = { calls: [], runs: { gallery: { update: [], deploy: [] }, maps: { update: [], deploy: [] } } };
  const keyOf = url => Object.keys(SITES).find(k => url === SITES[k].worker || url.includes(`/repos/${SITES[k].repo}/`));
  const fetchImpl = async (url, init = {}) => {
    const key = keyOf(url);
    const method = init.method ?? 'GET';
    const json = body => ({ ok: true, status: 200, json: async () => body, text: async () => '' });
    if (url === SITES[key].worker) {
      const body = JSON.parse(init.body);
      hub.calls.push(['worker', key, body]);
      if (body.password !== 'secret') return { ok: false, status: 401, text: async () => 'Invalid password' };
      // The run ends at once, commits new data and starts the publishing.
      hub.runs[key].update.unshift({ id: hub.runs[key].update.length + 10, status: 'completed', conclusion: 'success', created_at: '2026-10-14T01:00:00Z', updated_at: '2026-10-14T01:00:40Z', html_url: `https://github.com/run/${key}` });
      hub.runs[key].deploy.unshift({ id: 99, status: 'in_progress', conclusion: null, created_at: '2026-10-14T01:00:41Z', html_url: `https://github.com/deploy/${key}` });
      return { ok: true, status: 200, text: async () => 'Update triggered successfully' };
    }
    hub.calls.push([method, key, url.replace(`https://api.github.com/repos/${SITES[key].repo}`, '')]);
    return json({ workflow_runs: hub.runs[key][/update_data/.test(url) ? 'update' : 'deploy'] });
  };
  try {
    hub.runs.gallery.update.push({ id: 1, status: 'completed', conclusion: 'success', created_at: '2026-02-24T15:39:28Z', updated_at: '2026-02-24T15:40:10Z', html_url: 'https://github.com/run/1' });
    hub.runs.gallery.deploy.push({ id: 2, status: 'completed', conclusion: 'success', created_at: '2026-09-29T05:05:30Z', updated_at: '2026-09-29T05:07:00Z', html_url: 'https://github.com/run/2' });
    // Without the password: the last runs are still told; starting one is not possible.
    const reader = createSiteUpdates({ passwordFile, fetchImpl, waitMs: 200, stepMs: 5 });
    const seen = await reader.run({}, kim);
    assert.deepEqual(seen.sites[0], {
      site: 'Wings Gallery',
      url: 'https://rapidspeciation.github.io/Shiny_Ikiam_Wings_Gallery/',
      lastUpdate: { started: '2026-02-24T15:39:28Z', state: 'success', ended: '2026-02-24T15:40:10Z', link: 'https://github.com/run/1' },
      publishing: { started: '2026-09-29T05:05:30Z', state: 'success', ended: '2026-09-29T05:07:00Z', link: 'https://github.com/run/2' },
    });
    assert.deepEqual(seen.sites[1], { site: 'Ithomiini maps', url: 'https://rapidspeciation.github.io/ithomiini_maps/', lastUpdate: 'never run' });
    assert.equal(reader.ready(), false);
    assert.match((await reader.run({ sites: ['gallery'] }, kim)).error, /no password/);
    assert.match((await reader.run({ sites: ['moths'] }, kim)).error, /gallery or maps/);
    assert.match((await reader.run({}, { role: 'viewer' })).error, /role/);
    assert.ok(hub.calls.every(c => c[0] === 'GET'));

    // With it: each worker is asked (the maps' with its repository and the GBIF choice), the run followed to its end, and its publishing told.
    writeFileSync(passwordFile, 'secret\n');
    const updates = createSiteUpdates({ passwordFile, fetchImpl, waitMs: 500, stepMs: 5 });
    hub.calls.length = 0;
    const out = await updates.run({ sites: ['gallery', 'maps'], gbif: true }, kim);
    assert.deepEqual(out.sites.map(s => [s.site, s.update.state, s.publishing.state]), [['Wings Gallery', 'success', 'running'], ['Ithomiini maps', 'success', 'running']]);
    assert.match(out.next, /Still running/);
    assert.deepEqual(hub.calls.filter(c => c[0] === 'worker').sort(), [
      ['worker', 'gallery', { password: 'secret' }],
      ['worker', 'maps', { password: 'secret', owner: 'rapidspeciation', repo: 'ithomiini_maps', update_sanger: true, update_gbif: true }],
    ]);
    // An update already running is not started twice.
    hub.runs.maps.update.unshift({ id: 50, status: 'in_progress', conclusion: null, created_at: '2026-10-14T02:00:00Z', html_url: 'https://github.com/run/50' });
    hub.calls.length = 0;
    const busy = await updates.run({ sites: ['maps'] }, kim);
    assert.match(busy.sites[0].update.note, /already running/);
    assert.equal(hub.calls.filter(c => c[0] === 'worker').length, 0);
    // GitHub's 60 requests an hour: asked again within minutes, the runs held are told; refused for the limit, too.
    Object.assign(hub.runs.gallery.deploy[0], { status: 'completed', conclusion: 'success', updated_at: '2026-10-14T01:02:00Z' });
    let clock = Date.parse('2026-10-14T03:00:00Z');
    let limited = false;
    const counted = [];
    const sparing = createSiteUpdates({
      passwordFile,
      now: () => clock,
      fetchImpl: async (url, init) => {
        counted.push(url);
        return limited ? { ok: false, status: 403, text: async () => 'API rate limit exceeded' } : fetchImpl(url, init);
      },
    });
    const first = await sparing.run({}, kim);
    assert.equal(counted.length, 4);
    clock += 60_000;
    assert.deepEqual(await sparing.run({ fresh: true }, kim), first);
    // Only the maps had a run going: only theirs were asked again.
    assert.deepEqual(counted.slice(4).map(u => keyOf(u)), ['maps', 'maps']);
    clock += 10 * 60_000;
    limited = true;
    assert.deepEqual((await sparing.run({}, kim)).sites[0].lastUpdate, first.sites[0].lastUpdate);
    // A wrong password.
    writeFileSync(passwordFile, 'old\n');
    const refused = await updates.run({ sites: ['gallery'] }, kim);
    assert.match(refused.sites[0].error, /refused the password/);
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});

test('a folder opened again is found again: its name and the names its photos had when read', async () => {
  const { folderKey } = await import('../server/rename-photos.mjs');
  const rows = names => names.map((file, i) => ({ file, now: `CAM00000${i}d.JPG` }));
  const once = folderKey({ folder: 'wing photos', rows: rows(['IMG_1.JPG', 'IMG_2.JPG']) });
  // Renamed since (their `file` is still the camera's name), in another order: the same folder.
  assert.equal(folderKey({ folder: 'wing photos', rows: rows(['IMG_2.JPG', 'IMG_1.JPG']).map(r => ({ ...r, now: 'x' })) }), once);
  assert.notEqual(folderKey({ folder: 'wing photos 2', rows: rows(['IMG_1.JPG', 'IMG_2.JPG']) }), once);
  assert.notEqual(folderKey({ folder: 'wing photos', rows: rows(['IMG_1.JPG']) }), once);

  const store = new Store({ localMode: true }, { sheets: new LocalSheets({}) });
  const rename = createRenamePhotos({ store, pollMs: 40, answerMs: 2000 });
  const kim = { id: 'kim', username: 'kim', role: 'editor' };
  try {
    assert.equal(rename.folderOf(kim), null);
    const tab = fakeTab(rename, kim);
    for (let i = 0; i < 50 && !rename.folderOf(kim); i++) await new Promise(r => setTimeout(r, 10));
    assert.deepEqual(rename.folderOf(kim), { folder: batch().folder, key: folderKey(batch()) });
    await tab.close();
  } finally {
    rename.close();
    store.close();
  }
});
