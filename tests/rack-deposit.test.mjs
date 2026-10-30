import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { copyFileSync, mkdirSync, mkdtempSync, readFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { inflateRawSync } from 'node:zlib';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { LocalDrive } from '../server/drive.mjs';
import { loadPhoto, readOne } from '../server/rack-reader.mjs';
import { DRIVE_FOLDER, changesSheets, reportFile, workbook } from '../server/rack-tools.mjs';
import { DRIVE_PHOTO_TOOLS } from '../server/drive-photos.mjs';
import { placeReading, tubeSheet } from '../server/rack-deposit.mjs';

// A rack deposit as «Cambios propuestos» shows it: Franz's photo of FK00274230 (28 Oct), the workbook's
// tube-locations sheet holding some of its tubes, and the proposal the reading makes at once.
const RACK_PHOTO = new URL('./fixtures/rack-bottom-fk00274230.jpg', import.meta.url).pathname;
const expected = well => `FS${50849233 + (Number(well.slice(1)) - 1) * 8 + 'ABCDEFGH'.indexOf(well[0])}`;
const SHEET = tubeSheet();
let photo = null;
const readPhoto = async () => (photo ??= await readOne(await loadPhoto(RACK_PHOTO)));

const seed = () => ({
  Collection_data: [{ row: 2, values: { CAM_ID: 'CAM079877', SPECIES: 'Ithomia salapia', Tube_1_id: 'FS50849233' } }],
  [SHEET]: [
    // Already where the photos have it.
    { row: 2, values: { Sort: 10, Tube_ID: expected('B1'), Rack_ID: 'FK00274230', Tube_location: 'B1' } },
    // Recorded on another rack and well.
    { row: 3, values: { Sort: 11, Tube_ID: expected('C1'), Rack_ID: 'FK00999999', Tube_location: 'E5' } },
    // On this rack in the sheet, but not in the photos.
    { row: 4, values: { Sort: 12, Tube_ID: 'FS11111111', Rack_ID: 'FK00274230', Tube_location: 'H12' } },
  ],
});

async function setup({ drive = null, protect = false } = {}) {
  const store = new Store({ localMode: true }, { sheets: new LocalSheets(seed()) });
  await store.sync({ sheets: Object.keys(seed()) });
  const home = mkdtempSync(join(tmpdir(), 'deposit-t3-'));
  mkdirSync(join(home, 'userdata', 'attachments'), { recursive: true });
  copyFileSync(RACK_PHOTO, join(home, 'userdata', 'attachments', 'thread-1-rack.jpg'));
  const rackReportsDir = mkdtempSync(join(tmpdir(), 'deposit-reports-'));
  const config = { t3: { home }, rackReportsDir, publicUrl: 'https://app.test', rackReadPhoto: readPhoto, ...(drive ? { drive } : {}) };
  const assistant = createAssistant({ store, config });
  store.db
    .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u-franz','franz','Franz Chandi','editor','s','h',1,'2026-01-01')")
    .run();
  store.db.prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')").run(createHash('sha256').update('franz-token').digest('hex'), 'u-franz');
  if (protect) store.db.prepare('INSERT OR REPLACE INTO settings(key,value) VALUES(?,?)').run(`protected:${SHEET}`, '[{"range":{"sheetId":7}}]');
  const call = async (name, args) =>
    JSON.parse((await assistant.mcp({ authorization: 'Bearer franz-token' }, { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } })).body.result.content[0].text);
  const user = { id: 'u-franz', username: 'franz', displayName: 'Franz Chandi', role: 'editor' };
  const http = (method, path, body = {}, query = {}) => assistant.handle({ method, path, body, user, query });
  const list = async () => (await http('GET', '/api/chat/proposals', {}, { all: '1' })).body.proposals;
  return { store, call, http, list, rackReportsDir, user };
}

test('a rack read goes at once into a deposit proposal: grid, defaults, the tube-locations rows; nothing else', async () => {
  const { call, list } = await setup();
  const read = await call('rack_read_photos', { photos: ['thread-1-rack.jpg'] });
  assert.equal(read.rack_id, 'FK00274230');
  // The code's corner places A1 (the team's racks: beside A12), an assumption the IDs here happen to check.
  assert.deepEqual(read.orientation.basis, ['rack_code_default', 'tube_sequence']);
  assert.equal(read.orientation.assumed, true);
  const d = read.deposit;
  assert.ok(d.proposalId);
  assert.equal(d.responsable, 'Franz Chandi');
  assert.equal(d.ultracongelador, 'LBI062');
  assert.ok(d.ask.some(a => /confirm the rack code sits beside A12/.test(a)));
  assert.ok(d.ask.some(a => /estanteria, fila_de_rack, caja/.test(a)));
  const [rack] = d.racks;
  assert.equal(rack.tubes.same, 1);
  assert.equal(rack.tubes.moved, 1);
  assert.equal(rack.tubes.new, rack.tubes.same + rack.tubes.moved + rack.tubes.new - 2);
  assert.deepEqual(rack.elsewhere, [{ tube: 'FS11111111', well: 'H12' }]);
  assert.deepEqual(d.racks.length, 1);
  // Tubes no sheet holds need their rows (one has its Collection_data row).
  assert.ok(rack.need_rows.length >= 90 && !rack.need_rows.includes('FS50849233'));

  const [p] = await list();
  assert.equal(p.id, d.proposalId);
  assert.equal(p.status, 'pending');
  assert.deepEqual(p.sheets, [SHEET]);
  assert.ok(p.changes.every(c => c.sheet === SHEET), 'no other sheet');
  const moved = p.changes.find(c => !c.create);
  assert.equal(moved.values.Rack_ID, 'FK00274230');
  assert.equal(moved.values.Tube_location, 'C1');
  assert.deepEqual([moved.current?.Rack_ID ?? moved.rowValues?.Rack_ID, moved.current?.Tube_location ?? moved.rowValues?.Tube_location], ['FK00999999', 'E5']);
  // A1 (FS50849233) sits under glare in this photo; Sort goes on from the sheet's 12 in fill order (D1..H1 are 13..17).
  const a2 = p.changes.find(c => c.create && c.values.Tube_ID === expected('A2'));
  assert.deepEqual([a2?.values.Rack_ID, a2?.values.Tube_location, a2?.values.Sort], ['FK00274230', 'A2', 18]);
  assert.equal(p.deposit.racks[0].wells.B1.s, 'same');
  assert.equal(p.deposit.racks[0].wells.C1.s, 'moved');
  assert.deepEqual(p.deposit.racks[0].wells.C1.was, { rack: 'FK00999999', well: 'E5' });
  assert.equal(p.deposit.racks[0].wells.A2.s, 'new');
  assert.ok(p.deposit.racks[0].missing.includes('A1'));
  assert.deepEqual(p.deposit.racks[0].code_options, ['A12', 'H1']);
  assert.equal(p.deposit.racks[0].assumed, true);
  // The sheet's own tube at H12 of this rack (not in the photos): marked on that well.
  assert.deepEqual(p.deposit.racks[0].sheet_here, { H12: 'FS11111111' });
  assert.ok(p.page?.photos >= 1, 'the rack photo beside the table');
});

test('the person turns the rack (code beside H1): the wells and the rows follow; fields and wells from the sidebar', async () => {
  const { call, http, list } = await setup();
  const read = await call('rack_read_photos', { photos: ['thread-1-rack.jpg'] });
  const id = read.deposit.proposalId;
  const refused = await call('rack_deposit', { racks: [{ rack: 'FK00274230', code_beside: 'H12' }] });
  assert.ok(refused.ask.some(a => /can only be beside A12 or H1, not H12/.test(a)));
  const turned = await call('rack_deposit', { racks: [{ rack: 'FK00274230', code_beside: 'H1' }] });
  assert.equal(turned.racks[0].code_beside, 'H1');
  assert.equal(turned.racks[0].assumed, false);
  assert.equal(turned.racks[0].checked_by_ids, false, 'the IDs no longer follow their sequence');
  let [p] = await list();
  // Turned: the tube that was in A2 is now in H11 (the other mirror of the same photo).
  assert.equal(p.deposit.racks[0].wells.H11.t, read.wells.A2);
  const back = await http('POST', `/api/chat/proposals/${id}/rack`, { racks: [{ rack: '1', code_beside: 'A12', estanteria: '2', fila_de_rack: '1', caja: '3' }], responsable: 'Ana P.' });
  assert.equal(back.status, 200);
  p = back.body.proposal;
  assert.equal(p.deposit.responsable, 'Ana P.');
  assert.equal(p.deposit.racks[0].caja, '3');
  assert.deepEqual(p.deposit.racks[0].unset, []);
  assert.equal(p.lastBy, 'person');
  // A well the photos missed: its tube from the side label; another said empty.
  const open = p.deposit.racks[0].missing;
  assert.ok(open.length >= 1);
  const filled = await http('POST', `/api/chat/proposals/${id}/rack`, { racks: [{ rack: '1', fill: { [open[0]]: expected(open[0]) }, empty: open.slice(1) }] });
  p = filled.body.proposal;
  assert.deepEqual(p.deposit.racks[0].missing, []);
  assert.equal(p.deposit.racks[0].wells[open[0]].t, expected(open[0]));
  assert.ok(p.changes.some(c => c.create && c.values.Tube_ID === expected(open[0])));
  // A side label typed with another well's tube: said once, as two wells of this rack (not as two racks).
  const twice = await call('rack_deposit', { racks: [{ rack: '1', fill: { [open[0]]: read.wells.A2 } }] });
  assert.deepEqual(twice.ask.filter(a => /FS/.test(a) && /two wells|is on/.test(a)), [`FK00274230: ${read.wells.A2} on two wells.`]);
  await call('rack_deposit', { racks: [{ rack: '1', fill: { [open[0]]: expected(open[0]) } }] });
  const bad = await http('POST', `/api/chat/proposals/${id}/rack`, { racks: [{ rack: 'FK00000001', caja: '1' }] });
  assert.equal(bad.status, 400);
});

test('the .xlsx from the sidebar, refused while wells are open; the Drive copy on click, in «Depósitos LBI (racks)»', async () => {
  const drive = new LocalDrive([{ id: 'admin', name: 'Admin', mimeType: 'application/vnd.google-apps.folder', parents: ['ikiam'] }], { me: 'project@example.org' });
  const { call, http, rackReportsDir, user } = await setup({ drive });
  const read = await call('rack_read_photos', { photos: ['thread-1-rack.jpg'] });
  const id = read.deposit.proposalId;
  const refused = await http('POST', `/api/chat/proposals/${id}/rack-files`, {});
  assert.equal(refused.status, 409);
  assert.ok(refused.body.error.details.problems.some(p => /no tube for/.test(p)));
  const open = read.missing.map(m => m.well);
  // Every well said, but the deposit sheet's fields not given yet: still refused (nothing invented in the file).
  await call('rack_deposit', { racks: [{ rack: '1', fill: Object.fromEntries(open.map(w => [w, expected(w)])) }] });
  const noFields = await http('POST', `/api/chat/proposals/${id}/rack-files`, {});
  assert.equal(noFields.status, 409);
  assert.ok(noFields.body.error.details.problems.some(p => /estanteria, fila_de_rack, caja not given/.test(p)));
  await call('rack_deposit', { racks: [{ rack: '1', estanteria: '2', fila_de_rack: '1', caja: '1' }] });
  const made = await http('POST', `/api/chat/proposals/${id}/rack-files`, {});
  assert.equal(made.status, 200);
  assert.equal(made.body.file, `Anexo1-deposito-${new Date().toISOString().slice(0, 10)}-FK00274230.xlsx`);
  assert.equal(made.body.drive, undefined, 'no Drive copy unless asked');
  assert.equal(made.body.proposal.deposit.xlsx.file, made.body.file);
  const [, token, file] = /\/api\/rack-reports\/([a-f0-9]{24})\/(.+)$/.exec(made.body.download);
  assert.ok(reportFile(rackReportsDir, token, file, user));
  const saved = await http('POST', `/api/chat/proposals/${id}/rack-files`, { drive: true });
  assert.equal(saved.status, 200);
  assert.match(saved.body.drive.link, /^https:\/\/drive\.google\.com\/file\/d\//);
  const folder = [...drive.items.values()].find(f => f.name === DRIVE_FOLDER);
  assert.deepEqual(folder.parents, ['root'], 'My Drive without a configured parent');
  const copy = [...drive.items.values()].find(f => f.parents?.includes(folder.id));
  assert.equal(copy.name, saved.body.file);
  assert.equal(drive.contents.get(copy.id).subarray(0, 2).toString(), 'PK');
  assert.equal(saved.body.proposal.deposit.drive.link, saved.body.drive.link);
  // Through the chat too (rack_report with drive), into the same folder.
  const again = await call('rack_report', { drive: true });
  assert.match(again.markdown, /^\[Anexo1-deposito-/);
  assert.equal([...drive.items.values()].filter(f => f.name === DRIVE_FOLDER).length, 1);
});

test('Drive unreachable: a clear failure, the download still works', async () => {
  const drive = new LocalDrive([], {});
  drive.upload = async () => {
    throw new Error('Google Drive did not answer (TimeoutError)');
  };
  const { call } = await setup({ drive });
  const read = await call('rack_read_photos', { photos: ['thread-1-rack.jpg'] });
  await call('rack_deposit', { racks: [{ rack: '1', empty: read.missing.map(m => m.well), estanteria: '2', fila_de_rack: '1', caja: '1' }] });
  const out = await call('rack_report', { drive: true });
  assert.match(out.download, /\/api\/rack-reports\//);
  assert.match(out.drive.error, /did not take the copy.*download link still works/);
});

test('a protected tube-locations sheet: said in the answer and on the proposal, the proposal still made', async () => {
  const { call, list } = await setup({ protect: true });
  const read = await call('rack_read_photos', { photos: ['thread-1-rack.jpg'] });
  assert.match(read.deposit.protected, /no edit access to .*«Aplicar» is not offered.*changes_download/);
  const [p] = await list();
  assert.equal(p.deposit.protected, true);
});

test('two racks in one chat: one deposit, one section and one sheet each', async () => {
  const { call, list, http } = await setup();
  await call('rack_read_photos', { photos: ['thread-1-rack.jpg'] });
  // The same rack read again replaces its section, keeping its fields.
  await call('rack_deposit', { racks: [{ rack: '1', caja: '7' }] });
  await call('rack_read_photos', { photos: ['thread-1-rack.jpg'] });
  let [p] = await list();
  assert.equal(p.deposit.racks.length, 1);
  assert.equal(p.deposit.racks[0].caja, '7');
  // A second rack (its code not read): asked for, its own section.
  const r = await call('rack_read_photos', { photos: ['thread-1-rack.jpg'], rack_code_beside: 'A12' });
  assert.equal(r.deposit.racks.length, 1, 'the same code is the same rack');
  [p] = await list();
  const out = await http('POST', `/api/chat/proposals/${p.id}/rack`, { racks: [{ rack: '1', rack_id: 'FK00274231' }] });
  assert.equal(out.body.proposal.deposit.racks[0].rack_id, 'FK00274231');
  await call('rack_read_photos', { photos: ['thread-1-rack.jpg'] });
  [p] = await list();
  assert.deepEqual(
    p.deposit.racks.map(x => x.rack_id),
    ['FK00274231', 'FK00274230'],
  );
  assert.match(p.reason, /racks FK00274231, FK00274230/);
  // Their tubes are the same tubes here: each is said to be on both racks, and nothing is drafted for them.
  assert.ok(p.deposit.racks[0].problems.some(x => /is on FK00274231 and FK00274230/.test(x)));
});

test('a reading placed by the other corner: the photo positions turned into the other mirror', () => {
  const reading = { mapping: 'm1', code_corner: [11, 0], frame_cells: { '11,0': 'FS1', '0,7': 'FS2' }, conflicts: [], seen: [] };
  assert.deepEqual(Object.fromEntries(placeReading(reading).wells), { A1: 'FS1', H12: 'FS2' });
  assert.deepEqual(Object.fromEntries(placeReading(reading, 'H12').wells), { H12: 'FS1', A1: 'FS2' });
  assert.equal(placeReading({ mapping: null, code_corner: null, frame_cells: {} }), null);
});

// ------------------------------------------------------------ the person's edits of the rows go into the rack
/** The deposit proposal after a read, every well said (so nothing waits), and helpers to edit its rows. */
async function editable() {
  const env = await setup();
  const read = await env.call('rack_read_photos', { photos: ['thread-1-rack.jpg'] });
  const id = read.deposit.proposalId;
  const p = async () => (await env.list()).find(x => x.id === id);
  const rowOf = (proposal, tube) => proposal.changes.find(c => (c.create ? c.values.Tube_ID : c.label) === tube);
  const key = c => c.key ?? c.clientId ?? c.recordId;
  const edit = (cells, more = {}) => env.http('POST', `/api/chat/proposals/${id}/edit`, { cells, ...more });
  return { ...env, read, id, p, rowOf, key, edit };
}

test('a Tube_ID typed in a row sets that well on the grid: the row follows, marked as the person\'s, same key', async () => {
  const { read, p, rowOf, key, edit } = await editable();
  const before = await p();
  const a2 = rowOf(before, read.wells.A2);
  const out = await edit([{ key: key(a2), field: 'Tube_ID', value: 'fs99000001', before: read.wells.A2 }]);
  assert.equal(out.status, 200);
  assert.deepEqual(out.body.rejected, []);
  const after = out.body.proposal;
  assert.equal(after.deposit.racks[0].wells.A2.t, 'FS99000001');
  assert.equal(after.deposit.racks[0].set.A2, 'FS99000001');
  const row = rowOf(after, 'FS99000001');
  assert.equal(row.values.Tube_location, 'A2');
  assert.equal(key(row), key(a2), 'the row keeps its key');
  assert.equal(row.personEdits.Tube_ID.ai, read.wells.A2);
  assert.ok(!rowOf(after, read.wells.A2), 'the old ID has no row');
});

test('a Tube_location typed in a row moves the tube on the grid; onto another tube it is a conflict, kept and resolved on the grid', async () => {
  const { read, p, rowOf, key, edit, http, id, call } = await editable();
  // A1 is not read (glare): moving A2's tube there frees A2.
  let out = await edit([{ key: key(rowOf(await p(), read.wells.A2)), field: 'Tube_location', value: 'a01' }]);
  assert.deepEqual(out.body.rejected, []);
  let rack = out.body.proposal.deposit.racks[0];
  assert.equal(rack.wells.A1.t, read.wells.A2);
  assert.equal(rack.wells.A2, undefined);
  assert.ok(rack.missing.includes('A2'));
  assert.equal(rowOf(out.body.proposal, read.wells.A2).values.Tube_location, 'A1');
  assert.equal(rowOf(out.body.proposal, read.wells.A2).personEdits.Tube_location.ai, 'A2');

  // A3's tube typed into A4, which holds another tube: nothing overwritten, both wells flagged, the person's A4 kept.
  out = await edit([{ key: key(rowOf(out.body.proposal, read.wells.A3)), field: 'Tube_location', value: 'A4' }]);
  assert.deepEqual(out.body.rejected, []);
  rack = out.body.proposal.deposit.racks[0];
  assert.deepEqual(rack.conflicts, [{ tube: read.wells.A3, from: 'A3', to: 'A4', other: read.wells.A4 }]);
  assert.equal(rack.wells.A3.t, read.wells.A3);
  assert.equal(rack.wells.A4.t, read.wells.A4);
  const row = rowOf(out.body.proposal, read.wells.A3);
  assert.equal(row.values.Tube_location, 'A4');
  assert.match(row.rackWarnings.Tube_location.text, /A4 ya tiene otro tubo.*sigue en A3/);
  assert.equal(row.warnings?.Tube_location?.label, 'Conflicto', 'marked on the cell');
  // Still there after a field of the rack changes; «Aplicar» and the .xlsx wait for it.
  out = await http('POST', `/api/chat/proposals/${id}/rack`, { racks: [{ rack: '1', caja: '4' }] });
  assert.equal(rowOf(out.body.proposal, read.wells.A3).values.Tube_location, 'A4');
  const applied = await http('POST', `/api/chat/proposals/${id}/apply`, { requestId: 'apply-conflict-1' });
  assert.equal(applied.status, 409);
  assert.equal(applied.body.error.code, 'rack_conflict');
  const asked = await call('rack_deposit', {});
  assert.ok(asked.ask.some(a => /moved to A4 in the table/.test(a)));
  // Resolved on the grid: the two tubes swapped.
  out = await http('POST', `/api/chat/proposals/${id}/rack`, { racks: [{ rack: '1', resolve: [{ well: 'A4', how: 'swap' }] }] });
  rack = out.body.proposal.deposit.racks[0];
  assert.deepEqual(rack.conflicts, []);
  assert.equal(rack.wells.A4.t, read.wells.A3);
  assert.equal(rack.wells.A3.t, read.wells.A4);
  assert.equal(rowOf(out.body.proposal, read.wells.A3).values.Tube_location, 'A4');
  assert.equal(rowOf(out.body.proposal, read.wells.A4).values.Tube_location, 'A3');
});

test('a conflict kept as the photos have it; or gone through once its well is free', async () => {
  const { read, p, rowOf, key, edit, http, id } = await editable();
  await edit([{ key: key(rowOf(await p(), read.wells.A3)), field: 'Tube_location', value: 'A4' }]);
  let out = await http('POST', `/api/chat/proposals/${id}/rack`, { racks: [{ rack: '1', resolve: [{ well: 'A3', how: 'keep' }] }] });
  let rack = out.body.proposal.deposit.racks[0];
  assert.deepEqual(rack.conflicts, []);
  assert.equal(rack.wells.A3.t, read.wells.A3);
  assert.equal(rowOf(out.body.proposal, read.wells.A3).values.Tube_location, 'A3');
  // Again, then A4 said empty on the grid: the waiting move goes through.
  await edit([{ key: key(rowOf(out.body.proposal, read.wells.A3)), field: 'Tube_location', value: 'A4' }]);
  out = await http('POST', `/api/chat/proposals/${id}/rack`, { racks: [{ rack: '1', empty: ['A4'] }] });
  rack = out.body.proposal.deposit.racks[0];
  assert.deepEqual(rack.conflicts, []);
  assert.equal(rack.wells.A4.t, read.wells.A3);
  assert.ok(rack.missing.includes('A3'));
});

test('Rack_ID and Sort are refused in the rows; a bad well or an empty tube ID refused, the value kept; in the person\'s language', async () => {
  const { read, p, rowOf, key, edit } = await editable();
  const before = await p();
  const a2 = rowOf(before, read.wells.A2);
  const out = await edit([
    { key: key(a2), field: 'Rack_ID', value: 'FK00000001' },
    { key: key(a2), field: 'Sort', value: 3 },
    { key: key(a2), field: 'Tube_location', value: 'Z9' },
    { key: key(a2), field: 'Tube_ID', value: '' },
    { key: key(rowOf(before, read.wells.A3)), field: 'Tube_ID', value: read.wells.A5 },
  ]);
  assert.equal(out.status, 200);
  assert.equal(out.body.rejected.length, 5);
  assert.match(out.body.rejected.find(r => r.field === 'Rack_ID').message, /se cambia en los datos del rack/);
  assert.match(out.body.rejected.find(r => r.field === 'Sort').message, /automático/);
  assert.match(out.body.rejected.find(r => r.field === 'Tube_location').message, /Z9 no es un pocillo \(A1 a H12\): se mantiene A2/);
  assert.ok(out.body.rejected.some(r => /no puede quedar vacío/.test(r.message)));
  assert.ok(out.body.rejected.some(r => new RegExp(`${read.wells.A5} ya está en FK00274230 A5`).test(r.message)));
  assert.ok(out.body.rejected.every(r => r.messageMsg?.key), 'each with its descriptor (shown in English too)');
  const after = await p();
  const row = rowOf(after, read.wells.A2);
  assert.deepEqual([row.values.Rack_ID, row.values.Tube_location, row.values.Sort], [a2.values.Rack_ID, 'A2', a2.values.Sort]);
  assert.equal(rowOf(after, read.wells.A3).values.Tube_ID, read.wells.A3);
});

test('nothing the person edited is lost when the rack\'s fields or corner change: other columns, IDs, wells, rows taken out', async () => {
  const { read, p, rowOf, key, edit, http, id, call } = await editable();
  let now = await p();
  await edit([
    { key: key(rowOf(now, read.wells.A2)), field: 'Batch', value: 'B7' },
    { key: key(rowOf(now, read.wells.A3)), field: 'Tube_ID', value: 'FS99000003' },
    { key: key(rowOf(now, read.wells.A4)), field: 'Tube_location', value: 'A1' },
  ]);
  now = await p();
  await edit([], { remove: [key(rowOf(now, read.wells.A5))] });
  // The rack's fields, the corner turned and back, a field from the chat: each drafts the rows again.
  await http('POST', `/api/chat/proposals/${id}/rack`, { racks: [{ rack: '1', estanteria: '2', fila_de_rack: '1', caja: '3' }] });
  await call('rack_deposit', { racks: [{ rack: '1', code_beside: 'H1' }] });
  await call('rack_deposit', { racks: [{ rack: '1', code_beside: 'A12' }], responsable: 'Ana P.' });
  now = await p();
  const a2 = rowOf(now, read.wells.A2);
  assert.equal(a2.values.Batch, 'B7');
  assert.ok(a2.personEdits?.Batch, 'still the person\'s');
  assert.equal(rowOf(now, 'FS99000003')?.values.Tube_location, 'A3');
  assert.equal(rowOf(now, read.wells.A4)?.values.Tube_location, 'A1');
  assert.ok(!rowOf(now, read.wells.A5), 'a row taken out stays out');
  assert.deepEqual(now.deposit.left_out, [read.wells.A5]);
  // Brought back from the grid.
  const back = await http('POST', `/api/chat/proposals/${id}/rack`, { restore: [read.wells.A5] });
  assert.ok(rowOf(back.body.proposal, read.wells.A5));
  // The same rack read again keeps the wells the person set.
  await call('rack_read_photos', { photos: ['thread-1-rack.jpg'] });
  now = await p();
  assert.equal(now.deposit.racks[0].wells.A3.t, 'FS99000003');
  assert.equal(rowOf(now, read.wells.A2).values.Batch, 'B7');
});

test('the assistant changes the rows through the rack too: update_proposal refuses what the rack decides; rack_deposit move', async () => {
  const { read, p, rowOf, call, id } = await editable();
  const now = await p();
  const index = now.changes.indexOf(rowOf(now, read.wells.A2));
  const refused = await call('update_proposal', { proposalId: id, rows: [{ index, values: { Tube_location: 'A1' } }] });
  assert.match(refused.error, /follow its rack.*rack_deposit/);
  const batch = await call('update_proposal', { proposalId: id, rows: [{ index, values: { Batch: 'B9' } }] });
  assert.ok(!batch.error, JSON.stringify(batch));
  const moved = await call('rack_deposit', { racks: [{ rack: '1', move: { A2: 'A1' } }] });
  assert.ok(!moved.error, JSON.stringify(moved));
  const after = await p();
  assert.equal(after.deposit.racks[0].wells.A1.t, read.wells.A2);
  assert.equal(rowOf(after, read.wells.A2).values.Tube_location, 'A1');
  assert.equal(rowOf(after, read.wells.A2).values.Batch, 'B9', 'the assistant\'s own edit carried too');
  const onto = await call('rack_deposit', { racks: [{ rack: '1', move: { A3: 'A4' } }] });
  assert.deepEqual(onto.racks[0].conflicts, [{ tube: read.wells.A3, from: 'A3', to: 'A4', other: read.wells.A4 }]);
});

test('the rack turned after edits in its rows: the person\'s tube and a waiting conflict turn with it, nothing lost', async () => {
  const { read, p, rowOf, key, edit, http, id, call } = await editable();
  let now = await p();
  await edit([
    { key: key(rowOf(now, read.wells.A2)), field: 'Tube_ID', value: 'FS99000001' },
    { key: key(rowOf(now, read.wells.A3)), field: 'Tube_location', value: 'A4' },
  ]);
  await call('rack_deposit', { racks: [{ rack: '1', code_beside: 'H1' }] });
  now = await p();
  let rack = now.deposit.racks[0];
  // A2 of the old corner is H11 now: the person's tube is there, the tube the photos put at A2 stays.
  assert.equal(rack.wells.H11.t, 'FS99000001');
  assert.equal(rack.wells.A2.t, read.wells.H11);
  assert.ok(!Object.values(rack.wells).some(w => w.t === read.wells.A2), 'the replaced ID is gone from the grid');
  assert.deepEqual(rack.conflicts, [{ tube: read.wells.A3, from: 'H10', to: 'H9', other: read.wells.A4 }]);
  const applied = await http('POST', `/api/chat/proposals/${id}/apply`, { requestId: 'apply-conflict-turned' });
  assert.equal(applied.status, 409);
  await call('rack_deposit', { racks: [{ rack: '1', code_beside: 'A12' }] });
  now = await p();
  rack = now.deposit.racks[0];
  assert.equal(rack.wells.A2.t, 'FS99000001');
  assert.deepEqual(rack.conflicts, [{ tube: read.wells.A3, from: 'A3', to: 'A4', other: read.wells.A4 }]);
  assert.equal(rowOf(now, read.wells.A3).values.Tube_location, 'A4');
});

test('a tube ID typed while its move waits on a conflict: the move goes on under the new ID', async () => {
  const { read, p, rowOf, key, edit } = await editable();
  await edit([{ key: key(rowOf(await p(), read.wells.A3)), field: 'Tube_location', value: 'A4' }]);
  const out = await edit([{ key: key(rowOf(await p(), read.wells.A3)), field: 'Tube_ID', value: 'FS99000003' }]);
  assert.deepEqual(out.body.rejected, []);
  assert.deepEqual(out.body.proposal.deposit.racks[0].conflicts, [{ tube: 'FS99000003', from: 'A3', to: 'A4', other: read.wells.A4 }]);
  assert.equal(rowOf(out.body.proposal, 'FS99000003').values.Tube_location, 'A4');
});

test('one save with a tube ID and another column of an existing row: both kept (the row\'s new key followed)', async () => {
  const { read, p, rowOf, key, edit } = await editable();
  const c1 = rowOf(await p(), read.wells.C1);
  assert.ok(!c1.create, 'C1 is an existing row of the sheet');
  const out = await edit([
    { key: key(c1), field: 'Tube_ID', value: 'FS99000002' },
    { key: key(c1), field: 'Batch', value: 'B5' },
  ]);
  assert.deepEqual(out.body.rejected, []);
  const row = rowOf(out.body.proposal, 'FS99000002');
  assert.equal(row.values.Tube_location, 'C1');
  assert.equal(row.values.Batch, 'B5');
});

test('rows the assistant takes out go through the rack: removeRows by ID in any case refused, leave_out per rack as the tool says', async () => {
  const { read, p, rowOf, call, id } = await editable();
  const refused = await call('update_proposal', { proposalId: id, removeRows: [read.wells.A2.toLowerCase()] });
  assert.match(refused.error ?? '', /leave_out/);
  assert.ok(rowOf(await p(), read.wells.A2));
  await call('rack_deposit', { racks: [{ rack: '1', leave_out: [read.wells.A2] }] });
  assert.ok(!rowOf(await p(), read.wells.A2));
  await call('rack_deposit', { racks: [{ rack: '1', restore: [read.wells.A2] }] });
  assert.ok(rowOf(await p(), read.wells.A2));
});

// ------------------------------------------------------------ the changes to paste by hand (no edit access)
/** The entries of an .xlsx (our own zip: deflated, sizes in the local headers). */
function unzip(buf) {
  const out = {};
  let at = 0;
  while (buf.readUInt32LE(at) === 0x04034b50) {
    const size = buf.readUInt32LE(at + 18);
    const nameLength = buf.readUInt16LE(at + 26);
    const name = buf.subarray(at + 30, at + 30 + nameLength).toString();
    const start = at + 30 + nameLength;
    out[name] = inflateRawSync(buf.subarray(start, start + size)).toString();
    at = start + size;
  }
  return out;
}
/** A sheet's cells by reference: { A1: { v, s } } (inline strings and numbers). */
function cells(sheetXml) {
  const out = {};
  for (const m of sheetXml.matchAll(/<c r="([A-Z]+\d+)" s="(\d+)"(?: t="inlineStr")?(?:\/>|>(?:<v>([^<]*)<\/v>|<is><t[^>]*>([^<]*)<\/t><\/is>)<\/c>)/g))
    out[m[1]] = { v: m[3] !== undefined ? Number(m[3]) : (m[4] ?? null), s: Number(m[2]) };
  return out;
}
const FORMULAS = { Genus: '=1', Genera_0176: '=1', DATE_OF_COLLECTION: '=1', 'Next export permit': '=1', COLLECTOR_SAMPLE_ID: '=1', FILTER: '=1' };

test('changes .xlsx: refused while wells are open; then new rows in the sheet\'s column order, Sort going on, the changed row highlighted, Léeme', async () => {
  const { call, http, store, rackReportsDir, user } = await setup({ protect: true });
  // The sheet's last row has the formula columns (Genus … FILTER): left to the sheet, after Exclude.
  store.db.prepare('UPDATE records SET formulas_json = ? WHERE sheet = ? AND row_num = 4').run(JSON.stringify(FORMULAS), SHEET);
  const read = await call('rack_read_photos', { photos: ['thread-1-rack.jpg'] });
  const id = read.deposit.proposalId;
  assert.equal(read.deposit.changes_download, undefined, 'not while a well is open');
  const refused = await http('POST', `/api/chat/proposals/${id}/rack-changes`, {});
  assert.equal(refused.status, 409);
  assert.ok(refused.body.error.details.problems.some(p => /no tube for A1/.test(p)));
  const early = await http('POST', `/api/chat/proposals/${id}/rack-by-hand`, {});
  assert.equal(early.status, 409, 'not marked by hand before the changes were downloaded');
  // Every well said (the deposit sheet's own fields not given: they do not block the changes).
  const open = read.missing.map(m => m.well);
  const done = await call('rack_deposit', { racks: [{ rack: '1', fill: Object.fromEntries(open.map(w => [w, expected(w)])) }] });
  assert.match(done.changes_download, /^\[Cambios-tube-locations-\d{4}-\d\d-\d\d-FK00274230\.xlsx\]\(https:\/\/app\.test\/api\/rack-reports\//);
  assert.deepEqual(done.changes_rows, { new: 94, changed: 1 });
  assert.match(done.status, /pasted into the sheet by someone with access/);
  const made = await http('POST', `/api/chat/proposals/${id}/rack-changes`, {});
  assert.equal(made.status, 200);
  assert.equal(made.body.download, /\((.+)\)$/.exec(done.changes_download)[1], 'the same rows: the same file');
  assert.equal(made.body.proposal.deposit.protected, true);
  const [, token, file] = /\/api\/rack-reports\/([a-f0-9]{24})\/(.+)$/.exec(made.body.download);
  const xlsx = unzip(readFileSync(reportFile(rackReportsDir, token, file, user).path));
  assert.match(xlsx['xl/workbook.xml'], /name="Filas nuevas".*name="Filas cambiadas".*name="Léeme"/);
  const added = cells(xlsx['xl/worksheets/sheet1.xml']);
  assert.deepEqual(
    ['A1', 'B1', 'C1', 'D1', 'E1', 'K1'].map(r => added[r].v),
    ['Sort', 'Tube_ID', 'Rack_ID', 'Tube_location', 'Batch', 'Exclude'],
  );
  assert.equal(added.L1, undefined, 'formula columns left out');
  // A1 first (fill order), Sort going on from the sheet's 12.
  assert.deepEqual(['A2', 'B2', 'C2', 'D2'].map(r => added[r].v), [13, expected('A1'), 'FK00274230', 'A1']);
  assert.equal(added.A95.v, 13 + 93);
  assert.equal(added.A96, undefined);
  const moved = cells(xlsx['xl/worksheets/sheet2.xml']);
  assert.deepEqual(['A2', 'B2', 'C2'].map(r => moved[r].v), [3, expected('C1'), 'Rack_ID (C), Tube_location (D)']);
  assert.deepEqual(['D2', 'E2', 'F2', 'G2'].map(r => moved[r].v), ['FK00999999', 'FK00274230', 'E5', 'C1']);
  assert.deepEqual([moved.E2.s, moved.G2.s, moved.D2.s], [6, 6, 1], 'the new values highlighted');
  const readme = xlsx['xl/worksheets/sheet3.xml'];
  for (const said of ['Racks?: FK00274230', 'Hecho por: Franz Chandi', 'la 5, según la última lectura', 'Sort sigue la numeración: 13–106', 'L–Q (Genus', 'Aplicado a mano', expected('A2'), 'FS11111111 (FK00274230 H12)'])
    assert.ok(new RegExp(said.replace(/[()]/g, '\\$&')).test(readme), said);
  assert.ok(!readme.includes(`${expected('A1')} (`), 'A1 has its Collection_data row: not listed as needing rows');
  // Through rack_report too, beside the deposit sheet (refused there: its fields are not given).
  const report = await call('rack_report', {});
  assert.match(report.error, /Not made yet/);
  assert.equal(report.changes_download, done.changes_download);
  // Pasted by hand: out of the pending list, who and when; nothing written.
  const marked = await http('POST', `/api/chat/proposals/${id}/rack-by-hand`, {});
  assert.equal(marked.status, 200);
  assert.equal(marked.body.proposal.status, 'closed');
  assert.equal(marked.body.proposal.deposit.manual.by, 'Franz Chandi');
  assert.ok(marked.body.proposal.changes.every(c => !c.applied), 'nothing written');
  assert.equal((await http('POST', `/api/chat/proposals/${id}/rack-by-hand`, {})).status, 409);
});

test('«Aplicado a mano» refused when the rows changed after the download; access given later shows at once', async () => {
  const { call, http, store, list } = await setup({ protect: true });
  const read = await call('rack_read_photos', { photos: ['thread-1-rack.jpg'] });
  const id = read.deposit.proposalId;
  await call('rack_deposit', { racks: [{ rack: '1', empty: read.missing.map(m => m.well) }] });
  assert.equal((await http('POST', `/api/chat/proposals/${id}/rack-changes`, {})).status, 200);
  // A row taken out in the table (no new file handed out): what was downloaded is no longer what it says.
  let [p] = await list();
  const a2 = p.changes.find(c => c.create && c.values.Tube_ID === read.wells.A2);
  assert.equal((await http('POST', `/api/chat/proposals/${id}/edit`, { cells: [], remove: [a2.key ?? a2.clientId] })).status, 200);
  const stale = await http('POST', `/api/chat/proposals/${id}/rack-by-hand`, {});
  assert.equal(stale.status, 409);
  assert.match(stale.body.error.message, /cambiaron después de la descarga/);
  [p] = await list();
  assert.equal(p.status, 'pending');
  assert.equal(p.deposit.protected, true);
  // The account gets edit access (the next sync notes no locked range): «Aplicar» is back, nothing else changed.
  store.db.prepare('UPDATE settings SET value = ? WHERE key = ?').run('[]', `protected:${SHEET}`);
  [p] = await list();
  assert.equal(p.deposit.protected, false);
  const answer = await call('rack_deposit', {});
  assert.equal(answer.protected, undefined);
  assert.match(answer.status, /«Aplicar»/);
});

test('changes .xlsx of several racks: one file, each tube once; the cells to write in amber', () => {
  const columns = ['Sort', 'Tube_ID', 'Rack_ID', 'Tube_location'].map((key, k) => ({ key, letter: 'ABCD'[k] }));
  const sheets = changesSheets({
    columns,
    added: [
      { Sort: 7, Tube_ID: 'FS1', Rack_ID: 'FK00000001', Tube_location: 'A1' },
      { Sort: 8, Tube_ID: 'FS2', Rack_ID: 'FK00000002', Tube_location: 'A1' },
    ],
    changed: [{ tube: 'FS3', row: 9, cells: [{ key: 'Tube_location', letter: 'D', was: 'B2', now: 'B3' }], current: { Rack_ID: 'FK00000002', Tube_location: 'B2' } }],
    readme: [{ title: 'Cambios' }, 'Racks: FK00000001, FK00000002'],
  });
  assert.deepEqual(sheets.map(s => s.name), ['Filas nuevas', 'Filas cambiadas', 'Léeme']);
  const xlsx = unzip(workbook(sheets));
  const added = cells(xlsx['xl/worksheets/sheet1.xml']);
  assert.deepEqual([added.C2.v, added.C3.v], ['FK00000001', 'FK00000002']);
  const moved = cells(xlsx['xl/worksheets/sheet2.xml']);
  assert.deepEqual(['A1', 'D1', 'E1'].map(r => moved[r].v), ['Fila en la hoja', 'Tube_location (D) ahora', 'Tube_location nuevo']);
  assert.deepEqual([moved.D2.v, moved.E2.v, moved.E2.s], ['B2', 'B3', 6]);
  assert.match(xlsx['xl/styles.xml'], /<cellXfs count="9">/);
});

test('identify_species names the live head (alias, b8127dbf), not the release of 14 Oct', () => {
  const tool = DRIVE_PHOTO_TOOLS.find(t => t.function.name === 'identify_species');
  const text = tool.function.description;
  assert.ok(!/release of 14 Oct/.test(text));
  assert.match(text, /head_hier_b8127dbf\.pt/);
  assert.match(text, /measured with the earlier head/);
});
