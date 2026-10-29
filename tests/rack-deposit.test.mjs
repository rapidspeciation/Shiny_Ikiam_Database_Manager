import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { copyFileSync, mkdirSync, mkdtempSync, readFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { LocalDrive } from '../server/drive.mjs';
import { loadPhoto, readOne } from '../server/rack-reader.mjs';
import { DRIVE_FOLDER, reportFile } from '../server/rack-tools.mjs';
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
  assert.match(read.deposit.protected, /cannot write .*«Aplicar» will be refused/);
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
