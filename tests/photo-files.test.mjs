import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { readOrientation } from '../server/orientation.mjs';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { FOLDER, GoogleDrive, LocalDrive, driveTree } from '../server/drive.mjs';
import { planPhotoLinks } from '../server/photo-links.mjs';
import { changeNeeds, describeChanges, kindOf, leaveOutChanges, planFileChanges, runFileChanges, settleChanges, undoFileChanges } from '../server/photo-files.mjs';
import { GoogleTransferSheet, LocalTransferSheet, parseTransfer } from '../server/transfer-sheet.mjs';
import { createAssistant, mcpTools } from '../server/assistant.mjs';
import { moduleMap } from '../server/schema.mjs';

const ME = 'project@example.org';
const id = n => `drive-id-${String(n).padStart(20, '0')}`;
const folderUrl = n => `https://drive.google.com/drive/folders/${id(n)}`;
const fileUrl = n => `https://drive.google.com/file/d/${id(n)}/view?usp=drivesdk`;
const folder = (n, name, parent) => ({ id: id(n), name, mimeType: FOLDER, parents: parent ? [id(parent)] : [], webViewLink: folderUrl(n), createdTime: '2026-01-14T19:35:00.000Z', owners: [{ emailAddress: 'curator@example.org' }] });
const file = (n, name, parent, extra = {}) => ({
  id: id(n),
  name,
  mimeType: 'image/jpeg',
  parents: [id(parent)],
  webViewLink: fileUrl(n),
  createdTime: '2026-07-21T20:21:46.221Z',
  size: '2048',
  owners: [{ emailAddress: 'curator@example.org' }],
  imageMediaMetadata: { time: '2026:07:20 10:15:30' },
  ...extra,
});
const mine = { owners: [{ emailAddress: ME }] };
const row = (n, path, type, extra = {}) => ({
  'Full Path': path,
  Name: path.split('/').at(-1),
  Type: type,
  URL: type === 'Folder' ? folderUrl(n) : fileUrl(n),
  'Size KB': type === 'Folder' ? 0 : 2,
  'Owner Email': 'curator@example.org',
  ...extra,
});

// Photos (1) › Wild (2, listed) › Peru (3) with JPG (4) and RAW (5), Panama (6) with JPG (7) and RAW (8).
const drive = () => [
  folder(1, 'Photos'),
  folder(2, 'Wild', 1),
  folder(3, 'Peru', 2),
  folder(4, 'JPG', 3),
  folder(5, 'RAW', 3),
  folder(6, 'Panama', 2),
  folder(7, 'JPG', 6),
  folder(8, 'RAW', 6),
  // Peru: a photo named with an underscore, with its RAW file.
  file(40, 'CAM000001_d.jpg', 4),
  file(50, 'CAM000001_d.CR2', 5, { size: '9000' }),
  file(41, 'CAM000002d.JPG', 4),
  // A Panama butterfly's photos uploaded to Peru too: the same files, the project's own.
  file(42, 'CAM000003d.JPG', 4, mine),
  file(51, 'CAM000003d.CR2', 5, { ...mine, size: '9000' }),
  // …and one that is the curator's.
  file(43, 'CAM000004d.JPG', 4),
  file(70, 'CAM000003d.JPG', 7),
  file(80, 'CAM000003d.CR2', 8, { size: '9000' }),
  file(71, 'CAM000004d.JPG', 7),
  // Left by «Copy files»: an original beside its copy (the project's own), and one whose copy is not there.
  file(72, 'CAM000005d.JPG_todelete', 7, mine),
  file(73, 'CAM000005d.JPG', 7),
  file(74, 'CAM000006d.JPG_todelete', 7, mine),
  file(75, 'CAM000007d.JPG_todelete', 7),
  file(76, 'CAM000007d.JPG', 7),
];
const ids = new Set(['CAM000001', 'CAM000002', 'CAM000003', 'CAM000004', 'CAM000005', 'CAM000006', 'CAM000007']);
const sheetRows = () => [
  row(2, 'Wild', 'Folder'),
  row(40, 'Wild/Peru/JPG/CAM000001_d.jpg', 'File'),
  row(41, 'Wild/Peru/JPG/CAM000002d.JPG', 'File'),
  // The Peru copies were listed first.
  row(42, 'Wild/Peru/JPG/CAM000003d.JPG', 'File', { 'Owner Email': ME }),
  row(43, 'Wild/Peru/JPG/CAM000004d.JPG', 'File'),
];
async function reading(files = drive()) {
  const local = new LocalDrive(files, { me: ME });
  const items = await driveTree(local, [id(1)]);
  const rows = sheetRows().map((values, i) => ({ id: `r${i + 2}`, row: i + 2, values }));
  return { local, index: planPhotoLinks({ rows, items, home: id(1), ids }).index };
}
const told = ops => ops.map(o => [kindOf(o), `${o.folder}/${o.name}`, o.to ?? null]);

test('file selection keeps dependencies and remaps folders without mutating the plan', () => {
  const ops = [
    { action: 'rename', id: 'photo', name: 'before.jpg', to: 'after.jpg' },
    { action: 'rename', id: 'raw', photo: 'photo', raw: true, name: 'before.CR2', to: 'after.CR2' },
    { action: 'folder', name: 'new', toId: 'parent' },
    { action: 'move', id: 'moving', toNew: 2 },
    { action: 'remove', id: 'foreign', how: 'mark', owner: 'owner@example.org' },
    { action: 'transfer', auto: true, others: 0, by: [{ account: 'owner@example.org', files: 1 }] },
    { action: 'rotate', id: 'turn', degrees: 90 },
  ];
  const needs = changeNeeds(ops);
  assert.deepEqual(needs[1].all, [{ on: 0, why: 'raw' }]);
  assert.deepEqual(needs[3].all, [{ on: 2, why: 'folder' }]);
  assert.deepEqual(needs[4].all, [{ on: 5, why: 'handover' }]);
  assert.deepEqual(needs[5].any, [4]);
  const onlyTurn = leaveOutChanges(ops, [0, 1, 2, 3, 4, 5]);
  assert.deepEqual(onlyTurn.kept, [6]);
  assert.deepEqual(onlyTurn.ops, [{ action: 'rotate', id: 'turn', degrees: 90 }]);
  const withoutFolder = leaveOutChanges(ops, [0, 2]);
  assert.deepEqual(withoutFolder.kept, [4, 5, 6]);
  assert.deepEqual(withoutFolder.dropped.map(d => [d.index, d.why]), [[1, 'raw'], [3, 'folder']]);
  const withoutMark = leaveOutChanges(ops, [4]);
  assert.deepEqual(withoutMark.dropped.map(d => [d.index, d.why]), [[5, 'mark']]);
  assert.deepEqual(ops[3].toNew, 2);
  assert.throws(() => leaveOutChanges(ops, [-1]), RangeError);
  assert.throws(() => leaveOutChanges(ops, [ops.length]), RangeError);
  assert.throws(() => leaveOutChanges(ops, ['0']), RangeError);
});

test('a plan: files by path or name, a RAW file with its photo, a folder made and filled', async () => {
  const { local, index } = await reading();
  const plan = await settleChanges(
    local,
    planFileChanges({
      index,
      operations: [
        { action: 'rename', file: 'Peru/JPG/CAM000001_d.jpg', name: 'CAM000001d.JPG' },
        { action: 'folder', name: 'Ecuador', in: 'Wild' },
        { action: 'move', file: 'CAM000002d.JPG', to: 'Wild/Ecuador' },
        { action: 'remove', file: fileUrl(42) },
        { action: 'remove', file: 'Peru/JPG/CAM000004d.JPG' },
        // Not planned: no such file, a name two files have, a second change of a file.
        { action: 'remove', file: 'CAM000099d.JPG' },
        { action: 'remove', file: 'CAM000003d.JPG' },
        { action: 'rename', file: fileUrl(42), name: 'x.JPG' },
      ],
    }),
  );
  assert.deepEqual(told(plan.ops), [
    ['rename', 'Wild/Peru/JPG/CAM000001_d.jpg', 'CAM000001d.JPG'],
    ['rename', 'Wild/Peru/RAW/CAM000001_d.CR2', 'CAM000001d.CR2'],
    ['folder', 'Wild/Ecuador', null],
    ['move', 'Wild/Peru/JPG/CAM000002d.JPG', 'Wild/Ecuador'],
    // The project's own file is binned, its RAW file with it; the curator's is renamed for its owner to delete.
    ['bin', 'Wild/Peru/JPG/CAM000003d.JPG', null],
    ['bin', 'Wild/Peru/RAW/CAM000003d.CR2', null],
    ['mark', 'Wild/Peru/JPG/CAM000004d.JPG', 'CAM000004d.JPG_todelete'],
  ]);
  assert.equal(plan.ops[1].raw, true);
  assert.deepEqual(
    plan.problems.map(p => p.error.replace(/".*/, '')),
    ['no file ', '2 files are named ', 'two changes of it (remove and rename)'],
  );
  const said = describeChanges(plan.ops, plan.problems);
  assert.deepEqual(Object.fromEntries(Object.entries(said).map(([k, v]) => [k, v.count])), { rename: 2, newFolders: 1, move: 1, binned: 2, renamedToDelete: 1, leftOut: 3 });
  assert.equal(said.rename.rawFilesWithTheirPhoto, 1);

  await runFileChanges(local, plan.ops);
  assert.ok(plan.ops.every(o => o.done));
  const now = await driveTree(local, [id(1)]);
  const names = parent => [...now.values()].filter(f => f.parent === parent).map(f => f.name).sort();
  assert.deepEqual(names(id(4)), ['CAM000001d.JPG', 'CAM000004d.JPG_todelete']);
  assert.deepEqual(names(id(5)), ['CAM000001d.CR2']);
  const ecuador = [...now.values()].find(f => f.name === 'Ecuador');
  assert.deepEqual(names(ecuador.id), ['CAM000002d.JPG']);

  // Taken back: names, folders and the bin as they were (the folder made stays).
  assert.deepEqual(await undoFileChanges(local, plan.ops), { undone: 6, failed: 0 });
  const back = await driveTree(local, [id(1)]);
  assert.deepEqual([...back.values()].filter(f => f.parent === id(4)).map(f => f.name).sort(), ['CAM000001_d.jpg', 'CAM000002d.JPG', 'CAM000003d.JPG', 'CAM000004d.JPG']);
});

test('a photo turned by its orientation tag, its RAW file too, beside a rename; the same files; taken back', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'photo-turn-'));
  try {
    // A JPEG and a TIFF-based RAW file, each with its orientation tag (upright).
    const tiff = () => {
      const out = new Uint8Array(8 + 2 + 12 + 4);
      const view = new DataView(out.buffer);
      out.set([0x49, 0x49]);
      view.setUint16(2, 42, true);
      view.setUint32(4, 8, true);
      view.setUint16(8, 1, true);
      view.setUint16(10, 0x0112, true);
      view.setUint16(12, 3, true);
      view.setUint32(14, 1, true);
      view.setUint16(18, 1, true);
      return out;
    };
    const exif = [...Buffer.from('Exif\0\0'), ...tiff()];
    const jpeg = new Uint8Array([0xff, 0xd8, 0xff, 0xe1, (exif.length + 2) >> 8, (exif.length + 2) & 0xff, ...exif, 0xff, 0xda, 0, 2, 1, 0xff, 0xd9]);
    writeFileSync(join(dir, 'photo.jpg'), jpeg);
    writeFileSync(join(dir, 'photo.cr2'), tiff());
    const files = drive().map(f => (f.id === id(40) ? { ...f, localPath: join(dir, 'photo.jpg') } : f.id === id(50) ? { ...f, localPath: join(dir, 'photo.cr2') } : f));
    const { local, index } = await reading(files);
    const plan = planFileChanges({
      index,
      operations: [
        { action: 'rename', file: fileUrl(40), name: 'CAM000001d.JPG' },
        { action: 'rotate', file: fileUrl(40), degrees: 90 },
        { action: 'rotate', file: fileUrl(41), degrees: 45 },
      ],
    });
    assert.deepEqual(
      plan.ops.map(o => [kindOf(o), o.name, o.degrees ?? o.to]),
      [
        ['rename', 'CAM000001_d.jpg', 'CAM000001d.JPG'],
        ['rename', 'CAM000001_d.CR2', 'CAM000001d.CR2'],
        ['rotate', 'CAM000001_d.jpg', 90],
        ['rotate', 'CAM000001_d.CR2', 90],
      ],
    );
    assert.match(plan.problems[0].error, /degrees, 90, 180 or 270/);
    assert.equal(describeChanges(plan.ops, []).turned.count, 2);
    assert.deepEqual(leaveOutChanges(plan.ops, [0, 1]).kept, [2, 3]);
    assert.deepEqual(leaveOutChanges(plan.ops, [2, 3]).kept, [0, 1]);

    const tagOf = async fileId => {
      const path = join(dir, `read-${fileId}`);
      await local.download(fileId, path);
      return readOrientation(new Uint8Array(readFileSync(path))).value;
    };
    await runFileChanges(local, plan.ops);
    assert.ok(plan.ops.every(o => o.done), JSON.stringify(plan.ops.map(o => o.error)));
    assert.deepEqual([await tagOf(id(40)), await tagOf(id(50))], [6, 6]);
    assert.equal((await local.get(id(40))).name, 'CAM000001d.JPG');
    assert.deepEqual(await undoFileChanges(local, plan.ops), { undone: 4, failed: 0 });
    assert.deepEqual([await tagOf(id(40)), await tagOf(id(50))], [1, 1]);
    // A file with no tag to change says so.
    const bare = planFileChanges({ index, operations: [{ action: 'rotate', file: fileUrl(41), degrees: 180 }] });
    await runFileChanges(local, bare.ops);
    assert.match(bare.ops[0].error, /no bytes|no orientation tag/);
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});

test('the fixes: names the sheets look up, files that are in another folder too, originals already copied', async () => {
  const { local, index } = await reading();
  const renamed = planFileChanges({ index, fix: [{ check: 'rename', in: 'Peru' }] });
  assert.deepEqual(told(renamed.ops), [
    ['rename', 'Wild/Peru/JPG/CAM000001_d.jpg', 'CAM000001d.JPG'],
    ['rename', 'Wild/Peru/RAW/CAM000001_d.CR2', 'CAM000001d.CR2'],
  ]);
  assert.match(planFileChanges({ index, fix: [{ check: 'rename', in: 'Panama' }] }).problems[0].error, /no photo there needs another name/);

  // Every file of Peru that Panama has too (the same name and size), photos and RAW files.
  const copies = await settleChanges(local, planFileChanges({ index, fix: [{ check: 'copies', in: 'Wild/Peru' }] }));
  assert.deepEqual(told(copies.ops), [
    ['bin', 'Wild/Peru/JPG/CAM000003d.JPG', null],
    ['mark', 'Wild/Peru/JPG/CAM000004d.JPG', 'CAM000004d.JPG_todelete'],
    ['bin', 'Wild/Peru/RAW/CAM000003d.CR2', null],
  ]);
  assert.match(planFileChanges({ index, fix: [{ check: 'copies' }] }).problems[0].error, /give the folder/);
  // Where each stays, and only those that stay in the folder named.
  assert.deepEqual(describeChanges(copies.ops, []).binned.sameFileKeptIn, { 'Wild/Panama/JPG': 1, 'Wild/Panama/RAW': 1 });
  assert.deepEqual(told(planFileChanges({ index, fix: [{ check: 'copies', in: 'Wild/Peru', keptIn: 'Panama/RAW' }] }).ops), [['mark', 'Wild/Peru/RAW/CAM000003d.CR2', null]]);
  assert.match(planFileChanges({ index, fix: [{ check: 'copies', in: 'Wild/Peru', keptIn: 'Peru/RAW' }] }).problems[0].error, /no file there is also in another listed folder/);

  // …_todelete beside its copy: binned when the project's own; another account's is left to its owner; no copy, not touched.
  const copied = await settleChanges(local, planFileChanges({ index, fix: [{ check: 'copied', in: 'Panama' }] }));
  assert.deepEqual(told(copied.ops), [['bin', 'Wild/Panama/JPG/CAM000005d.JPG_todelete', null]]);
  assert.match(copied.problems[0].error, /1 …_todelete files belong to another account/);
});

test('copies: a removal that takes away the file the sheets look up is planned, marked and said', async () => {
  // CAM000002 twice, the same file: Peru as the sheets look it up, Panama with an underscore.
  const { index } = await reading([...drive(), file(44, 'CAM000002_d.jpg', 7)]);
  const peru = planFileChanges({ index, fix: [{ check: 'copies', in: 'Wild/Peru', keptIn: 'Wild/Panama' }] });
  const lost = peru.ops.find(o => o.name === 'CAM000002d.JPG');
  assert.equal(lost.lookedUp, true);
  assert.equal(lost.keptAs, 'CAM000002_d.jpg');
  assert.ok(!peru.ops.find(o => o.name === 'CAM000003d.JPG').lookedUp);
  assert.equal(peru.problems[0].warning, true);
  assert.match(peru.problems[0].error, /1 of these removals take away the file the sheets look up .*CAM000002d\.JPG removed, CAM000002_d\.jpg kept in Wild\/Panama\/JPG.*in: "Wild\/Panama\/JPG", keptIn: "Wild\/Peru\/JPG"/);
  // The other way round: the underscore copy goes, nothing to say.
  const panama = planFileChanges({ index, fix: [{ check: 'copies', in: 'Wild/Panama/JPG', keptIn: 'Wild/Peru/JPG' }] });
  const op = panama.ops.find(o => o.name === 'CAM000002_d.jpg');
  assert.equal(op.keptAs, 'CAM000002d.JPG');
  assert.ok(!op.lookedUp);
  assert.deepEqual(panama.problems, []);
});

test('shortcuts to the photos of a list of specimens, in a folder', async () => {
  const { local, index } = await reading([...drive(), folder(9, 'Shared', 1), file(44, 'CAM000002v.JPG', 4)]);
  const plan = await settleChanges(local, planFileChanges({ index, operations: [{ action: 'shortcut', ids: ['cam000002', 'CAM000099'], to: 'Shared' }] }));
  assert.deepEqual(told(plan.ops), [
    ['shortcut', 'Wild/Peru/JPG/CAM000002d.JPG', 'Shared'],
    ['shortcut', 'Wild/Peru/JPG/CAM000002v.JPG', 'Shared'],
  ]);
  assert.match(plan.problems[0].error, /no listed photo of 2 views: CAM000099d, CAM000099v/);
  await runFileChanges(local, plan.ops);
  const made = (await local.children(id(9))).map(f => [f.name, f.shortcutDetails.targetId]);
  assert.deepEqual(made.sort(), [
    ['CAM000002d.JPG', id(41)],
    ['CAM000002v.JPG', id(44)],
  ]);
  // The photos stay where they are; the shortcuts go with the undo.
  assert.equal((await local.children(id(4))).length, 5);
  await undoFileChanges(local, plan.ops);
  assert.equal((await local.children(id(9))).length, 0);
});

const cellsOf = (sheets, rowNumber) => {
  const fields = moduleMap.get('Photo_links').fields;
  const cells = sheets.rows.get('Photo_links').find(r => r.row === rowNumber)?.cells ?? [];
  return Object.fromEntries(fields.map(f => [f.key, cells[f.column]?.userEnteredValue?.stringValue ?? cells[f.column]?.userEnteredValue?.numberValue ?? null]));
};

test('organize_photos: a card, the changes made on Drive, Photo_links following, and all of it taken back', async () => {
  const sheets = new LocalSheets({
    Photo_links: sheetRows().map((values, i) => ({ row: i + 2, values })),
    Insectary_data: [...ids].map((cam, i) => ({ row: i + 2, values: { Insectary_ID: `A${i}B`, Sex: 'female', CAM_ID: cam } })),
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Photo_links', 'Insectary_data'] });
  store.db.prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('kim','kim','Kim','editor','s','h',1,'2026-01-01')").run();
  const local = new LocalDrive(drive(), { me: ME });
  const sheet = new LocalTransferSheet({ copiedBy: 'curator@example.org' });
  const assistant = createAssistant({ store, config: { drive: local, transferSheet: sheet } });
  store.db.prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')").run(createHash('sha256').update('kim-token').digest('hex'), 'kim');
  const kim = { id: 'kim', username: 'kim', role: 'editor' };
  const call = async (name, args) =>
    JSON.parse((await assistant.mcp({ authorization: 'Bearer kim-token' }, { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } })).body.result.content[0].text);
  const http = (method, path, body = {}) => assistant.handle({ method, path: `/api/chat/proposals${path}`, user: kim, query: {}, body });
  const names = async parent => (await local.children(id(parent))).map(f => f.name).sort();
  try {
    assert.ok(mcpTools().some(t => t.name === 'organize_photos'));
    // The project account's space, the folders where it still owns files, and who took the photos over before.
    const space = await call('drive_space', {});
    assert.deepEqual([space.account, space.limitGb, space.freeGb, space.nearlyFull ?? false], [ME, 100, 100, false]);
    // Where the account owns files, as Drive says; its originals already copied by another account.
    assert.deepEqual(space.ownByFolder.map(f => [f.folder, f.files, f.alreadyCopiedToDelete ?? 0]), [
      ['Photos › Wild › Peru › RAW', 1, 0],
      ['Photos › Wild › Panama › JPG', 2, 2],
      ['Photos › Wild › Peru › JPG', 1, 0],
    ]);
    assert.equal(space.ownByFolder[0].link, folderUrl(5));
    assert.equal(space.originalsAlreadyCopied.files, 2);
    assert.equal(space.takenOverBefore, 'curator@example.org');
    assert.equal((await assistant.handle({ method: 'GET', path: '/api/chat/drive-space', user: kim, query: {}, body: {} })).body.space.limitGb, 100);
    const plan = await call('organize_photos', { fix: [{ check: 'rename', in: 'Peru' }, { check: 'copies', in: 'Peru' }] });
    assert.match(plan.link, /propuestas/);
    assert.deepEqual(Object.fromEntries(Object.entries(plan.changes).map(([k, v]) => [k, v.count ?? v.step])), { rename: 2, binned: 2, renamedToDelete: 1, otherAccountStep: 'delete' });
    assert.deepEqual(plan.changes.rename.examples, ['Wild/Peru/JPG/CAM000001_d.jpg → CAM000001d.JPG']);

    const [card] = (await http('GET', '')).body.proposals;
    assert.equal(card.kind, 'photo_files');
    assert.equal(card.reason, 'Google Drive: photo files');
    assert.deepEqual(card.sync.counts, { rename: 2, bin: 2, mark: 1, sheet: 1 });
    assert.equal(card.sync.total, 6);
    assert.deepEqual(card.sync.handover, {
      step: 'delete',
      button: 'Delete copied files',
      folder: 'Wild/Peru/JPG',
      folderUrl: folderUrl(4),
      sheet: 'Transfer Ownership & Free Up Space',
      subfolders: true,
      permanent: false,
      by: [{ account: 'curator@example.org', files: 1 }],
      auto: true,
    });
    const rows = (await http('GET', `/${card.id}/photo-links/rows`)).body.rows;
    // The photo and its RAW file go together: each needs the other.
    assert.deepEqual(rows[0], { change: 'rename', folder: 'Wild/Peru/JPG', name: 'CAM000001_d.jpg', to: 'CAM000001d.JPG', url: fileUrl(40), needs: [{ on: 1, why: 'raw' }] });
    assert.equal(rows.filter(r => r.raw).length, 1);
    assert.deepEqual(rows[1].needs, [{ on: 0, why: 'raw' }]);
    // The sheet's step added for the curator's file needs that file marked; the file counts under its owner.
    assert.deepEqual(rows.at(-1).needsAny, [rows.findIndex(r => r.change === 'mark')]);
    assert.equal(rows.find(r => r.change === 'mark').owner, 'curator@example.org');
    assert.equal(rows.filter(r => r.copy).length, 3);
    assert.equal((await http('POST', `/${card.id}/photo-links/refresh`)).status, 409);
    // Nothing changed yet.
    assert.deepEqual(await names(4), ['CAM000001_d.jpg', 'CAM000002d.JPG', 'CAM000003d.JPG', 'CAM000004d.JPG']);

    // The chat does not make them: neither the tool nor apply_proposal. The card's button does.
    assert.equal((await call('organize_photos', { plan: plan.plan, confirmed: true })).status, 'pending');
    assert.equal((await call('apply_proposal', { proposalId: plan.plan })).status, 'pending');
    assert.deepEqual(await names(4), ['CAM000001_d.jpg', 'CAM000002d.JPG', 'CAM000003d.JPG', 'CAM000004d.JPG']);
    assert.equal((await http('POST', `/${card.id}/photo-links/apply`)).status, 202);
    const done = await call('organize_photos', { plan: plan.plan });
    assert.equal(done.status, 'done');
    assert.equal(done.filesChanged, 5);
    // Three rows follow their files; the RAW file renamed to its photo's name gets its own.
    assert.equal(done.photoLinksRowsUpdated, 4);
    // The curator's file was left to its owner: the sheet is ready for the folder holding it.
    assert.deepEqual(sheet.written, [{ folder: id(4), subfolders: true, permanent: false }]);
    assert.equal(done.otherAccountStep.button, 'Delete copied files');
    assert.deepEqual(done.otherAccountStep.by, [{ account: 'curator@example.org', files: 1 }]);
    assert.deepEqual(await names(4), ['CAM000001d.JPG', 'CAM000002d.JPG', 'CAM000004d.JPG_todelete']);
    assert.deepEqual(await names(5), ['CAM000001d.CR2']);
    // Photo_links: the renamed photo's row follows it; the rows of the copies removed take the files kept in Panama.
    assert.equal(cellsOf(sheets, 3).Name, 'CAM000001d.JPG');
    assert.equal(cellsOf(sheets, 3)['Full Path'], 'Wild/Peru/JPG/CAM000001d.JPG');
    assert.equal(cellsOf(sheets, 5).URL, fileUrl(70));
    assert.equal(cellsOf(sheets, 5)['Full Path'], 'Wild/Panama/JPG/CAM000003d.JPG');
    assert.equal(cellsOf(sheets, 6).URL, fileUrl(71));
    assert.equal(cellsOf(sheets, 6).Name, 'CAM000004d.JPG');
    const applied = store.db.prepare('SELECT status, sync_json FROM ai_proposals WHERE id = ?').get(card.id);
    assert.equal(applied.status, 'applied');
    // Its card still shows the changes it made.
    assert.deepEqual(
      (await http('GET', `/${card.id}/photo-links/rows`)).body.rows,
      rows.map(({ needs: _needs, needsAny: _any, owner: _owner, ...r }) => r),
    );
    const progress = JSON.parse(applied.sync_json).progress;
    assert.deepEqual({ ...progress, handover: progress.handover.step }, { state: 'done', written: 6, total: 6, phase: 'sheet', sheetRows: 4, handover: 'delete', handoverWritten: true });

    // Taken back: the files, and the sheet's rows.
    const undone = await call('organize_photos', { undo: plan.plan });
    assert.deepEqual(undone, { status: 'undone', filesAsTheyWere: 5, photoLinksSavesUndone: 1 });
    assert.equal((await call('organize_photos', { plan: plan.plan })).status, 'undone');
    assert.deepEqual(await names(4), ['CAM000001_d.jpg', 'CAM000002d.JPG', 'CAM000003d.JPG', 'CAM000004d.JPG']);
    assert.equal(cellsOf(sheets, 3).Name, 'CAM000001_d.jpg');
    assert.equal(cellsOf(sheets, 5).URL, fileUrl(42));
    assert.match((await call('organize_photos', { undo: plan.plan })).error, /taken back already/);

    // From the card's own button.
    const again = await call('organize_photos', { operations: [{ action: 'remove', file: 'Peru/JPG/CAM000003d.JPG' }] });
    assert.equal((await http('POST', `/${again.plan}/photo-links/apply`)).status, 202);
    for (let i = 0; i < 200 && store.db.prepare('SELECT status FROM ai_proposals WHERE id = ?').get(again.plan).status !== 'applied'; i++)
      await new Promise(r => setTimeout(r, 10));
    assert.deepEqual(await names(4), ['CAM000001_d.jpg', 'CAM000002d.JPG', 'CAM000004d.JPG']);
    assert.equal(cellsOf(sheets, 5).URL, fileUrl(70));
    // A file renamed to a name the sheet lists from now on gets its row (a …_todelete one given its own name).
    const named = await call('organize_photos', { operations: [{ action: 'rename', file: fileUrl(75), name: 'CAM000007v.JPG' }] });
    assert.equal((await http('POST', `/${named.plan}/photo-links/apply`)).status, 202);
    for (let i = 0; i < 200 && store.db.prepare('SELECT status FROM ai_proposals WHERE id = ?').get(named.plan).status !== 'applied'; i++)
      await new Promise(r => setTimeout(r, 10));
    const rowsNamed = name => sheets.rows.get('Photo_links').filter(r => cellsOf(sheets, r.row).Name === name).length;
    assert.equal(rowsNamed('CAM000007v.JPG'), 1);
    // A failed mutation can be retried without repeating the photo and RAW that already succeeded.
    const originalRename = local.rename.bind(local);
    let renamed40 = 0;
    let fail41 = true;
    local.rename = async (fileId, name) => {
      if (fileId === id(40)) renamed40++;
      if (fileId === id(41) && fail41) {
        fail41 = false;
        throw new Error('temporary Drive error');
      }
      return originalRename(fileId, name);
    };
    const partial = await call('organize_photos', { operations: [
      { action: 'rename', file: fileUrl(40), name: 'CAM000001x.JPG' },
      { action: 'rename', file: fileUrl(41), name: 'CAM000002x.JPG' },
    ] });
    assert.equal((await http('POST', `/${partial.plan}/photo-links/apply`, { leaveOut: [99] })).status, 409);
    assert.equal((await http('POST', `/${partial.plan}/photo-links/apply`, { leaveOut: [] })).status, 202);
    let partialDone;
    for (let i = 0; i < 200; i++) {
      partialDone = await call('organize_photos', { plan: partial.plan });
      if (partialDone.status === 'done') break;
      await new Promise(r => setTimeout(r, 10));
    }
    assert.equal(partialDone.failed, 1);
    assert.equal(renamed40, 1);
    assert.ok((await names(4)).includes('CAM000001x.JPG'));
    assert.ok((await names(4)).includes('CAM000002d.JPG'));
    assert.equal((await http('POST', `/${partial.plan}/photo-links/apply`, { retry: true })).status, 202);
    let retried;
    for (let i = 0; i < 200; i++) {
      retried = await call('organize_photos', { plan: partial.plan });
      if (retried.status === 'done') break;
      await new Promise(r => setTimeout(r, 10));
    }
    assert.equal(retried.failed ?? 0, 0);
    assert.equal(renamed40, 1);
    assert.ok((await names(4)).includes('CAM000002x.JPG'));
    assert.deepEqual(
      (({ filesAsTheyWere, photoLinksSavesUndone }) => ({ filesAsTheyWere, photoLinksSavesUndone }))(await call('organize_photos', { undo: partial.plan })),
      { filesAsTheyWere: 3, photoLinksSavesUndone: 2 },
    );
    const subset = await call('organize_photos', { operations: [
      { action: 'rename', file: fileUrl(40), name: 'CAM000001x.JPG' },
      { action: 'rename', file: fileUrl(41), name: 'CAM000002x.JPG' },
    ] });
    assert.equal((await http('POST', `/${subset.plan}/photo-links/apply`, { leaveOut: [0, 1, 2] })).body.error.code, 'nothing');
    assert.equal((await http('POST', `/${subset.plan}/photo-links/apply`, { leaveOut: [0, 1] })).status, 202);
    for (let i = 0; i < 200 && (await call('organize_photos', { plan: subset.plan })).status !== 'done'; i++)
      await new Promise(r => setTimeout(r, 10));
    assert.ok((await names(4)).includes('CAM000001_d.jpg'));
    assert.ok((await names(4)).includes('CAM000002x.JPG'));
    assert.equal((await call('organize_photos', { undo: subset.plan })).filesAsTheyWere, 1);
    // If Drive changed the file despite a timeout, the retry checks its current name and stops.
    const ambiguous = await call('organize_photos', { operations: [{ action: 'rename', file: fileUrl(41), name: 'CAM000002z.JPG' }] });
    local.rename = async (fileId, name) => {
      const result = await originalRename(fileId, name);
      if (fileId === id(41)) throw new Error('reply lost after Drive changed the file');
      return result;
    };
    assert.equal((await http('POST', `/${ambiguous.plan}/photo-links/apply`)).status, 202);
    for (let i = 0; i < 200 && (await call('organize_photos', { plan: ambiguous.plan })).status !== 'done'; i++)
      await new Promise(r => setTimeout(r, 10));
    const unsafeRetry = await http('POST', `/${ambiguous.plan}/photo-links/apply`, { retry: true });
    assert.equal(unsafeRetry.status, 409);
    assert.equal(unsafeRetry.body.error.code, 'replan');
    assert.match((await call('organize_photos', { operations: [{ action: 'remove', file: 'nothing.JPG' }] })).error, /Nothing to change/);
  } finally {
    store.close();
  }
});

test('what only another account can do is left ready in the transfer sheet', async () => {
  const { local, index } = await reading();
  // Asked for: the folder's photos handed to another account («Copy files»), without the folders inside it.
  const sheet = new LocalTransferSheet({ folder: 'old-folder', last: { action: 'DELETE', folder: 'Guyana', files: 506, status: 'COMPLETED', runtime: '2m 28s', hoursAgo: 9 } });
  const copy = await settleChanges(local, planFileChanges({ index, operations: [{ action: 'transfer', step: 'copy', in: 'Panama/JPG', subfolders: false }] }), { index, sheet });
  assert.deepEqual(told(copy.ops), [['sheet', 'Wild/Panama/JPG', null]]);
  // Whose the files are now: the account that presses «Copy files» copies the others'.
  assert.deepEqual(copy.ops[0].by, [{ account: 'curator@example.org', files: 4 }]);
  const said = describeChanges(copy.ops, copy.problems).otherAccountStep;
  assert.equal(said.button, 'Copy files');
  assert.equal(said.lastRun, 'DELETE of Guyana: 506 files, COMPLETED, 2m 28s');
  assert.match(said.then, /takes the photos over/);
  await runFileChanges(local, copy.ops, { sheet });
  assert.deepEqual(sheet.written, [{ folder: id(7), subfolders: false }]);
  assert.equal((await sheet.read()).permanent, true);
  // Nothing of it to take back.
  assert.deepEqual(await undoFileChanges(local, copy.ops), { undone: 0, failed: 0 });

  // «Delete copied files» for good, in a folder and those inside it: every account with …_todelete files there.
  const del = await settleChanges(local, planFileChanges({ index, operations: [{ action: 'transfer', step: 'delete', in: 'Panama', permanent: true }] }), { index, sheet });
  assert.deepEqual(del.ops[0].by, [
    { account: ME, files: 2 },
    { account: 'curator@example.org', files: 1 },
  ]);
  await runFileChanges(local, del.ops, { sheet });
  assert.deepEqual(sheet.written.at(-1), { folder: id(6), subfolders: true, permanent: true });

  // The fix `copied`: the project's own is binned, and the other account's gets the sheet ready by itself.
  const copied = await settleChanges(local, planFileChanges({ index, fix: [{ check: 'copied', in: 'Panama' }] }), { index, sheet });
  assert.deepEqual(told(copied.ops), [
    ['bin', 'Wild/Panama/JPG/CAM000005d.JPG_todelete', null],
    ['sheet', 'Wild/Panama/JPG', null],
  ]);
  assert.equal(copied.ops[1].auto, true);
  // Those this plan bins are not left to anyone.
  assert.deepEqual(copied.ops[1].by, [
    { account: ME, files: 1 },
    { account: 'curator@example.org', files: 1 },
  ]);

  // For good, to free the space at once: only the project's own originals whose copy is still beside them.
  const forGood = await settleChanges(local, planFileChanges({ index, fix: [{ check: 'copied', in: 'Panama', permanent: true }] }), { index, sheet });
  assert.deepEqual(told(forGood.ops)[0], ['delete', 'Wild/Panama/JPG/CAM000005d.JPG_todelete', null]);
  assert.equal(describeChanges(forGood.ops, []).deletedForGood.count, 1);
  const before = await local.about();
  assert.deepEqual([before.account, before.limit, before.used > 0], [ME, 100 * 2 ** 30, true]);
  await runFileChanges(local, forGood.ops, { sheet });
  assert.equal(forGood.ops[0].done, true);
  assert.deepEqual(await local.get(id(72)), { gone: true });
  assert.equal((await local.about()).used, before.used - 2048);
  // Nothing of it can be brought back.
  assert.deepEqual(await undoFileChanges(local, forGood.ops), { undone: 0, failed: 0, deletedForGood: 1 });
  // A copy that went meanwhile: its original stays.
  const again = await reading([...drive(), file(90, 'CAM000008d.JPG_todelete', 7, mine), file(91, 'CAM000008d.JPG', 7)]);
  const gone = await settleChanges(again.local, planFileChanges({ index: again.index, fix: [{ check: 'copied', in: 'Panama', permanent: true }] }), { index: again.index });
  await again.local.trash(id(73)).catch(() => again.local.items.delete(id(73)));
  again.local.items.delete(id(91));
  await runFileChanges(again.local, gone.ops);
  assert.match(gone.ops.find(o => o.id === id(90)).error, /its copy is no longer beside it/);
  assert.equal((await again.local.get(id(90))).name, 'CAM000008d.JPG_todelete');

  // One folder at a time, a step that exists, a folder that exists; and not while the sheet is in use.
  const wrong = planFileChanges({
    index,
    operations: [
      { action: 'transfer', step: 'delete', in: 'Peru' },
      { action: 'transfer', step: 'copy', in: 'Panama' },
      { action: 'transfer', step: 'move', in: 'Panama' },
    ],
  });
  assert.equal(wrong.ops.length, 1);
  assert.deepEqual(wrong.problems.map(p => p.error.slice(0, 28)), ['the sheet takes one folder a', 'transfer: step is copy or de']);
  const busy = new LocalTransferSheet({ busy: true, last: { action: 'COPY', folder: 'RAW', files: 0, status: 'Running', runtime: '0s', hoursAgo: 1.5 } });
  const waiting = await settleChanges(local, planFileChanges({ index, operations: [{ action: 'transfer', step: 'delete', in: 'Peru' }] }), { index, sheet: busy });
  assert.deepEqual(waiting.ops, []);
  assert.match(waiting.problems[0].error, /is in use \(COPY of RAW: Running, 1.5 h ago\)/);
  assert.match((await settleChanges(local, planFileChanges({ index, operations: [{ action: 'transfer', step: 'delete', in: 'Peru' }] }), { index })).problems[0].error, /no sheet/);
});

test('the transfer sheet as Google gives it: its cells by their labels, its last run, and what is written', async () => {
  const serial = ms => (ms - Date.UTC(1899, 11, 30)) / 86_400_000;
  const now = Date.UTC(2026, 9, 13, 20, 0, 0);
  const grid = (status, hoursAgo) => [
    ['Step 1 (Run as New Owner): …'],
    ['Source Folder ID', 'folder-a'],
    [],
    ['Mode: Send files to trash (0)\nDelete permanently, skip trash (1)', '1'],
    ['Process subfolders: yes(1), no (0)', 0],
    ['Last Log'],
    ['Timestamp', 'User Email', 'Action', 'Parent Folder Name', 'Processed Files', 'Status', 'Runtime', 'Source ID'],
    [serial(now - hoursAgo * 3_600_000), 'someone@example.org', 'DELETE', 'Guyana_2025', '506', status, '2m 28s', 'folder-a'],
  ];
  const read = parseTransfer(grid('COMPLETED', 2), { timeZone: 'UTC', now });
  assert.deepEqual(read, {
    folder: 'folder-a',
    subfolders: false,
    permanent: true,
    last: { action: 'DELETE', folder: 'Guyana_2025', files: 506, status: 'COMPLETED', runtime: '2m 28s', by: 'someone@example.org', hoursAgo: 2 },
    busy: false,
    cells: { folder: 'B2', permanent: 'B4', subfolders: 'B5' },
  });
  // A run the sheet shows as going on: for a few hours only (one left at «Starting...» is not waited for).
  assert.equal(parseTransfer(grid('Auto-Paused (Time Limit)', 1), { timeZone: 'UTC', now }).busy, true);
  assert.equal(parseTransfer(grid('Starting...', 30), { timeZone: 'UTC', now }).busy, false);
  assert.equal(parseTransfer(grid('Error: too many triggers', 1), { timeZone: 'UTC', now }).busy, false);

  const calls = [];
  const fetchFn = async (url, options = {}) => {
    calls.push([options.method ?? 'GET', url.replace('https://sheets.googleapis.com/v4/spreadsheets/sheet-id', ''), options.body ? JSON.parse(options.body) : null, options.headers.authorization]);
    const body = url.includes('fields=properties') ? { properties: { timeZone: 'UTC' } } : url.includes('/values/') ? { values: grid('COMPLETED', 2) } : {};
    return { ok: true, status: 200, json: async () => body };
  };
  const sheet = new GoogleTransferSheet({ id: 'sheet-id', token: async () => 'tok', fetch: fetchFn });
  await sheet.write({ folder: 'folder-b', subfolders: true });
  assert.deepEqual(calls.at(-1), [
    'POST',
    '/values:batchUpdate',
    { valueInputOption: 'RAW', data: [{ range: 'B2', values: [['folder-b']] }, { range: 'B5', values: [[1]] }] },
    'Bearer tok',
  ]);
  assert.equal(sheet.url, 'https://docs.google.com/spreadsheets/d/sheet-id/edit');
});

test('Google Drive as the app calls it: pages, a file gone, a change, a busy answer tried again', async () => {
  const calls = [];
  const answers = [];
  const fetchFn = async (url, options = {}) => {
    calls.push([options.method ?? 'GET', decodeURIComponent(url.replace('https://www.googleapis.com/drive/v3/files', '').replaceAll('+', ' ')), options.body ? JSON.parse(options.body) : null]);
    const { status = 200, body = {} } = answers.shift() ?? {};
    return { ok: status < 300, status, json: async () => body, text: async () => JSON.stringify(body) };
  };
  const google = new GoogleDrive({ token: async () => 'tok', fetch: fetchFn });

  answers.push({ body: { files: [{ id: 'a' }], nextPageToken: 'p2' } }, { body: { files: [{ id: 'b' }] } });
  assert.deepEqual(await google.children('folder-1', { foldersOnly: true }), [{ id: 'a' }, { id: 'b' }]);
  assert.match(calls[0][1], /'folder-1' in parents and trashed = false and mimeType = 'application\/vnd.google-apps.folder'/);
  assert.match(calls[1][1], /pageToken=p2/);

  answers.push({ status: 404 }, { body: { id: 'c', trashed: true } }, { body: { id: 'd', name: 'x.JPG' } });
  assert.deepEqual(await google.get('gone'), { gone: true });
  assert.deepEqual(await google.get('c'), { gone: true });
  assert.equal((await google.get('d')).name, 'x.JPG');

  answers.push({ body: { id: 'd', name: 'y.JPG' } });
  assert.equal((await google.rename('d', 'y.JPG')).name, 'y.JPG');
  assert.deepEqual(calls.at(-1).slice(0, 1).concat([calls.at(-1)[2]]), ['PATCH', { name: 'y.JPG' }]);
  answers.push({ body: { id: 'd' } });
  await google.move('d', 'from-1', 'to-1');
  assert.match(calls.at(-1)[1], /addParents=to-1&removeParents=from-1/);
  answers.push({ body: { id: 'd' } });
  await google.trash('d');
  assert.deepEqual(calls.at(-1)[2], { trashed: true });
  answers.push({ status: 204 });
  await google.delete('d');
  assert.deepEqual(calls.at(-1).slice(0, 2), ['DELETE', '/d?supportsAllDrives=true']);
  answers.push({ body: { user: { emailAddress: 'project@example.org' }, storageQuota: { limit: '107374182400', usage: '75161927680', usageInDriveTrash: '1073741824' } } });
  assert.deepEqual(await google.about(), { account: 'project@example.org', limit: 107374182400, used: 75161927680, inBin: 1073741824 });
  answers.push({ body: { id: 'f' } });
  await google.createFolder('New', 'parent-1');
  assert.deepEqual(calls.at(-1)[2], { name: 'New', mimeType: FOLDER, parents: ['parent-1'] });

  // Google busy: asked again; refused: said as Google says it.
  answers.push({ status: 503 }, { body: { id: 'd', name: 'x.JPG' } });
  assert.equal((await google.get('d')).name, 'x.JPG');
  answers.push({ status: 403, body: { error: { message: 'insufficientFilePermissions' } } });
  await assert.rejects(google.trash('d'), /Google Drive 403: .*insufficientFilePermissions/);
});

test('two names swapped in one plan: the first file steps aside, so a failed step leaves no two files of one name', async () => {
  const files = () => [...drive(), file(90, 'CAM000010d.JPG', 4, mine), file(91, 'CAM000010v.JPG', 4, mine)];
  const { local, index } = await reading(files());
  const plan = await settleChanges(
    local,
    planFileChanges({
      index,
      withRaw: false,
      operations: [
        { action: 'rename', file: fileUrl(90), name: 'CAM000010v.JPG' },
        { action: 'rename', file: fileUrl(91), name: 'CAM000010d.JPG' },
      ],
    }),
  );
  assert.deepEqual(plan.problems, []);
  assert.deepEqual(told(plan.ops).sort(), [
    ['rename', 'Wild/Peru/JPG/CAM000010d.JPG', 'CAM000010v.JPG'],
    ['rename', 'Wild/Peru/JPG/CAM000010v.JPG', 'CAM000010d.JPG'],
  ]);
  assert.equal(plan.ops.filter(o => o.via).length, 1);
  const name = n => local.items.get(id(n)).name;
  await runFileChanges(local, structuredClone(plan.ops));
  assert.deepEqual([name(90), name(91)], ['CAM000010v.JPG', 'CAM000010d.JPG']);

  // The second file cannot be renamed: the first keeps the name it stepped aside to.
  const { local: other, index: again } = await reading(files());
  const ops = (await settleChanges(other, planFileChanges({ index: again, withRaw: false, operations: [
    { action: 'rename', file: fileUrl(90), name: 'CAM000010v.JPG' },
    { action: 'rename', file: fileUrl(91), name: 'CAM000010d.JPG' },
  ] }))).ops;
  const failing = Object.create(other);
  failing.rename = async (fileId, to) => {
    if (fileId === ops.find(o => !o.via).id) throw new Error('no permission');
    return other.rename(fileId, to);
  };
  await runFileChanges(failing, ops);
  const names = [other.items.get(id(90)).name, other.items.get(id(91)).name];
  assert.equal(new Set(names).size, 2);
  assert.ok(names.some(n => n.includes('__swap')));
  const stepped = ops.find(o => o.via);
  assert.match(stepped.error, /left named .*__swap/);
  // Undo gives it its name back.
  await undoFileChanges(other, ops);
  assert.deepEqual([other.items.get(id(90)).name, other.items.get(id(91)).name], ['CAM000010d.JPG', 'CAM000010v.JPG']);
});

test('a file removed and another renamed to its name, in one plan: the rename waits for the removal', async () => {
  const files = [...drive(), file(92, 'CAM000011d.JPG', 4), file(93, 'CAM000011d_upright.JPG', 4, mine)];
  const { local, index } = await reading(files);
  const plan = await settleChanges(
    local,
    planFileChanges({
      index,
      withRaw: false,
      operations: [
        { action: 'rename', file: fileUrl(93), name: 'CAM000011d.JPG' },
        { action: 'remove', file: fileUrl(92) },
      ],
    }),
  );
  assert.deepEqual(plan.problems, []);
  const renamed = plan.ops.find(o => o.id === id(93));
  assert.equal(renamed.after, id(92));
  await runFileChanges(local, plan.ops);
  assert.deepEqual([local.items.get(id(92)).name, local.items.get(id(93)).name], ['CAM000011d.JPG_todelete', 'CAM000011d.JPG']);

  // A name another file keeps is refused, in words that say what to do.
  const refused = planFileChanges({ index, withRaw: false, operations: [{ action: 'rename', file: fileUrl(93), name: 'CAM000011d.JPG' }] });
  assert.match(refused.problems[0].error, /another file of its folder keeps the name CAM000011d\.JPG .* in the same plan/);
});

test('one photo kept per side of four: the kept ventral takes the name of a removed photo in the same plan', async () => {
  const files = [...drive(), file(90, 'CAM000002d2.JPG', 4, mine), file(91, 'CAM000002v.JPG', 4, mine), file(92, 'CAM000002v2.JPG', 4, mine)];
  const { local, index } = await reading(files);
  const plan = await settleChanges(local, planFileChanges({
    index,
    operations: [
      { action: 'remove', file: fileUrl(90) },
      { action: 'remove', file: fileUrl(91) },
      { action: 'rename', file: fileUrl(92), name: 'CAM000002v' },
    ],
  }));
  assert.deepEqual(plan.problems, []);
  assert.deepEqual(told(plan.ops).map(([kind, , to]) => [kind, to]).sort(), [['bin', null], ['bin', null], ['rename', 'CAM000002v.JPG']]);
});
