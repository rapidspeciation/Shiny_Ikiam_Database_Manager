import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { FOLDER, LocalDrive, driveTree } from '../server/drive.mjs';
import { planPhotoLinks } from '../server/photo-links.mjs';
import { describeChanges, kindOf, planFileChanges, runFileChanges, settleChanges, undoFileChanges } from '../server/photo-files.mjs';
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

  // …_todelete beside its copy: binned when the project's own; another account's is left to its owner; no copy, not touched.
  const copied = await settleChanges(local, planFileChanges({ index, fix: [{ check: 'copied', in: 'Panama' }] }));
  assert.deepEqual(told(copied.ops), [['bin', 'Wild/Panama/JPG/CAM000005d.JPG_todelete', null]]);
  assert.match(copied.problems[0].error, /1 …_todelete files belong to another account/);
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
  const assistant = createAssistant({ store, config: { drive: local } });
  store.db.prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')").run(createHash('sha256').update('kim-token').digest('hex'), 'kim');
  const kim = { id: 'kim', username: 'kim', role: 'editor' };
  const call = async (name, args) =>
    JSON.parse((await assistant.mcp({ authorization: 'Bearer kim-token' }, { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } })).body.result.content[0].text);
  const http = (method, path, body = {}) => assistant.handle({ method, path: `/api/chat/proposals${path}`, user: kim, query: {}, body });
  const names = async parent => (await local.children(id(parent))).map(f => f.name).sort();
  try {
    assert.ok(mcpTools().some(t => t.name === 'organize_photos'));
    const plan = await call('organize_photos', { fix: [{ check: 'rename', in: 'Peru' }, { check: 'copies', in: 'Peru' }] });
    assert.match(plan.link, /propuestas/);
    assert.deepEqual(Object.fromEntries(Object.entries(plan.changes).map(([k, v]) => [k, v.count])), { rename: 2, binned: 2, renamedToDelete: 1 });
    assert.deepEqual(plan.changes.rename.examples, ['Wild/Peru/JPG/CAM000001_d.jpg → CAM000001d.JPG']);

    const [card] = (await http('GET', '')).body.proposals;
    assert.equal(card.kind, 'photo_files');
    assert.equal(card.reason, 'Google Drive: photo files');
    assert.deepEqual(card.sync.counts, { rename: 2, bin: 2, mark: 1 });
    assert.equal(card.sync.total, 5);
    const rows = (await http('GET', `/${card.id}/photo-links/rows`)).body.rows;
    assert.deepEqual(rows[0], { change: 'rename', folder: 'Wild/Peru/JPG', name: 'CAM000001_d.jpg', to: 'CAM000001d.JPG', url: fileUrl(40) });
    assert.equal(rows.filter(r => r.raw).length, 1);
    assert.equal(rows.filter(r => r.copy).length, 3);
    assert.equal((await http('POST', `/${card.id}/photo-links/refresh`)).status, 409);
    // Nothing changed yet.
    assert.deepEqual(await names(4), ['CAM000001_d.jpg', 'CAM000002d.JPG', 'CAM000003d.JPG', 'CAM000004d.JPG']);

    const done = await call('organize_photos', { plan: plan.plan, confirmed: true });
    assert.equal(done.status, 'done');
    assert.equal(done.filesChanged, 5);
    assert.equal(done.photoLinksRowsUpdated, 3);
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
    assert.deepEqual(JSON.parse(applied.sync_json).progress, { state: 'done', written: 5, total: 5, phase: 'sheet', sheetRows: 3 });

    // Taken back: the files, and the sheet's rows.
    const undone = await call('organize_photos', { undo: plan.plan });
    assert.deepEqual(undone, { status: 'undone', filesAsTheyWere: 5, photoLinksSavesUndone: 1 });
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
    assert.match((await call('organize_photos', { operations: [{ action: 'remove', file: 'nothing.JPG' }] })).error, /Nothing to change/);
  } finally {
    store.close();
  }
});
