import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { FOLDER, LocalDrive, driveTree } from '../server/drive.mjs';
import { captureSerial, driveIdOf, planPhotoLinks, sheetSerial, specimenName } from '../server/photo-links.mjs';
import { createPhotoLinksTool } from '../server/photo-links-tool.mjs';
import { createAssistant, mcpTools } from '../server/assistant.mjs';
import { moduleMap } from '../server/schema.mjs';

const id = n => `drive-id-${String(n).padStart(20, '0')}`;
const folderUrl = n => `https://drive.google.com/drive/folders/${id(n)}`;
const fileUrl = n => `https://drive.google.com/file/d/${id(n)}/view?usp=drivesdk`;
const folder = (n, name, parent) => ({
  id: id(n),
  name,
  mimeType: FOLDER,
  parents: parent ? [id(parent)] : [],
  webViewLink: folderUrl(n),
  createdTime: '2026-01-14T19:35:00.000Z',
  owners: [{ emailAddress: 'curator@example.org' }],
});
const file = (n, name, parent, extra = {}) => ({
  id: id(n),
  name,
  mimeType: 'image/jpeg',
  parents: [id(parent)],
  webViewLink: fileUrl(n),
  createdTime: '2026-07-21T20:21:46.221Z',
  size: '2048',
  owners: [{ emailAddress: 'photos@example.org' }],
  imageMediaMetadata: { time: '2026:07:20 10:15:30' },
  ...extra,
});
const row = (n, path, type, extra = {}) => ({
  'Full Path': path,
  Name: path.split('/').at(-1),
  Type: type,
  URL: type === 'Folder' ? folderUrl(n) : fileUrl(n),
  'Size KB': type === 'Folder' ? 0 : 2,
  'Owner Email': type === 'Folder' ? 'curator@example.org' : 'photos@example.org',
  ...extra,
});

// Photos (1) holds the listed folder Wild (2) and the unlisted Insectary (3).
const drive = () => [
  folder(1, 'Photos'),
  folder(2, 'Wild', 1),
  folder(3, 'Insectary', 1),
  folder(4, 'Ecuador_2026', 2),
  folder(5, 'bad_photos_temp', 2),
  file(10, 'CAM000001d.JPG', 2),
  // Renamed in Drive since it was listed as CAM000009v.JPG.
  file(11, 'CAM000001v.JPG', 2),
  // Uploaded again: the sheet's row holds the deleted file 12.
  file(13, 'CAM000002d.JPG', 2),
  file(20, 'CAM000003d.JPG', 4),
  file(21, 'CAM000003_v.jpg', 4),
  file(22, 'IMG_0271.JPG', 4),
  file(23, 'CAM999999d.JPG', 4),
  file(24, 'CAM000003d.CR2', 4, { mimeType: 'image/x-canon-cr2', imageMediaMetadata: undefined }),
  // The same name as a listed photo: a copy, and another photo.
  file(25, 'CAM000001d.JPG', 4),
  file(26, 'CAM000001v.JPG', 4, { size: '9999' }),
  file(27, 'CAM000003v.JPG_todelete', 4),
  file(30, 'CAM000004d.JPG', 5),
  file(40, 'CAM000005d.JPG', 3),
  file(41, 'CAM000005v.JPG', 3),
];
const sheetRows = () => [
  row(2, 'Wild', 'Folder'),
  row(10, 'Wild/CAM000001d.JPG', 'File'),
  row(11, 'Wild/CAM000009v.JPG', 'File'),
  row(12, 'Wild/CAM000002d.JPG', 'File'),
  // Deleted, and no file of that name is left.
  row(14, 'Wild/CAM000008d.JPG', 'File'),
  { 'Full Path': '✓ Sync done: 0 files added in 49s' },
];
const ids = new Set(['CAM000001', 'CAM000002', 'CAM000003', 'CAM000004', 'CAM000005']);

test('a photo name is read as its ID and view', () => {
  assert.deepEqual(specimenName('CAM077884d.JPG'), { id: 'CAM077884', view: 'd', extra: '', ext: 'JPG' });
  assert.deepEqual(specimenName('CAM077986_V 2.jpg'), { id: 'CAM077986', view: 'v', extra: '2', ext: 'jpg' });
  assert.deepEqual(specimenName('M6285d.JPG'), { id: 'M6285', view: 'd', extra: '', ext: 'JPG' });
  assert.equal(specimenName('IMG_0271.JPG'), null);
  assert.equal(specimenName('DSC00412.JPG'), null);
  assert.equal(driveIdOf(fileUrl(7)), id(7));
  assert.equal(driveIdOf(folderUrl(7)), id(7));
});

test('dates are written as the sheet\'s script writes them: in the workbook\'s zone', () => {
  // 28 May 2026 22:48:25 UTC is 23:48:25 in London (summer time).
  assert.equal(sheetSerial(Date.UTC(2026, 4, 28, 22, 48, 25), 'Europe/London').toFixed(5), (46170 + (23 * 3600 + 48 * 60 + 25) / 86400).toFixed(5));
  // In winter the two agree.
  assert.equal(sheetSerial(Date.UTC(2026, 0, 14, 19, 35, 0), 'Europe/London').toFixed(5), (46036 + (19 * 3600 + 35 * 60) / 86400).toFixed(5));
  // The camera's time, read as UTC and shown in the zone (an hour later in summer), as the rows already there.
  assert.equal(captureSerial('2026:05:06 20:15:28', 'Europe/London').toFixed(5), (46148 + (21 * 3600 + 15 * 60 + 28) / 86400).toFixed(5));
  assert.equal(captureSerial('', 'Europe/London'), null);
});

test('the plan: new files of the listed folders, rows following their file, names checked', async () => {
  const local = new LocalDrive(drive());
  const items = await driveTree(local, [id(1)]);
  const rows = sheetRows().map((values, i) => ({ id: `r${i + 2}`, row: i + 2, values }));
  const first = planPhotoLinks({ rows, items, home: id(1), ids });
  // The two rows whose file the listing lacks and no file replaces by name: asked one by one.
  assert.deepEqual(first.unknown, [id(14)]);
  const plan = planPhotoLinks({ rows, items, home: id(1), ids, lookups: new Map([[id(14), { gone: true }]]) });
  const s = plan.summary;

  const kinds = Object.fromEntries(plan.edits.map(e => [e.row, e.kind]));
  assert.deepEqual(kinds, { 4: 'rename', 5: 'relink', 6: 'remove', 7: 'status' });
  const renamed = plan.edits.find(e => e.kind === 'rename');
  assert.deepEqual(renamed.values, { 'Full Path': 'Wild/CAM000001v.JPG', Name: 'CAM000001v.JPG' });
  assert.deepEqual(renamed.expected, { 'Full Path': 'Wild/CAM000009v.JPG', Name: 'CAM000009v.JPG' });
  const relinked = plan.edits.find(e => e.kind === 'relink');
  assert.equal(relinked.values.URL, fileUrl(13));
  assert.equal(relinked.values.Name, undefined, 'the row keeps its name');
  assert.ok(Object.values(plan.edits.find(e => e.kind === 'remove').values).every(v => v === null));

  // The new folder's row, then its files by name; nothing from …_temp, …_todelete or the unlisted folder.
  assert.deepEqual(
    plan.creates.map(c => c.values['Full Path']),
    [
      'Wild/Ecuador_2026',
      'Wild/Ecuador_2026/CAM000003_v.jpg',
      'Wild/Ecuador_2026/CAM000003d.CR2',
      'Wild/Ecuador_2026/CAM000003d.JPG',
      'Wild/Ecuador_2026/CAM999999d.JPG',
      'Wild/Ecuador_2026/IMG_0271.JPG',
    ],
  );
  const photo = plan.creates[3].values;
  assert.equal(photo.Type, 'File');
  assert.equal(photo.URL, fileUrl(20));
  assert.equal(photo['Size KB'], 2);
  assert.equal(photo['Owner Email'], 'photos@example.org');
  assert.ok(photo.Date > 46000 && photo['Capture Date'] > 46000);
  assert.equal(plan.creates[0].values.Type, 'Folder');
  assert.equal(plan.creates[2].values['Capture Date'], null);

  assert.equal(s.newFiles, 5);
  assert.deepEqual(s.newFolders, ['Wild/Ecuador_2026']);
  assert.deepEqual(s.byFolder, [{ folder: 'Wild/Ecuador_2026', new: 5, files: 8 }]);
  assert.equal(s.leftOut, 1);
  assert.deepEqual(s.setAside, [{ folder: 'Wild/bad_photos_temp', files: 1 }]);
  assert.deepEqual(s.sameName, {
    copies: 1,
    different: 1,
    examples: [{ name: 'CAM000001v.JPG', in: 'Wild/Ecuador_2026', listed: 'Wild/CAM000001v.JPG' }],
  });
  assert.deepEqual(
    s.notListed.map(n => [n.folder, n.files, n.namedAsSpecimens]),
    [['Insectary', 2, 2]],
  );
  // RAW files are not looked up; of the four photos, one is found as it is.
  assert.equal(s.names.photos, 4);
  assert.equal(s.names.linked, 1);
  assert.equal(s.names.butterflies, 1);
  assert.deepEqual(s.names.rename, { count: 1, examples: [{ name: 'CAM000003_v.jpg', to: 'CAM000003v.JPG', in: 'Wild/Ecuador_2026' }] });
  assert.equal(s.names.unknownId.count, 1);
  assert.equal(s.names.unnamed.examples[0].name, 'IMG_0271.JPG');

  // A folder asked for is listed from its own name down.
  const more = planPhotoLinks({ rows, items, home: id(1), ids, add: [id(3)], lookups: new Map([[id(14), { gone: true }]]) });
  assert.deepEqual(
    more.creates.map(c => c.values['Full Path']).filter(p => p.startsWith('Insectary')),
    ['Insectary', 'Insectary/CAM000005d.JPG', 'Insectary/CAM000005v.JPG'],
  );
  assert.deepEqual(more.summary.notListed, []);
});

test('a path written from a subfolder stays; a file moved to another folder gets its new path', async () => {
  const files = [folder(1, 'Photos'), folder(2, 'Wild', 1), folder(4, 'JPG', 2), folder(6, 'Peru', 2), file(20, 'CAM000003d.JPG', 4), file(21, 'CAM000003v.JPG', 6)];
  const items = await driveTree(new LocalDrive(files), [id(1)]);
  const rows = [row(2, 'Wild', 'Folder'), row(20, 'JPG/CAM000003d.JPG', 'File'), row(21, 'JPG/CAM000003v.JPG', 'File')].map((values, i) => ({
    id: `r${i}`,
    row: i + 2,
    values,
  }));
  const plan = planPhotoLinks({ rows, items, home: id(1), ids });
  assert.deepEqual(
    plan.edits.map(e => [e.kind, e.values]),
    [['move', { 'Full Path': 'Wild/Peru/CAM000003v.JPG' }]],
  );
});

async function fixture() {
  const sheets = new LocalSheets({
    Photo_links: sheetRows().map((values, i) => ({ row: i + 2, values })),
    // The butterflies the photos are named after.
    Insectary_data: [...ids].map((cam, i) => ({ row: i + 2, values: { Insectary_ID: `A${i}B`, Sex: 'female', CAM_ID: cam } })),
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Photo_links', 'Insectary_data'] });
  store.db
    .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('kim','kim','Kim','editor','s','h',1,'2026-01-01')")
    .run();
  return { sheets, store };
}
const cellsOf = (sheets, rowNumber) => {
  const fields = moduleMap.get('Photo_links').fields;
  const cells = sheets.rows.get('Photo_links').find(r => r.row === rowNumber)?.cells ?? [];
  return Object.fromEntries(
    fields.map(f => {
      const v = cells[f.column]?.userEnteredValue;
      return [f.key, v?.formulaValue ?? v?.stringValue ?? v?.numberValue ?? null];
    }),
  );
};

test('sync_photo_links reads, then writes once confirmed', async () => {
  const { sheets, store } = await fixture();
  const tool = createPhotoLinksTool({ store, config: { drive: new LocalDrive(drive()) }, chunk: 4 });
  const kim = { id: 'kim', username: 'kim', role: 'editor' };
  try {
    assert.match((await tool.run({}, { id: 'obs', role: 'observer' })).error, /role/);

    const read = await tool.run({}, kim);
    assert.equal(read.changes.newFiles, 5);
    assert.deepEqual(read.changes.removed, [{ row: 6, path: 'Wild/CAM000008d.JPG' }]);
    assert.equal(read.changes.scriptStatusRowsCleared, 1);
    assert.deepEqual(read.notListed, [{ folder: 'Insectary', files: 2, photos: 2, namedAsSpecimens: 2 }]);
    assert.match(read.next, new RegExp(read.scan));
    assert.equal(cellsOf(sheets, 8).Name, null, 'nothing written yet');

    assert.match((await tool.run({ confirmed: true, scan: 'another' }, kim)).error, /no longer kept/);
    const done = await tool.run({ confirmed: true, scan: read.scan }, kim);
    assert.equal(done.status, 'done');
    assert.equal(done.rowsWritten, 10);
    assert.equal(done.saves, '3 of 3');

    assert.equal(cellsOf(sheets, 4).Name, 'CAM000001v.JPG');
    assert.equal(cellsOf(sheets, 5).URL, fileUrl(13));
    // The new rows start after the last row in use: here over the emptied row and the script's status line.
    assert.equal(cellsOf(sheets, 6)['Full Path'], 'Wild/Ecuador_2026');
    assert.equal(cellsOf(sheets, 7).Name, 'CAM000003_v.jpg');
    assert.equal(cellsOf(sheets, 11).Name, 'IMG_0271.JPG');
    assert.equal(cellsOf(sheets, 12).Name, null);
    assert.equal(typeof cellsOf(sheets, 9).Date, 'number');
    const saves = store.db.prepare("SELECT source, reason FROM actions WHERE status='verified'").all();
    assert.equal(saves.length, 3);
    assert.ok(saves.every(a => a.source === 'ai_approved' && /Photo_links/.test(a.reason)));

    // The same answer when the call is repeated; then nothing is left to do.
    assert.equal((await tool.run({ confirmed: true, scan: read.scan }, kim)).status, 'done');
    const again = await tool.run({}, kim);
    assert.equal(again.upToDate, true);
    assert.equal(again.changes, undefined);

    // A folder offered is added by its name.
    const added = await tool.run({ add: ['insectary'] }, kim);
    assert.equal(added.changes.newFiles, 2);
    assert.deepEqual(added.changes.newFolders, ['Insectary']);
    assert.deepEqual((await tool.run({ add: ['no such folder'] }, kim)).foldersNotFound, ['no such folder']);
  } finally {
    store.close();
  }
});

test('the assistant offers sync_photo_links to editors', async () => {
  const { store } = await fixture();
  const assistant = createAssistant({ store, config: { drive: new LocalDrive(drive()) } });
  store.db
    .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
    .run(createHash('sha256').update('kim-token').digest('hex'), 'kim');
  try {
    assert.ok(mcpTools().some(t => t.name === 'sync_photo_links'));
    const out = await assistant.mcp(
      { authorization: 'Bearer kim-token' },
      { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name: 'sync_photo_links', arguments: {} } },
    );
    const read = JSON.parse(out.body.result.content[0].text);
    assert.equal(read.changes.newFiles, 5);
    assert.equal(read.sheetRows, 6);
  } finally {
    store.close();
  }
});
