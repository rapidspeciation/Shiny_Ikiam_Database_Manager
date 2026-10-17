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
  // Ecuador_2026's renamed photos taken that day are turned half (their tag); CAM000003d.JPG's is not.
  file(20, 'CAM000003d.JPG', 4, { imageMediaMetadata: { time: '2026:07:20 10:15:30', rotation: 0 } }),
  file(21, 'CAM000003_v.jpg', 4, { imageMediaMetadata: { time: '2026:07:20 10:15:30', rotation: 2 } }),
  file(22, 'IMG_0271.JPG', 4),
  file(23, 'CAM999999d.JPG', 4, { imageMediaMetadata: { time: '2026:07:20 10:15:30', rotation: 2 } }),
  file(24, 'CAM000003d.CR2', 4, { mimeType: 'image/x-canon-cr2', imageMediaMetadata: undefined }),
  // The same name as a listed photo: a copy, and another photo.
  file(25, 'CAM000001d.JPG', 4, { imageMediaMetadata: { time: '2026:07:20 10:15:30', rotation: 2 } }),
  file(26, 'CAM000001v.JPG', 4, { size: '9999', imageMediaMetadata: { time: '2026:07:20 10:15:30', rotation: 2 } }),
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
  // each folder's new rows and its changed ones (a row without a path: no folder)
  assert.deepEqual(s.byFolder, [
    // a folder not listed yet: its own row among its new rows
    { folder: 'Wild/Ecuador_2026', new: 6, files: 8, changed: 0, kinds: {} },
    { folder: 'Wild', new: 0, files: 3, changed: 3, kinds: { rename: 1, relink: 1, remove: 1 } },
    { folder: '', new: 0, files: null, changed: 1, kinds: { status: 1 } },
  ]);
  assert.equal(s.leftOut, 1);
  // the photo turned unlike the others of its folder that day, and the turn that matches them
  assert.deepEqual(s.turned.map(t => [t.path, t.degrees]), [['Wild/Ecuador_2026/CAM000003d.JPG', 180]]);
  // …_todelete files by folder; this one has no file of its plain name beside it
  assert.deepEqual(s.toDelete, [{ folder: 'Wild/Ecuador_2026', files: 1, kb: 2, noCopy: ['CAM000003v.JPG_todelete'] }]);
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
  assert.deepEqual(s.names.rename, { count: 1, examples: [{ name: 'CAM000003_v.jpg', in: 'Wild/Ecuador_2026', url: fileUrl(21), to: 'CAM000003v.JPG' }] });
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

test("photos listed twice: the same file and copies are emptied (the country's folder kept), two different photos are told", async () => {
  // Peru and Panama folders under the listed «Photos»; a Peru butterfly's photo copied into Panama.
  const items = await driveTree(
    new LocalDrive([
      folder(1, 'Home'),
      folder(2, 'Photos', 1),
      folder(3, 'PANAMA_2025', 2),
      folder(4, 'PERU_2024', 2),
      file(30, 'CAM000001d.JPG', 3, { size: '4096', imageMediaMetadata: { time: '2026:07:20 10:15:30' } }),
      file(40, 'CAM000001d.JPG', 4, { size: '4096', imageMediaMetadata: { time: '2026:07:20 10:15:30' } }),
      // The same name, another photo.
      file(31, 'CAM000002v.JPG', 3, { size: '9000' }),
      file(41, 'CAM000002v.JPG', 4, { size: '3000' }),
      file(42, 'CAM000003d.JPG', 4, { size: '4096' }),
      // Renamed …_todelete once copied by another account; its copy has a row already.
      file(43, 'CAM000004d.JPG_todelete', 4),
      file(44, 'CAM000004d.JPG', 4, { size: '4096' }),
      // The same, its copy not listed yet: the row follows the copy, and takes its name.
      file(45, 'CAM000005d.JPG_todelete', 4),
      file(46, 'CAM000005d.JPG', 4, { size: '4096' }),
    ]),
    [id(1)],
  );
  const listed = (n, path, extra = {}) => ({ ...row(n, path, 'File'), 'Size KB': 4, 'Capture Date': captureSerial('2026:07:20 10:15:30', 'Europe/London'), ...extra });
  const rows = [
    row(2, 'Photos', 'Folder'),
    row(3, 'Photos/PANAMA_2025', 'Folder'),
    row(4, 'Photos/PERU_2024', 'Folder'),
    listed(30, 'Photos/PANAMA_2025/CAM000001d.JPG'),
    listed(40, 'Photos/PERU_2024/CAM000001d.JPG'),
    listed(31, 'Photos/PANAMA_2025/CAM000002v.JPG', { 'Size KB': 8.7890625, 'Capture Date': null }),
    listed(41, 'Photos/PERU_2024/CAM000002v.JPG', { 'Size KB': 2.9296875, 'Capture Date': null }),
    listed(42, 'Photos/PERU_2024/CAM000003d.JPG'),
    // The same file twice.
    listed(42, 'Photos/PERU_2024/CAM000003d.JPG'),
    listed(43, 'Photos/PERU_2024/CAM000004d.JPG_todelete', { Name: 'CAM000004d.JPG_todelete' }),
    listed(44, 'Photos/PERU_2024/CAM000004d.JPG'),
    listed(45, 'Photos/PERU_2024/CAM000005d.JPG_todelete', { Name: 'CAM000005d.JPG_todelete' }),
  ].map((values, i) => ({ id: `r${i + 2}`, row: i + 2, values }));
  const ids = new Map([
    ['CAM000001', { country: 'Peru' }],
    ['CAM000002', { country: 'Peru' }],
    ['CAM000003', { country: 'Peru' }],
    ['CAM000004', { country: 'Peru' }],
    ['CAM000005', { country: 'Peru' }],
  ]);
  const plan = planPhotoLinks({ rows, items, home: id(1), ids, timeZone: 'Europe/London' });
  const kinds = Object.fromEntries(plan.edits.map(e => [e.row, e.kind]));
  // Row 5 (the Panama copy) and row 10 (the second row of one file) emptied; the …_todelete row (11) too, its copy listed.
  assert.deepEqual(kinds, { 5: 'duplicate', 10: 'duplicate', 11: 'remove', 13: 'relink' });
  const followed = plan.edits.find(e => e.row === 13).values;
  assert.deepEqual([followed.Name, followed.URL], ['CAM000005d.JPG', fileUrl(46)]);
  const twice = plan.summary.listedTwice;
  assert.equal(twice.sameFile, 1);
  assert.deepEqual(twice.copies, [{ name: 'CAM000001d.JPG', kept: 'Photos/PERU_2024/CAM000001d.JPG', emptied: ['Photos/PANAMA_2025/CAM000001d.JPG'] }]);
  assert.deepEqual(
    twice.different.map(d => [d.name, d.rows.map(r => r.row)]),
    [['CAM000002v.JPG', [7, 8]]],
  );
  assert.equal(plan.creates.length, 0);
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

test('the checks: out of the order the photos were taken in, a view missing, another date or country', async () => {
  const at = (minute, extra = {}) => ({ imageMediaMetadata: { time: `2026:07:20 10:${String(minute).padStart(2, '0')}:00` }, ...extra });
  const files = [folder(1, 'Photos'), folder(2, 'Peru_2026', 1)];
  // A session of CAM000101…CAM000112 in order, with CAM000950 in the middle of it.
  for (let i = 0; i < 12; i++) {
    files.push(file(100 + 2 * i, `CAM${String(101 + i).padStart(6, '0')}d.JPG`, 2, at(2 * i)));
    if (i !== 3) files.push(file(101 + 2 * i, `CAM${String(101 + i).padStart(6, '0')}v.JPG`, 2, at(2 * i + 1)));
  }
  files.push(file(200, 'CAM000950d.JPG', 2, at(9)), file(201, 'CAM000950v.JPG', 2, at(9)));
  const ids = new Map();
  for (let i = 0; i < 12; i++) ids.set(`CAM${String(101 + i).padStart(6, '0')}`, { country: i < 11 ? 'Peru' : 'Brazil', date: i === 5 ? 46250 : 46000 });
  // One character from the name out of order, and it fits the photos around it.
  ids.set('CAM000150', { country: 'Peru' });
  const items = await driveTree(new LocalDrive(files), [id(1)]);
  const rows = [{ id: 'r2', row: 2, values: row(2, 'Peru_2026', 'Folder') }];
  // The folder is one country's only with enough photos of it.
  const plan = planPhotoLinks({ rows, items, home: id(1), ids });
  const n = plan.summary.names;
  assert.deepEqual(n.outOfOrder.examples.map(e => [e.name, e.around]), [
    ['CAM000950d.JPG', ['CAM000105', 'CAM000106']],
    ['CAM000950v.JPG', ['CAM000105', 'CAM000106']],
  ]);
  assert.deepEqual(n.unknownId.examples.map(e => [e.name, e.maybe]), [
    ['CAM000950d.JPG', undefined],
    ['CAM000950v.JPG', undefined],
  ]);
  assert.deepEqual(n.unpaired.examples.map(e => [e.name, e.missing]), [['CAM000104d.JPG', 'ventral']]);
  // CAM000106's date is after its photos' day.
  assert.equal(n.beforeCollection.count, 2);
  assert.equal(n.beforeCollection.examples[0].name, 'CAM000106d.JPG');
  assert.ok(n.beforeCollection.examples[0].days > 0);
  assert.equal(n.beforeCollection.examples[0].date, '2026-08-16');
  assert.deepEqual(n.otherCountry.examples.map(e => [e.name, e.country, e.folder]), [
    ['CAM000112d.JPG', 'Brazil', 'Peru'],
    ['CAM000112v.JPG', 'Brazil', 'Peru'],
  ]);
  // The panel's lines carry the same flags.
  const line = plan.display.find(d => d.name === 'CAM000104d.JPG');
  assert.deepEqual(line, { change: 'new', folder: 'Peru_2026', name: 'CAM000104d.JPG', taken: '2026-07-20 10:06', url: fileUrl(106), checks: [{ kind: 'unpaired', missing: 'ventral' }] });
  assert.equal(plan.display.length, plan.creates.length);

  // An unknown ID out of order: the workbook ID one character away that fits is offered.
  const typo = [folder(1, 'Photos'), folder(2, 'Peru_2026', 1)];
  for (let i = 0; i < 6; i++) typo.push(file(100 + i, `CAM${String(145 + i).padStart(6, '0')}d.JPG`, 2, at(2 * i)));
  typo.push(file(300, 'CAM000950d.JPG', 2, at(5)));
  const known = new Map([...Array(6)].map((_, i) => [`CAM${String(145 + i).padStart(6, '0')}`, {}]));
  known.set('CAM000151', {});
  const second = planPhotoLinks({ rows, items: await driveTree(new LocalDrive(typo), [id(1)]), home: id(1), ids: known });
  // CAM000950 is not in the workbook; CAM000150 has its photo already, so nothing one character away is free.
  assert.deepEqual(second.summary.names.unknownId.examples.map(e => e.maybe), [undefined]);
  known.set('CAM000900', {});
  const third = planPhotoLinks({ rows, items: await driveTree(new LocalDrive(typo), [id(1)]), home: id(1), ids: known });
  // …and CAM000900, free and one character away, is far from the photos around it: not offered either.
  assert.deepEqual(third.summary.names.unknownId.examples.map(e => e.maybe), [undefined]);
  // CAM000751 is one character from CAM000151, which has no photo and fits the photos around: likely the one meant.
  typo.push(file(301, 'CAM000751d.JPG', 2, at(9)));
  const fourth = planPhotoLinks({ rows, items: await driveTree(new LocalDrive(typo), [id(1)]), home: id(1), ids: known });
  assert.deepEqual(fourth.summary.names.unknownId.examples.find(e => e.name === 'CAM000751d.JPG').maybe, ['CAM000151']);
});

test('the same ID photographed in two folders: another photo, or the same one filed twice', async () => {
  const files = [
    folder(1, 'Photos'),
    folder(2, 'Wild', 1),
    folder(3, 'Peru', 2),
    folder(4, 'Panama', 2),
    // Listed, from a path further down ("Peru/…").
    file(10, 'CAM000001d.JPG', 3),
    file(11, 'CAM000002d.JPG', 3),
    // The same butterfly's ID, written another way: another photo, and the same photo again.
    file(20, 'CAM000001_d.jpg', 4, { size: '4096', imageMediaMetadata: { time: '2026:01:23 09:00:00' } }),
    file(21, 'CAM000002_d.jpg', 4),
    file(22, 'CAM000003d.JPG', 4),
  ];
  const items = await driveTree(new LocalDrive(files), [id(1)]);
  const capture = captureSerial('2026:07:20 10:15:30', 'Europe/London');
  const rows = [row(2, 'Wild', 'Folder'), row(10, 'Peru/CAM000001d.JPG', 'File', { 'Capture Date': capture }), row(11, 'Peru/CAM000002d.JPG', 'File', { 'Capture Date': capture })].map(
    (values, i) => ({ id: `r${i}`, row: i + 2, values }),
  );
  const plan = planPhotoLinks({ rows, items, home: id(1), ids });
  assert.deepEqual(
    plan.summary.names.otherFolder.examples.map(e => [e.name, e.other, e.copy]),
    [
      ['CAM000001_d.jpg', 'Peru', undefined],
      ['CAM000002_d.jpg', 'Peru', true],
    ],
  );
  // A row written from a folder further down ("Peru/…") is in the same folder as Wild/Peru: a new photo
  // there is told of the one in Panama, not of its own folder's.
  const more = [...files, file(12, 'CAM000001v.JPG', 3)];
  const again = planPhotoLinks({ rows, items: await driveTree(new LocalDrive(more), [id(1)]), home: id(1), ids });
  assert.deepEqual(
    again.display.find(d => d.name === 'CAM000001v.JPG').checks.filter(c => c.kind === 'otherFolder'),
    [{ kind: 'otherFolder', other: 'Wild/Panama' }],
  );
});

test('only some folders: their files are read, and the rest of the sheet is left as it is', async () => {
  const { store } = await fixture();
  const local = new LocalDrive(drive());
  const tool = createPhotoLinksTool({ store, config: { drive: local } });
  const kim = { id: 'kim', username: 'kim', role: 'editor' };
  try {
    const read = await tool.run({ only: ['Ecuador_2026'] }, kim);
    assert.deepEqual(read.onlyFolders, ['Ecuador_2026']);
    // CAM000001v.JPG too: the sheet has no row of that name while the folder above is not looked at.
    assert.equal(read.changes.newFiles, 6);
    // The renamed, replaced and deleted files of the folder above are not looked at.
    assert.equal(read.changes.renamed, undefined);
    assert.equal(read.changes.removed, undefined);
    assert.equal(read.notListed, undefined);
    assert.equal(read.drive.files, 8);
    // A file of a listed name whose listed file lies outside the folders read: a copy by what the sheet says of it.
    assert.equal(read.sameName.copies, 1);
    const done = await tool.run({ confirmed: true, scan: read.scan, skip: [] }, kim);
    assert.equal(done.status, 'done');
    // The folder above, by its name: its own files and those inside it. A row whose file is gone follows the
    // file of its name there without asking Drive (copies another account made); Drive is asked about the rest.
    const asked = [];
    const get = local.get.bind(local);
    local.get = async fileId => (asked.push(fileId), get(fileId));
    const all = await tool.run({ only: ['wild'] }, kim);
    assert.deepEqual(all.changes.removed, [{ row: 6, path: 'Wild/CAM000008d.JPG' }]);
    assert.deepEqual(all.changes.relinked, [{ row: 5, name: 'CAM000002d.JPG' }]);
    assert.ok(asked.includes(id(14)));
    assert.ok(!asked.includes(id(12)));
    assert.equal(all.changes.renamed.length, 1);
    assert.deepEqual((await tool.run({ only: ['nowhere'] }, kim)).foldersNotFound, ['nowhere']);
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

test('a reading is a card of Cambios propuestos: its rows, «Aplicar» with a folder left for later, its progress', async () => {
  const { sheets, store } = await fixture();
  const assistant = createAssistant({ store, config: { drive: new LocalDrive(drive()) } });
  store.db
    .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
    .run(createHash('sha256').update('kim-token').digest('hex'), 'kim');
  const kim = { id: 'kim', username: 'kim', role: 'editor' };
  const call = async (name, args) =>
    JSON.parse(
      (
        await assistant.mcp(
          { authorization: 'Bearer kim-token' },
          { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } },
        )
      ).body.result.content[0].text,
    );
  const http = (method, path, body = {}) => assistant.handle({ method, path: `/api/chat/proposals${path}`, user: kim, query: {}, body });
  const cards = async () => (await http('GET', '')).body.proposals.filter(p => p.kind === 'photo_links');
  try {
    const read = await call('sync_photo_links', { add: ['Insectary'] });
    assert.match(read.link, /propuestas/);
    let [card] = await cards();
    assert.equal(card.id, read.scan);
    assert.equal(card.status, 'pending');
    assert.equal(card.reason, 'Photo_links ← Google Drive');
    assert.equal(card.sync.total, 13);
    // each folder with its new rows and its rows that change
    assert.deepEqual(card.sync.byFolder.map(f => [f.folder, f.new, f.changed]), [
      ['Insectary', 3, 0],
      ['Wild/Ecuador_2026', 6, 0],
      ['Wild', 0, 3],
      ['', 0, 1],
    ]);
    assert.equal(card.sync.checks.rename.count, 1);
    assert.deepEqual(card.sync.add, ['Insectary']);

    // Read again in the chat: the same card.
    assert.equal((await call('sync_photo_links', { add: ['Insectary'] })).scan, read.scan);
    assert.equal((await cards()).length, 1);

    const rows = (await http('GET', `/${card.id}/photo-links/rows`)).body.rows;
    assert.equal(rows.length, 12);
    assert.deepEqual(rows.find(r => r.name === 'CAM000003_v.jpg').checks, [{ kind: 'rename', to: 'CAM000003v.JPG' }]);
    assert.deepEqual(
      rows.filter(r => r.change !== 'new' && r.change !== 'folder').map(r => [r.change, r.row]),
      [
        ['rename', 4],
        ['relink', 5],
        ['remove', 6],
      ],
    );

    // An ordinary proposal's tools and endpoints leave it alone.
    assert.match((await call('update_proposal', { proposalId: card.id, changes: [] })).error, /sync_photo_links/);
    assert.equal((await http('POST', `/${card.id}/edit`, { cells: [] })).status, 409);

    // The same rows as the sheet has them: a changed row with its values now, a new row with all of its own.
    const changes = (await http('GET', `/${card.id}/photo-links/rows`)).body.changes;
    const renamed = changes.find(c => c.row === 4);
    assert.equal(renamed.key, `r:${renamed.recordId}`);
    assert.equal(renamed.values.Name, 'CAM000001v.JPG');
    assert.equal(typeof renamed.rowValues['Full Path'], 'string');
    const fresh = changes.find(c => c.create && c.values.Name === 'CAM000003_v.jpg');
    assert.deepEqual(fresh.checks, [{ kind: 'rename', to: 'CAM000003v.JPG' }]);
    // A cell corrected on the card before applying is what gets written.
    const typed = await http('POST', `/${card.id}/photo-links/edit`, { cells: [{ key: fresh.key, field: 'Description', value: 'checked by hand' }, { key: fresh.key, field: 'Nope', value: 1 }] });
    assert.deepEqual(typed.body, { edited: 1 });
    const after = (await http('GET', `/${card.id}/photo-links/rows`)).body.changes.find(c => c.key === fresh.key);
    assert.equal(after.values.Description, 'checked by hand');
    assert.deepEqual(after.personEdits, { Description: { ai: null } });

    // «Aplicar» with the insectary folder left for later, and the changes of the rows in Wild.
    const started = await http('POST', `/${card.id}/photo-links/apply`, { skipNew: ['Insectary'], skipChanges: ['Insectary', 'Wild'] });
    assert.equal(started.status, 202);
    for (let i = 0; i < 200 && (await cards()).some(c => c.status === 'applying'); i++) await new Promise(r => setTimeout(r, 10));
    const done = (await http('GET', '', {})).body.proposals;
    assert.equal(done.filter(p => p.kind === 'photo_links' && p.status === 'pending').length, 0);
    const applied = store.db.prepare('SELECT status, sync_json, applied_at FROM ai_proposals WHERE id = ?').get(card.id);
    assert.equal(applied.status, 'applied');
    assert.ok(applied.applied_at);
    const progress = JSON.parse(applied.sync_json).progress;
    assert.equal(progress.state, 'done');
    assert.equal(progress.written, 7);
    assert.deepEqual(progress.leftFolders, ['Insectary', 'Wild']);
    // the row of a file deleted from Wild stays as it was until Wild is applied
    assert.equal(cellsOf(sheets, 6)['Full Path'], 'Wild/CAM000008d.JPG');
    assert.equal((await http('POST', `/${card.id}/photo-links/apply`, {})).status, 409);
    // Applied, the card shows the rows it wrote, the one corrected by hand as it was written.
    const wrote = (await http('GET', `/${card.id}/photo-links/rows`)).body.changes;
    assert.equal(wrote.length, 7);
    assert.equal(wrote.find(c => c.key === fresh.key)?.values.Description ?? null, fresh.folder === 'Insectary' ? null : 'checked by hand');

    // What was left: a new card, applied from the chat as any proposal is.
    const rest = await call('sync_photo_links', { add: ['Insectary'] });
    assert.notEqual(rest.scan, read.scan);
    assert.equal(rest.changes.newFiles, 2);
    const written = await call('apply_proposal', { proposalId: rest.scan });
    assert.equal(written.status, 'done');
    assert.equal(written.rowsWritten, 6, 'the insectary folder and the rows of Wild');
    const names = sheets.rows.get('Photo_links').map(r => cellsOf(sheets, r.row).Name);
    assert.ok(names.includes('CAM000005d.JPG'), 'the insectary photos written');
    assert.ok(!names.includes('CAM000008d.JPG'), 'and the row of the file deleted from Wild emptied');

    // Nothing left: no card.
    assert.equal((await call('sync_photo_links', { add: ['Insectary'] })).upToDate, true);
    assert.equal((await cards()).filter(c => c.status === 'pending').length, 0);
  } finally {
    store.close();
  }
});

test('the «Fotos» tab reads Photo_links without the assistant: the folders listed, a reading of some, its card applied', async () => {
  const { sheets, store } = await fixture();
  const assistant = createAssistant({ store, config: { drive: new LocalDrive(drive()) } });
  const kim = { id: 'kim', username: 'kim', role: 'editor' };
  const http = (method, path, body = {}) => assistant.handle({ method, path, user: kim, query: {}, body });
  try {
    // The folders the sheet lists, with the files under each.
    const { folders } = (await http('GET', '/api/photo-links/folders')).body;
    assert.ok(folders.length);
    assert.ok(folders.every(f => typeof f.path === 'string' && typeof f.files === 'number'));
    assert.equal((await assistant.handle({ method: 'GET', path: '/api/photo-links/folders', user: { id: 'v', role: 'viewer' }, query: {}, body: {} })).status, 403);

    // A reading of a folder to start listing: the person's own card, in Cambios propuestos.
    const read = await http('POST', '/api/photo-links/read', { add: ['Insectary'] });
    assert.equal(read.status, 200);
    const card = (await http('GET', '/api/chat/proposals')).body.proposals.find(p => p.id === read.body.proposalId);
    assert.equal(card.kind, 'photo_links');
    assert.equal(card.status, 'pending');
    assert.equal(card.lastBy, 'person');
    // Read again from the tab: the same card.
    assert.equal((await http('POST', '/api/photo-links/read', { add: ['Insectary'] })).body.proposalId, read.body.proposalId);

    // Applied from the card, then nothing is left.
    assert.equal((await http('POST', `/api/chat/proposals/${card.id}/photo-links/apply`, {})).status, 202);
    for (let i = 0; i < 200 && anyApplying(store); i++) await new Promise(r => setTimeout(r, 10));
    assert.equal(store.db.prepare('SELECT status FROM ai_proposals WHERE id = ?').get(card.id).status, 'applied');
    assert.ok(Object.keys(cellsOf(sheets, 6)).length);
    const again = await http('POST', '/api/photo-links/read', { add: ['Insectary'] });
    assert.deepEqual([again.body.proposalId, again.body.upToDate], [null, true]);
    // A folder Drive does not have.
    assert.deepEqual((await http('POST', '/api/photo-links/read', { only: ['Nowhere_2030'] })).body.notFound, ['Nowhere_2030']);
  } finally {
    store.close();
  }
});
const anyApplying = store => !!store.db.prepare("SELECT 1 FROM ai_proposals WHERE status = 'applying'").get();

test('a card whose rows are no longer kept is read again from its button', async () => {
  const { store } = await fixture();
  const assistant = createAssistant({ store, config: { drive: new LocalDrive(drive()) } });
  store.db
    .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
    .run(createHash('sha256').update('kim-token').digest('hex'), 'kim');
  const kim = { id: 'kim', username: 'kim', role: 'editor' };
  try {
    const out = await assistant.mcp(
      { authorization: 'Bearer kim-token' },
      { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name: 'sync_photo_links', arguments: { only: ['Ecuador_2026'] } } },
    );
    const { scan } = JSON.parse(out.body.result.content[0].text);
    // Another app (a restart): the card is there, its rows are not.
    const later = createAssistant({ store, config: { drive: new LocalDrive(drive()) } });
    const http = (method, path, body = {}) => later.handle({ method, path: `/api/chat/proposals/${scan}/photo-links/${path}`, user: kim, query: {}, body });
    assert.equal((await http('GET', 'rows')).status, 410);
    assert.equal((await http('POST', 'apply')).status, 410);
    assert.equal((await http('POST', 'refresh')).status, 200);
    // The folder's row and its six new files.
    const rows = (await http('GET', 'rows')).body.rows;
    assert.equal(rows.length, 7);
    assert.deepEqual(JSON.parse(store.db.prepare('SELECT sync_json FROM ai_proposals WHERE id = ?').get(scan).sync_json).only, ['Ecuador_2026']);
    assert.equal((await http('POST', 'apply')).status, 202);
  } finally {
    await new Promise(r => setTimeout(r, 200));
    store.close();
  }
});
