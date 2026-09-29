import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { Store } from '../server/store.mjs';
import { GoogleSheets, LocalSheets } from '../server/sheets.mjs';
import { applyBatch } from '../server/batch.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { createSheetHook } from '../server/hooks.mjs';
import { configFromEnv } from '../server/index.mjs';
import { REAL_ID, SANDBOX_ID, checkWorkbookId, workbookFromEnv } from '../server/workbook.mjs';
import { readWorkbook, switchWorkbook, formatReport } from '../server/switch.mjs';
import { modules } from '../server/schema.mjs';

const ana = { id: 'u-ana', username: 'ana', displayName: 'Ana', role: 'editor' };

function tempDir(t) {
  const dir = mkdtempSync(join(tmpdir(), 'switch-test-'));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  return dir;
}

test('the workbook comes from WORKBOOK_ID, the team workbook by default; other text is refused', () => {
  assert.equal(workbookFromEnv({}).id, REAL_ID);
  assert.equal(configFromEnv({}).spreadsheetId, REAL_ID);
  assert.equal(workbookFromEnv({ WORKBOOK_ID: SANDBOX_ID }).id, SANDBOX_ID);
  assert.equal(
    workbookFromEnv({ WORKBOOK_ID: SANDBOX_ID }).url,
    `https://docs.google.com/spreadsheets/d/${SANDBOX_ID}/edit`,
  );
  for (const bad of [`https://docs.google.com/spreadsheets/d/${REAL_ID}/edit`, 'abc', `${REAL_ID} x`])
    assert.throws(() => checkWorkbookId(bad), { code: 'INVALID_WORKBOOK_ID' });
  assert.throws(() => configFromEnv({ WORKBOOK_ID: 'not an id' }), { code: 'INVALID_WORKBOOK_ID' });
});

test('a read-only Google connection refuses writes before any request', async t => {
  const dir = tempDir(t);
  const credentials = join(dir, 'google.json');
  writeFileSync(credentials, JSON.stringify({ client_id: 'c', client_secret: 's', refresh_token: 'r' }));
  assert.throws(() => new GoogleSheets({ spreadsheetId: 'bad id', googleCredentialsFile: credentials }), {
    code: 'INVALID_WORKBOOK_ID',
  });
  const sheets = new GoogleSheets({ spreadsheetId: REAL_ID, googleCredentialsFile: credentials, readOnly: true });
  sheets.accessToken = () => assert.fail('no token is needed to refuse a write');
  await assert.rejects(sheets.batchUpdate([]), /read-only/);
  await assert.rejects(
    sheets.writeBatch([{ sheet: 'Collection_data', row: 2, changes: {}, columns: {} }]),
    /read-only|No cells/,
  );
});

test('a database that caches another workbook is refused until it is switched', async t => {
  const dir = tempDir(t);
  const path = join(dir, 'app.sqlite');
  // A database from before workbookId was stored: records of the test copy.
  const first = new Store({ databasePath: path }, { sheets: new LocalSheets({}, { spreadsheetId: SANDBOX_ID }) });
  first.db.exec('DELETE FROM settings');
  first.db
    .prepare(
      "INSERT INTO records(id,sheet,row_num,values_json,formulas_json,identity_json,label,version,updated_at) VALUES('r1','Collection_data',2,'{}','{}','{}','x',1,'t')",
    )
    .run();
  first.close();
  const google = id => ({ spreadsheetId: id, readSheet: async () => [] });
  assert.throws(() => new Store({ databasePath: path }, { sheets: google(REAL_ID) }), { code: 'WORKBOOK_MISMATCH' });
  const legacy = new Store({ databasePath: path }, { sheets: google(SANDBOX_ID) });
  assert.equal(legacy.getSetting('workbookId'), SANDBOX_ID);
  legacy.close();
  assert.throws(() => new Store({ databasePath: path }, { sheets: google(REAL_ID) }), { code: 'WORKBOOK_MISMATCH' });
  // A new database takes the workbook it is opened with.
  const fresh = new Store({ databasePath: join(dir, 'new.sqlite') }, { sheets: google(REAL_ID) });
  assert.equal(fresh.getSetting('workbookId'), REAL_ID);
  fresh.close();
});

test('the Sheets hook ignores reports from another workbook', () => {
  const store = new Store({ localMode: true }, { sheets: new LocalSheets({}) });
  const hook = createSheetHook(store, { secret: 'secret', delayMs: 60_000 });
  const headers = { 'x-hook-secret': 'secret' };
  const event = { sheet: 'Collection_data', startRow: 2, numRows: 1, change: 'EDIT' };
  assert.deepEqual(hook.receive(headers, { spreadsheetId: SANDBOX_ID, events: [event] }), {
    accepted: 0,
    ignored: 'other workbook',
  });
  assert.equal(hook.receive(headers, { spreadsheetId: REAL_ID, events: [event] }).accepted, 1);
  store.close();
});

/** Every sheet of the app (with its header row), plus the given rows. */
const workbook = rows => ({ ...Object.fromEntries(modules.map(m => [m.id, []])), ...rows });
const cam = (id, extra = {}) => ({
  values: { CAM_ID: id, SPECIES: 'Oleria gunilla', Collection_date: 46290, ...extra },
});

test('the switch keeps record ids by identity, remaps moved rows and resets the test history', async t => {
  const dir = tempDir(t);
  const path = join(dir, 'app.sqlite');
  const testCopy = new LocalSheets(
    workbook({
      Collection_data: [
        { row: 2, ...cam('CAM000001') },
        { row: 3, ...cam('CAM000002') },
        {
          row: 4,
          values: { SPECIES: 'Hyposcada illinissa', Collector: 'FCH', Notes_Collection_data: 'no identifiers' },
        },
      ],
      Insectary_data: [
        { row: 2, values: { Insectary_ID: 'A0A', SPECIES: 'Melinaea menophilus', Sex: 'male' } },
        { row: 3, values: { Insectary_ID: 'A1A' } },
      ],
    }),
    { spreadsheetId: SANDBOX_ID },
  );
  const store = new Store({ databasePath: path }, { sheets: testCopy });
  await store.sync();
  store.db
    .prepare(
      "INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES(?,?,?,?,'s','h',1,'t')",
    )
    .run(ana.id, ana.username, ana.displayName, ana.role);
  // A test save: a new specimen (row 5) and an edit of the pre-made Insectary row.
  const created = await applyBatch(
    store,
    {
      requestId: 'switch-test-save-1',
      creates: [{ module: 'Collection_data', values: { CAM_ID: 'CAM099999', SPECIES: 'Test butterfly' } }],
    },
    ana,
  );
  const testOnly = created.records.find(r => r.values.CAM_ID === 'CAM099999').id;
  const premade = store.getRecordBySheetRow('Insectary_data', 3);
  await applyBatch(
    store,
    {
      requestId: 'switch-test-save-2',
      edits: [{ id: premade.id, values: { SPECIES: 'Mechanitis polymnia' }, expectedVersion: premade.version }],
    },
    ana,
  );
  const ids = {
    one: store.getRecordBySheetRow('Collection_data', 2).id,
    two: store.getRecordBySheetRow('Collection_data', 3).id,
    blank: store.getRecordBySheetRow('Collection_data', 4).id,
    male: store.getRecordBySheetRow('Insectary_data', 2).id,
  };
  // App state pointing at records: a monitoring walk with a manual link, a pending proposal, a verdict.
  store.db
    .prepare(
      "INSERT INTO monitoring_tracks(id,request_id,fingerprint,date,collector,name,data_json,created_by,created_at) VALUES('t1',NULL,'f1','2026-09-25','FCH','walk',?,'u-ana','t')",
    )
    .run(
      JSON.stringify({
        track: [],
        captures: [
          { lat: 0, lon: 0, text: 'B', row: 3, recordId: ids.two, link: 'manual' },
          { lat: 0, lon: 0, text: 'T', row: 5, recordId: testOnly, link: 'manual' },
        ],
      }),
    );
  createAssistant({ store, config: { claude: {} } });
  store.db
    .prepare("INSERT INTO ai_threads(id,owner_id,title,created_at,updated_at) VALUES('th','u-ana','t','t','t')")
    .run();
  store.db
    .prepare(
      "INSERT INTO ai_proposals(id,thread_id,owner_id,changes_json,reason,status,created_at) VALUES('p1','th','u-ana','[]','Benchmark – test','pending','t'),('p2','th','u-ana','[]','done','applied','t')",
    )
    .run();
  store.close();

  // The team's workbook: a row inserted above CAM000002, a team edit, the test rows absent.
  const real = new LocalSheets(
    workbook({
      Collection_data: [
        { row: 2, ...cam('CAM000001', { Sex: 'female' }) },
        { row: 3, ...cam('CAM000123') },
        { row: 4, ...cam('CAM000002') },
        {
          row: 5,
          values: { SPECIES: 'Hyposcada illinissa', Collector: 'FCH', Notes_Collection_data: 'no identifiers' },
        },
      ],
      Insectary_data: [
        { row: 2, values: { Insectary_ID: 'A0A', SPECIES: 'Melinaea menophilus', Sex: 'male' } },
        { row: 3, values: { Insectary_ID: 'A1A', CAM_ID: 'CAM000123', SPECIES: 'Oleria gunilla' } },
        { row: 4, values: { Insectary_ID: 'A2A' } },
      ],
    }),
    { spreadsheetId: REAL_ID },
  );
  const writes = [];
  real.writeBatch = async w => writes.push(w);
  real.batchUpdate = async r => writes.push(r);
  const adapter = await readWorkbook(real);
  const switching = new Store({ databasePath: path }, { sheets: adapter, switching: true });
  const report = await switchWorkbook(switching);
  assert.equal(report.from, SANDBOX_ID);
  assert.equal(report.to, REAL_ID);
  assert.equal(writes.length, 0, 'the switch never writes to the workbook');
  const at = (sheet, row) => switching.getRecordBySheetRow(sheet, row);
  // Same specimen, same id: in place (with the team's edit), or moved by an inserted row.
  assert.equal(at('Collection_data', 2).id, ids.one);
  assert.equal(at('Collection_data', 2).values.Sex, 'female');
  assert.equal(at('Collection_data', 4).id, ids.two);
  assert.equal(at('Collection_data', 5).id, ids.blank);
  assert.equal(at('Insectary_data', 2).id, ids.male);
  // New rows of the team, and the test copy's own rows retired.
  assert.ok(![ids.one, ids.two, ids.blank, testOnly].includes(at('Collection_data', 3).id));
  assert.equal(switching.getRecord(testOnly).missing, true);
  assert.equal(at('Insectary_data', 3).values.CAM_ID, 'CAM000123');
  assert.notEqual(at('Insectary_data', 3).id, premade.id, 'another specimen took the pre-made row');
  assert.equal(at('Insectary_data', 4).values.Insectary_ID, 'A2A');
  const collection = report.sheets.find(s => s.sheet === 'Collection_data');
  assert.deepEqual(
    {
      moved: collection.moved,
      added: collection.added,
      retired: collection.retired,
      changedRows: collection.changedRows,
    },
    { moved: 2, added: 1, retired: 1, changedRows: 1 },
  );
  // No Historial entries for the differences; the test history archived.
  assert.equal(switching.db.prepare('SELECT count(*) n FROM actions').get().n, 0);
  assert.equal(switching.db.prepare('SELECT count(*) n FROM changes').get().n, 0);
  assert.equal(switching.db.prepare('SELECT count(*) n FROM archived_actions').get().n, 2);
  assert.ok(switching.db.prepare('SELECT count(*) n FROM archived_changes').get().n >= 3);
  assert.equal(switching.getHistory().total, 0);
  // Pending proposals discarded, applied ones kept as they were.
  assert.deepEqual(
    report.discardedProposals.map(p => p.id),
    ['p1'],
  );
  assert.deepEqual(
    switching.db
      .prepare('SELECT id, status FROM ai_proposals ORDER BY id')
      .all()
      .map(p => [p.id, p.status]),
    [
      ['p1', 'discarded'],
      ['p2', 'applied'],
    ],
  );
  assert.deepEqual(report.references['monitoring (manual link)'], { total: 2, kept: 1, lost: 1 });
  assert.equal(switching.getSetting('workbookId'), REAL_ID);
  assert.equal(switching.getSetting('previousWorkbookId'), SANDBOX_ID);
  assert.match(formatReport(report).join('\n'), /AI proposals discarded \(1\)/);
  await assert.rejects(switchWorkbook(switching), { code: 'SAME_WORKBOOK' });
  switching.close();

  // From now on the database opens on the team's workbook only, and a sync finds nothing new to log.
  assert.throws(() => new Store({ databasePath: path }, { sheets: testCopy }), { code: 'WORKBOOK_MISMATCH' });
  const after = new Store({ databasePath: path }, { sheets: real });
  const sync = await after.sync({ force: true });
  assert.equal(sync.changed + sync.added + sync.moved + sync.missing, 0);
  assert.equal(after.getHistory().total, 0);
  after.close();
});

test('a new workbook whose header cannot be read stops the switch before anything changes', async () => {
  const real = new LocalSheets(
    { Collection_data: [{ row: 1, cells: [{ userEnteredValue: { stringValue: 'nothing' } }] }] },
    { spreadsheetId: REAL_ID },
  );
  await assert.rejects(readWorkbook(real), { code: 'HEADER_PROBLEMS' });
});
