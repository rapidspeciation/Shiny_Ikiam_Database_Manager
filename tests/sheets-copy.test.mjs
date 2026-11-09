import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { existsSync, mkdtempSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { DatabaseSync } from 'node:sqlite';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { moduleMap, parseDateText } from '../server/schema.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { buildCopy, createSheetsCopy, isoOfText } from '../server/replica.mjs';
import { formatRows, openCopy, runQuery, sqlProblem } from '../server/query-tool.mjs';
import { addToken, addUser, mcpClient } from './helpers/assistant.mjs';

// The sheets' copy the assistant's `query` reads (server/replica.mjs) and the tool itself
// (server/query-tool.mjs): only the workbook's data, typed for SQL, read-only, with a time limit.

const d = text => parseDateText(text);
const user = { id: 'u-ana', username: 'ana', displayName: 'Ana Pérez', role: 'editor' };

function formula(sheets, sheet, row, field, text, value) {
  const column = moduleMap.get(sheet).fields.find(f => f.key === field).column;
  sheets.rows.get(sheet).find(r => r.row === row).cells[column] = {
    userEnteredValue: { formulaValue: text },
    effectiveValue: typeof value === 'number' ? { numberValue: value } : { stringValue: value },
  };
}
const cellOf = (sheets, sheet, row, field) => {
  const column = moduleMap.get(sheet).fields.find(f => f.key === field).column;
  return (sheets.rows.get(sheet).find(r => r.row === row).cells[column] = { userEnteredValue: {} });
};

const SEED = {
  Insectary_data: [
    {
      row: 2,
      values: {
        Insectary_ID: 'A1A',
        SPECIES: 'Mechanitis lysimnia',
        Sex: 'male',
        'CLUTCH NUMBER': 997,
        Intro2Insectary_date: d('2026-09-30'),
        Collection_location: 'Ikiam',
        COLLECTION_LOCATION: 'IKIAM_CAMPUS',
      },
    },
    { row: 3, values: { Insectary_ID: 'A2A', SPECIES: 'Melinaea mothone', Sex: 'NA', 'CLUTCH NUMBER': 'NA', Death_date: '9-mrt-26' } },
    // Made ahead: only its ID formula.
    { row: 4, values: {} },
  ],
  Insectary_stocks: [{ row: 2, values: { 'CLUTCH NUMBER': 997, SPECIES: 'Mechanitis lysimnia', Species: 'lysimnia', 'DATE LAID': d('2026-09-01') } }],
};

async function setup(dir, seed = SEED) {
  const sheets = new LocalSheets(structuredClone(seed));
  if (seed === SEED) formula(sheets, 'Insectary_data', 4, 'Insectary_ID', '=CONCATENATE("A",3,"A")', 'A3A');
  const store = new Store({ localMode: true, databasePath: join(dir, 'app.sqlite') }, { sheets });
  await store.sync();
  addUser(store, user);
  const at = row => store.getRecordBySheetRow('Insectary_data', row);
  return { sheets, store, at };
}

async function until(check, ms = 15000) {
  const end = Date.now() + ms;
  for (;;) {
    const value = await check();
    if (value) return value;
    if (Date.now() > end) throw new Error(`timed out waiting: ${check}`);
    await new Promise(resolve => setTimeout(resolve, 10));
  }
}

test('the copy: a table per sheet with its rows in use, typed columns, the lists, the history; nothing private', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'sheets-copy-'));
  try {
    const { sheets, store, at } = await setup(dir);
    // A save in the app, and an edit in Google Sheets read by a sync.
    await store.updateRecord(at(2).id, { values: { Sex: 'female' }, requestId: randomUUID() }, user);
    cellOf(sheets, 'Insectary_data', 3, 'Sex').userEnteredValue.stringValue = 'male';
    await store.sync({ force: true });

    const out = join(dir, 'sheets.sqlite');
    const result = buildCopy(store.db, out);
    assert.equal(result.rows, 4);
    const copy = new DatabaseSync(out, { readOnly: true });
    const all = sql => copy.prepare(sql).all();
    const one = sql => copy.prepare(sql).get();

    const tables = all("SELECT name, type FROM sqlite_master WHERE type IN ('table','view') ORDER BY name").map(r => `${r.type}:${r.name}`);
    assert.ok(tables.includes('table:Insectary_data_all') && tables.includes('view:Insectary_data'));
    assert.ok(tables.includes('table:history') && tables.includes('table:_columns') && tables.includes('table:_tables'));
    assert.ok(tables.includes('view:F1/F2_MutationRate'), 'every sheet, its name as it is');
    for (const name of ['users', 'sessions', 'settings', 'records', 'actions', 'changes', 'ai_tokens', 'ai_proposals'])
      assert.ok(!tables.some(t => t.endsWith(`:${name}`)), `${name} is not copied`);

    // Rows in use; the pre-made row only in _all.
    assert.equal(one('SELECT count(*) n FROM Insectary_data').n, 2);
    assert.equal(one('SELECT count(*) n FROM Insectary_data_all').n, 3);
    assert.deepEqual({ ...one("SELECT _row, _premade, Insectary_ID FROM Insectary_data_all WHERE _premade = 1") }, { _row: 4, _premade: 1, Insectary_ID: 'A3A' });
    assert.equal(one("SELECT _id FROM Insectary_data WHERE Insectary_ID = 'a1a'")._id, at(2).id, 'IDs match without case; _id is the recordId');
    assert.deepEqual({ ...one('SELECT * FROM _tables WHERE name = \'Insectary_data\'') }, { name: 'Insectary_data', rows: 2, premade: 1 });

    // Types: dates as YYYY-MM-DD (also typed as text), CLUTCH NUMBER numeric beside "NA".
    assert.equal(one("SELECT Intro2Insectary_date v FROM Insectary_data WHERE _row = 2").v, '2026-09-30');
    assert.equal(one("SELECT Death_date v FROM Insectary_data WHERE _row = 3").v, '2026-03-09');
    assert.equal(one('SELECT count(*) n FROM Insectary_data WHERE "CLUTCH NUMBER" = 997').n, 1);
    assert.deepEqual(all('SELECT typeof("CLUTCH NUMBER") t FROM Insectary_data ORDER BY _row').map(r => r.t), ['integer', 'text']);
    assert.equal(one('SELECT i.Insectary_ID FROM Insectary_data i JOIN Insectary_stocks s ON s."CLUTCH NUMBER" = i."CLUTCH NUMBER"').Insectary_ID, 'A1A');
    assert.equal(one('SELECT "DATE LAID" v FROM Insectary_stocks').v, '2026-09-01');

    // Names that differ only in case: the later one gets _2, and _columns says which header it is.
    assert.equal(one("SELECT column FROM _columns WHERE sheet = 'Insectary_data' AND header = 'COLLECTION_LOCATION'").column, 'COLLECTION_LOCATION_2');
    assert.equal(one('SELECT COLLECTION_LOCATION_2 v FROM Insectary_data WHERE _row = 2').v, 'IKIAM_CAMPUS');
    assert.equal(one("SELECT column FROM _columns WHERE sheet = 'Insectary_stocks' AND header = 'Species'").column, 'Species_2');
    // Dropdown lists as JSON.
    const sex = one("SELECT type, list, strict FROM _columns WHERE sheet = 'Insectary_data' AND column = 'Sex'");
    assert.ok(JSON.parse(sex.list).includes('female') && sex.type === 'text');
    assert.equal(one("SELECT type FROM _columns WHERE sheet = 'Insectary_data' AND column = 'Intro2Insectary_date'").type, 'date');

    // The history: people by name, Google Sheets edits without one; formulas never as text.
    const saved = one("SELECT * FROM history WHERE field = 'Sex' AND id_label = 'A1A'");
    assert.equal(saved.who, 'Ana Pérez');
    assert.equal(saved.before, 'male');
    assert.equal(saved.after, 'female');
    assert.equal(saved.record_id, at(2).id);
    assert.equal(saved.row, 2);
    const edited = one("SELECT * FROM history WHERE field = 'Sex' AND id_label = 'A2A'");
    assert.equal(edited.who, null);
    assert.equal(edited.source, 'Google Sheets');
    assert.equal(edited.after, 'male');
    const everything = JSON.stringify(all('SELECT * FROM history')) + JSON.stringify(all('SELECT * FROM Insectary_data_all'));
    assert.doesNotMatch(everything, /CONCATENATE/);
    copy.close();
    store.close();
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});

test('dates typed as text, day first', () => {
  assert.equal(isoOfText('30/9/26'), '2026-09-30');
  assert.equal(isoOfText('09-Aug-26'), '2026-08-09');
  assert.equal(isoOfText('2026-09-30'), '2026-09-30');
  assert.equal(isoOfText('31/4'), null);
  assert.equal(isoOfText('NA'), null);
});

test('date columns hold dates only: NA and other text are in <column>_text, so a "since" count or a sort sees dates', async () => {
  // Lab, 4 Nov 2026: "27 deaths without cause and 43 clutches without species since 29 Oct" were rows with NA as date
  // ('NA' > '2026-10-29' as text).
  const dir = mkdtempSync(join(tmpdir(), 'sheets-copy-dates-'));
  try {
    const { store } = await setup(dir, {
      Insectary_data: [
        { row: 2, values: { Insectary_ID: 'A1A', Death_date: d('2026-10-30'), Death_cause: 'Unknown' } },
        { row: 3, values: { Insectary_ID: 'A2A', SPECIES: 'Melinaea mothone', Death_date: 'NA', Death_cause: 'Disappearance', Preservation_date: 'NA' } },
        { row: 4, values: { Insectary_ID: 'A3A', Death_date: '9/30', Death_cause: 'NA' } },
        { row: 5, values: { Insectary_ID: 'A4A', Sex: 'male' } },
      ],
      Insectary_stocks: [
        { row: 2, values: { 'CLUTCH NUMBER': 997, SPECIES: 'Mechanitis lysimnia', 'DATE LAID': d('2026-10-30') } },
        { row: 3, values: { 'CLUTCH NUMBER': 998, 'DATE LAID': 'NA' } },
      ],
    });
    const out = join(dir, 'sheets.sqlite');
    buildCopy(store.db, out);
    const copy = new DatabaseSync(out, { readOnly: true });
    const one = sql => copy.prepare(sql).get();
    const all = sql => copy.prepare(sql).all().map(r => ({ ...r }));
    try {
      assert.equal(one("SELECT count(*) n FROM Insectary_data WHERE Death_date >= '2026-10-29'").n, 1);
      assert.equal(one("SELECT count(*) n FROM Insectary_stocks WHERE \"DATE LAID\" >= '2026-10-29'").n, 1);
      assert.equal(one('SELECT Insectary_ID FROM Insectary_data ORDER BY Death_date DESC LIMIT 1').Insectary_ID, 'A1A');
      assert.equal(one('SELECT max(Death_date) m FROM Insectary_data').m, '2026-10-30');
      // What was typed instead of a date stays readable beside it; an empty cell is NULL in both.
      assert.deepEqual(all('SELECT Insectary_ID, Death_date, Death_date_text FROM Insectary_data ORDER BY _row'), [
        { Insectary_ID: 'A1A', Death_date: '2026-10-30', Death_date_text: null },
        { Insectary_ID: 'A2A', Death_date: null, Death_date_text: 'NA' },
        { Insectary_ID: 'A3A', Death_date: null, Death_date_text: '9/30' },
        { Insectary_ID: 'A4A', Death_date: null, Death_date_text: null },
      ]);
      assert.equal(one('SELECT "DATE LAID_text" t FROM Insectary_stocks WHERE "CLUTCH NUMBER" = 998').t, 'NA');
      // A date column holding only NA (no date yet) is still a date column.
      assert.equal(one("SELECT count(*) n FROM Insectary_data WHERE Preservation_date >= '2026-10-29'").n, 0);
      assert.equal(one("SELECT Preservation_date_text t FROM Insectary_data WHERE Insectary_ID = 'A2A'").t, 'NA');
      // _columns lists the companion right after its date column, without a header of its own.
      const cols = all("SELECT column, header, type FROM _columns WHERE sheet = 'Insectary_data'");
      const at = cols.findIndex(c => c.column === 'Death_date');
      assert.deepEqual(cols[at + 1], { column: 'Death_date_text', header: null, type: 'text' });
    } finally {
      copy.close();
      store.close();
    }
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});

test('tubes: every tube cell with its number, so the last or highest tube is not the text sort (typos, NA dates)', async () => {
  // Lab, 3 Nov 2026: three answers to "the last wing tube": MAX(Tube_1_id) gave FS904215346 (a typo with an extra digit),
  // and ORDER BY Preservation_date DESC put the dead clipped butterflies (Preservation_date NA) above the living ones.
  const dir = mkdtempSync(join(tmpdir(), 'sheets-copy-tubes-'));
  const clip = '**OTHER_SOMATIC_ANIMAL_TISSUE** | WING CLIP';
  const tube = (id, tissue, extra = {}) => ({ SPECIES: 'Mechanitis lysimnia', Tube_1_id: id, Tube_1_tissue: tissue, T1_Preservation_medium: 'Flash frozen', Tube_2_id: 'NA', Tube_2_tissue: 'NOT_COLLECTED', ...extra });
  try {
    const { store } = await setup(dir, {
      Insectary_data: [
        { row: 2, values: { Insectary_ID: '7VP', CAM_ID: 'CAM076001', ...tube('FS90415684', 'WHOLE_ORGANISM', { Preservation_date: d('2025-09-30') }) } },
        { row: 3, values: { Insectary_ID: 'H4B', CAM_ID: 'CAM078465', ...tube('FS90415345', clip) } },
        { row: 4, values: { Insectary_ID: 'J7A', CAM_ID: 'CAM078151', ...tube('FS63886669', clip, { Death_date: d('2026-09-22'), Preservation_date: 'NA' }) } },
        { row: 5, values: { Insectary_ID: 'P4D', CAM_ID: 'CAM078466', ...tube('FS904215346', clip) } },
        { row: 6, values: { Insectary_ID: 'F1B', CAM_ID: 'CAM078277', ...tube('FS6388765', 'THORAX | LEG') } },
        { row: 7, values: { Insectary_ID: 'A7E.1', CAM_ID: 'CAM078468', ...tube('FS90415348', clip) } },
        { row: 8, values: { Insectary_ID: 'B1E', SPECIES: 'Mechanitis lysimnia' } },
      ],
      Collection_data: [{ row: 2, values: { CAM_ID: 'CAM077538', Tube_1_id: 'FS90415349', Tube_1_tissue: 'WHOLE_ORGANISM', Preservation_medium: 'Flash frozen' } }],
    });
    // A clip saved in the app: the tubes table knows when.
    const b1e = store.getRecordBySheetRow('Insectary_data', 8);
    await store.updateRecord(b1e.id, { values: { CAM_ID: 'CAM078470', Tube_1_id: 'FS90415350', Tube_1_tissue: clip }, requestId: randomUUID() }, user);
    await store.sync({ force: true });
    const out = join(dir, 'sheets.sqlite');
    buildCopy(store.db, out);
    const copy = new DatabaseSync(out, { readOnly: true });
    const one = sql => ({ ...copy.prepare(sql).get() });
    try {
      // The control arm's query: the newest row of a living clipped butterfly, not a dead one with NA.
      assert.equal(one("SELECT Insectary_ID FROM Insectary_data WHERE Tube_1_tissue LIKE '%WING%' AND _row < 8 ORDER BY Preservation_date DESC, _row DESC LIMIT 1").Insectary_ID, 'A7E.1');
      // A row per tube cell (NA left out), with its tissue, medium, label and CAM.
      assert.equal(one('SELECT count(*) n FROM tubes').n, 8);
      assert.deepEqual(one("SELECT tube, prefix, number, digits, well_formed, sheet, column, _row, label, cam, tissue, medium FROM tubes WHERE tube = 'fs90415345'"), {
        tube: 'FS90415345', prefix: 'FS', number: 90415345, digits: 8, well_formed: 1, sheet: 'Insectary_data', column: 'Tube_1_id', _row: 3,
        label: 'H4B', cam: 'CAM078465', tissue: clip, medium: 'Flash frozen',
      });
      assert.equal(one("SELECT preserved FROM tubes WHERE tube = 'FS90415684'").preserved, '2025-09-30');
      // Typos are not well formed; by number the highest real FS tube is FS90415684, not FS904215346 or FS6388765.
      assert.equal(one('SELECT group_concat(tube) t FROM (SELECT tube FROM tubes WHERE well_formed = 0 ORDER BY tube)').t, 'FS6388765,FS904215346');
      assert.equal(one("SELECT tube FROM tubes WHERE prefix = 'FS' AND well_formed = 1 ORDER BY number DESC LIMIT 1").tube, 'FS90415684');
      // The last wing tube entered: the one saved in the app, then the sheet's later rows.
      assert.match(one("SELECT entered FROM tubes WHERE tube = 'FS90415350'").entered, /^\d{4}-\d\d-\d\dT/);
      assert.equal(one("SELECT entered FROM tubes WHERE tube = 'FS90415348'").entered, null);
      assert.equal(one("SELECT tube FROM tubes WHERE tissue LIKE '%WING CLIP%' ORDER BY entered DESC, _row DESC LIMIT 1").tube, 'FS90415350');
      // The next number after the clip run, checked against every sheet: 349 and 350 are held.
      assert.deepEqual(
        copy.prepare("SELECT number FROM tubes WHERE prefix = 'FS' AND number BETWEEN 90415348 AND 90415351 ORDER BY number").all().map(r => r.number),
        [90415348, 90415349, 90415350],
      );
    } finally {
      copy.close();
      store.close();
    }
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});

test('query: one read-only statement, capped, stopped after the time limit, with fold and km', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'sheets-query-'));
  const { store, at } = await setup(dir);
  const assistant = createAssistant({
    store,
    config: { sheetsCopyPath: join(dir, 'sheets.sqlite'), sheetsCopy: { startMs: 0, delayMs: 50, log: { log() {}, error: console.error } }, sheetsQuery: { timeoutMs: 1000 } },
  });
  // The time limit counts from when the query's process is ready (tests/query-runner.test.mjs); 1 s keeps the
  // runaway check short.
  addToken(store, user.id, 'ana-token');
  const { result } = mcpClient(assistant, 'ana-token');
  const call = async (sql, limit) => {
    const out = await result('query', { sql, ...(limit ? { limit } : {}) });
    const { text } = out.content[0];
    return out.isError ? JSON.parse(text) : text;
  };
  try {
    // Rows as tab-separated lines, the copy's age last.
    const text = await call('SELECT Insectary_ID, Sex, "CLUTCH NUMBER" FROM Insectary_data ORDER BY _row');
    const lines = text.split('\n');
    assert.deepEqual(lines.slice(0, 3), ['Insectary_ID\tSex\tCLUTCH NUMBER', 'A1A\tmale\t997', 'A2A\tNA\tNA']);
    assert.match(lines.at(-1), /^\(Copy of \d{4}-\d\d-\d\d \d\d:\d\d UTC, (just now; saves since then arrive within a minute|current: nothing was saved or synced since)\.\)$/);
    assert.equal((await call("SELECT ';' AS x")).split('\n')[1], ';', 'a ; inside quotes is text');

    // Only reading.
    for (const sql of ["INSERT INTO _meta VALUES ('a', 'b')", "ATTACH 'x.sqlite' AS x", 'PRAGMA query_only = 0', "UPDATE Insectary_data_all SET Sex = 'x'"])
      assert.match((await call(sql)).error, /^Only reading/, sql);
    assert.match((await call('SELECT 1; DELETE FROM _meta')).error, /^One statement per query/);
    assert.match((await call('SELECT 1; -- a comment')).split('\n')[1], /^1$/, 'a comment after the ; is fine');
    const written = await call("WITH x AS (SELECT 1) INSERT INTO _meta VALUES ('a', 'b')");
    assert.match(written.error, /^SQLite: .*readonly/i);
    assert.match(written.hint, /_tables/);
    // A wrong column: SQLite's message and the columns named like it.
    const wrong = await call('SELECT Sexo FROM Insectary_data');
    assert.match(wrong.error, /no such column: Sexo/);
    assert.match(wrong.hint, /_columns/, 'a close column (Sex): the hint, not the list');
    // Nothing like it (retro 8 Nov: Sampling_protocol, Transect_ID): the table's real columns, so the next query needs no look-up.
    const none = await call("SELECT count(*) FROM Insectary_data WHERE Sampling_protocol = 'x'");
    assert.match(none.error, /no such column: Sampling_protocol/);
    assert.match(none.columns.Insectary_data, /^Insectary_ID, Wild_Reared, "CLUTCH NUMBER", Stock_of_origin, SPECIES, Sex, /);
    assert.equal(none.hint, undefined);

    // The rows shown are capped; the total says how many there are.
    const many = (await call('WITH RECURSIVE c(x) AS (SELECT 1 UNION ALL SELECT x + 1 FROM c WHERE x < 1000) SELECT x FROM c', 10)).split('\n');
    assert.equal(many.length, 1 + 10 + 2);
    assert.match(many[11], /^\(10 of 1000 rows shown; limit goes up to 500\./);

    // A runaway query is stopped; the next one runs in a new process.
    const started = Date.now();
    const runaway = await call('WITH RECURSIVE c(x) AS (SELECT 1 UNION ALL SELECT x + 1 FROM c) SELECT count(*) FROM c');
    assert.match(runaway.error, /ran over 1 s and was stopped/);
    assert.ok(Date.now() - started < 15_000, 'stopped at the limit, not left running');
    assert.equal((await call('SELECT count(*) FROM Insectary_data')).split('\n')[1], '2');

    // Helpers: fold for matching without accents or case, km between two points.
    const helpers = (await call("SELECT fold('Ñandú ÁBC') f, round(km(0, 0, 0, 1), 1) k, km('NA', 0, 0, 1) n")).split('\n');
    assert.equal(helpers[1], 'nandu abc\t111.2\t');

    // The copy follows the app's saves.
    await store.updateRecord(at(3).id, { values: { Sex: 'female' }, requestId: randomUUID() }, user);
    await new Promise(resolve => setImmediate(resolve));
    assert.match(await call('SELECT 1'), /saves since then arrive within a minute/);
    await until(async () => (await call("SELECT Sex FROM Insectary_data WHERE Insectary_ID = 'A2A'")).split('\n')[1] === 'female');
    // Once the copy holds them it is current, however long ago it was built.
    await until(async () => (await call('SELECT 1')).endsWith('UTC, current: nothing was saved or synced since.)'));
  } finally {
    assistant.close();
    store.close();
    rmSync(dir, { recursive: true, force: true });
  }
});

test('the copy is rebuilt after a sync that changed rows, and only when something changed', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'sheets-rebuild-'));
  const { sheets, store } = await setup(dir);
  const path = join(dir, 'sheets.sqlite');
  const logged = [];
  const copy = createSheetsCopy({ store, path, startMs: 0, delayMs: 60_000, log: { log: m => logged.push(m), error: (...m) => logged.push(m.join(" ")) } });
  try {
    await until(() => copy.status().builtAt && !copy.status().pending);
    assert.ok(existsSync(path));
    const first = copy.status().builtAt;
    assert.equal(logged.length, 1, logged.join('\n'));
    // Nothing changed: no new copy.
    await copy.rebuild();
    assert.equal(copy.status().builtAt, first);
    assert.equal(logged.length, 1);

    // An edit in Google Sheets read by a sync: the copy follows right after it (not 60 s later).
    cellOf(sheets, 'Insectary_data', 2, 'SPECIES').userEnteredValue.stringValue = 'Mechanitis polymnia';
    await store.sync({ force: true });
    await until(() => copy.status().builtAt !== first && !copy.status().pending);
    const db = new DatabaseSync(path, { readOnly: true });
    assert.equal(db.prepare("SELECT SPECIES FROM Insectary_data WHERE Insectary_ID = 'A1A'").get().SPECIES, 'Mechanitis polymnia');
    db.close();
  } finally {
    copy.close();
    store.close();
    rmSync(dir, { recursive: true, force: true });
  }
});

test("the copy's age: current when nothing was saved or synced since it was built", () => {
  const result = { columns: ['n'], rows: [[1]], total: 1, builtAt: '2026-10-15T20:17:03.000Z' };
  const now = Date.parse('2026-10-16T15:20:00Z');
  assert.equal(formatRows(result, { now }).split('\n').at(-1), '(Copy of 2026-10-15 20:17 UTC, current: nothing was saved or synced since.)');
  assert.equal(formatRows(result, { now, pending: true }).split('\n').at(-1), '(Copy of 2026-10-15 20:17 UTC, 19 h ago; saves since then arrive within a minute.)');
});

test('a column the query does not find: the closest of its own tables, else where its values or name are', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'sheets-near-'));
  const cams = ['CAM070476', 'CAM070477', 'CAM070478', 'CAM070479', 'CAM070480', 'CAM070481'];
  const sheets = new LocalSheets({
    Collection_data: cams.map((CAM_ID, i) => ({
      row: i + 2,
      values: { CAM_ID, FieldMark_ID: `B${i}`, Collection_date: d('2026-10-14'), Death_date: d('2026-10-15'), DATE_OF_COLLECTION: d('2026-10-14') },
    })),
    MEIER_manifests_23Jun26: cams.map((cam, i) => ({ row: i + 2, values: { 'Sampleset ID': 'MEIER_000002', 'Tube/Well ID': `FD3088045${i}`, 'STS Collector Sample ID': cam } })),
    Insectary_data: [{ row: 2, values: { Insectary_ID: 'A1A', 'CLUTCH NUMBER': 976, Stock_of_origin: 'deceptus' } }],
    Insectary_stocks: [{ row: 2, values: { 'CLUTCH NUMBER': 976, SPECIES: 'Mechanitis messenoides deceptus' } }],
  });
  const store = new Store({ localMode: true, databasePath: join(dir, 'app.sqlite') }, { sheets });
  await store.sync();
  const path = join(dir, 'sheets.sqlite');
  buildCopy(store.db, path);
  const db = openCopy(path);
  const near = sql => {
    const out = runQuery(db, sql, 5);
    assert.ok(out.error, sql);
    return out.near;
  };
  try {
    // The production chats' guesses (13-18 Oct): the column meant comes first, from the query's own table.
    assert.deepEqual(near("SELECT CAM_ID, Field_ID FROM Collection_data WHERE Country = 'Colombia'"), ['Collection_data.FieldMark_ID']);
    assert.deepEqual(near('SELECT c.Field_ID FROM Collection_data c JOIN Insectary_data i ON i.Insectary_ID = c.Insectary_ID'), ['Collection_data.FieldMark_ID']);
    const date = near('SELECT Date, count(*) FROM Collection_data GROUP BY Date');
    assert.equal(date[0], 'Collection_data.Collection_date');
    assert.ok(date.every(n => n.startsWith('Collection_data.')), 'no other sheet');
    // The manifest has no CAM_ID: its column holding the CAMs.
    const cam = near("SELECT count(*) FROM MEIER_manifests_23Jun26 WHERE CAM_ID GLOB 'M6[23][0-9][0-9]'");
    assert.match(cam[0], /^MEIER_manifests_23Jun26\."STS Collector Sample ID" \(holds Collection_data\.CAM_ID values: 6 of 6 looked at\)$/);
    // Nothing like it in the clutches: the sheet that has it, and the column to join them by.
    const stock = near('SELECT "CLUTCH NUMBER", "Stock_of_origin" FROM Insectary_stocks');
    assert.equal(stock[0], 'Insectary_data.Stock_of_origin (not in Insectary_stocks; shares "CLUTCH NUMBER" with Insectary_stocks)');
    // `name` is _columns' column too, as in _tables.
    assert.deepEqual(
      runQuery(db, "SELECT name FROM _columns WHERE sheet = 'Insectary_stocks' AND name LIKE 'CLUTCH%'", 5).rows,
      [['CLUTCH NUMBER']],
    );
    assert.deepEqual(near('SELECT * FROM Colection_data').slice(0, 1), ['Collection_data']);

    // The 8 Nov retro (30 "no such column" in a week). A UNION's half that lacks the column is named.
    const union = runQuery(db, "SELECT 'C', CAM_ID, Stock_of_origin FROM Collection_data UNION ALL SELECT 'I', Insectary_ID, Stock_of_origin FROM Insectary_data", 5);
    assert.deepEqual(union.near, ['Insectary_data.Stock_of_origin (not in Collection_data)']);
    assert.equal(union.tableColumns, undefined, 'the near says it all');
    // A word in common, not the table's own: Transect_ID → Transect_section; the columns listed too, the match being weak.
    const transect = runQuery(db, "SELECT count(*) FROM Collection_data WHERE Transect_ID LIKE '%T1%'", 5);
    assert.deepEqual(transect.near, ['Collection_data.Transect_section']);
    assert.match(transect.tableColumns.Collection_data, /^Data_entry_order, Release_Collect, FieldMark_ID, .*Transect_section, .*Notes_Collection_data, /);
    assert.doesNotMatch(transect.tableColumns.Collection_data, /\b_row\b|\b_id\b|_premade/, 'the columns every table has are left out');
    // Nothing close: no near, the columns of the query's table.
    const sampling = runQuery(db, "SELECT count(*) n, sum(fold(coalesce(Sampling_protocol, '')) LIKE '%monitor%') FROM Collection_data", 5);
    assert.equal(sampling.near, undefined);
    assert.deepEqual(Object.keys(sampling.tableColumns), ['Collection_data']);
    assert.match(sampling.tableColumns.Collection_data, /\bPurpose\b/);
    assert.ok(sampling.tableColumns.Collection_data.length <= 1500, String(sampling.tableColumns.Collection_data.length));
    // The very name in a sheet it joins with: the near says how to join, no list.
    const joined = runQuery(db, 'SELECT CAM_ID, Stock_of_origin FROM Collection_data', 5);
    assert.match(joined.near[0], /^Insectary_data\.Stock_of_origin \(not in Collection_data; shares /);
    assert.equal(joined.tableColumns, undefined);
    // The very name only in sheets it shares no ID with (the logged Photo_links.CAM_ID): its own columns listed, where the CAM is.
    const photos = runQuery(db, "SELECT * FROM MEIER_manifests_23Jun26 WHERE \"Tube/Well ID\" = 'x' AND Insectary_ID = 'A1A'", 5);
    assert.match(photos.tableColumns.MEIER_manifests_23Jun26, /"STS Collector Sample ID"/);
  } finally {
    db.close();
    store.close();
    rmSync(dir, { recursive: true, force: true });
  }
});

test('sqlProblem: comments and quotes are not statements', () => {
  assert.equal(sqlProblem('SELECT 1 /* ; */ -- ;\n'), null);
  assert.equal(sqlProblem('  with x as (select 1) select * from x;  '), null);
  assert.equal(sqlProblem('SELECT "a;b" FROM t'), null);
  assert.match(sqlProblem("SELECT 'a"), /not closed/);
  assert.match(sqlProblem('SELECT 1;SELECT 2'), /One statement/);
  assert.match(sqlProblem('DELETE FROM t'), /Only reading/);
  assert.match(sqlProblem(''), /required/);
});
