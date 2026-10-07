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
import { sqlProblem } from '../server/query-tool.mjs';
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

async function setup(dir) {
  const sheets = new LocalSheets(structuredClone(SEED));
  formula(sheets, 'Insectary_data', 4, 'Insectary_ID', '=CONCATENATE("A",3,"A")', 'A3A');
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

test('query: one read-only statement, capped, stopped after the time limit, with fold and km', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'sheets-query-'));
  const { store, at } = await setup(dir);
  const assistant = createAssistant({
    store,
    config: { sheetsCopyPath: join(dir, 'sheets.sqlite'), sheetsCopy: { startMs: 0, delayMs: 50, log: { log() {}, error: console.error } }, sheetsQuery: { timeoutMs: 600 } },
  });
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
    assert.match(lines.at(-1), /^\(Copy of \d{4}-\d\d-\d\d \d\d:\d\d UTC, just now\.\)$/);
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

    // The rows shown are capped; the total says how many there are.
    const many = (await call('WITH RECURSIVE c(x) AS (SELECT 1 UNION ALL SELECT x + 1 FROM c WHERE x < 1000) SELECT x FROM c', 10)).split('\n');
    assert.equal(many.length, 1 + 10 + 2);
    assert.match(many[11], /^\(10 of 1000 rows shown; limit goes up to 500\./);

    // A runaway query is stopped; the next one runs in a new process.
    const started = Date.now();
    const runaway = await call('WITH RECURSIVE c(x) AS (SELECT 1 UNION ALL SELECT x + 1 FROM c) SELECT count(*) FROM c');
    assert.match(runaway.error, /ran over 0\.6 s and was stopped/);
    assert.ok(Date.now() - started < 5000);
    assert.equal((await call('SELECT count(*) FROM Insectary_data')).split('\n')[1], '2');

    // Helpers: fold for matching without accents or case, km between two points.
    const helpers = (await call("SELECT fold('Ñandú ÁBC') f, round(km(0, 0, 0, 1), 1) k, km('NA', 0, 0, 1) n")).split('\n');
    assert.equal(helpers[1], 'nandu abc\t111.2\t');

    // The copy follows the app's saves.
    await store.updateRecord(at(3).id, { values: { Sex: 'female' }, requestId: randomUUID() }, user);
    await new Promise(resolve => setImmediate(resolve));
    assert.match(await call('SELECT 1'), /saves since then arrive within a minute/);
    await until(async () => (await call("SELECT Sex FROM Insectary_data WHERE Insectary_ID = 'A2A'")).split('\n')[1] === 'female');
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

test('sqlProblem: comments and quotes are not statements', () => {
  assert.equal(sqlProblem('SELECT 1 /* ; */ -- ;\n'), null);
  assert.equal(sqlProblem('  with x as (select 1) select * from x;  '), null);
  assert.equal(sqlProblem('SELECT "a;b" FROM t'), null);
  assert.match(sqlProblem("SELECT 'a"), /not closed/);
  assert.match(sqlProblem('SELECT 1;SELECT 2'), /One statement/);
  assert.match(sqlProblem('DELETE FROM t'), /Only reading/);
  assert.match(sqlProblem(''), /required/);
});
