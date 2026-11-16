// Totals the counting tools return, so a reply reads a total instead of adding one up (16 Nov 2026: a reply
// said 41 females where the per-species counts 10, 13, 9, 12 and 4 add up to 48).
import test from 'node:test';
import assert from 'node:assert/strict';
import { DatabaseSync } from 'node:sqlite';
import { formatRows, runQuery, sumColumns, sumsText } from '../server/query-tool.mjs';
import { SUBTOTAL_VALUES, groupSubtotals } from '../server/records-tool.mjs';

const P1 = [
  ['Ithomia salapia salapia', 'female', 10],
  ['Ithomia salapia salapia', 'male', 15],
  ['Ithomia salapia salapia', null, 1],
  ['Mechanitis messenoides deceptus', 'female', 13],
  ['Mechanitis messenoides deceptus', 'male', 10],
  ['Mechanitis polymnia proceriformis', 'female', 9],
  ['Mechanitis polymnia proceriformis', 'male', 13],
  ['Mechanitis messenoides intermedia', 'female', 12],
  ['Mechanitis messenoides intermedia', 'male', 6],
  ['Mechanitis lysimnia', 'female', 4],
  ['Mechanitis lysimnia', 'male', 5],
];

function p1Db() {
  const db = new DatabaseSync(':memory:');
  db.exec('CREATE TABLE Insectary_data (SPECIES TEXT, Sex TEXT, Insectary_ID TEXT)');
  const insert = db.prepare('INSERT INTO Insectary_data VALUES (?, ?, ?)');
  let id = 0;
  for (const [species, sex, n] of P1) for (let i = 0; i < n; i++) insert.run(species, sex, `X${id++}`);
  return db;
}

test('sumColumns: COUNT, SUM and TOTAL columns and their aliases; not COUNT(DISTINCT …) nor other columns', () => {
  assert.deepEqual(sumColumns('SELECT SPECIES, Sex, COUNT(*) AS n FROM t GROUP BY 1, 2', ['SPECIES', 'Sex', 'n']), [2]);
  assert.deepEqual(sumColumns('SELECT SPECIES, count(*) FROM t GROUP BY 1', ['SPECIES', 'count(*)']), [1]);
  assert.deepEqual(sumColumns("SELECT SPECIES, SUM(Sex = 'female') hembras, SUM(CASE WHEN Sex = 'male' THEN 1 ELSE 0 END) AS \"machos\" FROM t GROUP BY 1", ['SPECIES', 'hembras', 'machos']), [1, 2]);
  assert.deepEqual(sumColumns('SELECT Sex, COUNT(DISTINCT SPECIES) AS species FROM t GROUP BY 1', ['Sex', 'species']), []);
  assert.deepEqual(sumColumns('SELECT Sex, COUNT(DISTINCT SPECIES) FROM t GROUP BY 1', ['Sex', 'COUNT(DISTINCT SPECIES)']), []);
  assert.deepEqual(sumColumns('SELECT Sex, MAX(days) AS longest, AVG(days) AS mean FROM t GROUP BY 1', ['Sex', 'longest', 'mean']), []);
  // The word after COUNT(…) that is no alias (FROM, a division) names no column.
  assert.deepEqual(sumColumns('SELECT ROUND(100.0 * COUNT(*) / 98, 1) AS pct FROM t', ['pct']), []);
  assert.deepEqual(sumColumns('SELECT COUNT(*) FROM t', ['COUNT(*)']), [0]);
});

test('query: the P1 table by species and sex comes with the totals per sex and per species', () => {
  const db = p1Db();
  const out = runQuery(db, 'SELECT SPECIES, Sex, COUNT(*) AS n FROM Insectary_data GROUP BY SPECIES, Sex', 200);
  assert.equal(out.total, 11);
  assert.deepEqual(out.sums.totals, [98]);
  const bySex = Object.fromEntries(out.sums.by.find(b => b.column === 'Sex').values.map(([v, [n]]) => [v, n]));
  assert.deepEqual(bySex, { female: 48, male: 49, null: 1 });
  const bySpecies = Object.fromEntries(out.sums.by.find(b => b.column === 'SPECIES').values.map(([v, [n]]) => [v, n]));
  assert.equal(bySpecies['Ithomia salapia salapia'], 26);
  assert.equal(bySpecies['Mechanitis lysimnia'], 9);
  const text = formatRows(out);
  assert.match(text, /\(Totals over all 11 rows: n 98\.\)/);
  assert.match(text, /\(n by Sex: male 49, female 48, \(empty\) 1\.\)/);
  assert.match(text, /n by SPECIES: Ithomia salapia salapia 26, Mechanitis messenoides deceptus 23, /);
  db.close();
});

test('query: a pivot (a column per sex) gets the column totals, not a breakdown by its unique rows', () => {
  const db = p1Db();
  const out = runQuery(db, "SELECT SPECIES, SUM(Sex = 'female') AS hembras, SUM(Sex = 'male') AS machos FROM Insectary_data GROUP BY SPECIES", 200);
  assert.deepEqual(out.sums.columns, ['hembras', 'machos']);
  assert.deepEqual(out.sums.totals, [48, 49]);
  assert.deepEqual(out.sums.by, []);
  assert.deepEqual(sumsText(out.sums), ['(Totals over all 5 rows: hembras 48, machos 49.)']);
  db.close();
});

test('query: totals count every row, also those past the limit or cut for size; none for one row or no count column', () => {
  const db = p1Db();
  const limited = runQuery(db, 'SELECT SPECIES, Sex, COUNT(*) AS n FROM Insectary_data GROUP BY SPECIES, Sex ORDER BY n DESC', 3);
  assert.equal(limited.rows.length, 3);
  assert.deepEqual(limited.sums.totals, [98]);
  assert.match(formatRows(limited, { limit: 3 }), /Totals over all 11 rows: n 98/);
  // A tiny budget keeps the totals and drops rows.
  const tight = formatRows(runQuery(db, 'SELECT SPECIES, Sex, COUNT(*) AS n FROM Insectary_data GROUP BY SPECIES, Sex', 200), { budget: 600 });
  assert.match(tight, /Totals over all 11 rows: n 98/);
  assert.match(tight, /rows shown: size limit/);
  assert.equal(runQuery(db, 'SELECT COUNT(*) AS n FROM Insectary_data', 5).sums, undefined);
  assert.equal(runQuery(db, 'SELECT SPECIES, Sex FROM Insectary_data', 5).sums, undefined);
  // A column that is not whole numbers is not summed.
  assert.equal(runQuery(db, 'SELECT Sex, SUM(0.5) AS half FROM Insectary_data GROUP BY Sex', 5).sums, undefined);
  db.close();
});

test('count_records subtotals: the rows per value of each groupBy column, over all groups', () => {
  const groups = P1.map(([s, x, n]) => ({ values: [s, x ?? '(empty)'], n }));
  const sub = groupSubtotals(['SPECIES', 'Sex'], groups);
  assert.deepEqual(sub.Sex, { male: 49, female: 48, '(empty)': 1 });
  assert.equal(sub.SPECIES['Mechanitis messenoides deceptus'], 23);
  assert.deepEqual(Object.keys(sub.Sex), ['male', 'female', '(empty)'], 'most first');
  assert.equal(groupSubtotals(['SPECIES'], groups), null, 'one column: the groups are its totals');
  const many = Array.from({ length: SUBTOTAL_VALUES + 1 }, (_, i) => ({ values: [`sp${i}`, i % 2 ? 'male' : 'female'], n: 1 }));
  const cut = groupSubtotals(['SPECIES', 'Sex'], many);
  assert.equal(cut.SPECIES, undefined);
  assert.deepEqual(cut.Sex, { female: 31, male: 30 });
  assert.match(cut._omitted, /SPECIES: over 60 values/);
});

test('the shared brief (app chat and every WhatsApp voice) says figures are copied and totals come from a tool', async () => {
  const { readdirSync } = await import('node:fs');
  const { join } = await import('node:path');
  const { composeBrief, GENERIC_PERSON, SERVER_PATHS } = await import('../server/brief.mjs');
  const { RECORD_TOOLS } = await import('../server/records-tool.mjs');
  const { QUERY_TOOL } = await import('../server/query-tool.mjs');
  const root = join(import.meta.dirname, '..');
  const wa = join(root, 'assistant/voice/whatsapp-d3l');
  const voices = [null, readdirSync(wa).filter(f => f.endsWith('.md')).map(f => join(wa, f))];
  for (const voice of voices) {
    const brief = composeBrief(GENERIC_PERSON, { ...SERVER_PATHS, voice }).replace(/\s+/g, ' ');
    assert.match(brief, /never add, subtract or work out a share in your head/, voice ? 'WhatsApp' : 'app');
    assert.match(brief, /`count_records` `subtotals`, the totals `query` prints for COUNT and SUM columns/);
    assert.match(brief, /Only a tool's total may come before the list it sums/);
  }
  // The names the rule cites are the tools'.
  assert.match(RECORD_TOOLS.find(t => t.function.name === 'count_records').function.description, /`subtotals`/);
  assert.match(QUERY_TOOL.function.description, /COUNT and SUM columns come with their totals/);
});
