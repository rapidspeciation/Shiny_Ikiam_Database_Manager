// server/sheet-ranges.mjs: the workbook's fixed end rows kept ahead of its rows (the nightly's `ranges` job). Fake rules
// in a fake workbook that answers the same Sheets API requests; nothing here reaches Google.
import test from 'node:test';
import assert from 'node:assert/strict';
import { execFile } from 'node:child_process';
import { mkdtempSync, readFileSync, existsSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { conditionalFormatPlan, listSource, listTarget, validationPlan, validationCells, keepRangesAhead, leftOutReport, blockingProtections } from '../server/sheet-ranges.mjs';
import { alertList, leftOutRanges } from '../server/alerts.mjs';
import { sheetRangesRun } from '../scripts/nightly-app-checks.mjs';

const red = { backgroundColor: { red: 0.96, green: 0.8, blue: 0.8 }, backgroundColorStyle: { rgbColor: { red: 0.96, green: 0.8, blue: 0.8 } } };
const range = (sheetId, r0, r1, c0, c1) => ({ sheetId, startRowIndex: r0, endRowIndex: r1, startColumnIndex: c0, endColumnIndex: c1 });
const formula = (ranges, value, format = red) => ({ ranges, booleanRule: { condition: { type: 'CUSTOM_FORMULA', values: [{ userEnteredValue: value }] }, format } });
const text = (ranges, value) => ({ ranges, booleanRule: { condition: { type: 'TEXT_CONTAINS', values: [{ userEnteredValue: value }] }, format: red } });
const list = (value, extra = {}) => ({ condition: { type: 'ONE_OF_RANGE', values: [{ userEnteredValue: value }] }, strict: true, showCustomUi: true, ...extra });

// The repeated-ID rule as it was on 11 Nov 2026 (Insectary_data's grid and the rule both ended at row 20846), beside
// rules of other kinds that must never change.
const insectary = (grid = 20846, end = 20846) => ({
  properties: { sheetId: 7, title: 'Insectary_data', gridProperties: { rowCount: grid } },
  conditionalFormats: [
    formula([range(7, 0, grid, 15, 16)], '=SI(P:P="NA","",CONTAR.SI(P:P,P1)>1)'),
    text([range(7, 0, grid, 36, 40)], 'Not in STS'),
    formula([range(7, 1, grid, 0, 1)], `=Y(A2<>"", CONTAR.SI($A$2:$A$${end}, A2) > 1)`),
  ],
});

test('a conditional format: its formula end moves when the grid comes within 1,000 rows, its covered range is left to Google', () => {
  const { changes } = conditionalFormatPlan([insectary()]);
  assert.equal(changes.length, 1);
  const [c] = changes;
  assert.equal(c.index, 2);
  assert.equal(c.after, '=Y(A2<>"", CONTAR.SI($A$2:$A$30846, A2) > 1)');
  // Only the end differs: the same rule with the formula's number changed.
  const back = structuredClone(c.next);
  back.booleanRule.condition.values[0].userEnteredValue = c.before;
  assert.deepEqual(back, c.rule);
  // After the 11 Nov edit: 10,000 ahead, nothing to do until the grid reaches 29,846 rows.
  assert.equal(conditionalFormatPlan([insectary(20846, 30846)]).changes.length, 0);
  assert.equal(conditionalFormatPlan([insectary(29845, 30846)]).changes.length, 0);
  const near = conditionalFormatPlan([insectary(29846, 30846)]).changes;
  assert.equal(near[0].after, '=Y(A2<>"", CONTAR.SI($A$2:$A$39846, A2) > 1)');
  // Rows appended past the end since the last night (up to 1,000): still moved.
  assert.equal(conditionalFormatPlan([insectary(31846, 30846)]).changes[0].after, '=Y(A2<>"", CONTAR.SI($A$2:$A$41846, A2) > 1)');
  // Further behind: taken as meant (a short block), named, not moved.
  const short = conditionalFormatPlan([insectary(31847, 30846)]);
  assert.equal(short.changes.length, 0);
  assert.deepEqual(short.behind, [{ sheet: 'Insectary_data', index: 2, end: 30846, grid: 31847 }]);
});

test('a conditional format of another kind is never touched', () => {
  const sheet = (rules, grid = 100) => [{ properties: { sheetId: 1, title: 'S', gridProperties: { rowCount: grid } }, conditionalFormats: rules }];
  const r = [range(1, 1, 100, 0, 1)];
  for (const value of [
    '=CONTAR.SI(G:G, G1)>1', // whole columns
    '=COUNTIF(INDIRECTO("A2:A100"), A2)>1', // computed ranges
    '=Y(A2<>"", CONTAR.SI(DESREF($A$2,0,0,99), A2) > 1)',
    '=CONTAR.SI(Lists!$A$2:$A$100, A2)>0', // another sheet's range
    '=CONTAR.SI($A2:$A100, A2)>1', // relative rows: they move with the cell
    '=CONTAR.SI($A$2:$B$100, A2)>1', // two columns
  ])
    assert.equal(conditionalFormatPlan(sheet([formula(r, value)])).changes.length, 0, value);
  assert.equal(conditionalFormatPlan(sheet([text(r, 'Not in TOL704')])).changes.length, 0);
  assert.equal(conditionalFormatPlan(sheet([{ ranges: r, gradientRule: { minpoint: {}, maxpoint: {} } }])).changes.length, 0);
  // Two fixed ranges: each end moved on its own.
  const both = conditionalFormatPlan(sheet([formula(r, '=CONTAR.SI($A$2:$A$100, A2)+CONTAR.SI($B$2:$B$5000, A2)>1')])).changes;
  assert.equal(both[0].after, '=CONTAR.SI($A$2:$A$10100, A2)+CONTAR.SI($B$2:$B$5000, A2)>1');
  // A short lookup block far above the grid's end is not a growing range.
  assert.equal(conditionalFormatPlan(sheet([formula(r, '=COINCIDIR(A2,$K$2:$K$12,0)')], 5000)).changes.length, 0);
});

test('a dropdown list: parsed, and where it should end', () => {
  assert.deepEqual(listSource('=Lists!$Z$2:$Z$3030'), { sheet: 'Lists', column: 'Z', start: 2, end: 3030, key: 'Lists!Z' });
  assert.deepEqual(listSource("='Bob''s list'!$A$3:$A$634"), { sheet: "Bob's list", column: 'A', start: 3, end: 634, key: "Bob's list!A" });
  assert.equal(listSource('=Lists!$Z$2:$AA$3030'), null);
  assert.equal(listSource('=Lists!Z:Z'), null);
  assert.equal(listSource('=Lists!$Z$2:$Z$3030+1'), null);
  // Lists!Z had 3,775 rows: from 3030 to 13775. Far from the end: nothing. Past the list sheet's grid is fine (Google
  // keeps it as sent): Insectary_stocks!A, 1,122 rows on a 1,999-row grid.
  assert.equal(listTarget(3030, { last: 3775, grid: 50500 }), 13775);
  assert.equal(listTarget(13775, { last: 3775, grid: 50500 }), null);
  assert.equal(listTarget(634, { last: 1122, grid: 1999 }), 11122);
  assert.equal(listTarget(1999, { last: 1122, grid: 1999 }), 11122);
  assert.equal(listTarget(11122, { last: 1122, grid: 1999 }), null);
  assert.equal(listTarget(100, undefined), null);
});

test('dropdowns: only those on a growing list, one request per run of the same rule, the way back exact', () => {
  const data = [
    {
      startRow: 1,
      startColumn: 4,
      rowData: [
        { values: [{ dataValidation: list('=Lists!$Z$2:$Z$3030') }, { dataValidation: list('=Lists!$G$2:$G$50') }] },
        { values: [{ dataValidation: list('=Lists!$Z$2:$Z$3030') }, { dataValidation: list('=Lists!$G$2:$G$50') }] },
        { values: [{ dataValidation: list('=Lists!$Z$2:$Z$3505', { showCustomUi: false }) }, {}] },
        { values: [{ dataValidation: list('=Lists!$Z$2:$Z$3030') }] },
        { values: [{}] },
        { values: [{ dataValidation: list('=Lists!$Z$2:$Z$3030') }] },
      ],
    },
  ];
  const cells = validationCells('Collection_data', 3, data);
  assert.equal(cells.length, 7);
  const lists = new Map([
    ['Lists!Z', { last: 3775, grid: 50500 }],
    ['Lists!G', { last: 27, grid: 50500 }],
  ]);
  const plan = validationPlan(cells, lists);
  // Lists!G (a short list of choices, maybe on purpose a part of it) is not a growing list: untouched.
  assert.deepEqual(
    plan.map(p => [p.cells, p.before, p.after]),
    [
      ['E2:E3', '=Lists!$Z$2:$Z$3030', '=Lists!$Z$2:$Z$13775'],
      ['E4:E4', '=Lists!$Z$2:$Z$3505', '=Lists!$Z$2:$Z$13775'],
      ['E5:E5', '=Lists!$Z$2:$Z$3030', '=Lists!$Z$2:$Z$13775'],
      ['E7:E7', '=Lists!$Z$2:$Z$3030', '=Lists!$Z$2:$Z$13775'],
    ],
  );
  assert.deepEqual(plan[0].request.setDataValidation.range, range(3, 1, 3, 4, 5));
  assert.equal(plan[1].request.setDataValidation.rule.showCustomUi, false);
  assert.deepEqual(plan[1].inverse.setDataValidation.rule, list('=Lists!$Z$2:$Z$3505', { showCustomUi: false }));
  assert.equal(validationPlan(cells, new Map([['Lists!Z', { last: 100, grid: 50500 }]])).length, 0);
});

// A1 notation of the requests keepRangesAhead sends: 'Sheet'!2:2, 'Sheet'!E:E, 'Sheet'!E2:E9.
const colIndex = letters => [...letters].reduce((n, ch) => n * 26 + ch.charCodeAt(0) - 64, 0) - 1;
function parseA1(a1) {
  const m = /^'((?:[^']|'')+)'!(.+)$/.exec(a1);
  const title = m[1].replaceAll("''", "'");
  let r;
  if ((r = /^(\d+):(\d+)$/.exec(m[2]))) return { title, r0: Number(r[1]) - 1, r1: Number(r[2]), c0: 0, c1: Infinity };
  if ((r = /^([A-Z]+):([A-Z]+)$/.exec(m[2]))) return { title, r0: 0, r1: Infinity, c0: colIndex(r[1]), c1: colIndex(r[2]) + 1 };
  r = /^([A-Z]+)(\d+):([A-Z]+)(\d+)$/.exec(m[2]);
  return { title, r0: Number(r[2]) - 1, r1: Number(r[4]), c0: colIndex(r[1]), c1: colIndex(r[3]) + 1 };
}

/** A workbook in memory answering keepRangesAhead's requests as the Sheets API does (ranges stopped at the grid). */
function fakeWorkbook(sheets, { protectedSheets = [], ignoreWrites = false } = {}) {
  const calls = [];
  const byId = id => sheets.find(s => s.properties.sheetId === id);
  const byTitle = t => sheets.find(s => s.properties.title === t);
  const request = async (path, options = {}) => {
    calls.push({ path, method: options.method || 'GET' });
    if (path === ':batchUpdate') {
      const { requests } = JSON.parse(options.body);
      requests.forEach((r, i) => {
        const sheet = byId((r.updateConditionalFormatRule || r.setDataValidation.range).sheetId);
        if (r.setDataValidation && protectedSheets.includes(sheet.properties.title))
          throw new Error(`Google Sheets 400: {"error":{"message":"Invalid requests[${i}].setDataValidation: You are trying to edit a protected cell or object."}}`);
      });
      if (ignoreWrites) return { replies: [] };
      for (const r of requests) {
        if (r.updateConditionalFormatRule) {
          const { sheetId, index, rule } = r.updateConditionalFormatRule;
          const sheet = byId(sheetId);
          const kept = structuredClone(rule);
          for (const g of kept.ranges) g.endRowIndex = Math.min(g.endRowIndex, sheet.properties.gridProperties.rowCount);
          sheet.conditionalFormats[index] = kept;
        } else {
          const { range: g, rule } = r.setDataValidation;
          const sheet = byId(g.sheetId);
          for (let row = g.startRowIndex; row < g.endRowIndex; row++) sheet.dv.set(`${row}:${g.startColumnIndex}`, structuredClone(rule));
        }
      }
      return { replies: requests.map(() => ({})) };
    }
    const url = new URL(`https://x/${path.replace(/^\?/, 'meta?')}`);
    if (path.startsWith('/values:batchGet')) {
      return {
        valueRanges: url.searchParams.getAll('ranges').map(a1 => {
          const { title, c0 } = parseA1(a1);
          return { values: (byTitle(title).values?.[c0] || []).map(v => (v === '' ? [] : [v])) };
        }),
      };
    }
    const ranges = url.searchParams.getAll('ranges');
    if (!ranges.length)
      return { sheets: structuredClone(sheets.map(({ properties, conditionalFormats, protectedRanges }) => ({ properties, conditionalFormats, ...(protectedRanges ? { protectedRanges } : {}) }))) };
    const out = new Map();
    for (const a1 of ranges) {
      const { title, r0, r1, c0, c1 } = parseA1(a1);
      const sheet = byTitle(title);
      const rows = Math.min(r1, sheet.properties.gridProperties.rowCount);
      const rowData = [];
      for (let row = r0; row < rows; row++) {
        const values = [];
        const width = Math.min(c1, 60);
        for (let c = c0; c < width; c++) {
          const dv = sheet.dv.get(`${row}:${c}`);
          values.push(dv ? { dataValidation: structuredClone(dv) } : {});
        }
        rowData.push({ values });
      }
      if (!out.has(title)) out.set(title, { properties: { sheetId: sheet.properties.sheetId, title }, data: [] });
      out.get(title).data.push({ startRow: r0, startColumn: c0, rowData });
    }
    return { sheets: [...out.values()] };
  };
  return { request, calls };
}

const column = (n, fill = 'x') => Array.from({ length: n }, () => fill);
function workbook({ grid = 20846, end = 20846, zEnd = 3030, zLast = 3775 } = {}) {
  const dv = new Map();
  for (let row = 1; row < 50; row++) dv.set(`${row}:4`, list(`=Lists!$Z$2:$Z$${zEnd}`));
  for (let row = 1; row < 50; row++) dv.set(`${row}:7`, list('=Lists!$G$2:$G$50'));
  const wing = new Map();
  for (let row = 1; row < 20; row++) wing.set(`${row}:0`, list('=Lists!$T$2:$T$3507'));
  return [
    { ...insectary(grid, end), dv: new Map() },
    { properties: { sheetId: 3, title: 'Collection_data', gridProperties: { rowCount: 50 } }, conditionalFormats: [text([range(3, 0, 50, 50, 54)], 'Not in TOL704')], dv },
    { properties: { sheetId: 9, title: 'Barcoding_DNA', gridProperties: { rowCount: 20 } }, conditionalFormats: [], dv: wing },
    {
      properties: { sheetId: 5, title: 'Lists', gridProperties: { rowCount: 50500 } },
      conditionalFormats: [],
      dv: new Map(),
      values: { 6: column(27), 19: column(7732), 25: column(zLast) },
    },
  ];
}

test('the nightly pass: moves what came near, reads it back, keeps the rules as they were; protected ranges left to their owner', async () => {
  const sheets = workbook();
  const before = structuredClone(sheets.map(s => s.conditionalFormats));
  const fake = fakeWorkbook(sheets, { protectedSheets: ['Barcoding_DNA'] });
  const out = await keepRangesAhead(fake.request, { at: Date.parse('2026-11-11T06:30:00Z') });
  assert.equal(out.code, 3, out.lines.join('\n'));
  assert.deepEqual(out.lines, [
    'Sheet ranges: WARNING: dropdown Barcoding_DNA A2:A20 is protected from the app: its owner moves =Lists!$T$2:$T$3507 to =Lists!$T$2:$T$17732 by hand',
    'Sheet ranges: moved conditional format Insectary_data rule 3: =Y(A2<>"", CONTAR.SI($A$2:$A$20846, A2) > 1) → =Y(A2<>"", CONTAR.SI($A$2:$A$30846, A2) > 1) (the grid has 20846 rows)',
    'Sheet ranges: moved dropdown Collection_data E2:E50: =Lists!$Z$2:$Z$3030 → =Lists!$Z$2:$Z$13775 (Lists!Z has 3775 rows of 50500)',
  ]);
  // Only the rule's formula changed (its covered range stopped at the grid by Google); the other rules as they were.
  const now = sheets.map(s => s.conditionalFormats);
  const expected = structuredClone(before);
  expected[0][2].booleanRule.condition.values[0].userEnteredValue = '=Y(A2<>"", CONTAR.SI($A$2:$A$30846, A2) > 1)';
  assert.deepEqual(now, expected);
  assert.deepEqual(sheets[1].dv.get('1:7'), list('=Lists!$G$2:$G$50'));
  assert.deepEqual(sheets[1].dv.get('49:4'), list('=Lists!$Z$2:$Z$13775'));
  // The record puts everything back.
  assert.equal(out.record.changes.length, 2);
  assert.equal(out.record.leftOut.length, 1);
  assert.equal(out.record.at, '2026-11-11T06:30:00.000Z');
  await fake.request(':batchUpdate', { method: 'POST', body: JSON.stringify({ requests: out.record.changes.map(c => c.inverse) }) });
  assert.deepEqual(sheets.map(s => s.conditionalFormats), before);
  assert.deepEqual(sheets[1].dv.get('49:4'), list('=Lists!$Z$2:$Z$3030'));
  // Writes: one batch refused (protected), one sent.
  assert.equal(fake.calls.filter(c => c.method === 'POST').length, 3);
});

test('the nightly pass with nothing near: no write, one line', async () => {
  const sheets = workbook({ end: 30846, zEnd: 13775 });
  sheets[2].dv = new Map();
  const fake = fakeWorkbook(sheets);
  const out = await keepRangesAhead(fake.request);
  assert.equal(out.code, 0);
  assert.deepEqual(out.lines, ['Sheet ranges: ok, nothing within 1000 rows of its end (nearest: Insectary_data rule 3 counts to row 30846, the grid has 20846)']);
  assert.equal(fake.calls.filter(c => c.method === 'POST').length, 0);
  assert.deepEqual(out.record.changes, []);
});

test('a rule that does not read back as sent fails the job', async () => {
  const sheets = workbook({ zEnd: 13775 });
  sheets[2].dv = new Map();
  const out = await keepRangesAhead(fakeWorkbook(sheets, { ignoreWrites: true }).request);
  assert.equal(out.code, 1);
  assert.match(out.lines.at(-1), /FAILING: Insectary_data's conditional formats read back other than sent/);
});

test('--sheet-ranges: the log gets the plan before a write and the outcome after, nothing when nothing moves; an error is a failing line', async t => {
  const dir = mkdtempSync(join(tmpdir(), 'sheet-ranges-'));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  const log = join(dir, 'sheet-ranges.jsonl');
  const quiet = workbook({ end: 30846, zEnd: 13775 });
  quiet[2].dv = new Map();
  assert.equal((await sheetRangesRun(fakeWorkbook(quiet).request, log)).code, 0);
  assert.equal(existsSync(log), false);
  const near = workbook();
  near[2].dv = new Map();
  const out = await sheetRangesRun(fakeWorkbook(near).request, log);
  assert.equal(out.code, 0);
  const lines = readFileSync(log, 'utf8').trim().split('\n').map(line => JSON.parse(line));
  // The plan (with the rules as they were) before the write, the outcome after it.
  assert.deepEqual(lines.map(l => [l.status, l.changes.length]), [['planned', 2], ['sent', 2]]);
  assert.equal(lines[0].conditionalFormatsAsRead.Insectary_data.length, 3);
  const broken = await sheetRangesRun(async () => {
    throw new Error('Google Sheets 503: busy');
  }, log);
  assert.deepEqual(broken, { code: 1, lines: ['Sheet ranges: FAILING: Google Sheets 503: busy'] });
  // No log file: nothing sent.
  const fake = fakeWorkbook(workbook());
  const unlogged = await sheetRangesRun(fake.request, null);
  assert.deepEqual(unlogged.lines, ['Sheet ranges: FAILING: no --log file: nothing sent']);
  assert.equal(fake.calls.filter(c => c.method === 'POST').length, 0);
});

test('a write that may have landed keeps its way back: write not confirmed, or the read-back failing', async t => {
  const dir = mkdtempSync(join(tmpdir(), 'sheet-ranges-'));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  const log = join(dir, 'sheet-ranges.jsonl');
  const sheets = workbook({ zEnd: 13775 });
  sheets[2].dv = new Map();
  const fake = fakeWorkbook(sheets);
  // The write lands, then Google stops answering.
  let posted = false;
  const flaky = async (path, options = {}) => {
    if (posted) throw new Error('fetch failed');
    const out = await fake.request(path, options);
    if (options.method === 'POST') posted = true;
    return out;
  };
  const late = await sheetRangesRun(flaky, log);
  assert.equal(late.code, 1);
  assert.match(late.lines.at(-1), /FAILING: the read-back failed \(fetch failed\)/);
  // A write that timed out (it may have landed), on a workbook not yet moved.
  const fresh = workbook({ zEnd: 13775 });
  fresh[2].dv = new Map();
  const again = fakeWorkbook(fresh);
  const timeout = await sheetRangesRun(async (path, options = {}) => {
    if (options.method === 'POST') throw Object.assign(new Error('Google Sheets did not answer in 120 s'), { timeout: true });
    return again.request(path, options);
  }, log);
  assert.equal(timeout.code, 1);
  assert.match(timeout.lines.at(-1), /FAILING: the write did not confirm/);
  const records = readFileSync(log, 'utf8').trim().split('\n').map(line => JSON.parse(line));
  assert.deepEqual(records.map(r => r.status), ['planned', 'sent', 'planned', 'unconfirmed']);
  for (const r of records) assert.ok((r.changes.length || r.unconfirmed.length) && (r.changes[0] || r.unconfirmed[0]).inverse);
});

test('a dropdown block further down a column is found though the sampled rows are already ahead', async () => {
  const sheets = workbook({ end: 30846, zEnd: 13775 });
  sheets[2].dv = new Map();
  for (let row = 20; row < 25; row++) sheets[1].dv.set(`${row}:4`, list('=Lists!$Z$2:$Z$3030'));
  // And one on another growing list that no sampled row names.
  sheets[1].dv.set('30:4', list('=Lists!$T$2:$T$3507'));
  const out = await keepRangesAhead(fakeWorkbook(sheets).request);
  assert.equal(out.code, 0, out.lines.join('\n'));
  assert.deepEqual(out.lines, [
    'Sheet ranges: moved dropdown Collection_data E21:E25: =Lists!$Z$2:$Z$3030 → =Lists!$Z$2:$Z$13775 (Lists!Z has 3775 rows of 50500)',
    'Sheet ranges: moved dropdown Collection_data E31:E31: =Lists!$T$2:$T$3507 → =Lists!$T$2:$T$17732 (Lists!T has 7732 rows of 50500)',
  ]);
  assert.deepEqual(sheets[1].dv.get('22:4'), list('=Lists!$Z$2:$Z$13775'));
});

test('the command without a credential: exit 1, nothing sent', async () => {
  const script = fileURLToPath(new URL('../scripts/nightly-app-checks.mjs', import.meta.url));
  const env = { PATH: '/usr/bin:/bin', HOME: tmpdir(), NODE_OPTIONS: '' };
  const { code, stdout } = await new Promise(resolve =>
    execFile(process.execPath, [script, '--sheet-ranges'], { env }, (error, out) => resolve({ code: error ? error.code : 0, stdout: out })),
  );
  assert.equal(code, 1);
  assert.match(stdout, /^Sheet ranges: FAILING: GOOGLE_CREDENTIALS_FILE is required/);
});

// Barcoding_DNA A2:A226 on 11 Nov 2026: protected from the app's account, its list Lists!T with 7,732 rows.
const barcoding = (rows = 7732, protections = ['42:0-226:0-1'], label = 'Barcoding_DNA A2:A226') => ({
  kind: 'dropdown',
  key: 'dropdown:Barcoding_DNA:A2',
  sheet: 'Barcoding_DNA',
  label,
  before: '=Lists!$T$2:$T$3507',
  after: `=Lists!$T$2:$T$${rows + 10000}`,
  rows,
  protections,
});

test('a protected range is told once, then again only when its situation changes', () => {
  const first = leftOutReport([barcoding()], {}, { at: '2026-11-12T06:30:00.000Z' });
  assert.equal(first.warn, true);
  assert.deepEqual(first.lines, [
    'Sheet ranges: WARNING: dropdown Barcoding_DNA A2:A226 is protected from the app: its owner moves =Lists!$T$2:$T$3507 to =Lists!$T$2:$T$17732 by hand',
  ]);
  // The nights after: the same, the list 999 rows longer, or the run grown with new rows: one quiet line, no warning.
  for (const c of [barcoding(), barcoding(8731), barcoding(7732, undefined, 'Barcoding_DNA A2:A240')]) {
    const again = leftOutReport([c], first.state, { at: '2026-11-13T06:30:00.000Z' });
    assert.equal(again.warn, false);
    assert.deepEqual(again.lines, ['Sheet ranges: 1 known range still left for its owner (told before: sheet-ranges-state.json)']);
    assert.equal(again.state['dropdown:Barcoding_DNA:A2'].since, '2026-11-12T06:30:00.000Z');
  }
  // Told again: the list 1,000 rows longer than when told, another or a moved protection, or another rule.
  for (const c of [barcoding(8732), barcoding(7732, ['42:0-226:0-1', '43:-:-']), barcoding(7732, ['42:0-300:0-1']), { ...barcoding(), before: '=Lists!$T$2:$T$5000' }]) {
    const changed = leftOutReport([c], first.state, { at: '2026-11-20T06:30:00.000Z' });
    assert.equal(changed.warn, true);
    assert.match(changed.lines[0], /WARNING: .* by hand \(known since 2026-11-12; changed since told on 2026-11-12\)$/);
  }
  // Moved by its owner, or writable again (the nightly moved it): told once and forgotten.
  const gone = leftOutReport([], first.state, { at: '2026-11-21T06:30:00.000Z' });
  assert.deepEqual(gone, {
    state: {},
    lines: ['Sheet ranges: dropdown Barcoding_DNA A2:A226 is no longer left out (moved, changed by hand, or writable again)'],
    warn: false,
  });
});

test('the protections that keep the app from a change', () => {
  const change = { request: { setDataValidation: { range: range(9, 1, 226, 0, 1), rule: {} } } };
  const sheet = protectedRanges => ({ protectedRanges });
  // Google leaves requestingUserCanEdit out when it is false.
  assert.deepEqual(blockingProtections(sheet([{ protectedRangeId: 42, range: range(9, 0, 226, 0, 2) }]), change), ['42:0-226:0-2']);
  assert.deepEqual(blockingProtections(sheet([{ protectedRangeId: 42, range: range(9, 0, 226, 0, 2), requestingUserCanEdit: false }]), change), ['42:0-226:0-2']);
  // A whole sheet protected (a range with only its sheet).
  assert.deepEqual(blockingProtections(sheet([{ protectedRangeId: 7, range: { sheetId: 9 } }]), change), ['7:-:-']);
  assert.deepEqual(blockingProtections(sheet([{ protectedRangeId: 42, range: range(9, 0, 226, 0, 2), requestingUserCanEdit: true }]), change), []);
  assert.deepEqual(blockingProtections(sheet([{ protectedRangeId: 42, range: range(9, 0, 226, 0, 2), warningOnly: true }]), change), []);
  assert.deepEqual(blockingProtections(sheet([{ protectedRangeId: 42, range: range(9, 0, 226, 3, 4) }]), change), []);
  // A protection with exceptions: none only when one of them holds all the change's cells.
  const withException = { protectedRangeId: 7, range: { sheetId: 9 }, unprotectedRanges: [range(9, 0, 226, 0, 1)] };
  assert.deepEqual(blockingProtections(sheet([withException]), change), []);
  const otherColumnFree = { protectedRangeId: 7, range: { sheetId: 9 }, unprotectedRanges: [range(9, 0, 226, 1, 2)] };
  assert.deepEqual(blockingProtections(sheet([otherColumnFree]), change), ['7:-:-']);
  const partlyFree = { protectedRangeId: 7, range: { sheetId: 9 }, unprotectedRanges: [range(9, 0, 100, 0, 1)] };
  assert.deepEqual(blockingProtections(sheet([partlyFree]), change), ['7:-:-']);
  assert.deepEqual(blockingProtections(undefined, change), []);
  // A conditional format: its covered ranges.
  const cf = { request: { updateConditionalFormatRule: { rule: { ranges: [range(9, 1, 226, 5, 6)] } } } };
  assert.deepEqual(blockingProtections(sheet([{ protectedRangeId: 3, range: range(9, 0, 10, 5, 6) }]), cf), ['3:0-10:5-6']);
});

const toEnd = (dv, end) => ({ ...dv, condition: { ...dv.condition, values: [{ userEnteredValue: `=Lists!$T$2:$T$${end}` }] } });

test('nights with a protected dropdown: never sent, told the first night only, kept in the state file the alerts read', async t => {
  const dir = mkdtempSync(join(tmpdir(), 'sheet-ranges-'));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  const log = join(dir, 'sheet-ranges.jsonl');
  const state = join(dir, 'sheet-ranges-state.json');
  const sheets = workbook({ end: 30846, zEnd: 13775 });
  // As Google lists it for an account that cannot edit it: no requestingUserCanEdit.
  sheets[2].protectedRanges = [{ protectedRangeId: 42, range: range(9, 0, 20, 0, 1) }];
  const fake = fakeWorkbook(sheets);
  const first = await sheetRangesRun(fake.request, log, { at: Date.parse('2026-11-12T06:30:00Z') });
  assert.equal(first.code, 3);
  assert.match(first.lines[0], /^Sheet ranges: ok, nothing else within 1000 rows/);
  assert.deepEqual(first.lines.slice(1), [
    'Sheet ranges: WARNING: dropdown Barcoding_DNA A2:A20 is protected from the app: its owner moves =Lists!$T$2:$T$3507 to =Lists!$T$2:$T$17732 by hand',
  ]);
  const second = await sheetRangesRun(fake.request, log, { at: Date.parse('2026-11-13T06:30:00Z') });
  assert.equal(second.code, 0);
  assert.deepEqual(second.lines.slice(1), ['Sheet ranges: 1 known range still left for its owner (told before: sheet-ranges-state.json)']);
  // Never sent; one record in the log (the first night's), the state beside it.
  assert.equal(fake.calls.filter(c => c.method === 'POST').length, 0);
  assert.deepEqual(readFileSync(log, 'utf8').trim().split('\n').map(l => JSON.parse(l).status), ['left out']);
  const ranges = leftOutRanges(state);
  assert.equal(ranges.length, 1);
  assert.equal(ranges[0].since, '2026-11-12T06:30:00.000Z');
  // The app's alerts: one piece of information, after the others.
  const list = alertList([], { reached: [], close: [], limit: 30 }, [], 46000, [], [], ranges);
  assert.deepEqual(list.map(a => [a.id, a.level]), [['sheet-range:dropdown:Barcoding_DNA:A2', 'info']]);
  assert.equal(
    list[0].text,
    'Barcoding_DNA A2:A20: el desplegable toma sus opciones de =Lists!$T$2:$T$3507 y la lista ya llega a la fila 7732; debería ser =Lists!$T$2:$T$17732. La app no puede cambiarlo (rango protegido): PAS puede hacerlo a mano (Datos → Validación de datos). Visto el 12/11/2026.',
  );
  const cfAlert = alertList([], { reached: [], close: [], limit: 30 }, [], 46000, [], [], [
    { kind: 'conditional format', sheet: 'S', label: 'S rule 3', before: '=CONTAR.SI($A$2:$A$100, A2)>1', after: '=CONTAR.SI($A$2:$A$10100, A2)>1', rows: 100, since: '2026-11-12T06:30:00.000Z' },
  ]);
  assert.match(cfAlert[0].text, /^S, formato condicional =CONTAR\.SI\(\$A\$2:\$A\$100, A2\)>1: debería ser =CONTAR\.SI\(\$A\$2:\$A\$10100, A2\)>1\. /);
  // Runs of one sheet on the same list: one alert naming them all (Sperm_dissections B35:B37 and L34 on 11 Nov).
  const sperm = (key, label) => ({ key, kind: 'dropdown', sheet: 'Sperm_dissections', label, before: '=Insectary_data!$P$2:$P$4948', after: '=Insectary_data!$P$2:$P$24512', rows: 14512, since: '2026-11-12T06:30:00.000Z' });
  const grouped = alertList([], { reached: [], close: [], limit: 30 }, [], 46000, [], [], [
    sperm('dropdown:Sperm_dissections:B35', 'Sperm_dissections B35:B37'),
    sperm('dropdown:Sperm_dissections:L34', 'Sperm_dissections L34:L34'),
  ]);
  assert.equal(grouped.length, 1);
  assert.match(grouped[0].text, /^Sperm_dissections B35:B37, L34: el desplegable toma sus opciones de =Insectary_data!\$P\$2:\$P\$4948 y la lista ya llega a la fila 14512;/);
  assert.deepEqual(leftOutRanges(join(dir, 'none.json')), []);
  // Its owner moves it by hand: the next night forgets it, and the alert goes.
  sheets[2].dv = new Map([...sheets[2].dv].map(([k, v]) => [k, toEnd(v, 17732)]));
  const third = await sheetRangesRun(fake.request, log, { at: Date.parse('2026-11-14T06:30:00Z') });
  assert.equal(third.code, 0);
  assert.ok(third.lines.includes('Sheet ranges: dropdown Barcoding_DNA A2:A20 is no longer left out (moved, changed by hand, or writable again)'));
  assert.deepEqual(leftOutRanges(state), []);
});

test('a write Google refuses as protected (its protection not listed as such) closes its plan; tried again, told once', async t => {
  const dir = mkdtempSync(join(tmpdir(), 'sheet-ranges-'));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  const log = join(dir, 'sheet-ranges.jsonl');
  const sheets = workbook({ end: 30846, zEnd: 13775 });
  const fake = fakeWorkbook(sheets, { protectedSheets: ['Barcoding_DNA'] });
  const first = await sheetRangesRun(fake.request, log, { at: Date.parse('2026-11-12T06:30:00Z') });
  assert.equal(first.code, 3);
  const second = await sheetRangesRun(fake.request, log, { at: Date.parse('2026-11-13T06:30:00Z') });
  assert.equal(second.code, 0, second.lines.join('\n'));
  assert.deepEqual(readFileSync(log, 'utf8').trim().split('\n').map(l => JSON.parse(l).status), ['planned', 'refused', 'planned', 'refused']);
  // A state file that does not read: said, and its ranges told again (not a failing night).
  writeFileSync(join(dir, 'sheet-ranges-state.json'), '{');
  const third = await sheetRangesRun(fake.request, log, { at: Date.parse('2026-11-14T06:30:00Z') });
  assert.equal(third.code, 3);
  assert.match(third.lines[0], /sheet-ranges-state\.json did not read .*: its ranges are told again$/);
});

test('a whole sheet protected with another column free: no write; a night whose write times out keeps what it knew', async t => {
  const dir = mkdtempSync(join(tmpdir(), 'sheet-ranges-'));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  const log = join(dir, 'sheet-ranges.jsonl');
  const sheets = workbook({ end: 30846, zEnd: 13775 });
  sheets[2].protectedRanges = [{ protectedRangeId: 7, range: { sheetId: 9 }, unprotectedRanges: [range(9, 0, 20, 1, 2)] }];
  const fake = fakeWorkbook(sheets);
  const first = await sheetRangesRun(fake.request, log, { at: Date.parse('2026-11-12T06:30:00Z') });
  assert.equal(first.code, 3);
  assert.equal(fake.calls.filter(c => c.method === 'POST').length, 0);
  // The protection lifted, but the write (now with that range) times out: tonight cannot tell, the entry stays, unsaid.
  delete sheets[2].protectedRanges;
  const timedOut = await sheetRangesRun(async (path, options = {}) => {
    if (options.method === 'POST') throw Object.assign(new Error('Google Sheets did not answer in 120 s'), { timeout: true });
    return fake.request(path, options);
  }, log, { at: Date.parse('2026-11-13T06:30:00Z') });
  assert.equal(timedOut.code, 1);
  assert.ok(!timedOut.lines.some(l => /no longer left out/.test(l)), timedOut.lines.join('\n'));
  const kept = leftOutRanges(join(dir, 'sheet-ranges-state.json'));
  assert.deepEqual(kept.map(r => [r.label, r.since]), [['Barcoding_DNA A2:A20', '2026-11-12T06:30:00.000Z']]);
  // The next night it goes through: moved, told once, forgotten.
  const third = await sheetRangesRun(fake.request, log, { at: Date.parse('2026-11-14T06:30:00Z') });
  assert.equal(third.code, 0, third.lines.join('\n'));
  assert.ok(third.lines.includes('Sheet ranges: dropdown Barcoding_DNA A2:A20 is no longer left out (moved, changed by hand, or writable again)'));
  assert.deepEqual(leftOutRanges(join(dir, 'sheet-ranges-state.json')), []);
});
