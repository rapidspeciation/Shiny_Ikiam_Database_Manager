import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { columnOf } from '../server/schema.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { applyBatch } from '../server/batch.mjs';
import { runHistoryTool } from '../server/history.mjs';
import { findRecords, resolveRows } from '../server/records-tool.mjs';
import { RESULT_BUDGET, fitResult } from '../server/tool-budget.mjs';

// Tool answers within one size, rows named by their ID in the sheet, and column names as people
// write them.

const editor = { id: 'u-franz', username: 'franz', displayName: 'Franz Chandi', role: 'editor' };

async function setup(seed) {
  const sheets = new LocalSheets(seed);
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: Object.keys(seed) });
  const assistant = createAssistant({ store, config: {} });
  store.db
    .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES(?,?,?,?,'s','h',1,'2026-01-01')")
    .run(editor.id, editor.username, editor.displayName, editor.role);
  store.db
    .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
    .run(createHash('sha256').update('franz-token').digest('hex'), editor.id);
  const mcp = (method, params) => assistant.mcp({ authorization: 'Bearer franz-token' }, { jsonrpc: '2.0', id: 1, method, params });
  const raw = async (name, args) => (await mcp('tools/call', { name, arguments: args })).body.result.content[0].text;
  const call = async (name, args) => JSON.parse(await raw(name, args));
  const row = (sheet, n) => store.getRecordBySheetRow(sheet, n);
  return { store, mcp, raw, call, row };
}

const INSECTARY = {
  Insectary_data: [
    { row: 2, values: { Insectary_ID: '4OO', SPECIES: 'Mechanitis lysimnia', Sex: 'female' } },
    { row: 3, values: { Insectary_ID: 'W2B', SPECIES: 'Mechanitis lysimnia', Sex: 'male' } },
    { row: 4, values: { Insectary_ID: 'X1X', SPECIES: 'Oleria onega' } },
    { row: 5, values: { Insectary_ID: 'X1X', SPECIES: 'Oleria onega' } },
  ],
  // The same wild butterfly in Collection_data, with its Insectary_ID.
  Collection_data: [{ row: 2, values: { CAM_ID: 'CAM000001', Insectary_ID: 'W2B', SPECIES: 'Mechanitis lysimnia' } }],
  Insectary_stocks: [{ row: 2, values: { 'CLUTCH NUMBER': 1014, SPECIES: 'Mechanitis lysimnia' } }],
};

test('fitResult: an answer over the budget keeps the start of its longest lists and says what was left out', () => {
  const small = { a: 1, list: [1, 2, 3] };
  assert.equal(fitResult(small), small, 'an answer that fits is left as it is');
  const big = { total: 900, rows: Array.from({ length: 900 }, (_, i) => ({ i, text: 'x'.repeat(80) })), note: 'kept' };
  const out = fitResult(big, { narrow: 'page with offset' });
  assert.ok(JSON.stringify(out).length <= RESULT_BUDGET);
  assert.equal(out.truncated, true);
  assert.ok(out.rows.length > 100 && out.rows.length < 900);
  assert.equal(out.rows[0].i, 0, 'the first items stay');
  assert.equal(out.note, 'kept');
  assert.match(out.next, new RegExp(`^Shown: rows: the first ${out.rows.length} of 900\\. For the rest, page with offset\\.$`));
  // A list inside an object of the answer.
  const nested = fitResult({ group: { id: 'g', changes: big.rows } });
  assert.ok(JSON.stringify(nested).length <= RESULT_BUDGET);
  assert.match(nested.next, /group\.changes: the first \d+ of 900/);
  // Nothing to cut: an error that says how to ask for less.
  assert.match(fitResult({ text: 'x'.repeat(RESULT_BUDGET + 10) }, { narrow: 'ask for one field' }).error, /too long.*ask for one field/);
});

test('column names as people write them: one column, or the nearest ones named', () => {
  assert.deepEqual(columnOf('Insectary_stocks', 'pupa date'), { key: 'PUPA DATE' });
  assert.deepEqual(columnOf('Insectary_data', 'insectary id'), { key: 'Insectary_ID' });
  assert.deepEqual(columnOf('Collection_data', 'collection dáte'), { key: 'Collection_date' });
  assert.match(columnOf('Insectary_data', 'Sexo').error, /^Unknown column Sexo in Insectary_data; did you mean Sex\b/);
  assert.match(columnOf('Insectary_data', 'zzzzzz').error, /describe_sheet lists its columns/);
});

test('rows by their ID: a bare ID, {sheet, id}, a key; the first ID column wins across sheets; repeated IDs are asked about', async () => {
  const { store, call, row } = await setup(INSECTARY);
  try {
    const w2b = row('Insectary_data', 3).id;
    // W2B is also the Insectary_ID of a Collection_data row: the Insectary_data row is the one whose ID it is.
    assert.deepEqual(resolveRows(store, ['W2B', { sheet: 'Collection_data', id: 'W2B' }, { sheet: 'Insectary_stocks', key: { 'clutch number': '1014' } }]).ids, [
      w2b,
      row('Collection_data', 2).id,
      row('Insectary_stocks', 2).id,
    ]);
    const { problems } = resolveRows(store, ['X1X', 'NOPE', { id: 'W2B', sheet: 'Nowhere' }]);
    assert.deepEqual(problems.map(p => p.kind), ['ambiguous', 'missing', 'invalid']);
    assert.match(problems[0].error, /^X1X is the ID of 2 rows: Insectary_data row 4 .*Insectary_data row 5 .*Give \{"sheet", "id"\} or the recordId/);
    assert.match(problems[1].error, /^No row with ID NOPE; give the row's recordId, or look it up with find_records/);

    // The production mistake: an Insectary_ID given as recordId now names its row.
    const proposed = await call('propose_changes', { reason: 'x', changes: [{ recordId: '4OO', values: { sex: 'male' } }] });
    assert.ok(proposed.proposalId, JSON.stringify(proposed));
    assert.deepEqual(proposed.table.map(r => [r.label, r.row]), [['4OO', 2]]);
    // {sheet, id} and a values key named loosely; an ambiguous ID is refused with what to give.
    const more = await call('update_proposal', { proposalId: proposed.proposalId, changes: [{ sheet: 'Insectary_data', id: 'W2B', values: { SEX: 'female' } }] });
    assert.deepEqual(more.changed.map(r => [r.index, r.label, r.values]), [[1, 'W2B', { Sex: 'female' }]]);
    const twice = await call('propose_changes', { reason: 'x', changes: [{ recordId: 'X1X', values: { Sex: 'male' } }] });
    assert.match(twice.error, /^changes\[0\]: X1X is the ID of 2 rows/);
    // Rows of the proposal by their ID too.
    const byLabel = await call('update_proposal', { proposalId: proposed.proposalId, rows: [{ id: '4oo', values: { Sex: 'NA' } }], removeRows: ['W2B'] });
    assert.deepEqual(byLabel.changed.map(r => [r.index, r.values]), [[0, { Sex: 'NA' }]]);
    assert.deepEqual(byLabel.removed, [{ index: 1, label: 'W2B' }]);

    // bulk, show_rows and get_record take IDs too.
    const bulk = await call('propose_changes', { reason: 'y', bulk: [{ sheet: 'Insectary_data', recordIds: ['W2B', 'ZZZ'], set: { notes_insectary_data: 'checked' } }] });
    assert.deepEqual([bulk.rows, bulk.bulk[0].notInSheet], [1, ['ZZZ']]);
    const shown = await call('show_rows', { title: 't', sheet: 'Insectary_data', recordIds: ['W2B', '4OO'], notes: [{ id: 'W2B', field: 'sex', text: 'male?' }] });
    assert.equal(shown.rows, 2);
    assert.ok(!shown.notesNotShown, JSON.stringify(shown));
    assert.ok(shown.columns.includes('Sex'));
    assert.equal((await call('get_record', { id: 'W2B' })).id, w2b);
    assert.equal((await call('get_record', { id: '1014', sheet: 'Insectary_stocks' })).row, 2);
    assert.match((await call('get_record', { id: 'X1X' })).error, /2 rows/);
  } finally {
    store.close();
  }
});

test('find_records: identifiers in one pass, any case, missing in the order given; count_records groupBy named loosely', async () => {
  const rows = Array.from({ length: 300 }, (_, i) => ({ row: i + 2, values: { Insectary_ID: `A${i}`, SPECIES: i % 2 ? 'Oleria onega' : 'Mechanitis lysimnia' } }));
  const { store, call } = await setup({ Insectary_data: rows });
  try {
    const values = ['a5', 'Z9', ' A7 ', 'Q1', ...Array.from({ length: 200 }, (_, i) => `A${i + 50}`)];
    const out = findRecords(store, { module: 'Insectary_data', field: 'insectary_id', values, idsOnly: true });
    assert.deepEqual(out.missing, ['Z9', 'Q1']);
    assert.deepEqual(out.found.slice(0, 2).map(r => r.label), ['A5', 'A7']);
    assert.equal(out.total, 202);
    const counted = await call('count_records', { sheet: 'Insectary_data', groupBy: 'species' });
    assert.deepEqual(counted.groupBy, ['SPECIES']);
    assert.deepEqual(counted.groups.map(g => g.n), [150, 150]);
  } finally {
    store.close();
  }
});

test('check_data, describe_sheet and long count groups stay within one answer and say how to go on', async () => {
  // 260 rows with the same tube: 260 "repeat" issues.
  const rows = Array.from({ length: 260 }, (_, i) => ({
    row: i + 2,
    values: { Insectary_ID: `B${i}`, SPECIES: 'Oleria onega', Tube_1_id: 'FS50000001', Notes_Insectary_data: `${i} ${'larva con hongos '.repeat(12)}` },
  }));
  const { store, raw, call } = await setup({ Insectary_data: rows });
  try {
    const text = await raw('check_data', { kind: 'repeat', limit: 200 });
    assert.ok(text.length <= RESULT_BUDGET, String(text.length));
    const page = JSON.parse(text);
    assert.equal(page.truncated, true);
    assert.ok(page.issues.length > 0 && page.issues.length < 200);
    assert.match(page.next, new RegExp(`check_data with offset: ${page.issues.length}`));
    assert.ok(!page.kinds, 'the kinds are explained only when no kind is asked');
    const notes = await raw('count_records', { sheet: 'Insectary_data', groupBy: 'Notes_Insectary_data' });
    assert.ok(notes.length <= RESULT_BUDGET);
    assert.equal(JSON.parse(notes).truncated, true);
    assert.ok((await call('describe_sheet', { module: 'Insectary_data' })).columns.length > 10);
  } finally {
    store.close();
  }
});

test('get_history_group and record_history: formulas as "(formula)" unless asked, saves without changes counted, pages within the budget', async () => {
  const species = row => ({ formula: `=XLOOKUP(C${row},Insectary_stocks!A:A,Insectary_stocks!C:C,"")` });
  const { store, row } = await setup({ Insectary_data: [{ row: 2, values: { Insectary_ID: 'A0A', SPECIES: species(2), Sex: 'female' } }] });
  try {
    const a0 = row('Insectary_data', 2).id;
    const out = await applyBatch(store, { requestId: 'history-budget-1', purpose: 'tablas', edits: [{ id: a0, values: { Sex: 'male' } }] }, editor);
    const action = out.action.id;
    // A sync that logged a whole row's lookup formulas, many times over, and a save with nothing left to show.
    const long = n => JSON.stringify({ formula: `=IFERROR(XLOOKUP(${n},Collection_data!A:A,Collection_data!B:B),"${'x'.repeat(900)}")` });
    const insert = store.db.prepare(
      'INSERT INTO changes (id, action_id, record_id, sheet, row_num, field, before_json, after_json) VALUES (?,?,?,?,?,?,?,?)',
    );
    for (let i = 0; i < 400; i++) insert.run(`c-${i}`, action, a0, 'Insectary_data', 2, `Lookup_${i}`, long(i), long(i + 1));
    store.db
      .prepare('INSERT INTO actions (id, actor, source, created_at, status, reason, purpose) SELECT ?, actor, source, created_at, status, reason, purpose FROM actions WHERE id = ?')
      .run('quiet-save', action);
    const tool = args => runHistoryTool(store, 'get_history_group', args, {}, {});
    const group = await tool({ id: action });
    assert.ok(JSON.stringify(group).length <= RESULT_BUDGET, String(JSON.stringify(group).length));
    assert.equal(group.cells, 401);
    assert.equal(group.savesWithoutChanges, 1);
    assert.ok(group.actions.every(a => a.changes.length));
    const lookups = group.actions.flatMap(a => a.changes).filter(c => c.field.startsWith('Lookup_'));
    assert.ok(lookups.length > 100);
    assert.deepEqual([lookups[0].before, lookups[0].after], ['(formula)', '(formula)']);
    assert.ok(group.next > 0 && group.next < 401);
    const texts = await tool({ id: action, formulas: true });
    assert.ok(JSON.stringify(texts).length <= RESULT_BUDGET);
    assert.equal(texts.truncated, true);
    assert.match(texts.actions[0].changes.find(c => c.field.startsWith('Lookup_')).before, /^fórmula =IFERROR/);
    const rest = await tool({ id: action, formulas: true, offset: texts.next });
    assert.ok(rest.actions[0].changes.length > 0);

    const history = await runHistoryTool(store, 'record_history', { recordId: a0 }, {}, {});
    assert.ok(JSON.stringify(history).length <= RESULT_BUDGET);
    assert.match(history.saves[0].cells.find(c => c.startsWith('Lookup_0')), /^Lookup_0: \(formula\) → \(formula\)$/);
    const withTexts = await runHistoryTool(store, 'record_history', { recordId: a0, formulas: true }, {}, {});
    assert.ok(JSON.stringify(withTexts).length <= RESULT_BUDGET);
    assert.match(withTexts.saves[0].moreCells, /more cells of this save not shown/);
  } finally {
    store.close();
  }
});
