// What the formulas of a proposal cost the workbook's recalculation (server/formula-cost.mjs):
// cells × rows each scans, whole columns against bounded ranges, lookups into other sheets copied
// down many rows, and the formulas elsewhere that read the written column whole (each write makes
// them recalculate); in propose_changes' and update_proposal's answers and in the table's view.
import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { DatabaseSync } from 'node:sqlite';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { moduleMap } from '../server/schema.mjs';
import { createAssistant } from '../server/assistant.mjs';
import {
  HEAVY_COMPARISONS,
  HEAVY_DEPENDENTS,
  dependentsOf,
  formulaCostAnswer,
  formulaCostOf,
  formulaRefs,
  proposalFormulaCost,
  workbookIndex,
} from '../server/formula-cost.mjs';

const CAM = r => `=XLOOKUP(A${r},Collection_data!D:D,Collection_data!E:E,"NA")`;
const T2 = r => `=IFS(U${r}="","",U${r}="NA","NA")`;

test('references: what each range is to its function (searched, one value picked, read whole)', () => {
  const refs = formulaRefs(`=IF(A2="","",XLOOKUP(A2,'F1/F2_MutationRate'!$D:$D,Collection_data!E:E,"x, y"))`);
  assert.deepEqual(
    refs.map(r => [r.text, r.sheet, r.role, r.r1, r.r2]),
    [
      ['A2', null, 'read', 2, 2],
      ['A2', null, 'read', 2, 2],
      ["'F1/F2_MutationRate'!$D:$D", 'F1/F2_MutationRate', 'search', null, null],
      ['Collection_data!E:E', 'Collection_data', 'pick', null, null],
    ],
  );
  // INDEX/MATCH: MATCH searches, INDEX picks; SUM reads its range whole; names and text are no references.
  assert.deepEqual(
    formulaRefs('=INDEX(Lists!B:B,MATCH(C3,Lists!A:A,0))&SUM(F2:F9)&TRUE&LOG10(2)&"A1:B2"').map(r => [r.text, r.role]),
    [
      ['Lists!B:B', 'pick'],
      ['C3', 'read'],
      ['Lists!A:A', 'search'],
      ['F2:F9', 'read'],
    ],
  );
});

test('one formula: a whole column scans its sheet, a bounded range its size, a cell of the row 1', () => {
  const rows = new Map([
    ['Collection_data', 9755],
    ['Insectary_data', 20846],
  ]);
  const whole = formulaCostOf(CAM(12000), 'Insectary_data', 1000, rows);
  assert.equal(whole.perCell, 1 + 9755 + 1, 'the key, the column searched, the value picked');
  assert.deepEqual(
    whole.scans.map(s => [s.range, s.rows, s.lookup, s.otherSheet]),
    [['Collection_data!D:D', 9755, true, true]],
  );
  assert.deepEqual(whole.flags, ['wholeColumnLookup', 'crossSheetRepeated']);
  assert.deepEqual(whole.tips, ['bounded', 'guard']);
  assert.equal(whole.bounded, 'Collection_data!$D$2:$D$9755');
  // The same over a bounded range: its size.
  const bounded = formulaCostOf(
    '=IF(A5="","",XLOOKUP(A5,Collection_data!$D$2:$D$501,Collection_data!$E$2:$E$501))',
    'Insectary_data',
    1000,
    rows,
  );
  assert.equal(bounded.perCell, 2 + 500 + 1);
  assert.deepEqual(bounded.flags, []);
  assert.deepEqual(bounded.tips, []);
  // Repeated over 500 rows or less: not the heavy blocks' shape. Within its own sheet: not another sheet.
  assert.deepEqual(formulaCostOf(CAM(2), 'Insectary_data', 500, rows).flags, ['wholeColumnLookup']);
  assert.deepEqual(formulaCostOf('=IF(C2="","",COUNTIF(C:C,C2))', 'Insectary_data', 2000, rows).flags, [
    'wholeColumnLookup',
  ]);
  // Same-row references only (the T2 formula): about one cell read each.
  const t2 = formulaCostOf(T2(12000), 'Insectary_data', 1000, rows);
  assert.equal(t2.perCell, 2);
  assert.deepEqual([t2.scans, t2.flags, t2.tips], [[], [], []]);
  // The same range searched twice per row: a helper column (XMATCH once).
  const twice = formulaCostOf(
    '=IF(ISERROR(MATCH(P2,Photo_links!B:B,0)),"",INDEX(Photo_links!E:E,MATCH(P2,Photo_links!B:B,0)))',
    'Insectary_data',
    100,
    rows,
  );
  assert.equal(twice.scans[0].times, 2);
  assert.deepEqual(twice.tips, ['helper', 'guard']);
  assert.equal(twice.perCell, 1 + 1000 + 1 + 1 + 1000, 'a sheet the copy does not hold: 1000 rows');
  assert.deepEqual(formulaCostOf('=OFFSET(A2,1,0)', 'Insectary_data', 1, rows).flags, ['volatile']);
});

/** A copy with the given records (sheet, row, formulas), as the store keeps them. */
function copy(records, lastSync = 's1') {
  const db = new DatabaseSync(':memory:');
  db.exec(`CREATE TABLE records(id TEXT PRIMARY KEY, sheet TEXT, row_num INTEGER, formulas_json TEXT, missing INTEGER DEFAULT 0);
    CREATE TABLE settings(key TEXT PRIMARY KEY, value TEXT)`);
  const insert = db.prepare('INSERT INTO records(id, sheet, row_num, formulas_json) VALUES (?,?,?,?)');
  for (const [sheet, row, formulas] of records)
    insert.run(`${sheet}:${row}`, sheet, row, JSON.stringify(formulas ?? {}));
  db.prepare("INSERT INTO settings VALUES ('lastSync', ?)").run(lastSync);
  return {
    db,
    layouts: new Map(),
    getSetting: key => db.prepare('SELECT value FROM settings WHERE key=?').get(key)?.value,
  };
}

test('the formulas elsewhere that read a column whole: counted once per sync, with what they scan', () => {
  const records = [
    ['Insectary_data', 20846, {}],
    ['Collection_data', 9755, {}],
  ];
  // F1/F2_MutationRate: a lookup per row into Insectary_data A:A returning V; another returning B.
  for (let r = 2; r <= 1001; r++)
    records.push([
      'F1/F2_MutationRate',
      r,
      {
        T2: `=IF(D${r}="","",XLOOKUP(D${r},Insectary_data!A:A,Insectary_data!V:V,"MISSING"))`,
        Wild_Reared: `=IF(D${r}="","",XLOOKUP(D${r},Insectary_data!A:A,Insectary_data!B:B,"MISSING"))`,
        Sex: `=IF(D${r}="","",XLOOKUP(D${r},Insectary_data!$A$2:$A$30000,Insectary_data!F2:F2,"MISSING"))`,
      },
    ]);
  // A same-row reference and a small bounded range are not counted.
  records.push(['Insectary_stocks', 2, { Sum: '=SUM(Insectary_data!V2:V20)+Insectary_data!V5' }]);
  // Insectary_data V itself (the column written) is left out.
  records.push(['Insectary_data', 3, { T2_Preservation_medium: '=COUNTIF(V:V,"NA")' }]);
  const store = copy(records);
  const index = workbookIndex(store);
  assert.equal(workbookIndex(store), index, 'kept while the sync is the same');
  // V: the 1,000 lookups returning it; each recalculation scans A:A (20,846 rows).
  assert.deepEqual(dependentsOf(store, 'Insectary_data', 'T2_Preservation_medium'), {
    cells: 1000,
    comparisons: 1000 * (1 + 1 + 20846 + 1),
    sheets: [['F1/F2_MutationRate', 1000]],
  });
  // A: all three columns (the bounded $A$2:$A$30000 is as wide as the column).
  const a = dependentsOf(store, 'Insectary_data', 'Insectary_ID');
  assert.equal(a.cells, 3000);
  assert.equal(a.comparisons, 2 * 1000 * 20849 + 1000 * (1 + 1 + 29999 + 1));
  assert.equal(dependentsOf(store, 'Insectary_data', 'Sex'), null, 'F2:F2 is a cell of the row');
  // A new sync: read again.
  store.db.prepare("UPDATE settings SET value='s2'").run();
  assert.notEqual(workbookIndex(store), index);
});

test('a proposal: per distinct formula, heavy above the thresholds, and the answer in short', () => {
  const records = [
    ['Insectary_data', 20846, {}],
    ['Collection_data', 9755, {}],
  ];
  for (let r = 2; r <= 1001; r++)
    records.push(['F1/F2_MutationRate', r, { T2: `=XLOOKUP(D${r},Insectary_data!A:A,Insectary_data!V:V,"MISSING")` }]);
  const store = copy(records);
  const changes = [];
  for (let row = 12000; row < 13000; row++)
    changes.push({
      sheet: 'Insectary_data',
      row,
      recordId: `r${row}`,
      formulaCells: ['CAM_ID_CollData', 'T2_Preservation_medium'],
      values: { CAM_ID_CollData: CAM(row), T2_Preservation_medium: T2(row), Sex: 'male' },
    });
  changes.push({ sheet: 'Insectary_data', create: true, values: { Sex: 'male' } });
  const cost = proposalFormulaCost(store, changes);
  assert.equal(cost.length, 2, 'the same formula down the column is one');
  // The heaviest first: what T2's write makes recalculate weighs more than CAM's own lookups.
  const [t2, cam] = cost;
  assert.equal(cam.column, 'CAM_ID_CollData');
  assert.equal(cam.cells, 1000);
  assert.equal(cam.comparisons, 1000 * 9757);
  assert.ok(cam.comparisons >= HEAVY_COMPARISONS && cam.heavy);
  assert.equal(cam.dependents, null);
  // T2: light itself, but each write makes the 1,000 lookups reading V scan Insectary_data A:A.
  assert.equal(t2.comparisons, 2000);
  assert.equal(t2.dependents.cells, 1000);
  assert.ok(t2.dependents.comparisons >= HEAVY_DEPENDENTS && t2.heavy);
  assert.deepEqual(t2.flags, ['heavyDependents']);
  assert.deepEqual(t2.tips, ['batch']);
  // Fewer lookups reading V: under the threshold, not heavy.
  const light = proposalFormulaCost(copy(records.slice(0, 100)), changes.slice(0, 10));
  assert.ok(!light.some(p => p.heavy), JSON.stringify(light));

  const answer = formulaCostAnswer(cost);
  assert.deepEqual(answer[1], {
    sheet: 'Insectary_data',
    column: 'CAM_ID_CollData',
    cells: 1000,
    scans: ['Collection_data!D:D: 9,755 rows'],
    comparisons: '1,000 cells × 9,757 ≈ 9.8 M per full recalculation',
    flags: ['wholeColumnLookup', 'crossSheetRepeated'],
    heavy: true,
    suggestion:
      'a range ending at the last used row (Collection_data!$D$2:$D$9755) instead of the whole column; extend it when rows are added; skip rows that cannot match before searching (IF(key="","",…), or the row\'s kind)',
  });
  assert.match(
    answer[0].eachWrite,
    /^recalculates 1,000 formulas reading T2_Preservation_medium whole \(F1\/F2_MutationRate 1,000\) ≈ 21 M comparisons$/,
  );
  assert.match(answer[0].suggestion, /one proposal, not cell by cell/);
  assert.equal(
    proposalFormulaCost(store, [{ sheet: 'Insectary_data', row: 2, values: { Sex: 'x' } }]),
    null,
    'no formulas: nothing',
  );
});

test('propose_changes, update_proposal and the table say what the formulas cost', async () => {
  const fields = moduleMap.get('Insectary_data').fields;
  const column = key => fields.find(f => f.key === key).column;
  const data = [];
  for (let row = 2; row <= 6; row++) data.push({ row, values: { Insectary_ID: `A${row}Z`, Tube_2_tissue: 'NA' } });
  const sheets = new LocalSheets({
    Insectary_data: data,
    Collection_data: [{ row: 2, values: { CAM_ID: 'CAM1', Insectary_ID: 'A2Z' } }],
  });
  for (const r of sheets.rows.get('Insectary_data'))
    if (r.row >= 2)
      r.cells[column('T2_Preservation_medium')] = {
        userEnteredValue: { formulaValue: T2(r.row) },
        effectiveValue: { stringValue: 'NA' },
      };
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data', 'Collection_data'] });
  const assistant = createAssistant({ store, config: {} });
  store.db
    .prepare(
      "INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u1','franz','Franz','editor','s','h',1,'2026-01-01')",
    )
    .run();
  store.db
    .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
    .run(createHash('sha256').update('franz-token').digest('hex'), 'u1');
  const call = async (name, args) =>
    JSON.parse(
      (
        await assistant.mcp(
          { authorization: 'Bearer franz-token' },
          { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } },
        )
      ).body.result.content[0].text,
    );
  try {
    const out = await call('propose_changes', {
      reason: 'CAM from Collection_data',
      bulk: [
        { sheet: 'Insectary_data', rows: { from: 2, to: 6 }, set: { CAM_ID_CollData: { formula: CAM('{row}') } } },
      ],
    });
    assert.ok(out.proposalId, JSON.stringify(out));
    assert.deepEqual(out.formulaCost, [
      {
        sheet: 'Insectary_data',
        column: 'CAM_ID_CollData',
        cells: 5,
        scans: ['Collection_data!D:D: 2 rows'],
        comparisons: '5 cells × 4 ≈ 20 per full recalculation',
        flags: ['wholeColumnLookup'],
        suggestion:
          'a range ending at the last used row (Collection_data!$D$2:$D$2) instead of the whole column; extend it when rows are added; skip rows that cannot match before searching (IF(key="","",…), or the row\'s kind)',
      },
    ]);
    const up = await call('update_proposal', {
      proposalId: out.proposalId,
      rows: [
        {
          index: 0,
          values: {
            CAM_ID_CollData: {
              formula: '=IF(A{row}="","",XLOOKUP(A{row},Collection_data!D:D,Collection_data!E:E,"NA"))',
            },
          },
        },
      ],
    });
    assert.equal(up.formulaCost.length, 2, JSON.stringify(up));
    // Revised without formulas: nothing said.
    const plain = await call('update_proposal', { proposalId: out.proposalId, rows: [{ index: 1, highlight: true }] });
    assert.ok(!plain.error, JSON.stringify(plain));
    // The table's view: the numbers, for the notice above it.
    const view = (
      await assistant.handle({
        method: 'GET',
        path: '/api/chat/proposals',
        body: {},
        user: { id: 'u1', username: 'franz', displayName: 'Franz', role: 'editor' },
        query: { all: '1' },
      })
    ).body.proposals[0];
    assert.equal(view.formulaCost.length, 2);
    assert.deepEqual(
      view.formulaCost.map(p => [p.cells, p.comparisons, p.heavy, p.scans]),
      [
        [4, 16, false, [{ range: 'Collection_data!D:D', rows: 2, lookup: true }]],
        [1, 5, false, [{ range: 'Collection_data!D:D', rows: 2, lookup: true }]],
      ],
    );
  } finally {
    store.close();
  }
});
