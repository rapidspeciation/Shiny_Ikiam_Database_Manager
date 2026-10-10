// The assistant writes formulas through proposals: {"formula": "=..."} (a plain "=..." stays
// text), `{row}` the row's own number, checked before proposing, written as a formula over a
// formula or a value, kept in history as formula text and undone back to what was there; a bulk
// down a column (pre-made rows too) may take FORMULA_ROWS rows.
import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash, randomUUID } from 'node:crypto';
import { mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { Store } from '../server/store.mjs';
import { GoogleSheets, LocalSheets } from '../server/sheets.mjs';
import { SANDBOX_ID } from '../server/workbook.mjs';
import { moduleMap } from '../server/schema.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { checkFormula, formulaKey, sameCell } from '../server/formula-write.mjs';

const OLD = r => `=IFS(U${r}="","",U${r}="NA","NA")`;
const NEW = '=IFS(U{row}="","",U{row}="NA","NA",U{row}="NOT_COLLECTED","NOT_COLLECTED",TRUE,"")';
const user = { id: 'u1', username: 'franz', displayName: 'Franz', role: 'editor' };

test('a formula is checked before it is proposed', () => {
  assert.deepEqual(checkFormula('Insectary_data', { formula: NEW }, 12), {
    formula: '=IFS(U12="","",U12="NA","NA",U12="NOT_COLLECTED","NOT_COLLECTED",TRUE,"")',
  });
  assert.match(checkFormula('Insectary_data', { formula: 'IFS(U2="","")' }, 2).error, /starts with "="/);
  assert.match(checkFormula('Insectary_data', { formula: '=IFS(U2="",""' }, 2).error, /not closed/);
  assert.match(checkFormula('Insectary_data', { formula: '=IF(U2="x),1,2)' }, 2).error, /quote/);
  assert.match(checkFormula('Insectary_data', { formula: '=IF(ZZ2="",1,2)' }, 2).error, /column ZZ is not one of Insectary_data's columns/);
  assert.match(checkFormula('Insectary_data', { formula: '=IF(U2=,,)+' }, 2).error, /does not read/);
  // As the Sheets API takes formulas, whatever the workbook's language: English names, commas.
  assert.match(
    checkFormula('Insectary_data', { formula: '=SI(U2="","",BUSCARX(A2,Collection_data!D:D,Collection_data!E:E,"NA"))' }, 2).error,
    /English: IF for SI, XLOOKUP for BUSCARX/,
  );
  assert.match(checkFormula('Insectary_data', { formula: '=IF(U2="";"";1)' }, 2).error, /commas between arguments/);
  assert.ok(checkFormula('Insectary_data', { formula: '=IF(U2="a;b","SI(",1)' }, 2).formula, 'text in quotes is not looked at');
  // Volatile functions: refused, with what to use instead.
  assert.match(checkFormula('Insectary_data', { formula: '=INDIRECT("A"&ROW())' }, 2).error, /INDIRECT makes Sheets recalculate the whole workbook after every edit: use a fixed range/);
  assert.match(checkFormula('Insectary_data', { formula: '=IF(U2="",TODAY(),OFFSET(A2,1,0))' }, 2).error, /TODAY, OFFSET make Sheets recalculate .*the date typed as a value; a fixed range/);
  // Another sheet's columns are not checked against this one's.
  assert.ok(checkFormula('Insectary_data', { formula: '=XLOOKUP(A2,Lists!ZZ:ZZ,Lists!A:A)' }, 2).formula);
  // Google may change the spacing and the case of names: the same formula.
  assert.equal(formulaKey('=ifs( U2 = "a b", 1)'), formulaKey('=IFS(U2="a b",1)'));
  assert.ok(sameCell({ formula: '=ifs(U2="", "")' }, { formula: '=IFS(U2="","")' }));
  assert.ok(!sameCell({ formula: '=IFS(U2="a","")' }, { formula: '=IFS(U2="A","")' }), 'text in quotes keeps its case');
});

async function fixture(rows = 6, options = {}) {
  const fields = moduleMap.get('Insectary_data').fields;
  const column = key => fields.find(f => f.key === key).column;
  const data = [];
  for (let row = 2; row <= rows + 1; row++) data.push({ row, values: { Insectary_ID: `A${row}Z`, Tube_2_tissue: row === 3 ? 'NOT_COLLECTED' : 'NA', Sex: 'male' } });
  const sheets = new LocalSheets({ Insectary_data: data }, options);
  for (const r of sheets.rows.get('Insectary_data')) {
    if (r.row < 2) continue;
    // Row 4 typed over (no formula); the others hold the old formula.
    r.cells[column('T2_Preservation_medium')] =
      r.row === 4 ? { userEnteredValue: { stringValue: 'Ethanol' } } : { userEnteredValue: { formulaValue: OLD(r.row) }, effectiveValue: { stringValue: 'NA' } };
    if (r.row === 2) r.cells[column('Pedigree')] = { userEnteredValue: { formulaValue: '="NA"' }, effectiveValue: { stringValue: 'NA' } };
  }
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data'] });
  const assistant = createAssistant({ store, config: {} });
  store.db
    .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u1','franz','Franz','editor','s','h',1,'2026-01-01')")
    .run();
  store.db.prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')").run(createHash('sha256').update('franz-token').digest('hex'), 'u1');
  const call = async (name, args) =>
    JSON.parse(
      (await assistant.mcp({ authorization: 'Bearer franz-token' }, { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } }))
        .body.result.content[0].text,
    );
  const http = (method, path, body = {}) => assistant.handle({ method, path, body, user, query: { all: '1' } });
  const cell = row => sheets.rows.get('Insectary_data').find(r => r.row === row).cells[column('T2_Preservation_medium')];
  const record = row => store.getRecordBySheetRow('Insectary_data', row);
  return { store, sheets, call, http, cell, record, assistant };
}

test('a bulk replaces a formula down a column (where it still holds it), shown with what it gives; applied, then undone', async () => {
  const { store, call, http, cell, record } = await fixture();
  try {
    const out = await call('propose_changes', {
      reason: 'T2_Preservation_medium: NOT_COLLECTED',
      bulk: [{ sheet: 'Insectary_data', rows: { from: 2, to: 7 }, set: { T2_Preservation_medium: { formula: NEW } }, replacesFormula: OLD('{row}') }],
    });
    assert.ok(out.proposalId, JSON.stringify(out));
    // Row 4 was typed over: left alone.
    assert.equal(out.rows, 5);
    const p = (await http('GET', '/api/chat/proposals')).body.proposals[0];
    const row3 = p.changes.find(c => c.row === 3);
    assert.deepEqual(row3.formulaCells, ['T2_Preservation_medium']);
    assert.equal(row3.values.T2_Preservation_medium, '=IFS(U3="","",U3="NA","NA",U3="NOT_COLLECTED","NOT_COLLECTED",TRUE,"")');
    assert.equal(row3.formulaGives.T2_Preservation_medium, 'NOT_COLLECTED');
    assert.equal(row3.oldFormulas.T2_Preservation_medium, OLD(3));
    assert.ok(!p.changes.some(c => c.row === 4 && !c.gap), "row 4 only as a row in between");

    const applied = await http('POST', `/api/chat/proposals/${p.id}/apply`, { requestId: randomUUID() });
    assert.equal(applied.status, 200, JSON.stringify(applied.body));
    assert.equal(applied.body.status, 'applied');
    assert.equal(cell(3).userEnteredValue.formulaValue, row3.values.T2_Preservation_medium);
    assert.equal(cell(4).userEnteredValue.stringValue, 'Ethanol');
    assert.equal(record(5).formulas.T2_Preservation_medium, '=IFS(U5="","",U5="NA","NA",U5="NOT_COLLECTED","NOT_COLLECTED",TRUE,"")');
    // History: the formulas before and after.
    const change = store.db
      .prepare("SELECT before_json, after_json FROM changes WHERE row_num = 3 AND field = 'T2_Preservation_medium'")
      .get();
    assert.deepEqual(JSON.parse(change.before_json), { formula: OLD(3) });
    assert.deepEqual(JSON.parse(change.after_json), { formula: row3.values.T2_Preservation_medium });
    // Undone: the old formula again.
    const action = store.db.prepare("SELECT action_id FROM changes WHERE row_num = 3 AND field = 'T2_Preservation_medium'").get().action_id;
    await store.undo({ actionIds: [action], requestId: randomUUID() }, user);
    assert.equal(cell(3).userEnteredValue.formulaValue, OLD(3));
    assert.equal(record(3).formulas.T2_Preservation_medium, OLD(3));
  } finally {
    store.close();
  }
});

test('one row: a formula over a typed value, a plain "=..." stays text, mistakes are refused', async () => {
  const { store, call, record } = await fixture(3);
  try {
    const bad = await call('propose_changes', { reason: 'x', changes: [{ recordId: record(4).id, values: { T2_Preservation_medium: { formula: '=IFS(U4="",""' } } }] });
    assert.match(bad.error, /not closed/);
    const volatile = await call('propose_changes', { reason: 'x', changes: [{ recordId: record(4).id, values: { T2_Preservation_medium: { formula: '=OFFSET(U{row},0,0)' } } }] });
    assert.match(volatile.error, /T2_Preservation_medium: OFFSET makes Sheets recalculate the whole workbook/);
    const ok = await call('propose_changes', { reason: 'x', changes: [{ recordId: record(4).id, values: { T2_Preservation_medium: { formula: NEW } } }] });
    assert.ok(ok.proposalId, JSON.stringify(ok));
    // The same formula already there: nothing to write.
    const same = await call('propose_changes', { reason: 'x', changes: [{ recordId: record(2).id, values: { T2_Preservation_medium: { formula: OLD('{row}') } } }] });
    assert.match(same.error, /already in the sheet/);
    // Text over a formula cell: refused as before.
    const text = await call('propose_changes', { reason: 'x', changes: [{ recordId: record(2).id, values: { Pedigree: '=1+1' } }] });
    assert.match(text.error, /calculated by a formula/);
    // The Tube 2 medium as a value: left to the formula where it gives it (row 2: NA), typed over it
    // where it would not (row 3: the older formula has no case for NOT_COLLECTED).
    const gives = await call('propose_changes', { reason: 'x', changes: [{ recordId: record(2).id, values: { T2_Preservation_medium: 'NA' } }] });
    assert.match(gives.error, /already in the sheet/);
    const typed = await call('propose_changes', { reason: 'x', changes: [{ recordId: record(3).id, values: { T2_Preservation_medium: 'NOT_COLLECTED' } }] });
    assert.ok(typed.proposalId, JSON.stringify(typed));
    const shown = await call('get_proposal', { proposalId: typed.proposalId, full: true });
    assert.equal(shown.rows[0].values.T2_Preservation_medium, 'NOT_COLLECTED');
    assert.deepEqual(shown.rows[0].replaceFormula ?? ['T2_Preservation_medium'], ['T2_Preservation_medium']);
    // onlyWhereFormula needs a formula in set.
    const only = await call('propose_changes', { reason: 'x', bulk: [{ sheet: 'Insectary_data', rows: { from: 2, to: 4 }, set: { Sex: 'female' }, onlyWhereFormula: true }] });
    assert.match(only.error, /go with a \{"formula"/);
    // update_proposal takes a formula too.
    const up = await call('update_proposal', { proposalId: ok.proposalId, rows: [{ index: 0, values: { T2_Preservation_medium: { formula: '=IF(U{row}="","",U{row})' } } }] });
    assert.ok(!up.error && !up.problems?.length, JSON.stringify(up));
    const full = await call('get_proposal', { proposalId: ok.proposalId, full: true });
    assert.equal(full.rows[0].values.T2_Preservation_medium, '=IF(U4="","",U4)');
  } finally {
    store.close();
  }
});

test('a proposal that only writes formulas may take more rows than one save (others 500): written in parts, each its own undoable save', async () => {
  const { store, call, http, cell } = await fixture(600);
  try {
    const plain = await call('propose_changes', { reason: 'x', bulk: [{ sheet: 'Insectary_data', rows: { from: 2, to: 601 }, set: { Sex: 'female' } }] });
    assert.match(plain.error, /600 rows to change/);
    const out = await call('propose_changes', {
      reason: 'x',
      bulk: [{ sheet: 'Insectary_data', rows: { from: 2, to: 601 }, set: { T2_Preservation_medium: { formula: NEW } }, onlyWhereFormula: true }],
    });
    assert.equal(out.rows, 599, JSON.stringify(out).slice(0, 300));
    const p = (await http('GET', '/api/chat/proposals')).body.proposals[0];
    const applied = await http('POST', `/api/chat/proposals/${p.id}/apply`, { requestId: randomUUID() });
    assert.equal(applied.status, 200, JSON.stringify(applied.body).slice(0, 300));
    assert.equal(applied.body.status, 'applied');
    assert.equal(applied.body.result.parts, 2);
    assert.equal(applied.body.result.actions.length, 2);
    assert.equal(cell(601).userEnteredValue.formulaValue, NEW.replaceAll('{row}', '601'));
    // The second part undone: the first stays.
    await store.undo({ actionIds: [applied.body.result.actions[1].id], requestId: randomUUID() }, user);
    assert.equal(cell(601).userEnteredValue.formulaValue, OLD(601));
    assert.equal(cell(2).userEnteredValue.formulaValue, NEW.replaceAll('{row}', '2'));
  } finally {
    store.close();
  }
});

test('while Google does not answer, both parts wait in the app; the proposal is applied once the last one is written', async () => {
  const { store, sheets, call, cell, assistant } = await fixture(600, { health: { probeMs: 20 } });
  try {
    const { proposalId } = await call('propose_changes', {
      reason: 'x',
      bulk: [{ sheet: 'Insectary_data', rows: { from: 2, to: 601 }, set: { T2_Preservation_medium: { formula: NEW } }, onlyWhereFormula: true }],
    });
    sheets.simulateBusy({ minutes: 1, delayMs: 0 });
    await sheets.probe().catch(() => {});
    const out = await call('apply_proposal', { proposalId });
    assert.equal(out.status, 'queued', JSON.stringify(out).slice(0, 300));
    assert.equal(store.db.prepare("SELECT count(*) n FROM outbox WHERE kind = 'proposal' AND ref = ?").get(proposalId).n, 2);
    sheets.simulateBusy({ minutes: 0 });
    await sheets.health.runProbe();
    const end = Date.now() + 5000;
    while (store.db.prepare('SELECT status FROM ai_proposals WHERE id = ?').get(proposalId).status !== 'applied') {
      if (Date.now() > end) throw new Error('not applied');
      await new Promise(resolve => setTimeout(resolve, 10));
    }
    assert.equal(cell(601).userEnteredValue.formulaValue, NEW.replaceAll('{row}', '601'));
  } finally {
    assistant.close();
    store.close();
  }
});

test('rows are read 800 at most per request (a formula written down 2,000 rows)', async t => {
  const dir = mkdtempSync(join(tmpdir(), 'rows-'));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  const file = join(dir, 'google.json');
  writeFileSync(file, JSON.stringify({ client_id: 'c', client_secret: 's', refresh_token: 'r' }));
  const sheets = new GoogleSheets({ spreadsheetId: SANDBOX_ID, googleCredentialsFile: file });
  const requests = [];
  sheets.readRanges = async ranges => (requests.push(ranges.map(r => [r.start, r.end])), { sheets: [] });
  const rows = Array.from({ length: 2000 }, (_, i) => 12001 + i);
  const out = await sheets.readRows([{ sheet: 'Insectary_data', rows: [1, ...rows] }]);
  assert.equal(out.size, 2001);
  assert.ok(requests.every(r => r.reduce((n, [a, b]) => n + b - a + 1, 0) <= 800), JSON.stringify(requests));
  assert.deepEqual(requests.flat(), [[1, 1], [12001, 12800], [12801, 13600], [13601, 14000]]);
});

test('match_notebook: a death not preserved types the Tube 2 medium in the rows whose formula would not give it', async () => {
  const column = key => moduleMap.get('Insectary_data').fields.find(f => f.key === key).column;
  const NEWER = row => `=IFS(U${row}="","",OR(U${row}="NA",U${row}="NOT_COLLECTED",U${row}="NOT_PROVIDED"),"NOT_COLLECTED")`;
  const data = [2, 3].map(row => ({ row, values: { Insectary_ID: `A${row}Z`, Sex: 'male', Wild_Reared: 'Reared', Intro2Insectary_date: 46280 } }));
  const sheets = new LocalSheets({ Insectary_data: data });
  const cell = row => sheets.rows.get('Insectary_data').find(r => r.row === row).cells;
  cell(2)[column('T2_Preservation_medium')] = { userEnteredValue: { formulaValue: OLD(2) } };
  cell(3)[column('T2_Preservation_medium')] = { userEnteredValue: { formulaValue: NEWER(3) } };
  const store = new Store({ localMode: true }, { sheets });
  try {
    await store.sync({ sheets: ['Insectary_data'] });
    const assistant = createAssistant({ store, config: {} });
    store.db
      .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u1','franz','Franz','editor','s','h',1,'2026-01-01')")
      .run();
    store.db.prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')").run(createHash('sha256').update('franz-token').digest('hex'), 'u1');
    const call = async (name, args) =>
      JSON.parse(
        (await assistant.mcp({ authorization: 'Bearer franz-token' }, { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } }))
          .body.result.content[0].text,
      );
    const out = await call('match_notebook', {
      kind: 'emergence',
      year: 2026,
      lines: [2, 3].map(n => ({ raw: `A${n}Z 6/10 unk`, values: { Insectary_ID: `A${n}Z`, Death_date: '6/10', Death_cause: 'Unknown' } })),
    });
    assert.ok(out.proposalId, JSON.stringify(out).slice(0, 300));
    const full = await call('get_proposal', { proposalId: out.proposalId, full: true });
    assert.equal(full.rows[0].values.T2_Preservation_medium, 'NOT_COLLECTED');
    assert.ok(!('T2_Preservation_medium' in full.rows[1].values), 'the newer formula gives it');
    const applied = await assistant.handle({ method: 'POST', path: `/api/chat/proposals/${out.proposalId}/apply`, body: { requestId: randomUUID() }, user, query: { all: '1' } });
    assert.equal(applied.status, 200, JSON.stringify(applied.body).slice(0, 300));
    assert.equal(cell(2)[column('T2_Preservation_medium')].userEnteredValue.stringValue, 'NOT_COLLECTED');
    assert.equal(cell(3)[column('T2_Preservation_medium')].userEnteredValue.formulaValue, NEWER(3));
  } finally {
    store.close();
  }
});
