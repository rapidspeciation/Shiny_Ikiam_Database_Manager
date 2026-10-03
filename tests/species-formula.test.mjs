// SPECIES stays the clutch formula unless what emerged differs: a species equal to
// what the formula gives is never written over it, whether the assistant proposed
// it, the person typed it in the table, or a save sends it.
import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash, randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { applyBatch } from '../server/batch.mjs';
import { moduleMap } from '../server/schema.mjs';
import { createAssistant } from '../server/assistant.mjs';

const user = { id: 'u1', username: 'franz', displayName: 'Franz', role: 'editor' };
const column = key => moduleMap.get('Insectary_data').fields.find(f => f.key === key).column;
const INTERMEDIA = 'Mechanitis messenoides intermedia';
const MESSENOIDES = 'Mechanitis messenoides messenoides';
const DECEPTUS = 'Mechanitis messenoides deceptus';

async function fixture() {
  const sheets = new LocalSheets({
    Insectary_stocks: [
      { row: 2, values: { 'CLUTCH NUMBER': 838, SPECIES: INTERMEDIA } },
      { row: 3, values: { 'CLUTCH NUMBER': 848, SPECIES: MESSENOIDES } },
    ],
    Insectary_data: [
      { row: 2, values: { Insectary_ID: '5VB', 'CLUTCH NUMBER': 838, Sex: 'female' } },
      // Pre-made rows: only the ID, the SPECIES formula gives nothing until a clutch is typed.
      { row: 3, values: { Insectary_ID: '2AB' } },
      { row: 4, values: { Insectary_ID: '3AB' } },
    ],
  });
  for (const [row, value] of [
    [2, INTERMEDIA],
    [3, null],
    [4, null],
  ])
    sheets.rows.get('Insectary_data').find(r => r.row === row).cells[column('SPECIES')] = {
      userEnteredValue: { formulaValue: `=XLOOKUP(C${row},Insectary_stocks!A:A,Insectary_stocks!C:C,"")` },
      ...(value ? { effectiveValue: { stringValue: value } } : {}),
    };
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_stocks', 'Insectary_data'] });
  const cell = row => sheets.rows.get('Insectary_data').find(r => r.row === row).cells[column('SPECIES')];
  const record = row => store.getRecordBySheetRow('Insectary_data', row);
  return { sheets, store, cell, record };
}

test('a save leaves SPECIES to its formula when the value is what the clutch gives', async () => {
  const { store, cell, record } = await fixture();
  try {
    const save = (edits, source) => applyBatch(store, { requestId: randomUUID(), edits }, user, source ? { source } : {});
    // The formula's species again, asked to replace it or not: nothing to write, the formula stays.
    for (const replaceFormula of [['SPECIES'], undefined]) {
      const same = await save([{ id: record(2).id, values: { SPECIES: ` ${INTERMEDIA} ` }, replaceFormula }]);
      assert.equal(same.status, 'unchanged');
    }
    assert.match(cell(2).userEnteredValue.formulaValue, /^=XLOOKUP/);

    // A pre-made row takes its clutch with the clutch's species: only the clutch is written.
    const saved = await save(
      [{ id: record(3).id, values: { 'CLUTCH NUMBER': 838, SPECIES: INTERMEDIA }, replaceFormula: ['SPECIES'] }],
      'ai_approved',
    );
    assert.equal(saved.status, 'verified');
    assert.deepEqual(saved.action.changes.map(c => c.field), ['CLUTCH NUMBER']);
    assert.match(cell(3).userEnteredValue.formulaValue, /^=XLOOKUP/);

    // Another subspecies than the clutch's is written over the formula.
    const other = await save(
      [{ id: record(4).id, values: { 'CLUTCH NUMBER': 848, SPECIES: DECEPTUS }, replaceFormula: ['SPECIES'] }],
      'ai_approved',
    );
    assert.equal(other.status, 'verified');
    assert.equal(cell(4).userEnteredValue.stringValue, DECEPTUS);
  } finally {
    store.close();
  }
});

test("a new row's species equal to its clutch's keeps the formula, asked to replace it or not", async () => {
  const sheets = new LocalSheets({
    Insectary_data: [{ row: 2, values: { Insectary_ID: 'A0A', 'CLUTCH NUMBER': 900, Sex: 'male' } }],
    Insectary_stocks: [{ row: 2, values: { 'CLUTCH NUMBER': 973, SPECIES: 'Mechanitis polymnia proceriformis' } }],
  });
  for (const [row, id] of [
    [3, 'A1A'],
    [4, 'A2A'],
  ]) {
    const cells = [];
    cells[column('Insectary_ID')] = { userEnteredValue: { formulaValue: '=NEXTID()' }, effectiveValue: { stringValue: id } };
    cells[column('SPECIES')] = { userEnteredValue: { formulaValue: `=XLOOKUP(C${row},Insectary_stocks!A:A,Insectary_stocks!C:C,"")` } };
    sheets.rows.get('Insectary_data').push({ row, cells });
  }
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data', 'Insectary_stocks'] });
  try {
    const saved = await applyBatch(
      store,
      {
        requestId: randomUUID(),
        creates: [
          // Without replaceFormula (as the assistant's proposals send new rows): no FORMULA_CELL refusal.
          { module: 'Insectary_data', values: { Insectary_ID: 'A1A', 'CLUTCH NUMBER': 973, Sex: 'female', SPECIES: 'mechanitis polymnia  proceriformis' } },
          {
            module: 'Insectary_data',
            values: { Insectary_ID: 'A2A', 'CLUTCH NUMBER': 973, Sex: 'male', SPECIES: 'Mechanitis polymnia proceriformis' },
            replaceFormula: ['SPECIES'],
          },
        ],
      },
      user,
    );
    assert.equal(saved.status, 'verified');
    for (const row of [3, 4])
      assert.ok(sheets.rows.get('Insectary_data').find(r => r.row === row).cells[column('SPECIES')].userEnteredValue.formulaValue);
  } finally {
    store.close();
  }
});

test('in a proposal, the formula\'s species is dropped (the assistant\'s or typed) and shown as what the formula gives', async () => {
  const { store, record } = await fixture();
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
  const http = (method, path, body = {}) => assistant.handle({ method, path, body, user, query: { all: '1' } });
  try {
    const proposed = await call('propose_changes', {
      reason: 'Emergidos',
      changes: [
        { recordId: record(3).id, values: { 'CLUTCH NUMBER': 838, SPECIES: INTERMEDIA, Sex: 'female' } },
        { recordId: record(4).id, values: { 'CLUTCH NUMBER': 848, SPECIES: DECEPTUS, Sex: 'male' } },
      ],
    });
    assert.ok(proposed.proposalId, JSON.stringify(proposed));
    let [premade, other] = (await http('GET', '/api/chat/proposals')).body.proposals[0].changes;
    assert.ok(!('SPECIES' in premade.values), 'the clutch gives it');
    assert.deepEqual(premade.formulaGives, { SPECIES: INTERMEDIA });
    // What emerged differs: written, with what the formula would give beside it.
    assert.equal(other.values.SPECIES, DECEPTUS);
    assert.deepEqual(other.formulaGives, { SPECIES: MESSENOIDES });

    // The person types the formula's species over the assistant's: not written, the formula stays (no refusal).
    const out = await http('POST', `/api/chat/proposals/${proposed.proposalId}/edit`, {
      cells: [{ key: other.key, field: 'SPECIES', value: MESSENOIDES, before: DECEPTUS }],
    });
    assert.equal(out.status, 200, JSON.stringify(out.body));
    assert.deepEqual(out.body.rejected, []);
    other = out.body.proposal.changes[1];
    assert.ok(!('SPECIES' in other.values));
    assert.equal(other.personEdits.SPECIES.ai, DECEPTUS, "the assistant's value is kept aside");
    assert.deepEqual(other.formulaGives, { SPECIES: MESSENOIDES });

    // Applied: only the clutches and sexes; both SPECIES cells keep their formula.
    const applied = await http('POST', `/api/chat/proposals/${proposed.proposalId}/apply`, { requestId: randomUUID() });
    assert.equal(applied.status, 200, JSON.stringify(applied.body));
    assert.equal(applied.body.status, 'applied');
    assert.deepEqual([record(3).values['CLUTCH NUMBER'], record(4).values.Sex], [838, 'male']);
    for (const row of [3, 4]) assert.ok(record(row).formulas.SPECIES, `row ${row} keeps its formula`);
  } finally {
    store.close();
  }
});
