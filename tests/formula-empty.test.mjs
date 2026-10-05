// A row's formula cells in a proposal (5 Oct 2026): SPECIES comes from the clutch's formula,
// and where that formula gives nothing (a clutch without its species in Insectary_stocks yet) the species
// from the notebook is written, shown as typed, and a row left without one is pointed out;
// Pedigree and the other formulas show what they will give, also on a row whose record a
// sync replaced (the table read the old record, at no sheet row, and showed nothing).
import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash, randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { moduleMap } from '../server/schema.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { buildReview, checkTranscription } from '../server/notebook.mjs';

const user = { id: 'u1', username: 'franz', displayName: 'Franz', role: 'editor' };
const column = key => moduleMap.get('Insectary_data').fields.find(f => f.key === key).column;
const LYSIMNIA = 'Mechanitis lysimnia';
// The Insectary_data formulas as the sheet has them.
const FORMULAS = {
  SPECIES: r => `=IFS(C${r}="","",C${r}="NA","",OR(C${r}<>"",C${r}<>"NA"),XLOOKUP(C${r},Insectary_stocks!A:A,Insectary_stocks!C:C,""))`,
  Pedigree: r =>
    `=IFS(K${r}="","",K${r}="NA","NA",K${r}="F1/F2 mutation rate","YES or NO",K${r}="WEST x EAST polymnia crosses","YES or NO",K${r}="polymnia x lysimnia crosses","YES or NO",TRUE,"NA")`,
};

async function fixture() {
  const sheets = new LocalSheets({
    Insectary_stocks: [
      { row: 2, values: { 'CLUTCH NUMBER': 1006, SPECIES: LYSIMNIA } },
      // Registered from the clutches notebook, its species not written yet.
      { row: 3, values: { 'CLUTCH NUMBER': '1012(2)' } },
    ],
    Insectary_data: [
      { row: 2, values: { Insectary_ID: 'P4E', 'CLUTCH NUMBER': 1006, Sex: 'female' } },
      // Pre-made rows: only their ID and formulas.
      { row: 3, values: { Insectary_ID: 'P5E' } },
      { row: 4, values: { Insectary_ID: 'P6E' } },
      { row: 5, values: { Insectary_ID: 'P7E' } },
    ],
  });
  for (const row of [2, 3, 4, 5]) {
    const cells = sheets.rows.get('Insectary_data').find(r => r.row === row).cells;
    for (const [field, f] of Object.entries(FORMULAS))
      cells[column(field)] = { userEnteredValue: { formulaValue: f(row) }, ...(row === 2 && field === 'SPECIES' ? { effectiveValue: { stringValue: LYSIMNIA } } : {}) };
  }
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_stocks', 'Insectary_data'] });
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
  const record = row => store.getRecordBySheetRow('Insectary_data', row);
  const cell = (row, field) => sheets.rows.get('Insectary_data').find(r => r.row === row).cells[column(field)];
  return { store, sheets, assistant, call, http, record, cell };
}

test('a clutch without its species in Insectary_stocks: the species is written over the formula, shown as typed; a row without one is pointed out', async () => {
  const { store, assistant, call, http, record, cell } = await fixture();
  try {
    const out = await call('propose_changes', {
      reason: 'Emergidos',
      changes: [
        // Registered: the formula gives the species, typed or not.
        { recordId: record(3).id, values: { 'CLUTCH NUMBER': 1006, SPECIES: LYSIMNIA, Research_purpose: 'F1/F2 mutation rate' } },
        // Without its species there: the notebook's species stays in the proposal.
        { recordId: record(4).id, values: { 'CLUTCH NUMBER': '1012(2)', SPECIES: LYSIMNIA, Research_purpose: 'F1/F2 mutation rate' } },
        // No species given either: said in lookAt.
        { recordId: record(5).id, values: { 'CLUTCH NUMBER': '1012(2)', Research_purpose: 'NA' } },
      ],
    });
    assert.ok(out.proposalId, JSON.stringify(out));
    assert.deepEqual(out.lookAt.formulaEmpty, [
      {
        field: 'SPECIES',
        clutch: '1012(2)',
        problem: 'Its formula gives nothing for clutch 1012(2) (not in Insectary_stocks yet, or without SPECIES there): write SPECIES from the notebook',
        rows: ['P7E'],
      },
    ]);
    const [registered, typed, empty] = (await http('GET', '/api/chat/proposals')).body.proposals[0].changes;
    assert.ok(!('SPECIES' in registered.values), 'left to the formula');
    assert.deepEqual(registered.formulaGives, { SPECIES: LYSIMNIA, Pedigree: 'YES or NO' });
    assert.equal(typed.values.SPECIES, LYSIMNIA, 'typed: the formula gives nothing');
    assert.deepEqual(typed.replaceFormula, ['SPECIES']);
    assert.deepEqual(typed.formulaGives, { Pedigree: 'YES or NO' });
    assert.deepEqual(empty.formulaGives, { Pedigree: 'NA' });

    // A new row past the pre-made rows (they are made when it is applied): the formulas of the sheet's last row.
    const ahead = await call('propose_changes', {
      reason: 'Emergidos',
      newRows: [{ sheet: 'Insectary_data', values: { Insectary_ID: 'P9E', 'CLUTCH NUMBER': 1006, Research_purpose: 'NA' } }],
    });
    assert.ok(ahead.proposalId, JSON.stringify(ahead));
    const [created] = (await http('GET', '/api/chat/proposals')).body.proposals.find(p => p.id === ahead.proposalId).changes;
    assert.deepEqual(created.formulaGives, { SPECIES: LYSIMNIA, Pedigree: 'NA' });
    await http('POST', `/api/chat/proposals/${ahead.proposalId}/discard`);

    const applied = await http('POST', `/api/chat/proposals/${out.proposalId}/apply`, { requestId: randomUUID() });
    assert.equal(applied.status, 200, JSON.stringify(applied.body));
    assert.ok(cell(3, 'SPECIES').userEnteredValue.formulaValue, 'the formula stays where it gives the species');
    assert.equal(cell(4, 'SPECIES').userEnteredValue.stringValue, LYSIMNIA);
  } finally {
    assistant.close();
    store.close();
  }
});

test("a row whose record a sync replaced shows the sheet's row now and what its formulas will give", async () => {
  const { store, sheets, assistant, call, http, record } = await fixture();
  try {
    const old = record(3);
    const out = await call('propose_changes', {
      reason: 'Emergidos',
      changes: [{ recordId: old.id, values: { 'CLUTCH NUMBER': 1006, Research_purpose: 'F1/F2 mutation rate', Sex: 'male' } }],
    });
    assert.ok(out.proposalId, JSON.stringify(out));
    // Someone types a CAM on the pre-made row and a sync (before it kept the record) made the row a new
    // record: the old one is set aside, at no sheet row.
    const fresh = randomUUID();
    await sheets.externalEdit('Insectary_data', 3, { CAM_ID: 'CAM079001' });
    store.db.prepare('UPDATE records SET missing = 1, row_num = -3 WHERE id = ?').run(old.id);
    store.db
      .prepare(
        "INSERT INTO records(id,sheet,row_num,values_json,formulas_json,identity_json,label,version,updated_at,missing,observed) VALUES(?,'Insectary_data',3,?,?,?,'P5E',1,?,0,1)",
      )
      .run(fresh, JSON.stringify({ ...old.values, CAM_ID: 'CAM079001' }), JSON.stringify(old.formulas), JSON.stringify({ Insectary_ID: 'P5E', CAM_ID: 'CAM079001' }), new Date().toISOString());
    const [row] = (await http('GET', '/api/chat/proposals')).body.proposals[0].changes;
    assert.equal(row.recordId, fresh);
    assert.equal(row.key, old.id, 'its key in the table stays');
    assert.equal(row.rowValues.CAM_ID, 'CAM079001');
    assert.deepEqual(row.formulaGives, { SPECIES: LYSIMNIA, Pedigree: 'YES or NO' });
    assert.equal(row.formulaFallback, undefined);
    // The person edits it in the table by its key, and it is applied to the row as it is now.
    const edited = await http('POST', `/api/chat/proposals/${out.proposalId}/edit`, { cells: [{ key: row.key, field: 'Sex', value: 'female', before: 'male' }] });
    assert.equal(edited.status, 200, JSON.stringify(edited.body));
    const applied = await http('POST', `/api/chat/proposals/${out.proposalId}/apply`, { requestId: randomUUID() });
    assert.equal(applied.status, 200, JSON.stringify(applied.body));
    assert.equal(record(3).values.Sex, 'female');
  } finally {
    assistant.close();
    store.close();
  }
});

test('a notebook line: the species written where the formula gives nothing for the clutch the row takes', () => {
  const rows = [{ id: 'r1', row: 3, version: 1, values: { Insectary_ID: 'P5E', SPECIES: null } }];
  const lookup = {
    find: ([key]) => rows.filter(r => r.values.Insectary_ID === key).map(r => ({ ...r, formulas: { SPECIES: '=X' }, label: r.id })),
    clutch: value => ({ 1006: 1006 })[String(value)] ?? null,
    // 1012(2) is not in Insectary_stocks yet.
    speciesOfClutch: value => ({ 1006: LYSIMNIA })[String(value)] ?? null,
    list: () => undefined,
    holder: () => null,
    newRowFormulas: new Set(),
    typedOverFormula: new Set(['SPECIES']),
  };
  const read = clutch =>
    buildReview({
      transcription: checkTranscription({
        kind: 'emergence',
        lines: [{ raw: `P5E lysimnia ${clutch}`, values: { Insectary_ID: 'P5E', SPECIES: LYSIMNIA, 'CLUTCH NUMBER': clutch } }],
      }).transcription,
      today: '2026-10-05',
      initials: 'FCH',
      lookup,
    }).lines[0].cells.SPECIES;
  const unregistered = read('1012(2)');
  assert.equal(unregistered.status, 'fill');
  assert.ok(unregistered.include);
  const registered = read('1006');
  assert.equal(registered.status, 'same', 'the formula gives it');
  assert.ok(!registered.include);
});
