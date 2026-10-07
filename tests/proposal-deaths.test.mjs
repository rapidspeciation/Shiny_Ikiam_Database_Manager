// A notebook page's table marks the butterflies already dead in the sheet (sheetDeath) and those
// this proposal kills (diesHere) on their ID, read from the sheet's copy each time the table is
// built: the person marks those lines in the paper notebook.
import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { parseDateText } from '../server/schema.mjs';
import { createAssistant, deathMark } from '../server/assistant.mjs';

const d = text => parseDateText(text);
const TEMPLATE = Object.fromEntries(
  ['CAM_ID', 'Tube_1_id', 'Research_purpose', 'Preservation_date', 'Tube_2_id', 'Tube_3_id', 'Tube_4_id', 'Preserved_Dead_Alive', 'Location_body']
    .map(f => [f, 'NA'])
    .concat(['Tube_1_tissue', 'T1_Preservation_medium', 'Tube_2_tissue', 'T2_Preservation_medium', 'Tube_3_tissue', 'Tube_4_tissue', 'Preservation_medium'].map(f => [f, 'NOT_COLLECTED'])),
);

async function setup() {
  const sheets = new LocalSheets({
    Insectary_data: [
      // Dead and with its death's template already: a line as in the sheet.
      { row: 2, values: { Insectary_ID: '1AA', Sex: 'male', Death_date: d('2026-09-28'), Death_cause: 'Unknown', ...TEMPLATE } },
      { row: 3, values: { Insectary_ID: '2AA', Sex: 'female' } },
      { row: 4, values: { Insectary_ID: '3AA' } },
    ],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data'] });
  const assistant = createAssistant({ store, config: {} });
  store.db
    .prepare(
      "INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u-franz','franz','Franz Chandi','editor','s','h',1,'2026-01-01')",
    )
    .run();
  store.db
    .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
    .run(createHash('sha256').update('franz-token').digest('hex'), 'u-franz');
  const call = async (name, args) =>
    JSON.parse(
      (await assistant.mcp({ authorization: 'Bearer franz-token' }, { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } }))
        .body.result.content[0].text,
    );
  const user = { id: 'u-franz', username: 'franz', displayName: 'Franz Chandi', role: 'editor' };
  // The proposal as Cambios propuestos shows it (pending or applied).
  const listed = async id => {
    const { body } = await assistant.handle({ method: 'GET', path: '/api/chat/proposals', user, query: { only: id, all: '1' } });
    return [...(body.proposals ?? []), ...(body.reviewed ?? [])].find(p => p.id === id).changes;
  };
  return { store, call, listed };
}

const PAGE = {
  kind: 'emergence',
  year: 2026,
  includeUnchanged: true,
  lines: [
    { raw: '1AA ♂', values: { Insectary_ID: '1AA', Sex: 'male' } },
    { raw: '2AA 30/9 unk', values: { Insectary_ID: '2AA', Death_date: '30/9', Death_cause: 'Unknown' } },
    { raw: '3AA ♀', values: { Insectary_ID: '3AA', Sex: 'female' } },
  ],
};

test("a page's table marks the butterflies dead in the sheet and those dying through it, after a rebuild and once applied", async () => {
  const { store, call, listed } = await setup();
  try {
    const out = await call('match_notebook', PAGE);
    assert.ok(out.proposalId, JSON.stringify(out));
    const marks = async () => Object.fromEntries((await listed(out.proposalId)).map(c => [c.label, [c.sheetDeath ?? null, c.diesHere ?? null, !!c.context]]));
    const expected = {
      '1AA': [{ date: d('2026-09-28'), cause: 'Unknown' }, null, true],
      '2AA': [null, { date: d('2026-09-30'), cause: 'Unknown' }, false],
      '3AA': [null, null, false],
    };
    assert.deepEqual(await marks(), expected, 'a context row dead in the sheet is marked too');
    // Matched again: the marks come from the sheet, not from the proposal's rows.
    await call('match_notebook', { ...PAGE, replaceProposalId: out.proposalId });
    assert.deepEqual(await marks(), expected);
    // Applied: the death it wrote is still this proposal's.
    const applied = await call('apply_proposal', { proposalId: out.proposalId });
    assert.equal(applied.status, 'applied', JSON.stringify(applied));
    assert.equal(store.getRecordBySheetRow('Insectary_data', 3).values.Death_date, d('2026-09-30'));
    assert.deepEqual((await marks())['2AA'].slice(0, 2), [null, { date: d('2026-09-30'), cause: 'Unknown' }]);
  } finally {
    store.close();
  }
});

test('deathMark: only Insectary_data rows with a death date; a pending proposal shows the sheet first', () => {
  const dead = { values: { Death_date: 46000, Death_cause: 'Eaten' } };
  const alive = { values: {} };
  const row = (values, more = {}) => ({ sheet: 'Insectary_data', values, ...more });
  assert.deepEqual(deathMark(row({}), dead, true), { sheetDeath: { date: 46000, cause: 'Eaten' } });
  assert.deepEqual(deathMark(row({ Death_date: 46001 }), dead, true), { sheetDeath: { date: 46000, cause: 'Eaten' } });
  assert.deepEqual(deathMark(row({ Death_date: 46001 }), alive, true), { diesHere: { date: 46001, cause: null } });
  assert.deepEqual(deathMark(row({ Death_date: 46001, Death_cause: 'Spider' }), dead, false), { diesHere: { date: 46001, cause: 'Spider' } });
  assert.equal(deathMark(row({ Death_date: 'NA' }), alive, true), null);
  assert.equal(deathMark(row({ Death_date: 46001 }, { context: true }), alive, true), null, 'a context row writes nothing');
  assert.equal(deathMark({ sheet: 'Collection_data', values: {} }, dead, true), null);
  assert.equal(deathMark(row({}, { placeholder: true }), null, true), null);
});
