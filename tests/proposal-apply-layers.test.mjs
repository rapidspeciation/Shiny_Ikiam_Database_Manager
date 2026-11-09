// The checks inside applyProposal (server/assistant.mjs), behind the ones the page's route and the chat's tool
// make first: a person who may not edit is refused there too, and a proposal the assistant revised after it was
// read is not written (the claim's revision check). Nothing is written to the sheet either time.
import test from 'node:test';
import assert from 'node:assert/strict';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { moduleMap } from '../server/schema.mjs';
import { signIn } from './helpers/assistant.mjs';

const SEX = moduleMap.get('Collection_data').fields.find(f => f.key === 'Sex').column;
const base = {
  Purpose: 'Monitoring',
  Collection_location: 'Ikiam',
  Collector: 'FCH - Franz Chandi',
  Release_Collect: 'Mark_Released',
  SPECIES: 'Oleria gunilla',
};

async function setup() {
  const sheets = new LocalSheets({
    Collection_data: [2, 3].map(row => ({ row, values: { ...base, FieldMark_ID: `B${row}`, Sex: 'female' } })),
    Location_data: [{ row: 2, values: { Collection_location: 'Ikiam' } }],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Collection_data', 'Location_data'] });
  const assistant = createAssistant({ store, config: {} });
  const person = signIn(store, assistant);
  const record = row => store.getRecordBySheetRow('Collection_data', row);
  // The cell in the (local) Google Sheet, not the app's copy.
  const sheetSex = row => sheets.cell('Collection_data', row, SEX).userEnteredValue?.stringValue;
  const proposal = id => ({ ...store.db.prepare('SELECT status, revision FROM ai_proposals WHERE id = ?').get(id) });
  const propose = async () => {
    const out = await person.call('propose_changes', { reason: 'B2 is a male', changes: [{ recordId: record(2).id, values: { Sex: 'male' } }] });
    assert.ok(out.proposalId, JSON.stringify(out));
    return out.proposalId;
  };
  return { sheets, store, assistant, person, record, sheetSex, proposal, propose };
}

test('applyProposal: a person whose role became viewer is refused (from the chat and from the page), nothing written', async () => {
  const { store, assistant, person, record, sheetSex, proposal, propose } = await setup();
  try {
    const id = await propose();
    // Made while an editor; the role is lowered before it is applied, and the same chat acts as a viewer.
    store.db.prepare("UPDATE users SET role = 'viewer' WHERE id = ?").run(person.user.id);
    const fromChat = await person.call('apply_proposal', { proposalId: id });
    assert.equal(fromChat.error, 'Your role cannot apply changes.', JSON.stringify(fromChat));
    const fromPage = await assistant.handle({
      method: 'POST',
      path: `/api/chat/proposals/${id}/apply`,
      body: { requestId: 'apply-viewer-1' },
      user: { ...person.user, role: 'viewer' },
      query: {},
      headers: {},
    });
    assert.deepEqual([fromPage.status, fromPage.body.error.code], [403, 'forbidden']);
    assert.deepEqual([sheetSex(2), record(2).values.Sex], ['female', 'female']);
    assert.deepEqual(proposal(id), { status: 'pending', revision: 1 });
    assert.equal(store.db.prepare('SELECT count(*) n FROM actions').get().n, 0, 'no save');
  } finally {
    store.close();
  }
});

test("applyProposal: the assistant revises the proposal after it was read: the claim takes nothing, it is not written", async () => {
  const { store, person, record, sheetSex, proposal, propose } = await setup();
  try {
    const id = await propose();
    // The assistant's worker (another connection to the database) saves a revision between the apply's read of the
    // proposal and its claim: here, just before the claim's statement is prepared.
    const prepare = store.db.prepare.bind(store.db);
    let revised = false;
    store.db.prepare = sql => {
      if (!revised && sql.includes("SET status = 'applying' WHERE id = ? AND status = 'pending'")) {
        revised = true;
        prepare("UPDATE ai_proposals SET revision = revision + 1 WHERE id = ?").run(id);
      }
      return prepare(sql);
    };
    const out = await person.call('apply_proposal', { proposalId: id });
    assert.equal(revised, true, 'the claim was reached');
    assert.equal(out.error, 'Not applied: the proposal was revised while it was being checked. get_proposal, then apply again.');
    assert.deepEqual([sheetSex(2), record(2).values.Sex], ['female', 'female']);
    assert.deepEqual(proposal(id), { status: 'pending', revision: 2 });
    // Read again (the new revision), it applies.
    delete store.db.prepare;
    const again = await person.call('apply_proposal', { proposalId: id });
    assert.equal(again.status, 'applied', JSON.stringify(again));
    assert.deepEqual([sheetSex(2), record(2).values.Sex], ['male', 'male']);
  } finally {
    store.close();
  }
});
