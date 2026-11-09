// A T3 Code chat keeps its token for the whole chat (server/assistant.mjs, agentTurn): each call acts with the
// person as they are now in the users table, not as they were at the chat's first call. A person lowered to
// observer is refused the same chat's apply; a deactivated person, or a revoked token, gets no answer at all.
// Nothing is written to the sheet either time.
import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
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
  const sheetSex = row => sheets.cell('Collection_data', row, SEX).userEnteredValue?.stringValue;
  const proposal = id => ({ ...store.db.prepare('SELECT status FROM ai_proposals WHERE id = ?').get(id) });
  const setUser = (fields, value) => store.db.prepare(`UPDATE users SET ${fields} = ? WHERE id = ?`).run(value, person.user.id);
  const propose = async () => {
    const out = await person.call('propose_changes', { reason: 'B2 is a male', changes: [{ recordId: record(2).id, values: { Sex: 'male' } }] });
    assert.ok(out.proposalId, JSON.stringify(out));
    return out.proposalId;
  };
  const unchanged = () => assert.deepEqual([sheetSex(2), record(2).values.Sex], ['female', 'female']);
  return { store, person, sheetSex, record, proposal, setUser, propose, unchanged };
}

test('T3 chat: the role lowered from editor to observer mid-chat, the same chat (same token) cannot apply or propose', async () => {
  const { store, person, sheetSex, record, proposal, setUser, propose, unchanged } = await setup();
  try {
    const id = await propose();
    setUser('role', 'observer');
    const applied = await person.call('apply_proposal', { proposalId: id });
    assert.equal(applied.error, 'Your role cannot apply changes.', JSON.stringify(applied));
    const proposed = await person.call('propose_changes', { reason: 'B3', changes: [{ recordId: record(3).id, values: { Sex: 'male' } }] });
    assert.equal(proposed.error, 'Your role cannot propose edits', JSON.stringify(proposed));
    unchanged();
    assert.deepEqual(proposal(id), { status: 'pending' });
    assert.equal(store.db.prepare('SELECT count(*) n FROM actions').get().n, 0, 'no save');
    // Made editor again, the same chat applies it.
    setUser('role', 'editor');
    const again = await person.call('apply_proposal', { proposalId: id });
    assert.equal(again.status, 'applied', JSON.stringify(again));
    assert.deepEqual([sheetSex(2), record(2).values.Sex], ['male', 'male']);
  } finally {
    store.close();
  }
});

test('T3 chat: a deactivated person, or a revoked token, is refused mid-chat (401), nothing written', async () => {
  const { store, person, proposal, setUser, propose, unchanged } = await setup();
  try {
    const id = await propose();
    const apply = () => person.mcp('tools/call', { name: 'apply_proposal', arguments: { proposalId: id } });
    setUser('active', 0);
    assert.equal((await apply()).status, 401);
    setUser('active', 1);
    store.db
      .prepare("UPDATE ai_tokens SET revoked_at = '2026-11-08' WHERE token_hash = ?")
      .run(createHash('sha256').update(person.token).digest('hex'));
    assert.equal((await apply()).status, 401);
    unchanged();
    assert.deepEqual(proposal(id), { status: 'pending' });
  } finally {
    store.close();
  }
});
