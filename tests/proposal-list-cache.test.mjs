// Cambios propuestos keeps each proposal's table as last built and builds it again only when
// something it reads changed: the proposal, the local copy (whatever writes it), its history.
import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createAssistant } from '../server/assistant.mjs';

const user = { id: 'u-franz', username: 'franz', displayName: 'Franz', role: 'editor' };

async function setup() {
  const sheets = new LocalSheets({
    Insectary_data: [
      { row: 2, values: { Insectary_ID: '1AA', Sex: 'female' } },
      { row: 3, values: { Insectary_ID: '2AA', Sex: 'male' } },
    ],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data'] });
  const assistant = createAssistant({ store, config: {} });
  store.db
    .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES(?,?,?,?,'s','h',1,'2026-01-01')")
    .run(user.id, user.username, user.displayName, user.role);
  store.db
    .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
    .run(createHash('sha256').update('franz-token').digest('hex'), user.id);
  const call = async (name, args) =>
    JSON.parse(
      (await assistant.mcp({ authorization: 'Bearer franz-token' }, { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } }))
        .body.result.content[0].text,
    );
  const list = async () => (await assistant.handle({ method: 'GET', path: '/api/chat/proposals', user, query: { all: '1' } })).body.proposals;
  return { store, call, list };
}

test('the list keeps a proposal as built until it, or the copy it reads, changes', async () => {
  const { store, call, list } = await setup();
  try {
    const record = store.getRecordBySheetRow('Insectary_data', 2);
    const out = await call('propose_changes', { reason: 'sexo', changes: [{ recordId: record.id, values: { Sex: 'male' } }] });
    assert.ok(out.proposalId, JSON.stringify(out));
    const [first] = await list();
    assert.equal(first.changes[0].rowValues.Sex, 'female');
    let [p] = await list();
    assert.equal(p, first, 'nothing changed: the same view, not built again');

    // Typed in the sheet and saved to the copy: built again with the sheet's value.
    store.persistRecord({ ...record, values: { ...record.values, Sex: 'male' }, version: record.version + 1, updatedAt: new Date().toISOString() });
    [p] = await list();
    assert.notEqual(p, first);
    assert.equal(p.changes[0].rowValues.Sex, 'male');

    // A row moved by a write that leaves updated_at alone (a record moved aside): seen too.
    const before = p;
    store.db.prepare('UPDATE records SET row_num=? WHERE id=?').run(40, record.id);
    [p] = await list();
    assert.notEqual(p, before);
    assert.equal(p.changes[0].row, 40);

    // The proposal itself revised: built again.
    const again = p;
    await call('update_proposal', { proposalId: out.proposalId, rows: [{ index: 0, values: { Sex: 'female' } }] });
    [p] = await list();
    assert.notEqual(p, again);
  } finally {
    store.close();
  }
});
