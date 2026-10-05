// The assistant while Google does not answer: apply_proposal says the save waits in the app
// (the proposal is `queued`, then applied when it is written), its tools say the workbook's
// state, and new rows never take an ID someone holds in an Emergidos entry kept in the app.
import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash, randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createAssistant } from '../server/assistant.mjs';

async function fixture() {
  const sheets = new LocalSheets(
    {
      Insectary_data: [
        { row: 2, values: { Insectary_ID: 'K2B', Sex: 'female', SPECIES: 'Ithomia salapia' } },
        { row: 3, values: { Insectary_ID: 'K3B' } },
        { row: 4, values: { Insectary_ID: 'K4B' } },
      ],
    },
    { health: { probeMs: 20 } },
  );
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data'] });
  const assistant = createAssistant({ store, config: {} });
  for (const [id, name] of [['u1', 'Franz'], ['u2', 'Ana']])
    store.db
      .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES(?,?,?,'editor','s','h',1,'2026-01-01')")
      .run(id, name.toLowerCase(), name);
  store.db.prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')").run(createHash('sha256').update('franz-token').digest('hex'), 'u1');
  const call = async (name, args) =>
    JSON.parse(
      (await assistant.mcp({ authorization: 'Bearer franz-token' }, { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } }))
        .body.result.content[0].text,
    );
  return { store, sheets, assistant, call };
}
const until = async (check, ms = 3000) => {
  const end = Date.now() + ms;
  while (!check()) {
    if (Date.now() > end) throw new Error('timed out');
    await new Promise(resolve => setTimeout(resolve, 10));
  }
};

test('apply_proposal while the workbook is busy: queued, said so, then applied when Google answers', async () => {
  const { store, sheets, assistant, call } = await fixture();
  try {
    const at = row => store.getRecordBySheetRow('Insectary_data', row).id;
    const { proposalId } = await call('propose_changes', { reason: 'Sexo', changes: [{ recordId: at(2), values: { Sex: 'male' } }] });
    sheets.simulateBusy({ minutes: 1, delayMs: 0 });
    await sheets.probe().catch(() => {});
    const out = await call('apply_proposal', { proposalId });
    assert.equal(out.status, 'queued');
    assert.match(out.queued, /written automatically/);
    assert.equal(out.google.workbook, 'busy');
    assert.equal((await call('get_proposal', { proposalId })).status, 'queued');
    // Applying again does not write twice.
    assert.match((await call('apply_proposal', { proposalId })).error, /already waiting for Google/);
    sheets.simulateBusy({ minutes: 0 });
    await sheets.health.runProbe();
    await until(() => store.db.prepare('SELECT status FROM ai_proposals WHERE id = ?').get(proposalId).status === 'applied');
    assert.equal(store.getRecordBySheetRow('Insectary_data', 2).values.Sex, 'male');
  } finally {
    assistant.close();
    store.close();
  }
});

test('a new row with an Insectary ID someone holds in an Emergidos entry is not proposed', async () => {
  const { store, assistant, call } = await fixture();
  try {
    const ana = { id: 'u2', username: 'ana', role: 'editor' };
    const staged = await store.staged.stage(
      {
        requestId: randomUUID(),
        purpose: 'emergidos',
        creates: [{ clientId: 'c1', module: 'Insectary_data', values: { Insectary_ID: 'K3B', Sex: 'male', Intro2Insectary_date: 46300 } }],
      },
      ana,
    );
    assert.equal(staged.status, 'staged');
    const out = await call('propose_changes', {
      reason: 'Emergidos',
      newRows: [{ sheet: 'Insectary_data', values: { Insectary_ID: 'K3B', Sex: 'female' } }],
    });
    assert.match(out.error, /K3B is held by Ana's entry/);
    // Its pre-made row, edited as an existing row: the same.
    const edit = await call('propose_changes', { changes: [{ recordId: store.getRecordBySheetRow('Insectary_data', 3).id, values: { Sex: 'female' } }] });
    assert.match(edit.error, /K3B is held by Ana's entry/);
    const ok = await call('propose_changes', { reason: 'Emergidos', newRows: [{ sheet: 'Insectary_data', values: { Insectary_ID: 'K4B', Sex: 'female' } }] });
    assert.ok(ok.proposalId);
    assert.match(ok.staged, /1 Emergidos\/Clutches changes are kept in the app/);
  } finally {
    assistant.close();
    store.close();
  }
});
