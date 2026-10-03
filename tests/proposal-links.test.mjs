import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { mkdtempSync, mkdirSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { parseDateText } from '../server/schema.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { createApp } from '../server/index.mjs';

// Links that open a proposal beside its chat (#/asistente?propuesta=…&chat=…), and the live list of
// Cambios propuestos sending nothing again when nothing changed but the page's own edits.

const SEED = {
  Insectary_stocks: [
    { row: 2, values: { 'CLUTCH NUMBER': 900, SPECIES: 'Mechanitis lysimnia', 'HATCHING DATE': parseDateText('2026-09-05') } },
    { row: 3, values: { 'CLUTCH NUMBER': 901, SPECIES: 'Mechanitis lysimnia', 'INSECTARY OR LABORATORY': 'Insectary' } },
  ],
  Collection_data: [{ row: 2, values: { Collector: 'FCH - Franz Chandi', SPECIES: 'Oleria onega' } }],
};
const THREAD = '5c8de89d-bbaf-4328-b354-74733e099781';

async function setup(config = {}) {
  const store = new Store({ localMode: true }, { sheets: new LocalSheets(SEED) });
  await store.sync({ sheets: Object.keys(SEED) });
  const assistant = createAssistant({ store, config });
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
      (
        await assistant.mcp(
          { authorization: 'Bearer franz-token' },
          { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } },
        )
      ).body.result.content[0].text,
    );
  const user = { id: 'u-franz', username: 'franz', displayName: 'Franz Chandi', role: 'editor' };
  const http = (method, path, { body = {}, query = {}, page = null } = {}) =>
    assistant.handle({ method, path, body, user, query, page });
  const stock = row => store.getRecordBySheetRow('Insectary_stocks', row);
  return { store, call, http, stock };
}

test('proposal results carry a link that opens the proposal beside its chat', async () => {
  const { store, call, stock } = await setup({ publicUrl: 'https://ithomiini-ikiam.com/' });
  try {
    const made = await call('propose_changes', {
      reason: 'Insectario',
      changes: [{ recordId: stock(2).id, values: { 'INSECTARY OR LABORATORY': 'Laboratory' } }],
    });
    assert.equal(made.link, `https://ithomiini-ikiam.com/#/asistente?propuesta=${made.proposalId}`);
    // Once the proposal is known to be a T3 chat's, its links name the chat too.
    store.db.prepare('UPDATE ai_proposals SET t3_thread = ? WHERE id = ?').run(THREAD, made.proposalId);
    const chatLink = `https://ithomiini-ikiam.com/#/asistente?propuesta=${made.proposalId}&chat=${THREAD}`;
    assert.equal((await call('get_proposal', { proposalId: made.proposalId })).link, chatLink);
    const revised = await call('update_proposal', { proposalId: made.proposalId, rows: [{ index: 0, values: { NOTES: 'ok' } }] });
    assert.equal(revised.link, chatLink, JSON.stringify(revised));
    // A notebook page matched: its proposal's link; matched again in place: the same proposal and chat.
    const page = {
      kind: 'stocks',
      year: 2026,
      lines: [{ raw: '901 larvas sanas', values: { 'CLUTCH NUMBER': '901', NOTES: 'larvas sanas' } }],
    };
    const matched = await call('match_notebook', page);
    assert.equal(matched.link, `https://ithomiini-ikiam.com/#/asistente?propuesta=${matched.proposalId}`, JSON.stringify(matched));
    store.db.prepare('UPDATE ai_proposals SET t3_thread = ? WHERE id = ?').run(THREAD, matched.proposalId);
    const again = await call('match_notebook', { ...page, replaceProposalId: matched.proposalId });
    assert.equal(again.link, `https://ithomiini-ikiam.com/#/asistente?propuesta=${matched.proposalId}&chat=${THREAD}`);
  } finally {
    store.close?.();
  }
  // Without the app's address: the link within the app.
  const plain = await setup();
  const made = await plain.call('propose_changes', {
    reason: 'x',
    changes: [{ recordId: plain.stock(3).id, values: { 'INSECTARY OR LABORATORY': 'Laboratory' } }],
  });
  assert.equal(made.link, `#/asistente?propuesta=${made.proposalId}`);
});

test('the live list: "unchanged" when nothing changed but this page\'s own edits', async () => {
  const { call, http, stock } = await setup({ proposalWaitMs: 60 });
  const made = await call('propose_changes', {
    reason: 'Insectario',
    changes: [{ recordId: stock(2).id, values: { 'INSECTARY OR LABORATORY': 'Laboratory' } }],
  });
  const list = (page, query = {}) => http('GET', '/api/chat/proposals', { page, query: { all: '1', chat: 'all', ...query } });
  // The first request: the whole list, tagged (a reload with nothing new is a 304), with its stamp.
  const first = await list('A');
  assert.equal(first.tagged, true);
  assert.ok(first.body.revision && first.body.stamp);
  assert.equal(first.body.proposals.length, 1);
  const held = { wait: '1', revision: first.body.revision, stamp: first.body.stamp };
  // Nothing changed: the long poll ends with only that.
  const quiet = await list('A', held);
  assert.deepEqual(quiet.body, { unchanged: true, revision: first.body.revision, stamp: first.body.stamp });
  assert.ok(!quiet.tagged);
  // Another stamp (the page shows other chats or titles): the whole list.
  assert.equal((await list('A', { ...held, stamp: 'other' })).body.proposals.length, 1);

  // Page A edits a cell: A's waiting request is not woken, B's is (with the whole list).
  let doneA = false;
  const waitA = list('A', held).then(out => ((doneA = true), out));
  const waitB = list('B', held);
  const edit = await http('POST', `/api/chat/proposals/${made.proposalId}/edit`, {
    page: 'A',
    body: { cells: [{ key: stock(2).id, field: 'NOTES', value: 'larvas sanas' }] },
  });
  assert.equal(edit.status, 200, JSON.stringify(edit.body));
  const outB = await waitB;
  assert.equal(outB.body.proposals.length, 1);
  assert.notEqual(outB.body.revision, first.body.revision);
  assert.equal(doneA, false, "A's own edit does not wake A");
  const outA = await waitA;
  assert.deepEqual(outA.body, { unchanged: true, revision: outB.body.revision, stamp: first.body.stamp });
  // A page that held the list before the edit, asking now: A has it already, B does not.
  assert.equal((await list('A', held)).body.unchanged, true);
  assert.equal((await list('B', held)).body.proposals.length, 1);
  // Anyone else's change after A's edit: the whole list for A too.
  await call('update_proposal', { proposalId: made.proposalId, rows: [{ index: 0, values: { 'INSECTARY OR LABORATORY': 'Insectary' } }] });
  assert.equal((await list('A', held)).body.proposals.length, 1);
});

test('the app: T3 status names its environment, and the first list answers 304 when unchanged', async () => {
  const home = mkdtempSync(join(tmpdir(), 'ithomiini-t3-'));
  mkdirSync(join(home, 'userdata'));
  const ENV = '4e6c4765-8cfa-4adc-b761-3c3bae2ae7e0';
  writeFileSync(join(home, 'userdata', 'environment-id'), `${ENV}\n`);
  const store = new Store({ localMode: true }, { sheets: new LocalSheets(SEED) });
  await store.sync({ sheets: Object.keys(SEED) });
  const app = await createApp(
    {
      localMode: true,
      secureCookies: false,
      setupToken: 'test-setup-secret',
      syncIntervalMs: 0,
      t3: { url: 'https://t3.example.org', local: 'http://127.0.0.1:9', home },
    },
    { store, skipInitialSync: true },
  );
  const address = await app.listen(0, '127.0.0.1');
  try {
    const api = `http://127.0.0.1:${address.port}/ithomiini/api`;
    const setupAnswer = await fetch(`${api}/auth/setup`, {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({ token: 'test-setup-secret', username: 'testadmin', password: 'test12', displayName: 'Test' }),
    });
    const cookie = setupAnswer.headers.get('set-cookie').split(';')[0];
    assert.deepEqual(await (await fetch(`${api}/t3/status`, { headers: { cookie } })).json(), {
      url: 'https://t3.example.org',
      environmentId: ENV,
    });
    const path = `${api}/chat/proposals?all=1&chat=auto&wait=1&revision=`;
    const first = await fetch(path, { headers: { cookie } });
    assert.equal(first.status, 200);
    const etag = first.headers.get('etag');
    assert.ok(etag);
    assert.deepEqual((await first.json()).proposals, []);
    assert.equal((await fetch(path, { headers: { cookie, 'if-none-match': etag } })).status, 304);
  } finally {
    await app.close();
    rmSync(home, { recursive: true, force: true });
  }
});
