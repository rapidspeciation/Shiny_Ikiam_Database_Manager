import test from 'node:test';
import assert from 'node:assert/strict';
import { DatabaseSync } from 'node:sqlite';
import { createHash } from 'node:crypto';
import { createAssistant } from '../server/assistant.mjs';
import { createReports } from '../server/reports.mjs';

const alice = { id: 'alice', role: 'editor' };
const bob = { id: 'bob', role: 'viewer' };
const specimen = {
  id: 'r-1',
  sheet: 'Insectary_data',
  row: 9,
  version: 3,
  label: 'A0A',
  values: { Insectary_ID: 'A0A', Sex: 'Female', Research_purpose: '' },
  formulas: {},
  sourceUrl: 'https://example.test/sheet#range=A9',
};
const stock = {
  id: 's-1',
  sheet: 'Insectary_stocks',
  row: 4,
  version: 1,
  values: {
    'CLUTCH NUMBER': 944,
    'NUMBER OF EGGS': 12,
    'NUMBER OF LARVAE': 10,
    'NUMBER OF PUPA': 4,
    'NUMBER OF ADULTS': '',
  },
};

function fixture(config = {}, applyHook) {
  const db = new DatabaseSync(':memory:');
  let applied;
  const records = structuredClone([specimen, stock]);
  const store = {
    db,
    listModules: async () => [
      { id: 'insectary', sheet: 'Insectary_data' },
      { id: 'stocks', sheet: 'Insectary_stocks' },
    ],
    searchRecords: async ({ module, q, limit = 500, offset = 0 }) => {
      const found = records.filter(
        record =>
          (!module ||
            module === record.sheet ||
            module === (record.sheet === 'Insectary_data' ? 'insectary' : 'stocks')) &&
          (!q || JSON.stringify(record).toLowerCase().includes(q.toLowerCase())),
      );
      return { records: found.slice(offset, offset + limit), total: found.length };
    },
    getRecord: id => records.find(record => record.id === id),
    applyProposal: async (changes, options) => {
      applied = { changes, options };
      return applyHook ? applyHook(changes, options, records) : { records: [records[0]], status: 'verified' };
    },
  };
  const assistant = createAssistant({
    store,
    config: {
      ai: {
        baseUrl: 'https://mock.example/v1',
        model: 'test-model',
        apiKey: 'fake',
        visionModel: 'vision-model',
        transcriptionModel: 'audio-model',
        transcriptionMode: 'chat',
      },
      ...config,
    },
  });
  // The people, and the personal tokens their T3 Code chats call the tools with (scripts/t3-provision.mjs).
  db.exec('CREATE TABLE users (id TEXT PRIMARY KEY, username TEXT, display_name TEXT, role TEXT, active INTEGER)');
  for (const user of [alice, bob]) {
    db.prepare('INSERT INTO users VALUES (?,?,?,?,1)').run(user.id, user.id, user.id, user.role);
    db.prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')").run(
      createHash('sha256').update(`${user.id}-token`).digest('hex'),
      user.id,
    );
  }
  /** A tool called from a T3 chat of this person. */
  const call = async (user, name, args) => {
    const out = await assistant.mcp(
      { authorization: `Bearer ${user.id}-token` },
      { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } },
    );
    return JSON.parse(out.body.result.content[0].text);
  };
  /** A proposal as Cambios propuestos lists it. */
  const listed = async (user, id) =>
    (await assistant.handle({ method: 'GET', path: '/api/chat/proposals', user, query: { all: '1', only: id } })).body.proposals[0];
  return { assistant, db, call, listed, getApplied: () => applied };
}

function fakeProvider(replies) {
  const original = globalThis.fetch;
  const requests = [];
  globalThis.fetch = async (_url, options) => {
    requests.push(JSON.parse(options.body));
    return { ok: true, json: async () => ({ choices: [{ message: replies.shift() }] }) };
  };
  return {
    requests,
    restore: () => {
      globalThis.fetch = original;
    },
  };
}

test("proposals need a sign-in (the tools a person's token) and stay with their owner", async () => {
  const { assistant, db, call, listed } = fixture();
  assert.equal((await assistant.handle({ method: 'GET', path: '/api/chat/proposals' })).status, 401);
  const ask = { jsonrpc: '2.0', id: 1, method: 'tools/list' };
  assert.equal((await assistant.mcp({}, ask)).status, 401);
  assert.equal((await assistant.mcp({ authorization: 'Bearer nobody' }, ask)).status, 401);
  const { proposalId } = await call(alice, 'propose_changes', {
    changes: [{ recordId: 'r-1', values: { Research_purpose: 'Review' } }],
    reason: 'Clutch 944',
  });
  assert.equal((await listed(alice, proposalId)).status, 'pending');
  assert.equal(await listed(bob, proposalId), undefined);
  const apply = { method: 'POST', path: `/api/chat/proposals/${proposalId}/apply`, body: { requestId: 'b' }, user: bob };
  assert.equal((await assistant.handle(apply)).status, 404);
  // Bob only reads the workbook.
  assert.match((await call(bob, 'propose_changes', { changes: [{ recordId: 'r-1', values: { Sex: 'Male' } }] })).error, /cannot propose/);
  db.close();
});

test('partial apply is marked for review with current field values and cannot be replayed', async () => {
  const { assistant, db, call, listed } = fixture({}, async (_changes, _options, records) => {
    records[0].values.Research_purpose = 'Review';
    records[0].version++;
    throw Object.assign(new Error('Second record changed'), { code: 'VERSION_CONFLICT', status: 409 });
  });
  const { proposalId: id } = await call(alice, 'propose_changes', {
    changes: [
      { recordId: 'r-1', values: { Research_purpose: 'Review' } },
      { recordId: 's-1', values: { 'NUMBER OF PUPA': 5 } },
    ],
  });
  const apply = () =>
    assistant.handle({ method: 'POST', path: `/api/chat/proposals/${id}/apply`, body: { requestId: 'partial-1' }, user: alice });
  const result = await apply();
  assert.equal(result.status, 409);
  assert.equal(result.body.error.details.status, 'needs_review');
  assert.equal(result.body.error.details.current[0].values.Research_purpose, 'Review');
  assert.equal((await apply()).status, 409);
  assert.equal((await listed(alice, id)).status, 'needs_review');
  db.close();
});

test('search_records reads exact rows through the tools', async () => {
  const { db, call } = fixture();
  const found = await call(alice, 'search_records', { query: 'A0A' });
  assert.deepEqual(found.records.map(r => r.id), ['r-1']);
  assert.match((await call(alice, 'search_records', { query: ' ' })).error, /required/);
  db.close();
});

test('proposal stores before, after and version, then uses validated apply hook once', async () => {
  const { assistant, db, call, listed, getApplied } = fixture();
  const { proposalId } = await call(alice, 'propose_changes', {
    changes: [{ recordId: 'r-1', values: { Research_purpose: 'Review' } }],
    reason: 'Requested correction',
  });
  const proposal = await listed(alice, proposalId);
  const { recordId, values, label } = proposal.changes[0];
  assert.deepEqual({ recordId, values, label }, { recordId: 'r-1', values: { Research_purpose: 'Review' }, label: 'A0A' });
  // Kept with the proposal (the save checks them), not sent to the table.
  const [stored] = JSON.parse(db.prepare('SELECT changes_json FROM ai_proposals WHERE id = ?').get(proposalId).changes_json);
  assert.deepEqual([stored.expectedVersion, stored.before], [3, { Research_purpose: '' }]);
  assert.ok(!('before' in proposal.changes[0]) && !('expectedVersion' in proposal.changes[0]) && !('current' in proposal.changes[0]));
  const apply = (user, body = {}) =>
    assistant.handle({ method: 'POST', path: `/api/chat/proposals/${proposalId}/apply`, body, user });
  assert.equal((await apply(bob)).status, 404);
  assert.equal((await apply(alice, { requestId: 'request-1' })).status, 200);
  assert.equal(getApplied().changes[0].expectedVersion, 3);
  assert.equal(getApplied().options.requestId, 'request-1');
  assert.equal((await apply(alice)).status, 409);
  db.close();
});

test('stage report sums observed values and marks missingness', async () => {
  const { assistant, db } = fixture();
  const response = await assistant.handle({
    method: 'GET',
    path: '/api/reports',
    query: { kind: 'stages' },
    user: alice,
  });
  assert.equal(response.status, 200);
  assert.equal(response.body.rows.find(row => row.stage === 'EGGS').total, 12);
  assert.equal(response.body.rows.find(row => row.stage === 'ADULTS').missingClutches, 1);
  assert.equal(response.body.sources[0].id, 's-1');
  db.close();
});

test('voice and image calls use configured chat models and return reviewable drafts', async () => {
  const { assistant, db } = fixture();
  const provider = fakeProvider([
    { content: 'Clutch 944 has four pupae.' },
    { content: '{"text":"Tube A1","uncertain":["date"]}' },
  ]);
  try {
    const voice = await assistant.handle({
      method: 'POST',
      path: '/api/ai/transcribe',
      body: { mimeType: 'audio/wav', dataBase64: Buffer.from('RIFFtest').toString('base64') },
      user: alice,
    });
    assert.equal(voice.status, 200);
    assert.equal(voice.body.requiresReview, true);
    assert.equal(provider.requests[0].messages[1].content[1].type, 'input_audio');
    const image = await assistant.handle({
      method: 'POST',
      path: '/api/ai/extract',
      body: { mimeType: 'image/png', dataBase64: Buffer.from('png').toString('base64') },
      user: alice,
    });
    assert.equal(image.body.text, 'Tube A1');
    assert.equal(provider.requests[1].messages[1].content[1].type, 'image_url');
  } finally {
    provider.restore();
    db.close();
  }
});

test('weekly report accepts Sheets serial dates and scans beyond 5000 rows', async () => {
  const rows = Array.from({ length: 5200 }, (_, index) => ({
    id: `row-${index}`,
    sheet: 'Insectary_data',
    row: index + 2,
    version: 1,
    values: { Insectary_ID: `A${index}`, Sex: 'Female', Intro2Insectary_date: 46200 },
    formulas: {},
  }));
  let calls = 0;
  const store = {
    listModules: () => [{ id: 'Insectary_data', sheet: 'Insectary_data', recordCount: rows.length }],
    searchRecords: ({ limit, offset, observedOnly }) => {
      calls++;
      assert.equal(observedOnly, true);
      return { records: rows.slice(offset, offset + limit), total: rows.length };
    },
  };
  const response = await createReports({ store }).handle({
    method: 'GET',
    path: '/api/reports',
    query: { kind: 'weekly' },
    user: alice,
  });
  assert.equal(response.status, 200);
  assert.equal(response.body.rows[0].count, 5200);
  assert.equal(response.body.truncated, false);
  assert.equal(response.body.sourceCount, 5200);
  assert.equal(response.body.sources.length, 500);
  assert.ok(calls > 10);
});
