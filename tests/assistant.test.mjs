import test from 'node:test';
import assert from 'node:assert/strict';
import { DatabaseSync } from 'node:sqlite';
import { mkdtemp, writeFile, rm } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
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
  return {
    assistant: createAssistant({
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
    }),
    db,
    getApplied: () => applied,
  };
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

test("private threads require authentication and hide another user's messages", async () => {
  const { assistant, db } = fixture();
  assert.equal((await assistant.handle({ method: 'GET', path: '/api/chat/threads' })).status, 401);
  const created = await assistant.handle({
    method: 'POST',
    path: '/api/chat/threads',
    body: { title: 'Clutch 944' },
    user: alice,
  });
  const id = created.body.thread.id;
  assert.equal((await assistant.handle({ method: 'GET', path: `/api/chat/threads/${id}`, user: bob })).status, 404);
  assert.deepEqual((await assistant.handle({ method: 'GET', path: '/api/chat/threads', user: bob })).body.threads, []);
  assert.equal((await assistant.handle({ method: 'DELETE', path: `/api/chat/threads/${id}`, user: bob })).status, 404);
  db.close();
});

test('partial apply is marked for review with current field values and cannot be replayed', async () => {
  const { assistant, db } = fixture({}, async (_changes, _options, records) => {
    records[0].values.Research_purpose = 'Review';
    records[0].version++;
    throw Object.assign(new Error('Second record changed'), { code: 'VERSION_CONFLICT', status: 409 });
  });
  const provider = fakeProvider([
    {
      content: null,
      tool_calls: [
        { id: 'get-1', type: 'function', function: { name: 'get_record', arguments: '{"id":"r-1"}' } },
        { id: 'get-2', type: 'function', function: { name: 'get_record', arguments: '{"id":"s-1"}' } },
      ],
    },
    {
      content: null,
      tool_calls: [
        {
          id: 'draft',
          type: 'function',
          function: {
            name: 'propose_changes',
            arguments:
              '{"changes":[{"recordId":"r-1","values":{"Research_purpose":"Review"}},{"recordId":"s-1","values":{"NUMBER OF PUPA":5}}]}',
          },
        },
      ],
    },
    { content: 'Review the two proposed edits [r-1] [s-1].' },
  ]);
  try {
    const thread = (await assistant.handle({ method: 'POST', path: '/api/chat/threads', body: {}, user: alice })).body
      .thread;
    const response = await assistant.handle({
      method: 'POST',
      path: `/api/chat/threads/${thread.id}/messages`,
      body: { message: 'Draft two edits' },
      user: alice,
    });
    const id = response.body.proposals[0].id;
    const result = await assistant.handle({
      method: 'POST',
      path: `/api/chat/proposals/${id}/apply`,
      body: { requestId: 'partial-1' },
      user: alice,
    });
    assert.equal(result.status, 409);
    assert.equal(result.body.error.details.status, 'needs_review');
    assert.equal(result.body.error.details.current[0].values.Research_purpose, 'Review');
    assert.equal(
      (
        await assistant.handle({
          method: 'POST',
          path: `/api/chat/proposals/${id}/apply`,
          body: { requestId: 'partial-1' },
          user: alice,
        })
      ).status,
      409,
    );
    const restored = await assistant.handle({ method: 'GET', path: `/api/chat/threads/${thread.id}`, user: alice });
    assert.equal(restored.body.messages.at(-1).proposals[0].status, 'needs_review');
  } finally {
    provider.restore();
    db.close();
  }
});

test('model searches exact records and only returns cited sources', async () => {
  const { assistant, db } = fixture();
  const provider = fakeProvider([
    {
      content: null,
      tool_calls: [
        { id: 'call-1', type: 'function', function: { name: 'search_records', arguments: '{"query":"A0A"}' } },
      ],
    },
    { content: 'The row records a female [r-1]. Unknown claim [fake-2].' },
  ]);
  try {
    const thread = (await assistant.handle({ method: 'POST', path: '/api/chat/threads', body: {}, user: alice })).body
      .thread;
    const response = await assistant.handle({
      method: 'POST',
      path: `/api/chat/threads/${thread.id}/messages`,
      body: { message: 'Find A0A' },
      user: alice,
    });
    assert.equal(response.status, 200);
    assert.equal(response.body.sources[0].id, 'r-1');
    assert.match(response.body.message.content, /\[r-1\]/);
    assert.doesNotMatch(response.body.message.content, /fake-2/);
    assert.equal(provider.requests.length, 2);
    const restored = await assistant.handle({ method: 'GET', path: `/api/chat/threads/${thread.id}`, user: alice });
    assert.equal(restored.body.messages.length, 2);
  } finally {
    provider.restore();
    db.close();
  }
});

test('proposal stores before, after and version, then uses validated apply hook once', async () => {
  const { assistant, db, getApplied } = fixture();
  const provider = fakeProvider([
    {
      content: null,
      tool_calls: [{ id: 'get', type: 'function', function: { name: 'get_record', arguments: '{"id":"r-1"}' } }],
    },
    {
      content: null,
      tool_calls: [
        {
          id: 'draft',
          type: 'function',
          function: {
            name: 'propose_changes',
            arguments:
              '{"changes":[{"recordId":"r-1","values":{"Research_purpose":"Review"}}],"reason":"Requested correction"}',
          },
        },
      ],
    },
    { content: 'I drafted a change for review [r-1].' },
  ]);
  try {
    const thread = (await assistant.handle({ method: 'POST', path: '/api/chat/threads', body: {}, user: alice })).body
      .thread;
    const response = await assistant.handle({
      method: 'POST',
      path: `/api/chat/threads/${thread.id}/messages`,
      body: { message: 'Draft a change' },
      user: alice,
    });
    assert.equal(response.status, 200);
    const proposal = response.body.proposals[0];
    const { recordId, expectedVersion, before, values, label, current } = proposal.changes[0];
    assert.deepEqual(
      { recordId, expectedVersion, before, values, label, current },
      {
        recordId: 'r-1',
        expectedVersion: 3,
        before: { Research_purpose: '' },
        values: { Research_purpose: 'Review' },
        label: 'A0A',
        current: { Research_purpose: '' },
      },
    );
    assert.equal(
      (
        await assistant.handle({
          method: 'POST',
          path: `/api/chat/proposals/${proposal.id}/apply`,
          body: {},
          user: bob,
        })
      ).status,
      404,
    );
    const applied = await assistant.handle({
      method: 'POST',
      path: `/api/chat/proposals/${proposal.id}/apply`,
      body: { requestId: 'request-1' },
      user: alice,
    });
    assert.equal(applied.status, 200);
    assert.equal(getApplied().changes[0].expectedVersion, 3);
    assert.equal(getApplied().options.requestId, 'request-1');
    assert.equal(
      (
        await assistant.handle({
          method: 'POST',
          path: `/api/chat/proposals/${proposal.id}/apply`,
          body: {},
          user: alice,
        })
      ).status,
      409,
    );
  } finally {
    provider.restore();
    db.close();
  }
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
  assert.match(response.body.method, /neither current live occupancy nor a cohort survival calculation/);
  assert.equal(response.body.sources[0].id, 's-1');
  db.close();
});

test('knowledge index only reads configured documents and preserves source URL', async () => {
  const dir = await mkdtemp(join(tmpdir(), 'ithomiini-knowledge-'));
  await writeFile(
    join(dir, 'note.md'),
    '---\ntitle: Meeting 137\nsourceUrl: https://docs.google.com/document/d/abc/edit\n---\n# Meeting\nClutch 944 reached pupae.',
  );
  const { assistant, db } = fixture({ knowledgeRoots: [dir] });
  try {
    const found = await assistant.handle({
      method: 'GET',
      path: '/api/knowledge',
      query: { q: 'clutch 944' },
      user: alice,
    });
    assert.equal(found.body.documents.length, 1);
    assert.equal(found.body.documents[0].sourceUrl, 'https://docs.google.com/document/d/abc/edit');
    const document = await assistant.handle({
      method: 'GET',
      path: `/api/knowledge/${found.body.documents[0].id}`,
      user: alice,
    });
    assert.match(document.body.text, /reached pupae/);
    assert.doesNotMatch(document.body.text, /sourceUrl:/);
  } finally {
    db.close();
    await rm(dir, { recursive: true, force: true });
  }
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
