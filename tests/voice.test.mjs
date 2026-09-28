import test from 'node:test';
import assert from 'node:assert/strict';
import { DatabaseSync } from 'node:sqlite';
import { createAssistant } from '../server/assistant.mjs';
import { affirmative, liveSetup, lockedFields, startVoiceSession, voiceConfig } from '../server/voice.mjs';

const alice = { id: 'alice', username: 'alice', displayName: 'Alice Example', role: 'editor' };
const bob = { id: 'bob', username: 'bob', role: 'viewer' };
const tools = [
  {
    type: 'function',
    function: { name: 'get_record', description: 'Fetch one row.', parameters: { type: 'object', properties: {} } },
  },
  {
    type: 'function',
    function: { name: 'apply_proposal', description: 'Chat wording.', parameters: { type: 'object', properties: {} } },
  },
];

/** Stands in for Google's token endpoint and remembers what was sent. */
function fakeGoogle(status = 200) {
  const calls = [];
  const fetchImpl = async (url, options) => {
    calls.push({ url, headers: options.headers, body: JSON.parse(options.body) });
    return {
      ok: status === 200,
      status,
      json: async () => ({ name: 'auth_tokens/abc123' }),
      text: async () => 'API key not valid',
    };
  };
  return { calls, fetchImpl };
}

function fixture(voice = { provider: 'gemini', model: 'gemini-3.8-live', apiKey: 'secret-key' }) {
  const db = new DatabaseSync(':memory:');
  const applied = [];
  // A long note checks that tool results reach the model whole.
  const long = 'x'.repeat(9000);
  const records = [
    {
      id: 'r-1',
      sheet: 'Insectary_data',
      row: 9,
      version: 3,
      label: '5VB',
      values: { Insectary_ID: '5VB', Sex: '', Notes_Insectary_data: long },
      formulas: {},
    },
  ];
  const store = {
    db,
    getRecord: id => records.find(record => record.id === id),
    applyProposal: async (changes, options) => {
      applied.push({ changes, options });
      return { status: 'verified' };
    },
  };
  const google = fakeGoogle();
  const original = globalThis.fetch;
  globalThis.fetch = google.fetchImpl;
  const assistant = createAssistant({ store, config: { voice, claude: { users: new Set() } } });
  return {
    assistant,
    db,
    applied,
    google,
    long,
    close: () => {
      globalThis.fetch = original;
      db.close();
    },
  };
}

const post = (assistant, path, body, user = alice) => assistant.handle({ method: 'POST', path, body, user });

test('the voice setup carries the prompt, every tool as JSON Schema and transcripts of both sides', () => {
  const setup = liveSetup({ model: 'gemini-3.8-live', voiceName: 'Kore', prompt: 'Habla español.', tools });
  assert.equal(setup.model, 'models/gemini-3.8-live');
  assert.deepEqual(setup.generationConfig.responseModalities, ['AUDIO']);
  assert.equal(setup.generationConfig.speechConfig.voiceConfig.prebuiltVoiceConfig.voiceName, 'Kore');
  assert.equal(setup.systemInstruction.parts[0].text, 'Habla español.');
  const declared = setup.tools[0].functionDeclarations;
  assert.deepEqual(
    declared.map(d => d.name),
    ['get_record', 'apply_proposal'],
  );
  assert.deepEqual(declared[0].parametersJsonSchema, { type: 'object', properties: {} });
  assert.match(declared[1].description, /said yes out loud/);
  assert.deepEqual(setup.inputAudioTranscription, {});
  assert.deepEqual(setup.outputAudioTranscription, {});
  assert.ok(setup.contextWindowCompression.slidingWindow);
  assert.equal(liveSetup({ model: 'm', prompt: '', tools }).generationConfig.speechConfig, undefined);
});

test('the token locks prompt, tools and model but leaves session resumption to the browser', () => {
  const fields = lockedFields(liveSetup({ model: 'm', prompt: 'p', tools })).split(',');
  for (const field of ['model', 'systemInstruction.parts', 'tools', 'generationConfig.responseModalities'])
    assert.ok(fields.includes(field), field);
  assert.ok(!fields.some(f => f.startsWith('sessionResumption')));
});

test('only the person saying yes unlocks saving', () => {
  for (const said of ['Sí, guárdalo', 'si', 'dale', 'Está correcto', 'ok aplícalo', 'de acuerdo', 'Confirmo'])
    assert.equal(affirmative(said), true, said);
  for (const said of ['', 'no', 'No, espera', 'todavía no', 'cambia la fecha', '5VB hembra', undefined])
    assert.equal(affirmative(said), false, String(said));
});

test('minting asks Google for a one-use token and never returns the key', async () => {
  const google = fakeGoogle();
  const now = Date.parse('2026-09-28T12:00:00Z');
  const session = await startVoiceSession(
    { ...voiceConfig({}), apiKey: 'secret-key' },
    { prompt: 'p', tools, fetchImpl: google.fetchImpl, now },
  );
  const [call] = google.calls;
  assert.equal(call.url, 'https://generativelanguage.googleapis.com/v1alpha/auth_tokens');
  assert.equal(call.headers['x-goog-api-key'], 'secret-key');
  assert.equal(call.body.uses, 1);
  assert.equal(call.body.newSessionExpireTime, '2026-09-28T12:02:00.000Z');
  assert.equal(call.body.expireTime, '2026-09-28T12:30:00.000Z');
  assert.equal(call.body.bidiGenerateContentSetup.model, 'models/gemini-3.8-live');
  assert.equal(call.body.fieldMask, lockedFields(call.body.bidiGenerateContentSetup));
  assert.equal(session.token, 'auth_tokens/abc123');
  assert.match(session.url, /^wss:\/\/generativelanguage\.googleapis\.com\/.+BidiGenerateContentConstrained$/);
  assert.ok(!JSON.stringify(session).includes('secret-key'));

  await assert.rejects(
    startVoiceSession({ ...voiceConfig({}), apiKey: '' }, { prompt: 'p', tools }),
    e => e.unconfigured,
  );
  await assert.rejects(
    startVoiceSession({ ...voiceConfig({}), apiKey: 'k' }, { prompt: 'p', tools, fetchImpl: fakeGoogle(400).fetchImpl }),
    /HTTP 400/,
  );
});

test('a call without a voice key says it is not configured', async () => {
  const { assistant, close } = fixture({ provider: 'gemini', model: 'gemini-3.8-live', apiKey: '' });
  try {
    const status = await assistant.handle({ method: 'GET', path: '/api/ai/status', user: alice });
    assert.deepEqual(status.body.voice, { configured: false, provider: 'gemini', model: 'gemini-3.8-live' });
    const response = await post(assistant, '/api/ai/voice/session', {});
    assert.equal(response.status, 501);
    assert.equal(response.body.error.code, 'voice_unconfigured');
    assert.equal((await assistant.handle({ method: 'GET', path: '/api/chat/threads', user: alice })).body.threads.length, 0);
  } finally {
    close();
  }
});

test('a call proposes, saves only after a spoken yes, and keeps its transcript and proposals', async () => {
  const { assistant, applied, google, long, close } = fixture();
  try {
    assert.equal((await post(assistant, '/api/ai/voice/session', {}, null)).status, 401);
    const session = await post(assistant, '/api/ai/voice/session', {});
    assert.equal(session.status, 200);
    const { threadId, token, setup, title } = session.body;
    assert.equal(token, 'auth_tokens/abc123');
    assert.match(title, /^Llamada \d{4}-\d{2}-\d{2} \d{2}:\d{2}$/);
    const prompt = setup.systemInstruction.parts[0].text;
    assert.match(prompt, /Alice Example \(initials AE, role editor\)/);
    assert.match(prompt, /voice call/);
    assert.ok(setup.tools[0].functionDeclarations.some(d => d.name === 'propose_changes'));
    assert.equal(google.calls.length, 1);

    // A reconnect keeps the same conversation.
    const again = await post(assistant, '/api/ai/voice/session', { threadId });
    assert.equal(again.body.threadId, threadId);
    assert.equal((await post(assistant, '/api/ai/voice/session', { threadId }, bob)).status, 404);

    const read = await post(assistant, '/api/ai/voice/tool', {
      threadId,
      name: 'get_record',
      args: { id: 'r-1' },
      lines: [{ role: 'user', text: 'Busca la 5VB' }],
    });
    assert.equal(read.body.result.values.Notes_Insectary_data, long);

    const proposed = await post(assistant, '/api/ai/voice/tool', {
      threadId,
      name: 'propose_changes',
      args: { changes: [{ recordId: 'r-1', values: { Sex: 'female' }, note: 'dictado' }], reason: 'Dictado' },
      lines: [
        { role: 'assistant', text: 'La 5VB no tiene sexo.' },
        { role: 'user', text: 'Es hembra' },
      ],
    });
    const proposalId = proposed.body.result.proposalId;
    assert.ok(proposalId);
    assert.equal(proposed.body.proposals.length, 1);
    assert.equal(proposed.body.proposals[0].changes[0].label, '5VB');
    // It also waits in Cambios propuestos, like proposals from T3 Code.
    const waiting = await assistant.handle({ method: 'GET', path: '/api/chat/proposals', user: alice });
    assert.deepEqual(
      waiting.body.proposals.map(p => p.id),
      [proposalId],
    );

    const refused = await post(assistant, '/api/ai/voice/tool', {
      threadId,
      name: 'apply_proposal',
      args: { proposalId },
      heard: 'cambia la fecha',
    });
    assert.match(refused.body.result.error, /not said yes/);
    assert.equal(applied.length, 0);

    const saved = await post(assistant, '/api/ai/voice/tool', {
      threadId,
      name: 'apply_proposal',
      args: { proposalId },
      heard: 'Sí, guárdalo',
      lines: [{ role: 'user', text: 'Sí, guárdalo' }],
    });
    assert.deepEqual(saved.body.result, { status: 'applied', rows: 1 });
    assert.deepEqual(saved.body.applied, [proposalId]);
    assert.equal(saved.body.proposals[0].status, 'applied');
    assert.equal(applied[0].options.reason, 'Confirmado de voz en la llamada');

    await post(assistant, '/api/ai/voice/transcript', { threadId, lines: [{ role: 'assistant', text: 'Guardado.' }] });
    const history = await assistant.handle({ method: 'GET', path: `/api/chat/threads/${threadId}`, user: alice });
    assert.deepEqual(
      history.body.messages.map(m => `${m.role}: ${m.content}`),
      [
        'user: Busca la 5VB',
        'assistant: La 5VB no tiene sexo.',
        'user: Es hembra',
        'assistant: Propuesta durante la llamada',
        'user: Sí, guárdalo',
        'assistant: Guardado.',
      ],
    );
    assert.equal((await post(assistant, '/api/ai/voice/tool', { threadId, name: 'rm_rf' })).status, 400);
    assert.equal(
      (await post(assistant, '/api/ai/voice/tool', { threadId, name: 'get_record', args: { id: 'r-1' } }, bob)).status,
      404,
    );
  } finally {
    close();
  }
});

test('a viewer on a call can read but not propose', async () => {
  const { assistant, close } = fixture();
  try {
    const session = await post(assistant, '/api/ai/voice/session', {}, bob);
    assert.match(session.body.setup.systemInstruction.parts[0].text, /can only read/);
    const out = await post(
      assistant,
      '/api/ai/voice/tool',
      {
        threadId: session.body.threadId,
        name: 'propose_changes',
        args: { changes: [{ recordId: 'r-1', values: { Sex: 'female' } }], reason: 'x' },
      },
      bob,
    );
    assert.match(out.body.result.error, /cannot propose/);
    assert.deepEqual(out.body.proposals, []);
  } finally {
    close();
  }
});
