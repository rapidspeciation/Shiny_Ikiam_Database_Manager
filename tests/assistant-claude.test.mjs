import test from 'node:test';
import assert from 'node:assert/strict';
import http from 'node:http';
import { mkdtemp, rm } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { fileURLToPath } from 'node:url';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { moduleMap } from '../server/schema.mjs';
import { createAssistant } from '../server/assistant.mjs';

const franz = { id: 'u-franz', username: 'franz', displayName: 'Franz Chandi', role: 'editor' };
const species = moduleMap.get('Insectary_data').fields.find(f => f.key === 'SPECIES').column;

test('Claude reads rows and drafts edits through MCP; only the chosen rows are written', async () => {
  const sheets = new LocalSheets({
    Insectary_data: [
      { row: 2, values: { Insectary_ID: '5VB', 'CLUTCH NUMBER': 838, Sex: 'female' } },
      { row: 3, values: { Insectary_ID: '8VD', 'CLUTCH NUMBER': 843, Sex: 'female' } },
    ],
  });
  for (const [row, value] of [
    [2, 'Mechanitis messenoides intermedia'],
    [3, 'Mechanitis messenoides messenoides'],
  ])
    sheets.rows.get('Insectary_data').find(r => r.row === row).cells[species] = {
      userEnteredValue: { formulaValue: '=XLOOKUP(C2,Insectary_stocks!A:A,Insectary_stocks!C:C,"")' },
      effectiveValue: { stringValue: value },
    };
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data'] });
  const workspace = await mkdtemp(join(tmpdir(), 'claude-ws-'));
  const server = http.createServer(async (req, res) => {
    let raw = '';
    for await (const chunk of req) raw += chunk;
    const out = await assistant.mcp(req.headers, JSON.parse(raw));
    res.writeHead(out.status, { 'content-type': 'application/json' });
    res.end(out.body ? JSON.stringify(out.body) : '');
  });
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  const assistant = createAssistant({
    store,
    config: {
      mcpUrl: `http://127.0.0.1:${server.address().port}/mcp`,
      claude: {
        bin: fileURLToPath(new URL('./fixtures/fake-claude.mjs', import.meta.url)),
        model: 'sonnet',
        users: new Set(['franz']),
        workspace,
        timeoutMs: 20000,
      },
    },
  });
  try {
    const status = await assistant.handle({ method: 'GET', path: '/api/ai/status', user: franz });
    assert.equal(status.body.provider, 'Claude');
    // Without the per-turn token the tools are closed.
    assert.equal((await assistant.mcp({}, { jsonrpc: '2.0', id: 1, method: 'tools/list' })).status, 401);

    const thread = (await assistant.handle({ method: 'POST', path: '/api/chat/threads', body: {}, user: franz })).body
      .thread;
    const send = message =>
      assistant.handle({
        method: 'POST',
        path: `/api/chat/threads/${thread.id}/messages`,
        body: { message },
        user: franz,
      });
    const first = await send('Revisa esta página del cuaderno');
    assert.equal(first.status, 200);
    assert.match(first.body.message.content, /con 2 filas; no encontrado: 9ZZ/);
    const proposal = first.body.proposals[0];
    assert.deepEqual(proposal.fields, ['SPECIES', 'CLUTCH NUMBER']);
    assert.deepEqual(proposal.changes[0].replaceFormula, ['SPECIES']);
    assert.equal(proposal.changes[1].current['CLUTCH NUMBER'], 843);

    // "Está correcto, aplica solo la fila del clutch": the model applies row 1 only.
    const second = await send(`Está correcto, aplica la propuesta ${proposal.id}`);
    assert.match(second.body.message.content, /Aplicado: applied 1/);
    assert.equal(store.getRecordBySheetRow('Insectary_data', 3).values['CLUTCH NUMBER'], 848);
    // The species row was not chosen, so its formula is untouched.
    assert.ok(store.getRecordBySheetRow('Insectary_data', 2).formulas.SPECIES);
    const saved = await assistant.handle({ method: 'GET', path: `/api/chat/threads/${thread.id}`, user: franz });
    const view = saved.body.messages.find(m => m.proposals.length).proposals[0];
    assert.equal(view.status, 'applied');
    assert.deepEqual(view.applied, [1]);
  } finally {
    server.close();
    store.close();
    await rm(workspace, { recursive: true, force: true });
  }
});
