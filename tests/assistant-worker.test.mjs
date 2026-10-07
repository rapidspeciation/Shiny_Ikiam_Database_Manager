import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { mkdtempSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createAssistantHost } from '../server/assistant-host.mjs';
import { PROPOSAL_TABLES, createStoreReader } from '../server/store-reader.mjs';

// The AI's tool calls in a worker thread (server/assistant-host.mjs), on a database file: people's
// requests are answered meanwhile, the worker writes only proposals, and a worker that hangs or
// dies is replaced.

const hooks = new URL('./fixtures/assistant-worker-hooks.mjs', import.meta.url);
const FCH = 'FCH - Franz Chandi';
const base = {
  Purpose: 'Monitoring',
  Collection_location: 'Ikiam',
  Collector: FCH,
  Release_Collect: 'Mark_Released',
  SPECIES: 'Oleria gunilla',
};

async function fixture(t, { callMs = 20_000, backoffMs } = {}) {
  const dir = mkdtempSync(join(tmpdir(), 'assistant-worker-'));
  const databasePath = join(dir, 'app.sqlite');
  const sheets = new LocalSheets({
    Collection_data: [2, 3, 4, 5].map(row => ({ row, values: { ...base, FieldMark_ID: `B${row}`, Sex: 'female' } })),
    Location_data: [{ row: 2, values: { Collection_location: 'Ikiam' } }],
    Insectary_stocks: [
      { row: 2, values: { 'CLUTCH NUMBER': 838, SPECIES: 'Mechanitis lysimnia', 'DATE LAID': 45870 } },
    ],
  });
  const store = new Store({ databasePath, localMode: true }, { sheets });
  await store.sync({ sheets: ['Collection_data', 'Location_data', 'Insectary_stocks'] });
  const host = createAssistantHost({
    store,
    config: { databasePath, localMode: true, proposalWaitMs: 10_000 },
    workerUrl: hooks,
    callMs,
    backoffMs,
  });
  store.db
    .prepare(
      "INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u1','franz','Franz','editor','s','h',1,'2026-01-01')",
    )
    .run();
  store.db
    .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
    .run(createHash('sha256').update('franz-token').digest('hex'), 'u1');
  t.after(() => {
    host.close();
    store.close();
    rmSync(dir, { recursive: true, force: true });
  });
  const rpc = (name, args = {}) =>
    host.mcp(
      { authorization: 'Bearer franz-token' },
      { jsonrpc: '2.0', id: 7, method: 'tools/call', params: { name, arguments: args } },
    );
  const call = async (name, args) => {
    const out = await rpc(name, args);
    assert.ok(out.body.result, `${name}: ${JSON.stringify(out.body)}`);
    return JSON.parse(out.body.result.content[0].text);
  };
  const user = { id: 'u1', username: 'franz', displayName: 'Franz', role: 'editor' };
  const http = (method, path, body = {}, query = {}) => host.handle({ method, path, body, user, query, headers: {} });
  const record = row => store.getRecordBySheetRow('Collection_data', row);
  return { databasePath, store, host, rpc, call, http, record };
}

test('propose_changes runs in the worker; the page waiting for the list wakes; apply_proposal runs in the app and writes', async t => {
  const { store, host, call, http, record } = await fixture(t);
  assert.equal(host.mode, 'worker');
  const first = (await http('GET', '/api/chat/proposals', {}, { all: '1' })).body;
  assert.deepEqual(first.proposals, []);
  // The page's long poll, held until a proposal of Franz's changes.
  let woke = null;
  const waiting = http('GET', '/api/chat/proposals', {}, { all: '1', wait: '1', revision: first.revision }).then(
    out => ((woke = Date.now()), out),
  );
  const asked = Date.now();
  const proposed = await call('propose_changes', {
    reason: 'Sexo corregido',
    changes: [{ recordId: record(2).id, values: { Sex: 'male' }, note: 'foto' }],
  });
  assert.ok(proposed.proposalId, JSON.stringify(proposed));
  assert.equal(host.status().running, true, 'the tools worker answered it');
  const listed = (await waiting).body;
  assert.ok(woke - asked < 5000, 'woken by the change, not by the wait running out');
  assert.equal(listed.proposals.length, 1);
  assert.equal(listed.proposals[0].id, proposed.proposalId);
  assert.equal(host.status().views.running, true, 'the views worker built the list');
  // The proposal is in the app's own connection too.
  assert.equal(
    store.db.prepare('SELECT status FROM ai_proposals WHERE id = ?').get(proposed.proposalId).status,
    'pending',
  );

  // Applying writes to the sheet: in the app's thread.
  const applied = await call('apply_proposal', { proposalId: proposed.proposalId });
  assert.equal(applied.status, 'applied', JSON.stringify(applied));
  assert.equal(record(2).values.Sex, 'male');
  // The worker reads what the app wrote.
  const read = await call('get_record', { id: record(2).id });
  assert.equal(read.values?.Sex ?? read.Sex, 'male');
  assert.equal(host.inFlight(), 0);
});

test("the worker's connection refuses writes to the sheets' copy; a reader refuses what it does not have", async t => {
  const { databasePath, rpc, record } = await fixture(t);
  const out = await rpc('__write');
  assert.match(out.body.refused ?? '', /not authorized/);
  assert.notEqual(record(2).label, 'changed by the worker');

  const reader = createStoreReader({ path: databasePath, writable: PROPOSAL_TABLES, localMode: true });
  t.after(() => reader.close());
  assert.throws(() => reader.db.prepare('DELETE FROM records').run(), /not authorized/);
  assert.throws(() => reader.db.exec('ALTER TABLE records ADD COLUMN x TEXT'), /not authorized/);
  // Shared code's lazy set-up of tables the app made is a no-op, allowed.
  reader.db.exec('CREATE TABLE IF NOT EXISTS records(id TEXT PRIMARY KEY)');
  assert.equal(reader.getRecord(record(2).id).label, record(2).label);
  assert.equal(reader.outbox, undefined);
  assert.throws(() => reader.sync(), /not available to the assistant's worker/);
  assert.throws(() => reader.applyProposal([]), /not available/);
});

test("the assistant's revision racing a person's edit: neither is lost, the assistant hears to look again", async t => {
  const { store, call, http, record } = await fixture(t);
  const proposed = await call('propose_changes', {
    reason: 'Dos filas',
    changes: [
      { recordId: record(2).id, values: { Sex: 'male' } },
      { recordId: record(3).id, values: { Sex: 'male' } },
    ],
  });
  const keys = (await http('GET', '/api/chat/proposals', {}, { all: '1' })).body.proposals[0].changes.map(c => c.key);
  // The app holds the database's write lock: the worker reads the proposal, then waits to save its revision.
  store.db.exec('BEGIN IMMEDIATE');
  let committed = false;
  try {
    const revising = call('update_proposal', {
      proposalId: proposed.proposalId,
      rows: [{ index: 1, values: { Notes_Collection_data: 'the assistant' } }],
    });
    // The worker is warm (it made the proposal): it reads the proposal at once, then waits for the lock.
    await new Promise(resolve => setTimeout(resolve, 300));
    // Meanwhile the person types in the table.
    const edited = await http('POST', `/api/chat/proposals/${proposed.proposalId}/edit`, {
      cells: [{ key: keys[0], field: 'Sex', value: 'female' }],
    });
    assert.equal(edited.status, 200, JSON.stringify(edited.body));
    store.db.exec('COMMIT');
    committed = true;
    const out = await revising;
    assert.match(out.error ?? '', /edited the table meanwhile/);
  } finally {
    if (!committed) store.db.exec('ROLLBACK');
  }
  // The assistant does as told: reads it again, revises again.
  const again = await call('update_proposal', {
    proposalId: proposed.proposalId,
    rows: [{ index: 1, values: { Notes_Collection_data: 'the assistant' } }],
  });
  assert.ok(!again.error, JSON.stringify(again));
  const changes = JSON.parse(
    store.db.prepare('SELECT changes_json FROM ai_proposals WHERE id = ?').get(proposed.proposalId).changes_json,
  );
  assert.equal(changes[0].personEdits?.Sex?.by, 'Franz', "the person's cell is theirs");
  assert.equal('Sex' in changes[0].values, false, "set back to the sheet's value (female): no change there");
  assert.match(String(changes[1].values.Notes_Collection_data), /the assistant/);
  assert.equal(changes[1].values.Sex, 'male');

  // A page holding an older revision cannot apply what the assistant changed since.
  const stale = await http('POST', `/api/chat/proposals/${proposed.proposalId}/apply`, {
    requestId: 'apply-stale-1',
    revision: 1,
  });
  assert.equal(stale.status, 409);
  assert.equal(stale.body.error.code, 'proposal_changed');
  assert.equal(record(3).values.Sex, 'female', 'nothing written');
});

test('a call that hangs or a worker that dies: the call is answered "call again", a new worker answers the next one', async t => {
  // callMs covers the first worker's start; no wait before starting a worker again.
  const { host, rpc, call, record } = await fixture(t, { callMs: 1500, backoffMs: 0 });
  const hung = await rpc('__hang');
  assert.match(hung.body.error.message, /call again/);
  assert.equal(host.status().restarts, 1);
  const found = await call('find_records', { sheet: 'Collection_data', filters: { FieldMark_ID: 'B2' } });
  assert.ok(JSON.stringify(found).includes(record(2).id), 'a new worker answers');

  const died = await rpc('__crash');
  assert.match(died.body.error.message, /stopped unexpectedly.*call again/);
  const after = await call('get_record', { id: record(3).id });
  assert.ok(!after.error, JSON.stringify(after));
  assert.equal(host.status().mode, 'worker');

  // Three failures in a minute: the worker is left aside and the tools run in the app's thread.
  await rpc('__crash');
  await rpc('__crash');
  assert.equal(host.status().mode, 'degraded');
  const inline = await call('get_record', { id: record(4).id });
  assert.ok(!inline.error, JSON.stringify(inline));
});

test("every tool the worker answers runs there: nothing reaches for the app's memory or writes outside the proposals", async t => {
  const { store, host, rpc, call, http, record } = await fixture(t);
  const proposed = await call('propose_changes', {
    reason: 'Una fila',
    changes: [{ recordId: record(4).id, values: { Sex: 'male' } }],
  });
  const shown = await call('show_rows', {
    sheet: 'Collection_data',
    title: 'Marcas',
    recordIds: [record(2).id, record(3).id],
  });
  assert.ok(shown.tableId, JSON.stringify(shown));
  const calls = [
    ['search_records', { query: 'Oleria' }],
    ['find_records', { sheet: 'Collection_data', filters: { Sex: 'female' } }],
    ['count_records', { sheet: 'Collection_data', groupBy: 'Sex' }],
    ['get_record', { id: 'B3', sheet: 'Collection_data' }],
    ['describe_sheet', { module: 'Collection_data', latestRows: 2 }],
    ['run_report', { kind: 'overview' }],
    ['run_report', { kind: 'quality' }],
    ['check_data', {}],
    ['check_data', { kind: 'repeat,date_order', sheet: 'Collection_data' }],
    ['get_walk', { url: 'https://es.wikiloc.com/rutas-senderismo/ikiam-123456789' }],
    ['list_agreed_fixes', {}],
    ['list_suggested_edits', {}],
    ['get_alerts', {}],
    ['list_history', {}],
    ['record_history', { recordId: record(2).id }],
    ['list_proposals', { allChats: true }],
    ['get_proposal', { proposalId: proposed.proposalId, full: true }],
    [
      'update_proposal',
      { proposalId: proposed.proposalId, rows: [{ index: 0, values: { Sex: 'female' } }], reason: 'Otra' },
    ],
    [
      'match_notebook',
      {
        kind: 'stocks',
        year: 2025,
        lines: [{ raw: '838 lys larvas', values: { 'CLUTCH NUMBER': '838', NOTES: 'larvas enfermas' } }],
      },
    ],
    ['show_rows', { tableId: shown.tableId, title: 'Marcas (2)' }],
  ];
  for (const [name, args] of calls) {
    const out = await rpc(name, args);
    const text = out.body.result?.content?.[0]?.text ?? JSON.stringify(out.body);
    assert.doesNotMatch(
      text,
      /not available to the assistant's worker|cannot be set in the assistant's worker|not authorized|SQLITE/i,
      `${name}: ${text.slice(0, 400)}`,
    );
    assert.ok(out.body.result, `${name}: ${text.slice(0, 400)}`);
  }
  assert.equal(host.status().restarts, 0);

  // Cambios propuestos: built by the views' worker, as the app's thread builds them.
  const listed = (await http('GET', '/api/chat/proposals', {}, { all: '1' })).body.proposals;
  assert.equal(host.status().views.running, true);
  assert.deepEqual(new Set(listed.map(p => p.kind ?? 'proposal')), new Set(['table', 'proposal']));
  const row = id =>
    store.db
      .prepare('SELECT p.*, t.title FROM ai_proposals p JOIN ai_threads t ON t.id = p.thread_id WHERE p.id = ?')
      .get(id);
  for (const p of listed) assert.deepEqual((await host.listedViews([row(p.id)]))[0], p, p.id);
});
