import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, mkdir, rm, writeFile } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { createHash } from 'node:crypto';
import { DatabaseSync } from 'node:sqlite';
import { createAssistant } from '../server/assistant.mjs';
import { chunkText, createKnowledge, dateFromTitle, fold, runKnowledgeTool } from '../server/knowledge.mjs';

const MEETING_OLD = '1AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA';
const MEETING_NEW = '1BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB';
const PROTOCOL = '1CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC';
const DECK = '1DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD';

const driveDoc = (id, meta, body) =>
  `---\n${Object.entries({ driveId: id, sourceUrl: `https://docs.google.com/document/d/${id}/edit`, ...meta })
    .map(([k, v]) => `${k}: ${v}`)
    .join('\n')}\n---\n# ${meta.title}\n\n${body}\n`;

async function corpus() {
  const dir = await mkdtemp(join(tmpdir(), 'ithomiini-knowledge-'));
  await mkdir(join(dir, 'drive'));
  // The hand-copied snapshot of a meeting that the Drive mirror also has: the mirror wins.
  await writeFile(
    join(dir, `${MEETING_NEW}.md`),
    `---\ntitle: 137 Meeting-10/09/2026\nsourceUrl: https://docs.google.com/document/d/${MEETING_NEW}/edit\n---\n# 137 Meeting-10/09/2026\n\nTexto viejo copiado a mano.\n`,
  );
  await writeFile(join(dir, 'notes.md'), '---\ntitle: Notas sueltas\n---\nUna nota local sobre el invernadero.\n');
  await writeFile(
    join(dir, 'drive', `${MEETING_OLD}.md`),
    driveDoc(MEETING_OLD, { title: '12 Meeting-05/03/2025', kind: 'meeting', date: '2025-03-05' }, 'Reunión: la eclosión de los huevos de Mechanitis fue baja. Larvas en observación.'),
  );
  await writeFile(
    join(dir, 'drive', `${MEETING_NEW}.md`),
    driveDoc(MEETING_NEW, { title: '137 Meeting-10/09/2026', kind: 'meeting', date: '2026-09-10' }, 'Se acordó revisar las larvas cada mañana y marcar las crías nuevas.'),
  );
  await writeFile(
    join(dir, 'drive', `${PROTOCOL}.md`),
    driveDoc(
      PROTOCOL,
      { title: 'Protocolo de cría', kind: 'protocol', modifiedTime: '2024-01-10T00:00:00Z' },
      'Las larvas se alimentan con hojas frescas de Solanaceae. Limpiar los recipientes a diario.',
    ),
  );
  const slides = Array.from({ length: 12 }, (_, i) => `## Diapositiva ${i + 1}\n\n${i === 9 ? 'Temperatura del insectario: 26 grados' : 'Relleno '.repeat(60)}`).join('\n\n');
  await writeFile(join(dir, 'drive', `${DECK}.md`), driveDoc(DECK, { title: 'Informe anual', kind: 'presentation' }, slides));
  return dir;
}

test('dates are read day first from titles', () => {
  assert.equal(dateFromTitle('137 Meeting-10/09/2026'), '2026-09-10');
  assert.equal(dateFromTitle('Reunión 3-2-25'), '2025-02-03');
  assert.equal(dateFromTitle('Informe 2024-11-05'), '2024-11-05');
  assert.equal(dateFromTitle('Reunión 5 de septiembre de 2026'), '2026-09-05');
  assert.equal(dateFromTitle('Meeting Sept 10, 2026'), '2026-09-10');
  assert.equal(dateFromTitle('31/02/2026'), null);
  assert.equal(dateFromTitle('Protocolo de cría'), null);
  assert.equal(fold('Eclosión ÁRBOL'), 'eclosion arbol');
});

test('chunks break at headings and slides and stay near 1500 characters', () => {
  const text = Array.from({ length: 6 }, (_, i) => `## Diapositiva ${i + 1}\n\n${'palabra '.repeat(120)}`).join('\n\n');
  const chunks = chunkText(text);
  assert.ok(chunks.length >= 3);
  for (const c of chunks) {
    assert.ok(c.end - c.start <= 1500);
    assert.match(text.slice(c.start, c.end), /^## Diapositiva/);
  }
  const long = 'Una frase larga. '.repeat(400);
  assert.ok(chunkText(long).every(c => c.end - c.start <= 1500));
});

test('search folds accents, filters by kind and date, and prefers the Drive copy of a document', async () => {
  const dir = await corpus();
  try {
    const knowledge = createKnowledge({ knowledgeRoots: [dir] });
    const found = await knowledge.search({ query: 'reunion eclosion huevos' });
    assert.equal(found[0].id, MEETING_OLD);
    assert.equal(found[0].kind, 'meeting');
    assert.equal(found[0].date, '2025-03-05');
    assert.match(found[0].sourceUrl, /docs\.google\.com/);
    assert.match(found[0].snippet, /eclosión/);

    const larvae = await knowledge.search({ query: 'larvas' });
    assert.deepEqual(new Set(larvae.map(p => p.id)), new Set([MEETING_OLD, MEETING_NEW, PROTOCOL]));
    assert.deepEqual((await knowledge.search({ query: 'larvas', kind: 'protocol' })).map(p => p.id), [PROTOCOL]);
    assert.deepEqual((await knowledge.search({ query: 'larvas', kind: 'meeting', from: '2026-01-01' })).map(p => p.id), [MEETING_NEW]);
    assert.deepEqual((await knowledge.search({ query: 'larvas', to: '31/12/2025' })).map(p => p.id).sort(), [MEETING_OLD, PROTOCOL].sort());

    // The same Drive document once: the mirror's text, not the old hand copy.
    assert.equal((await knowledge.search({ query: 'viejo copiado' })).length, 0);
    const listed = await knowledge.list({});
    assert.equal(listed.documents.filter(d => d.id === MEETING_NEW).length, 1);
    assert.equal(listed.total, 5);

    // A slide deep in a deck is found by its own passage.
    const [slide] = await knowledge.search({ query: 'temperatura insectario' });
    assert.equal(slide.id, DECK);
    assert.match(slide.snippet, /Diapositiva 10/);
    assert.ok(slide.offset > 0);
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
});

test('list gives the last meeting first; read pages a document by id or Drive link', async () => {
  const dir = await corpus();
  try {
    const knowledge = createKnowledge({ knowledgeRoots: [dir], knowledgeCheckMs: 0 });
    const last = await knowledge.list({ kind: 'meeting', limit: 1 });
    assert.equal(last.documents[0].id, MEETING_NEW);
    assert.equal(last.documents[0].date, '2026-09-10');
    assert.equal(last.total, 2);
    assert.equal(last.counts.meeting, 2);
    assert.equal((await knowledge.list({ query: 'protocolo' })).documents[0].id, PROTOCOL);

    const page = await knowledge.read({ id: DECK, max: 500 });
    assert.equal(page.text.length, 500);
    assert.equal(page.nextOffset, 500);
    const rest = await knowledge.read({ id: `https://docs.google.com/document/d/${DECK}/edit`, offset: page.nextOffset, max: 50000 });
    assert.equal(rest.offset, 500);
    assert.ok(rest.text.length <= 20000);
    assert.equal(await knowledge.read({ id: 'nope' }), null);

    // A new file is seen on the next call.
    await writeFile(join(dir, 'drive', '1EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE.md'), driveDoc('1EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE', { title: '138 Meeting-17/09/2026', kind: 'meeting' }, 'Nuevo.'));
    assert.equal((await knowledge.list({ kind: 'meeting', limit: 1 })).documents[0].date, '2026-09-17');
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
});

test('document tools cite their documents as sources', async () => {
  const dir = await corpus();
  try {
    const knowledge = createKnowledge({ knowledgeRoots: [dir] });
    const context = { sources: new Map() };
    const out = await runKnowledgeTool(knowledge, 'search_knowledge', { query: 'hojas frescas', kind: 'protocol' }, context);
    assert.equal(out.passages[0].title, 'Protocolo de cría');
    assert.equal(context.sources.get(PROTOCOL).sourceUrl, `https://docs.google.com/document/d/${PROTOCOL}/edit`);
    const read = await runKnowledgeTool(knowledge, 'read_document', { id: MEETING_OLD }, context);
    assert.match(read.text, /eclosión/);
    const missing = await runKnowledgeTool(knowledge, 'read_document', { id: 'x' }, context);
    assert.ok(missing.error);
    const list = await runKnowledgeTool(knowledge, 'list_documents', { kind: 'meeting', limit: 1 }, context);
    assert.equal(list.documents.length, 1);
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
});

test('the document tools reach T3 Code and Claude through MCP, and /api/knowledge still answers', async () => {
  const dir = await corpus();
  const db = new DatabaseSync(':memory:');
  try {
    db.exec('CREATE TABLE users (id TEXT PRIMARY KEY, username TEXT, display_name TEXT, role TEXT, active INTEGER)');
    db.prepare("INSERT INTO users VALUES ('u1','ana','Ana','viewer',1)").run();
    const assistant = createAssistant({ store: { db, getRecord: () => null }, config: { knowledgeRoots: [dir] } });
    const token = 'token-for-ana';
    db.prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')").run(
      createHash('sha256').update(token).digest('hex'),
      'u1',
    );
    const mcp = (method, params) => assistant.mcp({ authorization: `Bearer ${token}` }, { jsonrpc: '2.0', id: 1, method, params });
    const tools = (await mcp('tools/list')).body.result.tools.map(t => t.name);
    // The chats read the documents' folder with their own file tools: only the sync is listed; the readers
    // still answer a chat that loaded them before.
    assert.ok(tools.includes('sync_documents'));
    assert.ok(!['search_knowledge', 'read_document', 'list_documents'].some(n => tools.includes(n)));
    const call = async (name, args) => JSON.parse((await mcp('tools/call', { name, arguments: args })).body.result.content[0].text);
    const last = await call('list_documents', { kind: 'meeting', limit: 1 });
    assert.equal(last.documents[0].title, '137 Meeting-10/09/2026');
    const read = await call('read_document', { id: last.documents[0].id });
    assert.match(read.text, /revisar las larvas/);
    const found = await call('search_knowledge', { query: 'eclosion', from: '2025-01-01', to: '2025-12-31' });
    assert.equal(found.passages[0].id, MEETING_OLD);

    const user = { id: 'u1', role: 'viewer' };
    const http = await assistant.handle({ method: 'GET', path: '/api/knowledge', query: { q: 'larvas', kind: 'protocol' }, user });
    assert.deepEqual(http.body.documents.map(d => d.id), [PROTOCOL]);
    const doc = await assistant.handle({ method: 'GET', path: `/api/knowledge/${PROTOCOL}`, user });
    assert.equal(doc.body.kind, 'protocol');
    assert.equal((await assistant.handle({ method: 'GET', path: '/api/knowledge', query: { q: 'larvas' } })).status, 401);
  } finally {
    db.close();
    await rm(dir, { recursive: true, force: true });
  }
});

test('sync_documents starts the Drive sync unit on request and reports when the mirror is fresh', async () => {
  const root = await mkdtemp(join(tmpdir(), 'ks-sync-'));
  try {
    await mkdir(join(root, 'drive'));
    const manifest = join(root, 'drive', 'manifest.json');
    await writeFile(manifest, JSON.stringify({ version: 1, syncedAt: '2026-09-29T04:37:32.036Z', files: {} }));
    // A stub systemctl: "start" writes a newer manifest, "is-active" says the run is over.
    const stub = join(root, 'systemctl');
    await writeFile(
      stub,
      `#!/bin/sh\ncase "$2" in start) echo '{"version":1,"syncedAt":"2026-09-29T12:00:00.000Z","files":{}}' > ${manifest};; is-active) echo inactive; exit 3;; esac\n`,
      { mode: 0o755 },
    );
    const knowledge = createKnowledge({ knowledgeRoots: [root], systemctl: stub, syncPollMs: 10 });
    const out = await runKnowledgeTool(knowledge, 'sync_documents', {}, { sources: new Map() });
    assert.equal(out.running, false);
    assert.equal(out.lastSync, '2026-09-29T12:00:00.000Z');
    assert.match(out.note, /al día/);
  } finally {
    await rm(root, { recursive: true, force: true });
  }
});
