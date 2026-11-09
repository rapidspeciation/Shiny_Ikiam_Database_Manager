import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash, randomUUID } from 'node:crypto';
import { execFile } from 'node:child_process';
import { mkdirSync, mkdtempSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join } from 'node:path';
import { promisify } from 'node:util';
import { createApp } from '../server/index.mjs';
import { createUser } from '../server/auth.mjs';
import { parseLines } from '../tools/lab/doc-proposals-import.mjs';

// An outside proposer of instruction changes (tools/lab/doc-proposals.md): its token (label «external»), the import
// and export scripts over MCP, and «Pedir versión nueva» on its cards waiting for it instead of starting a T3 chat.

const run = promisify(execFile);
const repo = new URL('..', import.meta.url).pathname;
const SKILL = '---\nname: demo\ndescription: Demo.\n---\n\n# Demo\n\n## Fotos\n\nDos fotos por lado.\n';

test('the JSONL lines: what is missing is refused with its line number', () => {
  const lines = parseLines('{"path":"a.md","section":"","before":"x","after":"y"}\n\n# note\n{"path":"a.md","before":"x","after":"y"}\nnot json\n');
  assert.deepEqual(lines.map(l => [l.line, l.error ?? 'ok']), [
    [1, 'ok'],
    [4, '"section" is missing (\'\' for the text before the first heading)'],
    [5, lines[2].error],
  ]);
  assert.match(lines[2].error, /JSON/);
});

test('import and export over MCP with an outside token; a revision request waits for it and its answer clears it', async t => {
  const root = mkdtempSync(join(tmpdir(), 'doc-proposals-'));
  mkdirSync(join(root, 'assistant', 'skills', 'demo'), { recursive: true });
  for (const [rel, text] of [
    ['assistant/AGENTS.md', '# Brief\n\nHola {{person}}.\n'],
    ['assistant/voice/app.md', 'Corto.\n'],
    ['assistant/voice/core.md', 'Claro.\n'],
    ['assistant/skills/demo/SKILL.md', SKILL],
  ]) {
    mkdirSync(dirname(join(root, rel)), { recursive: true });
    writeFileSync(join(root, rel), text);
  }
  const app = await createApp({ databasePath: ':memory:', localMode: true, secureCookies: false, syncIntervalMs: 0, setupToken: 'dp-setup' }, { seed: {}, instructions: { root } });
  await app.ready;
  const address = await app.listen(0);
  t.after(() => app.close());
  const base = `http://127.0.0.1:${address.port}/ithomiini`;
  // Franz (admin, signed in) and the auditor (admin, token labelled «external»).
  let cookie = '';
  let csrf = '';
  const call = async (path, method = 'GET', body) => {
    const r = await fetch(base + path, { method, headers: { ...(body ? { 'content-type': 'application/json' } : {}), ...(cookie ? { cookie, 'x-csrf-token': csrf } : {}) }, body: body ? JSON.stringify({ requestId: randomUUID(), ...body }) : undefined });
    const data = await r.json();
    if (r.headers.get('set-cookie')) cookie = r.headers.get('set-cookie').split(';')[0];
    if (data.csrf) csrf = data.csrf;
    return { status: r.status, data };
  };
  await call('/api/auth/setup', 'POST', { token: 'dp-setup', username: 'franz_admin', password: 'test-admin-123', displayName: 'Franz' });
  const auditor = createUser(app.store, { username: 'auditor', displayName: 'Auditoría', role: 'admin', password: 'auditor-pass-1' });
  app.store.db.prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'external','2026-11-09')").run(createHash('sha256').update('auditor-token').digest('hex'), auditor.id);
  const tokenFile = join(root, 'token.txt');
  writeFileSync(tokenFile, 'auditor-token\n');
  const url = `${base}/api/ai/mcp`;
  const script = name => join(repo, 'tools', 'lab', name);
  const runScript = async (name, ...args) => {
    try {
      const { stdout } = await run(process.execPath, [script(name), ...args, '--url', url, '--token-file', tokenFile]);
      return { code: 0, stdout };
    } catch (e) {
      return { code: e.code, stdout: e.stdout };
    }
  };

  const jsonl = join(root, 'p.jsonl');
  writeFileSync(
    jsonl,
    [
      { path: 'assistant/skills/demo/SKILL.md', section: 'Fotos', before: 'Dos fotos', after: 'Tres fotos', reason: 'Estilo: cifras.' },
      { path: 'assistant/skills/demo/SKILL.md', section: 'Fotos', before: 'Cuatro fotos', after: 'x' },
      { path: 'server/index.mjs', section: '', before: 'a', after: 'b' },
    ]
      .map(l => JSON.stringify(l))
      .join('\n'),
  );
  const dry = await runScript('doc-proposals-import.mjs', jsonl, '--dry-run');
  assert.equal(dry.code, 1);
  assert.match(dry.stdout, /line 1: ok \(dry run\)/);
  assert.match(dry.stdout, /line 2: refused: before does not match/);
  assert.match(dry.stdout, /line 3: refused: Not an instructions file/);
  assert.equal((await call('/api/doc-review')).data.changes.length, 0, 'a dry run proposes nothing');
  const real = await runScript('doc-proposals-import.mjs', jsonl);
  assert.match(real.stdout, /line 1: ok [0-9a-f-]{36} v1/);
  const card = (await call('/api/doc-review')).data.changes[0];
  assert.equal(card.external, true);
  assert.deepEqual(card.comments.map(c => [c.kind, c.author, c.body]), [['claude', null, 'Estilo: cifras.']]);

  // «Pedir versión nueva»: no chat, the card waits; the export shows it with Franz's comment.
  const asked = await call(`/api/doc-review/changes/${card.seriesId}/revise`, 'POST', { body: 'Mejor «tres por lado».', why: 'comment' });
  assert.deepEqual([asked.data.sent, asked.data.waiting, asked.data.chat, asked.data.change.awaiting], [false, true, null, true]);
  let exported = JSON.parse((await runScript('doc-proposals-export.mjs', '--pending-revisions')).stdout);
  assert.equal(exported.changes.length, 1);
  assert.equal(exported.changes[0].status, 'pending');
  assert.equal(exported.changes[0].revisionRequested, true);
  assert.deepEqual(exported.changes[0].commentsSinceVersion.map(c => [c.by, c.text]), [['Franz', 'Mejor «tres por lado».']]);
  // The auditor answers with v2 over MCP: cleared.
  const v2 = await fetch(url, {
    method: 'POST',
    headers: { 'content-type': 'application/json', authorization: 'Bearer auditor-token' },
    body: JSON.stringify({ jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name: 'revise_doc_change', arguments: { change_id: card.seriesId, after: 'Tres fotos' } } }),
  }).then(r => r.json());
  assert.equal(JSON.parse(v2.result.content[0].text).version, 2);
  exported = JSON.parse((await runScript('doc-proposals-export.mjs', '--pending-revisions')).stdout);
  assert.deepEqual(exported.changes, []);
  const all = JSON.parse((await runScript('doc-proposals-export.mjs')).stdout);
  assert.deepEqual(all.changes.map(c => [c.version, c.status, c.revisionRequested]), [[2, 'pending', false]]);
  // «Pedir a Claude» finds nothing to send (no open thread), and the page shows no ask.
  assert.equal((await call('/api/doc-review/ask', 'POST', {})).status, 409);
});
