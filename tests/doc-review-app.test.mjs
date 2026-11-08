import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash, randomUUID } from 'node:crypto';
import { execFileSync } from 'node:child_process';
import { mkdirSync, mkdtempSync, readFileSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join } from 'node:path';
import { DatabaseSync } from 'node:sqlite';
import { createApp } from '../server/index.mjs';
import { createUser } from '../server/auth.mjs';
import { createGitSync, hashText, initDocReview } from '../server/doc-review.mjs';

// The doc review through the app: its routes for people (names only), the MCP tools for an admin's chat,
// the page reading the text in effect, «Pedir a Claude» without T3, and the workspaces' copy of the change.

const repoRoot = new URL('..', import.meta.url).pathname;

function root() {
  const dir = mkdtempSync(join(tmpdir(), 'doc-review-app-'));
  const put = (rel, text) => {
    mkdirSync(dirname(join(dir, rel)), { recursive: true });
    writeFileSync(join(dir, rel), text);
  };
  put('assistant/AGENTS.md', '# Brief\n\nHola {{person}}.\n');
  put('assistant/voice/app.md', 'Corto.\n');
  put('assistant/voice/core.md', 'Claro.\n');
  put('assistant/skills/demo/SKILL.md', '---\nname: demo\ndescription: Demo.\n---\n\n# Demo\n\n## Datos\n\nUsa NA para el sexo.\n');
  return dir;
}

test('people comment through the app; an admin chat proposes over MCP; applying shows on the page and in its history', async t => {
  const dir = root();
  const app = await createApp(
    { databasePath: ':memory:', localMode: true, secureCookies: false, syncIntervalMs: 0, setupToken: 'doc-setup' },
    { seed: {}, instructions: { root: dir } },
  );
  await app.ready;
  const address = await app.listen(0);
  t.after(() => app.close());
  const base = `http://127.0.0.1:${address.port}/ithomiini`;
  const session = () => {
    let cookie = '';
    let csrf = '';
    return async (path, method = 'GET', body, headers = {}) => {
      const response = await fetch(base + path, {
        method,
        headers: { ...(body ? { 'content-type': 'application/json' } : {}), ...(cookie ? { cookie, 'x-csrf-token': csrf } : {}), ...headers },
        body: body ? JSON.stringify({ requestId: randomUUID(), ...body }) : undefined,
      });
      const data = await response.json();
      if (response.headers.get('set-cookie')) cookie = response.headers.get('set-cookie').split(';')[0];
      if (data.csrf) csrf = data.csrf;
      return { status: response.status, data };
    };
  };
  const adminCall = session();
  const obsCall = session();
  assert.equal((await obsCall('/api/doc-review')).status, 401);
  await adminCall('/api/auth/setup', 'POST', { token: 'doc-setup', username: 'boss_admin', password: 'test-admin-123', displayName: 'Jefa' });
  createUser(app.store, { username: 'obs_person', displayName: 'Observadora', role: 'observer', password: 'obs-pass-1' });
  assert.equal((await obsCall('/api/auth/login', 'POST', { username: 'obs_person', password: 'obs-pass-1' })).status, 200);

  const path = 'assistant/skills/demo/SKILL.md';
  const made = await obsCall('/api/doc-review/threads', 'POST', { path, quote: 'Usa NA para el sexo', prefix: 'Datos', suffix: '', offset: 6, heading: 'Datos', body: 'NOT_COLLECTED?' });
  assert.equal(made.status, 200);
  const thread = made.data.thread;
  assert.equal((await adminCall(`/api/doc-review/threads/${thread.id}/comments`, 'POST', { body: 'Sí.' })).status, 200);
  const seen = await obsCall('/api/doc-review');
  assert.equal(seen.data.admin, false);
  assert.deepEqual(seen.data.threads[0].comments.map(c => [c.author, c.own]), [
    ['Observadora', true],
    ['Jefa', false],
  ]);
  assert.doesNotMatch(JSON.stringify(seen.data), /boss_admin|obs_person/);
  // «Pedir a Claude» is for admins; without T3 the request comes back to be pasted.
  assert.equal((await obsCall('/api/doc-review/ask', 'POST', {})).status, 403);
  const asked = await adminCall('/api/doc-review/ask', 'POST', {});
  assert.equal(asked.data.chat, null);
  assert.match(asked.data.prompt, /doc_threads.*edit-instructions/);

  // The admin's chat, with the admin's token.
  const adminId = app.store.db.prepare("SELECT id FROM users WHERE username = 'boss_admin'").get().id;
  app.store.db.prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-11-08')").run(createHash('sha256').update('admin-token').digest('hex'), adminId);
  const mcp = async (name, args) => {
    const response = await fetch(`${base}/api/ai/mcp`, {
      method: 'POST',
      headers: { 'content-type': 'application/json', authorization: 'Bearer admin-token' },
      body: JSON.stringify({ jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } }),
    });
    return JSON.parse((await response.json()).result.content[0].text);
  };
  const threads = await mcp('doc_threads', {});
  assert.equal(threads.threads[0].heading, 'Datos');
  const proposed = await mcp('propose_doc_change', { path, heading: 'Datos', before: 'Usa NA', after: 'Usa NOT_COLLECTED', thread_ids: [thread.id] });
  assert.equal(proposed.version, 1);
  assert.equal((await obsCall(`/api/doc-review/changes/${proposed.changeId}/apply`, 'POST', {})).status, 403);
  assert.equal((await obsCall(`/api/doc-review/changes/${proposed.changeId}/comments`, 'POST', { body: 'Bien' })).status, 200);
  const applied = await adminCall(`/api/doc-review/changes/${proposed.changeId}/apply`, 'POST', {});
  assert.equal(applied.status, 200);
  assert.equal(applied.data.change.status, 'applied');

  const page = await obsCall('/api/instructions');
  const entry = page.data.entries.find(e => e.id === path);
  assert.match(entry.content, /Usa NOT_COLLECTED para el sexo/);
  assert.equal(entry.revisions.length, 1);
  assert.equal(entry.revisions[0].author, 'Jefa');
  const diff = await obsCall(`/api/instructions/diff?id=${encodeURIComponent(path)}&commit=rev:${entry.revisions[0].revision}`);
  assert.match(diff.data.diff, /^\+NOT_COLLECTED$/m);
  const after = await obsCall('/api/doc-review');
  assert.equal(after.data.threads[0].status, 'resolved');
  assert.equal(after.data.changes[0].comments[0].author, 'Observadora');
  // Undo for admins only.
  assert.equal((await obsCall(`/api/doc-review/changes/${proposed.changeId}/undo`, 'POST', {})).status, 403);
  assert.equal((await adminCall(`/api/doc-review/changes/${proposed.changeId}/undo`, 'POST', {})).data.change.status, 'undone');
  assert.match((await obsCall('/api/instructions')).data.entries.find(e => e.id === path).content, /Usa NA para el sexo/);
});

test('a refresh writes the change into the workspaces: their brief and their skill copies', () => {
  const home = mkdtempSync(join(tmpdir(), 'doc-provision-'));
  const shared = join(home, 'ithomiini', 'shared');
  mkdirSync(shared, { recursive: true });
  const db = new DatabaseSync(join(shared, 'database.sqlite'));
  db.exec(`CREATE TABLE users(id TEXT PRIMARY KEY, username TEXT UNIQUE NOT NULL, display_name TEXT NOT NULL, role TEXT NOT NULL, active INTEGER NOT NULL DEFAULT 1);
    CREATE TABLE ai_tokens(token_hash TEXT PRIMARY KEY, user_id TEXT NOT NULL, label TEXT NOT NULL, created_at TEXT NOT NULL, revoked_at TEXT);
    INSERT INTO users VALUES ('u1','ana','Ana Pérez','editor',1);`);
  initDocReview(db);
  const revise = (rel, edit) => {
    const was = readFileSync(join(repoRoot, rel), 'utf8');
    const text = edit(was);
    db.prepare('INSERT INTO doc_revisions (path, parent_hash, hash, text, applied_at) VALUES (?,?,?,?,?)').run(rel, hashText(was), hashText(text), text, '2026-11-08');
  };
  revise('assistant/AGENTS.md', s => `${s.trimEnd()}\n\nA line applied in the app.\n`);
  revise('assistant/skills/data-rules/SKILL.md', s => `${s.trimEnd()}\n\nAnother line applied in the app.\n`);
  db.close();
  mkdirSync(join(home, '.config', 'ithomiini'), { recursive: true });
  writeFileSync(join(home, '.config', 'ithomiini', 'service.env'), 'APP_PORT=8794\n');
  writeFileSync(join(home, '.claude.json'), '{}');
  const env = { ...process.env, HOME: home, T3CODE_HOME: join(home, '.t3'), ITHOMIINI_SHARED: shared, T3_BIN: '/bin/true', CODEX_HOME: join(home, 'no-codex') };
  execFileSync(process.execPath, [join(repoRoot, 'scripts', 't3-provision.mjs'), 'ana'], { env, encoding: 'utf8' });
  const workspace = join(shared, 't3-workspaces', 'ana');
  assert.match(readFileSync(join(workspace, 'AGENTS.md'), 'utf8'), /A line applied in the app\.\n\n[\s\S]*Ana Pérez|Ana Pérez[\s\S]*A line applied in the app\./);
  for (const folder of ['.claude', '.agents'])
    assert.match(readFileSync(join(workspace, folder, 'skills', 'data-rules', 'SKILL.md'), 'utf8'), /Another line applied in the app\.\n$/);
  assert.doesNotMatch(readFileSync(join(workspace, '.claude', 'skills', 'app-dev', 'SKILL.md'), 'utf8'), /applied in the app/);
});

test('git: a change that grows a part records it in assistant/context-budget.json in the same commit', async () => {
  // A clone of this checkout's last commit: the real budget script measures it.
  const base = mkdtempSync(join(tmpdir(), 'doc-budget-'));
  const work = join(base, 'work');
  execFileSync('git', ['clone', '-q', '--no-hardlinks', repoRoot, work]);
  const g = (...args) => execFileSync('git', ['-C', work, ...args], { encoding: 'utf8' });
  const rel = 'assistant/AGENTS.md';
  const was = readFileSync(join(work, rel), 'utf8');
  const budgetBefore = JSON.parse(readFileSync(join(work, 'assistant', 'context-budget.json'), 'utf8')).parts.brief.bytes;
  const text = `${was.trimEnd()}\n\nOne more line.\n`;
  const sync = createGitSync({ worktree: work, push: false, author: { name: 'App', email: 'app@example.invalid' } });
  const out = await sync.commit({ path: rel, text, parentHash: hashText(was), message: 'Instrucciones: AGENTS.md § Brief', budgetReason: 'Instrucciones: AGENTS.md § Brief (doc change x)' });
  assert.equal(out.state, 'committed');
  assert.deepEqual(g('show', '--name-only', '--format=', 'HEAD').trim().split('\n').sort(), ['assistant/AGENTS.md', 'assistant/context-budget.json']);
  const brief = JSON.parse(readFileSync(join(work, 'assistant', 'context-budget.json'), 'utf8')).parts.brief;
  assert.equal(brief.bytes, budgetBefore + Buffer.byteLength('\n\nOne more line.'));
  assert.equal(brief.reason, 'Instrucciones: AGENTS.md § Brief (doc change x)');
});
