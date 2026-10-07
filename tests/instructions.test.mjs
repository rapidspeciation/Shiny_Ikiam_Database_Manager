import test, { after, before } from 'node:test';
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { randomUUID } from 'node:crypto';
import { cpSync, mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join } from 'node:path';
import { createApp } from '../server/index.mjs';
import { REPO_ROOT, TOOL_RANGES, createInstructions, frontMatter, parseLog, writeHistory } from '../server/instructions.mjs';

// A checkout with the real assistant/ folder and a short history: the brief was assistant/CLAUDE.md
// first, then renamed to AGENTS.md; the tool descriptions' blocks changed twice.
let repo;
before(() => {
  repo = mkdtempSync(join(tmpdir(), 'instructions-repo-'));
  const git = (...args) => execFileSync('git', ['-C', repo, '-c', 'user.name=Ana', '-c', 'user.email=ana@example.test', ...args]);
  const tools = text => {
    for (const [file, from, to] of TOOL_RANGES) {
      mkdirSync(dirname(join(repo, file)), { recursive: true });
      const [start, end] = [from, to].map(re => re.slice(2, -1).replace(/\\/g, ''));
      writeFileSync(join(repo, file), `${start} = [\n  '${text}',\n${end}\n`);
    }
  };
  git('init', '-q');
  const brief = readFileSync(join(REPO_ROOT, 'assistant', 'AGENTS.md'), 'utf8');
  mkdirSync(join(repo, 'assistant'));
  writeFileSync(join(repo, 'assistant', 'CLAUDE.md'), brief.replace(/\n[^\n]*\n*$/, '\n'));
  tools('first');
  git('add', '-A');
  git('commit', '-qm', 'First brief');
  rmSync(join(repo, 'assistant', 'CLAUDE.md'));
  cpSync(join(REPO_ROOT, 'assistant'), join(repo, 'assistant'), { recursive: true });
  tools('second');
  git('add', '-A');
  git('commit', '-qm', 'assistant/CLAUDE.md → assistant/AGENTS.md');
});
after(() => rmSync(repo, { recursive: true, force: true }));

test('the AI instructions page: every file in order, the live tool list, history from git; only for signed-in people', async t => {
  const app = await createApp(
    { databasePath: ':memory:', localMode: true, secureCookies: false, syncIntervalMs: 0, setupToken: 'instructions-setup' },
    { seed: {}, instructions: { root: repo } },
  );
  await app.ready;
  const address = await app.listen(0);
  t.after(() => app.close());
  const base = `http://127.0.0.1:${address.port}/ithomiini`;
  let cookie = '';
  let csrf = '';
  async function call(path, method = 'GET', body) {
    const response = await fetch(base + path, {
      method,
      headers: { ...(body ? { 'content-type': 'application/json' } : {}), ...(cookie ? { cookie, 'x-csrf-token': csrf } : {}) },
      body: body ? JSON.stringify({ requestId: randomUUID(), ...body }) : undefined,
    });
    const data = await response.json();
    if (response.headers.get('set-cookie')) cookie = response.headers.get('set-cookie').split(';')[0];
    if (data.csrf) csrf = data.csrf;
    return { status: response.status, data };
  }
  assert.equal((await call('/api/instructions')).status, 401);
  await call('/api/auth/setup', 'POST', { token: 'instructions-setup', username: 'reader_admin', password: 'test-admin-123' });

  const { status, data } = await call('/api/instructions');
  assert.equal(status, 200);
  const ids = data.entries.map(e => e.id);
  assert.equal(ids[0], 'assistant/AGENTS.md');
  assert.equal(data.entries.filter(e => e.group === 'brief').length, 1, 'one brief');
  assert.equal(ids.at(-1), 'tools');
  assert.deepEqual([...new Set(data.entries.map(e => e.group))], ['brief', 'skills', 'agents', 'tools']);
  // Each skill: its SKILL.md first (titled by the skill), then its reference files.
  const guide = data.entries.filter(e => e.skill === 'app-guide');
  assert.equal(guide[0].id, 'assistant/skills/app-guide/SKILL.md');
  assert.equal(guide[0].title, 'app-guide');
  assert.equal(guide[0].meta.name, 'app-guide');
  assert.ok(guide.some(e => e.title === 'reference/monitoreo.md'));
  assert.ok(!guide[0].content.startsWith('---'), 'the front matter is shown apart');
  const reader = data.entries.find(e => e.id === 'assistant/agents/notebook-reader.md');
  assert.equal(reader.group, 'agents');
  assert.match(reader.meta.model, /^claude-/);

  // The brief as a workspace on the server gets it, with a generic person.
  const agents = data.entries[0].content;
  assert.ok(agents.includes('‹person›') && agents.includes('‹username›'), 'the generic person filled in');
  assert.ok(agents.includes('`/home/ubuntu/ithomiini/current/docs`'), 'the docs folder filled in');
  assert.doesNotMatch(agents, /\{\{/);
  assert.doesNotMatch(agents, /Bearer\s+\S{16,}|sk-[A-Za-z0-9_-]{16,}|PRIVATE KEY|client_secret|password\s*[:=]/i, 'no secrets');

  // The tools exactly as T3 Code's chats get them over MCP.
  const tools = data.entries.at(-1).tools;
  assert.ok(tools.length >= 20);
  assert.ok(tools.every(tool => tool.name && tool.description && tool.inputSchema?.type === 'object'));
  assert.ok(tools.some(tool => tool.name === 'match_notebook'));

  assert.equal(data.historySource, 'git');
  const brief = data.entries[0];
  assert.equal(brief.lastChanged, brief.history[0].date);
  assert.ok(!('diff' in brief.history[0]), 'the list carries no diffs');
  // The history follows the rename from assistant/CLAUDE.md.
  assert.deepEqual(brief.history.map(c => c.subject), ['assistant/CLAUDE.md → assistant/AGENTS.md', 'First brief']);
  assert.equal(data.entries.at(-1).history.length, 2, 'the tool descriptions have a history');

  const diff = await call(`/api/instructions/diff?id=${encodeURIComponent(brief.id)}&commit=${brief.history[0].commit}`);
  assert.equal(diff.status, 200);
  assert.match(diff.data.diff, /^@@ /m);
  assert.equal((await call(`/api/instructions/diff?id=${encodeURIComponent(brief.id)}&commit=nope`)).status, 404);
});

test('a release (no .git) reads the history written at build time', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'instructions-'));
  try {
    cpSync(join(repo, 'assistant'), join(dir, 'assistant'), { recursive: true });
    const file = join(dir, 'history.json');
    const written = await writeHistory(repo, file);
    const page = await createInstructions({ root: dir, historyFile: file, tools: () => [{ name: 'x', description: 'y', inputSchema: { type: 'object' } }] }).list();
    assert.equal(page.historySource, 'release');
    assert.equal(page.historyHead, written.head);
    const skill = page.entries.find(e => e.id === 'assistant/skills/monitoring/SKILL.md');
    assert.equal(skill.history.length, 1);
    assert.equal(page.entries[0].history.length, 2);
    assert.deepEqual(page.entries.at(-1).tools.map(tool => tool.name), ['x']);
    // Without the file, the page still shows the files, without history.
    const bare = await createInstructions({ root: dir, historyFile: join(dir, 'missing.json') }).list();
    assert.equal(bare.historySource, null);
    assert.equal(bare.entries[0].history, null);
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});

test('git log with a word diff is read commit by commit; front matter is split from the text', () => {
  const log =
    '\x1eabc\x1f2026-09-30T20:00:00-05:00\x1fFranz\x1fA subject: with colons\n\ndiff --git a/f.md b/f.md\nindex 1..2 100644\n--- a/f.md\n+++ b/f.md\n@@ -1 +1 @@\n old\n-word\n+words\n~\n' +
    '\x1edef\x1f2026-09-29T10:00:00-05:00\x1fAna\x1fFirst\n\ndiff --git a/f.md b/f.md\nnew file mode 100644\n';
  const commits = parseLog(log);
  assert.deepEqual(
    commits.map(c => [c.commit, c.author, c.subject]),
    [
      ['abc', 'Franz', 'A subject: with colons'],
      ['def', 'Ana', 'First'],
    ],
  );
  assert.ok(!commits[0].diff.includes('index 1..2'));
  assert.match(commits[0].diff, /^-word\n\+words$/m);
  assert.deepEqual(frontMatter('---\nname: x\ndescription: a: b\n---\n\n# Title\n'), { meta: { name: 'x', description: 'a: b' }, body: '# Title\n' });
  assert.deepEqual(frontMatter('# No front matter'), { meta: null, body: '# No front matter' });
});
