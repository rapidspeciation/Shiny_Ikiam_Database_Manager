// The guard that keeps every test off Franz's AI subscriptions and T3 (tests/helpers/no-ai-core.mjs): loaded in this run,
// and refusing what it should, offline, while a test's own stubs and loopback servers still work.
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { execFile, execFileSync, execSync, spawnSync } from 'node:child_process';
import { mkdtempSync, writeFileSync } from 'node:fs';
import { createServer } from 'node:http';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { promisify } from 'node:util';
import { blockedCli, blockedHost, noAiGuard } from './helpers/no-ai-core.mjs';

const guard = noAiGuard();
// Without the guard only the first test runs (and fails): the others would reach the real CLIs and hosts.
const off = !guard && 'the guard is not loaded';

test('the run loads the guard (node --test --import ./tests/helpers/no-ai.mjs, as package.json and scripts/deploy.sh)', () => {
  assert.ok(guard, 'run the server tests with: node --test --import ./tests/helpers/no-ai.mjs tests/*.test.mjs');
  assert.equal(process.env.PATH.split(':')[0], guard.shims);
  assert.match(process.env.NODE_OPTIONS, /--import=\S+\/tests\/helpers\/no-ai\.mjs/);
  for (const k of ['ANTHROPIC_API_KEY', 'OPENAI_API_KEY', 'ITHOMIINI_AI_API_KEY', 'ITHOMIINI_T3_URL', 'T3CODE_HOME', 'T3CODE_PORT', 'CLAUDE_CONFIG_DIR'])
    assert.equal(process.env[k], undefined, k);
});

test('a model CLI, wacli or ssh is refused and written down; a stub a test wrote is started', { skip: off }, async () => {
  const before = guard.take();
  assert.deepEqual(before, []);
  // By name (the shim on PATH stands for it), by the shell, and by the real path.
  assert.equal(spawnSync('claude', ['-p', 'hola']).status, 1);
  assert.throws(() => execSync('cd /tmp && codex exec hi', { stdio: 'pipe' }));
  await assert.rejects(promisify(execFile)('t3', ['serve']));
  assert.throws(() => execFileSync('/usr/local/bin/opencode', ['run'], { stdio: 'pipe' }));
  const got = guard.take();
  assert.equal(got.length, 4, got.join('\n'));
  for (const [i, cli] of ['claude', 'codex', 't3', 'opencode'].entries()) assert.match(got[i], new RegExp(`refused CLI \\S*${cli}`));

  const dir = mkdtempSync(join(tmpdir(), 'no-ai-stub-'));
  writeFileSync(join(dir, 'claude'), '#!/bin/sh\necho stub "$@"\n', { mode: 0o755 });
  assert.equal(execFileSync(join(dir, 'claude'), ['-p', 'x'], { encoding: 'utf8' }), 'stub -p x\n');
  assert.equal(execFileSync('claude', ['y'], { encoding: 'utf8', env: { PATH: `${dir}:/usr/bin:/bin` } }), 'stub y\n');
  assert.equal(blockedCli('git'), null);
  assert.deepEqual(guard.take(), []);
});

test('a script a test runs, and the node processes it starts, report to the test', { skip: off }, () => {
  // A shell script calling claude meets the PATH shim; a node child gets the guard through NODE_OPTIONS.
  spawnSync('sh', ['-c', 'x=1; claude --version'], { stdio: 'ignore' });
  spawnSync(process.execPath, ['-e', "fetch('https://api.anthropic.com/v1/messages').catch(() => {})"], { stdio: 'ignore' });
  const got = guard.take();
  assert.equal(got.length, 2, got.join('\n'));
  assert.match(got[0], /refused CLI claude/);
  assert.match(got[1], /refused connection to AI host api\.anthropic\.com:443/);
});

test('AI hosts and T3 are refused before any name lookup (no network needed); loopback test servers work', { skip: off }, async () => {
  for (const url of ['https://api.anthropic.com/v1/messages', 'https://api.openai.com/v1/chat/completions', 'https://openrouter.ai/api/v1', 'https://generativelanguage.googleapis.com/v1beta', 'https://t3.ithomiini-ikiam.com/', 'http://127.0.0.1:3774/api/orchestration/dispatch'])
    await assert.rejects(fetch(url), url);
  const got = guard.take();
  assert.equal(got.length, 6, got.join('\n'));
  assert.match(got[5], /refused connection to T3 port 127\.0\.0\.1:3774/);

  const server = createServer((req, res) => res.end('ok'));
  await new Promise(r => server.listen(0, '127.0.0.1', r));
  try {
    assert.equal(await (await fetch(`http://127.0.0.1:${server.address().port}/`)).text(), 'ok');
  } finally {
    server.close();
  }
  assert.equal(blockedHost({ host: 'api.gbif.org', port: 443 }), null);
  assert.equal(blockedHost({ host: 'example.anthropic.com.evil.test', port: 443 }), null);
  assert.deepEqual(guard.take(), []);
});
