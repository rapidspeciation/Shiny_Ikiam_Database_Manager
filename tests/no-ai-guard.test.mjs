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
import { Worker } from 'node:worker_threads';
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
  assert.match(got[0], /refused CLI \S*claude/);
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

// Round 3 (review 9 Nov): worker threads, a child's own env, shells given a script, T3 ports on any address.
test('a worker thread gets the guard and reports to the test; its workerData arrives', { skip: off }, async () => {
  const dir = mkdtempSync(join(tmpdir(), 'no-ai-worker-'));
  const file = join(dir, 'w.mjs');
  writeFileSync(file, `import net from 'node:net'; import { parentPort, workerData } from 'node:worker_threads';
const s = net.connect(3774, '127.0.0.1'); s.on('error', e => parentPort.postMessage({ code: e.code, data: workerData })); s.on('connect', () => { parentPort.postMessage({ code: 'CONNECTED' }); s.destroy(); });`);
  const w = new Worker(file, { workerData: { role: 'tools' } });
  const got = await new Promise((res, rej) => { w.once('message', res); w.once('error', rej); });
  await w.terminate();
  assert.deepEqual(got, { code: 'ECONNREFUSED', data: { role: 'tools' } });
  const lines = guard.take();
  assert.equal(lines.length, 1, lines.join('\n'));
  assert.match(lines[0], /refused connection to T3 port 127\.0\.0\.1:3774/);
});

test('a node child given its own env keeps the guard, and the PATH it was given', { skip: off }, () => {
  const probe = "process.stdout.write(String(!!globalThis[Symbol.for('ithomiini.noAiGuard')]) + ' ' + process.env.PATH); fetch('https://api.openai.com/v1/models').catch(() => {})";
  const out = execFileSync(process.execPath, ['-e', probe], { env: { PATH: '/usr/bin:/bin' }, encoding: 'utf8' });
  assert.equal(out, 'true /usr/bin:/bin');
  const lines = guard.take();
  assert.equal(lines.length, 1, lines.join('\n'));
  assert.match(lines[0], /AI host api\.openai\.com/);
});

test('a shell given a script that names a model CLI (by name or path) is refused; other scripts run', { skip: off }, () => {
  assert.equal(spawnSync('bash', ['-c', '/opt/elsewhere/claude -p hola']).status, 1);
  assert.equal(spawnSync('bash', ['-lc', 'command -v claude']).status, 1);
  assert.throws(() => execSync("env -i PATH=/usr/bin:/bin sh -c 'codex exec hola'", { stdio: 'pipe' }));
  assert.equal(spawnSync('env', ['PATH=/usr/bin:/bin', 'opencode', 'run']).status, 1);
  const lines = guard.take();
  assert.equal(lines.length, 4, lines.join('\n'));
  assert.match(lines[0], /refused CLI \/opt\/elsewhere\/claude/);
  assert.equal(execFileSync('bash', ['-c', 'echo claude; git --version >/dev/null'], { encoding: 'utf8' }), 'claude\n');
  assert.deepEqual(guard.take(), []);
});

test('the T3 ports are refused on any address (the tailnet one too), offline', { skip: off }, async () => {
  for (const host of ['100.83.134.33', '192.0.2.10']) await assert.rejects(fetch(`http://${host}:3775/`));
  const lines = guard.take();
  assert.equal(lines.length, 2, lines.join('\n'));
  assert.match(lines[0], /T3 port 100\.83\.134\.33:3775/);
});
