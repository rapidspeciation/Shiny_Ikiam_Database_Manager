// No test uses the AI subscriptions (Franz, 9 Nov 2026): the eval cases (tools/assistant-eval/tasks/*.json) run only
// from the manual eval runner, never from the test suite, scripts/deploy.sh, CI or a timer.
//
// Static guard: the test files (tests/*.test.mjs with their helpers and fixtures, frontend __tests__ and *.test.*)
// must not reference the eval runner (run.mjs, iterate.sh, stop-check.mjs), tools/whatsapp/t3-turn.mjs or
// calibrate-gate.mjs; may import soul-run.mjs only statically (its helpers; its chat-starting main runs only as a
// script); must not spawn or exec `claude`, `codex`, a path ending in /claude or /codex, or `ssh`; must not call
// haikuGate without an injected spawnImpl; and must not name a real T3 (127.0.0.1:3774 main, :3775 lab).
// Deploy, package scripts, CI workflows and systemd units must not reference the eval runner, iterate.sh or soul-run.
//
// The existing tests that mention T3 or claude were read on 9 Nov and are all mocked:
//  - soul-run.test.mjs: only chatLimit/runPool/withArchive and runScenario with a fake createBridge and archive.
//  - whatsapp.test.mjs: the Haiku gate gets an injected spawnImpl; 'claude' appears as an asserted argument only.
//  - whatsapp-bridge.test.mjs: turns run stand-in scripts written to a temp dir, wacli is a shell stub; the
//    127.0.0.1:3774/3775 strings in «only the two live routes…» are route labels for the live-route check, which
//    calls send() only (wacli stub), never a turn (allowed below by exact count).
//  - t3commands.test.mjs, t3projects.test.mjs: injected fetchImpl, fake URLs (127.0.0.1:9, *.test).
//  - t3admin / t3-update / t3-provision / t3bridge / t3chats / t3-progress: fake fetch, local stand-in servers on
//    port 0, T3_BIN=/bin/true, or stored rows; 'claude' only as process-table text or config names.
// Runtime belt: run.mjs, t3-turn.mjs, soul-run's main, the bridge's ssh turn and the gate's real `claude -p` refuse to
// run when NODE_TEST_CONTEXT is set (node --test sets it for its children); the tests at the end show it.
import test from 'node:test';
import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { existsSync, mkdtempSync, readFileSync, readdirSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { basename, join, relative } from 'node:path';
import { fileURLToPath } from 'node:url';

const ROOT = fileURLToPath(new URL('..', import.meta.url));
const SELF = fileURLToPath(import.meta.url);
const SKIP_DIRS = new Set(['node_modules', '.git', 'dist', '.work']);

function walk(dir, keep, out = []) {
  if (!existsSync(dir)) return out;
  for (const e of readdirSync(dir, { withFileTypes: true })) {
    const p = join(dir, e.name);
    if (e.isDirectory()) {
      if (!SKIP_DIRS.has(e.name)) walk(p, keep, out);
    } else if (keep(p)) out.push(p);
  }
  return out;
}

function testFiles() {
  const tests = walk(join(ROOT, 'tests'), p => /\.(mjs|js|cjs|ts)$/.test(p) && p !== SELF);
  const frontend = walk(join(ROOT, 'frontend'), p => /[\\/]__tests__[\\/]/.test(p) || /\.test\.[cm]?[jt]sx?$/.test(basename(p)));
  return [...tests, ...frontend];
}

const EVAL_REF = /assistant-eval\/(run\.mjs|iterate\.sh|stop-check\.mjs)|['"`/](run\.mjs|iterate\.sh|stop-check\.mjs)['"`]|t3-turn\.mjs|calibrate-gate\.mjs/;
const SOUL_STATIC_IMPORT = /^\s*import\s+[^;]*?from\s+['"][^'"]*soul-run\.mjs['"];?\s*$/gm;
const SPAWN_CALL = /\b(spawn|spawnSync|exec|execSync|execFile|execFileSync|fork|execa)\s*\(\s*(['"`])((?:(?!\2).)*)\2/g;
const AI_BIN = /(^|[\s/;&|(])(claude|codex|ssh)(\s|$)/;
const REAL_T3 = /(127\.0\.0\.1|localhost|\[::1\]):377[45]\b/g;
// file → how many real-T3 strings it may hold, and why they are never fetched.
const REAL_T3_ALLOWED = { 'tests/whatsapp-bridge.test.mjs': 3 }; // route labels for the live-route check; send() only, wacli stub
// The runtime guard's own tests (tests/helpers/no-ai-core.mjs): they start claude and codex and reach a T3 port on purpose,
// under that guard, to show each attempt is refused. Not scanned here; tests/no-ai-guard.test.mjs fails if the guard is off.
const GUARD_TESTS = new Set(['tests/no-ai-guard.test.mjs', 'frontend/src/lib/__tests__/noAiGuard.test.ts']);

/** The reasons a test file could reach an AI subscription or a real T3, empty when it cannot. */
export function problems(rel, source) {
  // Whole-line and block comments are prose («a stand-in for t3-turn.mjs»), not code.
  const text = source.replace(/\/\*[\s\S]*?\*\//g, '').replace(/^\s*\/\/.*$/gm, '');
  const out = [];
  const m = text.match(EVAL_REF);
  if (m) out.push(`references the eval runner or a chat starter (${m[0]})`);
  if (/soul-run\.mjs/.test(text.replace(SOUL_STATIC_IMPORT, ''))) out.push('uses soul-run.mjs other than a static import of its helpers');
  for (const [, fn, , arg] of text.matchAll(SPAWN_CALL)) if (AI_BIN.test(arg)) out.push(`${fn}(${JSON.stringify(arg)}) starts claude, codex or ssh`);
  for (const call of text.matchAll(/\bhaikuGate\s*\(([^)]*)\)/g)) if (!/spawnImpl/.test(call[1])) out.push('haikuGate() without an injected spawnImpl runs the real claude -p');
  const t3 = text.match(REAL_T3)?.length ?? 0;
  if (t3 > (REAL_T3_ALLOWED[rel] ?? 0)) out.push(`names a real T3 (127.0.0.1:3774/3775) ${t3} times`);
  return out;
}

test('no test file imports the eval runner or a chat starter, spawns claude/codex, or reaches a real T3', () => {
  const files = testFiles();
  assert.ok(files.length > 50, `found only ${files.length} test files`);
  const bad = files.flatMap(f => {
    const rel = relative(ROOT, f).split('\\').join('/');
    if (GUARD_TESTS.has(rel)) return [];
    return problems(rel, readFileSync(f, 'utf8')).map(p => `${rel}: ${p}`);
  });
  assert.deepEqual(bad, []);
});

test('the guard catches the patterns it is for, and lets the stand-ins through', () => {
  const bad = [
    "import '../tools/assistant-eval/run.mjs';",
    "spawn(process.execPath, [join(HERE, 'run.mjs')]);",
    "execFileSync('bash', ['tools/assistant-eval/iterate.sh', 'x']);",
    "spawn(process.execPath, ['../tools/whatsapp/t3-turn.mjs']);",
    "await import('../tools/whatsapp/soul-run.mjs');",
    "spawn('claude', ['-p']);",
    "execFile('/home/franz/.local/bin/codex', []);",
    "execSync('claude -p hola');",
    "spawnSync('ssh', ['claudeclaw', 'node t3-turn']);",
    'haikuGate({ bin: "claude" });',
    "await fetch('http://127.0.0.1:3775/api/orchestration/dispatch');",
    "spawn(process.execPath, [t]); // runs t3-turn.mjs",
  ];
  for (const t of bad) assert.notDeepEqual(problems('tests/x.test.mjs', t), [], t);
  const good = [
    "import { chatLimit, runPool } from '../tools/whatsapp/soul-run.mjs';",
    'const gate = haikuGate({ spawnImpl });',
    "assert.equal(seen.bin, 'claude');",
    "chatStarter({ local: 'http://127.0.0.1:9', fetchImpl });",
    "t3Admin({ local: 'http://t3-045.test', spawnImpl });",
    "spawn(process.execPath, [script]);",
    "import { progressUpdates } from '../tools/whatsapp/t3-progress.mjs';",
    '  // A stand-in for t3-turn.mjs: answers with what the chat would.',
  ];
  for (const t of good) assert.deepEqual(problems('tests/x.test.mjs', t), [], t);
});

test('deploy, package scripts, CI and systemd units never run the eval runner, iterate.sh or soul-run', () => {
  const RUNNER = /assistant-eval\/(run|iterate|stop-check)|iterate\.sh|soul-run/;
  const files = [
    join(ROOT, 'scripts', 'deploy.sh'),
    ...walk(join(ROOT, '.github'), p => /\.ya?ml$/.test(p)),
    ...walk(ROOT, p => /\.(service|timer)$/.test(p)),
  ].filter(existsSync);
  assert.ok(files.some(f => f.endsWith('deploy.sh')), 'scripts/deploy.sh not found');
  const bad = files.filter(f => RUNNER.test(readFileSync(f, 'utf8'))).map(f => relative(ROOT, f));
  for (const pkg of [join(ROOT, 'package.json'), join(ROOT, 'frontend', 'package.json')].filter(existsSync)) {
    for (const [name, cmd] of Object.entries(JSON.parse(readFileSync(pkg, 'utf8')).scripts ?? {}))
      if (RUNNER.test(cmd)) bad.push(`${relative(ROOT, pkg)} scripts.${name}`);
  }
  assert.deepEqual(bad, []);
});

// --- runtime belt (every child gets NODE_TEST_CONTEXT explicitly, also when this file runs without node --test)
const UNDER_TEST = { ...process.env, NODE_TEST_CONTEXT: 'child-v8' };
const REFUSED = /refusing to run under node --test/;

test('run.mjs refuses under node --test before any chat: no out dir made, exit non-zero', () => {
  const dir = mkdtempSync(join(tmpdir(), 'no-ai-run-'));
  const tasks = join(dir, 'tasks.json');
  writeFileSync(tasks, '{"tasks":[]}');
  const out = join(dir, 'out');
  const r = spawnSync(process.execPath, [join(ROOT, 'tools/assistant-eval/run.mjs'), tasks, out, '--target', 'lab'], { env: UNDER_TEST, encoding: 'utf8', timeout: 20_000 });
  assert.notEqual(r.status, 0);
  assert.match(r.stderr, REFUSED);
  assert.ok(!existsSync(out));
});

test('t3-turn.mjs refuses under node --test without reading its request', () => {
  const r = spawnSync(process.execPath, [join(ROOT, 'tools/whatsapp/t3-turn.mjs')], {
    env: UNDER_TEST,
    input: JSON.stringify({ op: 'busy', home: '/nonexistent', local: 'http://t3-guard.test', threadId: 'T' }),
    encoding: 'utf8',
    timeout: 20_000,
  });
  assert.equal(r.status, 3);
  assert.match(r.stderr, REFUSED);
  assert.equal(JSON.parse(r.stdout).ok, false);
});

test("soul-run's chat-starting entry refuses under node --test", () => {
  const dir = mkdtempSync(join(tmpdir(), 'no-ai-soul-'));
  const r = spawnSync(process.execPath, [join(ROOT, 'tools/whatsapp/soul-run.mjs'), join(dir, 'none.json'), join(dir, 'none.md'), join(dir, 'out')], { env: UNDER_TEST, encoding: 'utf8', timeout: 20_000 });
  assert.notEqual(r.status, 0);
  assert.match(r.stderr, REFUSED);
  assert.ok(!existsSync(join(dir, 'out')));
});

test('the Haiku gate with its real spawn and the bridge ssh turn refuse under node --test', async () => {
  const before = process.env.NODE_TEST_CONTEXT;
  process.env.NODE_TEST_CONTEXT = before ?? 'child-v8';
  try {
    const { haikuGate } = await import('../server/whatsapp-gate.mjs');
    // A bin that does not exist, so even a broken belt could not reach the real claude.
    const g = await haikuGate({ bin: '/nonexistent/claude-guard', timeoutMs: 2000 })({ line: 'hola' });
    assert.match(g.error, REFUSED);

    const { createBridge } = await import('../tools/whatsapp/bridge.mjs');
    const dir = mkdtempSync(join(tmpdir(), 'no-ai-bridge-'));
    const config = {
      chats: { self: { jid: 'ME@s.whatsapp.net', label: 'yo', route: 'r', enabled: true, dryRun: true, repliesHour: 60, contextLines: 0 } },
      routes: { r: { ssh: 'claudeclaw-guard.invalid', node: '/nonexistent/node', script: '/nonexistent/t3-turn' } },
      wacli: { bin: '/bin/false', store: dir },
    };
    const bridge = createBridge({ config, stateDir: join(dir, 'state'), log: { error() {}, info() {} } });
    bridge.handle({ Chat: 'ME@s.whatsapp.net', ID: 'G1', SenderJID: 'ME@s.whatsapp.net', Timestamp: new Date().toISOString(), FromMe: true, Text: '@claude hola' });
    for (let i = 0; i < 100 && !bridge.idle(); i++) await new Promise(r => setTimeout(r, 50));
    const log = join(dir, 'state', 'bridge.log.jsonl');
    const turns = readFileSync(log, 'utf8').trim().split('\n').map(l => JSON.parse(l)).filter(l => l.dir === 'turn' && 'ok' in l);
    assert.equal(turns.length, 1);
    assert.equal(turns[0].ok, false);
    assert.match(turns[0].error, REFUSED);
  } finally {
    if (before === undefined) delete process.env.NODE_TEST_CONTEXT;
    else process.env.NODE_TEST_CONTEXT = before;
  }
});
