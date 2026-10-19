import assert from 'node:assert/strict';
import { existsSync, mkdtempSync, readFileSync, writeFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { test } from 'node:test';
import { openBrowser } from '../tools/wikiloc/browser.mjs';

const directory = mkdtempSync(join(tmpdir(), 'wikiloc-browser-test-'));
const fakePython = join(directory, 'python');
writeFileSync(fakePython, `#!/usr/bin/env node
const { spawn } = require('node:child_process');
const { writeFileSync } = require('node:fs');
let input = '';
if (process.env.FAKE_READY_FILE) writeFileSync(process.env.FAKE_READY_FILE, 'ready');
process.stdin.on('data', chunk => {
  input += chunk;
  for (let end; (end = input.indexOf('\\n')) >= 0;) {
    const request = JSON.parse(input.slice(0, end));
    input = input.slice(end + 1);
    if (process.env.FAKE_MODE === 'death') process.exit(4);
    if (process.env.FAKE_MODE === 'hang_tree') {
      const child = spawn(process.execPath, ['-e', 'setInterval(() => {}, 1000)'], { stdio: 'ignore' });
      writeFileSync(process.env.FAKE_CHILD_FILE, String(child.pid));
      continue;
    }
    if (process.env.FAKE_MODE === 'hang') continue;
    if (process.env.FAKE_MODE === 'invalid') { process.stdout.write('bad json\\n'); continue; }
    if (request.op === 'goto') process.stderr.write('wikiloc: check on ' + request.url + ' (turnstile), try 1\\nother library noise\\n');
    const data = request.op === 'title' ? 'Trail' : request.op === 'evaluate' ? request.code : null;
    process.stdout.write(JSON.stringify({ id: request.id, data }) + '\\n');
    if (request.op === 'close') {
      if (process.env.FAKE_MODE === 'slow_close') setTimeout(() => process.exit(0), 100);
      else process.exit(0);
    }
  }
});
`, { mode: 0o755 });

const originalPython = process.env.ITHOMIINI_WIKILOC_PYTHON;
process.env.ITHOMIINI_WIKILOC_PYTHON = fakePython;
test.after(() => {
  if (originalPython === undefined) delete process.env.ITHOMIINI_WIKILOC_PYTHON;
  else process.env.ITHOMIINI_WIKILOC_PYTHON = originalPython;
  delete process.env.FAKE_MODE;
  rmSync(directory, { recursive: true, force: true });
});

test('browser adapter sends requests and closes', async () => {
  const browser = await openBrowser({ timeoutMs: 1000 });
  try {
    await browser.page.goto('https://es.wikiloc.com/');
    assert.equal(await browser.page.title(), 'Trail');
    assert.match(await browser.page.evaluate(() => window.mapData), /window.mapData/);
  } finally {
    await browser.close();
  }
});

test("the browser's own lines (what Cloudflare showed) reach the worker's log", async () => {
  const lines = [];
  const browser = await openBrowser({ timeoutMs: 1000, log: line => lines.push(line) });
  try {
    await browser.page.goto('https://es.wikiloc.com/x-1');
    await browser.page.title();
    for (let i = 0; i < 50 && !lines.length; i++) await new Promise(resolve => setTimeout(resolve, 10));
    assert.deepEqual(lines, ['check on https://es.wikiloc.com/x-1 (turnstile), try 1']);
  } finally {
    await browser.close();
  }
});

test('browser process death rejects pending requests', async () => {
  process.env.FAKE_MODE = 'death';
  const browser = await openBrowser({ timeoutMs: 1000 });
  await assert.rejects(browser.page.goto('https://es.wikiloc.com/'), /exited/);
  delete process.env.FAKE_MODE;
});

test('close waits for Python teardown after the close response', async () => {
  process.env.FAKE_MODE = 'slow_close';
  const browser = await openBrowser({ timeoutMs: 1000 });
  const started = Date.now();
  await browser.close();
  assert.ok(Date.now() - started >= 90);
  delete process.env.FAKE_MODE;
});

test('malformed responses reject pending requests', async () => {
  process.env.FAKE_MODE = 'invalid';
  const browser = await openBrowser({ timeoutMs: 1000 });
  await assert.rejects(browser.page.title(), /invalid JSON/);
  delete process.env.FAKE_MODE;
});

test('timed out requests terminate the browser', async () => {
  process.env.FAKE_MODE = 'hang';
  const browser = await openBrowser({ timeoutMs: 100 });
  await assert.rejects(browser.page.goto('https://es.wikiloc.com/'), /timed out/);
  delete process.env.FAKE_MODE;
});

test('timeout stops browser descendants', { skip: process.platform !== 'linux' }, async () => {
  process.env.FAKE_MODE = 'hang_tree';
  process.env.FAKE_CHILD_FILE = join(directory, 'child.pid');
  process.env.FAKE_READY_FILE = join(directory, 'ready');
  const browser = await openBrowser({ timeoutMs: 150 });
  // Started (a busy machine may take longer than the timeout to start a process): then the request.
  for (let i = 0; i < 500 && !existsSync(process.env.FAKE_READY_FILE); i++) await new Promise(resolve => setTimeout(resolve, 10));
  await assert.rejects(browser.page.goto('https://es.wikiloc.com/'), /timed out/);
  const pid = Number(readFileSync(process.env.FAKE_CHILD_FILE, 'utf8'));
  let running = true;
  for (let i = 0; i < 20; i++) {
    const proc = `/proc/${pid}/stat`;
    running = existsSync(proc) && !/\) Z /.test(readFileSync(proc, 'utf8'));
    if (!running) break;
    await new Promise(resolve => setTimeout(resolve, 50));
  }
  assert.equal(running, false);
  delete process.env.FAKE_CHILD_FILE;
  delete process.env.FAKE_READY_FILE;
  delete process.env.FAKE_MODE;
});

test("the checkbox click moves like a hand: a curved path in 0.4–1.2 s, overshoot and back, a dwell, a 60–120 ms press", { skip: !existsSync('/usr/bin/python3') }, async () => {
  const { execFileSync } = await import('node:child_process');
  const script = `
import json, math, random, sys
sys.path.insert(0, sys.argv[1])
from xinput import click_plan, human_path
out = []
for seed in range(40):
    rng = random.Random(seed)
    plan = click_plan(900, 700, 220, 338, rng)
    moves = [p for p in plan if p[0] == 'move']
    path = human_path(900, 700, 220, 338, random.Random(seed))
    straight = [abs((220 - 900) * (700 - y) - (900 - x) * (338 - 700)) / math.hypot(220 - 900, 338 - 700) for x, y in path]
    out.append({
        'moving': sum(p[3] for p in moves[:-1]),
        'dwell': moves[-1][3],
        'end': moves[-1][1:3],
        'press': plan[-2][3],
        'kinds': [p[0] for p in plan[-2:]],
        'farthest': max(straight),
        'past': max(x < 220 - 1 for x, _ in path[:-5]),
        'points': len(moves),
    })
print(json.dumps(out))
`;
  const runs = JSON.parse(execFileSync('python3', ['-I', '-c', script, join(import.meta.dirname, '..', 'tools', 'wikiloc')], { encoding: 'utf8' }));
  for (const r of runs) {
    assert.ok(r.moving > 0.2 && r.moving < 2, `moving ${r.moving}`);
    assert.ok(r.dwell >= 0.15 && r.dwell <= 0.4, `dwell ${r.dwell}`);
    assert.deepEqual(r.end, [220, 338]);
    assert.ok(r.press >= 0.06 && r.press <= 0.12);
    assert.deepEqual(r.kinds, ['press', 'release']);
    assert.ok(r.points >= 30);
  }
  // Curved (not a straight line), and past the box before settling on it, in most clicks.
  assert.ok(runs.filter(r => r.farthest > 10).length > 30);
  assert.ok(runs.filter(r => r.past).length > 30);
});

test('the browser process needs no captcha package', () => {
  const source = readFileSync(join(import.meta.dirname, '..', 'tools', 'wikiloc', 'browser.py'), 'utf8');
  assert.doesNotMatch(source, /playwright_captcha/);
  assert.doesNotMatch(readFileSync(join(import.meta.dirname, '..', 'tools', 'wikiloc', 'requirements.txt'), 'utf8'), /captcha/);
});
