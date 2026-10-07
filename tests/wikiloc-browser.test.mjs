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
