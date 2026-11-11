// scripts/nightly-app-checks.mjs: the nightly run's look at the live app's /health (here a local stand-in), its line and
// exit code (0 ok, 3 warning, 1 failing or no answer), which nightly-tests.sh keeps as its `wikiloc` result.
import test from 'node:test';
import assert from 'node:assert/strict';
import http from 'node:http';
import { execFile } from 'node:child_process';
import { mkdtempSync, rmSync, symlinkSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { wikilocLine } from '../scripts/nightly-app-checks.mjs';

const script = fileURLToPath(new URL('../scripts/nightly-app-checks.mjs', import.meta.url));
// /health's wikiloc as it would read for the live facts of 10 Nov 2026 (three failed on 16 Oct, the last that worked on 1 Oct).
const live = {
  status: 'failing',
  reason: 'failed',
  lastOk: '2026-10-01T00:00:11.999Z',
  failedSinceOk: 3,
  profilesFailing: 0,
  lastFailed: '2026-10-16T18:09:20.890Z',
  waiting: 0,
  workerSeen: '2026-11-10T22:50:01.000Z',
  profiles: 6,
  oldestProfileCheck: '2026-09-27T13:29:08.377Z',
};

test('the line and exit code for each state', () => {
  const failing = wikilocLine(live);
  assert.equal(failing.code, 1);
  assert.match(failing.line, /FAILING: 3 failed since the last that worked \(latest 2026-10-16\); last that worked 2026-10-01/);
  assert.match(wikilocLine({ ...live, reason: 'worker', failedSinceOk: 0 }).line, /worker has not asked for work/);
  const stale = wikilocLine({ ...live, status: 'stale', reason: undefined, failedSinceOk: 0 });
  assert.equal(stale.code, 3);
  assert.match(stale.line, /WARNING: a followed profile has not been looked at in 30 days/);
  assert.equal(wikilocLine({ ...live, status: 'ok', failedSinceOk: 0 }).code, 0);
  // A followed profile's latest check failed: said, with its count (singular and plural).
  assert.match(wikilocLine({ ...live, failedSinceOk: 0, profilesFailing: 1 }).line, /0 failed since the last that worked, 1 followed profile whose latest check failed/);
  assert.match(wikilocLine({ ...live, profilesFailing: 2 }).line, /2 followed profiles whose/);
  // The daily check on: a warning once it has not run for over 2 days (Ecuador days), not before.
  const at = Date.parse('2026-11-14T03:00:00Z'); // 13 Nov, 22:00 in Ecuador
  const ok = { ...live, status: 'ok', failedSinceOk: 0, daily: 'on' };
  assert.equal(wikilocLine({ ...ok, dailyQueued: '2026-11-11' }, at).code, 0);
  const late = wikilocLine({ ...ok, dailyQueued: '2026-11-10' }, at);
  assert.equal(late.code, 3);
  assert.match(late.line, /the daily check is on but last ran 2026-11-10/);
  assert.equal(wikilocLine({ ...ok, daily: 'off', dailyQueued: '2026-11-01' }, at).code, 0);
  assert.equal(wikilocLine({ ...ok, dailyQueued: null }, at).code, 0);
  assert.equal(wikilocLine(undefined).code, 1);
  assert.equal(wikilocLine({ status: 'unknown' }).code, 1);
});

function run(url) {
  return new Promise(resolve => execFile(process.execPath, [script, url], (error, stdout) => resolve({ code: error ? error.code : 0, stdout })));
}

test('run against /health: its exit code, and 1 when /health does not answer', async t => {
  let answer = { status: 'ok', wikiloc: live };
  const server = http.createServer((req, res) => {
    res.writeHead(req.url === '/health' ? 200 : 404, { 'content-type': 'application/json' });
    res.end(JSON.stringify(answer));
  });
  await new Promise(r => server.listen(0, '127.0.0.1', r));
  t.after(() => server.close());
  const base = `http://127.0.0.1:${server.address().port}`;
  const failing = await run(`${base}/health`);
  assert.equal(failing.code, 1);
  assert.match(failing.stdout, /^Wikiloc imports: FAILING/);
  answer = { status: 'ok', wikiloc: { ...live, status: 'stale', failedSinceOk: 0 } };
  assert.equal((await run(`${base}/health`)).code, 3);
  answer = { status: 'ok', wikiloc: { ...live, status: 'ok', failedSinceOk: 0 } };
  assert.equal((await run(`${base}/health`)).code, 0);
  const gone = await run(`${base}/nothing`);
  assert.equal(gone.code, 1);
  assert.match(gone.stdout, /did not answer \(answered 404\)/);
});

test('run through a symlinked release path (the nightly runs current/scripts/…): it runs and says so; an unknown option fails', async t => {
  // /home/ubuntu/ithomiini/current → releases/<stamp>: here a link to this checkout. On 11 Nov 2026 the run did nothing
  // and passed, as Node gives import.meta.url the real path.
  const dir = mkdtempSync(join(tmpdir(), 'nightly-link-'));
  t.after(() => rmSync(dir, { recursive: true, force: true }));
  const current = join(dir, 'current');
  symlinkSync(fileURLToPath(new URL('..', import.meta.url)), current, 'dir');
  const linked = join(current, 'scripts', 'nightly-app-checks.mjs');
  const env = { PATH: '/usr/bin:/bin', HOME: dir, NODE_OPTIONS: '' };
  const run = args =>
    new Promise(resolve => execFile(process.execPath, [linked, ...args], { env }, (error, stdout) => resolve({ code: error ? error.code : 0, stdout })));
  // No credential: it reached the run (and sent nothing).
  const ranges = await run(['--sheet-ranges', '--log', join(dir, 'sheet-ranges.jsonl')]);
  assert.equal(ranges.code, 1);
  assert.match(ranges.stdout, /^Sheet ranges: FAILING: GOOGLE_CREDENTIALS_FILE is required/);
  const unknown = await run(['--sheet-range']);
  assert.equal(unknown.code, 1);
  assert.match(unknown.stdout, /^Unknown option --sheet-range/);
  // The health check too (nothing listens there).
  const health = await run(['http://127.0.0.1:9/health']);
  assert.equal(health.code, 1);
  assert.match(health.stdout, /^Wikiloc imports: http:\/\/127\.0\.0\.1:9\/health did not answer/);
});
