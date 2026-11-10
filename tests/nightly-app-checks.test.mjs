// scripts/nightly-app-checks.mjs: the nightly run's look at the live app's /health (here a local stand-in), its line and
// exit code (0 ok, 3 warning, 1 failing or no answer), which nightly-tests.sh keeps as its `wikiloc` result.
import test from 'node:test';
import assert from 'node:assert/strict';
import http from 'node:http';
import { execFile } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { wikilocLine } from '../scripts/nightly-app-checks.mjs';

const script = fileURLToPath(new URL('../scripts/nightly-app-checks.mjs', import.meta.url));
// /health's wikiloc as it would read for the live facts of 10 Nov 2026 (three failed on 16 Oct, the last that worked on 1 Oct).
const live = {
  status: 'failing',
  reason: 'failed',
  lastOk: '2026-10-01T00:00:11.999Z',
  failedSinceOk: 3,
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
