import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { DatabaseSync } from 'node:sqlite';
import { createQueryRunner } from '../server/query-tool.mjs';

// A query process whose start is held up `ms` before its own code loads, as under load.
const slowStart = ms => ['--import', `data:text/javascript,Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, ${ms})`];

function copy() {
  const dir = mkdtempSync(join(tmpdir(), 'query-runner-'));
  const path = join(dir, 'sheets.sqlite');
  const db = new DatabaseSync(path);
  db.exec("CREATE TABLE _meta (key TEXT, value TEXT); INSERT INTO _meta VALUES ('built_at', '2026-11-07T00:00:00Z')");
  db.close();
  return { dir, path };
}

test('the query limit counts from when the process is ready, not from its start', async () => {
  const { dir, path } = copy();
  const runner = createQueryRunner({ path, timeoutMs: 600, execArgv: slowStart(1500) });
  try {
    const result = await runner.run('SELECT 1 AS x');
    assert.equal(result.error, undefined, `a quick query after a slow start: ${result.error}`);
    assert.deepEqual(result.rows, [[1]]);
    // A runaway is still stopped at the limit once the process is up.
    const started = Date.now();
    const runaway = await runner.run('WITH RECURSIVE c(x) AS (SELECT 1 UNION ALL SELECT x + 1 FROM c) SELECT count(*) FROM c');
    assert.match(runaway.error, /ran over 0.6 s and was stopped/);
    assert.equal(runaway.timeout, true);
    assert.ok(Date.now() - started < 10_000);
    // The next query gets a new process, slow to start again, and runs.
    assert.deepEqual((await runner.run('SELECT 2 AS x')).rows, [[2]]);
  } finally {
    runner.close();
    rmSync(dir, { recursive: true, force: true });
  }
});

test('a process that does not start in time is stopped with its own message', async () => {
  const { dir, path } = copy();
  const runner = createQueryRunner({ path, timeoutMs: 600, startTimeoutMs: 300, execArgv: slowStart(3000) });
  try {
    const result = await runner.run('SELECT 1 AS x');
    assert.match(result.error, /did not start within 0.3 s/);
    assert.equal(result.timeout, undefined);
  } finally {
    runner.close();
    rmSync(dir, { recursive: true, force: true });
  }
});
