import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { execFileSync } from 'node:child_process';
import { existsSync, mkdtempSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { DatabaseSync } from 'node:sqlite';
import { createApp } from '../server/index.mjs';
import { initDocReview } from '../server/doc-review.mjs';

// Production starts the doc comments from zero (Franz, 11 Nov 2026): the comments live only in the database's doc_*
// tables, a database without them gets them empty, and the release carries no comment of a lab or a review.

const repoRoot = new URL('..', import.meta.url).pathname;
const TABLES = ['doc_threads', 'doc_comments', 'doc_changes', 'doc_revisions', 'doc_asks', 'doc_ask_seen'];

test('a database file that does not exist yet gets the doc tables, all empty', () => {
  const file = join(mkdtempSync(join(tmpdir(), 'doc-empty-')), 'app.sqlite');
  assert.equal(existsSync(file), false);
  const db = new DatabaseSync(file);
  initDocReview(db);
  initDocReview(db); // a restart: the same empty tables, nothing added
  for (const table of TABLES) assert.equal(db.prepare(`SELECT COUNT(*) n FROM ${table}`).get().n, 0, table);
  db.close();
});

test('a fresh app on the shipped instructions shows no threads, no changes and no history from the app', async t => {
  const dir = mkdtempSync(join(tmpdir(), 'doc-empty-app-'));
  const app = await createApp(
    { databasePath: join(dir, 'app.sqlite'), localMode: true, secureCookies: false, syncIntervalMs: 0, setupToken: 'empty-setup' },
    { seed: {}, instructions: { root: repoRoot } },
  );
  await app.ready;
  const address = await app.listen(0);
  t.after(() => app.close());
  let cookie = '';
  let csrf = '';
  const call = async (path, method = 'GET', body) => {
    const response = await fetch(`http://127.0.0.1:${address.port}/ithomiini${path}`, {
      method,
      headers: { ...(body ? { 'content-type': 'application/json' } : {}), ...(cookie ? { cookie, 'x-csrf-token': csrf } : {}) },
      body: body ? JSON.stringify({ requestId: randomUUID(), ...body }) : undefined,
    });
    const data = await response.json();
    if (response.headers.get('set-cookie')) cookie = response.headers.get('set-cookie').split(';')[0];
    if (data.csrf) csrf = data.csrf;
    return { status: response.status, data };
  };
  await call('/api/auth/setup', 'POST', { token: 'empty-setup', username: 'first_admin', password: 'test-admin-123', displayName: 'Admin' });
  const { status, data } = await call('/api/doc-review');
  assert.equal(status, 200);
  assert.deepEqual(data.threads ?? [], []);
  assert.deepEqual(data.changes ?? [], []);
  for (const table of TABLES) assert.equal(app.store.db.prepare(`SELECT COUNT(*) n FROM ${table}`).get().n, 0, table);
});

// The WhatsApp bridge's example cases (made-up messages, no doc comments) are the one JSONL the repo keeps.
const KEPT = ['tools/whatsapp/examples/cases.jsonl'];

test('the release carries no database or comment export: no tracked .sqlite, .db or .jsonl outside test fixtures', () => {
  const tracked = execFileSync('git', ['ls-files'], { cwd: repoRoot, encoding: 'utf8' }).split('\n').filter(Boolean);
  const data = tracked.filter(f => /\.(sqlite|sqlite3|db|jsonl)$/i.test(f) && !f.startsWith('tests/fixtures/') && !KEPT.includes(f));
  assert.deepEqual(data, []);
});
