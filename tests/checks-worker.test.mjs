import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { allIssues, cachedIssues, freshIssues, readyIssues } from '../server/checks.mjs';
import { createChecksHost } from '../server/checks-host.mjs';
import { layoutsFeed } from '../server/worker-slot.mjs';
import { createApp } from '../server/index.mjs';

// The Revisión checks' scan in a worker thread (server/checks-host.mjs), on a database file: the
// same issues as a scan in the app's thread, kept while the copy is as scanned, and found again
// after a change by a scan that starts after the request.

const sheets = () =>
  new LocalSheets({
    Collection_data: [
      { row: 2, values: { CAM_ID: 'CAM070918', Tube_1_id: 'FD30881820', SPECIES: 'Thyridia psidii' } },
      { row: 3, values: { CAM_ID: 'CAM070919', Tube_1_id: 'FD30881999', SPECIES: 'Thyridia psidii' } },
      { row: 4, values: { CAM_ID: 'CAM070920', Tube_1_id: 'FD30882000', SPECIES: 'Thyridia psidii', Sex: 'female_?' } },
    ],
    Barcoding_DNA: [{ row: 2, values: { CAM_ID: 'CAM070111', Tube_1_id: 'FD30881999' } }],
  });

async function fixture(t, { workerUrl } = {}) {
  const dir = mkdtempSync(join(tmpdir(), 'checks-worker-'));
  const databasePath = join(dir, 'app.sqlite');
  const store = new Store({ databasePath, localMode: true }, { sheets: sheets() });
  await store.sync({ sheets: ['Collection_data', 'Barcoding_DNA'] });
  const host = createChecksHost({ store, config: { databasePath, localMode: true }, ...(workerUrl ? { workerUrl } : {}) });
  t.after(() => {
    host.close();
    store.close();
    rmSync(dir, { recursive: true, force: true });
  });
  /** A cell saved as the app saves it (the copy changes; each save a millisecond later than the last). */
  let saves = 0;
  const save = (row, field, value) => {
    const record = store.getRecordBySheetRow('Collection_data', row);
    store.db
      .prepare('UPDATE records SET values_json=?, version=version+1, updated_at=? WHERE id=?')
      .run(JSON.stringify({ ...record.values, [field]: value }), new Date(Date.now() + ++saves).toISOString(), record.id);
  };
  return { databasePath, store, host, save };
}
const repeats = entry => entry.issues.filter(i => i.kind === 'repeat').map(i => i.value);

test('the checks scan in their worker: the same issues as in the app, and «Resueltos» kept by the app', async t => {
  const { databasePath, store, host } = await fixture(t);
  assert.equal(host.mode, 'worker');
  const found = await freshIssues(store);
  assert.equal(host.status().scans, 1);
  // A scan in the app's thread of the same file (another store: nothing cached).
  const here = new Store({ databasePath, localMode: true }, {});
  t.after(() => here.close());
  const inline = allIssues(here);
  assert.equal(found.stamp, inline.stamp);
  assert.deepEqual(found.issues, inline.issues);
  assert.ok(repeats(found).includes('FD30881999'));
  // The app wrote the issues' first seen (the worker cannot write).
  const kept = store.db.prepare("SELECT count(*) n FROM findings WHERE type='check' AND solved_at IS NULL").get().n;
  assert.equal(kept, found.issues.length);
  // Kept while the copy is as scanned: no second scan.
  assert.equal(await freshIssues(store), found);
  assert.equal(readyIssues(store), found);
  assert.equal(host.status().scans, 1);
});

test('after a change the kept issues are not served: a request waits for a scan that starts after it', async t => {
  const { store, host, save } = await fixture(t);
  // Nothing scanned yet: a page that never waits gets nothing, and a scan starts.
  assert.equal(readyIssues(store), null);
  const first = await freshIssues(store);
  assert.equal(readyIssues(store), first);
  assert.ok(!repeats(first).includes('FD30881820'));

  // A change: what was kept is out of date for everyone.
  save(3, 'Tube_1_id', 'FD30881820');
  assert.equal(cachedIssues(store), null);
  assert.equal(readyIssues(store), null);
  const second = await freshIssues(store);
  assert.ok(repeats(second).includes('FD30881820'));
  assert.equal(cachedIssues(store), second);

  // Requests while a scan of an older copy runs share the next one, which sees the change they followed.
  save(4, 'Tube_1_id', 'FD30881820');
  const running = freshIssues(store);
  save(2, 'Notes', 'changed while it scans');
  const [a, b] = [freshIssues(store), freshIssues(store)];
  await running;
  const [after, same] = await Promise.all([a, b]);
  assert.equal(after, same);
  assert.equal(after.issues.filter(i => i.kind === 'repeat' && i.value === 'FD30881820').length, 3);
  assert.equal(cachedIssues(store), after);
  // Four scans: the first, after the first change, the one running, and one for both requests after it.
  assert.equal(host.status().scans, 4);
  // Solved: the issue of a fixed cell leaves the list, and «Resueltos» has it.
  save(4, 'Sex', 'female');
  const fixed = await freshIssues(store);
  assert.ok(!fixed.issues.some(i => i.kind === 'list' && i.value === 'female_?'));
  const solved = store.db.prepare("SELECT count(*) n FROM findings WHERE type='check' AND key LIKE 'list:%' AND solved_at IS NOT NULL").get().n;
  assert.equal(solved, 1);
});

test('a checks worker that fails: the scan runs in the app instead', async t => {
  const crash = new URL('./fixtures/checks-worker-crash.mjs', import.meta.url);
  const { store } = await fixture(t, { workerUrl: crash });
  const errors = t.mock.method(console, 'error', () => {});
  const found = await freshIssues(store);
  assert.ok(repeats(found).includes('FD30881999'));
  assert.ok(errors.mock.calls.some(c => /done in the app instead/.test(c.arguments.join(' '))));
});

test('the app serves Revisión from the checks worker and says so on /health', async t => {
  const dir = mkdtempSync(join(tmpdir(), 'checks-app-'));
  const app = await createApp(
    {
      databasePath: join(dir, 'app.sqlite'),
      localMode: true,
      secureCookies: false,
      syncIntervalMs: 0,
      setupToken: 'setup-token-123',
    },
    { sheets: sheets(), skipInitialSync: true },
  );
  t.after(async () => {
    await app.close();
    rmSync(dir, { recursive: true, force: true });
  });
  await app.store.sync({ sheets: ['Collection_data', 'Barcoding_DNA'] });
  const address = await app.listen(0, '127.0.0.1');
  const base = `http://127.0.0.1:${address.port}`;
  const setup = await fetch(`${base}/api/auth/setup`, {
    method: 'POST',
    headers: { 'content-type': 'application/json', origin: base },
    body: JSON.stringify({ token: 'setup-token-123', username: 'ana', displayName: 'Ana', password: 'secret12' }),
  });
  assert.equal(setup.status, 201);
  const cookie = setup.headers
    .getSetCookie()
    .map(c => c.split(';')[0])
    .join('; ');
  const checks = await (await fetch(`${base}/api/checks?kind=repeat`, { headers: { cookie } })).json();
  assert.ok(checks.issues.some(i => i.value === 'FD30881999'));
  const alerts = await (await fetch(`${base}/api/alerts`, { headers: { cookie } })).json();
  assert.ok(Array.isArray(alerts.alerts) && alerts.camPools && alerts.preserveRule);
  const suggested = await (await fetch(`${base}/api/suggested-edits`, { headers: { cookie } })).json();
  assert.ok(Array.isArray(suggested.items) && suggested.sources.length);
  const health = await (await fetch(`${base}/health`)).json();
  assert.equal(health.checks.mode, 'worker');
  assert.ok(health.checks.scans >= 1);
  assert.ok(health.checks.alerts.runs >= 1);
  assert.ok(health.checks.suggestions.runs >= 1);
});

test('the workers get each sheet’s columns at their start, and those a sync changed', () => {
  let synced = null;
  const store = { layouts: new Map([['Insectary_data', { columns: ['A', 'B'] }]]), watchSyncs: fn => ((synced = fn), () => (synced = null)) };
  const posted = [];
  const feed = layoutsFeed(store, changed => posted.push(changed));
  assert.deepEqual(feed.current(), [['Insectary_data', { columns: ['A', 'B'] }]]);
  // A sync that read the same columns: nothing to send.
  synced();
  assert.deepEqual(posted, []);
  store.layouts.set('Insectary_data', { columns: ['A', 'B', 'C'] });
  store.layouts.set('Clutches', { columns: ['X'] });
  synced();
  assert.deepEqual(posted, [
    [
      ['Insectary_data', { columns: ['A', 'B', 'C'] }],
      ['Clutches', { columns: ['X'] }],
    ],
  ]);
  feed.stop();
  assert.equal(synced, null);
});
