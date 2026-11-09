#!/usr/bin/env node
// Opt-in verification of the deployed app. Never logs credentials.
// LIVE_WRITE_TEST=1 writes (and undoes) a note in a Collection_data row: only against a test workbook.
// /health and direct API calls only: it never sends a chat or runs an AI model (Franz's rule, 9 Nov 2026).
import { readFileSync, writeFileSync } from 'node:fs';
import { randomUUID } from 'node:crypto';
import assert from 'node:assert/strict';
import { REAL_ID } from '../server/workbook.mjs';

const base = process.env.LIVE_SMOKE_URL?.replace(/\/$/, '');
if (!base || !process.env.TEST_ACCOUNT_FILE) throw new Error('LIVE_SMOKE_URL and TEST_ACCOUNT_FILE are required');
const account = JSON.parse(readFileSync(process.env.TEST_ACCOUNT_FILE));
let cookie = '',
  csrf = '';
async function call(path, method = 'GET', body) {
  const response = await fetch(base + path, {
    method,
    headers: {
      ...(cookie ? { cookie, 'x-csrf-token': csrf } : {}),
      ...(body ? { 'content-type': 'application/json' } : {}),
    },
    body: body ? JSON.stringify({ requestId: randomUUID(), ...body }) : undefined,
  });
  const data = await response.json();
  if (!response.ok)
    throw new Error(`${method} ${path}: ${response.status} ${data.error?.code || ''} ${data.error?.message || ''}`);
  if (response.headers.get('set-cookie')) cookie = response.headers.get('set-cookie').split(';')[0];
  if (data.csrf) csrf = data.csrf;
  return data;
}
const health = await (await fetch(`${base}/health`)).json();
assert.equal(health.status, 'ok');
console.log(`Health ok: sync ${health.sync}, workbook ${health.google?.workbook?.state ?? '?'}`);
const initial = await call('/api/auth/session');
await call(initial.setupRequired ? '/api/auth/setup' : '/api/auth/login', 'POST', account);
const boot = await call('/api/bootstrap');
assert.match(boot.settings.sheetUrl, /^https:\/\/docs\.google\.com\/spreadsheets\/d\//);
console.log(
  `Authenticated; ${boot.modules.length} modules; ${boot.stats.totalRecords} indexed records; sync=${boot.sync.state}`,
);
const stats = { modules: boot.modules.length, records: boot.stats.totalRecords, sync: boot.sync.state };
if (process.env.LIVE_WRITE_TEST === '1' && boot.settings.sheetUrl.includes(REAL_ID))
  throw new Error("LIVE_WRITE_TEST writes into a row: the app now uses the team's real workbook");
if (process.env.LIVE_WRITE_TEST === '1') {
  const { records } = await call('/api/records?module=Collection_data&observedOnly=true&limit=10');
  const record = records.find(r => !r.formulas.Notes_Collection_data && r.values.SPECIES);
  assert.ok(record, 'An observed collection row must exist');
  const before = record.values.Notes_Collection_data;
  let action;
  try {
    const edit = await call(`/api/records/${record.id}`, 'PATCH', {
      values: { Notes_Collection_data: `ITHOMIINI DEPLOYMENT CHECK ${randomUUID()}` },
      expectedVersion: record.version,
      reason: 'Deployment verification. Automatically restored immediately.',
    });
    assert.equal(edit.status, 'verified');
    action = edit.action;
    const preview = await call('/api/history/preview', 'POST', { actionIds: [action.id] });
    assert.equal(preview.conflicts.length, 0);
    console.log('Live Sheet write and reversal preview verified');
  } finally {
    if (action) {
      await call('/api/history/undo', 'POST', {
        actionIds: [action.id],
        reason: 'Restore deployment verification value',
      });
      const restored = (await call(`/api/records/${record.id}`)).record;
      assert.deepEqual(restored.values.Notes_Collection_data, before);
      assert.deepEqual(restored.values.Sex, record.values.Sex);
      console.log('Live undo verified; original value and unrelated field preserved');
      stats.writeUndo = true;
    }
  }
}
for (const kind of ['overview', 'stages', 'crosses', 'samples', 'quality', 'weekly']) {
  const report = await call(`/api/reports?kind=${kind}`);
  assert.ok(report.method && report.columns);
  assert.equal(Boolean(report.truncated), false);
  console.log(`Report ${kind}: ${report.rows.length} result rows, complete=${!report.truncated}`);
}
if (process.env.SMOKE_RESULT_FILE)
  writeFileSync(
    process.env.SMOKE_RESULT_FILE,
    JSON.stringify({ ...stats, checkedAt: new Date().toISOString() }, null, 2),
    { mode: 0o600 },
  );
