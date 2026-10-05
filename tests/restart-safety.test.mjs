// A restart (a deploy) never cuts a save: the app stops taking saves, waits for those in progress,
// reports them on /health for scripts/deploy.sh, and saves left unconfirmed are checked again after
// every sync. Requests Google does not answer are given up.
import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { createApp } from '../server/index.mjs';
import { applyBatch } from '../server/batch.mjs';
import { mkdtempSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { GoogleSheets } from '../server/sheets.mjs';
import { SANDBOX_ID } from '../server/workbook.mjs';

const user = { id: 'editor-1', username: 'editor', role: 'editor' };

test('stopping: saves in progress finish, new ones wait for the next process, /health says what is being written', async () => {
  const app = await createApp(
    { databasePath: ':memory:', localMode: true, secureCookies: false, syncIntervalMs: 0 },
    { seed: { Collection_data: [{ row: 2, values: { CAM_ID: 'CAM000001', Sex: 'male' } }] } },
  );
  await app.ready;
  const { store } = app;
  try {
    const sheets = store.sheets;
    const write = sheets.writeBatch.bind(sheets);
    let release;
    const held = new Promise(resolve => (release = resolve));
    sheets.writeBatch = async writes => (await held, write(writes));
    const id = store.db.prepare("SELECT id FROM records WHERE sheet='Collection_data' AND row_num=2").get().id;
    const saving = applyBatch(store, { requestId: randomUUID(), edits: [{ id, values: { Sex: 'female' } }] }, user);
    await new Promise(resolve => setTimeout(resolve, 20));
    assert.deepEqual(app.writingNow(), { applying: 0, inFlight: 1, unconfirmed: 1, draining: false });

    const drained = app.drain(5000);
    // A save made now is kept in the database for the next process (server/outbox.mjs), not written by this one.
    const kept = await applyBatch(store, { requestId: randomUUID(), edits: [{ id, values: { Sex: 'NA' } }] }, user);
    assert.equal(kept.status, 'queued');
    let done = false;
    drained.then(() => (done = true));
    await new Promise(resolve => setTimeout(resolve, 300));
    assert.equal(done, false, 'waits for the save in progress');
    release();
    assert.equal((await saving).status, 'verified');
    assert.equal(await drained, true);
    assert.deepEqual(app.writingNow(), { applying: 0, inFlight: 0, unconfirmed: 0, draining: true });
  } finally {
    await app.close();
  }
});

test('a save left unconfirmed is checked again after the next sync', async () => {
  const app = await createApp(
    { databasePath: ':memory:', localMode: true, secureCookies: false, syncIntervalMs: 0 },
    { seed: { Collection_data: [{ row: 2, values: { CAM_ID: 'CAM000001', Sex: 'male' } }] } },
  );
  await app.ready;
  const { store } = app;
  try {
    store.sheets.failNextWrite = Object.assign(new Error('socket hang up'), { status: undefined });
    const id = store.db.prepare("SELECT id FROM records WHERE sheet='Collection_data' AND row_num=2").get().id;
    await assert.rejects(
      applyBatch(store, { requestId: randomUUID(), edits: [{ id, values: { Sex: 'female' } }] }, user),
      e => e.code === 'WRITE_UNCERTAIN',
    );
    clearTimeout(store.recoveryTimer);
    store.recoveryTimer = null;
    assert.equal(store.unconfirmedCount(), 1);
    await store.sync({ force: true });
    await store.recovering;
    assert.equal(store.unconfirmedCount(), 0, 'settled (nothing reached the sheet: failed)');
  } finally {
    await app.close();
  }
});

test('a request Google does not answer is given up: a read is retried, a write left unconfirmed', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'restart-'));
  const file = join(dir, 'google.json');
  writeFileSync(file, JSON.stringify({ client_id: 'c', client_secret: 's', refresh_token: 'r' }));
  const sheets = new GoogleSheets({ spreadsheetId: SANDBOX_ID, googleCredentialsFile: file });
  sheets.token = { value: 't', expires: Date.now() + 3600_000 };
  const fetches = [];
  const original = globalThis.fetch;
  const realTimeout = AbortSignal.timeout;
  // Every request times out at once.
  AbortSignal.timeout = () => AbortSignal.abort(new DOMException('timed out', 'TimeoutError'));
  globalThis.fetch = async (url, options) => {
    fetches.push(options.method ?? 'GET');
    if (options.signal?.aborted) throw options.signal.reason;
    throw new Error('not reached');
  };
  try {
    sheets.readSlot = async () => {};
    await assert.rejects(sheets.request('?x'), e => e.status === 504 && e.timeout);
    assert.equal(fetches.length, 5, 'a read is tried five times');
    fetches.length = 0;
    await assert.rejects(sheets.request(':batchUpdate', { method: 'POST', body: '{}' }), e => e.status === 504);
    assert.deepEqual(fetches, ['POST'], 'a write once: it may have landed');
  } finally {
    globalThis.fetch = original;
    AbortSignal.timeout = realTimeout;
  }
});
