// While the team's workbook recalculates, Google answers slowly, with 503 or not at all
// (5 Oct 2026). The app then tells everyone (the workbook's state), keeps saves in its
// database and writes them, in order and together, when Google answers again; a cell
// changed in the sheet meanwhile is refused and reported, never overwritten; nothing is
// lost across a restart.
import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { mkdtempSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { applyBatch } from '../server/batch.mjs';
import { createApp } from '../server/index.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { Store } from '../server/store.mjs';
import { WorkbookHealth, busyError } from '../server/workbook-health.mjs';

const ana = { id: 'ana', username: 'ana', displayName: 'Ana', role: 'editor' };
const luis = { id: 'luis', username: 'luis', displayName: 'Luis', role: 'editor' };
const seed = () => ({
  Collection_data: [
    { row: 2, values: { CAM_ID: 'CAM000001', Sex: 'male' } },
    { row: 3, values: { CAM_ID: 'CAM000002', Sex: 'male' } },
    { row: 4, values: { CAM_ID: 'CAM000003', Sex: 'male' } },
  ],
});
async function fixture(config = {}) {
  const sheets = new LocalSheets(seed(), { health: { probeMs: 20 } });
  const store = new Store({ localMode: true, ...config }, { sheets });
  await store.sync({ sheets: ['Collection_data'] });
  for (const u of [ana, luis])
    store.db
      .prepare("INSERT OR IGNORE INTO users(id,username,display_name,role,salt,password_hash,created_at) VALUES(?,?,?,?,'s','h',?)")
      .run(u.id, u.username, u.displayName, u.role, new Date().toISOString());
  return { store, sheets };
}
const idAt = (store, row) => store.db.prepare("SELECT id FROM records WHERE sheet='Collection_data' AND row_num=?").get(row).id;
const sexIn = (sheets, row) => sheets.rows.get('Collection_data').find(r => r.row === row).cells.find(c => c?.userEnteredValue?.stringValue && /male/.test(c.userEnteredValue.stringValue))?.userEnteredValue.stringValue;
const save = (store, user, edits, extra = {}) => applyBatch(store, { requestId: randomUUID(), partial: true, edits, ...extra }, user);
const until = async (check, ms = 3000) => {
  const end = Date.now() + ms;
  while (!check()) {
    if (Date.now() > end) throw new Error('timed out');
    await new Promise(resolve => setTimeout(resolve, 10));
  }
};

test('workbook state: busy on a 503 or no answer, slow on a slow answer, ok once it answers fast; probed meanwhile', async () => {
  let probes = 0;
  const health = new WorkbookHealth({ slowMs: 100, probeMs: 10, probe: async () => (probes++, health.record({ ms: probes < 3 ? 500 : 5 })) });
  const seen = [];
  health.onChange((state, before) => seen.push(`${before}→${state}`));
  health.record({ ms: 5 });
  assert.equal(health.state, 'ok');
  health.record({ ms: 2, status: 429 });
  assert.equal(health.state, 'ok', 'a rate limit says nothing about the recalculation');
  health.record({ ms: 40, status: 503 });
  assert.equal(health.state, 'busy');
  assert.match(health.snapshot().lastError, /503/);
  await until(() => health.state === 'ok');
  assert.deepEqual(seen, ['ok→busy', 'busy→slow', 'slow→ok']);
  assert.equal(probes, 3, 'asked until it answered fast, then no more');
  health.record({ ms: 30, timeout: true });
  assert.equal(health.state, 'busy');
  health.record({ ms: 500, background: true });
  assert.equal(health.state, 'busy', "a whole sheet's slow read does not say it answers");
  health.stop();
  assert.equal(busyError({ status: 503 }), true);
  assert.equal(busyError({ status: 400 }), false);
});

test('a save while the workbook is busy is kept, shown as waiting, and written when Google answers', async () => {
  const { store, sheets } = await fixture();
  try {
    sheets.simulateBusy({ minutes: 1, mode: 'unavailable', delayMs: 0 });
    await sheets.probe().catch(() => {});
    assert.equal(store.googleState().workbook.state, 'busy');
    const out = await save(store, ana, [{ id: idAt(store, 2), values: { Sex: 'female' }, expected: { Sex: 'male' } }]);
    assert.equal(out.status, 'queued');
    assert.equal(out.outbox.position, 1);
    assert.equal(store.googleState().outbox.waiting, 1);
    assert.equal(sexIn(sheets, 2), 'male', 'nothing written yet');
    // A retry of the same request: the same item, not a second one.
    const again = await applyBatch(store, { requestId: store.outbox.get(out.outboxId).request_id, edits: [] }, ana);
    assert.equal(again.outboxId, out.outboxId);

    sheets.simulateBusy({ minutes: 0 });
    await until(() => store.outbox.get(out.outboxId).status === 'done');
    assert.equal(sexIn(sheets, 2), 'female');
    assert.equal(store.googleState().workbook.state, 'ok');
    const answer = store.outbox.answer(store.outbox.get(out.outboxId), ana);
    assert.equal(answer.status, 'verified');
    assert.equal(answer.records[0].values.Sex, 'female');
    assert.ok(answer.action.id);
  } finally {
    store.close();
  }
});

test('saves that waited are written in order, one batch per person and tab', async () => {
  const { store, sheets } = await fixture();
  try {
    sheets.simulateBusy({ minutes: 1, mode: 'unavailable', delayMs: 0 });
    await sheets.probe().catch(() => {});
    const a1 = await save(store, ana, [{ id: idAt(store, 2), values: { Sex: 'female' } }], { purpose: 'tablas' });
    const a2 = await save(store, ana, [{ id: idAt(store, 3), values: { Sex: 'female' } }], { purpose: 'tablas' });
    const l1 = await save(store, luis, [{ id: idAt(store, 4), values: { Sex: 'female' } }], { purpose: 'tablas' });
    // Ana again, on the row Luis wrote: after his save.
    const a3 = await save(store, ana, [{ id: idAt(store, 4), values: { Sex: 'male' } }], { purpose: 'tablas' });
    assert.deepEqual([a1, a2, l1, a3].map(o => o.status), ['queued', 'queued', 'queued', 'queued']);
    const writes = [];
    const write = sheets.writeBatch.bind(sheets);
    sheets.writeBatch = async w => (writes.push(w.map(x => x.row)), write(w));
    sheets.simulateBusy({ minutes: 0 });
    await store.sheets.health.runProbe();
    await until(() => [a1, a2, l1, a3].every(o => store.outbox.get(o.outboxId).status === 'done'));
    assert.deepEqual(writes, [[2, 3], [4], [4]], "Ana's first two together, then Luis's, then Ana's");
    const actions = [a1, a2, l1, a3].map(o => store.outbox.get(o.outboxId).action_id);
    assert.equal(actions[0], actions[1]);
    assert.notEqual(actions[1], actions[2]);
    assert.equal(store.db.prepare('SELECT actor FROM actions WHERE id=?').get(actions[2]).actor, 'luis');
    assert.equal(sexIn(sheets, 4), 'male', 'as Ana wrote last');
  } finally {
    store.close();
  }
});

test('a cell changed in the sheet while the save waited is refused and reported, not overwritten', async () => {
  const { store, sheets } = await fixture();
  try {
    sheets.simulateBusy({ minutes: 1, mode: 'unavailable', delayMs: 0 });
    await sheets.probe().catch(() => {});
    const out = await save(store, ana, [{ id: idAt(store, 2), values: { Sex: 'female' }, expected: { Sex: 'male' } }]);
    await sheets.externalEdit('Collection_data', 2, { Sex: 'NA' });
    sheets.simulateBusy({ minutes: 0 });
    await store.sheets.health.runProbe();
    await until(() => ['conflict', 'done'].includes(store.outbox.get(out.outboxId).status));
    const item = store.outbox.get(out.outboxId);
    assert.equal(item.status, 'conflict');
    assert.equal(JSON.parse(item.error_json).details.items[0].code, 'EXTERNAL_CONFLICT');
    assert.equal(sheets.rows.get('Collection_data').find(r => r.row === 2).cells.some(c => c?.userEnteredValue?.stringValue === 'female'), false);
    // The page that saved asks: the refusal, as the save would have answered.
    assert.throws(() => store.outbox.answer(item, ana), e => e.code === 'BATCH_CONFLICT');
  } finally {
    store.close();
  }
});

test('a save that finds the workbook busy while reading its rows waits instead of failing', async () => {
  const { store, sheets } = await fixture();
  try {
    const read = sheets.readRows.bind(sheets);
    sheets.readRows = async (...args) => {
      sheets.readRows = read;
      sheets.health.record({ ms: 30_000, timeout: true });
      throw Object.assign(new Error('Google Sheets did not answer in 30 s'), { status: 504, timeout: true });
    };
    sheets.health.probeMs = 60_000;
    const out = await save(store, ana, [{ id: idAt(store, 3), values: { Sex: 'female' } }]);
    assert.equal(out.status, 'queued');
    assert.equal(store.unconfirmedCount(), 0, 'nothing was written: no save to confirm');
    await store.sheets.health.runProbe();
    await until(() => store.outbox.get(out.outboxId).status === 'done');
    assert.equal(sexIn(sheets, 3), 'female');
  } finally {
    store.close();
  }
});

test('saves kept while Google was busy survive a restart and are written by the next process', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'outbox-'));
  const databasePath = join(dir, 'app.sqlite');
  const config = { databasePath, localMode: true, secureCookies: false, syncIntervalMs: 0, sheetsCopyPath: null };
  const first = await createApp(config, { seed: seed(), skipInitialSync: true });
  await first.store.sync({ sheets: ['Collection_data'] });
  first.store.db.prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,created_at) VALUES('ana','ana','Ana','editor','s','h',?)").run(new Date().toISOString());
  first.store.sheets.simulateBusy({ minutes: 5, delayMs: 0 });
  await first.store.sheets.probe().catch(() => {});
  const out = await save(first.store, ana, [{ id: idAt(first.store, 2), values: { Sex: 'female' } }]);
  assert.equal(out.status, 'queued');
  // A deploy: the app stops while the save waits.
  assert.equal(await first.drain(1000), true, 'a waiting save does not hold the restart');
  await first.close();

  const second = await createApp(config, { seed: seed() });
  try {
    await second.ready;
    await until(() => second.store.outbox.get(out.outboxId).status === 'done');
    assert.equal(sexIn(second.store.sheets, 2), 'female');
  } finally {
    await second.close();
  }
});

test('/health and the pulse say how Google answers and how many saves wait', async () => {
  const app = await createApp({ databasePath: ':memory:', localMode: true, secureCookies: false, syncIntervalMs: 0 }, { seed: seed() });
  await app.ready;
  try {
    app.store.sheets.simulateBusy({ minutes: 1, delayMs: 0 });
    await app.store.sheets.probe().catch(() => {});
    await new Promise(resolve => app.server.listen(0, '127.0.0.1', resolve));
    const { port } = app.server.address();
    const health = await (await fetch(`http://127.0.0.1:${port}/health`)).json();
    assert.equal(health.google.workbook.state, 'busy');
    assert.equal(health.google.outbox.waiting, 0);
    assert.equal(health.google.simulated.mode, 'unavailable');
  } finally {
    await app.close();
  }
});
