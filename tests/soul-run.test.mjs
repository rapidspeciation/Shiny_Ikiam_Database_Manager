import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { chatLimit, runPool, runScenario, withArchive, MAX_CHATS } from '../tools/whatsapp/soul-run.mjs';

const sleep = ms => new Promise(r => setTimeout(r, ms));

test('--parallel: 4 by default, never more than 4', () => {
  assert.equal(MAX_CHATS, 4);
  assert.equal(chatLimit(undefined), 4);
  assert.equal(chatLimit('2'), 2);
  assert.equal(chatLimit('12'), 4);
  assert.equal(chatLimit('0'), 1);
  assert.equal(chatLimit('abc'), 4);
});

test('the pool runs every item, at most 4 at once, and keeps the order', async () => {
  let now = 0;
  let most = 0;
  const out = await runPool([...Array(11).keys()], 9, async i => {
    most = Math.max(most, ++now);
    await sleep(5 + (i % 3) * 5);
    now--;
    return i * 2;
  });
  assert.equal(most, 4);
  assert.deepEqual(out, [...Array(11).keys()].map(i => i * 2));
});

test('the chat is archived after an answer, a failure or a timeout', async () => {
  const archived = [];
  const archive = async id => void archived.push(id);
  assert.equal(await withArchive(async () => 'ok', () => 'T1', archive), 'ok');
  await assert.rejects(withArchive(async () => { throw new Error('turn failed'); }, () => 'T2', archive), /turn failed/);
  await withArchive(async () => 'no chat made', () => null, archive);
  await withArchive(async () => 'x', () => 'T3', async () => { throw new Error('T3 down'); }, { error() {} }); // an archive error does not hide the result
  assert.deepEqual(archived, ['T1', 'T2']);
});

test('a scenario whose turn never ends is reported as a timeout and its chat is archived', async () => {
  const archived = [];
  const fakeBridge = () => ({ handle() {}, idle: () => false, state: () => ({ threads: { group: 'T-stuck' } }) });
  const r = await runScenario(
    { id: 'S99', chat: 'group', sender: 'técnico B', message: '@claude hola', preceding: [] },
    { outDir: mkdtempSync(join(tmpdir(), 'soul-run-')), soulFile: null, round: 'rt', tono: 'normal', lab: {}, createBridge: fakeBridge, archive: async (_route, id) => void archived.push(id), waitMs: 50 },
  );
  assert.equal(r.ok, false);
  assert.equal(r.reply, '[timeout]');
  assert.deepEqual(archived, ['T-stuck']);
});
