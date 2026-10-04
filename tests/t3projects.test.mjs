import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { chatStarter, createT3Projects, projectKey, provisioner } from '../server/t3projects.mjs';

// Each person's own T3 project: made the first time they open the Asistente tab, then the frame opens on it.

const ENV = '4e6c4765-8cfa-4adc-b761-3c3bae2ae7e0';

/** T3's projects as server/t3chats.mjs reads them; `add` is what provisioning does. */
function fakeChats(start = {}, threads = {}) {
  const projects = new Map(Object.entries(start));
  return {
    available: true,
    projectOf: username => projects.get(username) ?? null,
    chatsOf: username => (threads[username] ?? []).map(id => ({ id })),
    add: username => projects.set(username, { id: `p-${username}`, root: `/w/${username}` }),
  };
}

test("T3's key for a project: its environment and folder", () => {
  assert.equal(projectKey(ENV, '/w/ana/'), `${ENV}:/w/ana`);
  assert.equal(projectKey(null, '/w/ana'), null);
  assert.equal(projectKey(ENV, null), null);
  assert.equal(provisioner('off'), null);
  assert.equal(typeof provisioner('systemd'), 'function');
});

test('a person without a project gets one, made once even when asked twice at the same time', async () => {
  const chats = fakeChats({ franz: { id: 'p-franz', root: '/w/franz' } });
  const made = [];
  const projects = createT3Projects({
    chats,
    wait: 1,
    provision: async username => {
      made.push(username);
      await new Promise(resolve => setTimeout(resolve, 5));
      chats.add(username);
    },
  });
  assert.deepEqual((await projects.ensure({ username: 'franz' })).project, { id: 'p-franz', root: '/w/franz' });
  const [a, b] = await Promise.all([projects.ensure({ username: 'angel' }), projects.ensure({ username: 'angel' })]);
  assert.deepEqual(a.project, { id: 'p-angel', root: '/w/angel' });
  assert.deepEqual(b, a);
  assert.deepEqual(made, ['angel'], 'franz has one already; angel is made once');
  // A name a unit or folder cannot carry as it is: never provisioned.
  assert.deepEqual(await projects.ensure({ username: 'a b/../c' }), { project: null, chat: null });
  assert.deepEqual(made, ['angel']);
});

test('a failed try is not repeated on every opening of the tab; without T3, nothing', async () => {
  let clock = 0;
  const logged = [];
  let tries = 0;
  const projects = createT3Projects({
    chats: fakeChats(),
    wait: 1,
    now: () => clock,
    log: (...args) => logged.push(args.join(' ')),
    provision: async () => {
      tries++;
      throw new Error('t3 project add failed');
    },
  });
  assert.equal((await projects.ensure({ username: 'ana' })).project, null);
  assert.equal((await projects.ensure({ username: 'ana' })).project, null);
  assert.equal(tries, 1);
  assert.match(logged[0], /T3 project for ana: t3 project add failed/);
  clock += 11 * 60_000;
  await projects.ensure({ username: 'ana' });
  assert.equal(tries, 2, 'tried again later');
  const off = createT3Projects({ chats: { ...fakeChats(), available: false }, provision: async () => assert.fail('no T3') });
  assert.deepEqual(await off.ensure({ username: 'ana' }), { project: null, chat: null });
});

test("the frame opens on the person's latest chat, or on an empty one made once in their project", async () => {
  const chats = fakeChats({ franz: { id: 'p-franz', root: '/w/franz' }, ana: { id: 'p-ana', root: '/w/ana' } }, { franz: ['t-2', 't-1'] });
  const started = [];
  const projects = createT3Projects({
    chats,
    startChat: async project => {
      started.push(project.id);
      await new Promise(resolve => setTimeout(resolve, 5));
      return 't-new';
    },
  });
  assert.equal((await projects.ensure({ username: 'franz' })).chat, 't-2');
  const [a, b] = await Promise.all([projects.ensure({ username: 'ana' }), projects.ensure({ username: 'ana' })]);
  assert.deepEqual([a.chat, b.chat, started], ['t-new', 't-new', ['p-ana']]);
});

test("an empty chat is T3's own thread.create, with the app's T3 token", async () => {
  const dir = mkdtempSync(join(tmpdir(), 't3projects-'));
  writeFileSync(join(dir, 'token'), 'secret\n');
  const sent = [];
  const start = chatStarter({
    local: 'http://127.0.0.1:9',
    tokenFile: join(dir, 'token'),
    fetchImpl: async (url, init) => (sent.push({ url, init }), { ok: true }),
  });
  const id = await start({ id: 'p-ana' });
  assert.equal(sent[0].url, 'http://127.0.0.1:9/api/orchestration/dispatch');
  assert.equal(sent[0].init.headers.authorization, 'Bearer secret');
  const command = JSON.parse(sent[0].init.body);
  assert.deepEqual([command.type, command.threadId, command.projectId, command.title], ['thread.create', id, 'p-ana', 'New thread']);
  // Never fast mode; the person may change the model before writing.
  assert.deepEqual(command.modelSelection.options.find(o => o.id === 'fastMode'), { id: 'fastMode', value: false });
  assert.equal(chatStarter({ local: 'http://127.0.0.1:9' }), null);
  const refused = chatStarter({ local: 'x', tokenFile: join(dir, 'token'), fetchImpl: async () => ({ ok: false, status: 403 }) });
  await assert.rejects(refused({ id: 'p' }), /T3 answered 403/);
});
