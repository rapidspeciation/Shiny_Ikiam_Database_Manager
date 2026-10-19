import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { beforeYouUpdate, majorJump, newer, openSessions, openTerminals, runLog, t3Admin, t3Checks, t3Pairing } from '../server/t3admin.mjs';

test('only a higher stable T3 version is an update', () => {
  assert.equal(newer('0.0.43', '0.0.42'), true);
  assert.equal(newer('0.1.0', '0.0.99'), true);
  assert.equal(newer('0.0.42', '0.0.42'), false);
  assert.equal(newer('0.0.41', '0.0.42'), false);
  assert.equal(newer('0.0.43-nightly.20260928', '0.0.42'), false);
  assert.equal(newer(null, '0.0.42'), false);
});

/** GitHub's release list as the app reads it (newest first), with a nightly and notes. */
const releases = [
  { tag_name: 'v0.1.1-nightly.20261018', prerelease: true, body: '' },
  { tag_name: 'v0.1.0', name: 'T3 Code v0.1.0', html_url: 'https://github.com/pingdotgg/t3code/releases/tag/v0.1.0', published_at: '2026-10-15T23:18:08Z',
    body: '## T3 Code Orchestrator V2\n\nIntro\n\n### Before you update\n\n- **Your threads come with you, but it is a one-way copy.**\n- Update every app.\n\n### Changed or removed\n- Gone.' },
  { tag_name: 'v0.0.45', body: 'fixes' },
];
/** T3 as the app reaches it on its host: its own description, and GitHub. */
const fakeFetch = (running = { serverVersion: '0.0.45', orchestrationProtocolVersion: 1 }) => async url =>
  url.includes('api.github.com')
    ? { ok: true, json: async () => releases }
    : url.endsWith('/.well-known/t3/environment') && running
      ? { ok: true, json: async () => running }
      : Promise.reject(new Error('refused'));

function t3HomeDir({ versions = ['0.0.44', '0.0.45'], run } = {}) {
  const home = mkdtempSync(join(tmpdir(), 't3-'));
  mkdirSync(join(home, 'runtime'));
  mkdirSync(join(home, 'userdata', 'logs'), { recursive: true });
  writeFileSync(join(home, 'runtime', 'service-state.json'), JSON.stringify({ protocol: 3, activeVersion: '0.0.45' }));
  for (const v of versions) {
    mkdirSync(join(home, 'runtime', 'versions', v), { recursive: true });
    writeFileSync(join(home, 'runtime', 'versions', v, '.install-complete'), `${v}\n`);
  }
  // An unfinished download is not a version to go back to.
  mkdirSync(join(home, 'runtime', 'versions', '0.0.43'), { recursive: true });
  if (run) writeFileSync(join(home, 'userdata', 'logs', 't3-update.json'), JSON.stringify(run));
  writeFileSync(join(home, 'userdata', 'logs', 't3-update.log'), 'line 1\nline 2\n');
  return home;
}

test('a major jump: the first number that is not 0 changes', () => {
  assert.equal(majorJump('0.0.45', '0.1.0'), true);
  assert.equal(majorJump('0.0.44', '0.0.45'), true, 'in 0.0.x every release may break');
  assert.equal(majorJump('0.1.0', '0.1.3'), false);
  assert.equal(majorJump('0.1.3', '0.2.0'), true);
  assert.equal(majorJump('1.4.0', '1.9.2'), false);
  assert.equal(majorJump('1.4.0', '2.0.0'), true);
  assert.equal(majorJump(null, '0.1.0'), false);
});

test("release notes: their «Before you update», up to the next heading", () => {
  assert.equal(beforeYouUpdate(releases[1].body), '- **Your threads come with you, but it is a one-way copy.**\n- Update every app.');
  assert.equal(beforeYouUpdate('## Fixes\n- a'), null);
  assert.equal(beforeYouUpdate('Intro\r\n\r\n### Before you update\r\n\r\n- a\r\n- b\r\n\r\n### Changed\r\n- c'), '- a\n- b', "GitHub's line ends");
});

test("the update's log: the last run's lines, a line rewritten in place as it ended", () => {
  const text = '[t1] T3 Code 0.0.44 → 0.0.45; home h\nold\n[t2] T3 Code 0.0.45 → 0.1.0; home h\n  Downloading 10%\r  Downloading 90%\r  Downloaded\nDone.\n';
  assert.equal(runLog(text), '[t2] T3 Code 0.0.45 → 0.1.0; home h\n  Downloaded\nDone.');
  assert.equal(runLog('no header\n'), 'no header');
});

test('T3 status: the version running (T3 says it), the newest stable release with its notes, the jump, versions to go back to, the last run', async () => {
  const home = t3HomeDir({ run: { pid: 999999999, step: 'done', from: '0.0.44', to: '0.0.45', startedAt: '2026-10-02T18:33:00Z', steps: [] } });
  const status = await t3Admin({ home, local: 'http://t3.test', fetchImpl: fakeFetch() }).status();
  assert.equal(status.current, '0.0.45');
  assert.equal(status.protocol, 1);
  assert.equal(status.answering, true);
  assert.equal(status.latest, '0.1.0');
  assert.equal(status.updateAvailable, true);
  assert.equal(status.release.url, 'https://github.com/pingdotgg/t3code/releases/tag/v0.1.0');
  assert.match(status.release.before, /one-way copy/);
  assert.deepEqual(status.jump, { major: true, orchestratorV2: true });
  assert.deepEqual(status.previous, ['0.0.44']);
  assert.equal(status.updating, false);
  assert.equal(status.run.running, false);
  assert.equal(status.log, 'line 1\nline 2', 'a log without a run header: its end');
  // T3 not answering (restarting): the version its service runs.
  const down = await t3Admin({ home, local: 'http://t3.test', fetchImpl: fakeFetch(null) }).status();
  assert.deepEqual([down.current, down.answering, down.protocol], ['0.0.45', false, null]);
  // On 0.1.0: up to date, 0.0.45 and 0.0.44 to go back to.
  const after = await t3Admin({ home: t3HomeDir({ versions: ['0.0.44', '0.0.45', '0.1.0'] }), local: 'http://t3.test', fetchImpl: fakeFetch({ serverVersion: '0.1.0', orchestrationProtocolVersion: 2 }) }).status();
  assert.deepEqual([after.updateAvailable, after.jump, after.release, after.previous], [false, null, null, ['0.0.45', '0.0.44']]);
});

test('the update starts the unit for the version the admin saw; one T3 has not got is refused, and so is a second one at once', async () => {
  const home = t3HomeDir();
  const dir = mkdtempSync(join(tmpdir(), 't3-systemctl-'));
  const calls = join(dir, 'calls');
  const systemctl = join(dir, 'systemctl');
  writeFileSync(systemctl, `#!/bin/sh\necho "$@" >> '${calls}'\n`, { mode: 0o755 });
  const admin = t3Admin({ home, local: 'http://t3.test', systemctl, fetchImpl: fakeFetch() });
  assert.deepEqual(await admin.update({ version: '0.1.0' }), { started: true, version: '0.1.0' });
  assert.deepEqual(await admin.update({ version: '0.0.44' }), { started: true, version: '0.0.44' }, 'going back to a kept version');
  assert.equal(readFileSync(calls, 'utf8'), '--user start --no-block ithomiini-t3-update@0.1.0.service\n--user start --no-block ithomiini-t3-update@0.0.44.service\n');
  await assert.rejects(admin.update({ version: '0.0.43' }), /no está publicada ni guardada/);
  await assert.rejects(admin.update({ version: '0.1.0; rm -rf ~' }), /Elige una versión/);
  await assert.rejects(admin.update({}), /Elige una versión/);
  await assert.rejects(t3Admin({ home, systemctl: 'false', fetchImpl: fakeFetch() }).update({ version: '0.1.0' }), /No se pudo iniciar/);
  // An update under way (its script alive: this test's own pid stands for it).
  writeFileSync(join(home, 'userdata', 'logs', 't3-update.json'), JSON.stringify({ pid: process.pid, step: 'waiting', startedAt: new Date().toISOString(), steps: [] }));
  assert.equal((await admin.status()).updating, true);
  await assert.rejects(admin.update({ version: '0.1.0' }), /Ya se está actualizando/);
});

test('the lab: its command, detached, gets the version', async () => {
  const home = t3HomeDir();
  const spawned = [];
  const spawnImpl = (bin, args, options) => (spawned.push([bin, args, options.detached]), { unref() {} });
  await t3Admin({ home, local: 'http://t3.test', command: '/lab/t3.sh --update', fetchImpl: fakeFetch(), spawnImpl }).update({ version: '0.1.0' });
  assert.equal(spawned[0][0], 'sh');
  assert.match(spawned[0][1][1], /^\/lab\/t3\.sh --update 0\.1\.0 >> '.*t3-update\.log' 2>&1$/);
  assert.equal(spawned[0][2], true);
});

test("open Claude sessions: only those under T3's own server; and its terminals", async () => {
  const cmd = text => () => Promise.resolve(text);
  const claude = 'claude\0--input-format\0stream-json\0--permission-prompt-tool\0stdio';
  const list = async () =>
    new Map([
      [10, { ppid: 1, cmd: cmd('t3\0serve') }],
      [11, { ppid: 10, cmd: cmd(claude) }],
      [12, { ppid: 11, cmd: cmd('node\0mcp') }],
      [13, { ppid: 10, cmd: cmd('/bin/bash\0') }],
      [14, { ppid: 13, cmd: cmd(claude) }],
      [20, { ppid: 1, cmd: cmd('t3\0serve\0--base-dir\0other') }],
      [21, { ppid: 20, cmd: cmd(claude) }],
    ]);
  assert.equal(await openSessions(10, list), 2);
  assert.equal(await openTerminals(10, list), 1, "a terminal: a shell T3's server started");
  assert.equal(await openTerminals(null, list), 0);
  assert.equal(await openSessions(null, list), 3, "T3's pid not known: every one on the host");
});

test('the checks after an update: what the app needs from T3, each with what it found', async () => {
  const dir = mkdtempSync(join(tmpdir(), 't3-checks-'));
  writeFileSync(join(dir, 'token'), 'admin-token\n');
  const t3 = { local: 'http://t3.test', url: 'https://t3.example.org', tokenFile: join(dir, 'token') };
  const full = ['orchestration:read', 'orchestration:operate', 'settings:write', 'providers:manage', 'filesystem:read', 'access:write'];
  let held = full;
  const fetchImpl = async (url, init = {}) => {
    if (url.endsWith('/.well-known/t3/environment')) return { ok: true, json: async () => ({ serverVersion: '0.1.0', orchestrationProtocolVersion: 2 }) };
    if (url.endsWith('/api/auth/session')) return { ok: true, json: async () => ({ authenticated: true, permissions: held }) };
    if (url.endsWith('/api/auth/pairing-token')) return { ok: true, status: 200, json: async () => ({ credential: 'c', expiresAt: '2026-10-19T15:00:00Z' }) };
    if (url.endsWith('/api/orchestration/dispatch')) return { ok: false, status: 404 };
    if (url.endsWith('/api/auth/websocket-ticket')) return { ok: true, json: async () => ({ ticket: 't' }) };
    if (url === 'http://t3.test/') return { ok: true, text: async () => '<!doctype html><html><head><script type="module" src="/assets/main.js"></script></head></html>' };
    throw new Error(`unexpected ${url} ${init.method}`);
  };
  class WebSocketImpl {
    constructor() {
      this.readyState = 0;
      this.listeners = {};
      setTimeout(() => ((this.readyState = 1), (this.listeners.open ?? []).forEach(f => f())), 0);
    }
    addEventListener(name, f) {
      (this.listeners[name] ??= []).push(f);
    }
    close() {}
  }
  const chats = {
    available: true,
    file: '/home/t3/userdata/statev2.sqlite',
    projectOf: () => ({ id: 'p-lab', root: '/w/lab' }),
    chatsOf: () => [{ id: 'chat-1' }, { id: 'chat-2' }],
    ownerOf: () => 'lab',
  };
  const admin = t3Admin({ home: t3HomeDir(), local: t3.local, fetchImpl });
  const checks = await t3Checks({ t3, admin, chats, user: { username: 'lab' }, fetchImpl, WebSocketImpl });
  assert.deepEqual(checks.map(c => [c.id, c.ok]), [['answers', true], ['token', true], ['pairing', true], ['commands', true], ['chats', true], ['page', true]]);
  assert.equal(checks[0].detail, '0.1.0 · protocol 2');
  assert.equal(checks[3].detail, 'over its socket (V2)');
  assert.equal(checks[4].detail, 'statev2.sqlite · your project, 2 chats');
  // A token from 0.0.45 on 0.1.0: links without the new permissions (T3's «Permissions have changed»).
  held = ['orchestration:read', 'orchestration:operate', 'terminal:operate', 'access:write'];
  const old = await t3Checks({ t3, admin, chats: { ...chats, ownerOf: () => 'ana' }, user: { username: 'lab' }, fetchImpl, WebSocketImpl });
  assert.deepEqual(old.filter(c => !c.ok).map(c => c.id), ['token', 'chats']);
  assert.match(old[1].detail, /older T3: sign-in links lack settings:write, providers:manage, filesystem:read/);
  assert.match(old[4].detail, /reads as ana's/);
});

test("a person's T3 sign-in link: T3's own client scopes; with an admin token older than the T3, the client scopes it has", async () => {
  const dir = mkdtempSync(join(tmpdir(), 't3-pair-'));
  writeFileSync(join(dir, 'token'), 'admin-token\n');
  const t3 = { local: 'http://t3.test', url: 'https://t3.example.org', tokenFile: join(dir, 'token') };
  const asked = [];
  // T3 0.0.46 with a token 0.0.45 issued: its default client set needs scopes the token lacks.
  const fetchImpl = async (url, init = {}) => {
    asked.push({ url, auth: init.headers.authorization, body: init.body ? JSON.parse(init.body) : null });
    if (url.endsWith('/api/auth/session'))
      return { ok: true, json: async () => ({ scopes: ['orchestration:read', 'orchestration:operate', 'terminal:operate', 'review:write', 'relay:read', 'access:read', 'access:write', 'relay:write'] }) };
    const body = JSON.parse(init.body);
    if (!body.scopes) return { ok: false, status: 403 };
    return { ok: true, status: 200, json: async () => ({ credential: 'one time', expiresAt: 'soon' }) };
  };
  const link = await t3Pairing(t3, { username: 'ana' }, { fetchImpl });
  assert.deepEqual(link, { url: 'https://t3.example.org/pair#token=one%20time', expiresAt: 'soon' });
  assert.deepEqual(asked.map(a => [a.url, a.auth]), [
    ['http://t3.test/api/auth/pairing-token', 'Bearer admin-token'],
    ['http://t3.test/api/auth/session', 'Bearer admin-token'],
    ['http://t3.test/api/auth/pairing-token', 'Bearer admin-token'],
  ]);
  assert.deepEqual(asked[0].body, { label: 'ithomiini:ana' });
  assert.deepEqual(asked[2].body.scopes, ['orchestration:read', 'orchestration:operate', 'terminal:operate', 'relay:read']);
  // A token as new as its T3 (and 0.0.45, whose default set is what was asked by name before): one request.
  asked.length = 0;
  const direct = async (url, init) => (asked.push(url), { ok: true, status: 200, json: async () => ({ credential: 'c', expiresAt: 'e' }) });
  await t3Pairing(t3, { username: 'ana' }, { fetchImpl: direct });
  assert.deepEqual(asked, ['http://t3.test/api/auth/pairing-token']);
  await assert.rejects(t3Pairing(t3, { username: 'ana' }, { fetchImpl: async () => ({ ok: false, status: 502 }) }), /T3 Code answered 502/);
});
