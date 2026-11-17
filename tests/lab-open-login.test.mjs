// The lab's open login (ITHOMIINI_LAB_OPEN_LOGIN=1, server/index.mjs labOpenLoginAllowed): in a lab copy any password
// logs in (a blank or unknown username: the lab's admin); production's settings refuse to start with it; without it a
// wrong password still fails.
import test from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { randomBytes, randomUUID } from 'node:crypto';
import { configFromEnv, createApp } from '../server/index.mjs';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { setPasswordHashCost } from '../server/auth.mjs';
import { REAL_ID } from '../server/workbook.mjs';

setPasswordHashCost(16);

// deploy/service.env.example, as the live server's service.env (comments and blanks dropped).
const PRODUCTION = Object.fromEntries(
  readFileSync(new URL('../deploy/service.env.example', import.meta.url), 'utf8')
    .split('\n')
    .filter(line => /^[A-Z_]+=/.test(line))
    .map(line => [line.slice(0, line.indexOf('=')), line.slice(line.indexOf('=') + 1)]),
);
// tools/lab/dev-app.sh's settings, offline.
const LAB = { LOCAL_MODE: '1', APP_HOST: '127.0.0.1', APP_PORT: '8885', SECURE_COOKIES: '0', ITHOMIINI_LAB_URL: 'http://127.0.0.1:8885' };
const COPY = 'x'.repeat(44);

test('the flag in a lab config: on (offline, or a Google copy that is not the team workbook); off when unset or 0', () => {
  assert.equal(configFromEnv({ ...LAB, ITHOMIINI_LAB_OPEN_LOGIN: '1' }).labOpenLogin, true);
  assert.equal(configFromEnv({ LOCAL_MODE: '1', ITHOMIINI_LAB_OPEN_LOGIN: '1' }).labOpenLogin, true, 'APP_HOST unset: 127.0.0.1');
  // tools/lab/app.sh --workbook: no LOCAL_MODE, a copy of the workbook, the lab's address.
  assert.equal(configFromEnv({ APP_HOST: '127.0.0.1', ITHOMIINI_LAB_URL: 'http://127.0.0.1:8795', WORKBOOK_ID: COPY, ITHOMIINI_LAB_OPEN_LOGIN: '1' }).labOpenLogin, true);
  assert.equal(configFromEnv(LAB).labOpenLogin, false);
  assert.equal(configFromEnv({ ...LAB, ITHOMIINI_LAB_OPEN_LOGIN: '0' }).labOpenLogin, false);
  assert.equal(configFromEnv(PRODUCTION).labOpenLogin, false, 'production without the flag starts as before');
});

test('the flag where production could be: the app refuses to start', () => {
  // The live server's config is the PRODUCTION cases: it sets no NODE_ENV and binds loopback (behind Caddy), so the
  // missing lab signal is what refuses it there; the NODE_ENV and host cases are extra guards.
  const refused = (env, why) => assert.throws(() => configFromEnv({ ITHOMIINI_LAB_OPEN_LOGIN: '1', ...env }), e => e.code === 'LAB_OPEN_LOGIN_REFUSED' && why.test(e.message));
  // The live server's service.env: loopback too, but no lab signal (no LOCAL_MODE, no lab address, the team workbook).
  refused(PRODUCTION, /no lab signal/);
  refused({ ...PRODUCTION, WORKBOOK_ID: REAL_ID }, /no lab signal/);
  // A lab address with the team's workbook (Google, not offline) is no lab.
  refused({ APP_HOST: '127.0.0.1', ITHOMIINI_LAB_URL: 'http://127.0.0.1:8795', WORKBOOK_ID: REAL_ID }, /no lab signal/);
  refused({ ...PRODUCTION, ITHOMIINI_LAB_URL: 'http://127.0.0.1:8794' }, /no lab signal/);
  refused({ ...LAB, NODE_ENV: 'production' }, /NODE_ENV=production/);
  for (const host of ['0.0.0.0', '100.64.1.2', '192.168.1.5', '::', 'franz-gpdwin4.tailf5d2f1.ts.net']) refused({ ...LAB, APP_HOST: host }, /not a loopback address/);
  refused({ ...LAB, ITHOMIINI_LAB_OPEN_LOGIN: 'yes' }, /its value/);
  refused({ ...PRODUCTION, ITHOMIINI_LAB_OPEN_LOGIN: 'true' }, /its value.*no lab signal/);
});

async function app(config) {
  const sheets = new LocalSheets({});
  const store = new Store({ localMode: true }, { sheets });
  const a = await createApp({ localMode: true, secureCookies: false, setupToken: 'test-setup-secret', syncIntervalMs: 0, ...config }, { store, skipInitialSync: true });
  const address = await a.listen(0, '127.0.0.1');
  const api = `http://127.0.0.1:${address.port}/ithomiini/api`;
  const post = async (path, body, cookie) => {
    const r = await fetch(api + path, { method: 'POST', headers: { 'content-type': 'application/json', ...(cookie ? { cookie } : {}) }, body: JSON.stringify({ requestId: randomUUID(), ...body }) });
    return { status: r.status, body: await r.json(), cookie: r.headers.get('set-cookie')?.split(';')[0] };
  };
  const session = async cookie => (await fetch(api + '/auth/session', { headers: { cookie } })).json();
  try {
    await people(post, api);
  } catch (e) {
    await a.close();
    throw e;
  }
  return { a, post, session };
}
async function people(post, api) {
  const setup = await post('/auth/setup', { token: 'test-setup-secret', username: 'lab', password: 'secret1', displayName: 'Lab' });
  assert.equal(setup.status, 201, JSON.stringify(setup.body));
  // A second account, made by the admin.
  const csrf = setup.body.csrf;
  const made = await fetch(api + '/admin/users', {
    method: 'POST',
    headers: { 'content-type': 'application/json', cookie: setup.cookie, 'x-csrf-token': csrf },
    body: JSON.stringify({ requestId: randomUUID(), username: 'ana', password: 'secret2', displayName: 'Ana', role: 'observer' }),
  });
  assert.equal(made.status, 201, await made.text());
}

test('lab open login: a random password logs in; a blank or unknown username is the admin; a named account is itself', async () => {
  const { a, post, session } = await app({ labOpenLogin: true, labUrl: 'http://127.0.0.1:8885' });
  try {
    const random = () => randomBytes(9).toString('base64url');
    const lab = await post('/auth/login', { username: 'lab', password: random() });
    assert.equal(lab.status, 200, JSON.stringify(lab.body));
    assert.equal((await session(lab.cookie)).user.username, 'lab');
    for (const username of ['', '   ', 'nobody-here', undefined]) {
      const r = await post('/auth/login', { username, password: random() });
      assert.equal(r.status, 200, `${username}: ${JSON.stringify(r.body)}`);
      assert.equal((await session(r.cookie)).user.username, 'lab', `${JSON.stringify(username)} → the admin`);
    }
    const blank = await post('/auth/login', { username: '', password: '' });
    assert.equal(blank.status, 200, 'no password at all');
    const ana = await post('/auth/login', { username: 'Ana', password: random() });
    assert.equal(ana.status, 200);
    assert.equal((await session(ana.cookie)).user.username, 'ana');
    // A deactivated account's name: the lab admin, never the disabled account.
    a.store.db.prepare("UPDATE users SET active=0 WHERE username='ana'").run();
    const gone = await post('/auth/login', { username: 'ana', password: random() });
    assert.equal(gone.status, 200);
    assert.equal((await session(gone.cookie)).user.username, 'lab');
    // listen() given a public address: refused, never bound.
    await assert.rejects(a.listen(0, '0.0.0.0'), /not a loopback address/);
  } finally {
    await a.close();
  }
});

test('without the flag a wrong password still fails; createApp refuses the flag on a production-like config', async () => {
  const { a, post } = await app({});
  try {
    assert.equal((await post('/auth/login', { username: 'lab', password: 'wrong-pass' })).status, 401);
    assert.equal((await post('/auth/login', { username: '', password: 'wrong-pass' })).status, 401);
    assert.equal((await post('/auth/login', { username: 'lab', password: 'secret1' })).status, 200);
  } finally {
    await a.close();
  }
  // createApp checks the config as given, too (not only configFromEnv's).
  await assert.rejects(createApp({ labOpenLogin: true, localMode: false, host: '127.0.0.1' }, { store: new Store({ localMode: true }, { sheets: new LocalSheets({}) }) }), /no lab signal/);
  await assert.rejects(createApp({ labOpenLogin: true, localMode: true, host: '0.0.0.0' }, { store: new Store({ localMode: true }, { sheets: new LocalSheets({}) }) }), /not a loopback/);
});
