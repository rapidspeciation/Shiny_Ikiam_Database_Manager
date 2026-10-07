import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { createApp } from '../server/index.mjs';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { setPasswordHashCost } from '../server/auth.mjs';

// Hashing at production cost takes most of a second per sign-in here; the rules tested do not depend on it.
setPasswordHashCost(16);

const sha = text => createHash('sha256').update(text).digest('hex');

/** An app with an admin (boss) and an editor (ana, with an email); `ip` varies the client address. */
async function start({ mail } = {}) {
  const store = new Store({ localMode: true }, { sheets: new LocalSheets({}) });
  const sent = [];
  const app = await createApp(
    { localMode: true, secureCookies: false, setupToken: 'test-setup-secret', syncIntervalMs: 0 },
    {
      store,
      skipInitialSync: true,
      mailer: { bin: 'gog', account: 'team@example.org', publicUrl: 'https://app.example.org/ithomiini' },
      mail: mail ?? (async (_mailer, message) => void sent.push(message)),
    },
  );
  const address = await app.listen(0, '127.0.0.1');
  const origin = `http://127.0.0.1:${address.port}/ithomiini/api`;
  const call = async (path, { method = 'GET', body, cookie, csrf, ip } = {}) => {
    const response = await fetch(`${origin}${path}`, {
      method,
      headers: {
        'content-type': 'application/json',
        ...(cookie ? { cookie } : {}),
        ...(csrf ? { 'x-csrf-token': csrf } : {}),
        ...(ip ? { 'x-forwarded-for': ip } : {}),
      },
      body: body && JSON.stringify(body),
    });
    return {
      status: response.status,
      body: await response.json(),
      cookie: response.headers.get('set-cookie')?.split(';')[0],
    };
  };
  const admin = await call('/auth/setup', {
    method: 'POST',
    body: { token: 'test-setup-secret', username: 'boss', password: 'secret1', displayName: 'La Jefa' },
  });
  const as = { cookie: admin.cookie, csrf: admin.body.csrf };
  const ana = await call('/admin/users', {
    method: 'POST',
    ...as,
    body: { username: 'ana', password: 'oldpass1', role: 'editor', displayName: 'Ana', requestId: 'req-ana-001' },
  });
  store.db.prepare('UPDATE users SET email=? WHERE username=?').run('Ana.Perez@Example.org', 'ana');
  const signIn = (username, password, ip) => call('/auth/login', { method: 'POST', body: { username, password }, ip });
  return { app, store, sent, call, as, anaId: ana.body.user.id, signIn };
}
const tokenOf = message => /#\/restablecer\?t=([\w-]+)/.exec(message.text)[1];

test('sign in with the username or the email, in any case and with spaces around', async () => {
  const { app, signIn } = await start();
  try {
    assert.equal((await signIn('  ANA ', 'oldpass1')).status, 200);
    const byEmail = await signIn(' ana.perez@EXAMPLE.org ', 'oldpass1');
    assert.equal(byEmail.status, 200);
    assert.equal(byEmail.body.user.username, 'ana');
    assert.equal((await signIn('ana.perez@example.org', 'wrong11')).status, 401);
    assert.equal((await signIn('nobody@example.org', 'oldpass1')).status, 401);
  } finally {
    await app.close();
  }
});

test('failed sign-ins count against the account whether it is named by username or email', async () => {
  const { app, signIn } = await start();
  try {
    for (let i = 0; i < 4; i++) assert.equal((await signIn('ana', 'wrong11', `10.0.0.${i}`)).status, 401);
    for (let i = 4; i < 8; i++)
      assert.equal((await signIn('ANA.PEREZ@example.org', 'wrong11', `10.0.0.${i}`)).status, 401);
    // The ninth attempt is refused even with the right password, by either name, from a new address.
    assert.equal((await signIn('ana', 'oldpass1', '10.0.1.1')).status, 429);
    assert.equal((await signIn('ana.perez@example.org', 'oldpass1', '10.0.1.2')).status, 429);
    assert.equal((await signIn('boss', 'secret1', '10.0.1.3')).status, 200);
  } finally {
    await app.close();
  }
});

test('asking for a reset link answers the same whether or not the account exists; the email has the link', async () => {
  const { app, store, sent, call } = await start();
  try {
    const known = await call('/auth/reset/request', { method: 'POST', body: { identifier: ' Ana.Perez@example.org ' } });
    const unknown = await call('/auth/reset/request', { method: 'POST', body: { identifier: 'ghost@example.org' } });
    // boss has no email: nothing can be sent, same answer.
    const noEmail = await call('/auth/reset/request', { method: 'POST', body: { identifier: 'boss' } });
    for (const r of [known, unknown, noEmail]) assert.deepEqual([r.status, r.body], [202, { ok: true }]);
    assert.equal(sent.length, 1);
    assert.equal(sent[0].to, 'Ana.Perez@Example.org');
    assert.match(sent[0].subject, /Reset your Ithomiini database password/);
    assert.ok(sent[0].text.indexOf('Your username is ana') < sent[0].text.indexOf('Tu usuario es ana'));
    const token = tokenOf(sent[0]);
    assert.ok(sent[0].html.includes(`https://app.example.org/ithomiini/#/restablecer?t=${token}`));
    // Only the hash of the token is kept.
    const rows = store.db.prepare('SELECT * FROM password_resets').all();
    assert.equal(rows.length, 1);
    assert.equal(rows[0].token_hash, sha(token));
    assert.ok(!JSON.stringify(rows).includes(token));
    assert.ok(Date.parse(rows[0].expires_at) - Date.now() > 23.9 * 3600_000);
    assert.ok(rows[0].sent_at);
    // By username too; the new link replaces the older unused one.
    await call('/auth/reset/request', { method: 'POST', body: { identifier: 'ANA' } });
    assert.equal(sent.length, 2);
    assert.equal((await call(`/auth/reset/lookup?t=${token}`)).body.reset.status, 'expired');
    assert.equal((await call(`/auth/reset/lookup?t=${tokenOf(sent[1])}`)).body.reset.status, 'valid');
    assert.equal((await call('/auth/reset/request', { method: 'POST', body: { identifier: ' ' } })).status, 400);
  } finally {
    await app.close();
  }
});

test('the reset link changes the password, signs out other sessions, signs the person in and works once', async () => {
  const { app, store, sent, call, signIn } = await start();
  try {
    const old = await signIn('ana', 'oldpass1');
    await call('/auth/reset/request', { method: 'POST', body: { identifier: 'ana' } });
    const token = tokenOf(sent[0]);
    const lookup = await call(`/auth/reset/lookup?t=${token}`);
    assert.deepEqual(lookup.body.reset, { username: 'ana', displayName: 'Ana', status: 'valid' });
    assert.equal((await call('/auth/reset/lookup?t=nope')).status, 404);

    const weak = await call('/auth/reset', { method: 'POST', body: { token, password: '123' } });
    assert.equal(weak.body.error.code, 'WEAK_PASSWORD');
    const done = await call('/auth/reset', { method: 'POST', body: { token, password: 'newpass1' } });
    assert.equal(done.status, 200);
    assert.equal(done.body.user.username, 'ana');
    assert.equal((await call('/auth/session', { cookie: done.cookie })).body.user.username, 'ana');
    // The session from before the reset no longer works.
    assert.equal((await call('/auth/session', { cookie: old.cookie })).body.user, null);
    assert.equal((await signIn('ana', 'oldpass1')).status, 401);
    assert.equal((await signIn('ana', 'newpass1')).status, 200);
    // Single use.
    assert.equal((await call(`/auth/reset/lookup?t=${token}`)).body.reset.status, 'used');
    const again = await call('/auth/reset', { method: 'POST', body: { token, password: 'other12' } });
    assert.deepEqual([again.status, again.body.error.code], [409, 'RESET_USED']);

    // After 24 hours the link has expired.
    await call('/auth/reset/request', { method: 'POST', body: { identifier: 'ana' } });
    const late = tokenOf(sent[1]);
    store.db.prepare('UPDATE password_resets SET expires_at=? WHERE token_hash=?').run('2020-01-01T00:00:00.000Z', sha(late));
    assert.equal((await call(`/auth/reset/lookup?t=${late}`)).body.reset.status, 'expired');
    const expired = await call('/auth/reset', { method: 'POST', body: { token: late, password: 'other12' } });
    assert.deepEqual([expired.status, expired.body.error.code], [410, 'RESET_EXPIRED']);
    assert.equal((await signIn('ana', 'newpass1')).status, 200);
  } finally {
    await app.close();
  }
});

test('an administrator makes a reset link: emailed when there is an address, and always shown to copy', async () => {
  const { app, store, sent, call, as, anaId, signIn } = await start();
  try {
    const out = await call(`/admin/users/${anaId}/reset-link`, { method: 'POST', ...as, body: {} });
    assert.equal(out.status, 201);
    assert.equal(out.body.sent, true);
    assert.equal(out.body.email, 'Ana.Perez@Example.org');
    assert.match(out.body.link, /^https:\/\/app\.example\.org\/ithomiini\/#\/restablecer\?t=[\w-]+$/);
    assert.equal(sent.length, 1);
    assert.match(sent[0].text, /La Jefa made you a link/);
    assert.equal(tokenOf(sent[0]), out.body.link.split('t=')[1]);

    // Without an email the link is only shown.
    const bossId = store.db.prepare("SELECT id FROM users WHERE username='boss'").get().id;
    const own = await call(`/admin/users/${bossId}/reset-link`, { method: 'POST', ...as, body: {} });
    assert.deepEqual([own.body.sent, own.body.email], [false, null]);
    assert.equal(sent.length, 1);

    const token = out.body.link.split('t=')[1];
    assert.equal((await call('/auth/reset', { method: 'POST', body: { token, password: 'fromadm1' } })).status, 200);
    assert.equal((await signIn('ana', 'fromadm1')).status, 200);

    // Only administrators.
    const editor = await signIn('ana', 'fromadm1');
    const denied = await call(`/admin/users/${bossId}/reset-link`, {
      method: 'POST',
      cookie: editor.cookie,
      csrf: editor.body.csrf,
      body: {},
    });
    assert.equal(denied.status, 403);
    assert.equal((await call('/admin/users/nope/reset-link', { method: 'POST', ...as, body: {} })).status, 404);
  } finally {
    await app.close();
  }
});

test('a failed email still gives the administrator the link', async () => {
  const { app, call, as, anaId } = await start({
    mail: async () => {
      throw new Error('invalid_grant');
    },
  });
  try {
    const out = await call(`/admin/users/${anaId}/reset-link`, { method: 'POST', ...as, body: {} });
    assert.deepEqual([out.body.sent, out.body.sendError], [false, 'invalid_grant']);
    assert.match(out.body.link, /#\/restablecer\?t=/);
  } finally {
    await app.close();
  }
});

test('reset requests are limited per account (silently) and per address', async () => {
  const { app, sent, call } = await start();
  try {
    const ask = (identifier, ip) => call('/auth/reset/request', { method: 'POST', body: { identifier }, ip });
    // Three emails per account every 15 minutes, however it is named; later requests get the same answer.
    for (const [i, id] of ['ana', 'ANA.PEREZ@example.org', 'ana', 'ana', 'ana.perez@example.org'].entries())
      assert.equal((await ask(id, `10.1.0.${i}`)).status, 202);
    assert.equal(sent.length, 3);
    // Nine requests per address, then 429 (for any account).
    for (let i = 0; i < 9; i++) assert.equal((await ask(`user${i}`, '10.2.0.1')).status, 202);
    assert.equal((await ask('ana', '10.2.0.1')).status, 429);
    assert.equal((await ask('ana', '10.2.0.2')).status, 202);
  } finally {
    await app.close();
  }
});

test('guessing reset links is limited per address', async () => {
  const { app, call } = await start();
  try {
    const use = ip => call('/auth/reset', { method: 'POST', body: { token: 'guess', password: 'newpass1' }, ip });
    for (let i = 0; i < 24; i++) assert.equal((await use('10.3.0.1')).status, 404);
    assert.equal((await use('10.3.0.1')).status, 429);
    assert.equal((await use('10.3.0.2')).status, 404);
  } finally {
    await app.close();
  }
});
