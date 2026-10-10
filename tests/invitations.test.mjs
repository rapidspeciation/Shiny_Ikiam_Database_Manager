import test from 'node:test';
import assert from 'node:assert/strict';
import { createApp } from '../server/index.mjs';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createHash } from 'node:crypto';
import { DatabaseSync } from 'node:sqlite';
import { createInvitations, initInvitations, statusOf } from '../server/invitations.mjs';
import { createAssistant, mcpTools } from '../server/assistant.mjs';
import { setPasswordHashCost } from '../server/auth.mjs';

setPasswordHashCost(16);

test('an admin invites someone by email; the link creates their account once and signs them in', async () => {
  const store = new Store({ localMode: true }, { sheets: new LocalSheets({}) });
  const sent = [];
  const app = await createApp(
    { localMode: true, secureCookies: false, setupToken: 'test-setup-secret', syncIntervalMs: 0 },
    {
      store,
      skipInitialSync: true,
      mailer: { bin: 'gog', account: 'team@example.org', publicUrl: 'https://app.example.org/ithomiini' },
      mail: async (_mailer, message) => void sent.push(message),
    },
  );
  const address = await app.listen(0, '127.0.0.1');
  const origin = `http://127.0.0.1:${address.port}/ithomiini/api`;
  const call = async (path, { method = 'GET', body, cookie, csrf } = {}) => {
    const response = await fetch(`${origin}${path}`, {
      method,
      headers: {
        'content-type': 'application/json',
        ...(cookie ? { cookie } : {}),
        ...(csrf ? { 'x-csrf-token': csrf } : {}),
      },
      body: body && JSON.stringify(body),
    });
    return {
      status: response.status,
      body: await response.json(),
      cookie: response.headers.get('set-cookie')?.split(';')[0],
    };
  };
  try {
    const admin = await call('/auth/setup', {
      method: 'POST',
      body: { token: 'test-setup-secret', username: 'boss', password: 'secret1', displayName: 'La Jefa' },
    });
    const as = { cookie: admin.cookie, csrf: admin.body.csrf };
    const created = await call('/admin/invitations', {
      method: 'POST',
      ...as,
      body: { email: 'Ana@Example.org', displayName: 'Ana Pérez', role: 'editor' },
    });
    assert.equal(created.status, 201);
    assert.equal(created.body.invitation.status, 'pending');
    assert.ok(created.body.invitation.sentAt);
    assert.equal(sent.length, 1);
    assert.equal(sent[0].to, 'ana@example.org');
    // Who invited them and the address they may sign in with.
    assert.ok(sent[0].text.includes('La Jefa'));
    assert.ok(sent[0].text.includes('ana@example.org'));
    const token = /#\/activar\?t=([\w-]+)/.exec(sent[0].text)[1];
    assert.ok(sent[0].html.includes(`https://app.example.org/ithomiini/#/activar?t=${token}`));

    // Anyone with the link sees who it is for; nothing else is exposed.
    const lookup = await call(`/invitations/lookup?t=${token}`);
    assert.deepEqual(lookup.body.invitation, {
      email: 'ana@example.org',
      displayName: 'Ana Pérez',
      role: 'editor',
      status: 'pending',
      codeSent: false,
    });
    assert.equal((await call('/invitations/lookup?t=wrong')).status, 404);

    assert.equal(
      (await call('/invitations/accept', { method: 'POST', body: { token, username: 'boss', password: 'secret9' } }))
        .status,
      409,
    );
    const accepted = await call('/invitations/accept', {
      method: 'POST',
      body: { token, username: 'ana', password: 'secret9' },
    });
    assert.equal(accepted.status, 201);
    assert.equal(accepted.body.user.role, 'editor');
    assert.equal(accepted.body.user.email, 'ana@example.org');
    const session = await call('/auth/session', { cookie: accepted.cookie });
    assert.equal(session.body.user.username, 'ana');
    // The link works once.
    assert.equal(
      (await call('/invitations/accept', { method: 'POST', body: { token, username: 'ana2', password: 'secret9' } }))
        .status,
      409,
    );
    // The same email cannot be invited again while the account is active.
    assert.equal(
      (
        await call('/admin/invitations', {
          method: 'POST',
          ...as,
          body: { email: 'ana@example.org', displayName: 'Ana' },
        })
      ).status,
      409,
    );
    const list = await call('/admin/invitations', as);
    assert.equal(list.body.invitations[0].status, 'used');
    // Only administrators manage invitations.
    const ana = { cookie: accepted.cookie, csrf: accepted.body.csrf };
    assert.equal((await call('/admin/invitations', ana)).status, 403);
  } finally {
    await app.close();
  }
});

test('a failed email still gives the administrator the link to share by hand', async () => {
  const store = new Store({ localMode: true }, { sheets: new LocalSheets({}) });
  const app = await createApp(
    { localMode: true, secureCookies: false, setupToken: 'test-setup-secret', syncIntervalMs: 0 },
    {
      store,
      skipInitialSync: true,
      mailer: { bin: 'gog', account: 'team@example.org', publicUrl: 'https://app.example.org/ithomiini' },
      mail: async () => {
        throw new Error('invalid_grant');
      },
    },
  );
  const address = await app.listen(0, '127.0.0.1');
  const origin = `http://127.0.0.1:${address.port}/ithomiini/api`;
  try {
    const setup = await fetch(`${origin}/auth/setup`, {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({ token: 'test-setup-secret', username: 'boss', password: 'secret1' }),
    });
    const { csrf } = await setup.json();
    const response = await fetch(`${origin}/admin/invitations`, {
      method: 'POST',
      headers: {
        'content-type': 'application/json',
        cookie: setup.headers.get('set-cookie').split(';')[0],
        'x-csrf-token': csrf,
      },
      body: JSON.stringify({ email: 'luis@example.org', displayName: 'Luis' }),
    });
    const out = await response.json();
    assert.equal(out.invitation.sentAt, null);
    assert.equal(out.invitation.sendError, 'invalid_grant');
    assert.match(out.link, /#\/activar\?t=/);
  } finally {
    await app.close();
  }
});

/** The app with an administrator signed in and the emails caught in `sent`. */
async function start() {
  const store = new Store({ localMode: true }, { sheets: new LocalSheets({}) });
  const sent = [];
  const app = await createApp(
    { localMode: true, secureCookies: false, setupToken: 'test-setup-secret', syncIntervalMs: 0 },
    {
      store,
      skipInitialSync: true,
      mailer: { bin: 'gog', account: 'team@example.org', publicUrl: 'https://app.example.org/ithomiini' },
      mail: async (_mailer, message) => void sent.push(message),
    },
  );
  const address = await app.listen(0, '127.0.0.1');
  const origin = `http://127.0.0.1:${address.port}/ithomiini/api`;
  const call = async (path, { method = 'GET', body, cookie, csrf } = {}) => {
    const response = await fetch(`${origin}${path}`, {
      method,
      headers: {
        'content-type': 'application/json',
        ...(cookie ? { cookie } : {}),
        ...(csrf ? { 'x-csrf-token': csrf } : {}),
      },
      body: body && JSON.stringify(body),
    });
    return { status: response.status, body: await response.json(), cookie: response.headers.get('set-cookie')?.split(';')[0] };
  };
  const admin = await call('/auth/setup', {
    method: 'POST',
    body: { token: 'test-setup-secret', username: 'boss', password: 'secret1', displayName: 'La Jefa' },
  });
  const as = { cookie: admin.cookie, csrf: admin.body.csrf };
  /** Invites someone; returns the invitation and its link's token. */
  const invite = async (email, role = 'editor') => {
    const out = await call('/admin/invitations', { method: 'POST', ...as, body: { email, displayName: 'Paula', role } });
    return { invitation: out.body.invitation, token: /#\/activar\?t=([\w-]+)/.exec(out.body.link)[1] };
  };
  /** The invitation past its 7 days. */
  const age = id => store.db.prepare('UPDATE invitations SET expires_at=? WHERE id=?').run('2026-01-01T00:00:00.000Z', id);
  const listed = async id => (await call('/admin/invitations', as)).body.invitations.find(i => i.id === id);
  const lastCode = () => /^(\d{6}) ·/.exec(sent.at(-1).subject)?.[1];
  return { store, sent, app, call, as, invite, age, listed, lastCode };
}

test('the invitation email: the link does not expire, a code after 7 days, sign in with username or email', async () => {
  const { sent, app, invite } = await start();
  try {
    await invite('paula@example.org');
    const { text, html, subject } = sent[0];
    assert.match(subject, /Ithomiini database/);
    for (const part of ['La Jefa', 'paula@example.org', '6-digit code', '6 cifras', "Ikiam's Wi-Fi", 'Wi-Fi de Ikiam'])
      assert.ok(text.includes(part), part);
    assert.match(text, /#\/activar\?t=/);
    assert.match(html, /<strong>Ithomiini database<\/strong>/);
    assert.doesNotMatch(text, /single use|una sola vez/);
  } finally {
    await app.close();
  }
});

test('after 7 days the link asks for an emailed code, then creates the account; openings are counted', async () => {
  const { store, sent, app, call, invite, age, listed, lastCode } = await start();
  try {
    const { invitation, token } = await invite('paula@example.org');
    age(invitation.id);
    const looked = (await call(`/invitations/lookup?t=${token}`)).body.invitation;
    assert.equal(looked.status, 'expired');
    assert.equal(looked.email, 'p•••a@example.org', 'the address masked');
    assert.equal(looked.codeSent, false);
    await call(`/invitations/lookup?t=${token}`);
    const shown = await listed(invitation.id);
    assert.equal(shown.status, 'expired');
    assert.equal(shown.expiredOpens, 2);
    assert.ok(shown.expiredOpenedAt);

    // Without a code the link does not create the account.
    const body = { token, username: 'paula', password: 'secret9' };
    assert.equal((await call('/invitations/accept', { method: 'POST', body })).body.error.code, 'CODE_NEEDED');

    const asked = await call('/invitations/code', { method: 'POST', body: { token } });
    assert.equal(asked.status, 200);
    assert.equal(asked.body.email, 'p•••a@example.org');
    assert.equal(sent.at(-1).to, 'paula@example.org');
    const code = lastCode();
    assert.match(code, /^\d{6}$/);
    assert.ok(sent.at(-1).text.includes(code) && sent.at(-1).html.includes(code));
    assert.equal((await call(`/invitations/lookup?t=${token}`)).body.invitation.codeSent, true);
    // Only its hash is kept.
    const row = store.db.prepare('SELECT * FROM invitations WHERE id=?').get(invitation.id);
    assert.ok(!Object.values(row).some(v => String(v).includes(code)));

    const wrong = String((Number(code) + 1) % 1e6).padStart(6, '0');
    assert.equal((await call('/invitations/code/check', { method: 'POST', body: { token, code: wrong } })).body.error.code, 'CODE_WRONG');
    const right = await call('/invitations/code/check', { method: 'POST', body: { token, code } });
    assert.equal(right.status, 200);
    assert.equal(right.body.email, 'paula@example.org', 'the right code shows the whole address');
    assert.equal((await call('/invitations/accept', { method: 'POST', body: { ...body, code: wrong } })).status, 400);
    const accepted = await call('/invitations/accept', { method: 'POST', body: { ...body, code } });
    assert.equal(accepted.status, 201);
    assert.equal(accepted.body.user.email, 'paula@example.org');
    assert.equal((await listed(invitation.id)).status, 'used');
    // The code and the link work once.
    assert.equal((await call('/invitations/accept', { method: 'POST', body: { ...body, username: 'paula2', code } })).status, 409);
  } finally {
    await app.close();
  }
});

test('codes: a new one replaces the last, they expire, wrong tries and requests are limited', async () => {
  const { store, app, call, invite, age, lastCode } = await start();
  try {
    const { invitation, token } = await invite('paula@example.org');
    age(invitation.id);
    const ask = () => call('/invitations/code', { method: 'POST', body: { token } });
    const check = code => call('/invitations/code/check', { method: 'POST', body: { token, code } });
    await ask();
    const first = lastCode();
    await ask();
    const second = lastCode();
    if (first !== second) assert.equal((await check(first)).status, 400, 'the older code stopped working');
    // Expired after its 15 minutes.
    store.db.prepare('UPDATE invitations SET code_expires_at=? WHERE id=?').run('2026-01-01T00:00:00.000Z', invitation.id);
    assert.equal((await check(second)).body.error.code, 'CODE_EXPIRED');

    // A few wrong tries, then the code is used up, even the right one.
    await ask();
    const third = lastCode();
    const wrong = String((Number(third) + 1) % 1e6).padStart(6, '0');
    const answers = [];
    for (let i = 0; i < 5; i++) answers.push((await check(wrong)).body.error.code);
    assert.deepEqual(answers, ['CODE_WRONG', 'CODE_WRONG', 'CODE_WRONG', 'CODE_WRONG', 'CODE_EXPIRED']);
    assert.notEqual((await check(third)).status, 200);

    // Three codes per invitation an hour.
    const limited = await ask();
    assert.equal(limited.status, 429);
    assert.equal(limited.body.error.code, 'CODE_LIMIT');
  } finally {
    await app.close();
  }
});

test('a revoked invitation never works, with or without a code; resending makes it work again with 7 new days', async () => {
  const { app, call, as, invite, age, listed } = await start();
  try {
    const { invitation, token } = await invite('paula@example.org');
    const revoked = await call(`/admin/invitations/${invitation.id}/revoke`, { method: 'POST', ...as, body: {} });
    assert.equal(revoked.body.invitation.status, 'revoked');
    assert.equal((await call(`/invitations/lookup?t=${token}`)).body.invitation.status, 'revoked');
    const body = { token, username: 'paula', password: 'secret9' };
    assert.equal((await call('/invitations/accept', { method: 'POST', body })).body.error.code, 'INVITATION_REVOKED');
    age(invitation.id);
    assert.equal((await call(`/invitations/lookup?t=${token}`)).body.invitation.status, 'revoked');
    assert.equal((await call('/invitations/code', { method: 'POST', body: { token } })).body.error.code, 'INVITATION_REVOKED');
    assert.equal((await listed(invitation.id)).expiredOpens, 0, 'a revoked link is not counted as expired');

    const resent = await call(`/admin/invitations/${invitation.id}/resend`, { method: 'POST', ...as, body: {} });
    assert.equal(resent.body.invitation.status, 'pending');
    assert.ok(resent.body.invitation.expiresAt > new Date(Date.now() + 6 * 864e5).toISOString());
    const fresh = /#\/activar\?t=([\w-]+)/.exec(resent.body.link)[1];
    assert.equal((await call('/invitations/accept', { method: 'POST', body })).status, 404, 'the old link stopped working');
    assert.equal((await call('/invitations/accept', { method: 'POST', body: { ...body, token: fresh } })).status, 201);

    // Inviting the same address again replaces its open invitation.
    const a = await invite('luis@example.org');
    const b = await invite('luis@example.org', 'observer');
    assert.equal((await listed(a.invitation.id)).status, 'revoked');
    assert.equal((await listed(b.invitation.id)).status, 'pending');
  } finally {
    await app.close();
  }
});

test('invitations revoked before revoked_at existed are marked revoked when the column is added', () => {
  const db = new DatabaseSync(':memory:');
  db.exec(`CREATE TABLE users(id TEXT PRIMARY KEY, username TEXT, display_name TEXT, role TEXT, salt TEXT, password_hash TEXT, active INTEGER, created_at TEXT);
    CREATE TABLE invitations(
    id TEXT PRIMARY KEY, email TEXT NOT NULL, display_name TEXT NOT NULL, role TEXT NOT NULL,
    token_hash TEXT NOT NULL UNIQUE, created_by TEXT NOT NULL, created_at TEXT NOT NULL, expires_at TEXT NOT NULL,
    sent_at TEXT, send_error TEXT, used_at TEXT, user_id TEXT)`);
  const add = db.prepare(
    "INSERT INTO invitations(id,email,display_name,role,token_hash,created_by,created_at,expires_at,used_at) VALUES(?,'a@b.org','A','editor',?,'boss',?,?,?)",
  );
  add.run('revoked', 'h1', '2026-09-29T10:00:00.000Z', '2026-09-30T08:00:00.000Z', null);
  add.run('lapsed', 'h2', '2026-09-29T10:00:00.000Z', '2026-10-06T10:00:00.000Z', null);
  add.run('used', 'h4', '2026-09-29T10:00:00.000Z', '2026-09-30T10:00:00.000Z', '2026-09-30T09:00:00.000Z');
  initInvitations(db);
  initInvitations(db);
  const statuses = Object.fromEntries(db.prepare('SELECT * FROM invitations').all().map(r => [r.id, statusOf(r)]));
  assert.deepEqual(statuses, { revoked: 'revoked', lapsed: 'expired', used: 'used' });
});

test('invite_person from a chat: administrators only, a preview first, sent on confirmed, an open invitation sent again', async () => {
  const store = new Store({ localMode: true }, { sheets: new LocalSheets({}) });
  const sent = [];
  let failing = false;
  const invitations = createInvitations(
    store,
    { bin: 'gog', account: 'team@example.org', publicUrl: 'https://app.example.org' },
    {
      send: async (_m, message) => {
        if (failing) throw new Error('invalid_grant');
        sent.push(message);
      },
    },
  );
  const assistant = createAssistant({ store, config: {}, invitations });
  const person = (id, role) => {
    store.db
      .prepare(
        "INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at,email) VALUES(?,?,?,?,'s','h',1,'2026-01-01',?)",
      )
      .run(id, id, `Person ${id}`, role, `${id}@example.org`);
    store.db
      .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
      .run(createHash('sha256').update(`${id}-token`).digest('hex'), id);
    return async (name, args) => {
      const out = await assistant.mcp(
        { authorization: `Bearer ${id}-token` },
        { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } },
      );
      return JSON.parse(out.body.result.content[0].text);
    };
  };
  const boss = person('boss', 'admin');
  const ed = person('ed', 'editor');
  try {
    const tools = mcpTools().filter(t => ['invite_person', 'list_invitations'].includes(t.name));
    assert.equal(tools.length, 2);
    assert.ok(tools.every(t => !t._meta?.['anthropic/alwaysLoad']), 'loaded on demand');

    const asEditor = await ed('invite_person', { email: 'paula@example.org', name: 'Paula', role: 'editor', confirmed: true });
    assert.match(asEditor.error, /administrator/);
    assert.match((await ed('list_invitations', {})).error, /administrator/);

    // A role is needed.
    assert.match((await boss('invite_person', { email: 'paula@example.org', name: 'Paula' })).error, /role/);
    const preview = await boss('invite_person', { email: 'Paula@Example.org', name: 'Paula', role: 'editor' });
    assert.equal(preview.preview, true);
    assert.equal(preview.sent, false);
    assert.equal(preview.action, 'invite');
    assert.equal(preview.email, 'paula@example.org');
    assert.match(preview.access, /edit/);
    assert.equal(sent.length, 0, 'a preview sends nothing');
    assert.equal(invitations.list().length, 0);

    const done = await boss('invite_person', { email: 'paula@example.org', name: 'Paula', role: 'editor', confirmed: true });
    assert.equal(done.sent, true);
    assert.equal(done.status, 'pending');
    assert.ok(!done.link, 'the link only when the email failed');
    assert.equal(sent.length, 1);
    assert.equal(sent[0].to, 'paula@example.org');
    assert.ok(sent[0].text.includes('Person boss'), 'sent in the administrator’s name');

    // The same address again: its open invitation is sent again, not a second one.
    const again = await boss('invite_person', { email: 'paula@example.org', name: 'Paula', role: 'editor' });
    assert.equal(again.action, 'resend');
    assert.equal(again.openInvitation.status, 'pending');
    failing = true;
    const resent = await boss('invite_person', { email: 'paula@example.org', name: 'Paula', role: 'editor', confirmed: true });
    assert.equal(resent.sent, false);
    assert.equal(resent.sendError, 'invalid_grant');
    assert.match(resent.link, /#\/activar\?t=/);
    assert.equal(invitations.list().length, 1);

    // An address with an account: nothing to send.
    const exists = await boss('invite_person', { email: 'ed@example.org', name: 'Ed', role: 'admin', confirmed: true });
    assert.equal(exists.action, 'exists');
    assert.equal(exists.sent, false);
    assert.equal(exists.account.username, 'ed');

    const listed = await boss('list_invitations', {});
    assert.deepEqual(
      listed.invitations.map(i => [i.email, i.status]),
      [['paula@example.org', 'pending']],
    );

    // A role taken away counts at once, in a chat already open.
    store.db.prepare("UPDATE users SET role='editor' WHERE id='boss'").run();
    assert.match((await boss('list_invitations', {})).error, /administrator/);
  } finally {
    assistant.close?.();
  }
});
