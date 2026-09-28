import test from 'node:test';
import assert from 'node:assert/strict';
import { createApp } from '../server/index.mjs';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';

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
    assert.match(sent[0].text, /El proyecto de ithómidos te invitó a Ithomiini database/);
    assert.match(sent[0].text, /Invitación enviada por La Jefa\./);
    const token = /#\/activar\?t=([\w-]+)/.exec(sent[0].text)[1];
    assert.ok(sent[0].html.includes(`https://app.example.org/ithomiini/#/activar?t=${token}`));

    // Anyone with the link sees who it is for; nothing else is exposed.
    const lookup = await call(`/invitations/lookup?t=${token}`);
    assert.deepEqual(lookup.body.invitation, {
      email: 'ana@example.org',
      displayName: 'Ana Pérez',
      role: 'editor',
      status: 'pending',
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
