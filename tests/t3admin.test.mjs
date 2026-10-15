import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, mkdirSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { newer, t3Admin, t3Pairing } from '../server/t3admin.mjs';

test('only a higher stable T3 version is an update', () => {
  assert.equal(newer('0.0.43', '0.0.42'), true);
  assert.equal(newer('0.1.0', '0.0.99'), true);
  assert.equal(newer('0.0.42', '0.0.42'), false);
  assert.equal(newer('0.0.41', '0.0.42'), false);
  assert.equal(newer('0.0.43-nightly.20260928', '0.0.42'), false);
  assert.equal(newer(null, '0.0.42'), false);
});

test('T3 status: installed version, newest stable release (nightlies skipped), unit state', async () => {
  const home = mkdtempSync(join(tmpdir(), 't3-'));
  mkdirSync(join(home, 'runtime'));
  writeFileSync(join(home, 'runtime', 'service-state.json'), JSON.stringify({ protocol: 2, activeVersion: '0.0.42' }));
  const fetchImpl = async () => ({ ok: true, json: async () => [{ tag_name: 'v0.0.44-nightly.1' }, { tag_name: 'v0.0.43' }, { tag_name: 'v0.0.42' }] });
  const status = await t3Admin({ home, systemctl: 'false', fetchImpl }).status();
  assert.equal(status.current, '0.0.42');
  assert.equal(status.latest, '0.0.43');
  assert.equal(status.updateAvailable, true);
  assert.equal(status.updating, false);
  await assert.rejects(t3Admin({ home, systemctl: 'false', fetchImpl }).update(), /No se pudo iniciar/);
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
