import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, mkdirSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { newer, t3Admin } from '../server/t3admin.mjs';

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
