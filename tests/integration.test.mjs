import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { createApp } from '../server/index.mjs';

test('HTTP workflow enforces access, saves exact fields, reverses a selected edit and persists operations', async t => {
  const app = await createApp(
    {
      databasePath: ':memory:',
      localMode: true,
      secureCookies: false,
      syncIntervalMs: 0,
      setupToken: 'private-integration-setup',
    },
    {
      seed: {
        Collection_data: [
          {
            row: 2,
            values: {
              CAM_ID: 'CAMTEST',
              SPECIES: 'Test species',
              Sex: 'male',
              Subspecies_Form: 'alpha',
              Notes_Collection_data: 'original',
            },
          },
        ],
        Lists: [
          {
            row: 2,
            values: { Name: 'Unused full name', Abbr_name: 'AB Test', 'InsectaryWild&Reared_CAMid': 'CAM9001' },
          },
        ],
        Location_data: [{ row: 2, values: { Collection_location: 'Test river' } }],
      },
    },
  );
  await app.ready;
  const address = await app.listen(0);
  t.after(() => app.close());
  const base = `http://127.0.0.1:${address.port}/ithomiini`;
  let cookie = '',
    csrf = '';
  async function call(path, method = 'GET', body, authenticate = true) {
    const response = await fetch(base + path, {
      method,
      headers: {
        ...(body ? { 'content-type': 'application/json' } : {}),
        ...(authenticate && cookie ? { cookie, 'x-csrf-token': csrf } : {}),
      },
      body: body ? JSON.stringify({ requestId: randomUUID(), ...body }) : undefined,
    });
    const data = await response.json();
    if (response.headers.get('set-cookie')) cookie = response.headers.get('set-cookie').split(';')[0];
    if (data.csrf) csrf = data.csrf;
    return { response, data };
  }
  assert.equal((await call('/api/records')).response.status, 401);
  const setup = await call('/api/auth/setup', 'POST', {
    token: 'private-integration-setup',
    username: 'integration_admin',
    password: 'test-admin-123',
  });
  assert.equal(setup.response.status, 201);
  const boot = await call('/api/bootstrap');
  // An offline copy (LOCAL_MODE) has no sheet of its own: no link to the team's workbook.
  assert.equal(boot.data.settings.sheetUrl, null);
  assert.equal(boot.data.settings.localMode, true);
  assert.ok(
    (await call('/api/options?module=Collection_data&field=Identifier')).data.options.some(o => o.value === 'AB Test'),
  );
  assert.ok(
    (await call('/api/options?module=Collection_data&field=Collection_location')).data.options.some(
      o => o.value === 'Test river',
    ),
  );
  assert.equal(
    (await call('/api/options?module=Collection_data&field=Subspecies_Form&species=Missing')).data.options.length,
    0,
  );
  assert.equal(
    (await call('/api/options?module=Collection_data&field=Subspecies_Form&species=Test%20species')).data.options[0]
      .value,
    'alpha',
  );
  assert.equal(
    (await call('/api/options?module=Collection_data&field=CAM_ID_insectary')).data.options[0].value,
    'CAM9001',
  );
  const list = await call('/api/records?module=Collection_data');
  assert.equal(list.data.total, 1);
  const record = list.data.records[0];
  const patch = await call(`/api/records/${record.id}`, 'PATCH', {
    values: { Notes_Collection_data: 'reviewed correction' },
    expectedVersion: record.version,
  });
  assert.equal(patch.response.status, 200, JSON.stringify(patch.data));
  assert.equal(patch.data.record.values.Sex, 'male');
  assert.equal(patch.data.status, 'verified');
  const preview = await call('/api/history/preview', 'POST', { actionIds: [patch.data.action.id] });
  assert.equal(preview.response.status, 200);
  assert.equal(preview.data.conflicts.length, 0);
  const undo = await call('/api/history/undo', 'POST', {
    actionIds: [patch.data.action.id],
    reason: 'Integration reversal',
  });
  assert.equal(undo.response.status, 200, JSON.stringify(undo.data));
  const restored = await call(`/api/records/${record.id}`);
  assert.equal(restored.data.record.values.Notes_Collection_data, 'original');
  assert.equal(restored.data.record.values.Sex, 'male');
  const id = randomUUID();
  const event = await call('/api/events', 'POST', {
    requestId: id,
    kind: 'stage_round',
    recordId: record.id,
    values: { observed: 4 },
  });
  const duplicate = await call('/api/events', 'POST', {
    requestId: id,
    kind: 'stage_round',
    recordId: record.id,
    values: { observed: 4 },
  });
  assert.equal(duplicate.data.event.id, event.data.event.id);
  assert.equal((await call('/api/events')).data.events.length, 1);
  const task = await call('/api/tasks', 'POST', { title: 'Check specimen', recordId: record.id });
  assert.equal(task.response.status, 201);
  assert.equal((await call(`/api/tasks/${task.data.task.id}`, 'PATCH', { status: 'done' })).data.task.status, 'done');
  const user = await call('/api/admin/users', 'POST', {
    username: 'integration_observer',
    password: 'test-reader-123',
    role: 'observer',
  });
  assert.equal(user.response.status, 201);
  await call('/api/auth/logout', 'POST', {});
  await call('/api/auth/login', 'POST', { username: 'integration_observer', password: 'test-reader-123' });
  assert.equal((await call('/api/records')).response.status, 200);
  assert.equal((await call('/api/events', 'POST', { kind: 'attempted_edit' })).response.status, 403);
  assert.equal((await call('/api/admin/users')).response.status, 403);
});
