import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { createApp } from '../server/index.mjs';
import { moduleMap } from '../server/schema.mjs';

const fields = moduleMap.get('Insectary_stocks').fields;
const col = key => fields.find(f => f.key === key).column;
/** A stocks row as Google Sheets sends it: values and the counts' sum formulas. */
function stocksRow(row, values, formulas = {}) {
  const cells = [];
  for (const [key, value] of Object.entries(values))
    cells[col(key)] = { userEnteredValue: typeof value === 'number' ? { numberValue: value } : { stringValue: value } };
  for (const [key, [formula, total]] of Object.entries(formulas))
    cells[col(key)] = { userEnteredValue: { formulaValue: formula }, effectiveValue: { numberValue: total } };
  return { row, cells };
}

test('clutch checks: marked by editors, seen by everyone, with the day\'s changes before → after', async t => {
  const app = await createApp(
    { databasePath: ':memory:', localMode: true, secureCookies: false, syncIntervalMs: 0, setupToken: 'clutch-checks-setup' },
    {
      seed: {
        Insectary_stocks: [
          stocksRow(1012, { 'CLUTCH NUMBER': 1012, SPECIES: 'Mechanitis lysimnia', 'DATE LAID': 46286 }, {
            'NUMBER OF EGGS': ['=14+12', 26],
            'NUMBER OF LARVAE': ['=3+5-2', 6],
          }),
          stocksRow(1013, { 'CLUTCH NUMBER': 1013, SPECIES: 'Mechanitis polymnia proceriformis' }, { 'NUMBER OF EGGS': ['=15+1', 16] }),
        ],
        Collection_data: [{ row: 2, values: { CAM_ID: 'CAM1', SPECIES: 'Test species' } }],
      },
    },
  );
  await app.ready;
  const address = await app.listen(0);
  t.after(() => app.close());
  const base = `http://127.0.0.1:${address.port}/ithomiini`;
  /** One signed-in person (their own cookie and CSRF token). */
  function person() {
    const me = { cookie: '', csrf: '' };
    me.call = async (path, method = 'GET', body) => {
      const response = await fetch(base + path, {
        method,
        headers: { ...(body ? { 'content-type': 'application/json' } : {}), ...(me.cookie ? { cookie: me.cookie, 'x-csrf-token': me.csrf } : {}) },
        body: body ? JSON.stringify(body) : undefined,
      });
      const data = await response.json();
      if (response.headers.get('set-cookie')) me.cookie = response.headers.get('set-cookie').split(';')[0];
      if (data.csrf) me.csrf = data.csrf;
      return { status: response.status, data };
    };
    return me;
  }
  const admin = person();
  assert.equal((await admin.call('/api/clutches/day')).status, 401);
  assert.equal(
    (await admin.call('/api/auth/setup', 'POST', { token: 'clutch-checks-setup', username: 'checks_admin', password: 'test-admin-123' })).status,
    201,
  );
  for (const [username, role, displayName] of [
    ['ana', 'editor', 'Ana Torres'],
    ['beto', 'editor', 'Beto Paz'],
    ['olga', 'observer', 'Olga Ríos'],
  ])
    assert.equal(
      (await admin.call('/api/admin/users', 'POST', { requestId: randomUUID(), username, role, displayName, password: 'test-pass-123' })).status,
      201,
    );
  const login = async username => {
    const p = person();
    assert.equal((await p.call('/api/auth/login', 'POST', { username, password: 'test-pass-123' })).status, 200);
    return p;
  };
  const ana = await login('ana');
  const beto = await login('beto');
  const olga = await login('olga');

  // The counts' formulas (the table only carries their totals).
  const state = (await ana.call('/api/clutches/state')).data;
  const clutches = (await ana.call('/api/records?module=Insectary_stocks')).data.records;
  const c1012 = clutches.find(r => r.values['CLUTCH NUMBER'] === 1012);
  const c1013 = clutches.find(r => r.values['CLUTCH NUMBER'] === 1013);
  assert.deepEqual(state.sums[c1012.id], { 'NUMBER OF EGGS': '=14+12', 'NUMBER OF LARVAE': '=3+5-2' });
  assert.deepEqual(state.last, {});
  let day = (await olga.call('/api/clutches/day')).data;
  assert.deepEqual([day.checks, day.changes], [[], []]);
  assert.match(day.day, /^\d{4}-\d{2}-\d{2}$/);

  // "Checked, no change"; the same request again is the same check.
  const requestId = randomUUID();
  const first = await ana.call('/api/clutches/checks', 'POST', { requestId, recordId: c1013.id });
  assert.equal(first.status, 201);
  assert.deepEqual([first.data.check.clutch, first.data.check.fields, first.data.check.name], ['1013', [], 'Ana Torres']);
  const again = await ana.call('/api/clutches/checks', 'POST', { requestId, recordId: c1013.id });
  assert.equal(again.status, 200);
  assert.equal(again.data.check.id, first.data.check.id);

  // A change saved as usual (3 larvae counted today: =3+5-2 → =3+5-2-3), then its check.
  const saved = await beto.call('/api/records/batch', 'POST', {
    requestId: randomUUID(),
    purpose: 'clutches',
    edits: [{ id: c1012.id, values: { 'NUMBER OF LARVAE': '=3+5-2-3', NOTES: '1/10/26 BP: 3 larvae dead' }, expected: { 'NUMBER OF LARVAE': 6, NOTES: null } }],
  });
  assert.equal(saved.status, 200);
  const checked = await beto.call('/api/clutches/checks', 'POST', {
    requestId: randomUUID(),
    recordId: c1012.id,
    fields: ['NUMBER OF LARVAE', 'NOTES'],
    actionId: saved.data.action.id,
  });
  assert.equal(checked.status, 201);
  day = (await olga.call('/api/clutches/day')).data;
  assert.deepEqual(
    day.checks.map(c => [c.clutch, c.name, c.fields]),
    [
      ['1013', 'Ana Torres', []],
      ['1012', 'Beto Paz', ['NUMBER OF LARVAE', 'NOTES']],
    ],
  );
  const larvae = day.changes.find(c => c.field === 'NUMBER OF LARVAE');
  assert.deepEqual(
    [larvae.clutch, larvae.before, larvae.after, larvae.actors, larvae.isNew],
    ['1012', { formula: '=3+5-2' }, { formula: '=3+5-2-3' }, ['Beto Paz'], false],
  );
  assert.equal((await ana.call('/api/clutches/state')).data.last[c1012.id].name, 'Beto Paz');
  assert.equal((await ana.call('/api/clutches/state')).data.sums[c1012.id]['NUMBER OF LARVAE'], '=3+5-2-3');
  // Another day has nothing.
  assert.deepEqual((await olga.call('/api/clutches/day?day=2020-01-01')).data.checks, []);
  assert.equal((await olga.call('/api/clutches/day?day=yesterday')).status, 400);

  // Observers see the checks but cannot mark them; bad requests are refused.
  assert.equal((await olga.call('/api/clutches/checks', 'POST', { requestId: randomUUID(), recordId: c1013.id })).status, 403);
  assert.equal((await ana.call('/api/clutches/checks', 'POST', { recordId: c1013.id })).status, 400);
  const other = (await ana.call('/api/records?module=Collection_data')).data.records[0];
  assert.equal((await ana.call('/api/clutches/checks', 'POST', { requestId: randomUUID(), recordId: other.id })).status, 404);
  assert.equal((await ana.call('/api/clutches/checks', 'POST', { requestId: randomUUID(), recordId: c1013.id, fields: ['Nope'] })).status, 400);
  assert.equal(
    (await ana.call('/api/clutches/checks', 'POST', { requestId: randomUUID(), recordId: c1013.id, actionId: 'missing' })).status,
    404,
  );

  // A check marked by mistake is taken back by its author (or a reviewer or admin), not by others.
  const mine = first.data.check.id;
  assert.equal((await beto.call(`/api/clutches/checks/${mine}`, 'DELETE', {})).status, 403);
  assert.equal((await olga.call(`/api/clutches/checks/${mine}`, 'DELETE', {})).status, 403);
  assert.equal((await ana.call(`/api/clutches/checks/${mine}`, 'DELETE', {})).status, 200);
  assert.equal((await ana.call(`/api/clutches/checks/${mine}`, 'DELETE', {})).status, 404);
  assert.deepEqual((await olga.call('/api/clutches/day')).data.checks.map(c => c.clutch), ['1012']);
  assert.equal((await admin.call(`/api/clutches/checks/${checked.data.check.id}`, 'DELETE', {})).status, 200);
});
