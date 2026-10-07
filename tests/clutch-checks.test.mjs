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

// The clutches' routes: who may read and who may mark. What they do: tests/clutch-events.test.mjs.
test('clutch routes: checks and events marked by editors, seen by everyone; the setting only by an admin', async t => {
  const app = await createApp(
    { databasePath: ':memory:', localMode: true, secureCookies: false, syncIntervalMs: 0, setupToken: 'clutch-checks-setup' },
    {
      seed: {
        Insectary_stocks: [
          stocksRow(1012, { 'CLUTCH NUMBER': 1012, SPECIES: 'Mechanitis lysimnia', 'DATE LAID': 46286 }, {
            'NUMBER OF EGGS': ['=14+12', 26],
            'NUMBER OF LARVAE': ['=3+5-2', 6],
          }),
        ],
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
  const people = {};
  for (const [username, role, displayName] of [
    ['ana', 'editor', 'Ana Torres'],
    ['olga', 'observer', 'Olga Ríos'],
  ]) {
    assert.equal(
      (await admin.call('/api/admin/users', 'POST', { requestId: randomUUID(), username, role, displayName, password: 'test-pass-123' })).status,
      201,
    );
    people[username] = person();
    assert.equal((await people[username].call('/api/auth/login', 'POST', { username, password: 'test-pass-123' })).status, 200);
  }
  const { ana, olga } = people;

  // The counts' formulas (the table only carries their totals).
  const state = (await olga.call('/api/clutches/state')).data;
  const [c1012] = (await olga.call('/api/records?module=Insectary_stocks')).data.records;
  assert.deepEqual(state.sums[c1012.id], { 'NUMBER OF EGGS': '=14+12', 'NUMBER OF LARVAE': '=3+5-2' });

  // Checks: an editor marks (the same request again: 200), an observer sees them; a bad one is 400, a missing row 404.
  const requestId = randomUUID();
  const marked = await ana.call('/api/clutches/checks', 'POST', { requestId, recordId: c1012.id, state: 'verify', note: 'Count unsure' });
  assert.deepEqual([marked.status, marked.data.check.state, marked.data.check.name], [201, 'verify', 'Ana Torres']);
  assert.equal((await ana.call('/api/clutches/checks', 'POST', { requestId, recordId: c1012.id })).status, 200);
  assert.equal((await olga.call('/api/clutches/checks', 'POST', { requestId: randomUUID(), recordId: c1012.id })).status, 403);
  assert.equal((await ana.call('/api/clutches/checks', 'POST', { recordId: c1012.id })).status, 400);
  assert.equal((await ana.call('/api/clutches/checks', 'POST', { requestId: randomUUID(), recordId: 'missing' })).status, 404);
  assert.deepEqual((await olga.call('/api/clutches/day')).data.checks.map(c => c.name), ['Ana Torres']);
  assert.equal((await olga.call('/api/clutches/day?day=yesterday')).status, 400);
  assert.equal((await olga.call(`/api/clutches/checks/${marked.data.check.id}`, 'DELETE', {})).status, 403);
  assert.equal((await ana.call(`/api/clutches/checks/${marked.data.check.id}`, 'DELETE', {})).status, 200);

  // Events: recorded by editors, seen by everyone; taken back by their author.
  const died = { requestId: randomUUID(), recordId: c1012.id, stage: 'larva', kind: 'died', count: 2 };
  assert.equal((await olga.call('/api/clutches/events', 'POST', died)).status, 403);
  const event = await ana.call('/api/clutches/events', 'POST', died);
  assert.deepEqual([event.status, event.data.event.kind, event.data.event.name], [201, 'died', 'Ana Torres']);
  assert.equal((await ana.call('/api/clutches/events', 'POST', died)).status, 200);
  const events = (await olga.call(`/api/clutches/events?recordId=${c1012.id}`)).data;
  assert.deepEqual([events.events.length, events.tally.larva.died], [1, 2]);
  assert.equal((await olga.call(`/api/clutches/events/${event.data.event.id}`, 'DELETE', {})).status, 403);

  // The setting only by an admin; the notebook's list for everyone, its "up to" set by editors.
  assert.equal((await ana.call('/api/clutches/settings', 'PUT', { subtractPreserved: true })).status, 403);
  assert.equal((await admin.call('/api/clutches/settings', 'PUT', { subtractPreserved: true })).status, 200);
  assert.equal((await olga.call('/api/clutches/state')).data.settings.subtractPreserved, true);
  assert.equal((await olga.call('/api/clutches/notebook')).status, 200);
  assert.equal((await olga.call('/api/clutches/notebook/up-to', 'PUT', { at: new Date().toISOString() })).status, 403);
  assert.equal((await ana.call('/api/clutches/notebook/up-to', 'PUT', { at: new Date().toISOString() })).status, 200);
  assert.equal((await olga.call('/api/clutches/notebook')).data.upTo.name, 'Ana Torres');
});
