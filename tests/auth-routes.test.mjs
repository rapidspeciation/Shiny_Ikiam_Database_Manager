// Who may call which route: the team's data needs a sign-in, some work a role. One app, a
// visitor and a person of each role; what each route answers once allowed is tested with its
// feature (search, summary, wikiloc-transects, premade, review, proposal-links).
import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { createApp } from '../server/index.mjs';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { setPasswordHashCost } from '../server/auth.mjs';

setPasswordHashCost(16);

const ROUTES = [
  // [method, path, visitor, observer, editor, admin]
  ['GET', '/search?q=B5B', 401, 200, 200, 200],
  ['GET', '/search/rows?module=Insectary_data&from=x&to=4', 401, 400, 400, 400],
  ['GET', '/table?module=Collection_data', 401, 200, 200, 200],
  // No sheet is open to visitors under another name.
  ['GET', '/public/table?module=Collection_data', 401],
  ['GET', '/monitoring/wikiloc-data', 401, 200, 200, 200],
  ['GET', '/monitoring/export/rows.csv', 401, 200, 200, 200],
  ['GET', '/monitoring/export/walks.gpx?walk=nope', 401, 404, 404, 404],
  // At most 500 rows: the count is checked once the role is.
  ['POST', '/sheets/Insectary_data/extend', 401, 403, 403, 400, { count: 501 }],
  ['GET', '/review?kind=envelope_sex', 401, 403, 200, 200],
  ['GET', '/photo/nope?w=400', 401],
  ['GET', '/chat/proposals', 401],
  ['GET', '/t3/status', 401],
];

test('the routes a visitor and each role may call', async () => {
  const sheets = new LocalSheets({
    Insectary_data: [{ row: 2, values: { Insectary_ID: 'B5B', SPECIES: 'Oleria onega' } }],
    Collection_data: [{ row: 2, values: { CAM_ID: 'CAM000001', Collection_location: 'Ikiam', SPECIES: 'Oleria onega' } }],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data', 'Collection_data'] });
  const app = await createApp(
    { localMode: true, secureCookies: false, setupToken: 'test-setup-secret', syncIntervalMs: 0 },
    { store, skipInitialSync: true },
  );
  const address = await app.listen(0, '127.0.0.1');
  const api = `http://127.0.0.1:${address.port}/ithomiini/api`;
  const call = async (method, path, { body, session } = {}) => {
    const response = await fetch(api + path, {
      method,
      headers: { 'content-type': 'application/json', ...(session ? { cookie: session.cookie, 'x-csrf-token': session.csrf } : {}) },
      body: body && JSON.stringify({ requestId: randomUUID(), ...body }),
    });
    const type = response.headers.get('content-type') ?? '';
    return {
      status: response.status,
      body: type.startsWith('application/json') ? await response.json() : await response.text(),
      session: { cookie: response.headers.get('set-cookie')?.split(';')[0], csrf: null },
    };
  };
  const signedIn = answer => ({ cookie: answer.session.cookie, csrf: answer.body.csrf });
  try {
    const admin = signedIn(
      await call('POST', '/auth/setup', { body: { token: 'test-setup-secret', username: 'boss', password: 'secret1', displayName: 'Boss' } }),
    );
    const people = { admin };
    for (const role of ['observer', 'editor']) {
      const made = await call('POST', '/admin/users', { session: admin, body: { username: role, password: 'secret1', role, displayName: role } });
      assert.equal(made.status, 201, JSON.stringify(made.body));
      people[role] = signedIn(await call('POST', '/auth/login', { body: { username: role, password: 'secret1' } }));
    }

    const wrong = [];
    for (const [method, path, visitor, observer, editor, adminStatus, body] of ROUTES) {
      const expected = { visitor, observer, editor, admin: adminStatus };
      for (const [who, status] of Object.entries(expected)) {
        if (status === undefined) continue;
        const answer = await call(method, path, { body, session: people[who] });
        if (answer.status !== status) wrong.push(`${who} ${method} ${path}: ${answer.status}, not ${status}`);
      }
    }
    assert.deepEqual(wrong, []);

    // The home page is open: natural history for visitors, the team's counts only with a session.
    const visitor = await call('GET', '/summary');
    assert.equal(visitor.status, 200);
    assert.equal(visitor.body.team, null);
    assert.ok(visitor.body.nature);
    assert.ok((await call('GET', '/summary', { session: people.observer })).body.team);
  } finally {
    await app.close();
  }
});
