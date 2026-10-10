import test, { after } from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { applyBatch } from '../server/batch.mjs';
import { createApp } from '../server/index.mjs';
import { setPasswordHashCost } from '../server/auth.mjs';

setPasswordHashCost(16);

const user = { id: 'editor-1', username: 'editor', role: 'editor' };
const formulaCell = (formula, value) => ({
  userEnteredValue: { formulaValue: formula },
  effectiveValue: { stringValue: value },
});

function seed() {
  return {
    Insectary_data: [
      {
        row: 2,
        values: {
          Insectary_ID: 'A0A',
          SPECIES: 'Melinaea menophilus',
          Sex: 'female',
          'CLUTCH NUMBER': '944',
          Tube_1_id: 'FS00000010',
        },
      },
      { row: 3, values: { Insectary_ID: 'A1A', SPECIES: 'Melinaea menophilus', Sex: 'male', 'CLUTCH NUMBER': '944' } },
      // Unused rows with pre-filled formula IDs, as in the real workbook.
      { row: 4, cells: [formulaCell('="A2A"', 'A2A')] },
      { row: 5, cells: [formulaCell('="A3A"', 'A3A')] },
    ],
    Collection_data: [{ row: 2, values: { CAM_ID: 'CAM000001', SPECIES: 'Species', Tube_1_id: 'FS00000020' } }],
    Stocks_Matings: [
      { row: 2, values: { male_ID: 'M1', female_ID: 'F1' } },
      { row: 3, values: { male_ID: 'M2', female_ID: 'F2' } },
    ],
  };
}

async function fixture() {
  const sheets = new LocalSheets(seed());
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data', 'Collection_data', 'Stocks_Matings'] });
  const calls = { read: 0, write: 0 };
  const readRows = sheets.readRows.bind(sheets);
  const writeBatch = sheets.writeBatch.bind(sheets);
  sheets.readRows = async targets => (calls.read++, readRows(targets));
  sheets.writeBatch = async writes => (calls.write++, writeBatch(writes));
  const at = (sheet, row) => store.getRecordBySheetRow(sheet, row);
  return { store, sheets, calls, at };
}

test('a multi-row save is one action and three Google requests', async () => {
  const { store, calls, at } = await fixture();
  const a = at('Insectary_data', 2),
    b = at('Insectary_data', 3);
  const result = await applyBatch(
    store,
    {
      requestId: randomUUID(),
      edits: [
        { id: a.id, values: { Death_date: '14-Aug-25', Death_cause: 'Natural' }, expected: { Death_date: null } },
        { id: b.id, values: { Death_date: '2025-08-15' } },
      ],
    },
    user,
  );
  assert.equal(result.status, 'verified');
  assert.equal(result.actions.length, 1);
  assert.equal(result.action.changes.length, 3);
  assert.deepEqual(calls, { read: 2, write: 1 });
  assert.equal(at('Insectary_data', 2).values.Death_date, 45883);
  assert.equal(at('Insectary_data', 3).values.Death_date, 45884);
  store.close();
});

test('a cell changed by someone else rejects the whole save and writes nothing', async () => {
  const { store, sheets, at } = await fixture();
  const a = at('Insectary_data', 2),
    b = at('Insectary_data', 3);
  await sheets.externalEdit('Insectary_data', 3, { Sex: 'female' });
  await assert.rejects(
    applyBatch(
      store,
      {
        requestId: randomUUID(),
        edits: [
          { id: a.id, values: { Sex: 'male' }, expected: { Sex: 'female' } },
          { id: b.id, values: { Sex: 'NA' }, expected: { Sex: 'male' } },
        ],
      },
      user,
    ),
    e =>
      e.code === 'BATCH_CONFLICT' &&
      e.details.items.some(
        i =>
          i.code === 'EXTERNAL_CONFLICT' &&
          i.field === 'Sex' &&
          // The message with its descriptor, for the interface language (server/messages.mjs).
          i.message &&
          i.messageMsg?.vars?.field === 'Sex',
      ),
  );
  assert.equal((await sheets.readRow('Insectary_data', 2)).cells[5].userEnteredValue.stringValue, 'female');
  store.close();
});

test('a header written twice blocks writes and sync for that sheet', async () => {
  const { store, sheets, at } = await fixture();
  const header = sheets.rows.get('Insectary_data').find(r => r.row === 1);
  header.cells[1] = { userEnteredValue: { stringValue: 'Sex' } };
  await assert.rejects(
    applyBatch(
      store,
      { requestId: randomUUID(), edits: [{ id: at('Insectary_data', 2).id, values: { Sex: 'male' } }] },
      user,
    ),
    e => e.details.items[0].code === 'HEADER_MISMATCH',
  );
  const status = await store.sync({ sheets: ['Insectary_data'] });
  assert.equal(status.skipped, 1);
  assert.ok(status.headerProblems.Insectary_data.length);
  store.close();
});

test('editing a row that moved before the next sync does not crash and finds the row', async () => {
  const { store, sheets, at } = await fixture();
  const record = at('Insectary_data', 2);
  // Someone inserts a row above A0A in Google Sheets.
  for (const row of sheets.rows.get('Insectary_data')) if (row.row >= 2) row.row++;
  sheets.rows.get('Insectary_data').push({ row: 2, cells: [{ userEnteredValue: { stringValue: 'Z9Z' } }] });
  const result = await store.updateRecord(record.id, { values: { Sex: 'NA' }, requestId: randomUUID() }, user);
  assert.equal(result.status, 'verified');
  assert.equal(store.getRecord(record.id).row, 3);
  store.close();
});

test('a sync that reads during a write does not revert the write', async () => {
  const { store, sheets, at } = await fixture();
  const record = at('Insectary_data', 2);
  let release, entered;
  const gate = new Promise(resolve => (release = resolve));
  const writing = new Promise(resolve => (entered = resolve));
  const write = sheets.writeBatch;
  sheets.writeBatch = async writes => {
    entered();
    await gate;
    return write(writes);
  };
  const saving = store.updateRecord(record.id, { values: { Sex: 'male' }, requestId: randomUUID() }, user);
  await writing;
  const syncing = store.sync({ sheets: ['Insectary_data'] });
  release();
  await saving;
  const status = await syncing;
  assert.equal(status.skipped, 1);
  assert.equal(store.getRecord(record.id).values.Sex, 'male');
  assert.equal(store.getHistory({ source: 'sheet_reconciliation' }).total, 0);
  store.close();
});

test('tube IDs must be unique across both main sheets, including on edits', async () => {
  const { store, at } = await fixture();
  await assert.rejects(
    applyBatch(
      store,
      { requestId: randomUUID(), edits: [{ id: at('Insectary_data', 3).id, values: { Tube_1_id: 'FS00000020' } }] },
      user,
    ),
    e => e.details.items[0].code === 'DUPLICATE_ID',
  );
  await assert.rejects(
    applyBatch(
      store,
      {
        requestId: randomUUID(),
        edits: [
          { id: at('Insectary_data', 3).id, values: { Tube_1_id: 'FS00000030' } },
          { id: at('Collection_data', 2).id, values: { Tube_2_id: 'FS00000030' } },
        ],
      },
      user,
    ),
    e => e.details.items.some(i => i.code === 'DUPLICATE_ID'),
  );
  // Moving an ID from one row to another in the same save is allowed.
  const moved = await applyBatch(
    store,
    {
      requestId: randomUUID(),
      edits: [
        { id: at('Insectary_data', 2).id, values: { Tube_1_id: 'NA' } },
        { id: at('Insectary_data', 3).id, values: { Tube_1_id: 'FS00000010' } },
      ],
    },
    user,
  );
  assert.equal(moved.status, 'verified');
  store.close();
});

test('new rows fill pre-filled rows by ID or the next free rows, keeping formulas', async () => {
  const { store, at } = await fixture();
  const result = await applyBatch(
    store,
    {
      requestId: randomUUID(),
      creates: [
        {
          clientId: 'x',
          module: 'Insectary_data',
          values: { Insectary_ID: 'A3A', Sex: 'male', 'CLUTCH NUMBER': '944' },
        },
        { clientId: 'y', module: 'Insectary_data', values: { Sex: 'female', 'CLUTCH NUMBER': '944' } },
      ],
    },
    user,
  );
  assert.equal(result.records.length, 2);
  assert.equal(at('Insectary_data', 5).values.Sex, 'male');
  assert.equal(at('Insectary_data', 5).formulas.Insectary_ID, '="A3A"');
  assert.equal(at('Insectary_data', 4).values.Sex, 'female');
  assert.equal(at('Insectary_data', 4).values.Insectary_ID, 'A2A');
  assert.deepEqual(result.created.map(c => c.clientId).sort(), ['x', 'y']);
  await assert.rejects(
    applyBatch(
      store,
      {
        requestId: randomUUID(),
        creates: [{ module: 'Insectary_data', values: { Insectary_ID: 'A0A', Sex: 'male' } }],
      },
      user,
    ),
    e => e.details.items[0].code === 'DUPLICATE_ID',
  );
  store.close();
});

test('rows without identifier columns keep their identity when a row is inserted above', async () => {
  const { store, sheets, at } = await fixture();
  const first = at('Stocks_Matings', 2).id,
    second = at('Stocks_Matings', 3).id;
  for (const row of sheets.rows.get('Stocks_Matings')) if (row.row >= 2) row.row++;
  sheets.rows.get('Stocks_Matings').push({ row: 2, cells: [] });
  await sheets.externalEdit('Stocks_Matings', 2, { male_ID: 'M0', female_ID: 'F0' });
  // Google returns rows top to bottom.
  sheets.rows.get('Stocks_Matings').sort((a, b) => a.row - b.row);
  const status = await store.sync({ sheets: ['Stocks_Matings'] });
  assert.equal(status.changed, 0);
  assert.equal(store.getRecord(first).row, 3);
  assert.equal(store.getRecord(second).row, 4);
  store.close();
});

test('an edit of an unused row and a new row in one save never share a row', async () => {
  const { store, at } = await fixture();
  const unused = at('Insectary_data', 4);
  const result = await applyBatch(
    store,
    {
      requestId: randomUUID(),
      edits: [{ id: unused.id, values: { Sex: 'female' } }],
      creates: [{ module: 'Insectary_data', values: { Sex: 'male', 'CLUTCH NUMBER': '945' } }],
    },
    user,
  );
  assert.equal(result.status, 'verified');
  assert.equal(at('Insectary_data', 4).values.Sex, 'female');
  assert.equal(at('Insectary_data', 5).values.Sex, 'male');
  store.close();
});

test('new rows skip a free row where someone typed a value, instead of overwriting it', async () => {
  const { store, sheets, at } = await fixture();
  await sheets.externalEdit('Collection_data', 3, { CAM_ID: 'CAM000077' });
  await applyBatch(
    store,
    {
      requestId: randomUUID(),
      creates: [{ module: 'Collection_data', values: { CAM_ID: 'CAM000078', SPECIES: 'X' } }],
    },
    user,
  );
  assert.equal((await sheets.readRow('Collection_data', 3)).cells[5].userEnteredValue.stringValue, 'CAM000077');
  assert.equal(at('Collection_data', 4).values.CAM_ID, 'CAM000078');
  store.close();
});

test('while a save is unconfirmed, new rows in that sheet wait instead of risking a duplicate', async () => {
  const { store, sheets } = await fixture();
  sheets.failNextWrite = Object.assign(new Error('socket hang up'), { status: undefined });
  const body = { requestId: randomUUID(), creates: [{ module: 'Collection_data', values: { CAM_ID: 'CAM000099' } }] };
  await assert.rejects(applyBatch(store, body, user), e => e.code === 'WRITE_UNCERTAIN');
  await assert.rejects(applyBatch(store, body, user), e => e.code === 'WRITE_UNCERTAIN');
  await assert.rejects(
    applyBatch(store, { requestId: randomUUID(), creates: body.creates }, user),
    e => e.details.items[0].code === 'WRITE_UNCERTAIN',
  );
  // Nothing reached the Sheet, so recovery marks it failed and the same request can run.
  assert.equal((await store.recoverPending()).failed, 1);
  assert.equal((await applyBatch(store, body, user)).status, 'verified');
  store.close();
});

test('repeated syncs after rows are deleted and inserted keep working', async () => {
  const { store, sheets } = await fixture();
  const rows = sheets.rows.get('Stocks_Matings');
  rows.splice(
    rows.findIndex(r => r.row === 2),
    1,
  );
  for (const row of rows) if (row.row > 2) row.row--;
  await store.sync({ sheets: ['Stocks_Matings'] });
  for (const row of rows) if (row.row >= 2) row.row++;
  rows.push({ row: 2, cells: [] });
  await sheets.externalEdit('Stocks_Matings', 2, { male_ID: 'M9', female_ID: 'F9' });
  rows.sort((a, b) => a.row - b.row);
  await store.sync({ sheets: ['Stocks_Matings'] });
  for (const row of rows) if (row.row >= 2) row.row++;
  rows.push({ row: 2, cells: [] });
  await sheets.externalEdit('Stocks_Matings', 2, { male_ID: 'M8', female_ID: 'F8' });
  rows.sort((a, b) => a.row - b.row);
  const status = await store.sync({ sheets: ['Stocks_Matings'] });
  assert.equal(status.state, 'offline_seed');
  store.close();
});

// One app for the HTTP tests, with its administrator (admin / secret1).
let http = null;
const httpApp = () =>
  (http ??= (async () => {
    const app = await createApp(
      { databasePath: ':memory:', localMode: true, secureCookies: false, syncIntervalMs: 0, setupToken: 'setup-token-batch' },
      { seed: seed() },
    );
    await app.ready;
    const address = await app.listen(0);
    const base = `http://127.0.0.1:${address.port}/ithomiini/api`;
    const setup = await fetch(`${base}/auth/setup`, {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({ token: 'setup-token-batch', username: 'admin', displayName: 'Admin', password: 'secret1' }),
    });
    const { csrf } = await setup.json();
    return { app, base, cookie: setup.headers.get('set-cookie').split(';')[0], csrf };
  })());
after(async () => (await http)?.app.close());

test('HTTP: batch, table and ID endpoints; action types cannot pick the history source', async () => {
  let { base, cookie, csrf } = await httpApp();
  const call = async (path, method = 'GET', body, headers = {}) => {
    const response = await fetch(base + path, {
      method,
      headers: { ...(body ? { 'content-type': 'application/json' } : {}), cookie, 'x-csrf-token': csrf, ...headers },
      body: body ? JSON.stringify({ requestId: randomUUID(), ...body }) : undefined,
    });
    const text = await response.text();
    const data = text ? JSON.parse(text) : null;
    if (response.headers.get('set-cookie')) cookie = response.headers.get('set-cookie').split(';')[0];
    if (data?.csrf) csrf = data.csrf;
    return { response, data };
  };

  const table = await call('/table?module=Insectary_data');
  assert.equal(table.response.status, 200);
  const keys = table.data.columns.map(c => c.key);
  const a0a = table.data.rows.find(r => r.v[keys.indexOf('Insectary_ID')] === 'A0A');
  assert.ok(a0a && a0a.observed);
  assert.deepEqual(table.data.rows.find(r => r.row === 4).f, [keys.indexOf('Insectary_ID')]);
  const etag = table.response.headers.get('etag');
  assert.equal(
    (await call('/table?module=Insectary_data', 'GET', undefined, { 'if-none-match': etag })).response.status,
    304,
  );

  const saved = await call('/records/batch', 'POST', {
    edits: [{ id: a0a.id, values: { Sex: 'NA' }, expected: { Sex: 'female' } }],
  });
  assert.equal(saved.data.status, 'verified');
  assert.notEqual(
    (await call('/table?module=Insectary_data', 'GET', undefined, { 'if-none-match': etag })).response.status,
    304,
  );

  const ids = await call('/ids?kind=insectary&count=5');
  assert.deepEqual(ids.data.sequence, ['A2A', 'A3A']);
  const tubes = await call('/ids?kind=tube&start=FS00000009&count=3');
  assert.deepEqual(tubes.data.sequence, ['FS00000009', 'FS00000011', 'FS00000012']);

  const spoof = await call('/actions', 'POST', {
    type: 'undo',
    recordId: a0a.id,
    values: { Notes_Insectary_data: { formula: '=1' } },
  });
  assert.equal(spoof.response.status, 400);
  const unknown = await call('/actions', 'POST', { type: 'edit', recordId: a0a.id, values: { Sexx: 'male' } });
  assert.equal(unknown.response.status, 409);
  assert.equal(unknown.data.error.details.items[0].code, 'INVALID_FIELD');
  assert.equal((await call('/records?module=Insectary_data&filters=not-json')).response.status, 400);
});

test('HTTP: failed sign-ins lock one account, not everyone behind the proxy', async () => {
  const { base } = await httpApp();
  const post = (path, body, ip) =>
    fetch(base + path, {
      method: 'POST',
      headers: { 'content-type': 'application/json', 'x-forwarded-for': ip },
      body: JSON.stringify(body),
    });
  for (let i = 0; i < 8; i++) await post('/auth/login', { username: 'someone', password: 'wrong-pass' }, '2.2.2.2');
  assert.equal((await post('/auth/login', { username: 'someone', password: 'wrong-pass' }, '2.2.2.2')).status, 429);
  assert.equal((await post('/auth/login', { username: 'admin', password: 'secret1' }, '3.3.3.3')).status, 200);
});
