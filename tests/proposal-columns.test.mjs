import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { KINDS, reviewColumns } from '../server/notebook.mjs';
import { moduleMap } from '../server/schema.mjs';

// The review table shows each row whole enough to spot a wrong one, whatever the proposal changes:
// a proposal setting only Sex still shows the species, CAM and tubes beside it.

const columnsOf = (sheet, kind = null) => {
  const mod = moduleMap.get(sheet);
  return reviewColumns(sheet, mod.fields.map(f => f.key), mod.identityFields, kind);
};

test('Insectary_data: the notebook columns first, then every other column up to Notes_Insectary_data (AF)', () => {
  const sheet = moduleMap.get('Insectary_data').fields.map(f => f.key);
  const notes = sheet.indexOf('Notes_Insectary_data');
  assert.equal(notes, 31, 'column AF');
  const { fields, keys } = columnsOf('Insectary_data');
  assert.deepEqual(fields.slice(0, KINDS.emergence.fields.length), KINDS.emergence.fields);
  assert.deepEqual([...fields].sort(), sheet.slice(0, notes + 1).sort());
  // The rest in the sheet's order.
  const rest = fields.slice(KINDS.emergence.fields.length);
  assert.deepEqual(rest, sheet.slice(0, notes + 1).filter(f => rest.includes(f)));
  assert.deepEqual(keys, ['Insectary_ID']);
  // A page of the deaths notebook: its own columns first, the same set.
  const deaths = columnsOf('Insectary_data', 'deaths');
  assert.deepEqual(deaths.fields.slice(0, KINDS.deaths.fields.length), KINDS.deaths.fields);
  assert.deepEqual([...deaths.fields].sort(), [...fields].sort());
});

test('other sheets: a notebook\'s columns up to its notes, else the identifying ones', () => {
  const stocks = columnsOf('Insectary_stocks');
  assert.deepEqual(stocks.fields.slice(0, KINDS.stocks.fields.length), KINDS.stocks.fields);
  assert.ok(stocks.fields.includes('Generation') && !stocks.fields.includes('HatchingTime'), stocks.fields.join());
  assert.deepEqual(columnsOf('Collection_data'), { fields: ['FieldMark_ID', 'Insectary_ID', 'CAM_ID', 'Tube_1_id', 'SPECIES'], keys: [] });
});

test('a proposal changing one column sends the columns to show, and the sheet values of a reviewed one', async () => {
  const sheets = new LocalSheets({
    Insectary_data: [2, 3].map(row => ({
      row,
      values: { Insectary_ID: `K${row}B`, SPECIES: 'Oleria onega', Sex: 'NA', CAM_ID: `CAM07900${row}`, Tube_1_id: `FS0${row}` },
    })),
  });
  const store = new Store({ localMode: true }, { sheets });
  try {
    await store.sync({ sheets: ['Insectary_data'] });
    const assistant = createAssistant({ store, config: {} });
    store.db
      .prepare(
        "INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u1','franz','Franz','editor','s','h',1,'2026-01-01')",
      )
      .run();
    store.db
      .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
      .run(createHash('sha256').update('franz-token').digest('hex'), 'u1');
    const at = row => store.getRecordBySheetRow('Insectary_data', row).id;
    const out = await assistant.mcp(
      { authorization: 'Bearer franz-token' },
      {
        jsonrpc: '2.0',
        id: 1,
        method: 'tools/call',
        params: {
          name: 'propose_changes',
          arguments: {
            reason: 'Sexo no anotado',
            changes: [2, 3].map(row => ({ recordId: at(row), values: { Sex: 'NOT_COLLECTED' } })),
          },
        },
      },
    );
    assert.ok(!out.body.result.isError, out.body.result.content[0].text);
    const user = { id: 'u1', username: 'franz', displayName: 'Franz', role: 'editor' };
    const list = async () =>
      (await assistant.handle({ method: 'GET', path: '/api/chat/proposals', body: {}, user, query: { all: '1' } })).body.proposals[0];
    const pending = await list();
    assert.deepEqual(pending.fields, ['Sex']);
    assert.deepEqual(pending.shownColumns.Insectary_data, columnsOf('Insectary_data'));
    assert.equal(pending.changes[0].rowValues.CAM_ID, 'CAM079002');
    await assistant.handle({ method: 'POST', path: `/api/chat/proposals/${pending.id}/discard`, body: {}, user, query: {} });
    // Reviewed: the sheet's values of the changed column and of the shown ones that hold something.
    const reviewed = await list();
    assert.equal(reviewed.status, 'discarded');
    assert.deepEqual(reviewed.changes[0].current, {
      Sex: 'NA',
      Insectary_ID: 'K2B',
      SPECIES: 'Oleria onega',
      CAM_ID: 'CAM079002',
      Tube_1_id: 'FS02',
    });
  } finally {
    store.close?.();
  }
});

test("the assistant's view: its columns first, or only them (and the changed ones); a column the sheets lack is said", async () => {
  const sheets = new LocalSheets({
    Insectary_data: [2, 3].map(row => ({
      row,
      values: { Insectary_ID: `K${row}B`, SPECIES: 'Oleria onega', Sex: 'NA', CAM_ID: `CAM07900${row}`, Tube_1_id: `FS0${row}` },
    })),
  });
  const store = new Store({ localMode: true }, { sheets });
  try {
    await store.sync({ sheets: ['Insectary_data'] });
    const assistant = createAssistant({ store, config: {} });
    store.db
      .prepare(
        "INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u1','franz','Franz','editor','s','h',1,'2026-01-01')",
      )
      .run();
    store.db
      .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
      .run(createHash('sha256').update('franz-token').digest('hex'), 'u1');
    const call = async (name, args) =>
      JSON.parse(
        (await assistant.mcp({ authorization: 'Bearer franz-token' }, { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } }))
          .body.result.content[0].text,
      );
    const user = { id: 'u1', username: 'franz', displayName: 'Franz', role: 'editor' };
    const shownOf = async id =>
      (await assistant.handle({ method: 'GET', path: '/api/chat/proposals', body: {}, user, query: { all: '1' } })).body.proposals.find(p => p.id === id)
        .shownColumns.Insectary_data;
    const at = row => store.getRecordBySheetRow('Insectary_data', row).id;
    const changes = [2, 3].map(row => ({ recordId: at(row), values: { Sex: 'NOT_COLLECTED' } }));

    // Named loosely, as people write them; in that order, before the sheet's standard ones.
    const first = await call('propose_changes', { reason: 'Sexo', changes, view: { columns: ['tube 1 id', 'cam id'] } });
    assert.ok(!first.error, first.error);
    const standard = columnsOf('Insectary_data');
    const shown = await shownOf(first.proposalId);
    assert.deepEqual(shown.fields.slice(0, 2), ['Tube_1_id', 'CAM_ID']);
    assert.deepEqual([...shown.fields].sort(), [...standard.fields].sort());
    assert.deepEqual(shown.keys, standard.keys);
    // Only them (the table adds the changed ones after them).
    await call('update_proposal', { proposalId: first.proposalId, view: { onlyColumns: true } });
    assert.deepEqual(await shownOf(first.proposalId), { fields: ['Tube_1_id', 'CAM_ID'], keys: standard.keys });
    // Back to the standard set.
    await call('update_proposal', { proposalId: first.proposalId, view: { columns: null, onlyColumns: null } });
    assert.deepEqual(await shownOf(first.proposalId), standard);

    const wrong = await call('propose_changes', { reason: 'x', changes, view: { columns: ['Sexo'] } });
    assert.match(wrong.error, /^view\.columns: Unknown column Sexo in Insectary_data; did you mean Sex/);
    assert.match((await call('propose_changes', { reason: 'x', changes, view: { between: 'yes' } })).error, /^view\.between: true or false/);
    assert.match((await call('update_proposal', { proposalId: first.proposalId, view: { rows: 3 } })).problems[0], /^view: unknown key rows/);
  } finally {
    store.close?.();
  }
});
