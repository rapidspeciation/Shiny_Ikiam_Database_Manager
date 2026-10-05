import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { KINDS, NOT_PRESERVED, columnsOf as columnsOfKind, reviewColumns } from '../server/notebook.mjs';
import { DEFAULT_COLUMNS, isHiddenColumn, isNotWritten } from '../server/proposal-columns.mjs';
import { moduleMap } from '../server/schema.mjs';

// The review table shows each row whole enough to spot a wrong one, whatever the proposal changes:
// a proposal setting only Sex still shows the species, CAM and tubes beside it.

const columnsOf = (sheet, kind = null) => {
  const mod = moduleMap.get(sheet);
  return reviewColumns(sheet, mod.fields.map(f => f.key), mod.identityFields, kind);
};

test('Insectary_data: its default columns (server/proposal-columns.mjs), whatever the notebook; Preservation_medium and the photos never', () => {
  const sheet = moduleMap.get('Insectary_data').fields.map(f => f.key);
  const { fields, keys } = columnsOf('Insectary_data');
  assert.deepEqual(fields, DEFAULT_COLUMNS.Insectary_data);
  assert.equal(fields[0], 'Insectary_ID');
  assert.equal(fields.at(-1), 'Notes_Insectary_data');
  for (const f of ['Preservation_medium', 'Photo_dorsal', 'Photo_ventral']) assert.ok(!fields.includes(f), f);
  // Every default column is one of the sheet's, in the sheet's own order.
  assert.deepEqual(fields, sheet.filter(f => fields.includes(f)));
  assert.deepEqual(keys, ['Insectary_ID']);
  for (const kind of ['deaths', 'emergence', 'labels']) assert.deepEqual(columnsOf('Insectary_data', kind), { fields, keys });
  assert.ok(isHiddenColumn('Insectary_data', 'Preservation_medium'));
  for (const f of ['Preservation_medium', 'Photo_dorsal', 'Photo_ventral']) assert.ok(isNotWritten('Insectary_data', f), f);
  assert.ok(!isNotWritten('Collection_data', 'Preservation_medium'), 'only Insectary_data');
  // The notebooks' templates no longer imply it.
  assert.ok(!('Preservation_medium' in NOT_PRESERVED));
  for (const kind of Object.values(KINDS)) assert.ok(!columnsOfKind(kind).includes('Preservation_medium'), kind.label);
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
    assert.deepEqual(pending.shownColumns.Insectary_data, { ...columnsOf('Insectary_data'), hidden: ['Preservation_medium'] });
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
    const shownOf = async id => {
      const { hidden, ...shown } = (
        await assistant.handle({ method: 'GET', path: '/api/chat/proposals', body: {}, user, query: { all: '1' } })
      ).body.proposals.find(p => p.id === id).shownColumns.Insectary_data;
      assert.deepEqual(hidden, ['Preservation_medium']);
      return shown;
    };
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
    // Deprecated: never shown, never written; the photos are left to their formula.
    assert.match((await call('propose_changes', { reason: 'x', changes, view: { columns: ['Preservation_medium'] } })).error, /deprecated/);
    const medium = await call('propose_changes', { reason: 'x', changes: [{ recordId: at(2), values: { Preservation_medium: 'NOT_COLLECTED' } }] });
    assert.match(medium.error, /Preservation_medium is deprecated in Insectary_data and is not written/);
    const photo = await call('propose_changes', { reason: 'x', changes: [{ recordId: at(2), values: { Photo_dorsal: 'x' } }] });
    assert.match(photo.error, /Photo_dorsal is left to its formula/);
    const bulk = await call('propose_changes', { reason: 'x', bulk: { sheet: 'Insectary_data', recordIds: [at(2)], set: { Preservation_medium: 'NA' } } });
    assert.match(bulk.error, /deprecated/);
  } finally {
    store.close?.();
  }
});
