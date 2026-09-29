import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash, randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createAssistant } from '../server/assistant.mjs';

// A proposal revised in place: by the assistant (update_proposal) and by the person in the table.

const EPOCH = Date.UTC(1899, 11, 30);
const serial = iso => Math.round((Date.parse(`${iso}T00:00:00Z`) - EPOCH) / 864e5);
const FCH = 'FCH - Franz Chandi';
const base = { Purpose: 'Monitoring', Collection_location: 'Ikiam', Collector: FCH };

async function fixture() {
  const sheets = new LocalSheets({
    Collection_data: [
      {
        row: 2,
        values: { ...base, Release_Collect: 'Collected_Preserved', SPECIES: 'Oleria gunilla', Sex: 'male', CAM_ID: 'CAM000001' },
      },
      { row: 3, values: { ...base, Release_Collect: 'Mark_Released', SPECIES: 'Oleria gunilla', FieldMark_ID: 'B40', Sex: 'female' } },
      // The next pre-made row, whose Tribe is a formula.
      { row: 4, values: { Tribe: { formula: '=VLOOKUP(S4,Taxonomy_v18Jun25!J:G,1,0)' } } },
    ],
    Taxonomy_v18Jun25: [
      { row: 2, values: { species: 'Oleria gunilla', tribe: 'Ithomiini' } },
      { row: 3, values: { species: 'Hypothyris anastasia', tribe: 'Ithomiini' } },
    ],
    Location_data: [{ row: 2, values: { Collection_location: 'Ikiam' } }],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Collection_data', 'Taxonomy_v18Jun25', 'Location_data'] });
  const assistant = createAssistant({ store, config: { claude: {} } });
  store.db
    .prepare(
      "INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u1','franz','Franz','editor','s','h',1,'2026-01-01')",
    )
    .run();
  store.db
    .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
    .run(createHash('sha256').update('franz-token').digest('hex'), 'u1');
  const call = async (name, args) => {
    const out = await assistant.mcp(
      { authorization: 'Bearer franz-token' },
      { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } },
    );
    return JSON.parse(out.body.result.content[0].text);
  };
  const user = { id: 'u1', username: 'franz', displayName: 'Franz', role: 'editor' };
  const http = (method, path, body = {}, query = {}) => assistant.handle({ method, path, body, user, query });
  const list = async () => (await http('GET', '/api/chat/proposals', {}, { all: '1' })).body;
  const record = row => store.getRecordBySheetRow('Collection_data', row);
  return { store, assistant, call, http, list, record, user };
}

const newRow = (values, note = 'M1') => ({
  sheet: 'Collection_data',
  values: { ...base, Release_Collect: 'Mark_Released', Collection_date: '2026-09-26', ...values },
  note,
});

test('update_proposal revises the same proposal: cells, rows added and removed, a revision per change', async () => {
  const { store, assistant, call, list, user } = await fixture();
  try {
    const tools = (await assistant.mcp({ authorization: 'Bearer franz-token' }, { jsonrpc: '2.0', id: 1, method: 'tools/list' }))
      .body.result.tools;
    assert.ok(tools.some(t => t.name === 'update_proposal') && tools.some(t => t.name === 'get_proposal'));

    const proposed = await call('propose_changes', {
      reason: 'Recorrido del 26 sep',
      newRows: [newRow({ SPECIES: 'Oleria gunilla', Sex: 'female', FieldMark_ID: 'B41' }), newRow({ SPECIES: 'Oleria gunilla', Sex: 'male' }, 'M2')],
      changes: [{ recordId: record2(store).id, values: { Sex: 'female' }, note: 'foto' }],
    });
    assert.equal(proposed.rows, 3);
    // The rows' indexes, for update_proposal: the new rows first.
    assert.deepEqual(proposed.table.map(r => [r.index, !!r.create]), [[0, true], [1, true], [2, false]]);
    let shown = (await list()).proposals[0];
    assert.equal(shown.revision, 1);
    const keys = shown.changes.map(c => c.key);

    // The Asistente tab is waiting: an update to the same proposal wakes it.
    const waiting = assistant.handle({
      method: 'GET',
      path: '/api/chat/proposals',
      user,
      query: { all: '1', wait: '1', revision: (await list()).revision },
    });
    const updated = await call('update_proposal', {
      proposalId: proposed.proposalId,
      rows: [{ index: 0, values: { SPECIES: 'Hypothyris anastasia', Collection_time: '9:05' }, note: 'M1 corregido' }],
      removeRows: [1],
      newRows: [newRow({ SPECIES: 'Oleria gunilla', Sex: 'female', Release_Collect: 'Collected_Preserved', CAM_ID: 'CAM000002' }, 'M3')],
    });
    assert.equal(updated.revision, 2, JSON.stringify(updated));
    const live = (await waiting).body;
    shown = live.proposals[0];
    assert.equal(shown.id, proposed.proposalId, 'the same proposal, not a new one');
    assert.equal(live.proposals.length, 1);
    assert.equal(shown.revision, 2);
    assert.equal(shown.lastBy, 'ai');
    assert.deepEqual(
      shown.changes.map(c => c.note),
      ['M1 corregido', 'foto', 'M3'],
    );
    // Rows keep their key; the removed one is gone and the new one is last.
    assert.deepEqual(shown.changes.slice(0, 2).map(c => c.key), [keys[0], keys[2]]);
    assert.equal(shown.changes[0].values.SPECIES, 'Hypothyris anastasia');
    assert.equal(shown.changes[0].values.Collection_time, (9 * 60 + 5) / 1440);
    assert.deepEqual(updated.rows.map(r => r.index), [0, 1, 2]);
    assert.equal(updated.rows[0].values.Collection_date, '2026-09-26');
    // The new rows' formula columns and the edited row's are known to the table.
    assert.deepEqual(shown.newRowFormulas.Collection_data, ['Tribe']);
    assert.ok(Array.isArray(shown.changes[1].formulas));
    assert.equal(shown.changes[1].rowValues.CAM_ID, 'CAM000001');

    // A value the sheet's list refuses: nothing changes.
    const refused = await call('update_proposal', { proposalId: proposed.proposalId, rows: [{ index: 0, values: { Sex: 'hembra' } }] });
    assert.match(refused.error, /Nothing was changed/);
    assert.match(refused.problems[0].message, /Sex/);
    // An ID already used in the sheet, or twice in the proposal, is refused too.
    const twice = await call('update_proposal', { proposalId: proposed.proposalId, rows: [{ index: 0, values: { CAM_ID: 'CAM000002' } }] });
    assert.match(twice.problems[0].message, /appears twice/);
    const used = await call('update_proposal', { proposalId: proposed.proposalId, rows: [{ index: 0, values: { CAM_ID: 'CAM000001' } }] });
    assert.match(used.problems[0].message, /already used in Collection_data row 2/);
    assert.equal((await list()).proposals[0].revision, 2);

    // A row of the sheet already in it is revised, not added again.
    const merged = await call('update_proposal', {
      proposalId: proposed.proposalId,
      changes: [{ recordId: record2(store).id, values: { CAM_ID: 'CAM000009' } }],
    });
    assert.equal(merged.rows.length, 3);
    assert.deepEqual(merged.rows[1].values, { Sex: 'female', CAM_ID: 'CAM000009' });
  } finally {
    store.close();
  }
});

const record2 = store => store.getRecordBySheetRow('Collection_data', 2);

test('the person edits cells in the table: checked, marked, kept from the assistant unless it is told to', async () => {
  const { store, call, http, list, record } = await fixture();
  try {
    const { proposalId } = await call('propose_changes', {
      reason: 'Recorrido',
      newRows: [newRow({ SPECIES: 'Oleria gunilla', Sex: 'female', FieldMark_ID: 'B41' })],
      changes: [{ recordId: record(2).id, values: { Sex: 'female' } }],
    });
    const [created, edited] = (await list()).proposals[0].changes;
    const edit = cells => http('POST', `/api/chat/proposals/${proposalId}/edit`, { cells });

    // Typed as the grid sends it: a date as a serial; a strict list value outside the list is refused, the rest kept.
    const out = await edit([
      { key: created.key, field: 'SPECIES', value: 'Hypothyris anastasia', before: 'Oleria gunilla' },
      { key: created.key, field: 'Collection_date', value: serial('2026-09-27') },
      { key: created.key, field: 'Sex', value: 'hembra' },
      { key: edited.key, field: 'Flight_height', value: 2 },
    ]);
    assert.equal(out.status, 200, JSON.stringify(out.body));
    assert.equal(out.body.rejected.length, 1);
    assert.equal(out.body.rejected[0].field, 'Sex');
    assert.match(out.body.rejected[0].message, /no está en la lista/);
    let shown = out.body.proposal;
    assert.equal(shown.revision, 2);
    assert.equal(shown.lastBy, 'person');
    assert.equal(shown.changes[0].values.SPECIES, 'Hypothyris anastasia');
    assert.equal(shown.changes[0].values.Sex, 'female');
    assert.equal(shown.changes[0].values.Collection_date, serial('2026-09-27'));
    // What the assistant had proposed is kept with the person's mark.
    assert.equal(shown.changes[0].personEdits.SPECIES.ai, 'Oleria gunilla');
    assert.equal(shown.changes[0].personEdits.SPECIES.by, 'Franz');
    assert.ok(!('Sex' in shown.changes[0].personEdits));
    // A column added to an existing row, and it appears as a column of the table.
    assert.equal(shown.changes[1].values.Flight_height, 2);
    assert.ok(!('ai' in shown.changes[1].personEdits.Flight_height), 'the assistant proposed no change there');
    assert.ok(shown.fields.includes('Flight_height'));

    // The assistant reads the person's edits…
    const read = await call('get_proposal', { proposalId });
    assert.deepEqual(read.rows[0].personEdits.SPECIES, { value: 'Hypothyris anastasia', youProposed: 'Oleria gunilla' });
    assert.deepEqual(read.rows[0].personEdits.Collection_date, { value: '2026-09-27', youProposed: '2026-09-26' });
    assert.equal(read.lastChangedBy, 'person');
    // …and cannot overwrite them without being told: a conflict, the person's value stays.
    const clash = await call('update_proposal', {
      proposalId,
      rows: [{ index: 0, values: { SPECIES: 'Oleria gunilla', Collection_time: '10:00' } }],
    });
    assert.equal(clash.conflicts.length, 1);
    assert.deepEqual(
      { field: clash.conflicts[0].field, person: clash.conflicts[0].person, yours: clash.conflicts[0].yours },
      { field: 'SPECIES', person: 'Hypothyris anastasia', yours: 'Oleria gunilla' },
    );
    assert.equal(clash.rows[0].values.SPECIES, 'Hypothyris anastasia');
    assert.equal(clash.rows[0].values.Collection_time, 10 / 24, 'the other cell is updated');
    // Removing a row the person edited is a conflict too.
    const keep = await call('update_proposal', { proposalId, removeRows: [1] });
    assert.equal(keep.conflicts[0].index, 1);
    assert.equal(keep.rows.length, 2);
    // Told to: the assistant's value replaces the person's, and the mark goes.
    const forced = await call('update_proposal', {
      proposalId,
      rows: [{ index: 0, values: { SPECIES: 'Oleria gunilla' } }],
      overridePersonEdits: true,
    });
    assert.ok(!forced.conflicts);
    assert.equal(forced.rows[0].values.SPECIES, 'Oleria gunilla');
    assert.ok(!('SPECIES' in forced.rows[0].personEdits));

    // The person types over a cell the assistant changed meanwhile: theirs wins, and they are told.
    const over = await edit([{ key: created.key, field: 'Collection_time', value: 0.4, before: 10 / 24 - 0.01 }]);
    assert.equal(over.body.overrode[0].field, 'Collection_time');
    // Setting an existing row's cell back to the sheet's value drops that change.
    const back = await edit([{ key: edited.key, field: 'Sex', value: 'male' }]);
    assert.deepEqual(back.body.proposal.changes[1].values, { Flight_height: 2 });

    // Apply: the rows chosen on an older revision are refused; on the current one, written with the person's values.
    shown = (await list()).proposals[0];
    const stale = await http('POST', `/api/chat/proposals/${proposalId}/apply`, { requestId: randomUUID(), revision: shown.revision - 1 });
    assert.equal(stale.status, 409);
    assert.equal(stale.body.error.code, 'proposal_changed');
    const applied = await http('POST', `/api/chat/proposals/${proposalId}/apply`, { requestId: randomUUID(), revision: shown.revision });
    assert.equal(applied.body.status, 'applied', JSON.stringify(applied.body));
    assert.equal(record(4).values.SPECIES, 'Oleria gunilla');
    assert.equal(record(4).values.Collection_date, serial('2026-09-27'));
    assert.equal(record(4).values.Collection_time, 0.4);
    assert.equal(record(2).values.Flight_height, 2);
    assert.equal(record(2).values.Sex, 'male');
    // Once applied, neither side can change it.
    assert.equal((await edit([{ key: created.key, field: 'Sex', value: 'male' }])).status, 409);
    assert.match((await call('update_proposal', { proposalId, rows: [{ index: 0, values: { Sex: 'male' } }] })).error, /applied/);
  } finally {
    store.close();
  }
});

test('the person adds, empties and removes rows; an empty row is not written', async () => {
  const { store, call, http, list, record } = await fixture();
  try {
    const { proposalId } = await call('propose_changes', {
      reason: 'Recorrido',
      newRows: [newRow({ SPECIES: 'Oleria gunilla', Sex: 'female' }), newRow({ SPECIES: 'Oleria gunilla', Sex: 'male' }, 'M2')],
    });
    const post = body => http('POST', `/api/chat/proposals/${proposalId}/edit`, body);
    const added = await post({ add: [{ sheet: 'Collection_data' }] });
    const rows = added.body.proposal.changes;
    assert.equal(rows.length, 3);
    assert.deepEqual(rows[2].values, {});
    // A formula column of the pre-made row is refused in a new row.
    const formula = await post({ cells: [{ key: rows[2].key, field: 'Tribe', value: 'Ithomiini' }, { key: rows[2].key, field: 'Sex', value: 'male' }] });
    assert.match(formula.body.rejected[0].message, /Tribe/);
    assert.deepEqual(formula.body.proposal.changes[2].values, { Sex: 'male' });
    // Emptying every cell of a row leaves it empty (not refused), and it is skipped when applying.
    const emptied = await post({ cells: Object.keys(rows[0].values).map(field => ({ key: rows[0].key, field, value: '' })) });
    assert.deepEqual(emptied.body.rejected, []);
    assert.deepEqual(emptied.body.proposal.changes[0].values, {});
    const removed = await post({ remove: [rows[1].key] });
    assert.deepEqual(removed.body.proposal.changes.map(c => c.key), [rows[0].key, rows[2].key]);
    const shown = (await list()).proposals[0];
    const applied = await http('POST', `/api/chat/proposals/${proposalId}/apply`, { requestId: randomUUID(), revision: shown.revision });
    assert.equal(applied.body.status, 'applied', JSON.stringify(applied.body));
    assert.deepEqual(applied.body.applied, [1]);
    assert.equal(record(4).values.Sex, 'male');
    // Applied: no more edits.
    assert.equal((await post({ cells: [] })).status, 409);
  } finally {
    store.close();
  }
});
