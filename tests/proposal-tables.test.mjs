// Tables of rows the assistant shows beside the chat (show_rows): kept with the
// proposals (listed live, linked to their T3 chat, closed like a proposal is
// discarded), always with the sheet's current values, never applied.
import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { parseDateText } from '../server/schema.mjs';

const user = { id: 'u1', username: 'franz', displayName: 'Franz', role: 'editor' };
const THREAD = '5c8de89d-bbaf-4328-b354-74733e099781';

const dissection = (row, species, father, mother, date, notes = '') => ({
  row,
  values: {
    SPECIES: species,
    Father_CAMid: father,
    Mother_CAMid: mother,
    Dissection_date: parseDateText(date),
    Notes: notes,
  },
});
const SEED = {
  Sperm_dissections: [
    dissection(2, 'Mechanitis polymnia', 'CAM071001', 'CAM071002', '2026-08-01'),
    dissection(3, 'Mechanitis lysimnia', 'CAM071003', 'CAM071004', '2026-08-03', 'two spermatophores'),
    dissection(4, 'Oleria onega', 'CAM071005', 'CAM071006', '2026-08-05'),
    dissection(5, 'Mechanitis polymnia', 'CAM071007', 'CAM071008', '2026-08-09'),
  ],
  Insectary_stocks: [{ row: 2, values: { 'CLUTCH NUMBER': 900, SPECIES: 'Mechanitis lysimnia' } }],
};

async function fixture(seed = SEED, config = {}) {
  const sheets = new LocalSheets(seed);
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: Object.keys(seed) });
  const assistant = createAssistant({ store, config: { proposalWaitMs: 300, ...config } });
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
  const list = async (query = {}) =>
    (
      await assistant.handle({
        method: 'GET',
        path: '/api/chat/proposals',
        body: {},
        user,
        query: { all: '1', chat: 'all', ...query },
      })
    ).body;
  const listed = async id => (await list({ only: id })).proposals[0];
  const post = (id, action, body = {}) =>
    assistant.handle({ method: 'POST', path: `/api/chat/proposals/${id}/${action}`, body, user });
  const typeInSheet = async (sheet, row, changes) => {
    await sheets.externalEdit(sheet, row, changes);
    await store.refreshRows(sheet, [row]);
  };
  const row = n => store.getRecordBySheetRow('Sperm_dissections', n);
  return { sheets, store, assistant, call, list, listed, post, typeInSheet, row, close: () => store.close?.() };
}

test('show_rows: the rows a filter finds, as a read-only table beside the chat, with its links and notes', async () => {
  const f = await fixture(SEED, { publicUrl: 'https://ithomiini-ikiam.com' });
  try {
    const shown = await f.call('show_rows', {
      title: 'Spermatophore dissections: M. polymnia and M. lysimnia',
      sheet: 'Sperm_dissections',
      filters: { SPECIES: ['Mechanitis polymnia', 'Mechanitis lysimnia'] },
      notes: [
        { recordId: f.row(3).id, text: 'the only lysimnia' },
        { recordId: f.row(5).id, field: 'Dissection_date', text: 'eight days after the first', highlight: true },
        { recordId: f.row(4).id, text: 'not in the table' },
        { recordId: f.row(2).id, field: 'Nope', text: 'no such column' },
      ],
    });
    assert.ok(shown.tableId, JSON.stringify(shown));
    assert.equal(shown.link, `https://ithomiini-ikiam.com/#/propuestas/${shown.tableId}`);
    assert.equal(shown.assistantLink, `https://ithomiini-ikiam.com/#/asistente?propuesta=${shown.tableId}`);
    assert.equal(shown.rows, 3);
    // Without columns: the ID columns first, then the filled ones.
    assert.deepEqual(shown.columns.slice(0, 2), ['Father_CAMid', 'Mother_CAMid']);
    assert.ok(shown.columns.includes('SPECIES') && shown.columns.includes('Dissection_date'));
    assert.deepEqual(shown.notesNotShown, [f.row(4).id, `${f.row(2).id} Nope`]);
    // Not a proposal: no review note added by the MCP answer.
    assert.equal(shown.review, undefined);

    const table = await f.listed(shown.tableId);
    assert.equal(table.kind, 'table');
    assert.equal(table.status, 'shown');
    assert.deepEqual(table.sheets, ['Sperm_dissections']);
    assert.deepEqual(table.changes, []);
    assert.equal(table.types.Dissection_date, 'date');
    assert.deepEqual(
      table.rows.map(r => [r.row, r.values.SPECIES]),
      [
        [2, 'Mechanitis polymnia'],
        [3, 'Mechanitis lysimnia'],
        [5, 'Mechanitis polymnia'],
      ],
    );
    assert.equal(table.rows[1].note, 'the only lysimnia');
    assert.deepEqual(table.rows[2].cells, { Dissection_date: 'eight days after the first' });
    assert.deepEqual(table.rows[2].marked, ['Dissection_date']);

    // In the panel's list with the pending proposals, without counting as one to review.
    const all = await f.list();
    assert.deepEqual(
      all.proposals.map(p => p.id),
      [shown.tableId],
    );
    assert.deepEqual(
      all.chats.map(c => c.pending),
      [0],
    );
    // And in list_proposals, as a table.
    const mine = await f.call('list_proposals', {});
    assert.deepEqual(
      mine.proposals.map(p => [p.tableId, p.status, p.rows]),
      [[shown.tableId, 'shown', 3]],
    );
  } finally {
    f.close();
  }
});

test("show_rows: the table shows the sheet as it is now, and its owner's list wakes when one of its rows changes", async () => {
  const f = await fixture();
  try {
    const shown = await f.call('show_rows', {
      title: 'Lysimnia',
      sheet: 'Sperm_dissections',
      recordIds: [f.row(3).id],
      columns: ['Notes', 'SPECIES'],
    });
    const before = await f.list();
    assert.equal(before.proposals[0].rows[0].values.Notes, 'two spermatophores');
    // Typed in Google Sheets meanwhile: the long poll ends, and the table has the new value.
    const waiting = f.list({ wait: '1', revision: before.revision, stamp: before.stamp });
    await f.typeInSheet('Sperm_dissections', 3, { Notes: 'three spermatophores' });
    const after = await waiting;
    assert.notEqual(after.revision, before.revision);
    const now = after.proposals.find(p => p.id === shown.tableId);
    assert.equal(now.rows[0].values.Notes, 'three spermatophores');
    assert.notEqual(now.sheetStamp, before.proposals[0].sheetStamp);
    // The columns as asked, in that order.
    assert.deepEqual(now.fields, ['Notes', 'SPECIES']);
  } finally {
    f.close();
  }
});

test('show_rows: never applied nor edited, closed like a discarded proposal, and the sheet is never written', async () => {
  const f = await fixture();
  try {
    const shown = await f.call('show_rows', {
      title: 'Polymnia',
      sheet: 'Sperm_dissections',
      filters: { SPECIES: 'Mechanitis polymnia' },
    });
    const actions = () => f.store.db.prepare('SELECT count(*) n FROM actions').get().n;
    const writes = actions();
    const applied = await f.post(shown.tableId, 'apply', { requestId: 'apply-table-1' });
    assert.equal(applied.status, 409);
    assert.equal(applied.body.error.code, 'read_only_table');
    const edited = await f.post(shown.tableId, 'edit', { cells: [{ key: f.row(2).id, field: 'Notes', value: 'x' }] });
    assert.equal(edited.status, 409);
    for (const [tool, args] of [
      ['apply_proposal', { proposalId: shown.tableId }],
      ['update_proposal', { proposalId: shown.tableId, rows: [{ index: 0, values: { Notes: 'x' } }] }],
      ['get_proposal', { proposalId: shown.tableId }],
    ]) {
      const out = await f.call(tool, args);
      assert.match(out.error ?? '', /show_rows/, `${tool}: ${JSON.stringify(out)}`);
    }
    assert.equal(actions(), writes, 'nothing was saved');
    assert.equal(f.row(2).values.Notes ?? '', '');

    // Closed from the panel: out of the list, still on its own page.
    const closed = await f.post(shown.tableId, 'discard');
    assert.deepEqual(closed.body, { proposalId: shown.tableId, status: 'closed' });
    assert.deepEqual((await f.list()).proposals, []);
    assert.equal((await f.listed(shown.tableId)).status, 'closed');
    // Shown again by the assistant (tableId): open again, changed in place.
    const again = await f.call('show_rows', { tableId: shown.tableId, columns: ['SPECIES', 'Notes'] });
    assert.equal(again.tableId, shown.tableId);
    assert.equal(again.rows, 2);
    const table = await f.listed(shown.tableId);
    assert.deepEqual(
      [table.status, table.reason, table.fields, table.revision],
      ['shown', 'Polymnia', ['SPECIES', 'Notes'], 2],
    );
  } finally {
    f.close();
  }
});

test('show_rows: at most 500 rows, one sheet, known columns, and rows to show', async () => {
  const many = {
    Insectary_stocks: Array.from({ length: 501 }, (_, i) => ({
      row: i + 2,
      values: { 'CLUTCH NUMBER': 1000 + i, SPECIES: 'Mechanitis polymnia' },
    })),
    Sperm_dissections: SEED.Sperm_dissections,
  };
  const f = await fixture(many);
  try {
    const big = await f.call('show_rows', {
      title: 'All',
      sheet: 'Insectary_stocks',
      filters: { SPECIES: 'Mechanitis polymnia' },
    });
    assert.match(big.error, /501 rows match; a table holds at most 500/);
    const narrowed = await f.call('show_rows', {
      title: 'Some',
      sheet: 'Insectary_stocks',
      filters: { SPECIES: 'Mechanitis polymnia', 'CLUTCH NUMBER': { from: 1000, to: 1499 } },
    });
    assert.equal(narrowed.rows, 500);
    // The ID column first.
    assert.equal(narrowed.columns[0], 'CLUTCH NUMBER');

    const other = await f.call('show_rows', { title: 'x', sheet: 'Insectary_stocks', recordIds: [f.row(2).id] });
    assert.match(other.error, /is in Sperm_dissections, not Insectary_stocks/);
    const columns = await f.call('show_rows', {
      title: 'x',
      sheet: 'Sperm_dissections',
      recordIds: [f.row(2).id],
      columns: ['Nope'],
    });
    assert.match(columns.error, /Unknown column Nope in Sperm_dissections; did you mean Notes\?/);
    assert.match((await f.call('show_rows', { title: 'x', sheet: 'Sperm_dissections' })).error, /Give the rows/);
    assert.match((await f.call('show_rows', { sheet: 'Sperm_dissections', recordIds: [f.row(2).id] })).error, /title/);
    const none = await f.call('show_rows', {
      title: 'x',
      sheet: 'Sperm_dissections',
      field: 'Father_CAMid',
      values: ['CAM999999'],
    });
    assert.match(none.error, /No rows found \(not found: CAM999999\)/);
    // A row not found among others: shown without it, and said.
    const some = await f.call('show_rows', {
      title: 'x',
      sheet: 'Sperm_dissections',
      field: 'Father_CAMid',
      values: ['CAM071001', 'CAM999999'],
    });
    assert.deepEqual([some.rows, some.notFound], [1, ['CAM999999']]);
  } finally {
    f.close();
  }
});

test('show_rows from a T3 chat: linked to that chat, listed with its proposals', async () => {
  const t3Chats = {
    available: true,
    threads: ids => new Map(ids.map(id => [id, { title: 'Disecciones' }])),
    threadOfToolUse: () => THREAD,
    onlyRunning: () => THREAD,
    open: () => null,
    chatsOf: () => [],
    findProposals: async () => new Map(),
  };
  const f = await fixture(SEED, { t3Chats, publicUrl: 'https://ithomiini-ikiam.com' });
  try {
    const shown = await f.call('show_rows', {
      title: 'Polymnia',
      sheet: 'Sperm_dissections',
      filters: { SPECIES: 'Mechanitis polymnia' },
    });
    assert.equal(
      shown.assistantLink,
      `https://ithomiini-ikiam.com/#/asistente?propuesta=${shown.tableId}&chat=${THREAD}`,
    );
    const chat = await f.list({ chat: THREAD });
    assert.deepEqual(
      chat.proposals.map(p => [p.id, p.chat, p.source]),
      [[shown.tableId, THREAD, 'Disecciones']],
    );
    const listed = await f.call('list_proposals', {});
    assert.equal(listed.chat, THREAD);
    assert.deepEqual(
      listed.proposals.map(p => p.tableId),
      [shown.tableId],
    );
  } finally {
    f.close();
  }
});
