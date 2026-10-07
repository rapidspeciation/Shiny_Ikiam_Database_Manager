// Columns the app's Google account cannot edit (Data_entry_order, protected by the sheet's
// owner): a new row leaves them to the sheet, and the proposal's table says so instead of
// what their formula would give. A write Google refuses keeps the proposal pending with why
// (lastError); the same rows (or new rows with the same ID or clutch number) in someone
// else's pending proposal are said in the answer and above the table.
import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { moduleMap } from '../server/schema.mjs';
import { createAssistant } from '../server/assistant.mjs';

const EPOCH = Date.UTC(1899, 11, 30);
const today = Math.round(
  (Date.parse(`${new Intl.DateTimeFormat('en-CA', { timeZone: 'America/Guayaquil' }).format(new Date())}T00:00:00Z`) - EPOCH) / 864e5,
);
const col = (sheet, key) => moduleMap.get(sheet).fields.find(f => f.key === key).column;
const SENT = 'Collected_Sent2Insectary';
const order = r => `=IF(B${r}="","",A${r - 1}+1)`;
const lookup = (r, letter) => `=XLOOKUP(D${r}, Insectary_data!A:A, Insectary_data!${letter}:${letter},"")`;
const id = i => `${String.fromCharCode(65 + Math.floor(i / 10))}${i % 10}T`;

/**
 * Collection_data: 30 wild butterflies sent to the insectary with Data_entry_order and the
 * Death_date and Preservation_date lookups (rows 2–31), then pre-made rows 32–35 holding the
 * Data_entry_order formula only. Column A (Data_entry_order) and Death_date are protected:
 * only the sheet's owner edits them. Insectary_stocks: clutches 990 and 991.
 */
async function fixture() {
  const cells = (values, formulas = {}) => {
    const out = [];
    for (const [key, value] of Object.entries(values))
      out[col('Collection_data', key)] = { userEnteredValue: typeof value === 'number' ? { numberValue: value } : { stringValue: value } };
    for (const [key, formula] of Object.entries(formulas)) out[col('Collection_data', key)] = { userEnteredValue: { formulaValue: formula } };
    return out;
  };
  const collection = [];
  for (let i = 0; i < 30; i++) {
    const r = 2 + i;
    collection.push({
      row: r,
      cells: cells(
        { Release_Collect: SENT, Insectary_ID: id(i), SPECIES: 'Oleria onega', Sex: 'male', Collection_date: today - 40 + i, ...(r === 2 ? { Data_entry_order: 1 } : {}) },
        { ...(r === 2 ? {} : { Data_entry_order: order(r) }), Death_date: lookup(r, 'I'), Preservation_date: lookup(r, 'M') },
      ),
    });
  }
  for (const r of [32, 33, 34, 35]) collection.push({ row: r, cells: cells({}, { Data_entry_order: order(r) }) });
  const death = col('Collection_data', 'Death_date');
  const sheets = new LocalSheets(
    {
      Collection_data: collection,
      Insectary_data: [{ row: 2, values: { Insectary_ID: 'A0T', Wild_Reared: 'Wild-caught', SPECIES: 'Oleria onega' } }],
      Insectary_stocks: [
        { row: 2, values: { 'CLUTCH NUMBER': 990, SPECIES: 'Oleria onega', 'DATE LAID': today - 20, 'NUMBER OF EGGS': 20 } },
        { row: 3, values: { 'CLUTCH NUMBER': 991, SPECIES: 'Oleria onega', 'DATE LAID': today - 19, 'NUMBER OF EGGS': 12 } },
      ],
    },
    {
      // A formula of the protected column gives an error with the new row's values (as Angel's table showed).
      evaluate: (formula, { column }) => (column === 0 ? '#VALUE!' : undefined),
      protectedRanges: {
        Collection_data: [
          { startRowIndex: 1, startColumnIndex: 0, endColumnIndex: 1 },
          { startRowIndex: 1, startColumnIndex: death, endColumnIndex: death + 1 },
        ],
      },
    },
  );
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Collection_data', 'Insectary_data', 'Insectary_stocks'] });
  const assistant = createAssistant({ store, config: {} });
  const people = [
    ['u-ana', 'ana', 'Ana'],
    ['u-franz', 'franz', 'Franz'],
  ];
  for (const [uid, username, name] of people) {
    store.db
      .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES(?,?,?,'editor','s','h',1,'2026-01-01')")
      .run(uid, username, name);
    store.db
      .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
      .run(createHash('sha256').update(`${username}-token`).digest('hex'), uid);
  }
  const callAs = username => async (name, args) =>
    JSON.parse(
      (
        await assistant.mcp(
          { authorization: `Bearer ${username}-token` },
          { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } },
        )
      ).body.result.content[0].text,
    );
  const ana = { id: 'u-ana', username: 'ana', displayName: 'Ana', role: 'editor' };
  const listed = async id =>
    (await assistant.handle({ method: 'GET', path: '/api/chat/proposals', user: ana, query: { all: '1', only: id } })).body.proposals[0];
  const cell = (row, key) => sheets.cell('Collection_data', row, col('Collection_data', key))?.userEnteredValue;
  return { store, sheets, call: callAs('ana'), franz: callAs('franz'), listed, cell };
}

const wild = (insectaryId, extra = {}) => ({
  sheet: 'Collection_data',
  values: { Release_Collect: SENT, Insectary_ID: insectaryId, SPECIES: 'Oleria onega', Sex: 'female', ...extra },
});

test("a wild-caught butterfly's new Collection_data row applies and leaves the protected columns to the sheet", async () => {
  const { store, call, listed, cell } = await fixture();
  try {
    const proposed = await call('propose_changes', { reason: 'Silvestres', newRows: [wild('Y5T', { Death_date: 'NA' })] });
    assert.ok(proposed.proposalId, JSON.stringify(proposed));
    // The table: the protected column is said as such, never as what its formula would give (#VALUE!).
    const view = await listed(proposed.proposalId);
    const row = view.changes.find(c => c.create);
    assert.ok(row.protectedCells?.includes('Data_entry_order'), JSON.stringify(row));
    assert.equal(row.formulaGives?.Data_entry_order, undefined);
    const applied = await call('apply_proposal', { proposalId: proposed.proposalId });
    assert.equal(applied.status, 'applied', JSON.stringify(applied));
    // Its pre-made row: the protected cells as the sheet had them, the column's other formulas written.
    assert.equal(cell(32, 'Insectary_ID')?.stringValue, 'Y5T');
    assert.equal(cell(32, 'Data_entry_order')?.formulaValue, order(32));
    assert.equal(cell(32, 'Death_date'), undefined, 'no formula nor NA in a protected column');
    assert.equal(cell(32, 'Preservation_date')?.formulaValue, lookup(32, 'M'));
  } finally {
    store.close();
  }
});

test('Google refusing a protected cell keeps the proposal pending with why; a needs_review one says its status', async () => {
  const { store, call, listed, cell } = await fixture();
  try {
    // A date typed in the protected column: the app writes it, Google refuses the whole save.
    const proposed = await call('propose_changes', { reason: 'Silvestres', newRows: [wild('Y6T', { Death_date: '2026-10-01' })] });
    const refused = await call('apply_proposal', { proposalId: proposed.proposalId });
    assert.match(refused.error, /Google rechazó la escritura: celda protegida en Death_date/, JSON.stringify(refused));
    assert.equal(refused.proposalStatus, 'pending');
    assert.equal(cell(32, 'Insectary_ID'), undefined, 'nothing was written');
    const got = await call('get_proposal', { proposalId: proposed.proposalId });
    assert.equal(got.status, 'pending');
    assert.equal(got.lastError.code, 'CELLS_PROTECTED');
    assert.match(got.lastError.message, /celda protegida en Death_date/);
    assert.ok(got.lastError.at);
    const view = await listed(proposed.proposalId);
    assert.equal(view.status, 'pending');
    assert.match(view.lastError.message, /celda protegida en Death_date/);
    assert.equal(view.lastError.messageMsg.vars.fields[0], 'Death_date');
    // Still usable: corrected and applied; the error goes with it.
    const fixed = await call('update_proposal', { proposalId: proposed.proposalId, rows: [{ index: 0, values: { Death_date: null } }] });
    assert.ok(!fixed.error, JSON.stringify(fixed));
    const applied = await call('apply_proposal', { proposalId: proposed.proposalId });
    assert.equal(applied.status, 'applied', JSON.stringify(applied));
    assert.equal((await call('get_proposal', { proposalId: proposed.proposalId })).lastError, undefined);

    // A proposal partly written (needs_review): its real status and why, not "already applied".
    const other = await call('propose_changes', { reason: 'Otra', newRows: [wild('Y7T')] });
    store.db
      .prepare("UPDATE ai_proposals SET status = 'needs_review', last_error_json = ? WHERE id = ?")
      .run(JSON.stringify({ code: 'WRITE_UNCERTAIN', message: 'No se pudo confirmar la escritura en Google Sheets', at: new Date().toISOString() }), other.proposalId);
    const again = await call('apply_proposal', { proposalId: other.proposalId });
    assert.match(again.error, /needs_review: its last write failed: No se pudo confirmar/);
    assert.doesNotMatch(again.error, /already been applied/);
    assert.match((await call('update_proposal', { proposalId: other.proposalId, rows: [{ index: 0, values: { Sex: 'male' } }] })).error, /needs_review/);
  } finally {
    store.close();
  }
});

test("the same new rows in someone else's pending proposal are said, in the answer and above the table", async () => {
  const { store, call, franz, listed } = await fixture();
  try {
    const clutches = [1019, 1020].map(n => ({
      sheet: 'Insectary_stocks',
      values: { 'CLUTCH NUMBER': n, SPECIES: 'Oleria onega', 'DATE LAID': '2026-10-01', 'NUMBER OF EGGS': 10 },
    }));
    const first = await franz('propose_changes', { reason: 'Posturas 1019–1020', newRows: clutches });
    assert.ok(first.proposalId, JSON.stringify(first));
    assert.equal(first.overlaps, undefined);
    const second = await call('propose_changes', { reason: 'Posturas (otra vez)', newRows: [...clutches, wild('Y8T')] });
    assert.equal(second.overlaps?.length, 1, JSON.stringify(second));
    assert.equal(second.overlaps[0].proposalId, first.proposalId);
    assert.equal(second.overlaps[0].by, 'Franz');
    assert.deepEqual(second.overlaps[0].rows.sort(), ['1019', '1020']);
    assert.equal(second.overlaps[0].count, 2);
    // Above each table, both ways.
    const view = await listed(second.proposalId);
    assert.equal(view.overlaps?.[0]?.proposalId, first.proposalId);
    assert.equal(view.overlaps[0].by, 'Franz');
    // An edit of the same sheet row by two people overlaps too.
    const record = store.db.prepare("SELECT id FROM records WHERE sheet='Insectary_stocks' AND row_num=2").get().id;
    await franz('propose_changes', { reason: 'Huevos', changes: [{ recordId: record, values: { 'NUMBER OF EGGS': 21 } }] });
    const edit = await call('propose_changes', { reason: 'Huevos', changes: [{ recordId: record, values: { 'NUMBER OF EGGS': 22 } }] });
    assert.equal(edit.overlaps?.[0]?.by, 'Franz', JSON.stringify(edit));
    // Once Franz's is no longer pending, it is not said.
    store.db.prepare("UPDATE ai_proposals SET status = 'discarded' WHERE id = ?").run(first.proposalId);
    assert.equal((await listed(second.proposalId)).overlaps, undefined);
  } finally {
    store.close();
  }
});
