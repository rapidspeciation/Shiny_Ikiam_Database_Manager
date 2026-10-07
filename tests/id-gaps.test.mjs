// Emergidos' «Siguiente ID»: the runs of free pre-made Insectary IDs (server/grid.mjs
// insectaryGaps), newest first, the one after the last row used marked; IDs held by
// someone's cards stay in their run, counted apart; rows with data break a run. A row
// with butterfly data is never taken by a new row: overwriting it is an edit of that row,
// kept in the app like any entry, written by «Guardar en Google Sheets» and undone from
// Historial.
import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { insectaryGaps } from '../server/grid.mjs';
import { holdId, releaseHold } from '../server/holds.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { Store } from '../server/store.mjs';

const ana = { id: 'ana', username: 'ana', displayName: 'Ana', role: 'editor' };
const luis = { id: 'luis', username: 'luis', displayName: 'Luis', role: 'editor' };

async function fixture() {
  const used = (id, extra = {}) => ({ Insectary_ID: id, 'CLUTCH NUMBER': 990, Sex: 'female', Intro2Insectary_date: 46290, Wild_Reared: 'Reared', ...extra });
  const sheets = new LocalSheets(
    {
      Insectary_data: [
        { row: 2, values: used('A0E') },
        { row: 3, values: { Insectary_ID: 'A1E' } },
        { row: 4, values: { Insectary_ID: 'A2E' } },
        { row: 5, values: used('A3E', { Notes_Insectary_data: 'old note' }) },
        { row: 6, values: { Insectary_ID: 'A4E' } },
        { row: 7, values: { Insectary_ID: 'A5E' } },
        { row: 8, values: { Insectary_ID: 'A6E' } },
        // Something typed (a skipped ID's note): not offered, and it splits the run.
        { row: 9, values: { Insectary_ID: 'A7E', Notes_Insectary_data: 'we skipped this ID' } },
        { row: 10, values: { Insectary_ID: 'A8E' } },
        { row: 11, values: used('A9E') },
        { row: 12, values: { Insectary_ID: 'B0E' } },
        { row: 13, values: { Insectary_ID: 'B1E' } },
        { row: 14, values: { Insectary_ID: 'B2E' } },
      ],
      Insectary_stocks: [{ row: 2, values: { 'CLUTCH NUMBER': 990, 'DATE LAID': 46280, 'NUMBER OF EGGS': 20 } }],
    },
    { health: { probeMs: 20 } },
  );
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data', 'Insectary_stocks'] });
  for (const u of [ana, luis])
    store.db
      .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,created_at) VALUES(?,?,?,?,'s','h',?)")
      .run(u.id, u.username, u.displayName, u.role, new Date().toISOString());
  return { store, sheets };
}
const brief = gaps => gaps.map(g => `${g.from}-${g.to}:${g.rowFrom}-${g.rowTo}:${g.free}/${g.held}${g.latest ? '*' : ''}`);
const cellIn = (sheets, sheet, row, field) => {
  const rows = sheets.rows.get(sheet);
  const header = rows.find(r => r.row === 1).cells.findIndex(c => c?.userEnteredValue?.stringValue === field);
  const cell = rows.find(r => r.row === row)?.cells[header]?.userEnteredValue;
  return cell ? (cell.stringValue ?? cell.numberValue ?? cell.formulaValue ?? null) : null;
};

test('the gaps: runs of free pre-made rows, newest first, the one after the last row used marked', async () => {
  const { store } = await fixture();
  try {
    assert.deepEqual(brief(insectaryGaps(store).gaps), [
      'B0E-B2E:12-14:3/0*',
      'A8E-A8E:10-10:1/0',
      'A4E-A6E:6-8:3/0',
      'A1E-A2E:3-4:2/0',
    ]);
  } finally {
    store.close();
  }
});

test("IDs someone's cards hold stay in their run but are not offered; a run all held says so", async () => {
  const { store } = await fixture();
  try {
    const a = randomUUID();
    const b = randomUUID();
    holdId(store, { key: a, value: 'A4E' }, luis);
    holdId(store, { key: b, value: 'A5E' }, luis);
    holdId(store, { key: randomUUID(), value: 'B0E' }, ana);
    holdId(store, { key: randomUUID(), value: 'A8E' }, ana);
    assert.deepEqual(brief(insectaryGaps(store).gaps), [
      // The latest gap is still the one after the last row used.
      'B1E-B2E:12-14:2/1*',
      'null-null:10-10:0/1',
      'A6E-A6E:6-8:1/2',
      'A1E-A2E:3-4:2/0',
    ]);
    // Let go: offered again (the answer follows the claims).
    releaseHold(store, a, luis);
    assert.equal(insectaryGaps(store).gaps[2].from, 'A4E');
  } finally {
    store.close();
  }
});

test('a saved butterfly splits its gap; a row with data is never in one', async () => {
  const { store } = await fixture();
  try {
    const out = await store.staged.stage(
      {
        requestId: randomUUID(),
        purpose: 'emergidos',
        partial: false,
        creates: [{ clientId: randomUUID(), module: 'Insectary_data', values: { Insectary_ID: 'A5E', 'CLUTCH NUMBER': 990, Sex: 'male', Intro2Insectary_date: 46300 } }],
      },
      ana,
    );
    assert.ok(out.entryId);
    // Kept in the app (claimed): A5E is held in its run until written.
    assert.deepEqual(brief(insectaryGaps(store).gaps).at(2), 'A4E-A6E:6-8:2/1');
    await store.staged.flush({ requestId: randomUUID() }, ana);
    const gaps = brief(insectaryGaps(store).gaps);
    assert.deepEqual(gaps.slice(2), ['A6E-A6E:8-8:1/0', 'A4E-A4E:6-6:1/0', 'A1E-A2E:3-4:2/0']);
    for (const id of ['A0E', 'A3E', 'A5E', 'A7E', 'A9E']) assert.ok(!gaps.some(g => g.includes(id)), id);
  } finally {
    store.close();
  }
});

test('a row with data is never taken by a new row; overwriting it is an edit kept in the app, undone from Historial', async () => {
  const { store, sheets } = await fixture();
  try {
    const row = store.db.prepare("SELECT id FROM records WHERE sheet='Insectary_data' AND row_num=5").get().id;
    // A new butterfly on A3E (a row with data) is refused.
    await assert.rejects(
      store.staged.stage(
        {
          requestId: randomUUID(),
          purpose: 'emergidos',
          partial: false,
          creates: [{ clientId: randomUUID(), module: 'Insectary_data', values: { Insectary_ID: 'A3E', 'CLUTCH NUMBER': 990, Sex: 'male' } }],
        },
        ana,
      ),
      e => e.code === 'BATCH_CONFLICT' && e.details.items[0].code === 'DUPLICATE_ID',
    );
    // «Sobrescribir de todas formas»: the card's values on that row, what it held cleared, as the person saw it.
    const values = { Sex: 'male', Intro2Insectary_date: 46300, Notes_Insectary_data: null };
    const expected = { Sex: 'female', Intro2Insectary_date: 46290, Notes_Insectary_data: 'old note' };
    const staged = await store.staged.stage({ requestId: randomUUID(), purpose: 'emergidos', partial: false, edits: [{ id: row, values, expected }] }, ana);
    assert.ok(staged.entryId);
    assert.equal(cellIn(sheets, 'Insectary_data', 5, 'Sex'), 'female', 'not in the sheet until saved');
    // Someone who saw the row before is refused (the cell changed in the app meanwhile).
    const late = await store.staged.stage(
      { requestId: randomUUID(), purpose: 'emergidos', edits: [{ id: row, values: { Sex: 'NA' }, expected: { Sex: 'female' } }] },
      luis,
    );
    assert.equal(late.skipped[0]?.code, 'EXTERNAL_CONFLICT');
    // A row being overwritten offers no gap meanwhile, nor after.
    assert.ok(!brief(insectaryGaps(store).gaps).some(g => g.includes(':5-')));
    const out = await store.staged.flush({ requestId: randomUUID() }, ana);
    assert.equal(out.status, 'done');
    assert.equal(cellIn(sheets, 'Insectary_data', 5, 'Sex'), 'male');
    assert.equal(cellIn(sheets, 'Insectary_data', 5, 'Intro2Insectary_date'), 46300);
    assert.equal(cellIn(sheets, 'Insectary_data', 5, 'Notes_Insectary_data'), null);
    // In Historial as an Emergidos save; undone, the row holds what it had.
    const action = store.db.prepare('SELECT * FROM actions WHERE id = ?').get(out.outbox.actionId);
    assert.equal(action.purpose, 'emergidos');
    await store.undo({ actionIds: [action.id], requestId: randomUUID() }, ana);
    assert.equal(cellIn(sheets, 'Insectary_data', 5, 'Sex'), 'female');
    assert.equal(cellIn(sheets, 'Insectary_data', 5, 'Intro2Insectary_date'), 46290);
    assert.equal(cellIn(sheets, 'Insectary_data', 5, 'Notes_Insectary_data'), 'old note');
  } finally {
    store.close();
  }
});

test('the gaps answer fast on a 20 000-row sheet', async () => {
  const rows = [];
  const ids = [];
  for (let round = 0; round < 3; round++)
    for (let letter = 0; letter < 26; letter++) for (let d = 0; d < 10; d++) ids.push(`${String.fromCharCode(65 + letter)}${d}${'CDE'[round]}`);
  // 20 000 rows: old-form IDs first, then the pre-made series; every 7th row of the series used.
  for (let i = 0; i < 20000 - ids.length; i++) rows.push({ row: i + 2, values: { Insectary_ID: `${i}X`, Sex: 'male' } });
  ids.forEach((id, i) =>
    rows.push({ row: rows.length + 2, values: i % 7 === 0 && i < 600 ? { Insectary_ID: id, Sex: 'female', 'CLUTCH NUMBER': 1 } : { Insectary_ID: id } }),
  );
  const sheets = new LocalSheets({ Insectary_data: rows }, { health: { probeMs: 20 } });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data'] });
  try {
    insectaryGaps(store);
    // A hold changes the claims: the answer is worked out again from the kept rows.
    holdId(store, { key: randomUUID(), value: 'B3C' }, { ...ana, role: 'admin' });
    const t0 = performance.now();
    const { gaps } = insectaryGaps(store);
    const ms = performance.now() - t0;
    assert.ok(ms < 50, `${ms.toFixed(1)} ms`);
    assert.ok(gaps[0].latest);
    assert.ok(gaps.length > 50);
  } finally {
    store.close();
  }
});
