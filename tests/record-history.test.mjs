import test from 'node:test';
import assert from 'node:assert/strict';
import { Store } from '../server/store.mjs';
import { LocalSheets, formulaRowShift, shiftFormula } from '../server/sheets.mjs';
import { applyBatch } from '../server/batch.mjs';
import { historyGroup, historyGroups, markMovedFormulas, previewEdits, runHistoryTool } from '../server/history.mjs';

const ana = { id: 'u-ana', username: 'ana', displayName: 'Ana Pérez', role: 'editor' };
const bob = { id: 'u-bob', username: 'bob', displayName: 'Bob Díaz', role: 'editor' };
const BASE = Date.parse('2026-09-20T15:00:00.000Z');
const time = minutes => new Date(BASE + minutes * 60_000).toISOString();
const species = row => ({ formula: `=XLOOKUP(C${row},Insectary_stocks!A:A,Insectary_stocks!C:C,"")` });

async function fixture() {
  const insect = (id, row, extra = {}) => ({ row, values: { Insectary_ID: id, SPECIES: species(row), Sex: 'female', ...extra } });
  const sheets = new LocalSheets({
    Insectary_data: [insect('A0A', 2), insect('A1A', 3), insect('B0B', 4), insect('B0B', 5)],
    Collection_data: [{ row: 2, values: { CAM_ID: 'CAM000001', Insectary_ID: 'A0A', SPECIES: 'Species' } }],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data', 'Collection_data'] });
  for (const u of [ana, bob])
    store.db
      .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES(?,?,?,?,'s','h',1,'2026-01-01')")
      .run(u.id, u.username, u.displayName, u.role);
  let n = 0;
  const save = async (who, purpose, minutes, body) => {
    const out = await applyBatch(store, { requestId: `record-history-${++n}`, purpose, ...body }, who);
    store.db.prepare('UPDATE actions SET created_at=? WHERE id=?').run(time(minutes), out.action.id);
    return out.action.id;
  };
  const row = (sheet, r) => store.getRecordBySheetRow(sheet, r);
  const tool = (name, args) => runHistoryTool(store, name, args, {}, { publicUrl: 'https://app.example/' });
  return { store, sheets, save, row, tool };
}

/** Saves of two people on A0A and A1A, then an edit typed in Google Sheets. */
async function scene() {
  const f = await fixture();
  const { store, save, row } = f;
  const a0 = row('Insectary_data', 2).id;
  const a1 = row('Insectary_data', 3).id;
  const ids = {};
  ids.s0 = await save(ana, 'tablas', 0, { edits: [{ id: a0, values: { Sex: 'male', Notes_Insectary_data: 'uno' } }] });
  ids.s10 = await save(ana, 'tablas', 10, { edits: [{ id: a1, values: { Sex: 'male', Notes_Insectary_data: 'otro' } }] });
  ids.m60 = await save(bob, 'muertes', 60, { edits: [{ id: a0, values: { Death_date: '2026-09-19' } }] });
  await f.sheets.externalEdit('Insectary_data', 2, { Notes_Insectary_data: 'dos' });
  await store.refreshRows('Insectary_data', [2]);
  ids.sync = store.db.prepare("SELECT id FROM actions WHERE source='sheet_reconciliation' ORDER BY rowid DESC LIMIT 1").get().id;
  store.db.prepare('UPDATE actions SET created_at=? WHERE id=?').run(time(180), ids.sync);
  return { ...f, ids, a0, a1 };
}

test('formulaRowShift: the same formula with every relative row reference moved by one amount', () => {
  assert.equal(formulaRowShift('=M12963', '=M12972'), 9);
  const lookup = '=IFS($G5="","",TRUE,XLOOKUP($G5,Location_data!$A:$A,Lists!B$2:B))';
  assert.equal(formulaRowShift(lookup, shiftFormula(lookup, 3)), 3);
  assert.equal(formulaRowShift(lookup, shiftFormula(lookup, -2)), -2);
  assert.equal(formulaRowShift('=SUM(A$2:A9)', '=SUM(A$2:A10)'), 1, 'absolute rows stay');
  assert.equal(formulaRowShift('="A12"&B3', '="A12"&B4'), 1, 'text in quotes is no reference');
  // Edits, not moves.
  assert.equal(formulaRowShift('=M5', '=M5'), 0);
  assert.equal(formulaRowShift('=A5+B5', '=A6+B5'), 0, 'one reference moved, the other not');
  assert.equal(formulaRowShift('="A12"&B3', '="A13"&B4'), 0);
  assert.equal(formulaRowShift('=7', '=7+16'), 0);
  assert.equal(formulaRowShift('=Q5', '=#REF!'), 0);
  assert.equal(formulaRowShift('=SUM(A$2:A9)', '=SUM(A$3:A10)'), 0, 'an absolute row changed');
  assert.equal(formulaRowShift(null, '=A1'), 0);
});

test('sync: formulas that only moved with their row are not logged; edits on moved rows are', async () => {
  const { store, sheets, row } = await fixture();
  const before = store.db.prepare('SELECT count(*) n FROM changes').get().n;
  // A row inserted above row 3 in Google Sheets: rows 3–5 move down one, their formulas with them.
  const rows = sheets.rows.get('Insectary_data');
  for (const r of rows)
    if (r.row >= 3) {
      r.row++;
      for (const cell of r.cells)
        if (cell?.userEnteredValue?.formulaValue) cell.userEnteredValue.formulaValue = shiftFormula(cell.userEnteredValue.formulaValue, 1);
    }
  await sheets.externalEdit('Insectary_data', 3, { Insectary_ID: 'N0A', SPECIES: species(3), Sex: 'male' });
  // On a moved row (A1A, now row 4), one real edit of a value and one of a formula.
  await sheets.externalEdit('Insectary_data', 4, { Sex: 'male', SPECIES: { formula: '=XLOOKUP(C4,Insectary_stocks!A:A,Insectary_stocks!D:D,"")' } });
  await store.sync({ sheets: ['Insectary_data'] });
  const logged = store.db
    .prepare('SELECT c.row_num, c.field FROM changes c ORDER BY c.rowid')
    .all()
    .slice(before)
    .map(c => [c.row_num, c.field]);
  assert.deepEqual(logged, [
    [4, 'SPECIES'],
    [4, 'Sex'],
  ]);
  // The moved rows keep their records with the new formulas.
  assert.equal(row('Insectary_data', 6).label, 'B0B');
  assert.equal(row('Insectary_data', 6).formulas.SPECIES, species(6).formula);
  assert.equal(row('Insectary_data', 3).label, 'N0A');
  store.close();
});

test('older syncs: formulas moved with their rows are marked and left out of the Historial and its undo', async () => {
  const { store, row } = await fixture();
  const a0 = row('Insectary_data', 2);
  const a1 = row('Insectary_data', 3);
  // As syncs logged them before: one save with only moved formulas, a later one with a moved formula and an edit.
  const external = (record, diffs, minutes) => {
    store.recordExternalChanges(record, diffs);
    const id = store.db.prepare("SELECT id FROM actions WHERE source='sheet_reconciliation' ORDER BY rowid DESC LIMIT 1").get().id;
    store.db.prepare('UPDATE actions SET created_at=? WHERE id=?').run(time(minutes), id);
    return id;
  };
  const movedOnly = external(a0, [{ field: 'SPECIES', before: species(1), after: species(2) }], 0);
  const mixedFirst = external(a0, [{ field: 'SPECIES', before: species(2), after: species(7) }], 60);
  const mixed = external(
    a1,
    [
      { field: 'SPECIES', before: species(2), after: species(3) },
      { field: 'Notes_Insectary_data', before: null, after: 'nota' },
    ],
    60.5,
  );
  assert.equal(markMovedFormulas(store.db), 3);
  assert.equal(markMovedFormulas(store.db), 0, 'once');

  // The group of the second sync shows only the edit; its id stays its oldest save.
  const { groups } = historyGroups(store, {});
  assert.deepEqual(
    groups.map(g => [g.id, g.counts.actions, g.counts.cells, g.fields]),
    [[mixedFirst, 1, 1, ['Notes_Insectary_data']]],
    'the sync that only moved formulas is gone',
  );
  const detail = historyGroup(store, mixedFirst);
  assert.deepEqual(
    detail.actions.map(a => [a.id, a.changes.map(c => c.field)]),
    [[mixed, ['Notes_Insectary_data']]],
  );
  // A link to the sync that only moved formulas still opens it, empty.
  const empty = historyGroup(store, movedOnly);
  assert.equal(empty.movedOnly, true);
  assert.equal(empty.id, movedOnly);
  assert.deepEqual(empty.actions.map(a => a.changes.length), [0]);
  assert.equal(empty.undoable, false);
  // Searching finds the edit, not the moves; undoing the group leaves the formulas alone.
  assert.deepEqual(historyGroups(store, { text: 'A0A' }).groups, []);
  assert.deepEqual(historyGroups(store, { recordId: a1.id }).groups[0].matched, [mixed]);
  const preview = previewEdits(store, { groupIds: [mixedFirst] });
  // (The made-up sync did not write the row: its cell is a conflict here.)
  assert.deepEqual([...preview.changes, ...preview.conflicts].map(c => c.field), ['Notes_Insectary_data']);
  assert.deepEqual(preview.selection.actionIds, [mixed]);
  store.close();
});

test('record_history: every change to one row, oldest first, with who, why and a link to each save', async () => {
  const { tool, ids, a0 } = await scene();
  const out = await tool('record_history', { id: 'A0A' });
  assert.deepEqual(out.row, { recordId: a0, sheet: 'Insectary_data', row: 2, label: 'A0A' });
  assert.deepEqual(
    out.saves.map(s => [s.who, s.purpose, s.cells]),
    [
      ['Ana Pérez', 'Tablas', ['Sex: female → male', 'Notes_Insectary_data: (empty) → uno']],
      ['Bob Díaz', 'Muertes', ['Death_date: (empty) → 2026-09-19']],
      ['unknown', 'Google Sheets', ['Notes_Insectary_data: uno → dos']],
    ],
  );
  assert.equal(out.saves[0].url, `https://app.example/#/historial?accion=${ids.s0}`);
  assert.equal(out.saves[2].reason, 'Snapshot comparison; intermediate edits and editor unknown');
  assert.equal(out.total, 3);
  assert.equal(out.next, null);
  // Another row with that identifier (its collection row) is named apart.
  assert.deepEqual(
    out.others.map(r => [r.sheet, r.label]),
    [['Collection_data', 'CAM000001']],
  );

  // Filters and pages.
  assert.deepEqual((await tool('record_history', { id: 'A0A', fields: ['Notes_Insectary_data'] })).saves.map(s => s.cells), [
    ['Notes_Insectary_data: (empty) → uno'],
    ['Notes_Insectary_data: uno → dos'],
  ]);
  assert.equal((await tool('record_history', { id: 'A0A', from: '2026-09-21' })).total, 0);
  assert.equal((await tool('record_history', { id: 'A0A', to: '2026-09-20' })).total, 3);
  const page = await tool('record_history', { id: 'A0A', limit: 2 });
  assert.equal(page.saves.length, 2);
  assert.equal(page.next, 2);
  assert.equal((await tool('record_history', { id: 'A0A', limit: 2, offset: 2 })).saves[0].who, 'unknown');

  // By record id, by another case, and a name several rows share.
  assert.equal((await tool('record_history', { recordId: a0 })).total, 3);
  assert.equal((await tool('record_history', { id: 'a0a' })).row.recordId, a0);
  const shared = await tool('record_history', { id: 'B0B' });
  assert.deepEqual(
    shared.rows.map(r => [r.label, r.row]),
    [
      ['B0B', 4],
      ['B0B', 5],
    ],
  );
  assert.equal(shared.saves, undefined);
  assert.equal((await tool('record_history', { id: 'Z9Z' })).code, 'RECORD_NOT_FOUND');
});

test('get_history_group: filters, pages and one save alone; list_history names the saves that match', async () => {
  const { store, tool, ids, a1 } = await scene();
  const group = await tool('get_history_group', { id: ids.s0 });
  assert.equal(group.cells, 4);
  assert.equal(group.next, null);
  assert.deepEqual(
    (await tool('get_history_group', { id: ids.s0, field: 'sex' })).actions.map(a => a.changes.map(c => [c.label, c.after])),
    [[['A1A', 'male']], [['A0A', 'male']]],
  );
  const one = await tool('get_history_group', { id: ids.s0, recordId: a1 });
  assert.deepEqual([one.cells, one.actions.map(a => a.id)], [2, [ids.s10]]);
  assert.deepEqual(
    (await tool('get_history_group', { id: ids.s0, text: 'UNO' })).actions.flatMap(a => a.changes.map(c => c.after)),
    ['uno'],
  );
  // Pages of maxChanges cells: the saves past the last cell are not listed.
  const first = await tool('get_history_group', { id: ids.s0, maxChanges: 3 });
  assert.deepEqual(first.actions.map(a => a.changes.length), [2, 1]);
  assert.equal(first.next, 3);
  const rest = await tool('get_history_group', { id: ids.s0, maxChanges: 3, offset: first.next });
  assert.deepEqual([rest.actions.map(a => [a.id, a.changes.length]), rest.next], [[[ids.s0, 1]], null]);
  // One save, not its group.
  const single = await tool('get_history_group', { actionId: ids.s10 });
  assert.deepEqual(single.actions.map(a => a.id), [ids.s10]);
  assert.equal(single.url, `https://app.example/#/historial?accion=${ids.s10}`);
  assert.equal(single.counts.cells, 2);

  // list_history: with a filter, the saves of a group that match (not when all of them do).
  const found = await tool('list_history', { text: 'otro' });
  assert.deepEqual(found.groups[0].matched, [ids.s10]);
  assert.equal((await tool('list_history', { user: 'ana' })).groups[0].matched, undefined);
  store.close();
});
