import test from 'node:test';
import assert from 'node:assert/strict';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { applyBatch } from '../server/batch.mjs';
import { cellHistory, sheetAsOf, undoEdits } from '../server/history.mjs';

const ana = { id: 'u-ana', username: 'ana', displayName: 'Ana Pérez', role: 'editor' };
const bob = { id: 'u-bob', username: 'bob', displayName: 'Bob Díaz', role: 'editor' };
const BASE = Date.parse('2026-09-20T15:00:00.000Z');
const time = minutes => new Date(BASE + minutes * 60_000).toISOString();
const species = row => ({ formula: `=XLOOKUP(C${row},Insectary_stocks!A:A,Insectary_stocks!C:C,"")` });

/**
 * Insectary rows A0A–A3A and one collection row; saves by Ana and Bob at
 * chosen minutes, an edit read from Google Sheets, an undo, a new row.
 */
async function scene() {
  const insect = (id, row) => ({ row, values: { Insectary_ID: id, SPECIES: species(row), Sex: 'female' } });
  const sheets = new LocalSheets({
    Insectary_data: [insect('A0A', 2), insect('A1A', 3), insect('A2A', 4), insect('A3A', 5)],
    Collection_data: [{ row: 2, values: { CAM_ID: 'CAM000001', Insectary_ID: 'A0A', SPECIES: 'Species' } }],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data', 'Collection_data'] });
  for (const u of [ana, bob])
    store.db
      .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES(?,?,?,?,'s','h',1,'2026-01-01')")
      .run(u.id, u.username, u.displayName, u.role);
  let n = 0;
  const stamp = (id, minutes) => store.db.prepare('UPDATE actions SET created_at=? WHERE id=?').run(time(minutes), id);
  const save = async (who, purpose, minutes, body) => {
    const out = await applyBatch(store, { requestId: `as-of-save-${++n}`, purpose, ...body }, who);
    stamp(out.action.id, minutes);
    return out.action.id;
  };
  const row = (sheet, r) => store.getRecordBySheetRow(sheet, r);
  const a0 = row('Insectary_data', 2).id;
  const a1 = row('Insectary_data', 3).id;
  const ids = {};
  // Ana: three saves on A0A's notes a few minutes apart (one edit), then much later another.
  ids.n0 = await save(ana, 'tablas', 0, { edits: [{ id: a0, values: { Notes_Insectary_data: 'uno', Sex: 'male' } }] });
  ids.n3 = await save(ana, 'tablas', 3, { edits: [{ id: a0, values: { Notes_Insectary_data: 'dos' } }] });
  ids.n8 = await save(ana, 'tablas', 8, { edits: [{ id: a0, values: { Notes_Insectary_data: 'tres' } }] });
  ids.n40 = await save(ana, 'tablas', 40, { edits: [{ id: a0, values: { Notes_Insectary_data: 'cuatro' } }] });
  // Bob, on two rows at once: the save someone wants to look into.
  ids.b60 = await save(bob, 'muertes', 60, {
    edits: [
      { id: a0, values: { Death_date: '2026-09-19' } },
      { id: a1, values: { Death_date: '2026-09-19', Notes_Insectary_data: 'mal' } },
    ],
  });
  // Typed in Google Sheets.
  await sheets.externalEdit('Insectary_data', 2, { Notes_Insectary_data: 'cinco' });
  await store.refreshRows('Insectary_data', [2]);
  ids.sync = store.db.prepare("SELECT id FROM actions WHERE source='sheet_reconciliation' ORDER BY rowid DESC LIMIT 1").get().id;
  stamp(ids.sync, 90);
  // Bob's save undone.
  const undo = await undoEdits(store, { actionIds: [ids.b60], requestId: 'as-of-undo-1' }, ana);
  ids.undo = (undo.action ?? undo.actions[0]).id;
  stamp(ids.undo, 120);
  // A new collection row.
  ids.c150 = await save(ana, 'colecta', 150, { creates: [{ module: 'Collection_data', values: { CAM_ID: 'CAM000002', SPECIES: 'Nueva' } }] });
  return { store, sheets, ids, a0, a1, row };
}

const valuesOf = (store, out, sheet) => {
  const keys = (store.listModules().find(m => m.id === sheet) ?? {}).fields.map(f => f.key);
  return Object.fromEntries(out.rows.map(r => [r.row, Object.fromEntries(keys.map((k, i) => [k, r.v[i]]))]));
};

test('cell history: saves by one person within 10 minutes are one edit; others, syncs and undos apart', async () => {
  const { store, ids, a0 } = await scene();
  const out = cellHistory(store, { recordId: a0, field: 'Notes_Insectary_data' });
  assert.deepEqual(
    out.edits.map(e => [e.actor, e.purpose, e.cells.map(c => [c.before, c.after, c.edits])]),
    [
      ['u-ana', 'tablas', [[null, 'tres', 3]]],
      ['u-ana', 'tablas', [['tres', 'cuatro', 1]]],
      ['unknown', 'sheets', [['cuatro', 'cinco', 1]]],
    ],
  );
  assert.deepEqual(out.edits[0].actionIds, [ids.n0, ids.n3, ids.n8]);
  assert.equal(out.edits[0].first, ids.n0);
  assert.equal(out.edits[0].last, ids.n8);
  assert.equal(out.edits[0].actorName, 'Ana Pérez');
  assert.equal(out.edits[0].link, `#/historial?grupo=${ids.n8}`);
  assert.equal(out.saves, 5);
  assert.equal(out.since, store.db.prepare('SELECT min(created_at) t FROM actions').get().t);
  assert.equal(out.sheetsSince, time(90));

  // The whole row: the death date, then its undo, as edits of their own.
  const row = cellHistory(store, { recordId: a0 });
  // (Dates are kept as the sheet's day numbers.)
  const death = row.edits[2].cells[0].after;
  assert.ok(death);
  assert.deepEqual(
    row.edits.map(e => [e.purpose, e.cells.map(c => `${c.field}:${c.before}→${c.after}${c.undone ? ' (undone)' : ''}`)]),
    [
      ['tablas', ['Notes_Insectary_data:null→tres', 'Sex:female→male']],
      ['tablas', ['Notes_Insectary_data:tres→cuatro']],
      ['muertes', [`Death_date:null→${death} (undone)`]],
      ['sheets', ['Notes_Insectary_data:cuatro→cinco']],
      ['deshacer', [`Death_date:${death}→null`]],
    ],
  );
  assert.throws(() => cellHistory(store, {}), { code: 'RECORD_NOT_FOUND' });
  store.close();
});

test('as of a save: every later change undone, newest first; the cells it changed and those that differ from now', async () => {
  const { store, ids, a0, a1 } = await scene();
  const now = valuesOf(store, sheetAsOf(store, { module: 'Insectary_data', at: time(1000), row: 3 }), 'Insectary_data');
  assert.equal(now[2].Notes_Insectary_data, 'cinco');

  // Just before Bob's save: A0A's notes after Ana's four saves, no death dates.
  const before = sheetAsOf(store, { module: 'Insectary_data', action: ids.b60, row: 3, context: 2 });
  assert.equal(before.side, 'before');
  assert.equal(before.at, time(60));
  assert.deepEqual([before.from, before.to], [1, 5]);
  const then = valuesOf(store, before, 'Insectary_data');
  assert.deepEqual(Object.keys(then).map(Number), [2, 3, 4, 5]);
  assert.equal(then[2].Notes_Insectary_data, 'cuatro');
  assert.equal(then[2].Death_date, null);
  assert.equal(then[3].Notes_Insectary_data, null);
  assert.deepEqual(before.touched, { [a0]: ['Death_date'], [a1]: ['Death_date', 'Notes_Insectary_data'] });
  // A0A differs from now only in its notes (the death date was undone since); A1A not at all.
  assert.deepEqual(before.changed, { [a0]: { Notes_Insectary_data: 'cinco' } });
  assert.equal(before.action.actorName, 'Bob Díaz');
  assert.equal(before.action.purpose, 'muertes');
  assert.equal(before.action.link, `#/historial?grupo=${ids.b60}`);
  // Formulas: the same one then and now keeps today's value, marked as a formula.
  const species = store.listModules().find(m => m.id === 'Insectary_data').fields.findIndex(f => f.key === 'SPECIES');
  assert.ok(before.rows[0].f.includes(species));

  // Just after it: the death dates are there.
  const after = valuesOf(store, sheetAsOf(store, { module: 'Insectary_data', action: ids.b60, side: 'after', row: 3, context: 2 }), 'Insectary_data');
  assert.ok(after[2].Death_date);
  assert.equal(after[3].Notes_Insectary_data, 'mal');

  // Between Ana's saves of one edit, by time.
  const mid = valuesOf(store, sheetAsOf(store, { module: 'Insectary_data', at: time(5), row: 2, context: 0 }), 'Insectary_data');
  assert.equal(mid[2].Notes_Insectary_data, 'dos');
  assert.equal(mid[2].Sex, 'male');
  const start = valuesOf(store, sheetAsOf(store, { module: 'Insectary_data', action: ids.n0, row: 2, context: 0 }), 'Insectary_data');
  assert.deepEqual([start[2].Notes_Insectary_data, start[2].Sex], [null, 'female']);

  // Before the sync and after the undo.
  const synced = valuesOf(store, sheetAsOf(store, { module: 'Insectary_data', action: ids.sync, row: 2, context: 0 }), 'Insectary_data');
  assert.equal(synced[2].Notes_Insectary_data, 'cuatro');
  assert.ok(synced[2].Death_date, 'the death date was still there before the undo');
  const undone = valuesOf(store, sheetAsOf(store, { module: 'Insectary_data', action: ids.undo, side: 'after', row: 2, context: 0 }), 'Insectary_data');
  assert.equal(undone[2].Death_date, null);
  store.close();
});

test('as of: rows created later come back empty and marked absent; bad requests are refused', async () => {
  const { store, ids, row } = await scene();
  const created = row('Collection_data', 3);
  assert.equal(created.values.CAM_ID, 'CAM000002');
  const before = sheetAsOf(store, { module: 'Collection_data', action: ids.c150, row: 2, context: 5 });
  assert.deepEqual(before.absent, [created.id]);
  const then = valuesOf(store, before, 'Collection_data');
  assert.equal(then[3].CAM_ID, null);
  assert.equal(then[2].CAM_ID, 'CAM000001');
  const after = sheetAsOf(store, { module: 'Collection_data', action: ids.c150, side: 'after', row: 2, context: 5 });
  assert.deepEqual(after.absent, []);
  assert.equal(valuesOf(store, after, 'Collection_data')[3].CAM_ID, 'CAM000002');

  assert.throws(() => sheetAsOf(store, { module: 'Nope', at: time(0), row: 2 }), { code: 'MODULE_NOT_FOUND' });
  assert.throws(() => sheetAsOf(store, { module: 'Collection_data', at: 'yesterday', row: 2 }), { code: 'INVALID_MOMENT' });
  assert.throws(() => sheetAsOf(store, { module: 'Collection_data', action: 'nope', row: 2 }), { code: 'GROUP_NOT_FOUND' });
  assert.throws(() => sheetAsOf(store, { module: 'Collection_data', at: time(0) }), { code: 'INVALID_RANGE' });
  store.close();
});
