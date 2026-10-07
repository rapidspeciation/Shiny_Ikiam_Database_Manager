// Emergidos and Clutches entries are kept in the app (shared by everyone, checked as a save
// is) until someone presses «Guardar en Google Sheets»: then all of them are written as one
// save through the outbox. A new row's Insectary ID, CAM, tube or clutch number is claimed
// when it is entered: two people never get the same one.
import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { applyBatch } from '../server/batch.mjs';
import { idSuggestions } from '../server/grid.mjs';
import { clutchDay, notebookChanges } from '../server/clutches.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { Store } from '../server/store.mjs';
import { buildCopy } from '../server/replica.mjs';
import { DatabaseSync } from 'node:sqlite';
import { mkdtempSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';

const ana = { id: 'ana', username: 'ana', displayName: 'Ana', role: 'editor' };
const luis = { id: 'luis', username: 'luis', displayName: 'Luis', role: 'editor' };
const viewer = { id: 'vera', username: 'vera', displayName: 'Vera', role: 'viewer' };
const SPECIES = 'Mechanitis messenoides';

async function fixture() {
  const sheets = new LocalSheets(
    {
      Insectary_data: [
        { row: 2, values: { Insectary_ID: 'A0E', 'CLUTCH NUMBER': 990, SPECIES, Sex: 'female', CAM_ID: 'CAM000100', Tube_1_id: 'FS00000100' } },
        { row: 3, values: { Insectary_ID: 'A1E' } },
        { row: 4, values: { Insectary_ID: 'A2E' } },
        { row: 5, values: { Insectary_ID: 'A3E' } },
        { row: 6, values: { Insectary_ID: 'A4E' } },
      ],
      Insectary_stocks: [
        { row: 2, values: { 'CLUTCH NUMBER': 990, SPECIES, 'DATE LAID': 46280, 'NUMBER OF EGGS': 20 } },
        { row: 3, values: { 'CLUTCH NUMBER': 991, SPECIES, 'DATE LAID': 46281, 'NUMBER OF EGGS': 12 } },
      ],
    },
    { health: { probeMs: 20 } },
  );
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data', 'Insectary_stocks'] });
  for (const u of [ana, luis, viewer])
    store.db
      .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,created_at) VALUES(?,?,?,?,'s','h',?)")
      .run(u.id, u.username, u.displayName, u.role, new Date().toISOString());
  return { store, sheets };
}
const emerged = (id, extra = {}) => ({
  clientId: randomUUID(),
  module: 'Insectary_data',
  values: { Insectary_ID: id, 'CLUTCH NUMBER': 990, Sex: 'male', Intro2Insectary_date: 46300, Wild_Reared: 'Reared', ...extra },
});
const stage = (store, user, body) => store.staged.stage({ requestId: randomUUID(), purpose: 'emergidos', ...body }, user);
const clutchId = (store, n) =>
  store.db.prepare("SELECT id FROM records WHERE sheet='Insectary_stocks' AND json_extract(values_json,'$.\"CLUTCH NUMBER\"')=?").get(n).id;
const cellIn = (sheets, sheet, row, field) => {
  const rows = sheets.rows.get(sheet);
  const header = rows.find(r => r.row === 1).cells.findIndex(c => c?.userEnteredValue?.stringValue === field);
  const cell = rows.find(r => r.row === row)?.cells[header]?.userEnteredValue;
  return cell?.formulaValue ?? cell?.stringValue ?? cell?.numberValue ?? null;
};
const until = async (check, ms = 3000) => {
  const end = Date.now() + ms;
  while (!check()) {
    if (Date.now() > end) throw new Error('timed out');
    await new Promise(resolve => setTimeout(resolve, 10));
  }
};

test('an emerged butterfly is kept in the app for everyone, its ID claimed and no longer offered', async () => {
  const { store, sheets } = await fixture();
  try {
    assert.equal(idSuggestions(store, { kind: 'insectary', count: 5 }).sequence[0], 'A1E');
    const out = await stage(store, ana, { creates: [emerged('A1E')] });
    assert.equal(out.status, 'staged');
    assert.equal(out.created.length, 1);
    assert.match(out.created[0].recordId, /^staged:/);
    assert.equal(cellIn(sheets, 'Insectary_data', 3, 'Sex'), null, 'not written to the sheet');
    const list = store.staged.list();
    assert.equal(list.items.length, 1);
    assert.equal(list.items[0].actorName, 'Ana');
    assert.deepEqual(list.claims.map(c => [c.kind, c.value, c.actorName]), [['insectary', 'A1E', 'Ana']]);
    assert.deepEqual(idSuggestions(store, { kind: 'insectary', count: 5 }).sequence, ['A2E', 'A3E', 'A4E']);
    assert.equal(store.googleState().staged.rows, 1);
    // Only people who edit.
    await assert.rejects(stage(store, viewer, { creates: [emerged('A2E')] }), e => e.status === 403);
  } finally {
    store.close();
  }
});

test('two people taking the same next ID at once: one gets it, the other is told who holds it and the next free one', async () => {
  const { store } = await fixture();
  try {
    const [a, b] = await Promise.all([
      stage(store, ana, { creates: [emerged('A1E')] }),
      stage(store, luis, { creates: [emerged('A1E', { Sex: 'female' })] }),
    ]);
    assert.equal(a.created.length, 1);
    assert.equal(b.created.length, 0);
    assert.equal(b.skipped[0].code, 'CLAIMED');
    assert.equal(b.skipped[0].next, 'A2E');
    assert.match(b.skipped[0].message, /A1E ya lo tiene Ana .* el siguiente libre es A2E/);
    // The same with CAMs and tubes of preserved bodies.
    const preserved = { Death_date: 46300, Death_cause: 'Killed_Preserved', CAM_ID: 'CAM000101', Tube_1_id: 'FS00000101' };
    const [c, d] = await Promise.all([
      stage(store, ana, { creates: [emerged('A2E', preserved)] }),
      stage(store, luis, { creates: [emerged('A3E', preserved)] }),
    ]);
    assert.equal(c.created.length + d.created.length, 1);
    const refused = [c, d].find(o => !o.created.length).skipped[0];
    assert.equal(refused.code, 'CLAIMED');
    assert.match(refused.next, /^(CAM000102|FS00000102)$/);
    assert.equal(idSuggestions(store, { kind: 'cam', start: 'CAM000100', count: 1 }).sequence[0], 'CAM000102');
    // An ID typed by hand that someone else holds, in another tab's save: refused, with who.
    const direct = applyBatch(store, { requestId: randomUUID(), creates: [emerged('A1E')] }, luis);
    await assert.rejects(direct, e => e.code === 'BATCH_CONFLICT' && e.details.items[0].code === 'CLAIMED');
  } finally {
    store.close();
  }
});

test('a clutch count changed by two people in turn: the second builds on the first; one who did not see it is refused', async () => {
  const { store } = await fixture();
  try {
    const id = clutchId(store, 990);
    const first = await store.staged.stage(
      { requestId: randomUUID(), purpose: 'clutches', edits: [{ id, values: { 'NUMBER OF LARVAE': '=10' }, expected: { 'NUMBER OF LARVAE': null } }] },
      ana,
    );
    assert.equal(first.status, 'staged');
    const second = await store.staged.stage(
      { requestId: randomUUID(), purpose: 'clutches', edits: [{ id, values: { 'NUMBER OF LARVAE': '=10+4' }, expected: { 'NUMBER OF LARVAE': '=10' } }] },
      luis,
    );
    assert.equal(second.status, 'staged');
    const late = await store.staged.stage(
      { requestId: randomUUID(), purpose: 'clutches', edits: [{ id, values: { 'NUMBER OF LARVAE': '=3' }, expected: { 'NUMBER OF LARVAE': null } }] },
      ana,
    );
    assert.equal(late.status, 'unchanged');
    assert.equal(late.skipped[0].code, 'EXTERNAL_CONFLICT');
    // The first can't be undone while the second builds on it.
    const firstItem = store.staged.list().items.find(i => i.actor === 'ana');
    await assert.rejects(store.staged.remove({ itemId: firstItem.id }, ana), e => e.code === 'STAGED_LATER');
    // The day's list and the notebook's show them, marked as not in the sheet.
    const day = clutchDay(store, {});
    const line = day.changes.find(c => c.field === 'NUMBER OF LARVAE');
    assert.equal(line.staged, true);
    assert.deepEqual([line.before, line.after, line.actors], [null, { formula: '=10+4' }, ['Ana', 'Luis']]);
    assert.equal(notebookChanges(store, {}).clutches[0].lines[0].staged, true);
  } finally {
    store.close();
  }
});

test('a count changed and set back to what the sheet holds: the waiting entry is cancelled, nothing is left to save', async () => {
  const { store } = await fixture();
  try {
    const id = clutchId(store, 991);
    await store.staged.stage(
      { requestId: randomUUID(), purpose: 'clutches', edits: [{ id, values: { 'NUMBER OF EGGS': '=12+3' }, expected: { 'NUMBER OF EGGS': 12 } }] },
      ana,
    );
    assert.equal(store.staged.list().items.length, 1);
    const back = await store.staged.stage(
      { requestId: randomUUID(), purpose: 'clutches', edits: [{ id, values: { 'NUMBER OF EGGS': 12 }, expected: { 'NUMBER OF EGGS': '=12+3' } }] },
      ana,
    );
    assert.notEqual(back.status, 'error');
    assert.deepEqual(store.staged.list().items, [], 'both edits cancel out');
  } finally {
    store.close();
  }
});

test('«Guardar en Google Sheets» writes every entry of both tabs as one save; their claims end', async () => {
  const { store, sheets } = await fixture();
  try {
    await stage(store, ana, { creates: [emerged('A1E'), emerged('A2E', { Sex: 'female' })] });
    await store.staged.stage(
      { requestId: randomUUID(), purpose: 'clutches', edits: [{ id: clutchId(store, 990), values: { 'NUMBER OF ADULTS': '=2' }, expected: { 'NUMBER OF ADULTS': null } }] },
      luis,
    );
    const newClutch = { clientId: randomUUID(), module: 'Insectary_stocks', values: { 'CLUTCH NUMBER': 992, SPECIES, 'DATE LAID': 46301, 'NUMBER OF EGGS': 9 } };
    await store.staged.stage({ requestId: randomUUID(), purpose: 'clutches', creates: [newClutch] }, luis);
    // A clutch number someone holds is refused for another new clutch.
    const twice = await store.staged.stage({ requestId: randomUUID(), purpose: 'clutches', creates: [{ ...newClutch, clientId: randomUUID() }] }, ana);
    assert.equal(twice.skipped[0].code, 'CLAIMED');
    assert.equal(twice.skipped[0].next, '993');
    const writes = [];
    const write = sheets.writeBatch.bind(sheets);
    sheets.writeBatch = async w => (writes.push(w.length), write(w));
    const out = await store.staged.flush({ requestId: randomUUID() }, ana);
    assert.equal(out.status, 'done');
    assert.equal(writes.length, 1, 'one write to Google for everything');
    assert.equal(cellIn(sheets, 'Insectary_data', 3, 'Sex'), 'male');
    assert.equal(cellIn(sheets, 'Insectary_data', 4, 'Sex'), 'female');
    assert.equal(cellIn(sheets, 'Insectary_stocks', 2, 'NUMBER OF ADULTS'), '=2');
    assert.equal(cellIn(sheets, 'Insectary_stocks', 4, 'CLUTCH NUMBER'), 992);
    assert.equal(store.staged.list().items.length, 0);
    assert.equal(store.db.prepare('SELECT count(*) n FROM claims').get().n, 0);
    const action = store.db.prepare('SELECT * FROM actions WHERE id = ?').get(out.outbox.actionId);
    assert.equal(action.actor, 'ana');
    assert.match(action.reason, /Ana \(2\).*Luis \(2\)/);
    assert.equal(action.purpose, 'emergidos');
  } finally {
    store.close();
  }
});

test('saved while the workbook is busy: the entries wait, marked as being written, and go when Google answers', async () => {
  const { store, sheets } = await fixture();
  try {
    await stage(store, ana, { creates: [emerged('A1E')] });
    sheets.simulateBusy({ minutes: 1, delayMs: 0 });
    await sheets.probe().catch(() => {});
    const out = await store.staged.flush({ requestId: randomUUID() }, luis, { waitMs: 50 });
    assert.equal(out.status, 'queued');
    assert.equal(store.staged.list().items[0].status, 'sent');
    assert.equal(store.googleState().staged.sent, 1);
    // Not undone or edited while it is being written.
    await assert.rejects(store.staged.remove({ itemId: store.staged.list().items[0].id }, ana), e => e.code === 'STAGED_SENT');
    sheets.simulateBusy({ minutes: 0 });
    await sheets.health.runProbe();
    await until(() => store.staged.list().items.length === 0);
    assert.equal(cellIn(sheets, 'Insectary_data', 3, 'Sex'), 'male');
  } finally {
    store.close();
  }
});

test('an entry the sheet contradicts by the time it is written comes back, with why; the rest is written', async () => {
  const { store, sheets } = await fixture();
  try {
    const id = clutchId(store, 991);
    await store.staged.stage(
      { requestId: randomUUID(), purpose: 'clutches', edits: [{ id, values: { 'NUMBER OF EGGS': '=12+3' }, expected: { 'NUMBER OF EGGS': 12 } }] },
      ana,
    );
    await stage(store, ana, { creates: [emerged('A1E')] });
    // Someone types the count in Google Sheets meanwhile.
    await sheets.externalEdit('Insectary_stocks', 3, { 'NUMBER OF EGGS': 14 });
    await store.sync({ sheets: ['Insectary_stocks'] });
    const out = await store.staged.flush({ requestId: randomUUID() }, ana);
    assert.equal(out.status, 'done');
    const left = store.staged.list().items;
    assert.equal(left.length, 1);
    assert.equal(left[0].kind, 'edit');
    assert.equal(left[0].status, 'staged');
    assert.equal(left[0].error.code, 'EXTERNAL_CONFLICT');
    assert.equal(cellIn(sheets, 'Insectary_stocks', 3, 'NUMBER OF EGGS'), 14, 'never overwritten');
    assert.equal(cellIn(sheets, 'Insectary_data', 3, 'Sex'), 'male');
  } finally {
    store.close();
  }
});

test('a row entered here can be changed or undone before it is saved; undoing frees its ID', async () => {
  const { store } = await fixture();
  try {
    const row = emerged('A1E');
    const out = await stage(store, ana, { creates: [row] });
    const rowId = out.created[0].recordId;
    const edited = await stage(store, luis, { edits: [{ id: rowId, values: { Sex: 'female' }, expected: { Sex: 'male' } }] });
    assert.equal(edited.status, 'staged');
    let [item] = store.staged.list().items;
    assert.equal(item.values.Sex, 'female');
    assert.equal(item.editedByName, 'Luis');
    // Seen before Luis's change: refused.
    const stale = await stage(store, ana, { edits: [{ id: rowId, values: { Sex: 'NA' }, expected: { Sex: 'male' } }] });
    assert.equal(stale.skipped[0].code, 'EXTERNAL_CONFLICT');
    // Its ID changed to one someone else holds: refused, the row keeps its own.
    await stage(store, luis, { creates: [emerged('A2E')] });
    const taken = await stage(store, ana, { edits: [{ id: rowId, values: { Insectary_ID: 'A2E' }, expected: { Insectary_ID: 'A1E' } }] });
    assert.equal(taken.skipped[0].code, 'CLAIMED');
    [item] = store.staged.list().items;
    assert.equal(item.values.Insectary_ID, 'A1E');
    await store.staged.remove({ entryId: item.entryId }, luis);
    assert.equal(store.staged.list().items.length, 1);
    assert.equal(idSuggestions(store, { kind: 'insectary', count: 1 }).sequence[0], 'A1E', 'free again');
  } finally {
    store.close();
  }
});

test("the assistant's query copy has the entries kept in the app, a row per cell", async () => {
  const { store } = await fixture();
  const dir = mkdtempSync(join(tmpdir(), 'staged-copy-'));
  try {
    await stage(store, ana, { creates: [emerged('A1E')] });
    const out = join(dir, 'sheets.sqlite');
    buildCopy(store.db, out);
    const copy = new DatabaseSync(out, { readOnly: true });
    const rows = copy
      .prepare("SELECT who, tab, kind, sheet, id_label, field, value, status FROM staged WHERE field IN ('Insectary_ID', 'Intro2Insectary_date') ORDER BY field")
      .all();
    copy.close();
    assert.deepEqual(
      rows.map(r => ({ ...r })),
      [
        { who: 'Ana', tab: 'Emergidos', kind: 'new row', sheet: 'Insectary_data', id_label: 'A1E', field: 'Insectary_ID', value: 'A1E', status: 'staged' },
        { who: 'Ana', tab: 'Emergidos', kind: 'new row', sheet: 'Insectary_data', id_label: 'A1E', field: 'Intro2Insectary_date', value: '2026-10-05', status: 'staged' },
      ],
    );
  } finally {
    store.close();
  }
});

test('the Emergidos cards are kept all or nothing: one butterfly refused keeps every card and count back', async () => {
  const { store } = await fixture();
  try {
    await stage(store, luis, { creates: [emerged('A2E')] });
    const body = {
      requestId: randomUUID(),
      purpose: 'emergidos',
      partial: false,
      creates: [emerged('A1E'), emerged('A2E')],
      edits: [{ id: clutchId(store, 990), values: { 'NUMBER OF ADULTS': '=2' }, expected: { 'NUMBER OF ADULTS': null } }],
    };
    await assert.rejects(store.staged.stage(body, ana), e => e.code === 'BATCH_CONFLICT' && e.details.items[0].code === 'CLAIMED');
    assert.equal(store.staged.list().items.length, 1, "only Luis's");
    assert.equal(idSuggestions(store, { kind: 'insectary', count: 1 }).sequence[0], 'A1E', 'A1E was not taken');
  } finally {
    store.close();
  }
});
