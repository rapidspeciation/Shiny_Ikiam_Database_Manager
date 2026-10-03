import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { applyBatch } from '../server/batch.mjs';
import {
  addClutchCheck,
  addClutchEvent,
  clutchDay,
  clutchEvents,
  clutchState,
  ecuadorDay,
  notebookChanges,
  notebookUpTo,
  removeClutchEvent,
  setClutchSettings,
  setNotebookUpTo,
  tally,
} from '../server/clutches.mjs';

const ana = { id: 'u-ana', username: 'ana', displayName: 'Ana Pérez', role: 'editor' };
const bob = { id: 'u-bob', username: 'bob', displayName: 'Bob Díaz', role: 'editor' };
const boss = { id: 'u-boss', username: 'boss', displayName: 'Admin', role: 'admin' };
const BASE = Date.parse('2026-10-02T15:00:00.000Z');
const time = minutes => new Date(BASE + minutes * 60_000).toISOString();
const code = async (promise, expected) => {
  try {
    await promise();
  } catch (e) {
    assert.equal(e.code, expected);
    return;
  }
  assert.fail(`expected ${expected}`);
};

async function fixture() {
  const stock = (row, clutch, extra = {}) => ({ row, values: { 'CLUTCH NUMBER': clutch, SPECIES: 'Mechanitis lysimnia', ...extra } });
  const young = (row, id, cause) => ({
    row,
    values: { Insectary_ID: id, 'CLUTCH NUMBER': 1012, LIFESTAGE: '3rd instar larva', Death_cause: cause, Death_date: 46297, Preservation_date: 46297 },
  });
  const sheets = new LocalSheets({
    Insectary_stocks: [
      stock(10, 999),
      stock(11, 1012, { 'NUMBER OF EGGS': { formula: '=14+12' }, 'NUMBER OF LARVAE': { formula: '=3+5' } }),
      stock(12, 1013, { 'NUMBER OF EGGS': { formula: '=15+1' } }),
      stock(13, '1012(2)'),
    ],
    Insectary_data: [young(2, 'M0E', 'Killed_Preserved'), young(3, 'M1E', 'Killed_Preserved'), young(4, 'M2E', 'Other')],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_stocks', 'Insectary_data'] });
  for (const u of [ana, bob, boss])
    store.db
      .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES(?,?,?,?,'s','h',1,'2026-01-01')")
      .run(u.id, u.username, u.displayName, u.role);
  const row = r => store.getRecordBySheetRow('Insectary_stocks', r);
  return { store, sheets, row };
}

test('checks: checked or needs verification, with a short reason; the latest counts', async () => {
  const { store, row } = await fixture();
  const c1012 = row(11).id;
  const first = addClutchCheck(store, { requestId: randomUUID(), recordId: c1012, state: 'verify', note: "  couldn't find all   larvae " }, ana);
  assert.deepEqual([first.check.state, first.check.note, first.check.name], ['verify', "couldn't find all larvae", 'Ana Pérez']);
  const second = addClutchCheck(store, { requestId: randomUUID(), recordId: c1012 }, bob);
  assert.deepEqual([second.check.state, second.check.note], ['checked', null]);
  const day = clutchDay(store);
  assert.deepEqual(
    day.checks.map(c => [c.name, c.state]),
    [
      ['Ana Pérez', 'verify'],
      ['Bob Díaz', 'checked'],
    ],
  );
  assert.throws(() => addClutchCheck(store, { requestId: randomUUID(), recordId: c1012, state: 'maybe' }, ana), { code: 'INVALID_STATE' });
  assert.throws(() => addClutchCheck(store, { requestId: randomUUID(), recordId: c1012, note: 'x'.repeat(201) }, ana), { code: 'INVALID_NOTE' });
});

test('events: per clutch and day, with the eggs and larvae registered in Insectary_data counted once', async () => {
  const { store, row } = await fixture();
  const c1012 = row(11).id;
  const add = (body, who = ana) => addClutchEvent(store, { requestId: randomUUID(), recordId: c1012, ...body }, who);
  const hatched = add({ stage: 'larva', kind: 'hatched', count: 4 });
  assert.deepEqual([hatched.event.clutch, hatched.event.day, hatched.event.count, hatched.event.ids], ['1012', ecuadorDay(), 4, []]);
  add({ stage: 'larva', kind: 'died', count: 1 }, bob);
  add({ stage: 'larva', kind: 'disappeared', count: 2 });
  // Preserved with the IDs on their tubes: M0E is also its Insectary_data row, so it counts once.
  const preserved = add({ stage: 'larva', kind: 'preserved', count: 2, ids: ['m0e', ' N9E '], note: 'for life history' });
  assert.deepEqual(preserved.event.ids, ['M0E', 'N9E']);
  // The same request again is the same event.
  const pupated = { requestId: 'dup-request-1', recordId: c1012, stage: 'pupa', kind: 'pupated', count: 1 };
  const once = addClutchEvent(store, pupated, ana);
  const twice = addClutchEvent(store, pupated, ana);
  assert.deepEqual([twice.duplicate, twice.event.id], [true, once.event.id]);

  const all = clutchEvents(store, { recordId: c1012 });
  assert.equal(all.events.length, 5);
  assert.deepEqual(
    all.young.map(y => [y.id, y.stage, y.kind, y.day]),
    [
      ['M0E', 'larva', 'preserved', '2026-10-02'],
      ['M1E', 'larva', 'preserved', '2026-10-02'],
      ['M2E', 'larva', 'died', '2026-10-02'],
    ],
  );
  // Larvae: 4 hatched; died 1 + M2E found dead; preserved 2 (M0E, N9E) + M1E; disappeared 2.
  assert.deepEqual(all.tally, { larva: { gained: 4, died: 2, disappeared: 2, preserved: 3 }, pupa: { gained: 1, died: 0, disappeared: 0, preserved: 0 } });
  assert.deepEqual(clutchState(store).tallies[c1012], all.tally);
  assert.equal(clutchDay(store).events.length, 5);
  assert.deepEqual(tally([{ stage: 'adult', kind: 'died', count: 3 }]), {}, 'an event a stage does not have is not counted');

  // Refused: a kind the stage has not, counts out of range, more IDs than the count, a day to come, another sheet.
  await code(() => add({ stage: 'adult', kind: 'died', count: 1 }), 'INVALID_KIND');
  await code(() => add({ stage: 'imago', kind: 'died', count: 1 }), 'INVALID_STAGE');
  await code(() => add({ stage: 'larva', kind: 'died', count: 0 }), 'INVALID_COUNT');
  await code(() => add({ stage: 'larva', kind: 'died', count: 1.5 }), 'INVALID_COUNT');
  await code(() => add({ stage: 'larva', kind: 'preserved', count: 1, ids: ['A0A', 'A1A'] }), 'INVALID_IDS');
  await code(() => add({ stage: 'larva', kind: 'preserved', count: 1, ids: ['not an id'] }), 'INVALID_IDS');
  await code(() => add({ stage: 'larva', kind: 'died', count: 1, day: '2999-01-01' }), 'INVALID_DAY');
  await code(
    () => addClutchEvent(store, { requestId: randomUUID(), recordId: store.getRecordBySheetRow('Insectary_data', 2).id, stage: 'larva', kind: 'died', count: 1 }, ana),
    'RECORD_NOT_FOUND',
  );
  // An earlier day is fine (the person remembers yesterday's).
  assert.equal(add({ stage: 'egg', kind: 'died', count: 3, day: '2026-10-01' }).event.day, '2026-10-01');

  // Taken back by its author, or by a reviewer or admin; not by someone else.
  assert.throws(() => removeClutchEvent(store, hatched.event.id, bob), { code: 'FORBIDDEN' });
  assert.deepEqual(removeClutchEvent(store, hatched.event.id, ana), { removed: hatched.event.id });
  assert.deepEqual(removeClutchEvent(store, preserved.event.id, boss), { removed: preserved.event.id });
  assert.throws(() => removeClutchEvent(store, preserved.event.id, boss), { code: 'EVENT_NOT_FOUND' });
  // Without the event naming M0E, its Insectary_data row counts by itself.
  assert.equal(clutchEvents(store, { recordId: c1012 }).tally.larva.preserved, 2);
});

test('the setting: preserved larvae taken off NUMBER OF LARVAE by default; only an admin changes it', async () => {
  const { store } = await fixture();
  assert.deepEqual(clutchState(store).settings, { subtractPreserved: true });
  assert.throws(() => setClutchSettings(store, { subtractPreserved: false }, ana), { code: 'FORBIDDEN' });
  assert.throws(() => setClutchSettings(store, { subtractPreserved: 'no' }, boss), { code: 'INVALID_SETTING' });
  assert.deepEqual(setClutchSettings(store, { subtractPreserved: false }, boss), { subtractPreserved: false });
  assert.deepEqual(clutchState(store).settings, { subtractPreserved: false });
  assert.deepEqual(setClutchSettings(store, { subtractPreserved: true }, boss), { subtractPreserved: true });
});

test("the notebook's list: the app's changes in the notebook's order, without what came from the notebook", async () => {
  const { store, sheets, row } = await fixture();
  // The assistant's proposals (assistant.mjs): created here as far as this list needs them.
  store.db.exec('CREATE TABLE IF NOT EXISTS ai_proposals(id TEXT PRIMARY KEY, owner_id TEXT, reason TEXT, page_json TEXT, applied_at TEXT)');
  const [c999, c1012, c1013, c1012b] = [row(10).id, row(11).id, row(12).id, row(13).id];
  let n = 0;
  const at = (id, minutes) => store.db.prepare('UPDATE actions SET created_at=? WHERE id=?').run(time(minutes), id);
  const save = async (who, minutes, edits, options = {}) => {
    const out = await applyBatch(store, { requestId: `notebook-${++n}`, purpose: 'clutches', edits }, who, options);
    at(out.action.id, minutes);
    return out.action.id;
  };
  const propose = async (who, minutes, reason, edits) => {
    const out = await store.applyProposal(
      edits.map(e => ({ recordId: e.id, values: e.values })),
      { user: who, requestId: `proposal-${++n}`, reason },
    );
    at(out.action.id, minutes);
    return out.action.id;
  };
  // 1012: Ana counts 4 more larvae; the notebook's photos add =+2 and fix the eggs; Bob takes 1 off.
  await save(ana, 0, [{ id: c1012, values: { 'NUMBER OF LARVAE': '=3+5+4' } }]);
  await propose(ana, 10, 'Cuaderno Posturas (Insectary_stocks): posturas 1012', [
    { id: c1012, values: { 'NUMBER OF LARVAE': '=3+5+4+2', 'NUMBER OF EGGS': '=14+12+1' } },
  ]);
  await save(bob, 20, [{ id: c1012, values: { 'NUMBER OF LARVAE': '=3+5+4+2-1', NOTES: '2/10/26 BD: 1 larva dead' } }]);
  // 1013: a proposal saved with the person's own reason, written from a notebook proposal: left out.
  const chat = await propose(bob, 30, 'Confirmado en el chat', [{ id: c1013, values: { 'NUMBER OF EGGS': '=15+7' } }]);
  store.db
    .prepare('INSERT INTO ai_proposals(id, owner_id, reason, page_json, applied_at) VALUES(?,?,?,?,?)')
    .run('p1', bob.id, 'Clutch 1013: restore eggs =15+1 (notebook)', null, new Date(Date.parse(time(30)) + 900).toISOString());
  // 999: an assistant change not from the notebook (kept); 1012(2): a change and its undo (nothing left).
  await propose(ana, 40, 'Generation for the F1 clutches', [{ id: c999, values: { Generation: 'F1' } }]);
  const species = await save(ana, 50, [{ id: c1012b, values: { SPECIES: 'Mechanitis messenoides' } }]);
  await save(ana, 55, [{ id: c1012b, values: { SPECIES: 'Mechanitis lysimnia', 'NUMBER OF EGGS': '=9' } }], { source: 'undo', reverses: species });
  // Typed in Google Sheets (as a rule the notebook's own transcription): left out unless asked for.
  await sheets.externalEdit('Insectary_stocks', 12, { NOTES: 'Some eggs dry' });
  await store.refreshRows('Insectary_stocks', [12]);
  const sync = store.db.prepare("SELECT id FROM actions WHERE source='sheet_reconciliation' ORDER BY rowid DESC LIMIT 1").get().id;
  at(sync, 60);
  // An app-only event, in the same period.
  const event = addClutchEvent(store, { requestId: randomUUID(), recordId: c1012, stage: 'larva', kind: 'died', count: 1 }, bob).event;
  store.db.prepare('UPDATE clutch_events SET created_at=? WHERE id=?').run(time(21), event.id);

  const list = notebookChanges(store, { from: time(-1), to: time(120) });
  assert.deepEqual(list.excluded, { notebook: 3, sheets: 1 });
  assert.deepEqual(
    list.clutches.map(c => c.clutch),
    ['999', '1012', '1012(2)'],
    'clutch number ascending, batches under their number',
  );
  const c = list.clutches.find(x => x.clutch === '1012');
  assert.deepEqual(
    c.lines.map(l => [l.field, l.before, l.after, l.actors]),
    [
      // The notebook's +2 is never shown as new: the runs before and after it are apart.
      ['NUMBER OF LARVAE', { formula: '=3+5' }, { formula: '=3+5+4' }, ['Ana Pérez']],
      ['NUMBER OF LARVAE', { formula: '=3+5+4+2' }, { formula: '=3+5+4+2-1' }, ['Bob Díaz']],
      ['NOTES', null, '2/10/26 BD: 1 larva dead', ['Bob Díaz']],
    ],
  );
  assert.deepEqual(c.events.map(e => [e.kind, e.count, e.name]), [['died', 1, 'Bob Díaz']]);
  assert.deepEqual(list.clutches.find(x => x.clutch === '999').lines.map(l => [l.field, l.after, l.sources]), [['Generation', 'F1', ['assistant']]]);
  // The undo took the species back: only the eggs it also wrote remain.
  assert.deepEqual(list.clutches.find(x => x.clutch === '1012(2)').lines.map(l => l.field), ['NUMBER OF EGGS']);
  assert.ok(chat);

  // With Google Sheets' edits.
  const withSheets = notebookChanges(store, { from: time(-1), to: time(120), sheets: '1' });
  assert.deepEqual(
    withSheets.clutches.find(x => x.clutch === '1013').lines.map(l => [l.field, l.after, l.actors]),
    [['NOTES', 'Some eggs dry', ['Google Sheets']]],
  );

  // "The notebook is up to date until …": remembered for everyone, and the list starts there by default.
  assert.equal(notebookUpTo(store), null);
  const { upTo } = setNotebookUpTo(store, { at: time(15) }, ana);
  assert.deepEqual([upTo.at, upTo.name], [time(15), 'Ana Pérez']);
  assert.deepEqual(notebookUpTo(store).at, time(15));
  const since = notebookChanges(store, { to: time(120) });
  assert.equal(since.from, time(15));
  assert.deepEqual(
    since.clutches.find(x => x.clutch === '1012').lines.map(l => l.field),
    ['NUMBER OF LARVAE', 'NOTES'],
  );
  assert.throws(() => setNotebookUpTo(store, { at: 'tomorrow' }, ana), { code: 'INVALID_DATE' });
  assert.throws(() => setNotebookUpTo(store, { at: '2999-01-01T00:00:00Z' }, ana), { code: 'INVALID_DATE' });
  assert.throws(() => notebookChanges(store, { from: time(10), to: time(5) }), { code: 'INVALID_RANGE' });
});
