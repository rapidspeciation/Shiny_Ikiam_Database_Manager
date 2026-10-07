import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { simpleSum, sumGroups } from '../server/schema.mjs';
import { correctedTerms, readValue, sumTerms } from '../server/notebook.mjs';
import { evaluateFormula } from '../server/formula.mjs';
import { sumTotal } from '../server/staged.mjs';
import {
  addClutchEvent,
  addClutchStep,
  clutchDay,
  clutchEvents,
  ecuadorDay,
  notebookChanges,
  renameClutchGroup,
  setClutchNote,
  undoClutchStep,
  updateClutchEvent,
} from '../server/clutches.mjs';
import { createClutchPhotos } from '../server/clutch-photos.mjs';

const ana = { id: 'u-ana', username: 'ana', displayName: 'Ana Pérez', role: 'editor' };
const bob = { id: 'u-bob', username: 'bob', displayName: 'Bob Díaz', role: 'editor' };

async function fixture() {
  const sheets = new LocalSheets({
    Insectary_stocks: [
      { row: 10, values: { 'CLUTCH NUMBER': 1020, SPECIES: 'Mechanitis lysimnia', 'NUMBER OF EGGS': { formula: '=10+8' } } },
      { row: 11, values: { 'CLUTCH NUMBER': 1021, SPECIES: 'Melinaea menophilus', 'NUMBER OF LARVAE': { formula: '=27-2-11-3' } } },
    ],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_stocks'] });
  for (const u of [ana, bob])
    store.db
      .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES(?,?,?,?,'s','h',1,'2026-01-01')")
      .run(u.id, u.username, u.displayName, u.role);
  return { store, id: row => store.getRecordBySheetRow('Insectary_stocks', row).id };
}
const step = (store, body, who = ana) => addClutchStep(store, { requestId: randomUUID(), ...body }, who).step;

test('sums in groups: one parenthesized sub-sum per group, read and added up everywhere', () => {
  assert.equal(simpleSum('=(6-2)+(5+3)'), '=(6-2)+(5+3)');
  assert.equal(simpleSum('( 6 - 2 ) + ( 5 + 3 )'), '=(6-2)+(5+3)');
  assert.equal(simpleSum('=(6-2)-(5)'), null, 'groups are added, never subtracted');
  assert.equal(simpleSum('=((6))'), null);
  assert.deepEqual(sumGroups('=(6-2)+(5+3)'), [
    [6, -2],
    [5, 3],
  ]);
  assert.deepEqual(sumGroups('=27-2-11-3'), [[27, -2, -11, -3]]);
  // The notebook's readers: the terms in order, and a count in groups kept as written.
  assert.deepEqual(sumTerms('(6-2)+(5+3)'), [6, -2, 5, 3]);
  assert.deepEqual(correctedTerms('(6-2)+(5+3)'), [6, -2, 5, 3]);
  assert.deepEqual(readValue('NUMBER OF LARVAE', '(6-2)+(5+3)', { year: 2026, sheet: 'Insectary_stocks' }), { value: '=(6-2)+(5+3)' });
  // What the sheet shows, and what an entry kept in the app adds up to.
  assert.equal(evaluateFormula('=(6-2-2)+(2)+(5+3)', 2, { cell: () => null }), 12);
  assert.equal(sumTotal('=(6-2)+(5+3)'), 12);
});

test('steps: eggs in two groups hatch into larvae groups that mirror them; terms per group; lineage', async () => {
  const { store, id } = await fixture();
  const c = id(10);
  // The eggs counted on two plants: two groups (=(10)+(8) in the sheet), each its laid event.
  const eggs = step(store, {
    recordId: c,
    groups: [{ field: 'NUMBER OF EGGS', list: [{ key: 'a', label: 'A' }, { key: 'b', label: 'plant 2' }] }],
    events: [
      { stage: 'egg', kind: 'laid', count: 10, term: 10, groupKey: 'a' },
      { stage: 'egg', kind: 'laid', count: 8, term: 8, groupKey: 'b' },
    ],
  });
  const [A, B] = eggs.groups.filter(g => g.field === 'NUMBER OF EGGS');
  assert.deepEqual([A.label, A.position, B.label, B.position], ['A', 0, 'plant 2', 1]);
  assert.deepEqual(eggs.events.map(e => [e.groupId, e.term, e.field]), [
    [A.id, 10, 'NUMBER OF EGGS'],
    [B.id, 8, 'NUMBER OF EGGS'],
  ]);
  // Hatched today: 8 of A (2 did not hatch), all 8 of B.
  const hatch = step(store, {
    recordId: c,
    groups: [{ field: 'NUMBER OF LARVAE', list: [{ key: 'la', label: 'A', originId: A.id }, { key: 'lb', label: 'plant 2', originId: B.id }] }],
    events: [
      { stage: 'larva', kind: 'hatched', count: 8, term: 8, groupKey: 'la', fromGroupId: A.id },
      { stage: 'egg', kind: 'not_hatched', count: 2, term: null, groupId: A.id },
      { stage: 'larva', kind: 'hatched', count: 8, term: 8, groupKey: 'lb', fromGroupId: B.id },
    ],
  });
  const larvae = hatch.groups.filter(g => g.field === 'NUMBER OF LARVAE');
  assert.deepEqual(larvae.map(g => [g.label, g.originId]), [
    ['A', A.id],
    ['plant 2', B.id],
  ]);
  // The terms of each group, from its events: what the formula =(8)+(8) holds.
  const all = clutchEvents(store, { recordId: c });
  const termsOf = groupId => all.events.filter(e => e.groupId === groupId && e.term !== null).map(e => e.term);
  assert.deepEqual(larvae.map(g => termsOf(g.id)), [[8], [8]]);
  assert.deepEqual(all.tally.egg, { gained: 18, died: 0, disappeared: 0, preserved: 0, notHatched: 2 });
  assert.equal(all.groups.length, 4);
  // A group's label changed later.
  assert.equal(renameClutchGroup(store, larvae[1].id, { label: '  box  B ' }).group.label, 'box B');
  // Refused: a group of another stage, an origin that is not the stage before, a key used twice, adults in groups.
  assert.throws(() => step(store, { recordId: c, events: [{ stage: 'larva', kind: 'died', count: 1, term: -1, groupId: A.id }] }), { code: 'GROUP_NOT_FOUND' });
  assert.throws(
    () => step(store, { recordId: c, groups: [{ field: 'NUMBER OF PUPA', list: [{ key: 'p', label: 'A', originId: A.id }] }] }),
    { code: 'GROUP_NOT_FOUND' },
  );
  assert.throws(() => step(store, { recordId: c, groups: [{ field: 'NUMBER OF LARVAE', list: [{ key: 'x' }, { key: 'x' }] }] }), { code: 'INVALID_GROUPS' });
  assert.throws(() => step(store, { recordId: c, groups: [{ field: 'NUMBER OF ADULTS', list: [] }] }), { code: 'INVALID_FIELD' });
  assert.throws(() => step(store, { recordId: c }), { code: 'INVALID_STEP' });
});

test('the terms an event may write: a gain +N, a loss −N or none, a correction or a transfer ±N; a day not known', async () => {
  const { store, id } = await fixture();
  const c = id(11);
  const add = body => addClutchEvent(store, { requestId: randomUUID(), recordId: c, ...body }, ana);
  assert.equal(add({ stage: 'larva', kind: 'correction', count: 1, term: -1, note: 'counted 10, not 11' }).event.term, -1);
  assert.throws(() => add({ stage: 'larva', kind: 'correction', count: 1 }), { code: 'INVALID_TERM' });
  assert.throws(() => add({ stage: 'larva', kind: 'correction', count: 2, term: -1 }), { code: 'INVALID_TERM' });
  assert.throws(() => add({ stage: 'larva', kind: 'died', count: 2, term: 2 }), { code: 'INVALID_TERM' });
  assert.throws(() => add({ stage: 'larva', kind: 'hatched', count: 2, term: -2 }), { code: 'INVALID_TERM' });
  assert.throws(() => add({ stage: 'larva', kind: 'not_hatched', count: 2 }), { code: 'INVALID_KIND' });
  assert.equal(add({ stage: 'larva', kind: 'preserved', count: 2, term: null }).event.term, null);
  // Larvae found already big: the hatch day is NA (the event keeps the day it was recorded).
  const big = add({ stage: 'larva', kind: 'hatched', count: 9, term: 9, dayKnown: false }).event;
  assert.deepEqual([big.dayKnown, big.day, big.field], [false, ecuadorDay(), 'NUMBER OF LARVAE']);
  // What is said of an event, corrected: its day, the cause of a loss, a note; not a gain into a loss.
  const died = add({ stage: 'larva', kind: 'died', count: 2, term: -2 }).event;
  const fixed = updateClutchEvent(store, died.id, { kind: 'disappeared', day: '2026-10-01', note: 'plant 2' }, ana).event;
  assert.deepEqual([fixed.kind, fixed.day, fixed.note, fixed.term], ['disappeared', '2026-10-01', 'plant 2', -2]);
  assert.equal(updateClutchEvent(store, big.id, { dayKnown: true, day: '2026-09-30' }, ana).event.dayKnown, true);
  assert.throws(() => updateClutchEvent(store, big.id, { kind: 'died' }, ana), { code: 'INVALID_KIND' });
  assert.throws(() => updateClutchEvent(store, died.id, { note: 'x' }, bob), { code: 'FORBIDDEN' });
});

test('regrouping as transfers: the first groups wrap the earlier terms; undo puts everything back', async () => {
  const { store, id } = await fixture();
  const c = id(11);
  // Before any group: two events of the plain sum =27-2-11-3.
  const before = step(store, {
    recordId: c,
    events: [
      { stage: 'larva', kind: 'died', count: 2, term: -2 },
      { stage: 'larva', kind: 'disappeared', count: 3, term: -3 },
    ],
  });
  assert.ok(before.events.every(e => e.groupId === null));
  // «6+5»: =(27-2-11-3-5)+(5): two transfer events, the groups A and B.
  const regroup = step(store, {
    recordId: c,
    groups: [{ field: 'NUMBER OF LARVAE', list: [{ key: 'a', label: 'A' }, { key: 'b', label: 'B' }] }],
    events: [
      { stage: 'larva', kind: 'transfer', count: 5, term: -5, groupKey: 'a', fromGroupKey: 'b' },
      { stage: 'larva', kind: 'transfer', count: 5, term: 5, groupKey: 'b', fromGroupKey: 'a' },
    ],
    log: [{ kind: 'regroup', field: 'NUMBER OF LARVAE', before: '=27-2-11-3', after: '=(27-2-11-3-5)+(5)' }],
  });
  const [A, B] = regroup.groups;
  // The events recorded before the groups are the first group's now.
  let all = clutchEvents(store, { recordId: c });
  assert.deepEqual(
    before.events.map(e => all.events.find(x => x.id === e.id).groupId),
    [A.id, A.id],
  );
  const sums = Object.fromEntries([A, B].map(g => [g.label, all.events.filter(e => e.groupId === g.id).reduce((n, e) => n + (e.term ?? 0), 0)]));
  assert.deepEqual(sums, { A: -10, B: 5 }, 'the events in A: −2 −3 −5 (27 and −11 came from the notebook)');
  assert.deepEqual(all.log.map(l => [l.kind, l.before, l.after]), [['regroup', '=27-2-11-3', '=(27-2-11-3-5)+(5)']]);
  // B merged back into A: B's parentheses go (the group ends), A gets +5.
  const merge = step(store, {
    recordId: c,
    groups: [{ field: 'NUMBER OF LARVAE', list: [{ id: A.id, label: 'A' }] }],
    events: [{ stage: 'larva', kind: 'transfer', count: 5, term: 5, groupId: A.id, fromGroupId: B.id }],
  });
  assert.deepEqual(merge.groups.map(g => g.id), [A.id]);
  assert.ok(clutchEvents(store, { recordId: c }).groups.find(g => g.id === B.id).endedAt);
  // Undo the merge: B is open again at its place; then the regrouping: no groups, the events loose again.
  assert.throws(() => undoClutchStep(store, merge.id, bob), { code: 'FORBIDDEN' });
  undoClutchStep(store, merge.id, ana);
  assert.deepEqual(clutchEvents(store, { recordId: c }).groups.filter(g => !g.endedAt).map(g => [g.label, g.position]), [
    ['A', 0],
    ['B', 1],
  ]);
  undoClutchStep(store, regroup.id, ana);
  all = clutchEvents(store, { recordId: c });
  assert.deepEqual(all.groups, []);
  assert.deepEqual(all.log, []);
  assert.deepEqual(all.events.map(e => [e.kind, e.groupId]), [
    ['died', null],
    ['disappeared', null],
  ]);
  // The same request again is the same step; transfers are not listed for the notebook.
  const body = { requestId: 'same-step', recordId: c, events: [{ stage: 'larva', kind: 'hatched', count: 1, term: 1 }] };
  const once = addClutchStep(store, body, ana);
  assert.deepEqual([addClutchStep(store, body, ana).duplicate, addClutchStep(store, body, ana).step.id], [true, once.step.id]);
  assert.ok(notebookChanges(store, {}).clutches.flatMap(x => x.events).every(e => e.kind !== 'transfer'));
});

test("photos of a group; the day's note (one per clutch and day, only in the app)", async () => {
  const { store, id } = await fixture();
  const c = id(10);
  const g = step(store, { recordId: c, groups: [{ field: 'NUMBER OF EGGS', list: [{ key: 'a', label: 'A' }] }] }).groups[0];
  const photos = createClutchPhotos(store);
  store.db
    .prepare(
      "INSERT INTO clutch_photos(id, request_id, record_id, clutch, day, actor, width, height, bytes, thumb_bytes, file, created_at) VALUES('p1','r1',?,'1020',?,?,10,10,1,1,'x','2026-10-06')",
    )
    .run(c, ecuadorDay(), ana.id);
  assert.equal(photos.update('p1', { groupId: g.id }, ana).photo.groupId, g.id);
  assert.equal(clutchEvents(store, { recordId: c }).photos[0].groupId, g.id);
  assert.throws(() => photos.update('p1', { groupId: 'nope' }, ana), { code: 'GROUP_NOT_FOUND' });

  const first = setClutchNote(store, { recordId: c, text: 'Larvae on the second plant look small.\nCheck tomorrow.' }, ana).note;
  assert.deepEqual([first.day, first.name], [ecuadorDay(), 'Ana Pérez']);
  const again = setClutchNote(store, { recordId: c, text: 'Larvae look small. ' + 'x'.repeat(1000) }, bob).note;
  assert.deepEqual([again.id, again.updatedName, again.text.length > 1000], [first.id, 'Bob Díaz', true]);
  assert.equal(clutchDay(store).notes.length, 1);
  assert.equal(clutchEvents(store, { recordId: c }).notes[0].id, first.id);
  assert.deepEqual(setClutchNote(store, { recordId: c, text: '  ' }, ana), { note: null, removed: first.id });
  assert.equal(clutchDay(store).notes.length, 0);
});
