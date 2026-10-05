// Censo: everyone marks the butterflies of one species seen alive; finishing keeps the ones
// not seen as disappeared (Death_cause Disappearance, the census day) in the app until
// «Guardar en Google Sheets», like Emergidos and Clutches.
import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { mkdtempSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { LocalSheets } from '../server/sheets.mjs';
import { Store } from '../server/store.mjs';
import {
  DISAPPEARED,
  addMark,
  aliveButterflies,
  aliveSpecies,
  cancelCensus,
  censusDetail,
  censusOverview,
  finishCensus,
  learnedLookAlikes,
  removeMark,
  reopenCensus,
  serialOf,
  setCensusNotebook,
  startCensus,
  updateMark,
} from '../server/census.mjs';

const ana = { id: 'ana', username: 'ana', displayName: 'Ana', role: 'editor' };
const luis = { id: 'luis', username: 'luis', displayName: 'Luis', role: 'editor' };
const viewer = { id: 'vera', username: 'vera', displayName: 'Vera', role: 'viewer' };
const POLY = 'Mechanitis polymnia proceriformis';
const LYS = 'Mechanitis lysimnia';
const DAY = '2026-10-05';
const SERIAL = serialOf(DAY);

function seed() {
  return {
    Insectary_data: [
      { row: 2, values: { Insectary_ID: 'A1B', SPECIES: POLY, Sex: 'female', 'CLUTCH NUMBER': 990, Intro2Insectary_date: 46280, Wild_Reared: 'Reared' } },
      { row: 3, values: { Insectary_ID: 'A2B', SPECIES: POLY, Sex: 'male', 'CLUTCH NUMBER': 990, Intro2Insectary_date: 46280, Wild_Reared: 'Reared' } },
      { row: 4, values: { Insectary_ID: 'A3B', SPECIES: POLY, Sex: 'male', Intro2Insectary_date: 46290, Wild_Reared: 'Wild-caught', CAM_ID: 'CAM000500' } },
      // Dead: a date, only a cause, a date "NA" (neither).
      { row: 5, values: { Insectary_ID: 'A4B', SPECIES: POLY, Sex: 'male', Death_date: 46295, Death_cause: 'Spider' } },
      { row: 6, values: { Insectary_ID: 'A5B', SPECIES: POLY, Sex: 'female', Death_cause: 'Eaten' } },
      { row: 7, values: { Insectary_ID: 'A6B', SPECIES: POLY, Sex: 'female', Death_date: 'NA' } },
      // Another species, alive.
      { row: 8, values: { Insectary_ID: 'A7B', SPECIES: LYS, Sex: 'female', Intro2Insectary_date: 46291, Wild_Reared: 'Wild-caught' } },
      { row: 9, values: { Insectary_ID: 'A8B', SPECIES: POLY, Sex: 'female', Intro2Insectary_date: 46292, Wild_Reared: 'Reared' } },
      // Pre-made rows (only the ID): not butterflies yet.
      { row: 10, values: { Insectary_ID: 'A9B' } },
      { row: 11, values: { Insectary_ID: 'A0C' } },
    ],
    Insectary_stocks: [{ row: 2, values: { 'CLUTCH NUMBER': 990, SPECIES: POLY, 'DATE LAID': 46250, 'NUMBER OF EGGS': 20 } }],
  };
}

async function fixture(databasePath = ':memory:', sheets = new LocalSheets(seed(), { health: { probeMs: 20 } })) {
  const store = new Store({ localMode: true, databasePath }, { sheets });
  await store.sync({ sheets: ['Insectary_data', 'Insectary_stocks'] });
  for (const u of [ana, luis, viewer])
    store.db
      .prepare("INSERT OR IGNORE INTO users(id,username,display_name,role,salt,password_hash,created_at) VALUES(?,?,?,?,'s','h',?)")
      .run(u.id, u.username, u.displayName, u.role, new Date().toISOString());
  return { store, sheets };
}
const recordOf = (store, id) =>
  store.db
    .prepare("SELECT id FROM records WHERE sheet='Insectary_data' AND observed=1 AND json_extract(values_json,'$.Insectary_ID')=?")
    .get(id).id;
const start = (store, user, species = POLY, day = DAY) => startCensus(store, { requestId: randomUUID(), species, day }, user);
const mark = (store, census, user, body) => addMark(store, census, { requestId: randomUUID(), ...body }, user);
/** What the Censo tab sends for a butterfly not seen: Muertes' cells of a death not preserved (lib/deaths.ts deathCells). */
const NOT_PRESERVED = {
  Preserved_Dead_Alive: 'NA',
  CAM_ID: 'NA',
  Tube_1_id: 'NA',
  Tube_1_tissue: 'NOT_COLLECTED',
  T1_Preservation_medium: 'NOT_COLLECTED',
  Tube_2_id: 'NA',
  Tube_2_tissue: 'NOT_COLLECTED',
  T2_Preservation_medium: 'NOT_COLLECTED',
  Tube_3_id: 'NA',
  Tube_3_tissue: 'NOT_COLLECTED',
  Tube_4_id: 'NA',
  Tube_4_tissue: 'NOT_COLLECTED',
  Preservation_medium: 'NOT_COLLECTED',
  Preservation_date: 'NA',
  Location_body: 'NA',
};
function disappearance(recordId, { preserved = false } = {}) {
  const values = { Death_date: SERIAL, Death_cause: DISAPPEARED, ...(preserved ? {} : NOT_PRESERVED) };
  return { id: recordId, values, expected: Object.fromEntries(Object.keys(values).map(f => [f, null])) };
}
const cellIn = (sheets, row, field) => {
  const rows = sheets.rows.get('Insectary_data');
  const header = rows.find(r => r.row === 1).cells.findIndex(c => c?.userEnteredValue?.stringValue === field);
  const cell = rows.find(r => r.row === row)?.cells[header]?.userEnteredValue;
  return cell?.formulaValue ?? cell?.stringValue ?? cell?.numberValue ?? null;
};

test('alive in the insectary: no death date and no cause, typed rows only, entries kept in the app counted', async () => {
  const { store } = await fixture();
  try {
    assert.deepEqual(
      aliveButterflies(store, { species: POLY }).map(b => b.id),
      ['A1B', 'A2B', 'A3B', 'A8B'],
      'dead (date, cause only, NA date), pre-made rows and other species left out; sheet order',
    );
    assert.deepEqual(aliveButterflies(store, { species: 'mechanitis  polymnia PROCERIFORMIS' }).length, 4, 'case and spacing aside');
    const a3 = aliveButterflies(store, { species: POLY }).find(b => b.id === 'A3B');
    assert.deepEqual(
      { sex: a3.sex, wild: a3.wild, entered: a3.entered, row: a3.row },
      { sex: 'male', wild: true, entered: 46290, row: 4 },
    );
    assert.deepEqual(aliveSpecies(store), [
      { species: POLY, alive: 4 },
      { species: LYS, alive: 1 },
    ]);
    // Emerged in Emergidos (kept in the app): alive, of its clutch's species, in its pre-made row.
    await store.staged.stage(
      {
        requestId: randomUUID(),
        purpose: 'emergidos',
        creates: [
          {
            clientId: 'new-a9b',
            module: 'Insectary_data',
            values: { Insectary_ID: 'A9B', 'CLUTCH NUMBER': 990, Sex: 'female', Intro2Insectary_date: 46300, Wild_Reared: 'Reared' },
          },
        ],
      },
      ana,
    );
    // A death typed in Clutches/Emergidos and kept in the app: no longer alive.
    await store.staged.stage(
      { requestId: randomUUID(), purpose: 'emergidos', edits: [{ id: recordOf(store, 'A2B'), values: { Death_date: 46299, Death_cause: 'Unknown' }, expected: {} }] },
      ana,
    );
    const now = aliveButterflies(store, { species: POLY });
    assert.deepEqual(
      now.map(b => b.id),
      ['A1B', 'A3B', 'A8B', 'A9B'],
    );
    assert.deepEqual(now.at(-1), {
      recordId: 'staged:new-a9b',
      id: 'A9B',
      row: 10,
      species: POLY,
      sex: 'female',
      clutch: '990',
      entered: 46300,
      wild: false,
      staged: true,
    });
  } finally {
    store.close();
  }
});

test('a census: start or join, marks by several people, doubts, undo, an unknown ID, another species', async () => {
  const { store } = await fixture();
  try {
    const first = start(store, ana);
    assert.equal(first.joined, false);
    assert.equal(first.census.expected, 4);
    assert.deepEqual(first.roster.map(b => b.id), ['A1B', 'A2B', 'A3B', 'A8B']);
    // Luis opens the same species and day on his phone: the same census.
    const joined = start(store, luis, ' Mechanitis polymnia  proceriformis');
    assert.equal(joined.joined, true);
    assert.equal(joined.census.id, first.census.id);
    // Another day is another census.
    assert.notEqual(start(store, luis, POLY, '2026-10-06').census.id, first.census.id);
    const id = first.census.id;

    const a1 = mark(store, id, ana, { recordId: recordOf(store, 'A1B') });
    assert.equal(a1.mark.kind, 'seen');
    assert.equal(a1.mark.actorName, 'Ana');
    // Sent twice (a retry): the same mark.
    const body = { requestId: randomUUID(), recordId: recordOf(store, 'A2B') };
    const once = addMark(store, id, body, luis);
    assert.equal(addMark(store, id, body, luis).mark.id, once.mark.id);
    // Seen by Ana too: she is told Luis marked it.
    const again = mark(store, id, ana, { recordId: recordOf(store, 'A2B') });
    assert.equal(again.already, true);
    assert.equal(again.mark.actorName, 'Luis');
    // Alive, but the sex looks different: kept for review.
    const doubt = updateMark(store, id, a1.mark.id, { doubt: 'sex', note: 'Looks male' }, ana);
    assert.deepEqual([doubt.mark.doubt, doubt.mark.note], ['sex', 'Looks male']);
    assert.throws(() => updateMark(store, id, a1.mark.id, { doubt: 'colour' }, ana), e => e.code === 'INVALID_VALUES');
    // Found in this cage: another species' butterfly, and an ID that is in no row.
    mark(store, id, luis, { recordId: recordOf(store, 'A7B') });
    const unknown = mark(store, id, luis, { kind: 'unknown', text: ' q9 z ' });
    assert.equal(unknown.mark.insectaryId, 'Q9Z');
    // A wrong tap, undone.
    const wrong = mark(store, id, luis, { recordId: recordOf(store, 'A3B') });
    removeMark(store, id, wrong.mark.id, luis);
    // Excluded (in another cage), then seen after all; a seen one cannot be excluded.
    mark(store, id, ana, { recordId: recordOf(store, 'A8B'), kind: 'excluded', note: 'In the other cage' });
    assert.throws(() => mark(store, id, ana, { recordId: recordOf(store, 'A1B'), kind: 'excluded' }), e => e.code === 'CENSUS_SEEN');
    assert.throws(() => mark(store, id, ana, { recordId: 'nope' }), e => e.status === 404);
    assert.throws(() => mark(store, id, viewer, { recordId: recordOf(store, 'A3B') }), e => e.status === 403);

    const detail = censusDetail(store, id);
    assert.deepEqual(detail.census.counts, { roster: 4, seen: 2, excluded: 1, disappeared: 0, otherSpecies: 1, offList: 0, unknown: 1, doubts: 1 });
    assert.deepEqual(detail.census.people.sort(), ['Ana', 'Luis']);
    assert.deepEqual(
      detail.marks.map(m => [m.insectaryId, m.kind]),
      [
        ['A1B', 'seen'],
        ['A2B', 'seen'],
        ['A7B', 'seen'],
        ['Q9Z', 'unknown'],
        ['A8B', 'excluded'],
      ],
    );
    assert.equal(censusOverview(store).open.length, 2);
  } finally {
    store.close();
  }
});

test('two people marking at once: each butterfly marked once, the second told who was first', async () => {
  const { store } = await fixture();
  try {
    const id = start(store, ana).census.id;
    const ids = ['A1B', 'A2B', 'A3B', 'A8B'].map(i => recordOf(store, i));
    const both = await Promise.all(
      ids.flatMap(recordId => [ana, luis].map(user => Promise.resolve().then(() => mark(store, id, user, { recordId })))),
    );
    assert.equal(both.filter(o => !o.already).length, 4);
    assert.equal(both.filter(o => o.already).length, 4);
    for (const o of both.filter(x => x.already)) assert.equal(o.mark.actorName, 'Ana');
    assert.equal(censusDetail(store, id).marks.length, 4);
  } finally {
    store.close();
  }
});

test('finishing keeps the disappearances in the app with Muertes cells; reopening undoes them; saving writes them', async () => {
  const { store, sheets } = await fixture();
  try {
    const id = start(store, ana).census.id;
    mark(store, id, ana, { recordId: recordOf(store, 'A1B') });
    mark(store, id, luis, { recordId: recordOf(store, 'A8B'), kind: 'excluded', note: 'In the other cage' });
    const a2 = recordOf(store, 'A2B');
    const a3 = recordOf(store, 'A3B');
    // The plan must be exactly the butterflies not seen (A2B, A3B), on the census day, as a disappearance.
    const finish = edits => finishCensus(store, id, { requestId: randomUUID(), edits }, ana);
    await assert.rejects(finish([disappearance(a2)]), e => e.code === 'CENSUS_CHANGED' && e.details.lacking[0] === a3);
    await assert.rejects(finish([disappearance(a2), disappearance(a3), disappearance(recordOf(store, 'A1B'))]), e => e.code === 'CENSUS_CHANGED');
    const wrongDay = disappearance(a2);
    wrongDay.values.Death_date = SERIAL - 1;
    await assert.rejects(finish([wrongDay, disappearance(a3)]), e => e.code === 'INVALID_VALUES');
    const wrongCell = disappearance(a2);
    wrongCell.values.Sex = 'female';
    await assert.rejects(finish([wrongCell, disappearance(a3)]), e => e.code === 'INVALID_VALUES');
    assert.equal(store.staged.list().items.length, 0, 'nothing kept by a refused finish');

    // A3B has a CAM already: Muertes leaves its preservation cells alone.
    const done = await finish([disappearance(a2), disappearance(a3, { preserved: true })]);
    assert.equal(done.census.status, 'finished');
    assert.equal(done.census.deaths, 'staged');
    assert.deepEqual(
      done.roster.map(b => [b.id, b.status]),
      [
        ['A1B', 'seen'],
        ['A2B', 'disappeared'],
        ['A3B', 'disappeared'],
        ['A8B', 'excluded'],
      ],
    );
    assert.equal(done.roster.find(b => b.id === 'A8B').note, 'In the other cage');
    const items = store.staged.list().items;
    assert.equal(items.length, 2);
    assert.ok(items.every(i => i.purpose === 'censo' && i.kind === 'edit' && i.actorName === 'Ana'));
    assert.deepEqual(items.find(i => i.recordId === a2).values, { Death_date: SERIAL, Death_cause: DISAPPEARED, ...NOT_PRESERVED });
    assert.deepEqual(items.find(i => i.recordId === a3).values, { Death_date: SERIAL, Death_cause: DISAPPEARED });
    assert.equal(cellIn(sheets, 3, 'Death_cause'), null, 'not in the sheet before «Guardar en Google Sheets»');
    // Kept in the app, they already count: no longer alive.
    assert.deepEqual(aliveButterflies(store, { species: POLY }).map(b => b.id), ['A1B', 'A8B']);
    // A finished census takes no more marks.
    assert.throws(() => mark(store, id, luis, { recordId: recordOf(store, 'A8B') }), e => e.code === 'CENSUS_CLOSED');
    assert.equal(censusOverview(store).history[0].counts.disappeared, 2);

    // Reopened (one more was seen after all): the entry is undone, the census open again with its marks.
    const reopened = await reopenCensus(store, id, luis);
    assert.equal(reopened.census.status, 'open');
    assert.equal(store.staged.list().items.length, 0);
    assert.equal(reopened.marks.length, 2);
    mark(store, id, luis, { recordId: a3 });
    const again = await finish([disappearance(a2)]);
    assert.deepEqual(
      again.roster.filter(b => b.status === 'disappeared').map(b => b.id),
      ['A2B'],
    );
    // «Guardar en Google Sheets»: written, the census knows its save.
    const out = await store.staged.flush({ requestId: randomUUID() }, ana);
    assert.equal(out.status, 'done');
    assert.equal(cellIn(sheets, 3, 'Death_cause'), DISAPPEARED);
    assert.equal(cellIn(sheets, 3, 'Death_date'), SERIAL);
    assert.equal(cellIn(sheets, 3, 'Tube_1_tissue'), 'NOT_COLLECTED');
    const written = censusDetail(store, id).census;
    assert.equal(written.deaths, 'written');
    const action = store.db.prepare('SELECT purpose FROM actions WHERE id = (SELECT action_id FROM censuses WHERE id = ?)').get(id);
    assert.equal(action.purpose, 'censo');
    await assert.rejects(reopenCensus(store, id, ana), e => e.code === 'CENSUS_WRITTEN');
    // The paper notebook brought up to date.
    assert.equal(setCensusNotebook(store, id, {}, luis).census.notebookByName, 'Luis');
    assert.equal(setCensusNotebook(store, id, { done: false }, luis).census.notebookAt, null);
  } finally {
    store.close();
  }
});

test('undoing a census entry in the bar opens the census again; a census with no one missing finishes with nothing kept', async () => {
  const { store } = await fixture();
  try {
    const id = start(store, ana, LYS).census.id;
    const finished = await finishCensus(store, id, { requestId: randomUUID(), edits: [disappearance(recordOf(store, 'A7B'))] }, ana);
    assert.equal(finished.census.status, 'finished');
    const entry = store.staged.list().items[0].entryId;
    await store.staged.remove({ entryId: entry }, luis);
    assert.equal(censusDetail(store, id).census.status, 'open');
    mark(store, id, ana, { recordId: recordOf(store, 'A7B') });
    const none = await finishCensus(store, id, { requestId: randomUUID(), edits: [] }, ana);
    assert.equal(none.census.deaths, 'none');
    assert.equal(store.staged.list().items.length, 0);
    // Cancelled: no longer open, its marks kept.
    const other = start(store, luis).census.id;
    mark(store, other, luis, { recordId: recordOf(store, 'A1B') });
    const cancelled = cancelCensus(store, other, luis);
    assert.equal(cancelled.census.status, 'cancelled');
    assert.equal(cancelled.marks.length, 1);
    assert.equal(censusOverview(store).open.length, 0);
  } finally {
    store.close();
  }
});

test('censuses and marks survive a restart', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'census-'));
  const sheets = new LocalSheets(seed(), { health: { probeMs: 20 } });
  const path = join(dir, 'app.sqlite');
  let { store } = await fixture(path, sheets);
  const id = start(store, ana).census.id;
  mark(store, id, ana, { recordId: recordOf(store, 'A1B') });
  store.close();
  ({ store } = await fixture(path, sheets));
  try {
    const detail = censusDetail(store, id);
    assert.equal(detail.census.status, 'open');
    assert.deepEqual(detail.marks.map(m => m.insectaryId), ['A1B']);
  } finally {
    store.close();
  }
});

test('look-alike pairs learned from corrected Insectary IDs', async () => {
  const { store } = await fixture();
  try {
    store.db.prepare("INSERT INTO actions(id, actor, source, created_at, status) VALUES('a', 'ana', 'app', ?, 'applied')").run(new Date().toISOString());
    const add = (before, after) =>
      store.db
        .prepare("INSERT INTO changes(id, action_id, record_id, sheet, row_num, field, before_json, after_json) VALUES(?, 'a', 'r', 'Insectary_data', 2, 'Insectary_ID', ?, ?)")
        .run(randomUUID(), JSON.stringify(before), JSON.stringify(after));
    add('A6B', 'A8B');
    add('C8D', 'C6D');
    add('W2B', 'W7B');
    add('A1B', 'B2C');
    add(null, 'A1B');
    assert.deepEqual(learnedLookAlikes(store), [
      { a: '6', b: '8', n: 2 },
      { a: '2', b: '7', n: 1 },
    ]);
  } finally {
    store.close();
  }
});
