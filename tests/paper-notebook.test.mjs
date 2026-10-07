// What to mark in the paper Emergidos notebook: after a day's censuses (☺ seen, ✗ disappeared to
// write, ▬ already dead in the database to highlight), and the deaths entered since a moment
// (from the history, plus the deaths kept in the app).
import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { mkdtempSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { createChecksHost } from '../server/checks-host.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { Store } from '../server/store.mjs';
import { applyBatch } from '../server/batch.mjs';
import { undoEdits } from '../server/history.mjs';
import { DISAPPEARED, addMark, censusDetail, finishCensus, serialOf, startCensus } from '../server/census.mjs';
import { enteredDeaths, notebookUpdate, openedHighlights } from '../server/paper-notebook.mjs';

const ana = { id: 'ana', username: 'ana', displayName: 'Ana', role: 'editor' };
const luis = { id: 'luis', username: 'luis', displayName: 'Luis', role: 'editor' };
const POLY = 'Mechanitis polymnia proceriformis';
const EURY = 'Mechanitis polymnia eurydice';
const LYS = 'Mechanitis lysimnia';
const SAL = 'Hypothyris salapia';
const DAY = '2026-10-05';
const SERIAL = serialOf(DAY);

function seed() {
  const b = (row, id, species, values = {}) => ({
    row,
    values: {
      Insectary_ID: id,
      SPECIES: species,
      Sex: 'female',
      Intro2Insectary_date: SERIAL - 20,
      Wild_Reared: 'Reared',
      ...values,
    },
  });
  return {
    Insectary_data: [
      b(2, 'A1B', POLY, { Intro2Insectary_date: SERIAL - 300, Death_date: SERIAL - 250, Death_cause: 'Natural' }), // an old page
      b(3, 'A2B', POLY), // seen in the census
      b(4, 'A3B', EURY), // a subspecies: in the same census; not seen
      b(5, 'A4B', POLY, { Death_date: SERIAL - 7, Death_cause: 'Unknown' }), // dead before the census
      b(6, 'A5B', LYS, { Intro2Insectary_date: SERIAL - 40 }), // another species censused the same day
      b(7, 'A6B', POLY, { Intro2Insectary_date: SERIAL }), // emerged on the census day
      b(8, 'A7B', POLY, { Intro2Insectary_date: 'NA' }), // alive, no entry date: in the insectary
      b(9, 'A8B', SAL), // a species not asked for
      b(10, 'A9B', POLY, { Intro2Insectary_date: SERIAL - 30 }), // left out of the census (in another cage)
      b(11, 'B1B', POLY, { Intro2Insectary_date: SERIAL - 400 }), // a straggler: alive, entered long ago
      { row: 12, values: { Insectary_ID: 'B2B' } }, // pre-made
    ],
    Insectary_stocks: [{ row: 2, values: { 'CLUTCH NUMBER': 990, SPECIES: POLY } }],
  };
}

async function fixture(config = {}) {
  const sheets = new LocalSheets(seed(), { health: { probeMs: 20 } });
  const store = new Store({ localMode: true, databasePath: ':memory:', ...config }, { sheets });
  await store.sync({ sheets: ['Insectary_data', 'Insectary_stocks'] });
  for (const u of [ana, luis])
    store.db
      .prepare(
        "INSERT OR IGNORE INTO users(id,username,display_name,role,salt,password_hash,created_at) VALUES(?,?,?,?,'s','h',?)",
      )
      .run(u.id, u.username, u.displayName, u.role, new Date().toISOString());
  return { store, sheets };
}
const recordOf = (store, id) =>
  store.db
    .prepare(
      "SELECT id FROM records WHERE sheet='Insectary_data' AND observed=1 AND json_extract(values_json,'$.Insectary_ID')=?",
    )
    .get(id).id;
const disappearance = recordId => ({
  id: recordId,
  values: { Death_date: SERIAL, Death_cause: DISAPPEARED },
  expected: { Death_date: null, Death_cause: null },
});
/** A census of `species` on DAY: `seen` marked, `excluded` left out, the rest disappeared. */
async function census(store, species, { seen = [], excluded = [] } = {}) {
  const c = startCensus(store, { requestId: randomUUID(), species, day: DAY }, ana).census;
  for (const id of seen) addMark(store, c.id, { requestId: randomUUID(), recordId: recordOf(store, id) }, ana);
  for (const id of excluded)
    addMark(
      store,
      c.id,
      { requestId: randomUUID(), recordId: recordOf(store, id), kind: 'excluded', note: 'Other cage' },
      ana,
    );
  const roster = censusDetail(store, c.id).roster;
  const marked = new Set([...seen, ...excluded]);
  const edits = roster.filter(b => !marked.has(b.id)).map(b => disappearance(b.recordId));
  return finishCensus(store, c.id, { requestId: randomUUID(), edits }, ana);
}
const brief = out =>
  Object.fromEntries(out.lines.map(l => [l.id, [l.census?.status ?? null, l.todo, l.death?.staged ?? null]]));

test('«Actualizar el cuaderno»: a day of censuses, several species, subspecies together, in notebook order', async () => {
  const { store } = await fixture();
  try {
    await census(store, 'Mechanitis polymnia', { seen: ['A2B'], excluded: ['A9B'] });
    await census(store, LYS, { seen: ['A5B'] });
    // Without species: those censused that day. Pages in use since 90 days before.
    const since = '2026-07-07';
    const out = await notebookUpdate(store, { day: DAY, since });
    assert.deepEqual(out.species, ['Mechanitis polymnia', 'Mechanitis lysimnia']);
    assert.deepEqual(out.censused, ['Mechanitis polymnia', 'Mechanitis lysimnia']);
    assert.deepEqual(
      out.lines.map(l => l.id),
      ['A2B', 'A3B', 'A4B', 'A5B', 'A6B', 'A7B', 'A9B', 'B1B'],
      'sheet order; the old page (A1B), another species (A8B) and pre-made rows left out',
    );
    assert.deepEqual(brief(out), {
      A2B: ['seen', null, null],
      // The census's own disappearances: to write (kept in the app until saved).
      A3B: ['disappeared', 'write', true],
      A4B: [null, 'highlight', false],
      A5B: ['seen', null, null],
      A6B: ['disappeared', 'write', true],
      A7B: ['disappeared', 'write', true],
      A9B: ['excluded', null, null],
      B1B: ['disappeared', 'write', true],
    });
    assert.equal(out.lines.find(l => l.id === 'A9B').census.note, 'Other cage');
    assert.deepEqual(out.lines.find(l => l.id === 'A4B').death, { date: SERIAL - 7, cause: 'Unknown', staged: false });
    assert.ok(out.lines.find(l => l.id === 'A3B').census.waiting, 'disappearances not in Google Sheets yet');
    // One species asked: only it. Without `since`, every dead one of the species that entered by then.
    const poly = await notebookUpdate(store, { day: DAY, species: 'Mechanitis polymnia' });
    assert.deepEqual(
      poly.lines.map(l => l.id),
      ['A1B', 'A2B', 'A3B', 'A4B', 'A6B', 'A7B', 'A9B', 'B1B'],
    );
    // A day with no census: those in the insectary then (not one emerged after), the dead ones to highlight.
    const before = await notebookUpdate(store, { day: '2026-10-04', species: 'Mechanitis polymnia', since });
    assert.deepEqual(
      before.lines.map(l => l.id),
      ['A2B', 'A3B', 'A4B', 'A7B', 'A9B', 'B1B'],
    );
    assert.deepEqual(before.censuses, []);
    assert.deepEqual(
      before.lines.filter(l => l.todo).map(l => [l.id, l.todo]),
      [
        ['A3B', 'highlight'],
        ['A4B', 'highlight'],
        ['A7B', 'highlight'],
        ['B1B', 'highlight'],
      ],
      'the disappearances of the 5th are deaths in the database seen from the 4th',
    );
    assert.deepEqual(poly.censuses.length, 1);
    // A species with no census that day: alive ones nothing to do, dead ones to highlight.
    const sal = await notebookUpdate(store, { day: DAY, species: `${SAL},${POLY}` });
    assert.deepEqual(sal.species, ['Hypothyris salapia', 'Mechanitis polymnia']);
    assert.deepEqual(sal.lines.find(l => l.id === 'A8B').todo, null);
    await assert.rejects(notebookUpdate(store, { day: '5/10/2026' }), e => e.code === 'INVALID_DAY');
  } finally {
    store.close();
  }
});

test('«Actualizar el cuaderno»: saved vs only in the app; a death after the census; a disappearance undone', async () => {
  const { store } = await fixture();
  try {
    await census(store, 'Mechanitis polymnia', { seen: ['A2B', 'A7B', 'B1B'], excluded: ['A9B'] });
    const a2 = recordOf(store, 'A2B');
    // Seen, and found dead two days later: kept in the app (amber), then saved.
    await store.staged.stage(
      {
        requestId: randomUUID(),
        purpose: 'clutches',
        edits: [
          {
            id: a2,
            values: { Death_date: SERIAL + 2, Death_cause: 'Natural' },
            expected: { Death_date: null, Death_cause: null },
          },
        ],
      },
      luis,
    );
    let out = await notebookUpdate(store, { day: DAY, species: 'Mechanitis polymnia' });
    assert.deepEqual(out.lines.find(l => l.id === 'A2B').todo, 'highlight');
    assert.deepEqual(out.lines.find(l => l.id === 'A2B').death, { date: SERIAL + 2, cause: 'Natural', staged: true });
    assert.deepEqual(out.lines.find(l => l.id === 'A3B').census.waiting, true);
    await store.staged.flush({ requestId: randomUUID() }, ana);
    out = await notebookUpdate(store, { day: DAY, species: 'Mechanitis polymnia' });
    assert.deepEqual(brief(out).A2B, ['seen', 'highlight', false]);
    assert.deepEqual(brief(out).A3B, ['disappeared', 'write', false]);
    assert.equal(out.lines.find(l => l.id === 'A3B').census.waiting, false, 'in Google Sheets now');
    // The disappearance undone in Historial (found alive): nothing to do.
    const action = store.db.prepare('SELECT action_id FROM censuses').get().action_id;
    const changes = store.db
      .prepare('SELECT id FROM changes WHERE action_id = ? AND record_id = ?')
      .all(action, recordOf(store, 'A3B'))
      .map(c => c.id);
    await undoEdits(store, { requestId: randomUUID(), changeIds: changes }, ana);
    out = await notebookUpdate(store, { day: DAY, species: 'Mechanitis polymnia' });
    const a3 = out.lines.find(l => l.id === 'A3B');
    assert.deepEqual([a3.todo, a3.undone, a3.death], [null, true, null]);
    // Emerged in Emergidos, kept in the app: in its pre-made row, of its clutch's species, nothing to do; dead, to highlight (amber).
    await store.staged.stage(
      {
        requestId: randomUUID(),
        purpose: 'emergidos',
        creates: [
          {
            clientId: 'new-b2b',
            module: 'Insectary_data',
            values: {
              Insectary_ID: 'B2B',
              'CLUTCH NUMBER': 990,
              Intro2Insectary_date: SERIAL - 1,
              Death_date: SERIAL + 1,
              Death_cause: 'Natural',
            },
          },
        ],
      },
      ana,
    );
    const b2 = (await notebookUpdate(store, { day: DAY, species: 'Mechanitis polymnia' })).lines.at(-1);
    assert.deepEqual(
      [b2.id, b2.row, b2.recordId, b2.staged, b2.todo, b2.death.staged],
      ['B2B', 12, 'staged:new-b2b', true, 'highlight', true],
    );
  } finally {
    store.close();
  }
});

test('«Filas para resaltar»: deaths entered since a moment, from the history and the entries kept in the app', async () => {
  const { store, sheets } = await fixture();
  try {
    const at = (id, iso) => store.db.prepare('UPDATE actions SET created_at = ? WHERE id = ?').run(iso, id);
    const save = async (who, edits, iso) => {
      const out = await applyBatch(store, { requestId: randomUUID(), purpose: 'muertes', edits }, who);
      at(out.action.id, iso);
      return out.action.id;
    };
    const death = (id, date, cause) => ({ id: recordOf(store, id), values: { Death_date: date, Death_cause: cause } });
    await save(ana, [death('A2B', SERIAL - 5, 'Natural')], '2026-10-01T15:00:00.000Z');
    await save(luis, [death('A3B', SERIAL - 1, 'Predation')], '2026-10-04T15:00:00.000Z');
    // Entered on the 4th, then its cause corrected on the 5th: still entered on the 4th.
    await save(ana, [{ id: recordOf(store, 'A3B'), values: { Death_cause: 'Spider' } }], '2026-10-05T15:00:00.000Z');
    // Entered and undone: not listed.
    const a5 = await save(ana, [death('A5B', SERIAL, 'Natural')], '2026-10-04T16:00:00.000Z');
    const undone = await undoEdits(store, { requestId: randomUUID(), actionIds: [a5] }, ana);
    at(undone.action.id, '2026-10-04T17:00:00.000Z');
    // Typed in Google Sheets: seen by the sync.
    await sheets.externalEdit('Insectary_data', 10, { Death_date: SERIAL, Death_cause: 'Unknown' });
    await store.refreshRows('Insectary_data', [10]);
    const sync = store.db
      .prepare("SELECT id FROM actions WHERE source='sheet_reconciliation' ORDER BY rowid DESC LIMIT 1")
      .get().id;
    at(sync, '2026-10-05T20:00:00.000Z');
    // Kept in the app (not in Google Sheets yet).
    await store.staged.stage(
      {
        requestId: randomUUID(),
        purpose: 'clutches',
        edits: [
          {
            id: recordOf(store, 'A7B'),
            values: { Death_date: SERIAL, Death_cause: 'Natural' },
            expected: { Death_date: null, Death_cause: null },
          },
        ],
      },
      luis,
    );

    const since3 = enteredDeaths(store, { since: '2026-10-03T05:00:00.000Z' });
    assert.deepEqual(
      since3.items.map(i => [i.id, i.death.cause, i.source, i.death.staged]),
      [
        ['A3B', 'Spider', 'app', false],
        ['A7B', 'Natural', 'app', true],
        ['A9B', 'Unknown', 'sheets', false],
      ],
      'sheet order; A2B before the moment; A5B undone',
    );
    assert.equal(
      since3.items[0].enteredAt,
      '2026-10-04T15:00:00.000Z',
      'when it went from alive to dead, not its correction',
    );
    assert.deepEqual(since3.items[0].by, ['Luis', 'Ana']);
    assert.equal(since3.items[0].row, 4);
    const all = enteredDeaths(store, { since: '2026-09-30T05:00:00.000Z' });
    assert.deepEqual(
      all.items.map(i => i.id),
      ['A2B', 'A3B', 'A7B', 'A9B'],
    );
    assert.ok(all.historyStart);
    // Entered again after the undo: counts from then.
    await save(luis, [death('A5B', SERIAL, 'Eaten')], '2026-10-06T15:00:00.000Z');
    assert.deepEqual(
      enteredDeaths(store, { since: '2026-10-06T05:00:00.000Z' }).items.map(i => i.id),
      ['A5B', 'A7B'],
    );
    assert.throws(
      () => enteredDeaths(store, {}),
      e => e.code === 'INVALID_DAY',
    );
    // When this person last opened the list: the next time starts there.
    assert.equal(openedHighlights(store, ana).previous, null);
    const second = openedHighlights(store, ana).previous;
    assert.ok(second && !Number.isNaN(Date.parse(second)));
    assert.equal(openedHighlights(store, luis).previous, null, 'each person their own');
  } finally {
    store.close();
  }
});

test('the butterflies «Actualizar el cuaderno» reads come from the checks worker, read again after a change', async t => {
  const dir = mkdtempSync(join(tmpdir(), 'paper-notebook-'));
  const databasePath = join(dir, 'app.sqlite');
  const store = new Store({ databasePath, localMode: true }, { sheets: new LocalSheets(seed()) });
  await store.sync({ sheets: ['Insectary_data', 'Insectary_stocks'] });
  const host = createChecksHost({ store, config: { databasePath, localMode: true } });
  t.after(() => {
    host.close();
    store.close();
    rmSync(dir, { recursive: true, force: true });
  });
  assert.equal(host.mode, 'worker');
  const first = await notebookUpdate(store, { day: DAY, species: 'Mechanitis polymnia' });
  assert.deepEqual(
    first.lines.map(l => l.id),
    ['A1B', 'A2B', 'A3B', 'A4B', 'A6B', 'A7B', 'A9B', 'B1B'],
  );
  assert.equal(host.status().notebookFacts.runs, 1);
  // Kept while the copy is as read.
  await notebookUpdate(store, { day: DAY, species: 'Mechanitis lysimnia' });
  assert.equal(host.status().notebookFacts.runs, 1);
  // A death saved: read again, and highlighted.
  const edit = { id: recordOf(store, 'A2B'), values: { Death_date: SERIAL - 1, Death_cause: 'Natural' } };
  await applyBatch(store, { requestId: randomUUID(), edits: [edit] }, ana);
  const after = await notebookUpdate(store, { day: DAY, species: 'Mechanitis polymnia' });
  assert.equal(after.lines.find(l => l.id === 'A2B').todo, 'highlight');
  assert.equal(host.status().notebookFacts.runs, 2);
});
