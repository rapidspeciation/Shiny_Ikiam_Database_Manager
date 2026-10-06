// Emergidos' +♀ / +♂ give the next free Insectary ID at the tap and hold it (server/holds.mjs):
// nobody else is offered it or can save it; the card's save takes it over; taking the card
// away lets it go; and a hold nobody saved goes after a day and a half.
import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { applyBatch } from '../server/batch.mjs';
import { idSuggestions } from '../server/grid.mjs';
import { HOLD_HOURS, dropStaleHolds, holdId, releaseHold } from '../server/holds.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { Store } from '../server/store.mjs';

const ana = { id: 'ana', username: 'ana', displayName: 'Ana', role: 'editor' };
const luis = { id: 'luis', username: 'luis', displayName: 'Luis', role: 'editor' };
const viewer = { id: 'vera', username: 'vera', displayName: 'Vera', role: 'viewer' };

async function fixture() {
  const sheets = new LocalSheets(
    {
      Insectary_data: [
        { row: 2, values: { Insectary_ID: 'A0E', 'CLUTCH NUMBER': 990, Sex: 'female' } },
        { row: 3, values: { Insectary_ID: 'A1E' } },
        { row: 4, values: { Insectary_ID: 'A2E' } },
        { row: 5, values: { Insectary_ID: 'A3E' } },
        { row: 6, values: { Insectary_ID: 'A4E' } },
      ],
      Insectary_stocks: [{ row: 2, values: { 'CLUTCH NUMBER': 990, 'DATE LAID': 46280, 'NUMBER OF EGGS': 20 } }],
    },
    { health: { probeMs: 20 } },
  );
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data', 'Insectary_stocks'] });
  for (const u of [ana, luis, viewer])
    store.db
      .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,created_at) VALUES(?,?,?,?,'s','h',?)")
      .run(u.id, u.username, u.displayName, u.role, new Date().toISOString());
  return store;
}
const card = () => randomUUID();
const emerged = (id, clientId = randomUUID()) => ({
  clientId,
  module: 'Insectary_data',
  values: { Insectary_ID: id, 'CLUTCH NUMBER': 990, Sex: 'male', Intro2Insectary_date: 46300, Wild_Reared: 'Reared' },
});
const free = store => idSuggestions(store, { kind: 'insectary', count: 10 }).sequence;

test('a tap holds its ID: the next tap and everyone else get the next one', async () => {
  const store = await fixture();
  try {
    const a = card();
    assert.deepEqual(holdId(store, { key: a, value: 'a1e' }, ana), { held: true, value: 'A1E' });
    assert.deepEqual(free(store), ['A2E', 'A3E', 'A4E']);
    // The same card asking again (a retry): still held, nothing changes.
    assert.deepEqual(holdId(store, { key: a, value: 'A1E' }, ana), { held: true, value: 'A1E' });
    // Someone else's tap on the same ID: refused, with who and the next free one.
    const refused = holdId(store, { key: card(), value: 'A1E' }, luis);
    assert.equal(refused.held, false);
    assert.equal(refused.code, 'CLAIMED');
    assert.equal(refused.holder, 'Ana');
    assert.equal(refused.next, 'A2E');
    // A butterfly of the sheet is never held.
    const used = holdId(store, { key: card(), value: 'A0E' }, luis);
    assert.equal(used.code, 'USED');
    assert.equal(used.next, 'A2E');
    // Everyone sees who holds it.
    assert.deepEqual(
      store.staged.list().claims.map(c => [c.value, c.hold, c.itemId, c.actorName]),
      [['A1E', a, null, 'Ana']],
    );
    // Another tab's save of a held ID is refused.
    await assert.rejects(
      applyBatch(store, { requestId: randomUUID(), creates: [emerged('A1E')] }, luis),
      e => e.code === 'BATCH_CONFLICT' && e.details.items[0].code === 'CLAIMED',
    );
    assert.throws(() => holdId(store, { key: card(), value: 'A3E' }, viewer), e => e.status === 403);
  } finally {
    store.close();
  }
});

test("a card's ID changed (the wing says another) moves its hold; taking the card away lets it go", async () => {
  const store = await fixture();
  try {
    const a = card();
    holdId(store, { key: a, value: 'A1E' }, ana);
    assert.equal(holdId(store, { key: a, value: 'A3E' }, ana).held, true);
    assert.deepEqual(free(store), ['A1E', 'A2E', 'A4E']);
    assert.deepEqual(releaseHold(store, a, ana), { released: 1 });
    assert.deepEqual(free(store), ['A1E', 'A2E', 'A3E', 'A4E']);
  } finally {
    store.close();
  }
});

test('saving the cards passes their held IDs to the entry; undoing it frees them', async () => {
  const store = await fixture();
  try {
    const a = card();
    const b = card();
    holdId(store, { key: a, value: 'A1E' }, ana);
    holdId(store, { key: b, value: 'A2E' }, ana);
    const out = await store.staged.stage({ requestId: randomUUID(), purpose: 'emergidos', partial: false, creates: [emerged('A1E', a), emerged('A2E', b)] }, ana);
    assert.equal(out.created.length, 2);
    const claims = store.staged.list().claims;
    assert.deepEqual(claims.map(c => [c.value, !!c.itemId, c.hold ?? null]), [['A1E', true, null], ['A2E', true, null]]);
    // Someone else's hold is not theirs to save.
    const c = card();
    holdId(store, { key: c, value: 'A3E' }, luis);
    await assert.rejects(
      store.staged.stage({ requestId: randomUUID(), purpose: 'emergidos', partial: false, creates: [emerged('A3E')] }, ana),
      e => e.code === 'BATCH_CONFLICT' && e.details.items[0].code === 'CLAIMED',
    );
    await store.staged.remove({ entryId: out.entryId }, ana);
    assert.deepEqual(free(store), ['A1E', 'A2E', 'A4E']);
  } finally {
    store.close();
  }
});

test('a hold nobody saved goes after a day and a half', async () => {
  const store = await fixture();
  try {
    holdId(store, { key: card(), value: 'A1E' }, ana);
    dropStaleHolds(store.db, Date.now() + (HOLD_HOURS - 1) * 3_600_000);
    assert.equal(free(store)[0], 'A2E');
    dropStaleHolds(store.db, Date.now() + (HOLD_HOURS + 1) * 3_600_000);
    assert.equal(free(store)[0], 'A1E');
  } finally {
    store.close();
  }
});
