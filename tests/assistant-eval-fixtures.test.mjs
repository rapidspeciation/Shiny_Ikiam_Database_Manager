// Seeded fixtures of the assistant eval (tools/assistant-eval/fixtures.mjs): written together, waited for together.
import test from 'node:test';
import assert from 'node:assert/strict';
import { setCells } from '../tools/assistant-eval/fixtures.mjs';

/** A sheet whose replica shows a write `lag` polls later (the eval app's `query` replica lags about 35 s). */
function fakeApp(lag) {
  const cells = new Map();
  let poll = 0;
  const log = [];
  const k = c => `${c.sheet}:${c.row}:${c.field}`;
  return {
    log,
    edit: c => {
      log.push(`edit ${k(c)}`);
      cells.set(k(c), { value: c.value, at: poll });
    },
    read: c => {
      const x = cells.get(k(c));
      return x && poll - x.at >= lag ? x.value : 'old';
    },
    sleep: async () => {
      poll++;
      log.push('sleep');
    },
  };
}
const cell = (row, field, value) => ({ sheet: 'Insectary_data', row, field, value });
const twelve = [10954, 10960, 10961, 10962].flatMap(r => ['Sex', 'Intro2Insectary_date', 'Death_date'].map(f => cell(r, f, '')));

test('all fixture cells are written before any wait, and the replica lag is waited for once', async () => {
  const app = fakeApp(7);
  const r = await setCells(twelve, app);
  assert.deepEqual(app.log.slice(0, 12), twelve.map(c => `edit ${c.sheet}:${c.row}:${c.field}`));
  // One lag (7 pauses) for all 12 cells, not 12 lags: D17 took about 7 min per side with one cell at a time.
  assert.equal(app.log.filter(l => l === 'sleep').length, 7);
  assert.equal(r.polls, 8);
});

test('a cell written twice is waited for at its last value', async () => {
  const app = fakeApp(1);
  await setCells([cell(1, 'Sex', 'male'), cell(1, 'Sex', 'female')], app);
  assert.equal(app.read(cell(1, 'Sex')), 'female');
});

test('a failed write does not stop the others, and the error names it and any cell never shown', async () => {
  const app = fakeApp(1);
  const edit = c => {
    if (c.row === 2) throw new Error('401');
    if (c.row !== 3) app.edit(c);
  };
  await assert.rejects(setCells([cell(1, 'Sex', 'male'), cell(2, 'Sex', 'male'), cell(3, 'Sex', 'male')], { ...app, edit, tries: 5 }), e => {
    assert.match(e.message, /Insectary_data:2:Sex: 401/);
    assert.match(e.message, /Insectary_data:3:Sex never showed "male"/);
    assert.doesNotMatch(e.message, /Insectary_data:1:/);
    return true;
  });
  assert.equal(app.read(cell(1, 'Sex')), 'male');
});
