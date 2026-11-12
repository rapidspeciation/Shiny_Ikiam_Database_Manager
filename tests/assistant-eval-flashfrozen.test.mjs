// The species-by-medium count cases (tools/assistant-eval/tasks/flashfrozen.json), offline: each case fails the
// replies of the production chat's patterns and passes a reply with the wanted number on its first line. No model.
import test from 'node:test';
import assert from 'node:assert/strict';
import { existsSync, readFileSync } from 'node:fs';
import { grade } from '../tools/assistant-eval/grade.mjs';

const TASKS = JSON.parse(readFileSync(new URL('../tools/assistant-eval/tasks/flashfrozen.json', import.meta.url), 'utf8')).tasks;
const FX = JSON.parse(readFileSync(new URL('./fixtures/assistant-eval/flashfrozen.json', import.meta.url), 'utf8'));
// The production chat's literal replies stay outside the repo (no chat text in git).
const LOGGED = '/home/franz/.cache/ithomiini-lab/assistant-eval/fixtures/flashfrozen-logged.json';
const LX = existsSync(LOGGED) ? JSON.parse(readFileSync(LOGGED, 'utf8')) : null;
const score = (task, fx) => grade(task, { finalText: fx.finalText, tools: [], proposals: [] }, { checkFiles: false }).score;

for (const id of ['F01', 'F02', 'F03', 'F04', 'F05', 'F06']) {
  test(`${id} fails the production patterns and passes a reply that opens with the wanted number`, () => {
    const task = TASKS.find(t => t.id === id);
    assert.ok(task, id);
    assert.ok(FX[id].fail.length && FX[id].pass.length);
    for (const fx of FX[id].fail) assert.ok(score(task, fx) < 1, `${id} should fail: ${fx.from}`);
    for (const fx of FX[id].pass) assert.equal(score(task, fx), 1, `${id} should pass: ${fx.from}`);
  });
}

test('the opening check reads the first non-empty line only', () => {
  const opening = TASKS.find(t => t.id === 'F01').checks[0];
  const s = text => grade({ checks: [opening] }, { finalText: text, tools: [], proposals: [] }).score;
  assert.equal(s('\n\n**48** de Ecuador, 61 en total.'), 1);
  assert.equal(s('Hola.\n48 de Ecuador, 61 en total.'), 0);
  assert.equal(s('Hay 6 sin subespecie.\n\n| total | 61 |\n| Ecuador | 48 |'), 0);
});

test("F01's checks fail the production chat's literal replies, also with the production numbers", { skip: LX ? false : `logged replies not on this machine (${LOGGED})` }, () => {
  const task = TASKS.find(t => t.id === 'F01');
  // The same checks with the numbers of the production copy (11 Nov): total 224 (Collection_data 66 + Insectary_data
  // 158), Ecuador 210 by Country or 58 = empty + psamathe in Collection_data: the failure is the reply's shape.
  const prod = { checks: [{ regex: '^\\s*(?=[^\\n]*\\b(?:224|66)\\b)(?=[^\\n]*\\b(?:58|52|210|216)\\b)', w: 2 }, task.checks[1]] };
  for (const fx of LX.F01.fail) {
    assert.ok(score(task, fx) < 1, fx.from);
    assert.ok(score(prod, fx) < 1, `${fx.from} (production numbers)`);
  }
});
