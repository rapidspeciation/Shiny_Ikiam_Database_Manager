// The eval grader's caption claim check (tools/assistant-eval/grade.mjs): a caption that denies egg-laying is no claim.
import test from 'node:test';
import assert from 'node:assert/strict';
import { existsSync, readFileSync } from 'node:fs';
import { claims, cutGraded, grade, regradeRouting } from '../tools/assistant-eval/grade.mjs';

const EGGS = '(poniendo|pone|puso) huevos|ovipositando|oviposici[oó]n|laying eggs|desovando';
const reply = caption => `Aquí van.\n\n[images]\nhttps://example.org/a.jpg | ${caption}\n[/images]`;
const check = { block: { min: 0, max: 5, captionNot: EGGS, negationOk: true, exists: false } };

test('a caption that denies egg-laying is not a claim', () => {
  assert.equal(claims('Planta hospedera (no muestra oviposición): Juanulloa mexicana', EGGS, true), false);
  assert.equal(claims('Hembra adulta, sin estar poniendo huevos', EGGS, true), false);
  assert.equal(grade({ checks: [check] }, { finalText: reply('Planta hospedera (no muestra oviposición): Juanulloa mexicana'), tools: [], proposals: [] }).score, 1);
});

test('a caption that claims egg-laying still fails', () => {
  assert.equal(claims('Hembra poniendo huevos en Juanulloa, Napo 2023', EGGS, true), true);
  assert.equal(claims('Oviposición en la hoja, no en el tallo', EGGS, true), true);
  assert.equal(grade({ checks: [check] }, { finalText: reply('Melinaea menophilus ♀ poniendo huevos, Napo'), tools: [], proposals: [] }).score, 0);
});

test('without negationOk every match counts, as before', () => {
  assert.equal(claims('Planta hospedera (no muestra oviposición)', EGGS), true);
});

// Widened checks still fail what they were written to catch (lesson L3): fixtures from stored eval runs.
const FX = JSON.parse(readFileSync(new URL('./fixtures/assistant-eval/loosened-checks.json', import.meta.url), 'utf8'));
// M03's fixtures are outputs of its held-out twin N03, so they stay outside the repo with the held-out sets.
const HELDOUT_FX = '/home/franz/.cache/ithomiini-lab/assistant-eval/fixtures/loosened-heldout.json';
const HX = existsSync(HELDOUT_FX) ? JSON.parse(readFileSync(HELDOUT_FX, 'utf8')) : null;
const heldout = { skip: HX ? false : `held-out fixtures not on this machine (${HELDOUT_FX})` };
const TASKS = new URL('../tools/assistant-eval/tasks/', import.meta.url);
const taskOf = (file, id) => JSON.parse(readFileSync(new URL(file, TASKS), 'utf8')).tasks.find(t => t.id === id);
const run = fx => ({ finalText: fx.finalText ?? '', turnTexts: fx.turnTexts, tools: fx.tools ?? [], proposals: [] });
const score = (check, fx) => grade({ checks: [check] }, run(fx), { checkFiles: false }).score;

for (const [file, id] of [['applied.json', 'D14'], ['applied.json', 'D16']]) {
  test(`${id}'s widened "already applied" regex still fails a reply that offers to apply again`, () => {
    const check = taskOf(file, id).checks.find(c => c.regex && c.turn === 2);
    for (const fx of FX.d14Regex.fail) assert.equal(score(check, fx), 0, fx.from);
    for (const fx of FX.d14Regex.pass) assert.equal(score(check, fx), 1, fx.from);
  });
}

test("M03's widened 'offers to look' (anyOf) still fails a reply that neither offers nor sends sheet photos", heldout, () => {
  const check = taskOf('mapsphotos.json', 'M03').checks.find(c => c.anyOf);
  for (const fx of HX.offersToLook.fail) assert.equal(score(check, fx), 0, fx.from);
  for (const fx of HX.offersToLook.pass) {
    assert.equal(score(check, fx), 1, fx.from);
    assert.equal(score(check.anyOf[0], fx), 0, 'the regex alone missed it: the reason for anyOf');
  }
});

test("M03's egg-laying caption check with negationOk still fails a caption that claims it", heldout, () => {
  const check = taskOf('mapsphotos.json', 'M03').checks.find(c => c.block?.captionNot);
  assert.equal(check.block.negationOk, true);
  for (const fx of HX.eggCaption.fail) assert.equal(score(check, fx), 0, fx.from);
  for (const fx of HX.eggCaption.pass) {
    assert.equal(score(check, fx), 1, fx.from);
    assert.equal(score({ ...check, block: { ...check.block, negationOk: false } }, fx), 0, 'without negationOk the denial counted: the reason for the change');
  }
});

// Routing checks (lesson L1): skill loaded, calls counted, and only the first `stopAfter` calls graded.
const call = (name, input = {}) => ({ name, input: JSON.stringify(input) });
test('skill / noSkill read Skill calls and SKILL.md reads', () => {
  const tools = [call('Skill', { skill: 'whatsapp-bridge' }), call('Read', { file_path: '/ws/.claude/skills/maps-gallery/SKILL.md' })];
  assert.equal(grade({ checks: [{ skill: 'maps-gallery' }] }, { tools }).score, 1);
  assert.equal(grade({ checks: [{ skill: 'whatsapp-bridge' }] }, { tools }).score, 1);
  assert.equal(grade({ checks: [{ skill: 'photos' }] }, { tools }).score, 0);
  assert.equal(grade({ checks: [{ noSkill: ['maps-gallery', 'photos'] }] }, { tools }).score, 0);
  assert.equal(grade({ checks: [{ noSkill: 'photos' }] }, { tools }).score, 1);
});

test('maxCalls and noToolArgs', () => {
  const tools = [call('ToolSearch', { query: 'select:mcp__db__host_plants' }), call('ToolSearch', { query: 'select:mcp__db__send_sheet_photos' }), call('host_plants', { butterfly: 'Scada zibia' })];
  assert.equal(grade({ checks: [{ maxCalls: { tool: 'ToolSearch', max: 1 } }] }, { tools }).score, 0);
  assert.equal(grade({ checks: [{ maxCalls: { tool: 'ToolSearch', max: 2 } }] }, { tools }).score, 1);
  assert.equal(grade({ checks: [{ noToolArgs: { tool: 'host_plants', match: { butterfly: 'scada' } } }] }, { tools }).score, 0);
  assert.equal(grade({ checks: [{ noToolArgs: { tool: 'host_plants', match: { butterfly: 'oleria' } } }] }, { tools }).score, 1);
});

test('a routing case is graded on its first stopAfter calls only', () => {
  const tools = [call('Skill', { skill: 'whatsapp-bridge' }), call('query', { sql: 'SELECT 1' }), call('Bash', {})];
  const task = { tier: 'routing', stopAfter: 2, checks: [{ noTool: 'Bash' }, { tool: 'query' }] };
  assert.equal(grade(task, { tools }).score, 1, 'the Bash call came after the stop');
  assert.equal(grade({ ...task, tier: 'e2e' }, { tools }).score, 0.5);
  assert.equal(grade({ ...task, checks: [{ anyOf: [{ noTool: 'Bash' }] }] }, { tools }).score, 1, 'anyOf sees the same calls');
});

test('a routing call the interrupt cut is reported, so the sample can be left out', () => {
  const task = { tier: 'routing', stopAfter: 2 };
  assert.deepEqual(cutGraded(task, [call('Skill'), call('query')]), []);
  assert.deepEqual(cutGraded(task, [call('Skill'), { ...call('taxon_photos'), cut: true }]).map(t => t.name), ['taxon_photos']);
  assert.deepEqual(cutGraded(task, [call('Skill'), call('query'), { ...call('Bash'), cut: true }]), [], 'a call after the stop is not graded');
});

test('a stored routing row is re-graded on the calls it was stopped after', () => {
  const task = { tier: 'routing', stopAfter: 5, checks: [] };
  const tools = [call('Skill'), call('query'), call('query'), call('get_record'), call('propose_changes')];
  assert.match(regradeRouting(task, { stopped: true, stopAfter: 4 }, tools).invalid, /stopped after 4 calls; the task now grades 5/);
  assert.deepEqual(regradeRouting(task, { stopped: true, stopAfter: 8 }, tools), { task: { ...task, stopAfter: 5 } });
  assert.deepEqual(regradeRouting(task, { stopped: false, stopAfter: 4 }, tools.slice(0, 2)), { task }, 'a run that ended by itself made every call it would');
  assert.match(regradeRouting(task, { stopped: true, stopAfter: 5 }, [...tools.slice(0, 4), { ...call('propose_changes'), cut: true }]).invalid, /cut by the interrupt: propose_changes/);
  assert.deepEqual(regradeRouting({ tier: 'e2e' }, {}, tools), { task: { tier: 'e2e' } });
});
