import test from 'node:test';
import { execFileSync } from 'node:child_process';
import assert from 'node:assert/strict';
import { againstBase, compareBudget, measureLean, measureParts, readBudget, updatedBudget } from '../scripts/context-budget.mjs';
import { checkProbe } from '../scripts/context-probe-check.mjs';

// Every chat's model call carries the brief, the listed skills and the always-loaded tools: their sizes are
// recorded in assistant/context-budget.json so that a growth is a decision, with its reason, and not a drift.

const budget = readBudget();
const parts = await measureParts();
const lean = await measureLean();
const diff = compareBudget(budget, parts, lean);
const show = list => list.map(d => `${d.key}: ${d.was ?? '-'} → ${d.bytes ?? 'gone'}`).join('; ');

test('no part of the fixed context grew beyond assistant/context-budget.json', () => {
  assert.deepEqual(
    [...diff.grown, ...diff.added],
    [],
    `grew: ${show([...diff.grown, ...diff.added])}. If it is worth its tokens on every call: node scripts/context-budget.mjs --write --reason "why"`,
  );
});

test('assistant/context-budget.json is up to date (a shrink is recorded too)', () => {
  assert.deepEqual([...diff.shrunk, ...diff.removed], [], `shrank or went: ${show([...diff.shrunk, ...diff.removed])}. Run node scripts/context-budget.mjs --write`);
  assert.equal(diff.leanStale && !diff.leanLost.length, false, 'the lean settings gained an entry: node scripts/context-budget.mjs --write');
});

test('the lean Claude Code settings keep every key and deny rule', () => {
  assert.deepEqual(diff.leanLost, []);
  for (const key of ['enableArtifact', 'disableBundledSkills', 'deny', 'claudeMdExcludes']) assert.ok(key in budget.lean, `lean.${key} recorded`);
  assert.equal(lean.enableArtifact, false);
  assert.equal(lean.disableBundledSkills, true);
  // Pinned here too, so a --write cannot quietly record a shorter list.
  assert.deepEqual(budget.lean.deny, [
    'Agent(Explore)', 'Agent(Plan)', 'Agent(claude)', 'Agent(claude-code-guide)', 'Agent(general-purpose)', 'Agent(statusline-setup)', 'Agent(web-fetch)',
    'ListAgents', 'ReportFindings', 'ScheduleWakeup', 'Workflow',
  ]);
});

test('writing a lost lean setting needs a reason', () => {
  const base = { parts: {}, lean: { enableArtifact: false, deny: ['Workflow', 'ListAgents'] } };
  assert.throws(() => updatedBudget(base, {}, { enableArtifact: false, deny: ['Workflow'] }), /lean setting lost: deny: ListAgents/);
  assert.deepEqual(updatedBudget(base, {}, { enableArtifact: false, deny: ['Workflow'] }, 'ListAgents is used now').lean.deny, ['Workflow']);
});

test('--against a ref that does not exist fails', () => {
  const script = new URL('../scripts/context-budget.mjs', import.meta.url).pathname;
  let status = 0;
  try {
    execFileSync(process.execPath, [script, '--against', 'no-such-ref-for-the-budget-test'], { stdio: 'pipe' });
  } catch (e) {
    status = e.status;
  }
  assert.equal(status, 2);
});

test('a growth needs a reason; a shrink does not', () => {
  const base = { parts: { brief: { bytes: 100, reason: 'old' } }, lean: {} };
  assert.throws(() => updatedBudget(base, { brief: 101 }, {}), /grew: brief/);
  assert.deepEqual(updatedBudget(base, { brief: 101 }, {}, 'a new rule').parts.brief, { bytes: 101, reason: 'a new rule' });
  assert.deepEqual(updatedBudget(base, { brief: 90 }, {}).parts.brief, { bytes: 90, reason: 'old' });
});

test('the deploy check refuses a growth over the base without a reason of its own', () => {
  const base = { parts: { brief: { bytes: 100, reason: 'old' } }, lean: { enableArtifact: false, deny: ['Workflow'] } };
  const lean = { enableArtifact: false, deny: ['Workflow'] };
  assert.deepEqual(againstBase(base, { parts: { brief: { bytes: 90, reason: 'old' } }, lean }), []);
  assert.equal(againstBase(base, { parts: { brief: { bytes: 101, reason: 'old' } }, lean }).length, 1);
  assert.deepEqual(againstBase(base, { parts: { brief: { bytes: 101, reason: 'new rule' } }, lean }), []);
  assert.equal(againstBase(base, { parts: { brief: { bytes: 100, reason: 'old' }, 'skill:x': { bytes: 5 } }, lean }).length, 1);
  assert.match(againstBase(base, { parts: { brief: { bytes: 100 } }, lean: { enableArtifact: false, deny: [] } })[0], /lean setting lost: deny/);
});

test('the probe check after a Claude Code update', () => {
  const init = tools => JSON.stringify({ type: 'system', subtype: 'init', tools });
  const call = n => JSON.stringify({ type: 'assistant', message: { usage: { input_tokens: 4, cache_creation_input_tokens: n - 4, cache_read_input_tokens: 0 } } });
  assert.equal(checkProbe([init(['Bash', 'mcp__db__query']), call(35729)]).ok, true);
  assert.match(checkProbe([init(['Bash', 'Artifact']), call(35729)]).problems[0], /Artifact/);
  assert.match(checkProbe([init(['Bash']), call(57794)]).problems[0], /57794/);
});
