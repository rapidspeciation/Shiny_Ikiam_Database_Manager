// Case discipline of the assistant eval (lesson L3): every task names its kind and source, controls exist,
// routing cases are graded on tool calls only, and every check is one the grader knows.
import test from 'node:test';
import assert from 'node:assert/strict';
import { readdirSync, readFileSync } from 'node:fs';
import { grade } from '../tools/assistant-eval/grade.mjs';

const DIR = new URL('../tools/assistant-eval/tasks/', import.meta.url);
// notebook.json and other git-ignored task files are checked too when present.
const files = readdirSync(DIR).filter(f => f.endsWith('.json'));
const all = files.flatMap(f => JSON.parse(readFileSync(new URL(f, DIR), 'utf8')).tasks.map(t => ({ ...t, file: f })));
const TEXT = ['includes', 'includesAny', 'excludes', 'regex', 'noRegex', 'block', 'proposal', 'proposalOnly', 'noApply'];

test('every task has a kind (bad, guard or control) and a source', () => {
  const bad = all.filter(t => !['bad', 'guard', 'control'].includes(t.kind) || typeof t.source !== 'string' || t.source.trim().length < 3);
  assert.deepEqual(bad.map(t => `${t.file} ${t.id}`), []);
});

test('task ids are unique within a file', () => {
  for (const f of files) {
    const ids = all.filter(t => t.file === f).map(t => t.id);
    assert.equal(new Set(ids).size, ids.length, f);
  }
});

test('the routing set has 3 or more controls that keep maps-gallery, drafts and ToolSearch out', () => {
  const controls = all.filter(t => t.file === 'routing.json' && t.kind === 'control');
  assert.ok(controls.length >= 3, `${controls.length} controls`);
  for (const t of controls) {
    const banned = t.checks.flatMap(c => [c.noTool, c.noSkill].flat()).filter(Boolean);
    for (const name of ['maps-gallery', 'propose_changes', 'ToolSearch']) assert.ok(banned.includes(name), `${t.id} does not rule out ${name}`);
  }
});

test('routing cases have one prompt, a stop, and only tool checks', () => {
  for (const t of all.filter(t => t.tier === 'routing')) {
    assert.ok(typeof t.prompt === 'string' && !t.turns, `${t.id}: one prompt`);
    assert.ok(Number.isInteger(t.stopAfter ?? 3) && (t.stopAfter ?? 3) > 0, `${t.id}: stopAfter`);
    const text = t.checks.filter(c => TEXT.some(k => k in c) || c.anyOf?.some(s => TEXT.some(k => k in s)));
    assert.deepEqual(text, [], `${t.id}: a stopped run has no answer to grade`);
  }
});

test('every check of every task is one the grader knows', () => {
  for (const t of all.filter(t => t.checks))
    assert.doesNotThrow(() => grade(t, { finalText: '', turnTexts: [], tools: [], proposals: [] }, { checkFiles: false }), `${t.file} ${t.id}`);
});

test('a task asked in several files has the same kind in each', () => {
  const byPrompt = new Map();
  for (const t of all) {
    const key = JSON.stringify(t.turns ?? t.prompt ?? t.calls);
    byPrompt.set(key, [...(byPrompt.get(key) ?? []), t]);
  }
  for (const ts of byPrompt.values()) assert.equal(new Set(ts.map(t => t.kind)).size, 1, ts.map(t => `${t.file} ${t.id}: ${t.kind}`).join('; '));
});
