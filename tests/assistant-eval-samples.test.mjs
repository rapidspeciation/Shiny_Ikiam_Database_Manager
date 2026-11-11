// Samples and the pass rule of the assistant eval (tools/assistant-eval/samples.mjs, compare.mjs).
import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, writeFileSync, mkdirSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { idleAfterStop, needsRetry, needsTieBreak, passRate, samplePass, verdict } from '../tools/assistant-eval/samples.mjs';
import { compare } from '../tools/assistant-eval/compare.mjs';

const row = (id, score, extra = {}) => ({ id, score, usage: { input: 1000, cachedInput: 800, cacheCreation: 100, output: 50 }, runMs: 1000, toolCalls: 2, toolErrors: 0, commands: 0, ...extra });

test('a sample passes at score ≥ passAt (default 1.0)', () => {
  assert.equal(samplePass(row('A', 1)), true);
  assert.equal(samplePass(row('A', 0.86)), false);
  assert.equal(samplePass(row('A', 0.86, { passAt: 0.8 })), true);
  assert.equal(samplePass({ id: 'A', score: 0, failed: 'crash' }), false);
});

test('a case fails when half or more of its samples fail', () => {
  assert.equal(verdict([row('A', 1), row('A', 1)]).pass, true);
  assert.equal(verdict([row('A', 1), row('A', 0.5)]).pass, false);
  assert.equal(verdict([row('A', 1), row('A', 0.5), row('A', 1)]).pass, true);
  assert.equal(verdict([row('A', 1), row('A', 0.5), row('A', 0.5)]).label, '1/3');
  assert.equal(verdict([row('A', 1), row('A', 0.5, { invalid: 'app down' })]).label, '1/1', 'an invalid row is no sample');
});

test('a 3rd sample runs only when the first 2 disagree', () => {
  assert.equal(needsTieBreak([row('A', 1), row('A', 0.5)], 2), true);
  assert.equal(needsTieBreak([row('A', 1), row('A', 1)], 2), false);
  assert.equal(needsTieBreak([row('A', 0), row('A', 0.5)], 2), false);
  assert.equal(needsTieBreak([row('A', 1), row('A', 0.5), row('A', 1)], 2), false, 'once');
});

test('pass rate counts cases and samples', () => {
  const r = passRate([row('A', 1), row('A', 0.5), row('A', 1), row('B', 0), row('B', 0)]);
  assert.deepEqual(r, { cases: 2, casesPassed: 1, samples: 5, samplesPassed: 2, uncounted: [] });
});

test('compare reads an old single-run folder against a folder with samples', () => {
  const dir = mkdtempSync(join(tmpdir(), 'eval-cmp-'));
  const write = (name, rows) => {
    mkdirSync(join(dir, name));
    writeFileSync(join(dir, name, 'results.jsonl'), rows.map(r => JSON.stringify(r)).join('\n') + '\n');
    return join(dir, name);
  };
  const old = write('old', [row('A', 0.86), row('B', 1)]);
  const fresh = write('new', [row('A', 1, { sample: 1 }), row('A', 0.86, { sample: 2 }), row('A', 1, { sample: 3 }), row('B', 1, { sample: 1 }), row('B', 1, { sample: 2 })]);
  const { rows, set } = compare([old], fresh);
  const a = rows.find(r => r.id === 'A');
  assert.equal(`${a.pass0} → ${a.pass1}`, '0/1 → 2/3');
  assert.equal(a.case0, false);
  assert.equal(a.case1, true);
  assert.ok(Math.abs(a.q1 - (1 + 0.86 + 1) / 3) < 1e-9, 'mean quality over samples');
  assert.deepEqual(set.passGained, ['A']);
  assert.deepEqual(set.pass1, { cases: 2, casesPassed: 2, samples: 5, samplesPassed: 4, uncounted: [] });
});

test('compare leaves a sample without usage out of the cost means', () => {
  const dir = mkdtempSync(join(tmpdir(), 'eval-cmp-'));
  const write = (name, rows) => {
    mkdirSync(join(dir, name));
    writeFileSync(join(dir, name, 'results.jsonl'), rows.map(r => JSON.stringify(r)).join('\n') + '\n');
    return join(dir, name);
  };
  const base = write('base', [row('A', 1)]);
  const variant = write('variant', [row('A', 1, { sample: 1 }), row('A', 1, { sample: 2, usage: { input: 0, cachedInput: 0, cacheCreation: 0, output: 0, unavailable: true } })]);
  const a = compare([base], variant).rows[0];
  assert.equal(a.w1, a.w0, 'the sample with usage.unavailable is not averaged in as zero');
  assert.equal(a.pass1, '2/2', 'it still counts for quality');
});

test('an invalid sample gets one more try; a skipped or crashed one does not', () => {
  assert.equal(needsRetry(row('A', 1, { invalid: 'graded call cut' })), true);
  assert.equal(needsRetry(row('A', 0, { failed: 'crash' })), false, 'a crash is a sample (score 0)');
  assert.equal(needsRetry({ id: 'A', skipped: 'run stopped' }), false);
  assert.equal(needsRetry(row('A', 0.5)), false);
});

test('a stop answer confirms an idle chat only when ok and not busy', () => {
  assert.equal(idleAfterStop({ ok: true, stopped: 1, busy: false }), true);
  assert.equal(idleAfterStop({ ok: true, stopped: 1, busy: true }), false);
  assert.equal(idleAfterStop({ ok: false, error: 'socket' }), false);
  assert.equal(idleAfterStop(undefined), false);
});

test('a case without a counted sample is listed, not dropped', () => {
  const r = passRate([row('A', 1), row('B', 1, { invalid: 'cut' }), { id: 'B', skipped: 'run stopped' }]);
  assert.deepEqual(r.uncounted, ['B']);
  assert.equal(r.cases, 1);
  const dir = mkdtempSync(join(tmpdir(), 'eval-cmp-'));
  for (const [name, rows] of [['base', [row('A', 1), row('B', 1)]], ['variant', [row('A', 1, { sample: 1 }), row('B', 1, { sample: 1, invalid: 'cut' })]]]) {
    mkdirSync(join(dir, name));
    writeFileSync(join(dir, name, 'results.jsonl'), rows.map(x => JSON.stringify(x)).join('\n') + '\n');
  }
  const { set } = compare([join(dir, 'base')], join(dir, 'variant'));
  assert.deepEqual(set.uncounted1, ['B']);
  assert.deepEqual(set.uncounted0, []);
});
