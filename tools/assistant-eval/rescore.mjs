#!/usr/bin/env node
// Re-score stored results after checks were ADDED to a task, without the lab thread store:
//   node tools/assistant-eval/rescore.mjs <tasks.json> <from dir> <new dir>
// Old check results are kept as stored; only checks not in the stored list are graded, and only text and
// tool checks (they read the stored finalText, turnTexts and tools). Then the score is recomputed.
import { readFileSync, writeFileSync, mkdirSync } from 'node:fs';
import { join } from 'node:path';
import { grade } from './grade.mjs';

const [tasksFile, from, out] = process.argv.slice(2);
const byId = Object.fromEntries(JSON.parse(readFileSync(tasksFile, 'utf8')).tasks.map(t => [t.id, t]));
const key = c => JSON.stringify(c);
const rows = readFileSync(join(from, 'results.jsonl'), 'utf8').trim().split('\n').map(l => JSON.parse(l));
const fresh = rows.map(r => {
  const t = byId[r.id];
  if (!t || !r.checks) return r;
  const have = new Set(r.checks.map(c => key(c.check)));
  const added = t.checks.filter(c => !have.has(key(c)));
  if (!added.length) return r;
  if (added.some(c => c.proposal || c.proposalOnly || c.noApply)) throw new Error(`${r.id}: added proposal check needs --regrade`);
  const g = grade({ checks: added }, { finalText: r.finalText, turnTexts: r.turnTexts, tools: r.tools ?? [] });
  const checks = [...r.checks, ...g.results];
  const total = checks.reduce((s, c) => s + c.w, 0);
  return { ...r, checks, score: checks.filter(c => c.pass).reduce((s, c) => s + c.w, 0) / total };
});
mkdirSync(out, { recursive: true });
writeFileSync(join(out, 'results.jsonl'), fresh.map(r => JSON.stringify(r)).join('\n') + '\n');
for (const r of fresh) if (r.checks) console.log(r.id, r.score?.toFixed(2), r.checks.filter(c => !c.pass).map(c => Object.keys(c.check)[0] + (c.detail ? ' ' + c.detail : '')).join(' | '));
