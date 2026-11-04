#!/usr/bin/env node
// Paired comparison of eval runs: node tools/assistant-eval/compare.mjs <baseline dir>[,<baseline dir 2>…] <variant dir> [--md]
// Per task present in both: quality, weighted tokens, time, tool calls and errors, Bash; then the set:
// paired mean quality delta, paired mean ratio of weighted tokens (geometric), total weighted tokens, median time ratio.
// Several baseline dirs (repeats) are averaged per task, and their spread is shown as the noise next to the effect.
import { readFileSync, existsSync } from 'node:fs';
import { join } from 'node:path';

export const neutral = u => (u ? 0.1 * u.input + 5 * u.output : null);
export const weighted = u => (u ? u.input - u.cachedInput - u.cacheCreation + 0.1 * u.cachedInput + 2 * u.cacheCreation + 5 * u.output : null);
const load = dir =>
  existsSync(join(dir, 'results.jsonl'))
    ? Object.fromEntries(
        readFileSync(join(dir, 'results.jsonl'), 'utf8')
          .trim()
          .split('\n')
          .map(l => JSON.parse(l))
          .filter(r => r.usage || r.failed)
          .map(r => [r.id, r]),
      )
    : {};
const mean = a => a.reduce((s, x) => s + x, 0) / a.length;
const median = a => [...a].sort((x, y) => x - y)[Math.floor((a.length - 1) / 2)];

export function compare(baseDirs, variantDir) {
  const bases = baseDirs.map(load);
  const v = load(variantDir);
  const rows = [];
  for (const id of Object.keys(v).sort()) {
    const bs = bases.map(b => b[id]).filter(Boolean);
    if (!bs.length) continue;
    const bw = bs.map(b => weighted(b.usage) ?? 0);
    rows.push({
      id,
      q0: mean(bs.map(b => b.score ?? 0)),
      q1: v[id].score ?? 0,
      w0: mean(bw),
      w0spread: bw.length > 1 ? Math.max(...bw) / Math.min(...bw) : null,
      w1: weighted(v[id].usage) ?? 0,
      n0: mean(bs.map(b => neutral(b.usage) ?? 0)),
      n1: neutral(v[id].usage) ?? 0,
      t0: mean(bs.map(b => b.runMs ?? 0)),
      t1: v[id].runMs ?? 0,
      tools0: mean(bs.map(b => b.toolCalls ?? 0)),
      tools1: v[id].toolCalls ?? 0,
      err0: mean(bs.map(b => b.toolErrors ?? 0)),
      err1: v[id].toolErrors ?? 0,
      bash0: mean(bs.map(b => b.commands ?? 0)),
      bash1: v[id].commands ?? 0,
    });
  }
  const ok = rows.filter(r => r.w0 > 0 && r.w1 > 0);
  const set = {
    n: rows.length,
    qualityDelta: mean(rows.map(r => r.q1 - r.q0)),
    tokenRatio: Math.exp(mean(ok.map(r => Math.log(r.w1 / r.w0)))),
    neutralRatio: Math.exp(mean(ok.map(r => Math.log(r.n1 / r.n0)))),
    neutralTotal0: ok.reduce((s, r) => s + r.n0, 0),
    neutralTotal1: ok.reduce((s, r) => s + r.n1, 0),
    totalW0: ok.reduce((s, r) => s + r.w0, 0),
    totalW1: ok.reduce((s, r) => s + r.w1, 0),
    timeRatio: median(ok.map(r => r.t1 / r.t0)),
    errors0: rows.reduce((s, r) => s + r.err0, 0),
    errors1: rows.reduce((s, r) => s + r.err1, 0),
    bash0: rows.reduce((s, r) => s + r.bash0, 0),
    bash1: rows.reduce((s, r) => s + r.bash1, 0),
  };
  return { rows, set };
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const [baseArg, variantDir] = process.argv.slice(2);
  const { rows, set } = compare(baseArg.split(','), variantDir);
  const k = n => `${Math.round(n / 1000)}k`;
  console.log('| task | quality base → variant | neutral | weighted tokens base → variant | ratio | base spread | time s | tool calls | tool errors | Bash |');
  console.log('|---|---|---|---|---|---|---|---|---|---|');
  for (const r of rows)
    console.log(
      `| ${r.id} | ${r.q0.toFixed(2)} → ${r.q1.toFixed(2)} | ${k(r.n0)} → ${k(r.n1)} (×${(r.n1 / r.n0).toFixed(2)}) | ${k(r.w0)} → ${k(r.w1)} | ${(r.w1 / r.w0).toFixed(2)} | ${r.w0spread ? '×' + r.w0spread.toFixed(2) : '-'} | ${(r.t0 / 1000).toFixed(0)} → ${(r.t1 / 1000).toFixed(0)} | ${r.tools0.toFixed(1)} → ${r.tools1} | ${r.err0.toFixed(1)} → ${r.err1} | ${r.bash0.toFixed(1)} → ${r.bash1} |`,
    );
  console.log(
    `\n**Set (${set.n} paired tasks):** quality ${set.qualityDelta >= 0 ? '+' : ''}${set.qualityDelta.toFixed(3)}; **neutral ×${set.neutralRatio.toFixed(3)}** (total ${k(set.neutralTotal0)} → ${k(set.neutralTotal1)}); weighted tokens ×${set.tokenRatio.toFixed(3)} (geometric mean of ratios), total ${k(set.totalW0)} → ${k(set.totalW1)} (${(((set.totalW1 - set.totalW0) / set.totalW0) * 100).toFixed(1)} %); median time ×${set.timeRatio.toFixed(2)}; tool errors ${set.errors0.toFixed(1)} → ${set.errors1}; Bash ${set.bash0.toFixed(1)} → ${set.bash1}`,
  );
}
