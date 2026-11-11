#!/usr/bin/env node
// Paired comparison of eval runs: node tools/assistant-eval/compare.mjs <baseline dir>[,<baseline dir 2>…] <variant dir> [--md]
// Per task present in both: quality, weighted tokens, time, tool calls and errors, Bash; then the set:
// paired mean quality delta, paired mean ratio of weighted tokens (geometric), total weighted tokens, median time ratio.
// Several baseline dirs (repeats) are averaged per task, and their spread is shown as the noise next to the effect.
// Samples (samples.mjs): every row of a case, from every dir given for that side, is one sample; quality, tokens and
// time are means over samples, and each case shows its passed samples ("2/3 → 3/3") and verdict, then the overall
// pass rate per side. Folders from before samples (one row per task, no `sample`) read as one sample per row.
import { byCase, counted, loadRows, passRate, rateText, verdict } from './samples.mjs';

import { existsSync as fixedExists, readFileSync as fixedRead } from 'node:fs';
import { fixedDiff } from './fixed.mjs';
export const neutral = u => (u ? 0.1 * u.input + 5 * u.output : null);
export const weighted = u => (u ? u.input - u.cachedInput - u.cacheCreation + 0.1 * u.cachedInput + 2 * u.cacheCreation + 5 * u.output : null);
/** Chat rows of these dirs (with usage, crashed, invalid or skipped); unit rows never count, even when skipped. */
const chatRowsOf = dirs => dirs.flatMap(loadRows).filter(r => r.tier !== 'unit' && (r.usage || r.failed || r.invalid || r.skipped));
/** Case ids with chat rows but no counted sample: listed in the output, never dropped silently. */
const uncountedOf = rows => passRate(rows).uncounted;
const hasUsage = r => Boolean(r.usage) && !r.usage.unavailable;
const mean = a => a.reduce((s, x) => s + x, 0) / a.length;
const median = a => [...a].sort((x, y) => x - y)[Math.floor((a.length - 1) / 2)];

export function compare(baseDirs, variantDir) {
  const baseAll = chatRowsOf(baseDirs);
  const variantAll = chatRowsOf([variantDir]);
  const base = byCase(baseAll.filter(counted));
  const variant = byCase(variantAll.filter(counted));
  const rows = [];
  const m = (rs, f) => mean(rs.map(f));
  // Cost and time: means over the samples with a usage (a crashed sample, or one whose usage T3 and the transcript
  // both lack, counts in quality only).
  const u = (rs, f) => (rs.some(hasUsage) ? mean(rs.filter(hasUsage).map(f)) : 0);
  for (const id of [...variant.keys()].sort()) {
    const bs = base.get(id);
    const vs = variant.get(id);
    if (!bs?.length) continue;
    const bw = bs.filter(hasUsage).map(b => weighted(b.usage));
    const vb = verdict(bs);
    const vv = verdict(vs);
    rows.push({
      id,
      pass0: vb.label,
      pass1: vv.label,
      case0: vb.pass,
      case1: vv.pass,
      q0: m(bs, b => b.score ?? 0),
      q1: m(vs, b => b.score ?? 0),
      w0: bw.length ? mean(bw) : 0,
      w0spread: bw.length > 1 ? Math.max(...bw) / Math.min(...bw) : null,
      w1: u(vs, b => weighted(b.usage)),
      n0: u(bs, b => neutral(b.usage)),
      n1: u(vs, b => neutral(b.usage)),
      t0: u(bs, b => b.runMs ?? 0),
      t1: u(vs, b => b.runMs ?? 0),
      tools0: u(bs, b => b.toolCalls ?? 0),
      tools1: u(vs, b => b.toolCalls ?? 0),
      err0: u(bs, b => b.toolErrors ?? 0),
      err1: u(vs, b => b.toolErrors ?? 0),
      bash0: u(bs, b => b.commands ?? 0),
      bash1: u(vs, b => b.commands ?? 0),
    });
  }
  const ok = rows.filter(r => r.w0 > 0 && r.w1 > 0);
  const paired = new Set(rows.map(r => r.id));
  const set = {
    n: rows.length,
    pass0: passRate([...base.entries()].filter(([id]) => paired.has(id)).flatMap(([, rs]) => rs)),
    pass1: passRate([...variant.entries()].filter(([id]) => paired.has(id)).flatMap(([, rs]) => rs)),
    uncounted0: uncountedOf(baseAll),
    uncounted1: uncountedOf(variantAll),
    passLost: rows.filter(r => r.case0 && !r.case1).map(r => r.id),
    passGained: rows.filter(r => !r.case0 && r.case1).map(r => r.id),
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
  // Did baseline and variant see the same fixed context (fixed.mjs)? A difference that is not the change tested makes a cost comparison suspect.
  const manifest = d => (fixedExists(`${d}/run.json`) ? JSON.parse(fixedRead(`${d}/run.json`, 'utf8')).fixed ?? null : null);
  const vFixed = manifest(variantDir);
  for (const b of baseArg.split(',')) {
    const diff = fixedDiff(manifest(b), vFixed);
    console.log(diff === null ? `Fixed context: no manifest for ${b.split('/').pop()} or the variant.` : diff.length ? `Fixed context differs from ${b.split('/').pop()}: ${diff.join(', ')}.` : `Fixed context: same as ${b.split('/').pop()}.`);
  }
  const { rows, set } = compare(baseArg.split(','), variantDir);
  const k = n => `${Math.round(n / 1000)}k`;
  const f1 = x => (Number.isInteger(x) ? String(x) : x.toFixed(1));
  const mark = (p, label) => `${label}${p ? '' : ' ✗'}`;
  console.log('| task | samples passed base → variant | quality base → variant | neutral | weighted tokens base → variant | ratio | base spread | time s | tool calls | tool errors | Bash |');
  console.log('|---|---|---|---|---|---|---|---|---|---|---|');
  for (const r of rows)
    console.log(
      `| ${r.id} | ${mark(r.case0, r.pass0)} → ${mark(r.case1, r.pass1)} | ${r.q0.toFixed(2)} → ${r.q1.toFixed(2)} | ${k(r.n0)} → ${k(r.n1)} (×${(r.n1 / r.n0).toFixed(2)}) | ${k(r.w0)} → ${k(r.w1)} | ${(r.w1 / r.w0).toFixed(2)} | ${r.w0spread ? '×' + r.w0spread.toFixed(2) : '-'} | ${(r.t0 / 1000).toFixed(0)} → ${(r.t1 / 1000).toFixed(0)} | ${r.tools0.toFixed(1)} → ${f1(r.tools1)} | ${r.err0.toFixed(1)} → ${f1(r.err1)} | ${r.bash0.toFixed(1)} → ${f1(r.bash1)} |`,
    );
  console.log('\n✗ = the case fails (half or more of its samples below passAt, default 1.0).');
  if (set.uncounted0.length || set.uncounted1.length)
    console.log(`**No counted sample** (all invalid or skipped; not in the table): base ${set.uncounted0.join(', ') || 'none'}; variant ${set.uncounted1.join(', ') || 'none'}`);
  console.log(`**Pass rate (paired cases):** base ${rateText(set.pass0)} → variant ${rateText(set.pass1)}${set.passLost.length ? `; lost: ${set.passLost.join(', ')}` : ''}${set.passGained.length ? `; gained: ${set.passGained.join(', ')}` : ''}`);
  if (!set.n) console.log('\n**Set:** no paired case with a counted sample on both sides.');
  else console.log(
    `\n**Set (${set.n} paired tasks):** quality ${set.qualityDelta >= 0 ? '+' : ''}${set.qualityDelta.toFixed(3)}; **neutral ×${set.neutralRatio.toFixed(3)}** (total ${k(set.neutralTotal0)} → ${k(set.neutralTotal1)}); weighted tokens ×${set.tokenRatio.toFixed(3)} (geometric mean of ratios), total ${k(set.totalW0)} → ${k(set.totalW1)} (${(((set.totalW1 - set.totalW0) / set.totalW0) * 100).toFixed(1)} %); median time ×${set.timeRatio.toFixed(2)}; tool errors ${set.errors0.toFixed(1)} → ${f1(set.errors1)}; Bash ${set.bash0.toFixed(1)} → ${f1(set.bash1)}`,
  );
}
