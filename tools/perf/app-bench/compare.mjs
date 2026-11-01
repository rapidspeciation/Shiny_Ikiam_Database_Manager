#!/usr/bin/env node
// The arms of one interleaved run (run.mjs --arms) against its first arm (the base): for the key metrics, each
// arm's median and range beside the base's, the change, and a verdict: "better"/"worse" when the ranges do not
// overlap and the change is over 10 %, else "~". Then whether each arm answered as the base did (the search's
// matches and digests, each tool's answer digest).
//   node tools/perf/app-bench/compare.mjs <run folder> [--all]
import { readFileSync, readdirSync, existsSync } from 'node:fs';
import { join } from 'node:path';
import { metricsOf } from './report.mjs';

const argv = process.argv.slice(2);
const dir = argv.find(a => !a.startsWith('--'));
const all = argv.includes('--all');
const KEY = [
  /^start\.ready_s$/,
  /^run\.(server_hwm_mb|cgroup_peak_mb)$/,
  /^(idle|tools|concurrent|idle_end)\.anon_peak_mb$/,
  /^(pages|search|sync|concurrent)\.loop_probe_(p95|max)_ms$/,
  /^pages\..*\.settled_ms$/,
  /^search\.(first|again)_p50_ms$/,
  /^sync\.ms$/,
  /^tools\.(rack_read_photos|host_plants|identify_species)\.(first|again)_ms$/,
  /^concurrent\.(p50|p95|max)_ms$/,
  /^concurrent\.per_s$/,
  /^(pages|search|sync|concurrent)\.server_main_cpu_s$/,
];
const HIGHER_IS_BETTER = /per_s$/;

const median = xs => {
  const s = [...xs].sort((a, b) => a - b);
  const m = s.length >> 1;
  return s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2;
};
const r1 = n => Math.round(n * 10) / 10;
const arms = readdirSync(dir).filter(d => existsSync(join(dir, d, 'results.json')));
const data = Object.fromEntries(arms.map(a => [a, JSON.parse(readFileSync(join(dir, a, 'results.json'), 'utf8'))]));
const order = data[arms[0]].meta.arms ?? arms;
const [base, ...others] = order;
const metrics = Object.fromEntries(order.map(a => [a, data[a].results.map(metricsOf)]));
const summary = (a, k) => {
  const xs = metrics[a].map(m => m[k]).filter(x => x != null);
  return xs.length ? { med: median(xs), lo: Math.min(...xs), hi: Math.max(...xs), n: xs.length } : null;
};
const keys = Object.keys(Object.assign({}, ...metrics[base])).filter(k => all || KEY.some(re => re.test(k)));
console.log(`| metric | ${base} | ${others.map(a => `${a} | Δ`).join(' | ')} |`);
console.log(`|---|---:|${others.map(() => '---:|---|').join('')}`);
for (const k of keys) {
  const b = summary(base, k);
  if (!b) continue;
  const cells = others.map(a => {
    const s = summary(a, k);
    if (!s) return ' | ';
    const pct = b.med ? ((s.med - b.med) / b.med) * 100 : 0;
    const apart = s.hi < b.lo || s.lo > b.hi;
    const better = HIGHER_IS_BETTER.test(k) ? pct > 0 : pct < 0;
    const verdict = apart && Math.abs(pct) > 10 ? (better ? '**better**' : '**worse**') : '~';
    return `${r1(s.med)} (${r1(s.lo)}–${r1(s.hi)}) | ${pct >= 0 ? '+' : ''}${r1(pct)} % ${verdict}`;
  });
  console.log(`| ${k} | ${r1(b.med)} (${r1(b.lo)}–${r1(b.hi)}) | ${cells.join(' | ')} |`);
}

// Behaviour: the same answers as the base's (search: matches and digest; tools: digest of the answer's text).
const prints = a =>
  data[a].results.map(r => {
    const out = {};
    for (const s of Array.isArray(r.workloads.search) ? r.workloads.search : []) out[`search ${s.q} ${s.pass}`] = `${s.found} ${s.digest}`;
    for (const t of Array.isArray(r.workloads.tools) ? r.workloads.tools : []) if (t.pass) out[`${t.tool} ${t.pass}`] = t.digest ?? null;
    return out;
  });
console.log('');
const basePrints = prints(base);
for (const a of others) {
  const diffs = new Set();
  for (const p of prints(a))
    for (const [k, v] of Object.entries(p)) if (basePrints.some(bp => k in bp) && !basePrints.some(bp => bp[k] === v)) diffs.add(k);
  // The base's own answers that differ between its repeats are not the change's.
  const unstable = new Set(Object.keys(basePrints[0] ?? {}).filter(k => new Set(basePrints.map(bp => bp[k])).size > 1));
  const real = [...diffs].filter(k => !unstable.has(k));
  console.log(`${a}: ${real.length ? `answers differ from ${base}: ${real.join('; ')}` : `same answers as ${base}`}${unstable.size ? ` (${base} itself varies on: ${[...unstable].join('; ')})` : ''}`);
}
