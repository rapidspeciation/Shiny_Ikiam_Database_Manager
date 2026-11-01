#!/usr/bin/env node
// A run's results.json (run.mjs) as Markdown: the median over the repeats of each metric, and with a second
// run (--vs other/results.json) both side by side with the change in %.
//   node tools/perf/app-bench/report.mjs <results.json> [--vs <before results.json>] [--json]
import { readFileSync } from 'node:fs';

const argv = process.argv.slice(2);
const vsAt = argv.indexOf('--vs');
const vsFile = vsAt >= 0 ? argv.splice(vsAt, 2)[1] : null;
const asJson = argv.includes('--json');
const file = argv.find(a => !a.startsWith('--'));
if (!file) throw new Error('report.mjs <results.json> [--vs <before>]');

const median = xs => {
  const s = xs.filter(x => typeof x === 'number' && Number.isFinite(x)).sort((a, b) => a - b);
  if (!s.length) return null;
  const m = s.length >> 1;
  return s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2;
};
const r1 = n => (n == null ? null : Math.round(n * 10) / 10);

/** The metrics of one repeat, flat: name → number. */
export function metricsOf(rep) {
  const m = {};
  const put = (k, v) => {
    if (typeof v === 'number' && Number.isFinite(v)) m[k] = v;
  };
  const w = rep.workloads;
  put('start.listen_s', w.cold_start?.listenS);
  put('start.ready_s', w.cold_start?.readyS);
  put('run.cgroup_peak_mb', rep.cgroupPeakMb?.mem);
  put('run.cgroup_swap_peak_mb', rep.cgroupPeakMb?.swap);
  put('run.server_hwm_mb', Math.max(0, ...Object.entries(rep.peakHwmMb ?? {}).filter(([k]) => k.startsWith('server:')).map(([, v]) => v)));
  for (const [phase, p] of Object.entries(rep.phases)) {
    put(`${phase}.peak_total_mb`, p.peakTotalMb);
    put(`${phase}.anon_peak_mb`, p.peakAnonMb);
    put(`${phase}.anon_end_mb`, p.endAnonMb);
    put(`${phase}.end_total_mb`, p.endTotalMb);
    put(`${phase}.seconds`, p.seconds);
    for (const [kind, v] of Object.entries(p.processes ?? {})) {
      put(`${phase}.${kind}.peak_rss_mb`, v.peakRssMb);
      put(`${phase}.${kind}.peak_swap_mb`, v.peakSwapMb);
    }
    put(`${phase}.cpu_s`, Object.values(p.cpuS ?? {}).reduce((a, b) => a + b, 0));
    put(`${phase}.server_main_cpu_s`, p.serverThreadsCpuS?.MainThread ?? p.serverThreadsCpuS?.node);
    put(`${phase}.loop_probe_p95_ms`, p.probe?.p95);
    put(`${phase}.loop_probe_max_ms`, p.probe?.max);
  }
  for (const t of Array.isArray(w.pages) ? w.pages : []) {
    put(`pages.${t.tab}.settled_ms`, t.settledMs);
    put(`pages.${t.tab}.kb`, t.kb);
  }
  if (Array.isArray(w.search)) {
    const first = w.search.filter(x => x.pass === 'first').map(x => x.ms);
    const again = w.search.filter(x => x.pass === 'again').map(x => x.ms);
    put('search.first_p50_ms', median(first));
    put('search.first_max_ms', Math.max(...first));
    put('search.again_p50_ms', median(again));
    put('search.kb_total', w.search.reduce((a, x) => a + (x.kb ?? 0), 0));
  }
  put('sync.ms', w.sync?.ms);
  for (const t of Array.isArray(w.tools) ? w.tools : []) {
    if (!t.pass) continue;
    put(`tools.${t.tool}.${t.pass}_ms`, t.ms);
    put(`tools.${t.tool}.${t.pass}_kb`, t.kb);
  }
  if (w.offline?.available) {
    put('offline.total_s', w.offline.totalS);
    put('offline.wire_mb', w.offline.files.reduce((a, f) => a + f.wireMb, 0));
  }
  const c = w.concurrent;
  if (c && !c.error) {
    put('concurrent.p50_ms', c.p50);
    put('concurrent.p95_ms', c.p95);
    put('concurrent.max_ms', c.max);
    put('concurrent.per_s', c.perSecond);
    put('concurrent.errors', c.errors);
  }
  return m;
}

function summarize(path) {
  const data = JSON.parse(readFileSync(path, 'utf8'));
  const per = data.results.map(metricsOf);
  const keys = [...new Set(per.flatMap(Object.keys))];
  const out = {};
  for (const k of keys) {
    const xs = per.map(m => m[k]).filter(x => x != null);
    out[k] = { median: r1(median(xs)), min: r1(Math.min(...xs)), max: r1(Math.max(...xs)), n: xs.length };
  }
  return { meta: data.meta, metrics: out, data };
}

const now = summarize(file);
const before = vsFile ? summarize(vsFile) : null;
if (asJson) {
  console.log(JSON.stringify({ now: now.metrics, before: before?.metrics ?? null }, null, 1));
  process.exit(0);
}
const lines = [];
lines.push(`# ${now.meta.label} (${now.meta.at}, ${now.meta.host}, commit ${now.meta.commit}${now.meta.dirty ? '+' : ''}, ${now.meta.repeats} repeats)`);
lines.push('');
if (before) lines.push(`Compared with ${before.meta.label} (${before.meta.at}, commit ${before.meta.commit}).`, '');
lines.push(before ? '| metric | before | now | change | now min–max |' : '| metric | median | min–max |');
lines.push(before ? '|---|---:|---:|---:|---|' : '|---|---:|---|');
for (const [k, v] of Object.entries(now.metrics)) {
  if (before) {
    const b = before.metrics[k]?.median;
    const pct = b ? `${r1(((v.median - b) / b) * 100)} %` : '';
    lines.push(`| ${k} | ${b ?? ''} | ${v.median} | ${pct} | ${v.min}–${v.max} |`);
  } else lines.push(`| ${k} | ${v.median} | ${v.min}–${v.max} |`);
}
const prof = now.data.profiles;
if (prof) {
  lines.push('', '## CPU profile (self time, the busiest threads)', '');
  for (const p of prof.cpu.slice(0, 3)) {
    lines.push(`### ${p.file}: ${p.busyMs} ms busy`, '', '| function | ms |', '|---|---:|');
    for (const [f, ms] of p.topFunctionsMs.slice(0, 15)) lines.push(`| \`${f}\` | ${ms} |`);
    lines.push('', '| file | ms |', '|---|---:|');
    for (const [f, ms] of p.topFilesMs.slice(0, 10)) lines.push(`| \`${f}\` | ${ms} |`);
    lines.push('');
  }
  lines.push('## Heap profile (sampled allocations alive at stop)', '');
  for (const p of prof.heap.slice(0, 3)) {
    lines.push(`### ${p.file}: ${p.liveMb} MB`, '', '| allocation site ← caller | MB |', '|---|---:|');
    for (const [f, mb] of p.topSitesMb.slice(0, 15)) lines.push(`| \`${f}\` | ${mb} |`);
    lines.push('');
  }
}
console.log(lines.join('\n'));
