// Summaries of the profiles Node writes with --cpu-prof (.cpuprofile, one per thread) and --heap-prof
// (.heapprofile: sampled allocations still alive when the app stopped): the functions and files that took the
// most CPU (self time), and the allocation sites that hold the most memory.
import { readFileSync, readdirSync } from 'node:fs';
import { join } from 'node:path';

const short = url => String(url || '').replace(/^file:\/\/.*?\/(server|tools|node_modules)\//, '$1/').replace(/^node:/, 'node:') || '(native)';
const top = (map, n) =>
  [...map.entries()]
    .sort((a, b) => b[1] - a[1])
    .slice(0, n)
    .map(([k, v]) => [k, Math.round(v * 10) / 10]);

export function summarizeCpuProfiles(dir, n = 25) {
  const out = [];
  for (const file of readdirSync(dir).filter(f => f.endsWith('.cpuprofile'))) {
    const p = JSON.parse(readFileSync(join(dir, file), 'utf8'));
    const byId = new Map(p.nodes.map(node => [node.id, node]));
    const self = new Map();
    const dt = p.timeDeltas;
    for (let i = 0; i < p.samples.length; i++) self.set(p.samples[i], (self.get(p.samples[i]) ?? 0) + (dt[i] ?? 0));
    const fn = new Map();
    const files = new Map();
    let total = 0;
    for (const [id, us] of self) {
      const cf = byId.get(id).callFrame;
      const name = `${cf.functionName || '(anonymous)'} ${short(cf.url)}:${cf.lineNumber + 1}`;
      if (cf.functionName === '(idle)' || cf.functionName === '(program)') continue;
      total += us;
      fn.set(name, (fn.get(name) ?? 0) + us / 1000);
      files.set(short(cf.url), (files.get(short(cf.url)) ?? 0) + us / 1000);
    }
    out.push({ file, busyMs: Math.round(total / 1000), topFunctionsMs: top(fn, n), topFilesMs: top(files, 15) });
  }
  return out.sort((a, b) => b.busyMs - a.busyMs);
}

export function summarizeHeapProfiles(dir, n = 25) {
  const out = [];
  for (const file of readdirSync(dir).filter(f => f.endsWith('.heapprofile'))) {
    const p = JSON.parse(readFileSync(join(dir, file), 'utf8'));
    const sites = new Map();
    const files = new Map();
    let total = 0;
    const walk = (node, stack) => {
      const cf = node.callFrame;
      const here = `${cf.functionName || '(anonymous)'} ${short(cf.url)}:${cf.lineNumber + 1}`;
      const self = node.selfSize / 2 ** 20;
      if (self) {
        total += self;
        // The allocation site with its caller, which usually tells what is being kept.
        const key = stack.length ? `${here} ← ${stack.at(-1)}` : here;
        sites.set(key, (sites.get(key) ?? 0) + self);
        files.set(short(cf.url), (files.get(short(cf.url)) ?? 0) + self);
      }
      for (const c of node.children ?? []) walk(c, [...stack.slice(-3), here]);
    };
    walk(p.head, []);
    out.push({ file, liveMb: Math.round(total), topSitesMb: top(sites, n), topFilesMb: top(files, 15) });
  }
  return out.sort((a, b) => b.liveMb - a.liveMb);
}
