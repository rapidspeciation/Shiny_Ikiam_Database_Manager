#!/usr/bin/env node
// Early stop (sequential): exit 1 when a partial variant run is clearly worse than the baseline:
// cache-neutral cost ×1.10 or more (geometric mean over the paired tasks), or a check the baseline passed now fails.
import { readFileSync } from 'node:fs';
import { join } from 'node:path';
const [baseDir, varDir] = process.argv.slice(2);
const load = d => Object.fromEntries(readFileSync(join(d, 'results.jsonl'), 'utf8').trim().split('\n').map(l => JSON.parse(l)).map(r => [r.id, r]));
const b = load(baseDir), v = load(varDir);
const neutral = u => 0.1 * u.input + 5 * u.output;
let logs = [], lost = [];
for (const [id, r] of Object.entries(v)) {
  if (!b[id] || !r.usage || !b[id].usage) continue;
  logs.push(Math.log(neutral(r.usage) / neutral(b[id].usage)));
  b[id].checks?.forEach((c, i) => c.pass && r.checks?.[i] && !r.checks[i].pass && lost.push(`${id}#${i}`));
}
const ratio = Math.exp(logs.reduce((s, x) => s + x, 0) / logs.length);
console.log(`partial: ${logs.length} tasks, neutral ×${ratio.toFixed(3)}, checks lost: ${lost.join(', ') || 'none'}`);
process.exit(ratio >= 1.1 || lost.length ? 1 : 0);
