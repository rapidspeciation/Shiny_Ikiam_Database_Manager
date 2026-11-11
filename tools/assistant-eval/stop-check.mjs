#!/usr/bin/env node
// Early stop (sequential): exit 1 when a partial variant run is clearly worse than the baseline:
// cache-neutral cost ×1.10 or more (geometric mean over the paired tasks), or a check the baseline passed now fails.
// With samples (samples.mjs), costs are per-case means, and a check counts as passed in a run when it passed in
// more than half of that case's samples; folders without samples read as one sample per task.
// So against a single-sample baseline, a check the variant passes in exactly half its samples (1 of 2) counts as lost.
import { byCase, counted, loadRows } from './samples.mjs';
const [baseDir, varDir] = process.argv.slice(2);
const load = d => byCase(loadRows(d).filter(counted));
const b = load(baseDir), v = load(varDir);
const neutral = u => 0.1 * u.input + 5 * u.output;
const meanNeutral = rs => {
  const u = rs.filter(r => r.usage && !r.usage.unavailable);
  return u.length ? u.reduce((s, r) => s + neutral(r.usage), 0) / u.length : null;
};
/** Check i passes in more than half of the samples. */
const held = (rs, i) => rs.filter(r => r.checks?.[i]?.pass).length * 2 > rs.length;
let logs = [], lost = [];
for (const [id, rs] of v) {
  const bs = b.get(id);
  if (!bs) continue;
  const n0 = meanNeutral(bs), n1 = meanNeutral(rs);
  if (n0 && n1) logs.push(Math.log(n1 / n0));
  bs.find(r => r.checks)?.checks.forEach((c, i) => held(bs, i) && rs.some(r => r.checks) && !held(rs, i) && lost.push(`${id}#${i}`));
}
const ratio = Math.exp(logs.reduce((s, x) => s + x, 0) / logs.length);
console.log(`partial: ${logs.length} tasks, neutral ×${ratio.toFixed(3)}, checks lost: ${lost.join(', ') || 'none'}`);
process.exit(ratio >= 1.1 || lost.length ? 1 : 0);
