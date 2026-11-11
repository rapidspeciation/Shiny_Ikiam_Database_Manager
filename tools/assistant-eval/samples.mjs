// Samples and the pass rule of the assistant eval (run.mjs, compare.mjs, rescore.mjs, stop-check.mjs).
//
// A case (task) runs `--samples n` times (default 2); each run is one row of results.jsonl with `sample` (1-based).
// - A sample passes when its quality score is at least the task's `passAt` (default 1.0: every check passed).
//   The row keeps the score too, so the mean quality per case stays comparable with older runs.
// - When n is even and exactly half the samples pass (with n = 2: one passes, one fails), one more sample runs.
// - A case fails when half or more of its samples fail.
// Rows from folders made before samples existed have no `sample` or `passAt`: each row counts as one sample
// with passAt 1.0. Rows marked `invalid` (the eval app went down, a graded call cut) or `skipped` are not samples;
// an invalid sample gets one more try (needsRetry). A case with rows but no counted sample is listed as such
// (passRate's `uncounted`), never dropped silently.
import { existsSync, readFileSync } from 'node:fs';
import { join } from 'node:path';

export const DEFAULT_PASS_AT = 1;

/** Whether a row counts as a sample at all. */
export const counted = r => r && !r.invalid && !r.skipped;

/** The per-sample pass rule: score ≥ passAt (a crashed sample has score 0 and fails). */
export const samplePass = (r, passAt = r?.passAt ?? DEFAULT_PASS_AT) => (r?.score ?? 0) >= passAt - 1e-9;

/** The rows of a run folder (empty when it has none). */
export function loadRows(dir) {
  const f = join(dir, 'results.jsonl');
  if (!existsSync(f)) return [];
  return readFileSync(f, 'utf8')
    .split('\n')
    .filter(l => l.trim())
    .map(l => JSON.parse(l));
}

/** One case's verdict from its sample rows: { n, passes, fails, pass, label: "2/3", meanScore }. */
export function verdict(rows) {
  const s = rows.filter(counted);
  const passes = s.filter(r => samplePass(r)).length;
  const n = s.length;
  return { n, passes, fails: n - passes, pass: n > 0 && (n - passes) * 2 < n, label: `${passes}/${n}`, meanScore: n ? s.reduce((t, r) => t + (r.score ?? 0), 0) / n : null };
}

/** Rows grouped by case id, in first-seen order: Map id → rows. */
export function byCase(rows) {
  const m = new Map();
  for (const r of rows) {
    if (!r?.id) continue;
    if (!m.has(r.id)) m.set(r.id, []);
    m.get(r.id).push(r);
  }
  return m;
}

/** Whether a case that ran `planned` samples needs one more: an exact tie (half pass), once. */
export function needsTieBreak(rows, planned) {
  const v = verdict(rows);
  return v.n === planned && planned % 2 === 0 && v.passes * 2 === v.n;
}

/** Whether a sample row is worth one more try: it ran but does not count (invalid). Skipped rows (the run was
 * aborted) and crashes (score 0, a sample) are not retried. */
export const needsRetry = r => Boolean(r) && !r.skipped && !counted(r);

/** Whether t3-turn's `stop` answer confirms the chat is idle (a busy chat could still draft into later samples). */
export const idleAfterStop = st => st?.ok === true && !st.busy;

/** Pass rate of a set of runs: cases passed / cases and samples passed / samples; `uncounted`: the ids of cases
 * that have rows but no counted sample (all invalid or skipped). */
export function passRate(rows) {
  const all = [...byCase(rows).entries()].map(([id, rs]) => ({ id, ...verdict(rs) }));
  const cases = all.filter(v => v.n);
  return {
    cases: cases.length,
    casesPassed: cases.filter(v => v.pass).length,
    samples: cases.reduce((t, v) => t + v.n, 0),
    samplesPassed: cases.reduce((t, v) => t + v.passes, 0),
    uncounted: all.filter(v => !v.n).map(v => v.id),
  };
}

export const pct = (a, b) => (b ? `${Math.round((100 * a) / b)} %` : '-');
export const rateText = r =>
  `${r.casesPassed}/${r.cases} cases (${pct(r.casesPassed, r.cases)}), ${r.samplesPassed}/${r.samples} samples (${pct(r.samplesPassed, r.samples)})` +
  (r.uncounted?.length ? `; no counted sample: ${r.uncounted.join(', ')}` : '');
