// Scores tags against the eye labels by the pass bar. node score.mjs <labels.json> <tags.json> [threshold]
import { readFileSync } from 'node:fs';
const eye = JSON.parse(readFileSync(process.argv[2], 'utf8'));
const tags = JSON.parse(readFileSync(process.argv[3], 'utf8'));
const th = Number(process.argv[4] ?? 0);
const lh = v => v === 'leaves' || v === 'habit';
// The model's leaves/habit: the summed probability over threshold (0: its top view is leaves or habit).
const isLH = t => (th ? t[2] >= th : lh(t[0]));
let tp = 0, fp = 0, fl = 0, flAsLH = 0, flRec = 0;
const rows = [];
for (const [k, v] of Object.entries(eye)) {
  const t = tags[k];
  if (!t) continue;
  if (isLH(t)) (lh(v) ? tp++ : fp++);
  if (v === 'flower') { fl++; if (isLH(t)) flAsLH++; if (t[0] === 'flower') flRec++; }
  rows.push(`${k}:${v}->${t[0]}(${t[2]})`);
}
console.log(`precision LH ${(100 * tp / (tp + fp)).toFixed(0)} % (${tp}/${tp + fp}); flowers as LH ${flAsLH}/${fl}; flower recall ${(100 * flRec / fl).toFixed(0)} %; LH recall ${tp}/${Object.values(eye).filter(lh).length}`);
console.log(rows.join('  '));
