// The tools' tags file from the parts tagged so far: { model, prompts, threshold, made, photos: { inatPhotoId: [view, pLeavesOrHabit, pFlower] } }.
//   node merge.mjs <T> <out.json>
import { readFileSync, readdirSync, writeFileSync, renameSync } from 'node:fs';
const [T, out] = process.argv.slice(2);
const photos = {};
const files = ['tags-pilot.json', ...readdirSync(`${T}/data`).filter(f => /^part-\d+\.tags\.json$/.test(f))];
for (const f of files) {
  const tags = JSON.parse(readFileSync(`${T}/data/${f}`, 'utf8'));
  for (const [k, [view, , pLH, pFl]] of Object.entries(tags)) photos[k] = [view, pLH, pFl];
}
const json = { model: 'open_clip ViT-L-14 laion2b_s32b_b82k, zero-shot, prompts v2', threshold_leaves_or_habit: 0.8, calibration: 'PASS-BAR.md (validation 17/17, 0 of 14 flowers as leaves)', made: new Date().toISOString(), parts: files.length, photos };
writeFileSync(`${out}.tmp`, JSON.stringify(json));
renameSync(`${out}.tmp`, out);
const c = {};
for (const [v] of Object.values(photos)) c[v] = (c[v] ?? 0) + 1;
console.log(Object.keys(photos).length, 'photos', JSON.stringify(c), (JSON.stringify(json).length / 1e6).toFixed(1), 'MB');
