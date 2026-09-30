#!/usr/bin/env node
// The lab app's seed: the snapshot with every scored cell of the cases emptied, so
// a model transcribing a photo cannot find the answers in the sheet and its
// proposal holds everything it read. The ID column (clutch, Insectary_ID) stays.
//   node tools/lab/seed.mjs   → $LAB/seed.json and $LAB/blanked.json (both 600)
import { caseFields, groundTruth, labPath, loadCases, loadSnapshot, writePrivate } from './lib.mjs';

const snapshot = loadSnapshot();
const cases = loadCases();
const blanked = {};
for (const kase of cases) {
  const rows = groundTruth(snapshot, kase);
  blanked[kase.id] = rows.map(r => ({ row: r.row, label: r.label, fields: Object.keys(r.values) }));
  for (const r of rows) {
    const target = snapshot[kase.sheet].find(x => x.row === r.row);
    for (const column of Object.values(r.columns)) if (target.cells[column]) target.cells[column] = {};
  }
  const cells = rows.reduce((n, r) => n + Object.keys(r.values).length, 0);
  console.log(`${kase.id}: ${rows.length} rows, ${cells} cells emptied (${caseFields(kase).length} fields)`);
}
writePrivate(labPath('seed.json'), JSON.stringify({ sheets: snapshot }));
writePrivate(labPath('blanked.json'), JSON.stringify(blanked, null, 1));
console.log(`Seed written: ${labPath('seed.json')}`);
