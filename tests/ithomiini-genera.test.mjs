import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { ITHOMIINI_GENERA } from '../frontend/src/lib/ithomiiniGenera.ts';
import { ithomiiniSpecies } from '../server/identify-photos.mjs';
import { KEYTERMS } from '../tools/whatsapp/bridge-lib.mjs';

// The app's one list of Ithomiini genera (frontend/src/lib/ithomiiniGenera.ts; its content is checked against the
// 46-genus reference in frontend/src/lib/__tests__/ithomiiniGenera.test.ts) as the server and the scripts read it.

test('«Fotos para el campo» takes the head species of every Ithomiini genus, not Macrosoma', () => {
  const head = ['Athyrtis mechanitis', 'Leucothyris victoria', 'Elzunia humboldt', 'Pteronymia zerlina', 'Macrosoma heliconiaria', 'Heliconius erato'];
  assert.deepEqual(ithomiiniSpecies(head, ITHOMIINI_GENERA), ['Athyrtis mechanitis', 'Elzunia humboldt', 'Leucothyris victoria', 'Pteronymia zerlina']);
});

test('scripts/field-photos.py reads the same list from the TypeScript file', () => {
  // The script's own pattern (ithomiini_genera()), in JavaScript.
  const src = readFileSync(new URL('../frontend/src/lib/ithomiiniGenera.ts', import.meta.url), 'utf8');
  const body = /ITHOMIINI_GENERA[^=]*=\s*Object\.freeze\(\[([^\]]*)\]/.exec(src)[1];
  assert.deepEqual([...body.matchAll(/'([A-Za-z]+)'/g)].map(m => m[1]), [...ITHOMIINI_GENERA]);
  const py = readFileSync(new URL('../scripts/field-photos.py', import.meta.url), 'utf8');
  assert.match(py, /GENERA = ithomiini_genera\(\)/);
  assert.doesNotMatch(py, /"Aeria Athesis/, 'no second copy of the list in the script');
});

test('the voice keyterms name no genus outside the list (Heliconius aside)', () => {
  const genera = KEYTERMS.filter(k => /^[A-Z][a-z]+$/.test(k) && !['Ithomiini', 'Heliconius', 'Ikiam', 'Claude'].includes(k));
  assert.deepEqual(genera.filter(g => !ITHOMIINI_GENERA.includes(g)), []);
});
