// match_notebook's answer kept short (a page's answer was 10–16k characters; a chat of some 45
// pages filled its context): counts, the year, only the lines that need a look (those saying the
// same as one entry), the wild-caught rows' template once, a short lookAt.
import test from 'node:test';
import assert from 'node:assert/strict';
import { COLLECTION_TEMPLATES, matchSummary } from '../server/notebook-tool.mjs';
import { briefLookAt } from '../server/look-at.mjs';

const counts = { lines: 0, fills: 0, conflicts: 0, doubts: 0, unreadable: 0, errors: 0, created: 0, same: 0 };
const line = (n, label, cells, more = {}) => ({ n, raw: `${label} ${'x'.repeat(100)}`, status: 'match', row: 100 + n, label, cells, ...more });
const differs = { SPECIES: { status: 'conflict', before: 'Mechanitis messenoides intermedia', value: 'Mechanitis messenoides deceptus', message: 'La fórmula da otra' } };
const fills = { Sex: { status: 'fill', value: 'male' }, LIFESTAGE: { status: 'fill', value: 'Adult', inferred: true }, Insectary_ID: { status: 'same' } };

test('match_notebook answers only the lines that need a look; lines saying the same as one entry', () => {
  const lines = [
    line(1, '1VB', { ...differs, ...fills }),
    line(2, '2VB', { ...differs, ...fills }),
    line(3, '3VB', { ...differs }),
    line(4, '4VB', fills),
    line(5, '5VB', { Insectary_ID: { status: 'same' } }),
    line(6, '6VB', { Sex: { status: 'error', message: 'no está en la lista' } }),
    line(7, '7ZZ', {}, { status: 'missing', row: null, message: '7ZZ no está', near: [{ value: '7VB', row: 107 }] }),
    line(8, '8VB', { ...fills, 'CLUTCH NUMBER': { status: 'formula', message: 'Columna con fórmula en la hoja: no se escribe' } }),
    line(9, '9VB', {}, { status: 'new', row: null, message: 'Fila nueva en Insectary_data' }),
  ];
  const changes = [1, 2, 3, 4, 6, 8, 9].map(n => ({ line: n }));
  const out = matchSummary({ review: { kind: 'emergence', sheet: 'Insectary_data', year: 2025, yearSource: 'page', counts: { ...counts, lines: 9 }, lines }, changes, ignored: [] }, 'p1');
  assert.deepEqual(
    out.lines.map(l => l.lines ?? l.n),
    [[1, 2, 3], 6, 7],
  );
  // The run of the same difference: once, with its lines and IDs.
  assert.deepEqual(out.lines[0], {
    lines: [1, 2, 3],
    ids: ['1VB', '2VB', '3VB'],
    status: 'match',
    differs: { SPECIES: { sheet: 'Mechanitis messenoides intermedia', notebook: 'Mechanitis messenoides deceptus', note: 'La fórmula da otra' } },
    inProposal: true,
  });
  // A listed line: its raw text cut, its fills and implied cells left to the proposal.
  const missing = out.lines[2];
  assert.equal(missing.raw.length, 60);
  assert.deepEqual(missing.didYouMean, [{ id: '7VB', row: 107 }]);
  assert.ok(!out.lines.some(l => 'fill' in l || 'implied' in l || 'same' in l));
  // A formula column that agrees, a new row: counted, not listed.
  assert.equal(out.counts.linesAsInSheet, 1);
  assert.equal(out.counts.linesOnlyFilled, 3);
  assert.match(out.rest, /get_proposal/);
  assert.equal(out.proposalId, 'p1');
  assert.deepEqual([out.year, out.yearSource], [2025, 'page']);
});

test("match_notebook: the wild-caught rows' template once, the rows near the page with its slip capped", () => {
  const template = COLLECTION_TEMPLATES.Collected_Sent2Insectary;
  const wild = ['1WC', '2WC'].map(id => ({ id, row: { sheet: 'Collection_data', values: { ...template, Insectary_ID: id, Sex: 'male' } } }));
  const sameError = Array.from({ length: 14 }, (_, i) => ({ row: 200 + i, label: `R${i}`, field: 'Tube_1_id', value: 'FS1', suggested: 'FS01', reason: { text: 'falta un 0' }, inProposal: true }));
  const out = matchSummary(
    { review: { kind: 'emergence', sheet: 'Insectary_data', year: 2025, yearSource: 'page', counts, lines: [] }, changes: [], ignored: [], wildWithoutCollection: wild, sameError },
    'p1',
  );
  assert.deepEqual(out.wildWithoutCollection.template, { sheet: 'Collection_data', values: template });
  assert.deepEqual(out.wildWithoutCollection.rows, [
    { Insectary_ID: '1WC', Sex: 'male' },
    { Insectary_ID: '2WC', Sex: 'male' },
  ]);
  assert.match(out.wildWithoutCollection.todo, /`template` plus its cells.*update_proposal newRows/);
  assert.equal(out.sameErrorNearby.length, 10);
  assert.equal(out.sameErrorMore, 4);
});

test('a short lookAt: issues of one kind on one column as one item, lists cut, the rest in get_proposal', () => {
  const photo = n => ({ kind: 'photo_missing', field: 'Photo_dorsal', problem: `CAM07800${n} preservada sin foto`, rows: [`${n}VC`] });
  const look = {
    issues: [photo(1), photo(2), photo(3), { kind: 'repeat', field: 'Tube_1_id', problem: 'FS1 también en fila 9', rows: ['2VD: FS1'] }],
    notes: Array.from({ length: 12 }, (_, i) => ({ field: 'NOTES', note: `nota ${i}`, rows: [`${i}`] })),
  };
  const brief = briefLookAt(look);
  assert.deepEqual(brief.issues, [
    { kind: 'photo_missing', field: 'Photo_dorsal', problem: 'CAM078001 preservada sin foto', rows: ['1VC', '2VC', '3VC'], problems: 3 },
    look.issues[3],
  ]);
  assert.equal(brief.notes.length, 10);
  assert.equal(brief.notesMore, 2);
  assert.match(brief.rest, /get_proposal/);
  // Little to say: as it was.
  const small = { notes: look.notes.slice(0, 2) };
  assert.deepEqual(briefLookAt(small), small);
});
