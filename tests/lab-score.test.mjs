import test from 'node:test';
import assert from 'node:assert/strict';
import { noteIds, scoreCase } from '../tools/lab/lib.mjs';

// A case of three clutches as the bench reads it: truth rows and the proposals made for them.
const kase = { id: 'p', sheet: 'Insectary_stocks', notOnPage: { 958: ['NUMBER OF PUPA'] } };
const rows = [
  { row: 10, label: '957', values: { 'NUMBER OF EGGS': '=12+15', 'HATCHING DATE': 46200, NOTES: '29/9/26 FCH: U8A♀ + C8B♂' } },
  { row: 11, label: '958', values: { 'NUMBER OF EGGS': '=7', 'NUMBER OF PUPA': '=7+3+12', NOTES: null } },
  { row: 12, label: '959', values: { 'NUMBER OF EGGS': '=8+1', 'HATCHING DATE': 46210, NOTES: 'Some eggs with fungi' } },
];
const proposal = {
  changes: [
    // Right; the couple named by its IDs.
    { row: 10, sheet: 'Insectary_stocks', values: { 'NUMBER OF EGGS': '=12+15', 'HATCHING DATE': 46200, NOTES: '30/9/26 FCH: U8A♀ + C8B♂' } },
    // A doubtful count read right, a pupae count whose truth is not on the photo, a note where the sheet has none.
    {
      row: 11,
      sheet: 'Insectary_stocks',
      values: { 'NUMBER OF EGGS': '=7', 'NUMBER OF PUPA': '=7+3', NOTES: '30/9/26 FCH: 3 pupae dead' },
      doubts: { 'NUMBER OF EGGS': { confidence: 0.5, alternatives: ['=1'] } },
    },
    // A doubtful count read wrong (1 read as 7), and a wrong date with no flag; the note is missing.
    {
      row: 12,
      sheet: 'Insectary_stocks',
      values: { 'NUMBER OF EGGS': '=8+7', 'HATCHING DATE': 46211 },
      doubts: { 'NUMBER OF EGGS': { confidence: 0.4, alternatives: ['=8+1'] } },
    },
    { context: true, row: 13, sheet: 'Insectary_stocks', values: {} },
  ],
};

test('the bench scores proposals the old way and the new: cells not on the photo, doubtful cells, couple notes', () => {
  const { legacy, v2, errors, rowsProposed } = scoreCase(kase, rows, [proposal]);
  // Legacy, as every earlier run: a doubtful cell counts as left out (the tool used to leave it).
  assert.deepEqual(legacy, { correct: 2, total: 6, wrong: 2, missing: 2, filledCorrect: 2, filledTotal: 6, notesMatch: 0, notesTotal: 2 });
  // New: 958's pupae are not on the photo; doubtful cells count by value.
  assert.deepEqual(v2, {
    correct: 3,
    total: 5,
    wrong: 2,
    missing: 0,
    notOnPage: 1,
    flaggedRight: 1,
    flaggedWrong: 1,
    wrongUnflagged: 1,
    notesMatch: 1,
    notesTotal: 2,
    extraNotes: 1,
  });
  assert.deepEqual(
    errors.map(e => [e.row, e.field, e.kind]),
    [
      ['959', 'NUMBER OF EGGS', 'wrong (flagged)'],
      ['959', 'HATCHING DATE', 'wrong'],
    ],
  );
  assert.equal(rowsProposed, 3, 'context rows are no proposal');
});

test('the IDs of a note without words', () => {
  assert.deepEqual([...noteIds('U8A♀ + C8B♂')], ['U8A', 'C8B']);
  assert.deepEqual([...noteIds('F2 clutch parents 9HO + 0GG')], ['F2', '9HO', '0GG']);
  assert.deepEqual([...noteIds('Some eggs with fungi')], []);
});
