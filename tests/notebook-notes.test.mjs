// A page photographed again as it fills: its notes come back reworded (in English, by the reader),
// sometimes on the neighbouring line. Cases from the Posturas pages of 16 and 18 Oct 2026 (the
// initials of the notes changed).
import test from 'node:test';
import assert from 'node:assert/strict';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { buildReview, checkTranscription, noteCoverage, noteNews, proposalRows } from '../server/notebook.mjs';
import { matchSummary } from '../server/notebook-tool.mjs';
import { signIn } from './helpers/assistant.mjs';

const page = (kind, lines) => checkTranscription({ kind, lines: lines.map(l => ({ raw: l.raw ?? '', values: l.v, confidence: l.c })) }).transcription;
const lookupOf = (rows, clutches = {}) => ({
  find: ([k]) => rows.filter(r => String(r.values.Insectary_ID ?? r.values['CLUTCH NUMBER']).toUpperCase() === String(k).toUpperCase()).map(r => ({ formulas: {}, label: String(r.values.Insectary_ID ?? r.values['CLUTCH NUMBER']), ...r })),
  clutch: value => (clutches[String(value)] ? Number(value) : null),
  speciesOfClutch: value => clutches[String(value)] ?? null,
  list: () => undefined,
  holder: () => null,
  newRowFormulas: new Set(),
  typedOverFormula: new Set(['SPECIES']),
});

const NOTE_1003 = '29/9/26 AB: V0A♀ + F1B♂; eggs looks dry / all of them are died (24-9); female dead → clutch to stock';
const NOTE_1004 = '29/9/26 AB: Z5A♀ + E9B♂; some eggs dry and with fungus, clutch is dead; 28/9 17 larvae preserved';
const NOTE_994_6 = '29/9/26 AB: U7A♀ + C7B♂; 3 larvae preserved; +9 larvae 28/9/26';
const NOTE_1005 = '29/9/26 AB: some eggs dry and 3 eggs fungus';

test("a note the row's note already says in other words is there; a new date, count or part is not", () => {
  // Row 1003 read again on 18 Oct: the same note in English, a parent misread (V6A for V0A).
  const again = noteNews('Eggs look dry; all of them died (24/9); female dead, clutch to stock. V6A♀ + F1B♂', NOTE_1003);
  assert.equal(again.already, true);
  assert.equal(noteNews('Parents Z8C female + P4D male. Preserved alive (12/10)', '1/10/26 AB: Z8C♀ + P4D♂ | 13/10/26 CD: Preserved alive (12/10)').already, true);
  assert.equal(noteNews('One larva disappeared (10/10)', '7/10/26 CD: A7E♀ | 12/10/26 CD: One larva disappeared 10 Oct').already, true);
  assert.equal(noteNews('lost 1 larva when trying to preserve', '16/10/26 CD: Lost a larva while trying to preserve it (11/10).').already, true);
  assert.equal(noteNews('HOB♀ + H4B♂', '16/10/26 CD: H0B♀ + H4B♂').already, true, 'O written for 0 in a parent');
  assert.equal(noteNews('1 dry egg; 2 larvae dead during pupating; 1 pupa looks dead', '29/9/26 AB: 1 dry egg | 12/10/26 CD: 2 larvae dead during pupation. 1 pupa looks dead').already, true);

  // New information stays: a death date, another count, a new word, a part said once more.
  assert.equal(noteNews('Died 3/10', '29/9/26 AB: 1 larva dead').already, false);
  const count = noteNews('5 pupae dead', '29/9/26 AB: 2 larvae died → preserved | 7/10/26 CD: 7 pupae dead');
  assert.deepEqual([count.already, count.near], [false, ['5']], 'all its words there, the count not: likely misread, still added');
  assert.deepEqual(noteNews('W2B♀ + F1B♂ | 1 larva dissected, preserved (14/10)', '7/10/26 CD: W2B♀ + F1B♂ | 16/10/26 CD: 9 larvae preserved (14/10).').fresh, ['1 larva dissected, preserved (14/10)']);
  assert.deepEqual(noteNews('W2B♀ + F1B♂; egg looks dry -> preserved', '12/10/26 CD: W2B♀ + F1B♂').fresh, ['egg looks dry', 'preserved']);
  assert.deepEqual(noteNews('1 larva disappeared; 1 larva disappeared', '12/10/26 CD: 1 larva disappeared').fresh, ['1 larva disappeared'], 'twice on the page, once in the row');
  assert.equal(noteNews('anything', ''), null);
  assert.ok(noteCoverage('Z5A♀ + E9B♂. Some eggs dry and with fungus; clutch dead?', NOTE_1004) > 0.9);
  assert.ok(noteCoverage('Z5A♀ + E9B♂. Some eggs dry and with fungus; clutch dead?', NOTE_994_6) < 0.3);
});

/** The first lines of the 16 Oct page as the reader gave them: from line 2 on, each note is the row above's. */
const SHIFTED_PAGE = [
  { raw: '1003 lysimnia 10/9 10', v: { 'CLUTCH NUMBER': '1003', NOTES: 'Eggs look dry; all of them died (24/9). Female dead, clutch to stock.' } },
  { raw: '1004 polymnia 15/9 18+12', v: { 'CLUTCH NUMBER': '1004', 'NUMBER OF EGGS': '18+12', NOTES: 'V6A♀ + F1B♂. 28/9: 17 larvae preserved.' } },
  { raw: '994(6) lysimnia 14/9 6+8', v: { 'CLUTCH NUMBER': '994(6)', NOTES: 'Z5A♀ + E9B♂. Some eggs dry and with fungus; clutch dead?' } },
  { raw: '1005 lysimnia 16/9 4+3', v: { 'CLUTCH NUMBER': '1005', NOTES: 'U7A♀ + C7B♂. 3 larvae preserved + 9 larvae (28/9/26). Some eggs dry and 3 eggs with fungus.' } },
  // Two batches of the same parents: the same note on both lines is no shift.
  { raw: '1012(2) lysimnia 25/9 17', v: { 'CLUTCH NUMBER': '1012(2)', NOTES: 'W2B♀ + F1B♂' } },
  { raw: '1012(3) lysimnia 28/9 11', v: { 'CLUTCH NUMBER': '1012(3)', NOTES: 'W2B♀ + F1B♂; egg looks dry -> preserved' } },
];
const SHIFTED_ROWS = [
  { id: 's1', row: 1050, version: 1, values: { 'CLUTCH NUMBER': 1003, NOTES: NOTE_1003 } },
  { id: 's2', row: 1051, version: 1, values: { 'CLUTCH NUMBER': 1004, NOTES: NOTE_1004 } },
  { id: 's3', row: 1052, version: 1, values: { 'CLUTCH NUMBER': '994(6)', NOTES: NOTE_994_6 } },
  { id: 's4', row: 1053, version: 1, values: { 'CLUTCH NUMBER': 1005, NOTES: NOTE_1005 } },
  { id: 's5', row: 1054, version: 1, values: { 'CLUTCH NUMBER': '1012(2)', NOTES: '29/9/26 AB: W2B♀ + F1B♂' } },
  { id: 's6', row: 1055, version: 1, values: { 'CLUTCH NUMBER': '1012(3)', NOTES: '29/9/26 AB: W2B♀ + F1B♂' } },
];

test("a page's notes read one line off are left out and said; a note already there is not added again", () => {
  const review = buildReview({ transcription: page('stocks', SHIFTED_PAGE), year: 2026, today: '2026-10-16', initials: 'CD', lookup: lookupOf(SHIFTED_ROWS) });
  const [n1003, n1004, n994, n1005, n1012b, n1012c] = review.lines;
  assert.equal(n1003.cells.NOTES.status, 'same');
  assert.ok(n1003.cells.NOTES.alreadyThere);
  // 1004 got 1003's parents, 994(6) 1004's note, 1005 994(6)'s.
  assert.deepEqual([n1004, n994, n1005].map(l => [l.cells.NOTES.status, l.cells.NOTES.like.label]), [
    ['shifted', '1003'],
    ['shifted', '1004'],
    ['shifted', '994(6)'],
  ]);
  assert.ok(!n1004.cells.NOTES.include);
  assert.match(n994.warnings[0], /^NOTES not added: it reads like the note of 1004 \(row 1051, line 2: «29\/9\/26 AB: Z5A♀/);
  assert.equal(n1012b.cells.NOTES.status, 'same');
  // What the row already says is left out of what is added.
  assert.equal(n1012c.cells.NOTES.status, 'fill');
  assert.equal(n1012c.cells.NOTES.write, '29/9/26 AB: W2B♀ + F1B♂ | 16/10/26 CD: egg looks dry; preserved');
  assert.equal(n1012c.cells.NOTES.message, 'Lo demás ya está en la nota de la fila: «W2B♀ + F1B♂»');
  assert.deepEqual([review.counts.notesAlreadyThere, review.counts.notesShifted], [1, 3]);

  // In the proposal: 1004 writes its eggs, not the shifted note, and its row says why.
  const { changes } = proposalRows(review);
  const row1004 = changes.find(c => c.recordId === 's2');
  assert.deepEqual(Object.keys(row1004.values), ['NUMBER OF EGGS']);
  assert.match(row1004.note, /NOTES not added: it reads like the note of 1003/);
  assert.ok(!changes.some(c => c.recordId === 's1'), 'nothing new for 1003');
  const summary = matchSummary({ review, changes: [], ignored: [] }, null);
  assert.deepEqual([summary.counts.notesAlreadyThere, summary.counts.notesShifted], [1, 3]);
});

test('a dash for the stock of a Mechanitis messenoides clutch is no difference: the row keeps its stock', () => {
  const rows = [
    { id: 'h0', row: 13852, version: 1, values: { Insectary_ID: 'H0F', 'CLUTCH NUMBER': 1007, Stock_of_origin: 'deceptus' } },
    { id: 'h1', row: 13853, version: 1, values: { Insectary_ID: 'H1F', 'CLUTCH NUMBER': 1007, Stock_of_origin: 'intermedia' } },
    { id: 'h2', row: 13854, version: 1, values: { Insectary_ID: 'H2F', 'CLUTCH NUMBER': 1011, Stock_of_origin: 'deceptus' } },
  ];
  const review = buildReview({
    transcription: page('emergence', [
      { raw: 'H0F', v: { Insectary_ID: 'H0F', Stock_of_origin: 'NA' } },
      { raw: 'H1F 1007 -', v: { Insectary_ID: 'H1F', 'CLUTCH NUMBER': '1007', Stock_of_origin: '-' } },
      { raw: 'H2F 1011 -', v: { Insectary_ID: 'H2F', 'CLUTCH NUMBER': '1011', Stock_of_origin: '-' } },
    ]),
    year: 2026,
    today: '2026-10-13',
    lookup: lookupOf(rows, { 1007: 'Mechanitis messenoides deceptus', 1011: 'Mechanitis lysimnia' }),
  });
  const [h0, h1, h2] = review.lines.map(l => l.cells.Stock_of_origin);
  assert.equal(h0.status, 'same', 'the clutch is deceptus: the stock the row has');
  assert.equal(h1.status, 'keep', 'another stock in the row stays, with no difference');
  assert.ok(!h0.include && !h1.include);
  // Not a messenoides clutch: the dash is NA, unlike the row (as before).
  assert.equal(h2.status, 'conflict');
});

const delay = ms => new Promise(done => setTimeout(done, ms));

test("overlapping pages say how much of them the sheet has; one a later applied copy of the page covers is closed", async () => {
  const sheets = new LocalSheets({
    Insectary_stocks: [
      { row: 2, values: { 'CLUTCH NUMBER': 1003, SPECIES: 'Mechanitis lysimnia', NOTES: NOTE_1003 } },
      { row: 3, values: { 'CLUTCH NUMBER': 1004, SPECIES: 'Mechanitis lysimnia', NOTES: NOTE_1004 } },
    ],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_stocks'] });
  const assistant = createAssistant({ store, config: {} });
  const { call, http } = signIn(store, assistant, { id: 'u-ana', displayName: 'Ana Example', role: 'admin' });
  try {
    const PAGE = {
      kind: 'stocks',
      year: 2026,
      lines: [
        { raw: '1003 10/9 10', values: { 'CLUTCH NUMBER': '1003', 'DATE LAID': '10/9', 'NUMBER OF EGGS': '10' } },
        { raw: '1004 15/9 18+12', values: { 'CLUTCH NUMBER': '1004', 'DATE LAID': '15/9', 'NUMBER OF EGGS': '18+12' } },
      ],
    };
    // The page read in two chats (two pending copies), then the person edits one of them in the table.
    const first = await call('match_notebook', PAGE);
    const edited = await call('match_notebook', PAGE);
    const listed = (await http('GET', '/api/chat/proposals', {})).body.proposals;
    const row = listed.find(p => p.id === edited.proposalId).changes[0];
    await http('POST', `/api/chat/proposals/${edited.proposalId}/edit`, { body: { cells: [{ key: row.key, field: 'NUMBER OF EGGS', value: '=11' }] } });
    await delay(5);
    // A third reading, applied.
    const third = await call('match_notebook', PAGE);
    assert.deepEqual(
      third.overlaps.map(o => [o.proposalId, o.alreadyInSheet, o.cells]).sort(),
      [
        [first.proposalId, 0, 4],
        [edited.proposalId, 0, 4],
      ].sort(),
    );
    assert.equal((await call('apply_proposal', { proposalId: third.proposalId })).status, 'applied');
    await delay(5);
    // The page read once more: the copies the applied one covered are superseded; the untouched one is closed.
    const fourth = await call('match_notebook', { ...PAGE, lines: [...PAGE.lines.slice(0, 1), { ...PAGE.lines[1], values: { ...PAGE.lines[1].values, 'NUMBER OF LARVAE': '4' } }] });
    const of = id => fourth.overlaps.find(o => o.proposalId === id);
    assert.equal(of(first.proposalId).alreadyInSheet, 4, 'the applied copy wrote the same cells');
    assert.deepEqual([of(first.proposalId).superseded.by, of(first.proposalId).superseded.closed], [third.proposalId, true]);
    assert.deepEqual([of(edited.proposalId).superseded.closed, of(edited.proposalId).superseded.why], [false, 'the person worked on it in the table']);
    const status = id => store.db.prepare('SELECT status FROM ai_proposals WHERE id = ?').get(id).status;
    assert.deepEqual([status(first.proposalId), status(edited.proposalId), status(fourth.proposalId)], ['discarded', 'pending', 'pending']);
  } finally {
    store.close();
  }
});
