import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { parseDateText } from '../server/schema.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { createNotebookMatcher, matchSummary } from '../server/notebook-tool.mjs';
import {
  buildReview,
  checkTranscription,
  clutchRuns,
  digitSlip,
  impliedValues,
  lifeStage,
  lookAlikeValues,
  proposalRows,
  rawDoubts,
  speciesFits,
} from '../server/notebook.mjs';

const d = text => parseDateText(text);
const page = (kind, lines, extra = {}) =>
  checkTranscription({
    kind,
    lines: lines.map(l => ({ raw: l.raw ?? '', values: l.v, confidence: l.c, alternatives: l.a, reasons: l.r })),
    ...extra,
  }).transcription;
/** A sheet of rows by their key, with the lookups match_notebook gives buildReview. */
function lookupOf(rows, extra = {}) {
  const key = r => String(r.values.Insectary_ID ?? r.values['CLUTCH NUMBER']).toUpperCase();
  return {
    find: ([k]) => rows.filter(r => key(r) === String(k).toUpperCase()).map(r => ({ formulas: {}, label: key(r), ...r })),
    clutch: value => (extra.clutches?.[String(value)] ? (/^\d+$/.test(String(value)) ? Number(value) : String(value)) : null),
    laidOfClutch: value => extra.clutches?.[String(value)]?.laid,
    speciesOfClutch: value => extra.clutches?.[String(value)]?.species ?? null,
    list: () => undefined,
    holder: () => null,
    newRowFormulas: new Set(),
    typedOverFormula: new Set(['SPECIES']),
    ...extra.lookup,
  };
}
const blank = (id, row, values = {}) => ({ id: `r${row}`, row, version: 1, values: { Insectary_ID: id, ...values } });

test('look-alike digits: a digit this hand writes like another, or a 1 more or less', () => {
  assert.deepEqual(digitSlip('15', '17'), null, '5 and 7 are not alike');
  assert.deepEqual(digitSlip('11', '17'), ['1', '7']);
  assert.deepEqual(digitSlip('11', '14'), ['1', '4']);
  assert.deepEqual(digitSlip('15', '5'), ['1', '']);
  assert.deepEqual(digitSlip('42', '47'), ['2', '7']);
  // Sums term by term, dates by day or month, clutch numbers.
  assert.deepEqual(lookAlikeValues('NUMBER OF EGGS', '=15+1', '=15+7'), ['1', '7']);
  assert.deepEqual(lookAlikeValues('NUMBER OF LARVAE', '=17+21', '=11+21'), ['7', '1']);
  assert.deepEqual(lookAlikeValues('NUMBER OF EGGS', '=15+1', '=15'), ['+1', '']);
  assert.equal(lookAlikeValues('NUMBER OF EGGS', '=15+1', '=16+2'), null, 'two terms differ');
  assert.deepEqual(lookAlikeValues('HATCHING DATE', d('2026-09-13'), d('2026-09-17')), ['3', '7']);
  assert.deepEqual(lookAlikeValues('Death_date', d('2026-07-19'), d('2026-01-19')), ['7', '1']);
  assert.equal(lookAlikeValues('Death_date', d('2026-09-13'), d('2026-08-17')), null);
  assert.deepEqual(lookAlikeValues('CLUTCH NUMBER', '987', '997'), ['8', '9']);
  assert.equal(lookAlikeValues('Sex', 'male', 'female'), null);
});

test("a reading one look-alike digit off the sheet's value is a doubt with the sheet's value to pick", () => {
  const rows = [
    { id: 's1', row: 2, version: 1, values: { 'CLUTCH NUMBER': 1013, 'NUMBER OF EGGS': 16, 'HATCHING DATE': d('2026-09-17') }, formulas: { 'NUMBER OF EGGS': '=15+1' } },
    { id: 's2', row: 3, version: 1, values: { 'CLUTCH NUMBER': 947, 'NUMBER OF LARVAE': 104 }, formulas: { 'NUMBER OF LARVAE': '=5+15+25+4+48+7' } },
    { id: 's3', row: 4, version: 1, values: { 'CLUTCH NUMBER': 964, 'NUMBER OF EGGS': 10, 'NUMBER OF LARVAE': 8 }, formulas: { 'NUMBER OF EGGS': '=10', 'NUMBER OF LARVAE': '=8' } },
  ];
  const lookup = lookupOf(rows, { lookup: { lastWrite: (id, field) => (id === 's1' && field === 'NUMBER OF EGGS' ? { notebook: true, date: '29/9/26' } : null) } });
  const review = buildReview({
    transcription: page('stocks', [
      { raw: '1013 15+7 13/9', v: { 'CLUTCH NUMBER': '1013', 'NUMBER OF EGGS': '15+7', 'HATCHING DATE': '13/9' } },
      { raw: '947 8+15+25+4+48+7+3', v: { 'CLUTCH NUMBER': '947', 'NUMBER OF LARVAE': '8+15+25+4+48+7+3' } },
      { raw: '964 10 12', v: { 'CLUTCH NUMBER': '964', 'NUMBER OF EGGS': '10', 'NUMBER OF LARVAE': '12' } },
    ]),
    year: 2026,
    today: '2026-10-02',
    lookup,
  });
  const [a, b, c] = review.lines;
  const eggs = a.cells['NUMBER OF EGGS'];
  assert.equal(eggs.status, 'conflict');
  assert.ok(eggs.doubt && eggs.include);
  assert.equal(eggs.value, '=15+7', 'the reading stays the value');
  assert.deepEqual(eggs.alternatives, ['=15+1']);
  assert.equal(eggs.reason, 'La hoja tiene =15+1: 1 y 7 se parecen en esta letra (pasado de una foto del cuaderno el 29/9/26)');
  assert.equal(eggs.reasonMsg.key, '{why} (pasado de una foto del cuaderno el {date})');
  const hatch = a.cells['HATCHING DATE'];
  assert.ok(hatch.doubt);
  assert.deepEqual(hatch.alternatives, [d('2026-09-17')]);
  assert.match(hatch.reason, /^La hoja tiene 17-Sep-26: 7 y 3 se parecen/);
  // A page that rewrites a term the sheet's sum already has (new terms go at the end).
  const larvae = b.cells['NUMBER OF LARVAE'];
  assert.ok(larvae.doubt);
  assert.match(larvae.reason, /cambia el término 1 de la suma de la hoja/);
  assert.deepEqual(larvae.alternatives, ['=5+15+25+4+48+7+3'], "the sheet's terms, then the page's new one");
  // More larvae than eggs: said, not a doubt.
  assert.ok(!c.cells['NUMBER OF LARVAE'].doubt);
  assert.deepEqual(c.warnings, ['NUMBER OF LARVAE (12) is more than NUMBER OF EGGS (10): check both readings']);
  const { changes } = proposalRows(review);
  assert.deepEqual(changes[0].doubts['NUMBER OF EGGS'].alternatives, ['=15+1']);
});

test('a clutch read unlike the run next to it stays as read when the line writes its species', () => {
  const texts = [
    ...Array(4).fill({ 'CLUTCH NUMBER': '987', SPECIES: 'polymnia', Intro2Insectary_date: '19/9' }),
    ...Array(2).fill({ 'CLUTCH NUMBER': '997', SPECIES: 'salapia', Intro2Insectary_date: '19/9' }),
  ].map(t => ({ ...t }));
  const species = { 987: 'Mechanitis polymnia proceriformis', 997: 'Ithomia salapia salapia' };
  // 997 "has no plausible laid date" by the dates, but the lines say salapia: 997 it is.
  assert.deepEqual(clutchRuns(texts, [], c => c === '987', c => species[c]), {});
  // Without species written, as before: taken from the run, doubtful.
  const bare = texts.map(({ SPECIES, ...t }) => t);
  assert.equal(clutchRuns(bare, [], c => c === '987', c => species[c])[4].value, '987');
  // Species of the other clutch: taken from the run, saying why.
  const other = texts.map((t, i) => (i >= 4 ? { ...t, SPECIES: 'pol. p.' } : t));
  const run = clutchRuns(other, [], () => null, c => species[c]);
  assert.equal(run[4].value, '987');
  assert.match(run[4].reason, /la especie escrita es la del 987/);
  assert.ok(speciesFits('pol. p.', 'Mechanitis polymnia proceriformis'));
  assert.ok(speciesFits('Salapia', 'Ithomia salapia salapia'));
  assert.ok(!speciesFits('salapia', 'Mechanitis polymnia proceriformis'));
});

test("Stock_of_origin is the clutch's subspecies: another one on the page goes in doubtful", () => {
  const rows = [blank('X5B', 10), blank('X6B', 11), blank('X7B', 12)];
  const review = buildReview({
    transcription: page('emergence', [
      { v: { Insectary_ID: 'X5B', 'CLUTCH NUMBER': '975', Stock_of_origin: 'messen.' } },
      { v: { Insectary_ID: 'X6B', 'CLUTCH NUMBER': '975', Stock_of_origin: 'deceptus' } },
      { v: { Insectary_ID: 'X7B', 'CLUTCH NUMBER': '980', Stock_of_origin: 'NA' } },
    ]),
    year: 2026,
    today: '2026-10-02',
    lookup: lookupOf(rows, {
      clutches: { 975: { species: 'Mechanitis messenoides deceptus' }, 980: { species: 'Mechanitis lysimnia' } },
      lookup: { list: f => (f === 'Stock_of_origin' ? { strict: false, values: new Set(['deceptus', 'messenoides', 'intermedia', 'NA']) } : undefined) },
    }),
  });
  const [messen, deceptus, other] = review.lines.map(l => l.cells.Stock_of_origin);
  assert.equal(messen.value, 'deceptus');
  assert.ok(messen.doubt);
  assert.deepEqual(messen.alternatives, ['messenoides']);
  assert.match(messen.reason, /El clutch 975 es Mechanitis messenoides deceptus: el stock es su subespecie; la página dice «messen\.»/);
  assert.ok(!deceptus.doubt);
  assert.equal(other.value, 'NA');
  assert.ok(!other.doubt);
});

test('death templates: unused tubes on every preserved or dead row; preserved larvae flash frozen, sex NOT_COLLECTED; "N/A" cause', () => {
  const recent = d('2026-09-28');
  // A wing-clipped butterfly preserved: its unused Tube_2 cells too.
  const clip = impliedValues({ text: { CAM_ID: 'CAM078001', Tube_1_id: 'FS1', Death_cause: 'Killed_Preserved' }, note: { wingClip: true }, death: recent });
  assert.equal(clip.values.Tube_2_id, 'NA');
  assert.equal(clip.values.Tube_2_tissue, 'NOT_COLLECTED');
  // A death without cause or sample: the not-preserved block.
  const bare = impliedValues({ text: {}, death: recent });
  assert.equal(bare.values.Tube_2_tissue, 'NOT_COLLECTED');
  assert.equal(bare.values.CAM_ID, 'NA');
  assert.equal(bare.values.Death_cause, undefined, 'the cause stays for the person');
  // "N/A" written as the cause: Unknown.
  const na = impliedValues({ text: { Death_cause: 'N/A' }, death: recent });
  assert.equal(na.values.Death_cause, 'Unknown');
  assert.equal(na.values.T1_Preservation_medium, 'NOT_COLLECTED');
  // An old larva (before 2025) is still flash frozen; found dead: Other, Dead.
  const old = d('2024-03-01');
  const larva = impliedValues({ text: { Tube_1_id: 'FS2' }, note: { larva: true, preserved: true, dead: true }, death: old });
  assert.equal(larva.values.T1_Preservation_medium, 'Flash frozen');
  assert.equal(larva.values.Death_cause, 'Other');
  assert.equal(larva.values.Preserved_Dead_Alive, 'Dead');
  assert.equal(larva.values.Research_purpose, 'F1/F2 mutation rate');

  // Through the review: a preserved 3rd instar takes Sex NOT_COLLECTED and emergence NA.
  const rows = [blank('J5E', 10), blank('J6E', 11)];
  const review = buildReview({
    transcription: page('emergence', [
      { v: { Insectary_ID: 'J5E', Sex: '-', 'CLUTCH NUMBER': '1006', Intro2Insectary_date: '-', Death_date: '2/10', Tube_1_id: 'FS90415474', Notes_Insectary_data: 'Preserved alive 3rd instar' } },
      { v: { Insectary_ID: 'J6E', Sex: 'female', 'CLUTCH NUMBER': '1006', Intro2Insectary_date: '28/9', Death_date: '2/10', Death_cause: 'N/A' } },
    ]),
    year: 2026,
    today: '2026-10-02',
    lookup: lookupOf(rows),
  });
  const [l, adult] = review.lines;
  assert.equal(l.cells.Sex.value, 'NOT_COLLECTED');
  assert.ok(l.cells.Sex.inferred && l.cells.Sex.include);
  assert.equal(l.cells.Intro2Insectary_date.value, 'NA');
  assert.equal(l.cells.Death_cause.value, 'Killed_Preserved');
  assert.equal(l.cells.T1_Preservation_medium.value, 'Flash frozen');
  assert.equal(l.cells.Tube_2_tissue.value, 'NOT_COLLECTED');
  assert.equal(l.cells.Research_purpose.value, 'F1/F2 mutation rate');
  assert.equal(adult.cells.Sex.value, 'female', 'an adult keeps its sex');
  assert.equal(adult.cells.Death_cause.value, 'Unknown');
  assert.equal(adult.cells.Tube_2_tissue.value, 'NOT_COLLECTED');
});

test("a preserved egg's or larva's LIFESTAGE comes from its note when the note names one stage", () => {
  assert.deepEqual(lifeStage('Preserved alive 3rd instar'), { value: '3rd instar larva', words: '3rd instar' });
  assert.equal(lifeStage('4th instar larva, flash frozen').value, '4th instar larva');
  assert.equal(lifeStage('instar 2').value, '2nd instar larva');
  assert.equal(lifeStage('tercer estadio').value, '3rd instar larva');
  assert.equal(lifeStage('eggs preserved').value, 'Egg');
  assert.equal(lifeStage('huevo').value, 'Egg');
  assert.equal(lifeStage('pre-pupa').value, 'Pre-pupa');
  assert.equal(lifeStage('prepupa dead').value, 'Pre-pupa');
  for (const vague of ['larva', 'larvas preserved', 'eggs and 3rd instar', '3rd and 4th instar', '13rd instar', 'pupa'])
    assert.equal(lifeStage(vague), null, vague);

  const rows = [blank('K1E', 10), blank('K2E', 11), blank('K3E', 12), blank('K4E', 13, { LIFESTAGE: '5th instar larva' }), blank('K5E', 14)];
  const line = (id, note, extra = {}) => ({
    v: { Insectary_ID: id, 'CLUTCH NUMBER': '1006', Death_date: '2/10', Tube_1_id: `FS9041547${id[1]}`, Notes_Insectary_data: note, ...extra },
  });
  const review = buildReview({
    transcription: page('emergence', [
      line('K1E', 'Preserved alive 4th instar'),
      line('K2E', 'eggs preserved'),
      // "larva" alone: no stage to write.
      line('K3E', 'larva preserved'),
      // The row already says another stage: it is kept.
      line('K4E', 'preserved 3rd instar'),
      // An adult with a word of a stage in its note is no larva.
      line('K5E', 'pupa malformed 3rd instar?', { Sex: 'female' }),
    ]),
    year: 2026,
    today: '2026-10-02',
    lookup: lookupOf(rows),
  });
  const [fourth, egg, vague, kept, adult] = review.lines.map(l => l.cells.LIFESTAGE);
  assert.equal(fourth.value, '4th instar larva');
  assert.ok(fourth.inferred && fourth.include);
  assert.equal(fourth.message, 'De la nota: «4th instar»');
  assert.equal(egg.value, 'Egg');
  assert.ok(egg.include);
  assert.equal(vague.value, null);
  assert.ok(!vague.include);
  assert.equal(kept.status, 'keep');
  assert.ok(!kept.include);
  assert.ok(!adult.include);
  // The note keeps its words.
  assert.equal(review.lines[0].cells.Notes_Insectary_data.value, 'Preserved alive 4th instar');
});

test('doubts written in a line instead of on its cells go to the cells they are about', () => {
  const s1d = rawDoubts('S1D Salapia ♀ 997 26/9 † 28/9 eaten (dudoso: entre S0D y S2D)', {
    Insectary_ID: 'S1D',
    Sex: 'female',
    Intro2Insectary_date: '26/9',
    Death_date: '28/9',
    Death_cause: 'Eaten',
  }, ['Insectary_ID']);
  assert.deepEqual(Object.keys(s1d.cells), ['Death_date', 'Death_cause']);
  assert.equal(s1d.cells.Death_date.reason, 'dudoso: entre S0D y S2D');
  assert.deepEqual(rawDoubts('P9B ♂ 980 (sexo dudoso: ♂ o ♀)', { Sex: 'male' }).cells.Sex.alternatives, ['female']);
  assert.deepEqual(rawDoubts('Z4C ♂ 997 «19/7» (¿19/9?)', { Sex: 'male', Intro2Insectary_date: '19/7' }).cells.Intro2Insectary_date.alternatives, ['19/9']);
  // A remark explaining a death that is not on this line; one in the page's own note.
  assert.deepEqual(rawDoubts('M0B ♂ viva (el 27/8 es de L9B)', { Sex: 'male' }), { cells: {}, unplaced: [] });
  assert.deepEqual(rawDoubts('H6D (labeled with another ID, unclear)', { Notes_Insectary_data: 'labeled with another ID, unclear' }).cells, {});

  const rows = [blank('S1D', 10), blank('X9C', 11)];
  const review = buildReview({
    transcription: page('emergence', [
      { raw: 'S1D ♀ 997 26/9 † 28/9 (¿23/9?)', v: { Insectary_ID: 'S1D', Sex: 'female', Intro2Insectary_date: '26/9', Death_date: '28/9' } },
      { raw: 'X9C (X/Y dudosa) ♀', v: { Insectary_ID: 'X9C', Sex: 'female' } },
    ]),
    year: 2026,
    today: '2026-10-02',
    lookup: lookupOf(rows),
  });
  const death = review.lines[0].cells.Death_date;
  assert.ok(death.doubt && death.include);
  assert.deepEqual(death.alternatives, [d('2026-09-23')]);
  assert.equal(death.reason, '¿23/9?');
  assert.match(review.lines[1].warnings[0], /«X\/Y dudosa» but no cell is marked doubtful/);
});

test('spans: a brace or ditto given once fills the lines between its ends that leave the column out', () => {
  const t = page(
    'emergence',
    [{ v: { Insectary_ID: 'A1A' } }, { v: { Insectary_ID: 'A2A', Death_date: '3/9' } }, { v: { Insectary_ID: 'A3A' } }, { v: { Insectary_ID: 'A4A' } }],
    { spans: [{ field: 'Death_date', value: '2/9', from: 'A1A', to: 'A3A' }] },
  );
  assert.deepEqual(
    t.lines.map(l => l.v.Death_date),
    ['2/9', '3/9', '2/9', undefined],
  );
  assert.throws(() => page('emergence', [{ v: { Insectary_ID: 'A1A' } }], { spans: [{ field: 'Death_date', value: '2/9', from: 'A1A', to: 'Z9Z' }] }), /Z9Z is not the Insectary_ID of a line/);
});

test('a line the save refuses shows as its row with the reason, never as a context row; a death line with 23 values is accepted', async () => {
  const sheets = new LocalSheets({
    Insectary_stocks: [{ row: 2, values: { 'CLUTCH NUMBER': 997, SPECIES: 'Ithomia salapia salapia' } }],
    Insectary_data: [
      { row: 2, values: { Insectary_ID: 'P5B' } },
      { row: 3, values: { Insectary_ID: 'P6B' } },
    ],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_stocks', 'Insectary_data'] });
  const lines = [
    { raw: 'P5B ♂ 997 26/9 † 28/9 unk', values: { Insectary_ID: 'P5B', Sex: 'male', 'CLUTCH NUMBER': '997', Stock_of_origin: 'NA', Intro2Insectary_date: '26/9', Death_date: '28/9', Death_cause: 'Unknown', Notes_Insectary_data: 'deformed wing' } },
    { raw: 'P6B ♀ 997 26/9', values: { Insectary_ID: 'P6B', Sex: 'female', 'CLUTCH NUMBER': '997', Intro2Insectary_date: '26/9' } },
  ];

  // The real save: the death line's 20+ values go in.
  const assistant = createAssistant({ store, config: {} });
  store.db
    .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u-franz','franz','Franz Chandi','editor','s','h',1,'2026-01-01')")
    .run();
  store.db.prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')").run(createHash('sha256').update('tok').digest('hex'), 'u-franz');
  const call = async (name, args) =>
    JSON.parse(
      (await assistant.mcp({ authorization: 'Bearer tok' }, { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } })).body.result.content[0].text,
    );
  const out = await call('match_notebook', { kind: 'emergence', year: 2026, lines, includeUnchanged: true });
  const p5b = out.lines.find(l => l.n === 1);
  assert.ok(!p5b.rowError, p5b.rowError);
  assert.equal(p5b.inProposal, true);

  // A save that refuses the first line: its row shows with the reason, nothing to write.
  const matcher = createNotebookMatcher({
    store,
    db: store.db,
    newIds: () => ({ proposed: new Set(), used: () => new Map() }),
    draftChanges: (args, ids) => {
      const row = (args.changes ?? args.newRows)[0];
      if (row.line === 1) return { error: 'Invalid values for P5B' };
      const record = store.getRecord(row.recordId);
      return { changes: [{ recordId: record.id, sheet: record.sheet, row: record.row, label: record.label, expectedVersion: record.version, before: {}, values: row.values, replaceFormula: [], note: row.note }] };
    },
    initialsFor: () => 'FCH',
  });
  const user = { id: 'u-franz', username: 'franz' };
  const matched = matcher.match({ kind: 'emergence', year: 2026, lines, includeUnchanged: true }, user);
  const refused = matched.changes.find(c => c.line === 1);
  assert.ok(!refused.context, 'not a context row');
  assert.equal(refused.rowError, 'Invalid values for P5B');
  assert.deepEqual(refused.values, {});
  assert.match(refused.note, /No se puede escribir: Invalid values for P5B/);
  const summary = matchSummary(matched, 'p1');
  assert.equal(summary.lines[0].rowError, 'Invalid values for P5B');
  assert.equal(summary.lines[0].inProposal, false);
  assert.ok(!summary.lines[0].contextRow);
  assert.equal(summary.counts.rowsInProposal, 1);
});
