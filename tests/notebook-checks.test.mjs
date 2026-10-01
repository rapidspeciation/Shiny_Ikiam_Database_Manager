import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { parseDateText } from '../server/schema.mjs';
import { createAssistant } from '../server/assistant.mjs';
import {
  NOT_PRESERVED,
  WING_CLIP,
  buildReview,
  checkTranscription,
  clutchRuns,
  idChecks,
  impliedValues,
  nearIds,
  noteColumns,
  proposalRows,
} from '../server/notebook.mjs';
import { carryChecks, setChecked, uncheckedDoubts, withoutUnchecked } from '../server/doubts.mjs';
import { twinRows } from '../server/verifications.mjs';

const d = text => parseDateText(text);
const page = (kind, lines, year = null) =>
  checkTranscription({
    kind,
    year,
    lines: lines.map(l => ({ raw: l.raw ?? '', values: l.v, confidence: l.c, alternatives: l.a, reasons: l.r })),
  }).transcription;
/** A sheet of rows by Insectary_ID, with the lookups match_notebook gives buildReview. */
function lookupOf(rows, extra = {}) {
  return {
    find: ([key]) => rows.filter(r => String(r.values.Insectary_ID).toUpperCase() === String(key).toUpperCase()).map(r => ({ formulas: {}, label: r.values.Insectary_ID, ...r })),
    clutch: value => (extra.clutches?.[String(value)] ? (/^\d+$/.test(String(value)) ? Number(value) : String(value)) : null),
    laidOfClutch: value => extra.clutches?.[String(value)]?.laid,
    speciesOfClutch: () => null,
    list: () => undefined,
    holder: () => null,
    newRowFormulas: new Set(),
    typedOverFormula: new Set(['SPECIES']),
    ...extra.lookup,
  };
}
const blank = (id, row, values = {}) => ({ id: `r${row}`, row, version: 1, values: { Insectary_ID: id, ...values } });

test("an Emergidos note's column words leave it: medium, wing clip, pheromones, cause, CAMs and tubes", () => {
  const read = note => {
    const text = { Notes_Insectary_data: note };
    const said = noteColumns(text);
    return { ...said, note: text.Notes_Insectary_data ?? null };
  };
  assert.deepEqual(read('{ ethanol'), { cams: [], tubes: [], medium: 'Ethanol', note: null });
  assert.equal(read('flash frozen }').medium, 'Flash frozen');
  assert.deepEqual(read('pheromone'), { cams: [], tubes: [], pheromone: true, note: null });
  assert.deepEqual(read('unk preserv. FS90415628'), { cams: [], tubes: ['FS90415628'], unknown: true, preserved: true, note: null });
  assert.deepEqual(read('preserved CAM076603 FS50851380'), {
    cams: ['CAM076603'],
    tubes: ['FS50851380'],
    preserved: true,
    note: null,
  });
  // What is a note stays a note, without the column words.
  assert.deepEqual(read('abit deformed, ethanol'), { cams: [], tubes: [], medium: 'Ethanol', note: 'abit deformed' });
  assert.equal(read('Preserved in ultrafridge at -80ºC').note, 'Preserved in ultrafridge at -80ºC');
  assert.equal(read('wc FS90415634').wingClip, true);
  assert.equal(read('emerged in cage of parents').note, 'emerged in cage of parents');
});

test('a death line implies its other columns, only where the row is empty', () => {
  const died = d('2024-10-10');
  // Killed and preserved in ethanol on the day it emerged.
  const kept = impliedValues({
    text: { CAM_ID: 'CAM076671', Tube_1_id: 'FS50851380', Death_date: '10/10' },
    note: { medium: 'Ethanol' },
    death: died,
    intro: died,
  }).values;
  assert.deepEqual(kept, {
    Death_cause: 'Killed_Preserved',
    Preservation_date: died,
    Preserved_Dead_Alive: 'Alive',
    Location_body: 'Ikiam',
    Tube_1_tissue: 'WHOLE_ORGANISM',
    T1_Preservation_medium: 'Ethanol',
    // Unused tubes: ID NA, tissue NOT_COLLECTED (Franz, 1 Oct 2026).
    Tube_2_id: 'NA',
    Tube_2_tissue: 'NOT_COLLECTED',
    T2_Preservation_medium: 'NOT_COLLECTED',
    Tube_3_id: 'NA',
    Tube_3_tissue: 'NOT_COLLECTED',
    Tube_4_id: 'NA',
    Tube_4_tissue: 'NOT_COLLECTED',
  });
  // Died, not preserved: the block Muertes writes (with Research_purpose NA): tubes NA, tissues and media NOT_COLLECTED.
  const lost = impliedValues({ text: { Death_date: '6/10', Death_cause: 'Unknown' }, death: d('2025-10-06') }).values;
  assert.deepEqual(lost, NOT_PRESERVED);
  for (const n of [1, 2, 3, 4]) {
    assert.equal(lost[`Tube_${n}_id`], 'NA');
    assert.equal(lost[`Tube_${n}_tissue`], 'NOT_COLLECTED');
  }
  // A wing clip at emergence (alive): its tube's tissue and medium; pheromones as the purpose.
  const clip = impliedValues({ text: { CAM_ID: 'CAM078054', Tube_1_id: 'FS90415634' }, note: { wingClip: true, pheromone: true }, intro: d('2025-08-11') }).values;
  assert.deepEqual(clip, { Research_purpose: 'Pheromones', Tube_1_tissue: WING_CLIP, T1_Preservation_medium: 'Flash frozen' });
  // "unk preserv. FS…" of a clipped butterfly: cause Unknown, the body in Tube_2.
  const body = impliedValues({
    text: { Death_date: '7/8', Tube_2_id: 'FS90415628' },
    row: { CAM_ID: 'CAM078050', Tube_1_id: 'FS90415600', Tube_1_tissue: WING_CLIP },
    note: { unknown: true, preserved: true },
    death: d('2025-08-07'),
  }).values;
  assert.equal(body.Death_cause, 'Unknown');
  assert.equal(body.Tube_2_tissue, 'WHOLE_ORGANISM');
  assert.equal(body.T2_Preservation_medium, 'Flash frozen');
  assert.equal(body.Preservation_date, d('2025-08-07'));
  assert.ok(!('Preserved_Dead_Alive' in body), 'found dead or alive is not said');
  // Nothing written about a death: nothing implied.
  assert.deepEqual(impliedValues({ text: { Sex: 'male' } }).values, {});
});

test('match_notebook fills the implied columns, keeps what the row has, and the note loses its column words', () => {
  const rows = [
    blank('6OO', 10),
    // Already closed by the Shiny bulk tool (NOT_COLLECTED tissues), with T2's medium a formula.
    blank('0VD', 11, { Tube_2_tissue: 'NOT_COLLECTED', T2_Preservation_medium: 'NOT_COLLECTED' }),
    blank('5VE', 12),
  ];
  rows[1].formulas = { T2_Preservation_medium: '=IF(Q11="","",X)' };
  const review = buildReview({
    transcription: page('emergence', [
      { v: { Insectary_ID: '6OO', Intro2Insectary_date: '10/10', Death_date: '10/10', CAM_ID: 'CAM076671', Tube_1_id: 'FS50851380', Notes_Insectary_data: 'ethanol' } },
      { v: { Insectary_ID: '0VD', Death_date: '6/10', Death_cause: 'Unknown' } },
      { v: { Insectary_ID: '5VE', Intro2Insectary_date: '11/8', Notes_Insectary_data: 'pheromone wc CAM078054 FS90415634' } },
    ]),
    year: 2025,
    today: '2026-09-30',
    initials: 'FCH',
    lookup: lookupOf(rows),
  });
  const [kept, lost, clip] = review.lines;
  assert.equal(kept.cells.Death_cause.value, 'Killed_Preserved');
  assert.ok(kept.cells.Death_cause.inferred && kept.cells.Death_cause.include);
  assert.equal(kept.cells.T1_Preservation_medium.value, 'Ethanol');
  assert.match(kept.cells.T1_Preservation_medium.message, /De la nota: «ethanol»/);
  assert.equal(kept.cells.Notes_Insectary_data.status, 'empty', 'the note was only the medium');
  assert.equal(lost.cells.T1_Preservation_medium.value, 'NOT_COLLECTED');
  assert.equal(lost.cells.Tube_2_tissue.status, 'same', 'NOT_COLLECTED already there: what the block writes');
  assert.match(lost.cells.Tube_2_tissue.message, /Muerte sin preservar/);
  assert.equal(lost.cells.T2_Preservation_medium.status, 'same');
  assert.equal(clip.cells.CAM_ID.value, 'CAM078054', 'the CAM written in the note goes to its column');
  assert.equal(clip.cells.Tube_1_id.value, 'FS90415634');
  assert.equal(clip.cells.Tube_1_tissue.value, WING_CLIP);
  assert.equal(clip.cells.Research_purpose.value, 'Pheromones');
  const { changes } = proposalRows(review);
  assert.ok(changes[0].inferred.includes('Death_cause'));
  assert.match(changes[0].hints.Location_body.text, /preservado/);
  assert.deepEqual(changes[0].hints.Location_body.msg, { key: 'Individuo preservado: lo que el equipo escribe siempre' });
  assert.ok(!('Notes_Insectary_data' in changes[2].values));
});

test('a clutch read unlike the run next to it (848 among 843s) is flagged, and taken from the run when it cannot be', () => {
  const runs = clutchRuns(
    [
      { 'CLUTCH NUMBER': '843', Intro2Insectary_date: '8/8' },
      { 'CLUTCH NUMBER': '843', Intro2Insectary_date: '8/8' },
      { 'CLUTCH NUMBER': '843', Intro2Insectary_date: '8/8' },
      { 'CLUTCH NUMBER': '848', Intro2Insectary_date: '8/8' },
      { 'CLUTCH NUMBER': '848', Intro2Insectary_date: '8/8' },
      { 'CLUTCH NUMBER': '839', Intro2Insectary_date: '8/8' },
    ],
    [],
    clutch => clutch === '843',
  );
  assert.deepEqual(Object.keys(runs), ['3', '4']);
  assert.equal(runs[3].value, '843');
  assert.deepEqual(runs[3].alternatives, ['848']);
  assert.match(runs[3].reason, /no tiene una puesta 20–90 días antes/);
  // Both possible: only the smaller run is pointed out, as it was read.
  const small = clutchRuns(
    [{ 'CLUTCH NUMBER': '843', Intro2Insectary_date: '8/8' }, { 'CLUTCH NUMBER': '843', Intro2Insectary_date: '8/8' }, { 'CLUTCH NUMBER': '848', Intro2Insectary_date: '8/8' }],
    [],
    () => true,
  );
  assert.deepEqual(Object.keys(small), ['2']);
  assert.equal(small[2].value, '848');
  assert.equal(small[2].confidence, 0.6);

  // Through the review: the clutch the row gets is 843, doubtful, with 848 to pick.
  const rows = ['5VD', '6VD', '7VD', '8VD', '9VD'].map((id, i) => blank(id, 20 + i));
  const review = buildReview({
    transcription: page(
      'emergence',
      ['843', '843', '843', '848', '848'].map((clutch, i) => ({ v: { Insectary_ID: rows[i].values.Insectary_ID, 'CLUTCH NUMBER': clutch, Intro2Insectary_date: '8/8' } })),
    ),
    year: 2025,
    today: '2026-09-30',
    lookup: lookupOf(rows, { clutches: { 843: { laid: d('2025-07-05') }, 848: { laid: null } } }),
  });
  const cell = review.lines[3].cells['CLUTCH NUMBER'];
  assert.equal(cell.value, 843);
  assert.ok(cell.doubt && cell.include);
  assert.deepEqual(cell.alternatives, [848]);
  assert.ok(!review.lines[0].cells['CLUTCH NUMBER'].doubt);
});

test('CAMs and tubes that do not fit their run: a digit too many or dropped, or far from the lines around', () => {
  const checks = idChecks(
    [
      { CAM_ID: 'CAM078045', Tube_1_id: 'FS50848960' },
      { CAM_ID: 'CAM0780460', Tube_1_id: 'FS5848961' },
      { CAM_ID: 'CAM078047', Tube_1_id: 'FS50848962' },
      { CAM_ID: 'CAM079934' },
      { CAM_ID: 'CAM078049' },
      { CAM_ID: 'CAM078050' },
    ],
    [],
  );
  assert.equal(checks[1].CAM_ID.value, 'CAM078046');
  assert.ok(checks[1].CAM_ID.alternatives.includes('CAM0780460'));
  assert.match(checks[1].CAM_ID.reason, /7 cifras/);
  assert.equal(checks[1].Tube_1_id.value, 'FS50848961', 'the digit dropped from FS50848…');
  assert.deepEqual(checks[1].Tube_1_id.alternatives.slice(0, 1), ['FS5848961']);
  assert.equal(checks[3].CAM_ID.value, 'CAM079934', 'far from its run: kept as read, flagged');
  assert.deepEqual(checks[3].CAM_ID.alternatives, ['CAM078048']);
  assert.ok(!checks[0] && !checks[2]);
  // Alone on the page: flagged as read, nothing to suggest.
  const alone = idChecks([{ Tube_1_id: 'FS3886683' }], []);
  assert.equal(alone[0].Tube_1_id.value, 'FS3886683');
  assert.deepEqual(alone[0].Tube_1_id.alternatives, []);
});

test('a value outside a list goes in doubtful, with the listed values it could be; an epithet takes its nominate subspecies', () => {
  const species = { strict: false, values: new Set(['Ithomia salapia salapia', 'Ithomia salapia derasa', 'Mechanitis lysimnia']) };
  const review = buildReview({
    transcription: page('emergence', [{ v: { Insectary_ID: '1AB', SPECIES: 'salapia' } }, { v: { Insectary_ID: '2AB', SPECIES: 'Ithomia' } }]),
    today: '2026-09-30',
    lookup: lookupOf([blank('1AB', 5), blank('2AB', 6)], { lookup: { list: field => (field === 'SPECIES' ? species : undefined) } }),
  });
  const [salapia, genus] = review.lines.map(l => l.cells.SPECIES);
  assert.equal(salapia.value, 'Ithomia salapia salapia');
  assert.ok(salapia.doubt && salapia.include);
  assert.deepEqual(salapia.alternatives, ['Ithomia salapia derasa']);
  assert.equal(salapia.reason, '«salapia» no está en la lista de SPECIES');
  assert.equal(genus.value, 'Ithomia', 'no single reading: kept as written');
  assert.deepEqual(genus.alternatives, ['Ithomia salapia salapia', 'Ithomia salapia derasa']);
});

test('an ID not in the sheet: did you mean the IDs one character away, nearest to its neighbours first', () => {
  assert.deepEqual(nearIds('9NM', ['9MN', '9NN', '8NM', 'ABC', '9NMX']), ['9MN', '9NN', '8NM']);
  const rows = [blank('4MN', 30), blank('9MN', 35), blank('9NN', 90)];
  const review = buildReview({
    transcription: page('emergence', [{ v: { Insectary_ID: '4MN', Sex: 'male' } }, { v: { Insectary_ID: '9NM', Sex: 'female' } }]),
    today: '2026-09-30',
    lookup: lookupOf(rows, {
      lookup: {
        nearIds: ([id]) =>
          nearIds(id, rows.map(r => r.values.Insectary_ID))
            .map(value => ({ value, row: rows.find(r => r.values.Insectary_ID === value).row }))
            .reverse(),
      },
    }),
  });
  const missing = review.lines[1];
  assert.equal(missing.status, 'missing');
  assert.deepEqual(missing.near.map(n => n.value), ['9MN', '9NN'], 'row 35 is next to row 30, where the line before is');
  assert.match(missing.message, /¿Quisiste decir 9MN \(fila 35\) o 9NN \(fila 90\)\?/);
});

test('doubtful cells: unchecked until edited or marked, carried to a page matched again, left out on request', () => {
  const rows = [
    { label: '8VD', sheet: 'Insectary_data', values: { 'CLUTCH NUMBER': 843, Sex: 'female' }, doubts: { 'CLUTCH NUMBER': { confidence: 0.4, alternatives: [848], reason: 'x' } } },
    { label: '9VD', sheet: 'Insectary_data', values: { Sex: 'male' }, doubts: { Sex: { confidence: 0.5, alternatives: ['female'] } }, personEdits: { Sex: { ai: 'female' } } },
    { label: '0VE', sheet: 'Insectary_data', values: {}, doubts: { Sex: { confidence: 0.5, alternatives: [] } }, context: true },
  ];
  assert.deepEqual(
    uncheckedDoubts(rows).map(u => [u.index, u.field, u.value]),
    [[0, 'CLUTCH NUMBER', 843]],
    'edited by the person, set back or a context row: not unchecked',
  );
  const checked = setChecked(rows[0], 'CLUTCH NUMBER', true, 'Franz');
  assert.equal(checked.doubts['CLUTCH NUMBER'].checked.by, 'Franz');
  assert.deepEqual(uncheckedDoubts([checked]), []);
  assert.deepEqual(withoutUnchecked(rows[0]).values, { Sex: 'female' });
  const same = (a, b) => JSON.stringify(a) === JSON.stringify(b);
  const byLabel = (a, b) => a.label === b.label;
  const again = carryChecks([checked], [{ ...rows[0] }, { ...rows[0], label: 'x' }], byLabel, same);
  assert.ok(again[0].doubts['CLUTCH NUMBER'].checked, 'same value read again: still checked');
  const other = carryChecks([checked], [{ ...rows[0], values: { 'CLUTCH NUMBER': 848 } }], byLabel, same);
  assert.ok(!other[0].doubts['CLUTCH NUMBER'].checked, 'another value: to check again');
});

test("a tube held by the butterfly's own Collection_data row is not a repeat", () => {
  assert.ok(twinRows({ Insectary_ID: '2VD', CAM_ID: 'CAM078045' }, { Insectary_ID: 'NA', CAM_ID_insectary: 'CAM078045' }));
  assert.ok(twinRows({ Insectary_ID: 'J6D' }, { Insectary_ID: 'j6d' }));
  assert.ok(!twinRows({ Insectary_ID: 'NA', CAM_ID: 'NA' }, { Insectary_ID: 'NA', CAM_ID_insectary: 'NA' }));
  assert.ok(!twinRows({ Insectary_ID: '2VD', CAM_ID: 'CAM078045' }, { Insectary_ID: 'K1Z', CAM_ID_insectary: 'CAM078046' }));
});

// ---------------------------------------------------------------------------
// Through MCP and the table's routes, as T3 Code and the person use them.

async function setup() {
  const sheets = new LocalSheets({
    Insectary_stocks: [
      { row: 2, values: { 'CLUTCH NUMBER': 833, SPECIES: 'Mechanitis lysimnia' } },
      { row: 3, values: { 'CLUTCH NUMBER': 843, SPECIES: 'Mechanitis messenoides messenoides' } },
    ],
    Insectary_data: [
      { row: 2, values: { Insectary_ID: '2VD', 'CLUTCH NUMBER': 833 } },
      { row: 3, values: { Insectary_ID: '3VD', 'CLUTCH NUMBER': 843 } },
    ],
    Collection_data: [
      // 2VD's own Collection_data row (its CAM in CAM_ID_insectary) holds its tube.
      { row: 2, values: { Release_Collect: 'Collected_Sent2Insectary', SPECIES: 'Mechanitis lysimnia', Insectary_ID: 'NA', CAM_ID_insectary: 'CAM078045', Tube_1_id: 'FS50851824' } },
      { row: 3, values: { Release_Collect: 'Collected_Preserved', SPECIES: 'Oleria onega', CAM_ID: 'CAM079001', Tube_1_id: 'FS50851999' } },
    ],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_stocks', 'Insectary_data', 'Collection_data'] });
  const assistant = createAssistant({ store, config: {} });
  store.db
    .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u-1','fch','FCH','editor','s','h',1,'2026-01-01')")
    .run();
  const token = 'token-checks';
  store.db
    .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
    .run(createHash('sha256').update(token).digest('hex'), 'u-1');
  const mcp = (method, params) => assistant.mcp({ authorization: `Bearer ${token}` }, { jsonrpc: '2.0', id: 1, method, params });
  const call = async (name, args) => JSON.parse((await mcp('tools/call', { name, arguments: args })).body.result.content[0].text);
  const user = { id: 'u-1', username: 'fch', displayName: 'FCH', role: 'editor' };
  const http = (method, path, body) => assistant.handle({ method, path, body, user, query: {} });
  return { store, call, http };
}

test('doubtful cells go into the proposal; apply waits until the person checks them; the twin tube is saved', async () => {
  const { store, call, http } = await setup();
  try {
    const out = await call('match_notebook', {
      kind: 'emergence',
      year: 2025,
      lines: [
        {
          raw: '2VD lys ♀? 833 CAM078045 FS50851824',
          values: { Insectary_ID: '2VD', Sex: 'female', CAM_ID: 'CAM078045', Tube_1_id: 'FS50851824' },
          confidence: { Sex: 0.5 },
          alternatives: { Sex: ['male'] },
          reasons: { Sex: 'the symbol is smudged' },
        },
        { raw: '3VD ♂ FS50851999', values: { Insectary_ID: '3VD', Sex: 'male', Tube_1_id: 'FS50851999' } },
      ],
    });
    const [twin, used] = out.lines;
    assert.ok(!twin.problems, `the twin row's tube is no repeat: ${JSON.stringify(twin.problems)}`);
    assert.equal(twin.doubtful.Sex.reason, 'the symbol is smudged');
    assert.match(used.problems.Tube_1_id, /ya está en Collection_data fila 3/, 'another butterfly still is');

    // Not applied while the doubtful sex is unchecked: the cells come back to ask the person about.
    const refused = await call('apply_proposal', { proposalId: out.proposalId });
    assert.match(refused.error, /doubtful cells not checked/);
    assert.deepEqual(refused.doubtful, [{ index: 0, label: '2VD', field: 'Sex', value: 'female', alternatives: ['male'], reason: 'the symbol is smudged' }]);
    const table = await call('get_proposal', { proposalId: out.proposalId });
    assert.deepEqual(table.rows[0].doubtful, { Sex: { alternatives: ['male'], reason: 'the symbol is smudged', checked: false } });

    // The table's apply asks too (409), and the person marks the cell checked there.
    const asked = await http('POST', `/api/chat/proposals/${out.proposalId}/apply`, { requestId: 'req-doubt-1' });
    assert.equal(asked.status, 409);
    assert.equal(asked.body.error.code, 'doubtful_unchecked');
    const listed = (await http('GET', '/api/chat/proposals')).body.proposals[0];
    const key = listed.changes[0].key;
    const marked = await http('POST', `/api/chat/proposals/${out.proposalId}/edit`, { cells: [], check: [{ key, field: 'Sex' }] });
    assert.equal(marked.status, 200);
    assert.equal(marked.body.proposal.changes[0].doubts.Sex.checked.by, 'FCH');

    const applied = await call('apply_proposal', { proposalId: out.proposalId });
    assert.equal(applied.status, 'applied', JSON.stringify(applied));
    const row = store.getRecordBySheetRow('Insectary_data', 2).values;
    assert.equal(row.Tube_1_id, 'FS50851824', 'saved: the same tube as its Collection_data row');
    assert.equal(row.Sex, 'female');
  } finally {
    store.close();
  }
});

test('the person confirms doubtful cells in the chat, or applies only the sure ones', async () => {
  const { store, call, http } = await setup();
  try {
    const lines = [{ raw: '3VD ♂? 843', values: { Insectary_ID: '3VD', Sex: 'male', Death_date: '9/8', Death_cause: 'Unknown' }, confidence: { Sex: 0.5 } }];
    const first = await call('match_notebook', { kind: 'emergence', year: 2025, lines });
    // "Sí, es macho": checked through update_proposal.
    const updated = await call('update_proposal', { proposalId: first.proposalId, rows: [{ index: 0, checked: ['Sex'] }] });
    assert.equal(updated.rows[0].doubtful.Sex.checked, true);
    assert.equal((await call('apply_proposal', { proposalId: first.proposalId })).status, 'applied');
    assert.equal(store.getRecordBySheetRow('Insectary_data', 3).values.Sex, 'male');

    // Another page: "aplica solo lo seguro" writes the rest and leaves the doubtful cell out.
    const second = await call('match_notebook', {
      kind: 'emergence',
      year: 2025,
      lines: [{ raw: '2VD ♀? 833', values: { Insectary_ID: '2VD', Sex: 'female', Intro2Insectary_date: '8/8' }, confidence: { Sex: 0.4 } }],
    });
    const out = await http('POST', `/api/chat/proposals/${second.proposalId}/apply`, { requestId: 'req-doubt-2', doubtful: 'skip' });
    assert.equal(out.status, 200, JSON.stringify(out.body));
    const row = store.getRecordBySheetRow('Insectary_data', 2).values;
    assert.equal(row.Intro2Insectary_date, d('2025-08-08'));
    assert.ok(!row.Sex, 'the doubtful sex was not written');
  } finally {
    store.close();
  }
});
