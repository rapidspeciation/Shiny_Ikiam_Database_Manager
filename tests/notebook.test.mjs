import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash, randomUUID } from 'node:crypto';
import { readFileSync } from 'node:fs';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { moduleMap, parseDateText } from '../server/schema.mjs';
import { createAssistant } from '../server/assistant.mjs';
import {
  KINDS,
  buildReview,
  checkTranscription,
  proposalRows,
  readValue,
  sameValue,
  sumTerms,
} from '../server/notebook.mjs';

const d = text => parseDateText(text);
/** A page in the shape the tests were written in ({raw, v, c, a, x}), through the tool's input check. */
const parseTranscription = text => {
  const data = JSON.parse(text);
  return checkTranscription({
    kind: data.kind,
    year: data.year,
    lines: data.lines.map(l => ({ raw: l.raw, values: l.v, confidence: l.c, alternatives: l.a, crossedOut: l.x })),
  }).transcription;
};

test('the skill names every column of every notebook the tool matches', () => {
  const skill = readFileSync(new URL('../assistant/skills/digitalizar-cuaderno/SKILL.md', import.meta.url), 'utf8');
  assert.match(skill, /^---\nname: digitalizar-cuaderno\ndescription: .+photo/m);
  for (const [id, kind] of Object.entries(KINDS)) {
    assert.ok(skill.includes(`\`${id}\``), `kind ${id}`);
    for (const field of kind.fields) assert.ok(skill.includes(`\`${field}\``), `${id}: ${field}`);
  }
});

test('the tool input is checked: unknown columns reported, doubts kept, crossed lines marked', () => {
  const { transcription, ignored } = checkTranscription({
    kind: 'emergence',
    year: '2025',
    lines: [
      { raw: '5VB decept', values: { Insectary_ID: '5VB', Pedigree: 'Yes', Sex: 'female', Death_date: null }, alternatives: { Sex: ['male', 'female'] } },
      { raw: '6VB tachado', values: { Insectary_ID: '6VB' }, crossedOut: true, confidence: { Insectary_ID: 3 } },
    ],
  });
  assert.equal(transcription.year, 2025);
  assert.deepEqual(ignored, ['Pedigree']);
  const [a, b] = transcription.lines;
  assert.deepEqual(a.v, { Insectary_ID: '5VB', Sex: 'female', Death_date: null });
  // Alternatives without a confidence make the cell doubtful; an unreadable cell has confidence 0.
  assert.deepEqual(a.c, { Sex: 0.5, Death_date: 0 });
  assert.deepEqual(a.a, { Sex: ['male'] });
  assert.ok(b.crossed);
  assert.deepEqual(b.c, { Insectary_ID: 1 });
  assert.throws(() => checkTranscription({ kind: 'diary', lines: [{ raw: 'x', values: {} }] }), /Unknown notebook kind/);
  assert.throws(() => checkTranscription({ kind: 'stocks', lines: [] }), /Give the lines/);
});

test('counts written as sums keep their terms in Insectary_stocks', () => {
  assert.deepEqual(sumTerms('12 + 15'), [12, 15]);
  assert.deepEqual(sumTerms('2+4=6+8=14'), [2, 4, 8]);
  assert.equal(sumTerms('4+6=0'), null, 'the steps do not add up');
  const stocks = { sheet: 'Insectary_stocks' };
  assert.equal(readValue('NUMBER OF EGGS', '12 + 15', stocks).value, '=12+15');
  assert.equal(readValue('NUMBER OF LARVAE', '2+4=6+8=14', stocks).value, '=2+4+8');
  assert.equal(readValue('NUMBER OF ADULTS', '19', stocks).value, '=19', 'typed like the sheet types them');
  assert.equal(readValue('NUMBER OF LARVAE', '4+6=0', stocks).value, 0);
  assert.ok(sameValue('NUMBER OF EGGS', '=12+15', '=12+15'));
  assert.ok(sameValue('NUMBER OF EGGS', 27, '=12+15'), 'a plain number and a sum: their total');
  assert.ok(!sameValue('NUMBER OF EGGS', '=14+13', '=12+15'), 'two sums: their terms');
});

test('notebook values are read as the sheet stores them', () => {
  assert.equal(readValue('Intro2Insectary_date', '17/9', { year: 2025 }).value, d('2025-09-17'));
  assert.equal(readValue('Intro2Insectary_date', '19-6-23', { year: 2025 }).value, d('2023-06-19'));
  assert.equal(readValue('Intro2Insectary_date', '17-Sep-25', { year: 2020 }).value, d('2025-09-17'));
  assert.match(readValue('Death_date', '3?/9', { year: 2025 }).error, /no legible/);
  assert.equal(readValue('Death_date', '—', { year: 2025 }).value, null);
  assert.equal(readValue('CLUTCH NUMBER', '994 (7)', {}).value, '994(7)');
  assert.equal(readValue('CLUTCH NUMBER', '838', {}).value, 838);
  assert.equal(readValue('NUMBER OF EGGS', '12 + 15', {}).value, 27, 'eggs of two plants, written as a sum');
  assert.equal(readValue('NUMBER OF LARVAE', '2+4=6+8=14', {}).value, 14);
  assert.equal(readValue('Sex', '♂', {}).value, 'male');
  assert.equal(readValue('CAM_ID', 'cam78038', {}).value, 'CAM078038');
  assert.equal(readValue('Tube_1_id', 'fs 50851817', {}).value, 'FS50851817');
  assert.ok(sameValue('CLUTCH NUMBER', '685 (3)', '685(3)'));
  assert.ok(sameValue('SPECIES', 'Mechanitis messenoides deceptus', 'mechanitis  messenoides deceptus.'));
  assert.ok(sameValue('Notes_Insectary_data', '4/8/25 FCH: abit deformed', 'abit deformed'));
  assert.ok(!sameValue('Sex', 'female', 'male'));
});

/** A small sheet for buildReview: rows by key, a strict Sex list, one CAM already used. */
function fakeLookup(rows, { formulas = {}, clutches = {}, newRows = new Set() } = {}) {
  return {
    find: ([key]) =>
      rows
        .filter(r => String(r.values.Insectary_ID ?? r.values['CLUTCH NUMBER']).toLowerCase() === String(key).toLowerCase())
        .map(r => ({ ...r, formulas: formulas[r.id] ?? {}, label: r.id })),
    clutch: value => clutches[String(value).replace(/\s/g, '')]?.written ?? null,
    speciesOfClutch: value => clutches[String(value).replace(/\s/g, '')]?.species ?? null,
    list: field =>
      ({
        Sex: { strict: true, values: new Set(['female', 'male', 'NA']) },
        Stock_of_origin: { strict: false, values: new Set(['deceptus', 'messenoides', 'intermedia', 'NA']) },
        SPECIES: {
          strict: false,
          values: new Set(['Mechanitis messenoides deceptus', 'Mechanitis messenoides intermedia', 'Mechanitis lysimnia']),
        },
      })[field],
    holder: (field, value, id) => (field === 'CAM_ID' && value === 'CAM078045' && id !== 'r2' ? { sheet: 'Insectary_data', row: 3 } : null),
    newRowFormulas: newRows,
    typedOverFormula: new Set(['SPECIES']),
  };
}

test('each line is compared with its row: fills, conflicts, doubts, formulas and problems', () => {
  const rows = [
    {
      id: 'r1',
      row: 2,
      version: 1,
      values: { Insectary_ID: '5VB', SPECIES: 'Mechanitis messenoides intermedia', Sex: 'female', 'CLUTCH NUMBER': 838, Intro2Insectary_date: d('2025-08-04') },
    },
    { id: 'r2', row: 3, version: 1, values: { Insectary_ID: '2VD', SPECIES: 'Mechanitis lysimnia', Sex: 'female', 'CLUTCH NUMBER': 833 } },
    { id: 'r3', row: 4, version: 1, values: { Insectary_ID: '1VD', SPECIES: 'Mechanitis messenoides intermedia', Sex: 'male' } },
  ];
  const lookup = fakeLookup(rows, {
    formulas: { r1: { SPECIES: '=X' }, r2: { SPECIES: '=X' }, r3: { SPECIES: '=X' } },
    clutches: { 838: { written: 838, species: 'Mechanitis messenoides intermedia' }, 833: { written: 833, species: 'Mechanitis lysimnia' } },
  });
  const transcription = parseTranscription(
    JSON.stringify({
      kind: 'emergence',
      lines: [
        // Same date without its year, a species that emerged differently (typed over the formula), a note.
        { y: 0.2, raw: '5VB decept ♀ 838 interme 4/8', v: { Insectary_ID: '5VB', SPECIES: 'Mechanitis messenoides deceptus', Sex: 'female', 'CLUTCH NUMBER': '838', Stock_of_origin: 'interme', Intro2Insectary_date: '4/8', Death_date: '7/8', Notes_Insectary_data: 'abit deformed' } },
        // A used CAM, a doubtful sex, a sex that differs, a death date read with low confidence.
        { y: 0.3, raw: '2VD lysimnia ♂ 833 CAM078045', v: { Insectary_ID: '2VD', SPECIES: 'Mechanitis lysimnia', Sex: 'male', CAM_ID: 'CAM078045', Death_date: '6/8' }, c: { Death_date: 0.5 }, a: { Death_date: ['8/8'] } },
        { y: 0.4, raw: '9ZZ', v: { Insectary_ID: '9ZZ', Sex: 'male' } },
        { y: 0.5, raw: '1VD (tachado)', x: true, v: { Insectary_ID: '1VD', Sex: 'female' } },
        { y: 0.6, raw: '1VD ♂ hybrid', v: { Insectary_ID: '1VD', Sex: 'mael', SPECIES: 'hybrid x hybrid' } },
      ],
    }),
  );
  const review = buildReview({ transcription, today: '2026-09-28', initials: 'FCH', lookup });
  assert.equal(review.year, 2025, 'year taken from the rows whose dates match');
  assert.equal(review.yearSource, 'inferred');
  const [a, b, c, crossed, e] = review.lines;

  assert.equal(a.status, 'match');
  assert.equal(a.cells.Insectary_ID.status, 'same');
  assert.equal(a.cells.Intro2Insectary_date.status, 'same');
  assert.equal(a.cells.Death_date.status, 'fill');
  assert.equal(a.cells.Death_date.value, d('2025-08-07'));
  assert.equal(a.cells.Stock_of_origin.value, 'intermedia', 'a short list value is completed');
  assert.equal(a.cells.SPECIES.status, 'conflict');
  assert.ok(a.cells.SPECIES.formula && a.cells.SPECIES.include);
  assert.equal(a.cells.Notes_Insectary_data.write, '28/9/26 FCH: abit deformed');
  assert.ok(a.picked);

  assert.equal(b.cells.Sex.status, 'conflict');
  assert.equal(b.cells.Sex.before, 'female');
  assert.ok(b.cells.Sex.include, 'the notebook is the primary record');
  assert.equal(b.cells.SPECIES.status, 'same', 'never type the species the formula already gives');
  assert.equal(b.cells.CAM_ID.status, 'fill', 'the row holding the CAM is this one');
  assert.ok(b.cells.Death_date.doubt && !b.cells.Death_date.include);
  assert.deepEqual(b.cells.Death_date.alternatives, [d('2025-08-08')]);

  assert.equal(c.status, 'missing');
  assert.ok(!c.picked && !c.changes);
  assert.equal(crossed.status, 'crossed');
  assert.ok(!crossed.picked);
  assert.equal(e.status, 'match', 'a crossed-out line does not count as a repeat');
  assert.equal(e.cells.Sex.status, 'error');
  assert.match(e.cells.Sex.message, /lista de Sex/);
  assert.ok(e.cells.SPECIES.doubt, 'a species outside the list waits for the person');

  // The person corrects the sex and confirms the date: they become part of the proposal.
  const edited = buildReview({
    transcription,
    edits: { 5: { Sex: 'male' }, 2: { Death_date: '8/8' } },
    picks: { 1: false },
    today: '2026-09-28',
    initials: 'FCH',
    lookup,
  });
  assert.equal(edited.lines[4].cells.Sex.status, 'same');
  assert.equal(edited.lines[1].cells.Death_date.value, d('2025-08-08'));
  assert.ok(edited.lines[1].cells.Death_date.include);
  assert.ok(!edited.lines[0].picked);
  const { changes, newRows } = proposalRows(edited);
  assert.deepEqual(newRows, []);
  assert.deepEqual(
    changes.map(ch => [ch.recordId, ch.line, Object.keys(ch.values).sort()]),
    [['r2', 2, ['CAM_ID', 'Death_date', 'Sex']]],
  );
  assert.match(changes[0].note, /Sex: hoja female → cuaderno male/);

  // Another CAM holder makes the cell an error.
  const other = buildReview({ transcription, today: '2026-09-28', lookup: { ...lookup, holder: () => ({ sheet: 'Collection_data', row: 9 }) } });
  assert.equal(other.lines[1].cells.CAM_ID.status, 'error');
  assert.match(other.lines[1].cells.CAM_ID.message, /Collection_data fila 9/);
});

test('IDs read with 0 for O find the pre-made rows in step with the page, not an old butterfly; CAM and tube runs continue', () => {
  const blankRow = (id, row) => ({ id: `r${row}`, row, version: 1, values: { Insectary_ID: id } });
  const rows = [
    blankRow('9OO', 100),
    blankRow('0OP', 101),
    blankRow('1OP', 102),
    blankRow('2OP', 103),
    // Older butterflies whose IDs look the same (digit zero).
    { id: 'old1', row: 40, version: 1, values: { Insectary_ID: '00P', Sex: 'male', 'CLUTCH NUMBER': 364 } },
    { id: 'old2', row: 50, version: 1, values: { Insectary_ID: '10P', Sex: 'female', 'CLUTCH NUMBER': 364 } },
  ];
  const lookup = { ...fakeLookup(rows), list: () => undefined, holder: () => null };
  const transcription = parseTranscription(
    JSON.stringify({
      kind: 'emergence',
      lines: [
        { raw: '9OO ♀ 715 CAM076671 FS50851380', v: { Insectary_ID: '9OO', Sex: 'female', CAM_ID: 'CAM076671', Tube_1_id: 'FS50851380' } },
        { raw: '00P ♀ 715 72 81', v: { Insectary_ID: '00P', Sex: 'female', CAM_ID: '72', Tube_1_id: '81' } },
        { raw: '10P ♀ 715 73 82', v: { Insectary_ID: '10P', Sex: 'female', CAM_ID: 'cam673', Tube_1_id: '82' } },
        { raw: '2OP ♂', v: { Insectary_ID: '2OP', Sex: 'male' } },
      ],
    }),
  );
  const review = buildReview({ transcription, today: '2026-09-28', lookup });
  assert.deepEqual(
    review.lines.map(l => [l.status, l.row, l.cells.Insectary_ID.value]),
    [
      ['match', 100, '9OO'],
      ['match', 101, '0OP'],
      ['match', 102, '1OP'],
      ['match', 103, '2OP'],
    ],
  );
  assert.equal(review.lines[2].message, 'Leído «10P»; en la hoja es 1OP');
  assert.deepEqual(
    review.lines.map(l => [l.cells.CAM_ID.value, l.cells.Tube_1_id.value]),
    [
      ['CAM076671', 'FS50851380'],
      ['CAM076672', 'FS50851381'],
      ['CAM076673', 'FS50851382'],
      [null, null],
    ],
  );
  assert.match(review.lines[1].cells.CAM_ID.message, /Escrito «72»/);
  // With nothing around it to tell them apart, a look-alike is left for the person.
  const alone = buildReview({
    transcription: parseTranscription(JSON.stringify({ kind: 'emergence', lines: [{ raw: '10P', v: { Insectary_ID: '10P' } }] })),
    today: '2026-09-28',
    lookup,
  });
  assert.equal(alone.lines[0].status, 'match', 'the ID as read wins a tie');
  assert.equal(alone.lines[0].row, 50);
});

test('a full species name in Stock_of_origin takes the list value; a wild butterfly gets its species typed', () => {
  const rows = [{ id: 'w1', row: 5, version: 1, values: { Insectary_ID: '7VC', SPECIES: null, Stock_of_origin: null } }];
  const lookup = fakeLookup(rows, { formulas: { w1: { SPECIES: '=X' } } });
  const transcription = parseTranscription(
    JSON.stringify({
      kind: 'emergence',
      lines: [{ raw: '7VC zaneka — messen.', v: { Insectary_ID: '7VC', SPECIES: 'Mechanitis lysimnia', Stock_of_origin: 'Mechanitis messenoides messenoides' } }],
    }),
  );
  const [line] = buildReview({ transcription, today: '2026-09-28', lookup }).lines;
  assert.equal(line.cells.Stock_of_origin.value, 'messenoides');
  assert.equal(line.cells.Stock_of_origin.status, 'fill');
  // The formula gives nothing (no clutch): the species is filled over it.
  assert.equal(line.cells.SPECIES.status, 'fill');
  assert.ok(line.cells.SPECIES.include && line.cells.SPECIES.formula);
});

test('a clutch page adds the clutches the sheet does not have yet, and its counts as the notebook sums them', () => {
  const rows = [
    { id: 's1', row: 900, version: 2, values: { 'CLUTCH NUMBER': '994(6)', SPECIES: 'Mechanitis lysimnia', 'DATE LAID': d('2026-09-01'), 'NUMBER OF EGGS': 0 } },
    { id: 's2', row: 901, version: 2, values: { 'CLUTCH NUMBER': 993, 'NUMBER OF EGGS': 27, 'NUMBER OF LARVAE': 20, 'NUMBER OF PUPA': 3 } },
  ];
  const lookup = {
    ...fakeLookup(rows, {
      newRows: new Set(['NUMBER OF EGGS', 'SPECIES']),
      formulas: {
        s1: { 'NUMBER OF EGGS': '=0' },
        s2: { 'NUMBER OF EGGS': '=12+15', 'NUMBER OF LARVAE': '=10+10', 'NUMBER OF PUPA': '=COUNTIF(A:A,1)' },
      },
    }),
    list: () => undefined,
  };
  const transcription = parseTranscription(
    JSON.stringify({
      kind: 'stocks',
      year: 2026,
      lines: [
        { raw: '994(6) lys 1/9 30 huevos, eclosión 5/9', v: { 'CLUTCH NUMBER': '994 (6)', 'DATE LAID': '1/9', 'NUMBER OF EGGS': '30', 'HATCHING DATE': '5/9' } },
        { raw: '994(7) lys 20/9 12+3', v: { 'CLUTCH NUMBER': '994(7)', SPECIES: 'Mechanitis lysimnia', 'DATE LAID': '20/9', 'NUMBER OF EGGS': '12+3' } },
        // Written in December, read in September: last year; it emerged in January, this year.
        { raw: '995 lys 28/12 … 20/1', v: { 'CLUTCH NUMBER': '995', 'DATE LAID': '28/12', 'EMERGENCE DATE': '20/1' } },
        // Counts kept as sums: the same sum agrees, another sum replaces it, a real formula stays.
        { raw: '993 12+15 larvas 10+11 pupas 4', v: { 'CLUTCH NUMBER': '993', 'NUMBER OF EGGS': '12+15', 'NUMBER OF LARVAE': '10+11', 'NUMBER OF PUPA': '4' } },
      ],
    }),
  );
  const review = buildReview({ transcription, today: '2026-09-28', lookup });
  assert.equal(review.yearSource, 'page');
  const [known, fresh, december, sums] = review.lines;
  assert.equal(sums.cells['NUMBER OF EGGS'].status, 'same');
  assert.equal(sums.cells['NUMBER OF LARVAE'].status, 'conflict');
  assert.equal(sums.cells['NUMBER OF LARVAE'].before, '=10+10', 'compared with the formula the sheet has');
  assert.equal(sums.cells['NUMBER OF LARVAE'].value, '=10+11');
  assert.ok(sums.cells['NUMBER OF LARVAE'].include);
  assert.equal(sums.cells['NUMBER OF PUPA'].status, 'formula');
  assert.ok(!sums.cells['NUMBER OF PUPA'].include);
  assert.equal(known.status, 'match');
  assert.equal(known.cells['HATCHING DATE'].status, 'fill');
  assert.equal(known.cells['NUMBER OF EGGS'].status, 'fill', 'a count still at =0 is filled');
  assert.equal(fresh.status, 'new');
  assert.equal(fresh.cells['NUMBER OF EGGS'].status, 'new', 'the new row takes the sum over its =0');
  assert.equal(fresh.cells.SPECIES.status, 'formula', 'another formula column of the new row is left');
  assert.equal(december.cells['DATE LAID'].value, d('2025-12-28'));
  assert.equal(december.cells['EMERGENCE DATE'].value, d('2026-01-20'));
  const { changes, newRows } = proposalRows(review);
  assert.deepEqual(
    changes.map(c => c.values),
    [{ 'NUMBER OF EGGS': '=30', 'HATCHING DATE': d('2026-09-05') }, { 'NUMBER OF LARVAE': '=10+11' }],
  );
  assert.match(changes[1].note, /NUMBER OF LARVAE: hoja =10\+10 → cuaderno =10\+11/);
  assert.deepEqual(newRows[0].values, { 'CLUTCH NUMBER': '994(7)', 'DATE LAID': d('2026-09-20'), 'NUMBER OF EGGS': '=12+3' });
});

// ---------------------------------------------------------------------------
// match_notebook through MCP, as T3 Code's Claude calls it.

async function setup() {
  const species = moduleMap.get('Insectary_data').fields.find(f => f.key === 'SPECIES').column;
  const sheets = new LocalSheets({
    Insectary_stocks: [
      { row: 2, values: { 'CLUTCH NUMBER': 838, SPECIES: 'Mechanitis messenoides intermedia' } },
      { row: 3, values: { 'CLUTCH NUMBER': 848, SPECIES: 'Mechanitis messenoides messenoides' } },
    ],
    Insectary_data: [
      { row: 2, values: { Insectary_ID: '5VB', 'CLUTCH NUMBER': 838, Sex: 'female', Intro2Insectary_date: d('2025-08-04') } },
      { row: 3, values: { Insectary_ID: '8VD', 'CLUTCH NUMBER': 848 } },
      { row: 4, values: { Insectary_ID: '9VD', 'CLUTCH NUMBER': 848, Sex: 'male', Tube_2_id: 'FD41377125' } },
      { row: 5, values: { Insectary_ID: '6OO', 'CLUTCH NUMBER': 848 } },
    ],
  });
  for (const [row, value] of [
    [2, 'Mechanitis messenoides intermedia'],
    [3, 'Mechanitis messenoides messenoides'],
    [4, 'Mechanitis messenoides messenoides'],
    [5, 'Mechanitis messenoides messenoides'],
  ])
    sheets.rows.get('Insectary_data').find(r => r.row === row).cells[species] = {
      userEnteredValue: { formulaValue: '=XLOOKUP(C2,Insectary_stocks!A:A,Insectary_stocks!C:C,"")' },
      effectiveValue: { stringValue: value },
    };
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_stocks', 'Insectary_data'] });
  const assistant = createAssistant({ store, config: { claude: { bin: '', users: new Set() } } });
  store.db
    .prepare(
      "INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u-franz','franz','Franz Chandi','editor','s','h',1,'2026-01-01')",
    )
    .run();
  const token = 'token-for-franz';
  store.db
    .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
    .run(createHash('sha256').update(token).digest('hex'), 'u-franz');
  const mcp = (method, params) => assistant.mcp({ authorization: `Bearer ${token}` }, { jsonrpc: '2.0', id: 1, method, params });
  const call = async (name, args) => JSON.parse((await mcp('tools/call', { name, arguments: args })).body.result.content[0].text);
  const user = { id: 'u-franz', username: 'franz', displayName: 'Franz Chandi', role: 'editor' };
  const http = (method, path, body) => assistant.handle({ method, path, body, user, query: {} });
  return { store, mcp, call, http };
}

const PAGE = {
  kind: 'emergence',
  year: 2025,
  title: 'emergidos 5VB–6OO',
  lines: [
    { raw: '5VB deceptus ♀ 838 4/8', values: { Insectary_ID: '5VB', SPECIES: 'Mechanitis messenoides deceptus', Sex: 'female', 'CLUTCH NUMBER': '838', Intro2Insectary_date: '4/8' } },
    { raw: '8VD messen. ♀ 848 8/8', values: { Insectary_ID: '8VD', SPECIES: 'Mechanitis messenoides messenoides', Sex: 'female', 'CLUTCH NUMBER': '848', Intro2Insectary_date: '8/8' }, confidence: { Sex: 0.5 }, alternatives: { Sex: ['male'] } },
    { raw: '9VD messen ♂ 848 8/8 dead 9/8 unk wc FD41377125', values: { Insectary_ID: '9VD', Sex: 'male', 'CLUTCH NUMBER': '848', Intro2Insectary_date: '8/8', Death_date: '9/8', Death_cause: 'Unknown', Tube_1_id: 'FD41377125' } },
    // The ID written with zeros: the row is 6OO (letter O).
    { raw: '600 " ♂ 848 8/8', values: { Insectary_ID: '600', SPECIES: 'Mechanitis messenoides messenoides', Sex: 'male', Intro2Insectary_date: '8/8' } },
    { raw: '7ZZ ♀', values: { Insectary_ID: '7ZZ', Sex: 'female' } },
  ],
};

test('match_notebook matches a transcribed page and leaves one proposal beside T3; a correction replaces it', async () => {
  const { store, mcp, call, http } = await setup();
  try {
    const tools = (await mcp('tools/list')).body.result.tools;
    const tool = tools.find(t => t.name === 'match_notebook');
    assert.ok(tool, 'offered to T3 Code');
    assert.match(tool.description, /stocks \(Posturas, Insectary_stocks\): CLUTCH NUMBER/);
    assert.ok(!tools.some(t => t.name === 'notebook_page'));

    const out = await call('match_notebook', PAGE);
    assert.equal(out.sheet, 'Insectary_data');
    assert.equal(out.year, 2025);
    const line = n => out.lines.find(l => l.n === n);
    // The species emerged differently from the clutch's prediction: typed over the formula.
    assert.deepEqual(line(1).differs.SPECIES, {
      sheet: 'Mechanitis messenoides intermedia',
      notebook: 'Mechanitis messenoides deceptus',
      note: 'La fórmula da «Mechanitis messenoides intermedia»; se escribirá encima',
    });
    // A doubtful sex is left out and reported with its alternative; the date still goes.
    assert.deepEqual(line(2).doubtful.Sex, { read: 'female', alternatives: ['male'], sheet: null });
    assert.deepEqual(line(2).fill, { Intro2Insectary_date: '2025-08-08' });
    // Dates as ISO; the tube already filed as this butterfly's Tube_2_id.
    assert.deepEqual(line(3).fill, { Intro2Insectary_date: '2025-08-08', Death_date: '2025-08-09', Death_cause: 'Unknown' });
    assert.match(line(3).problems.Tube_1_id, /ya está en Insectary_data fila 4/);
    assert.equal(line(4).label, '6OO');
    assert.match(line(4).message, /Leído «600»; en la hoja es 6OO/);
    assert.equal(line(5).status, 'missing');
    assert.equal(line(5).inProposal, false);
    assert.equal(out.counts.rowsInProposal, 4);
    assert.ok(out.proposalId);
    assert.match(out.review, /Cambios propuestos/);

    // Cambios propuestos (the panel beside T3) shows it at once, from T3 Code, with each row's notebook line.
    const listed = (await http('GET', '/api/chat/proposals')).body.proposals;
    assert.equal(listed.length, 1);
    assert.equal(listed[0].id, out.proposalId);
    assert.equal(listed[0].source, 'T3 Code');
    assert.match(listed[0].reason, /Cuaderno Emergidos \(Insectary_data\): emergidos 5VB–6OO/);
    assert.deepEqual(listed[0].changes.map(c => c.line), [1, 2, 3, 4]);
    assert.deepEqual(listed[0].changes[0].replaceFormula, ['SPECIES']);
    assert.match(listed[0].changes[1].note, /Sex dudoso: female \/ male \(no incluido\)/);

    // "Es macho": the page matched again replaces the proposal.
    const corrected = structuredClone(PAGE);
    corrected.lines[1] = { ...corrected.lines[1], values: { ...corrected.lines[1].values, Sex: 'male' }, confidence: {}, alternatives: {} };
    const again = await call('match_notebook', { ...corrected, replaceProposalId: out.proposalId });
    assert.equal(again.replaced, out.proposalId);
    assert.ok(!again.overlaps, 'the replaced proposal is not an overlap');
    assert.equal(again.lines[1].fill.Sex, 'male');
    const pending = (await http('GET', '/api/chat/proposals')).body.proposals;
    assert.deepEqual(pending.map(p => p.id), [again.proposalId]);

    // The same page matched from another chat without replacing: the overlap is pointed out.
    const twice = await call('match_notebook', PAGE);
    assert.equal(twice.overlaps[0].proposalId, again.proposalId);
    assert.equal(twice.overlaps[0].count, 4);
    await http('POST', `/api/chat/proposals/${twice.proposalId}/discard`, {});

    // "Sí, aplícalo": applied through apply_proposal, only on the person's word.
    const applied = await call('apply_proposal', { proposalId: again.proposalId });
    assert.equal(applied.status, 'applied', JSON.stringify(applied));
    assert.equal(store.getRecordBySheetRow('Insectary_data', 3).values.Sex, 'male');
    assert.equal(store.getRecordBySheetRow('Insectary_data', 2).values.SPECIES, 'Mechanitis messenoides deceptus');
    assert.equal(store.getRecordBySheetRow('Insectary_data', 4).values.Death_cause, 'Unknown');
  } finally {
    store.close();
  }
});

test('a clutch page proposes its counts as the notebook sums them, and they are written as such', async () => {
  const { store, call, http } = await setup();
  try {
    const out = await call('match_notebook', {
      kind: 'stocks',
      year: 2025,
      lines: [
        { raw: '838 interm. 12+15', values: { 'CLUTCH NUMBER': '838', 'NUMBER OF EGGS': '12+15', 'NUMBER OF LARVAE': '2+4=6+8=14' } },
        { raw: '848 messen. 7', values: { 'CLUTCH NUMBER': '848', 'NUMBER OF EGGS': '7' } },
      ],
    });
    assert.deepEqual(out.lines[0].fill, { 'NUMBER OF EGGS': '=12+15', 'NUMBER OF LARVAE': '=2+4+8' });
    const [proposal] = (await http('GET', '/api/chat/proposals')).body.proposals;
    assert.deepEqual(
      proposal.changes.map(c => c.values),
      [{ 'NUMBER OF EGGS': '=12+15', 'NUMBER OF LARVAE': '=2+4+8' }, { 'NUMBER OF EGGS': '=7' }],
    );
    const applied = await call('apply_proposal', { proposalId: out.proposalId });
    assert.equal(applied.status, 'applied', JSON.stringify(applied));
    const clutch = store.getRecordBySheetRow('Insectary_stocks', 2);
    assert.equal(clutch.formulas['NUMBER OF EGGS'], '=12+15');
    assert.equal(clutch.formulas['NUMBER OF LARVAE'], '=2+4+8');
  } finally {
    store.close();
  }
});

test('a dash in a text column is the sheet\'s NA: it fills an empty cell and matches an NA', () => {
  const rows = [
    { id: 'r1', row: 2, version: 1, values: { Insectary_ID: '8VE', Sex: 'female' } },
    { id: 'r2', row: 3, version: 1, values: { Insectary_ID: '9VE', Sex: 'female', Stock_of_origin: 'NA' } },
  ];
  const lookup = fakeLookup(rows);
  const transcription = parseTranscription(
    JSON.stringify({
      kind: 'emergence',
      lines: [
        { y: 0.2, raw: '8VE ♀ — 10/8', v: { Insectary_ID: '8VE', Sex: 'female', Stock_of_origin: '—', Death_date: '—' } },
        { y: 0.3, raw: '9VE ♀ —', v: { Insectary_ID: '9VE', Sex: 'female', Stock_of_origin: '—' } },
      ],
    }),
  );
  const [a, b] = buildReview({ transcription, today: '2026-09-28', initials: 'FCH', lookup }).lines;
  assert.equal(a.cells.Stock_of_origin.value, 'NA');
  assert.equal(a.cells.Stock_of_origin.status, 'fill');
  assert.equal(a.cells.Death_date.value, null, 'a dash in a date means nothing to write');
  assert.equal(b.cells.Stock_of_origin.status, 'same');
});
