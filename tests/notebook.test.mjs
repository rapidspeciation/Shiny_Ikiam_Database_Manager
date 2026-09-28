import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { moduleMap, parseDateText } from '../server/schema.mjs';
import { createAssistant } from '../server/assistant.mjs';
import {
  buildReview,
  pageText,
  parseTranscription,
  proposalRows,
  readValue,
  sameValue,
  transcriptionPrompt,
} from '../server/notebook.mjs';

const franz = { id: 'u-franz', username: 'franz', displayName: 'Franz Chandi', role: 'editor' };
const d = text => parseDateText(text);

test('the prompt names the columns of the chosen notebook, or all of them to detect it', () => {
  const stocks = transcriptionPrompt({ kind: 'stocks', lists: { Sex: ['female', 'male'] }, today: '2026-09-28' });
  assert.match(stocks, /"NUMBER OF EGGS"/);
  assert.doesNotMatch(stocks, /"Insectary_ID"/);
  assert.match(stocks, /Sex: female \| male/);
  const auto = transcriptionPrompt({ kind: 'auto', today: '2026-09-28' });
  for (const kind of ['stocks', 'emergence', 'deaths', 'crispr']) assert.match(auto, new RegExp(`kind "${kind}"`));
});

test('the model answer is read even inside a fence, and unknown columns are dropped', () => {
  const text =
    'Aquí está:\n```json\n{"kind":"emergence","rotate":90,"year":2025,"lines":[{"y":1.4,"raw":"5VB decept","v":{"Insectary_ID":"5VB","Pedigree":"Yes","Sex":"female"},"c":{"Sex":0.4,"Nope":1},"a":{"Sex":["male","female"]}}]}\n```';
  const page = parseTranscription(text);
  assert.equal(page.kind, 'emergence');
  assert.equal(page.rotate, 90);
  assert.equal(page.year, 2025);
  const [line] = page.lines;
  assert.equal(line.n, 1);
  assert.equal(line.y, 1);
  assert.deepEqual(line.v, { Insectary_ID: '5VB', Sex: 'female' });
  assert.deepEqual(line.c, { Sex: 0.4 });
  // An alternative equal to the reading itself is not an alternative.
  assert.deepEqual(line.a, { Sex: ['male'] });
  assert.throws(() => parseTranscription('no JSON here'), /no tiene líneas/);
  assert.throws(() => parseTranscription('{"kind":"diary","lines":[]}'), /tipo de cuaderno/);
  // The person's choice wins over what the model detected.
  assert.equal(parseTranscription('{"kind":"emergence","lines":[]}', 'deaths').kind, 'deaths');
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
  assert.match(pageText(review), /^1\. «5VB decept/);

  // Another CAM holder makes the cell an error.
  const other = buildReview({ transcription, today: '2026-09-28', lookup: { ...lookup, holder: () => ({ sheet: 'Collection_data', row: 9 }) } });
  assert.equal(other.lines[1].cells.CAM_ID.status, 'error');
  assert.match(other.lines[1].cells.CAM_ID.message, /Collection_data fila 9/);
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

test('a clutch page adds the clutches the sheet does not have yet, without their formula columns', () => {
  const rows = [
    { id: 's1', row: 900, version: 2, values: { 'CLUTCH NUMBER': '994(6)', SPECIES: 'Mechanitis lysimnia', 'DATE LAID': d('2026-09-01') } },
    { id: 's2', row: 901, version: 2, values: { 'CLUTCH NUMBER': 993, 'NUMBER OF EGGS': 27, 'NUMBER OF LARVAE': 20 } },
  ];
  const lookup = {
    ...fakeLookup(rows, {
      newRows: new Set(['NUMBER OF EGGS']),
      formulas: { s2: { 'NUMBER OF EGGS': '=12+15', 'NUMBER OF LARVAE': '=10+10' } },
    }),
    list: () => undefined,
  };
  const transcription = parseTranscription(
    JSON.stringify({
      kind: 'stocks',
      year: 2026,
      lines: [
        { raw: '994(6) lys 1/9 30 huevos, eclosión 5/9', v: { 'CLUTCH NUMBER': '994 (6)', 'DATE LAID': '1/9', 'NUMBER OF EGGS': '30', 'HATCHING DATE': '5/9' } },
        { raw: '994(7) lys 20/9 12', v: { 'CLUTCH NUMBER': '994(7)', SPECIES: 'Mechanitis lysimnia', 'DATE LAID': '20/9', 'NUMBER OF EGGS': '12' } },
        // Written in December, read in September: last year.
        { raw: '995 lys 28/12', v: { 'CLUTCH NUMBER': '995', 'DATE LAID': '28/12' } },
        // Counts typed in the sheet as sums: the same sum agrees, another one is pointed out (never written).
        { raw: '993 12+15 larvas 21', v: { 'CLUTCH NUMBER': '993', 'NUMBER OF EGGS': '12+15', 'NUMBER OF LARVAE': '21' } },
      ],
    }),
  );
  const review = buildReview({ transcription, today: '2026-09-28', lookup });
  assert.equal(review.yearSource, 'page');
  const [known, fresh, december, sums] = review.lines;
  assert.equal(sums.cells['NUMBER OF EGGS'].status, 'same');
  assert.equal(sums.cells['NUMBER OF LARVAE'].status, 'formula');
  assert.ok(sums.cells['NUMBER OF LARVAE'].mismatch && !sums.cells['NUMBER OF LARVAE'].include);
  assert.match(sums.cells['NUMBER OF LARVAE'].message, /La hoja tiene =10\+10 \(20\); el cuaderno dice 21/);
  assert.equal(known.status, 'match');
  assert.equal(known.cells['HATCHING DATE'].status, 'fill');
  assert.equal(known.cells['NUMBER OF EGGS'].status, 'fill');
  assert.equal(fresh.status, 'new');
  assert.equal(fresh.cells['NUMBER OF EGGS'].status, 'formula', 'a formula column of the new row is left');
  assert.equal(december.cells['DATE LAID'].value, d('2025-12-28'));
  const { changes, newRows } = proposalRows(review);
  assert.equal(changes.length, 1);
  assert.deepEqual(changes[0].values, { 'NUMBER OF EGGS': 30, 'HATCHING DATE': d('2026-09-05') });
  assert.deepEqual(newRows[0].values, { 'CLUTCH NUMBER': '994(7)', SPECIES: 'Mechanitis lysimnia', 'DATE LAID': d('2026-09-20') });
});

// ---------------------------------------------------------------------------

const RECORDED = JSON.stringify({
  kind: 'emergence',
  rotate: 0,
  year: 2025,
  headers: ['ID', 'Species', 'Sex', '# Clutch', 'Stock origin', 'Emerge date', 'Dead date', 'Notes'],
  lines: [
    { n: 1, y: 0.2, raw: '5VB deceptus ♀ 838 interme 4/8', v: { Insectary_ID: '5VB', SPECIES: 'Mechanitis messenoides deceptus', Sex: 'female', 'CLUTCH NUMBER': '838', Intro2Insectary_date: '4/8' } },
    { n: 2, y: 0.3, raw: '8VD messen. ♀ 848 messen. 8/8', v: { Insectary_ID: '8VD', SPECIES: 'Mechanitis messenoides messenoides', Sex: 'female', 'CLUTCH NUMBER': '848', Intro2Insectary_date: '8/8' }, c: { Sex: 0.5 }, a: { Sex: ['male'] } },
    { n: 3, y: 0.4, raw: '9VD messen ♂ 848 8/8 dead 9/8 unk', v: { Insectary_ID: '9VD', Sex: 'male', 'CLUTCH NUMBER': '848', Intro2Insectary_date: '8/8', Death_date: '9/8', Death_cause: 'Unknown' } },
  ],
});

async function setup(transcribe) {
  const species = moduleMap.get('Insectary_data').fields.find(f => f.key === 'SPECIES').column;
  const sheets = new LocalSheets({
    Insectary_stocks: [
      { row: 2, values: { 'CLUTCH NUMBER': 838, SPECIES: 'Mechanitis messenoides intermedia' } },
      { row: 3, values: { 'CLUTCH NUMBER': 848, SPECIES: 'Mechanitis messenoides messenoides' } },
    ],
    Insectary_data: [
      { row: 2, values: { Insectary_ID: '5VB', 'CLUTCH NUMBER': 838, Sex: 'female', Intro2Insectary_date: d('2025-08-04') } },
      { row: 3, values: { Insectary_ID: '8VD', 'CLUTCH NUMBER': 848, Sex: 'female' } },
      { row: 4, values: { Insectary_ID: '9VD', 'CLUTCH NUMBER': 848, Sex: 'male' } },
    ],
  });
  for (const [row, value] of [
    [2, 'Mechanitis messenoides intermedia'],
    [3, 'Mechanitis messenoides messenoides'],
    [4, 'Mechanitis messenoides messenoides'],
  ])
    sheets.rows.get('Insectary_data').find(r => r.row === row).cells[species] = {
      userEnteredValue: { formulaValue: '=XLOOKUP(C2,Insectary_stocks!A:A,Insectary_stocks!C:C,"")' },
      effectiveValue: { stringValue: value },
    };
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_stocks', 'Insectary_data'] });
  const calls = [];
  const assistant = createAssistant({
    store,
    config: {
      claude: { bin: '', users: new Set() },
      notebook: {
        transcribe: async (user, request) => {
          calls.push({ user: user.username, prompt: request.prompt, bytes: request.image.data.length });
          return transcribe(request);
        },
      },
    },
  });
  const photo = (bytes = 'page-1') => {
    const id = randomUUID();
    store.db
      .prepare('INSERT INTO attachments VALUES(?,?,?,?,?,?,?)')
      .run(id, null, 'p12.jpeg', 'image/jpeg', Buffer.from(bytes), franz.id, new Date().toISOString());
    return id;
  };
  const call = (method, path, body) => assistant.handle({ method, path, body, user: franz, query: {} });
  const waitReady = async id => {
    for (let i = 0; i < 100; i++) {
      const { job } = (await call('GET', `/api/notebook/jobs/${id}`)).body;
      if (!['queued', 'reading'].includes(job.status)) return job;
      await new Promise(resolve => setTimeout(resolve, 20));
    }
    throw new Error('page never read');
  };
  return { store, assistant, photo, call, waitReady, calls };
}

test('a photographed page is read in the background, reviewed, corrected and applied', async () => {
  const { store, photo, call, waitReady, calls } = await setup(async () => ({ text: RECORDED, model: 'recorded' }));
  try {
    const created = await call('POST', '/api/notebook/jobs', { attachmentId: photo(), kind: 'auto' });
    assert.equal(created.status, 201);
    assert.ok(['queued', 'reading'].includes(created.body.job.status));
    const job = await waitReady(created.body.job.id);
    assert.equal(job.status, 'ready');
    assert.equal(job.kind, 'emergence');
    assert.equal(calls.length, 1);
    assert.match(calls[0].prompt, /kind "stocks"/);
    const byLine = Object.fromEntries(job.reviewLines.map(l => [l.n, l]));
    assert.equal(byLine[1].cells.SPECIES.status, 'conflict');
    assert.ok(byLine[2].cells.Sex.doubt);
    assert.equal(byLine[3].cells.Death_date.status, 'fill');
    assert.deepEqual(job.options.Sex.sort(), ['NA', 'NOT_COLLECTED', 'female', 'male']);

    // The same proposal is in Cambios propuestos, and the page has its conversation with the transcription.
    const live = (await call('GET', '/api/chat/proposals')).body.proposals;
    assert.equal(live.length, 1);
    assert.equal(live[0].id, job.proposalId);
    // Line 2's sex is doubtful (left out); its emergence date fills an empty cell.
    assert.deepEqual(live[0].changes.map(c => c.line), [1, 2, 3]);
    assert.deepEqual(Object.keys(live[0].changes[1].values), ['Intro2Insectary_date']);
    assert.deepEqual(live[0].changes[0].replaceFormula, ['SPECIES']);
    const thread = (await call('GET', `/api/chat/threads/${job.threadId}`)).body;
    assert.equal(thread.messages[0].attachments.length, 1);
    assert.match(thread.messages[1].content, /2\. «8VD messen\. ♀ 848 messen\. 8\/8» → .*Sex=female\?/);

    // The person picks the other reading of line 2 and unticks line 1: the proposal follows.
    const patched = (await call('PATCH', `/api/notebook/jobs/${job.id}`, { edits: { 2: { Sex: 'male' } }, picks: { 1: false } })).body.job;
    const line2 = patched.reviewLines.find(l => l.n === 2);
    assert.equal(line2.cells.Sex.status, 'conflict');
    assert.ok(line2.cells.Sex.include && line2.picked);
    const updated = (await call('GET', '/api/chat/proposals')).body.proposals[0];
    assert.equal(updated.id, job.proposalId, 'the same proposal, updated in place');
    assert.deepEqual(updated.changes.map(c => c.line), [2, 3]);

    // Apply only line 3; line 2 stays as a new proposal.
    const applied = await call('POST', `/api/notebook/jobs/${job.id}/apply`, { lines: [3], requestId: randomUUID() });
    assert.equal(applied.status, 200, JSON.stringify(applied.body));
    assert.equal(applied.body.result.applied, 1);
    const row4 = store.getRecordBySheetRow('Insectary_data', 4);
    assert.equal(row4.values.Death_date, d('2025-08-09'));
    assert.equal(row4.values.Death_cause, 'Unknown');
    assert.equal(store.getRecordBySheetRow('Insectary_data', 3).values.Sex, 'female', 'line 2 is not written yet');
    const after = applied.body.job;
    assert.deepEqual(after.appliedLines, [3]);
    assert.equal(after.status, 'ready');
    assert.notEqual(after.proposalId, job.proposalId);
    const rest = (await call('GET', '/api/chat/proposals')).body.proposals;
    assert.deepEqual(rest.map(p => p.changes.map(c => c.line)), [[2]]);

    // A second photo of the same page: warned, and the earlier reading is used again.
    const again = (await call('POST', '/api/notebook/jobs', { attachmentId: photo(), kind: 'auto' })).body.job;
    assert.equal(again.status, 'ready');
    assert.equal(calls.length, 1, 'not read again');
    assert.ok(again.warnings.some(w => w.kind === 'photo' && w.jobId === job.id));
    await call('POST', `/api/notebook/jobs/${again.id}/discard`, {});

    // Discarding closes the page and its proposal; the history keeps it with its applied line.
    const closed = (await call('POST', `/api/notebook/jobs/${job.id}/discard`, {})).body.job;
    assert.equal(closed.status, 'done');
    assert.deepEqual((await call('GET', '/api/chat/proposals')).body.proposals, []);
    const list = (await call('GET', '/api/notebook/jobs')).body.jobs;
    assert.deepEqual(
      list.map(j => [j.status, j.appliedLines.length]),
      [
        ['discarded', 0],
        ['done', 1],
      ],
    );
  } finally {
    store.close();
  }
});

test('a page the AI cannot read ends in error and can be read again as another notebook', async () => {
  let answer = 'Lo siento, no puedo leer la foto.';
  const { store, photo, call, waitReady, calls } = await setup(async () => ({ text: answer, model: 'recorded' }));
  try {
    const { job } = (await call('POST', '/api/notebook/jobs', { attachmentId: photo('blurry'), kind: 'auto' })).body;
    const failed = await waitReady(job.id);
    assert.equal(failed.status, 'error');
    assert.match(failed.error, /no tiene líneas/);
    answer = RECORDED;
    const retried = await call('POST', `/api/notebook/jobs/${job.id}/retry`, { kind: 'deaths' });
    assert.equal(retried.status, 200);
    const ready = await waitReady(job.id);
    assert.equal(ready.kind, 'deaths');
    assert.ok(!ready.fields.includes('CLUTCH NUMBER'));
    assert.match(calls[1].prompt, /the daily round of dead butterflies/);
    assert.doesNotMatch(calls[1].prompt, /kind "stocks"/);
  } finally {
    store.close();
  }
});
