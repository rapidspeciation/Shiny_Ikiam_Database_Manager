// The same Insectary ID written on two butterflies, resolved as the curators do in
// Google Sheets: the row's ID gets a suffix (W2B → W2B.1, typed over its formula),
// and the second butterfly gets a row inserted right below (W2B.2), whose undo
// deletes it again. The local copy follows the rows that move.
import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets, GoogleSheets, moveRowRefs, structureRequests } from '../server/sheets.mjs';
import { applyBatch } from '../server/batch.mjs';
import { duplicateIdRow, suffixedId } from '../server/premade.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { moduleMap } from '../server/schema.mjs';

const SHEET = 'Insectary_data';
const col = key => moduleMap.get(SHEET).fields.find(f => f.key === key).column;
const user = { id: 'ed-1', username: 'ed', displayName: 'ED', role: 'editor' };

// The workbook's ID formula reads only the first two characters of the ID above.
const idFormula = row =>
  `=IF(MID(A${row - 1},2,1)="9", CHAR(CODE(LEFT(A${row - 1},1))+1) & "0B", LEFT(A${row - 1},1) & (MID(A${row - 1},2,1)+1) & "B")`;
/** What Sheets computes from the ID above (W2B.1 → W3B: LEFT and MID only). */
const sheetsNext = previous => {
  const text = String(previous ?? '');
  return text[1] === '9' ? `${String.fromCharCode(text.charCodeAt(0) + 1)}0B` : `${text[0]}${Number(text[1]) + 1}B`;
};
function evaluate(formula, { value }) {
  const ref = /^=IF\(MID\(A(\d+),2,1\)/.exec(formula);
  if (ref) return sheetsNext(value(Number(ref[1]), 0));
  return null;
}
const speciesFormula = row => `=IF(C${row}="","",XLOOKUP(C${row},Insectary_stocks!A:A,Insectary_stocks!C:C,""))`;
const F = (formula, value = null) => ({
  userEnteredValue: { formulaValue: formula },
  ...(value === null ? {} : { effectiveValue: { stringValue: value } }),
});
const V = value =>
  typeof value === 'number'
    ? { userEnteredValue: { numberValue: value }, effectiveValue: { numberValue: value } }
    : { userEnteredValue: { stringValue: value }, effectiveValue: { stringValue: value } };
const SEX_LIST = { condition: { type: 'ONE_OF_LIST', values: [{ userEnteredValue: 'male' }, { userEnteredValue: 'female' }] } };
const DATE = { numberFormat: { type: 'DATE', pattern: 'd-mmm-yy' } };

/** W0B (typed) … W6B by formula; W0B–W4B hold butterflies, W5B and W6B are pre-made. */
function workbook() {
  const ids = ['W0B', 'W1B', 'W2B', 'W3B', 'W4B', 'W5B', 'W6B'];
  return ids.map((id, i) => {
    const row = i + 2;
    const used = row <= 6;
    const cells = [];
    cells[col('Insectary_ID')] = row === 2 ? V(id) : F(idFormula(row), id);
    cells[col('SPECIES')] = F(speciesFormula(row));
    cells[col('Sex')] = { dataValidation: SEX_LIST, ...(used ? V(row % 2 ? 'male' : 'female') : {}) };
    cells[col('Death_date')] = { userEnteredFormat: DATE };
    if (used) cells[col('CLUTCH NUMBER')] = V(800 + row);
    return { row, cells };
  });
}

async function fixture() {
  const sheets = new LocalSheets({ [SHEET]: workbook() }, { evaluate });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: [SHEET] });
  const at = row => store.getRecordBySheetRow(SHEET, row);
  const cell = (row, key) => sheets.cell(SHEET, row, col(key));
  const idOf = row => cell(row, 'Insectary_ID')?.userEnteredValue?.stringValue ?? cell(row, 'Insectary_ID')?.effectiveValue?.stringValue;
  return { sheets, store, at, cell, idOf };
}

test('suffixed IDs: W2B.2 is a second butterfly of W2B; it goes below the last row of its group', async () => {
  assert.deepEqual(suffixedId(' w2b.2 '), { id: 'W2B.2', base: 'W2B', n: 2 });
  assert.equal(suffixedId('W2B'), null);
  assert.equal(suffixedId('W2B.0'), null);
  const { store } = await fixture();
  assert.equal(duplicateIdRow(store, 'W2B.2').anchor.row, 4);
  assert.equal(duplicateIdRow(store, 'W3B'), null, 'no suffix: not a duplicate');
  assert.equal(duplicateIdRow(store, 'X9Z.1').problem, 'no_base');
  assert.equal(duplicateIdRow(store, 'W5B.1').problem, 'empty_base', 'an empty pre-made row takes the butterfly itself');
  store.close();
});

test('W2B → W2B.1 is typed over the ID formula only as a suffix of the row’s own ID; the series goes on and undo restores it', async () => {
  const { sheets, store, at, cell, idOf } = await fixture();
  const record = at(4);
  const rename = value => ({
    requestId: randomUUID(),
    edits: [{ id: record.id, values: { Insectary_ID: value }, replaceFormula: ['Insectary_ID'] }],
  });
  // Without asking to replace the formula, or with another ID, the cell stays the sheet's.
  await assert.rejects(applyBatch(store, { requestId: randomUUID(), edits: [{ id: record.id, values: { Insectary_ID: 'W2B.1' } }] }, user), e =>
    e.details.items.some(i => i.code === 'FORMULA_CELL'),
  );
  for (const wrong of ['W9B', 'W2B.X', 'W3B.1', 'W2B.0'])
    await assert.rejects(applyBatch(store, rename(wrong), user, { source: 'ai_approved' }), e =>
      e.details.items.some(i => ['FORMULA_CELL', 'INVALID_VALUES', 'DUPLICATE_ID'].includes(i.code)),
    );
  // Only Insectary_data's ID: another sheet's formula is not opened by it.
  const saved = await applyBatch(store, rename('W2B.1'), user, { source: 'ai_approved' });
  assert.equal(saved.status, 'verified');
  assert.equal(cell(4, 'Insectary_ID').userEnteredValue.stringValue, 'W2B.1');
  assert.equal(at(4).values.Insectary_ID, 'W2B.1');
  assert.equal(at(4).id, record.id);
  // The next row's formula still reads row 4, and LEFT/MID of W2B.1 still give W3B.
  assert.equal(cell(5, 'Insectary_ID').userEnteredValue.formulaValue, idFormula(5));
  assert.equal(sheetsNext(idOf(4)), 'W3B');

  await store.undo({ actionIds: [saved.action.id], requestId: randomUUID() }, user);
  assert.equal(cell(4, 'Insectary_ID').userEnteredValue.formulaValue, idFormula(4));
  assert.equal(sheets.rows.get(SHEET).length, 8);
  store.close();
});

test('W2B.2 is inserted below W2B with its formulas and dropdowns; rows below move in the sheet and in the app; undo deletes it', async () => {
  const { sheets, store, at, cell, idOf } = await fixture();
  const w3b = at(5),
    w4b = at(6),
    w5b = at(7);
  const saved = await store.applyProposal(
    [
      // The first butterfly keeps its row as W2B.1…
      { recordId: at(4).id, sheet: SHEET, values: { Insectary_ID: 'W2B.1' }, before: { Insectary_ID: 'W2B' }, replaceFormula: ['Insectary_ID'] },
      // …the second gets W2B.2 in a new row right below…
      { create: true, sheet: SHEET, clientId: 'c-dup', values: { Insectary_ID: 'W2B.2', 'CLUTCH NUMBER': 900, Notes_Insectary_data: 'second butterfly' } },
      // …and a row below it, and a pre-made row at the end, are written in the same save.
      { recordId: w4b.id, sheet: SHEET, values: { Sex: 'male' }, before: { Sex: 'female' } },
      { create: true, sheet: SHEET, clientId: 'c-next', values: { Insectary_ID: 'W5B', Sex: 'female' } },
    ],
    { user, requestId: randomUUID() },
  );
  assert.equal(saved.status, 'verified');
  // The sheet: W2B.1, W2B.2 (inserted), then W3B… one row lower.
  assert.deepEqual([2, 3, 4, 5, 6, 7, 8, 9].map(idOf), ['W0B', 'W1B', 'W2B.1', 'W2B.2', 'W3B', 'W4B', 'W5B', 'W6B']);
  assert.equal(cell(5, 'Insectary_ID').userEnteredValue.stringValue, 'W2B.2');
  assert.equal(cell(5, 'SPECIES').userEnteredValue.formulaValue, speciesFormula(5), 'the formulas of the row above, moved to the new row');
  // Dropdowns and formats come with the copy; the copied row's values do not.
  assert.deepEqual(cell(5, 'Sex').dataValidation, SEX_LIST);
  assert.equal(cell(5, 'Sex').userEnteredValue, undefined);
  assert.deepEqual(cell(5, 'Death_date').userEnteredFormat, DATE);
  assert.equal(cell(5, 'Notes_Insectary_data').userEnteredValue.stringValue, 'second butterfly');
  assert.equal(cell(5, 'CLUTCH NUMBER').userEnteredValue.numberValue, 900, 'its own values, not the copied row’s');
  // W3B's formula still reads the row above the inserted one: the series is intact.
  assert.match(cell(6, 'Insectary_ID').userEnteredValue.formulaValue, /MID\(A4,/);
  assert.match(cell(7, 'Insectary_ID').userEnteredValue.formulaValue, /MID\(A6,/);
  assert.equal(cell(6, 'SPECIES').userEnteredValue.formulaValue, speciesFormula(6));
  assert.equal(cell(7, 'Sex').userEnteredValue.stringValue, 'male', 'the edit of W4B landed on its moved row');
  assert.equal(cell(8, 'Sex').userEnteredValue.stringValue, 'female', 'the pre-made W5B was filled at its moved row');

  // The app's copy: same records, one row lower; the new one at row 5.
  assert.equal(at(6).id, w3b.id);
  assert.equal(at(7).id, w4b.id);
  assert.equal(at(8).id, w5b.id);
  assert.equal(at(7).values.Sex, 'male');
  assert.equal(at(8).values.Sex, 'female');
  const inserted = at(5);
  assert.equal(inserted.values.Insectary_ID, 'W2B.2');
  assert.equal(saved.created.find(c => c.clientId === 'c-dup').recordId, inserted.id);
  // A sync reads the sheet as the app already has it: nothing moves or changes.
  const sync = await store.sync({ sheets: [SHEET], force: true });
  assert.deepEqual(sync.bySheet[SHEET], { added: 0, changed: 0, moved: 0, missing: 0, cells: 0 });

  // A proposal drafted before the insert still finds its row by record (W4B, now row 7).
  const later = await store.applyProposal([{ recordId: w4b.id, sheet: SHEET, values: { Sex: 'female' }, before: { Sex: 'male' } }], {
    user,
    requestId: randomUUID(),
  });
  assert.equal(later.status, 'verified');
  assert.equal(cell(7, 'Sex').userEnteredValue.stringValue, 'female');

  // A third butterfly goes below the last of the group (W2B.2).
  const third = await store.applyProposal([{ create: true, sheet: SHEET, clientId: 'c-3', values: { Insectary_ID: 'W2B.3', Sex: 'female' } }], {
    user,
    requestId: randomUUID(),
  });
  assert.deepEqual([4, 5, 6, 7].map(idOf), ['W2B.1', 'W2B.2', 'W2B.3', 'W3B']);
  assert.equal(at(7).id, w3b.id);

  // Undo: the row inserted by that save is deleted; the rows below move back up.
  const preview = store.previewUndo({ actionIds: [third.action.id] });
  assert.equal(preview.eligible, true);
  assert.deepEqual(preview.rowDeletes.map(r => r.row), [6]);
  await store.undo({ actionIds: [third.action.id], requestId: randomUUID() }, user);
  assert.deepEqual([4, 5, 6, 7].map(idOf), ['W2B.1', 'W2B.2', 'W3B', 'W4B']);
  assert.equal(at(6).id, w3b.id);
  assert.equal(at(7).id, w4b.id);
  assert.equal(store.getRecord(third.created[0].recordId).missing, true);
  assert.match(cell(6, 'Insectary_ID').userEnteredValue.formulaValue, /MID\(A4,/);
  assert.match(cell(7, 'Insectary_ID').userEnteredValue.formulaValue, /MID\(A6,/);
  store.close();
});

test('an inserted row is undone whole, and not once another save wrote in it; its undo cannot be redone', async () => {
  const { store, at, idOf } = await fixture();
  const saved = await store.applyProposal([{ create: true, sheet: SHEET, clientId: 'c', values: { Insectary_ID: 'W2B.1', Sex: 'male', 'CLUTCH NUMBER': 901 } }], {
    user,
    requestId: randomUUID(),
  });
  const inserted = at(5);
  assert.equal(inserted.values.Insectary_ID, 'W2B.1');
  // One cell of it: refused (the row would stay, half empty, in the middle of the sheet).
  const one = store.db.prepare("SELECT id FROM changes WHERE action_id=? AND field='Sex'").get(saved.action.id).id;
  const partial = store.previewUndo({ actionIds: [saved.action.id], changeIds: [one] });
  assert.equal(partial.eligible, false);
  assert.deepEqual(partial.conflicts.map(c => c.reason), ['inserted_row_partial']);

  // Someone records its death: undoing the insert would lose it.
  const death = await store.applyProposal([{ recordId: inserted.id, sheet: SHEET, values: { Death_date: 46300 }, before: { Death_date: null } }], {
    user,
    requestId: randomUUID(),
  });
  const blocked = store.previewUndo({ actionIds: [saved.action.id] });
  assert.equal(blocked.eligible, false);
  assert.ok(blocked.conflicts.every(c => c.reason === 'inserted_row_changed'));
  await assert.rejects(store.undo({ actionIds: [saved.action.id], requestId: randomUUID() }, user), e => e.code === 'UNDO_CONFLICT');
  // Once that save is undone, the insert can be.
  await store.undo({ actionIds: [death.action.id], requestId: randomUUID() }, user);
  const undo = await store.undo({ actionIds: [saved.action.id], requestId: randomUUID() }, user);
  assert.equal(undo.status, 'verified');
  assert.deepEqual([4, 5, 6].map(idOf), ['W2B', 'W3B', 'W4B']);
  // Its undo (a row deleted) is not redone: the row is gone.
  const redo = store.previewUndo({ actionIds: [undo.action.id] });
  assert.equal(redo.eligible, false);
  assert.ok(redo.conflicts.every(c => c.reason === 'missing_record'));
  store.close();
});

test('a suffixed ID that is used, has no butterfly to follow, or whose group moved is refused', async () => {
  const { sheets, store } = await fixture();
  const create = id => ({ requestId: randomUUID(), creates: [{ module: SHEET, values: { Insectary_ID: id, Sex: 'male' } }] });
  await assert.rejects(applyBatch(store, create('X1Z.1'), user), e => e.details.items.some(i => i.code === 'ID_NOT_FOUND'));
  await assert.rejects(applyBatch(store, create('W5B.1'), user), e => e.details.items.some(i => i.code === 'ID_FREE'));
  // Two new rows of one group in one save: one at a time.
  await assert.rejects(
    applyBatch(store, { requestId: randomUUID(), creates: [create('W2B.1').creates[0], create('W2B.2').creates[0]] }, user),
    e => e.details.items.some(i => i.code === 'ROW_COLLISION'),
  );
  // Someone renamed the row in Google Sheets meanwhile: nothing is inserted.
  await sheets.externalEdit(SHEET, 4, { Insectary_ID: 'W2X' });
  await assert.rejects(applyBatch(store, create('W2B.1'), user), e => e.details.items.some(i => i.code === 'ROW_MOVED'));
  assert.equal(sheets.rows.get(SHEET).length, 8);
  store.close();
});

test('the requests: rows inserted from the bottom up, copied, emptied, then the cells at their final rows', async () => {
  const write = (at, extra = {}) => ({
    sheet: SHEET,
    row: at,
    changes: { Insectary_ID: 'X' },
    columns: { Insectary_ID: 0 },
    insert: { at, source: at - 1, width: 10, clear: [0, 2, 3, 7], formulas: [{ column: 7, from: at - 2 }] },
    ...extra,
  });
  const requests = structureRequests([write(5), write(40)], () => 77);
  assert.deepEqual(
    requests.map(r => Object.keys(r)[0]),
    ['insertDimension', 'copyPaste', 'updateCells', 'updateCells', 'updateCells', 'copyPaste', 'insertDimension', 'copyPaste', 'updateCells', 'updateCells', 'updateCells', 'copyPaste'],
  );
  assert.deepEqual(requests[0].insertDimension.range, { sheetId: 77, dimension: 'ROWS', startIndex: 39, endIndex: 40 });
  assert.equal(requests[0].insertDimension.inheritFromBefore, true);
  assert.deepEqual(requests[1].copyPaste.source, { sheetId: 77, startRowIndex: 38, endRowIndex: 39, startColumnIndex: 0, endColumnIndex: 10 });
  assert.equal(requests[1].copyPaste.pasteType, 'PASTE_NORMAL');
  // Columns 2–3 emptied in one request.
  assert.deepEqual([requests[2], requests[3], requests[4]].map(r => [r.updateCells.range.startColumnIndex, r.updateCells.range.endColumnIndex]), [[0, 1], [2, 4], [7, 8]]);
  assert.equal(requests[5].copyPaste.pasteType, 'PASTE_FORMULA');
  assert.deepEqual(
    structureRequests([{ sheet: SHEET, row: 9, changes: {}, deleteRow: { at: 9 } }], () => 77)[0],
    { deleteDimension: { range: { sheetId: 77, dimension: 'ROWS', startIndex: 8, endIndex: 9 } } },
  );

  // Google gets the structure first, then the cells; no row appended for an inserted one.
  const google = Object.create(GoogleSheets.prototype),
    calls = [];
  google.gridRows = new Map([[SHEET, 10]]);
  google.metadataAt = Date.now();
  google.request = async (path, options) => (calls.push(JSON.parse(options.body)), { replies: [] });
  await google.writeBatch([write(11, { row: 11 })]);
  const sent = calls[0].requests.map(r => Object.keys(r)[0]);
  assert.equal(sent[0], 'insertDimension');
  assert.equal(sent.at(-1), 'updateCells');
  assert.ok(!sent.includes('appendDimension'));
  assert.equal(google.gridRows.get(SHEET), 11);
  assert.equal(moveRowRefs('=IF(MID(A12,2,1)="9",A$13,Insectary_stocks!A13)', 13, 1), '=IF(MID(A12,2,1)="9",A$14,Insectary_stocks!A13)');
  assert.equal(moveRowRefs('=A12+A13', 13, -1), '=A12+#REF!');
});

// ---------------------------------------------------------------- the assistant's proposals

async function assistantFixture() {
  const { sheets, store, at, idOf } = await fixture();
  const assistant = createAssistant({ store, config: {} });
  store.db
    .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('ed-1','ed','ED','editor','s','h',1,'2026-01-01')")
    .run();
  const token = 'token-dup';
  const { createHash } = await import('node:crypto');
  store.db
    .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
    .run(createHash('sha256').update(token).digest('hex'), 'ed-1');
  const call = async (name, args) =>
    JSON.parse(
      (await assistant.mcp({ authorization: `Bearer ${token}` }, { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } }))
        .body.result.content[0].text,
    );
  return { sheets, store, at, idOf, call, assistant };
}

test('propose_changes: W2B → W2B.1 and a new W2B.2 row, applied as the curators do it', async () => {
  const { store, at, idOf, call } = await assistantFixture();
  try {
    const w2b = at(4);
    // Any other change of the ID formula is refused.
    const wrong = await call('propose_changes', { reason: 'x', changes: [{ recordId: w2b.id, values: { Insectary_ID: 'W7B' } }] });
    assert.match(wrong.error, /only takes a suffix \(W2B\.1\)/);
    const notFree = await call('propose_changes', { reason: 'x', newRows: [{ sheet: SHEET, values: { Insectary_ID: 'Q1Q.2', Sex: 'male' } }] });
    assert.match(notFree.error, /Q1Q is not in Insectary_data/);
    const out = await call('propose_changes', {
      reason: 'Dos mariposas con W2B',
      changes: [{ recordId: w2b.id, values: { Insectary_ID: 'w2b.1' }, note: 'first one' }],
      newRows: [{ sheet: SHEET, values: { Insectary_ID: 'W2B.2', Sex: 'female' }, note: 'second one' }],
    });
    assert.ok(out.proposalId, JSON.stringify(out));
    const applied = await call('apply_proposal', { proposalId: out.proposalId });
    assert.equal(applied.status, 'applied', JSON.stringify(applied));
    assert.deepEqual([4, 5, 6].map(idOf), ['W2B.1', 'W2B.2', 'W3B']);
    assert.equal(at(5).values.Sex, 'female');
  } finally {
    store.close();
  }
});

test('match_notebook: a suffixed ID is its own key, and a death line with all its implied cells goes into the proposal', async () => {
  const { store, at, call } = await assistantFixture();
  try {
    await store.applyProposal([{ create: true, sheet: SHEET, clientId: 'c', values: { Insectary_ID: 'W2B.1', Sex: 'male', 'CLUTCH NUMBER': 901 } }], {
      user,
      requestId: randomUUID(),
    });
    const out = await call('match_notebook', {
      kind: 'deaths',
      year: 2026,
      lines: [
        { raw: 'W2B.1 ♂ 20/9 preserved CAM079555 FS50850001 ethanol', values: { Insectary_ID: 'W2B.1', Sex: 'male', Death_date: '20/9', CAM_ID: 'CAM079555', Tube_1_id: 'FS50850001', Notes_Insectary_data: 'preserved ethanol' } },
        { raw: 'W3B unk 21/9', values: { Insectary_ID: 'W3B', Death_date: '21/9', Death_cause: 'Unknown' } },
      ],
    });
    assert.ok(out.proposalId, JSON.stringify(out));
    const [dup, plain] = out.lines;
    assert.equal(dup.status, 'match');
    assert.equal(dup.row, 5, 'W2B.1 is its own row, not W2B');
    assert.ok(!dup.rowError && !plain.rowError, JSON.stringify(out.lines));
    const proposal = await call('get_proposal', { proposalId: out.proposalId, full: true });
    assert.equal(proposal.rows.length, 2);
    const death = proposal.rows.find(r => r.label === 'W3B');
    assert.ok(Object.keys(death.values).length >= 18, JSON.stringify(death));
    assert.equal(death.values.Tube_4_tissue, 'NOT_COLLECTED');
    assert.equal(at(5).values.Insectary_ID, 'W2B.1');
  } finally {
    store.close();
  }
});

test('a proposal row for an existing record takes as many values as a new row (a death with its whole template)', async () => {
  const { store, at, call } = await assistantFixture();
  try {
    const values = {
      Sex: 'male',
      Intro2Insectary_date: '2026-08-30',
      Death_date: '2026-09-21',
      Death_cause: 'Killed_Preserved',
      CAM_ID: 'CAM079556',
      Tube_1_id: 'FS50850002',
      Notes_Insectary_data: 'preserved after mating',
      Research_purpose: 'NA',
      Preservation_date: '2026-09-21',
      Preserved_Dead_Alive: 'Alive',
      Location_body: 'Ikiam',
      Tube_1_tissue: 'WHOLE_ORGANISM',
      T1_Preservation_medium: 'Ethanol',
      Tube_2_id: 'NA',
      Tube_2_tissue: 'NOT_COLLECTED',
      T2_Preservation_medium: 'NOT_COLLECTED',
      Tube_3_id: 'NA',
      Tube_3_tissue: 'NOT_COLLECTED',
      Tube_4_id: 'NA',
      Tube_4_tissue: 'NOT_COLLECTED',
      Preservation_medium: 'Ethanol',
      Stock_of_origin: 'NA',
    };
    assert.ok(Object.keys(values).length > 20);
    const out = await call('propose_changes', { reason: 'Muertes', changes: [{ recordId: at(5).id, values }] });
    assert.ok(out.proposalId, JSON.stringify(out));
    assert.equal(out.rows, 1);
  } finally {
    store.close();
  }
});

test('a pending proposal on the rows below an inserted W2B.2 still maps to them: no edit in the sheet seen, nothing logged, applied where they went', async () => {
  const { store, at, idOf, call, assistant } = await assistantFixture();
  try {
    const [w3b, w4b] = [at(5), at(6)];
    const pending = await call('propose_changes', {
      reason: 'Sexos',
      changes: [
        { recordId: w3b.id, values: { Sex: 'female' } },
        { recordId: w4b.id, values: { Sex: 'male' } },
      ],
    });
    assert.ok(pending.proposalId, JSON.stringify(pending));
    const dup = await call('propose_changes', {
      reason: 'Dos mariposas con W2B',
      changes: [{ recordId: at(4).id, values: { Insectary_ID: 'W2B.1' } }],
      newRows: [{ sheet: SHEET, values: { Insectary_ID: 'W2B.2', Sex: 'female' } }],
    });
    assert.equal((await call('apply_proposal', { proposalId: dup.proposalId })).status, 'applied');
    assert.deepEqual([5, 6, 7].map(idOf), ['W2B.2', 'W3B', 'W4B']);
    // A sheet read after the insert sees the rows where the local copy put them: nothing to log.
    await store.sync({ sheets: [SHEET] });
    const logged = store.db
      .prepare("SELECT count(*) n FROM changes c JOIN actions a ON a.id=c.action_id WHERE a.source='sheet_reconciliation' AND c.record_id IN (?,?)")
      .get(w3b.id, w4b.id).n;
    assert.equal(logged, 0);
    const shown = (
      await assistant.handle({
        method: 'GET',
        path: '/api/chat/proposals',
        body: {},
        user: { ...user, id: 'ed-1' },
        query: { all: '1', only: pending.proposalId },
      })
    ).body.proposals[0].changes;
    assert.deepEqual(
      shown.map(c => [c.row, c.label, !!c.sheetChanged]),
      [
        [6, 'W3B', false],
        [7, 'W4B', false],
      ],
    );
    const applied = await call('apply_proposal', { proposalId: pending.proposalId });
    assert.equal(applied.status, 'applied', JSON.stringify(applied));
    assert.deepEqual([at(6).values.Sex, at(7).values.Sex], ['female', 'male']);
    assert.equal(at(6).id, w3b.id);
  } finally {
    store.close();
  }
});
