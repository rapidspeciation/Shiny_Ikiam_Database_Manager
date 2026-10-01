// The formulas the team copies down a column: found in the last year's rows
// of each kind, listed where they are missing (Revisión → Sugerencias), and
// written by the app on the rows it creates.
import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash, randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets, relativeFormula, shiftFormula } from '../server/sheets.mjs';
import { moduleMap } from '../server/schema.mjs';
import { applyBatch } from '../server/batch.mjs';
import { detectPatterns, formulaAt, newRowFormulas, windowRows } from '../server/formula-patterns.mjs';
import { evaluateFormula } from '../server/formula-eval.mjs';
import { allSuggestions } from '../server/suggestions/index.mjs';
import { undoEdits } from '../server/history.mjs';
import { createAssistant } from '../server/assistant.mjs';

const EPOCH = Date.UTC(1899, 11, 30);
const today = Math.round(
  (Date.parse(`${new Intl.DateTimeFormat('en-CA', { timeZone: 'America/Guayaquil' }).format(new Date())}T00:00:00Z`) -
    EPOCH) /
    864e5,
);
const col = (sheet, key) => moduleMap.get(sheet).fields.find(f => f.key === key).column;
const user = { id: 'editor-1', username: 'editor', role: 'editor' };

const SENT = 'Collected_Sent2Insectary';
const order = r => `=IF(B${r}="","",A${r - 1}+1)`;
const LOOKUPS = { Death_date: 'I', Preservation_date: 'M', Preservation_medium: 'AA', Preserved_dead_alive: 'AB' };
const lookup = (r, letter) => `=XLOOKUP(D${r}, Insectary_data!A:A, Insectary_data!${letter}:${letter},"")`;
const id = i => `${String.fromCharCode(65 + Math.floor(i / 10))}${i % 10}T`;

/** A book over a LocalSheets, so formulas written in tests give what Google would show. */
function localBook(sheets) {
  const effective = cell => {
    const v = cell?.userEnteredValue?.formulaValue === undefined ? cell?.userEnteredValue : cell?.effectiveValue;
    return v?.stringValue ?? v?.numberValue ?? v?.boolValue ?? null;
  };
  return {
    has: sheet => sheets.rows.has(sheet),
    lastRow: sheet => sheets.rowCount(sheet),
    cell: (sheet, row, column) => {
      const value = effective(sheets.cell(sheet, row, column));
      return value === '' ? null : value;
    },
  };
}

/**
 * Collection_data as the real one: 30 wild butterflies sent to the insectary
 * with Data_entry_order and the four lookups (rows 2–31), 10 preserved ones
 * with typed dates (32–41), then rows typed without them (42–49) and three
 * pre-made rows (50–52). Insectary_data holds their twins.
 */
function workbook() {
  const insectary = [];
  const collection = [];
  const cells = (sheet, values, formulas = {}) => {
    const out = [];
    for (const [key, value] of Object.entries(values))
      out[col(sheet, key)] = {
        userEnteredValue: typeof value === 'number' ? { numberValue: value } : { stringValue: value },
      };
    for (const [key, formula] of Object.entries(formulas))
      out[col(sheet, key)] = { userEnteredValue: { formulaValue: formula } };
    return out;
  };
  const twin = (i, extra = {}) =>
    insectary.push({
      row: 2 + insectary.length,
      cells: cells('Insectary_data', {
        Insectary_ID: id(i),
        Wild_Reared: 'Wild-caught',
        SPECIES: 'Oleria onega',
        Intro2Insectary_date: today - 40,
        ...extra,
      }),
    });
  const base = (i, kind) => ({
    Release_Collect: kind,
    Insectary_ID: kind === SENT ? id(i) : 'NA',
    SPECIES: 'Oleria onega',
    Sex: 'male',
    Collection_date: today - 40 + i,
  });
  for (let i = 0; i < 30; i++) {
    const r = 2 + i;
    twin(
      i,
      i % 2
        ? { Death_date: today - 5, Preservation_medium: 'Flash frozen', CAM_ID: `CAM0790${String(i).padStart(2, '0')}` }
        : {},
    );
    collection.push({
      row: r,
      // The first row's number is typed (the formula reads the header above it).
      cells: cells(
        'Collection_data',
        { ...base(i, SENT), ...(r === 2 ? { Data_entry_order: 1 } : {}) },
        {
          ...(r === 2 ? {} : { Data_entry_order: order(r) }),
          ...Object.fromEntries(Object.entries(LOOKUPS).map(([f, l]) => [f, lookup(r, l)])),
        },
      ),
    });
  }
  for (let i = 30; i < 40; i++) {
    const r = 2 + i;
    collection.push({
      row: r,
      cells: cells(
        'Collection_data',
        {
          ...base(i, 'Collected_Preserved'),
          CAM_ID_insectary: 'NA',
          Death_date: today - 10,
          Preservation_medium: 'Flash frozen',
        },
        { Data_entry_order: order(r) },
      ),
    });
  }
  // Rows typed later without the formulas.
  twin(42); // alive
  twin(43, { Death_date: today - 2, CAM_ID: 'CAM079043', Preservation_medium: 'Flash frozen' }); // preserved
  twin(44, { Death_date: today - 2, CAM_ID: 'NA', Preservation_medium: 'NOT_PRESERVED' }); // died, not preserved
  twin(45, { CAM_ID: 'CAM079045' });
  twin(47, { Death_date: today - 3 });
  const late = [
    [42, {}],
    [43, { CAM_ID_insectary: 'NA' }],
    [44, { CAM_ID_insectary: 'NA' }],
    [45, { CAM_ID_insectary: 'CAM079999' }],
    [46, { Insectary_ID: 'NA' }],
    [47, { Death_date: today - 1 }],
  ];
  for (const [r, extra] of late)
    collection.push({
      row: r,
      // Row 45 got its order number by hand in the middle of the gap.
      cells: cells('Collection_data', { ...base(r, SENT), ...extra }, r === 45 ? { Data_entry_order: order(r) } : {}),
    });
  for (const r of [48, 49])
    collection.push({
      row: r,
      cells: cells('Collection_data', { ...base(r, 'Collected_Preserved'), CAM_ID_insectary: 'NA' }),
    });
  // Pre-made rows: the formulas every row has, none of the kind's lookups.
  for (const r of [50, 51, 52])
    collection.push({
      row: r,
      cells: cells(
        'Collection_data',
        {},
        { Genus: `=IF($S${r}="","",XLOOKUP($S${r},Taxonomy_v18Jun25!$J:$J,Taxonomy_v18Jun25!H:H,"NOT_FOUND"))` },
      ),
    });
  return { Insectary_data: insectary, Collection_data: collection };
}

async function fixture() {
  let book;
  const evaluate = (formula, { sheet, row }) => evaluateFormula(formula, { sheet, row, book });
  const sheets = new LocalSheets(workbook(), { evaluate });
  book = localBook(sheets);
  // The formulas' values, as Google computes them.
  for (const sheet of ['Insectary_data', 'Collection_data'])
    for (const r of sheets.rows.get(sheet))
      r.cells.forEach((cell, column) => {
        const formula = cell?.userEnteredValue?.formulaValue;
        if (!formula) return;
        const value = evaluate(formula, { sheet, row: r.row });
        if (value !== undefined && value !== null && value !== '' && typeof value !== 'object')
          cell.effectiveValue = typeof value === 'number' ? { numberValue: value } : { stringValue: String(value) };
      });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data', 'Collection_data'] });
  return { store, sheets };
}

test('formulas are compared in relative form and moved row by row as Sheets does', () => {
  assert.equal(relativeFormula('=IF(B3000="","",A2999+1)', 3000), '=IF(B{+0}="","",A{-1}+1)');
  assert.equal(relativeFormula(order(10), 10), relativeFormula(order(7856), 7856));
  // Absolute rows, whole columns and text stay.
  assert.equal(
    relativeFormula('=VLOOKUP(A5,Insectary_stocks!$A$1:$C$999,2,FALSE)&"B12"', 5),
    '=VLOOKUP(A{+0},Insectary_stocks!$A$1:$C$999,2,FALSE)&"B12"',
  );
  assert.equal(shiftFormula(lookup(3667, 'P'), 4570), lookup(8237, 'P'));
});

test('the evaluator works out the lookups, conditions and text the team uses, and says when it cannot', () => {
  const cells = {
    Insectary_data: {
      1: ['Insectary_ID', 'CLUTCH NUMBER', 'Intro2Insectary_date', 'CAM_ID'],
      2: ['A0T', 994, 46200, 'CAM079001'],
      3: ['A1T', 994, 46210, null],
      4: ['NA', null, null, 'NA'],
    },
    Collection_data: { 5: [null, 'Collected_Sent2Insectary', null, 'A1T'], 6: [7, 'Collected_Preserved', null, 'A0T'] },
  };
  const book = {
    has: sheet => sheet in cells,
    lastRow: sheet => Math.max(...Object.keys(cells[sheet]).map(Number)),
    cell: (sheet, row, column) => cells[sheet][row]?.[column] ?? null,
  };
  const on = (formula, row = 5) => evaluateFormula(formula, { sheet: 'Collection_data', row, book });
  // A butterfly still alive: its CAM is blank, and so is the lookup (not "Missing", not 0).
  assert.equal(on('=XLOOKUP(D5,Insectary_data!A:A,Insectary_data!D:D,"Missing")'), null);
  assert.equal(on('=XLOOKUP(D6,Insectary_data!A:A,Insectary_data!D:D,"Missing")', 6), 'CAM079001');
  assert.equal(on('=XLOOKUP("Z9Z",Insectary_data!A:A,Insectary_data!D:D,"Missing")'), 'Missing');
  assert.deepEqual(on('=XLOOKUP("Z9Z",Insectary_data!A:A,Insectary_data!D:D)'), { error: '#N/A' });
  assert.equal(on('=IF(B6="","",A5+1)', 6), 1);
  assert.equal(on('=IF(B5="","",A6+1)'), 8);
  assert.equal(on('=IFS(B5="","",B5="NA","NOT_COLLECTED",TRUE,"x")'), 'x');
  assert.equal(on('=IF(COUNTIF(Insectary_data!B:B, 994) = 0, "NA", COUNTIF(Insectary_data!B:B, 994))'), 2);
  assert.equal(
    on('=IFNA(TEXT(INDEX(Insectary_data!C:C, MATCH(994, Insectary_data!B:B, 0)), "dd-mmm-yy"), "NA")'),
    '27-Jun-26',
  );
  assert.equal(
    on('=IFNA(TEXT(INDEX(Insectary_data!C:C, MATCH(995, Insectary_data!B:B, 0)), "dd-mmm-yy"), "NA")'),
    'NA',
  );
  assert.equal(on('=if(B5="NA","NA",(left(B5,1)&". "))'), 'C. ');
  assert.equal(on('=VLOOKUP(D5, Insectary_data!A:B, 2, FALSE)'), 994);
  // Not worked out: an approximate VLOOKUP, an unknown function, a sheet the app does not have.
  assert.equal(on('=VLOOKUP(D5, Insectary_data!A:B, 2)'), undefined);
  assert.equal(on('=FILTER(Insectary_data!A:A, Insectary_data!B:B=994)'), undefined);
  assert.equal(on('=XLOOKUP(D5,Filter_Coll_data!A:A,Filter_Coll_data!B:B,"")'), undefined);
});

test('a pattern is the formula most rows of a kind had in the last year; typed columns and kinds are not', () => {
  const rows = [];
  const cell = (r, values, formulas = {}) =>
    rows.push({ row: r, values: { Collection_date: today - 50, ...values }, formulas });
  for (let r = 2; r < 32; r++)
    cell(r, { Release_Collect: SENT, Insectary_ID: id(r) }, { Data_entry_order: order(r), Death_date: lookup(r, 'I') });
  for (let r = 32; r < 52; r++)
    cell(r, { Release_Collect: 'Collected_Preserved', Death_date: today - 50 }, { Data_entry_order: order(r) });
  for (let r = 52; r < 60; r++) cell(r, { Release_Collect: SENT, Insectary_ID: id(r) });
  // A few Mark_Released rows, only in the gap: they do not veto Data_entry_order for every row.
  for (let r = 60; r < 72; r++) cell(r, { Release_Collect: 'Mark_Released', Death_date: 'NA' });
  const patterns = detectPatterns('Collection_data', rows, { today });
  const find = (field, kind) => patterns.find(p => p.field === field && p.kind === kind);
  const entry = find('Data_entry_order', null);
  assert.ok(entry && entry.universal);
  assert.equal(formulaAt(entry, 70), order(70));
  const death = find('Death_date', SENT);
  assert.deepEqual([death.counts.formula, death.counts.blank, death.universal, death.example.row], [30, 8, true, 31]);
  // Preserved rows type their dates: no pattern for them, nor one for every row.
  assert.equal(find('Death_date', 'Collected_Preserved'), undefined);
  assert.equal(find('Death_date', null), undefined);
  // CAM_ID_insectary is the rule for rows sent to the insectary, with the team's formula.
  const cam = find('CAM_ID_insectary', SENT);
  assert.ok(cam.explicit);
  assert.equal(formulaAt(cam, 8237), '=XLOOKUP(D8237,Insectary_data!A:A,Insectary_data!P:P,"")');

  // Rows older than a year are history: a formula used then and typed over since is no pattern.
  const old = rows.map(r => ({
    ...r,
    values: { ...r.values, Collection_date: r.row < 52 ? today - 900 : today - 10 },
  }));
  assert.equal(windowRows('Collection_data', old, today)[0].row, 52);
  assert.equal(
    detectPatterns('Collection_data', old, { today }).find(p => p.field === 'Death_date'),
    undefined,
  );

  // Insectary_data: SPECIES (typed over when what emerged differs) and Pedigree are never patterns.
  const insectary = Array.from({ length: 40 }, (_, i) => ({
    row: 2 + i,
    values: { Intro2Insectary_date: today - 20, Wild_Reared: 'Reared' },
    formulas: i < 30 ? { SPECIES: `=IFS(C${2 + i}="","",TRUE,"x")`, Pedigree: `=IFS(K${2 + i}="","",TRUE,"NA")` } : {},
  }));
  assert.deepEqual(detectPatterns('Insectary_data', insectary, { today }), []);
});

test('Sugerencias lists the cells lacking the formula, with what it gives today and how sure it is', async () => {
  const { store } = await fixture();
  const { items } = await allSuggestions(store);
  const mine = items.filter(s => s.source === 'formulas');
  const at = (row, field) => mine.find(s => s.row === row && s.field === field);
  for (const s of mine) assert.equal(s.manual, true);

  // Blank cells of a near-universal column: certain, with the formula for that row.
  const death = at(43, 'Death_date');
  assert.equal(death.suggested, lookup(43, 'I'));
  assert.equal(death.certainty, 'certain');
  assert.equal(death.group, `Collection_data · Death_date · ${SENT}`);
  assert.match(
    death.reason,
    /^30 de 36 filas Collected_Sent2Insectary del último año tienen esta fórmula \(la última, fila 31\); hoy daría \d{4}-\d\d-\d\d/,
  );
  assert.match(at(42, 'Death_date').reason, /hoy daría vacío/);
  // A typed date that differs from the insectary's is someone's decision: left alone.
  assert.equal(at(47, 'Death_date'), undefined);
  // No lookups by an Insectary_ID "NA" (it would find another row's NA), but its order number.
  assert.equal(at(46, 'Death_date'), undefined);
  assert.equal(at(46, 'CAM_ID_insectary'), undefined);
  // Preserved rows type their dates: nothing for them.
  assert.equal(at(48, 'Death_date'), undefined);

  // CAM_ID_insectary: blank while alive (certain); a typed NA where the butterfly has a CAM (likely);
  // a typed NA where it died unpreserved (the formula gives NA too: certain); another CAM typed: left alone.
  assert.deepEqual([at(42, 'CAM_ID_insectary').certainty, at(42, 'CAM_ID_insectary').current], ['certain', null]);
  assert.equal(at(42, 'CAM_ID_insectary').suggested, '=XLOOKUP(D42,Insectary_data!A:A,Insectary_data!P:P,"")');
  assert.deepEqual([at(43, 'CAM_ID_insectary').certainty, at(43, 'CAM_ID_insectary').current], ['likely', 'NA']);
  assert.match(at(43, 'CAM_ID_insectary').reason, /hoy daría CAM079043/);
  assert.equal(at(44, 'CAM_ID_insectary').certainty, 'certain');
  assert.equal(at(45, 'CAM_ID_insectary'), undefined);

  // Data_entry_order, for every kind, numbered on from the row above as filling it down would
  // (row 45, which has it, counted on from the rows above once they are filled).
  assert.match(at(42, 'Data_entry_order').reason, /hoy daría 41$/);
  assert.equal(at(45, 'Data_entry_order'), undefined);
  assert.match(at(46, 'Data_entry_order').reason, /hoy daría 45$/);
  assert.match(at(49, 'Data_entry_order').reason, /hoy daría 48$/);
  assert.equal(at(49, 'Data_entry_order').suggested, order(49));
  // Rows that have their formulas get nothing (but CAM_ID_insectary, the rule, which they lack).
  assert.ok(!mine.some(s => s.row < 42 && s.field !== 'CAM_ID_insectary'));
  assert.equal(at(3, 'CAM_ID_insectary').certainty, 'certain');
  store.close();
});

test('a row the app creates gets the formulas of its kind; typed values and other kinds are left alone', async () => {
  const { store, sheets } = await fixture();
  const formulaOf = (row, key) =>
    sheets.cell('Collection_data', row, col('Collection_data', key))?.userEnteredValue?.formulaValue;
  const valueOf = (row, key) => {
    const v = sheets.cell('Collection_data', row, col('Collection_data', key))?.userEnteredValue;
    return v?.stringValue ?? v?.numberValue ?? null;
  };
  assert.deepEqual(Object.keys(newRowFormulas(store, 'Collection_data', 50, { Release_Collect: SENT })).sort(), [
    'CAM_ID_insectary',
    'Data_entry_order',
    'Death_date',
    'Preservation_date',
    'Preservation_medium',
    'Preserved_dead_alive',
  ]);
  const result = await applyBatch(
    store,
    {
      requestId: randomUUID(),
      creates: [
        // Colecta's wild butterfly taken to the insectary (the assistant may type NA in CAM_ID_insectary).
        {
          module: 'Collection_data',
          clientId: 'sent',
          values: {
            ...{ Release_Collect: SENT, Insectary_ID: 'Y0T', SPECIES: 'Oleria onega', Sex: 'male' },
            CAM_ID_insectary: 'NA',
          },
        },
        // A preserved one: its NA stays, it gets only the order number.
        {
          module: 'Collection_data',
          clientId: 'kept',
          values: {
            Release_Collect: 'Collected_Preserved',
            Insectary_ID: 'NA',
            SPECIES: 'Oleria onega',
            Sex: 'male',
            CAM_ID_insectary: 'NA',
            CAM_ID: 'CAM078500',
          },
        },
        // A date typed by the person stays typed.
        {
          module: 'Collection_data',
          clientId: 'typed',
          values: {
            Release_Collect: SENT,
            Insectary_ID: 'Y1T',
            SPECIES: 'Oleria onega',
            Sex: 'male',
            Death_date: today - 1,
          },
        },
      ],
    },
    user,
  ).catch(e => assert.fail(JSON.stringify(e.details)));
  assert.equal(result.status, 'verified', JSON.stringify(result));
  const rows = Object.fromEntries(result.created.map(c => [c.clientId, store.getRecord(c.recordId).row]));
  assert.deepEqual(rows, { sent: 50, kept: 51, typed: 52 });
  assert.equal(formulaOf(50, 'Data_entry_order'), order(50));
  assert.equal(formulaOf(50, 'Death_date'), lookup(50, 'I'));
  assert.equal(formulaOf(50, 'Preserved_dead_alive'), lookup(50, 'AB'));
  assert.equal(formulaOf(50, 'CAM_ID_insectary'), '=XLOOKUP(D50,Insectary_data!A:A,Insectary_data!P:P,"")');
  // The pre-made row's own formula is still there.
  assert.match(formulaOf(50, 'Genus'), /^=IF\(\$S50=""/);
  // What the sheet shows: Y0T has no CAM yet (alive, its Insectary row not typed): the cell is blank.
  const sent = store.getRecord(result.created[0].recordId);
  assert.equal(sent.values.CAM_ID_insectary || null, null);
  assert.equal(sent.formulas.Death_date, lookup(50, 'I'));
  assert.equal(formulaOf(51, 'Data_entry_order'), order(51));
  assert.equal(formulaOf(51, 'Death_date'), undefined);
  assert.equal(valueOf(51, 'CAM_ID_insectary'), 'NA');
  assert.equal(valueOf(52, 'Death_date'), today - 1);
  assert.equal(formulaOf(52, 'Preservation_date'), lookup(52, 'M'));
  // History keeps the formula written, and undo takes it away with the row's values.
  const changes = store.db
    .prepare("SELECT field, after_json FROM changes WHERE row_num=50 AND field='CAM_ID_insectary'")
    .all();
  assert.deepEqual(JSON.parse(changes[0].after_json), {
    formula: '=XLOOKUP(D50,Insectary_data!A:A,Insectary_data!P:P,"")',
  });
  const undone = await undoEdits(store, { actionIds: [result.action.id], requestId: randomUUID() }, user);
  assert.equal(undone.status, 'verified');
  assert.equal(formulaOf(50, 'CAM_ID_insectary'), undefined);
  assert.equal(formulaOf(50, 'Death_date'), undefined);
  assert.equal(valueOf(50, 'Release_Collect'), null);
  assert.match(formulaOf(50, 'Genus'), /^=IF\(\$S50=""/);
  store.close();
});

test("the assistant's new rows leave a placeholder NA out of the kind's formula columns; the save writes the formula", async () => {
  const { store, sheets } = await fixture();
  try {
    const assistant = createAssistant({ store, config: {} });
    store.db
      .prepare(
        "INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u-ana','ana','Ana','editor','s','h',1,'2026-01-01')",
      )
      .run();
    store.db
      .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
      .run(createHash('sha256').update('ana-token').digest('hex'), 'u-ana');
    const call = async (name, args) =>
      JSON.parse(
        (
          await assistant.mcp(
            { authorization: 'Bearer ana-token' },
            { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } },
          )
        ).body.result.content[0].text,
      );
    const proposed = await call('propose_changes', {
      reason: 'Colecta',
      newRows: [
        {
          sheet: 'Collection_data',
          values: {
            Release_Collect: SENT,
            Insectary_ID: 'Y5T',
            SPECIES: 'Oleria onega',
            Sex: 'female',
            CAM_ID_insectary: 'NA',
            Death_date: 'NA',
          },
        },
      ],
    });
    assert.match(proposed.leftOut, /CAM_ID_insectary/, JSON.stringify(proposed));
    const applied = await call('apply_proposal', { proposalId: proposed.proposalId });
    assert.equal(applied.status, 'applied', JSON.stringify(applied));
    const at = column =>
      sheets.cell('Collection_data', 50, col('Collection_data', column))?.userEnteredValue?.formulaValue;
    assert.equal(at('CAM_ID_insectary'), '=XLOOKUP(D50,Insectary_data!A:A,Insectary_data!P:P,"")');
    assert.equal(at('Death_date'), lookup(50, 'I'));
  } finally {
    store.close();
  }
});
