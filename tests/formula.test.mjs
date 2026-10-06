// The evaluator of the workbook's formulas (server/formula.mjs) and what a proposal's
// formula cells will give once applied (server/formula-gives.mjs, formulaGives in the
// «Cambios propuestos» table): never written, shown in their own colour.
import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { evaluateFormula, parseFormula, relativeFormula, Unsupported, FormulaError, shownResult } from '../server/formula.mjs';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { moduleMap } from '../server/schema.mjs';
import { createAssistant } from '../server/assistant.mjs';

// The Insectary_data formulas as the sheet has them (row 13396).
const F = {
  SPECIES: r => `=IFS(C${r}="","",C${r}="NA","",OR(C${r}<>"",C${r}<>"NA"),XLOOKUP(C${r},Insectary_stocks!A:A,Insectary_stocks!C:C,""))`,
  Collection_location: r =>
    `=IFS(B${r}="","",B${r}="NA","NOT_COLLECTED",B${r}="Reared","Mariposario Ikiam",B${r}="Wild-caught",XLOOKUP(A${r},Collection_data!D:D,Collection_data!Z:Z,"ERROR!"))`,
  Pedigree: r =>
    `=IFS(K${r}="","",K${r}="NA","NA",K${r}="F1/F2 mutation rate","YES or NO",K${r}="WEST x EAST polymnia crosses","YES or NO",K${r}="polymnia x lysimnia crosses","YES or NO",TRUE,"NA")`,
  T2_Preservation_medium: r => `=IFS(U${r}="","",U${r}="NA","NA")`,
  CAM_ID_CollData: r => `=XLOOKUP(A${r},Collection_data!D:D,Collection_data!E:E,"NA")`,
  Photo_dorsal: r =>
    `=IF(P${r}="", "", IF(P${r}="NA", "NA", IF(ISERROR(MATCH(P${r} & "d" & ".JPG", Photo_links!B:B)), "NOT FOUND", HYPERLINK(INDEX(Photo_links!E:E, MATCH(P${r} & "d" & ".JPG", Photo_links!B:B, 0)), P${r} & "d"))))\n`,
  Tube_1_rack: r =>
    `=IFS(Q${r}="","",Q${r}="NA","NA",OR(Q${r}<>"",Q${r}<>"NA"),XLOOKUP(Q${r},Ithomiini_tube_locations_18Jun26!$B:$B,Ithomiini_tube_locations_18Jun26!$C:$C,"Not in TOL704"))`,
  COLLECTOR_SAMPLE_ID: r => `=IFS(O${r}="NA",P${r},P${r}="NA",O${r},OR(O${r}="",P${r}=""),"")`,
  DATE_OF_COLLECTION: r => `=M${r}`,
};
const letter = c => String.fromCharCode(65 + c);

/** A context over plain objects: { sheet: { 'A2': value } }, the formula's own sheet as ''. */
function contextOf(cells) {
  const at = (sheet, col, row) => cells[sheet ?? '']?.[`${letter(col)}${row}`] ?? null;
  const rows = (sheet, col) =>
    Object.keys(cells[sheet ?? ''] ?? {})
      .filter(k => k.startsWith(letter(col)) && /^\d+$/.test(k.slice(1)))
      .map(k => Number(k.slice(1)))
      .sort((a, b) => a - b);
  const same = (a, b) => (typeof a === 'string' && typeof b === 'string' ? a.toLowerCase() === b.toLowerCase() : a === b);
  return {
    cell: at,
    find: (sheet, col, value, r1, r2) => rows(sheet, col).find(r => r >= r1 && r <= r2 && same(at(sheet, col, r), value)) ?? null,
    count: (sheet, col, value, r1, r2) => rows(sheet, col).filter(r => r >= r1 && r <= r2 && same(at(sheet, col, r), value)).length,
  };
}

test('the formula shapes of Insectary_data: a Reared preserved larva', () => {
  const ctx = contextOf({
    '': { A9: 'N4D', B9: 'Reared', C9: '994(6)', K9: 'F1/F2 mutation rate', M9: 46289, O9: 'NA', P9: 'CAM078282', Q9: 'FS50849028', U9: 'NOT_COLLECTED' },
    Insectary_stocks: { A1: 'CLUTCH NUMBER', A5: 838, C5: 'Mechanitis messenoides intermedia', A6: '994(6)', C6: 'Mechanitis lysimnia' },
    Collection_data: { D1: 'Insectary_ID', D2: 'A7E', E2: 'CAM079001', Z2: 'Cavernas' },
    Ithomiini_tube_locations_18Jun26: { B2: 'FS1', C2: 'Rack 3' },
  });
  const value = field => shownResult(evaluateFormula(F[field](9), 9, ctx));
  assert.equal(value('SPECIES'), 'Mechanitis lysimnia');
  assert.equal(value('Collection_location'), 'Mariposario Ikiam');
  assert.equal(value('Pedigree'), 'YES or NO');
  // An IFS without a matching branch: #N/A, as the sheet shows it.
  assert.equal(value('T2_Preservation_medium'), '#N/A');
  assert.equal(value('CAM_ID_CollData'), 'NA');
  assert.equal(value('Tube_1_rack'), 'Not in TOL704');
  assert.equal(value('COLLECTOR_SAMPLE_ID'), 'CAM078282');
  assert.equal(value('DATE_OF_COLLECTION'), 46289);
  // MATCH without exact match (Photo_dorsal) is not evaluated: the sheet's value stays.
  assert.throws(() => evaluateFormula(F.Photo_dorsal(9), 9, ctx), Unsupported);
});

test('lookups, text functions, errors and references', () => {
  const ctx = contextOf({
    '': { A9: 'N4D', A8: 'A9C', B9: 'Wild-caught', C9: 838, U9: 'NA', P9: 'CAM1' },
    Insectary_stocks: { A5: 838, C5: 'Mechanitis messenoides intermedia' },
    Collection_data: { D2: 'n4d', Z2: 'Cavernas', E2: 'CAM079001' },
    Photo_links: { B3: 'CAM1d.JPG', E3: 'https://drive/x' },
    Lists: { C2: 'PAS - Patricio Salazar', D2: 'SANGER' },
  });
  const ev = (f, row = 9) => shownResult(evaluateFormula(f, row, ctx));
  // Text lookups ignore case; a number matches only a number.
  assert.equal(ev(F.Collection_location(9)), 'Cavernas');
  assert.equal(ev(F.SPECIES(9)), 'Mechanitis messenoides intermedia');
  assert.equal(ev('=XLOOKUP("838",Insectary_stocks!A:A,Insectary_stocks!C:C,"none")'), 'none');
  assert.equal(ev(F.T2_Preservation_medium(9)), 'NA');
  assert.equal(ev('=HYPERLINK(INDEX(Photo_links!E:E,MATCH(P9&"d"&".JPG",Photo_links!B:B,0)),P9&"d")'), 'CAM1d');
  assert.equal(ev('=IFERROR(INDEX(Photo_links!E:E,MATCH("x",Photo_links!B:B,0)),"Not Found")'), 'Not Found');
  assert.equal(ev('=ISERROR(XLOOKUP("x",Lists!$C:$C,Lists!$D:$D))'), true);
  assert.equal(ev('=IFNA(XLOOKUP("x",Lists!$C$1:$C$10,Lists!$D$1:$D$10),"NA")'), 'NA');
  assert.equal(ev('=UPPER(RIGHT(Lists!C2,LEN(Lists!C2)-FIND(" ",Lists!C2)-2))'), 'PATRICIO SALAZAR');
  // The pre-made rows' Insectary ID formula: the row above's ID, the next in the series.
  assert.equal(ev('=IF(MID(A8,2,1)="9", CHAR(CODE(LEFT(A8,1))+1) & "0C", LEFT(A8,1) & (MID(A8,2,1)+1) & "C")'), 'B0C');
  assert.equal(ev('=IF(COUNTIF(Collection_data!D:D, A9) = 0, "NA", COUNTIF(Collection_data!D:D, A9))'), 1);
  assert.equal(ev('=IFS(P9="NA",#REF!,TRUE,"x")'), 'x');
  assert.equal(ev('=IFS(P9="NA","a",#REF!="NA","b")'), '#REF!');
  assert.equal(ev('=DATE(2026,10,5)'), 46300);
  assert.equal(ev('=1+2*3-4/2&"!"'), '5!');
  assert.equal(ev('=AND(B9<>"",NOT(B9="NA"))'), true);
  assert.equal(ev('=Z9=""'), true, 'a blank cell equals ""');
  // TEXT of a date (Insectary_stocks' Earliest Emerge Date); a number format is not worked out.
  assert.equal(ev('=TEXT(DATE(2026,10,5),"dd-mmm-yy")'), '05-Oct-26');
  assert.equal(ev('=TEXT(A9,"dd")'), 'N4D', 'TEXT of a text is the text');
  assert.throws(() => evaluateFormula('=TEXT(5,"0.00")', 9, ctx), Unsupported);
  // VLOOKUP (exact), ROW, XLOOKUP with its default modes written out, CONCAT, ROUND, SEARCH, REGEXMATCH.
  assert.equal(ev('=VLOOKUP("CAM1d.JPG",Photo_links!B:E,4,FALSE)'), 'https://drive/x');
  assert.equal(ev('=VLOOKUP("none",Photo_links!B:E,4,FALSE)'), '#N/A');
  assert.throws(() => evaluateFormula('=VLOOKUP("x",Photo_links!B:E,4,TRUE)', 9, ctx), Unsupported);
  assert.equal(ev('=ROW()'), 9);
  assert.equal(ev('=ROW(A3)'), 3);
  assert.equal(ev('=XLOOKUP(A9,Collection_data!D:D,Collection_data!Z:Z,"",0,1)'), 'Cavernas');
  assert.throws(() => evaluateFormula('=XLOOKUP(A9,Collection_data!D:D,Collection_data!Z:Z,"",0,-1)', 9, ctx), Unsupported);
  assert.equal(ev('=CONCAT(A9,"x")'), 'N4Dx');
  assert.equal(ev('=ROUND(2.345,2)'), 2.35);
  assert.equal(ev('=SEARCH("d",A9)'), 3);
  assert.equal(ev('=REGEXMATCH(P9,"^CAM\\d")'), true);
  assert.throws(() => evaluateFormula('=IF(A9,', 9, ctx), Unsupported);
  assert.ok(evaluateFormula('=1/0', 9, ctx) instanceof FormulaError);
});

test('a formula is parsed once for every row (relative to its own row)', () => {
  assert.equal(relativeFormula(F.T2_Preservation_medium(13001), 13001), '=IFS(U{0}="","",U{0}="NA","NA")');
  assert.equal(relativeFormula(F.T2_Preservation_medium(13001), 13001), relativeFormula(F.T2_Preservation_medium(20846), 20846));
  // Absolute rows, other sheets' names and text stay.
  assert.equal(relativeFormula('=XLOOKUP($R8,Lists!$AG$1:$AG$10,Taxonomy_v18Jun25!J:J,"F1")', 8), '=XLOOKUP($R{0},Lists!$AG$1:$AG$10,Taxonomy_v18Jun25!J:J,"F1")');
  assert.ok(parseFormula(relativeFormula(F.Photo_dorsal(5), 5)));
});

test('a proposal shows what its formula cells will give: the proposed values read, never written', async () => {
  const fields = moduleMap.get('Insectary_data').fields;
  const column = key => fields.find(f => f.key === key).column;
  const sheets = new LocalSheets({
    Insectary_stocks: [{ row: 2, values: { 'CLUTCH NUMBER': '994(6)', SPECIES: 'Mechanitis lysimnia' } }],
    Collection_data: [{ row: 2, values: { Insectary_ID: 'A7E', CAM_ID: 'CAM079001' } }],
    Insectary_data: [
      { row: 2, values: { Insectary_ID: 'N3D', Wild_Reared: 'Reared', Sex: 'male' } },
      { row: 3, values: { Insectary_ID: 'N4D' } },
    ],
  });
  for (const row of [2, 3]) {
    const r = sheets.rows.get('Insectary_data').find(x => x.row === row);
    for (const [key, f] of Object.entries(F)) {
      if (!fields.some(x => x.key === key)) continue;
      r.cells[column(key)] = { userEnteredValue: { formulaValue: f(row) }, ...(row === 2 && key === 'Collection_location' ? { effectiveValue: { stringValue: 'Mariposario Ikiam' } } : {}) };
    }
  }
  const store = new Store({ localMode: true }, { sheets });
  try {
    await store.sync({ sheets: ['Insectary_stocks', 'Collection_data', 'Insectary_data'] });
    const assistant = createAssistant({ store, config: {} });
    store.db
      .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u1','franz','Franz','editor','s','h',1,'2026-01-01')")
      .run();
    store.db
      .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
      .run(createHash('sha256').update('franz-token').digest('hex'), 'u1');
    const at = row => store.getRecordBySheetRow('Insectary_data', row).id;
    const call = async (name, args) =>
      JSON.parse(
        (await assistant.mcp({ authorization: 'Bearer franz-token' }, { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } }))
          .body.result.content[0].text,
      );
    const out = await call('propose_changes', {
      reason: 'Larva preservada',
      changes: [
        {
          recordId: at(3),
          values: {
            Wild_Reared: 'Reared',
            'CLUTCH NUMBER': '994(6)',
            Research_purpose: 'F1/F2 mutation rate',
            LIFESTAGE: '3rd instar larva',
            CAM_ID: 'CAM078282',
            Tube_1_id: 'FS50849028',
            Tube_2_tissue: 'NOT_COLLECTED',
          },
        },
        // Only its sex changes: its formulas do not read it, nothing to show.
        { recordId: at(2), values: { Sex: 'female' } },
      ],
    });
    assert.ok(out.proposalId, JSON.stringify(out));
    const user = { id: 'u1', username: 'franz', displayName: 'Franz', role: 'editor' };
    const p = (await assistant.handle({ method: 'GET', path: '/api/chat/proposals', body: {}, user, query: { all: '1' } })).body.proposals[0];
    const larva = p.changes.find(c => c.label === "N4D");
    const other = p.changes.find(c => c.label === "N3D");
    assert.deepEqual(larva.formulaGives, {
      SPECIES: 'Mechanitis lysimnia',
      Collection_location: 'Mariposario Ikiam',
      Pedigree: 'YES or NO',
      T2_Preservation_medium: '#N/A',
      COLLECTOR_SAMPLE_ID: 'CAM078282',
      Tube_1_rack: 'Not in TOL704',
    });
    // CAM_ID_CollData reads only the ID (unchanged); Photo_dorsal reads CAM_ID but cannot be evaluated.
    assert.deepEqual(larva.formulaFallback, ['Photo_dorsal']);
    assert.equal(other.formulaGives, undefined);
    // Never written: not among the row's values.
    for (const f of Object.keys(larva.formulaGives)) assert.ok(!(f in larva.values), f);
  } finally {
    store.close();
  }
});
