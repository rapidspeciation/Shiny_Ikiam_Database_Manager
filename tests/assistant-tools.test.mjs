import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { moduleMap, parseDateText } from '../server/schema.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { findRecords } from '../server/records-tool.mjs';
import { noteText } from '../server/notebook.mjs';

// The assistant's tools after their first real use: what null means in a proposal, notes in the
// team's form, notebook pages in line order with context rows, and row queries that stay small.

const d = text => parseDateText(text);
const today = () => new Intl.DateTimeFormat('en-CA', { timeZone: 'America/Guayaquil' }).format(new Date());
/** A formula cell with the value Sheets computed for it. */
function formula(sheets, sheet, row, field, text, value) {
  const column = moduleMap.get(sheet).fields.find(f => f.key === field).column;
  sheets.rows.get(sheet).find(r => r.row === row).cells[column] = {
    userEnteredValue: { formulaValue: text },
    effectiveValue: typeof value === 'number' ? { numberValue: value } : { stringValue: value },
  };
}

async function setup(seed, formulas = []) {
  const sheets = new LocalSheets(seed);
  for (const f of formulas) formula(sheets, ...f);
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: Object.keys(seed) });
  const assistant = createAssistant({ store, config: {} });
  store.db
    .prepare(
      "INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u-franz','franz','Franz Chandi','editor','s','h',1,'2026-01-01')",
    )
    .run();
  store.db
    .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
    .run(createHash('sha256').update('franz-token').digest('hex'), 'u-franz');
  const mcp = (method, params) =>
    assistant.mcp({ authorization: 'Bearer franz-token' }, { jsonrpc: '2.0', id: 1, method, params });
  const call = async (name, args) => JSON.parse((await mcp('tools/call', { name, arguments: args })).body.result.content[0].text);
  const user = { id: 'u-franz', username: 'franz', displayName: 'Franz Chandi', role: 'editor' };
  const http = (method, path, body = {}, query = {}) => assistant.handle({ method, path, body, user, query });
  const list = async () => (await http('GET', '/api/chat/proposals', {}, { all: '1' })).body.proposals;
  const stock = row => store.getRecordBySheetRow('Insectary_stocks', row);
  return { store, sheets, mcp, call, http, list, stock };
}

const STOCKS = {
  Insectary_stocks: [
    { row: 2, values: { 'CLUTCH NUMBER': 900, SPECIES: 'Mechanitis lysimnia', 'HATCHING DATE': d('2026-09-05'), NOTES: '15/7/26 MJS: some eggs dry' } },
    { row: 3, values: { 'CLUTCH NUMBER': 901, SPECIES: 'Mechanitis lysimnia', 'INSECTARY OR LABORATORY': 'Insectary' } },
  ],
  // The person's initials come from the Collector list.
  Collection_data: [{ row: 2, values: { Collector: 'FCH - Franz Chandi', SPECIES: 'Oleria onega' } }],
};
const STOCK_FORMULAS = [['Insectary_stocks', 2, 'NUMBER OF EGGS', '=16+2-1', 17]];

test('null in a proposal is no change, only {clear:true} empties a cell; the table and get_proposal say so', async () => {
  const { store, mcp, call, list, stock } = await setup(STOCKS, STOCK_FORMULAS);
  try {
    const tools = (await mcp('tools/list')).body.result.tools;
    assert.match(tools.find(t => t.name === 'update_proposal').description, /null drops your proposed change/);
    assert.match(tools.find(t => t.name === 'propose_changes').description, /\{"clear": true\}/);

    // Only nulls: nothing to propose, and the assistant is told how to empty a cell.
    const nothing = await call('propose_changes', { reason: 'x', changes: [{ recordId: stock(3).id, values: { 'INSECTARY OR LABORATORY': null } }] });
    assert.match(nothing.error, /null means no change.*"clear": true/);

    const proposed = await call('propose_changes', {
      reason: 'Posturas',
      changes: [
        { recordId: stock(2).id, values: { 'NUMBER OF EGGS': '=16+2', 'HATCHING DATE': null } },
        { recordId: stock(3).id, values: { 'INSECTARY OR LABORATORY': null } },
      ],
    });
    assert.equal(proposed.rows, 1, 'the row with only nulls is left out');
    assert.deepEqual(proposed.noChange, ['900: HATCHING DATE', '901: INSECTARY OR LABORATORY']);
    assert.match(proposed.noChangeNote, /keep the sheet value/);
    let [shown] = await list();
    assert.deepEqual(shown.changes[0].values, { 'NUMBER OF EGGS': '=16+2' });

    // The case of the first day: null meant "drop this change", not "empty =16+2-1".
    const dropped = await call('update_proposal', {
      proposalId: proposed.proposalId,
      rows: [{ index: 0, values: { 'NUMBER OF EGGS': null, 'HATCHING DATE': { clear: true } } }],
    });
    assert.equal(dropped.revision, 2, JSON.stringify(dropped));
    assert.deepEqual(dropped.rows[0].values, { 'HATCHING DATE': { clear: true } });
    [shown] = await list();
    assert.deepEqual(shown.changes[0].values, { 'HATCHING DATE': null }, 'the table gets null: the cell to empty');
    assert.equal(shown.changes[0].current['HATCHING DATE'], d('2026-09-05'));
    const read = await call('get_proposal', { proposalId: proposed.proposalId });
    assert.deepEqual(read.rows[0].values, { 'HATCHING DATE': { clear: true } });

    // A change merged through `changes` follows the same rule.
    const merged = await call('update_proposal', {
      proposalId: proposed.proposalId,
      changes: [{ recordId: stock(2).id, values: { 'HATCHING DATE': null, 'NUMBER OF LARVAE': '10' } }],
    });
    assert.deepEqual(merged.rows[0].values, { 'NUMBER OF LARVAE': 10 }, 'null took back the clear');
    await call('update_proposal', { proposalId: proposed.proposalId, rows: [{ index: 0, values: { 'HATCHING DATE': { clear: true } } }] });

    const applied = await call('apply_proposal', { proposalId: proposed.proposalId });
    assert.equal(applied.status, 'applied', JSON.stringify(applied));
    assert.equal(stock(2).formulas['NUMBER OF EGGS'], '=16+2-1', 'the sum the assistant dropped is untouched');
    assert.equal(stock(2).values['HATCHING DATE'] ?? null, null, 'emptied on {clear:true}');
    assert.equal(stock(2).values['NUMBER OF LARVAE'], 10);
    assert.equal(stock(3).values['INSECTARY OR LABORATORY'], 'Insectary');
  } finally {
    store.close();
  }
});

test('notes the assistant adds keep the "d/m/yy INI:" form and go after the existing note', async () => {
  const { store, call, list, stock } = await setup(STOCKS, STOCK_FORMULAS);
  try {
    const dated = text => noteText(text, { today: today(), initials: 'FCH' });
    const proposed = await call('propose_changes', {
      reason: 'Notas',
      changes: [
        { recordId: stock(2).id, values: { NOTES: 'larvas con hongos' } },
        { recordId: stock(3).id, values: { NOTES: 'NA' } },
      ],
      newRows: [{ sheet: 'Insectary_stocks', values: { 'CLUTCH NUMBER': 902, NOTES: '30/9/26 AA: puesta de J7A' } }],
    });
    let [shown] = await list();
    const byLabel = label => shown.changes.find(c => c.label === label);
    assert.equal(byLabel('900').values.NOTES, `15/7/26 MJS: some eggs dry | ${dated('larvas con hongos')}`);
    assert.equal(byLabel('901').values.NOTES, dated('NA'));
    assert.equal(byLabel('902').values.NOTES, '30/9/26 AA: puesta de J7A', 'a note already dated keeps its prefix');

    // Revised: the new text replaces the assistant's own addition, the old note stays; the full
    // value sent back as it was read is not appended twice; {replace} rewrites it only when asked.
    let out = await call('update_proposal', { proposalId: proposed.proposalId, rows: [{ index: 1, values: { NOTES: 'larvas sanas' } }] });
    const index = out.rows.find(r => r.label === '900').index;
    assert.equal(out.rows[index].values.NOTES, `15/7/26 MJS: some eggs dry | ${dated('larvas sanas')}`);
    out = await call('update_proposal', { proposalId: proposed.proposalId, rows: [{ index, values: { NOTES: out.rows[index].values.NOTES } }] });
    assert.equal(out.rows[index].values.NOTES, `15/7/26 MJS: some eggs dry | ${dated('larvas sanas')}`);
    out = await call('update_proposal', { proposalId: proposed.proposalId, rows: [{ index, values: { NOTES: { replace: 'some eggs dry' } } }] });
    assert.equal(out.rows[index].values.NOTES, 'some eggs dry');
    [shown] = await list();
    assert.equal(shown.changes[index].values.NOTES, 'some eggs dry');
  } finally {
    store.close();
  }
});

// ---------------------------------------------------------------------------
// match_notebook

const NOTEBOOK_SHEETS = {
  Insectary_stocks: [
    { row: 2, values: { 'CLUTCH NUMBER': 838, SPECIES: 'Mechanitis messenoides intermedia', NOTES: 'old' } },
    { row: 3, values: { 'CLUTCH NUMBER': 848, Generation: 'NA', SPECIES: 'Mechanitis messenoides messenoides', 'DATE LAID': d('2025-08-08') } },
  ],
};
const PAGE = {
  kind: 'stocks',
  year: 2025,
  lines: [
    { raw: '838 interm. disec 2+1 larvas enfermas', values: { 'CLUTCH NUMBER': '838', dissections: '2+1', NOTES: 'larvas enfermas' } },
    { raw: '848 messen. 8/8', values: { 'CLUTCH NUMBER': '848', SPECIES: 'Mechanitis messenoides messenoides', 'DATE LAID': '8/8' } },
    { raw: '999 lys (F1) 1/9 12', values: { 'CLUTCH NUMBER': '999', SPECIES: 'Mechanitis lysimnia (F1)', 'DATE LAID': '1/9', 'NUMBER OF EGGS': '12' } },
  ],
};

test('match_notebook: rows in the page order, context rows for lines already in the sheet (never written), (F1) and dissections', async () => {
  const { store, call, list, stock } = await setup(NOTEBOOK_SHEETS);
  try {
    const plain = await call('match_notebook', PAGE);
    let [shown] = await list();
    assert.deepEqual(shown.changes.map(c => c.line), [1, 3], 'the new row of line 3 comes after line 1');
    assert.equal(plain.counts.rowsInProposal, 2);
    await call('match_notebook', { ...PAGE, replaceProposalId: plain.proposalId, includeUnchanged: true }).then(out => {
      assert.equal(out.proposalId, plain.proposalId);
      assert.equal(out.counts.contextRows, 1);
      assert.equal(out.lines[1].contextRow, true);
      assert.equal(out.lines[1].inProposal, false);
    });
    [shown] = await list();
    assert.deepEqual(shown.changes.map(c => c.line), [1, 2, 3]);
    const [first, context, fresh] = shown.changes;
    assert.equal(context.context, true);
    assert.deepEqual(context.values, {});
    assert.match(context.note, /Línea 2: .*solo contexto, no se escribe/);
    // The dissections column, a count kept as a sum; the note after the old one, dated.
    assert.equal(first.values['NUMBER OF PUPAE/LARVAE FOR DISECTIONS'], '=2+1');
    assert.equal(first.values.NOTES, `old | ${noteText('larvas enfermas', { today: today(), initials: 'FC' })}`);
    // "(F1)" after the species is the Generation.
    assert.equal(fresh.values.Generation, 'F1');
    assert.equal(fresh.values.SPECIES, 'Mechanitis lysimnia');

    const applied = await call('apply_proposal', { proposalId: plain.proposalId });
    assert.equal(applied.status, 'applied', JSON.stringify(applied));
    assert.equal(applied.rows, 2, 'the context row is not written');
    assert.equal(stock(2).formulas['NUMBER OF PUPAE/LARVAE FOR DISECTIONS'], '=2+1');
    assert.equal(stock(3).version, 1, 'the context row was not touched');
    const created = store.db
      .prepare("SELECT values_json FROM records WHERE sheet = 'Insectary_stocks' AND json_extract(values_json, '$.\"CLUTCH NUMBER\"') = 999")
      .get();
    assert.equal(JSON.parse(created.values_json).Generation, 'F1');

    // A page whose lines are all in the sheet makes no proposal, even with includeUnchanged.
    const same = await call('match_notebook', { kind: 'stocks', year: 2025, lines: [PAGE.lines[1]], includeUnchanged: true });
    assert.equal(same.proposalId, null);
  } finally {
    store.close();
  }
});

test('a context row edited in the table becomes a real change', async () => {
  const { store, call, http, list } = await setup(NOTEBOOK_SHEETS);
  try {
    const out = await call('match_notebook', { ...PAGE, includeUnchanged: true });
    const [shown] = await list();
    const context = shown.changes.find(c => c.context);
    const edited = await http('POST', `/api/chat/proposals/${out.proposalId}/edit`, {
      cells: [{ key: context.key, field: 'NUMBER OF EGGS', value: '=5' }],
    });
    const row = edited.body.proposal.changes.find(c => c.key === context.key);
    assert.ok(!row.context);
    assert.deepEqual(row.values, { 'NUMBER OF EGGS': '=5' });
  } finally {
    store.close();
  }
});

// ---------------------------------------------------------------------------
// find_records / count_records

const IKIAM = { lat: -0.948557, lon: -77.86605 };
const PLACES = {
  Location_data: [
    { row: 2, values: { Collection_location: 'Ikiam', Latitude: IKIAM.lat, Longitude: IKIAM.lon } },
    { row: 3, values: { Collection_location: 'Mariposario Ikiam', Latitude: -0.94839, Longitude: -77.865074 } },
    { row: 4, values: { Collection_location: 'Tena', Latitude: -0.9938, Longitude: -77.8129 } },
    { row: 5, values: { Collection_location: 'Añangu, Yasuní', Latitude: -0.524863, Longitude: -76.384183 } },
  ],
  Collection_data: [
    { row: 2, values: { SPECIES: 'Oleria onega', Preservation_medium: 'Flash frozen', Collection_location: 'Ikiam', CAM_ID: 'CAM000001', Collection_date: d('2025-03-01') } },
    { row: 3, values: { SPECIES: 'Oleria onega', Preservation_medium: 'Flash frozen', Collection_location: 'Añangu, Yasuní', CAM_ID: 'CAM000002', Collection_date: d('2026-03-01') } },
    { row: 4, values: { SPECIES: 'Oleria onega', Preservation_medium: 'Ethanol', Collection_location: 'Ikiam', CAM_ID: 'CAM000003', Collection_date: d('2026-04-01') } },
    { row: 5, values: { SPECIES: 'Hypothyris anastasia', Preservation_medium: 'Flash frozen', Collection_location: 'Ikiam', CAM_ID: 'CAM000004', Collection_date: d('2026-04-02') } },
    { row: 6, values: { SPECIES: 'oleria  onega', Preservation_medium: 'flash frozen', Collection_location: 'Tena', CAM_ID: 'CAM000005', Collection_date: d('2026-05-01') } },
    { row: 7, values: { SPECIES: 'Oleria onega', Preservation_medium: 'Flash frozen', CAM_ID: 'CAM000006' } },
  ],
  Insectary_stocks: [{ row: 2, values: { 'CLUTCH NUMBER': 900, SPECIES: 'Mechanitis lysimnia' } }],
};
const LOOKUP = '=XLOOKUP($Z2,Location_data!$A:$A,Location_data!O:O,"NOT_FOUND")';
const PLACE_FORMULAS = [
  ['Collection_data', 2, 'DECIMAL_LATITUDE', LOOKUP, IKIAM.lat],
  ['Collection_data', 2, 'DECIMAL_LONGITUDE', LOOKUP, IKIAM.lon],
  ['Insectary_stocks', 2, 'NUMBER OF EGGS', '=16+2-1', 17],
];

test('find_records: filters, distance to a place, computed values with their formulas, fields and a size budget', async () => {
  const { store, call } = await setup(PLACES, PLACE_FORMULAS);
  try {
    // "Flash-frozen Oleria onega within 15 km of Ikiam": Ikiam and Tena, not Yasuní nor the ethanol one.
    const near = await call('find_records', {
      module: 'Collection_data',
      filters: { SPECIES: 'Oleria onega', Preservation_medium: 'Flash frozen' },
      near: { location: 'ikiam', km: 15 },
      fields: ['CAM_ID', 'Collection_location', 'DECIMAL_LATITUDE'],
    });
    assert.deepEqual(near.found.map(r => r.values.CAM_ID), ['CAM000001', 'CAM000005']);
    assert.equal(near.found[0].distanceKm, 0);
    assert.ok(near.found[1].distanceKm > 5 && near.found[1].distanceKm < 15, String(near.found[1].distanceKm));
    assert.equal(near.near.centre.name, 'Ikiam');
    assert.equal(near.near.rowsWithoutPlace, 1, 'the row without a place is counted apart');
    // Only the columns asked for; a formula column with its computed value and its formula.
    assert.deepEqual(Object.keys(near.found[0].values).sort(), ['CAM_ID', 'Collection_location', 'DECIMAL_LATITUDE']);
    assert.equal(near.found[0].values.DECIMAL_LATITUDE, IKIAM.lat);
    assert.equal(near.found[0].formulas.DECIMAL_LATITUDE, LOOKUP);
    assert.deepEqual(near.formulaColumns, ['DECIMAL_LATITUDE']);

    // By identifier, as before: a count typed as a sum shows its value and its terms.
    const byId = await call('find_records', { module: 'Insectary_stocks', field: 'CLUTCH NUMBER', values: ['900', '999'] });
    assert.equal(byId.found[0].values['NUMBER OF EGGS'], 17);
    assert.equal(byId.found[0].formulas['NUMBER OF EGGS'], '=16+2-1');
    assert.deepEqual(byId.missing, ['999']);
    // Without fields, long lookup formulas are named, not repeated in every row.
    const all = await call('find_records', { module: 'Collection_data', filters: { CAM_ID: 'CAM000001' } });
    assert.equal(all.found[0].values.DECIMAL_LONGITUDE, IKIAM.lon);
    assert.ok(!all.found[0].formulas);

    // limit, offset and the "truncated" note.
    const page = await call('find_records', { module: 'Collection_data', filters: { SPECIES: 'Oleria onega' }, limit: 2 });
    assert.equal(page.total, 5);
    assert.equal(page.returned, 2);
    assert.match(page.truncated, /3 more rows not shown.*offset=2/);
    const cut = findRecords(store.db, { module: 'Collection_data', filters: { SPECIES: { contains: 'oleria' } } }, { budget: 400 });
    assert.ok(cut.returned < cut.total);
    assert.match(cut.truncated, /size limit.*Narrow with filters/);
    // Date ranges, lists and "not".
    const dates = await call('find_records', {
      module: 'Collection_data',
      filters: { Collection_date: { from: '2026-01-01', to: '2026-04-30' }, Preservation_medium: { not: 'Ethanol' } },
      fields: ['CAM_ID'],
    });
    assert.deepEqual(dates.found.map(r => r.values.CAM_ID), ['CAM000002', 'CAM000004']);

    // Mistakes are explained.
    assert.match((await call('find_records', { module: 'Collection_data', near: { location: 'Mordor', km: 5 } })).error, /No single Collection_location/);
    assert.match((await call('find_records', { module: 'Collection_data', near: { location: 'iki', km: 5 } })).error, /did you mean: Ikiam, Mariposario Ikiam/);
    assert.match((await call('find_records', { module: 'Collection_data', filters: { Especie: 'x' } })).error, /Unknown column Especie/);
    assert.match((await call('find_records', { module: 'Insectary_stocks', near: { location: 'Ikiam', km: 5 } })).error, /no coordinates/);
  } finally {
    store.close();
  }
});

test('count_records counts by filters, place and groups', async () => {
  const { store, call, mcp } = await setup(PLACES, PLACE_FORMULAS);
  try {
    const tools = (await mcp('tools/list')).body.result.tools;
    assert.ok(tools.some(t => t.name === 'count_records'));
    const total = await call('count_records', { sheet: 'Collection_data', filters: { Preservation_medium: 'Flash frozen' } });
    assert.equal(total.total, 5);
    const bySpecies = await call('count_records', { sheet: 'Collection_data', groupBy: 'SPECIES', near: { location: 'Ikiam', km: 15 } });
    assert.equal(bySpecies.total, 4);
    assert.deepEqual(
      bySpecies.groups.map(g => [g.SPECIES, g.n]),
      [['Oleria onega', 2], ['Hypothyris anastasia', 1], ['oleria  onega', 1]],
    );
    const byYear = await call('count_records', { sheet: 'Collection_data', groupBy: ['Collection_date:year', 'Preservation_medium'] });
    assert.deepEqual(byYear.groups.find(g => g['Collection_date:year'] === '2026' && g.Preservation_medium === 'Flash frozen').n, 2);
    assert.ok(byYear.groups.some(g => g['Collection_date:year'] === '(empty)'));
    assert.match((await call('count_records', { sheet: 'Collection_data', groupBy: 'SPECIES:year' })).error, /only date columns/);
  } finally {
    store.close();
  }
});

test('a T3 chat finds rows and drafts edits; only the rows the person chose are written', async () => {
  const lookup = '=XLOOKUP(C2,Insectary_stocks!A:A,Insectary_stocks!C:C,"")';
  const { store, call, list } = await setup(
    {
      Insectary_data: [
        { row: 2, values: { Insectary_ID: '5VB', 'CLUTCH NUMBER': 838, Sex: 'female' } },
        { row: 3, values: { Insectary_ID: '8VD', 'CLUTCH NUMBER': 843, Sex: 'female' } },
      ],
    },
    [
      ['Insectary_data', 2, 'SPECIES', lookup, 'Mechanitis messenoides intermedia'],
      ['Insectary_data', 3, 'SPECIES', lookup, 'Mechanitis messenoides messenoides'],
    ],
  );
  try {
    const { found, missing } = await call('find_records', { module: 'Insectary_data', field: 'Insectary_ID', values: ['5VB', '8VD', '9ZZ'] });
    assert.deepEqual(missing, ['9ZZ']);
    const byId = Object.fromEntries(found.map(r => [r.values.Insectary_ID, r]));
    const out = await call('propose_changes', {
      reason: 'Cuaderno de insectario, agosto 2025',
      changes: [
        { recordId: byId['5VB'].id, values: { SPECIES: 'Mechanitis messenoides deceptus' }, note: 'emergió deceptus' },
        { recordId: byId['8VD'].id, values: { 'CLUTCH NUMBER': 848 }, note: 'cuaderno: 848' },
      ],
    });
    assert.equal(out.rows, 2);
    const [proposal] = await list();
    assert.deepEqual(proposal.fields.slice(0, 2), ['SPECIES', 'CLUTCH NUMBER']);
    assert.deepEqual(proposal.changes[0].replaceFormula, ['SPECIES']);
    assert.equal(proposal.changes[1].current['CLUTCH NUMBER'], 843);
    // "Está correcto, aplica solo la fila del clutch".
    const applied = await call('apply_proposal', { proposalId: out.proposalId, indexes: [1] });
    assert.equal(applied.status, 'applied');
    assert.equal(store.getRecordBySheetRow('Insectary_data', 3).values['CLUTCH NUMBER'], 848);
    // The species row was not chosen: its formula is untouched.
    assert.ok(store.getRecordBySheetRow('Insectary_data', 2).formulas.SPECIES);
    const [after] = await list();
    assert.deepEqual([after.status, after.applied], ['applied', [1]]);
  } finally {
    store.close();
  }
});
