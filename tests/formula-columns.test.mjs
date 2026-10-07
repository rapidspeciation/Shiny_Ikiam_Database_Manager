// Values a proposal types into formula columns (Collection_data's Family, Subfamily, Tribe and
// Genus, Data_entry_order, Insectary_stocks' HatchingTime…), weighed as Insectary_data's SPECIES
// is: what the column's formula gives with the row's values is shown beside the cell; a value it
// gives anyway is not written (the formula stays), another one is written over it as a doubtful
// cell, and a protected column is left to the sheet. Columns after the notes are never written.
import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { moduleMap } from '../server/schema.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { HIDDEN_COLUMNS, isNotWritten } from '../server/proposal-columns.mjs';

const EPOCH = Date.UTC(1899, 11, 30);
const today = Math.round(
  (Date.parse(`${new Intl.DateTimeFormat('en-CA', { timeZone: 'America/Guayaquil' }).format(new Date())}T00:00:00Z`) - EPOCH) / 864e5,
);
const col = (sheet, key) => moduleMap.get(sheet).fields.find(f => f.key === key).column;
const WILD = 'Collected_Preserved';
const taxon = (r, letter) => `=IF($S${r}="","",XLOOKUP($S${r},Taxonomy_v18Jun25!$J:$J,Taxonomy_v18Jun25!${letter}:${letter},"NOT_FOUND"))`;
const RANKS = { Family: 'C', Subfamily: 'F', Tribe: 'G', Genus: 'H' };
const ranks = r => Object.fromEntries(Object.entries(RANKS).map(([field, letter]) => [field, { formula: taxon(r, letter) }]));
const manifest = r =>
  `=IFS(G${r}="","",G${r}="NA","NA",OR(G${r}<>"",G${r}<>"NA"),XLOOKUP(G${r},MEIER_manifests_23Jun26!$B:$B,MEIER_manifests_23Jun26!$A:$A,"Not in STS"))`;
const hatching = r => `=if(or(D${r}="",G${r}=""),"",if(or(D${r}="NA",G${r}="NA"),"NA",G${r}-D${r}))`;

/**
 * Collection_data as the real one: 20 wild butterflies (rows 2–21) with Data_entry_order, the
 * taxonomy lookups and the tube manifest as formulas, then pre-made rows 22–25 holding the
 * lookups and the manifest but no Data_entry_order. Column A (Data_entry_order) is protected.
 * Insectary_stocks: 12 clutches with HatchingTime as a formula.
 */
async function fixture({ protect = true } = {}) {
  const collection = [];
  for (let i = 0; i < 20; i++) {
    const r = 2 + i;
    collection.push({
      row: r,
      values: {
        Data_entry_order: r === 2 ? 1 : { formula: `=IF(B${r}="","",A${r - 1}+1)` },
        Release_Collect: WILD,
        CAM_ID: `CAM0000${10 + i}`,
        SPECIES: i % 2 ? 'Hypothyris anastasia' : 'Oleria onega',
        Sex: 'male',
        Collection_date: today - 40 + i,
        Tube_1_manifest: { formula: manifest(r) },
        ...ranks(r),
      },
    });
  }
  for (const r of [22, 23, 24, 25]) collection.push({ row: r, values: { ...ranks(r), Tube_1_manifest: { formula: manifest(r) } } });
  const stocks = [];
  for (let i = 0; i < 12; i++) {
    const r = 2 + i;
    stocks.push({
      row: r,
      values: {
        'CLUTCH NUMBER': 900 + i,
        SPECIES: 'Oleria onega',
        'DATE LAID': today - 30 + i,
        'NUMBER OF EGGS': 10,
        'HATCHING DATE': today - 25 + i,
        HatchingTime: { formula: hatching(r) },
      },
    });
  }
  const sheets = new LocalSheets(
    {
      Collection_data: collection,
      Insectary_stocks: stocks,
      Taxonomy_v18Jun25: [
        { row: 2, values: { family: 'Nymphalidae', subfamily: 'Danainae', tribe: 'Ithomiini', genus: 'Oleria', species: 'Oleria onega' } },
        { row: 3, values: { family: 'Nymphalidae', subfamily: 'Danainae', tribe: 'Ithomiini', genus: 'Hypothyris', species: 'Hypothyris anastasia' } },
      ],
    },
    protect ? { protectedRanges: { Collection_data: [{ startRowIndex: 1, startColumnIndex: 0, endColumnIndex: 1 }] } } : {},
  );
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Collection_data', 'Insectary_stocks', 'Taxonomy_v18Jun25'] });
  const assistant = createAssistant({ store, config: {} });
  store.db
    .prepare("INSERT INTO users(id,username,display_name,role,salt,password_hash,active,created_at) VALUES('u1','ana','Ana','editor','s','h',1,'2026-01-01')")
    .run();
  store.db
    .prepare("INSERT INTO ai_tokens(token_hash,user_id,label,created_at) VALUES(?,?,'t3','2026-01-01')")
    .run(createHash('sha256').update('ana-token').digest('hex'), 'u1');
  const call = async (name, args) =>
    JSON.parse(
      (
        await assistant.mcp(
          { authorization: 'Bearer ana-token' },
          { jsonrpc: '2.0', id: 1, method: 'tools/call', params: { name, arguments: args } },
        )
      ).body.result.content[0].text,
    );
  const user = { id: 'u1', username: 'ana', displayName: 'Ana', role: 'editor' };
  const listed = async id =>
    (await assistant.handle({ method: 'GET', path: '/api/chat/proposals', user, query: { all: '1', only: id } })).body.proposals[0];
  const cell = (sheet, row, key) => sheets.cell(sheet, row, col(sheet, key))?.userEnteredValue;
  const hint = (view, change, field) => view.hintTable?.[change.hints?.[field]]?.msg?.key;
  const edit = (id, body) => assistant.handle({ method: 'POST', path: `/api/chat/proposals/${id}/edit`, body, user, query: {} });
  return { store, call, listed, cell, hint, edit };
}

test("a wild-caught butterfly's new Collection_data row: the taxonomy formulas stay where they give the same, another genus is doubtful, the protected column is the sheet's", async () => {
  const { store, call, listed, cell, hint } = await fixture();
  try {
    const proposed = await call('propose_changes', {
      reason: 'Silvestres',
      newRows: [
        {
          sheet: 'Collection_data',
          values: {
            Release_Collect: WILD,
            CAM_ID: 'CAM000099',
            SPECIES: 'Oleria onega',
            Sex: 'female',
            Collection_date: '2026-10-01',
            Data_entry_order: 21,
            Family: 'Nymphalidae',
            Subfamily: ' danainae ',
            Tribe: 'Ithomiini',
            Genus: 'Hypothyris',
          },
        },
      ],
    });
    assert.ok(proposed.proposalId, JSON.stringify(proposed));
    // The answer: what was left to the formulas and why, and the value written over one.
    assert.match(proposed.leftOut, /Family, Subfamily, Tribe \(the formula gives the same\)/, proposed.leftOut);
    assert.match(proposed.leftOut, /Data_entry_order \(protected column, left to the sheet\)/);
    assert.deepEqual(
      proposed.overFormula.map(o => [o.field, o.value, o.formulaGives]),
      [['Genus', 'Hypothyris', 'Oleria']],
    );

    const view = await listed(proposed.proposalId);
    const row = view.changes.find(c => c.create);
    for (const field of ['Family', 'Subfamily', 'Tribe', 'Data_entry_order']) assert.ok(!(field in row.values), field);
    assert.equal(row.values.Genus, 'Hypothyris');
    assert.deepEqual(row.replaceFormula, ['Genus']);
    // Beside each cell, what its formula gives with the row's values («fórmula → valor»).
    assert.equal(row.formulaGives.Family, 'Nymphalidae');
    assert.equal(row.formulaGives.Subfamily, 'Danainae');
    assert.equal(row.formulaGives.Genus, 'Oleria');
    // Another genus than the formula's: a doubtful cell, the formula's value its other reading.
    assert.equal(row.doubts.Genus.reason, 'La fórmula daría «Oleria»');
    assert.deepEqual(row.doubts.Genus.alternatives, ['Oleria']);
    assert.equal(hint(view, row, 'Family'), '«{value}» es lo que da la fórmula: se deja la fórmula');
    assert.equal(hint(view, row, 'Data_entry_order'), '«{value}» no se escribe: columna protegida en la hoja');
    // The protected column: said as such, never as what its formula would give.
    assert.ok(row.protectedCells?.includes('Data_entry_order'), JSON.stringify(row.protectedCells));
    assert.equal(row.formulaGives.Data_entry_order, undefined);
    assert.ok(view.newRowFormulas.Collection_data.includes('Family'));

    // Unchecked, the doubtful genus holds the apply back; confirmed, it goes over the formula.
    const held = await call('apply_proposal', { proposalId: proposed.proposalId });
    assert.match(held.error, /doubtful cells not checked yet/, JSON.stringify(held));
    assert.deepEqual(held.doubtful.map(d => [d.field, d.value, d.alternatives]), [['Genus', 'Hypothyris', ['Oleria']]]);
    const applied = await call('apply_proposal', { proposalId: proposed.proposalId, confirmDoubtful: true });
    assert.equal(applied.status, 'applied', JSON.stringify(applied));
    assert.equal(cell('Collection_data', 22, 'CAM_ID')?.stringValue, 'CAM000099');
    for (const field of ['Family', 'Subfamily', 'Tribe']) assert.equal(cell('Collection_data', 22, field)?.formulaValue, taxon(22, RANKS[field]), field);
    assert.equal(cell('Collection_data', 22, 'Genus')?.stringValue, 'Hypothyris');
    assert.equal(cell('Collection_data', 22, 'Data_entry_order'), undefined, 'the protected column untouched');
    assert.equal(cell('Collection_data', 22, 'Tube_1_manifest')?.formulaValue, manifest(22));
  } finally {
    store.close();
  }
});

test('in an existing row, a value its formula gives is left to it and another is written over it, doubtful', async () => {
  const { store, call, listed, cell, edit } = await fixture();
  try {
    const record = store.getRecordBySheetRow('Collection_data', 5);
    const proposed = await call('propose_changes', {
      reason: 'Taxonomía',
      changes: [{ recordId: record.id, values: { Family: 'nymphalidae', Genus: 'Mechanitis', Sex: 'female' } }],
    });
    assert.ok(proposed.proposalId, JSON.stringify(proposed));
    assert.match(proposed.leftOut, /Family \(the formula gives the same\)/);
    const row = (await listed(proposed.proposalId)).changes[0];
    assert.deepEqual(Object.keys(row.values).sort(), ['Genus', 'Sex']);
    assert.deepEqual(row.replaceFormula, ['Genus']);
    assert.equal(row.formulaGives.Genus, 'Hypothyris');
    assert.equal(row.doubts.Genus.reason, 'La fórmula daría «Hypothyris»');
    const applied = await call('apply_proposal', { proposalId: proposed.proposalId, confirmDoubtful: true });
    assert.equal(applied.status, 'applied', JSON.stringify(applied));
    assert.equal(cell('Collection_data', 5, 'Family')?.formulaValue, taxon(5, 'C'));
    assert.equal(cell('Collection_data', 5, 'Genus')?.stringValue, 'Mechanitis');

    // The person types the formula's genus in the table over the assistant's: not written, the formula stays.
    const other = store.getRecordBySheetRow('Collection_data', 7);
    const again = await call('propose_changes', { reason: 'Taxonomía', changes: [{ recordId: other.id, values: { Genus: 'Mechanitis', Sex: 'female' } }] });
    const shown = (await listed(again.proposalId)).changes[0];
    assert.equal(shown.values.Genus, 'Mechanitis');
    const out = await edit(again.proposalId, { cells: [{ key: shown.key, field: 'Genus', value: 'hypothyris', before: 'Mechanitis' }] });
    assert.equal(out.status, 200, JSON.stringify(out.body));
    assert.deepEqual(out.body.rejected, []);
    const typed = out.body.proposal.changes[0];
    assert.ok(!('Genus' in typed.values));
    assert.equal(typed.personEdits.Genus.ai, 'Mechanitis', "the assistant's value is kept aside");
    assert.equal(typed.doubts?.Genus, undefined, 'no longer doubtful');
    assert.deepEqual(typed.formulaLeft, [{ field: 'Genus', why: 'same' }]);
  } finally {
    store.close();
  }
});

test('new rows of one proposal take consecutive rows: Data_entry_order counts on from the new row above', async () => {
  const { store, call, listed } = await fixture({ protect: false });
  try {
    const wild = cam => ({ sheet: 'Collection_data', values: { Release_Collect: WILD, CAM_ID: cam, SPECIES: 'Oleria onega', Data_entry_order: 7 } });
    const proposed = await call('propose_changes', { reason: 'Silvestres', newRows: [wild('CAM000097'), wild('CAM000096')] });
    assert.ok(proposed.proposalId, JSON.stringify(proposed));
    // A typed order is left to the formula (it depends on the rows written before), whatever it says.
    assert.match(proposed.leftOut, /Data_entry_order \(a formula column\)/);
    const [first, second] = (await listed(proposed.proposalId)).changes;
    assert.equal(typeof first.formulaGives.Data_entry_order, 'number');
    assert.equal(second.formulaGives.Data_entry_order, first.formulaGives.Data_entry_order + 1);
    assert.equal(second.formulaGives.Genus, 'Oleria');
  } finally {
    store.close();
  }
});

test('Insectary_stocks too: HatchingTime typed as its formula gives it is left to it, another value is doubtful', async () => {
  const { store, call, listed } = await fixture();
  try {
    const proposed = await call('propose_changes', {
      reason: 'Posturas',
      newRows: [
        {
          sheet: 'Insectary_stocks',
          values: { 'CLUTCH NUMBER': 950, SPECIES: 'Oleria onega', 'DATE LAID': '2026-09-20', 'NUMBER OF EGGS': 8, 'HATCHING DATE': '2026-09-25', HatchingTime: 5 },
        },
      ],
      changes: [{ recordId: store.getRecordBySheetRow('Insectary_stocks', 3).id, values: { HatchingTime: 9 } }],
    });
    assert.ok(proposed.proposalId, JSON.stringify(proposed));
    const [created, edited] = (await listed(proposed.proposalId)).changes;
    assert.ok(!('HatchingTime' in created.values));
    assert.equal(created.formulaGives.HatchingTime, 5);
    assert.equal(edited.values.HatchingTime, 9);
    assert.equal(edited.doubts.HatchingTime.reason, 'La fórmula daría «5»');
  } finally {
    store.close();
  }
});

test('the columns after the notes (racks, manifests, the STS block) are never shown nor written, in either sheet', async () => {
  for (const sheet of ['Collection_data', 'Insectary_data'])
    for (const n of [1, 2, 3, 4]) {
      assert.ok(HIDDEN_COLUMNS[sheet].has(`Tube_${n}_manifest`), `${sheet} Tube_${n}_manifest`);
      assert.ok(isNotWritten(sheet, `Tube_${n}_rack`));
    }
  for (const field of ['COLLECTOR_SAMPLE_ID', 'Specimen ID', 'Select_TEMP']) assert.ok(isNotWritten('Collection_data', field), field);
  assert.ok(!isNotWritten('Collection_data', 'Notes_Collection_data'));
  const { store, call } = await fixture();
  try {
    const created = await call('propose_changes', {
      reason: 'Silvestres',
      newRows: [{ sheet: 'Collection_data', values: { Release_Collect: WILD, CAM_ID: 'CAM000098', SPECIES: 'Oleria onega', Tube_1_manifest: 'X1' } }],
    });
    assert.match(created.error, /Tube_1_manifest is not written in proposals of Collection_data/);
    const edited = await call('propose_changes', {
      reason: 'Silvestres',
      changes: [{ recordId: store.getRecordBySheetRow('Collection_data', 4).id, values: { Tube_2_manifest: 'X2' } }],
    });
    assert.match(edited.error, /Tube_2_manifest is not written in proposals of Collection_data/);
  } finally {
    store.close();
  }
});
