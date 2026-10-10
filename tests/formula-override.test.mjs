import test from 'node:test';
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { applyBatch } from '../server/batch.mjs';
import { moduleMap } from '../server/schema.mjs';

const user = { id: 'editor-1', username: 'editor', role: 'editor' };
const column = key => moduleMap.get('Insectary_data').fields.find(f => f.key === key).column;

async function fixture() {
  const sheets = new LocalSheets({
    Insectary_data: [{ row: 2, values: { Insectary_ID: '5VB', 'CLUTCH NUMBER': 838, Sex: 'female' } }],
  });
  // The species comes from the clutch: the formula predicts intermedia.
  sheets.rows.get('Insectary_data').find(r => r.row === 2).cells[column('SPECIES')] = {
    userEnteredValue: { formulaValue: '=XLOOKUP(C2,Insectary_stocks!A:A,Insectary_stocks!C:C,"")' },
    effectiveValue: { stringValue: 'Mechanitis messenoides intermedia' },
  };
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data'] });
  return { sheets, store, record: store.getRecordBySheetRow('Insectary_data', 2) };
}

test('the species formula is typed over only when what emerged differs, and undo restores it', async () => {
  const { sheets, store, record } = await fixture();
  const edit = values => ({ requestId: randomUUID(), edits: [{ id: record.id, values }] });
  // Without asking to replace the formula, the cell stays protected.
  await assert.rejects(applyBatch(store, edit({ SPECIES: 'Mechanitis messenoides deceptus' }), user), e =>
    e.details.items.some(i => i.code === 'FORMULA_CELL'),
  );
  // Typing what the formula already predicts writes nothing: the formula stays.
  const same = await applyBatch(
    store,
    {
      requestId: randomUUID(),
      edits: [{ id: record.id, values: { SPECIES: 'Mechanitis messenoides intermedia' }, replaceFormula: ['SPECIES'] }],
    },
    user,
  );
  assert.equal(same.status, 'unchanged');
  // Only listed fields may be replaced.
  await assert.rejects(
    applyBatch(
      store,
      {
        requestId: randomUUID(),
        edits: [
          { id: record.id, values: { SPECIES: 'Mechanitis messenoides deceptus' }, replaceFormula: ['Pedigree'] },
        ],
      },
      user,
    ),
    e => e.details.items.some(i => i.code === 'FORMULA_CELL'),
  );
  const saved = await applyBatch(
    store,
    {
      requestId: randomUUID(),
      edits: [{ id: record.id, values: { SPECIES: 'Mechanitis messenoides deceptus' }, replaceFormula: ['SPECIES'] }],
    },
    user,
    { source: 'ai_approved' },
  );
  assert.equal(saved.status, 'verified');
  const cell = sheets.rows.get('Insectary_data').find(r => r.row === 2).cells[column('SPECIES')];
  assert.equal(cell.userEnteredValue.stringValue, 'Mechanitis messenoides deceptus');
  const after = store.getRecordBySheetRow('Insectary_data', 2);
  assert.equal(after.values.SPECIES, 'Mechanitis messenoides deceptus');
  assert.equal(after.formulas.SPECIES, undefined);

  await store.undo({ actionIds: [saved.action.id], requestId: randomUUID() }, user);
  const restored = sheets.rows.get('Insectary_data').find(r => r.row === 2).cells[column('SPECIES')];
  assert.match(restored.userEnteredValue.formulaValue, /^=XLOOKUP/);
  store.close();
});

test('emerged butterflies keep the clutch prediction unless another subspecies emerged', async () => {
  const col = key => moduleMap.get('Insectary_data').fields.find(f => f.key === key).column;
  const sheets = new LocalSheets({
    Insectary_data: [{ row: 2, values: { Insectary_ID: 'A0A', 'CLUTCH NUMBER': 900, Sex: 'male' } }],
    Insectary_stocks: [{ row: 2, values: { 'CLUTCH NUMBER': 973, SPECIES: 'Mechanitis polymnia proceriformis' } }],
  });
  // Two pre-made rows: the ID and the species are formulas (empty species until a clutch is typed).
  for (const [row, id] of [
    [3, 'A1A'],
    [4, 'A2A'],
  ]) {
    const cells = [];
    cells[col('Insectary_ID')] = {
      userEnteredValue: { formulaValue: '=NEXTID()' },
      effectiveValue: { stringValue: id },
    };
    cells[col('SPECIES')] = {
      userEnteredValue: { formulaValue: `=XLOOKUP(C${row},Insectary_stocks!A:A,Insectary_stocks!C:C,"")` },
    };
    sheets.rows.get('Insectary_data').push({ row, cells });
  }
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data', 'Insectary_stocks'] });
  const emerged = (id, species) => ({
    module: 'Insectary_data',
    values: { Insectary_ID: id, 'CLUTCH NUMBER': 973, Sex: 'female', SPECIES: species },
    replaceFormula: ['SPECIES'],
  });
  const saved = await applyBatch(
    store,
    {
      requestId: randomUUID(),
      creates: [emerged('A1A', 'Mechanitis polymnia proceriformis'), emerged('A2A', 'Mechanitis polymnia eurydice')],
    },
    user,
  );
  assert.equal(saved.status, 'verified');
  const cell = row => sheets.rows.get('Insectary_data').find(r => r.row === row).cells[col('SPECIES')];
  assert.ok(cell(3).userEnteredValue.formulaValue, 'the predicted subspecies keeps the formula');
  assert.equal(cell(4).userEnteredValue.stringValue, 'Mechanitis polymnia eurydice');
  store.close();
});

test('the Tube 2 medium is typed over its formula only in the rows whose formula would not give it', async () => {
  const col = key => moduleMap.get('Insectary_data').fields.find(f => f.key === key).column;
  const letter = n => (n >= 26 ? String.fromCharCode(64 + Math.floor(n / 26)) : '') + String.fromCharCode(65 + (n % 26));
  const U = letter(col('Tube_2_tissue'));
  const sheets = new LocalSheets(
    {
      Insectary_data: [
        { row: 2, values: { Insectary_ID: 'D2B', Sex: 'male' } },
        { row: 3, values: { Insectary_ID: 'J1E', Sex: 'male' } },
      ],
    },
    {
      // What Google would show: the older formula has no case for NOT_COLLECTED.
      evaluate: (formula, { row, value }) => {
        const tissue = value(row, col('Tube_2_tissue'));
        if (tissue === null || tissue === undefined || tissue === '') return '';
        if (tissue === 'NA') return formula.includes('NOT_PROVIDED') ? 'NOT_COLLECTED' : 'NA';
        return formula.includes('NOT_PROVIDED') && tissue === 'NOT_COLLECTED' ? 'NOT_COLLECTED' : '#N/A';
      },
    },
  );
  const cell = row => sheets.rows.get('Insectary_data').find(r => r.row === row).cells;
  cell(2)[col('T2_Preservation_medium')] = { userEnteredValue: { formulaValue: `=IFS(${U}2="","",${U}2="NA","NA")` } };
  cell(3)[col('T2_Preservation_medium')] = {
    userEnteredValue: { formulaValue: `=IFS(${U}3="","",OR(${U}3="NA",${U}3="NOT_COLLECTED",${U}3="NOT_PROVIDED"),"NOT_COLLECTED")` },
  };
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data'] });
  const death = { Death_cause: 'Unknown', Tube_2_id: 'NA', Tube_2_tissue: 'NOT_COLLECTED', T2_Preservation_medium: 'NOT_COLLECTED' };
  const saved = await applyBatch(
    store,
    {
      requestId: randomUUID(),
      edits: [2, 3].map(row => ({ id: store.getRecordBySheetRow('Insectary_data', row).id, values: death })),
    },
    user,
  );
  assert.equal(saved.status, 'verified');
  // The older formula would show #N/A: the value is typed. The newer one gives it: the formula stays.
  assert.equal(cell(2)[col('T2_Preservation_medium')].userEnteredValue.stringValue, 'NOT_COLLECTED');
  assert.match(cell(3)[col('T2_Preservation_medium')].userEnteredValue.formulaValue, /NOT_PROVIDED/);
  assert.equal(store.getRecordBySheetRow('Insectary_data', 2).values.T2_Preservation_medium, 'NOT_COLLECTED');

  await store.undo({ actionIds: [saved.action.id], requestId: randomUUID() }, user);
  assert.match(cell(2)[col('T2_Preservation_medium')].userEnteredValue.formulaValue, /^=IFS/);
  store.close();
});
