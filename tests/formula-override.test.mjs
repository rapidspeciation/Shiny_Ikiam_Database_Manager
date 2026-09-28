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
  // Typing what the formula already predicts is refused: the formula stays.
  await assert.rejects(
    applyBatch(
      store,
      {
        requestId: randomUUID(),
        edits: [
          { id: record.id, values: { SPECIES: 'Mechanitis messenoides intermedia' }, replaceFormula: ['SPECIES'] },
        ],
      },
      user,
    ),
    e => e.details.items.some(i => i.code === 'MATCHES_FORMULA'),
  );
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
