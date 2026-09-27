import test from 'node:test';
import assert from 'node:assert/strict';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { idSuggestions } from '../server/grid.mjs';

test('tube suggestions follow each rack in use: by kind of work and medium', async () => {
  const ins = (row, id, purpose, medium, tube, date) => ({
    row,
    values: { Insectary_ID: id, SPECIES: 'Mechanitis polymnia', Research_purpose: purpose, Tube_1_id: tube, T1_Preservation_medium: medium, Preservation_date: date },
  });
  const col = (row, purpose, medium, tube, legs, date) => ({
    row,
    values: { CAM_ID: `CAM0799${row}`, SPECIES: 'Oleria onega', Purpose: purpose, Preservation_medium: medium, Tube_1_id: tube, Tube_4_id_LEGS: legs, Collection_date: date },
  });
  const sheets = new LocalSheets({
    Insectary_data: [
      ins(2, 'A0A', 'F1/F2 mutation rate', 'Flash frozen', 'FS50849033', 46280),
      ins(3, 'A1A', 'F1/F2 mutation rate', 'Flash frozen', 'FS50849034', 46281),
      ins(4, 'A2A', 'Stock', 'Ethanol', 'FF10000010', 46200),
    ],
    Collection_data: [
      col(10, 'Monitoring', 'Flash frozen', 'FS90415320', 'FF20000001', 46290),
      col(11, 'Monitoring', 'Flash frozen', 'FS90415321', 'FF20000002', 46291),
      // A tube used elsewhere is skipped by the suggestion.
      col(12, 'Collection', 'Flash frozen', 'FS90415322', 'NA', 46100),
    ],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data', 'Collection_data'] });
  const { suggestions } = idSuggestions(store, { kind: 'tube' });
  const find = (context, medium) => suggestions.find(s => s.context === context && s.medium === medium)?.value;
  assert.equal(find('Cruces', 'Flash frozen'), 'FS50849035');
  assert.equal(find('Insectario', 'Ethanol'), 'FF10000011');
  assert.equal(find('Monitoreo', 'Flash frozen'), 'FS90415323');
  assert.equal(find('Monitoreo (patas)', 'Patas'), 'FF20000003');
  // Newest rack first.
  assert.equal(suggestions[0].context.startsWith('Monitoreo'), true);
  store.close();
});
