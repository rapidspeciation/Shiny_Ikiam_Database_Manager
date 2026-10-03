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

test('ID suggestions are kept until a sheet they read changes', async () => {
  const sheets = new LocalSheets({
    Insectary_data: [
      {
        row: 2,
        values: { Insectary_ID: 'A0A', SPECIES: 'Mechanitis polymnia', CAM_ID: 'CAM000010', Tube_1_id: 'FS50849033', T1_Preservation_medium: 'Ethanol', Preservation_date: 46280 },
      },
    ],
    Collection_data: [],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data', 'Collection_data'] });
  const first = idSuggestions(store, { kind: 'tube' });
  assert.equal(first.suggestions[0].value, 'FS50849034');
  // A caller changing its copy does not change the kept answer.
  first.suggestions[0].value = 'changed';
  assert.equal(idSuggestions(store, { kind: 'tube' }).suggestions[0].value, 'FS50849034');
  // A kept answer does not read the rows again.
  let reads = 0;
  const prepare = store.db.prepare.bind(store.db);
  store.db.prepare = sql => (/values_json/.test(sql) && reads++, prepare(sql));
  idSuggestions(store, { kind: 'tube' });
  idSuggestions(store, { kind: 'cam' });
  const afterFirstCam = reads;
  idSuggestions(store, { kind: 'cam' });
  assert.ok(afterFirstCam > 0);
  assert.equal(reads, afterFirstCam);
  store.db.prepare = prepare;
  // A tube used in the sheet since: the next suggestion moves on.
  await sheets.externalEdit('Insectary_data', 3, {
    Insectary_ID: 'A1A',
    SPECIES: 'Mechanitis polymnia',
    Tube_1_id: 'FS50849034',
    T1_Preservation_medium: 'Ethanol',
    Preservation_date: 46281,
  });
  await store.sync({ sheets: ['Insectary_data'] });
  assert.equal(idSuggestions(store, { kind: 'tube' }).suggestions[0].value, 'FS50849035');
  store.close();
});

test('check says which typed CAMs and tubes are used already, and where', async () => {
  const sheets = new LocalSheets({
    Insectary_data: [
      { row: 2, values: { Insectary_ID: 'A0A', SPECIES: 'Mechanitis polymnia', CAM_ID: 'CAM078300', Tube_1_id: 'FS90415400' } },
    ],
    Collection_data: [{ row: 5, values: { CAM_ID: 'CAM079900', SPECIES: 'Oleria onega', Tube_1_id: 'FS90415401' } }],
  });
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: ['Insectary_data', 'Collection_data'] });
  const tubes = idSuggestions(store, { kind: 'tube', check: 'fs90415400, FS90415401,FS90415402' });
  assert.deepEqual(Object.keys(tubes.used).sort(), ['FS90415400', 'FS90415401']);
  assert.equal(tubes.used.FS90415401.sheet, 'Collection_data');
  assert.equal(tubes.used.FS90415400.label, 'A0A');
  const cams = idSuggestions(store, { kind: 'cam', check: 'CAM079900,CAM078301' });
  assert.deepEqual(Object.keys(cams.used), ['CAM079900']);
  assert.deepEqual(idSuggestions(store, { kind: 'tube', check: '' }).used, {});
  store.close();
});
