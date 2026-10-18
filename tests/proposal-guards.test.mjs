// propose_changes' guards on changes that look sure but rarely are: an insectary butterfly's
// species typed by hand set back to its clutch's (what emerged may differ on purpose), and
// identifiers renamed on many rows from file names alone.
import test from 'node:test';
import assert from 'node:assert/strict';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { createAssistant } from '../server/assistant.mjs';
import { moduleMap } from '../server/schema.mjs';
import { signIn } from './helpers/assistant.mjs';

const EPOCH = Date.UTC(1899, 11, 30);
const serial = iso => Math.round((Date.parse(`${iso}T00:00:00Z`) - EPOCH) / 864e5);
const column = (sheet, key) => moduleMap.get(sheet).fields.find(f => f.key === key).column;
const PROCERIFORMIS = 'Mechanitis polymnia proceriformis';
const EURYDICE = 'Mechanitis polymnia eurydice';

async function setup(seed, formulas = []) {
  const sheets = new LocalSheets(seed);
  for (const { sheet, row, field, formula, value } of formulas)
    sheets.rows.get(sheet).find(r => r.row === row).cells[column(sheet, field)] = {
      userEnteredValue: { formulaValue: formula },
      ...(value ? { effectiveValue: { stringValue: value } } : {}),
    };
  const store = new Store({ localMode: true }, { sheets });
  await store.sync({ sheets: Object.keys(seed) });
  const assistant = createAssistant({ store, config: {} });
  const { call } = signIn(store, assistant);
  return { sheets, store, call, record: (sheet, row) => store.getRecordBySheetRow(sheet, row) };
}

// Clutch 976 is proceriformis; H0B and H1B emerged from it looking like eurydice, typed by hand
// over the formula, as their sibling H3B. H2B keeps the formula.
const clutch = () => {
  const xlookup = row => `=XLOOKUP(C${row},Insectary_stocks!A:A,Insectary_stocks!C:C,"")`;
  return {
    seed: {
      Insectary_stocks: [{ row: 2, values: { 'CLUTCH NUMBER': 976, SPECIES: PROCERIFORMIS } }],
      Insectary_data: [
        { row: 2, values: { Insectary_ID: 'H0B', 'CLUTCH NUMBER': 976, Wild_Reared: 'Reared', SPECIES: EURYDICE } },
        { row: 3, values: { Insectary_ID: 'H1B', 'CLUTCH NUMBER': 976, Wild_Reared: 'Reared', SPECIES: EURYDICE } },
        { row: 4, values: { Insectary_ID: 'H2B', 'CLUTCH NUMBER': 976, Wild_Reared: 'Reared' } },
        { row: 5, values: { Insectary_ID: 'H3B', 'CLUTCH NUMBER': 976, Wild_Reared: 'Reared', SPECIES: EURYDICE } },
      ],
    },
    formulas: [{ sheet: 'Insectary_data', row: 4, field: 'SPECIES', formula: xlookup(4), value: PROCERIFORMIS }],
  };
};

test("a species typed by hand set to its clutch's goes back to the formula, doubtful, with the siblings counted", async () => {
  const { seed, formulas } = clutch();
  const { sheets, store, call, record } = await setup(seed, formulas);
  try {
    const out = await call('propose_changes', {
      reason: 'H0B: subspecies as in its clutch 976',
      changes: [{ recordId: record('Insectary_data', 2).id, values: { SPECIES: PROCERIFORMIS }, note: 'clutch 976 is proceriformis' }],
    });
    assert.ok(!out.error, JSON.stringify(out));
    assert.equal(out.speciesFromClutch.length, 1);
    assert.match(out.speciesFromClutch[0].reason, /vuelve la fórmula/);
    // The sheet's own formula: not told as an unusual or costly one.
    assert.equal(out.formulaNotes, undefined);
    const full = await call('get_proposal', { proposalId: out.proposalId, full: true });
    const [row] = full.rows;
    // The formula of the row next to it, moved to this row.
    assert.equal(row.values.SPECIES, '=XLOOKUP(C2,Insectary_stocks!A:A,Insectary_stocks!C:C,"")');
    assert.deepEqual(row.doubtful.SPECIES.alternatives, [EURYDICE]);
    assert.equal(row.doubtful.SPECIES.checked, false);
    // Its siblings: H2B (the formula's proceriformis) and H1B, H3B typed as eurydice.
    assert.match(row.doubtful.SPECIES.reason, /Hermanos: 2 eurydice, 1 proceriformis$/);

    // Unchecked, nothing is written; once the person confirms it, the cell holds the formula again.
    const refused = await call('apply_proposal', { proposalId: out.proposalId });
    assert.ok(refused.doubtful || refused.error, JSON.stringify(refused));
    const applied = await call('apply_proposal', { proposalId: out.proposalId, confirmDoubtful: true });
    assert.ok(!applied.error, JSON.stringify(applied));
    const cell = sheets.rows.get('Insectary_data').find(r => r.row === 2).cells[column('Insectary_data', 'SPECIES')];
    assert.match(cell.userEnteredValue.formulaValue, /^=XLOOKUP\(C2,/);
  } finally {
    store.close();
  }
});

test('a phenotype typed over the formula, and a typed species unlike the clutch, are written as given', async () => {
  const { seed, formulas } = clutch();
  const { store, call, record } = await setup(seed, formulas);
  try {
    // proceriformis → eurydice over H2B's formula: typed over it (the existing doubt), no guard.
    const out = await call('propose_changes', {
      changes: [
        { recordId: record('Insectary_data', 4).id, values: { SPECIES: EURYDICE }, note: 'emerged as eurydice' },
        { recordId: record('Insectary_data', 5).id, values: { SPECIES: 'Mechanitis polymnia werneri' }, note: 'emerged as werneri' },
      ],
    });
    assert.ok(!out.error, JSON.stringify(out));
    assert.equal(out.speciesFromClutch, undefined);
    const full = await call('get_proposal', { proposalId: out.proposalId, full: true });
    assert.deepEqual(
      full.rows.map(r => r.values.SPECIES),
      [EURYDICE, 'Mechanitis polymnia werneri'],
    );
    assert.equal(full.rows[1].doubtful, undefined);
  } finally {
    store.close();
  }
});

/** Collection_data from Colombia, 2023, with CAM IDs without the M; photos named M6001… in a folder of 2024 butterflies. */
function colombia() {
  const base = { Release_Collect: 'Collected_Preserved', Purpose: 'Monitoring', Country: 'Colombia', SPECIES: 'Heliconius sapho' };
  const Collection_data = [];
  const Photo_links = [{ row: 2, values: { 'Full Path': 'COLOMBIA_2025', Name: 'COLOMBIA_2025', Type: 'Folder' } }];
  for (let i = 0; i < 6; i++)
    Collection_data.push({ row: 2 + i, values: { ...base, CAM_ID: 6001 + i, Collection_date: serial('2023-07-06') } });
  // 24 butterflies of 2024 already named M7000… in the folder, and the photos the renames follow.
  for (let i = 0; i < 24; i++) Collection_data.push({ row: 8 + i, values: { ...base, CAM_ID: `M${7000 + i}`, Collection_date: serial('2024-05-10') } });
  const photo = name => ({ 'Full Path': `COLOMBIA_2025/raw_2/${name}`, Name: name, Type: 'File', 'Capture Date': serial('2026-03-24') });
  for (let i = 0; i < 24; i++) Photo_links.push({ row: Photo_links.length + 2, values: photo(`M${7000 + i}d.CR2`) });
  for (let i = 0; i < 6; i++) Photo_links.push({ row: Photo_links.length + 2, values: photo(`M${6001 + i}d.CR2`) });
  return { Collection_data, Photo_links };
}

test('CAM_IDs renamed on more than a few rows come doubtful; photos in a folder of another year are told', async () => {
  const { store, call, record } = await setup(colombia());
  try {
    const rename = rows =>
      call('propose_changes', {
        reason: 'CAM_ID with its M, as the photos',
        changes: rows.map(row => ({ recordId: record('Collection_data', row).id, values: { CAM_ID: `M${6001 + row - 2}` } })),
      });
    // Three rows: as proposed.
    const few = await rename([2, 3, 4]);
    assert.ok(!few.error, JSON.stringify(few));
    assert.equal(few.idChanges, undefined);
    assert.ok((await call('get_proposal', { proposalId: few.proposalId, full: true })).rows.every(r => !r.doubtful));
    await call('update_proposal', { proposalId: few.proposalId, discard: true });

    const many = await rename([2, 3, 4, 5, 6]);
    assert.ok(!many.error, JSON.stringify(many));
    assert.equal(many.idChanges.rows, 5);
    assert.equal(many.idChanges.conflicts.length, 1);
    assert.equal(many.idChanges.conflicts[0].rows, 5);
    assert.match(many.idChanges.conflicts[0].example, /COLOMBIA_2025\/raw_2, de mariposas colectadas en 2024 \(24 de 24\); la fila es de 2023/);
    const full = await call('get_proposal', { proposalId: many.proposalId, full: true });
    assert.equal(full.rows.length, 5);
    for (const r of full.rows) {
      assert.equal(r.doubtful.CAM_ID.checked, false);
      assert.match(r.doubtful.CAM_ID.reason, /mira una foto o el sobre antes de aplicar$/);
    }
    assert.deepEqual(full.rows[0].doubtful.CAM_ID.alternatives, [6001]);
  } finally {
    store.close();
  }
});

test('tube IDs renamed on many rows without photos are doubtful too; filling empty ones is not renaming', async () => {
  const rows = Array.from({ length: 5 }, (_, i) => ({
    row: 2 + i,
    values: { Insectary_ID: `A${i}A`, Wild_Reared: 'Reared', Tube_1_id: i < 4 ? `FD4100000${i}` : null },
  }));
  const { store, call, record } = await setup({ Insectary_data: rows });
  try {
    const out = await call('propose_changes', {
      changes: [2, 3, 4, 5].map((row, i) => ({ recordId: record('Insectary_data', row).id, values: { Tube_1_id: `FD4200000${i}` } })),
    });
    assert.ok(!out.error, JSON.stringify(out));
    assert.equal(out.idChanges.rows, 4);
    assert.equal(out.idChanges.conflicts, undefined);
    const full = await call('get_proposal', { proposalId: out.proposalId, full: true });
    assert.match(full.rows[0].doubtful.Tube_1_id.reason, /^Cambia el identificador FD41000000: mira una foto o el sobre/);

    // Five empty tubes filled: no guard.
    const filled = await call('propose_changes', {
      changes: [2, 3, 4, 5, 6].map((row, i) => ({ recordId: record('Insectary_data', row).id, values: { Notes_Insectary_data: `n${i}` } })),
    });
    assert.equal(filled.idChanges, undefined);
  } finally {
    store.close();
  }
});
