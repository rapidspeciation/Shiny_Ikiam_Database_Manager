// Correcting an Insectary ID after saving (e.g. the wings carry N9D but the row
// was saved as N7D). In Insectary_data the ID belongs to its row (pre-made rows,
// some with a formula), so the butterfly's data moves to the row of the right
// ID: into a free pre-made row ("move"), or exchanged with the butterfly that
// holds it ("swap"). Every other sheet that names the butterfly by its ID is
// updated too. All of it is one save, so it can be undone from Historial.

import { applyBatch, TYPED_OVER_FORMULA } from './batch.mjs';
import { moduleMap } from './schema.mjs';

const fail = (code, message, status = 400) => Object.assign(new Error(message), { code, status });
const text = value => (value === null || value === undefined ? '' : String(value).trim());
const blank = value => /^(|NA|N\/A)$/i.test(text(value));
const norm = value => text(value).toUpperCase();
const same = (a, b) => JSON.stringify(a ?? null) === JSON.stringify(b ?? null);

/** Columns that refer to a butterfly by its Insectary ID. */
export const REFERENCES = {
  Collection_data: ['Insectary_ID'],
  'F1/F2_MutationRate': [
    'Insectary_ID',
    'Mother_ID',
    'Father_ID',
    'M_grand_mother_ID',
    'M_grand_father_ID',
    'F_grand_mother_ID',
    'F_grand_father_ID',
  ],
  Melinaea_crosses: ['Female', 'Male'],
  Melinaea_eggs: ['Mother ID', 'Father ID'],
  Crosses_Lys_x_Pol: ['female Id', 'male Id'],
  Hybrid_Attempts: ['Insectary_ID', 'Mate_with'],
  Stocks_Matings: ['male_ID', 'female_ID'],
};

function records(store, sheet) {
  const mod = moduleMap.get(sheet);
  if (!mod) return [];
  return store.db
    .prepare(
      'SELECT id,row_num,observed,version,values_json,formulas_json FROM records WHERE sheet=? AND missing=0 AND row_num>? AND row_num<2000000000',
    )
    .all(sheet, mod.headerRow)
    .map(r => ({
      id: r.id,
      row: r.row_num,
      observed: !!r.observed,
      version: r.version,
      values: JSON.parse(r.values_json),
      formulas: JSON.parse(r.formulas_json),
    }));
}

/** What changing `from` into `to` would do: the rows and cells, and the edits to save. */
export function planIdChange(store, fromInput, toInput) {
  const from = norm(fromInput);
  const to = norm(toInput);
  if (!from || !to) throw fail('INVALID_ID', 'Escribe el Insectary ID actual y el correcto');
  if (from === to) throw fail('INVALID_ID', 'El ID nuevo es igual al actual');
  const insectary = records(store, 'Insectary_data');
  const holding = id => insectary.filter(r => norm(r.values.Insectary_ID) === id);
  const sources = holding(from).filter(r => r.observed);
  if (!sources.length) throw fail('ID_NOT_RECORDED', `${from} no está registrado en Insectary_data`, 404);
  if (sources.length > 1) throw fail('IDENTITY_CONFLICT', `Hay ${sources.length} filas con ${from} en Insectary_data`, 409);
  const targets = holding(to);
  if (!targets.length) throw fail('NO_PREMADE_ROW', `${to} no tiene fila preparada en Insectary_data`, 404);
  if (targets.length > 1) throw fail('IDENTITY_CONFLICT', `Hay ${targets.length} filas con ${to} en Insectary_data`, 409);
  const [source] = sources;
  const [target] = targets;
  const mode = target.observed ? 'swap' : 'move';
  const typedOver = TYPED_OVER_FORMULA.Insectary_data;

  const sourceEdit = { values: {}, expected: {}, replaceFormula: [] };
  const targetEdit = { values: {}, expected: {}, replaceFormula: [] };
  const moved = [];
  const skipped = [];
  for (const { key } of moduleMap.get('Insectary_data').fields) {
    if (key === 'Insectary_ID') continue;
    const sFormula = !!source.formulas[key];
    const tFormula = !!target.formulas[key];
    const sValue = source.values[key] ?? null;
    const tValue = target.values[key] ?? null;
    if (mode === 'move') {
      // A formula in the old row computes its own value; the new row computes it from the moved data.
      if (sFormula || blank(sValue)) continue;
      if (tFormula) {
        // Only a species or location typed over its formula (a different subspecies emerged) is carried over.
        if (typedOver.has(key) && !same(sValue, tValue)) {
          targetEdit.values[key] = sValue;
          targetEdit.expected[key] = tValue;
          targetEdit.replaceFormula.push(key);
        } else if (!typedOver.has(key)) skipped.push(key);
      } else {
        targetEdit.values[key] = sValue;
        targetEdit.expected[key] = tValue;
      }
      sourceEdit.values[key] = null;
      sourceEdit.expected[key] = sValue;
      moved.push(key);
    } else {
      if (sFormula || tFormula) {
        if (!(sFormula && tFormula) && !same(sValue, tValue)) skipped.push(key);
        continue;
      }
      if (same(sValue, tValue)) continue;
      targetEdit.values[key] = sValue;
      targetEdit.expected[key] = tValue;
      sourceEdit.values[key] = tValue;
      sourceEdit.expected[key] = sValue;
      moved.push(key);
    }
  }
  const edits = [];
  if (Object.keys(targetEdit.values).length) edits.push({ id: target.id, ...targetEdit });
  if (Object.keys(sourceEdit.values).length) edits.push({ id: source.id, ...sourceEdit });

  // Every other sheet that names either butterfly.
  const newId = text(target.values.Insectary_ID);
  const oldId = text(source.values.Insectary_ID);
  const references = [];
  for (const [sheet, fields] of Object.entries(REFERENCES)) {
    for (const r of records(store, sheet)) {
      if (!r.observed) continue;
      const values = {};
      const expected = {};
      for (const field of fields) {
        if (r.formulas[field]) continue;
        const current = norm(r.values[field]);
        const replacement = current === from ? newId : mode === 'swap' && current === to ? oldId : null;
        if (replacement === null) continue;
        values[field] = replacement;
        expected[field] = r.values[field];
        references.push({ sheet, row: r.row, field, before: text(r.values[field]), after: replacement });
      }
      if (Object.keys(values).length) edits.push({ id: r.id, values, expected });
    }
  }
  return {
    mode,
    from: oldId,
    to: newId,
    source: { row: source.row, species: text(source.values.SPECIES), sex: text(source.values.Sex) },
    target: {
      row: target.row,
      species: text(target.values.SPECIES),
      sex: text(target.values.Sex),
    },
    moved,
    skipped,
    references,
    edits,
  };
}

/** Saves the change as one action (undoable from Historial). */
export async function applyIdChange(store, body, user) {
  const plan = planIdChange(store, body.from, body.to);
  const reason =
    plan.mode === 'move'
      ? `Cambiar Insectary ID ${plan.from} → ${plan.to}`
      : `Intercambiar Insectary ID ${plan.from} ↔ ${plan.to}`;
  const result = await applyBatch(store, { requestId: body.requestId, reason, edits: plan.edits }, user, { source: 'app' });
  return { ...result, plan: { ...plan, edits: undefined } };
}
