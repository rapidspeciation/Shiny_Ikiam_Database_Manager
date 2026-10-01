// Pedigree left as the formula's "YES or NO" on butterflies that are dead or
// preserved (Insectary_data). The formula gives it when Research_purpose is a
// crosses purpose and a person types Yes or No over it; only PAS and the crosses
// team decide it. What the workbook shows about the butterfly is sorted into
// evidence classes: F1/F2_MutationRate has its row (same Insectary_ID and
// CAM_ID), has a row with its Insectary_ID only; else it is wild, its clutch
// is a cross generation in Insectary_stocks (F1, F2, Backcross), a stock clutch
// (Generation NA), or a clutch without Generation. How sure a
// suggestion is comes from the rows already decided: in each class, how often
// people chose Yes or No, measured on the data every time (the reason says
// it). A class decided the same way 95 % of the time (50+ rows) is likely;
// 75 % (20+ rows) is a lead to check; otherwise no value is suggested. "YES"
// or "yes" typed for Yes is certain.

import { msg, tpl } from '../messages.mjs';

const text = value => (value === null || value === undefined ? '' : String(value).trim());
const upper = value => text(value).toUpperCase();
const isDate = value => typeof value === 'number' && Number.isFinite(value) && value > 0;
const TEMPLATE = 'YES or NO';
const CROSS_GENERATION = /^(F1|F2|Backcross)$/i;
const isCam = value => /^CAM\d+$/i.test(text(value));

/** Why the class says what it says (Spanish, with the values of this row). */
function evidence(kind, row, link) {
  if (kind === 'cam')
    return msg('F1/F2_MutationRate fila {row} es esta mariposa ({id}, {cam})', {
      row: link.row,
      id: text(row.values.Insectary_ID),
      cam: text(row.values.CAM_ID),
    });
  if (kind === 'id')
    return msg('F1/F2_MutationRate fila {row} tiene su Insectary_ID {id}, con otro CAM ({cam})', {
      row: link.row,
      id: text(row.values.Insectary_ID),
      cam: text(link.values.CAM_ID) || '—',
    });
  if (kind === 'generation')
    return msg('su clutch {clutch} es {generation} en Insectary_stocks', {
      clutch: text(row.values['CLUTCH NUMBER']),
      generation: text(link),
    });
  if (kind === 'wild') return msg('silvestre, no está en F1/F2_MutationRate');
  if (kind === 'stock') return msg('no está en F1/F2_MutationRate; su clutch {clutch} es de stock (Generation NA)', { clutch: link });
  return msg('no está en F1/F2_MutationRate y su clutch no tiene Generation en Insectary_stocks');
}

export default {
  id: 'pedigree',
  title: tpl('Pedigree sin decidir'),
  describe: tpl(
    'Mariposas muertas o preservadas de Insectary_data con Pedigree «YES or NO» (la fórmula espera que alguien escriba Yes o No). Se sugiere lo que dice el libro: si está en F1/F2_MutationRate (mismo Insectary_ID y CAM), solo con su Insectary_ID, o si su clutch es F1, F2 o Backcross. La certeza sale de cómo se decidieron las filas ya decididas con la misma evidencia; sin evidencia no se sugiere valor. Lo deciden PAS y el equipo de cruces.',
  ),
  suggest(ctx) {
    const pedigreeRows = new Map();
    for (const r of ctx.observed('F1/F2_MutationRate')) {
      const id = upper(r.values.Insectary_ID);
      if (id && id !== 'NA') (pedigreeRows.get(id) || pedigreeRows.set(id, []).get(id)).push(r);
    }
    const generation = new Map();
    for (const r of ctx.observed('Insectary_stocks')) generation.set(upper(r.values['CLUTCH NUMBER']), text(r.values.Generation));
    const classify = row => {
      const found = pedigreeRows.get(upper(row.values.Insectary_ID)) || [];
      const same = isCam(row.values.CAM_ID) && found.find(r => upper(r.values.CAM_ID) === upper(row.values.CAM_ID));
      if (same) return { kind: 'cam', link: same };
      if (found.length) return { kind: 'id', link: found[0] };
      if (!/^Reared$/i.test(text(row.values.Wild_Reared))) return { kind: 'wild' };
      const gen = generation.get(upper(row.values['CLUTCH NUMBER']));
      if (CROSS_GENERATION.test(gen ?? '')) return { kind: 'generation', link: gen };
      if (/^NA$/i.test(gen ?? '')) return { kind: 'stock', link: text(row.values['CLUTCH NUMBER']) };
      return { kind: 'none' };
    };
    const dead = row => isDate(row.values.Death_date) || isDate(row.values.Preservation_date);
    const insectary = ctx.observed('Insectary_data');

    // How the dead rows already decided were decided, per class (only rows whose purpose asks for it).
    const decided = { cam: {}, id: {}, wild: {}, generation: {}, stock: {}, none: {} };
    for (const row of insectary) {
      const value = text(row.values.Pedigree);
      if (!/^(yes|no)$/i.test(value) || !dead(row)) continue;
      if (!/cross|mutation/i.test(text(row.values.Research_purpose))) continue;
      const tally = decided[classify(row).kind];
      const v = value.toLowerCase() === 'yes' ? 'Yes' : 'No';
      tally[v] = (tally[v] ?? 0) + 1;
    }

    const out = [];
    for (const row of insectary) {
      const value = text(row.values.Pedigree);
      const base = {
        sheet: row.sheet,
        row: row.row,
        recordId: row.id,
        label: row.label,
        field: 'Pedigree',
        current: value,
        // Typed over the formula in Google Sheets: the app does not write formula cells.
        ...(row.formulas.Pedigree ? { manual: true } : {}),
      };
      const cased = /^(yes|no)$/i.test(value) && value !== 'Yes' && value !== 'No';
      if (cased) {
        const right = value.toLowerCase() === 'yes' ? 'Yes' : 'No';
        out.push({ ...base, suggested: right, certainty: 'certain', reason: msg('«{value}» es {right} escrito en otra forma', { value, right }) });
        continue;
      }
      if (value !== TEMPLATE || !dead(row)) continue;
      const { kind, link } = classify(row);
      const tally = decided[kind];
      const n = (tally.Yes ?? 0) + (tally.No ?? 0);
      const top = (tally.Yes ?? 0) >= (tally.No ?? 0) ? 'Yes' : 'No';
      const k = tally[top] ?? 0;
      const share = n ? k / n : 0;
      const why = evidence(kind, row, link);
      const related = kind === 'cam' || kind === 'id' ? [ctx.ref(link, 'Insectary_ID')] : undefined;
      const lean = (n >= 50 && share >= 0.95) || (n >= 20 && share >= 0.75);
      out.push({
        ...base,
        suggested: lean ? top : null,
        certainty: n >= 50 && share >= 0.95 ? 'likely' : 'check',
        reason: n
          ? msg('{why}; en las filas ya decididas con lo mismo, {value} en {k} de {n}', { why, value: top, k, n })
          : msg('{why}; ninguna fila decidida con lo mismo', { why }),
        ...(related ? { related } : {}),
      });
    }
    return out;
  },
};
