// Collection_data rows of butterflies sent to the insectary (Collected_Sent2Insectary)
// whose Death_date, Preservation_date, Preservation_medium or Preserved_dead_alive
// is empty where the rows before have the formula that reads it from the
// butterfly's Insectary_data row (=XLOOKUP(D…, Insectary_data!A:A, Insectary_data!I:I,"")).
// Copying the formula down is certain; the suggestion shows the formula for this
// row and what it gives today. Rows without an Insectary_ID are left out: the
// lookup would find another butterfly's "NA" row.

import { msg, tpl } from '../messages.mjs';
import { moduleMap } from '../schema.mjs';
import { isIdValue } from '../verifications.mjs';

const FIELDS = ['Death_date', 'Preservation_date', 'Preservation_medium', 'Preserved_dead_alive'];
const SENT = 'Collected_Sent2Insectary';
const LOOKUP = /^=XLOOKUP\(\s*D(\d+)\s*,\s*Insectary_data!A:A\s*,\s*Insectary_data!([A-Z]+):\2\s*,\s*""\s*\)$/i;
/** How far back a row with the formula counts (older rows were typed by hand). */
const WINDOW = 500;

const letterIndex = letters => [...letters.toUpperCase()].reduce((n, c) => n * 26 + c.charCodeAt(0) - 64, 0) - 1;
const text = value => (value === null || value === undefined ? '' : String(value).trim());

export default {
  id: 'formulas',
  title: tpl('Fórmulas que faltan'),
  describe: tpl(
    'Filas de Collection_data enviadas al insectario (Collected_Sent2Insectary) con Death_date, Preservation_date, Preservation_medium o Preserved_dead_alive vacíos donde las filas anteriores tienen la fórmula que los lee de Insectary_data. Seguro: copiar la fórmula de la fila de arriba; se muestra lo que daría hoy.',
  ),
  suggest(ctx) {
    const rows = ctx.sheets.get('Collection_data') ?? [];
    // The Insectary_data column each letter is (the live header when synced, else the profile).
    const layout = ctx.store.layouts?.get('Insectary_data');
    const columns = new Map();
    for (const f of moduleMap.get('Insectary_data').fields) columns.set(layout?.columns?.get(f.key) ?? f.column, f.key);
    const insectary = new Map();
    for (const r of ctx.observed('Insectary_data')) {
      const id = text(r.values.Insectary_ID).toUpperCase();
      if (isIdValue(id) && !insectary.has(id)) insectary.set(id, r);
    }
    const last = Object.fromEntries(FIELDS.map(f => [f, null]));
    const out = [];
    for (const row of rows) {
      if (!row.observed || text(row.values.Release_Collect) !== SENT) continue;
      for (const field of FIELDS) {
        const formula = row.formulas[field];
        const m = formula && LOOKUP.exec(formula);
        if (m) {
          last[field] = { row, letter: m[2].toUpperCase() };
          continue;
        }
        const from = last[field];
        const value = row.values[field];
        if (!from || formula || row.row - from.row.row > WINDOW || (value !== null && value !== undefined && text(value) !== ''))
          continue;
        const id = text(row.values.Insectary_ID).toUpperCase();
        if (!isIdValue(id)) continue;
        const target = columns.get(letterIndex(from.letter));
        const twin = insectary.get(id);
        const gives = twin && target ? ctx.shown('Insectary_data', target, twin.values[target]) : null;
        out.push({
          sheet: row.sheet,
          row: row.row,
          recordId: row.id,
          label: row.label,
          field,
          current: null,
          suggested: `=XLOOKUP(D${row.row}, Insectary_data!A:A, Insectary_data!${from.letter}:${from.letter},"")`,
          certainty: 'certain',
          // A formula is copied down in Google Sheets: the app writes values only.
          manual: true,
          reason:
            gives === null || gives === undefined || text(gives) === ''
              ? msg('falta la fórmula de la fila {from}; hoy daría vacío ({id} no tiene {target} en Insectary_data)', {
                  from: from.row.row,
                  id,
                  target: target ?? from.letter,
                })
              : msg('falta la fórmula de la fila {from}; hoy daría {value} ({target} de {id} en Insectary_data)', {
                  from: from.row.row,
                  value: String(gives),
                  target,
                  id,
                }),
          ...(twin ? { related: [ctx.ref(twin, target ?? 'Insectary_ID')] } : {}),
        });
      }
    }
    return out;
  },
};
