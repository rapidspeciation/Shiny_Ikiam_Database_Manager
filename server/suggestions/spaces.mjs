// Spaces before or after a value where they change its meaning to a lookup or a
// list: " NA", an ID or a tube with a space ("FS63714012 "), a list value with a
// space the list does not have. Notes are left alone, and so are values whose
// list has the same space (the Lists sheet would need fixing first).

import { msg, tpl } from '../messages.mjs';
import { listOptions } from '../verify.mjs';
import { TUBE_FIELD } from '../verifications.mjs';

const ID_FIELD = /(^|_)(ID|id)$|CAM|^Insectary_ID$|^FieldMark_ID$/;
const NOTES = /notes?/i;

export default {
  id: 'spaces',
  title: tpl('Espacios de más'),
  describe: tpl(
    'Valores con espacios al inicio o al final que cambian lo que la hoja lee: « NA», un ID o un tubo con un espacio, o un valor de una lista que en la lista no lleva ese espacio. Las notas no se tocan. Seguro: solo se quitan los espacios.',
  ),
  suggest(ctx) {
    const out = [];
    for (const [sheet, rows] of ctx.sheets) {
      const lists = listOptions(ctx.store, sheet);
      for (const row of rows) {
        if (!row.observed) continue;
        for (const [field, value] of Object.entries(row.values)) {
          if (typeof value !== 'string' || row.formulas[field] || NOTES.test(field)) continue;
          const trimmed = value.trim();
          if (!trimmed || trimmed === value) continue;
          const list = lists[field]?.values;
          // A strict list's value with spaces is already a check with its fix (check_fixes).
          if (list && lists[field].strict && !list.has(value)) continue;
          const fits =
            /^NA$/i.test(trimmed) ||
            TUBE_FIELD.test(field) ||
            (ID_FIELD.test(field) && /\d/.test(trimmed)) ||
            (list && list.has(trimmed) && !list.has(value));
          if (!fits) continue;
          out.push({
            sheet,
            row: row.row,
            recordId: row.id,
            label: row.label,
            field,
            current: value,
            suggested: trimmed,
            certainty: 'certain',
            reason: msg('«{value}» lleva espacios que la hoja cuenta como parte del valor', { value }),
          });
        }
      }
    }
    return out;
  },
};
