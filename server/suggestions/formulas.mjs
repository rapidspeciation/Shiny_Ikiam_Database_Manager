// Cells lacking the formula the team copies down their column
// (server/formula-patterns.mjs): in the last year a large majority of the rows
// of the same kind have it (Collection_data's Death/Preservation lookups on the
// Collected_Sent2Insectary rows, Data_entry_order, Insectary_stocks' emergence
// lookups…), plus CAM_ID_insectary of butterflies sent to the insectary, by
// rule. Listed: a blank cell, a placeholder (NA) or a typed value the formula
// would give anyway; a typed value that differs is someone's decision and is
// left alone. Each suggestion is the formula moved to the row and what it gives
// today (worked out by server/formula-eval.mjs); certain when the cell is blank
// and the column is near-universally the formula, likely when a typed NA would
// change, check otherwise. A formula is copied by hand in Google Sheets
// (dragging), so they are `manual`; rows the app creates already get them.

import { msg, tpl } from '../messages.mjs';
import { moduleMap } from '../schema.mjs';
import { SCAN_ROWS, detectPatterns, formulaAt, isPlaceholder } from '../formula-patterns.mjs';
import { evaluateFormula, isFormulaError, rowsBook, sameShown } from '../formula-eval.mjs';
import { relativeFormula } from '../sheets.mjs';

const text = value => (value === null || value === undefined ? '' : String(value).trim());
const letterIndex = letters => [...letters].reduce((n, c) => n * 26 + c.charCodeAt(0) - 64, 0) - 1;
/** Lookups by a cell of the row itself: XLOOKUP(D12,…), VLOOKUP(A12,…), MATCH(A12,…), COUNTIF(…, A12). */
const LOOKUP_KEYS = [
  /\b(?:XLOOKUP|VLOOKUP|MATCH)\(\s*\$?([A-Z]{1,3})\$?(\d+)\s*,/gi,
  /\bCOUNTIF\([^,()]+,\s*\$?([A-Z]{1,3})\$?(\d+)\s*\)/gi,
];

const profileColumns = new Map(
  [...moduleMap.values()].map(m => [m.id, new Map(m.fields.filter(f => !f.readonly).map(f => [f.key, f.column]))]),
);

export default {
  id: 'formulas',
  title: tpl('Fórmulas que faltan'),
  describe: tpl(
    'Celdas sin la fórmula que el equipo copia hacia abajo en su columna: en el último año casi todas las filas del mismo tipo la tienen (p. ej. las búsquedas de Death_date y Preservation_* en las filas Collected_Sent2Insectary, Data_entry_order, la emergencia de los clutches en Insectary_stocks), y CAM_ID_insectary de las mariposas enviadas al insectario, que lee su CAM de Insectary_data. Se muestra la fórmula para esa fila y lo que daría hoy. Seguro: la celda está vacía y la columna casi siempre tiene la fórmula; probable: un NA escrito que la fórmula cambiaría; revisar: lo demás. Se copian a mano en Google Sheets (arrastrando); las filas nuevas que crea la app ya las llevan.',
  ),
  /** Listed by sheet and column (group), with how many each has. */
  byGroup: true,
  suggest(ctx) {
    const columnsOf = sheet => ctx.store.layouts?.get(sheet)?.columns ?? profileColumns.get(sheet) ?? null;
    const book = rowsBook(ctx.sheets, { columnsOf, headerRow: sheet => moduleMap.get(sheet)?.headerRow ?? 1 });
    const out = [];
    for (const [sheet, rows] of ctx.sheets) {
      if (!moduleMap.has(sheet)) continue;
      const observed = rows.filter(r => r.observed).slice(-SCAN_ROWS);
      const patterns = detectPatterns(sheet, observed, { today: ctx.today, columns: columnsOf });
      // In sheet order: a formula reading another row (Data_entry_order reads the one above) sees what
      // that row would give once filled; the rows that have it are worked out again for the same reason.
      const jobs = [];
      for (const p of patterns) {
        const readsOtherRows = /\{[-+][1-9]/.test(p.shape);
        const first = p.window.find(r => !r.formulas[p.field])?.row ?? Infinity;
        for (const r of p.window)
          if (!r.formulas[p.field]) jobs.push([r, p, false]);
          else if (readsOtherRows && r.row > first && relativeFormula(r.formulas[p.field], r.row) === p.shape)
            jobs.push([r, p, true]);
      }
      jobs.sort((a, b) => a[0].row - b[0].row);
      for (const [row, p, again] of jobs) {
        if (again) {
          const column = columnsOf(sheet)?.get(p.field);
          const gives = evaluateFormula(row.formulas[p.field], { sheet, row: row.row, book });
          if (gives !== undefined && !isFormulaError(gives) && column !== undefined)
            book.overlay.set(`${sheet}\u0000${row.row}\u0000${column}`, gives);
          continue;
        }
        const s = suggestion(ctx, book, columnsOf, sheet, row, p);
        if (s) out.push(s);
      }
    }
    return out;
  },
};

function suggestion(ctx, book, columnsOf, sheet, row, p) {
  const value = row.values[p.field];
  const state = text(value) === '' ? 'blank' : isPlaceholder(value) ? 'placeholder' : 'typed';
  const formula = formulaAt(p, row.row);
  // A lookup by an empty or NA key would find another row's "NA" (the Panama rows of Insectary_data).
  for (const re of LOOKUP_KEYS)
    for (const m of formula.matchAll(re)) {
      if (Number(m[2]) !== row.row) continue;
      const key = book.cell(sheet, row.row, letterIndex(m[1].toUpperCase()));
      if (text(key) === '' || isPlaceholder(key)) return null;
    }
  const gives = evaluateFormula(formula, { sheet, row: row.row, book });
  let certainty;
  if (gives !== undefined && isFormulaError(gives)) {
    if (state === 'typed') return null;
    certainty = 'check';
  } else if (state === 'blank') certainty = p.universal ? 'certain' : 'check';
  else if (gives === undefined) {
    if (state === 'typed') return null;
    certainty = 'check';
  } else if (sameShown(value, gives)) certainty = p.universal ? 'certain' : 'check';
  else if (state === 'placeholder') certainty = p.universal ? 'likely' : 'check';
  else return null;
  // What a later row reading this cell would see once it is filled.
  const column = columnsOf(sheet)?.get(p.field);
  if (gives !== undefined && column !== undefined) book.overlay.set(`${sheet}\u0000${row.row}\u0000${column}`, gives);
  const shownValue =
    gives === null || text(gives) === ''
      ? msg('vacío')
      : isFormulaError(gives)
        ? gives.error
        : String(ctx.shown(sheet, p.field, gives));
  const vars = {
    n: p.counts.formula,
    total: p.counts.rows,
    kind: p.kind ? ` ${p.kind}` : '',
    example: p.example.row,
    value: shownValue,
  };
  return {
    sheet,
    row: row.row,
    recordId: row.id,
    label: row.label,
    field: p.field,
    current: state === 'blank' ? null : ctx.shown(sheet, p.field, value),
    suggested: formula,
    certainty,
    // A formula is copied down in Google Sheets: the app's proposals write values only.
    manual: true,
    group: `${sheet} · ${p.field}${p.kind ? ` · ${p.kind}` : ''}`,
    reason: p.explicit
      ? msg(
          'regla del equipo: CAM_ID_insectary lee el CAM de la mariposa en Insectary_data (vacío mientras vive, su CAM al preservarse, NA si murió sin preservarse); hoy daría {value}',
          { value: shownValue },
        )
      : gives === undefined
        ? msg(
            '{n} de {total} filas{kind} del último año tienen esta fórmula (la última, fila {example}); lo que daría no se calcula aquí',
            vars,
          )
        : msg(
            '{n} de {total} filas{kind} del último año tienen esta fórmula (la última, fila {example}); hoy daría {value}',
            vars,
          ),
  };
}
