// Formulas the assistant writes through a proposal: {"formula": "=..."} in
// propose_changes / update_proposal / bulk `set` (a plain "=..." text stays text).
// `{row}` in the text is the row's own number, so one bulk gives every row its
// formula. Checked before proposing (it starts with "=", its brackets and quotes
// close, it parses, its same-row references are columns of the sheet), written as
// a formula (userEnteredValue.formulaValue), kept in history as formula text, so
// an undo puts back the formula or value it replaced.
//
// The Sheets API reads and writes cell formulas in English with commas
// (=IF(A2="","",XLOOKUP(…))) whatever the workbook's language: every formula the
// app reads from the team's Spanish workbook comes that way, and Sheets shows
// them in Spanish to people (SI, BUSCARX). A Spanish name or a ";" between
// arguments is refused with the English form. Functions that make Sheets
// recalculate the whole workbook after every edit (INDIRECT, OFFSET, NOW,
// TODAY, RAND, RANDBETWEEN) are refused too, with what to use instead.

import { moduleMap } from './schema.mjs';
import { parseFormula, relativeFormula } from './formula.mjs';

/** Rows a proposal may take when it only writes formulas (a column's formula fixed down the sheet). */
export const FORMULA_ROWS = 2000;

export const isFormulaValue = v => !!v && typeof v === 'object' && !Array.isArray(v) && typeof v.formula === 'string';
/** The text with `{row}` as the row's number. */
export const withRow = (text, row) => String(text).replaceAll('{row}', String(row));

/** A formula's text as compared (Sheets may change the spacing and the case of names): outside quotes, no spaces and upper case. */
export function formulaKey(text) {
  return String(text ?? '')
    .trim()
    .split('"')
    .map((part, i) => (i % 2 ? part : part.replace(/\s+/g, '').toUpperCase()))
    .join('"');
}
export const sameFormula = (a, b) => formulaKey(a) === formulaKey(b);
/** Two cells alike as the save checks them: formulas by their text as compared, the rest exactly. */
export function sameCell(a, b) {
  if (isFormulaValue(a) && isFormulaValue(b)) return sameFormula(a.formula, b.formula);
  return JSON.stringify(a ?? null) === JSON.stringify(b ?? null);
}

/** The function names of a formula, upper case, outside text in quotes. */
export function functionNames(text) {
  const outside = String(text ?? '')
    .split('"')
    .filter((_, i) => !(i % 2))
    .join('"');
  return [...outside.matchAll(/(?<![A-Za-z0-9_.$!'])([A-Za-zÁÉÍÓÚÑáéíóúñ][A-Za-z0-9_.ÁÉÍÓÚÑáéíóúñ]*)\s*\(/g)].map(m => m[1].toUpperCase());
}

/** Spanish function names (as Sheets shows them in Spanish) → the English ones the API takes. */
export const SPANISH_NAMES = {
  SI: 'IF',
  'SI.CONJUNTO': 'IFS',
  'SI.ERROR': 'IFERROR',
  'SI.ND': 'IFNA',
  Y: 'AND',
  O: 'OR',
  NO: 'NOT',
  BUSCARX: 'XLOOKUP',
  BUSCARV: 'VLOOKUP',
  COINCIDIR: 'MATCH',
  COINCIDIRX: 'XMATCH',
  INDICE: 'INDEX',
  ÍNDICE: 'INDEX',
  'CONTAR.SI': 'COUNTIF',
  'CONTAR.SI.CONJUNTO': 'COUNTIFS',
  CONTAR: 'COUNT',
  SUMA: 'SUM',
  PROMEDIO: 'AVERAGE',
  ESERROR: 'ISERROR',
  ESBLANCO: 'ISBLANK',
  ESTEXTO: 'ISTEXT',
  ESNUMERO: 'ISNUMBER',
  ESNÚMERO: 'ISNUMBER',
  HIPERVINCULO: 'HYPERLINK',
  HIPERVÍNCULO: 'HYPERLINK',
  IZQUIERDA: 'LEFT',
  DERECHA: 'RIGHT',
  EXTRAE: 'MID',
  LARGO: 'LEN',
  ENCONTRAR: 'FIND',
  HALLAR: 'SEARCH',
  MAYUSC: 'UPPER',
  MINUSC: 'LOWER',
  ESPACIOS: 'TRIM',
  CONCATENAR: 'CONCATENATE',
  TEXTO: 'TEXT',
  FECHA: 'DATE',
  FILA: 'ROW',
  CARACTER: 'CHAR',
  CODIGO: 'CODE',
  REDONDEAR: 'ROUND',
  VALOR: 'VALUE',
  INDIRECTO: 'INDIRECT',
  DESREF: 'OFFSET',
  AHORA: 'NOW',
  HOY: 'TODAY',
  ALEATORIO: 'RAND',
  'ALEATORIO.ENTRE': 'RANDBETWEEN',
};

/** Volatile functions: Sheets recalculates every cell that uses them after any edit to the workbook. */
export const VOLATILE = {
  INDIRECT: 'a fixed range (Insectary_data!$A$2:$A$30000) or INDEX over one',
  OFFSET: 'a fixed range, or INDEX(range, n) for a cell in it',
  NOW: 'the date or time typed as a value',
  TODAY: 'the date typed as a value',
  RAND: 'a number typed as a value',
  RANDBETWEEN: 'a number typed as a value',
};
/** The volatile functions a formula uses (English names, its Spanish ones included). */
export const volatileIn = text => [...new Set(functionNames(text).map(n => SPANISH_NAMES[n] ?? n))].filter(n => n in VOLATILE);

const LOOKUPS = new Set(['XLOOKUP', 'VLOOKUP', 'HLOOKUP', 'MATCH', 'XMATCH', 'INDEX', 'COUNTIF', 'COUNTIFS', 'SUMIF', 'SUMIFS', 'FILTER']);
/** How many lookups of a formula read whole columns (Insectary_data!A:A): each row of the column searches the whole sheet. */
export function wholeColumnLookups(text) {
  if (!functionNames(text).some(n => LOOKUPS.has(n))) return 0;
  const outside = String(text ?? '')
    .split('"')
    .filter((_, i) => !(i % 2))
    .join('"');
  return [...outside.matchAll(/(?<![A-Za-z0-9_$])\$?[A-Za-z]{1,3}:\$?[A-Za-z]{1,3}(?![A-Za-z0-9_(])/g)].length;
}

/** The highest column (zero-based) of a sheet, by its live header when known. */
function lastColumn(sheet, layout) {
  const columns = layout?.columns ? [...layout.columns.values()] : (moduleMap.get(sheet)?.fields ?? []).map(f => f.column);
  return columns.length ? Math.max(...columns) : -1;
}

/**
 * A formula given for `row` of `sheet`, checked: { formula } with `{row}` replaced, or { error }
 * saying what is wrong. `layout`: the sheet's live column map (store.layouts), if read.
 */
export function checkFormula(sheet, given, row, layout = null) {
  const raw = isFormulaValue(given) ? given.formula : given;
  if (typeof raw !== 'string' || !raw.trim()) return { error: 'a formula is {"formula": "=..."}' };
  const text = withRow(raw.trim(), row);
  if (!text.startsWith('=')) return { error: `a formula starts with "=" (${clip(text)})` };
  if (/\{row\}/i.test(text) || /\{[a-z]+\}/i.test(text.split('"').filter((_, i) => !(i % 2)).join('')))
    return { error: `only {row} is replaced in a formula (${clip(text)})` };
  if ((text.match(/"/g) ?? []).length % 2) return { error: `a quote is not closed in ${clip(text)}` };
  let depth = 0;
  for (const [i, part] of text.split('"').entries()) {
    if (i % 2) continue;
    for (const ch of part) {
      if (ch === '(') depth++;
      if (ch === ')' && --depth < 0) return { error: `a ")" without its "(" in ${clip(text)}` };
    }
  }
  if (depth) return { error: `${depth} "(" not closed in ${clip(text)}` };
  // As the Sheets API takes it: English names, commas.
  const spanish = [...new Set(functionNames(text))].filter(n => SPANISH_NAMES[n]);
  if (spanish.length)
    return {
      error: `the Sheets API takes function names in English: ${spanish.map(n => `${SPANISH_NAMES[n]} for ${n}`).join(', ')} (${clip(text)})`,
    };
  if (text.split('"').some((part, i) => !(i % 2) && part.includes(';')))
    return { error: `the Sheets API takes commas between arguments, not ";" (${clip(text)})` };
  const volatile = volatileIn(text);
  if (volatile.length)
    return {
      error: `${volatile.join(', ')} make${volatile.length > 1 ? '' : 's'} Sheets recalculate the whole workbook after every edit: use ${volatile.map(n => VOLATILE[n]).join('; ')} (${clip(text)})`,
    };
  let tree;
  try {
    tree = parseFormula(relativeFormula(text, row));
  } catch (e) {
    return { error: `the formula does not read: ${e.message} (${clip(text)})` };
  }
  const last = lastColumn(sheet, layout);
  const outside = [];
  const walk = node => {
    if (!node || typeof node !== 'object') return;
    if ((node.t === 'ref' && !node.sheet && node.col > last) || (node.t === 'range' && !node.sheet && Math.max(node.c1, node.c2) > last))
      outside.push(letters(node.t === 'ref' ? node.col : Math.max(node.c1, node.c2)));
    for (const child of [node.a, node.b, ...(node.args ?? [])]) walk(child);
  };
  walk(tree);
  if (outside.length) return { error: `column ${outside[0]} is not one of ${sheet}'s columns (${clip(text)})` };
  return { formula: text };
}

const clip = text => (text.length > 120 ? `${text.slice(0, 117)}...` : text);
function letters(index) {
  let n = index + 1,
    out = '';
  while (n) {
    n--;
    out = String.fromCharCode(65 + (n % 26)) + out;
    n = Math.floor(n / 26);
  }
  return out;
}
