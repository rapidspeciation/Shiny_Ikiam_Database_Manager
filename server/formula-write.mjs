// Formulas the assistant writes through a proposal: {"formula": "=..."} in
// propose_changes / update_proposal / bulk `set` (a plain "=..." text stays text).
// `{row}` in the text is the row's own number, so one bulk gives every row its
// formula. Checked before proposing (it starts with "=", its brackets and quotes
// close, it parses, its same-row references are columns of the sheet), written as
// a formula (userEnteredValue.formulaValue: as typed in Sheets), kept in history
// as formula text, so an undo puts back the formula or value it replaced.

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
