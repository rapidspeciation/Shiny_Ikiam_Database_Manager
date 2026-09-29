// Which sheet column holds each field, read from the live header row.
//
// The workbook profile (docs/workbook-schema.json) says which fields a sheet
// has, not where they are: people move, insert and rename columns in Google
// Sheets. Every read and write maps fields to columns by the header names read
// with the rows, so a moved column keeps working, a new column is ignored (and
// never written), and a field whose column disappeared is only unavailable.

import { moduleMap } from './schema.mjs';

export const headerText = cell =>
  String(
    cell?.effectiveValue?.stringValue ??
      cell?.userEnteredValue?.stringValue ??
      cell?.effectiveValue?.numberValue ??
      cell?.userEnteredValue?.numberValue ??
      '',
  ).trim();

/** A, B, …, Z, AA, … for a zero-based column index. */
export function columnLetter(index) {
  let n = index + 1,
    out = '';
  while (n) {
    n--;
    out = String.fromCharCode(65 + (n % 26)) + out;
    n = Math.floor(n / 26);
  }
  return out;
}

/**
 * The column map of a sheet from its live header row.
 * Returns { sheet, columns: Map(field → index), missing, extra, duplicates,
 * blocked, problems }. `problems` is what admins see; items with `blocking`
 * stop reads and writes of the whole sheet:
 *  - a known header written twice (more times than the profile has it): which
 *    column holds the field is ambiguous;
 *  - an identity column missing: rows could no longer be matched to records;
 *  - most headers unrecognized (e.g. a row inserted above the header).
 */
export function headerLayout(sheet, headerRow) {
  const mod = moduleMap.get(sheet);
  if (!mod) throw Object.assign(new Error(`Unknown sheet: ${sheet}`), { code: 'MODULE_NOT_FOUND', status: 404 });
  const found = new Map();
  (headerRow?.cells || []).forEach((cell, index) => {
    const name = headerText(cell);
    if (name) found.set(name, [...(found.get(name) || []), index]);
  });
  // The profile itself repeats a few headers (Sperm_dissections F_Tube_1_id): the
  // first one is the field, later copies are read-only duplicates of it.
  const expected = new Map();
  for (const f of mod.fields) expected.set(f.key, (expected.get(f.key) || 0) + 1);
  const columns = new Map();
  const missing = [];
  const duplicates = [];
  for (const [key, count] of expected) {
    const at = found.get(key) || [];
    if (!at.length) missing.push(key);
    else if (at.length > count) duplicates.push({ field: key, columns: at });
    else columns.set(key, at[0]);
  }
  const known = new Set(expected.keys());
  const extra = [...found]
    .filter(([name]) => !known.has(name))
    .flatMap(([name, at]) => at.map(index => ({ field: name, column: index })))
    .sort((a, b) => a.column - b.column);
  const problems = [];
  const unreadable = columns.size < Math.ceil(expected.size / 2);
  if (unreadable) problems.push({ kind: 'header', field: null, blocking: true });
  for (const d of duplicates)
    problems.push({ kind: 'duplicate', field: d.field, columns: d.columns.map(columnLetter), blocking: true });
  for (const key of missing)
    problems.push({ kind: 'missing', field: key, ...(mod.identityFields.includes(key) ? { blocking: true } : {}) });
  for (const e of extra) problems.push({ kind: 'new', field: e.field, column: columnLetter(e.column) });
  return {
    sheet,
    columns,
    missing,
    extra,
    duplicates,
    blocked: problems.some(p => p.blocking),
    problems,
  };
}

/** The layout the profile describes, for local test sheets built from it. */
export function profileLayout(sheet) {
  const mod = moduleMap.get(sheet);
  const cells = [];
  for (const f of mod.fields) cells[f.column] ??= { userEnteredValue: { stringValue: f.key } };
  return headerLayout(sheet, { cells });
}

/** Same column map (so rows read with one layout can be compared with another). */
export function sameLayout(a, b) {
  if (!a || !b || a.columns.size !== b.columns.size) return false;
  for (const [key, index] of a.columns) if (b.columns.get(key) !== index) return false;
  return true;
}

/** One line per problem, in Spanish, for errors and logs. */
export function describeProblems(sheet, problems) {
  const text = p =>
    p.kind === 'header'
      ? `no se reconoce la fila de encabezados de ${sheet}`
      : p.kind === 'duplicate'
        ? `la columna ${p.field} aparece dos veces (${p.columns.join(', ')})`
        : p.kind === 'missing'
          ? `falta la columna ${p.field}`
          : `columna nueva ${p.field} en ${p.column} (ignorada)`;
  return problems.map(text).join('; ');
}
