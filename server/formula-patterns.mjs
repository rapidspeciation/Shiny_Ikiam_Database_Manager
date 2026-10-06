// The formulas the team copies down a column: where, in the last year, a large
// majority of the rows of one kind (Collection_data's Release_Collect,
// Insectary_data's Wild_Reared; every row elsewhere) hold the same formula,
// compared in relative form (each row's own row number taken out, as R1C1
// does, spacing and case aside). What the column is meant to hold, used:
//  - Revisión → Sugerencias lists the rows of that kind lacking it
//    (server/suggestions/formulas.mjs), and a proposal can fill them
//    (propose_changes missingFormulas);
//  - a row the app creates gets those formulas, moved to its row
//    (newRowFormulas, used by server/batch.mjs), so new rows never widen the gap;
//  - a formula the assistant proposes for a column is compared with it
//    (formulaNotes): another formula, and lookups over whole columns, are said
//    in the tool's answer and in the table.
// A pattern using volatile functions (INDIRECT, OFFSET, NOW…: the whole workbook
// recalculates after every edit) is kept as found but never copied by the app.
//
// Left out: columns where a formula is typed over on purpose (TYPED_OVER_FORMULA:
// SPECIES when what emerged differs from the clutch), Pedigree (its own
// suggestion source), the Insectary_ID series (server/premade.mjs) and counts
// kept as sums (=12+15).

import { SUM_FIELDS, TYPED_OVER_FORMULA, moduleMap } from './schema.mjs';
import { shiftFormula } from './sheets.mjs';
import { relativeFormula } from './formula.mjs';
import { formulaKey, volatileIn, wholeColumnLookups } from './formula-write.mjs';
import { todaySerial } from './checks.mjs';

/** The column that tells rows of different kinds apart (their formulas differ). */
export const KIND_FIELD = { Collection_data: 'Release_Collect', Insectary_data: 'Wild_Reared' };
/** The date that places a row in time; sheets without one use their last WITHOUT_DATE rows. */
const DATE_FIELD = {
  Collection_data: 'Collection_date',
  Insectary_data: 'Intro2Insectary_date',
  Insectary_stocks: 'DATE LAID ',
};
const WITHOUT_DATE = 500;
/** Rows read: more than a year of any sheet. */
export const SCAN_ROWS = 3000;
const YEAR = 365;
/** A run of dated rows older than a year this long ends the window (one mistyped year does not). */
const OLD_RUN = 20;

/** A pattern needs this many rows with it, this share of the filled cells and of all the rows. */
const MIN_ROWS = 10;
const SHARE_FILLED = 0.8;
const SHARE_ALL = 0.3;
/** Near-universal: a blank cell of that kind is certainly a missing formula… */
const UNIVERSAL = 0.95;
/** …among this many latest filled cells of the column. */
const RECENT = 100;

const EXCLUDED = {
  Insectary_data: new Set(['Insectary_ID', 'Pedigree', ...TYPED_OVER_FORMULA.Insectary_data]),
  Insectary_stocks: SUM_FIELDS.Insectary_stocks,
};
const excluded = (sheet, field) => !!EXCLUDED[sheet]?.has(field);

const text = v => (v === null || v === undefined ? '' : String(v).trim());
export const isPlaceholder = v => /^(NA|N\/A|-)$/i.test(text(v));
const isBlank = v => text(v) === '';

/**
 * Where the last year starts in a list of row dates in sheet order: after the
 * latest run of OLD_RUN dated rows older than a year (undated rows and
 * impossible dates do not count). -1 when no row is dated.
 */
function yearStart(dates, today) {
  let old = 0;
  let firstOld = -1;
  let dated = false;
  for (let i = dates.length - 1; i >= 0; i--) {
    const date = dates[i];
    if (typeof date !== 'number' || date > today + 31) continue;
    dated = true;
    if (date >= today - YEAR) old = 0;
    else {
      if (!old++) firstOld = i;
      if (old >= OLD_RUN) return firstOld + 1;
    }
  }
  return dated ? 0 : -1;
}

/** The rows of the last year (`rows`: observed rows in sheet order). */
export function windowRows(sheet, rows, today) {
  const field = DATE_FIELD[sheet];
  const start = field
    ? yearStart(
        rows.map(r => r.values[field]),
        today,
      )
    : -1;
  return start < 0 ? rows.slice(-WITHOUT_DATE) : rows.slice(start);
}

/** A formula of row `row` as compared with the others of its column: relative, spacing and case aside. */
export const formulaShape = (formula, row) => formulaKey(relativeFormula(formula, row));

/** A row's formula of `field` in relative form, worked out once per row (the formulas are long). */
const shapes = new WeakMap();
function shapeOf(r, field) {
  const formula = r.formulas[field];
  if (!formula) return null;
  let mine = shapes.get(r);
  if (!mine) shapes.set(r, (mine = new Map()));
  if (!mine.has(field)) mine.set(field, formulaShape(formula, r.row));
  return mine.get(field);
}

/** A formula worth copying: not empty ("="), no broken reference. */
const usable = formula => /^=\s*\S/.test(formula) && !/#REF!/i.test(formula);

/**
 * The patterns of one sheet from its observed rows in sheet order (the last
 * SCAN_ROWS are enough): [{ sheet, kind (null: every row), field, shape,
 * example: { row, formula } (the latest row with it), counts: { rows, formula,
 * other, typed, blank, placeholder }, universal, volatile: [function names],
 * wholeColumn: lookups over whole columns, window: [rows] }].
 */
export function detectPatterns(sheet, rows, { today }) {
  const mod = moduleMap.get(sheet);
  if (!mod) return [];
  const window = windowRows(sheet, rows, today);
  const kindField = KIND_FIELD[sheet];
  const groups = [[null, window]];
  if (kindField) {
    const byKind = new Map();
    for (const r of window) {
      const kind = text(r.values[kindField]);
      if (kind) (byKind.get(kind) ?? byKind.set(kind, []).get(kind)).push(r);
    }
    for (const [kind, list] of byKind) if (list.length >= MIN_ROWS) groups.push([kind, list]);
  }
  const out = [];
  const seen = new Set();
  for (const field of mod.fields) {
    if (field.readonly || excluded(sheet, field.key) || seen.has(field.key)) continue;
    seen.add(field.key);
    const found = groups.map(([kind, list]) => ({ kind, list, pattern: columnPattern(list, field.key) }));
    // One pattern for every row when each kind with enough rows has it too (Data_entry_order);
    // else kind by kind (the lookups of Collected_Sent2Insectary rows).
    const [all, ...kinds] = found;
    // A kind without the pattern vetoes it only when its rows are mostly typed (or another formula).
    const agrees = k => {
      if (k.pattern) return k.pattern.shape === all.pattern.shape;
      const c = countCells(k.list, field.key, all.pattern.shape);
      return c.typed + c.placeholder + c.other <= c.formula;
    };
    const everyRow = all.pattern && kinds.every(agrees);
    for (const { kind, list, pattern } of everyRow ? [all] : kinds)
      if (pattern)
        out.push({
          sheet,
          kind,
          field: field.key,
          ...pattern,
          volatile: volatileIn(pattern.example.formula),
          wholeColumn: wholeColumnLookups(pattern.example.formula),
          window: list,
        });
  }
  return out;
}

function countCells(list, field, shape) {
  const counts = { rows: list.length, formula: 0, other: 0, typed: 0, blank: 0, placeholder: 0 };
  for (const r of list) {
    const formula = r.formulas[field];
    if (formula) counts[shapeOf(r, field) === shape ? 'formula' : 'other']++;
    else if (isBlank(r.values[field])) counts.blank++;
    else if (isPlaceholder(r.values[field])) counts.placeholder++;
    else counts.typed++;
  }
  return counts;
}

/** The pattern of one column in some rows, or null. */
function columnPattern(list, field) {
  const shapes = new Map();
  for (const r of list) {
    const formula = r.formulas[field];
    if (!formula) continue;
    const shape = shapeOf(r, field);
    const entry = shapes.get(shape) ?? shapes.set(shape, { n: 0, last: null }).get(shape);
    entry.n++;
    entry.last = { row: r.row, formula };
  }
  if (!shapes.size) return null;
  const [shape, top] = [...shapes].sort((a, b) => b[1].n - a[1].n)[0];
  if (top.n < MIN_ROWS || !usable(top.last.formula)) return null;
  const counts = countCells(list, field, shape);
  const filled = counts.formula + counts.other + counts.typed;
  if (counts.formula / filled < SHARE_FILLED || counts.formula / counts.rows < SHARE_ALL) return null;
  // Near-universal in the current practice: the latest filled cells (older typed ones may predate the formula).
  const recent = list
    .filter(r => r.formulas[field] || !(isBlank(r.values[field]) || isPlaceholder(r.values[field])))
    .slice(-RECENT);
  const same = recent.filter(r => shapeOf(r, field) === shape).length;
  return { shape, example: top.last, counts, universal: same / recent.length >= UNIVERSAL };
}

/** The formula of a pattern moved to `row`. */
export const formulaAt = (pattern, row) => shiftFormula(pattern.example.formula, row - pattern.example.row);

/** Does a pattern apply to a row of this kind? */
export const appliesTo = (pattern, kind) => pattern.kind === null || pattern.kind === text(kind);

const cache = new WeakMap();
const REUSE_MS = 60_000;
/**
 * The patterns of `sheet` from the app's copy, worked out again when the sheet
 * changed (and a minute has passed): only its last year's rows are read.
 */
export function sheetPatterns(store, sheet, today = todaySerial()) {
  const mod = moduleMap.get(sheet);
  if (!mod) return [];
  const bySheet = cache.get(store) ?? cache.set(store, new Map()).get(store);
  const hit = bySheet.get(sheet);
  // Saves change the stamp, rarely the patterns: worked out again at most once a minute. Asked once
  // per row of a long proposal: within the minute the stamp (a count over the whole sheet) is not read.
  if (hit && Date.now() - hit.at < REUSE_MS && hit.today === today) return hit.patterns;
  const state = store.db.prepare('SELECT count(*) n, max(updated_at) u FROM records WHERE sheet=?').get(sheet);
  const stamp = `${state.n}:${state.u}:${today}`;
  if (hit?.stamp === stamp) {
    hit.at = Date.now();
    return hit.patterns;
  }
  // Only the last year's rows are parsed (their formulas are long): found by their dates first.
  const where = 'sheet=? AND missing=0 AND observed=1 AND row_num>? AND row_num<2000000000';
  const field = DATE_FIELD[sheet];
  const dated = field
    ? store.db
        .prepare(
          `SELECT row_num, json_extract(values_json, ?) d FROM records WHERE ${where} ORDER BY row_num DESC LIMIT ?`,
        )
        .all(`$."${field}"`, sheet, mod.headerRow, SCAN_ROWS)
        .reverse()
    : [];
  const start = yearStart(
    dated.map(r => r.d),
    today,
  );
  const rows = (
    start >= 0
      ? store.db
          .prepare(
            `SELECT row_num,values_json,formulas_json FROM records WHERE ${where} AND row_num>=? ORDER BY row_num`,
          )
          .all(sheet, mod.headerRow, dated[start]?.row_num ?? Infinity)
      : store.db
          .prepare(`SELECT row_num,values_json,formulas_json FROM records WHERE ${where} ORDER BY row_num DESC LIMIT ?`)
          .all(sheet, mod.headerRow, WITHOUT_DATE)
          .reverse()
  ).map(r => ({ row: r.row_num, values: JSON.parse(r.values_json), formulas: JSON.parse(r.formulas_json) }));
  const patterns = detectPatterns(sheet, rows, { today }).map(({ window, ...p }) => p);
  bySheet.set(sheet, { stamp, today, at: Date.now(), patterns });
  return patterns;
}

/** The pattern of a column for a row holding `values` (its kind), or null. */
export function usualPattern(store, sheet, field, values = {}, today = todaySerial()) {
  const kind = KIND_FIELD[sheet] ? values[KIND_FIELD[sheet]] : null;
  return sheetPatterns(store, sheet, today).find(p => p.field === field && appliesTo(p, kind)) ?? null;
}

/**
 * The formulas a new row of `sheet` at `row` gets, by field: the patterns of
 * its kind (`values`: what the row will hold, for its kind), but volatile ones.
 */
export function newRowFormulas(store, sheet, row, values, today = todaySerial()) {
  const kind = KIND_FIELD[sheet] ? values[KIND_FIELD[sheet]] : null;
  const out = {};
  for (const p of sheetPatterns(store, sheet, today))
    if (appliesTo(p, kind) && !p.volatile.length && !out[p.field]) out[p.field] = formulaAt(p, row);
  return out;
}

/** The fields a new row of `sheet` holding `values` gets as formulas (newRowFormulas' keys). */
export function newRowPatternFields(store, sheet, values, today = todaySerial()) {
  const kind = KIND_FIELD[sheet] ? values[KIND_FIELD[sheet]] : null;
  return new Set(
    sheetPatterns(store, sheet, today)
      .filter(p => appliesTo(p, kind) && !p.volatile.length)
      .map(p => p.field),
  );
}

/**
 * What to tell about a formula proposed for `field` of a row (`values`: the
 * row's, for its kind): [{ code, … }] with
 *  - usual: it is not the formula the column holds in the last year's rows of
 *    that kind (`usual`: that one moved to this row, `n` of `rows` rows have it);
 *  - wholeColumn: it looks up whole columns (A:A), where the column's usual
 *    formula (if any) does not: each row then searches the whole sheet.
 */
export function formulaNotes(store, sheet, field, formula, row, values = {}) {
  const p = usualPattern(store, sheet, field, values);
  const usual = p && formulaShape(formula, row) !== p.shape ? p : null;
  const out = [];
  if (usual)
    out.push({ code: 'usual', usual: formulaAt(usual, row), n: usual.counts.formula, rows: usual.counts.rows, kind: usual.kind });
  if ((!p || (usual && !p.wholeColumn)) && wholeColumnLookups(formula)) out.push({ code: 'wholeColumn' });
  return out;
}
