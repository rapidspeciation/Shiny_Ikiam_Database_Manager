// What the formulas a proposal writes cost the team's workbook, before they are
// written. Google Sheets recalculates a formula whenever a cell it reads changes,
// and a lookup over a whole column (XLOOKUP(A2, Collection_data!D:D, …)) reads
// every row of that column each time. The model is the one of
// docs/workbook-performance-audit.md: cost = formula cells recalculated × rows
// each one scans. Its absolute speed is unknown; what it tells is which formulas
// are heavy and by how much.
//
// Per distinct formula of a proposal (sheet · column · the formula in relative
// form): how many cells; the rows each range it reads scans (a whole column: the
// rows its sheet has in the app's copy; a bounded range: its size; a cell: 1);
// the comparisons of one full recalculation; and the formulas elsewhere in the
// workbook that read the written column over a whole column: each write there
// makes all of them recalculate (Insectary_data O and V feed ~57,000 lookups of
// F1/F2_MutationRate, each scanning ~20,000 rows). That index of dependents is
// built from every formula of the copy once per sync (a second or so) and kept.

import { moduleMap } from './schema.mjs';
import { formulaKey, volatileIn } from './formula-write.mjs';
import { columnIndex, relativeFormula } from './formula.mjs';

/*
 * Thresholds (amber in the table, `heavy` in the answer), from the audit's measures: one edit whose
 * model cost was ~0.5 G comparisons kept the workbook busy ~170 s, about 3 M comparisons a second.
 *  - HEAVY_COMPARISONS: one full recalculation of the written formulas over ~1.5 s, added to every
 *    edit of a column they read.
 *  - HEAVY_DEPENDENTS: the formulas elsewhere that read the written column cost over ~7 s per write,
 *    which the app's saves and reads wait for (the 500-cell chunks written into Insectary_data O and
 *    V, feeding ~6,400 and ~2,300 lookups of F1/F2_MutationRate, took 30 s–2.5 min each).
 *  - REPEATED: a whole-column lookup into another sheet copied down this many rows is the shape of
 *    the audit's heaviest blocks (MutationRate, Collection_data's photo columns).
 */
export const HEAVY_COMPARISONS = 5_000_000;
export const HEAVY_DEPENDENTS = 20_000_000;
export const REPEATED = 500;
/** A bounded range this long counts as column-wide for the dependents (Insectary_data!$A$2:$A$30000). */
const WIDE = 1000;
/** A sheet the copy does not hold: the size of a new Google sheet. */
const DEFAULT_ROWS = 1000;
/** Patterns described per proposal (the heaviest first). */
const MAX_PATTERNS = 12;

// ------------------------------------------------------------------ references
const SHEET = String.raw`(?:'(?:[^']|'')+'|[A-Za-z_][A-Za-z0-9_.]*)!`;
const CORNER = String.raw`\$?[A-Za-z]{1,3}(?:\$?\d+)?`;
const TOKENS = new RegExp(
  [
    String.raw`(?<str>"(?:[^"]|"")*")`,
    String.raw`(?<fn>[A-Za-z_][A-Za-z0-9_.]*)\s*\(`,
    String.raw`(?<![A-Za-z0-9_$.'!])(?<ref>(?:${SHEET})?${CORNER}(?::${CORNER})?)(?![A-Za-z0-9_(!.])`,
    String.raw`(?<open>\()`,
    String.raw`(?<close>\))`,
    String.raw`(?<sep>[,;])`,
  ].join('|'),
  'g',
);

/** Functions that search a range for a value (or filter it): over a whole column, each cell scans the sheet. */
const SEARCH_ARGS = {
  XLOOKUP: [1],
  XMATCH: [1],
  MATCH: [1],
  VLOOKUP: [1],
  HLOOKUP: [1],
  LOOKUP: [1],
  COUNTIF: [0],
  SUMIF: [0],
  AVERAGEIF: [0],
};
/** Ranges of which one cell is read (the value returned): INDEX's, XLOOKUP's return range, SUMIF's sum range. */
const PICK_ARGS = {
  INDEX: [0],
  XLOOKUP: [2],
  LOOKUP: [2],
  SUMIF: [2],
  AVERAGEIF: [2],
  SUMIFS: [0],
  AVERAGEIFS: [0],
  MAXIFS: [0],
  MINIFS: [0],
};
/** Functions whose every range is searched (criteria ranges, filters, queries). */
const SEARCH_ALL = new Set([
  'COUNTIFS',
  'SUMIFS',
  'AVERAGEIFS',
  'MAXIFS',
  'MINIFS',
  'FILTER',
  'QUERY',
  'SUMPRODUCT',
  'UNIQUE',
  'SORT',
  'COUNTUNIQUE',
]);

function readRef(text) {
  let sheet = null;
  let rest = text;
  const bang = text.lastIndexOf('!');
  if (bang >= 0) {
    sheet = text.slice(0, bang).replace(/^'|'$/g, '').replaceAll("''", "'");
    rest = text.slice(bang + 1);
  }
  const corner = part => {
    const m = /^\$?([A-Za-z]{1,3})(?:\$?(\d+))?$/.exec(part);
    return m && { col: columnIndex(m[1]), row: m[2] ? Number(m[2]) : null };
  };
  const [a, b] = rest.split(':').map(corner);
  if (!a || (rest.includes(':') && !b)) return null;
  // A name without a row (TRUE, a named range) is not a reference.
  if (!b) return a.row === null ? null : { sheet, text, c1: a.col, c2: a.col, r1: a.row, r2: a.row, cell: true };
  return { sheet, text, c1: Math.min(a.col, b.col), c2: Math.max(a.col, b.col), r1: a.row, r2: b.row, cell: false };
}

/**
 * The references of a formula, each with the function it is an argument of and its role there:
 * 'search' (searched for a value: XLOOKUP's lookup range), 'pick' (one cell read: INDEX's range),
 * 'read' (read whole: SUM's range, or a cell).
 */
export function formulaRefs(text) {
  const out = [];
  const stack = [];
  for (const m of String(text ?? '')
    .replace(/^\s*=/, '')
    .matchAll(TOKENS)) {
    const g = m.groups;
    if (g.str !== undefined) continue;
    if (g.fn !== undefined) stack.push({ fn: g.fn.toUpperCase(), arg: 0 });
    else if (g.open !== undefined) stack.push({ ...(stack.at(-1) ?? { fn: null, arg: 0 }), group: true });
    else if (g.close !== undefined) stack.pop();
    else if (g.sep !== undefined) {
      const top = stack.at(-1);
      if (top && !top.group) top.arg++;
    } else if (g.ref !== undefined) {
      const ref = readRef(g.ref);
      if (!ref) continue;
      const { fn = null, arg = 0 } = stack.at(-1) ?? {};
      ref.fn = fn;
      ref.role = ref.cell
        ? 'read'
        : PICK_ARGS[fn]?.includes(arg)
          ? 'pick'
          : SEARCH_ARGS[fn]?.includes(arg) || SEARCH_ALL.has(fn)
            ? 'search'
            : 'read';
      out.push(ref);
    }
  }
  return out;
}

const whole = ref => !ref.cell && (ref.r1 === null || ref.r2 === null);
/** Rows a range spans: a whole column, the rows of its sheet (`rows`); open at one end (A2:A), from there down. */
function spanRows(ref, rows) {
  if (ref.r1 === null && ref.r2 === null) return rows;
  if (ref.r1 === null || ref.r2 === null) return Math.max(0, rows - (ref.r1 ?? ref.r2) + 1);
  return Math.abs(ref.r2 - ref.r1) + 1;
}
/** Cells one recalculation of the reference reads: 1 for a cell or a picked value; its rows for a search; rows × columns read whole. */
function scanOf(ref, rows) {
  if (ref.cell || ref.role === 'pick') return 1;
  const n = spanRows(ref, rows);
  return ref.role === 'search' ? n : n * (ref.c2 - ref.c1 + 1);
}

// ------------------------------------------------------------------ the copy: rows and dependents
const built = new WeakMap();
/** A stamp that changes with each sync (the copy's version where no sync is recorded). */
const stampOf = store => store.getSetting?.('lastSync') ?? store.copyVersion?.() ?? '0';

/** The rows each sheet has in the copy (its last row, pre-made rows with formulas included). */
function sheetRows(store) {
  const out = new Map();
  for (const r of store.db
    .prepare(
      'SELECT sheet, max(row_num) n FROM records WHERE missing=0 AND row_num>0 AND row_num<2000000000 GROUP BY sheet',
    )
    .all())
    out.set(r.sheet, r.n);
  return out;
}

/**
 * Formula cells of the copy that read a column over a whole column (or a range of WIDE rows or
 * more): by `${sheet}\u0000${column index}`, a Map of the sheet and column holding them →
 * { cells, scan } (scan: the rows those cells scan when they recalculate, every range they search).
 * The copies of one formula down a column differ only in their numbers: each is read once (a
 * million formulas in some 1.5 s).
 */
function buildDependents(store, rows) {
  const byColumn = new Map();
  const memo = new Map();
  const analyse = (sheet, field, text) => {
    const refs = formulaRefs(text);
    const wide = refs.filter(r => !r.cell && (whole(r) || spanRows(r, 0) >= WIDE));
    if (!wide.length) return null;
    const rowsOf = r => rows.get(r.sheet ?? sheet) ?? DEFAULT_ROWS;
    const scan = refs.reduce((n, r) => n + scanOf(r, rowsOf(r)), 0);
    const columns = new Set();
    for (const r of wide)
      for (let c = r.c1; c <= Math.min(r.c2, r.c1 + 60); c++) columns.add(`${r.sheet ?? sheet}\u0000${c}`);
    const source = `${sheet}\u0000${field}`;
    const entries = [...columns].map(column => {
      const sources = byColumn.get(column) ?? byColumn.set(column, new Map()).get(column);
      return sources.get(source) ?? sources.set(source, { cells: 0, scan: 0 }).get(source);
    });
    return { scan, entries };
  };
  const statement = store.db.prepare(
    "SELECT sheet, formulas_json FROM records WHERE missing=0 AND row_num>0 AND row_num<2000000000 AND formulas_json<>'{}'",
  );
  for (const r of statement.iterate()) {
    let formulas;
    try {
      formulas = JSON.parse(r.formulas_json);
    } catch {
      continue;
    }
    const known = memo.get(r.sheet) ?? memo.set(r.sheet, new Map()).get(r.sheet);
    for (const field in formulas) {
      const text = formulas[field];
      if (typeof text !== 'string' || !text.includes(':')) continue;
      const key = `${field}\u0000${text.replace(/\d+/g, '#')}`;
      let info = known.get(key);
      if (info === undefined) known.set(key, (info = analyse(r.sheet, field, text)));
      if (!info) continue;
      for (const entry of info.entries) {
        entry.cells++;
        entry.scan += info.scan;
      }
    }
  }
  return byColumn;
}

/** The copy's rows per sheet and its dependents index, built once per sync. */
export function workbookIndex(store) {
  const stamp = stampOf(store);
  const hit = built.get(store);
  if (hit?.stamp === stamp) return hit;
  const started = Date.now();
  const rows = sheetRows(store);
  const index = { stamp, rows, dependents: buildDependents(store, rows), ms: 0 };
  index.ms = Date.now() - started;
  built.set(store, index);
  return index;
}

/** The column (zero-based) of a field, by the sheet's live header when read. */
function columnOf(store, sheet, field) {
  const live = store.layouts?.get?.(sheet)?.columns?.get?.(field);
  if (Number.isInteger(live)) return live;
  return moduleMap.get(sheet)?.fields.find(f => f.key === field)?.column ?? null;
}

/**
 * The formulas elsewhere that read column `field` of `sheet` over the whole column (its own
 * column left out): { cells, comparisons, sheets: [[sheet, cells]] (most first) }, or null.
 */
export function dependentsOf(store, sheet, field, index = workbookIndex(store)) {
  const column = columnOf(store, sheet, field);
  if (column === null) return null;
  const sources = index.dependents.get(`${sheet}\u0000${column}`);
  if (!sources) return null;
  let cells = 0;
  let comparisons = 0;
  const bySheet = new Map();
  for (const [source, entry] of sources) {
    if (source === `${sheet}\u0000${field}`) continue;
    cells += entry.cells;
    comparisons += entry.scan;
    const from = source.split('\u0000')[0];
    bySheet.set(from, (bySheet.get(from) ?? 0) + entry.cells);
  }
  if (!cells) return null;
  return { cells, comparisons, sheets: [...bySheet].sort((a, b) => b[1] - a[1]).slice(0, 4) };
}

// ------------------------------------------------------------------ one formula
const LETTERS = n => {
  let s = '';
  for (n += 1; n; n = Math.floor((n - 1) / 26)) s = String.fromCharCode(65 + ((n - 1) % 26)) + s;
  return s;
};
const quoted = sheet => (/^[A-Za-z_][A-Za-z0-9_.]*$/.test(sheet) ? sheet : `'${sheet.replaceAll("'", "''")}'`);
/** A guard before the lookup: =IF(A2="","",…) or =IFS(B2="","",…): empty keys skip the search. */
const GUARDED = /^=\s*(?:IF|IFS)\s*\(\s*\$?[A-Za-z]{1,3}\$?\d+\s*=\s*""/i;

/**
 * The cost of one formula written in `cells` cells of `sheet` (`text`: as written in one of them):
 * { perCell, scans: [{ range, rows, lookup, otherSheet }] (the ranges searched or read whole),
 * flags, tips, bounded }.
 */
export function formulaCostOf(text, sheet, cells, rows) {
  const refs = formulaRefs(text);
  const rowsOf = r => rows.get(r.sheet ?? sheet) ?? DEFAULT_ROWS;
  let perCell = 0;
  const scans = [];
  for (const r of refs) {
    const scan = scanOf(r, rowsOf(r));
    perCell += scan;
    if (r.cell || r.role === 'pick') continue;
    const range = r.text.replace(/\$/g, '');
    const lookup = r.role === 'search';
    const known = scans.find(s => s.range === range);
    if (known) known.times++;
    else
      scans.push({
        range,
        rows: spanRows(r, rowsOf(r)),
        whole: whole(r),
        lookup,
        otherSheet: !!r.sheet && r.sheet !== sheet,
        times: 1,
        ref: r,
      });
  }
  const flags = [];
  const volatile = volatileIn(text);
  if (volatile.length) flags.push('volatile');
  const wholeLookups = scans.filter(s => s.whole && s.lookup);
  if (wholeLookups.length) flags.push('wholeColumnLookup');
  if (cells > REPEATED && wholeLookups.some(s => s.otherSheet)) flags.push('crossSheetRepeated');
  const tips = [];
  let bounded = null;
  if (scans.some(s => s.lookup && s.times > 1)) tips.push('helper');
  else if (wholeLookups.length) {
    const first = wholeLookups[0].ref;
    const last = rowsOf(first);
    const prefix = first.sheet ? `${quoted(first.sheet)}!` : '';
    bounded = `${prefix}$${LETTERS(first.c1)}$2:$${LETTERS(first.c2)}$${last}`;
    tips.push('bounded');
  }
  if (wholeLookups.length && !GUARDED.test(text)) tips.push('guard');
  return { perCell, scans, flags, tips, bounded, volatile };
}

// ------------------------------------------------------------------ a proposal
/** A cell's formula in relative form (its own row out), to group a column's copies. */
const patternKey = (sheet, field, text, row) => `${sheet}\u0000${field}\u0000${formulaKey(relativeFormula(text, row))}`;
const clipText = (text, n = 140) => (text.length > n ? `${text.slice(0, n - 1)}…` : text);

/**
 * The cost of the formulas a proposal writes (`changes`: its rows; `formulaCells` say which cells
 * are formulas): per distinct formula, the heaviest first, or null when it writes none.
 * [{ sheet, column, formula (as on its first row), cells, perCell, comparisons, scans: [{ range,
 *   rows, lookup }], dependents: { cells, comparisons, sheets } | null, flags, heavy, tips, bounded }]
 */
export function proposalFormulaCost(store, changes) {
  try {
    return costOf(store, changes);
  } catch (e) {
    // An estimate: never what stops a proposal.
    console.error('Formula cost:', e.message);
    return null;
  }
}
function costOf(store, changes) {
  const groups = new Map();
  for (const c of changes ?? []) {
    if (!c.formulaCells?.length || c.create || !c.row) continue;
    for (const field of c.formulaCells) {
      const text = c.values?.[field];
      if (typeof text !== 'string' || !text.trim().startsWith('=')) continue;
      const key = patternKey(c.sheet, field, text, c.row);
      const group =
        groups.get(key) ?? groups.set(key, { sheet: c.sheet, column: field, formula: text, cells: 0 }).get(key);
      group.cells++;
    }
  }
  if (!groups.size) return null;
  const index = workbookIndex(store);
  const dependents = new Map();
  const out = [...groups.values()].map(g => {
    const cost = formulaCostOf(g.formula, g.sheet, g.cells, index.rows);
    const depKey = `${g.sheet}\u0000${g.column}`;
    if (!dependents.has(depKey)) dependents.set(depKey, dependentsOf(store, g.sheet, g.column, index));
    const deps = dependents.get(depKey);
    const comparisons = g.cells * cost.perCell;
    const flags = [...cost.flags];
    if (deps && deps.comparisons >= HEAVY_DEPENDENTS) flags.push('heavyDependents');
    const tips = [...cost.tips];
    if (deps && deps.comparisons >= HEAVY_DEPENDENTS) tips.push('batch');
    return {
      sheet: g.sheet,
      column: g.column,
      formula: clipText(g.formula),
      cells: g.cells,
      perCell: cost.perCell,
      comparisons,
      scans: cost.scans.map(s => ({
        range: s.range,
        rows: s.rows,
        ...(s.lookup ? { lookup: true } : {}),
        ...(s.times > 1 ? { times: s.times } : {}),
      })),
      dependents: deps,
      flags,
      heavy:
        comparisons >= HEAVY_COMPARISONS || (deps?.comparisons ?? 0) >= HEAVY_DEPENDENTS || cost.volatile.length > 0,
      tips,
      ...(cost.bounded ? { bounded: cost.bounded } : {}),
    };
  });
  return out
    .sort(
      (a, b) => b.comparisons + (b.dependents?.comparisons ?? 0) - (a.comparisons + (a.dependents?.comparisons ?? 0)),
    )
    .slice(0, MAX_PATTERNS);
}

// ------------------------------------------------------------------ for the assistant
const big = n =>
  n >= 1e9
    ? `${(n / 1e9).toFixed(n >= 1e10 ? 0 : 1)} G`
    : n >= 1e6
      ? `${(n / 1e6).toFixed(n >= 1e7 ? 0 : 1)} M`
      : n.toLocaleString('en-US');
const TIP_TEXT = {
  helper: () =>
    'the same range is searched more than once per row: one helper column with XMATCH(key, range) per row, then INDEX(column, helper) for each value',
  bounded: p =>
    `a range ending at the last used row (${p.bounded}) instead of the whole column; extend it when rows are added`,
  guard: () => 'skip rows that cannot match before searching (IF(key="","",…), or the row\'s kind)',
  batch: () => 'write it in one proposal, not cell by cell: every write recalculates those formulas',
};

/** The cost as the tools' answers give it: one compact entry per formula, a line saying what to do when heavy. */
export function formulaCostAnswer(cost) {
  if (!cost?.length) return null;
  return cost.map(p => ({
    sheet: p.sheet,
    column: p.column,
    cells: p.cells,
    scans: p.scans.map(s => `${s.range}: ${s.rows.toLocaleString('en-US')} rows${s.times > 1 ? ` ×${s.times}` : ''}`),
    comparisons: `${p.cells.toLocaleString('en-US')} cells × ${p.perCell.toLocaleString('en-US')} ≈ ${big(p.comparisons)} per full recalculation`,
    ...(p.dependents
      ? {
          eachWrite: `recalculates ${p.dependents.cells.toLocaleString('en-US')} formulas reading ${p.column} whole (${p.dependents.sheets.map(([s, n]) => `${s} ${n.toLocaleString('en-US')}`).join(', ')}) ≈ ${big(p.dependents.comparisons)} comparisons`,
        }
      : {}),
    ...(p.flags.length ? { flags: p.flags } : {}),
    ...(p.heavy ? { heavy: true } : {}),
    ...(p.tips.length && (p.heavy || p.flags.length) ? { suggestion: p.tips.map(t => TIP_TEXT[t](p)).join('; ') } : {}),
  }));
}
