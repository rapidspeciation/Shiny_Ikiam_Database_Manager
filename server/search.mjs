// The Buscador: one text searched in every sheet, like Google Sheets' Ctrl+F.
// Each sheet with matches comes back with its number of matches, the rows
// where they are, and the rows around the best one, so the page shows each
// match among its neighbours (a run of tubes typed FS5… for FS50… shows up
// at once). More rows of a sheet are read by range while scrolling.

import { moduleMap } from './schema.mjs';
import { wireRow } from './grid.mjs';
import { msgError } from './messages.mjs';

const fail = (code, message, status = 400) => msgError(message, { code, status });

/** The rows a sheet shows (the same as GET /api/table): unused pre-made rows included. */
const VISIBLE = 'sheet=? AND missing=0 AND row_num>? AND row_num<2000000000';
/** Match rows listed per sheet; past this the newest are kept (and every exact one). */
const MAX_MATCHES = 1000;
/** Rows of one range read. */
const MAX_RANGE = 200;
/** Sheets in the order the page groups them; the reference lists come last. */
const GROUP_ORDER = ['insectary', 'field', 'breeding', 'research', 'samples', 'media', 'reference'];

const EPOCH = Date.UTC(1899, 11, 30);
const MONTHS = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
const TIME_FIELD = /(^|_)time$/i;

/** A cell's text as the grid shows it (frontend lib/cells.ts displayValue): dates 14-Aug-25, times 09:05. */
export function cellText(value, field) {
  if (value === null || value === undefined) return '';
  if (field?.type === 'date' && typeof value === 'number' && Number.isFinite(value)) {
    const d = new Date(EPOCH + Math.round(value) * 86_400_000);
    return `${d.getUTCDate()}-${MONTHS[d.getUTCMonth()]}-${String(d.getUTCFullYear()).slice(2)}`;
  }
  if (field && TIME_FIELD.test(field.key) && typeof value === 'number' && value >= 0 && value < 1) {
    const minutes = Math.round(value * 24 * 60);
    return `${String(Math.floor(minutes / 60)).padStart(2, '0')}:${String(minutes % 60).padStart(2, '0')}`;
  }
  if (typeof value === 'boolean') return value ? 'TRUE' : 'FALSE';
  return String(value);
}

/** Digits as 9: the text could be part of a date as shown (26-May-26) or a time (09:05). */
const shape = needle => needle.replace(/\d/g, '9');
const DATE_SHAPES = MONTHS.flatMap(m => [`9-${m.toLowerCase()}-99`, `99-${m.toLowerCase()}-99`]);
const dateLike = needle => DATE_SHAPES.some(s => s.includes(shape(needle)));
const timeLike = needle => '99:99'.includes(shape(needle));

/**
 * The stored JSON holds a cell's text as it is when the text has no quotes,
 * backslashes or control characters, so SQLite's LIKE (case-insensitive for
 * ASCII) finds every row that can match; the rows found are checked cell by
 * cell. Other text (accents, quotes) reads every row.
 */
function likePattern(needle) {
  // eslint-disable-next-line no-control-regex
  if (/[^\x20-\x7e]|["\\]/.test(needle)) return null;
  return `%${needle.replace(/[%_!]/g, '!$&')}%`;
}

/** The sheet's fields once each (a header written twice keeps the first column's value). */
function fieldsOf(mod) {
  const seen = new Set();
  return mod.fields.filter(f => !seen.has(f.key) && seen.add(f.key));
}

/** Where `needle` (lower case) is in one sheet: every matching row (exact cells marked) and the columns. */
function searchSheet(store, mod, needle, like) {
  const fields = fieldsOf(mod);
  const identity = new Set(mod.identityFields);
  // Dates and times are shown differently from how they are stored: those sheets are read whole.
  const whole =
    !like ||
    (dateLike(needle) && fields.some(f => f.type === 'date')) ||
    (timeLike(needle) && fields.some(f => TIME_FIELD.test(f.key)));
  const rows = store.db
    .prepare(
      `SELECT row_num, values_json FROM records WHERE ${VISIBLE}${whole ? '' : " AND values_json LIKE ? ESCAPE '!'"} ORDER BY row_num`,
    )
    .all(...(whole ? [mod.id, mod.headerRow] : [mod.id, mod.headerRow, like]));
  const matches = [];
  const columns = new Map();
  for (const r of rows) {
    const values = JSON.parse(r.values_json);
    let found = false;
    let exact = false;
    let id = false;
    for (const f of fields) {
      const value = values[f.key];
      if (value === null || value === undefined) continue;
      const text = cellText(value, f).toLowerCase();
      if (!text.includes(needle)) continue;
      found = true;
      columns.set(f.key, (columns.get(f.key) || 0) + 1);
      if (text.trim() === needle) {
        exact = true;
        if (identity.has(f.key)) id = true;
      }
    }
    if (found) matches.push({ row: r.row_num, exact, id });
  }
  // The columns holding the text, the most matches first.
  return { matches, columns: [...columns.entries()].sort((a, b) => b[1] - a[1]).map(([key]) => key) };
}

/** The match a sheet opens at: the newest exact match in an ID column, else the newest exact one, else the newest. */
function focusOf(matches) {
  return (matches.findLast(m => m.id) ?? matches.findLast(m => m.exact) ?? matches.at(-1)).row;
}

/** At most MAX_MATCHES rows: every exact match, then the newest others. */
function listed(matches) {
  if (matches.length <= MAX_MATCHES) return matches.map(m => m.row);
  const keep = new Set(matches.filter(m => m.exact).slice(-MAX_MATCHES).map(m => m.row));
  for (let i = matches.length - 1; i >= 0 && keep.size < MAX_MATCHES; i--) keep.add(matches[i].row);
  return [...keep].sort((a, b) => a - b);
}

function bounds(store, mod) {
  const r = store.db.prepare(`SELECT min(row_num) a, max(row_num) b FROM records WHERE ${VISIBLE}`).get(mod.id, mod.headerRow);
  return { first: r.a ?? null, last: r.b ?? null };
}

const COLUMNS = 'id,row_num,version,observed,values_json,formulas_json';
const keysOf = mod => mod.fields.map(f => f.key);

/** `context` rows before and after a row (the row itself included). */
function around(store, mod, row, context) {
  const before = store.db
    .prepare(`SELECT ${COLUMNS} FROM records WHERE ${VISIBLE} AND row_num<? ORDER BY row_num DESC LIMIT ?`)
    .all(mod.id, mod.headerRow, row, context)
    .reverse();
  const after = store.db
    .prepare(`SELECT ${COLUMNS} FROM records WHERE ${VISIBLE} AND row_num>=? ORDER BY row_num LIMIT ?`)
    .all(mod.id, mod.headerRow, row, context + 1);
  const rows = [...before, ...after];
  const keys = keysOf(mod);
  return { from: rows[0]?.row_num ?? row, to: rows.at(-1)?.row_num ?? row, rows: rows.map(r => wireRow(keys, r)) };
}

/** Columns missing from the live sheet (shown with their last value, read-only). */
function unavailable(store, mod) {
  const layout = store.layouts?.get(mod.id);
  return layout && !layout.blocked ? mod.fields.filter(f => !layout.columns.has(f.key)).map(f => f.key) : [];
}

function int(value, fallback, min, max) {
  if (value === undefined || value === null || value === '') return fallback;
  const n = Math.trunc(Number(value));
  return Number.isFinite(n) ? Math.min(Math.max(n, min), max) : fallback;
}

/**
 * Searches `q` (case-insensitive, at least two characters) in every column of
 * every sheet. Sheets come best first: `pin` (the sheet a link names), then
 * those where the text is a whole ID (Insectary_ID, CAM_ID…), then those where
 * it fills a whole cell, then the rest, by kind of sheet. The first `sheets`
 * of them carry the rows around their match (`context` before and after).
 */
export function searchAll(store, { q, context, sheets, pin } = {}) {
  const started = performance.now();
  const query = String(q ?? '').trim();
  if (query.length < 2) return { query, sheets: [], took: 0 };
  const needle = query.toLowerCase();
  const like = likePattern(needle);
  const around_ = int(context, 12, 0, 50);
  const withRows = int(sheets, 6, 0, 30);
  const found = [];
  for (const mod of moduleMap.values()) {
    const { matches, columns } = searchSheet(store, mod, needle, like);
    if (!matches.length) continue;
    found.push({
      mod,
      matches,
      total: matches.length,
      exact: matches.filter(m => m.exact).length,
      idExact: matches.filter(m => m.id).length,
      columns,
    });
  }
  const group = mod => {
    const i = GROUP_ORDER.indexOf(mod.group);
    return i < 0 ? GROUP_ORDER.length : i;
  };
  found.sort(
    (a, b) =>
      (b.mod.id === pin) - (a.mod.id === pin) ||
      (b.idExact > 0) - (a.idExact > 0) ||
      (b.exact > 0) - (a.exact > 0) ||
      group(a.mod) - group(b.mod) ||
      b.total - a.total,
  );
  const out = found.map((s, i) => {
    const focus = focusOf(s.matches);
    const rows = listed(s.matches);
    return {
      module: s.mod.id,
      total: s.total,
      exact: s.exact,
      idExact: s.idExact,
      columns: s.columns,
      matches: rows,
      truncated: rows.length < s.total,
      focus,
      ...bounds(store, s.mod),
      unavailable: unavailable(store, s.mod),
      window: i < withRows ? around(store, s.mod, focus, around_) : null,
    };
  });
  return { query, sheets: out, took: Math.round(performance.now() - started) };
}

/**
 * Rows `from`..`to` of a sheet (by row number, at most MAX_RANGE of them), for
 * a search result scrolled up or down or moved to another match.
 */
export function searchRange(store, { module, from, to } = {}) {
  const mod = moduleMap.get(String(module || ''));
  if (!mod) throw fail('MODULE_NOT_FOUND', 'Hoja desconocida', 404);
  const a = int(from, NaN, 0, 2_000_000_000);
  const b = int(to, NaN, 0, 2_000_000_000);
  if (!Number.isFinite(a) || !Number.isFinite(b) || b < a) throw fail('INVALID_RANGE', 'Rango de filas no válido');
  const rows = store.db
    .prepare(`SELECT ${COLUMNS} FROM records WHERE ${VISIBLE} AND row_num BETWEEN ? AND ? ORDER BY row_num LIMIT ?`)
    .all(mod.id, mod.headerRow, a, b, MAX_RANGE);
  const keys = keysOf(mod);
  // A range cut short ends at its last row, so the page asks for the rest from there.
  const end = rows.length === MAX_RANGE ? rows.at(-1).row_num : b;
  return { module: mod.id, from: a, to: end, rows: rows.map(r => wireRow(keys, r)) };
}
