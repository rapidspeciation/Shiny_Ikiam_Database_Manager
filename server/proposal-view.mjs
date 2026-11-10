// How a proposal's table is shown (Cambios propuestos), as the assistant asks
// with `view` (propose_changes, update_proposal, match_notebook): the columns
// first or only, and whether the sheet's rows lying between the proposal's rows
// come in, greyed, for context. The person's own controls in the table (Solo
// cambios, + Columna…) work on top of it.

import { columnOf } from './schema.mjs';
import { hiddenWhy, isHiddenColumn } from './proposal-columns.mjs';

const clip = (value, length) => String(value ?? '').slice(0, length);

export const VIEW_PARAM = {
  type: 'object',
  description:
    "columns: shown first (changed ones always show); onlyColumns: only those and the changed; between: the sheet's untouched rows between its rows, greyed (default: when few; not on a notebook page; false for a long run of one fix).",
  properties: {
    columns: { type: 'array', items: { type: 'string' } },
    onlyColumns: { type: 'boolean' },
    between: { type: 'boolean' },
  },
};
/** update_proposal's `view`, told once in propose_changes. */
export const VIEW_UPDATE = { type: 'object', description: 'As in propose_changes (null drops a key)' };

/** Rows of the sheet the proposal leaves alone shown between its rows, at most. */
export const BETWEEN_ROWS = 500;
/** By default the rows in between show when there are at most this many, or no more than the proposal's rows. */
export const FEW_BETWEEN = 30;
/** The sheet's rows a click on a marker of the table opens (a gap, a jump of the notebook's lines), at most at once. */
export const PEEK_ROWS = 50;
/** Beside `viewLeftOut` in an answer: the person asked to see those columns, so they hear why they are not there. */
export const VIEW_LEFT_OUT_NOTE = 'Its table does not show these columns. Tell the person which ones and why.';

/**
 * The `view` the assistant gives, checked against the proposal's sheets:
 * { columns?, onlyColumns?, between? } with the columns as the sheet names them,
 * null for none, or { error }. `old`: the view so far (update_proposal): the keys
 * given replace its own, null drops one. A column the sheet's proposals never
 * show is left out, not refused (the view only arranges the table): why goes
 * into `leftOut`, for the answer.
 */
export function readView(given, sheets, old = null, leftOut = []) {
  if (given === undefined || given === null) return old;
  if (typeof given !== 'object' || Array.isArray(given))
    return { error: 'view: give { columns, onlyColumns, between }' };
  const out = { ...old };
  for (const key of Object.keys(given))
    if (!['columns', 'onlyColumns', 'between'].includes(key))
      return { error: `view: unknown key ${clip(key, 40)} (columns, onlyColumns, between)` };
  if (given.columns === null) delete out.columns;
  else if (given.columns !== undefined) {
    if (!Array.isArray(given.columns) || given.columns.length > 80)
      return { error: 'view.columns: a list of up to 80 column names' };
    const keys = [];
    for (const name of given.columns) {
      const found = sheets.map(sheet => columnOf(sheet, name));
      const hit = found.find(f => f.key);
      if (!hit) return { error: `view.columns: ${found[0]?.error ?? `Unknown column ${clip(name, 60)}`}` };
      // A column the sheet's proposals never show (server/proposal-columns.mjs).
      const hiddenIn = sheets.find(sheet => isHiddenColumn(sheet, hit.key));
      if (hiddenIn) {
        leftOut.push(`view.columns: ${hiddenWhy(hiddenIn, hit.key)}`);
        continue;
      }
      if (!keys.includes(hit.key)) keys.push(hit.key);
    }
    // Every column given left out: the view's columns stay as they were (none, or the old ones), not emptied.
    if (keys.length || !given.columns.length) out.columns = keys;
  }
  for (const flag of ['onlyColumns', 'between']) {
    if (given[flag] === null) delete out[flag];
    else if (given[flag] !== undefined) {
      if (typeof given[flag] !== 'boolean') return { error: `view.${flag}: true or false` };
      out[flag] = given[flag];
    }
  }
  return Object.keys(out).length ? out : null;
}

/**
 * A sheet's columns always shown (reviewColumns in server/notebook.mjs) as the
 * view asks: its columns first (those of this sheet), then the rest, or only its
 * own with onlyColumns. The table adds the changed columns after these.
 */
export function viewColumns(sheet, base, view) {
  if (!view?.columns?.length && !view?.onlyColumns) return base;
  const named = [...new Set((view.columns ?? []).map(name => columnOf(sheet, name).key).filter(Boolean))];
  return { fields: view.onlyColumns ? named : [...new Set([...named, ...base.fields])], keys: base.keys };
}

/**
 * Whether the rows in between are shown: as the view says, else for a proposal
 * without a notebook page when they are few (`between`: how many there are,
 * `rows`: how many rows the proposal writes).
 */
export const showBetween = (view, { paged, between, rows }) =>
  view?.between ?? (!paged && between <= Math.max(FEW_BETWEEN, rows));
