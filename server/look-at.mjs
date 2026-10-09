// What a proposal's rows hold that a person reading its table would notice
// (`lookAt` in the answers of propose_changes, update_proposal, get_proposal and
// match_notebook). Those answers show few of the rows (a bulk call's preview),
// so the server goes through them all and hands the assistant a short list:
// - issues: the checks' issues on those rows (server/checks.mjs, cached), but
//   on the cells the proposal writes (it deals with them already);
// - notes: what the sheet's notes say on them, clipped;
// - gaps: in a long proposal, an ID column (or SPECIES) empty in a row where
//   nearly every other row of its sheet has one (what the checks already say,
//   and the cells a preserved butterfly lacks, are not said twice);
// - formulaEmpty: rows whose SPECIES formula will give nothing for their clutch
//   (worked out by the caller, server/assistant.mjs) and that write no species.
// Rows that say the same come as one item; each list is capped, with how to see the rest.

import { allIssues, ID_COLUMN, readyIssues } from './checks.mjs';
import { moduleMap } from './schema.mjs';
import { newRowFormulaFields } from './premade.mjs';

/** Items per list, and rows named per item. */
export const LOOK_ITEMS = 25;
const LOOK_ROWS = 10;
/** Gaps: looked for among this many rows of a sheet at least, in columns filled in this share of them. */
const GAP_ROWS = 10;
const GAP_SHARE = 0.9;
const NOTE_FIELD = /^notes?(?:_|$)/i;
const NOTE_CHARS = 120;
const REST = "The rest: review_issues with a row's recordId (its issues), or `query` (notes, empty cells).";

const text = v => (v === null || v === undefined || typeof v === 'object' ? '' : String(v).trim());
const clip = (s, n) => (s.length > n ? `${s.slice(0, n - 1)}…` : s);
const same = (a, b) => text(a).toLowerCase() === text(b).toLowerCase();

/**
 * The issues of each row (by recordId), built once per scan. `ready`: only if found already (pages
 * polled often); `found`: a scan the caller holds (freshIssues).
 */
const indexes = new WeakMap();
export function issuesByRecord(store, { ready = false, found = null } = {}) {
  let entry;
  try {
    entry = found ?? (ready ? readyIssues(store) : allIssues(store));
  } catch {
    // A store without the sheets' copy (some tests' stand-ins): the proposal goes on without them.
    return null;
  }
  if (!entry) return null;
  let index = indexes.get(entry.issues);
  if (!index) {
    index = new Map();
    for (const i of entry.issues) if (i.recordId) (index.get(i.recordId) || index.set(i.recordId, []).get(i.recordId)).push(i);
    indexes.set(entry.issues, index);
  }
  return index;
}

/** An issue the row's change is the fix of (Revisión's fixes in a proposal): nothing more to say. */
const fixedBy = (issue, change) =>
  !!issue.fix && Object.entries(issue.fix.values ?? {}).every(([f, v]) => f in (change.values ?? {}) && same(change.values[f], v));

/** The issues the checks find on a proposal row's cells, as field → [issue] (none for a new row). */
export function rowIssues(index, change) {
  if (!index || change.create || !change.recordId) return {};
  const out = {};
  for (const issue of index.get(change.recordId) ?? []) if (issue.field && !fixedBy(issue, change)) (out[issue.field] ??= []).push(issue);
  return out;
}

/** Items with the same key as one, with the rows they are about (the first LOOK_ROWS). */
function grouped() {
  const items = new Map();
  return {
    add(key, item, row) {
      const it = items.get(key) ?? items.set(key, { item, rows: [], n: 0 }).get(key);
      it.n++;
      if (it.rows.length < LOOK_ROWS) it.rows.push(row);
    },
    list: () => [...items.values()].map(({ item, rows, n }) => ({ ...item, rows, ...(n > rows.length ? { moreRows: n - rows.length } : {}) })),
  };
}

/**
 * The lookAt block of a proposal's rows (`changes` as saved), or null when there
 * is nothing to say. `only`: the indexes to look at (the rows a revision changed);
 * `told`: [{ index, field }] already said in the answer (preservedWithoutSample);
 * `formulaEmpty`: [{ index, field, clutch }] formula cells that will give nothing; `found`: the
 * checks' scan the caller holds (freshIssues), else it is found here.
 */
export function lookAt(store, changes, { only = null, told = [], formulaEmpty = [], found = null } = {}) {
  const rows = changes.map((change, index) => ({ change, index })).filter(r => !r.change.placeholder && (!only || only.has(r.index)));
  if (!rows.length) return null;
  const several = new Set(rows.map(r => r.change.sheet)).size > 1;
  const where = c => (several ? { sheet: c.sheet } : {});
  const name = (c, index) => c.label || `#${index}`;
  const index = issuesByRecord(store, { found });
  const issues = grouped();
  const notes = grouped();
  const said = new Set(told.map(t => `${t.index}\u0000${t.field}`));
  const records = new Map();

  for (const { change: c, index: i } of rows) {
    const record = !c.create && c.recordId ? store.getRecord(c.recordId) : null;
    records.set(i, record);
    for (const [field, list] of Object.entries(rowIssues(index, c)))
      for (const issue of field in (c.values ?? {}) ? [] : list) {
        said.add(`${i}\u0000${field}`);
        const value = text(issue.value);
        issues.add(
          [c.sheet, issue.kind, field, issue.problem].join('\u0000'),
          { ...where(c), kind: issue.kind, field, problem: clip(text(issue.problem), 200) },
          value ? `${name(c, i)}: ${clip(value, 40)}` : name(c, i),
        );
      }
    // The sheet's notes on the row (what the proposal adds is the assistant's own).
    for (const [field, value] of Object.entries(record?.values ?? {})) {
      const note = NOTE_FIELD.test(field) ? clip(text(value), NOTE_CHARS) : '';
      if (note) notes.add([c.sheet, field, note].join('\u0000'), { ...where(c), field, note }, name(c, i));
    }
  }

  // Key columns left empty in a few rows of a long proposal, where the rest of its sheet's rows have them.
  const gaps = grouped();
  for (const [sheet, list] of Map.groupBy(rows, r => r.change.sheet)) {
    if (list.length < GAP_ROWS) continue;
    const fields = (moduleMap.get(sheet)?.fields ?? []).map(f => f.key).filter(k => k === 'SPECIES' || ID_COLUMN.test(k));
    const formulas = list.some(r => r.change.create) ? newRowFormulaFields(store, sheet) : new Set();
    const valuesOf = r => {
      const c = r.change;
      if (c.create) return c.values ?? {};
      return { ...(records.get(r.index)?.values ?? {}), ...c.values };
    };
    const all = list.map(r => ({ r, values: valuesOf(r) }));
    for (const field of fields) {
      const counted = all.filter(({ r }) => !(r.change.create && formulas.has(field)));
      const empty = counted.filter(({ values }) => !text(values[field]));
      if (counted.length < GAP_ROWS || !empty.length || counted.length - empty.length < GAP_SHARE * counted.length) continue;
      for (const { r } of empty) {
        if (said.has(`${r.index}\u0000${field}`)) continue;
        gaps.add(
          [sheet, field].join('\u0000'),
          { ...where(r.change), field, filledIn: `${counted.length - empty.length} of ${counted.length} rows` },
          name(r.change, r.index),
        );
      }
    }
  }

  // A formula that gives nothing for the row's clutch: its value comes from the notebook.
  const empty = grouped();
  for (const { index: i, field, clutch } of formulaEmpty) {
    const c = changes[i];
    if (!c || (only && !only.has(i))) continue;
    empty.add(
      [c.sheet, field, text(clutch)].join('\u0000'),
      {
        ...where(c),
        field,
        clutch,
        problem: `Its formula gives nothing for clutch ${text(clutch)} (not in Insectary_stocks yet, or without ${field} there): write ${field} from the notebook`,
      },
      name(c, i),
    );
  }

  const out = {};
  let rest = false;
  for (const [key, list] of [
    ['issues', issues.list()],
    ['notes', notes.list()],
    ['gaps', gaps.list()],
    ['formulaEmpty', empty.list()],
  ]) {
    if (!list.length) continue;
    out[key] = list.slice(0, LOOK_ITEMS);
    if (list.length > LOOK_ITEMS) out[`${key}More`] = list.length - LOOK_ITEMS;
    rest ||= list.length > LOOK_ITEMS || list.some(item => item.moreRows);
  }
  if (!Object.keys(out).length) return null;
  return rest ? { ...out, rest: REST } : out;
}

/** Items per list in a short lookAt (match_notebook answers one per page). */
const BRIEF_ITEMS = 10;
/**
 * A lookAt kept short (match_notebook answers one per page): issues of one kind on one column
 * (a photo missing on each preserved butterfly) as one item, with the first problem as its
 * example and how many there are; each list cut at BRIEF_ITEMS. The rest: get_proposal.
 */
export function briefLookAt(look) {
  if (!look) return look;
  const out = {};
  let cut = false;
  for (const [key, value] of Object.entries(look)) {
    if (!Array.isArray(value)) {
      if (key !== 'rest' && !/More$/.test(key)) out[key] = value;
      cut ||= key === 'rest';
      continue;
    }
    let list = value;
    if (key === 'issues')
      list = [...Map.groupBy(value, i => [i.sheet, i.kind, i.field].join('\u0000')).values()].flatMap(group => {
        if (group.length < 3) return group;
        const rows = group.flatMap(i => i.rows);
        const more = group.reduce((n, i) => n + (i.moreRows ?? 0), 0) + Math.max(0, rows.length - LOOK_ROWS);
        return [{ ...group[0], problems: group.length, rows: rows.slice(0, LOOK_ROWS), ...(more ? { moreRows: more } : {}) }];
      });
    out[key] = list.slice(0, BRIEF_ITEMS);
    const more = list.length - BRIEF_ITEMS + (look[`${key}More`] ?? 0);
    if (more > 0) out[`${key}More`] = more;
    cut ||= more > 0;
  }
  return cut ? { ...out, rest: 'The rest: get_proposal.' } : out;
}
