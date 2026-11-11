// Fixed end rows in the workbook's rules, kept ahead of the data (the nightly run: scripts/nightly-app-checks.mjs
// --sheet-ranges). Formulas and conditional formats stay light with fixed ranges instead of INDIRECTO or whole columns
// (assistant/AGENTS.md), but a fixed end stops counting the rows that come after it:
//  - a conditional format's covered range that ends at the grid's last row grows when rows are appended, its formula's
//    `$A$2:$A$20846` does not: a repeat typed in a new row was not marked (tried on the test copy, 11 Nov 2026);
//  - a dropdown whose choices come from a growing ID list (`=Lists!$Z$2:$Z$3030`) stops offering the IDs added after
//    its end (Lists!Z had 3,775 rows on 11 Nov 2026).
// When the rows that exist come within MARGIN of a formula's end, it moves to EXTEND past them (a formula range past
// the grid costs nothing: Google counts only the rows that exist); the covered range is left to Google, which grows it
// with the grid. An end further behind the grid than MARGIN is taken as meant (a short lookup block) and only named;
// on a sheet under MARGIN rows that cannot be told, so such a block there would be moved too (none on 11 Nov 2026).
// A dropdown's end moves when its list's last filled row comes within MARGIN, to EXTEND past it (an end past the list
// sheet's grid is kept as sent: tried on the test copy). Only these two kinds are touched: other rules, their order,
// formats and conditions stay as they are.
import { columnLetter } from './columns.mjs';

export const MARGIN = 1000;
export const EXTEND = 10_000;
/** The ID lists dropdowns take their choices from that grow with the data (sheet!column). */
export const GROWING_LISTS = ['Lists!Z', 'Lists!T', 'Insectary_stocks!A', 'Insectary_data!P'];

// An absolute one-column range on the rule's own sheet: `$A$2:$A$20846` (not `Lists!$A$2:...`, not `A:A`).
const OWN_RANGE = /(?<![!\w$])\$([A-Z]{1,3})\$(\d+):\$\1\$(\d+)(?![\w(])/g;
// Formulas whose ranges are computed: never touched.
const COMPUTED = /\b(INDIRECT|INDIRECTO|OFFSET|DESREF)\s*\(/i;
// A dropdown's list: `=Lists!$Z$2:$Z$3030` or `='Some sheet'!$A$3:$A$634`.
const LIST_SOURCE = /^=(?:'((?:[^']|'')+)'|([A-Za-z0-9_]+))!\$([A-Z]{1,3})\$(\d+):\$\3\$(\d+)$/;

export const quoteTitle = title => `'${title.replaceAll("'", "''")}'`;
const formulaOf = rule => rule.booleanRule?.condition?.type === 'CUSTOM_FORMULA' ? rule.booleanRule.condition.values?.[0]?.userEnteredValue : null;

/**
 * The conditional formats to move: `sheets` as the Sheets API gives them (properties with gridProperties.rowCount,
 * conditionalFormats). Each change: the rule at its index with only its formula's fixed ends moved; `rule` is the rule
 * as it was, for the way back. `behind`: ends more than the margin below the grid, left as they are.
 */
export function conditionalFormatPlan(sheets, { margin = MARGIN, extend = EXTEND } = {}) {
  const changes = [];
  const watched = [];
  const behind = [];
  for (const sheet of sheets) {
    const { sheetId, title } = sheet.properties;
    const grid = sheet.properties.gridProperties?.rowCount || 0;
    (sheet.conditionalFormats || []).forEach((rule, index) => {
      const formula = formulaOf(rule);
      if (!formula || COMPUTED.test(formula)) return;
      const ends = [...formula.matchAll(OWN_RANGE)].map(m => Number(m[3]));
      if (!ends.length) return;
      for (const end of ends) (end < grid - margin ? behind : watched).push({ sheet: title, index, end, grid });
      const moved = new Map(ends.filter(end => end >= grid - margin && end <= grid + margin).map(end => [end, grid + extend]));
      if (!moved.size) return;
      const next = structuredClone(rule);
      next.booleanRule.condition.values[0].userEnteredValue = formula.replace(OWN_RANGE, (all, column, start, end) =>
        moved.has(Number(end)) ? `$${column}$${start}:$${column}$${moved.get(Number(end))}` : all,
      );
      changes.push({ sheet: title, sheetId, index, grid, before: formula, after: next.booleanRule.condition.values[0].userEnteredValue, rule, next });
    });
  }
  return { changes, watched, behind };
}

/** A dropdown's list `{ sheet, column, start, end, key }`, or null when it is not a fixed one-column range. */
export function listSource(value) {
  const m = LIST_SOURCE.exec(value || '');
  if (!m) return null;
  const sheet = m[1] !== undefined ? m[1].replaceAll("''", "'") : m[2];
  return { sheet, column: m[3], start: Number(m[4]), end: Number(m[5]), key: `${sheet}!${m[3]}` };
}
const sourceOf = dv => (dv?.condition?.type === 'ONE_OF_RANGE' ? listSource(dv.condition.values?.[0]?.userEnteredValue) : null);

/** The cells holding a data validation, from the Sheets API's grid data of one sheet. */
export function validationCells(title, sheetId, data) {
  const cells = [];
  for (const block of data || []) {
    const r0 = block.startRow || 0, c0 = block.startColumn || 0;
    (block.rowData || []).forEach((row, i) =>
      (row.values || []).forEach((value, j) => {
        if (value.dataValidation) cells.push({ sheet: title, sheetId, row: r0 + i, column: c0 + j, dv: value.dataValidation });
      }),
    );
  }
  return cells;
}

/** Where a dropdown on a growing list should end: `{ last }` (the list column's last filled row) → its new end, or null. */
export function listTarget(end, list, { margin = MARGIN, extend = EXTEND } = {}) {
  if (!list || list.last + margin < end) return null;
  const target = list.last + extend;
  return target > end ? target : null;
}

/**
 * The dropdowns to move: `cells` (validationCells) whose list is a growing one (GROWING_LISTS) and whose list's last
 * row (`lists`: key → { last, grid }) came within the margin of their end. Each run down a column of cells with the
 * same rule becomes one setDataValidation with only the end changed; `inverse` puts each back.
 */
export function validationPlan(cells, lists, options = {}) {
  const growing = options.growing || GROWING_LISTS;
  const hits = [];
  for (const cell of cells) {
    const source = sourceOf(cell.dv);
    if (!source || !growing.includes(source.key)) continue;
    const target = listTarget(source.end, lists.get(source.key), options);
    if (target) hits.push({ ...cell, source, target });
  }
  hits.sort((a, b) => a.sheetId - b.sheetId || a.column - b.column || a.row - b.row);
  const runs = [];
  const same = (a, b) => JSON.stringify(a) === JSON.stringify(b);
  for (const hit of hits) {
    const last = runs.at(-1);
    if (last && last.sheetId === hit.sheetId && last.column === hit.column && last.end === hit.row && same(last.dv, hit.dv)) last.end++;
    else runs.push({ sheet: hit.sheet, sheetId: hit.sheetId, column: hit.column, start: hit.row, end: hit.row + 1, dv: hit.dv, source: hit.source, target: hit.target });
  }
  return runs.map(run => {
    const range = { sheetId: run.sheetId, startRowIndex: run.start, endRowIndex: run.end, startColumnIndex: run.column, endColumnIndex: run.column + 1 };
    const next = structuredClone(run.dv);
    const before = run.dv.condition.values[0].userEnteredValue;
    next.condition.values[0].userEnteredValue = before.replace(/\d+$/, String(run.target));
    return {
      sheet: run.sheet,
      cells: `${columnLetter(run.column)}${run.start + 1}:${columnLetter(run.column)}${run.end}`,
      before,
      after: next.condition.values[0].userEnteredValue,
      request: { setDataValidation: { range, rule: next } },
      inverse: { setDataValidation: { range, rule: run.dv } },
    };
  });
}

const enc = encodeURIComponent;
const lastFilled = values => {
  for (let i = (values || []).length - 1; i >= 0; i--) if (values[i]?.[0] !== undefined && values[i][0] !== '') return i + 1;
  return 0;
};

/** The cells a change writes: a dropdown's run, or a conditional format's covered ranges. */
const rangesOf = c => (c.request.setDataValidation ? [c.request.setDataValidation.range] : c.request.updateConditionalFormatRule.rule.ranges || []);
const overlaps = (a, b) =>
  (a.startRowIndex ?? 0) < (b.endRowIndex ?? Infinity) && (b.startRowIndex ?? 0) < (a.endRowIndex ?? Infinity) &&
  (a.startColumnIndex ?? 0) < (b.endColumnIndex ?? Infinity) && (b.startColumnIndex ?? 0) < (a.endColumnIndex ?? Infinity);
/**
 * The protections that keep the credential from a change: protected ranges of its sheet it cannot edit
 * (requestingUserCanEdit false) over its cells. One with exceptions (unprotectedRanges) is not counted: the write is
 * tried, and Google's refusal says.
 */
export function blockingProtections(sheet, change) {
  return (sheet?.protectedRanges || [])
    .filter(p => p.requestingUserCanEdit === false && !p.unprotectedRanges?.length && rangesOf(change).some(r => overlaps(p.range || {}, r)))
    .map(p => p.protectedRangeId)
    .sort((a, b) => a - b);
}

/**
 * What the night says about the changes left to their owners (`leftOut`, protected from the app), against `known`
 * (the state file of the nights before: key → entry). A range is told once, then again only when its situation
 * changes: its rule (before) or its protections changed, or its rows grew another margin since it was last told.
 * One that is no longer left out (moved, changed by hand, or writable again) is told once and forgotten.
 * Returns { state, lines, warn } (warn: something told as new or changed).
 */
export function leftOutReport(leftOut, known = {}, { margin = MARGIN, at } = {}) {
  const state = {};
  const lines = [];
  let warn = false;
  for (const c of leftOut) {
    const key = `${c.kind}:${c.label}`;
    const was = known[key];
    const protections = c.protections || [];
    const changed = !was || was.before !== c.before || String(was.protections) !== String(protections) || c.rows - was.toldRows >= margin;
    state[key] = { kind: c.kind, sheet: c.sheet, label: c.label, before: c.before, after: c.after, rows: c.rows, protections, since: was?.since || at, toldAt: changed ? at : was.toldAt, toldRows: changed ? c.rows : was.toldRows };
    if (!changed) continue;
    warn = true;
    lines.push(
      `Sheet ranges: WARNING: ${c.kind} ${c.label} is protected from the app: its owner moves ${c.before} to ${c.after} by hand${
        was ? ` (known since ${String(was.since).slice(0, 10)}; changed since told on ${String(was.toldAt).slice(0, 10)})` : ''
      }`,
    );
  }
  for (const [key, was] of Object.entries(known))
    if (!state[key]) lines.push(`Sheet ranges: ${was.kind} ${was.label} is no longer left out (moved, or changed by hand)`);
  const quiet = Object.values(state).filter(e => e.toldAt !== at);
  if (quiet.length)
    lines.push(`Sheet ranges: ${quiet.length} known range${quiet.length === 1 ? '' : 's'} still left for ${quiet.length === 1 ? 'its owner' : 'their owners'} (told before: sheet-ranges-state.json)`);
  return { state, lines, warn };
}

/**
 * One nightly pass: reads the rules, moves the ends that came near, reads them back. `request(path, options)` is a
 * GoogleSheets request (a fake in tests). Returns { code, lines, record }: code 0 (nothing near, or moved and checked),
 * 3 (a rule left out because the credential cannot edit its protected range, told for the first time or changed: the
 * owner moves it), 1 (not as expected). `record` holds every change with the rule as it was (`inverse`: the requests
 * that put them back) and `state`, the ranges left out for the next night (`options.known`: the last night's).
 * A change on a range the sheet's protections keep from the app is not sent.
 */
export async function keepRangesAhead(request, options = {}) {
  const lines = [];
  const fields = 'sheets(properties(sheetId,title,gridProperties(rowCount)),conditionalFormats)';
  const meta = await request(
    `?fields=${enc('sheets(properties(sheetId,title,gridProperties(rowCount)),conditionalFormats,protectedRanges(protectedRangeId,range,requestingUserCanEdit,unprotectedRanges))')}`,
  );
  const sheets = meta.sheets || [];
  const byTitle = new Map(sheets.map(s => [s.properties.title, s.properties]));
  const cf = conditionalFormatPlan(sheets, options);

  // Dropdowns: row 2 and the last row of every sheet show which columns take a growing list; those columns are read
  // whole (a block further down may end elsewhere). A column with such a dropdown in neither row is not seen.
  const sampled = new URLSearchParams({ fields: 'sheets(properties(sheetId,title),data(startRow,startColumn,rowData(values(dataValidation))))' });
  for (const s of sheets) {
    const rows = s.properties.gridProperties?.rowCount || 0;
    if (rows < 2) continue;
    sampled.append('ranges', `${quoteTitle(s.properties.title)}!2:2`);
    if (rows > 2) sampled.append('ranges', `${quoteTitle(s.properties.title)}!${rows}:${rows}`);
  }
  const samples = sheets.length ? (await request(`?${sampled}`)).sheets || [] : [];
  const sampleCells = samples.flatMap(s => validationCells(s.properties.title, s.properties.sheetId, s.data));
  const growing = options.growing || GROWING_LISTS;
  const near = new Map();
  for (const c of sampleCells) if (growing.includes(sourceOf(c.dv)?.key)) near.set(`${c.sheet}\u0000${c.column}`, c);
  let cells = [];
  if (near.size) {
    const p = new URLSearchParams({ fields: 'sheets(properties(sheetId,title),data(startRow,startColumn,rowData(values(dataValidation))))' });
    for (const c of near.values()) p.append('ranges', `${quoteTitle(c.sheet)}!${columnLetter(c.column)}:${columnLetter(c.column)}`);
    const whole = (await request(`?${p}`)).sheets || [];
    cells = whole.flatMap(s => validationCells(s.properties.title, s.properties.sheetId, s.data)).filter(c => near.has(`${c.sheet}\u0000${c.column}`));
  }
  // The lists those columns name (a block further down may name another).
  const used = [...new Set(cells.map(c => sourceOf(c.dv)?.key).filter(key => key && growing.includes(key)))];
  const lists = new Map();
  if (used.length) {
    const p = new URLSearchParams();
    for (const key of used) {
      const [sheet, column] = [key.slice(0, key.lastIndexOf('!')), key.slice(key.lastIndexOf('!') + 1)];
      p.append('ranges', `${quoteTitle(sheet)}!${column}:${column}`);
    }
    const read = await request(`/values:batchGet?${p}`);
    used.forEach((key, i) => {
      const sheet = key.slice(0, key.lastIndexOf('!'));
      lists.set(key, { last: lastFilled(read.valueRanges?.[i]?.values), grid: byTitle.get(sheet)?.gridProperties?.rowCount || 0 });
    });
  }
  const dv = validationPlan(cells, lists, options);

  const changes = [
    ...cf.changes.map(c => ({
      kind: 'conditional format',
      sheet: c.sheet,
      label: `${c.sheet} rule ${c.index + 1}`,
      before: c.before,
      after: c.after,
      note: `the grid has ${c.grid} rows`,
      rows: c.grid,
      request: { updateConditionalFormatRule: { sheetId: c.sheetId, index: c.index, rule: c.next } },
      inverse: { updateConditionalFormatRule: { sheetId: c.sheetId, index: c.index, rule: c.rule } },
      check: c,
    })),
    ...dv.map(d => {
      const source = listSource(d.before);
      const list = lists.get(source.key);
      return { kind: 'dropdown', sheet: d.sheet, label: `${d.sheet} ${d.cells}`, before: d.before, after: d.after, note: `${source.key} has ${list.last} rows of ${list.grid}`, rows: list.last, request: d.request, inverse: d.inverse };
    }),
  ];
  const record = { at: new Date(options.at ?? Date.now()).toISOString(), changes: [], leftOut: [] };
  const strip = ({ check, ...c }) => c;
  // Protected from the app, as the sheet's protections say: left out unsent.
  const bySheet = new Map(sheets.map(s => [s.properties.title, s]));
  for (let i = changes.length - 1; i >= 0; i--) {
    const protections = blockingProtections(bySheet.get(changes[i].sheet), changes[i]);
    if (protections.length) record.leftOut.unshift({ ...strip(changes.splice(i, 1)[0]), protections });
  }
  let code = 0;
  const report = () => {
    const r = leftOutReport(record.leftOut, options.known || {}, { margin: options.margin, at: record.at });
    record.state = r.state;
    record.toldLeftOut = r.warn;
    lines.push(...r.lines);
    if (r.warn && code === 0) code = 3;
  };
  for (const b of cf.behind)
    lines.push(`Sheet ranges: left as it is: ${b.sheet} rule ${b.index + 1} counts to row ${b.end}, over ${options.margin ?? MARGIN} rows short of the grid (${b.grid}): taken as meant`);
  if (!changes.length) {
    const nearest = cf.watched.sort((a, b) => a.end - a.grid - (b.end - b.grid))[0];
    lines.push(
      `Sheet ranges: ok, nothing ${record.leftOut.length ? 'else ' : ''}within ${options.margin ?? MARGIN} rows of its end${
        nearest ? ` (nearest: ${nearest.sheet} rule ${nearest.index + 1} counts to row ${nearest.end}, the grid has ${nearest.grid})` : ''
      }`,
    );
    report();
    return { code, lines, record };
  }

  // Before anything is sent, every planned change with the rule as it was, and each sheet's conditional formats as
  // read (an index shifted by an edit meanwhile would overwrite another rule): `beforeWrite` keeps them (the log file);
  // when it fails, nothing is sent. A write that may have landed is never without its way back.
  const firstRead = Object.fromEntries(
    sheets.filter(s => changes.some(c => c.check?.sheet === s.properties.title)).map(s => [s.properties.title, s.conditionalFormats]),
  );
  await options.beforeWrite?.({ at: record.at, status: 'planned', changes: changes.map(strip), conditionalFormatsAsRead: firstRead });

  // One atomic batchUpdate; a request on a range the credential cannot edit (protected: Google's 400 "Invalid
  // requests[i]") is left out and the rest sent.
  let pending = [...changes];
  try {
    for (;;) {
      if (!pending.length) break;
      try {
        await request(':batchUpdate', { method: 'POST', body: JSON.stringify({ requests: pending.map(c => c.request) }) });
        break;
      } catch (e) {
        const m = /Invalid requests\[(\d+)\]\.\w+: .*(protegid|protected)/i.exec(e.message || '');
        if (!m || !pending[Number(m[1])]) throw e;
        record.leftOut.push(...pending.splice(Number(m[1]), 1).map(strip));
      }
    }
  } catch (e) {
    // Sent or not (a timed-out write may have landed): the planned record says what to check and how to undo it.
    report();
    record.unconfirmed = pending.map(strip);
    lines.push(`Sheet ranges: FAILING: the write did not confirm (${String(e.message).slice(0, 200)}): check the planned changes in the log`);
    return { code: 1, lines, record };
  }
  report();

  for (const c of pending) {
    record.changes.push(strip(c));
    lines.push(`Sheet ranges: moved ${c.kind} ${c.label}: ${c.before} → ${c.after} (${c.note})`);
  }
  try {
    if (pending.length && (await readBack(request, sheets, pending, lines, fields))) code = 1;
  } catch (e) {
    lines.push(`Sheet ranges: FAILING: the read-back failed (${String(e.message).slice(0, 200)}): check the changes in the log`);
    code = 1;
  }
  return { code, lines, record };
}

/** The read-back: 1 when a sheet's rules or a dropdown read back other than sent, else 0. */
async function readBack(request, sheets, pending, lines, fields) {
  let code = 0;
  // In each sheet with a moved rule: that rule as sent, the others unchanged.
  const after = await request(`?fields=${enc(fields)}`);
  const afterBy = new Map((after.sheets || []).map(s => [s.properties.title, s]));
  for (const s of sheets.filter(s => pending.some(c => c.check?.sheet === s.properties.title))) {
    const now = afterBy.get(s.properties.title)?.conditionalFormats || [];
    const want = (s.conditionalFormats || []).map(
      (rule, index) => pending.find(c => c.check && c.check.sheet === s.properties.title && c.check.index === index)?.check.next || rule,
    );
    if (JSON.stringify(now) !== JSON.stringify(want)) {
      code = 1;
      lines.push(`Sheet ranges: FAILING: ${s.properties.title}'s conditional formats read back other than sent (someone edited them meanwhile?): compare with conditionalFormatsAsRead in the log`);
    }
  }
  const dvMoved = pending.filter(c => c.kind === 'dropdown');
  if (dvMoved.length) {
    const p = new URLSearchParams({ fields: 'sheets(properties(sheetId,title),data(startRow,startColumn,rowData(values(dataValidation))))' });
    for (const c of dvMoved) {
      const r = c.request.setDataValidation.range;
      p.append('ranges', `${quoteTitle(c.sheet)}!${columnLetter(r.startColumnIndex)}${r.startRowIndex + 1}:${columnLetter(r.startColumnIndex)}${r.endRowIndex}`);
    }
    const read = (await request(`?${p}`)).sheets || [];
    const got = new Map(read.flatMap(s => validationCells(s.properties.title, s.properties.sheetId, s.data)).map(c => [`${c.sheetId}:${c.row}:${c.column}`, c.dv]));
    for (const c of dvMoved) {
      const { range, rule } = c.request.setDataValidation;
      for (let row = range.startRowIndex; row < range.endRowIndex; row++)
        if (JSON.stringify(got.get(`${range.sheetId}:${row}:${range.startColumnIndex}`)) !== JSON.stringify(rule)) {
          code = 1;
          lines.push(`Sheet ranges: FAILING: dropdown ${c.label} reads back other than sent (row ${row + 1})`);
          break;
        }
    }
  }
  return code;
}
