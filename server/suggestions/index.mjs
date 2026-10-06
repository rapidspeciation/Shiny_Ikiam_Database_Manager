// Suggested edits: corrections the app can compute from the workbook itself,
// each with how sure it is, for the team to look at in Revisión → Sugerencias
// (and the assistant, list_suggested_edits). Nothing here writes: there is no
// apply button. A person who agrees asks the assistant to make an ordinary
// proposal from some of them (propose_changes), which they confirm as always.
//
// Sources are pluggable. A source is a module whose default export is
//
//   {
//     id: 'tubes',                     // short and stable (letters, digits, _): part of every key
//     title: tpl('Tubos con un dígito de más o de menos'),   // Spanish; English in
//     describe: tpl('…'),              //   frontend/src/locales/en/server-built.ts
//     revision(store) { return '…' },  // optional: changes when data outside the
//                                      //   sheets it reads changes (stored walks…)
//     async suggest(ctx) { return [suggestion, …] },
//     byGroup?: true,                  // listed group by group (sheet and column), with counts
//   }
//
// and ctx is
//
//   store       the Store (store.db for the app's own tables)
//   sheets      Map sheet → rows of the local copy, read-only and shared:
//               { id, sheet, row, observed, label, values, formulas }
//               (observed = false: an empty pre-made row)
//   observed(sheet)   the rows of a sheet with data
//   byId        Map recordId → row
//   issues()    the checks' issues (server/checks.mjs)
//   today       today as a sheet date serial (Ecuador); iso(serial) → 'YYYY-MM-DD'
//   shown(sheet, field, value)   a cell as people read it (dates as YYYY-MM-DD)
//   ref(row, field)   { sheet, row, recordId, label, field, value } for `related`
//   lastEdit(recordId, field)    the last saved change of that cell in the
//               history: { at, user, purpose, before, after } or null
//
// A suggestion is
//
//   { sheet, row, recordId, label?, field,
//     current,      the cell now (shown())
//     suggested,    the value proposed; null = a person has to decide (certainty 'check')
//     certainty,    'certain' (only the spelling changes) | 'likely' (strong evidence,
//                   a person still looks) | 'check' (a lead, needs someone who knows)
//     reason,       msg() from server/messages.mjs (or plain Spanish): the evidence
//     related?,     [ref(row, field)] other rows that show it (server/checks.mjs ref)
//     group?,       suggestions that go together (the two cells of one species; a
//                   byGroup source's heading, e.g. "Collection_data · Death_date")
//     manual?       true: made by hand in Google Sheets (a formula, or typing over one),
//                   which the app's proposals cannot write
//   }
//
// One suggestion per cell and source: its key is source:recordId:field, kept in
// the findings table (server/findings.mjs), so a suggestion the sheet no longer
// needs moves to «Resueltos». Register a new source in SOURCES below (or with
// registerSource) and add its English texts; tests/suggestions.test.mjs shows
// the shape a source must give.

import { textFields } from '../messages.mjs';
import { freshIssues, iso, recordsStamp, ref, sheetRows, shown, todaySerial } from '../checks.mjs';
import { firstSeen, trackFindings } from '../findings.mjs';
import pedigree from './pedigree.mjs';
import twins from './twins.mjs';
import tubes from './tubes.mjs';
import checkFixes from './check-fixes.mjs';
import spaces from './spaces.mjs';
import formulas from './formulas.mjs';
import dates from './dates.mjs';
import * as wikilocTransects from './wikiloc-transects.mjs';

export const CERTAINTIES = ['certain', 'likely', 'check'];
/** In the order they are listed. */
const SOURCES = [checkFixes, spaces, formulas, dates, tubes, twins, pedigree, wikilocTransects];

/**
 * Adds a source (see the header); its id must be new. The app computes the suggestions in a worker
 * thread (server/checks-host.mjs), which loads this module afresh: a source the app lists is in SOURCES.
 */
export function registerSource(source) {
  if (!/^\w+$/.test(source?.id ?? '') || typeof source.suggest !== 'function')
    throw new Error('A suggestion source needs an id (letters, digits, _) and suggest(ctx)');
  if (SOURCES.some(s => s.id === source.id)) throw new Error(`Suggestion source ${source.id} is already registered`);
  SOURCES.push(source);
  return source;
}
export const suggestionSources = () => [...SOURCES];

/** The last saved change of a cell (history), for sources that weigh which side was corrected. */
function historyReader(db) {
  const query = db.prepare(
    `SELECT a.actor, a.purpose, a.created_at, c.before_json, c.after_json FROM changes c JOIN actions a ON a.id = c.action_id
     WHERE c.record_id = ? AND c.field = ? AND c.moved = 0 AND a.status != 'failed' ORDER BY a.created_at DESC LIMIT 1`,
  );
  const users = db.prepare('SELECT display_name FROM users WHERE id = ?');
  const parse = text => (text === null || text === undefined ? null : JSON.parse(text));
  return (recordId, field) => {
    const r = query.get(String(recordId), String(field));
    if (!r) return null;
    return {
      at: r.created_at,
      user: r.actor === 'unknown' ? null : (users.get(r.actor)?.display_name ?? r.actor),
      purpose: r.purpose,
      before: parse(r.before_json),
      after: parse(r.after_json),
    };
  };
}

const cache = new WeakMap();
async function compute(store) {
  // The checks' issues (Revisión), found in their worker thread where there is one.
  const { issues } = await freshIssues(store);
  const sheets = sheetRows(store);
  const byId = new Map();
  for (const rows of sheets.values()) for (const r of rows) byId.set(r.id, r);
  const ctx = {
    store,
    sheets,
    observed: sheet => (sheets.get(sheet) || []).filter(r => r.observed),
    byId,
    issues: () => issues,
    today: todaySerial(),
    iso,
    shown,
    ref,
    lastEdit: historyReader(store.db),
  };
  const items = [];
  const timings = {};
  for (const source of SOURCES) {
    const started = Date.now();
    const seen = new Set();
    for (const s of (await source.suggest(ctx)) ?? []) {
      if (!CERTAINTIES.includes(s.certainty)) throw new Error(`${source.id}: certainty must be one of ${CERTAINTIES.join(', ')}`);
      if (s.suggested === null && s.certainty !== 'check') throw new Error(`${source.id}: a suggestion without a value needs certainty check`);
      const key = `${source.id}:${s.recordId}:${s.field}`;
      if (seen.has(key)) continue;
      seen.add(key);
      const { reason, ...rest } = s;
      items.push({
        key,
        source: source.id,
        ...rest,
        label: s.label ?? byId.get(s.recordId)?.label ?? '',
        current: s.current ?? null,
        ...textFields('reason', reason ?? ''),
      });
    }
    timings[source.id] = Date.now() - started;
  }
  return { items, timings };
}

/** The state of what the suggestions read: the local copy, the day and each source's own data. */
export const suggestionsStamp = store =>
  [recordsStamp(store), todaySerial(), ...SOURCES.map(s => s.revision?.(store) ?? '')].join(':');

/**
 * Every suggestion, computed again only when the local copy, the day or a source's own data
 * changed: { stamp, items, computedAt, ms, timings }. In the app they are computed in the
 * Revisión worker thread (server/checks-host.mjs), so people's requests are answered meanwhile.
 */
export async function allSuggestions(store) {
  const runner = runners.get(store);
  return runner ? runner.fresh() : suggestionsHere(store);
}
/** The same, computed here in this thread (the worker, the tests, the app when the worker cannot). */
export async function suggestionsHere(store) {
  const stamp = suggestionsStamp(store);
  const hit = cache.get(store);
  if (hit?.stamp === stamp) return hit.ready ?? hit.promise;
  const entry = { stamp };
  entry.promise = (async () => {
    const started = Date.now();
    const { items, timings } = await compute(store);
    entry.ready = keepSuggestions(store, { stamp, items, computedAt: new Date().toISOString(), ms: Date.now() - started, timings });
    return entry.ready;
  })();
  cache.set(store, entry);
  try {
    return await entry.promise;
  } catch (e) {
    if (cache.get(store) === entry) cache.delete(store);
    throw e;
  }
}

/**
 * Suggestions as the app keeps them, found here or in the worker: those new and those gone go to
 * «Resueltos» (server/findings.mjs); the cached answer while what they read is as it was.
 */
export function keepSuggestions(store, entry) {
  trackFindings(
    store,
    'suggestion',
    entry.items.map(s => ({
      key: s.key,
      kind: s.source,
      sheet: s.sheet,
      row: s.row,
      recordId: s.recordId,
      field: s.field,
      label: s.label,
      value: { current: s.current, suggested: s.suggested, certainty: s.certainty },
      text: s.reason,
      textMsg: s.reasonMsg,
    })),
    { at: entry.computedAt },
  );
  if (entry.stamp === suggestionsStamp(store)) cache.set(store, { stamp: entry.stamp, ready: entry });
  return entry;
}
/** The kept suggestions when they are still up to date, else null. */
export function cachedSuggestions(store) {
  const hit = cache.get(store);
  return hit?.ready && hit.stamp === suggestionsStamp(store) ? hit.ready : null;
}

/** Who computes the suggestions for the app's requests (server/checks-host.mjs: a worker thread), by store. */
const runners = new WeakMap();
export function useSuggestionsRunner(store, runner) {
  if (runner) runners.set(store, runner);
  else runners.delete(store);
}

const fail = (code, message, status = 400) => Object.assign(new Error(message), { code, status });
const ORDER = Object.fromEntries(CERTAINTIES.map((c, i) => [c, i]));

/**
 * The suggestions after the filters (source and certainty, comma-separated;
 * sheet; one row; a text in the row's label, values or reason), surest first
 * and newest rows first inside each source, with the counts per source and
 * certainty. `all`: every one (the CSV).
 */
export async function suggestionPage(store, query = {}, { all = false } = {}) {
  const list = v =>
    String(v ?? '')
      .split(',')
      .map(x => x.trim())
      .filter(Boolean);
  const sources = list(query.source);
  const certainties = list(query.certainty);
  const known = new Set(SOURCES.map(s => s.id));
  const unknown = sources.find(s => !known.has(s));
  if (unknown) throw fail('INVALID_SOURCE', `Unknown source ${unknown.slice(0, 40)}; use ${[...known].join(', ')}`);
  const badCertainty = certainties.find(c => !CERTAINTIES.includes(c));
  if (badCertainty) throw fail('INVALID_CERTAINTY', `certainty must be ${CERTAINTIES.join(', ')}`);
  const { items, computedAt, ms } = await allSuggestions(store);
  const text = String(query.q ?? '')
    .trim()
    .toLowerCase();
  const base = items.filter(
    s =>
      (!query.sheet || s.sheet === query.sheet) &&
      (!query.recordId || s.recordId === query.recordId) &&
      (!text || `${s.label} ${s.current ?? ''} ${s.suggested ?? ''} ${s.reason}`.toLowerCase().includes(text)),
  );
  const counts = Object.fromEntries(SOURCES.map(s => [s.id, Object.fromEntries([...CERTAINTIES, 'total'].map(c => [c, 0]))]));
  for (const s of base) {
    counts[s.source][s.certainty]++;
    counts[s.source].total++;
  }
  // A source listed by group (byGroup) keeps each group together: sheet and column, then certainty.
  const byGroup = new Set(SOURCES.filter(s => s.byGroup).map(s => s.id));
  const groupOrder = (a, b) => (byGroup.has(a.source) ? String(a.group ?? '').localeCompare(String(b.group ?? '')) : 0);
  const chosen = base
    .filter(s => (!sources.length || sources.includes(s.source)) && (!certainties.length || certainties.includes(s.certainty)))
    .sort(
      (a, b) =>
        SOURCES.findIndex(x => x.id === a.source) - SOURCES.findIndex(x => x.id === b.source) ||
        groupOrder(a, b) ||
        ORDER[a.certainty] - ORDER[b.certainty] ||
        a.sheet.localeCompare(b.sheet) ||
        (b.row ?? 0) - (a.row ?? 0),
    );
  // How many of the filtered suggestions each group has, with its certainties.
  const groups = {};
  for (const s of chosen) {
    if (!byGroup.has(s.source) || !s.group) continue;
    const g = (groups[s.group] ??= { total: 0, certain: 0, likely: 0, check: 0 });
    g.total++;
    g[s.certainty]++;
  }
  const size = all ? chosen.length : Math.min(Math.max(Number(query.limit) || 50, 1), 500);
  const start = all ? 0 : Math.max(Number(query.offset) || 0, 0);
  const page = chosen.slice(start, start + size);
  // Since when each is suggested (server/findings.mjs); not needed for the CSV.
  const seen = all ? new Map() : firstSeen(store, 'suggestion', page.map(s => s.key));
  return {
    computedAt,
    ms,
    total: chosen.length,
    offset: start,
    limit: size,
    sources: SOURCES.map(s => ({ id: s.id, title: s.title, describe: s.describe, counts: counts[s.id], ...(s.byGroup ? { byGroup: true } : {}) })),
    groups,
    sheets: [...new Set(items.map(s => s.sheet))].sort(),
    items: seen.size ? page.map(s => (seen.has(s.key) ? { ...s, firstSeen: seen.get(s.key) } : s)) : page,
  };
}

const COLUMNS = ['source', 'certainty', 'sheet', 'row', 'label', 'field', 'current', 'suggested', 'manual', 'reason', 'recordId'];
/**
 * The filtered suggestions as CSV, or tab-separated for pasting into a sheet
 * (`tsv`); the reasons in Spanish, as the assistant reads them.
 */
export async function suggestionsCsv(store, query = {}, { tsv = false } = {}) {
  const page = await suggestionPage(store, query, { all: true });
  const cell = value => {
    const t = value === null || value === undefined || value === false ? '' : String(value);
    if (tsv) return t.replace(/[\t\r\n]+/g, ' ');
    return /[",\n\r]/.test(t) ? `"${t.replace(/"/g, '""')}"` : t;
  };
  const sep = tsv ? '\t' : ',';
  const lines = [COLUMNS.join(sep)];
  for (const s of page.items) lines.push(COLUMNS.map(c => cell(s[c])).join(sep));
  return lines.join('\r\n') + '\r\n';
}
