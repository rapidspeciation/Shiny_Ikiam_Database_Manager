// The match_notebook tool: a notebook page Claude transcribed (in T3 Code or
// the chat, following the digitalizar-cuaderno skill) is matched line by line
// with the sheet by the rules of server/notebook.mjs (look-alike IDs, ditto
// marks already expanded by Claude, CAM/tube runs, the page's year, counts as
// sums, the SPECIES formula) and turned into one proposal the person reviews
// beside the chat. Nothing is written until they apply it.

import { moduleMap } from './schema.mjs';
import { TUBE_FIELD, isIdValue, isUnique } from './verifications.mjs';
import { listOptions } from './verify.mjs';
import { KINDS, KIND_IDS, buildReview, checkTranscription, clutchKey, proposalRows, typeOf } from './notebook.mjs';

const parse = (value, fallback) => {
  try {
    return JSON.parse(value) ?? fallback;
  } catch {
    return fallback;
  }
};
const clip = (value, length) => String(value ?? '').slice(0, length);
const ecuadorDay = () => new Intl.DateTimeFormat('en-CA', { timeZone: 'America/Guayaquil' }).format(new Date());
const isoOf = serial => new Date(Date.UTC(1899, 11, 30) + serial * 864e5).toISOString().slice(0, 10);

/** The tool's description and schema, with each notebook's columns taken from KINDS. */
export const MATCH_NOTEBOOK_TOOL = {
  type: 'function',
  function: {
    name: 'match_notebook',
    description: [
      'Match a transcribed notebook page (or envelopes/labels) with the sheet and draft ONE proposal from it, shown at once beside the chat (Cambios propuestos). Follow the digitalizar-cuaderno skill.',
      'Give every line of the page, top to bottom, with the values as written (dates day/month as written, e.g. "17/9"; ditto marks already replaced by the value above; CAMs/tubes written short like "cam505" or "81" may stay short, they continue the one above; counts as written, e.g. "12+15").',
      'The server finds each line\'s row (also through look-alike IDs 0/O, 1/I, 5/S and the order of the rows), infers the year, completes list values, keeps the SPECIES formula unless what emerged differs, and checks lists, IDs and tubes already used.',
      'It returns per line: the row found, cells to fill, differences with the sheet, doubtful cells (left out of the proposal), problems, and the proposalId. Doubtful cells go in with a confidence below 0.8 and their other readings.',
      `Columns per kind: ${KIND_IDS.map(id => `${id} (${KINDS[id].label}, ${KINDS[id].sheet}): ${KINDS[id].fields.join(', ')}`).join('; ')}.`,
    ].join(' '),
    parameters: {
      type: 'object',
      properties: {
        kind: { type: 'string', enum: KIND_IDS, description: 'Which notebook the page is' },
        year: { type: 'integer', description: 'The year of the page, only if written on it (a header, a sticky note, a full date)' },
        title: { type: 'string', description: 'Short name of the page for the proposal, e.g. "posturas 120–134"' },
        lines: {
          type: 'array',
          description: 'One entry per written line, top to bottom (up to 150)',
          items: {
            type: 'object',
            properties: {
              raw: { type: 'string', description: 'The line as written, short, keeping abbreviations and symbols' },
              values: { type: 'object', description: 'Column → text as read. null = cannot read it' },
              confidence: { type: 'object', description: 'Column → 0..1, only for cells you are not sure of' },
              alternatives: { type: 'object', description: 'Column → other possible readings (up to 3)' },
              crossedOut: { type: 'boolean', description: 'The line is crossed out or marked "no se usó el ID"' },
            },
            required: ['raw', 'values'],
          },
        },
        replaceProposalId: {
          type: 'string',
          description: 'A pending proposal of this page to replace (after the person corrects a reading)',
        },
      },
      required: ['kind', 'lines'],
    },
  },
};

/**
 * deps: store, db, newIds() (IDs already used, for the checks), draftChanges(args, ids),
 * initialsFor(user).
 */
export function createNotebookMatcher({ store, db, newIds, draftChanges, initialsFor }) {
  // ---- The sheet, as the review reads it --------------------------------
  const indexes = new Map();
  /** Rows by their key columns (normalized: "685 (3)" = "685(3)"), rebuilt when the sheet changes. */
  function keyIndex(sheet, keys) {
    const mod = moduleMap.get(sheet);
    // Both from indexes (a max() filtered on `missing` read every row: 100 ms a call).
    const stamp = db
      .prepare(
        'SELECT (SELECT count(*) FROM records WHERE sheet = ? AND missing = 0) n, (SELECT max(updated_at) FROM records WHERE sheet = ?) u',
      )
      .get(sheet, sheet);
    const cacheKey = `${sheet}\u0000${keys.join('|')}`;
    const hit = indexes.get(cacheKey);
    if (hit?.stamp === `${stamp.n}:${stamp.u}`) return hit.map;
    const map = new Map();
    const columns = keys.map((k, i) => `json_extract(values_json, '$."${k.replaceAll('"', '')}"') k${i}`).join(', ');
    for (const r of db
      .prepare(`SELECT id, ${columns} FROM records WHERE sheet = ? AND missing = 0 AND row_num > ?`)
      .all(sheet, mod.headerRow)) {
      const values = keys.map((_, i) => r[`k${i}`]);
      if (values.some(v => v === null || v === '')) continue;
      const key = values.map(clutchKey).join('|');
      map.set(key, [...(map.get(key) ?? []), { id: r.id, value: values[0] }]);
    }
    indexes.set(cacheKey, { stamp: `${stamp.n}:${stamp.u}`, map });
    return map;
  }

  /** Columns that are formulas in the next unused row of a sheet (a new row leaves them). */
  function newRowFormulas(sheet) {
    const last =
      db.prepare('SELECT max(row_num) n FROM records WHERE sheet=? AND missing=0 AND observed=1').get(sheet).n ??
      moduleMap.get(sheet).headerRow;
    const next = db
      .prepare(
        'SELECT formulas_json FROM records WHERE sheet=? AND missing=0 AND observed=0 AND row_num>? ORDER BY row_num LIMIT 1',
      )
      .get(sheet, last);
    return new Set(Object.keys(parse(next?.formulas_json ?? '{}', {})));
  }

  /** A page may be matched several times while it is corrected: the slower lookups are kept a few seconds. */
  const memo = new Map();
  const remembered = (key, ms, make) => {
    const hit = memo.get(key);
    if (hit && hit.until > Date.now()) return hit.value;
    const value = make();
    memo.set(key, { until: Date.now() + ms, value });
    return value;
  };
  const listsOf = sheet => remembered(`lists:${sheet}`, 3000, () => listOptions(store, sheet));
  // IDs used anywhere guide the review only (the save checks them again), so a short-lived copy will do.
  const usedIds = () => remembered('ids', 30000, () => newIds().used());
  const initials = user => remembered(`ini:${user.id ?? user.username}`, 600000, () => initialsFor(user));

  function lookupFor(sheet, keys) {
    const lists = listsOf(sheet);
    let own, stocks;
    const mine = () => (own ??= keyIndex(sheet, keys));
    const clutches = () => (stocks ??= keyIndex('Insectary_stocks', ['CLUTCH NUMBER']));
    const record = id => {
      const r = store.getRecord(id);
      return r && { id: r.id, row: r.row, version: r.version, label: r.label, values: r.values, formulas: r.formulas };
    };
    return {
      find: values => (mine().get(values.map(clutchKey).join('|')) ?? []).map(h => record(h.id)).filter(Boolean),
      clutch: value => clutches().get(clutchKey(value))?.[0]?.value ?? null,
      speciesOfClutch: value => {
        const hit = clutches().get(clutchKey(value))?.[0];
        return hit ? (store.getRecord(hit.id)?.values?.SPECIES ?? null) : null;
      },
      list: field => lists[field],
      holder: (field, value, recordId) => {
        if (!(isUnique(sheet, field) || TUBE_FIELD.test(field)) || !isIdValue(value)) return null;
        const unique = usedIds();
        const key = `${TUBE_FIELD.test(field) ? 'tube' : `${sheet}:${field}`}\u0000${String(value).trim()}`;
        // Another row, or another column of this row (the clip's tube already filed as Tube_2_id).
        return (unique.get(key) ?? []).find(h => h.id !== recordId || h.field !== field) ?? null;
      },
      newRowFormulas: newRowFormulas(sheet),
      typedOverFormula: new Set(sheet === 'Insectary_data' ? ['SPECIES'] : []),
    };
  }

  /**
   * The page matched with its sheet: the review (every line and cell) and the
   * proposal's rows, each checked as the save will check it (a bad row is left
   * out and reported, the rest still go).
   */
  function match(args, user) {
    const { transcription, ignored } = checkTranscription({ kind: args.kind, year: args.year, lines: args.lines });
    const kind = KINDS[transcription.kind];
    const review = buildReview({
      transcription,
      today: ecuadorDay(),
      initials: initials(user),
      lookup: lookupFor(kind.sheet, kind.keys),
    });
    const rows = proposalRows(review);
    const ids = newIds();
    const changes = [];
    for (const [key, list] of [
      ['newRows', rows.newRows],
      ['changes', rows.changes],
    ])
      for (const row of list) {
        const out = draftChanges({ [key]: [row] }, ids);
        const line = review.lines.find(l => l.n === row.line);
        if (out.error) {
          if (!/already in the sheet/.test(out.error)) line.rowError = clip(out.error.replace(/^newRows\[0\]: /, ''), 300);
          continue;
        }
        changes.push(...out.changes.map(c => ({ ...c, line: row.line })));
      }
    return { review, changes, ignored };
  }

  return { match };
}

/** What the tool tells Claude about the matched page: per line only what matters (not the equal cells). */
export function matchSummary({ review, changes, ignored }, proposalId) {
  const show = (field, value) =>
    typeOf(field) === 'date' && typeof value === 'number' ? isoOf(value) : value === undefined ? null : value;
  const inProposal = new Set(changes.map(c => c.line));
  const lines = review.lines.map(l => {
    const out = { n: l.n, raw: l.raw, status: l.status };
    if (l.row) Object.assign(out, { row: l.row, label: l.label });
    if (l.message) out.message = l.message;
    const group = {};
    for (const [field, cell] of Object.entries(l.cells)) {
      const put = (name, value) => ((group[name] ??= {})[field] = value);
      const notebook = show(field, cell.value);
      if (cell.status === 'unread') put('unread', true);
      else if (cell.status === 'error') put('problems', cell.message);
      else if (cell.status === 'formula')
        put('notWritten', cell.message ?? 'formula column');
      else if (cell.doubt && ['fill', 'conflict', 'new'].includes(cell.status))
        put('doubtful', {
          read: notebook,
          alternatives: cell.alternatives.map(a => show(field, a)),
          sheet: show(field, cell.before),
          ...(cell.message ? { note: cell.message } : {}),
        });
      else if (cell.status === 'conflict')
        put('differs', { sheet: show(field, cell.before), notebook, ...(cell.message ? { note: cell.message } : {}) });
      else if (cell.status === 'fill' || cell.status === 'new') put(cell.status === 'new' ? 'newRow' : 'fill', cell.write ?? notebook);
      else if (cell.status === 'same') out.same = (out.same ?? 0) + 1;
    }
    if (group.unread) group.unread = Object.keys(group.unread);
    Object.assign(out, group);
    if (l.rowError) out.rowError = l.rowError;
    out.inProposal = inProposal.has(l.n);
    return out;
  });
  const c = review.counts;
  return {
    kind: review.kind,
    sheet: review.sheet,
    year: review.year,
    yearSource: review.yearSource,
    counts: {
      lines: c.lines,
      rowsInProposal: inProposal.size,
      cellsToFill: c.fills,
      differences: c.conflicts,
      doubtful: c.doubts,
      problems: c.errors,
      newRows: c.created,
      same: c.same,
    },
    proposalId: proposalId ?? null,
    ...(ignored.length ? { ignoredColumns: ignored } : {}),
    lines,
  };
}
