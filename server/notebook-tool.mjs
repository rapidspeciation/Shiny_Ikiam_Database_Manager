// The match_notebook tool: a notebook page Claude transcribed (in T3 Code,
// following the digitalizar-cuaderno skill) is matched line by line
// with the sheet by the rules of server/notebook.mjs (look-alike IDs, ditto
// marks already expanded by Claude, CAM/tube runs, the page's year, counts as
// sums, the SPECIES formula) and turned into one proposal the person reviews
// beside the chat. Nothing is written until they apply it.

import { moduleMap } from './schema.mjs';
import { newRowFormulaFields } from './premade.mjs';
import { TUBE_FIELD, isIdValue, isUnique, twinRows } from './verifications.mjs';
import { listOptions } from './verify.mjs';
import {
  KINDS,
  KIND_IDS,
  RECENT_DAYS,
  buildReview,
  checkTranscription,
  clutchKey,
  columnsOf,
  isNone,
  nearIds,
  proposalRows,
  sameErrorRows,
  typeOf,
  unreadableOf,
} from './notebook.mjs';

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
      'Match a transcribed notebook page (or envelopes/labels) with the sheet and draft ONE proposal, shown at once beside the chat. Follow the digitalizar-cuaderno skill.',
      '',
      'Send every line, top to bottom, values as written:',
      '- dates as written ("17/9"); ditto marks replaced by the value above (a brace or ditto over many lines can go once in spans); short CAMs/tubes ("cam505", "81") may stay short;',
      '- counts as written ("12+15"; a corrected count as "12=9=4"); INSECTARY OR LABORATORY ("ins/oda") and notes columns as written.',
      '- Doubtful cell: your best reading as the value, confidence < 0.8, up to 3 alternatives and a short reason; it is highlighted.',
      '- Unreadable cell: null (never leave it out), a reason, and any partial reading in alternatives; it shows empty for the person to fill and is never written empty.',
      '- A cell left empty on the page: leave its column out.',
      '',
      'The server does the rest:',
      '- finds each row (look-alike IDs 0/O, 1/I, 5/S, row order) and infers the year;',
      '- completes list values; keeps the SPECIES formula unless what emerged differs;',
      '- writes notes as "d/m/yy INI: text" after the existing note;',
      '- turns owner codes, generations ("(F1)"), dashes and note words ("ethanol", "wc", a CAM…) into their columns, and fills a death\'s template;',
      '- flags (as doubtful, never silently) clutches, CAMs and tubes that break the run around them. Implied values never replace a value the row has.',
      'includeUnchanged: lines already in the sheet show as context rows (never written).',
      '',
      'Answer: proposalId, year/yearSource, counts, and per line its status (match, new, missing with didYouMean, ambiguous, duplicate, nokey, crossed), inProposal, rowError, warnings and the cells by group (fill, differs, doubtful, unreadable, implied, kept, notWritten, problems).',
      "- Tell the person about missing/ambiguous lines, rowError, differs and warnings (e.g. a clutch's adults unlike the butterflies typed in Insectary_data).",
      '- sameErrorNearby: rows near the page, not on its photo, whose ID in the same column has the typing slip a line of the page corrects (a digit missing or extra, two swapped, prefix, zeros). Those with inProposal are in the proposal after the page\'s rows, as doubtful cells; alreadyIn names another pending proposal that writes them.',
      '- wildWithoutCollection: wild-caught butterflies without their Collection_data row, drafted for you to complete.',
      '',
      'Columns per kind (exact names):',
      ...KIND_IDS.map(
        id =>
          `- ${id} (${KINDS[id].label}, ${KINDS[id].sheet}): ${columnsOf(KINDS[id]).join(', ')}${KINDS[id].aliases ? ` (also accepted: ${Object.entries(KINDS[id].aliases).map(([a, f]) => `${a} = ${f}`).join(', ')})` : ''}`,
      ),
    ].join('\n'),
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
              values: {
                type: 'object',
                description: 'Column → text as read. null = cannot read it at all: shown to the person as an unreadable cell to fill (give why in reasons)',
              },
              confidence: { type: 'object', description: 'Column → 0..1, only for cells you are not sure of' },
              alternatives: {
                type: 'object',
                description: 'Column → other possible readings (up to 3); for an unreadable cell (null), the part that could be read, as written (e.g. "1?/9")',
              },
              reasons: {
                type: 'object',
                description:
                  'Column → why the cell is doubtful or unreadable, a few words the person reads (e.g. "1 or 7: this hand", "smudged", "cut off by the photo edge")',
              },
              crossedOut: { type: 'boolean', description: 'The line is crossed out or marked "no se usó el ID"' },
              photo: { type: 'integer', description: 'With several photos: which one the line is on (0 = the first)' },
            },
            required: ['raw', 'values'],
          },
        },
        spans: {
          type: 'array',
          description:
            'A value a brace or ditto marks give to a run of lines, once: the column, the value, and the first and last line by their key (Insectary_ID, CLUTCH NUMBER…). Fills the lines between (both included) that leave that column out.',
          items: {
            type: 'object',
            properties: {
              field: { type: 'string' },
              value: { type: 'string' },
              from: { type: 'string', description: 'Key of the first line the brace covers' },
              to: { type: 'string', description: 'Key of the last line the brace covers' },
            },
            required: ['field', 'value', 'from', 'to'],
          },
        },
        photo: {
          description:
            'The photo of the page: the file name of the attachment (from "[Attached image … saved at …]" in the chat), or a list of them when the lines come from several photos. Shown beside the proposal.',
          anyOf: [{ type: 'string' }, { type: 'array', items: { type: 'string' } }],
        },
        rotate: {
          description: 'Clockwise turn that makes the photo upright (0, 90, 180, 270), as given to crops.py; a list for several photos',
          anyOf: [{ type: 'integer', enum: [0, 90, 180, 270] }, { type: 'array', items: { type: 'integer', enum: [0, 90, 180, 270] } }],
        },
        replaceProposalId: {
          type: 'string',
          description: 'A pending proposal of this page to replace (after the person corrects a reading)',
        },
        includeUnchanged: {
          type: 'boolean',
          description:
            'Also show the lines already in the sheet (nothing to write) as grey context rows, so the table follows the whole page. Context rows are never written.',
        },
      },
      required: ['kind', 'lines'],
    },
  },
};

/**
 * Collection_data's fixed cells per kind of record, as the team types them now
 * (field.md §2): only the cells a template fixes; the rest comes from the page.
 */
export const COLLECTION_TEMPLATES = {
  // A live capture sent to the insectary: its death and preservation stay blank until it dies.
  Collected_Sent2Insectary: {
    Release_Collect: 'Collected_Sent2Insectary',
    FieldMark_ID: 'NA',
    CAM_ID: 'NA',
    ID_status: 'COMPLETE',
    Transect_section: 'NA',
    Bait: 'NA',
    Forest_stratum: 'NA',
    Flight_height: 'NA',
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
    return newRowFormulaFields(store, sheet);
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

  let lastWriteQuery;
  function lookupFor(sheet, keys) {
    const lists = listsOf(sheet);
    let own, stocks;
    const mine = () => (own ??= keyIndex(sheet, keys));
    const clutches = () => (stocks ??= keyIndex('Insectary_stocks', ['CLUTCH NUMBER']));
    let reared;
    const butterflies = () => (reared ??= keyIndex('Insectary_data', ['CLUTCH NUMBER']));
    const record = id => {
      const r = store.getRecord(id);
      return r && { id: r.id, row: r.row, version: r.version, label: r.label, values: r.values, formulas: r.formulas, observed: r.observed };
    };
    return {
      find: values => (mine().get(values.map(clutchKey).join('|')) ?? []).map(h => record(h.id)).filter(Boolean),
      clutch: value => clutches().get(clutchKey(value))?.[0]?.value ?? null,
      // Adults only: eggs and larvae preserved with their own row (LIFESTAGE) are not emergences.
      adultsOfClutch: value =>
        sheet === 'Insectary_stocks'
          ? (butterflies().get(clutchKey(value)) ?? []).filter(h => {
              const stage = store.getRecord(h.id)?.values?.LIFESTAGE;
              return isNone(stage) || /adult/i.test(String(stage));
            }).length
          : null,
      speciesOfClutch: value => {
        const hit = clutches().get(clutchKey(value))?.[0];
        return hit ? (store.getRecord(hit.id)?.values?.SPECIES ?? null) : null;
      },
      // The clutch's DATE LAID (a serial), null when it has none (NA, eggs found), undefined when not in the sheet.
      laidOfClutch: value => {
        const hit = clutches().get(clutchKey(value))?.[0];
        if (!hit) return undefined;
        const laid = store.getRecord(hit.id)?.values?.['DATE LAID'];
        return typeof laid === 'number' ? laid : null;
      },
      // IDs of the sheet one character away from one read (or two swapped), with their rows.
      nearIds: values => {
        if (keys.length !== 1) return [];
        const map = mine();
        return nearIds(clutchKey(values[0]), map.keys())
          .slice(0, 20)
          .flatMap(key => (map.get(key) ?? []).map(h => ({ value: h.value, row: store.getRecord(h.id)?.row ?? null })))
          .filter(h => h.row !== null);
      },
      list: field => lists[field],
      holder: (field, value, recordId, own = {}) => {
        if (!(isUnique(sheet, field) || TUBE_FIELD.test(field)) || !isIdValue(value)) return null;
        const unique = usedIds();
        const key = `${TUBE_FIELD.test(field) ? 'tube' : `${sheet}:${field}`}\u0000${String(value).trim()}`;
        // A wild-caught butterfly's Collection_data row holds its tube too: not another butterfly.
        const twin = h =>
          TUBE_FIELD.test(field) && sheet === 'Insectary_data' && h.sheet === 'Collection_data' && twinRows(own, store.getRecord(h.id)?.values);
        // Another row, or another column of this row (the clip's tube already filed as Tube_2_id).
        return (unique.get(key) ?? []).find(h => (h.id !== recordId || h.field !== field) && !twin(h)) ?? null;
      },
      newRowFormulas: newRowFormulas(sheet),
      typedOverFormula: new Set(sheet === 'Insectary_data' ? ['SPECIES'] : []),
      // Who wrote a cell last: an applied notebook proposal (notebook: true) or anything else, and the day (d/m/yy).
      lastWrite: (recordId, field) => {
        const row = (lastWriteQuery ??= db.prepare(
          "SELECT a.source, a.reason, a.created_at FROM changes c JOIN actions a ON a.id = c.action_id WHERE c.record_id = ? AND c.field = ? AND a.status IN ('verified', 'observed') ORDER BY a.created_at DESC LIMIT 1",
        )).get(recordId, field);
        if (!row) return null;
        const [y, m, d] = new Intl.DateTimeFormat('en-CA', { timeZone: 'America/Guayaquil' }).format(new Date(row.created_at)).split('-');
        return { notebook: row.source === 'ai_approved' && /^Cuaderno /.test(row.reason ?? ''), date: `${Number(d)}/${Number(m)}/${y.slice(2)}` };
      },
    };
  }

  /**
   * The same typing slip in the rows around the page: each sure correction the page makes of an ID
   * (FS5848994 → FS50848994) read on the same column of the rows near it that are not on the photo
   * (sameErrorRows). A row whose cell another pending proposal of this person already writes is
   * listed with that proposal, not proposed again.
   */
  function sameErrorNearby(review, kind, user, replacing) {
    const fixes = review.lines.flatMap(l =>
      !l.recordId || l.crossed || l.row === null
        ? []
        : Object.entries(l.cells)
            .filter(
              ([field, c]) =>
                c.status === 'conflict' && c.include && !c.doubt && isUnique(kind.sheet, field) && !kind.keys.includes(field) && isIdValue(c.before) && isIdValue(c.value),
            )
            .map(([field, c]) => ({ field, line: l.n, row: l.row, wrong: String(c.before), right: String(c.value) })),
    );
    if (!fixes.length) return [];
    const onPage = new Set(review.lines.map(l => l.recordId).filter(Boolean));
    const [lo, hi] = [Math.min(...fixes.map(f => f.row)) - 300, Math.max(...fixes.map(f => f.row)) + 300];
    const column = db.prepare(
      'SELECT id, row_num, label, json_extract(values_json, ?) v, json_extract(formulas_json, ?) f FROM records WHERE sheet = ? AND missing = 0 AND row_num BETWEEN ? AND ?',
    );
    const rows = [];
    for (const field of new Set(fixes.map(f => f.field))) {
      const path = `$."${field.replaceAll('"', '')}"`;
      for (const r of column.all(path, path, kind.sheet, lo, hi))
        if (!onPage.has(r.id) && !r.f && isIdValue(r.v)) rows.push({ recordId: r.id, row: r.row_num, label: r.label, field, value: r.v });
    }
    const unique = usedIds();
    const scope = field => (TUBE_FIELD.test(field) ? 'tube' : `${kind.sheet}:${field}`);
    const families = new Map();
    const familyCount = (field, family) => {
      if (!families.has(scope(field))) {
        const count = new Map();
        const start = `${scope(field)}\u0000`;
        for (const key of unique.keys()) {
          if (!key.startsWith(start)) continue;
          const m = /^([A-Z]*)(\d+)$/i.exec(key.slice(start.length));
          const family = m && `${m[1].toUpperCase()}:${m[2].length}`;
          if (family) count.set(family, (count.get(family) ?? 0) + 1);
        }
        families.set(scope(field), count);
      }
      return families.get(scope(field)).get(family) ?? 0;
    };
    const found = sameErrorRows({ fixes, rows, taken: (field, value) => unique.has(`${scope(field)}\u0000${value}`), familyCount });
    if (!found.length) return [];
    // Cells another pending proposal of this person already writes.
    const pending = new Map();
    try {
      for (const p of db
        .prepare("SELECT id, changes_json FROM ai_proposals WHERE owner_id = ? AND status = 'pending' AND id != ?")
        .all(String(user?.id ?? user?.username ?? ''), String(replacing ?? '')))
        for (const c of parse(p.changes_json, []))
          if (c.recordId && !c.context) for (const field of Object.keys(c.values ?? {})) pending.set(`${c.recordId}\u0000${field}`, p.id);
    } catch {
      // No proposals table (a sheet without the assistant): nothing pending.
    }
    return found.map(f => {
      const elsewhere = pending.get(`${f.recordId}\u0000${f.field}`);
      return elsewhere ? { ...f, alreadyIn: elsewhere } : f;
    });
  }

  /**
   * The page matched with its sheet: the review (every line and cell) and the
   * proposal's rows, each checked as the save will check it (a bad row is left
   * out and reported, the rest still go).
   */
  function match(args, user) {
    const { transcription, ignored } = checkTranscription({ kind: args.kind, year: args.year, lines: args.lines, spans: args.spans });
    const kind = KINDS[transcription.kind];
    const review = buildReview({
      transcription,
      today: ecuadorDay(),
      initials: initials(user),
      lookup: lookupFor(kind.sheet, kind.keys),
    });
    // Dates without their year, not from the last months and not in the sheet: the person says the year.
    if (review.yearNeeded)
      throw new Error(
        `Which year is this page? Its year is not written, and its dates (${review.yearNeeded.join(', ')}) are not in these rows of the sheet nor from the last ${RECENT_DAYS} days. Ask the person, then call again with \`year\`. Nothing was proposed.`,
      );
    const rows = proposalRows(review);
    const ids = newIds();
    const changes = [];
    // In the page's order: the person reads the table beside the notebook.
    const ordered = [
      ...rows.newRows.map(row => ['newRows', row]),
      ...rows.changes.map(row => ['changes', row]),
    ].sort((a, b) => a[1].line - b[1].line);
    for (const [key, row] of ordered) {
      const out = draftChanges({ [key]: [row] }, ids);
      const line = review.lines.find(l => l.n === row.line);
      if (out.error) {
        if (!/already in the sheet/.test(out.error)) line.rowError = clip(out.error.replace(/^newRows\[0\]: /, ''), 300);
        continue;
      }
      // The doubts (and where implied values come from) of the cells still in the row.
      const keep = map => {
        const kept = Object.fromEntries(Object.entries(map ?? {}).filter(([f]) => f in (out.changes[0]?.values ?? {})));
        return Object.keys(kept).length ? kept : undefined;
      };
      const inferred = (row.inferred ?? []).filter(f => f in (out.changes[0]?.values ?? {}));
      // What a formula column will give (not written): only for cells the row leaves to the formula.
      const gives = Object.entries(row.formulaGives ?? {}).filter(([f]) => !(f in (out.changes[0]?.values ?? {})));
      changes.push(
        ...out.changes.map(c => ({
          ...c,
          line: row.line,
          ...(keep(row.doubts) ? { doubts: keep(row.doubts) } : {}),
          ...(keep(row.hints) ? { hints: keep(row.hints) } : {}),
          ...(inferred.length ? { inferred } : {}),
          ...(gives.length ? { formulaGives: Object.fromEntries(gives) } : {}),
          // Cells the reader could not read: never in `values`, for the person to fill.
          ...(row.unreadable ? { unreadable: row.unreadable } : {}),
        })),
      );
    }
    // A line the save would refuse (rowError) shows as its row with the reason and nothing to write,
    // so the person sees it beside the page (never as a line already in the sheet).
    {
      const rowsShown = new Set(changes.map(c => c.recordId).filter(Boolean));
      for (const line of review.lines) {
        if (!line.rowError || !line.recordId || changes.some(c => c.line === line.n)) continue;
        const record = store.getRecord(line.recordId);
        if (!record || record.missing || rowsShown.has(record.id)) continue;
        rowsShown.add(record.id);
        changes.push({
          recordId: record.id,
          sheet: record.sheet,
          row: record.row,
          label: record.label,
          expectedVersion: record.version,
          before: {},
          values: {},
          replaceFormula: [],
          note: clip(`Línea ${line.n}: «${line.raw}» · No se puede escribir: ${line.rowError}`, 300),
          line: line.n,
          rowError: line.rowError,
        });
      }
    }
    // A matched line whose only news is cells nobody could read still shows: the person may fill them
    // (the row writes nothing until they do). A line not found in the sheet stays out, as before.
    {
      const rowsShown = new Set(changes.map(c => c.recordId).filter(Boolean));
      for (const line of review.lines) {
        if (!line.toFill || line.status !== 'match' || line.rowError || changes.some(c => c.line === line.n)) continue;
        const record = line.recordId && store.getRecord(line.recordId);
        if (!record || record.missing || rowsShown.has(record.id)) continue;
        rowsShown.add(record.id);
        changes.push({
          recordId: record.id,
          sheet: record.sheet,
          row: record.row,
          label: record.label,
          expectedVersion: record.version,
          before: {},
          values: {},
          replaceFormula: [],
          note: clip(`Línea ${line.n}: «${line.raw}»`, 300),
          line: line.n,
          unreadable: unreadableOf(line),
        });
      }
      changes.sort((a, b) => a.line - b.line);
    }
    // includeUnchanged: every line found in the sheet shows, the ones with nothing to write as
    // read-only context rows (never written), so the table follows the whole page.
    if (args.includeUnchanged) {
      const shown = new Set(changes.map(c => c.line));
      // A row appears once in a proposal (its key is the recordId).
      const rowsShown = new Set(changes.map(c => c.recordId).filter(Boolean));
      for (const line of review.lines) {
        if (shown.has(line.n) || !line.recordId || line.crossed || rowsShown.has(line.recordId)) continue;
        const record = store.getRecord(line.recordId);
        if (!record || record.missing) continue;
        rowsShown.add(record.id);
        const why = line.message || (line.changes ? 'nada seguro que escribir' : 'ya está en la hoja');
        changes.push({
          context: true,
          recordId: record.id,
          sheet: record.sheet,
          row: record.row,
          label: record.label,
          expectedVersion: record.version,
          before: {},
          values: {},
          replaceFormula: [],
          note: clip(`Línea ${line.n}: «${line.raw}» · ${why} (solo contexto, no se escribe)`, 300),
          line: line.n,
        });
      }
      changes.sort((a, b) => a.line - b.line);
    }
    // The page's slip in the rows around it (not on the photo): after the page's rows, as doubtful
    // cells the person checks, each saying which line of the page it follows.
    const sameError = sameErrorNearby(review, kind, user, args.replaceProposalId);
    {
      const rowsShown = new Set(changes.map(c => c.recordId).filter(Boolean));
      const byRow = new Map();
      for (const f of sameError) if (!f.alreadyIn && !rowsShown.has(f.recordId)) byRow.set(f.recordId, [...(byRow.get(f.recordId) ?? []), f]);
      for (const [recordId, cells] of [...byRow].slice(0, 60)) {
        const note = clip(
          `Mismo error que en la página, no está en esta foto: ${cells.map(f => `${f.field} ${f.value} → ${f.suggested} (como la línea ${f.line})`).join(' · ')}`,
          300,
        );
        const out = draftChanges({ changes: [{ recordId, values: Object.fromEntries(cells.map(f => [f.field, f.suggested])), note }] }, ids);
        if (out.error) continue;
        for (const f of cells) f.inProposal = true;
        changes.push(
          ...out.changes.map(c => ({
            ...c,
            sameErrorAs: cells[0].line,
            doubts: Object.fromEntries(
              cells.map(f => [f.field, { confidence: 0.6, alternatives: [f.value], reason: clip(f.reason.text, 200), reasonMsg: f.reason.msg }]),
            ),
          })),
        );
      }
    }
    // A wild-caught butterfly also needs its Collection_data row (same Insectary_ID).
    const wildWithoutCollection = [];
    if (kind.sheet === 'Insectary_data') {
      const collected = keyIndex('Collection_data', ['Insectary_ID']);
      for (const line of review.lines) {
        const wild = line.cells.Wild_Reared;
        if (line.status !== 'match' || (wild?.include ? wild.value : wild?.before) !== 'Wild-caught') continue;
        const id = line.cells.Insectary_ID?.before ?? line.label;
        if (collected.has(clutchKey(id))) continue;
        // Its Collection_data row as the team types a live capture (field.md R2.6), to complete.
        const now = field => {
          const cell = line.cells[field];
          return cell ? (cell.include ? cell.value : cell.before) : null;
        };
        const species = String(now('SPECIES') ?? '').trim().split(/\s+/);
        const day = now('Intro2Insectary_date');
        wildWithoutCollection.push({
          id,
          row: {
            sheet: 'Collection_data',
            values: Object.fromEntries(
              Object.entries({
                ...COLLECTION_TEMPLATES.Collected_Sent2Insectary,
                Insectary_ID: id,
                SPECIES: species.length >= 2 ? species.slice(0, 2).join(' ') : null,
                Subspecies_Form: species.length >= 3 ? species.slice(2).join(' ') : null,
                Sex: now('Sex'),
                Collection_date: typeof day === 'number' ? isoOf(day) : null,
              }).filter(([, v]) => v !== null && v !== ''),
            ),
          },
        });
      }
    }
    // The whole page, kept with the proposal: its table shows every line in the notebook's order
    // (lines with nothing to write, not found or crossed out too), each on its photo.
    const sent = typeof args.lines === 'string' ? parse(args.lines, []) : Array.isArray(args.lines) ? args.lines : [];
    const page = {
      kind: transcription.kind,
      sheet: kind.sheet,
      lines: review.lines.map((l, i) => ({
        n: l.n,
        raw: l.raw,
        id: clip(l.label ?? '', 80),
        photo: Number.isInteger(sent[i]?.photo) && sent[i].photo >= 0 ? sent[i].photo : 0,
        status: l.status,
        ...(l.recordId ? { recordId: l.recordId } : {}),
        ...(l.rowError ? { error: l.rowError } : {}),
        ...(l.message ? { message: clip(l.message, 300) } : {}),
        ...(l.near?.length ? { near: l.near.slice(0, 3).map(n => ({ value: n.value, row: n.row })) } : {}),
      })),
    };
    return { review, changes, ignored, wildWithoutCollection, page, sameError };
  }

  /** The species Insectary_data's SPECIES formula gives for a clutch (its Insectary_stocks row), or null. */
  function speciesOfClutch(value) {
    const hit = keyIndex('Insectary_stocks', ['CLUTCH NUMBER']).get(clutchKey(value))?.[0];
    return hit ? (store.getRecord(hit.id)?.values?.SPECIES ?? null) : null;
  }

  return { match, speciesOfClutch };
}

/** What the tool tells Claude about the matched page: per line only what matters (not the equal cells). */
export function matchSummary({ review, changes, ignored, wildWithoutCollection = [], sameError = [] }, proposalId) {
  const show = (field, value) =>
    typeOf(field) === 'date' && typeof value === 'number' ? isoOf(value) : value === undefined ? null : value;
  // A row the save refused shows with its reason (rowError) but writes nothing: not in the proposal.
  // (Rows near the page with its slip have no line: they are in sameErrorNearby.)
  const inProposal = new Set(changes.filter(c => !c.context && !c.rowError && !c.sameErrorAs).map(c => c.line));
  const context = new Set(changes.filter(c => c.context).map(c => c.line));
  // Lines whose unreadable cells are in the table (also a row with nothing else to write).
  const inTable = new Set(changes.filter(c => c.unreadable).map(c => c.line));
  const lines = review.lines.map(l => {
    const out = { n: l.n, raw: l.raw, status: l.status };
    if (l.row) Object.assign(out, { row: l.row, label: l.label });
    if (l.message) out.message = l.message;
    const group = {};
    for (const [field, cell] of Object.entries(l.cells)) {
      const put = (name, value) => ((group[name] ??= {})[field] = value);
      const notebook = show(field, cell.value);
      // Could not be read: in the table for the person to fill (toFill), or not needed there
      // (the sheet already has a value, or the column is a formula).
      if (cell.status === 'unread')
        put('unreadable', {
          ...(cell.reason ? { reason: cell.reason } : {}),
          ...(cell.partial?.length ? { partial: cell.partial } : {}),
          ...(cell.toFill
            ? { toFill: inTable.has(l.n) }
            : {
                toFill: false,
                ...(!isNone(cell.before)
                  ? { sheet: show(field, cell.before) }
                  : ['match', 'new'].includes(l.status)
                    ? { note: 'formula column: the sheet computes it' }
                    : {}),
              }),
        });
      else if (cell.status === 'error') put('problems', cell.message);
      else if (cell.status === 'formula')
        put('notWritten', cell.message ?? 'formula column');
      // In the proposal, highlighted for the person to check (with these alternatives to pick from).
      else if (cell.doubt && ['fill', 'conflict', 'new'].includes(cell.status))
        put('doubtful', {
          read: notebook,
          alternatives: cell.alternatives.map(a => show(field, a)),
          confidence: Math.round(cell.confidence * 100) / 100,
          ...(cell.reason ? { reason: cell.reason } : {}),
          sheet: show(field, cell.before),
          ...(cell.message && cell.message !== cell.reason ? { note: cell.message } : {}),
        });
      // Not written on the line: the page's room, a death's template, a word of the note.
      else if (cell.inferred && (cell.status === 'fill' || cell.status === 'new')) put('implied', notebook);
      else if (cell.status === 'conflict')
        put('differs', { sheet: show(field, cell.before), notebook, ...(cell.message ? { note: cell.message } : {}) });
      else if (cell.status === 'fill' || cell.status === 'new') put(cell.status === 'new' ? 'newRow' : 'fill', cell.write ?? notebook);
      else if (cell.status === 'same') out.same = (out.same ?? 0) + 1;
      // The sheet's value stays (it holds the page's terms and more, or the page only implied one).
      else if (cell.status === 'keep' && cell.message) put('kept', { sheet: show(field, cell.before), notebook, note: cell.message });
    }
    Object.assign(out, group);
    if (l.near?.length) out.didYouMean = l.near.map(n => ({ id: n.value, row: n.row }));
    if (l.rowError) out.rowError = l.rowError;
    if (l.warnings) out.warnings = l.warnings;
    out.inProposal = inProposal.has(l.n);
    if (context.has(l.n)) out.contextRow = true;
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
      ...(context.size ? { contextRows: context.size } : {}),
      cellsToFill: c.fills,
      differences: c.conflicts,
      doubtful: c.doubts,
      ...(c.unreadable ? { unreadableToFill: c.unreadable } : {}),
      problems: c.errors,
      newRows: c.created,
      same: c.same,
    },
    proposalId: proposalId ?? null,
    ...(ignored.length ? { ignoredColumns: ignored } : {}),
    ...(wildWithoutCollection.length
      ? {
          wildWithoutCollection: {
            ids: wildWithoutCollection.map(w => w.id),
            rows: wildWithoutCollection.map(w => w.row),
            todo: 'Wild-caught without a Collection_data row: add these rows to this proposal with update_proposal newRows, completed from the page (Collector, Identifier, Collection_location, Collection_time, Rainfall, Cloud_cover, Purpose; NA when the page does not say). The template cells are as the team types a live capture: keep them; leave the death and preservation columns empty.',
          },
        }
      : {}),
    ...(sameError.length
      ? {
          sameErrorNearby: sameError.map(f => ({
            row: f.row,
            id: f.label,
            field: f.field,
            value: f.value,
            suggested: f.suggested,
            reason: f.reason.text,
            inProposal: Boolean(f.inProposal),
            ...(f.alreadyIn ? { alreadyIn: f.alreadyIn } : {}),
          })),
        }
      : {}),
    lines,
  };
}
