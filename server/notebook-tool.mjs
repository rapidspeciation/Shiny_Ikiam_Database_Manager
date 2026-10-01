// The match_notebook tool: a notebook page Claude transcribed (in T3 Code or
// the chat, following the digitalizar-cuaderno skill) is matched line by line
// with the sheet by the rules of server/notebook.mjs (look-alike IDs, ditto
// marks already expanded by Claude, CAM/tube runs, the page's year, counts as
// sums, the SPECIES formula) and turned into one proposal the person reviews
// beside the chat. Nothing is written until they apply it.

import { moduleMap } from './schema.mjs';
import { newRowFormulaFields } from './premade.mjs';
import { TUBE_FIELD, isIdValue, isUnique, twinRows } from './verifications.mjs';
import { listOptions } from './verify.mjs';
import { KINDS, KIND_IDS, buildReview, checkTranscription, clutchKey, columnsOf, nearIds, proposalRows, typeOf } from './notebook.mjs';

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
      'Give every line of the page, top to bottom, with the values as written (dates day/month as written, e.g. "17/9"; ditto marks already replaced by the value above; CAMs/tubes written short like "cam505" or "81" may stay short, they continue the one above; counts as written, e.g. "12+15"; a count corrected by crossing out: the first value, then each new one after "=", e.g. "31+4=1" or "12=9=4", kept as the team types it, =31+4-34).',
      'The server finds each line\'s row (also through look-alike IDs 0/O, 1/I, 5/S and the order of the rows), infers the year, completes list values, keeps the SPECIES formula unless what emerged differs, and checks lists, IDs and tubes already used.',
      'It returns per line: the row found, cells to fill, differences with the sheet, doubtful cells, implied cells, problems, didYouMean (sheet IDs one character away from an ID not found), and the proposalId.',
      'Doubtful cells GO INTO the proposal with your best reading as the value, highlighted for the person with their alternatives and reason: give a confidence below 0.8, up to 3 alternatives and a short reason. Only a cell you cannot read at all (null) stays out. Never leave a readable value out for being doubtful or implausible: flag it.',
      'The server also flags (as doubtful, never silently): a clutch unlike the run of lines next to it (848 among 843s, judged by the laid dates), a CAM with 7 digits or far from the run around it, a tube with 7 or 9 digits (the value becomes the reading that continues the run, the written one an alternative).',
      'apply_proposal refuses while doubtful cells are unchecked and lists them: ask the person about each; they check them in the table, or tell you, then call update_proposal rows[].checked (or the value they say) or apply_proposal with confirmDoubtful.',
      'The proposal\'s rows follow the page\'s line order. With includeUnchanged the lines already in the sheet show too, as context rows that are never written (never fake a change to make a line show).',
      'Notes are written as "d/m/yy INI: text" (today, the person\'s initials) after the note the cell already has, with " | ".',
      'Posturas: a generation written with the species, e.g. "lys (F1)", goes to Generation (F1, F2, Backcross), none written is NA; the dissections column goes to NUMBER OF PUPAE/LARVAE FOR DISECTIONS (a count, sums kept like the other counts); a dash in a date or count is NA; give INSECTARY OR LABORATORY as written ("ins", "lab", "ins/oda", "ins/este"): "ins/<person>" becomes Insectary plus the note "mariposas de <person>", and a line without it takes the page\'s room; a sheet sum that already holds the page\'s terms and more is kept.',
      'Emergidos: a line with a clutch is Wild_Reared Reared; give Wild_Reared "Wild-caught" for a wild butterfly (no clutch), and add its Collection_data row to the proposal (wildWithoutCollection lists the ones missing, with the row ready to complete).',
      'Emergidos and Muertes: give the notes column as written; its words that belong in columns leave the note and fill only empty cells: "ethanol"/"flash frozen" → the tube\'s medium (T1_/T2_Preservation_medium), "wc" → Tube_1_tissue wing clip, "pheromone" → Research_purpose Pheromones, "preserved" → Death_cause Killed_Preserved, "unk" → Death_cause Unknown, a CAM → CAM_ID, a tube → Tube_1_id, or Tube_2_id when there is one already. A death not preserved (a cause other than Killed_Preserved, no CAM or tube) gets the NA / NOT_COLLECTED block, a preserved one Preservation_date = Death_date, Preserved_Dead_Alive Alive (Killed_Preserved), Location_body Ikiam, WHOLE_ORGANISM, Flash frozen (since 2025) and the unused tubes NA: these show as implied, and never replace a value the row has.',
      `Columns per kind: ${KIND_IDS.map(id => `${id} (${KINDS[id].label}, ${KINDS[id].sheet}): ${columnsOf(KINDS[id]).join(', ')}${KINDS[id].aliases ? ` (also accepted: ${Object.entries(KINDS[id].aliases).map(([a, f]) => `${a} = ${f}`).join(', ')})` : ''}`).join('; ')}.`,
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
              reasons: { type: 'object', description: 'Column → why the cell is doubtful, a few words the person reads (e.g. "1 or 7: this hand")' },
              crossedOut: { type: 'boolean', description: 'The line is crossed out or marked "no se usó el ID"' },
            },
            required: ['raw', 'values'],
          },
        },
        replaceProposalId: {
          type: 'string',
          description: 'A pending proposal of this page to replace (after the person corrects a reading)',
        },
        includeUnchanged: {
          type: 'boolean',
          description:
            'Also show the lines already in the sheet (nothing to write) as grey context rows, so the table follows the whole page. Context rows are never written; never invent a change to make a line show.',
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
      changes.push(
        ...out.changes.map(c => ({
          ...c,
          line: row.line,
          ...(keep(row.doubts) ? { doubts: keep(row.doubts) } : {}),
          ...(keep(row.hints) ? { hints: keep(row.hints) } : {}),
          ...(inferred.length ? { inferred } : {}),
        })),
      );
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
    return { review, changes, ignored, wildWithoutCollection };
  }

  return { match };
}

/** What the tool tells Claude about the matched page: per line only what matters (not the equal cells). */
export function matchSummary({ review, changes, ignored, wildWithoutCollection = [] }, proposalId) {
  const show = (field, value) =>
    typeOf(field) === 'date' && typeof value === 'number' ? isoOf(value) : value === undefined ? null : value;
  const inProposal = new Set(changes.filter(c => !c.context).map(c => c.line));
  const context = new Set(changes.filter(c => c.context).map(c => c.line));
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
    if (group.unread) group.unread = Object.keys(group.unread);
    Object.assign(out, group);
    if (l.near?.length) out.didYouMean = l.near.map(n => ({ id: n.value, row: n.row }));
    if (l.rowError) out.rowError = l.rowError;
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
    lines,
  };
}
