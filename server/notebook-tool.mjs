// The match_notebook tool: a notebook page Claude transcribed (in T3 Code,
// following the digitalizar-cuaderno skill) is matched line by line
// with the sheet by the rules of server/notebook.mjs (look-alike IDs, ditto
// marks already expanded by Claude, CAM/tube runs, the page's year, counts as
// sums, the SPECIES formula) and turned into one proposal the person reviews
// beside the chat. Nothing is written until they apply it.

import { lstatSync, mkdirSync, readFileSync, realpathSync, statSync, writeFileSync } from 'node:fs';
import { basename, dirname, isAbsolute, join, resolve, sep } from 'node:path';
import { moduleMap } from './schema.mjs';
import { VIEW_UPDATE } from './proposal-view.mjs';
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
  sumTerms,
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
const ECUADOR_DAY = new Intl.DateTimeFormat('en-CA', { timeZone: 'America/Guayaquil' });
const ecuadorDay = () => ECUADOR_DAY.format(new Date());
const isoOf = serial => new Date(Date.UTC(1899, 11, 30) + serial * 864e5).toISOString().slice(0, 10);

/** The largest notebook-reader file match_notebook reads (a page of 150 lines is some 60 KB). */
export const LINES_FILE_BYTES = 512 * 1024;
/** What a notebook-reader file may give (anything else in it is ignored). */
const FILE_KEYS = ['kind', 'year', 'title', 'photo', 'rotate', 'lines', 'spans'];

/**
 * A notebook's columns for the description: listed, or "as <kind> without …" when they are an
 * earlier notebook's of the same sheet less a few (shorter: the description loads with every chat).
 */
function columnsText(id) {
  const own = columnsOf(KINDS[id]);
  const listed = own.join(', ');
  for (const other of KIND_IDS.slice(0, KIND_IDS.indexOf(id))) {
    const theirs = columnsOf(KINDS[other]);
    if (KINDS[other].sheet !== KINDS[id].sheet || !own.every(f => theirs.includes(f))) continue;
    const less = `as ${other} without ${theirs.filter(f => !own.includes(f)).join(', ')}`;
    if (less.length < listed.length) return less;
  }
  return listed;
}

/** The tool's description and schema, with each notebook's columns taken from KINDS. */
export const MATCH_NOTEBOOK_TOOL = {
  type: 'function',
  function: {
    name: 'match_notebook',
    description: [
      'Match a transcribed notebook page (or envelopes/labels) with the sheet and draft ONE proposal beside the chat. How to read a page and its answer: the digitalizar-cuaderno skill.',
      "- `lines` top to bottom, values as written (\"17/9\", \"12+15\"); a cell empty on the page: its column left out. Or `linesFile`: a notebook-reader's file.",
      `- \`year\` only when the page shows it. Without it: the sheet's year for those dates, or the current year for dates from the last ${RECENT_DAYS} days; otherwise the answer asks for it.`,
      'Answer: proposalId, year/yearSource, counts, `lookAt` and only the lines that need a look (status, cells by group, warnings); every row: get_proposal.',
      '',
      'Columns per kind:',
      ...KIND_IDS.map(id => `- ${id} (${KINDS[id].label}, ${KINDS[id].sheet}): ${columnsText(id)}`),
    ].join('\n'),
    parameters: {
      type: 'object',
      properties: {
        kind: { type: 'string', enum: KIND_IDS },
        year: { type: 'integer', description: 'Only if written on the page' },
        title: { type: 'string', description: 'e.g. "posturas 120–134"' },
        linesFile: {
          type: 'string',
          description: "A notebook-reader's file (work/…/<page>.json; get_proposal saveLines writes one); arguments given here go over it",
        },
        lines: {
          type: 'array',
          description: 'One per written line (up to 150)',
          items: {
            type: 'object',
            properties: {
              raw: { type: 'string', description: 'The line as written, short' },
              values: { type: 'object', description: 'Column → text as read; null: unreadable (why in reasons)' },
              confidence: { type: 'object', description: 'Column → 0..1, only unsure cells (< 0.8)' },
              alternatives: { type: 'object', description: 'Column → up to 3 other readings; for null, the part read ("1?/9")' },
              reasons: { type: 'object', description: 'Column → why doubtful or unreadable, a few words the person reads' },
              crossedOut: { type: 'boolean', description: 'Crossed out, or "no se usó el ID"' },
              photo: { type: 'integer', description: 'Which photo the line is on (0 = the first)' },
            },
            required: ['raw', 'values'],
          },
        },
        spans: {
          type: 'array',
          description: 'A value a brace or ditto gives to a run of lines, once: fills `field` on the lines from `from` to `to` (their keys, both included) that leave it out',
          items: {
            type: 'object',
            properties: { field: { type: 'string' }, value: { type: 'string' }, from: { type: 'string' }, to: { type: 'string' } },
            required: ['field', 'value', 'from', 'to'],
          },
        },
        photo: {
          description: "The page's photo, or a list: the attachment's file name (from \"[Attached image … saved at …]\", or another chat's), or {name, note: why it is here, a few words}",
          anyOf: [{ type: 'string' }, { type: 'array' }],
        },
        rotate: {
          description: 'Clockwise turn that makes it upright, as given to crops.py; a list for several photos',
          anyOf: [{ type: 'integer', enum: [0, 90, 180, 270] }, { type: 'array', items: { type: 'integer', enum: [0, 90, 180, 270] } }],
        },
        replaceProposalId: { type: 'string', description: "This page's pending proposal (any chat's), replaced by a corrected reading; edits kept where it reads the same" },
        includeUnchanged: { type: 'boolean', description: 'Lines already in the sheet as grey context rows (never written)' },
        view: VIEW_UPDATE,
      },
    },
  },
};

const exists = path => {
  try {
    lstatSync(path);
    return true;
  } catch {
    return false;
  }
};

/**
 * A file in the work/ folder of the person's own T3 workspace (<workspaces>/<username>, as
 * scripts/t3-provision.mjs makes it), given relative to the workspace or as its full path:
 * { path } (links followed, still in work/) or { why }. `create`: the file and its folders
 * may not be there yet.
 */
function workFile(given, { workspaces, username }, { create = false } = {}) {
  const name = String(username ?? '');
  if (!workspaces || !name || basename(name) !== name || name.startsWith('.')) return { why: 'no workspace of yours on this server', none: true };
  const root = join(workspaces, name);
  try {
    const work = exists(join(root, 'work')) ? realpathSync(join(root, 'work')) : join(realpathSync(root), 'work');
    // The part of the path that is there, links followed (so a link out of work/ is refused), then the rest.
    let base = resolve(isAbsolute(given) ? given : join(root, given));
    const rest = [];
    while (create && !exists(base) && dirname(base) !== base) {
      rest.unshift(basename(base));
      base = dirname(base);
    }
    const path = join(realpathSync(base), ...rest);
    if (!path.startsWith(work + sep)) return { why: "not in this workspace's work/ folder" };
    if (exists(path) && !statSync(path).isFile()) return { why: 'not a file' };
    return { path };
  } catch (e) {
    return { why: create ? 'cannot be written there' : e.code === 'ENOENT' ? 'not found' : 'cannot be read' };
  }
}

/**
 * match_notebook's arguments with a notebook-reader's file (`linesFile`) read in: a JSON file in
 * the work/ folder of the person's own T3 workspace (workFile). The file holds { kind, year,
 * title, photo, rotate, lines, spans } (or only the lines); the arguments given in the call go
 * over it. Returns { args } or { error }.
 */
export function withLinesFile(args, workspace) {
  if (args?.linesFile === undefined || args.linesFile === null || args.linesFile === '') return { args };
  const given = String(args.linesFile).trim();
  const fail = why => ({ error: `linesFile ${clip(given, 200)}: ${why}. Nothing was proposed.` });
  if (!/\.json$/i.test(given)) return fail('give the .json file the reader wrote in work/');
  const at = workFile(given, workspace);
  if (at.why) return fail(at.none ? `${at.why}; give \`lines\`` : at.why);
  let data;
  try {
    if (statSync(at.path).size > LINES_FILE_BYTES) return fail(`larger than ${LINES_FILE_BYTES / 1024} KB`);
    data = JSON.parse(readFileSync(at.path, 'utf8'));
  } catch (e) {
    if (e instanceof SyntaxError) return fail(`not valid JSON (${clip(e.message, 120)})`);
    return fail(e.code === 'ENOENT' ? 'not found' : 'cannot be read');
  }
  const file = Array.isArray(data) ? { lines: data } : data && typeof data === 'object' ? data : null;
  if (!Array.isArray(file?.lines)) return fail('no `lines` list in it');
  const own = Object.fromEntries(Object.entries(args).filter(([k, v]) => k !== 'linesFile' && v !== undefined && v !== null));
  const read = Object.fromEntries(FILE_KEYS.filter(k => file[k] !== undefined && file[k] !== null).map(k => [k, file[k]]));
  return { args: { ...read, ...own } };
}

/** A notebook proposal's reason: "Cuaderno Posturas (Insectary_stocks): posturas 838–999". */
const PAGE_REASON = /^Cuaderno (.+?) \([^)]*\)(?:: (.*))?$/s;
/** A row's note from match_notebook: "Línea 3: «999 lys (F1) 1/9 12» · …" (a long line is cut). */
const LINE_NOTE = /^Línea (\d+): «(.*?)(?:»(?= · |$)|$)/s;
/** The date and initials match_notebook puts before a page's note ("6/10/26 FC: "). */
const ADDED_NOTE = /^\d{1,2}\/\d{1,2}\/\d{2} \S+: /;
const NOTE_COLUMN = /^Notes|^NOTES$/;
const dayText = serial => {
  const day = new Date(Date.UTC(1899, 11, 30) + serial * 864e5);
  return `${day.getUTCDate()}/${day.getUTCMonth() + 1}/${day.getUTCFullYear()}`;
};

/**
 * A notebook page's proposal as a notebook-reader's file (what match_notebook's `linesFile`
 * reads), to match the page again with replaceProposalId. The lines in the page's order (as
 * kept with the proposal; one made before pages were kept: its rows in order, each line as
 * its note quotes it), each with its key and what the proposal writes now, as a reader gives
 * it: dates d/m/yyyy, sums as written, a note without the sheet's old note and the date and
 * initials the match put before it. Formulas and the columns the match implies are left out
 * (it implies them again); doubtful and unreadable cells keep their confidence, other readings
 * and the reader's reasons. getRecord(id): a sheet row, for the key of a row the proposal does
 * not change. Returns { file, left } (left: rows of the sheet that are no line) or { error }.
 */
export function proposalPage(proposal, getRecord) {
  const page = parse(proposal.page_json ?? 'null', null);
  const reason = PAGE_REASON.exec(String(proposal.reason ?? ''));
  const kindId = KINDS[page?.kind] ? page.kind : KIND_IDS.find(id => KINDS[id].label === reason?.[1]);
  if (!kindId) return { error: 'not a notebook page (match_notebook)' };
  const kind = KINDS[kindId];
  const columns = new Set(columnsOf(kind));
  const changes = (parse(proposal.changes_json, []) ?? []).filter(c => c.sheet === kind.sheet && !c.sameErrorAs);
  const lineOf = c => (Number.isInteger(c.line) ? c.line : Number(LINE_NOTE.exec(String(c.note ?? ''))?.[1]) || null);
  const text = (field, value) => (typeOf(field) === 'date' && typeof value === 'number' ? dayText(value) : String(value));
  const some = map => Object.keys(map).length > 0;
  const lineOut = (n, raw, change, id) => {
    const values = {};
    const record = change?.recordId ? getRecord(change.recordId) : null;
    for (const key of kind.keys) {
      const value = change?.values?.[key] ?? record?.values?.[key] ?? (kind.keys.length === 1 ? (change?.label ?? id) : null);
      if (!isNone(value)) values[key] = text(key, value);
    }
    const inferred = new Set(change?.inferred ?? []);
    for (const [field, value] of Object.entries(change?.values ?? {})) {
      if (kind.keys.includes(field) || !columns.has(field) || inferred.has(field)) continue;
      // An emptied cell (null) is no reading.
      if (value === null || typeof value === 'object') continue;
      if (typeof value === 'string' && value.startsWith('=')) {
        // A count kept as its sum (=12+15) is what the page says; another formula is the sheet's.
        if (typeOf(field) === 'number' && sumTerms(value)) values[field] = value.slice(1);
        continue;
      }
      if (NOTE_COLUMN.test(field) && typeof value === 'string') {
        const before = change.before?.[field];
        const added = !isNone(before) && value.startsWith(`${before} | `) ? value.slice(String(before).length + 3) : value;
        values[field] = added.replace(ADDED_NOTE, '');
        continue;
      }
      values[field] = text(field, value);
    }
    const confidence = {};
    const alternatives = {};
    const reasons = {};
    for (const [field, doubt] of Object.entries(change?.doubts ?? {})) {
      if (!(field in values)) continue;
      if (typeof doubt.confidence === 'number') confidence[field] = doubt.confidence;
      if (doubt.alternatives?.length) alternatives[field] = doubt.alternatives.map(a => text(field, a));
      // The reader's own words; a reason of the match's own (reasonMsg) it gives again.
      if (doubt.reason && !doubt.reasonMsg) reasons[field] = doubt.reason;
    }
    for (const [field, cell] of Object.entries(change?.unreadable ?? {})) {
      if (!columns.has(field) || field in values) continue;
      values[field] = null;
      if (cell.partial?.length) alternatives[field] = cell.partial;
      if (cell.reason) reasons[field] = cell.reason;
    }
    return {
      n,
      raw,
      values,
      ...(some(confidence) ? { confidence } : {}),
      ...(some(alternatives) ? { alternatives } : {}),
      ...(some(reasons) ? { reasons } : {}),
    };
  };
  const used = new Set();
  const lines = Array.isArray(page?.lines)
    ? page.lines.map(l => {
        const change = changes.find(c => lineOf(c) === l.n);
        if (change) used.add(change);
        return {
          ...lineOut(l.n, l.raw, change, l.id),
          ...(l.photo > 0 ? { photo: l.photo } : {}),
          ...(l.status === 'crossed' ? { crossedOut: true } : {}),
        };
      })
    : changes
        .filter(c => lineOf(c) !== null)
        .map(c => {
          used.add(c);
          return lineOut(lineOf(c), LINE_NOTE.exec(String(c.note ?? ''))?.[2] ?? String(c.label ?? ''), c, c.label);
        });
  if (!lines.length) return { error: 'no lines of a page in it' };
  const photos = Array.isArray(page?.photos) ? page.photos.filter(p => p?.file) : [];
  const file = {
    kind: kindId,
    ...(reason?.[2] ? { title: reason[2] } : {}),
    ...(photos.length ? { photo: photos.map(p => p.file), rotate: photos.map(p => p.rotate ?? 0) } : {}),
    lines,
  };
  return { file, left: changes.filter(c => !used.has(c) && !c.context).map(c => c.label) };
}

/** A notebook-reader's file written in the work/ folder of the person's workspace (workFile): { path } or { error }. */
export function writeLinesFile(given, file, workspace) {
  const fail = why => ({ error: `saveLines ${clip(given, 200)}: ${why}` });
  if (!/\.json$/i.test(given)) return fail('give a .json file in work/');
  const at = workFile(given, workspace, { create: true });
  if (at.why) return fail(at.why);
  try {
    mkdirSync(dirname(at.path), { recursive: true });
    writeFileSync(at.path, `${JSON.stringify(file, null, 1)}\n`);
  } catch {
    return fail('cannot be written there');
  }
  return { path: at.path };
}

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
  /** Per key index, its repeats by base ID (A0E → the rows of A0E.1, A0E.2): made once per index. */
  const repeatIndexes = new WeakMap();
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
      // The rows of an Insectary ID's repeats (A0E.1, A0E.2 for A0E).
      repeats: values => {
        if (keys.length !== 1 || keys[0] !== 'Insectary_ID') return [];
        const map = mine();
        let bases = repeatIndexes.get(map);
        if (!bases) {
          bases = new Map();
          for (const [key, hits] of map) {
            const m = /^(.+)\.[1-9]\d*$/.exec(key);
            if (m) bases.set(m[1], [...(bases.get(m[1]) ?? []), ...hits]);
          }
          repeatIndexes.set(map, bases);
        }
        return (bases.get(clutchKey(values[0])) ?? []).map(h => record(h.id)).filter(Boolean);
      },
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
    // The whole page, kept with the proposal: its table shows every line (in the sheet's order)
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

/** A line's raw text in the answer (Claude wrote it; the start is enough to tell it). */
const RAW_CHARS = 60;
/** Lines that need the same look (the same cells, the same warnings) are listed as one entry from this many. */
const SAME_LINES = 3;
/** The rows near the page with its slip listed (the rest counted). */
const SAME_ERROR_LISTED = 10;
/** The groups of a line's cells that need a look; fill, newRow and implied are in the proposal only. */
const ATTENTION = ['differs', 'doubtful', 'unreadable', 'problems', 'kept', 'notWritten'];
/** The lines not listed in match_notebook's answer, and where they are. */
const NOT_LISTED = 'Lines not listed only fill cells or are as in the sheet; get_proposal shows every row.';

/**
 * What the tool tells Claude about the matched page, kept short (a page's answer used to be 10–16k
 * characters; a chat of pages filled its context): counts, the year, and only the lines that need a
 * look (not found, crossed out, differences, doubts, unreadable cells, problems, warnings), those that
 * say the same as one entry. The cells a line only fills are in the proposal (get_proposal).
 */
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
    // A new row's message only says it is new (counted in newRows).
    if (l.message && l.status !== 'new') out.message = l.message;
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
      // A formula column the page writes: said only where the sheet's value differs (a sum to fix there).
      else if (cell.status === 'formula') {
        if (cell.mismatch) put('notWritten', cell.message ?? 'formula column');
      }
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
  // Only the lines that need a look: one not found as read or crossed out, a cell to look at, a warning.
  const needsLook = l =>
    !['match', 'new'].includes(l.status) || ATTENTION.some(g => g in l) || !!(l.message || l.didYouMean || l.rowError || l.warnings);
  const listedLines = lines.filter(needsLook).map(l => {
    const { fill, newRow, implied, same, ...rest } = l;
    return { ...rest, raw: rest.raw.length > RAW_CHARS ? `${rest.raw.slice(0, RAW_CHARS - 1)}…` : rest.raw };
  });
  // Lines saying the same (a ditto's species unlike the sheet on a whole run) as one entry.
  const sameKey = ({ n, raw, row, label, message, didYouMean, ...rest }) => (message || didYouMean ? null : JSON.stringify(rest));
  const byKey = Map.groupBy(listedLines, l => sameKey(l) ?? `\u0000${l.n}`);
  const listed = [];
  for (const group of byKey.values()) {
    if (group.length < SAME_LINES) {
      listed.push(...group);
      continue;
    }
    const { n, raw, row, label, ...rest } = group[0];
    listed.push({ lines: group.map(l => l.n), ids: group.map(l => l.label ?? l.raw), ...rest });
  }
  listed.sort((a, b) => (a.n ?? a.lines[0]) - (b.n ?? b.lines[0]));
  const asInSheet = lines.filter(l => l.status === 'match' && !needsLook(l) && !l.inProposal).length;
  const onlyFilled = lines.length - lines.filter(needsLook).length - asInSheet;
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
      ...(asInSheet ? { linesAsInSheet: asInSheet } : {}),
      ...(onlyFilled ? { linesOnlyFilled: onlyFilled } : {}),
    },
    proposalId: proposalId ?? null,
    ...(ignored.length ? { ignoredColumns: ignored } : {}),
    ...(wildWithoutCollection.length
      ? {
          wildWithoutCollection: {
            ids: wildWithoutCollection.map(w => w.id),
            // The template's cells once; each row its own.
            template: { sheet: 'Collection_data', values: COLLECTION_TEMPLATES.Collected_Sent2Insectary },
            rows: wildWithoutCollection.map(w =>
              Object.fromEntries(Object.entries(w.row.values).filter(([f, v]) => COLLECTION_TEMPLATES.Collected_Sent2Insectary[f] !== v)),
            ),
            todo: 'Wild-caught without a Collection_data row: add these rows (each `template` plus its cells) to this proposal with update_proposal newRows, completed from the page (Collector, Identifier, Collection_location, Collection_time, Rainfall, Cloud_cover, Purpose; NA when the page does not say). The template cells are as the team types a live capture: keep them; leave the death and preservation columns empty.',
          },
        }
      : {}),
    ...(sameError.length
      ? {
          ...(sameError.length > SAME_ERROR_LISTED ? { sameErrorMore: sameError.length - SAME_ERROR_LISTED } : {}),
          sameErrorNearby: sameError.slice(0, SAME_ERROR_LISTED).map(f => ({
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
    lines: listed,
    ...(listed.length < lines.length ? { rest: NOT_LISTED } : {}),
  };
}
