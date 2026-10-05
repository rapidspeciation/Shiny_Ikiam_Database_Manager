// The sheets' copy for the assistant's `query` tool (server/query-tool.mjs): a
// separate SQLite file with only the workbook's data, so a question about many
// rows (counts, ranges, comparisons across sheets, a cell's history) is one SQL
// query instead of pages of find_records.
//
// What it holds:
// - `<sheet>_all`: a table per sheet with every row below its header, real columns
//   named as the sheet's, plus `_row` (the sheet's row), `_id` (the app's recordId,
//   as propose_changes takes it) and `_premade` (1: a row the app does not count as
//   in use, records.observed = 0: made ahead with its ID formula, or holding only
//   NA or a dropdown's default). `<sheet>`: a view of its rows in use (_premade = 0).
// - Values as the sheet shows them (formula cells computed; formula texts left
//   out). Date columns as YYYY-MM-DD text, time columns as H:MM; numeric columns
//   with NUMERIC affinity, so "CLUTCH NUMBER" = 997 matches and "NA" stays text.
//   ID columns are text compared without case. Columns whose names differ only
//   in case (COLLECTION_LOCATION, Collection_location) get _2.
// - `_tables` (name, rows, premade), `_columns` (sheet, column, header, type,
//   list, strict: the dropdown's values, JSON), `_meta` (built_at, state).
// - `history`: every cell saved (by the app or edited in Google Sheets and read by
//   a sync), people by their display names, without the formulas syncs logged
//   only because their row moved.
// - `staged`: the Emergidos and Clutches entries kept in the app, not in the sheet
//   yet (server/staged.mjs), a row per cell: who, when, the tab, a new row or an
//   edit, the sheet's row (none for a new row), the value and the sheet's before.
// Nothing else of the app's database (people's accounts, sessions, settings, the
// assistant's chats) is copied.
//
// Built in a child process (`node server/replica.mjs <database> <copy>`) from the
// database opened read-only, into a temporary file renamed over the copy; again
// after each sync that changed rows and 30 s after the last save, and only when
// something changed (sourceState).

import { execFile } from 'node:child_process';
import { createHash } from 'node:crypto';
import { chmodSync, existsSync, renameSync, rmSync, statSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { DatabaseSync } from 'node:sqlite';
import { modules } from './schema.mjs';
import { listOptions } from './verify.mjs';
import { PURPOSES } from './history.mjs';

/** Changes when the copy's layout does: a new version rebuilds it. */
const LAYOUT = 2;
export const COPY_FILE = 'sheets.sqlite';
/** The copy beside a database file (none for an in-memory database). */
export const copyBeside = databasePath => (!databasePath || databasePath === ':memory:' ? null : join(dirname(databasePath), COPY_FILE));

const quote = name => `"${String(name).replaceAll('"', '""')}"`;
const EPOCH = Date.UTC(1899, 11, 30);
const DAY = 864e5;
/** Sheets serial days read as dates: 1927–2119 (smaller numbers in a date column are counts or typos). */
const SERIAL = n => typeof n === 'number' && n >= 10000 && n < 80000;
const NONE = new Set(['NA', 'N/A', 'NONE', 'NULL', 'NOT_COLLECTED', 'NOT_FOUND', 'NOT_PROVIDED']);
const isNone = text => NONE.has(text.toUpperCase()) || text.startsWith('#');
const NUMERIC_TEXT = /^-?\d+(?:\.\d+)?$/;
const TIME_FIELD = /(^|_)time$/i;
/** Columns of IDs (Insectary_ID, CAM_ID_insectary, Father_CAMid, female Id, Tube_1_id, Rack_ID, Tube_1_rack): text, compared without case. */
const ID_COLUMN = /(?:^|[_\s/])(?:ID|Id|id)(?:$|[_\s])|CAMid|[Tt]ube|[Rr]ack|manifest/;
/** Columns indexed besides the sheet's ID columns (schema identityFields). */
const INDEXED = /(?:^|[_\s/])(?:ID|Id|id)$|CAMid$|^CLUTCH NUMBER$|^Clutch_No\.$/;
const MONTHS = { jan: 1, ene: 1, feb: 2, mar: 3, mrt: 3, apr: 4, abr: 4, may: 5, mei: 5, jun: 6, jul: 7, aug: 8, ago: 8, sep: 9, set: 9, oct: 10, okt: 10, nov: 11, dec: 12, dic: 12 };

const isoOfSerial = n => new Date(EPOCH + Math.floor(n + 1e-9) * DAY).toISOString().slice(0, 10);
const ymd = (y, m, d) => {
  const year = y < 100 ? 2000 + y : y;
  const date = new Date(Date.UTC(year, m - 1, d));
  return date.getUTCFullYear() === year && date.getUTCMonth() === m - 1 && date.getUTCDate() === d ? date.toISOString().slice(0, 10) : null;
};
/** A date typed as text (2026-09-30, 30/9/26 day first, 9-Aug-26, 12-mrt-26) as YYYY-MM-DD, else null. */
export function isoOfText(value) {
  const s = String(value).trim();
  let m = /^(\d{4})-(\d{1,2})-(\d{1,2})(?:[T ][\d:.]+Z?)?$/.exec(s);
  if (m) return ymd(+m[1], +m[2], +m[3]);
  m = /^(\d{1,2})[/.-](\d{1,2})[/.-](\d{2}|\d{4})$/.exec(s);
  if (m) return ymd(+m[3], +m[2], +m[1]);
  m = /^(\d{1,2})[-\s]?([A-Za-z]{3})[a-z]*\.?[-\s]?(\d{2}|\d{4})$/.exec(s);
  if (m && MONTHS[m[2].toLowerCase()]) return ymd(+m[3], MONTHS[m[2].toLowerCase()], +m[1]);
  return null;
}
const hhmm = fraction => {
  const minutes = Math.round(fraction * 1440);
  return `${Math.floor(minutes / 60) % 24}:${String(minutes % 60).padStart(2, '0')}`;
};

/**
 * How each column is kept: 'date', 'time', 'number', 'id' or 'text', from the
 * schema's guess (server/schema.mjs) checked against what the column holds: a
 * "date" column of day counts is a number, a "number" column of country names
 * text.
 */
function kindOf(field, values) {
  if (ID_COLUMN.test(field.key)) return 'id';
  let numbers = 0,
    jsNumbers = 0,
    serials = 0,
    fractions = 0,
    dates = 0,
    texts = 0;
  for (const v of values) {
    if (v === null || v === undefined || v === '' || typeof v === 'object' || typeof v === 'boolean') continue;
    if (typeof v === 'number') {
      numbers++;
      jsNumbers++;
      if (SERIAL(v)) serials++;
      if (v >= 0 && v < 1) fractions++;
      continue;
    }
    const text = String(v).trim();
    if (!text || isNone(text)) continue;
    if (NUMERIC_TEXT.test(text)) numbers++;
    else if (isoOfText(text)) dates++;
    else texts++;
  }
  if (TIME_FIELD.test(field.key) && jsNumbers && fractions >= 0.8 * jsNumbers) return 'time';
  if (field.type === 'date' && (jsNumbers ? serials >= 0.8 * jsNumbers : dates > texts)) return 'date';
  const all = numbers + dates + texts;
  if (!numbers) return 'text';
  if (field.type !== 'text' && numbers >= 0.8 * all) return 'number';
  if (jsNumbers >= 0.95 * all) return 'number';
  return 'text';
}

const DECLARED = { date: 'TEXT', time: 'TEXT', number: 'NUMERIC', id: 'TEXT COLLATE NOCASE', text: 'TEXT' };

/** A cell as the copy keeps it. */
function cell(kind, v) {
  if (v === null || v === undefined || v === '') return null;
  if (typeof v === 'boolean') return v ? 'TRUE' : 'FALSE';
  if (typeof v === 'object') return JSON.stringify(v);
  if (kind === 'date') {
    if (typeof v === 'number') return SERIAL(v) ? isoOfSerial(v) : String(v);
    return isoOfText(v) ?? v;
  }
  if (kind === 'time' && typeof v === 'number' && v >= 0 && v < 1) return hhmm(v);
  if (kind === 'number') return v;
  // Numbers bound to a text column would read "5.0".
  return typeof v === 'number' ? String(v) : v;
}

/**
 * The columns of a sheet in the copy: [{ key, name, kind }], the sheet's order,
 * a repeated header once, names that differ only in case from an earlier one
 * with _2 (_3…).
 */
function columnsOf(mod, rows) {
  const taken = new Set(['_id', '_row', '_premade']);
  const out = [];
  for (const field of mod.fields) {
    if (field.readonly || out.some(c => c.key === field.key)) continue;
    let name = field.key;
    for (let n = 2; taken.has(name.toLowerCase()); n++) name = `${field.key}_${n}`;
    taken.add(name.toLowerCase());
    out.push({ key: field.key, name, kind: kindOf(field, rows.map(r => r.values[field.key])) });
  }
  return out;
}

/**
 * What the copy is made from: changes with every row written, moved or removed
 * (as the per-sheet fingerprints, store.sheetRevision), every save, and the people's names.
 */
export function sourceState(db) {
  const sheets = db
    .prepare('SELECT sheet, count(*) n, max(updated_at) u, total(version) v, total(row_num) r FROM records WHERE missing=0 GROUP BY sheet ORDER BY sheet')
    .all()
    .map(r => `${r.sheet}:${r.n}-${r.u}-${r.v}-${r.r}`);
  const actions = db.prepare("SELECT count(*) n, max(created_at) u, total(status IN ('verified','observed')) d FROM actions").get();
  const changes = db.prepare('SELECT count(*) n, max(rowid) m FROM changes').get();
  const people = db.prepare("SELECT count(*) n, group_concat(id || ':' || display_name, ',') p FROM users").get();
  const staged = hasTable(db, 'staged') ? db.prepare('SELECT count(*) n, max(updated_at) u, total(length(values_json)) l FROM staged').get() : { n: 0 };
  const all = [`v${LAYOUT}`, ...sheets, `a${actions.n}-${actions.u}-${actions.d}`, `c${changes.n}-${changes.m}`, `p${people.n}-${people.p}`, `s${staged.n}-${staged.u}-${staged.l}`];
  return createHash('sha1').update(all.join('|')).digest('base64url');
}

/**
 * Writes the copy of `source` (the app's database, opened read-only or the
 * store's own connection) to `out`: a temporary file beside it, renamed over it
 * when complete. Returns { state, builtAt, rows, changes, ms, bytes }.
 */
export function buildCopy(source, out, { now = () => new Date() } = {}) {
  const started = Date.now();
  const temp = `${out}.${process.pid}.${started}.tmp`;
  rmSync(temp, { force: true });
  const copy = new DatabaseSync(temp);
  let state, rowCount = 0, changeCount = 0;
  try {
    copy.exec('PRAGMA journal_mode=OFF; PRAGMA synchronous=OFF; PRAGMA page_size=8192;');
    copy.exec('BEGIN');
    // One read of the source: its rows, saves and lists as of the same moment.
    const inSource = source.isTransaction;
    if (!inSource) source.exec('BEGIN');
    try {
      state = sourceState(source);
      copy.exec(`CREATE TABLE _meta(key TEXT PRIMARY KEY, value TEXT);
        CREATE TABLE _tables(name TEXT PRIMARY KEY, rows INTEGER, premade INTEGER);
        CREATE TABLE _columns(sheet TEXT, column TEXT, header TEXT, type TEXT, list TEXT, strict INTEGER);`);
      const kinds = new Map();
      const lists = { db: source };
      const select = source.prepare(
        'SELECT id, row_num, observed, values_json FROM records WHERE sheet=? AND missing=0 AND row_num>? AND row_num<2000000000 ORDER BY row_num',
      );
      for (const mod of modules) {
        const rows = select.all(mod.id, mod.headerRow).map(r => ({ id: r.id, row: r.row_num, premade: r.observed ? 0 : 1, values: JSON.parse(r.values_json) }));
        const columns = columnsOf(mod, rows);
        const all = `${mod.id}_all`;
        copy.exec(
          `CREATE TABLE ${quote(all)} (_row INTEGER, ${columns.map(c => `${quote(c.name)} ${DECLARED[c.kind]}`).join(', ')}, _id TEXT PRIMARY KEY, _premade INTEGER)`,
        );
        const insert = copy.prepare(`INSERT INTO ${quote(all)} VALUES (${Array(columns.length + 3).fill('?').join(',')})`);
        for (const r of rows) insert.run(r.row, ...columns.map(c => cell(c.kind, r.values[c.key])), r.id, r.premade);
        copy.exec(`CREATE VIEW ${quote(mod.id)} AS SELECT * FROM ${quote(all)} WHERE _premade = 0`);
        copy.exec(`CREATE INDEX ${quote(`${all}:_row`)} ON ${quote(all)}(_row)`);
        for (const c of columns.filter(c => mod.identityFields.includes(c.key) || INDEXED.test(c.key)))
          copy.exec(`CREATE INDEX ${quote(`${all}:${c.name}`)} ON ${quote(all)}(${quote(c.name)})`);
        const premade = rows.filter(r => r.premade).length;
        copy.prepare('INSERT INTO _tables VALUES (?,?,?)').run(mod.id, rows.length - premade, premade);
        const options = listOptions(lists, mod.id);
        const addColumn = copy.prepare('INSERT INTO _columns VALUES (?,?,?,?,?,?)');
        for (const c of columns) {
          const list = options[c.key];
          addColumn.run(mod.id, c.name, c.key, c.kind === 'id' ? 'text' : c.kind, list ? JSON.stringify([...list.values]) : null, list ? (list.strict ? 1 : 0) : null);
        }
        kinds.set(mod.id, new Map(columns.map(c => [c.key, c.kind])));
        rowCount += rows.length;
      }
      copy.exec('CREATE INDEX _columns_sheet ON _columns(sheet)');
      changeCount = copyHistory(source, copy, kinds);
      copyStaged(source, copy, kinds);
    } finally {
      if (!inSource) source.exec('COMMIT');
    }
    const builtAt = now().toISOString();
    const meta = copy.prepare('INSERT INTO _meta VALUES (?,?)');
    meta.run('built_at', builtAt);
    meta.run('state', state);
    meta.run('rows', String(rowCount));
    copy.exec('COMMIT');
    copy.exec('ANALYZE; PRAGMA journal_mode=DELETE;');
    copy.close();
    chmodSync(temp, 0o600);
    renameSync(temp, out);
    return { state, builtAt, rows: rowCount, changes: changeCount, ms: Date.now() - started, bytes: statSync(out).size };
  } catch (e) {
    if (copy.isOpen) copy.close();
    rmSync(temp, { force: true });
    throw e;
  }
}

const hasTable = (db, name) => !!db.prepare("SELECT 1 FROM sqlite_master WHERE type='table' AND name=?").get(name);

/** The copy's `staged` table: the entries kept in the app, a row per cell. */
function copyStaged(source, copy, kinds) {
  copy.exec(`CREATE TABLE staged(at TEXT, who TEXT, tab TEXT, kind TEXT, sheet TEXT, row INTEGER, id_label TEXT, record_id TEXT,
    field TEXT, value, before, status TEXT)`);
  if (!hasTable(source, 'staged')) return;
  const insert = copy.prepare('INSERT INTO staged VALUES (?,?,?,?,?,?,?,?,?,?,?,?)');
  const shown = (sheet, field, v) => {
    if (v && typeof v === 'object' && 'formula' in v) return v.formula;
    const kind = kinds.get(sheet)?.get(field) ?? 'text';
    return cell(kind === 'id' ? 'text' : kind, v);
  };
  for (const s of source
    .prepare(
      `SELECT s.*, u.display_name who, r.row_num FROM staged s LEFT JOIN users u ON u.id = s.actor LEFT JOIN records r ON r.id = s.record_id
       WHERE s.status IN ('staged', 'sent') ORDER BY s.rowid`,
    )
    .all()) {
    const values = JSON.parse(s.values_json || '{}');
    const base = JSON.parse(s.base_json || '{}');
    for (const [field, value] of Object.entries(values))
      insert.run(
        s.updated_at,
        s.who ?? s.actor,
        PURPOSES[s.purpose] ?? s.purpose,
        s.kind === 'create' ? 'new row' : 'edit',
        s.sheet,
        s.kind === 'create' ? null : (s.row_num ?? null),
        s.label,
        s.kind === 'create' ? `staged:${s.client_id}` : s.record_id,
        field,
        shown(s.sheet, field, value),
        s.kind === 'create' ? null : shown(s.sheet, field, base[field] ?? null),
        s.status === 'sent' ? 'being written' : 'staged',
      );
  }
}

/** The copy's `history` table; returns how many cells it holds. */
function copyHistory(source, copy, kinds) {
  copy.exec(`CREATE TABLE history(at TEXT, sheet TEXT, row INTEGER, id_label TEXT, record_id TEXT, field TEXT, before, after,
    who TEXT, source TEXT, reason TEXT, action_id TEXT)`);
  const insert = copy.prepare('INSERT INTO history VALUES (?,?,?,?,?,?,?,?,?,?,?,?)');
  // A formula cell's text stays out ("(formula)"); a cell whose formula alone changed is left out.
  const value = (sheet, field, json) => {
    if (json === null || json === undefined) return null;
    let v;
    try {
      v = JSON.parse(json);
    } catch {
      return json;
    }
    if (v && typeof v === 'object' && 'formula' in v) return '(formula)';
    const kind = kinds.get(sheet)?.get(field) ?? 'text';
    return cell(kind === 'id' ? 'text' : kind, v);
  };
  let n = 0;
  for (const c of source
    .prepare(
      `SELECT a.created_at, c.sheet, c.row_num, r.label, c.record_id, c.field, c.before_json, c.after_json,
              u.display_name who, a.actor, a.purpose, a.source, a.reason, a.id action_id
       FROM changes c JOIN actions a ON a.id = c.action_id LEFT JOIN records r ON r.id = c.record_id LEFT JOIN users u ON u.id = a.actor
       WHERE a.status IN ('verified', 'observed') AND c.moved = 0
       ORDER BY a.created_at, c.rowid`,
    )
    .iterate()) {
    const before = value(c.sheet, c.field, c.before_json);
    const after = value(c.sheet, c.field, c.after_json);
    if (before === '(formula)' && after === '(formula)') continue;
    const fromSheet = c.source === 'sheet_reconciliation';
    insert.run(
      c.created_at,
      c.sheet,
      c.row_num,
      c.label,
      c.record_id,
      c.field,
      before,
      after,
      c.who ?? null,
      PURPOSES[c.purpose] ?? c.purpose ?? c.source,
      fromSheet ? null : (c.reason ?? null),
      c.action_id,
    );
    n++;
  }
  copy.exec(`CREATE INDEX history_record ON history(record_id, at);
    CREATE INDEX history_sheet ON history(sheet, at);
    CREATE INDEX history_label ON history(id_label COLLATE NOCASE);`);
  return n;
}

/** The copy's _meta (built_at, state), or null when there is no copy yet. */
export function copyMeta(path) {
  if (!path || !existsSync(path)) return null;
  try {
    const db = new DatabaseSync(path, { readOnly: true });
    try {
      return Object.fromEntries(db.prepare('SELECT key, value FROM _meta').all().map(r => [r.key, r.value]));
    } finally {
      db.close();
    }
  } catch {
    return null;
  }
}

const here = fileURLToPath(import.meta.url);

/** Builds the copy in a child process (the app keeps answering meanwhile): { done, child }. */
function buildInChild(database, out) {
  let child;
  const done = new Promise((resolve, reject) => {
    child = execFile(process.execPath, [here, database, out], { maxBuffer: 1 << 20, timeout: 10 * 60_000 }, (error, stdout, stderr) => {
      if (error) return reject(new Error(String(stderr || error.message).trim().split('\n').at(-1)));
      try {
        resolve(JSON.parse(stdout.trim().split('\n').at(-1)));
      } catch (e) {
        reject(e);
      }
    });
  });
  return { done, child };
}

/**
 * Keeps the copy at `path` up to date with the store: a rebuild when a sync
 * that changed rows ends, `delayMs` after the last save, and at start (when
 * the copy is missing or older than the database). Skipped when nothing
 * changed. An in-memory database (tests) is copied in this process.
 * Returns { path, status(), rebuild(), close() }.
 */
export function createSheetsCopy({ store, path, delayMs = 30_000, startMs = 3_000, log = console }) {
  const database = store.db.location?.() ?? null;
  const meta = copyMeta(path);
  let built = meta ? { state: meta.state, builtAt: meta.built_at } : null;
  let timer = null;
  let building = null;
  let again = false;
  let closed = false;
  let lastError = null;
  let child = null;
  // Rows saved since the copy being built (or the last one) was read.
  let changed = false;

  // When the next rebuild is due: a later save does not put it off (the copy is at most delayMs behind).
  let due = Infinity;
  function schedule(ms) {
    if (closed || (timer && due <= Date.now() + ms)) return;
    clearTimeout(timer);
    due = Date.now() + ms;
    timer = setTimeout(() => {
      timer = null;
      due = Infinity;
      // During a sync: when it ends (watchSyncs).
      if (!store.syncPromise) rebuild().catch(() => {});
    }, ms);
    timer.unref?.();
  }

  async function rebuild() {
    if (closed) return built;
    if (building) {
      again = true;
      return building;
    }
    let state;
    try {
      state = sourceState(store.db);
    } catch {
      return built; // the database is closing
    }
    if (built?.state === state && existsSync(path)) {
      changed = false;
      return built;
    }
    changed = false;
    building = (async () => {
      try {
        let out;
        if (database) {
          const run = buildInChild(database, path);
          child = run.child;
          out = await run.done;
        } else out = buildCopy(store.db, path);
        built = { state: out.state, builtAt: out.builtAt };
        lastError = null;
        log.log?.(`Sheets copy: ${out.rows} rows, ${out.changes} history cells, ${(out.bytes / 1e6).toFixed(1)} MB in ${(out.ms / 1000).toFixed(1)} s`);
      } catch (e) {
        lastError = e.message;
        if (!closed) log.error?.('Sheets copy failed:', e.message);
      } finally {
        building = null;
        child = null;
      }
      if (again && !closed) {
        again = false;
        schedule(delayMs);
      }
      return built;
    })();
    return building;
  }

  // Rows saved by the app (or by the sheet hook): a rebuild within delayMs. Rows read by a sync: right after it.
  const unwatchRecords = store.watchRecords?.(() => {
    changed = true;
    if (!store.syncPromise) schedule(delayMs);
  });
  // After every sync (also one that read nothing new: a rebuild put off during it); unchanged sources are skipped.
  const unwatchSyncs = store.watchSyncs?.(() => schedule(0));
  // Emergidos and Clutches entries kept in the app (the `staged` table): within delayMs too.
  const unwatchLive = store.watchLive?.(kind => {
    if (kind !== 'staged') return;
    changed = true;
    schedule(delayMs);
  });
  schedule(startMs);

  return {
    path,
    /** { builtAt, pending (saves the copy does not hold yet), error } */
    status: () => ({ builtAt: built?.builtAt ?? null, pending: changed || Boolean(building), error: lastError }),
    rebuild,
    close() {
      closed = true;
      clearTimeout(timer);
      unwatchRecords?.();
      unwatchSyncs?.();
      unwatchLive?.();
      child?.kill();
    },
  };
}

// `node server/replica.mjs <database> <copy>`: builds the copy from the database opened read-only.
if (process.argv[1] === here) {
  const [database, out] = process.argv.slice(2);
  if (!database || !out) {
    console.error('Usage: node server/replica.mjs <database.sqlite> <copy.sqlite>');
    process.exit(2);
  }
  const source = new DatabaseSync(database, { readOnly: true, timeout: 30_000 });
  const result = buildCopy(source, out);
  source.close();
  console.log(JSON.stringify(result));
}
