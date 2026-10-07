import { DatabaseSync } from 'node:sqlite';
import { randomUUID } from 'node:crypto';
import { mkdirSync, chmodSync } from 'node:fs';
import { dirname } from 'node:path';
import { modules, moduleMap, labelFor, validateValues, comparable, nextInsectaryId, makeSourceUrl } from './schema.mjs';
import { GoogleSheets, LocalSheets, formulaRowShift, moveRowRefs, rowKey, rowValues } from './sheets.mjs';
import { headerLayout, sameLayout } from './columns.mjs';
import { sameCell } from './formula-write.mjs';
import { applyBatch } from './batch.mjs';
import { noteProtection } from './premade.mjs';
import { initMonitoring } from './monitoring.mjs';
import { initHistory } from './history.mjs';
import { initClutches } from './clutches.mjs';
import { initClutchPhotos } from './clutch-photos.mjs';
import { initCensus, watchCensusSaves } from './census.mjs';
import { SANDBOX_ID } from './workbook.mjs';
import { initClaims } from './claims.mjs';
import { initOutbox, Outbox } from './outbox.mjs';
import { initStaged, Staged } from './staged.mjs';
import { turns } from './event-loop.mjs';

const json = value => JSON.stringify(value);
const parse = value => (value ? JSON.parse(value) : null);
const now = () => new Date().toISOString();
/** Saves a record (Store.persistRecord): a new one, or the same id with all its fields. */
const UPSERT_RECORD = `INSERT INTO records(id,sheet,row_num,values_json,formulas_json,identity_json,label,version,updated_at,missing,observed) VALUES(?,?,?,?,?,?,?,?,?,?,?)
  ON CONFLICT(id) DO UPDATE SET row_num=excluded.row_num,values_json=excluded.values_json,formulas_json=excluded.formulas_json,identity_json=excluded.identity_json,label=excluded.label,version=excluded.version,updated_at=excluded.updated_at,missing=excluded.missing,observed=excluded.observed`;
/** Rows a sync writes to the local copy in one transaction at most (Store.reconcile). */
const WRITE_ROWS = 1000;
const error = (code, message, status = 400, details) => Object.assign(new Error(message), { code, status, details });

/**
 * A counter that grows with every row written to the local copy (records) or to its history (actions,
 * changes, and the names it shows, users), by whatever connection writes it (the app, a script): views
 * built from them (the proposals' tables) are kept until it moves (Store#copyVersion). Writes to other
 * tables (the assistant's proposals) leave it alone.
 */
function initCopyVersion(db) {
  db.exec(`CREATE TABLE IF NOT EXISTS copy_version(id INTEGER PRIMARY KEY CHECK (id = 1), n INTEGER NOT NULL);
    INSERT OR IGNORE INTO copy_version(id, n) VALUES (1, 0);`);
  for (const table of ['records', 'actions', 'changes', 'users'])
    for (const event of ['INSERT', 'UPDATE', 'DELETE'])
      db.exec(
        `CREATE TRIGGER IF NOT EXISTS copy_${table}_${event.toLowerCase()} AFTER ${event} ON ${table} BEGIN UPDATE copy_version SET n = n + 1 WHERE id = 1; END`,
      );
}

export class Store {
  constructor(config = {}, { sheets, seed, switching = false } = {}) {
    this.config = config;
    const dbPath = config.databasePath || ':memory:';
    if (dbPath !== ':memory:') mkdirSync(dirname(dbPath), { recursive: true });
    // The assistant's worker (server/assistant-worker.mjs) writes its proposals on its own connection:
    // a write of the app waits its turn (a few ms) instead of failing as busy.
    this.db = new DatabaseSync(dbPath, { timeout: 5000 });
    if (dbPath !== ':memory:') chmodSync(dbPath, 0o600);
    this.db.exec(`PRAGMA journal_mode=WAL; PRAGMA foreign_keys=ON;
      CREATE TABLE IF NOT EXISTS users(id TEXT PRIMARY KEY, username TEXT UNIQUE NOT NULL, display_name TEXT NOT NULL, role TEXT NOT NULL, salt TEXT NOT NULL, password_hash TEXT NOT NULL, active INTEGER NOT NULL DEFAULT 1, created_at TEXT NOT NULL);
      CREATE TABLE IF NOT EXISTS sessions(id_hash TEXT PRIMARY KEY, user_id TEXT NOT NULL, csrf_hash TEXT NOT NULL, expires_at TEXT NOT NULL, created_at TEXT NOT NULL, FOREIGN KEY(user_id) REFERENCES users(id));
      CREATE TABLE IF NOT EXISTS records(id TEXT PRIMARY KEY, sheet TEXT NOT NULL, row_num INTEGER NOT NULL, values_json TEXT NOT NULL, formulas_json TEXT NOT NULL, identity_json TEXT NOT NULL, label TEXT NOT NULL, version INTEGER NOT NULL, updated_at TEXT NOT NULL, missing INTEGER NOT NULL DEFAULT 0, observed INTEGER NOT NULL DEFAULT 1, UNIQUE(sheet,row_num));
      CREATE INDEX IF NOT EXISTS records_sheet ON records(sheet,missing,row_num);
      CREATE TABLE IF NOT EXISTS actions(id TEXT PRIMARY KEY, request_id TEXT UNIQUE, actor TEXT NOT NULL, source TEXT NOT NULL, created_at TEXT NOT NULL, status TEXT NOT NULL, reason TEXT, reverses TEXT, result_json TEXT);
      CREATE TABLE IF NOT EXISTS changes(id TEXT PRIMARY KEY, action_id TEXT NOT NULL, record_id TEXT NOT NULL, sheet TEXT NOT NULL, row_num INTEGER NOT NULL, field TEXT NOT NULL, before_json TEXT, after_json TEXT, FOREIGN KEY(action_id) REFERENCES actions(id));
      CREATE INDEX IF NOT EXISTS changes_field ON changes(record_id,field);
      CREATE TABLE IF NOT EXISTS events(id TEXT PRIMARY KEY, kind TEXT NOT NULL, record_id TEXT, values_json TEXT NOT NULL, actor TEXT NOT NULL, created_at TEXT NOT NULL, request_id TEXT UNIQUE, source TEXT NOT NULL DEFAULT 'app');
      CREATE TABLE IF NOT EXISTS tasks(id TEXT PRIMARY KEY, title TEXT NOT NULL, description TEXT, due_date TEXT, assignee TEXT, status TEXT NOT NULL, record_id TEXT, created_by TEXT NOT NULL, created_at TEXT NOT NULL, updated_at TEXT NOT NULL);
      CREATE TABLE IF NOT EXISTS attachments(id TEXT PRIMARY KEY, record_id TEXT, name TEXT NOT NULL, mime_type TEXT NOT NULL, data BLOB NOT NULL, created_by TEXT NOT NULL, created_at TEXT NOT NULL);
      CREATE TABLE IF NOT EXISTS import_previews(id TEXT PRIMARY KEY, module TEXT NOT NULL, rows_json TEXT NOT NULL, errors_json TEXT NOT NULL, actor TEXT NOT NULL, created_at TEXT NOT NULL, applied INTEGER NOT NULL DEFAULT 0);
      CREATE TABLE IF NOT EXISTS undo_plans(request_id TEXT PRIMARY KEY, actor TEXT NOT NULL, selection_json TEXT NOT NULL, plan_json TEXT NOT NULL, status TEXT NOT NULL, created_at TEXT NOT NULL);
      CREATE TABLE IF NOT EXISTS settings(key TEXT PRIMARY KEY, value TEXT NOT NULL);
      CREATE TABLE IF NOT EXISTS audit(id TEXT PRIMARY KEY, kind TEXT NOT NULL, detail_json TEXT NOT NULL, created_at TEXT NOT NULL);
      CREATE TABLE IF NOT EXISTS inserted_rows(action_id TEXT NOT NULL, record_id TEXT NOT NULL, sheet TEXT NOT NULL, PRIMARY KEY(action_id, record_id));
    `);
    if (
      !this.db
        .prepare('PRAGMA table_info(records)')
        .all()
        .some(c => c.name === 'observed')
    )
      this.db.exec('ALTER TABLE records ADD COLUMN observed INTEGER NOT NULL DEFAULT 1');
    this.db.exec('CREATE INDEX IF NOT EXISTS records_updated ON records(sheet,updated_at)');
    // Covers the per-sheet fingerprints (tableRevision: every table poll, summary and ETag) and the
    // row counts, so they read this small index instead of every row's JSON (27 ms → 1 ms on 10k rows).
    this.db.exec('CREATE INDEX IF NOT EXISTS records_state ON records(sheet,missing,observed,row_num,version,updated_at)');
    // Numeric labels stored as REAL text ("1014.0") before labelFor returned text.
    this.db.exec("UPDATE records SET label=substr(label,1,length(label)-2) WHERE label GLOB '[0-9]*.0' AND label NOT GLOB '*[^0-9.]*'");
    // The purpose of each save (Colecta, Muertes…), indexes and purposes of older saves (Historial).
    initHistory(this.db);
    initMonitoring(this.db);
    // Clutches checked on phones and tablets (Clutches tab, cards).
    initClutches(this.db);
    initClutchPhotos(this.db);
    // Censuses of a species in the insectary (Censo tab) and everyone's marks.
    initCensus(this.db);
    // Identifiers held by changes not in the sheet yet, saves waiting for Google, Emergidos and Clutches entries.
    initClaims(this.db);
    initOutbox(this.db);
    initStaged(this.db);
    // Prepared once (prepare costs about as much as a small read): see statement().
    this.statements = new Map();
    initCopyVersion(this.db);
    this.sheets =sheets || (config.localMode ? new LocalSheets(seed || {}, { health: config.health }) : new GoogleSheets(config));
    this.localMode = this.sheets instanceof LocalSheets;
    // What open pages follow (GET /api/pulse): the workbook's state, the outbox, the staged entries.
    this.boot = randomUUID().slice(0, 8);
    this.liveCount = 0;
    this.liveWaiters = new Set();
    this.outbox = new Outbox(this);
    this.staged = new Staged(this);
    watchCensusSaves(this);
    // The workbook answers again (or only slowly): the waiting saves are written.
    this.sheets.health?.onChange(state => {
      this.bumpLive('workbook');
      if (state !== 'busy') this.outbox.kick();
    });
    // scripts/switch-workbook.mjs opens the database while it still caches the other workbook.
    if (!switching) this.checkWorkbook();
    // A workbook switch reads another workbook: a row there is not the same butterfly as the old copy's.
    this.switching = switching;
    this.queue = Promise.resolve();
    this.syncPromise = null;
    this.writeEpoch = new Map();
    this.inflight = new Map();
    this.headerProblems = new Map();
    // Each sheet's column map from its live header, as the last sync read it.
    this.layouts = new Map();
    // Per sheet, what the last sync read (a digest of the raw rows) and the local copy it left.
    this.sheetDigests = new Map();
    this.syncStatus = {
      state: this.localMode ? 'offline_seed' : 'not_synced',
      lastSync: this.getSetting('lastSync'),
      source: this.localMode ? 'local' : 'google',
      spreadsheetId: this.sheets.spreadsheetId,
    };
  }
  /**
   * Runs `fn` (synchronous, inside the caller's transaction) without the triggers that count rows
   * inserted into and updated in records one by one (initCopyVersion): any trigger makes each row's
   * write several times slower (20,000 rows: 1.1 s instead of 0.2 s). The count moves once instead.
   * They are back before the transaction ends, so no other connection ever misses them.
   */
  rowsUncounted(fn) {
    for (const event of ['insert', 'update']) this.db.exec(`DROP TRIGGER IF EXISTS copy_records_${event}`);
    try {
      return fn();
    } finally {
      this.db.exec('UPDATE copy_version SET n = n + 1 WHERE id = 1');
      initCopyVersion(this.db);
    }
  }
  /** A statement prepared once for this database (reads asked thousands of times per proposal). */
  statement(sql) {
    let s = this.statements.get(sql);
    if (!s) this.statements.set(sql, (s = this.db.prepare(sql)));
    return s;
  }
  /**
   * The local copy's version: changes whenever a row of it or of its history is written, here or by
   * another process on the same database file (scripts): what a view kept from them is checked against.
   * The assistant's proposals, written by its worker, leave it as it is.
   */
  copyVersion() {
    return String(this.statement('SELECT n FROM copy_version WHERE id = 1').get()?.n ?? 0);
  }
  close() {
    this.closed = true;
    this.sheets.health?.stop();
    this.outbox.stop();
    for (const wake of this.liveWaiters) wake();
    this.db.close();
  }
  /** Something open pages show changed (the workbook's state, the outbox, the staged entries): they are told. */
  bumpLive(kind = null) {
    this.liveCount++;
    for (const wake of [...this.liveWaiters]) wake();
    for (const fn of this.liveWatchers ?? []) {
      try {
        fn(kind);
      } catch (e) {
        console.error('Live watcher:', e.message);
      }
    }
  }
  /** Calls `fn(kind)` ('workbook', 'outbox', 'staged') when that changes. Returns the call that stops it. */
  watchLive(fn) {
    this.liveWatchers ??= new Set();
    this.liveWatchers.add(fn);
    return () => this.liveWatchers.delete(fn);
  }
  liveRevision() {
    return `${this.boot}.${this.liveCount}`;
  }
  /** Resolves when the live revision is no longer `seen`, or after `ms`. */
  waitLive(seen, ms = 25_000) {
    if (seen !== this.liveRevision()) return Promise.resolve();
    return new Promise(resolve => {
      const wake = () => {
        clearTimeout(timer);
        this.liveWaiters.delete(wake);
        resolve();
      };
      const timer = setTimeout(wake, ms);
      this.liveWaiters.add(wake);
    });
  }
  /** What the app's banner and /health say about Google: the workbook's state, saves waiting, entries kept in the app. */
  googleState() {
    return {
      workbook: this.sheets.health?.snapshot() ?? { state: 'ok' },
      outbox: { waiting: this.outbox.waiting() },
      staged: this.staged.count(),
      ...(this.localMode && this.sheets.busyState?.() ? { simulated: this.sheets.busyState() } : {}),
    };
  }
  /** The workbook this database caches (settings.workbookId). */
  cachedWorkbook() {
    const stored = this.getSetting('workbookId');
    if (stored) return stored;
    // Databases from before the setting existed cached the test copy, the only workbook allowed then.
    if (this.localMode) return null;
    return this.db.prepare('SELECT 1 FROM records LIMIT 1').get() ? SANDBOX_ID : null;
  }
  /**
   * Refuses to open a database that caches another workbook: syncing it would
   * log every difference between the two workbooks as an edit in Google Sheets
   * and keep row ids of the other one. scripts/switch-workbook.mjs moves it.
   */
  checkWorkbook() {
    const id = this.sheets.spreadsheetId;
    const cached = this.cachedWorkbook();
    if (cached && cached !== id)
      throw error(
        'WORKBOOK_MISMATCH',
        `The database caches workbook ${cached}, not ${id}: run scripts/switch-workbook.mjs first`,
        500,
      );
    if (!this.getSetting('workbookId')) this.setSetting('workbookId', id);
  }
  getSetting(key) {
    return this.db.prepare('SELECT value FROM settings WHERE key=?').get(key)?.value || null;
  }
  setSetting(key, value) {
    this.db
      .prepare('INSERT INTO settings(key,value) VALUES(?,?) ON CONFLICT(key) DO UPDATE SET value=excluded.value')
      .run(key, String(value));
  }
  runExclusive(fn) {
    const result = this.queue.then(fn);
    this.queue = result.catch(() => {});
    return result;
  }
  bumpWrite(sheet) {
    this.writeEpoch.set(sheet, (this.writeEpoch.get(sheet) || 0) + 1);
  }
  /**
   * Marks sheets as being written. A sync that read a sheet before or during a
   * write must not overwrite the local copy with that older snapshot, so the
   * epoch changes both when a write starts and when it ends.
   */
  startWrite(sheets) {
    for (const sheet of sheets) {
      this.inflight.set(sheet, (this.inflight.get(sheet) || 0) + 1);
      this.bumpWrite(sheet);
    }
  }
  endWrite(sheets) {
    for (const sheet of sheets) {
      this.inflight.set(sheet, Math.max(0, (this.inflight.get(sheet) || 0) - 1));
      this.bumpWrite(sheet);
    }
  }
  /** Saves a record at its row, first moving aside any stale record mapped to that row. */
  placeRecord(record) {
    this.db.exec('BEGIN IMMEDIATE');
    try {
      const occupant = this.db
        .prepare('SELECT id FROM records WHERE sheet=? AND row_num=? AND id<>?')
        .get(record.sheet, record.row, record.id);
      if (occupant)
        this.db.prepare('UPDATE records SET row_num=? WHERE id=?').run(this.displacedRow(record.sheet), occupant.id);
      this.persistRecord(record);
      this.db.exec('COMMIT');
    } catch (e) {
      this.db.exec('ROLLBACK');
      throw e;
    }
    // A displaced record is matched to its real row again by the next sync.
  }
  /**
   * The rows of `sheet` from `from` on move by `delta` (1: a row was inserted above
   * them, −1: one was deleted), as they did in the Sheet. They count as updated, so
   * open tables pick up their new row numbers. Runs inside the caller's transaction;
   * the numbers pass through negative ones so no two rows ever share one.
   */
  shiftRows(sheet, from, delta) {
    const at = now();
    // Their new row numbers reach the open proposals that show them.
    if (this.recordWatchers?.size)
      this.touchRecords(
        this.db
          .prepare('SELECT id, sheet FROM records WHERE sheet=? AND row_num>=? AND row_num<2000000000')
          .all(sheet, from),
      );
    this.db
      .prepare('UPDATE records SET row_num=-(row_num+?)-1000000000000, updated_at=? WHERE sheet=? AND row_num>=? AND row_num<2000000000')
      .run(delta, at, sheet, from);
    this.db
      .prepare('UPDATE records SET row_num=-row_num-1000000000000 WHERE sheet=? AND row_num<=-1000000000000')
      .run(sheet);
    // Sheets rewrote the references to the rows that moved: the stored formulas follow.
    const refAt = delta > 0 ? from : from - 1;
    const update = this.db.prepare('UPDATE records SET formulas_json=?, updated_at=? WHERE id=?');
    for (const r of this.db.prepare("SELECT id, formulas_json FROM records WHERE sheet=? AND formulas_json<>'{}'").all(sheet)) {
      const formulas = parse(r.formulas_json);
      let changed = false;
      for (const [field, formula] of Object.entries(formulas)) {
        const moved = moveRowRefs(formula, refAt, delta, sheet);
        if (moved !== formula) [formulas[field], changed] = [moved, true];
      }
      if (changed) update.run(json(formulas), at, r.id);
    }
    // The sheet's digest no longer describes the local copy.
    this.sheetDigests.delete(sheet);
  }
  /** A free row number below every stored one, for a record whose row is taken. */
  displacedRow(sheet) {
    return Math.min(0, this.db.prepare('SELECT min(row_num) n FROM records WHERE sheet=?').get(sheet).n ?? 0) - 1;
  }
  listModules() {
    return modules.map(m => ({
      ...m,
      fields: m.fields.map(({ column, ...f }) => f),
      recordCount: this.db
        .prepare('SELECT count(*) n FROM records WHERE sheet=? AND missing=0 AND observed=1')
        .get(m.id).n,
    }));
  }
  hydrate(row) {
    if (!row) return null;
    const values = parse(row.values_json),
      formulas = parse(row.formulas_json);
    return {
      id: row.id,
      sheet: row.sheet,
      module: row.sheet,
      row: row.row_num,
      values,
      formulas,
      label: row.label,
      kind: moduleMap.get(row.sheet)?.group || 'record',
      updatedAt: row.updated_at,
      version: row.version,
      // The offline lab copy has no sheet of its own: no link to the team's rows.
      sourceUrl: this.localMode ? null : makeSourceUrl(row.sheet, row.row_num, this.sheets.spreadsheetId),
      missing: Boolean(row.missing),
      observed: Boolean(row.observed),
    };
  }
  getRecord(id) {
    return this.hydrate(this.statement('SELECT * FROM records WHERE id=?').get(id));
  }
  getRecordBySheetRow(sheet, row) {
    return this.hydrate(this.statement('SELECT * FROM records WHERE sheet=? AND row_num=?').get(sheet, row));
  }
  searchRecords({ module, sheet, q = '', limit = 50, offset = 0, filters = {}, observedOnly = true } = {}) {
    const selected = module || sheet;
    if (selected && !moduleMap.has(selected)) throw error('MODULE_NOT_FOUND', 'Unknown module', 404);
    limit = Math.min(Math.max(Number(limit) || 50, 1), 100000);
    offset = Math.max(Number(offset) || 0, 0);
    const clauses = ['missing=0'];
    const args = [];
    if (observedOnly !== false && observedOnly !== 'false') clauses.push('observed=1');
    if (selected) {
      clauses.push('sheet=?');
      args.push(selected);
    }
    if (q) {
      clauses.push('(label LIKE ? OR values_json LIKE ?)');
      args.push(`%${q}%`, `%${q}%`);
    }
    for (const [field, value] of Object.entries(filters || {})) {
      if (!selected || !moduleMap.get(selected).fields.some(f => f.key === field))
        throw error('INVALID_FILTER', 'Filter needs a known module and field');
      clauses.push('json_extract(values_json, ?) = ?');
      args.push(`$.${JSON.stringify(field)}`, value);
    }
    const where = clauses.join(' AND ');
    const total = this.db.prepare(`SELECT count(*) n FROM records WHERE ${where}`).get(...args).n;
    const rows = this.db
      .prepare(`SELECT * FROM records WHERE ${where} ORDER BY updated_at DESC,row_num DESC LIMIT ? OFFSET ?`)
      .all(...args, limit, offset);
    return { records: rows.map(r => this.hydrate(r)), total, offset, limit };
  }
  getStats() {
    const rows = this.db
      .prepare('SELECT sheet,count(*) count FROM records WHERE missing=0 AND observed=1 GROUP BY sheet')
      .all();
    return {
      totalRecords: rows.reduce((n, r) => n + r.count, 0),
      byModule: Object.fromEntries(rows.map(r => [r.sheet, r.count])),
      openTasks: this.db.prepare("SELECT count(*) n FROM tasks WHERE status NOT IN ('done','cancelled')").get().n,
      lastSync: this.syncStatus.lastSync,
    };
  }
  listEvents({ kind, recordId, limit = 200 } = {}) {
    const clauses = [];
    const args = [];
    if (kind) {
      clauses.push('kind=?');
      args.push(kind);
    }
    if (recordId) {
      clauses.push('record_id=?');
      args.push(recordId);
    }
    return this.db
      .prepare(
        `SELECT * FROM events ${clauses.length ? 'WHERE ' + clauses.join(' AND ') : ''} ORDER BY created_at DESC LIMIT ?`,
      )
      .all(...args, Math.min(Number(limit) || 200, 1000))
      .map(r => ({
        id: r.id,
        kind: r.kind,
        recordId: r.record_id,
        values: parse(r.values_json),
        actor: r.actor,
        createdAt: r.created_at,
        source: r.source,
      }));
  }
  listTasks() {
    return this.db
      .prepare("SELECT * FROM tasks ORDER BY CASE status WHEN 'done' THEN 1 ELSE 0 END,due_date,created_at DESC")
      .all()
      .map(r => this.task(r));
  }
  task(r) {
    return {
      id: r.id,
      title: r.title,
      description: r.description,
      dueDate: r.due_date,
      assignee: r.assignee,
      status: r.status,
      recordId: r.record_id,
      createdBy: r.created_by,
      createdAt: r.created_at,
      updatedAt: r.updated_at,
    };
  }
  /**
   * History with filters. `q` matches the note, a person, or any identifier,
   * field or value in the changes; `from`/`to` are ISO dates (inclusive).
   */
  getHistory({ recordId, q, source, actor, sheet, status, purpose, from, to, limit = 50, offset = 0 } = {}) {
    const clauses = [];
    const args = [];
    if (purpose) {
      clauses.push('a.purpose=?');
      args.push(purpose);
    }
    if (recordId) {
      // Formulas a sync logged only because the row moved (changes.moved) are no change of the row.
      clauses.push('EXISTS(SELECT 1 FROM changes c WHERE c.action_id=a.id AND c.record_id=? AND c.moved=0)');
      args.push(recordId);
    }
    if (q) {
      const like = `%${String(q).replace(/[\\%_]/g, m => '\\' + m)}%`;
      clauses.push(`(a.reason LIKE ? ESCAPE '\\' OR EXISTS(SELECT 1 FROM changes c LEFT JOIN records r ON r.id=c.record_id
        WHERE c.action_id=a.id AND c.moved=0 AND (r.label LIKE ? ESCAPE '\\' OR c.field LIKE ? ESCAPE '\\' OR c.before_json LIKE ? ESCAPE '\\' OR c.after_json LIKE ? ESCAPE '\\')))`);
      args.push(like, like, like, like, like);
    }
    if (actor) {
      clauses.push('(a.actor=? OR a.actor IN (SELECT id FROM users WHERE username LIKE ? OR display_name LIKE ?))');
      args.push(actor, `%${actor}%`, `%${actor}%`);
    }
    if (source) {
      clauses.push('a.source=?');
      args.push(source);
    }
    if (status) {
      clauses.push('a.status=?');
      args.push(status);
    }
    if (sheet) {
      clauses.push('EXISTS(SELECT 1 FROM changes c WHERE c.action_id=a.id AND c.sheet=? AND c.moved=0)');
      args.push(sheet);
    }
    if (from) {
      clauses.push('a.created_at>=?');
      args.push(`${from}T00:00:00`);
    }
    if (to) {
      clauses.push('a.created_at<=?');
      args.push(`${to}T23:59:59.999Z`);
    }
    const where = clauses.length ? 'WHERE ' + clauses.join(' AND ') : '';
    const total = this.db.prepare(`SELECT count(*) n FROM actions a ${where}`).get(...args).n;
    limit = Math.min(Number(limit) || 50, 500);
    offset = Math.max(Number(offset) || 0, 0);
    const actions = this.db
      .prepare(`SELECT a.* FROM actions a ${where} ORDER BY a.created_at DESC LIMIT ? OFFSET ?`)
      .all(...args, limit, offset)
      .map(r => this.action(r));
    return { actions, total, limit, offset };
  }
  action(r) {
    const changes = this.db
      .prepare(
        'SELECT c.*, r.label FROM changes c LEFT JOIN records r ON r.id=c.record_id WHERE c.action_id=? ORDER BY c.rowid',
      )
      .all(r.id)
      .map(c => ({
        id: c.id,
        recordId: c.record_id,
        label: c.label ?? null,
        sheet: c.sheet,
        row: c.row_num,
        field: c.field,
        before: parse(c.before_json),
        after: parse(c.after_json),
      }));
    const editor = this.db.prepare('SELECT display_name FROM users WHERE id=?').get(r.actor);
    return {
      id: r.id,
      requestId: r.request_id,
      actor: r.actor,
      actorName: editor?.display_name || null,
      source: r.source,
      purpose: r.purpose ?? null,
      createdAt: r.created_at,
      status: r.status,
      reason: r.reason,
      reverses: r.reverses,
      reversedBy:
        this.db
          .prepare(
            "SELECT id FROM actions WHERE status='verified' AND source='undo' AND (',' || reverses || ',') LIKE ?",
          )
          .get(`%,${r.id},%`)?.id || null,
      changes,
    };
  }
  actionByRequest(requestId) {
    const row = this.db.prepare('SELECT * FROM actions WHERE request_id=?').get(requestId);
    return row ? { action: this.action(row), status: row.status, ...(parse(row.result_json) || {}) } : null;
  }
  /**
   * Calls `fn` with the rows saved to the local copy (a sync, the sheet hook, a
   * save of the app), as [{ id, sheet }], once per turn of the event loop.
   * Returns the call that stops it.
   */
  watchRecords(fn) {
    this.recordWatchers ??= new Set();
    this.recordWatchers.add(fn);
    return () => this.recordWatchers.delete(fn);
  }
  /** Tells the record watchers (watchRecords) about these rows, [{ id, sheet }], at the end of the turn. */
  touchRecords(records) {
    if (!this.recordWatchers?.size) return;
    if (!this.touched) {
      this.touched = new Map();
      setImmediate(() => {
        const rows = [...this.touched.values()];
        this.touched = null;
        for (const fn of this.recordWatchers) {
          try {
            fn(rows);
          } catch (e) {
            console.error('Record watcher:', e.message);
          }
        }
      });
    }
    for (const record of records) this.touched.set(record.id, { id: record.id, sheet: record.sheet });
  }
  /** `args`: recordArgs(record), when the caller made them already. */
  persistRecord(record, args = this.recordArgs(record)) {
    this.touchRecords([record]);
    this.statement(UPSERT_RECORD).run(...args);
  }
  /** The values persistRecord writes for `record` (UPSERT_RECORD's parameters). */
  recordArgs(record) {
    return [
      record.id,
      record.sheet,
      record.row,
      json(record.values),
      json(record.formulas),
      json(this.identity(record.sheet, record.values)),
      record.label,
      record.version,
      record.updatedAt,
      record.missing ? 1 : 0,
      this.hasObservation(record.sheet, record.values, record.formulas) ? 1 : 0,
    ];
  }
  identity(sheet, values) {
    const mod = moduleMap.get(sheet);
    return Object.fromEntries(
      mod.identityFields.filter(k => values[k] != null && values[k] !== '').map(k => [k, values[k]]),
    );
  }
  fingerprint(sheet, values) {
    return json(this.identity(sheet, values));
  }
  /**
   * A row read while some of its columns are missing from the sheet keeps the
   * last known values of those fields: a renamed column neither blanks the data
   * nor makes used rows look free. Fields stay in the profile's order.
   */
  keepUnavailable(sheet, item, previous, layout) {
    if (!layout.missing.length) return item;
    const values = {},
      formulas = {};
    for (const { key } of moduleMap.get(sheet).fields) {
      if (Object.hasOwn(values, key)) continue;
      const source = layout.columns.has(key) ? item : previous;
      if (source?.values && Object.hasOwn(source.values, key)) values[key] = source.values[key];
      if (source?.formulas?.[key]) formulas[key] = source.formulas[key];
    }
    return { ...item, values, formulas };
  }
  /** The column map for a sheet from a header row just read; records the problems admins see. */
  readLayout(sheet, header) {
    const layout = headerLayout(sheet, header);
    this.layouts.set(sheet, layout);
    this.headerProblems.set(sheet, layout.problems);
    return layout;
  }
  /**
   * Reads sheets and updates the local copy. `history: false` (the workbook
   * switch only) updates the rows without logging the differences as edits
   * made in Google Sheets; the result then counts them in `cells`.
   */
  async sync({ sheets = modules.map(m => m.id), force = false, history = true } = {}) {
    if (this.syncPromise) return this.syncPromise;
    const run = this.performSync({ sheets, force, history });
    this.syncPromise = run;
    let status;
    try {
      status = await run;
    } finally {
      this.syncPromise = null;
    }
    // Saves whose outcome is unknown (a write cut off, Google not answering) are checked again
    // after every sync that read the sheets, not only at startup.
    if (status?.state !== 'error' && this.unconfirmedCount() && !this.recovering) {
      this.recovering = this.runExclusive(() => this.recoverPending())
        .catch(e => console.error('Recovery:', e.message))
        .finally(() => (this.recovering = null));
    }
    for (const fn of this.syncWatchers ?? []) {
      try {
        fn(status);
      } catch (e) {
        console.error('Sync watcher:', e.message);
      }
    }
    return status;
  }
  /** Calls `fn` with the status of each sync that ends (store.sync). Returns the call that stops it. */
  watchSyncs(fn) {
    this.syncWatchers ??= new Set();
    this.syncWatchers.add(fn);
    return () => this.syncWatchers.delete(fn);
  }
  async performSync({ sheets, force, history = true }) {
    const full = sheets.length === modules.length;
    const revision = full && this.sheets.revision ? await this.sheets.revision() : null;
    if (full && !force && revision && revision === this.getSetting('sourceRevision') && this.syncStatus.lastSync) {
      this.syncStatus = { ...this.syncStatus, state: 'ok', checkedAt: now(), unchanged: true };
      return this.syncStatus;
    }
    this.syncStatus = { ...this.syncStatus, state: 'syncing', unchanged: false };
    const started = Date.now(),
      requestsBefore = this.sheets.requestCount ?? 0;
    let added = 0,
      changed = 0,
      moved = 0,
      missing = 0,
      skipped = 0,
      cells = 0,
      sheetsUnchanged = 0;
    const bySheet = {};
    try {
      for (const sheet of sheets) {
        const before = { added, changed, moved, missing, cells };
        const mod = moduleMap.get(sheet);
        if (!mod) throw error('MODULE_NOT_FOUND', 'Unknown module', 404);
        if (this.inflight.get(sheet)) {
          skipped++;
          continue;
        }
        const epoch = this.writeEpoch.get(sheet) || 0;
        const rows = await this.sheets.readSheet(sheet);
        // The columns the app's account cannot write (read with the sheet: no request more), for the proposal tables.
        try {
          const ranges = await this.sheets.protectedRangesOf?.(sheet);
          if (ranges) noteProtection(this.db, sheet, ranges);
        } catch {
          /* Kept as last noted. */
        }
        const layout = this.readLayout(sheet, rows.find(r => r.row === mod.headerRow));
        if (layout.blocked) {
          // Which column holds which field is unclear: reading would scramble values.
          skipped++;
          continue;
        }
        await this.runExclusive(async () => {
          if (epoch !== (this.writeEpoch.get(sheet) || 0)) {
            skipped++;
            return;
          }
          // The sheet reads exactly as when it was last reconciled and the local copy has not
          // changed since: reconciling again would change nothing, so its rows are not compared.
          // A forced sync ("Sincronizar ahora") always compares.
          const known = this.sheetDigests.get(sheet);
          if (!force && rows.digest && known?.digest === rows.digest && known.revision === this.sheetRevision(sheet)) {
            sheetsUnchanged++;
            return;
          }
          const counts = await this.reconcile(sheet, rows, layout, { history });
          added += counts.added;
          changed += counts.changed;
          moved += counts.moved;
          missing += counts.missing;
          cells += counts.cells;
          if (rows.digest) this.sheetDigests.set(sheet, { digest: rows.digest, revision: this.sheetRevision(sheet) });
        });
        const counts = { added, changed, moved, missing, cells };
        bySheet[sheet] = Object.fromEntries(Object.entries(counts).map(([k, v]) => [k, v - before[k]]));
      }
      this.syncStatus = {
        ...this.syncStatus,
        state: skipped ? 'stale' : this.localMode ? 'offline_seed' : 'ok',
        lastSync: skipped ? this.syncStatus.lastSync : now(),
        checkedAt: now(),
        added,
        changed,
        moved,
        missing,
        skipped,
        cells,
        sheetsUnchanged,
        bySheet,
        headerProblems: Object.fromEntries([...this.headerProblems].filter(([, problems]) => problems.length)),
        ms: Date.now() - started,
        requests: (this.sheets.requestCount ?? 0) - requestsBefore,
      };
      if (!this.localMode)
        console.log(
          `Sync: ${sheets.length} sheets read in ${(this.syncStatus.ms / 1000).toFixed(1)} s (${this.syncStatus.requests} requests), ` +
            `${sheetsUnchanged} unchanged; ${added} added, ${changed} changed, ${moved} moved, ${missing} missing, ${skipped} skipped`,
        );
      if (!skipped) {
        this.setSetting('lastSync', this.syncStatus.lastSync);
        if (full && revision) this.setSetting('sourceRevision', revision);
      }
      return this.syncStatus;
    } catch (e) {
      this.syncStatus = { ...this.syncStatus, state: 'error', error: e.message };
      throw e;
    }
  }
  /**
   * Brings the local copy of `sheet` to the rows of a whole-sheet read (`layout`: its header's
   * column map). Returns the counts { added, changed, moved, missing, cells }. Runs in the
   * write queue (runExclusive), so no save changes the sheet's rows meanwhile. A big sheet
   * takes seconds to compare: the reading and comparing give the other requests a turn every
   * few ms, and the rows that differ are written at the end, in transactions of up to WRITE_ROWS
   * rows (nothing else may write while one is open, so they never wait for a turn).
   */
  async reconcile(sheet, rows, layout, { history = true } = {}) {
    const mod = moduleMap.get(sheet);
    const turn = turns();
    let added = 0,
      changed = 0,
      moved = 0,
      missing = 0,
      cells = 0;
    // Only the columns present are compared: a missing column keeps its last known values.
    const partial = layout.missing.length > 0;
    const present = object => Object.fromEntries(Object.entries(object).filter(([k]) => layout.columns.has(k)));

    // The rows read, below the header and not empty; per row (same index), its identifiers as
    // stored (fingerprint) and, when every column is there, its values and formulas as stored.
    const current = [],
      keys = [],
      valuesJson = [],
      formulasJson = [];
    for (const r of rows) {
      if (turn.due()) await turn();
      if (r.row <= mod.headerRow) continue;
      const read = { row: r.row, ...rowValues(sheet, r, layout) };
      if (!Object.values(read.values).some(v => v !== null && v !== '') && !Object.keys(read.formulas).length) continue;
      current.push(read);
      keys.push(this.fingerprint(sheet, read.values));
      if (!partial) {
        valuesJson.push(json(read.values));
        formulasJson.push(json(read.formulas));
      }
    }
    const view = i =>
      partial ? json(present(current[i].values)) + json(present(current[i].formulas)) : valuesJson[i] + formulasJson[i];

    // The local copy's rows of the sheet, a page at a time.
    const old = [];
    const page = this.statement('SELECT * FROM records WHERE sheet=? AND missing=0 AND row_num>? ORDER BY row_num LIMIT 2000');
    for (let after = -Infinity; ; ) {
      const more = page.all(sheet, after);
      old.push(...more);
      if (more.length < 2000) break;
      after = more.at(-1).row_num;
      if (turn.due()) await turn();
    }
    const byRow = new Map(old.map(r => [r.row_num, r]));
    // Each stored row's identifiers (fingerprint of its values), and the rows by them.
    const oldKeys = new Map();
    const byIdentity = new Map();
    // Rows without identifier columns are matched by identical content
    // first, so inserting a row in the Sheet does not relabel every row below it.
    const byContent = new Map();
    const add = (map, key, r) => {
      const list = map.get(key);
      if (list) list.push(r);
      else map.set(key, [r]);
    };
    for (const r of old) {
      if (turn.due()) await turn();
      const key = this.fingerprint(sheet, parse(r.values_json));
      oldKeys.set(r.id, key);
      if (key !== '{}') add(byIdentity, key, r);
      if (Object.keys(parse(r.identity_json) || {}).length) continue;
      // values_json and formulas_json are what json() wrote: the same text as json() of them parsed.
      const content = partial
        ? json(present(parse(r.values_json))) + json(present(parse(r.formulas_json)))
        : r.values_json + r.formulas_json;
      add(byContent, content, r);
    }
    // Match in two passes so a row inserted above does not take the identity of
    // the row that used to be there: first exact matches (identifiers, or identical
    // content for sheets without identifiers), then row numbers for what is left.
    const seen = new Set();
    const matched = new Map();
    const claim = (item, record) => {
      matched.set(item, record);
      seen.add(record.id);
    };
    // The one stored row of `list` not matched yet; null when none or several.
    const onlyFree = list => {
      let free = null;
      for (const o of list ?? []) {
        if (seen.has(o.id)) continue;
        if (free) return null;
        free = o;
      }
      return free;
    };
    for (let i = 0; i < current.length; i++) {
      if (turn.due()) await turn();
      const item = current[i];
      const identity = keys[i];
      if (identity === '{}') {
        const same = onlyFree(byContent.get(view(i)));
        if (same) claim(item, same);
        continue;
      }
      const atRow = byRow.get(item.row);
      if (atRow && !seen.has(atRow.id) && oldKeys.get(atRow.id) === identity) {
        claim(item, atRow);
        continue;
      }
      const candidate = onlyFree(byIdentity.get(identity));
      if (candidate) claim(item, candidate);
    }
    for (const item of current) {
      if (turn.due()) await turn();
      if (matched.has(item)) continue;
      const atRow = byRow.get(item.row);
      // A row keeps its record when edited in place, unless its identifier changed: one that only
      // gained identifiers (a CAM_ID typed on a row known by its Insectary_ID) is the same row.
      const had = atRow ? parse(atRow.identity_json) || {} : {};
      const ids = this.identity(sheet, item.values);
      // (Not in a workbook switch: the other workbook's row is another butterfly.)
      const grew = !this.switching && Object.keys(had).length && Object.entries(had).every(([k, v]) => comparable(ids[k]) === comparable(v));
      if (atRow && !seen.has(atRow.id) && (!Object.keys(had).length || grew)) claim(item, atRow);
    }

    // What to write: the rows that differ from the stored ones, compared here and written below.
    const writes = [];
    for (let i = 0; i < current.length; i++) {
      if (turn.due()) await turn();
      const read = current[i];
      const found = matched.get(read) || null;
      // Most rows are as stored: nothing to compare or write (storedAs, with the texts made above).
      if (
        found &&
        found.row_num === read.row &&
        !partial &&
        found.values_json === valuesJson[i] &&
        found.formulas_json === formulasJson[i] &&
        found.label === labelFor(sheet, read.values) &&
        found.identity_json === keys[i] &&
        found.observed === (this.hasObservation(sheet, read.values, read.formulas) ? 1 : 0)
      )
        continue;
      const previous = found && this.hydrate(found);
      const item = this.keepUnavailable(sheet, read, previous, layout);
      const record = {
        id: found?.id || randomUUID(),
        sheet,
        row: item.row,
        values: item.values,
        formulas: item.formulas,
        label: labelFor(sheet, item.values),
        version: previous?.version || 1,
        // A moved row counts as updated so open pages pick up its new row number.
        updatedAt: found && found.row_num !== item.row ? now() : previous?.updatedAt || now(),
        missing: false,
      };
      if (found && found.row_num !== item.row) moved++;
      const diffs = [];
      if (previous) {
        // Rows inserted or deleted above move this row's formulas with it: not an edit for the Historial.
        const shift = found.row_num !== item.row ? item.row - found.row_num : 0;
        let shifted = 0;
        for (const field of mod.fields) {
          if (!layout.columns.has(field.key)) continue;
          const before = previous.formulas[field.key] ? { formula: previous.formulas[field.key] } : previous.values[field.key];
          const after = item.formulas[field.key] ? { formula: item.formulas[field.key] } : item.values[field.key];
          if (comparable(before) === comparable(after)) continue;
          if (shift && before?.formula && formulaRowShift(before.formula, after?.formula) === shift) shifted++;
          else diffs.push({ field: field.key, before, after });
        }
        if (diffs.length || shifted) {
          record.version++;
          record.updatedAt = now();
        }
        if (diffs.length) {
          changed++;
          cells += diffs.length;
        }
      } else added++;
      writes.push({ found, record, args: this.recordArgs(record), diffs: history ? diffs : [] });
    }
    const gone = old.filter(prior => !seen.has(prior.id));
    if (!writes.length && !gone.length) return { added, changed, moved, missing, cells };

    // Rows that move are written in the order that frees their new row first: from the bottom up when
    // rows went down (a row inserted above them), else from the top. A row still taken (by a row that
    // moves later) is parked below every number used, so no two rows ever share one.
    const down = writes.reduce((n, w) => n + (w.found ? Math.sign(w.record.row - w.found.row_num) : 0), 0) > 0;
    const order = down ? writes.toReversed() : writes;
    const park = this.statement('UPDATE records SET row_num=? WHERE id=?');
    const occupantAt = this.statement('SELECT id FROM records WHERE sheet=? AND row_num=? AND id<>?');
    const markMissing = this.statement('UPDATE records SET missing=1,row_num=?,updated_at=? WHERE id=?');
    // Parked row numbers go below every number already used, so repeated syncs never collide.
    let displaced = Math.min(0, this.statement('SELECT min(row_num) n FROM records WHERE sheet=?').get(sheet).n ?? 0) - 1;
    // Up to WRITE_ROWS rows per transaction, each row with its history: more (every row of a big sheet
    // moved by a row inserted at its top) are written in several, with a turn for the others between them.
    const index = new Map(writes.map((w, i) => [w, i]));
    for (let from = 0; from === 0 || from < order.length; from += WRITE_ROWS) {
      if (from) await turn();
      const part = order.slice(from, from + WRITE_ROWS);
      this.db.exec('BEGIN IMMEDIATE');
      try {
        this.rowsUncounted(() => {
          // The history in the sheet's order.
          for (const { record, diffs } of part.toSorted((a, b) => index.get(a) - index.get(b)))
            if (diffs.length) this.recordExternalChanges(record, diffs);
          if (!from)
            for (const prior of gone) {
              markMissing.run(displaced--, now(), prior.id);
              missing++;
            }
          for (const { record, args } of part) {
            const occupant = occupantAt.get(sheet, record.row, record.id);
            if (occupant) park.run(displaced--, occupant.id);
            this.persistRecord(record, args);
          }
        });
        this.db.exec('COMMIT');
      } catch (e) {
        this.db.exec('ROLLBACK');
        throw e;
      }
    }
    return { added, changed, moved, missing, cells };
  }
  /** Fingerprint of a sheet's local copy (grid.mjs tableRevision): changes with every write to its rows. */
  sheetRevision(sheet) {
    const r = this.db
      .prepare(
        'SELECT count(*) n, max(updated_at) u, total(version) v, total(row_num) r FROM records WHERE sheet=? AND missing=0',
      )
      .get(sheet);
    return `${r.n}-${r.u}-${r.v}-${r.r}`;
  }
  /** `via`: how the app saw them, 'sync' (a sheet read) or 'hook' (the sheet's edit trigger). */
  recordExternalChanges(record, diffs, via = 'sync') {
    const id = randomUUID(),
      created = now();
    this.statement(
      'INSERT INTO actions(id,request_id,actor,source,created_at,status,reason,reverses,result_json,purpose) VALUES(?,?,?,?,?,?,?,?,?,?)',
    ).run(
      id,
      null,
      'unknown',
      'sheet_reconciliation',
      created,
      'observed',
      'Snapshot comparison; intermediate edits and editor unknown',
      null,
      json({ via }),
      'sheets',
    );
    const change = this.statement(
      'INSERT INTO changes(id,action_id,record_id,sheet,row_num,field,before_json,after_json) VALUES(?,?,?,?,?,?,?,?)',
    );
    for (const d of diffs) change.run(randomUUID(), id, record.id, record.sheet, record.row, d.field, json(d.before ?? null), json(d.after ?? null));
  }
  /**
   * Re-reads a few rows after someone edited them directly in Google Sheets
   * (reported by the Apps Script trigger), instead of reading the whole sheet.
   * Each row updates the record already at that row number. Returns
   * { needsSync: true } when only a sheet sync can settle it: a write is in
   * progress, or a row matches no record and is not a new row at the end.
   */
  async refreshRows(sheet, rowNumbers) {
    const mod = moduleMap.get(sheet);
    if (!mod) throw error('MODULE_NOT_FOUND', 'Unknown module', 404);
    const rows = [...new Set(rowNumbers.map(Number))]
      .filter(r => Number.isInteger(r) && r > mod.headerRow)
      .sort((a, b) => a - b);
    if (!rows.length) return { changed: 0, added: 0, removed: 0 };
    if (this.inflight.get(sheet)) return { needsSync: true };
    const epoch = this.writeEpoch.get(sheet) || 0;
    // The header comes in the same request: the rows are read with the columns as they are now.
    const live = await this.sheets.readRows([{ sheet, rows: [mod.headerRow, ...rows] }]);
    const layout = headerLayout(sheet, live.get(rowKey(sheet, mod.headerRow)));
    // Columns moved or renamed since the last sync: only a whole-sheet read can settle it.
    if (layout.blocked || !sameLayout(layout, this.layouts.get(sheet))) return { needsSync: true };
    return this.runExclusive(() => {
      if (epoch !== (this.writeEpoch.get(sheet) || 0)) return { needsSync: true };
      let changed = 0,
        added = 0,
        removed = 0,
        needsSync = false;
      const last = this.db.prepare('SELECT max(row_num) n FROM records WHERE sheet=? AND missing=0').get(sheet).n ?? 0;
      this.db.exec('BEGIN IMMEDIATE');
      try {
        for (const row of rows) {
          const read = rowValues(sheet, live.get(rowKey(sheet, row)), layout);
          const previous = this.hydrate(
            this.db.prepare('SELECT * FROM records WHERE sheet=? AND row_num=? AND missing=0').get(sheet, row),
          );
          const item = this.keepUnavailable(sheet, read, previous, layout);
          const empty =
            !Object.values(item.values).some(v => v !== null && v !== '') && !Object.keys(item.formulas).length;
          if (!previous) {
            if (empty) continue;
            // A row typed below the data is new; anything else may be a moved row.
            if (row <= last) {
              needsSync = true;
              continue;
            }
            this.persistRecord({
              id: randomUUID(),
              sheet,
              row,
              ...item,
              label: labelFor(sheet, item.values),
              version: 1,
              updatedAt: now(),
              missing: false,
            });
            added++;
            continue;
          }
          if (empty) {
            this.db
              .prepare('UPDATE records SET missing=1,row_num=?,updated_at=? WHERE id=?')
              .run(this.displacedRow(sheet), now(), previous.id);
            removed++;
            continue;
          }
          const diffs = [];
          for (const field of mod.fields) {
            if (!layout.columns.has(field.key)) continue;
            const before = previous.formulas[field.key]
              ? { formula: previous.formulas[field.key] }
              : previous.values[field.key];
            const after = item.formulas[field.key] ? { formula: item.formulas[field.key] } : item.values[field.key];
            if (comparable(before) !== comparable(after)) diffs.push({ field: field.key, before, after });
          }
          if (!diffs.length) continue;
          const record = {
            ...previous,
            ...item,
            label: labelFor(sheet, item.values),
            version: previous.version + 1,
            updatedAt: now(),
            missing: false,
          };
          this.recordExternalChanges(record, diffs, 'hook');
          this.persistRecord(record);
          changed++;
        }
        this.db.exec('COMMIT');
      } catch (e) {
        this.db.exec('ROLLBACK');
        throw e;
      }
      return { changed, added, removed, needsSync };
    });
  }
  validateRole(user) {
    if (!user || !['editor', 'reviewer', 'admin'].includes(user.role))
      throw error('FORBIDDEN', 'Editing requires an editor role', 403);
  }
  requireRequestId(requestId) {
    if (typeof requestId !== 'string' || requestId.length < 8 || requestId.length > 160)
      throw error('REQUEST_ID_REQUIRED', 'A unique requestId is required');
  }
  /** Creates one row. Multi-row saves call applyBatch directly. */
  async createRecord({ module, values, requestId, reason }, user, source = 'app') {
    return single(applyBatch(this, { requestId, reason, creates: [{ module, values }] }, user, { source }));
  }
  hasObservation(module, values, formulas = {}) {
    const evidence =
      {
        Collection_data: [
          'SPECIES',
          'Collection_date',
          'Collection_location',
          'Sex',
          'Collector',
          'Death_date',
          'Insectary_ID',
          'FieldMark_ID',
        ],
        Insectary_data: [
          'SPECIES',
          'Intro2Insectary_date',
          'Collection_location',
          'Sex',
          'Death_date',
          'Stock_of_origin',
          'CLUTCH NUMBER',
        ],
        Pheromones_data: ['CAM_ID'],
        Melinaea_crosses: ['Female', 'Male'],
        Melinaea_eggs: ['Mother ID', 'Father ID', 'Tube_ID'],
        Crosses_Lys_x_Pol: ['female Id', 'male Id'],
        Stocks_Matings: ['male_ID', 'female_ID'],
        Insectary_stocks: ['CLUTCH NUMBER', 'SPECIES'],
        Location_data: ['Collection_location'],
      }[module] || Object.keys(values);
    return evidence.some(k => {
      const v = values[k],
        text = String(v ?? '')
          .trim()
          .toUpperCase();
      return (
        !formulas[k] &&
        v !== null &&
        v !== undefined &&
        !['', 'NA', 'N/A', 'NONE', 'NULL', 'BLANK', 'NOT_COLLECTED'].includes(text) &&
        !text.startsWith('#')
      );
    });
  }
  suggestInsectaryId() {
    const recent = this.db
      .prepare(
        "SELECT json_extract(values_json,'$.Insectary_ID') id FROM records WHERE sheet='Insectary_data' AND missing=0 AND observed=1 ORDER BY row_num DESC LIMIT 1000",
      )
      .all();
    const observed = recent.find(r => /^[A-ZÑ]\d[A-ZÑ]$/.test(r.id || ''));
    let next = nextInsectaryId(observed?.id);
    const exists = this.db.prepare(
      "SELECT 1 FROM records WHERE sheet='Insectary_data' AND missing=0 AND observed=1 AND json_extract(values_json,'$.Insectary_ID')=? LIMIT 1",
    );
    while (exists.get(next)) next = nextInsectaryId(next);
    return next;
  }
  /** Updates one row. Multi-row saves call applyBatch directly. */
  async updateRecord(id, { values, expectedVersion, expected, requestId, reason }, user, source = 'app') {
    return single(
      applyBatch(this, { requestId, reason, edits: [{ id, values, expectedVersion, expected }] }, user, { source }),
      () => this.getRecord(id),
    );
  }
  finishAction(id, status, result) {
    // Changes a partial save left out stay with the action, so a retry (or a later recovery) reports them.
    const skipped =
      result?.skipped ?? parse(this.db.prepare('SELECT result_json FROM actions WHERE id=?').get(id)?.result_json)?.skipped;
    const extra = skipped?.length ? { skipped } : {};
    this.db
      .prepare('UPDATE actions SET status=?,result_json=? WHERE id=?')
      .run(
        status,
        result
          ? json({ record: result.record, records: result.records, created: result.created, status: result.status, ...extra })
          : skipped?.length
            ? json(extra)
            : null,
        id,
      );
    this.db
      .prepare('INSERT INTO audit(id,kind,detail_json,created_at) VALUES(?,?,?,?)')
      .run(randomUUID(), 'action_status', json({ actionId: id, status }), now());
  }
  /** Saves not confirmed yet (being written, or their outcome unknown). */
  unconfirmedCount() {
    return this.db.prepare("SELECT count(*) n FROM actions WHERE status IN ('pending','uncertain')").get().n;
  }
  /**
   * Re-reads the cells of writes whose outcome is unknown. If Google holds the
   * new values the action is verified; if it still holds the old values
   * nothing was written and the action is marked failed.
   */
  async recoverPending() {
    const rows = this.db.prepare("SELECT * FROM actions WHERE status IN ('pending','uncertain')").all();
    let recovered = 0,
      failed = 0;
    for (const row of rows) {
      const changes = this.action(row).changes;
      if (!changes.length) {
        this.finishAction(row.id, 'failed', null);
        failed++;
        continue;
      }
      try {
        const bySheet = Map.groupBy(changes, c => c.sheet);
        const live = await this.sheets.readRows(
          [...bySheet].map(([sheet, list]) => ({
            sheet,
            rows: [moduleMap.get(sheet).headerRow, ...list.map(c => c.row)],
          })),
        );
        const layouts = new Map(
          [...bySheet.keys()].map(sheet => [
            sheet,
            headerLayout(sheet, live.get(rowKey(sheet, moduleMap.get(sheet).headerRow))),
          ]),
        );
        // Until the columns can be told apart again the outcome stays uncertain.
        if ([...layouts.values()].some(l => l.blocked || l.missing.some(f => changes.some(c => c.field === f))))
          continue;
        const read = c => rowValues(c.sheet, live.get(rowKey(c.sheet, c.row)), layouts.get(c.sheet));
        const at = c => {
          const current = read(c);
          return current.formulas[c.field] ? { formula: current.formulas[c.field] } : current.values[c.field];
        };
        if (changes.every(c => sameCell(at(c), c.after))) {
          const records = [];
          for (const c of changes) {
            if (records.some(r => r.id === c.recordId)) continue;
            const existing = this.getRecord(c.recordId);
            const current = this.keepUnavailable(c.sheet, read(c), existing, layouts.get(c.sheet));
            const record = {
              id: c.recordId,
              sheet: c.sheet,
              row: c.row,
              ...current,
              label: labelFor(c.sheet, current.values),
              version: (existing?.version || 0) + 1,
              updatedAt: now(),
              missing: false,
            };
            this.placeRecord(record);
            records.push(record);
          }
          this.finishAction(row.id, 'verified', { records, record: records[0], status: 'verified' });
          recovered++;
        } else if (changes.every(c => sameCell(at(c), c.before))) {
          this.finishAction(row.id, 'failed', null);
          failed++;
        }
      } catch {
        /* Keep uncertain while the Sheet cannot be read. */
      }
    }
    return { recovered, failed };
  }
  /** Lets an administrator settle a write whose outcome could not be determined. */
  resolveAction(id, status) {
    const row = this.db.prepare('SELECT * FROM actions WHERE id=?').get(id);
    if (!row) throw error('ACTION_NOT_FOUND', 'Action not found', 404);
    if (!['pending', 'uncertain'].includes(row.status)) throw error('ACTION_SETTLED', 'Action is already settled', 409);
    if (!['failed', 'verified'].includes(status)) throw error('INVALID_STATUS', 'Status must be failed or verified');
    this.finishAction(id, status, null);
    return this.action(this.db.prepare('SELECT * FROM actions WHERE id=?').get(id));
  }
  previewUndo({ actionIds, changeIds } = {}) {
    if (!Array.isArray(actionIds) || !actionIds.length) throw error('INVALID_SELECTION', 'Select at least one action');
    const selected = [];
    for (const actionId of actionIds) {
      const row = this.db.prepare('SELECT * FROM actions WHERE id=?').get(actionId);
      // Saves confirmed in Google Sheets, and edits read from it (made directly in the sheet).
      if (!row || !['verified', 'observed'].includes(row.status))
        throw error('INVALID_SELECTION', 'Action is not verified', 409);
      selected.push(
        ...this.action(row)
          .changes.filter(c => !changeIds || changeIds.includes(c.id))
          .map(c => ({
            ...c,
            actionId,
            ordinal: this.db.prepare('SELECT rowid n FROM changes WHERE id=?').get(c.id).n,
          })),
      );
    }
    const grouped = new Map();
    for (const c of selected) {
      const key = `${c.recordId}\u0000${c.field}`;
      grouped.set(key, [...(grouped.get(key) || []), c]);
    }
    const changes = [],
      conflicts = [];
    for (const chain of grouped.values()) {
      chain.sort((a, b) => a.ordinal - b.ordinal);
      const first = chain[0],
        last = chain.at(-1),
        record = this.getRecord(first.recordId);
      const lastOrdinal = this.db
        .prepare(`SELECT max(rowid) n FROM changes WHERE id IN (${chain.map(() => '?').join(',')})`)
        .get(...chain.map(c => c.id)).n;
      const laterRows = this.db
        .prepare(
          `SELECT c.id, c.action_id, a.source, a.reverses FROM changes c JOIN actions a ON a.id=c.action_id WHERE c.record_id=? AND c.field=? AND c.rowid>? AND a.status IN ('verified','observed') AND c.id NOT IN (${chain.map(() => '?').join(',')}) ORDER BY c.rowid`,
        )
        .all(first.recordId, first.field, lastOrdinal, ...chain.map(c => c.id));
      // A later edit that was itself undone cancels out with its undo: the cell is back to this change's value.
      const cancelled = new Set();
      laterRows.forEach((u, i) => {
        if (u.source !== 'undo' || !u.reverses) return;
        const reversed = u.reverses.split(',');
        const target = laterRows.slice(0, i).find(r => !cancelled.has(r.id) && reversed.includes(r.action_id));
        if (target) cancelled.add(target.id).add(u.id);
      });
      const later = laterRows.some(r => !cancelled.has(r.id));
      const current = record?.formulas[first.field]
        ? { formula: record.formulas[first.field] }
        : record?.values[first.field];
      const item = {
        recordId: first.recordId,
        field: first.field,
        before: current,
        after: first.before,
        selectedChangeIds: chain.map(c => c.id),
      };
      if (
        !record ||
        record.missing ||
        later ||
        !sameCell(current, last.after) ||
        chain.some((c, i) => i && comparable(c.before) !== comparable(chain[i - 1].after))
      ) {
        conflicts.push({
          ...item,
          reason: !record || record.missing ? 'missing_record' : later ? 'later_field_edit' : 'value_or_chain_changed',
        });
      } else changes.push(item);
    }
    const rowDeletes = this.insertedRowsUndone(actionIds, selected, changes, conflicts);
    return { changes, conflicts, rowDeletes, eligible: conflicts.length === 0 && changes.length > 0 };
  }
  /**
   * Rows a selected save inserted (a butterfly with a suffixed ID, W2B.2): undoing
   * the save deletes the row, so it is undone whole (every cell that save wrote in
   * it) and only while no other save wrote in it since. Their cells stay in
   * `changes`, marked `deleteRow`; a row that cannot go turns its cells into conflicts.
   */
  insertedRowsUndone(actionIds, selected, changes, conflicts) {
    const rows = this.db
      .prepare(`SELECT action_id, record_id, sheet FROM inserted_rows WHERE action_id IN (${actionIds.map(() => '?').join(',')})`)
      .all(...actionIds);
    const out = [];
    const refuse = (recordId, reason) => {
      for (let i = changes.length - 1; i >= 0; i--)
        if (changes[i].recordId === recordId) conflicts.push({ ...changes.splice(i, 1)[0], reason });
    };
    for (const inserted of rows) {
      const chosen = selected.filter(c => c.actionId === inserted.action_id && c.recordId === inserted.record_id);
      if (!chosen.length) continue;
      const written = this.db
        .prepare('SELECT count(*) n FROM changes WHERE action_id=? AND record_id=?')
        .get(inserted.action_id, inserted.record_id).n;
      if (chosen.length < written) {
        refuse(inserted.record_id, 'inserted_row_partial');
        continue;
      }
      const record = this.getRecord(inserted.record_id);
      if (!record || record.missing) continue;
      // Other saves that wrote in the row since, not undone (and not undos themselves).
      const others = this.db
        .prepare(
          `SELECT DISTINCT a.id FROM changes c JOIN actions a ON a.id=c.action_id WHERE c.record_id=? AND a.status IN ('verified','observed') AND a.source<>'undo' AND a.id NOT IN (${actionIds.map(() => '?').join(',')})
            AND NOT EXISTS(SELECT 1 FROM actions u WHERE u.status='verified' AND u.source='undo' AND (',' || u.reverses || ',') LIKE '%,' || a.id || ',%')`,
        )
        .all(inserted.record_id, ...actionIds);
      if (others.length) {
        refuse(inserted.record_id, 'inserted_row_changed');
        continue;
      }
      if (!changes.some(c => c.recordId === inserted.record_id)) continue;
      for (const c of changes) if (c.recordId === inserted.record_id) c.deleteRow = true;
      out.push({ recordId: record.id, sheet: record.sheet, row: record.row, label: record.label });
    }
    return out;
  }
  /**
   * Reverses the selected changes as one new action. The reversal is checked
   * against the live Sheet and written atomically, so it applies completely or
   * not at all.
   */
  async undo({ actionIds, changeIds, requestId, reason }, user) {
    this.validateRole(user);
    this.requireRequestId(requestId);
    const prior = this.actionByRequest(requestId);
    if (prior && prior.status !== 'failed') return applyBatch(this, { requestId }, user, { source: 'undo' });
    const preview = this.previewUndo({ actionIds, changeIds });
    if (!preview.eligible) throw error('UNDO_CONFLICT', 'Selected changes need review', 409, preview);
    const groups = [...Map.groupBy(preview.changes, c => c.recordId)];
    const edits = groups
      .filter(([, items]) => !items[0].deleteRow)
      .map(([id, items]) => ({
        id,
        values: Object.fromEntries(items.map(item => [item.field, item.after])),
        expected: Object.fromEntries(items.map(item => [item.field, item.before])),
      }));
    // A row the save inserted is deleted, not emptied: the rows below it move back up.
    const deletes = groups
      .filter(([, items]) => items[0].deleteRow)
      .map(([id, items]) => ({ id, expected: Object.fromEntries(items.map(item => [item.field, item.before])) }));
    return applyBatch(this, { requestId, reason, edits, deletes }, user, { source: 'undo', reverses: actionIds.join(',') });
  }
  /** Applies reviewed AI proposals (edits and new rows) as one action. */
  async applyProposal(changes, { user, requestId, reason, outbox = null } = {}) {
    if (!Array.isArray(changes) || !changes.length) throw error('INVALID_PROPOSAL', 'No proposed changes');
    return applyBatch(
      this,
      {
        requestId,
        reason,
        // Each proposed field is checked against the value the assistant read.
        edits: changes
          .filter(c => !c.create)
          .map(c => ({
            id: c.recordId,
            values: c.values,
            ...(c.before ? { expected: c.before } : { expectedVersion: c.expectedVersion }),
            ...(c.replaceFormula?.length ? { replaceFormula: c.replaceFormula } : {}),
          })),
        // New rows (e.g. the captures of a Wikiloc walk) go into the next unused rows.
        creates: changes.filter(c => c.create).map(c => ({ module: c.sheet, clientId: c.clientId, values: c.values })),
      },
      user,
      { source: 'ai_approved', outbox },
    );
  }
  getAttachment(id) {
    const r = this.db.prepare('SELECT * FROM attachments WHERE id=?').get(id);
    return (
      r && {
        id: r.id,
        recordId: r.record_id,
        name: r.name,
        mimeType: r.mime_type,
        data: r.data,
        createdBy: r.created_by,
        createdAt: r.created_at,
      }
    );
  }
}

/** Unwraps a one-row batch so single-record callers see the row's own error code. */
async function single(run, current = () => null) {
  try {
    const result = await run;
    return {
      record: result.records?.[0] || result.record || current(),
      action: result.action,
      actions: result.actions,
      status: result.status,
    };
  } catch (e) {
    const items = e.code === 'BATCH_CONFLICT' ? e.details?.items : null;
    if (items?.length === 1)
      throw Object.assign(new Error(items[0].message), {
        code: items[0].code,
        status: items[0].code === 'RECORD_NOT_FOUND' ? 404 : 409,
        details: items[0],
      });
    throw e;
  }
}

export function createStore(config, options) {
  return new Store(config, options);
}
