import { DatabaseSync } from 'node:sqlite';
import { randomUUID } from 'node:crypto';
import { mkdirSync, chmodSync } from 'node:fs';
import { dirname } from 'node:path';
import { modules, moduleMap, labelFor, validateValues, comparable, nextInsectaryId, makeSourceUrl } from './schema.mjs';
import { GoogleSheets, LocalSheets, rowKey, rowValues } from './sheets.mjs';
import { headerLayout, sameLayout } from './columns.mjs';
import { applyBatch } from './batch.mjs';
import { initMonitoring } from './monitoring.mjs';

const json = value => JSON.stringify(value);
const parse = value => (value ? JSON.parse(value) : null);
const now = () => new Date().toISOString();
const error = (code, message, status = 400, details) => Object.assign(new Error(message), { code, status, details });

export class Store {
  constructor(config = {}, { sheets, seed } = {}) {
    this.config = config;
    const dbPath = config.databasePath || ':memory:';
    if (dbPath !== ':memory:') mkdirSync(dirname(dbPath), { recursive: true });
    this.db = new DatabaseSync(dbPath);
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
    `);
    if (
      !this.db
        .prepare('PRAGMA table_info(records)')
        .all()
        .some(c => c.name === 'observed')
    )
      this.db.exec('ALTER TABLE records ADD COLUMN observed INTEGER NOT NULL DEFAULT 1');
    this.db.exec('CREATE INDEX IF NOT EXISTS records_updated ON records(sheet,updated_at)');
    initMonitoring(this.db);
    this.sheets = sheets || (config.localMode ? new LocalSheets(seed || {}) : new GoogleSheets(config));
    this.localMode = this.sheets instanceof LocalSheets;
    this.queue = Promise.resolve();
    this.syncPromise = null;
    this.writeEpoch = new Map();
    this.inflight = new Map();
    this.headerProblems = new Map();
    // Each sheet's column map from its live header, as the last sync read it.
    this.layouts = new Map();
    this.syncStatus = {
      state: this.localMode ? 'offline_seed' : 'not_synced',
      lastSync: this.getSetting('lastSync'),
      source: this.localMode ? 'local' : 'google',
      spreadsheetId: this.sheets.spreadsheetId,
    };
  }
  close() {
    this.db.close();
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
      sourceUrl: makeSourceUrl(row.sheet, row.row_num, this.sheets.spreadsheetId),
      missing: Boolean(row.missing),
      observed: Boolean(row.observed),
    };
  }
  getRecord(id) {
    return this.hydrate(this.db.prepare('SELECT * FROM records WHERE id=?').get(id));
  }
  getRecordBySheetRow(sheet, row) {
    return this.hydrate(this.db.prepare('SELECT * FROM records WHERE sheet=? AND row_num=?').get(sheet, row));
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
  getHistory({ recordId, q, source, actor, sheet, status, from, to, limit = 50, offset = 0 } = {}) {
    const clauses = [];
    const args = [];
    if (recordId) {
      clauses.push('EXISTS(SELECT 1 FROM changes c WHERE c.action_id=a.id AND c.record_id=?)');
      args.push(recordId);
    }
    if (q) {
      const like = `%${String(q).replace(/[\\%_]/g, m => '\\' + m)}%`;
      clauses.push(`(a.reason LIKE ? ESCAPE '\\' OR EXISTS(SELECT 1 FROM changes c LEFT JOIN records r ON r.id=c.record_id
        WHERE c.action_id=a.id AND (r.label LIKE ? ESCAPE '\\' OR c.field LIKE ? ESCAPE '\\' OR c.before_json LIKE ? ESCAPE '\\' OR c.after_json LIKE ? ESCAPE '\\')))`);
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
      clauses.push('EXISTS(SELECT 1 FROM changes c WHERE c.action_id=a.id AND c.sheet=?)');
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
  persistRecord(record) {
    this.db
      .prepare(
        `INSERT INTO records(id,sheet,row_num,values_json,formulas_json,identity_json,label,version,updated_at,missing,observed) VALUES(?,?,?,?,?,?,?,?,?,?,?)
      ON CONFLICT(id) DO UPDATE SET row_num=excluded.row_num,values_json=excluded.values_json,formulas_json=excluded.formulas_json,identity_json=excluded.identity_json,label=excluded.label,version=excluded.version,updated_at=excluded.updated_at,missing=excluded.missing,observed=excluded.observed`,
      )
      .run(
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
      );
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
  async sync({ sheets = modules.map(m => m.id), force = false } = {}) {
    if (this.syncPromise) return this.syncPromise;
    const run = this.performSync({ sheets, force });
    this.syncPromise = run;
    try {
      return await run;
    } finally {
      this.syncPromise = null;
    }
  }
  async performSync({ sheets, force }) {
    const full = sheets.length === modules.length;
    const revision = full && this.sheets.revision ? await this.sheets.revision() : null;
    if (full && !force && revision && revision === this.getSetting('sourceRevision') && this.syncStatus.lastSync) {
      this.syncStatus = { ...this.syncStatus, state: 'ok', checkedAt: now(), unchanged: true };
      return this.syncStatus;
    }
    this.syncStatus = { ...this.syncStatus, state: 'syncing' };
    let added = 0,
      changed = 0,
      moved = 0,
      missing = 0,
      skipped = 0;
    try {
      for (const sheet of sheets) {
        const mod = moduleMap.get(sheet);
        if (!mod) throw error('MODULE_NOT_FOUND', 'Unknown module', 404);
        if (this.inflight.get(sheet)) {
          skipped++;
          continue;
        }
        const epoch = this.writeEpoch.get(sheet) || 0;
        const rows = await this.sheets.readSheet(sheet);
        const layout = this.readLayout(sheet, rows.find(r => r.row === mod.headerRow));
        if (layout.blocked) {
          // Which column holds which field is unclear: reading would scramble values.
          skipped++;
          continue;
        }
        const current = rows
          .filter(r => r.row > mod.headerRow)
          .map(r => ({ row: r.row, ...rowValues(sheet, r, layout) }))
          .filter(r => Object.values(r.values).some(v => v !== null && v !== '') || Object.keys(r.formulas).length);
        // Only the columns present are compared: a missing column keeps its last known values.
        const view = record =>
          layout.missing.length
            ? json(Object.fromEntries(Object.entries(record.values).filter(([k]) => layout.columns.has(k)))) +
              json(Object.fromEntries(Object.entries(record.formulas).filter(([k]) => layout.columns.has(k))))
            : json(record.values) + json(record.formulas);
        await this.runExclusive(async () => {
          if (epoch !== (this.writeEpoch.get(sheet) || 0)) {
            skipped++;
            return;
          }
          this.db.exec('BEGIN IMMEDIATE');
          try {
            const old = this.db.prepare('SELECT * FROM records WHERE sheet=? AND missing=0').all(sheet);
            const byRow = new Map(old.map(r => [r.row_num, r]));
            const byIdentity = new Map();
            for (const r of old) {
              const key = this.fingerprint(sheet, parse(r.values_json));
              if (key !== '{}') byIdentity.set(key, [...(byIdentity.get(key) || []), r]);
            }
            // Rows without identifier columns are matched by identical content
            // first, so inserting a row in the Sheet does not relabel every row below it.
            const byContent = new Map();
            for (const r of old) {
              if (Object.keys(parse(r.identity_json) || {}).length) continue;
              const key = view({ values: parse(r.values_json), formulas: parse(r.formulas_json) });
              byContent.set(key, [...(byContent.get(key) || []), r]);
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
            for (const item of current) {
              const identity = this.fingerprint(sheet, item.values);
              if (identity === '{}') {
                const same = (byContent.get(view(item)) || []).filter(
                  o => !seen.has(o.id),
                );
                if (same.length === 1) claim(item, same[0]);
                continue;
              }
              const atRow = byRow.get(item.row);
              if (atRow && !seen.has(atRow.id) && this.fingerprint(sheet, parse(atRow.values_json)) === identity) {
                claim(item, atRow);
                continue;
              }
              const candidates = (byIdentity.get(identity) || []).filter(o => !seen.has(o.id));
              if (candidates.length === 1) claim(item, candidates[0]);
            }
            for (const item of current) {
              if (matched.has(item)) continue;
              const atRow = byRow.get(item.row);
              // A row keeps its record when edited in place, unless its identifier changed.
              if (atRow && !seen.has(atRow.id) && !Object.keys(parse(atRow.identity_json) || {}).length)
                claim(item, atRow);
            }
            // Parked row numbers go below every number already used, so repeated syncs never collide.
            let displaced =
              Math.min(0, this.db.prepare('SELECT min(row_num) n FROM records WHERE sheet=?').get(sheet).n ?? 0) - 1;
            for (const read of current) {
              const found = matched.get(read) || null;
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
              if (previous) {
                const diffs = [];
                for (const field of mod.fields) {
                  if (!layout.columns.has(field.key)) continue;
                  const before = previous.formulas[field.key]
                    ? { formula: previous.formulas[field.key] }
                    : previous.values[field.key];
                  const after = item.formulas[field.key]
                    ? { formula: item.formulas[field.key] }
                    : item.values[field.key];
                  if (comparable(before) !== comparable(after)) diffs.push({ field: field.key, before, after });
                }
                if (diffs.length) {
                  record.version++;
                  record.updatedAt = now();
                  changed++;
                  this.recordExternalChanges(record, diffs);
                }
              } else added++;
              // A moved row may currently be occupied by a different old record. Shift that old mapping aside first.
              if (found && found.row_num !== item.row)
                this.db.prepare('UPDATE records SET row_num=? WHERE id=?').run(displaced--, found.id);
              const occupant = this.db
                .prepare('SELECT id FROM records WHERE sheet=? AND row_num=? AND id<>?')
                .get(sheet, item.row, record.id);
              if (occupant) this.db.prepare('UPDATE records SET row_num=? WHERE id=?').run(displaced--, occupant.id);
              this.persistRecord(record);
              seen.add(record.id);
            }
            for (const prior of old)
              if (!seen.has(prior.id)) {
                this.db
                  .prepare('UPDATE records SET missing=1,row_num=?,updated_at=? WHERE id=?')
                  .run(displaced--, now(), prior.id);
                missing++;
              }
            this.db.exec('COMMIT');
          } catch (e) {
            this.db.exec('ROLLBACK');
            throw e;
          }
        });
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
        headerProblems: Object.fromEntries([...this.headerProblems].filter(([, problems]) => problems.length)),
      };
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
  recordExternalChanges(record, diffs) {
    const id = randomUUID(),
      created = now();
    this.db
      .prepare(
        'INSERT INTO actions(id,request_id,actor,source,created_at,status,reason,reverses,result_json) VALUES(?,?,?,?,?,?,?,?,?)',
      )
      .run(
        id,
        null,
        'unknown',
        'sheet_reconciliation',
        created,
        'observed',
        'Snapshot comparison; intermediate edits and editor unknown',
        null,
        null,
      );
    for (const d of diffs)
      this.db
        .prepare(
          'INSERT INTO changes(id,action_id,record_id,sheet,row_num,field,before_json,after_json) VALUES(?,?,?,?,?,?,?,?)',
        )
        .run(randomUUID(), id, record.id, record.sheet, record.row, d.field, json(d.before), json(d.after));
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
          this.recordExternalChanges(record, diffs);
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
        if (changes.every(c => comparable(at(c)) === comparable(c.after))) {
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
        } else if (changes.every(c => comparable(at(c)) === comparable(c.before))) {
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
      if (!row || row.status !== 'verified') throw error('INVALID_SELECTION', 'Action is not verified', 409);
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
      const later = this.db
        .prepare(
          `SELECT c.id FROM changes c JOIN actions a ON a.id=c.action_id WHERE c.record_id=? AND c.field=? AND c.rowid>? AND a.status IN ('verified','observed') AND c.id NOT IN (${chain.map(() => '?').join(',')}) LIMIT 1`,
        )
        .get(first.recordId, first.field, lastOrdinal, ...chain.map(c => c.id));
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
        comparable(current) !== comparable(last.after) ||
        chain.some((c, i) => i && comparable(c.before) !== comparable(chain[i - 1].after))
      ) {
        conflicts.push({
          ...item,
          reason: !record || record.missing ? 'missing_record' : later ? 'later_field_edit' : 'value_or_chain_changed',
        });
      } else changes.push(item);
    }
    return { changes, conflicts, eligible: conflicts.length === 0 && changes.length > 0 };
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
    const edits = [...Map.groupBy(preview.changes, c => c.recordId)].map(([id, items]) => ({
      id,
      values: Object.fromEntries(items.map(item => [item.field, item.after])),
      expected: Object.fromEntries(items.map(item => [item.field, item.before])),
    }));
    return applyBatch(this, { requestId, reason, edits }, user, { source: 'undo', reverses: actionIds.join(',') });
  }
  /** Applies reviewed AI proposals (edits and new rows) as one action. */
  async applyProposal(changes, { user, requestId, reason } = {}) {
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
      { source: 'ai_approved' },
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
