// The database as the assistant's workers see it (server/assistant-worker.mjs): their own connection
// to the app's database file, the Store's reads, and nothing that reaches Google or the app's state
// in memory. What they read of that state (each sheet's columns as the last sync read them, how
// Google answers) the app sends them as it changes. SQLite itself refuses any write they try outside
// the tables they may write (the proposals, for the tools' worker; none, for the views' worker), so a
// tool can never change the local copy, the history or a save.
import { DatabaseSync, constants } from 'node:sqlite';
import { Store } from './store.mjs';

/** The Store's methods that only read (they run on the reader's connection). */
const READS = [
  'statement',
  'copyVersion',
  'getSetting',
  'cachedWorkbook',
  'listModules',
  'hydrate',
  'getRecord',
  'getRecordBySheetRow',
  'searchRecords',
  'listEvents',
  'listTasks',
  'task',
  'getHistory',
  'action',
  'actionByRequest',
  'identity',
  'fingerprint',
  'keepUnavailable',
  'sheetRevision',
  'storedAs',
  'hasObservation',
  'validateRole',
  'requireRequestId',
  'suggestInsectaryId',
  'unconfirmedCount',
  'previewUndo',
  'insertedRowsUndone',
  'displacedRow',
  'getAttachment',
];

/**
 * What code shared with the app looks for and does without (`store.watchRecords?.(…)`), and
 * what a promise check asks (`then`): absent here. Anything else not here is a mistake: refused.
 */
const ABSENT = new Set([
  'then',
  'watchRecords',
  'watchSyncs',
  'watchLive',
  'refreshRows',
  'outbox',
  'staged',
  'recordWatchers',
  'syncPromise',
]);

/** The tables the tools' worker writes: its proposals (drafted, revised, linked to their chat), their notes and conversations. */
export const PROPOSAL_TABLES = ['ai_proposals', 'ai_messages', 'ai_threads'];

/** Pragmas that change the database (the rest only read it: table_info, data_version…). */
const PRAGMA_WRITES = new Set([
  'writable_schema',
  'journal_mode',
  'wal_checkpoint',
  'user_version',
  'schema_version',
  'locking_mode',
  'synchronous',
  'incremental_vacuum',
  'optimize',
]);

/**
 * SQLite's authorizer for a reader: writes only to `writable` tables. Creating a table or an index
 * is allowed (the lazy CREATE … IF NOT EXISTS of shared code: SQLite asks before it looks whether it
 * exists, and a missing one is made empty, writing the schema); altering or dropping one is not,
 * nor any pragma that changes the database.
 */
export function readerAuthorizer(writable = []) {
  const allowed = new Set(writable);
  const schema = new Set(['sqlite_master', 'sqlite_schema', 'sqlite_temp_master', 'sqlite_temp_schema']);
  const C = constants;
  const refused = new Set([
    C.SQLITE_ALTER_TABLE,
    C.SQLITE_DROP_TABLE,
    C.SQLITE_DROP_INDEX,
    C.SQLITE_DROP_TRIGGER,
    C.SQLITE_DROP_VIEW,
    C.SQLITE_DROP_VTABLE,
    C.SQLITE_CREATE_TRIGGER,
    C.SQLITE_CREATE_VIEW,
    C.SQLITE_CREATE_VTABLE,
    C.SQLITE_ATTACH,
    C.SQLITE_DETACH,
  ]);
  return (action, first, second) => {
    // The schema is written by a CREATE (ALTER and DROP are refused by their own action below).
    if (action === C.SQLITE_INSERT || action === C.SQLITE_UPDATE)
      return allowed.has(first) || schema.has(first) ? C.SQLITE_OK : C.SQLITE_DENY;
    if (action === C.SQLITE_DELETE) return allowed.has(first) ? C.SQLITE_OK : C.SQLITE_DENY;
    if (action === C.SQLITE_PRAGMA) return PRAGMA_WRITES.has(String(first).toLowerCase()) ? C.SQLITE_DENY : C.SQLITE_OK;
    return refused.has(action) ? C.SQLITE_DENY : C.SQLITE_OK;
  };
}

class StoreReader {}
for (const name of READS) StoreReader.prototype[name] = Store.prototype[name];

/**
 * A reader of the database file at `path`: the Store's reads (READS) on its own connection, writes
 * refused by SQLite outside `writable`. `localMode` and `spreadsheetId` as the app's store has them
 * (rows' links to the sheet); `layouts` and `google` as the app last sent them (setLayouts, setGoogle).
 * Any other member of the Store is refused when used, so shared code that would need the app's own
 * state (Google, the outbox, the watchers) fails loudly instead of acting on a copy.
 */
export function createStoreReader({
  path,
  writable = [],
  localMode = false,
  spreadsheetId = null,
  layouts = [],
  google = null,
  config = {},
}) {
  if (!path || path === ':memory:') throw new Error('A store reader needs a database file');
  const db = new DatabaseSync(path, { timeout: 30_000 });
  db.exec('PRAGMA foreign_keys=ON');
  db.setAuthorizer(readerAuthorizer(writable));
  const target = new StoreReader();
  let googleState = google ?? { workbook: { state: 'ok' }, outbox: { waiting: 0 }, staged: { staged: 0, sent: 0 } };
  Object.assign(target, {
    reader: true,
    db,
    config,
    localMode,
    sheets: Object.freeze({ spreadsheetId }),
    statements: new Map(),
    layouts: new Map(layouts),
    closed: false,
    /** How Google answers, as the app last said (server/store.mjs googleState). */
    googleState: () => googleState,
    setGoogle(state) {
      if (state) googleState = state;
    },
    /** Sheets whose columns the last sync read anew: [[sheet, layout]]. A sheet's layout object stays while it is the same. */
    setLayouts(entries) {
      for (const [sheet, layout] of entries) target.layouts.set(sheet, layout);
    },
    close() {
      target.closed = true;
      db.close();
    },
  });
  return new Proxy(target, {
    get(t, key, receiver) {
      if (typeof key === 'symbol' || key in t) return Reflect.get(t, key, receiver);
      if (ABSENT.has(key)) return undefined;
      throw Object.assign(new Error(`store.${key} is not available to the assistant's worker`), {
        code: 'READER_ONLY',
      });
    },
    set(t, key, value) {
      if (!(key in t))
        throw Object.assign(new Error(`store.${String(key)} cannot be set in the assistant's worker`), {
          code: 'READER_ONLY',
        });
      t[key] = value;
      return true;
    },
  });
}
