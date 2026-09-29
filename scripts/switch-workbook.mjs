#!/usr/bin/env node
// Moves the app's database to another Google Sheets workbook (the test copy →
// the team's workbook). See server/switch.mjs for what is kept and reset.
//
//   node scripts/switch-workbook.mjs            dry run (default): works on a temporary
//                                               copy of the database, prints the report,
//                                               deletes the copy
//   node scripts/switch-workbook.mjs --apply    with the service STOPPED: backs up the
//                                               database, then switches it in place
//
// Options: --db PATH (default DATABASE_PATH), --to ID (default WORKBOOK_ID, else the
// team's workbook), --backup-dir DIR (default <db dir>/backups), --keep-copy (dry run).
// Google access: GOOGLE_CREDENTIALS_FILE. The workbook is only read, never written.

import { DatabaseSync } from 'node:sqlite';
import { execFileSync } from 'node:child_process';
import { existsSync, mkdirSync, rmSync, chmodSync, mkdtempSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { tmpdir } from 'node:os';
import { Store } from '../server/store.mjs';
import { GoogleSheets } from '../server/sheets.mjs';
import { REAL_ID, checkWorkbookId } from '../server/workbook.mjs';
import { readWorkbook, switchWorkbook, formatReport } from '../server/switch.mjs';

const args = process.argv.slice(2);
const flag = name => args.includes(name);
const option = name => {
  const i = args.indexOf(name);
  return i >= 0 ? args[i + 1] : undefined;
};
const apply = flag('--apply');
const source = resolve(option('--db') || process.env.DATABASE_PATH || '.local/app.sqlite');
const to = checkWorkbookId(option('--to') || process.env.WORKBOOK_ID || REAL_ID);
const credentials = process.env.GOOGLE_CREDENTIALS_FILE;
const log = message => console.error(message);

if (!existsSync(source)) throw new Error(`No database at ${source}`);
if (!credentials) throw new Error('GOOGLE_CREDENTIALS_FILE is required');

/** A consistent copy of a database that may be in use (VACUUM INTO from a read-only connection). */
function snapshot(from, into) {
  const db = new DatabaseSync(from, { readOnly: true });
  try {
    db.prepare('VACUUM INTO ?').run(into);
  } finally {
    db.close();
  }
  chmodSync(into, 0o600);
  const check = new DatabaseSync(into, { readOnly: true });
  try {
    if (Object.values(check.prepare('PRAGMA integrity_check').get())[0] !== 'ok')
      throw new Error(`Integrity check failed for ${into}`);
  } finally {
    check.close();
  }
}

function serviceActive() {
  try {
    return (
      execFileSync('systemctl', ['--user', 'is-active', 'ithomiini.service'], { encoding: 'utf8' }).trim() === 'active'
    );
  } catch {
    return false; // inactive (non-zero exit) or no systemd here
  }
}

let target = source;
let scratch = null;
if (apply) {
  if (serviceActive() && !flag('--force'))
    throw new Error('The ithomiini service is running: stop it first (systemctl --user stop ithomiini)');
  const dir = resolve(option('--backup-dir') || join(dirname(source), 'backups'));
  mkdirSync(dir, { recursive: true, mode: 0o700 });
  const backup = join(dir, `before-workbook-switch-${new Date().toISOString().replace(/[:.]/g, '-')}.sqlite`);
  snapshot(source, backup);
  log(`Backup written and checked: ${backup}`);
  log(`To undo: cp ${backup} ${source} && rm -f ${source}-wal ${source}-shm`);
} else {
  scratch = mkdtempSync(join(tmpdir(), 'ithomiini-switch-'));
  target = join(scratch, 'copy.sqlite');
  snapshot(source, target);
  log(`Dry run on a copy: ${target}`);
}

try {
  const google = new GoogleSheets({ spreadsheetId: to, googleCredentialsFile: credentials, readOnly: true });
  log(`Reading workbook ${to} (read-only)…`);
  const sheets = await readWorkbook(google, { log });
  const store = new Store({ databasePath: target }, { sheets, switching: true });
  try {
    const report = await switchWorkbook(store);
    console.log(formatReport(report).join('\n'));
    console.log(
      apply
        ? '\nApplied. Set WORKBOOK_ID in service.env if it is not the default, then start the service.'
        : '\nDry run only: the database was not changed. Run again with --apply (service stopped) to switch.',
    );
    if (flag('--json')) console.log(JSON.stringify(report));
  } finally {
    store.close();
  }
} finally {
  if (scratch && !flag('--keep-copy')) rmSync(scratch, { recursive: true, force: true });
}
