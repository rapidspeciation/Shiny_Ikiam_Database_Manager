// The lab's workbook (LOCAL_MODE with SEED_FILE) in a SQLite file instead of memory.
//
// A LocalSheets keeps every row of the snapshot as Google's cell objects: some 700 MB of heap in the lab's
// workbook, after a JSON.parse of the 370 MB snapshot at every start. FileSheets answers the same, with the rows
// in a file: the snapshot is read once, in a child process, into `<snapshot>.rows.sqlite` (made again when the
// snapshot changes), and each start works on its own copy of that file, so a restart still undoes everything.
//
// Each row is kept as `v8.serialize(cells)`: what v8.deserialize gives back is what structuredClone gives
// (empty cells stay empty, not null), and in the order LocalSheets' array has (a row written past the others
// goes last until the sheet is sorted again), so the rows read here are those LocalSheets would give. A write loads the
// rows it touches (liveRow), changes them with LocalSheets' own code and writes them back before it returns
// (commitRows), with no await in between, as LocalSheets does.

import { spawn } from 'node:child_process';
import { constants, copyFileSync, existsSync, readFileSync, renameSync, rmSync, statSync, accessSync } from 'node:fs';
import { createHash } from 'node:crypto';
import { basename, dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { DatabaseSync } from 'node:sqlite';
import { deserialize, serialize } from 'node:v8';
import { LocalSheets, moveRowRefs, rowKey, seedRows } from './sheets.mjs';

/** Changes when the file's layout does: a new version makes the file again. */
const LAYOUT = 3;
/** Rows readSheet reads between two turns of the others (as LocalSheets). */
const READ_ROWS = 500;

const hasFormula = cells => cells.some(cell => cell?.userEnteredValue?.formulaValue !== undefined);

export class FileSheets extends LocalSheets {
  /** `path`: a rows file of buildRowsFile, changed in place (open a copy of it). */
  constructor(path, options = {}) {
    super({}, options);
    // Nothing of LocalSheets may reach rows in memory: they are in the file.
    this.rows = undefined;
    this.path = path;
    this.db = new DatabaseSync(path);
    // A working copy, thrown away at the next start: no need to survive a crash.
    this.db.exec('PRAGMA journal_mode = MEMORY; PRAGMA synchronous = OFF; PRAGMA cache_size = -8000');
    const q = sql => this.db.prepare(sql);
    this.q = {
      get: q('SELECT cells FROM rows WHERE sheet = ? AND row = ?'),
      put: q('UPDATE rows SET cells = ?, columns = ?, formulas = ? WHERE sheet = ? AND row = ?'),
      add: q('INSERT INTO rows (sheet, row, seq, cells, columns, formulas) VALUES (?, ?, ?, ?, 0, 0)'),
      maxSeq: q('SELECT MAX(seq) AS n FROM rows WHERE sheet = ?'),
      // Rows in row order again (as LocalSheets sorts its array), in two steps so no place is taken twice.
      sortOut: q('UPDATE rows SET seq = -row WHERE sheet = ? AND seq != row'),
      sortBack: q('UPDATE rows SET seq = -seq WHERE sheet = ? AND seq < 0'),
      piece: q('SELECT row, seq, cells FROM rows WHERE sheet = ? AND seq > ? ORDER BY seq LIMIT ?'),
      all: q('SELECT row, cells FROM rows WHERE sheet = ? ORDER BY seq'),
      maxRow: q('SELECT MAX(row) AS n FROM rows WHERE sheet = ?'),
      maxColumns: q('SELECT MAX(columns) AS n FROM rows WHERE sheet = ?'),
      remove: q('DELETE FROM rows WHERE sheet = ? AND row = ?'),
      shiftOut: q('UPDATE rows SET row = -(row + ?) WHERE sheet = ? AND row >= ?'),
      shiftBack: q('UPDATE rows SET row = -row WHERE sheet = ? AND row < 0'),
      formulaRows: q('SELECT row, cells FROM rows WHERE sheet = ? AND formulas = 1'),
      setCells: q('UPDATE rows SET cells = ? WHERE sheet = ? AND row = ?'),
      addSheet: q('INSERT OR IGNORE INTO sheets (name, ord) VALUES (?, (SELECT COALESCE(MAX(ord), -1) + 1 FROM sheets))'),
    };
    this.names = this.db.prepare('SELECT name FROM sheets ORDER BY ord').all().map(r => r.name);
    // The rows a write is changing, by rowKey: { sheet, target }.
    this.work = new Map();
  }
  close() {
    this.work.clear();
    if (this.db.isOpen) this.db.close();
  }
  load(sheet, number) {
    const found = this.q.get.get(sheet, number);
    return found ? { row: number, cells: deserialize(found.cells) } : undefined;
  }
  sheetNames() {
    return [...this.names];
  }
  findRow(sheet, number) {
    return this.work.get(rowKey(sheet, number))?.target ?? this.load(sheet, number);
  }
  liveRow(sheet, number) {
    const key = rowKey(sheet, number);
    const held = this.work.get(key);
    if (held) return held.target;
    const target = this.load(sheet, number);
    if (target) this.work.set(key, { sheet, target });
    return target;
  }
  /**
   * As LocalSheets: the new row goes last (`seq`, a row's place in the sheet's order), and with `sorted` the
   * sheet is put in row order. Written at once, its cells by commitRows.
   */
  addRow(sheet, target, sorted) {
    if (!this.names.includes(sheet)) {
      this.names.push(sheet);
      this.q.addSheet.run(sheet);
    }
    const last = this.q.maxSeq.get(sheet)?.n ?? 0;
    // A sheet in row order keeps seq = row, so its rows need no new places when it is sorted again.
    this.q.add.run(sheet, target.row, target.row > last ? target.row : last + 1, serialize([]));
    if (sorted) this.sortRows(sheet);
    this.work.set(rowKey(sheet, target.row), { sheet, target });
    return target;
  }
  sortRows(sheet) {
    this.q.sortOut.run(sheet);
    this.q.sortBack.run(sheet);
  }
  commitRows() {
    if (!this.work.size) return;
    this.db.exec('BEGIN');
    try {
      for (const { sheet, target } of this.work.values())
        this.q.put.run(serialize(target.cells), target.cells.length, hasFormula(target.cells) ? 1 : 0, sheet, target.row);
      this.db.exec('COMMIT');
    } catch (error) {
      this.db.exec('ROLLBACK');
      throw error;
    } finally {
      this.work.clear();
    }
  }
  maxRow(sheet) {
    this.commitRows();
    return this.q.maxRow.get(sheet)?.n ?? 0;
  }
  maxColumns(sheet) {
    this.commitRows();
    return this.q.maxColumns.get(sheet)?.n ?? 0;
  }
  async readSheet(sheet) {
    await this.gate(true);
    this.commitRows();
    // As LocalSheets: a piece at a time with a turn for the others in between, started again when a write
    // came meanwhile; after a few, the whole sheet in one go.
    for (let attempt = 0; attempt < 3; attempt++) {
      const count = this.writeCount;
      const rows = [];
      const digest = createHash('sha256');
      let last = -Infinity;
      for (let first = true; ; first = false) {
        if (!first) await new Promise(resolve => setImmediate(resolve));
        if (this.writeCount !== count) break;
        const piece = this.q.piece.all(sheet, last, READ_ROWS);
        for (const found of piece) {
          const r = { row: found.row, cells: deserialize(found.cells) };
          digest.update(JSON.stringify(r));
          rows.push(r);
        }
        if (piece.length < READ_ROWS) break;
        last = piece.at(-1).seq;
      }
      if (this.writeCount === count) return withDigest(rows, digest.digest('base64'));
    }
    const rows = this.q.all.all(sheet).map(found => ({ row: found.row, cells: deserialize(found.cells) }));
    const digest = createHash('sha256');
    for (const r of rows) digest.update(JSON.stringify(r));
    return withDigest(rows, digest.digest('base64'));
  }
  /** As LocalSheets.moveRows: the rows below move, and the formulas of the sheet that refer to them. */
  moveRows({ sheetId, startIndex }, delta) {
    const sheet = this.sheetById(sheetId);
    const at = startIndex + 1;
    this.commitRows();
    this.db.exec('BEGIN');
    try {
      if (delta < 0) this.q.remove.run(sheet, at);
      // In two steps, so no row number is taken twice on the way.
      this.q.shiftOut.run(delta, sheet, at);
      this.q.shiftBack.run(sheet);
      this.sortRows(sheet);
      const changed = [];
      for (const found of this.q.formulaRows.all(sheet)) {
        const cells = deserialize(found.cells);
        let moved = false;
        for (const cell of cells) {
          const formula = cell?.userEnteredValue?.formulaValue;
          if (!formula) continue;
          const next = moveRowRefs(formula, at, delta, sheet);
          cell.userEnteredValue = { formulaValue: next };
          if (next !== formula) moved = true;
        }
        if (moved) changed.push([found.row, cells]);
      }
      for (const [row, cells] of changed) this.q.setCells.run(serialize(cells), sheet, row);
      this.db.exec('COMMIT');
    } catch (error) {
      this.db.exec('ROLLBACK');
      throw error;
    }
    if (this.gridRows.has(sheet)) this.gridRows.set(sheet, this.gridRows.get(sheet) + delta);
  }
}

function withDigest(rows, digest) {
  return Object.defineProperty(rows, 'digest', { value: digest });
}

function createLayout(db) {
  db.exec(`
    CREATE TABLE meta (key TEXT PRIMARY KEY, value TEXT) WITHOUT ROWID;
    CREATE TABLE sheets (name TEXT PRIMARY KEY, ord INTEGER NOT NULL) WITHOUT ROWID;
    -- A rowid table: rows of some kB each fill a WITHOUT ROWID table's pages badly (597 MB instead of 438 MB for
    -- the lab's 360 MB of cells).
    -- seq: the row's place in the sheet's order, as LocalSheets' array has it (seq = row while it is in row order).
    CREATE TABLE rows (
      sheet TEXT NOT NULL, row INTEGER NOT NULL, seq INTEGER NOT NULL, cells BLOB NOT NULL,
      columns INTEGER NOT NULL, formulas INTEGER NOT NULL
    );
    CREATE UNIQUE INDEX rows_at ON rows (sheet, row);
    CREATE UNIQUE INDEX rows_order ON rows (sheet, seq);
    CREATE INDEX rows_formulas ON rows (sheet, formulas);
  `);
}

/** The code that shapes the rows (seedRows and the sheets' fields): a change to it makes the file again. */
const CODE = ['./sheets.mjs', './schema.mjs', './file-sheets.mjs'].map(f => readFileSync(new URL(f, import.meta.url)));
const codeHash = createHash('sha256').update(Buffer.concat(CODE)).digest('hex').slice(0, 16);
/** What the rows file was made from: the seed's size and time, the file's layout and the code. */
function seedStamp(seedFile) {
  const { size, mtimeMs } = statSync(seedFile);
  return JSON.stringify({ layout: LAYOUT, code: codeHash, size, mtimeMs });
}

/**
 * The rows of `seedFile` (the lab's snapshot JSON) into a new SQLite file `out`, as LocalSheets' constructor
 * keeps them. Throws, writing nothing, when a sheet has a row number twice (LocalSheets would keep both).
 */
export function buildRowsFile(seedFile, out) {
  const stamp = seedStamp(seedFile);
  let seed = JSON.parse(readFileSync(seedFile, 'utf8'));
  seed = seed.sheets || seed;
  const tmp = `${out}.${process.pid}.tmp`;
  rmSync(tmp, { force: true });
  const db = new DatabaseSync(tmp);
  try {
    db.exec('PRAGMA journal_mode = OFF; PRAGMA synchronous = OFF');
    createLayout(db);
    const addSheet = db.prepare('INSERT INTO sheets (name, ord) VALUES (?, ?)');
    const put = db.prepare('INSERT INTO rows (sheet, row, seq, cells, columns, formulas) VALUES (?, ?, ?, ?, ?, ?)');
    db.exec('BEGIN');
    let ord = 0;
    let count = 0;
    for (const [sheet, given] of Object.entries(seed)) {
      addSheet.run(sheet, ord++);
      const rows = seedRows(sheet, given);
      seed[sheet] = null;
      const inOrder = rows.every((r, i) => !i || rows[i - 1].row < r.row);
      for (const [i, r] of rows.entries()) {
        try {
          put.run(sheet, r.row, inOrder ? r.row : i, serialize(r.cells), r.cells.length, hasFormula(r.cells) ? 1 : 0);
        } catch (error) {
          if (/UNIQUE/.test(error.message)) throw new Error(`${sheet} has row ${r.row} twice in ${seedFile}`);
          throw error;
        }
        count++;
      }
    }
    db.prepare('INSERT INTO meta (key, value) VALUES (?, ?)').run('seed', stamp);
    db.exec('COMMIT');
    db.close();
    renameSync(tmp, out);
    return { rows: count, sheets: ord };
  } catch (error) {
    if (db.isOpen) db.close();
    rmSync(tmp, { force: true });
    throw error;
  }
}

/** True when `file` is a rows file made from `seedFile` as it is now. */
export function rowsFileFresh(file, seedFile) {
  if (!existsSync(file)) return false;
  try {
    const db = new DatabaseSync(file, { readOnly: true });
    try {
      return db.prepare("SELECT value FROM meta WHERE key = 'seed'").get()?.value === seedStamp(seedFile);
    } finally {
      db.close();
    }
  } catch {
    return false;
  }
}

const writable = dir => {
  try {
    accessSync(dir, constants.W_OK);
    return true;
  } catch {
    return false;
  }
};

/**
 * The rows file of `seedFile`, made (in a child process, so the snapshot's parse never sits in the app's
 * memory) when missing or out of date: beside the snapshot, else in `workDir`.
 */
async function rowsFileOf(seedFile, workDir, log) {
  const beside = `${seedFile}.rows.sqlite`;
  const name = `${basename(seedFile)}-${createHash('sha256').update(seedFile).digest('hex').slice(0, 8)}.rows.sqlite`;
  const file = writable(dirname(seedFile)) ? beside : join(workDir, name);
  if (rowsFileFresh(file, seedFile)) return file;
  const started = Date.now();
  const script = fileURLToPath(import.meta.url);
  await new Promise((resolve, reject) => {
    // Its own heap and no profiler: NODE_OPTIONS is the app's.
    const child = spawn(process.execPath, ['--max-old-space-size=4096', script, 'build', seedFile, file], {
      env: { ...process.env, NODE_OPTIONS: '' },
      stdio: ['ignore', 'ignore', 'pipe'],
    });
    const err = [];
    child.stderr.on('data', d => err.push(d));
    child.on('error', reject);
    child.on('exit', (code, signal) => {
      if (code === 0) return resolve();
      // Killed (out of memory): its half-made file is left over.
      rmSync(`${file}.${child.pid}.tmp`, { force: true });
      reject(new Error(Buffer.concat(err).toString().trim() || `exit ${code ?? signal}`));
    });
  });
  log?.(`Workbook rows file made from ${seedFile} in ${((Date.now() - started) / 1000).toFixed(1)} s: ${file}`);
  return file;
}

/**
 * A FileSheets on a fresh copy of the rows of `seedFile`, beside `databasePath`. Null when the rows cannot be
 * kept in a file (no database file, or it failed): the caller keeps them in memory.
 */
export async function openFileSheets({ seedFile, databasePath, health, log = console.log } = {}) {
  if (!databasePath || databasePath === ':memory:') return null;
  try {
    const workDir = dirname(databasePath);
    const source = await rowsFileOf(seedFile, workDir, log);
    const copy = join(workDir, 'workbook-rows.sqlite');
    for (const leftover of [copy, `${copy}-journal`]) rmSync(leftover, { force: true });
    // Copy-on-write where the disk can (btrfs, xfs): a start costs no copy of the rows.
    copyFileSync(source, copy, constants.COPYFILE_FICLONE);
    return new FileSheets(copy, { health });
  } catch (error) {
    log?.(`Workbook rows kept in memory (${error.message})`);
    return null;
  }
}

// node server/file-sheets.mjs build <snapshot.json> <out.rows.sqlite>
if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const [command, seedFile, out] = process.argv.slice(2);
  if (command !== 'build' || !seedFile || !out) {
    console.error('Usage: node server/file-sheets.mjs build <snapshot.json> <out.rows.sqlite>');
    process.exit(2);
  }
  const started = Date.now();
  try {
    const done = buildRowsFile(seedFile, out);
    console.error(`${done.rows} rows of ${done.sheets} sheets in ${((Date.now() - started) / 1000).toFixed(1)} s`);
  } catch (error) {
    console.error(error.message);
    process.exit(1);
  }
}
