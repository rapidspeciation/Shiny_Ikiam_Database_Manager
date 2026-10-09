// The assistant's `query` tool: read-only SQL over the sheets' copy
// (server/replica.mjs). One statement, SELECT or WITH, run by a long-lived child
// process on the copy opened read-only (query_only too). A query that runs past
// the time limit is stopped by killing that process (a running node:sqlite
// statement cannot be interrupted from outside); the next query starts a new one.
// The answer is compact text: a header line, a line per row with tab-separated
// values, and the copy's age.

import { fork } from 'node:child_process';
import { statSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { DatabaseSync } from 'node:sqlite';
import { RESULT_BUDGET } from './tool-budget.mjs';

export const QUERY_LIMIT = 200;
export const QUERY_MAX = 500;
export const QUERY_TIMEOUT_MS = 5000;

export const QUERY_TOOL = {
  type: 'function',
  function: {
    name: 'query',
    description: [
      "One read-only SQLite statement (SELECT or WITH) on a copy of the workbook and the app's save history, refreshed within a minute of a save; the rows come back as text, tab-separated under a header line. For counts, ranges, comparisons across sheets, cell histories and anything over many rows.",
      '- A table per sheet, named as the sheet (Insectary_data, Collection_data, Insectary_stocks, "F1/F2_MutationRate"…): its rows in use. <sheet>_all also holds the pre-made rows (made ahead with their ID, or holding only NA or a default; _premade = 1).',
      "- Their columns are the sheet's headers (describe_sheet lists them, or SELECT column FROM _columns WHERE sheet = '…'); a header with spaces or signs in double quotes (\"CLUTCH NUMBER\"). Also _row (the sheet's row) and _id (the recordId proposals take). Values as the sheet shows them (formulas computed); dates YYYY-MM-DD, times H:MM; ID columns compare without case.",
      '- _tables (name, rows, premade); _columns (sheet, column, header: the sheet\'s, when two differ only in case the second column takes _2; type; list: a dropdown\'s values as JSON; strict).',
      '- history: every cell saved, by the app or typed in Google Sheets: at, sheet, row, id_label, record_id, field, before, after, who, source (the tab, Asistente, Google Sheets, Deshacer…), reason, action_id; formula cells as "(formula)".',
      '- staged: Emergidos and Clutches entries kept in the app, not in the sheet yet, a row per cell: at, who, tab, kind (new row or edit), sheet, row, id_label, record_id, field, value, before, status.',
      "- fold(text): lowercase without accents (fold(SPECIES) LIKE '%oleria%'); km(lat1, lon1, lat2, lon2): the distance in km.",
    ].join('\n'),
    parameters: {
      type: 'object',
      properties: {
        sql: { type: 'string', description: 'One SELECT or WITH statement' },
        limit: { type: 'integer', description: `Rows shown (default ${QUERY_LIMIT}, up to ${QUERY_MAX})` },
      },
      required: ['sql'],
    },
  },
};

export const QUERY_HINT =
  "Tables: SELECT * FROM _tables. A sheet's columns: SELECT column, type FROM _columns WHERE sheet = 'Insectary_data'. Names with spaces or signs go in double quotes: \"CLUTCH NUMBER\", \"F1/F2_MutationRate\".";

/** Why `sql` cannot be run (not one SELECT or WITH statement), or null. */
export function sqlProblem(sql) {
  const text = String(sql ?? '');
  if (!text.trim()) return 'sql is required: one SELECT or WITH statement.';
  if (text.length > 20000) return 'The query is too long (over 20,000 characters).';
  // The code without comments and quoted text, where a ";" ends a statement.
  let code = '';
  for (let i = 0; i < text.length; ) {
    const c = text[i];
    if (c === '-' && text[i + 1] === '-') {
      const end = text.indexOf('\n', i);
      i = end < 0 ? text.length : end;
      code += ' ';
    } else if (c === '/' && text[i + 1] === '*') {
      const end = text.indexOf('*/', i + 2);
      if (end < 0) return 'A /* comment */ is not closed.';
      i = end + 2;
      code += ' ';
    } else if (c === "'" || c === '"' || c === '`' || c === '[') {
      const close = c === '[' ? ']' : c;
      let j = i + 1;
      for (; j < text.length; j++)
        if (text[j] === close) {
          if (close !== ']' && text[j + 1] === close) j++;
          else break;
        }
      if (j >= text.length) return `A quote (${c}) is not closed.`;
      code += c === "'" ? "''" : ' x ';
      i = j + 1;
    } else {
      code += c;
      i++;
    }
  }
  const body = code.replace(/[\s;]+$/, '');
  if (body.includes(';')) return 'One statement per query: nothing may follow its ";".';
  if (!/^[\s(]*(?:SELECT|WITH|VALUES)\b/i.test(body)) return 'Only reading: one SELECT (or WITH … SELECT) statement.';
  return null;
}

const here = fileURLToPath(import.meta.url);

/**
 * Runs queries on the copy at `path` in a child process, one at a time; a
 * query still running after `timeoutMs` has its process killed.
 * run(sql, limit) → { columns, rows, total | more, builtAt } or { error, near?, missing?, timeout? }.
 */
export function createQueryRunner({ path, timeoutMs = QUERY_TIMEOUT_MS }) {
  let child = null;
  let current = null;
  const queue = [];
  let seq = 0;
  let closed = false;

  function spawn() {
    const worker = fork(here, ['--sheets-query-worker', path], {
      execArgv: [],
      serialization: 'advanced',
      stdio: ['ignore', 'ignore', 'inherit', 'ipc'],
    });
    worker.on('message', message => {
      if (worker !== child || !current || message.id !== current.id) return;
      finish(message);
    });
    worker.on('exit', () => {
      if (worker !== child) return;
      child = null;
      if (current) finish({ error: 'The query process stopped unexpectedly.' });
    });
    worker.unref();
    worker.channel?.unref();
    return worker;
  }

  function finish(result) {
    const job = current;
    current = null;
    clearTimeout(job.timer);
    job.resolve(result);
    next();
  }

  function next() {
    if (current || !queue.length || closed) return;
    current = queue.shift();
    child ??= spawn();
    const worker = child;
    current.timer = setTimeout(() => {
      // The statement cannot be interrupted: its process goes, the next query gets a new one.
      child = null;
      worker.kill('SIGKILL');
      finish({ error: `The query ran over ${timeoutMs / 1000} s and was stopped.`, timeout: true });
    }, timeoutMs);
    worker.send({ id: current.id, sql: current.sql, limit: current.limit });
  }

  return {
    run(sql, limit = QUERY_LIMIT) {
      if (closed) return Promise.resolve({ error: 'The query process is closed.' });
      return new Promise(resolve => {
        queue.push({ id: ++seq, sql, limit, resolve });
        next();
      });
    },
    close() {
      closed = true;
      child?.kill('SIGKILL');
      child = null;
      for (const job of queue.splice(0)) job.resolve({ error: 'The query process is closed.' });
    },
  };
}

const fold = value =>
  value === null || value === undefined
    ? null
    : String(value)
        .normalize('NFD')
        .replace(/[\u0300-\u036f]/g, '')
        .toLowerCase();
/** Kilometres between two points (haversine); null when a coordinate is not a number. */
function km(lat1, lon1, lat2, lon2) {
  const n = [lat1, lon1, lat2, lon2].map(v => (v === null || v === '' ? NaN : Number(v)));
  if (n.some(v => !Number.isFinite(v))) return null;
  const rad = Math.PI / 180;
  const a = Math.sin(((n[2] - n[0]) * rad) / 2) ** 2 + Math.cos(n[0] * rad) * Math.cos(n[2] * rad) * Math.sin(((n[3] - n[1]) * rad) / 2) ** 2;
  return 6371.0088 * 2 * Math.asin(Math.min(1, Math.sqrt(a)));
}

/** The copy opened read-only with the helper functions; opened again when the copy is replaced. */
export function openCopy(path) {
  const db = new DatabaseSync(path, { readOnly: true });
  db.exec('PRAGMA query_only = 1');
  db.function('fold', { deterministic: true }, fold);
  db.function('km', { deterministic: true }, km);
  return db;
}

/** Counting the rows past the limit stops here (the total is then "more than"). */
const COUNT_MS = 1500;

/** One query on an open copy: the first `limit` rows and how many there are. */
export function runQuery(db, sql, limit) {
  const max = Math.min(Math.max(Number(limit) || QUERY_LIMIT, 1), QUERY_MAX);
  try {
    const statement = db.prepare(sql);
    statement.setReturnArrays(true);
    const columns = statement.columns().map(c => c.name);
    const rows = [];
    let total = 0;
    let more = false;
    const started = Date.now();
    for (const row of statement.iterate()) {
      if (rows.length < max) rows.push(row);
      total++;
      if (total > max && total % 1000 === 0 && Date.now() - started > COUNT_MS) {
        more = true;
        break;
      }
    }
    return { columns, rows, total, ...(more ? { more } : {}) };
  } catch (e) {
    return { error: e.message, ...nearNames(db, e.message) };
  }
}

/** For "no such column/table: x", the copy's names like it. */
function nearNames(db, message) {
  const m = /no such (column|table): (\S+)/.exec(message);
  if (!m) return {};
  const name = fold(m[2].split('.').at(-1).replace(/^["'`[]|["'`\]]$/g, '')).replace(/[\s_]/g, '');
  if (name.length < 2) return {};
  try {
    const near =
      m[1] === 'table'
        ? db
            .prepare("SELECT name FROM _tables WHERE replace(replace(fold(name), '_', ''), ' ', '') LIKE ? LIMIT 8")
            .all(`%${name}%`)
            .map(r => r.name)
        : db
            .prepare("SELECT sheet, column FROM _columns WHERE replace(replace(fold(column), '_', ''), ' ', '') LIKE ? LIMIT 12")
            .all(`%${name}%`)
            .map(r => `${r.sheet}.${r.column}`);
    return near.length ? { near } : {};
  } catch {
    return {};
  }
}

const CELL = 1000;
/** A value on one line of the answer. */
function show(value) {
  if (value === null || value === undefined) return '';
  if (value instanceof Uint8Array) return `x'${Buffer.from(value.subarray(0, 32)).toString('hex')}${value.length > 32 ? '…' : ''}'`;
  const text = String(value).replaceAll('\\', '\\\\').replaceAll('\t', '\\t').replaceAll('\n', '\\n').replaceAll('\r', '');
  return text.length > CELL ? `${text.slice(0, CELL)}…(+${text.length - CELL} characters)` : text;
}

const age = (builtAt, now) => {
  const minutes = Math.round((now - Date.parse(builtAt)) / 60000);
  return minutes < 1 ? 'just now' : minutes < 120 ? `${minutes} min ago` : `${Math.round(minutes / 60)} h ago`;
};

/** The answer's text: the columns, a line per row, what was left out and the copy's age. */
export function formatRows(result, { limit = QUERY_LIMIT, pending = false, now = Date.now(), budget = RESULT_BUDGET - 300 } = {}) {
  const lines = [result.columns.map(show).join('\t')];
  let size = lines[0].length;
  let shown = 0;
  for (const row of result.rows) {
    const line = row.map(show).join('\t');
    if (size + line.length + 1 > budget) break;
    lines.push(line);
    size += line.length + 1;
    shown++;
  }
  const max = Math.min(Math.max(Number(limit) || QUERY_LIMIT, 1), QUERY_MAX);
  const total = result.more ? `more than ${result.total}` : String(result.total);
  const notes = [];
  if (!result.total) notes.push('(no rows)');
  else if (shown < result.rows.length)
    notes.push(`(${shown} of ${total} rows shown: size limit. Ask for fewer columns or rows, or for counts.)`);
  else if (shown < result.total || result.more)
    notes.push(`(${shown} of ${total} rows shown${max < QUERY_MAX ? `; limit goes up to ${QUERY_MAX}` : ''}. Narrow the query or count.)`);
  if (result.builtAt)
    notes.push(
      `(Copy of ${result.builtAt.slice(0, 16).replace('T', ' ')} UTC, ${age(result.builtAt, now)}${pending ? '; saves since then arrive within a minute' : ''}.)`,
    );
  return [...lines, ...notes].join('\n');
}

// The child process: `--sheets-query-worker <copy>`, queries over IPC.
if (process.argv[1] === here && process.argv[2] === '--sheets-query-worker') {
  const path = process.argv[3];
  let db = null;
  let opened = null;
  let builtAt = null;
  process.on('message', ({ id, sql, limit }) => {
    let stat;
    try {
      stat = statSync(path);
    } catch {
      process.send({ id, error: 'The copy of the sheets is not built yet.', missing: true });
      return;
    }
    // The copy is replaced whole (a new file renamed over it): open the new one.
    const stamp = `${stat.ino}:${stat.mtimeMs}`;
    if (stamp !== opened) {
      try {
        db?.close();
        db = openCopy(path);
        opened = stamp;
        builtAt = db.prepare("SELECT value FROM _meta WHERE key = 'built_at'").get()?.value ?? null;
      } catch (e) {
        db = null;
        opened = null;
        process.send({ id, error: `The copy of the sheets cannot be opened: ${e.message}` });
        return;
      }
    }
    process.send({ id, ...runQuery(db, sql, limit), builtAt });
  });
  process.on('disconnect', () => process.exit(0));
}
