// Which T3 Code chat a proposal comes from, and which chat the person has open,
// read from the stock T3 install's own files (nothing patched, never written):
// - state.sqlite (opened read-only): its threads, their titles and projects, and
//   every tool call a chat made (projection_thread_activities, with Claude's
//   tool-use id as toolCallId and the tool's result);
// - its trace log (userdata/logs/server.trace.ndjson): T3's web page sends its
//   traces every ~3 s while it is open, and T3 logs each request with its
//   Referer, the page's address (/<environment>/<threadId>): the chat on screen.
// A person's chats are those of their T3 project (scripts/t3-provision.mjs
// makes it in <workspaces>/<username>). Without T3 (or its files) every
// answer is empty and the app shows all proposals together.
import { DatabaseSync } from 'node:sqlite';
import { existsSync, openSync, readSync, closeSync, statSync } from 'node:fs';
import { basename, join } from 'node:path';

const UUID = '[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}';
/** The chat a T3 page shows: /<environment>/<threadId> (a new chat not sent yet is /draft/<id>). */
const THREAD_PATH = new RegExp(`^/${UUID}/(${UUID})(?:[/?#]|$)`);
const TRACE_PATH = '"url.path":"/api/observability/v1/traces"';
const PROPOSAL_ID = new RegExp(`proposalId\\\\?"\\s*:\\s*\\\\?"(${UUID})`, 'g');
/** How long a page's last report counts as "open now" (it reports every ~3 s). */
const OPEN_MS = 20_000;
/**
 * A chat reported again after this long was opened again: a page reports every
 * ~2 s, with pauses of up to ~6 s. A streak needs two reports, so a tab the
 * browser lets report only about once a minute never counts as open.
 */
const GAP_MS = 15_000;

/**
 * The chats T3 pages showed lately, from the end of T3's trace log: each
 * report as [threadId, ms], oldest first. Pure, for tests.
 */
export function chatsOnScreen(text) {
  const seen = [];
  for (const line of text.split('\n')) {
    if (!line.includes(TRACE_PATH)) continue;
    let span;
    try {
      span = JSON.parse(line);
    } catch {
      continue; // the first line of a tail is cut
    }
    const referer = span.attributes?.['http.request.header.referer'];
    if (!referer) continue;
    let path;
    try {
      path = new URL(referer).pathname;
    } catch {
      continue;
    }
    const thread = THREAD_PATH.exec(path)?.[1];
    if (!thread) continue;
    seen.push([thread, Number(BigInt(span.startTimeUnixNano ?? 0) / 1_000_000n)]);
  }
  return seen.sort((a, b) => a[1] - b[1]);
}

/**
 * Adds reports to `streaks` (thread → { start, last, reports }): a chat's streak starts
 * when a page begins showing it, i.e. its first report after a gap. Mutates and
 * returns `streaks`. Pure otherwise, for tests.
 */
export function addReports(streaks, seen) {
  for (const [thread, at] of seen) {
    const known = streaks.get(thread);
    if (known && at <= known.last) continue;
    streaks.set(
      thread,
      !known || at - known.last > GAP_MS
        ? { start: at, last: at, reports: 1 }
        : { start: known.start, last: at, reports: known.reports + 1 },
    );
  }
  return streaks;
}

/**
 * The chat open now among a person's chats: of those on screen (reported
 * lately), the one opened most recently. A page left on another chat (a second
 * tab, the phone) keeps reporting it, but the chat the person just clicked
 * started later.
 */
export function openChat(streaks, mine, { now = Date.now() } = {}) {
  let best = null;
  for (const [thread, { start, last, reports }] of streaks) {
    if (!mine(thread) || reports < 2 || now - last > OPEN_MS) continue;
    if (!best || start > best.start || (start === best.start && last > best.last)) best = { thread, start, last };
  }
  return best?.thread ?? null;
}

/** Reads the last `bytes` of a file ('' if missing). */
function tail(file, bytes) {
  let fd;
  try {
    const size = statSync(file).size;
    const length = Math.min(size, bytes);
    const buffer = Buffer.alloc(length);
    fd = openSync(file, 'r');
    readSync(fd, buffer, 0, length, size - length);
    return buffer.toString('utf8');
  } catch {
    return '';
  } finally {
    if (fd !== undefined) closeSync(fd);
  }
}

export function createT3Chats({ home, now = Date.now } = {}) {
  const dbFile = home && join(home, 'userdata', 'state.sqlite');
  const traceFile = home && join(home, 'userdata', 'logs', 'server.trace.ndjson');
  let db = null;
  let failedAt = 0;
  /** T3's database, read-only; null without T3 (tried again after a minute). */
  function state() {
    if (db) return db;
    if (!dbFile || now() - failedAt < 60_000) return null;
    try {
      if (!existsSync(dbFile)) throw new Error('no T3 state');
      db = new DatabaseSync(dbFile, { readOnly: true, timeout: 2000 });
      return db;
    } catch {
      failedAt = now();
      return null;
    }
  }
  /** A query on T3's database; its errors (T3 updating its schema, a lock) read as nothing. */
  function query(sql, args, all = true) {
    const d = state();
    if (!d) return all ? [] : undefined;
    try {
      const statement = d.prepare(sql);
      return all ? statement.all(...args) : statement.get(...args);
    } catch {
      try {
        d.close();
      } catch {
        /* already closed */
      }
      db = null;
      return all ? [] : undefined;
    }
  }

  let projects = { at: 0, byUser: new Map() };
  /** The T3 projects of a person (their workspace folder is their username). */
  function projectsOf(username) {
    if (now() - projects.at > 60_000) {
      const byUser = new Map();
      for (const p of query('SELECT project_id, workspace_root FROM projection_projects WHERE deleted_at IS NULL', [])) {
        const name = basename(String(p.workspace_root ?? '').replace(/\/+$/, ''));
        byUser.set(name, [...(byUser.get(name) ?? []), p.project_id]);
      }
      projects = { at: now(), byUser };
    }
    return projects.byUser.get(String(username ?? '')) ?? [];
  }

  /** Threads by id: title, project, when the person last wrote in it. */
  function threads(ids) {
    const list = [...new Set(ids.filter(Boolean))];
    if (!list.length) return new Map();
    const rows = query(
      `SELECT thread_id, title, project_id, latest_user_message_at, deleted_at, archived_at
       FROM projection_threads WHERE thread_id IN (SELECT value FROM json_each(?))`,
      [JSON.stringify(list)],
    );
    return new Map(
      rows.map(r => [
        r.thread_id,
        { id: r.thread_id, title: r.title, projectId: r.project_id, lastUserAt: r.latest_user_message_at, gone: !!(r.deleted_at || r.archived_at) },
      ]),
    );
  }

  /** A person's chats in T3, most recently written in first. */
  function chatsOf(username, limit = 200) {
    const own = projectsOf(username);
    if (!own.length) return [];
    return query(
      `SELECT thread_id, title, project_id, latest_user_message_at FROM projection_threads
       WHERE project_id IN (SELECT value FROM json_each(?)) AND deleted_at IS NULL AND archived_at IS NULL
       ORDER BY coalesce(latest_user_message_at, created_at) DESC LIMIT ?`,
      [JSON.stringify(own), limit],
    ).map(r => ({ id: r.thread_id, title: r.title, projectId: r.project_id, lastUserAt: r.latest_user_message_at }));
  }

  /**
   * The chat that made a tool call, by Claude's tool-use id (MCP tools/call
   * _meta "claudecode/toolUseId", T3's toolCallId). null while T3 has not
   * recorded the call yet.
   */
  function threadOfToolUse(toolUseId, since) {
    if (!/^[A-Za-z0-9_-]{8,100}$/.test(String(toolUseId ?? ''))) return null;
    // Chats changed since (a thread's updated_at follows its activities), then their calls by index:
    // CROSS JOIN keeps that order (otherwise SQLite reads every call T3 ever recorded).
    const row = query(
      `SELECT a.thread_id FROM projection_threads t CROSS JOIN projection_thread_activities a
       WHERE t.updated_at >= ? AND a.thread_id = t.thread_id AND a.created_at >= ?
         AND a.kind IN ('tool.started', 'tool.updated', 'tool.completed') AND a.payload_json LIKE ?
       LIMIT 1`,
      [since, since, `%"toolCallId":"${toolUseId}"%`],
      false,
    );
    return row?.thread_id ?? null;
  }

  /** The only chat of the person answering right now (a tool call without a tool-use id, e.g. Codex). */
  function onlyRunning(username) {
    const own = projectsOf(username);
    if (!own.length) return null;
    const rows = query(
      `SELECT s.thread_id FROM projection_thread_sessions s JOIN projection_threads t ON t.thread_id = s.thread_id
       WHERE t.project_id IN (SELECT value FROM json_each(?)) AND s.status = 'running'`,
      [JSON.stringify(own)],
    );
    return rows.length === 1 ? rows[0].thread_id : null;
  }

  /**
   * The chats whose tool results name these proposals (the call that drafted
   * each one comes first): proposals made before they were linked, or whose
   * call T3 had not recorded yet. Reads chat by chat, letting other requests
   * run in between.
   */
  async function findProposals(ids, since) {
    const wanted = new Set(ids);
    const found = new Map();
    if (!wanted.size) return found;
    const recent = query('SELECT thread_id FROM projection_threads WHERE updated_at >= ?', [since]);
    for (const { thread_id: thread } of recent) {
      await new Promise(resolve => setImmediate(resolve));
      const rows = query(
        `SELECT created_at, payload_json FROM projection_thread_activities
         WHERE thread_id = ? AND created_at >= ? AND kind = 'tool.completed' AND payload_json LIKE '%proposalId%'`,
        [thread, since],
      );
      for (const r of rows)
        for (const m of r.payload_json.matchAll(PROPOSAL_ID)) {
          if (!wanted.has(m[1])) continue;
          const before = found.get(m[1]);
          if (!before || r.created_at < before.at) found.set(m[1], { thread, at: r.created_at });
        }
    }
    return new Map([...found].map(([id, f]) => [id, f.thread]));
  }

  let screen = { at: 0, size: -1 };
  const streaks = new Map();
  /** The chats on screen lately (T3's trace log, read again at most every second). */
  function onScreen() {
    if (!traceFile || now() - screen.at < 1000) return streaks;
    let size = -1;
    try {
      size = statSync(traceFile).size;
    } catch {
      /* no trace log */
    }
    if (size >= 0 && size !== screen.size) addReports(streaks, chatsOnScreen(tail(traceFile, 256 * 1024)));
    // Chats not reported for a while are forgotten (their next report starts a new streak anyway).
    for (const [thread, { last }] of streaks) if (now() - last > 10 * 60_000) streaks.delete(thread);
    screen = { at: now(), size };
    return streaks;
  }

  /** The person's chat open in T3 now, or null (no T3 page open, or T3 does not say). */
  function open(username) {
    const seen = onScreen();
    if (!seen.size) return null;
    const own = new Set(projectsOf(username));
    if (!own.size) return null;
    const info = threads([...seen.keys()]);
    return openChat(seen, thread => own.has(info.get(thread)?.projectId), { now: now() });
  }

  return {
    get available() {
      return !!state();
    },
    projectsOf,
    threads,
    chatsOf,
    threadOfToolUse,
    onlyRunning,
    findProposals,
    open,
    close() {
      try {
        db?.close();
      } catch {
        /* already closed */
      }
      db = null;
    },
  };
}
