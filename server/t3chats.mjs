// Which T3 Code chat a proposal comes from, and which chat the person has open,
// read from the stock T3 install's own files (nothing patched, never written):
// - its database, opened read-only (state.sqlite; from 0.0.46 statev2.sqlite, which
//   its update copies from the old one): its threads, their titles and projects, and
//   every tool call a chat made, with Claude's tool-use id and the tool's result.
//   Up to 0.0.45 in projection_threads and projection_thread_activities (toolCallId);
//   from 0.0.46 (its new orchestrator) in the orchestration_v2_projection_* tables (a
//   call is a turn item, its tool-use id nativeItemRef.nativeId; a chat answering has
//   a run under way); projects stay in projection_projects;
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
/** A proposal's id in a tool result, or a table's (show_rows), which is listed and linked the same way. */
const PROPOSAL_ID = new RegExp(`(?:proposalId|tableId)\\\\?"\\s*:\\s*\\\\?"(${UUID})`, 'g');
/** How long a page's last report counts as "open now" (it reports every ~3 s). */
const OPEN_MS = 20_000;
/**
 * A chat reported again after this long was opened again: a page reports every
 * ~2 s, with pauses of up to ~6 s. A streak needs two reports, so a tab the
 * browser lets report only about once a minute never counts as open.
 */
const GAP_MS = 15_000;
/** A run of the new orchestrator still under way (0.0.46 on): its chat is answering, or about to. */
const UNDER_WAY = "('queued', 'preparing', 'starting', 'running', 'waiting')";
/** In the new orchestrator's tables, the chat a thread is: its parent for a subagent's own thread. */
const CHAT_OF = `CASE WHEN json_extract(t.payload_json, '$.lineage.relationshipToParent') = 'subagent'
  THEN json_extract(t.payload_json, '$.lineage.parentThreadId') ELSE t.thread_id END`;
/** When the person last wrote in a chat of the new orchestrator (an imported chat's messages may come later). */
const LAST_USER = `coalesce(
  (SELECT m.updated_at FROM orchestration_v2_projection_messages m WHERE m.thread_id = t.thread_id AND m.role = 'user'
   ORDER BY m.updated_at DESC LIMIT 1),
  (SELECT l.latest_user_message_at FROM projection_threads l WHERE l.thread_id = t.thread_id))`;

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
  const folder = home && join(home, 'userdata');
  const traceFile = home && join(folder, 'logs', 'server.trace.ndjson');
  let db = null;
  let file = null;
  let failedAt = 0;
  let checkedAt = 0;
  let tables = { at: -Infinity, v2: false };
  /** T3's database: statev2.sqlite from 0.0.46 (its update copies state.sqlite there once and leaves the old file as it was). */
  const current = () => join(folder, existsSync(join(folder, 'statev2.sqlite')) ? 'statev2.sqlite' : 'state.sqlite');
  function shut() {
    try {
      db?.close();
    } catch {
      /* already closed */
    }
    db = null;
  }
  /** T3's database, read-only; null without T3 (tried again after a minute). Another file (T3 updated) is noticed within a minute. */
  function state() {
    if (db && now() - checkedAt > 60_000) {
      checkedAt = now();
      if (current() !== file) shut();
    }
    if (db) return db;
    if (!folder || now() - failedAt < 60_000) return null;
    try {
      file = current();
      if (!existsSync(file)) throw new Error('no T3 state');
      db = new DatabaseSync(file, { readOnly: true, timeout: 2000 });
      checkedAt = now();
      tables = { at: -Infinity, v2: false };
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
      shut();
      return all ? [] : undefined;
    }
  }

  /** Whether T3 keeps its chats in the new orchestrator's tables (0.0.46 on), checked again every minute. */
  function v2() {
    if (!state()) return false;
    if (now() - tables.at < 60_000) return tables.v2;
    const found = query("SELECT 1 AS found FROM sqlite_master WHERE type = 'table' AND name = 'orchestration_v2_projection_threads'", [], false);
    tables = { at: now(), v2: !!found };
    return tables.v2;
  }

  let projects = { at: 0, byUser: new Map() };
  /** The person's T3 projects as { id, root } (their workspace folder is their username). */
  function ownProjects(username) {
    if (now() - projects.at > 60_000) {
      const byUser = new Map();
      for (const p of query('SELECT project_id, workspace_root FROM projection_projects WHERE deleted_at IS NULL', [])) {
        const root = String(p.workspace_root ?? '').replace(/\/+$/, '');
        const name = basename(root);
        byUser.set(name, [...(byUser.get(name) ?? []), { id: p.project_id, root }]);
      }
      projects = { at: now(), byUser };
    }
    return projects.byUser.get(String(username ?? '')) ?? [];
  }
  /** The T3 projects of a person, by id. */
  const projectsOf = username => ownProjects(username).map(p => p.id);
  /** The person's own project ({ id, root }), or null; `fresh` reads T3 again (a project just made). */
  function projectOf(username, { fresh = false } = {}) {
    if (fresh) projects = { at: 0, byUser: new Map() };
    return ownProjects(username)[0] ?? null;
  }

  /** Whose a chat is: the username of its project's workspace folder (null: not a person's project, or no such chat). */
  function ownerOf(threadId) {
    const row = query(
      `SELECT p.workspace_root FROM ${v2() ? 'orchestration_v2_projection_threads' : 'projection_threads'} t
       JOIN projection_projects p ON p.project_id = t.project_id WHERE t.thread_id = ? AND p.deleted_at IS NULL`,
      [String(threadId ?? '')],
      false,
    );
    return row ? basename(String(row.workspace_root ?? '').replace(/\/+$/, '')) || null : null;
  }

  /** Threads by id: title, project, when the person last wrote in it. */
  function threads(ids) {
    const list = [...new Set(ids.filter(Boolean))];
    if (!list.length) return new Map();
    const rows = query(
      v2()
        ? `SELECT t.thread_id, t.title, t.project_id, ${LAST_USER} AS latest_user_message_at, t.deleted_at, t.archived_at
           FROM orchestration_v2_projection_threads t WHERE t.thread_id IN (SELECT value FROM json_each(?))`
        : `SELECT thread_id, title, project_id, latest_user_message_at, deleted_at, archived_at
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
      v2()
        ? // A subagent's own thread is part of its parent's chat, not one of the person's.
          `SELECT t.thread_id, t.title, t.project_id, ${LAST_USER} AS latest_user_message_at
           FROM orchestration_v2_projection_threads t
           WHERE t.project_id IN (SELECT value FROM json_each(?)) AND t.deleted_at IS NULL AND t.archived_at IS NULL
             AND json_extract(t.payload_json, '$.lineage.relationshipToParent') IS NOT 'subagent'
           ORDER BY coalesce(latest_user_message_at, t.created_at) DESC LIMIT ?`
        : `SELECT thread_id, title, project_id, latest_user_message_at FROM projection_threads
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
    if (v2()) {
      // Runs ended since or under way, then their calls by run (CROSS JOIN keeps that order).
      const row = query(
        `SELECT ${CHAT_OF} AS thread_id FROM orchestration_v2_projection_runs r
         CROSS JOIN orchestration_v2_projection_turn_items i
         JOIN orchestration_v2_projection_threads t ON t.thread_id = i.thread_id
         WHERE (r.completed_at >= ? OR (r.completed_at IS NULL AND r.status IN ${UNDER_WAY}))
           AND i.run_id = r.run_id AND i.payload_json LIKE ?
         LIMIT 1`,
        [since, `%"nativeId":"${toolUseId}"%`],
        false,
      );
      return row?.thread_id ?? null;
    }
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
      v2()
        ? `SELECT DISTINCT ${CHAT_OF} AS thread_id FROM orchestration_v2_projection_runs r
           JOIN orchestration_v2_projection_threads t ON t.thread_id = r.thread_id
           WHERE t.project_id IN (SELECT value FROM json_each(?)) AND r.status = 'running'`
        : `SELECT s.thread_id FROM projection_thread_sessions s JOIN projection_threads t ON t.thread_id = s.thread_id
           WHERE t.project_id IN (SELECT value FROM json_each(?)) AND s.status = 'running'`,
      [JSON.stringify(own)],
    );
    return rows.length === 1 ? rows[0].thread_id : null;
  }

  /**
   * A chat as a message sent to it needs it: its project, its modes, and whether
   * it is answering now (busy). null when T3 does not know it (or it is gone).
   */
  function session(threadId) {
    if (v2()) {
      const row = query(
        `SELECT t.project_id, t.runtime_mode, t.interaction_mode,
           EXISTS (SELECT 1 FROM orchestration_v2_projection_runs r WHERE r.thread_id = t.thread_id AND r.status IN ${UNDER_WAY}) AS busy
         FROM orchestration_v2_projection_threads t WHERE t.thread_id = ? AND t.deleted_at IS NULL AND t.archived_at IS NULL`,
        [threadId],
        false,
      );
      return row ? { projectId: row.project_id, runtimeMode: row.runtime_mode, interactionMode: row.interaction_mode, busy: !!row.busy } : null;
    }
    const row = query(
      `SELECT t.project_id, t.runtime_mode, t.interaction_mode, s.status, s.active_turn_id
       FROM projection_threads t LEFT JOIN projection_thread_sessions s ON s.thread_id = t.thread_id
       WHERE t.thread_id = ? AND t.deleted_at IS NULL AND t.archived_at IS NULL`,
      [threadId],
      false,
    );
    if (!row) return null;
    return {
      projectId: row.project_id,
      runtimeMode: row.runtime_mode,
      interactionMode: row.interaction_mode,
      busy: !!row.active_turn_id || row.status === 'running' || row.status === 'starting',
    };
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
    const newer = v2();
    // From 0.0.46: the runs ended since or under way, each read by its own calls.
    const recent = newer
      ? query(
          `SELECT r.run_id AS id, ${CHAT_OF} AS thread FROM orchestration_v2_projection_runs r
           JOIN orchestration_v2_projection_threads t ON t.thread_id = r.thread_id
           WHERE r.completed_at >= ? OR (r.completed_at IS NULL AND r.status IN ${UNDER_WAY})`,
          [since],
        )
      : query('SELECT thread_id AS id, thread_id AS thread FROM projection_threads WHERE updated_at >= ?', [since]);
    for (const { id, thread } of recent) {
      await new Promise(resolve => setImmediate(resolve));
      const rows = newer
        ? query(
            `SELECT coalesce(json_extract(payload_json, '$.startedAt'), updated_at) AS created_at, payload_json
             FROM orchestration_v2_projection_turn_items
             WHERE run_id = ? AND status = 'completed' AND (payload_json LIKE '%proposalId%' OR payload_json LIKE '%tableId%')`,
            [id],
          )
        : query(
            `SELECT created_at, payload_json FROM projection_thread_activities
             WHERE thread_id = ? AND created_at >= ? AND kind = 'tool.completed' AND (payload_json LIKE '%proposalId%' OR payload_json LIKE '%tableId%')`,
            [id, since],
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
    projectOf,
    ownerOf,
    threads,
    chatsOf,
    threadOfToolUse,
    onlyRunning,
    session,
    findProposals,
    open,
    close: shut,
  };
}
