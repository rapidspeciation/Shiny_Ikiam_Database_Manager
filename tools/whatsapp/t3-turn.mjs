#!/usr/bin/env node
// One turn in a T3 Code chat, for the WhatsApp bridge (tools/whatsapp/bridge.mjs): reads a JSON
// request on stdin, sends the message the way the app does (server/t3commands.mjs, with an admin
// token kept in a file), waits for the chat's run to end and prints the chat's last message as JSON.
// It runs on the machine of the T3 it talks to: on this PC directly, on claudeclaw over ssh
// (a copy in ~/ithomiini/work/wa-bridge/, with t3-progress.mjs beside it). Only node's own modules, ./t3-progress.mjs
// and the app's t3commands.mjs (path in the request), so it runs beside any release.
//
// Request: { op: 'turn' | 'busy' | 'thread' | 'archive' | 'stop', home, local, tokenFile, commandsModule,
//   threadId?, text?, timeoutMs?, instanceId?, steer?, stopAfterTools?, graceMs?, newThreadId?, progress?,
//   personalProgress?, create?: { projectId, title, model, effort } }
// - instanceId: the T3 provider instance the chat runs on (default 'claudeAgent'). WhatsApp chats use
//   their own Claude instance so T3's per-instance settings (compacting an idle chat just before its
//   prompt cache expires) reach them and no other chat. A chat on another instance is moved to it with
//   its next message, keeping its model and options; T3 resumes the same Claude session.
// - op 'turn': the message to `threadId` (made first with `create` when it is missing or gone);
//   waits until the chat is idle, then for its run to end. → { ok, threadId, status, text, ms }
//   With `stopAfterTools: k` (the assistant eval's routing cases) the run is interrupted once its first k tool
//   calls (Bash counted) have settled (result, error or cancelled), so none of them is cut; ok is then true for any end status
//   and `stopped` says it was cut. If the run is still going `graceMs` (default 60 s) after the interrupt, the
//   answer is { ok: false, error: 'interrupt not taken' }. `newThreadId`: the id a chat made here gets (the
//   eval picks it first, so it can stop and clean up a chat whose turn failed).
// - steer: true (op 'turn'): when the chat is answering, the message goes INTO that run (T3's steering;
//   the run's one answer then covers both) and the helper returns at once → { ok, threadId, steered: true }.
//   If the run ended meanwhile, T3 starts (or queues) a run for it instead, and the helper waits for that
//   run like any turn → { ok, threadId, steered: false, status, text, ms }.
// - who (op 'turn'): { token, jid, fromMe, map, database }: the sender's place in the app, read at turn time on this
//   machine. Every `token` in `text` becomes one line part (senderRole below): the app account the private map file
//   (`map`, JSON { "<jid>": "<username>", "self": "<username>" }; "self" is the bridge's own number) links to the JID,
//   and whether that account is an active admin in the app's user records (`database`, read only). Anyone else, or
//   anything that cannot be read, is a guest.
// - op 'whois': { who } → { ok, line } (the same lookup, nothing sent; for checks)
// - op 'busy': → { ok, busy, exists }
// - op 'thread': → { ok, exists, title, projectId }
// - op 'archive': archives `threadId` (an idle chat keeps a Claude process for 30 min) → { ok, threadId }
// - op 'stop': interrupts `threadId`'s runs under way and waits up to `graceMs` for them to end → { ok, stopped, busy }
//
// Run as a script only (`node t3-turn.mjs`, request on stdin). Importing it starts nothing: tests call `turn()` with a
// stand-in database and `send`. As a script it refuses to run under node --test.
import { progressUpdates } from './t3-progress.mjs';
import { DatabaseSync } from 'node:sqlite';
import { randomUUID } from 'node:crypto';
import { existsSync, readFileSync, realpathSync } from 'node:fs';
import { join } from 'node:path';
import { pathToFileURL } from 'node:url';

const UNDER_WAY = ['queued', 'preparing', 'starting', 'running', 'waiting'];
/** A tool call (turn item) still without its result; any other status (completed, failed, cancelled…) is settled. */
const ITEM_UNDER_WAY = ['idle', 'pending', 'running', 'waiting'];
const pause = ms => new Promise(r => setTimeout(r, ms));

async function readStdin() {
  let s = '';
  for await (const chunk of process.stdin) s += chunk;
  return JSON.parse(s);
}

function openDb(home) {
  const file = join(home, 'userdata', 'statev2.sqlite');
  if (!existsSync(file)) throw new Error('No T3 state database (statev2.sqlite) in that home');
  return new DatabaseSync(file, { readOnly: true, timeout: 5000 });
}

const threadRow = (db, id) =>
  db.prepare('SELECT thread_id, project_id, title, deleted_at, archived_at, provider_instance_id, payload_json FROM orchestration_v2_projection_threads WHERE thread_id = ?').get(id);
const busy = (db, id) =>
  Boolean(db.prepare(`SELECT 1 FROM orchestration_v2_projection_runs WHERE thread_id = ? AND status IN (${UNDER_WAY.map(() => '?').join(',')}) LIMIT 1`).get(id, ...UNDER_WAY));
const lastOrdinal = (db, id) => db.prepare('SELECT max(ordinal) n FROM orchestration_v2_projection_runs WHERE thread_id = ?').get(id)?.n ?? 0;

export function isAntigravity(instanceId, model) {
  return /^antigravity(?:$|[-_])/i.test(String(instanceId ?? '')) || /^gemini-/i.test(String(model ?? ''));
}

export function modelSelection({ model, effort = 'medium' } = {}, instanceId = 'claudeAgent') {
  const antigravity = isAntigravity(instanceId, model);
  const resolvedModel = model || (antigravity ? 'gemini-4-neon' : 'claude-opus-5-5');
  return {
    instanceId,
    model: resolvedModel,
    options: antigravity
      ? [{ id: 'thought_level', value: effort }]
      : [
          { id: 'effort', value: effort },
          { id: 'fastMode', value: false },
        ],
  };
}

/** The chat's own model selection on `instanceId`, or null when it is there already (or none is asked). */
function moveTo(row, instanceId) {
  if (!instanceId || !row || row.provider_instance_id === instanceId) return null;
  let current = null;
  try {
    current = JSON.parse(row.payload_json ?? '{}').modelSelection ?? null;
  } catch {}
  const fromAntigravity = isAntigravity(row.provider_instance_id, current?.model);
  const toAntigravity = isAntigravity(instanceId, '');
  if (fromAntigravity !== toAntigravity) return modelSelection({}, instanceId);
  return current?.model ? { ...current, instanceId } : modelSelection({}, instanceId);
}

const bare = jid => String(jid ?? '').replace(/:\d+(?=@)/, '');

/**
 * The sender's place in the app, as a header part: «app account pato (Patricio), admin» or «… : guest». The map file
 * links WhatsApp JIDs to app usernames; the admin flag is the account's live record (role 'admin', active). An unlinked,
 * unknown or inactive sender, or a map or database that cannot be read, is a guest.
 */
export function senderRole(who, { readMap = file => JSON.parse(readFileSync(file, 'utf8')), userOf = defaultUserOf } = {}) {
  let username = null;
  try {
    const map = who?.map ? readMap(who.map) : {};
    const key = who?.fromMe ? 'self' : bare(who?.jid);
    const v = map && typeof map === 'object' && key && Object.hasOwn(map, key) ? map[key] : null;
    username = typeof v === 'string' && v.trim() ? v.trim() : null;
  } catch {
    return 'app roles unreadable: guest';
  }
  if (!username) return 'no linked app account: guest';
  let user = null;
  try {
    user = userOf(who.database, username);
  } catch {
    return `app account ${username} (records unreadable): guest`;
  }
  if (!user || !user.active) return `app account ${username} (not an active account): guest`;
  const display = String(user.display_name ?? '').replace(/[\s«»[\]·,:()]+/g, ' ').trim();
  const name = display ? ` (${[...display].slice(0, 40).join('')})` : '';
  return user.role === 'admin' ? `app account ${username}${name}, admin` : `app account ${username}${name}, ${user.role || 'no role'}: guest`;
}

function defaultUserOf(database, username) {
  if (!database || !existsSync(database)) throw new Error('no app database');
  const db = new DatabaseSync(database, { readOnly: true, timeout: 5000 });
  try {
    return db.prepare('SELECT username, display_name, role, active FROM users WHERE username = ?').get(username) ?? null;
  } finally {
    db.close();
  }
}

/** The request's text with every `who.token` replaced by the sender's place in the app. */
const withSender = (req, people) =>
  req.who?.token ? String(req.text ?? '').split(String(req.who.token)).join(people(req.who)) : String(req.text ?? '');

/**
 * One request (see the top of this file) against the T3 state database `db` (read only), sending T3 commands with
 * `send(...commands)`; `sleep` waits between polls. Returns the answer object.
 */
export async function turn(req, { db, send, sleep = pause, people = senderRole }) {
  if (req.op === 'whois') return { ok: true, line: people(req.who ?? {}) };
  if (req.op === 'busy') {
    const row = req.threadId ? threadRow(db, req.threadId) : null;
    return { ok: true, exists: Boolean(row && !row.deleted_at), busy: row ? busy(db, req.threadId) : false };
  }
  if (req.op === 'thread') {
    const row = req.threadId ? threadRow(db, req.threadId) : null;
    return { ok: true, exists: Boolean(row && !row.deleted_at), title: row?.title ?? null, projectId: row?.project_id ?? null };
  }
  if (req.op === 'archive') {
    const row = req.threadId ? threadRow(db, req.threadId) : null;
    if (!row || row.deleted_at) return { ok: false, threadId: req.threadId ?? null, error: 'no such chat' };
    if (!row.archived_at) await send({ type: 'thread.archive', threadId: req.threadId });
    return { ok: true, threadId: req.threadId };
  }
  if (req.op === 'stop') {
    const runs = db.prepare(`SELECT run_id FROM orchestration_v2_projection_runs WHERE thread_id = ? AND status IN (${UNDER_WAY.map(() => '?').join(',')})`).all(req.threadId ?? '', ...UNDER_WAY);
    for (const r of runs) await send({ type: 'run.interrupt', threadId: req.threadId, runId: r.run_id, reason: 'stopped by the caller' });
    const until = Date.now() + (req.graceMs ?? 60_000);
    while (runs.length && busy(db, req.threadId) && Date.now() < until) await sleep(1000);
    return { ok: true, stopped: runs.length, busy: busy(db, req.threadId ?? '') };
  }
  if (req.op !== 'turn') throw new Error(`Unknown op ${req.op}`);
  const started = Date.now();
  let deadline = started + (req.timeoutMs ?? 15 * 60_000);
  let threadId = req.threadId ?? null;
  const row = threadId ? threadRow(db, threadId) : null;
  const message = { messageId: randomUUID(), role: 'user', text: withSender(req, people), attachments: [] };
  let before = 0;
  let runOf = null;
  if (!row || row.deleted_at) {
    if (!req.create?.projectId) throw new Error('The chat is missing and no project to make it in');
    threadId = req.newThreadId ?? randomUUID();
    const selection = modelSelection(req.create, req.instanceId ?? 'claudeAgent');
    await send(
      { type: 'thread.create', threadId, projectId: req.create.projectId, title: req.create.title ?? 'WhatsApp', modelSelection: selection, runtimeMode: 'full-access', interactionMode: 'default', branch: null, worktreePath: null },
      { type: 'thread.turn.start', threadId, message, modelSelection: selection, runtimeMode: 'full-access', interactionMode: 'default' },
    );
  } else if (req.steer && busy(db, threadId)) {
    // Into the running turn: T3's own message command with deliveryIntent 'steer' (an idle chat starts a run instead).
    await send({ type: 'message.dispatch', createdBy: 'user', creationSource: 'web', threadId, messageId: message.messageId, text: message.text, attachments: [],
      deliveryIntent: 'steer', dispatchMode: { type: 'queue_after_active' } });
    let item = null;
    for (const until = Date.now() + 30_000; !item && Date.now() < until; ) {
      await sleep(500);
      item = db.prepare("SELECT run_id, json_extract(payload_json, '$.inputIntent') intent FROM orchestration_v2_projection_turn_items WHERE thread_id = ? AND type = 'user_message' AND json_extract(payload_json, '$.messageId') = ?").get(threadId, message.messageId) ?? null;
    }
    if (!item) return { ok: false, threadId, error: 'steer not seen' };
    if (/steer/.test(String(item.intent))) return { ok: true, threadId, steered: true };
    runOf = item.run_id; // it started (or queued) a run of its own: wait for that one
  } else {
    // A message while the chat answers would cut in: wait for it to be idle.
    while (busy(db, threadId)) {
      if (Date.now() > deadline) return { ok: false, threadId, error: 'busy' };
      await sleep(2000);
    }
    before = lastOrdinal(db, threadId);
    const move = moveTo(row, req.instanceId);
    await send({ type: 'thread.turn.start', threadId, message, ...(move ? { modelSelection: move } : {}), runtimeMode: 'full-access', interactionMode: 'default' });
  }
  // The run this message started: the first with a higher ordinal; then its end. With `progress`,
  // each tool the chat starts is printed as a line { progress: { tool, title } } meanwhile.
  let run = null;
  const seen = new Map();
  const steps = db.prepare(
    "SELECT ordinal, type, payload_json FROM orchestration_v2_projection_turn_items WHERE run_id = ? AND type IN ('dynamic_tool', 'command_execution', 'subagent', 'assistant_message') ORDER BY ordinal",
  );
  // The run's tool calls in order (Bash counted, the workspace setup not): the first k must have settled.
  const firstCalls = db.prepare(
    "SELECT status FROM orchestration_v2_projection_turn_items WHERE run_id = ? AND (type = 'dynamic_tool' OR (type = 'command_execution' AND coalesce(json_extract(payload_json, '$.title'), '') != 'Workspace ready')) ORDER BY ordinal LIMIT ?",
  );
  const settled = runId => {
    const calls = firstCalls.all(runId, req.stopAfterTools);
    return calls.length >= req.stopAfterTools && calls.every(c => !ITEM_UNDER_WAY.includes(c.status));
  };
  let stopped = false;
  while (Date.now() < deadline) {
    await sleep(req.stopAfterTools ? 500 : 1500);
    run = runOf
      ? db.prepare('SELECT run_id, status FROM orchestration_v2_projection_runs WHERE run_id = ?').get(runOf)
      : db.prepare('SELECT run_id, status FROM orchestration_v2_projection_runs WHERE thread_id = ? AND ordinal > ? ORDER BY ordinal LIMIT 1').get(threadId, before);
    if (run && req.progress)
      for (const progress of progressUpdates(steps.all(run.run_id), seen, { personal: req.personalProgress === true }))
        process.stdout.write(JSON.stringify({ progress }) + '\n');
    if (run && !UNDER_WAY.includes(run.status)) break;
    if (run && req.stopAfterTools && !stopped && settled(run.run_id)) {
      await send({ type: 'run.interrupt', threadId, runId: run.run_id, reason: `stop after ${req.stopAfterTools} tool calls` });
      stopped = true;
      deadline = Math.min(deadline, Date.now() + (req.graceMs ?? 60_000));
    }
  }
  if (!run) return { ok: false, threadId, error: 'no run started' };
  if (UNDER_WAY.includes(run.status)) return { ok: false, threadId, error: stopped ? 'interrupt not taken' : 'timeout', status: run.status };
  const last = db
    .prepare("SELECT payload_json FROM orchestration_v2_projection_messages WHERE run_id = ? AND role = 'assistant' AND streaming = 0 ORDER BY created_at DESC LIMIT 1")
    .get(run.run_id);
  const text = last ? String(JSON.parse(last.payload_json).text ?? '') : '';
  return { ok: run.status === 'completed' || stopped, threadId, ...(req.steer ? { steered: false } : {}), status: run.status, text, ms: Date.now() - started, ...(stopped ? { stopped } : {}) };
}

async function main() {
  const req = await readStdin();
  if (req.op === 'whois') return turn(req, { db: null, send: null });
  const db = openDb(req.home);
  // T3's commands only for the ops that send any (busy and thread only read).
  const send = req.op === 'busy' || req.op === 'thread' ? null : (await import(req.commandsModule)).t3Commands({ local: req.local, tokenFile: req.tokenFile, timeout: 20_000 });
  return turn(req, { db, send });
}

/** Whether this file is the script node was started with (not an import). */
function isScript() {
  try {
    return Boolean(process.argv[1]) && pathToFileURL(realpathSync(process.argv[1])).href === pathToFileURL(realpathSync(new URL(import.meta.url))).href;
  } catch {
    return false;
  }
}

if (isScript()) {
  // Tests never start or touch a real chat (Franz, 9 Nov: no test uses the AI subscriptions); node --test sets NODE_TEST_CONTEXT for its children.
  if (process.env.NODE_TEST_CONTEXT) {
    process.stderr.write('t3-turn.mjs: refusing to run under node --test (NODE_TEST_CONTEXT is set): tests never use the AI subscriptions\n');
    process.stdout.write(JSON.stringify({ ok: false, error: 'refused under node --test' }) + '\n');
    process.exit(3);
  }
  main()
    .then(out => process.stdout.write(JSON.stringify(out) + '\n'))
    .catch(e => {
      process.stdout.write(JSON.stringify({ ok: false, error: String(e.message || e).slice(0, 300) }) + '\n');
      process.exitCode = 1;
    });
}
