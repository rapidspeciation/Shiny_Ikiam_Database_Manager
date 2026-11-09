#!/usr/bin/env node
// One turn in a T3 Code chat, for the WhatsApp bridge (tools/whatsapp/bridge.mjs): reads a JSON
// request on stdin, sends the message the way the app does (server/t3commands.mjs, with an admin
// token kept in a file), waits for the chat's run to end and prints the chat's last message as JSON.
// It runs on the machine of the T3 it talks to: on this PC directly, on claudeclaw over ssh
// (the bridge copies it to ~/ithomiini/work/wa-bridge/). Only node's own modules and the app's
// t3commands.mjs (path in the request), so it runs beside any release.
//
// Request: { op: 'turn' | 'busy' | 'thread', home, local, tokenFile, commandsModule,
//   threadId?, text?, timeoutMs?, instanceId?, create?: { projectId, title, model, effort } }
// - instanceId: the T3 provider instance the chat runs on (default 'claudeAgent'). WhatsApp chats use
//   their own Claude instance so T3's per-instance settings (compacting an idle chat just before its
//   prompt cache expires) reach them and no other chat. A chat on another instance is moved to it with
//   its next message, keeping its model and options; T3 resumes the same Claude session.
// - op 'turn': the message to `threadId` (made first with `create` when it is missing or gone);
//   waits until the chat is idle, then for its run to end. → { ok, threadId, status, text, ms }
// - op 'busy': → { ok, busy, exists }
// - op 'thread': → { ok, exists, title, projectId }
// - op 'archive': archives `threadId` (an idle chat keeps a Claude process for 30 min) → { ok, threadId }
import { progressUpdates } from './t3-progress.mjs';
import { DatabaseSync } from 'node:sqlite';
import { randomUUID } from 'node:crypto';
import { existsSync } from 'node:fs';
import { join } from 'node:path';

const UNDER_WAY = ['queued', 'preparing', 'starting', 'running', 'waiting'];
const sleep = ms => new Promise(r => setTimeout(r, ms));

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

function modelSelection({ model = 'claude-opus-5-5', effort = 'medium' } = {}, instanceId = 'claudeAgent') {
  return {
    instanceId,
    model,
    options: [
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
  return current?.model ? { ...current, instanceId } : modelSelection({}, instanceId);
}

async function main() {
  const req = await readStdin();
  const db = openDb(req.home);
  if (req.op === 'busy') {
    const row = req.threadId ? threadRow(db, req.threadId) : null;
    return { ok: true, exists: Boolean(row && !row.deleted_at), busy: row ? busy(db, req.threadId) : false };
  }
  if (req.op === 'thread') {
    const row = req.threadId ? threadRow(db, req.threadId) : null;
    return { ok: true, exists: Boolean(row && !row.deleted_at), title: row?.title ?? null, projectId: row?.project_id ?? null };
  }
  const { t3Commands } = await import(req.commandsModule);
  const send = t3Commands({ local: req.local, tokenFile: req.tokenFile, timeout: 20_000 });
  if (req.op === 'archive') {
    const row = req.threadId ? threadRow(db, req.threadId) : null;
    if (!row || row.deleted_at) return { ok: false, threadId: req.threadId ?? null, error: 'no such chat' };
    if (!row.archived_at) await send({ type: 'thread.archive', threadId: req.threadId });
    return { ok: true, threadId: req.threadId };
  }
  if (req.op !== 'turn') throw new Error(`Unknown op ${req.op}`);
  const started = Date.now();
  const deadline = started + (req.timeoutMs ?? 15 * 60_000);
  let threadId = req.threadId ?? null;
  const row = threadId ? threadRow(db, threadId) : null;
  const message = { messageId: randomUUID(), role: 'user', text: String(req.text ?? ''), attachments: [] };
  let before = 0;
  if (!row || row.deleted_at) {
    if (!req.create?.projectId) throw new Error('The chat is missing and no project to make it in');
    threadId = randomUUID();
    const selection = modelSelection(req.create, req.instanceId ?? 'claudeAgent');
    await send(
      { type: 'thread.create', threadId, projectId: req.create.projectId, title: req.create.title ?? 'WhatsApp', modelSelection: selection, runtimeMode: 'full-access', interactionMode: 'default', branch: null, worktreePath: null },
      { type: 'thread.turn.start', threadId, message, modelSelection: selection, runtimeMode: 'full-access', interactionMode: 'default' },
    );
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
  while (Date.now() < deadline) {
    await sleep(1500);
    run = db
      .prepare('SELECT run_id, status FROM orchestration_v2_projection_runs WHERE thread_id = ? AND ordinal > ? ORDER BY ordinal LIMIT 1')
      .get(threadId, before);
    if (run && req.progress)
      for (const progress of progressUpdates(steps.all(run.run_id), seen, { personal: req.personalProgress === true }))
        process.stdout.write(JSON.stringify({ progress }) + '\n');
    if (run && !UNDER_WAY.includes(run.status)) break;
  }
  if (!run) return { ok: false, threadId, error: 'no run started' };
  if (UNDER_WAY.includes(run.status)) return { ok: false, threadId, error: 'timeout', status: run.status };
  const last = db
    .prepare("SELECT payload_json FROM orchestration_v2_projection_messages WHERE run_id = ? AND role = 'assistant' AND streaming = 0 ORDER BY created_at DESC LIMIT 1")
    .get(run.run_id);
  const text = last ? String(JSON.parse(last.payload_json).text ?? '') : '';
  return { ok: run.status === 'completed', threadId, status: run.status, text, ms: Date.now() - started };
}

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
