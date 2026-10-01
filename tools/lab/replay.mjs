#!/usr/bin/env node
// Replay: what a bench run's model sent to the app (its match_notebook, update_proposal and
// propose_changes calls, as T3 recorded them), sent again to an app running the current server,
// and scored like the bench. It measures a change to the tools without running the models again.
//   node tools/lab/replay.mjs <run> --app http://127.0.0.1:8797 --token <token> --db <app.sqlite> [--cases id,prefix…]
// The app must be a copy (its own database and seed, never the lab's or the team's): replayed
// proposals are drafted there, never applied. <token>: an assistant token of a user of that app
// (ai_tokens). The run's threads come from $LAB/results/<run>/run.json and the lab T3's database.
// Prints the bench's table for the replay (both scorings) beside the run's own.
import { DatabaseSync } from 'node:sqlite';
import { homedir } from 'node:os';
import { join } from 'node:path';
import { groundTruth, labPath, loadCases, loadSnapshot, readJson, scoreCase } from './lib.mjs';

const argv = process.argv.slice(2);
const flag = (name, fallback) => {
  const i = argv.indexOf(name);
  if (i < 0) return fallback;
  const value = argv[i + 1];
  argv.splice(i, 2);
  return value;
};
const app = flag('--app', null);
const token = flag('--token', process.env.REPLAY_TOKEN ?? null);
const dbFile = flag('--db', null);
const only = flag('--cases', '');
const [runId] = argv;
if (!runId || !app || !token || !dbFile) {
  console.error('Usage: replay.mjs <run> --app <url of a copy of the app> --token <assistant token> --db <its app.sqlite> [--cases id,…]');
  process.exit(2);
}
const T3_HOME = process.env.LAB_T3_HOME || join(homedir(), '.t3-ithomiini-lab');
const run = readJson(labPath('results', runId, 'run.json'));
const cases = loadCases().filter(c => run.threads.some(t => t.case === c.id) && (!only || only.split(',').some(p => c.id.startsWith(p))));
const MUTATING = new Set(['match_notebook', 'update_proposal', 'propose_changes']);
const tag = `REPLAY ${runId} ${new Date().toISOString().slice(0, 16)}`;

/** The thread's ithomiini tool calls in order: { name, input, result } (result parsed when it is JSON). */
function calls(t3, threadId) {
  const out = [];
  for (const { p } of t3
    .prepare("SELECT payload_json p FROM projection_thread_activities WHERE thread_id = ? AND kind = 'tool.completed' ORDER BY sequence, created_at")
    .all(threadId)) {
    const data = JSON.parse(p).data ?? {};
    const name = String(data.toolName ?? '').replace(/^mcp__ithomiini__/, '');
    if (name === data.toolName) continue;
    const text = [data.result?.content ?? data.result]
      .flat()
      .map(c => (typeof c === 'string' ? c : (c?.text ?? '')))
      .join('');
    let result = null;
    try {
      result = JSON.parse(text);
    } catch {
      /* not JSON */
    }
    out.push({ name, input: data.input ?? {}, result });
  }
  return out;
}

let id = 0;
async function call(name, args) {
  const response = await fetch(`${app.replace(/\/$/, '')}/api/ai/mcp`, {
    method: 'POST',
    headers: { authorization: `Bearer ${token}`, 'content-type': 'application/json' },
    body: JSON.stringify({ jsonrpc: '2.0', id: ++id, method: 'tools/call', params: { name, arguments: args } }),
  });
  const body = await response.json();
  const text = body.result?.content?.[0]?.text ?? JSON.stringify(body.error ?? body);
  try {
    return JSON.parse(text);
  } catch {
    return { error: text };
  }
}

/** Where each index of a proposal's table points (its sheet row, or a new row's label), from a recorded result. */
function tableOf(result) {
  if (Array.isArray(result?.rows)) return result.rows.map(r => ({ row: r.row ?? null, label: r.label ?? null }));
  // match_notebook: the proposal's rows are its lines in the proposal (or shown for context), in line order.
  if (Array.isArray(result?.lines))
    return result.lines.filter(l => l.inProposal || l.contextRow).map(l => ({ row: l.status === 'new' ? null : (l.row ?? null), label: l.label ?? null }));
  return null;
}
const indexIn = (table, where) =>
  where ? table.findIndex(r => (where.row ? r.row === where.row : r.row === null && r.label === where.label)) : -1;

const t3 = new DatabaseSync(join(T3_HOME, 'userdata', 'state.sqlite'), { readOnly: true });
const snapshot = loadSnapshot();
const lines = [];
const sums = { legacy: {}, v2: {}, before: {} };
const add = (into, n) => {
  for (const [k, v] of Object.entries(n)) into[k] = (into[k] ?? 0) + v;
};
for (const kase of cases) {
  const thread = run.threads.find(t => t.case === kase.id);
  if (!thread?.threadId) continue;
  const ids = new Map(); // the run's proposal id → the replay's
  const oldTables = new Map(); // the run's proposal id → its table then
  const notes = [];
  for (const c of calls(t3, thread.threadId)) {
    const oldId = c.result?.proposalId ?? c.input?.proposalId ?? null;
    if (!MUTATING.has(c.name)) {
      if (oldId && tableOf(c.result)) oldTables.set(oldId, tableOf(c.result));
      continue;
    }
    const args = structuredClone(c.input);
    // Lists and objects T3 recorded as JSON text (the model sent them so; Claude Code parsed them).
    for (const [k, v] of Object.entries(args))
      if (typeof v === 'string' && /^\s*[[{]/.test(v))
        try {
          args[k] = JSON.parse(v);
        } catch {
          /* a text after all */
        }
    if (c.name === 'match_notebook') {
      args.title = `${tag} ${kase.id}${args.title ? ` ${args.title}` : ''}`;
      if (args.replaceProposalId) args.replaceProposalId = ids.get(args.replaceProposalId) ?? undefined;
    } else if (c.name === 'propose_changes') args.reason = `${tag} ${kase.id} ${args.reason ?? ''}`;
    else if (c.name === 'update_proposal') {
      const mine = ids.get(args.proposalId);
      if (!mine) {
        notes.push('update_proposal of a proposal not replayed: skipped');
        continue;
      }
      args.proposalId = mine;
      // Indexes of the run's table → the same rows in the replay's table.
      const before = oldTables.get(c.input.proposalId) ?? [];
      const now = tableOf(await call('get_proposal', { proposalId: mine })) ?? [];
      const map = i => indexIn(now, before[i]);
      const asked = (args.rows ?? []).length;
      args.rows = (args.rows ?? []).map(r => ({ ...r, index: map(r.index) })).filter(r => r.index >= 0);
      args.removeRows = (args.removeRows ?? []).map(map).filter(i => i >= 0);
      if (asked !== args.rows.length) notes.push(`${asked - args.rows.length} rows of an update not found`);
    }
    const out = await call(c.name, args);
    if (out.error) notes.push(`${c.name}: ${String(out.error).slice(0, 120)}`);
    if (oldId && out.proposalId) ids.set(oldId, out.proposalId);
    if (c.result?.proposalId && tableOf(c.result)) oldTables.set(c.result.proposalId, tableOf(c.result));
  }
  const db = new DatabaseSync(dbFile, { readOnly: true });
  const proposals = [...new Set(ids.values())].map(pid => db.prepare('SELECT changes_json FROM ai_proposals WHERE id = ?').get(pid)).filter(Boolean);
  db.close();
  const rows = groundTruth(snapshot, kase);
  const scored = scoreCase(kase, rows, proposals.map(p => ({ changes: JSON.parse(p.changes_json) })));
  add(sums.legacy, scored.legacy);
  add(sums.v2, scored.v2);
  lines.push({ case: kase.id, ...scored, notes });
}
t3.close();

const pct = (a, b) => (b ? `${Math.round((1000 * a) / b) / 10} %` : '–');
console.log(`# Replay of ${runId} with the current server (${tag})\n`);
console.log('| case | legacy | new scoring | flagged right / wrong | wrong unflagged | missing | notes | replay notes |');
console.log('|---|---|---|---|---|---|---|---|');
for (const l of lines)
  console.log(
    `| ${l.case} | ${l.legacy.correct}/${l.legacy.total} (${pct(l.legacy.correct, l.legacy.total)}) | ${l.v2.correct}/${l.v2.total} (${pct(l.v2.correct, l.v2.total)}) | ${l.v2.flaggedRight} / ${l.v2.flaggedWrong} | ${l.v2.wrongUnflagged} | ${l.v2.missing} | ${l.v2.notesMatch}/${l.v2.notesTotal} | ${l.notes.join('; ')} |`,
  );
const L = sums.legacy;
const V = sums.v2;
console.log(`\nLegacy: ${L.correct}/${L.total} (${pct(L.correct, L.total)}); notes ${L.notesMatch}/${L.notesTotal}`);
console.log(
  `New: ${V.correct}/${V.total} (${pct(V.correct, V.total)}), ${V.notOnPage} cells not on the photo left out; doubtful by value ${V.flaggedRight} right, ${V.flaggedWrong} wrong; ${V.wrongUnflagged} wrong unflagged, ${V.missing} missing; notes ${V.notesMatch}/${V.notesTotal}, ${V.extraNotes} extra`,
);
for (const l of lines)
  for (const e of l.errors.filter(e => e.kind !== 'missing' || process.env.REPLAY_ALL)) console.log(`${l.case}\t${e.row}\t${e.field}\t${e.read}\t${e.truth}\t${e.kind}`);
