#!/usr/bin/env node
// Assistant eval runner (Phase A pilot, 4 Nov 2026). Lab only.
//
//   node tools/assistant-eval/run.mjs <tasks.json> <new out dir> [--only E01,E02] [--parallel 3]
//        [--model claude-opus-5-5] [--effort medium] [--instance claude-ithomiini] [--variant label] [--keep]
//
// Per task:
//  - unit: direct MCP calls to the lab app (no model): ms, answer size, error, checks.
//  - e2e / multi: a new chat in the lab T3 "Lab" project (tools/whatsapp/t3-turn.mjs), one turn per prompt,
//    then metrics read (read-only) from the lab T3 state DB (tokens, tool calls, failed calls, wall time) and
//    the proposals the chat drafted from the lab app DB; graded by the task's checks; the chat is archived.
// Writes <out>/results.jsonl (one line per task) and <out>/summary.md. Never applies a proposal itself.
import { spawn, spawnSync } from 'node:child_process';
import { DatabaseSync } from 'node:sqlite';
import { existsSync, mkdirSync, readFileSync, writeFileSync, appendFileSync, renameSync, rmSync, statSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { mcpFrom } from './mcp.mjs';
import { grade } from './grade.mjs';
import { fixedContext } from './fixed.mjs';

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = join(HERE, '..', '..');
const argv0 = process.argv.slice(2);
const TARGET = argv0.includes('--target') ? argv0[argv0.indexOf('--target') + 1] : 'eval';
const EVAL_WS = '/home/franz/.t3-ithomiini-lab/workspaces-eval/eval';
const LAB = {
  home: '/home/franz/.t3-ithomiini-lab',
  local: 'http://127.0.0.1:3775',
  tokenFile: '/home/franz/.cache/ithomiini-lab/t3-admin-token',
  attachmentsDir: '/home/franz/.t3-ithomiini-lab/userdata/attachments',
  // --target eval (default): the eval's own app (eval-app.mjs, port 8805), reset before every task.
  // --target lab: the shared lab app (8795, Sheets copy) as in the 4 Nov pilot; no reset.
  ...(TARGET === 'eval'
    ? { projectId: readFileSync(join(EVAL_WS, '.claude', 't3-project'), 'utf8').trim(), workspace: EVAL_WS, appDb: '/home/franz/.cache/ithomiini-lab/assistant-eval/evalapp/run-8805/app.sqlite' }
    : { projectId: '2020befa-5e8a-47a9-8689-ece4297faa44', workspace: '/home/franz/.t3-ithomiini-lab/workspaces/lab', appDb: '/home/franz/.cache/ithomiini-lab/app-workbook/app.sqlite' }),
};

/** Back to the frozen state before a task: the eval app's pending proposals discarded (none is ever applied). */
const running = new Map(); // task id → start (ISO) of the chat tasks under way
function resetState({ self, ids = [] } = {}) {
  if (TARGET !== 'eval') return null;
  // Drafts of tasks still running in parallel are kept: only those older than every other running task go.
  const others = [...running].filter(([id]) => id !== self).map(([, t]) => t);
  const before = others.length ? others.sort()[0] : new Date().toISOString();
  const args = [join(HERE, 'eval-app.mjs'), 'reset', '--before', before, ...(ids.length ? ['--ids', ids.join(',')] : [])];
  const r = spawnSync(process.execPath, args, { encoding: 'utf8' });
  if (r.status !== 0) throw new Error(`reset: ${r.stderr}`);
  return JSON.parse(r.stdout.trim());
}

const argv = process.argv.slice(2);
const opt = (name, dflt) => {
  const i = argv.indexOf(`--${name}`);
  return i < 0 ? dflt : argv[i + 1];
};
const [tasksFile, outDir] = argv.filter((a, i) => !a.startsWith('--') && !(argv[i - 1]?.startsWith('--') && !['--keep', '--regrade'].includes(argv[i - 1])));
if (!tasksFile || !outDir) {
  console.error('usage: run.mjs <tasks.json> <new out dir> [--only ids] [--parallel n] [--model m] [--effort e] [--instance i] [--variant v]');
  process.exit(2);
}
if (existsSync(outDir)) throw new Error(`out dir exists (use a fresh one): ${outDir}`);
mkdirSync(outDir, { recursive: true });
const only = opt('only') ? new Set(opt('only').split(',')) : null;
const RUN = {
  model: opt('model', 'claude-opus-5-5'),
  effort: opt('effort', 'medium'),
  instance: opt('instance', 'claude-ithomiini'),
  variant: opt('variant', 'baseline'),
  parallel: Number(opt('parallel', 3)),
  keep: argv.includes('--keep'),
  tag: new Date().toISOString().replace(/[-:]/g, '').slice(0, 13),
};
const TODAY = new Date().toLocaleDateString('en-CA', { timeZone: 'America/Guayaquil' });
const tasks = JSON.parse(readFileSync(tasksFile, 'utf8')).tasks.filter(t => !only || only.has(t.id));

function t3turn(req) {
  return new Promise((resolve, reject) => {
    const child = spawn(process.execPath, [join(ROOT, 'tools/whatsapp/t3-turn.mjs')], { stdio: ['pipe', 'pipe', 'pipe'] });
    let out = '';
    let err = '';
    child.stdout.on('data', d => (out += d));
    child.stderr.on('data', d => (err += d));
    child.on('close', code => {
      try {
        resolve(JSON.parse(out.trim().split('\n').pop()));
      } catch {
        reject(new Error(`t3-turn exit ${code}: ${err.slice(-400) || out.slice(-400)}`));
      }
    });
    child.stdin.end(JSON.stringify({ home: LAB.home, local: LAB.local, tokenFile: LAB.tokenFile, commandsModule: join(ROOT, 'server/t3commands.mjs'), ...req }));
  });
}

function envelope(task, text, first) {
  if (task.channel !== 'wa') return text;
  const att = (first && task.attachments) || [];
  const lines = [`Franz: @claude ${text}`];
  if (att.length) lines.push('Attachments:', ...att.map(f => `  photo ${join(LAB.attachmentsDir, f)} (identify_species files: ${JSON.stringify([f])})`));
  lines.push('(Answer per the whatsapp-bridge skill: your last message is sent as it is.)');
  return lines.join('\n');
}

const DRAFTING = /(propose_changes|update_proposal|match_notebook)$/;

/** Metrics of one chat, read-only from the lab T3 state DB. */
function chatMetrics(threadId) {
  const db = new DatabaseSync(join(LAB.home, 'userdata', 'statev2.sqlite'), { readOnly: true, timeout: 5000 });
  try {
    const runs = db.prepare('SELECT run_id, ordinal, status, requested_at, completed_at FROM orchestration_v2_projection_runs WHERE thread_id = ? ORDER BY ordinal').all(threadId);
    const items = db.prepare('SELECT type, status, run_id, payload_json FROM orchestration_v2_projection_turn_items WHERE thread_id = ? ORDER BY ordinal').all(threadId);
    const lastByRun = new Map();
    const turns = db.prepare('SELECT payload_json FROM orchestration_v2_projection_provider_turns WHERE thread_id = ?').all(threadId).map(r => JSON.parse(r.payload_json));
    const tools = [];
    const toolUseIds = [];
    const uuids = new Set();
    let commands = 0;
    let subagents = 0;
    let errors = 0;
    let finalText = '';
    const assistantTexts = [];
    for (const it of items) {
      const p = JSON.parse(it.payload_json);
      if (it.type === 'dynamic_tool') {
        tools.push({ name: String(p.toolName ?? '?').replace(/^mcp__db__/, ''), failed: it.status === 'failed', input: JSON.stringify(p.input ?? '').slice(0, 300), output: it.status === 'failed' ? String(p.output ?? '').slice(0, 300) : undefined });
        // Proposals are attributed only to this chat's drafting calls (never to ids seen in list/get answers).
        if (DRAFTING.test(String(p.toolName ?? ''))) {
          if (p.nativeItemRef?.nativeId) toolUseIds.push(p.nativeItemRef.nativeId);
          for (const m of JSON.stringify(p.output ?? '').matchAll(/[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}/g)) uuids.add(m[0]);
        }
      }
      else if (it.type === 'command_execution' && p.title !== 'Workspace ready') {
        commands++;
        tools.push({ name: 'Bash', failed: it.status === 'failed', input: String(p.input ?? '').slice(0, 200) });
      } else if (it.type === 'subagent') subagents++;
      else if (it.type === 'error') errors++;
      else if (it.type === 'assistant_message') {
        assistantTexts.push(p.text ?? '');
        lastByRun.set(it.run_id, p.text ?? '');
      }
    }
    // The answer of each run is its last assistant message; the task's final answer is the last run's.
    // Graded: the last message of each run (what WhatsApp sends); finalText = the last run's.
    const turnTexts = runs.map(r => lastByRun.get(r.run_id) ?? '');
    finalText = turnTexts.at(-1) ?? '';
    const usage = { input: 0, cachedInput: 0, cacheCreation: 0, output: 0, scopes: [], subagentUsageMissing: false };
    for (const t of turns) {
      const u = t.turnTokenUsage;
      if (!u) continue;
      usage.input += u.inputTokens ?? 0;
      usage.cachedInput += u.cachedInputTokens ?? 0;
      usage.cacheCreation += u.cacheCreationTokens ?? 0;
      usage.output += u.outputTokens ?? 0;
      usage.scopes.push(u.usageScope ?? '?');
      // T3 records only the main agent's tokens: a run with sub-agents is under-counted.
      if (u.hasSubagents || subagents > 0) usage.subagentUsageMissing = true;
    }
    const ms = runs.reduce((s, r) => s + (r.completed_at ? Date.parse(r.completed_at) - Date.parse(r.requested_at) : 0), 0);
    return { toolUseIds, uuids: [...uuids], runs: runs.length, runStatus: runs.map(r => r.status), ms, usage, tools, toolCalls: tools.length, toolErrors: tools.filter(t => t.failed).length, commands, subagents, errors, finalText, turnTexts, allText: assistantTexts.join('\n\n') };
  } finally {
    db.close();
  }
}

/** The chat's proposals: by T3 thread, by the tool-use id of the call that drafted them, or by an id a tool answered with. */
function proposalsOf(threadId, toolUseIds = [], ids = [], since = '1970') {
  const db = new DatabaseSync(LAB.appDb, { readOnly: true, timeout: 5000 });
  try {
    const q = (col, list) => (list.length ? ` OR ${col} IN (${list.map(() => '?').join(',')})` : '');
    return db
      .prepare(`SELECT id, status, revision, changes_json, applied_at, created_at, updated_at FROM ai_proposals WHERE (t3_thread = ?${q('t3_tool_use', toolUseIds)}${q('id', ids)}) AND COALESCE(updated_at, created_at) >= ? ORDER BY COALESCE(updated_at, created_at)`)
      .all(threadId, ...toolUseIds, ...ids, since)
      .map(r => ({ id: r.id, status: r.status, revision: r.revision, appliedAt: r.applied_at, createdAt: r.created_at, updatedAt: r.updated_at, changes: JSON.parse(r.changes_json || '[]') }));
  } finally {
    db.close();
  }
}

/** A seeded fixture: one cell of the eval app's in-memory sheet set through its local sheet-edit API (LOCAL_MODE only). */
async function fixture({ sheet, row, field, value }) {
  if (TARGET !== 'eval') throw new Error('fixtures need the eval app');
  // sheet-edit.mjs reads the login from $ITHOMIINI_LAB_DIR/credentials.json: the eval app's run folder.
  const r = spawnSync(process.execPath, [join(ROOT, 'tools/lab/sheet-edit.mjs'), sheet, String(row), `${field}=${value ?? ''}`, '--app', 'http://127.0.0.1:8805'], {
    encoding: 'utf8',
    env: { ...process.env, ITHOMIINI_LAB_DIR: '/home/franz/.cache/ithomiini-lab/assistant-eval/evalapp/run-8805' },
  });
  if (r.status !== 0 || !/^200\b/.test(r.stdout.trim())) throw new Error(`fixture ${sheet}:${row} ${field}: ${r.stderr || r.stdout}`);
  // `query` reads a replica that catches up within about a minute: wait until it shows the value.
  const m = mcpFrom(LAB.workspace);
  const want = String(value ?? '');
  for (let i = 0; i < 40; i++) {
    const out = await m.call('query', { sql: `SELECT "${field}" v FROM "${sheet}" WHERE _row = ${Number(row)}` });
    const got = out.text.split('\n')[1] ?? '';
    if (got.trim() === want.trim()) return;
    await new Promise(r => setTimeout(r, 5000));
  }
  throw new Error(`fixture ${sheet}:${row} ${field}: the replica never showed ${want}`);
}

async function runUnit(task) {
  const m = mcpFrom(LAB.workspace);
  const calls = [];
  for (const c of task.calls) {
    const r = await m.call(c.tool, c.args);
    const g = grade({ checks: c.checks ?? [] }, { finalText: r.text, tools: [], proposals: [] });
    const errOk = c.anyError ? true : c.expectError ? r.isError : !r.isError;
    calls.push({ tool: c.tool, ms: r.ms, chars: r.chars, isError: r.isError, ok: errOk && g.score === 1, text: r.text.slice(0, 400) });
  }
  return { id: task.id, tier: task.tier, split: task.split, score: calls.filter(c => c.ok).length / calls.length, calls, ms: calls.reduce((s, c) => s + c.ms, 0), resultChars: calls.reduce((s, c) => s + c.chars, 0) };
}

/** `breakPhotoSheets` tasks (run alone, `mutates`): the eval app's contact-sheet folder replaced by a file, so making a
 * sheet fails (photo_sheet_error) and the host-plant tags fallback is what is left; put back after the task. */
const SHEETS = '/home/franz/.cache/ithomiini-lab/assistant-eval/evalapp/run-8805/photos/maps-cache/photo-sheets';
function photoSheets(on) {
  const isFile = existsSync(SHEETS) && statSync(SHEETS).isFile();
  if (on) {
    if (isFile) rmSync(SHEETS);
    if (existsSync(`${SHEETS}.off`)) renameSync(`${SHEETS}.off`, SHEETS);
  } else if (!isFile) {
    if (existsSync(SHEETS)) renameSync(SHEETS, `${SHEETS}.off`);
    mkdirSync(dirname(SHEETS), { recursive: true });
    writeFileSync(SHEETS, 'contact sheets off for this eval task\n');
  }
}

async function runChat(task) {
  const prompts = task.turns ?? [task.prompt];
  const before = resetState({ self: task.id });
  running.set(task.id, new Date().toISOString());
  for (const f of task.fixtures ?? []) await fixture(f);
  if (task.breakPhotoSheets) photoSheets(false);
  let threadId = null;
  const turnResults = [];
  const t0 = Date.now();
  for (let i = 0; i < prompts.length; i++) {
    const r = await t3turn({
      op: 'turn',
      threadId: threadId ?? undefined,
      text: envelope(task, prompts[i], i === 0),
      timeoutMs: 1_200_000,
      instanceId: RUN.instance,
      create: { projectId: LAB.projectId, title: `EVAL ${RUN.tag} ${RUN.variant} ${task.id}`, model: RUN.model, effort: RUN.effort },
    });
    if (!r.ok) throw new Error(`turn ${i + 1}: ${JSON.stringify(r).slice(0, 300)}`);
    threadId = r.threadId;
    // The person acts in the app between turns (e.g. applies the draft in Cambios propuestos).
    for (const a of (task.betweenTurns ?? []).filter(a => a.after === i + 1)) {
      if (a.action !== 'applyLatest') throw new Error(`unknown betweenTurns action ${a.action}`);
      const met = chatMetrics(threadId);
      const live = proposalsOf(threadId, met.toolUseIds, met.uuids, new Date(t0).toISOString()).filter(p => !['discarded', 'applied'].includes(p.status));
      if (!live.length) throw new Error('applyLatest: the chat drafted no proposal');
      const out = await mcpFrom(LAB.workspace).call('apply_proposal', { proposalId: live.at(-1).id });
      turnResults.push({ applied: live.at(-1).id, isError: out.isError, text: out.text.slice(0, 300) });
    }
    turnResults.push({ ms: r.ms, status: r.status, text: String(r.text ?? '').slice(0, 1500) });
  }
  const metrics = chatMetrics(threadId);
  const proposals = proposalsOf(threadId, metrics.toolUseIds, metrics.uuids, new Date(t0).toISOString());
  const g = grade(task, { ...metrics, proposals }, { today: TODAY });
  if (!RUN.keep) await t3turn({ op: 'archive', threadId }).catch(() => {});
  for (const f of (task.fixtures ?? []).slice().reverse()) await fixture({ ...f, value: f.restore });
  if (task.breakPhotoSheets) photoSheets(true);
  // This chat's own drafts go now; anything older than every running task too.
  running.delete(task.id);
  const after = resetState({ self: task.id, ids: proposals.map(p => p.id) });
  return {
    id: task.id,
    tier: task.tier,
    split: task.split,
    threadId,
    score: g.score,
    checks: g.results,
    wallMs: Date.now() - t0,
    runMs: metrics.ms,
    usage: metrics.usage,
    toolCalls: metrics.toolCalls,
    toolErrors: metrics.toolErrors,
    commands: metrics.commands,
    subagents: metrics.subagents,
    errors: metrics.errors,
    tools: metrics.tools,
    reset: { before, after },
    turns: turnResults,
    proposals: proposals.map(p => ({ id: p.id, status: p.status, revision: p.revision, changes: p.changes.map(c => ({ sheet: c.sheet, row: c.row, label: c.label, values: c.values })) })),
    finalText: metrics.finalText.slice(0, 3000),
    turnTexts: metrics.turnTexts.map(t => t.slice(0, 3000)),
    startedAt: new Date(t0).toISOString(),
    today: TODAY,
  };
}

async function pool(items, n, fn) {
  const out = [];
  let next = 0;
  await Promise.all(
    Array.from({ length: Math.min(n, items.length) }, async () => {
      while (next < items.length) {
        const item = items[next++];
        out.push(await fn(item));
      }
    }),
  );
  return out;
}

if (argv.includes('--regrade')) {
  // outDir here is a NEW dir; --from <old run dir> gives the results to re-grade.
  const from = opt('from');
  const byId = Object.fromEntries(JSON.parse(readFileSync(tasksFile, 'utf8')).tasks.map(t => [t.id, t]));
  const old = readFileSync(join(from, 'results.jsonl'), 'utf8').trim().split('\n').map(l => JSON.parse(l));
  // Results from before 4 Nov 16:00 lack startedAt: the batch start is looser than the chat start, but safe
  // because proposals are attributed only through this chat's drafting calls.
  const runStarted = JSON.parse(readFileSync(join(from, 'run.json'), 'utf8')).startedAt;
  // The day the run happened (Guayaquil), for $TODAY checks, whatever day the regrade runs.
  const runDay = new Date(runStarted).toLocaleDateString('en-CA', { timeZone: 'America/Guayaquil' });
  const fresh = old.map(r => {
    if (!r.threadId) return r;
    const metrics = chatMetrics(r.threadId);
    const proposals = proposalsOf(r.threadId, metrics.toolUseIds, metrics.uuids, r.startedAt ?? runStarted);
    const g = grade(byId[r.id], { ...metrics, proposals }, { today: r.today ?? runDay });
    return { ...r, today: r.today ?? runDay, startedAt: r.startedAt ?? runStarted, score: g.score, checks: g.results, proposals: proposals.map(p => ({ id: p.id, status: p.status, revision: p.revision, changes: p.changes.map(c => ({ sheet: c.sheet, row: c.row, label: c.label, values: c.values })) })) };
  });
  writeFileSync(join(outDir, 'results.jsonl'), fresh.map(r => JSON.stringify(r)).join('\n') + '\n');
  for (const r of fresh) console.log(r.id, r.score?.toFixed?.(2), (r.checks ?? []).filter(c => !c.pass).map(c => JSON.stringify(c.check) + ' ' + (c.detail ?? '')).join(' | '));
  process.exit(0);
}
const fixed = TARGET === 'eval' ? await fixedContext(LAB.workspace).catch(() => null) : null;
writeFileSync(join(outDir, 'run.json'), JSON.stringify({ ...RUN, tasksFile, tasks: tasks.map(t => t.id), startedAt: new Date().toISOString(), fixed }, null, 1));
// Tasks that change the app's sheets (betweenTurns apply) run last and alone; the next run restarts the app.
/** The eval app answers (an OOM kill on 4 Nov made later tasks fall back to Bash: such results are invalid). */
async function appUp() {
  if (TARGET !== 'eval') return true;
  try {
    return (await fetch('http://127.0.0.1:8805/health', { signal: AbortSignal.timeout(5000) })).ok;
  } catch {
    return false;
  }
}
let aborted = null;
const runOne = async task => {
  if (aborted) return { id: task.id, tier: task.tier, split: task.split, skipped: aborted };
  const started = Date.now();
  let res;
  try {
    res = task.tier === 'unit' ? await runUnit(task) : await runChat(task);
  } catch (e) {
    running.delete(task.id);
    // A crash counts as 0 in the set mean, so a variant cannot look cheaper by failing tasks.
    res = { id: task.id, tier: task.tier, split: task.split, score: 0, failed: String(e.message ?? e).slice(0, 500) };
  }
  res.elapsedMs = Date.now() - started;
  if (task.tier !== 'unit' && !(await appUp())) {
    // Not a model failure: the run stops, and this task's result does not count.
    res.invalid = 'eval app down during the task';
    aborted = `eval app down (after ${task.id})`;
  }
  appendFileSync(join(outDir, 'results.jsonl'), JSON.stringify(res) + '\n');
  console.log(`${task.id} score=${res.score?.toFixed?.(2) ?? '-'} ${res.failed ? 'FAILED ' + res.failed.slice(0, 120) : ''} in=${res.usage?.input ?? '-'} out=${res.usage?.output ?? '-'} tools=${res.toolCalls ?? '-'} ms=${res.runMs ?? res.ms ?? '-'}`);
  return res;
};
const results = [...(await pool(tasks.filter(t => !t.mutates), RUN.parallel, runOne)), ...(await pool(tasks.filter(t => t.mutates), 1, runOne))];

// Summary table.
const k = n => (n == null ? '-' : n >= 1e6 ? (n / 1e6).toFixed(2) + 'M' : n >= 1e3 ? Math.round(n / 1e3) + 'k' : String(n));
const rows = results.sort((a, b) => a.id.localeCompare(b.id));
const lines = [
  `# Assistant eval ${RUN.tag} (${RUN.variant}, ${RUN.model}/${RUN.effort}, instance ${RUN.instance})`,
  '',
  '| task | tier | split | score | input tok | cache-write tok | output tok | tool calls | tool errors | Bash | subagents | run time s |',
  '|---|---|---|---|---|---|---|---|---|---|---|---|',
  ...rows.map(r =>
    r.failed || r.skipped
      ? `| ${r.id} | ${r.tier} | ${r.split} | ${r.failed ? 'FAILED: ' + r.failed.slice(0, 80) : 'SKIPPED: ' + r.skipped} |||||||||`
      : r.tier === 'unit'
        ? `| ${r.id} | unit | ${r.split} | ${r.score.toFixed(2)} | - | - | - | ${r.calls.length} | ${r.calls.filter(c => c.isError).length} | - | - | ${(r.ms / 1000).toFixed(1)} |`
        : `| ${r.id} | ${r.tier} | ${r.split} | ${r.score.toFixed(2)} | ${k(r.usage.input)} | ${k(r.usage.cacheCreation)} | ${k(r.usage.output)} | ${r.toolCalls} | ${r.toolErrors} | ${r.commands} | ${r.subagents} | ${(r.runMs / 1000).toFixed(0)} |`,
  ),
];
writeFileSync(join(outDir, 'summary.md'), lines.join('\n') + '\n');
console.log(lines.join('\n'));
