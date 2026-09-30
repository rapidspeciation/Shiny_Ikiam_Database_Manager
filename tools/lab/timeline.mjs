#!/usr/bin/env node
// A run's threads step by step, to see where the time goes: every tool call but image
// views (counted per stretch), subagents with their model and duration, the turns.
//   node tools/lab/timeline.mjs <run> [case]
import { DatabaseSync } from 'node:sqlite';
import { existsSync, readdirSync, readFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { join } from 'node:path';
import { labPath, readJson } from './lib.mjs';

const T3_HOME = process.env.LAB_T3_HOME || join(homedir(), '.t3-ithomiini-lab');
const [runId, only] = process.argv.slice(2);
if (!runId) throw new Error('Usage: timeline.mjs <run> [case]');
const run = readJson(labPath('results', runId, 'run.json'));
const db = new DatabaseSync(join(T3_HOME, 'userdata', 'state.sqlite'), { readOnly: true });
const transcripts = join(homedir(), '.claude', 'projects', `${T3_HOME}/workspaces/lab`.replace(/[^A-Za-z0-9]/g, '-'));

/** The subagents' model and time span, by the tool call that started them. */
function subagents() {
  const out = new Map();
  if (!existsSync(transcripts)) return out;
  for (const session of readdirSync(transcripts)) {
    const dir = join(transcripts, session, 'subagents');
    if (!existsSync(dir)) continue;
    for (const f of readdirSync(dir).filter(f => f.endsWith('.meta.json'))) {
      const meta = JSON.parse(readFileSync(join(dir, f), 'utf8'));
      const lines = readFileSync(join(dir, f.replace('.meta.json', '.jsonl')), 'utf8').trim().split('\n').map(l => JSON.parse(l));
      const times = lines.map(l => l.timestamp).filter(Boolean).sort();
      const models = [...new Set(lines.map(l => l.message?.model).filter(Boolean))];
      out.set(meta.toolUseId, { type: meta.agentType, models, secs: Math.round((new Date(times.at(-1)) - new Date(times[0])) / 1000) });
    }
  }
  return out;
}
const agents = subagents();

for (const t of run.threads.filter(t => t.threadId && (!only || t.case === only))) {
  const turns = db.prepare('SELECT requested_at, completed_at FROM projection_turns WHERE thread_id = ? ORDER BY row_id').all(t.threadId);
  const t0 = new Date(turns[0]?.requested_at ?? t.sentAt).getTime();
  const at = iso => `${String(Math.round((new Date(iso).getTime() - t0) / 1000)).padStart(4)} s`;
  console.log(`\n## ${t.case} (${t.threadId})`);
  for (const turn of turns) console.log(`turn ${at(turn.requested_at)} → ${turn.completed_at ? at(turn.completed_at) : '…'}`);
  const rows = db
    .prepare("SELECT created_at, kind, payload_json FROM projection_thread_activities WHERE thread_id = ? AND kind IN ('tool.started', 'tool.completed') ORDER BY created_at")
    .all(t.threadId);
  let views = 0;
  const started = new Map();
  for (const r of rows) {
    const p = JSON.parse(r.payload_json);
    const name = (p.data?.toolName ?? '').replace(/^mcp__ithomiini__/, '');
    if (name === 'Read') {
      if (r.kind === 'tool.started') views++;
      continue;
    }
    if (r.kind === 'tool.started') {
      started.set(p.toolCallId, r.created_at);
      continue;
    }
    if (views) console.log(`       (${views} image/file views)`), (views = 0);
    const sub = agents.get(p.toolCallId);
    const extra = sub ? ` [${sub.type} · ${sub.models.join(',')} · ${sub.secs} s]` : '';
    console.log(`${at(started.get(p.toolCallId) ?? r.created_at)} → ${at(r.created_at)}  ${name}${extra}  ${String(p.data?.input?.description ?? '').slice(0, 60)}`);
  }
  if (views) console.log(`       (${views} image/file views)`);
}
