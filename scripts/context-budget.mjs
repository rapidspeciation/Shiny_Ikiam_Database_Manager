// The size ratchet for the fixed context of the assistant's chats: every model call carries the brief, the
// listed skills, the always-loaded MCP tools and the deferred tool names, so a few lines added to any of them
// are paid on every call of every chat. R15 (6-7 Nov 2026) cut that context from 57.8k to 35.7k tokens; this
// keeps it from growing back unnoticed.
//
// assistant/context-budget.json holds the bytes of each part we own, measured from source, and the Claude Code
// settings that take out what the chats never use (scripts/claude-lean.mjs). tests/context-budget.test.mjs fails
// when a part differs from the file, in either direction, or when a lean setting is lost.
//
//   node scripts/context-budget.mjs                          the table: recorded, measured, change
//   node scripts/context-budget.mjs --write                  record the sizes (a shrink needs no reason)
//   node scripts/context-budget.mjs --write --reason "…"     record a growth, with why it is worth its tokens
//   node scripts/context-budget.mjs --against origin/main    before a deploy: refuses a part larger than on that
//                                                            ref unless its entry carries a new reason
//
// Bytes, not tokens: they are exact and free to compute. The token counts measured on 6 Nov 2026 are kept in the
// file's `measuredTokens` for scale (not checked).
import { execFileSync } from 'node:child_process';
import { existsSync, readdirSync, readFileSync, writeFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');
export const BUDGET_FILE = join(ROOT, 'assistant', 'context-budget.json');
const bytes = s => Buffer.byteLength(s, 'utf8');
/** The name Claude Code gives the app's tools (scripts/t3-provision.mjs, MCP_SERVER). */
const MCP_PREFIX = 'mcp__db__';

/** A skill's name and description from its SKILL.md frontmatter, as Claude Code lists it ("- name: description"). */
export function skillListing(text) {
  const front = /^---\n([\s\S]*?)\n---/.exec(text)?.[1] ?? '';
  const field = key => new RegExp(`^${key}:\\s*(.*)$`, 'm').exec(front)?.[1]?.trim() ?? '';
  return `- ${field('name')}: ${field('description')}`;
}

/** The parts' sizes in bytes, from this checkout's source: { key: bytes }. */
export async function measureParts(root = ROOT) {
  const { composeBrief, GENERIC_PERSON, SERVER_PATHS } = await import(join(root, 'server', 'brief.mjs'));
  const { mcpTools, mcpInitialize } = await import(join(root, 'server', 'assistant.mjs'));
  const parts = {};
  parts.brief = bytes(composeBrief(GENERIC_PERSON, SERVER_PATHS));

  // Project skills: the release's (assistant/skills) and the WhatsApp workspace's bridge skill.
  const skillFiles = readdirSync(join(root, 'assistant', 'skills'))
    .sort()
    .map(n => join(root, 'assistant', 'skills', n, 'SKILL.md'))
    .concat(join(root, 'tools', 'whatsapp', 'bridge-skill', 'SKILL.md'))
    .filter(existsSync);
  let skills = 0;
  for (const f of skillFiles) {
    const line = skillListing(readFileSync(f, 'utf8'));
    const name = /^- ([^:]*):/.exec(line)[1];
    parts[`skill:${name}`] = bytes(line);
    skills += bytes(line) + 1;
  }
  parts['skills (total)'] = skills;

  // MCP tools as Claude Code sends them to the model: name, description and input schema.
  const tools = mcpTools();
  let always = 0;
  for (const t of tools.filter(t => t._meta?.['anthropic/alwaysLoad'])) {
    const n = bytes(JSON.stringify({ name: MCP_PREFIX + t.name, description: t.description, input_schema: t.inputSchema }));
    parts[`mcp:${t.name}`] = n;
    always += n;
  }
  parts['mcp always-loaded (total)'] = always;
  parts['mcp deferred names'] = bytes(tools.filter(t => !t._meta?.['anthropic/alwaysLoad']).map(t => MCP_PREFIX + t.name).join('\n'));
  parts['mcp server instructions'] = bytes(mcpInitialize().instructions ?? '');
  return parts;
}

/**
 * The Claude Code settings that keep the context lean: scripts/claude-lean.mjs's, plus the eval workspace's
 * claudeMdExcludes (tools/assistant-eval/eval-app.mjs; production has no parent CLAUDE.md to exclude).
 */
export async function measureLean(root = ROOT) {
  const { leanClaude } = await import(join(root, 'scripts', 'claude-lean.mjs'));
  const s = leanClaude({});
  const lean = { enableArtifact: s.enableArtifact, disableBundledSkills: s.disableBundledSkills, deny: [...(s.permissions?.deny ?? [])].sort() };
  const evalApp = join(root, 'tools', 'assistant-eval', 'eval-app.mjs');
  const m = existsSync(evalApp) && /settings\.claudeMdExcludes\s*=\s*(\[[^\]]*\])/.exec(readFileSync(evalApp, 'utf8'));
  lean.claudeMdExcludes = m ? JSON.parse(m[1].replaceAll("'", '"')).sort() : [];
  return lean;
}

/** The recorded file, or an empty one. */
export function readBudget(file = BUDGET_FILE) {
  return existsSync(file) ? JSON.parse(readFileSync(file, 'utf8')) : { parts: {}, lean: {} };
}

/** What the file says that the source no longer does: { grown, shrunk, added, removed, leanLost, leanStale }. */
export function compareBudget(budget, parts, lean) {
  const out = { grown: [], shrunk: [], added: [], removed: [], leanLost: [], leanStale: false };
  for (const [key, n] of Object.entries(parts)) {
    const was = budget.parts?.[key]?.bytes;
    if (was === undefined) out.added.push({ key, bytes: n });
    else if (n > was) out.grown.push({ key, was, bytes: n });
    else if (n < was) out.shrunk.push({ key, was, bytes: n });
  }
  for (const key of Object.keys(budget.parts ?? {})) if (!(key in parts)) out.removed.push({ key });
  for (const [key, value] of Object.entries(budget.lean ?? {})) {
    if (Array.isArray(value)) for (const v of value) (lean[key] ?? []).includes(v) || out.leanLost.push(`${key}: ${v}`);
    else if (lean[key] !== value) out.leanLost.push(`${key}: ${JSON.stringify(value)} is now ${JSON.stringify(lean[key])}`);
  }
  out.leanStale = JSON.stringify(budget.lean ?? {}) !== JSON.stringify(lean);
  return out;
}

/** The new file: measured sizes; a part that grew (or is new) takes `reason`, which a growth requires. */
export function updatedBudget(budget, parts, lean, reason = '') {
  const diff = compareBudget(budget, parts, lean);
  const grew = [...diff.grown, ...diff.added].map(d => d.key);
  if (grew.length && !reason) throw new Error(`grew: ${grew.join(', ')}. Say why with --reason "…" (it is paid on every model call).`);
  const next = { ...budget, parts: {}, lean };
  for (const [key, n] of Object.entries(parts)) {
    const old = budget.parts?.[key] ?? {};
    next.parts[key] = grew.includes(key) ? { bytes: n, reason } : { ...old, bytes: n };
  }
  return next;
}

/**
 * The deploy check: `head` (this branch's file) against `base` (the deployed ref's). A part larger than on the
 * base needs a reason of its own, not the base's. Returns the refusals ([] passes).
 */
export function againstBase(base, head) {
  const refusals = [];
  for (const [key, entry] of Object.entries(head.parts ?? {})) {
    const was = base.parts?.[key];
    if (was && entry.bytes <= was.bytes) continue;
    if (!entry.reason || entry.reason === was?.reason) refusals.push(`${key}: ${was?.bytes ?? 0} → ${entry.bytes} bytes without a reason`);
  }
  for (const [key, value] of Object.entries(base.lean ?? {})) {
    const now = head.lean?.[key];
    if (Array.isArray(value) ? !value.every(v => (now ?? []).includes(v)) : now !== value) refusals.push(`lean setting lost: ${key}`);
  }
  return refusals;
}

function table(budget, parts) {
  const rows = Object.entries(parts).map(([key, n]) => {
    const was = budget.parts?.[key]?.bytes;
    const change = was === undefined ? 'new' : n === was ? '' : `${n > was ? '+' : ''}${n - was}`;
    return [key, was ?? '', n, change];
  });
  const w = [0, 1, 2, 3].map(i => Math.max(...rows.map(r => String(r[i]).length), ['part', 'recorded', 'measured', 'change'][i].length));
  const line = r => r.map((c, i) => (i ? String(c).padStart(w[i]) : String(c).padEnd(w[i]))).join('  ');
  return [line(['part', 'recorded', 'measured', 'change']), ...rows.map(line)].join('\n');
}

async function main(argv) {
  const arg = name => {
    const i = argv.indexOf(name);
    return i >= 0 ? argv[i + 1] : undefined;
  };
  const budget = readBudget();
  const parts = await measureParts();
  const lean = await measureLean();
  console.log(table(budget, parts));
  console.log(`\nlean settings: ${JSON.stringify(lean)}`);
  if (argv.includes('--write')) {
    let next;
    try {
      next = updatedBudget(budget, parts, lean, arg('--reason') ?? '');
    } catch (e) {
      console.log(`\nnot written: ${e.message}`);
      return 1;
    }
    writeFileSync(BUDGET_FILE, JSON.stringify(next, null, 2) + '\n');
    console.log(`\nwrote ${BUDGET_FILE}`);
    return 0;
  }
  const diff = compareBudget(budget, parts, lean);
  const stale = diff.grown.length + diff.shrunk.length + diff.added.length + diff.removed.length + diff.leanLost.length || diff.leanStale;
  if (stale) console.log('\nThe file differs from the source: run with --write (and --reason "…" for a growth).');
  const ref = arg('--against');
  if (ref) {
    let base;
    try {
      base = JSON.parse(execFileSync('git', ['-C', ROOT, 'show', `${ref}:assistant/context-budget.json`], { encoding: 'utf8', stdio: ['ignore', 'pipe', 'ignore'] }));
    } catch {
      console.log(`\n${ref} has no assistant/context-budget.json yet: nothing to compare against.`);
      return stale ? 1 : 0;
    }
    const refusals = againstBase(base, budget);
    for (const r of refusals) console.log(`refused: ${r}`);
    if (!refusals.length) console.log(`\nno growth without a reason against ${ref}.`);
    return refusals.length || stale ? 1 : 0;
  }
  return stale ? 1 : 0;
}

if (process.argv[1] === fileURLToPath(import.meta.url)) process.exitCode = await main(process.argv.slice(2));
