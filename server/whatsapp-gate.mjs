// The WhatsApp gate: one small Haiku call per group message that no rule woke (server/whatsapp.mjs),
// deciding whether the shared WhatsApp chat should see it. It runs `claude -p` with no tools, no MCP,
// no settings or CLAUDE.md and a short system prompt, in an empty folder, and reads back
// {"wake": bool, "reason": "..."}. It only ever sees the lines of whitelisted people in enabled groups:
// the relay drops everyone else before it is called.
import { spawn } from 'node:child_process';
import { mkdtempSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';

export const GATE_MODEL = 'claude-haiku-5-5';

export const GATE_PROMPT = [
  'You read one WhatsApp group message of a butterfly research team (Ithomiini, Ikiam, Ecuador) and decide whether their assistant bot (called bot, asistente or ithobot) should look at it.',
  'The bot knows the team\'s database and documents: CAM and Insectary IDs, specimen rows, counts, clutches, larvae, emergences, monitoring numbers, where files and documents are, the protocol, the rules and dates agreed in meetings, data-entry rules, species identification from photos.',
  'Wake it (true) when the message asks or requests something those sources answer: a fact, a number, an ID, a document\'s place, a rule or protocol step (also how to mark, preserve, record, or what to do when it rains), what a meeting decided, the right value for a column, a species from a photo; or when a CAM or ID in it looks wrong; or when it speaks to the bot.',
  'Do not wake it (false) for: greetings, thanks, jokes, social chat; statements and plans that ask nothing; logistics (keys, transport, food, who goes where today); a question tagged to a person (@name) about their own work, plans or knowledge that is not written down; a request for judgement or a decision (is it useful, how many is enough, keep or release, preserve now or wait); a disagreement; a question a teammate already answered in the recent lines.',
  'Answer with JSON only: {"wake": true|false, "reason": "<at most 8 words>"}',
].join('\n');

/** The user message: the recent lines, then the one to decide on. Pure, for tests. */
export function gateInput({ chatName = null, context = [], line }) {
  const fmt = l => `${l.name || 'someone'}: ${String(l.text ?? '').replace(/\s*\n\s*/g, ' / ').slice(0, 600)}${l.photo ? ' [photo]' : ''}`;
  return [
    `Group: ${chatName ?? 'team group'}`,
    'Recent lines:',
    ...(context.length ? context.map(l => `  ${fmt(l)}`) : ['  (none)']),
    'Message to decide on:',
    `  ${fmt(line)}`,
  ].join('\n');
}

/** { wake, reason } from the model's text, or null when it is not that JSON. Pure, for tests. */
export function parseGate(text) {
  const m = /\{[\s\S]*\}/.exec(String(text ?? ''));
  if (!m) return null;
  try {
    const out = JSON.parse(m[0]);
    if (typeof out.wake !== 'boolean') return null;
    return { wake: out.wake, reason: String(out.reason ?? '').slice(0, 120) };
  } catch {
    return null;
  }
}

/**
 * The gate through `claude -p` (the server's own Claude login). Returns
 * `gate({ chatName, context, line })` → { wake, reason, costUsd, ms } or { error }.
 */
export function haikuGate({ bin = 'claude', model = GATE_MODEL, effort = 'low', timeoutMs = 45_000, spawnImpl = spawn } = {}) {
  const cwd = mkdtempSync(join(tmpdir(), 'wa-gate-'));
  const args = [
    '-p',
    '--model', model,
    '--effort', effort,
    '--tools', '',
    '--system-prompt', GATE_PROMPT,
    '--strict-mcp-config', '--mcp-config', '{"mcpServers":{}}',
    '--setting-sources', '',
    '--disable-slash-commands',
    '--no-session-persistence',
    '--output-format', 'json',
  ];
  return function gate(input) {
    const started = Date.now();
    return new Promise(resolve => {
      let out = '';
      let done = false;
      const finish = value => {
        if (done) return;
        done = true;
        clearTimeout(timer);
        resolve({ ...value, ms: Date.now() - started });
      };
      // The real `claude -p` (no spawnImpl injected) never runs under node --test: tests never use the AI subscriptions.
      if (spawnImpl === spawn && process.env.NODE_TEST_CONTEXT) return resolve({ ms: 0, error: 'gate refusing to run under node --test (NODE_TEST_CONTEXT is set): tests never use the AI subscriptions' });
      let proc;
      try {
        proc = spawnImpl(bin, args, { cwd, stdio: ['pipe', 'pipe', 'ignore'] });
      } catch (e) {
        return finish({ error: `gate could not start (${e.code ?? e.message})` });
      }
      const timer = setTimeout(() => {
        proc.kill('SIGKILL');
        finish({ error: 'gate timed out' });
      }, timeoutMs);
      timer.unref?.();
      proc.stdout.on('data', d => (out += d));
      proc.on('error', e => finish({ error: `gate could not start (${e.code ?? e.message})` }));
      proc.on('close', code => {
        let j = null;
        try {
          j = JSON.parse(out);
        } catch {
          /* not claude's JSON */
        }
        const decided = parseGate(j?.result);
        if (!decided) return finish({ error: `gate gave no decision (exit ${code})`, costUsd: j?.total_cost_usd ?? null });
        finish({ ...decided, costUsd: j?.total_cost_usd ?? null });
      });
      proc.stdin.on('error', () => {});
      proc.stdin.end(gateInput(input));
    });
  };
}
