// After a Claude Code update: are the lean settings (scripts/claude-lean.mjs) still honoured? A release could rename
// them, and the chats would silently carry the Artifact tool (12.5k tokens) and the rest again.
// Reads the stream-json of one bare probe turn in an app workspace, which you run yourself, e.g.
//   claude -p "hola" --output-format stream-json --verbose > probe.jsonl
//   node scripts/context-probe-check.mjs probe.jsonl [--max 37000]
// and checks: no Artifact tool and none of the denied tools in the init message's tool list, and the first model
// call's input (input + cache creation + cache read) at most --max tokens. 37,000 is the top of the band the R15
// deploy smoke used (34 to 37k; 36,574 on 8 Nov 2026 in a T3 app workspace). This script runs no chat.
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { UNUSED_TOOLS } from './claude-lean.mjs';

export const PROBE_MAX_TOKENS = 37000;

/** → { ok, problems, tools, inputTokens } for the lines of a stream-json probe. */
export function checkProbe(lines, { max = PROBE_MAX_TOKENS } = {}) {
  const events = lines
    .map(l => l.trim())
    .filter(Boolean)
    .map(l => {
      try {
        return JSON.parse(l);
      } catch {
        return null;
      }
    })
    .filter(Boolean);
  const problems = [];
  const init = events.find(e => e.type === 'system' && e.subtype === 'init');
  const tools = init?.tools ?? null;
  if (!tools) problems.push('no init message with a tool list (run with --output-format stream-json --verbose)');
  else {
    const bad = tools.filter(t => /^Artifact/.test(t) || UNUSED_TOOLS.includes(t));
    if (bad.length) problems.push(`tools that the lean settings should remove are listed: ${bad.join(', ')}`);
  }
  const usage = events.find(e => e.type === 'assistant' && e.message?.usage)?.message.usage;
  const inputTokens = usage ? (usage.input_tokens ?? 0) + (usage.cache_creation_input_tokens ?? 0) + (usage.cache_read_input_tokens ?? 0) : null;
  if (inputTokens === null) problems.push('no model call with usage');
  else if (inputTokens > max) problems.push(`first call read ${inputTokens} input tokens, above ${max}`);
  return { ok: !problems.length, problems, tools, inputTokens };
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const [file, ...rest] = process.argv.slice(2);
  const i = rest.indexOf('--max');
  const r = checkProbe(readFileSync(file, 'utf8').split('\n'), { max: i >= 0 ? Number(rest[i + 1]) : PROBE_MAX_TOKENS });
  console.log(`input tokens: ${r.inputTokens ?? '?'}; tools listed: ${r.tools?.length ?? '?'}`);
  for (const p of r.problems) console.log(`problem: ${p}`);
  console.log(r.ok ? 'pass' : 'FAIL');
  process.exitCode = r.ok ? 0 : 1;
}
