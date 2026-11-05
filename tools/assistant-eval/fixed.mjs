// The fixed context of an eval run, hashed by part, so a comparison can say when baseline and variant did not see the
// same brief, skills, settings or tools (the bare probe moved +135 and +730 tokens between app starts on 4 Nov with no
// change of ours). Stored in run.json as `fixed`; compare.mjs prints the parts that differ.
import { createHash } from 'node:crypto';
import { existsSync, readdirSync, readFileSync, statSync } from 'node:fs';
import { join, relative } from 'node:path';
import { mcpFrom } from './mcp.mjs';

const h = s => createHash('sha256').update(s).digest('hex').slice(0, 12);
const files = dir => {
  if (!existsSync(dir)) return [];
  return readdirSync(dir).sort().flatMap(n => {
    const p = join(dir, n);
    return statSync(p).isDirectory() ? files(p) : [p];
  });
};

export async function fixedContext(workspace) {
  const read = p => (existsSync(p) ? readFileSync(p) : '');
  const skillsDir = join(workspace, '.claude', 'skills');
  const skills = h(files(skillsDir).map(p => `${relative(skillsDir, p)}\n${h(readFileSync(p))}`).join('\n'));
  let tools = null;
  let always = null;
  let instructions = null;
  try {
    const m = mcpFrom(workspace);
    instructions = h(String((await m.init().catch(() => ({})))?.instructions ?? ''));
    const list = await m.tools();
    tools = h(JSON.stringify(list));
    always = h(JSON.stringify(list.filter(t => t._meta?.['anthropic/alwaysLoad'])));
  } catch {}
  const parts = { brief: h(read(join(workspace, 'AGENTS.md'))), skills, settings: h(read(join(workspace, '.claude', 'settings.json'))), tools, toolsAlwaysLoaded: always, mcpInstructions: instructions };
  return { ...parts, all: h(JSON.stringify(parts)) };
}

/** The parts that differ between two manifests ([] when equal; null when either is missing). */
export function fixedDiff(a, b) {
  if (!a || !b) return null;
  return Object.keys(a).filter(k => k !== 'all' && a[k] !== b[k]);
}
