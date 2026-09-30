#!/usr/bin/env node
// Claude Code PreToolUse hook of the T3 workspaces (installed by scripts/t3-provision.mjs, which
// writes guard-bash.json beside it): a shell command may not touch the server's secrets, its
// database or the other folders the workspace denies. Read rules only cover the Read, Grep and
// Glob tools; a chat once printed a secret with `grep … service.env`, and another read the
// database with python when a tool's answer was too long. The Google account's settings may only
// be sourced for gog (`. ~/.config/ithomiini/gog.env`), never printed.
import { readFileSync } from 'node:fs';

const config = JSON.parse(readFileSync(new URL('./guard-bash.json', import.meta.url), 'utf8'));
let input = '';
for await (const chunk of process.stdin) input += chunk;
let command = '';
try {
  command = String(JSON.parse(input).tool_input?.command ?? '');
} catch {
  process.exit(0);
}
const home = process.env.HOME ?? '';
let text = command.replace(/\$\{HOME\}|\$HOME|~(?=\/)/g, home);
for (const file of config.sourceOnly ?? [])
  text = text.replace(new RegExp(`(^|[;&|(\\s])(?:\\.|source)\\s+["']?${file.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}["']?(?=$|[;&|)\\s])`, 'g'), '$1');
const hit = (config.deny ?? []).find(path => text.includes(path));
if (hit) {
  process.stderr.write(
    `Blocked: ${hit} is off limits in this workspace (the server's secrets, its database or the lab's answers). ` +
      'Use the ithomiini tools (find_records with fields/filters/limit, count_records); if they cannot answer, tell the person what is missing.\n',
  );
  process.exit(2);
}
