#!/usr/bin/env node
// Sets up a person's project in T3 Code (stock install, nothing patched):
// a folder with the Ithomiini brief for Claude/Codex, the app's tools over MCP
// with a personal token, and `t3 project add`. Run on the server:
//   node scripts/t3-provision.mjs <username>
// Running it again refreshes the brief and replaces the token.

import { DatabaseSync } from 'node:sqlite';
import { createHash, randomBytes } from 'node:crypto';
import { execFileSync } from 'node:child_process';
import { chmodSync, existsSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const username = process.argv[2];
if (!username) throw new Error('Usage: t3-provision.mjs <username>');
const release = join(dirname(fileURLToPath(import.meta.url)), '..');
const shared = process.env.ITHOMIINI_SHARED || '/home/ubuntu/ithomiini/shared';
const database = process.env.DATABASE_PATH || join(shared, 'database.sqlite');
const mcpUrl = process.env.ITHOMIINI_MCP_URL || 'http://127.0.0.1:8794/ithomiini/api/ai/mcp';
const t3 = process.env.T3_BIN || join(process.env.HOME, '.local/bin/t3');

const db = new DatabaseSync(database, { timeout: 30000 });
const user = db.prepare('SELECT * FROM users WHERE username = ? AND active = 1').get(username);
if (!user) throw new Error(`No active user ${username}`);

const workspace = join(shared, 't3-workspaces', username);
mkdirSync(join(workspace, '.claude'), { recursive: true });

// The brief: the app assistant's CLAUDE.md, with the T3-specific opening.
const base = readFileSync(join(release, 'assistant', 'CLAUDE.md'), 'utf8');
const sheets = base.slice(base.indexOf('## The sheets'));
const brief = `# Ithomiini database assistant (T3 Code)

You help the Ikiam insectary team (Tena, Ecuador) keep their Google Sheets
workbook of Ithomiini butterflies correct. You are working for
**${user.display_name}** (app user \`${user.username}\`). Reply in Spanish.

- The workbook is reached only through the MCP server \`ithomiini\`
  (search_records, find_records, get_record, describe_sheet, check_data,
  queue_wikiloc, get_walk, propose_changes, apply_proposal, run_report,
  search_knowledge). Never edit the workbook any other way. The workbook is a
  **test copy**.
- Proposed edits (and new rows) appear in the app at once, beside this chat:
  **Asistente → Cambios propuestos**, a table with the changed cells in green.
  The person reviews them there; apply with \`apply_proposal\` only when they
  explicitly approve in the chat. The workflow is always: check → propose →
  the person confirms.
- \`check_data\` finds inconsistencies across the workbook with ready fixes;
  \`queue_wikiloc\` + \`get_walk\` turn a Wikiloc monitoring walk into proposed
  Collection_data rows (see the sections below).
- Project documentation (protocols, audit, monitoring, column map) is in
  \`${join(release, 'docs')}\`.
- This folder is your working folder: keep downloads and generated files here.
- The project's Google account (jmithominii@gmail.com) is available with gog:
  \`set -a; . ~/.config/ithomiini/gog.env; set +a; gog --account jmithominii@gmail.com --client ithomiini <command>\`
  (gmail search/get/send, drive, docs, sheets, slides, calendar, forms,
  appscript; \`gog <service> --help\`). Read freely; **send email, create
  events or share files only when the person explicitly asks**, and show them
  the text first.

${sheets}`;
writeFileSync(join(workspace, 'CLAUDE.md'), brief);
writeFileSync(join(workspace, 'AGENTS.md'), brief);

// A fresh personal token (the previous one for T3 stops working).
const token = randomBytes(32).toString('base64url');
db.prepare("UPDATE ai_tokens SET revoked_at = ? WHERE user_id = ? AND label = 't3' AND revoked_at IS NULL").run(
  new Date().toISOString(),
  user.id,
);
db.prepare('INSERT INTO ai_tokens (token_hash,user_id,label,created_at) VALUES (?,?,?,?)').run(
  createHash('sha256').update(token).digest('hex'),
  user.id,
  't3',
  new Date().toISOString(),
);
const mcp = join(workspace, '.mcp.json');
writeFileSync(
  mcp,
  JSON.stringify(
    { mcpServers: { ithomiini: { type: 'http', url: mcpUrl, headers: { Authorization: `Bearer ${token}` } } } },
    null,
    2,
  ),
);
chmodSync(mcp, 0o600);
writeFileSync(
  join(workspace, '.claude', 'settings.json'),
  JSON.stringify({ enableAllProjectMcpServers: true, permissions: { allow: ['mcp__ithomiini'] } }, null, 2),
);

// Claude Code only applies the folder's settings once the folder is trusted.
for (const config of [join(process.env.HOME, '.claude.json'), join(process.env.HOME, '.claude', '.claude.json')]) {
  if (!existsSync(config)) continue;
  const data = JSON.parse(readFileSync(config, 'utf8'));
  data.projects ??= {};
  data.projects[workspace] = { ...data.projects[workspace], hasTrustDialogAccepted: true };
  writeFileSync(config, JSON.stringify(data, null, 2));
}

const title = `Ithomiini · ${user.display_name}`;
const marker = join(workspace, '.claude', 't3-project');
if (!existsSync(marker)) {
  execFileSync(t3, ['project', 'add', workspace, '--title', title], { stdio: 'inherit' });
  writeFileSync(marker, title);
}
console.log(`Ready: ${title} → ${workspace}`);
