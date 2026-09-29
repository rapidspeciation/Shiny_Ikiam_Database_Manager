#!/usr/bin/env node
// Sets up a person's project in T3 Code (stock install, nothing patched):
// a folder with the Ithomiini brief for Claude/Codex (CLAUDE.md, AGENTS.md),
// the skills (every folder of assistant/skills: digitalizar-cuaderno, app-guide), the app's tools over MCP
// with a personal token, and `t3 project add`. Run on the server:
//   node scripts/t3-provision.mjs <username>     new person, or a fresh token
//   node scripts/t3-provision.mjs --refresh-all  after a release (scripts/deploy.sh):
//     every existing workspace gets the new brief and skills and keeps its token.

import { DatabaseSync } from 'node:sqlite';
import { createHash, randomBytes } from 'node:crypto';
import { execFileSync } from 'node:child_process';
import { chmodSync, cpSync, existsSync, mkdirSync, readFileSync, readdirSync, rmSync, writeFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const release = join(dirname(fileURLToPath(import.meta.url)), '..');
const shared = process.env.ITHOMIINI_SHARED || '/home/ubuntu/ithomiini/shared';
const database = process.env.DATABASE_PATH || join(shared, 'database.sqlite');
/** The app's MCP endpoint, from the service's port and base path (APP_BASE_PATH=/ on claudeclaw). */
function serviceMcpUrl() {
  const env = {};
  try {
    const file = process.env.ITHOMIINI_SERVICE_ENV || join(process.env.HOME, '.config/ithomiini/service.env');
    for (const line of readFileSync(file, 'utf8').split('\n')) {
      const m = /^\s*(APP_PORT|APP_BASE_PATH)\s*=\s*(.*?)\s*$/.exec(line);
      if (m) env[m[1]] = m[2];
    }
  } catch {
    /* No service file: the defaults. */
  }
  const base = '/' + String(env.APP_BASE_PATH ?? '/').replace(/^\/+|\/+$/g, '');
  return `http://127.0.0.1:${env.APP_PORT || 8794}${base === '/' ? '' : base}/api/ai/mcp`;
}
const mcpUrl = process.env.ITHOMIINI_MCP_URL || serviceMcpUrl();
const t3 = process.env.T3_BIN || join(process.env.HOME, '.local/bin/t3');
const workspaces = join(shared, 't3-workspaces');
// The docs of the release that is live (`current`), so the path survives the next release.
const liveDocs = join(dirname(shared), 'current', 'docs');
const docs = existsSync(liveDocs) ? liveDocs : join(release, 'docs');

const arg = process.argv[2];
if (!arg) throw new Error('Usage: t3-provision.mjs <username> | --refresh-all');
const db = new DatabaseSync(database, { timeout: 30000 });
const userOf = username => db.prepare('SELECT * FROM users WHERE username = ? AND active = 1').get(username);

/** The brief: the app assistant's CLAUDE.md, with the T3-specific opening. */
function brief(user) {
  const base = readFileSync(join(release, 'assistant', 'CLAUDE.md'), 'utf8');
  const sheets = base.slice(base.indexOf('## The sheets'));
  return `# Ithomiini database assistant (T3 Code)

You help the Ikiam insectary team (Tena, Ecuador) keep their Google Sheets
workbook of Ithomiini butterflies correct. You are working for
**${user.display_name}** (app user \`${user.username}\`). Reply in Spanish.

- The workbook is reached only through the MCP server \`ithomiini\`
  (search_records, find_records, get_record, describe_sheet, check_data,
  list_agreed_fixes, queue_wikiloc, get_walk, match_notebook, propose_changes,
  update_proposal, get_proposal, apply_proposal, run_report, search_knowledge,
  list_documents, read_document, sync_documents, list_history,
  get_history_group, preview_undo, undo_edits).
  Never edit the workbook any other way. The workbook is a **test copy**.
- Project documents (meeting notes, protocols, reports, presentations of the
  project Drive, mirrored as text): \`search_knowledge\`, \`list_documents\`
  (e.g. the last meeting) and \`read_document\`. The mirror is refreshed only
  on request: \`sync_documents\` when a document is new or was edited. Cite the document's title,
  date and Drive link (\`sourceUrl\`) when you answer from it.
- Proposed edits (and new rows) appear in the app at once, beside this chat:
  **Asistente → Cambios propuestos**, a table with the changed cells in green.
  The person reviews them there and applies them with ✓; apply with
  \`apply_proposal\` only when they explicitly approve in the chat. The workflow
  is always: check → propose → the person confirms. The table is live: when
  the person corrects something, revise the **same** proposal with
  \`update_proposal\` (they see it change); cells they typed in the table are
  theirs (\`get_proposal\` → personEdits; conflicts are reported, never overwrite
  them unless asked).
- **Photos** of notebook pages, envelopes or labels (attached to this chat):
  use the skill \`digitalizar-cuaderno\` (\`.claude/skills/digitalizar-cuaderno/SKILL.md\`;
  read that file if skills are not available): transcribe → \`match_notebook\`
  (one proposal per page, shown beside the chat) → a short summary → apply only
  on confirmation.
- \`check_data\` finds inconsistencies across the workbook with ready fixes
  (also from the specimen photos: envelope vs sheet, the gallery AI); people
  judge them in the app's **Revisión** tab. "Aplica las correcciones
  acordadas" → \`list_agreed_fixes\` → one \`propose_changes\` (with issueIds)
  → the person confirms → \`apply_proposal\` (see "Agreed corrections" below);
  \`queue_wikiloc\` + \`get_walk\` turn a Wikiloc monitoring walk into proposed
  Collection_data rows (see the sections below).
- **Historial** (every save, grouped by person, purpose and time):
  \`list_history\` finds the save someone got wrong; always give its \`url\`
  (opens the Historial tab at that save). Undo only after \`preview_undo\`
  and the person's explicit yes: \`undo_edits\` with \`confirmed: true\`
  (see "Historial" below).
- Project documentation (protocols, audit, monitoring, column map) is in
  \`${docs}\`.
- This folder is your working folder: keep downloads and generated files here.
- The project's Google account (jmithominii@gmail.com) is available with gog:
  \`set -a; . ~/.config/ithomiini/gog.env; set +a; gog --readonly --account jmithominii@gmail.com --client ithomiini <command>\`
  (gmail search/get, drive, docs, sheets, slides, calendar, forms, appscript;
  \`gog <service> --help\`). Always use \`--readonly\` (it blocks every
  change) unless the person explicitly asks for a write; then drop it only
  for that command: **send email, create events, edit or share files only
  when the person explicitly asks**, and show them the text first. Prefer
  the document tools above for Drive documents already mirrored.

${sheets}`;
}

/** A new personal token for T3 (the previous one stops working). */
function mintToken(user) {
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
  return token;
}

/** The token already in the workspace's .mcp.json, if it is still valid. */
function keptToken(workspace, user) {
  try {
    const mcp = JSON.parse(readFileSync(join(workspace, '.mcp.json'), 'utf8'));
    const token = /^Bearer\s+(\S+)$/.exec(mcp.mcpServers?.ithomiini?.headers?.Authorization ?? '')?.[1];
    if (!token) return null;
    const hash = createHash('sha256').update(token).digest('hex');
    const live = db
      .prepare('SELECT 1 FROM ai_tokens WHERE token_hash = ? AND user_id = ? AND revoked_at IS NULL')
      .get(hash, user.id);
    return live ? token : null;
  } catch {
    return null;
  }
}

/**
 * Writes (or refreshes) a person's workspace. Idempotent: the brief, the skills
 * and the settings are rewritten; the token is kept unless `freshToken`.
 */
function provision(user, { freshToken, addProject }) {
  const workspace = join(workspaces, user.username);
  mkdirSync(join(workspace, '.claude', 'skills'), { recursive: true });
  const text = brief(user);
  writeFileSync(join(workspace, 'CLAUDE.md'), text);
  writeFileSync(join(workspace, 'AGENTS.md'), text);

  // The release's skills replace the workspace's copies (a removed file goes too).
  const skills = join(release, 'assistant', 'skills');
  for (const name of existsSync(skills) ? readdirSync(skills) : []) {
    const target = join(workspace, '.claude', 'skills', name);
    rmSync(target, { recursive: true, force: true });
    cpSync(join(skills, name), target, { recursive: true });
  }

  const token = (!freshToken && keptToken(workspace, user)) || mintToken(user);
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

  // The app's tools and the skills run without asking; other settings of the folder are kept.
  const settingsFile = join(workspace, '.claude', 'settings.json');
  let settings = {};
  try {
    settings = JSON.parse(readFileSync(settingsFile, 'utf8'));
  } catch {
    /* A new workspace. */
  }
  settings.enableAllProjectMcpServers = true;
  settings.permissions ??= {};
  settings.permissions.allow = [...new Set([...(settings.permissions.allow ?? []), 'mcp__ithomiini', 'Skill'])];
  writeFileSync(settingsFile, JSON.stringify(settings, null, 2));

  // Claude Code only applies the folder's settings once the folder is trusted.
  for (const config of [join(process.env.HOME, '.claude.json'), join(process.env.HOME, '.claude', '.claude.json')]) {
    if (!existsSync(config)) continue;
    const data = JSON.parse(readFileSync(config, 'utf8'));
    if (data.projects?.[workspace]?.hasTrustDialogAccepted) continue;
    data.projects ??= {};
    data.projects[workspace] = { ...data.projects[workspace], hasTrustDialogAccepted: true };
    writeFileSync(config, JSON.stringify(data, null, 2));
  }

  const title = `Ithomiini · ${user.display_name}`;
  const marker = join(workspace, '.claude', 't3-project');
  if (addProject && !existsSync(marker)) {
    execFileSync(t3, ['project', 'add', workspace, '--title', title], { stdio: 'inherit' });
    writeFileSync(marker, title);
  }
  return `${title} → ${workspace}`;
}

if (arg === '--refresh-all') {
  const names = existsSync(workspaces)
    ? readdirSync(workspaces, { withFileTypes: true }).filter(d => d.isDirectory()).map(d => d.name)
    : [];
  for (const name of names) {
    const user = userOf(name);
    if (!user) {
      console.log(`Skipped ${name}: no active user`);
      continue;
    }
    console.log(`Refreshed: ${provision(user, { freshToken: false, addProject: false })}`);
  }
} else {
  const user = userOf(arg);
  if (!user) throw new Error(`No active user ${arg}`);
  console.log(`Ready: ${provision(user, { freshToken: true, addProject: true })}`);
}
