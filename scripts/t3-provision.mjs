#!/usr/bin/env node
// Sets up a person's project in T3 Code (stock install, nothing patched):
// a folder with the Ithomiini brief (AGENTS.md for Codex, CLAUDE.md a link to it for Claude),
// the skills (every folder of assistant/skills, in .claude/skills and .agents/skills), the Claude Code
// subagents (assistant/agents: notebook-reader, notebook-reviewer on Sonnet), a shell guard hook
// (assistant/hooks: no command may name the secrets or the database), the app's tools over MCP
// with a personal token (.mcp.json for Claude, .codex/config.toml for Codex), and `t3 project add`. Run on the server:
//   node scripts/t3-provision.mjs <username>     new person, or a fresh token
//   node scripts/t3-provision.mjs --refresh-all  after a release (scripts/deploy.sh):
//     every existing workspace gets the new brief, skills and subagents and keeps its token.
// The server's paths are the defaults; another install (the local test lab, tools/lab) sets them:
//   ITHOMIINI_SHARED (shared folder), DATABASE_PATH, ITHOMIINI_MCP_URL (or ITHOMIINI_SERVICE_ENV),
//   ITHOMIINI_T3_WORKSPACES (default <shared>/t3-workspaces), ITHOMIINI_SRC (the source checkout
//   the brief names), ITHOMIINI_CONFIG_DIR (the service's secrets), ITHOMIINI_DOCS, T3_BIN, and
//   ITHOMIINI_DENY_READ (more folders Claude threads may not read, separated by ":"), and
//   ITHOMIINI_LAB_URL (the lab app's address: changes to the app stay local, no push or deploy).

import { DatabaseSync } from 'node:sqlite';
import { createHash, randomBytes } from 'node:crypto';
import { execFileSync } from 'node:child_process';
import { chmodSync, cpSync, existsSync, mkdirSync, readFileSync, readdirSync, rmSync, symlinkSync, writeFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { composeBrief, labAppDev } from '../server/brief.mjs';

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
const workspaces = process.env.ITHOMIINI_T3_WORKSPACES || join(shared, 't3-workspaces');
/** The install's root (releases/, current/ and src/ beside shared/) and the service's secrets. */
const root = dirname(shared);
const source = process.env.ITHOMIINI_SRC || join(root, 'src');
const configDir = process.env.ITHOMIINI_CONFIG_DIR || join(process.env.HOME, '.config', 'ithomiini');
const extraDeny = (process.env.ITHOMIINI_DENY_READ || '').split(':').filter(Boolean);
// The local test lab (tools/lab/app.sh): an offline copy of the app on this address.
const labUrl = process.env.ITHOMIINI_LAB_URL || '';
// The docs of the release that is live (`current`), so the path survives the next release.
const liveDocs = join(root, 'current', 'docs');
const docs = process.env.ITHOMIINI_DOCS || (existsSync(liveDocs) ? liveDocs : join(release, 'docs'));

const arg = process.argv[2];
if (!arg) throw new Error('Usage: t3-provision.mjs <username> | --refresh-all');
const db = new DatabaseSync(database, { timeout: 30000 });
const userOf = username => db.prepare('SELECT * FROM users WHERE username = ? AND active = 1').get(username);

/** The brief: who the person is, assistant/AGENTS.md, and this workspace's folders (server/brief.mjs). */
const brief = user => composeBrief(user, { docs, source, releases: join(root, 'releases'), labUrl, root: release });

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

/** The PATH of the assistant's shell commands (Claude and Codex threads). */
const TOOLS_PATH = [
  process.execPath.replace(/\/node$/, ''),
  join(process.env.HOME, '.local', 'bin'),
  '/usr/local/bin',
  '/usr/bin',
  '/bin',
].join(':');

/**
 * The app's tools for Codex (GPT threads in T3): Codex reads neither .mcp.json nor
 * .claude, so the workspace gets .codex/config.toml with the same server and token,
 * its tools allowed without asking. Codex only loads a project's .codex/config.toml
 * once that exact folder is trusted (a trusted parent folder is not enough), so the
 * workspace alone is marked trusted in the Codex home's config.toml; nothing else of
 * that file changes (other Codex users on the host keep their settings).
 */
function codexConfig(workspace, token) {
  const q = s => JSON.stringify(s); // a TOML basic string
  const folder = join(workspace, '.codex');
  mkdirSync(folder, { recursive: true, mode: 0o700 });
  const file = join(folder, 'config.toml');
  writeFileSync(
    file,
    `# Written by scripts/t3-provision.mjs: the app's tools (MCP) for Codex threads in T3.
[mcp_servers.ithomiini]
url = ${q(mcpUrl)}
http_headers = { Authorization = ${q(`Bearer ${token}`)} }
default_tools_approval_mode = "approve"

[shell_environment_policy]
set = { PATH = ${q(TOOLS_PATH)} }
`,
    { mode: 0o600 },
  );
  chmodSync(file, 0o600);

  const home = process.env.CODEX_HOME || join(process.env.HOME, '.codex');
  if (!existsSync(home)) return; // Codex is not installed for this account.
  const global = join(home, 'config.toml');
  const text = existsSync(global) ? readFileSync(global, 'utf8') : '';
  const table = `[projects.${q(workspace)}]`;
  if (text.split('\n').some(line => line.trim() === table)) return;
  writeFileSync(global, `${text}${text && !text.endsWith('\n') ? '\n' : ''}\n${table}\ntrust_level = "trusted"\n`, {
    mode: 0o600,
  });
}

/**
 * Writes (or refreshes) a person's workspace. Idempotent: the brief, the skills
 * and the settings are rewritten; the token is kept unless `freshToken`.
 */
function provision(user, { freshToken, addProject }) {
  const workspace = join(workspaces, user.username);
  mkdirSync(join(workspace, '.claude', 'skills'), { recursive: true });
  // AGENTS.md is the brief (Codex reads it); CLAUDE.md is a link to it, so Claude Code reads the
  // same text whatever its AGENTS.md setting (it follows the link; a copy could drift).
  writeFileSync(join(workspace, 'AGENTS.md'), brief(user));
  rmSync(join(workspace, 'CLAUDE.md'), { force: true });
  symlinkSync('AGENTS.md', join(workspace, 'CLAUDE.md'));

  // The release's skills replace the workspace's copies (a removed file goes too):
  // .claude/skills for Claude Code, .agents/skills for Codex (its project skills folder).
  const skills = join(release, 'assistant', 'skills');
  for (const name of existsSync(skills) ? readdirSync(skills) : []) {
    for (const folder of ['.claude', '.agents']) {
      const target = join(workspace, folder, 'skills', name);
      rmSync(target, { recursive: true, force: true });
      cpSync(join(skills, name), target, { recursive: true });
      // In the lab, app-dev opens with the lab's steps (they replace pull, push and deploy).
      const skill = join(target, 'SKILL.md');
      if (labUrl && name === 'app-dev' && existsSync(skill)) {
        const text = readFileSync(skill, 'utf8');
        const end = text.indexOf('\n---\n', 4) + 5;
        const note = `\n> **Lab copy.** These steps are for the live server. Here:\n>\n${labAppDev(labUrl, source).replace(/^- .*?but this/, 'This').replace(/^/gm, '> ')}\n`;
        writeFileSync(skill, text.slice(0, end) + note + text.slice(end));
      }
    }
  }

  // Claude Code subagents (assistant/agents/*.md: notebook readers and reviewers on their own
  // model and effort) replace the workspace's .claude/agents. Codex has no subagent files.
  const agents = join(release, 'assistant', 'agents');
  const agentsTarget = join(workspace, '.claude', 'agents');
  rmSync(agentsTarget, { recursive: true, force: true });
  if (existsSync(agents)) cpSync(agents, agentsTarget, { recursive: true });

  // The shell guard (assistant/hooks/guard-bash.mjs): the deny rules below only cover Claude's
  // file tools, so shell commands naming the secrets, the database or a denied folder are stopped
  // by a PreToolUse hook. Sourcing gog.env for gog stays allowed.
  const hooksTarget = join(workspace, '.claude', 'hooks');
  rmSync(hooksTarget, { recursive: true, force: true });
  cpSync(join(release, 'assistant', 'hooks'), hooksTarget, { recursive: true });
  const denied = [configDir, database, ...extraDeny].map(p => p.replace(/\/+$/, ''));
  // Also the last two parts of a deep path (`cd ~ && cat .config/ithomiini/…`).
  const tails = denied.map(p => p.split('/').filter(Boolean)).filter(p => p.length >= 3).map(p => p.slice(-2).join('/'));
  writeFileSync(
    join(hooksTarget, 'guard-bash.json'),
    JSON.stringify({ deny: [...new Set([...denied, ...tails])], sourceOnly: [join(configDir, 'gog.env')] }, null, 2),
  );
  const guard = `'${process.execPath}' '${join(hooksTarget, 'guard-bash.mjs')}'`;

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
  codexConfig(workspace, token);

  // The app's tools and the skills run without asking; other settings of the folder are kept.
  const settingsFile = join(workspace, '.claude', 'settings.json');
  let settings = {};
  try {
    settings = JSON.parse(readFileSync(settingsFile, 'utf8'));
  } catch {
    /* A new workspace. */
  }
  settings.enableAllProjectMcpServers = true;
  // T3's service PATH lacks the tools: node/npm (building and testing the app's source in
  // ~/ithomiini/src), gog (the project Google account), claude and codex.
  settings.env = { ...settings.env, PATH: TOOLS_PATH };
  settings.permissions ??= {};
  settings.permissions.allow = [...new Set([...(settings.permissions.allow ?? []), 'mcp__ithomiini', 'Skill'])];
  // Our guard replaces its earlier copy; other hooks of the folder are kept.
  const own = entry => entry?.hooks?.some(h => String(h.command ?? '').includes('guard-bash.mjs'));
  settings.hooks = {
    ...settings.hooks,
    PreToolUse: [...(settings.hooks?.PreToolUse ?? []).filter(e => !own(e)), { matcher: 'Bash', hooks: [{ type: 'command', command: guard }] }],
  };
  // Secrets and built releases are off limits (data goes through the tools; code through ~/ithomiini/src).
  settings.permissions.deny = [
    ...new Set([
      ...(settings.permissions.deny ?? []),
      `Read(/${join(configDir, 'service.env')})`,
      `Read(/${join(configDir, '*.json')})`,
      `Read(/${configDir}/**)`,
      `Read(/${database}*)`,
      `Edit(/${join(root, 'releases')}/**)`,
      `Edit(/${join(root, 'current')}/**)`,
      `Write(/${join(root, 'releases')}/**)`,
      `Write(/${join(root, 'current')}/**)`,
      ...extraDeny.map(folder => `Read(/${folder.replace(/\/+$/, '')}/**)`),
      // The lab never deploys to the live server.
      ...(labUrl ? ['Bash(scripts/deploy.sh:*)', 'Bash(./scripts/deploy.sh:*)', `Bash(${join(source, 'scripts/deploy.sh')}:*)`] : []),
    ]),
  ];
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
