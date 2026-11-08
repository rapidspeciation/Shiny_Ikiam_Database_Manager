#!/usr/bin/env node
// Sets up a person's project in T3 Code (stock install, nothing patched):
// a folder with the Ithomiini brief (assistant/AGENTS.md with the person's name filled in, server/brief.mjs;
// AGENTS.md for Codex, CLAUDE.md a link to it for Claude),
// the skills (every folder of assistant/skills, in .claude/skills and .agents/skills), the Claude Code
// subagents (assistant/agents: notebook-reader, notebook-reviewer on Sonnet), the app's tools over MCP
// with a personal token (.mcp.json for Claude, .codex/config.toml for Codex), and `t3 project add`. Run on the server:
//   node scripts/t3-provision.mjs <username>     new person, or a fresh token
//   node scripts/t3-provision.mjs --refresh-all  after a release (scripts/deploy.sh):
//     every existing workspace gets the new brief, skills and subagents and keeps its token.
//   node scripts/t3-provision.mjs <username> --workspace <folder> --title <title>
//     a second workspace of the same person (Franz's WhatsApp chat beside his app chats): its own token
//     (label t3:<folder>), the person's tools and permissions; .claude/ithomiini-workspace.json names the person.
//   node scripts/t3-provision.mjs --refresh <folder>  one workspace (the WhatsApp bridge, after a voice change).
// A WhatsApp chat's workspace has its voice in .claude/voice (voice.json lists the files in order; the bridge's
// `voice-push` writes them): the brief ends with that voice instead of the app's.
// The server's paths are the defaults; another install (the local test lab, tools/lab) sets them:
//   ITHOMIINI_SHARED (shared folder), DATABASE_PATH (and SHEETS_COPY_PATH, else beside it), ITHOMIINI_MCP_URL (or ITHOMIINI_SERVICE_ENV),
//   ITHOMIINI_T3_WORKSPACES (default <shared>/t3-workspaces), ITHOMIINI_SRC (the source checkout
//   the brief names), ITHOMIINI_CONFIG_DIR (the service's secrets), ITHOMIINI_DOCS, T3_BIN, and
//   ITHOMIINI_DENY_READ (more folders, files or file patterns Claude threads may not read, separated by ":"), and
//   ITHOMIINI_LAB_URL (the lab app's address: changes to the app stay local, no push or deploy).

import { DatabaseSync } from 'node:sqlite';
import { createHash, randomBytes } from 'node:crypto';
import { execFileSync } from 'node:child_process';
import { chmodSync, cpSync, existsSync, mkdirSync, readFileSync, readdirSync, rmSync, statSync, symlinkSync, writeFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { composeBrief, labAppDev } from '../server/brief.mjs';
import { applyLabGuard } from './lab-guard.mjs';
import { leanClaude, unleanClaude } from './claude-lean.mjs';
import { overlaidFiles, overlayReader } from '../server/doc-review.mjs';

const release = join(dirname(fileURLToPath(import.meta.url)), '..');
const shared = process.env.ITHOMIINI_SHARED || '/home/ubuntu/ithomiini/shared';
const database = process.env.DATABASE_PATH || join(shared, 'database.sqlite');
/**
 * The Claude Code config files that hold a folder's trust: ~/.claude.json, ~/.claude/.claude.json, and
 * <homePath>/.claude.json of every Claude instance in T3's settings (T3CODE_HOME, ~/.t3 by default).
 */
function claudeConfigs({ home = process.env.HOME, t3Home = process.env.T3CODE_HOME || join(home, '.t3') } = {}) {
  const homes = [];
  try {
    const settings = JSON.parse(readFileSync(join(t3Home, 'userdata', 'settings.json'), 'utf8'));
    for (const instance of Object.values(settings.providerInstances ?? {}))
      if (instance?.driver === 'claudeAgent' && typeof instance.config?.homePath === 'string' && instance.config.homePath)
        homes.push(join(instance.config.homePath, '.claude.json'));
  } catch {
    /* No T3 settings (a fresh install): the default account only. */
  }
  return [...new Set([join(home, '.claude.json'), join(home, '.claude', '.claude.json'), ...homes])];
}

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
// The project's Drive documents as Markdown (the app's KNOWLEDGE_DIR, kept by scripts/drive-sync.mjs).
const knowledge = process.env.ITHOMIINI_KNOWLEDGE || join(shared, 'knowledge');
// The sheets' copy the assistant's `query` reads, beside the database (server/replica.mjs).
const sheets = process.env.SHEETS_COPY_PATH || join(dirname(database), 'sheets.sqlite');

const arg = process.argv[2];
if (!arg) throw new Error('Usage: t3-provision.mjs <username> [--workspace <folder> --title <title>] | --refresh <folder> | --refresh-all');
const flag = name => {
  const i = process.argv.indexOf(name, 3);
  return i > 0 ? process.argv[i + 1] : undefined;
};
const db = new DatabaseSync(database, { timeout: 30000 });
const userOf = username => db.prepare('SELECT * FROM users WHERE username = ? AND active = 1').get(username);
// The instructions as the app has them: the release's files with the changes applied in #/instrucciones
// (server/doc-review.mjs), until a release has them.
const readDoc = overlayReader(db, release);
const overlaid = overlaidFiles(db, release);

/**
 * The voice a WhatsApp chat's workspace carries (.claude/voice/voice.json: { files: [...] } in order), as
 * absolute paths; null for the app's voice. A listed file that is missing is an error: the app's voice
 * would come back in that chat beside its own.
 */
function workspaceVoice(workspace) {
  const folder = join(workspace, '.claude', 'voice');
  if (!existsSync(join(folder, 'voice.json'))) return null;
  const { files } = JSON.parse(readFileSync(join(folder, 'voice.json'), 'utf8'));
  if (!Array.isArray(files) || !files.length) throw new Error(`${folder}/voice.json lists no files`);
  return files.map(f => {
    const path = join(folder, String(f).replace(/^.*\//, ''));
    if (!existsSync(path)) throw new Error(`Voice file missing: ${path}`);
    return path;
  });
}

/** The brief: assistant/AGENTS.md with the person and the docs folder filled in (server/brief.mjs), and the workspace's voice. */
const brief = (user, workspace) => composeBrief(user, { docs, knowledge, sheets, source, labUrl, root: release, voice: workspaceVoice(workspace), read: readDoc });

/** A workspace's own settings (.claude/ithomiini-workspace.json): the person, when the folder is not named after them. */
const workspaceSettings = folder => {
  try {
    return JSON.parse(readFileSync(join(workspaces, folder, '.claude', 'ithomiini-workspace.json'), 'utf8'));
  } catch {
    return {};
  }
};

/** A new personal token for T3 (the previous one of the same label stops working; a second workspace has its own label). */
function mintToken(user, label = 't3') {
  const token = randomBytes(32).toString('base64url');
  db.prepare('UPDATE ai_tokens SET revoked_at = ? WHERE user_id = ? AND label = ? AND revoked_at IS NULL').run(
    new Date().toISOString(),
    user.id,
    label,
  );
  db.prepare('INSERT INTO ai_tokens (token_hash,user_id,label,created_at) VALUES (?,?,?,?)').run(
    createHash('sha256').update(token).digest('hex'),
    user.id,
    label,
    new Date().toISOString(),
  );
  return token;
}

/**
 * The name the app's tools have in a chat: Claude Code calls them mcp__<name>__<tool>
 * (mcp__db__query), so it is short. Workspaces made when it was `ithomiini` keep their token.
 */
const MCP_SERVER = 'db';
const OLD_MCP_SERVER = 'ithomiini';

/** The token already in the workspace's .mcp.json, if it is still valid. */
function keptToken(workspace, user) {
  try {
    const mcp = JSON.parse(readFileSync(join(workspace, '.mcp.json'), 'utf8'));
    const token = /^Bearer\s+(\S+)$/.exec((mcp.mcpServers?.[MCP_SERVER] ?? mcp.mcpServers?.[OLD_MCP_SERVER])?.headers?.Authorization ?? '')?.[1];
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
[mcp_servers.${MCP_SERVER}]
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
function provision(user, { freshToken, addProject, folder = user.username, title = user.display_name }) {
  const workspace = join(workspaces, folder);
  mkdirSync(join(workspace, '.claude', 'skills'), { recursive: true });
  // A second workspace of the person names them (a refresh finds the person here, not in the folder's name).
  if (folder !== user.username)
    writeFileSync(join(workspace, '.claude', 'ithomiini-workspace.json'), JSON.stringify({ ...workspaceSettings(folder), user: user.username, title }, null, 2) + '\n');
  const text = brief(user, workspace);
  // AGENTS.md is the brief (Codex reads it); CLAUDE.md is a link to it, so Claude Code reads the
  // same text whatever its AGENTS.md setting (it follows the link; a copy could drift).
  writeFileSync(join(workspace, 'AGENTS.md'), text);
  rmSync(join(workspace, 'CLAUDE.md'), { force: true });
  symlinkSync('AGENTS.md', join(workspace, 'CLAUDE.md'));

  // The release's skills replace the workspace's copies (a removed file goes too):
  // .claude/skills for Claude Code, .agents/skills for Codex (its project skills folder).
  // A skill an earlier release installed and this one no longer has (renamed, removed) goes as
  // well; skills of the workspace's own (made in a chat) stay. The installed names are kept in
  // .claude/t3-skills.json.
  const skills = join(release, 'assistant', 'skills');
  const names = existsSync(skills) ? readdirSync(skills) : [];
  const installedFile = join(workspace, '.claude', 't3-skills.json');
  let installed = [];
  try {
    installed = JSON.parse(readFileSync(installedFile, 'utf8'));
  } catch {
    /* none recorded yet */
  }
  for (const old of installed.filter(n => !names.includes(n) && /^[\w.-]+$/.test(n)))
    for (const folder of ['.claude', '.agents']) rmSync(join(workspace, folder, 'skills', old), { recursive: true, force: true });
  writeFileSync(installedFile, JSON.stringify(names));
  for (const name of names) {
    for (const folder of ['.claude', '.agents']) {
      const target = join(workspace, folder, 'skills', name);
      rmSync(target, { recursive: true, force: true });
      cpSync(join(skills, name), target, { recursive: true });
      for (const { path, text } of overlaid)
        if (path.startsWith(`assistant/skills/${name}/`)) writeFileSync(join(target, path.slice(`assistant/skills/${name}/`.length)), text);
      // In the lab, app-dev opens with the lab's steps (they replace pull, push and deploy).
      const skill = join(target, 'SKILL.md');
      if (labUrl && name === 'app-dev' && existsSync(skill)) {
        const text = readFileSync(skill, 'utf8');
        const end = text.indexOf('\n---\n', 4) + 5;
        const note = `\n> **Lab copy.** The steps below are for the live server.\n>\n${labAppDev(labUrl, source).replace(/^/gm, '> ')}\n`;
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
  for (const { path, text } of overlaid) if (path.startsWith('assistant/agents/')) writeFileSync(join(agentsTarget, path.slice('assistant/agents/'.length)), text);

  // Chats may write and run their own scripts, the database included (no shell guard): an older
  // release's guard hook is taken out.
  rmSync(join(workspace, '.claude', 'hooks'), { recursive: true, force: true });

  const token = (!freshToken && keptToken(workspace, user)) || mintToken(user, folder === user.username ? 't3' : `t3:${folder}`);
  const mcp = join(workspace, '.mcp.json');
  writeFileSync(
    mcp,
    JSON.stringify(
      { mcpServers: { [MCP_SERVER]: { type: 'http', url: mcpUrl, headers: { Authorization: `Bearer ${token}` } } } },
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
  delete settings.env.ENABLE_CLAUDEAI_MCP_SERVERS;
  // The claude.ai account's connectors (Drive, Gmail, Calendar…) stay out of the chats: the project's
  // documents come from the server's Drive copy, and the connectors add tools to every turn. Claude Code
  // reads the env variable before the folder's `env` applies, so the setting is the switch here.
  settings.disableClaudeAiConnectors = true;
  // No Claude Code auto-memory: what every chat should know lives in the app's documentation (assistant/,
  // the brief and skills), the same for every person. The setting and the variable both switch it off;
  // the Claude homes' own settings.json and the launch wrapper do the same on the server.
  settings.autoMemoryEnabled = false;
  settings.env.CLAUDE_CODE_DISABLE_AUTO_MEMORY = '1';
  settings.permissions ??= {};
  settings.permissions.allow = [...new Set([...(settings.permissions.allow ?? []).filter(rule => rule !== `mcp__${OLD_MCP_SERVER}`), `mcp__${MCP_SERVER}`, 'Skill'])];
  // The old guard hook out; other hooks of the folder are kept.
  const own = entry => entry?.hooks?.some(h => String(h.command ?? '').includes('guard-bash.mjs'));
  const kept = (settings.hooks?.PreToolUse ?? []).filter(e => !own(e));
  if (settings.hooks) {
    if (kept.length) settings.hooks.PreToolUse = kept;
    else delete settings.hooks.PreToolUse;
    if (!Object.keys(settings.hooks).length) delete settings.hooks;
  }
  // Secrets and built releases are off limits (code changes go through ~/ithomiini/src); the database
  // is not (an older release denied it: taken out).
  settings.permissions.deny = [
    ...new Set([
      // the read rules are all written again below (an older lab denied its whole folder: taken out)
      ...(settings.permissions.deny ?? []).filter(rule => !rule.startsWith('Read(')),
      `Read(/${join(configDir, 'service.env')})`,
      `Read(/${join(configDir, '*.json')})`,
      `Read(/${configDir}/**)`,
      `Edit(/${join(root, 'releases')}/**)`,
      `Edit(/${join(root, 'current')}/**)`,
      `Write(/${join(root, 'releases')}/**)`,
      `Write(/${join(root, 'current')}/**)`,
      ...extraDeny.map(path => {
        const clean = path.replace(/\/+$/, '');
        const file = /[*?]/.test(clean) || (existsSync(clean) && !statSync(clean).isDirectory());
        return file ? `Read(/${clean})` : `Read(/${clean}/**)`;
      }),
      // The lab never deploys to the live server.
      ...(labUrl ? ['Bash(scripts/deploy.sh:*)', 'Bash(./scripts/deploy.sh:*)', `Bash(${join(source, 'scripts/deploy.sh')}:*)`] : []),
    ]),
  ];
  // Claude Code features these chats never use, each sent with every model call (scripts/claude-lean.mjs).
  // ITHOMIINI_CLAUDE_LEAN=0 takes them out again (the rollback: then --refresh-all).
  if (process.env.ITHOMIINI_CLAUDE_LEAN === '0') unleanClaude(settings);
  else leanClaude(settings);
  // The lab's chats can neither send WhatsApp nor make timers (scripts/lab-guard.mjs).
  if (labUrl) applyLabGuard(settings, { home: process.env.HOME, source, cache: join(process.env.HOME, '.cache', 'ithomiini-lab') });
  writeFileSync(settingsFile, JSON.stringify(settings, null, 2));

  // Claude Code only applies the folder's settings once the folder is trusted: in the default account's
  // files and in each Claude instance's own home in T3 (its homePath, e.g. ~/.claude-ithomiini, the
  // account the app's chats run on since 4 Nov 2026).
  for (const config of claudeConfigs()) {
    if (!existsSync(config)) continue;
    const data = JSON.parse(readFileSync(config, 'utf8'));
    if (data.projects?.[workspace]?.hasTrustDialogAccepted) continue;
    data.projects ??= {};
    data.projects[workspace] = { ...data.projects[workspace], hasTrustDialogAccepted: true };
    writeFileSync(config, JSON.stringify(data, null, 2));
  }

  const marker = join(workspace, '.claude', 't3-project');
  if (addProject && !existsSync(marker)) {
    execFileSync(t3, ['project', 'add', workspace, '--title', title], { stdio: 'inherit' });
    writeFileSync(marker, title);
  }
  return `${title} → ${workspace}`;
}

/** Refreshes one existing workspace (its person from .claude/ithomiini-workspace.json, else the folder's name). */
function refresh(name) {
  const own = workspaceSettings(name);
  const user = userOf(own.user ?? name);
  if (!user) return console.log(`Skipped ${name}: no active user`);
  console.log(`Refreshed: ${provision(user, { freshToken: false, addProject: false, folder: name, title: own.title ?? user.display_name })}`);
}

if (arg === '--refresh-all') {
  const names = existsSync(workspaces)
    ? readdirSync(workspaces, { withFileTypes: true }).filter(d => d.isDirectory()).map(d => d.name)
    : [];
  let failed = 0;
  for (const name of names) {
    try {
      refresh(name);
    } catch (e) {
      // One workspace's broken voice keeps its last brief and does not stop the others.
      failed++;
      console.error(`Not refreshed ${name}: ${e.message}`);
    }
  }
  if (failed) process.exitCode = 1;
} else if (arg === '--refresh') {
  const name = process.argv[3];
  if (!name || !existsSync(join(workspaces, name))) throw new Error(`No workspace ${name ?? ''}`);
  refresh(name);
} else {
  const user = userOf(arg);
  if (!user) throw new Error(`No active user ${arg}`);
  const folder = flag('--workspace') ?? user.username;
  if (!/^[A-Za-z0-9._-]+$/.test(folder)) throw new Error(`Bad workspace folder ${folder}`);
  console.log(`Ready: ${provision(user, { freshToken: true, addProject: true, folder, title: flag('--title') ?? user.display_name })}`);
}
