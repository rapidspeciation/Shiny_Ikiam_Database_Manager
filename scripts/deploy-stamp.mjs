#!/usr/bin/env node
// What `t3-provision.mjs --refresh-all` reads and writes, as one hash (scripts/install-release.sh): a deploy runs
// the refresh only when this changed since the last refresh, or when the last one is not recorded.
//   node scripts/deploy-stamp.mjs t3-provision <release dir>
// It covers the release's part (assistant/: brief, voice, skills, subagents; the provision scripts and
// server/brief.mjs; the node that runs it), the server's settings it reads (the service's port and base path, the
// Claude homes of T3's settings, the provision variables), the people (users, without password columns) and their
// T3 tokens (hashes only), and every workspace: its folder name and each file the refresh reads or writes there
// (brief, CLAUDE.md link, skills, subagents, settings, .mcp.json, Codex config, voice) and whether each Claude
// config trusts it. The same paths and defaults as scripts/t3-provision.mjs. Keep the two in step: a new variable,
// input file or workspace file t3-provision.mjs reads or writes goes into ENV, RELEASE_FILES or WORKSPACE_FILES here,
// or a deploy skips a refresh it needed (these lists are a copy, nothing checks them).
import { createHash } from 'node:crypto';
import { existsSync, lstatSync, readdirSync, readFileSync, readlinkSync, realpathSync } from 'node:fs';
import { join } from 'node:path';
import { pathToFileURL } from 'node:url';

const ENV = [
  'ITHOMIINI_SHARED', 'DATABASE_PATH', 'SHEETS_COPY_PATH', 'ITHOMIINI_MCP_URL', 'ITHOMIINI_SERVICE_ENV',
  'ITHOMIINI_T3_WORKSPACES', 'ITHOMIINI_SRC', 'ITHOMIINI_CONFIG_DIR', 'ITHOMIINI_DOCS', 'ITHOMIINI_KNOWLEDGE',
  'ITHOMIINI_DENY_READ', 'ITHOMIINI_LAB_URL', 'ITHOMIINI_CLAUDE_LEAN', 'T3_BIN', 'T3CODE_HOME', 'CODEX_HOME', 'HOME',
];
const RELEASE_FILES = ['scripts/t3-provision.mjs', 'scripts/claude-lean.mjs', 'scripts/lab-guard.mjs', 'server/brief.mjs', 'assistant'];
const WORKSPACE_FILES = [
  'AGENTS.md', 'CLAUDE.md', '.mcp.json', '.codex/config.toml', '.claude/settings.json', '.claude/t3-skills.json',
  '.claude/ithomiini-workspace.json', '.claude/t3-project', '.claude/voice', '.claude/skills', '.agents/skills',
  '.claude/agents', '.claude/hooks',
];

/** Lines for a path: each file's content hash, each link's target, "missing" when absent. */
function tree(root, rel, out) {
  const path = join(root, rel);
  let st;
  try {
    st = lstatSync(path);
  } catch {
    return out.push(`missing ${rel}`);
  }
  if (st.isSymbolicLink()) out.push(`l ${rel} ${readlinkSync(path)}`);
  else if (st.isDirectory()) {
    out.push(`d ${rel}`);
    for (const name of readdirSync(path).sort()) tree(root, `${rel}/${name}`, out);
  } else out.push(`f ${rel} ${st.mode & 0o111 ? 'x' : '-'} ${createHash('sha256').update(readFileSync(path)).digest('hex')}`);
}

const readOr = (path, fallback = '') => {
  try {
    return readFileSync(path, 'utf8');
  } catch {
    return fallback;
  }
};

export async function t3ProvisionStamp(release, env = process.env) {
  const out = [`node ${process.execPath}`];
  for (const name of ENV) out.push(`env ${name}=${env[name] ?? ''}`);
  for (const rel of RELEASE_FILES) tree(release, rel, out);
  const home = env.HOME;
  const shared = env.ITHOMIINI_SHARED || '/home/ubuntu/ithomiini/shared';
  const database = env.DATABASE_PATH || join(shared, 'database.sqlite');
  const workspaces = env.ITHOMIINI_T3_WORKSPACES || join(shared, 't3-workspaces');
  const serviceEnv = env.ITHOMIINI_SERVICE_ENV || join(home, '.config/ithomiini/service.env');
  out.push(...readOr(serviceEnv).split('\n').filter(l => /^\s*(APP_PORT|APP_BASE_PATH)\s*=/.test(l)).map(l => `service ${l.trim()}`));
  const t3Home = env.T3CODE_HOME || join(home, '.t3');
  const configs = [join(home, '.claude.json'), join(home, '.claude', '.claude.json')];
  try {
    const settings = JSON.parse(readFileSync(join(t3Home, 'userdata', 'settings.json'), 'utf8'));
    for (const instance of Object.values(settings.providerInstances ?? {}))
      if (instance?.driver === 'claudeAgent' && typeof instance.config?.homePath === 'string' && instance.config.homePath)
        configs.push(join(instance.config.homePath, '.claude.json'));
  } catch {
    out.push('t3 settings unreadable');
  }
  // The people and their T3 tokens, as the refresh reads them.
  if (existsSync(database)) {
    const { DatabaseSync } = await import('node:sqlite');
    const db = new DatabaseSync(database, { readOnly: true, timeout: 30000 });
    try {
      for (const user of db.prepare('SELECT * FROM users ORDER BY id').all()) {
        delete user.salt;
        delete user.password_hash;
        out.push(`user ${JSON.stringify(user)}`);
      }
      for (const t of db.prepare("SELECT token_hash, user_id, label, revoked_at FROM ai_tokens WHERE label = 't3' OR label LIKE 't3:%' ORDER BY token_hash").all())
        out.push(`token ${JSON.stringify(t)}`);
    } finally {
      db.close();
    }
  } else out.push(`no database ${database}`);
  const names = existsSync(workspaces)
    ? readdirSync(workspaces, { withFileTypes: true }).filter(d => d.isDirectory()).map(d => d.name).sort()
    : [];
  const trust = configs.map(file => {
    try {
      return JSON.parse(readFileSync(file, 'utf8')).projects ?? {};
    } catch {
      return null;
    }
  });
  const codex = readOr(join(env.CODEX_HOME || join(home, '.codex'), 'config.toml'), null);
  for (const name of names) {
    const workspace = join(workspaces, name);
    out.push(`workspace ${name}`);
    for (const rel of WORKSPACE_FILES) tree(workspace, rel, out);
    trust.forEach((projects, i) => out.push(`trust ${i} ${projects === null ? 'none' : Boolean(projects[workspace]?.hasTrustDialogAccepted)}`));
    out.push(`codex ${codex === null ? 'none' : codex.split('\n').some(l => l.trim() === `[projects.${JSON.stringify(workspace)}]`)}`);
  }
  return createHash('sha256').update(out.join('\n')).digest('hex');
}

if (process.argv[1] && import.meta.url === pathToFileURL(realpathSync(process.argv[1])).href) {
  const [what, release] = process.argv.slice(2);
  if (what !== 't3-provision' || !release) {
    console.error('usage: deploy-stamp.mjs t3-provision <release dir>');
    process.exit(2);
  }
  console.log(await t3ProvisionStamp(release));
}
