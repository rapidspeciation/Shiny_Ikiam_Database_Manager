import test from 'node:test';
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { existsSync, lstatSync, mkdirSync, mkdtempSync, readFileSync, readlinkSync, rmSync, statSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { DatabaseSync } from 'node:sqlite';

const script = new URL('../scripts/t3-provision.mjs', import.meta.url).pathname;

test('T3 workspaces get the brief and the skills; a refresh after a release keeps each token', () => {
  const home = mkdtempSync(join(tmpdir(), 't3-provision-'));
  try {
    const shared = join(home, 'ithomiini', 'shared');
    mkdirSync(shared, { recursive: true });
    const db = new DatabaseSync(join(shared, 'database.sqlite'));
    db.exec(`CREATE TABLE users(id TEXT PRIMARY KEY, username TEXT UNIQUE NOT NULL, display_name TEXT NOT NULL, role TEXT NOT NULL, active INTEGER NOT NULL DEFAULT 1);
      CREATE TABLE ai_tokens(token_hash TEXT PRIMARY KEY, user_id TEXT NOT NULL, label TEXT NOT NULL, created_at TEXT NOT NULL, revoked_at TEXT);
      INSERT INTO users VALUES ('u1','ana','Ana Pérez','editor',1), ('u2','old','Old','editor',0);`);
    mkdirSync(join(home, '.config', 'ithomiini'), { recursive: true });
    writeFileSync(join(home, '.config', 'ithomiini', 'service.env'), 'APP_PORT=8794\nAPP_BASE_PATH=/\n');
    writeFileSync(join(home, '.claude.json'), '{}');
    // Codex is installed and has another user's settings (e.g. another service's trusted folder).
    mkdirSync(join(home, '.codex'), { recursive: true });
    const codexGlobal = '[projects."/home/other/job"]\ntrust_level = "trusted"\n\n[tui]\nscreen_reader_detection_done = true';
    writeFileSync(join(home, '.codex', 'config.toml'), codexGlobal);
    const env = { ...process.env, HOME: home, CODEX_HOME: join(home, '.codex'), ITHOMIINI_SHARED: shared, T3_BIN: '/bin/true' };
    const run = (...args) => execFileSync(process.execPath, [script, ...args], { env, encoding: 'utf8' });

    run('ana');
    const workspace = join(shared, 't3-workspaces', 'ana');
    const brief = readFileSync(join(workspace, 'AGENTS.md'), 'utf8');
    assert.match(brief, /You work for \*\*Ana Pérez\*\* \(app user `ana`\)/);
    // The brief is assistant/AGENTS.md with the person and the docs folder filled in; the rest is in skills.
    const docs = new URL('../docs', import.meta.url).pathname;
    const template = readFileSync(new URL('../assistant/AGENTS.md', import.meta.url), 'utf8');
    assert.equal(
      brief,
      template
        .replaceAll('{{person}}', 'Ana Pérez')
        .replaceAll('{{username}}', 'ana')
        .replaceAll('{{docs}}', docs.replace(/\/$/, ''))
        .replaceAll('{{knowledge}}', join(shared, 'knowledge'))
        .replaceAll('{{sheets}}', join(shared, 'sheets.sqlite')),
    );
    assert.doesNotMatch(brief, /\{\{|Local test lab/);
    for (const skill of ['data-rules', 'digitalizar-cuaderno', 'monitoring', 'data-review', 'historial', 'google-account', 'app-guide', 'app-dev']) {
      assert.match(brief, new RegExp(`\\| \`${skill}\` \\|`), `the brief names the skill ${skill}`);
      const file = join(workspace, '.claude', 'skills', skill, 'SKILL.md');
      assert.match(readFileSync(file, 'utf8'), new RegExp(`^---\\nname: ${skill}\\ndescription: .+\\n---\\n`), `skill ${skill} installed`);
    }
    // CLAUDE.md is a link to AGENTS.md: Claude Code and Codex read the same brief.
    assert.ok(lstatSync(join(workspace, 'CLAUDE.md')).isSymbolicLink());
    assert.equal(readlinkSync(join(workspace, 'CLAUDE.md')), 'AGENTS.md');
    assert.equal(readFileSync(join(workspace, 'CLAUDE.md'), 'utf8'), brief);
    assert.match(readFileSync(join(workspace, '.claude', 'skills', 'digitalizar-cuaderno', 'SKILL.md'), 'utf8'), /name: digitalizar-cuaderno/);
    // Every folder of assistant/skills is installed, with its reference files; the brief points to the app guide.
    assert.match(readFileSync(join(workspace, '.claude', 'skills', 'app-guide', 'SKILL.md'), 'utf8'), /name: app-guide/);
    assert.ok(existsSync(join(workspace, '.claude', 'skills', 'app-guide', 'reference', 'monitoreo.md')));
    assert.match(brief, /app-guide/);
    // Claude Code subagents (assistant/agents): the notebook readers and reviewers, on their own model.
    assert.match(readFileSync(join(workspace, '.claude', 'agents', 'notebook-reader.md'), 'utf8'), /^name: notebook-reader$/m);
    assert.match(readFileSync(join(workspace, '.claude', 'agents', 'notebook-reviewer.md'), 'utf8'), /^model: claude-sonnet-5-5$/m);
    const mcp = JSON.parse(readFileSync(join(workspace, '.mcp.json'), 'utf8')).mcpServers.ithomiini;
    // The service's base path is /: the endpoint is /api/ai/mcp.
    assert.equal(mcp.url, 'http://127.0.0.1:8794/api/ai/mcp');
    const settings = JSON.parse(readFileSync(join(workspace, '.claude', 'settings.json'), 'utf8'));
    assert.deepEqual(settings.permissions.allow, ['mcp__ithomiini', 'Skill']);
    assert.ok(settings.permissions.deny.includes(`Read(/${join(home, '.config', 'ithomiini')}/**)`));
    // The claude.ai account's connectors (Claude Docs…) are not loaded in the chats.
    assert.equal(settings.env.ENABLE_CLAUDEAI_MCP_SERVERS, 'false');
    assert.ok(JSON.parse(readFileSync(join(home, '.claude.json'), 'utf8')).projects[workspace].hasTrustDialogAccepted);
    // Shell commands naming the secrets or the database are stopped by the guard hook; gog.env may be sourced.
    const [hook] = settings.hooks.PreToolUse;
    assert.equal(hook.matcher, 'Bash');
    const guard = command => {
      try {
        execFileSync('sh', ['-c', hook.hooks[0].command], { input: JSON.stringify({ tool_input: { command } }), env: { ...env, HOME: home }, encoding: 'utf8', stdio: 'pipe' });
        return 'allowed';
      } catch (error) {
        assert.equal(error.status, 2, error.stderr);
        assert.match(error.stderr, /Blocked/);
        return 'blocked';
      }
    };
    assert.equal(guard('grep -iE sheet ~/.config/ithomiini/service.env'), 'blocked');
    assert.equal(guard('ls $HOME/.config/ithomiini'), 'blocked');
    assert.equal(guard(`python3 -c "import sqlite3; sqlite3.connect('${join(shared, 'database.sqlite')}')"`), 'blocked');
    assert.equal(guard('cd ~ && cat .config/ithomiini/gog.env'), 'blocked');
    assert.equal(guard('set -a; . ~/.config/ithomiini/gog.env; set +a; gog --readonly gmail search x'), 'allowed');
    assert.equal(guard('python3 crops.py photo.jpg --out work/2026-09-30-posturas'), 'allowed');

    // Codex (GPT threads): the same brief (AGENTS.md), skills and MCP server with the same token.
    assert.match(readFileSync(join(workspace, '.agents', 'skills', 'digitalizar-cuaderno', 'SKILL.md'), 'utf8'), /name: digitalizar-cuaderno/);
    assert.ok(existsSync(join(workspace, '.agents', 'skills', 'app-guide', 'reference', 'monitoreo.md')));
    const codex = readFileSync(join(workspace, '.codex', 'config.toml'), 'utf8');
    assert.match(codex, /^\[mcp_servers\.ithomiini\]$/m);
    assert.match(codex, /^url = "http:\/\/127\.0\.0\.1:8794\/api\/ai\/mcp"$/m);
    assert.ok(codex.includes(`http_headers = { Authorization = "${mcp.headers.Authorization}" }`));
    assert.match(codex, /^default_tools_approval_mode = "approve"$/m);
    assert.equal(statSync(join(workspace, '.codex', 'config.toml')).mode & 0o777, 0o600);
    // Only the workspace is trusted in the Codex home; the rest of that file is kept as it was.
    const trusted = `${codexGlobal}\n\n[projects."${workspace}"]\ntrust_level = "trusted"\n`;
    assert.equal(readFileSync(join(home, '.codex', 'config.toml'), 'utf8'), trusted);

    // A stale file of an old skill version, a person's own setting, a workspace of a user who left.
    writeFileSync(join(workspace, '.claude', 'skills', 'digitalizar-cuaderno', 'old.md'), 'x');
    writeFileSync(join(workspace, '.claude', 'agents', 'old-agent.md'), 'x');
    writeFileSync(join(workspace, '.claude', 'settings.json'), JSON.stringify({ ...settings, model: 'opus' }));
    mkdirSync(join(shared, 't3-workspaces', 'old'), { recursive: true });
    writeFileSync(join(workspace, 'AGENTS.md'), 'outdated');
    rmSync(join(workspace, 'CLAUDE.md'));
    writeFileSync(join(workspace, 'CLAUDE.md'), 'a copy from before the link');

    const out = run('--refresh-all');
    assert.match(out, /Refreshed: Ithomiini · Ana Pérez/);
    assert.match(out, /Skipped old: no active user/);
    assert.equal(readFileSync(join(workspace, 'AGENTS.md'), 'utf8'), brief);
    assert.equal(readlinkSync(join(workspace, 'CLAUDE.md')), 'AGENTS.md');
    assert.ok(!existsSync(join(workspace, '.claude', 'skills', 'digitalizar-cuaderno', 'old.md')));
    assert.ok(!existsSync(join(workspace, '.claude', 'agents', 'old-agent.md')));
    assert.ok(existsSync(join(workspace, '.claude', 'agents', 'notebook-reader.md')));
    assert.equal(JSON.parse(readFileSync(join(workspace, '.mcp.json'), 'utf8')).mcpServers.ithomiini.headers.Authorization, mcp.headers.Authorization, 'the token is kept');
    assert.equal(JSON.parse(readFileSync(join(workspace, '.claude', 'settings.json'), 'utf8')).model, 'opus');
    assert.equal(JSON.parse(readFileSync(join(workspace, '.claude', 'settings.json'), 'utf8')).hooks.PreToolUse.length, 1, 'the guard once');
    assert.equal(db.prepare('SELECT count(*) n FROM ai_tokens WHERE revoked_at IS NULL').get().n, 1);
    assert.equal(readFileSync(join(workspace, '.codex', 'config.toml'), 'utf8'), codex, 'Codex keeps the token too');
    assert.equal(readFileSync(join(home, '.codex', 'config.toml'), 'utf8'), trusted, 'the workspace is trusted once');

    // Provisioning the person again gives a fresh token (the old one stops working).
    run('ana');
    assert.notEqual(JSON.parse(readFileSync(join(workspace, '.mcp.json'), 'utf8')).mcpServers.ithomiini.headers.Authorization, mcp.headers.Authorization);
    assert.equal(db.prepare('SELECT count(*) n FROM ai_tokens WHERE revoked_at IS NULL').get().n, 1);
    const fresh = JSON.parse(readFileSync(join(workspace, '.mcp.json'), 'utf8')).mcpServers.ithomiini.headers.Authorization;
    assert.ok(readFileSync(join(workspace, '.codex', 'config.toml'), 'utf8').includes(`Authorization = "${fresh}"`), 'Codex gets the new token');
    db.close();
  } finally {
    rmSync(home, { recursive: true, force: true });
  }
});

test('Another install (the local lab) sets the database, MCP address, workspaces and source by env', () => {
  const home = mkdtempSync(join(tmpdir(), 't3-provision-lab-'));
  try {
    const lab = join(home, 'lab');
    mkdirSync(lab, { recursive: true });
    const database = join(lab, 'app.sqlite');
    const db = new DatabaseSync(database);
    db.exec(`CREATE TABLE users(id TEXT PRIMARY KEY, username TEXT UNIQUE NOT NULL, display_name TEXT NOT NULL, role TEXT NOT NULL, active INTEGER NOT NULL DEFAULT 1);
      CREATE TABLE ai_tokens(token_hash TEXT PRIMARY KEY, user_id TEXT NOT NULL, label TEXT NOT NULL, created_at TEXT NOT NULL, revoked_at TEXT);
      INSERT INTO users VALUES ('u1','lab','Lab','admin',1);`);
    const workspaces = join(home, 't3-lab', 'workspaces');
    const env = {
      ...process.env,
      HOME: home,
      CODEX_HOME: join(home, '.codex'),
      ITHOMIINI_SHARED: lab,
      DATABASE_PATH: database,
      ITHOMIINI_MCP_URL: 'http://127.0.0.1:8795/api/ai/mcp',
      ITHOMIINI_T3_WORKSPACES: workspaces,
      ITHOMIINI_SRC: '/work/ithomiini',
      ITHOMIINI_DENY_READ: `${lab}:/secret/answers/`,
      ITHOMIINI_LAB_URL: 'http://127.0.0.1:8795/',
      T3_BIN: '/bin/true',
    };
    execFileSync(process.execPath, [script, 'lab'], { env, encoding: 'utf8' });
    const workspace = join(workspaces, 'lab');
    assert.equal(JSON.parse(readFileSync(join(workspace, '.mcp.json'), 'utf8')).mcpServers.ithomiini.url, 'http://127.0.0.1:8795/api/ai/mcp');
    // The lab's brief ends with the lab note, and app-dev opens with it: changes stay local.
    const brief = readFileSync(join(workspace, 'AGENTS.md'), 'utf8');
    assert.match(brief, /## Local test lab\n\nThis is the \*\*local test lab\*\*: the app at http:\/\/127\.0\.0\.1:8795\//);
    assert.match(brief, /git checkout `\/work\/ithomiini`/);
    // The sheets' copy that `query` reads, beside the lab's database.
    assert.ok(brief.includes(`\`${join(lab, 'sheets.sqlite')}\``));
    const appDev = readFileSync(join(workspace, '.claude', 'skills', 'app-dev', 'SKILL.md'), 'utf8');
    assert.match(appDev, /^---\nname: app-dev\n[\s\S]*?\n---\n\n> \*\*Lab copy\.\*\*[^\n]*\n>\n> This is the \*\*local test lab\*\*/);
    assert.match(appDev, /> the checks, restart the lab app/);
    const deny = JSON.parse(readFileSync(join(workspace, '.claude', 'settings.json'), 'utf8')).permissions.deny;
    assert.ok(deny.includes(`Read(/${database}*)`));
    assert.ok(deny.includes(`Read(/${lab}/**)`) && deny.includes('Read(//secret/answers/**)'));
    assert.ok(deny.includes('Bash(scripts/deploy.sh:*)'), 'the lab never deploys');
    assert.equal(db.prepare('SELECT count(*) n FROM ai_tokens WHERE revoked_at IS NULL').get().n, 1);
    db.close();
  } finally {
    rmSync(home, { recursive: true, force: true });
  }
});
