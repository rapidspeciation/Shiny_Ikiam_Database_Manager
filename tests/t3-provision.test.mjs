import test from 'node:test';
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync, existsSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { DatabaseSync } from 'node:sqlite';

const script = new URL('../scripts/t3-provision.mjs', import.meta.url).pathname;

test('T3 workspaces get the brief and the skill; a refresh after a release keeps each token', () => {
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
    const env = { ...process.env, HOME: home, ITHOMIINI_SHARED: shared, T3_BIN: '/bin/true' };
    const run = (...args) => execFileSync(process.execPath, [script, ...args], { env, encoding: 'utf8' });

    run('ana');
    const workspace = join(shared, 't3-workspaces', 'ana');
    const brief = readFileSync(join(workspace, 'CLAUDE.md'), 'utf8');
    assert.match(brief, /working for\n\*\*Ana Pérez\*\*/);
    assert.match(brief, /match_notebook/);
    assert.match(brief, /## Photos of notebook pages/);
    assert.match(brief, /## Historial: finding and undoing a save/);
    assert.match(brief, /list_history/);
    assert.equal(readFileSync(join(workspace, 'AGENTS.md'), 'utf8'), brief);
    assert.match(readFileSync(join(workspace, '.claude', 'skills', 'digitalizar-cuaderno', 'SKILL.md'), 'utf8'), /name: digitalizar-cuaderno/);
    const mcp = JSON.parse(readFileSync(join(workspace, '.mcp.json'), 'utf8')).mcpServers.ithomiini;
    // The service's base path is /: the endpoint is /api/ai/mcp.
    assert.equal(mcp.url, 'http://127.0.0.1:8794/api/ai/mcp');
    const settings = JSON.parse(readFileSync(join(workspace, '.claude', 'settings.json'), 'utf8'));
    assert.deepEqual(settings.permissions.allow, ['mcp__ithomiini', 'Skill']);
    assert.ok(JSON.parse(readFileSync(join(home, '.claude.json'), 'utf8')).projects[workspace].hasTrustDialogAccepted);

    // A stale file of an old skill version, a person's own setting, a workspace of a user who left.
    writeFileSync(join(workspace, '.claude', 'skills', 'digitalizar-cuaderno', 'old.md'), 'x');
    writeFileSync(join(workspace, '.claude', 'settings.json'), JSON.stringify({ ...settings, model: 'opus' }));
    mkdirSync(join(shared, 't3-workspaces', 'old'), { recursive: true });
    writeFileSync(join(workspace, 'CLAUDE.md'), 'outdated');

    const out = run('--refresh-all');
    assert.match(out, /Refreshed: Ithomiini · Ana Pérez/);
    assert.match(out, /Skipped old: no active user/);
    assert.equal(readFileSync(join(workspace, 'CLAUDE.md'), 'utf8'), brief);
    assert.ok(!existsSync(join(workspace, '.claude', 'skills', 'digitalizar-cuaderno', 'old.md')));
    assert.equal(JSON.parse(readFileSync(join(workspace, '.mcp.json'), 'utf8')).mcpServers.ithomiini.headers.Authorization, mcp.headers.Authorization, 'the token is kept');
    assert.equal(JSON.parse(readFileSync(join(workspace, '.claude', 'settings.json'), 'utf8')).model, 'opus');
    assert.equal(db.prepare('SELECT count(*) n FROM ai_tokens WHERE revoked_at IS NULL').get().n, 1);

    // Provisioning the person again gives a fresh token (the old one stops working).
    run('ana');
    assert.notEqual(JSON.parse(readFileSync(join(workspace, '.mcp.json'), 'utf8')).mcpServers.ithomiini.headers.Authorization, mcp.headers.Authorization);
    assert.equal(db.prepare('SELECT count(*) n FROM ai_tokens WHERE revoked_at IS NULL').get().n, 1);
    db.close();
  } finally {
    rmSync(home, { recursive: true, force: true });
  }
});
