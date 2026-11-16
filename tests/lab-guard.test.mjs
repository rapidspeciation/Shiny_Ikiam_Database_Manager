import { after, test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdirSync, mkdtempSync, readFileSync, rmSync, statSync, writeFileSync } from 'node:fs';
import { execFileSync } from 'node:child_process';
import { homedir, tmpdir } from 'node:os';
// The hook's copy may not sit under /tmp (the sandbox writes there): a folder of the tests' own cache.
mkdirSync(join(homedir(), '.cache', 'ithomiini-test'), { recursive: true });
const hookDir = mkdtempSync(join(homedir(), '.cache', 'ithomiini-test', 'lab-hooks-'));
after(() => rmSync(hookDir, { recursive: true, force: true }));
import { join } from 'node:path';
import { applyLabGuard, labGuard } from '../scripts/lab-guard.mjs';

test('the lab guard: commands, files, scheduling tools, and the sandbox that closes what text rules cannot', () => {
  const { deny, sandbox } = labGuard({ home: '/home/x', source: '/src', cache: '/cache' });
  for (const rule of ['Bash(wacli*)', 'Bash(*wacli*)', 'Bash(*systemd-run*)', 'Bash(at *)', 'Bash(*crontab*)', 'Bash(*systemctl*)', 'Bash(*ithomiini-wa-bridge*)', 'Bash(*.wacli*)', 'Read(//home/x/.wacli/**)', 'Read(//home/x/.config/ithomiini-wa-bridge/**)', 'mcp__t3-code__schedule_task', 'mcp__t3-code__run_scheduled_task_now', 'mcp__t3-code__delete_scheduled_task', 'mcp__t3-code__list_scheduled_tasks', 'CronCreate', 'RemoteTrigger']) assert.ok(deny.includes(rule), rule);
  assert.equal(sandbox.enabled, true);
  assert.equal(sandbox.allowUnsandboxedCommands, false);
  assert.deepEqual(sandbox.filesystem.allowWrite, ['/src', '/cache', '/tmp']);
  assert.ok(['/home/x/.wacli', '/home/x/.local/bin/wacli', '/home/x/.config/ithomiini-wa-bridge'].every(p => sandbox.filesystem.denyRead.includes(p)));
  // gog: its binary, keyring and OAuth files out of the sandbox, the Gmail read hook in (scripts/lab-gmail-read.mjs)
  assert.ok(['/home/x/.local/bin/gog', '/home/x/.config/gogcli', '/home/x/.local/share/gogcli'].every(p => sandbox.filesystem.denyRead.includes(p)));
  assert.ok(deny.includes('Read(//home/x/.config/gogcli/**)') && deny.includes('Bash(*gogcli*)'));
  const settings = applyLabGuard({ hooks: { PreToolUse: [{ matcher: 'Edit', hooks: [{ type: 'command', command: 'mine' }] }] } }, { home: '/home/x', source: '/src', hookDir });
  assert.equal(settings.hooks.PreToolUse.length, 2);
  const cmd = settings.hooks.PreToolUse[0].hooks[0].command;
  assert.match(cmd, /lab-gmail-read-[0-9a-f]{10}\.mjs"$/);
  assert.ok(cmd.includes(hookDir) && !cmd.includes('/src/'), 'runs from the read-only copy, not the writable source');
  const copy = JSON.parse(cmd.slice(cmd.lastIndexOf(' ') + 1));
  assert.equal(statSync(copy).mode & 0o777, 0o444);
  assert.equal(readFileSync(copy, 'utf8'), readFileSync(new URL('../scripts/lab-gmail-read.mjs', import.meta.url), 'utf8'));
  assert.ok(settings.permissions.deny.includes(`Edit(/${hookDir}/**)`));
  assert.equal(settings.hooks.PreToolUse[0].matcher, 'Bash|Monitor');
  assert.equal(settings.hooks.PreToolUse[1].hooks[0].command, 'mine');
});

test('applying it keeps the workspace\'s own rules and sandbox settings, and is repeatable', () => {
  const settings = { permissions: { allow: ['Skill'], deny: ['Bash(rm -rf /:*)'] }, sandbox: { filesystem: { allowWrite: ['/mine'] }, excludedCommands: ['docker'] } };
  applyLabGuard(settings, { home: '/home/x', source: '/src', hookDir });
  const once = JSON.stringify(settings);
  applyLabGuard(settings, { home: '/home/x', source: '/src', hookDir });
  assert.equal(JSON.stringify(settings), once);
  assert.deepEqual(settings.permissions.allow, ['Skill']);
  assert.ok(settings.permissions.deny.includes('Bash(rm -rf /:*)') && settings.permissions.deny.includes('Bash(*wacli*)'));
  assert.ok(settings.sandbox.filesystem.allowWrite.includes('/mine') && settings.sandbox.filesystem.allowWrite.includes('/src'));
  assert.deepEqual(settings.sandbox.excludedCommands, ['docker']);
  // rules only, for the workspaces whose browsers need the lab app's loopback address
  const rulesOnly = applyLabGuard({}, { home: '/home/x', sandboxOn: false, hookDir });
  assert.ok(rulesOnly.permissions.deny.includes('Bash(*wacli*)'));
  assert.equal(rulesOnly.sandbox, undefined);
});

test('the command guards every workspace of a lab T3, the UI audits get the rules only', () => {
  const home = mkdtempSync(join(tmpdir(), 'lab-guard-'));
  for (const w of ['lab', 'whatsapp', 'uiaudit']) {
    mkdirSync(join(home, 'workspaces', w, '.claude'), { recursive: true });
    writeFileSync(join(home, 'workspaces', w, '.claude', 'settings.json'), JSON.stringify({ permissions: { allow: ['Skill'] } }));
  }
  execFileSync(process.execPath, [new URL('../scripts/lab-guard.mjs', import.meta.url).pathname, '--lab-home', home, '--src', '/src'], { encoding: 'utf8', env: { ...process.env, ITHOMIINI_LAB_HOOK_DIR: hookDir } });
  const read = w => JSON.parse(readFileSync(join(home, 'workspaces', w, '.claude', 'settings.json'), 'utf8'));
  for (const w of ['lab', 'whatsapp', 'uiaudit']) assert.ok(read(w).permissions.deny.includes('Bash(*wacli*)'), w);
  assert.equal(read('lab').sandbox.enabled, true);
  assert.equal(read('whatsapp').sandbox.enabled, true);
  assert.equal(read('uiaudit').sandbox, undefined);
});
