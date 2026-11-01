import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdirSync, mkdtempSync, readFileSync, writeFileSync } from 'node:fs';
import { execFileSync } from 'node:child_process';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { applyLabGuard, labGuard } from '../scripts/lab-guard.mjs';

test('the lab guard: commands, files, scheduling tools, and the sandbox that closes what text rules cannot', () => {
  const { deny, sandbox } = labGuard({ home: '/home/x', source: '/src', cache: '/cache' });
  for (const rule of ['Bash(wacli*)', 'Bash(*wacli*)', 'Bash(*systemd-run*)', 'Bash(at *)', 'Bash(*crontab*)', 'Bash(*systemctl*)', 'Bash(*ithomiini-wa-bridge*)', 'Bash(*.wacli*)', 'Read(//home/x/.wacli/**)', 'Read(//home/x/.config/ithomiini-wa-bridge/**)', 'mcp__t3-code__schedule_task', 'mcp__t3-code__run_scheduled_task_now', 'mcp__t3-code__delete_scheduled_task', 'mcp__t3-code__list_scheduled_tasks', 'CronCreate', 'RemoteTrigger']) assert.ok(deny.includes(rule), rule);
  assert.equal(sandbox.enabled, true);
  assert.equal(sandbox.allowUnsandboxedCommands, false);
  assert.deepEqual(sandbox.filesystem.allowWrite, ['/src', '/cache', '/tmp']);
  assert.ok(['/home/x/.wacli', '/home/x/.local/bin/wacli', '/home/x/.config/ithomiini-wa-bridge'].every(p => sandbox.filesystem.denyRead.includes(p)));
});

test('applying it keeps the workspace\'s own rules and sandbox settings, and is repeatable', () => {
  const settings = { permissions: { allow: ['Skill'], deny: ['Bash(rm -rf /:*)'] }, sandbox: { filesystem: { allowWrite: ['/mine'] }, excludedCommands: ['docker'] } };
  applyLabGuard(settings, { home: '/home/x', source: '/src' });
  const once = JSON.stringify(settings);
  applyLabGuard(settings, { home: '/home/x', source: '/src' });
  assert.equal(JSON.stringify(settings), once);
  assert.deepEqual(settings.permissions.allow, ['Skill']);
  assert.ok(settings.permissions.deny.includes('Bash(rm -rf /:*)') && settings.permissions.deny.includes('Bash(*wacli*)'));
  assert.ok(settings.sandbox.filesystem.allowWrite.includes('/mine') && settings.sandbox.filesystem.allowWrite.includes('/src'));
  assert.deepEqual(settings.sandbox.excludedCommands, ['docker']);
  // rules only, for the workspaces whose browsers need the lab app's loopback address
  const rulesOnly = applyLabGuard({}, { home: '/home/x', sandboxOn: false });
  assert.ok(rulesOnly.permissions.deny.includes('Bash(*wacli*)'));
  assert.equal(rulesOnly.sandbox, undefined);
});

test('the command guards every workspace of a lab T3, the UI audits get the rules only', () => {
  const home = mkdtempSync(join(tmpdir(), 'lab-guard-'));
  for (const w of ['lab', 'whatsapp', 'uiaudit']) {
    mkdirSync(join(home, 'workspaces', w, '.claude'), { recursive: true });
    writeFileSync(join(home, 'workspaces', w, '.claude', 'settings.json'), JSON.stringify({ permissions: { allow: ['Skill'] } }));
  }
  execFileSync(process.execPath, [new URL('../scripts/lab-guard.mjs', import.meta.url).pathname, '--lab-home', home, '--src', '/src'], { encoding: 'utf8' });
  const read = w => JSON.parse(readFileSync(join(home, 'workspaces', w, '.claude', 'settings.json'), 'utf8'));
  for (const w of ['lab', 'whatsapp', 'uiaudit']) assert.ok(read(w).permissions.deny.includes('Bash(*wacli*)'), w);
  assert.equal(read('lab').sandbox.enabled, true);
  assert.equal(read('whatsapp').sandbox.enabled, true);
  assert.equal(read('uiaudit').sandbox, undefined);
});
