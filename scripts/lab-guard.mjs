#!/usr/bin/env node
// What the local test lab's chats (the lab T3, home ~/.t3-ithomiini-lab: Lab, WhatsApp simulation, UI audits, the
// people's lab workspaces) must never be able to do: send WhatsApp, make timers or scheduled jobs, or reach the live
// WhatsApp bridge. A lab simulation once made a real systemd timer that would have messaged Franz's own chat.
//
// Two layers in each workspace's .claude/settings.json:
//  1. permissions.deny: the commands by name (wacli, systemd-run, at, crontab, systemctl, also inside `bash -c`, `env`,
//     `timeout`, an absolute path), the bridge's and wacli's folders, the T3 and Claude scheduling tools.
//     These match the command's text, so a command that builds the name in code (`'w'+'acli'`) gets through them,
//  2. the sandbox (bubblewrap) closes that: Bash runs without access to wacli's store, its binary, the bridge's
//     folders and the user's D-Bus socket (systemd), and writes only in the workspace, the lab's source and cache, and
//     /tmp. Commands cannot ask to run outside it.
//
//   node scripts/lab-guard.mjs --lab-home ~/.t3-ithomiini-lab --src <lab checkout>   every workspace of that T3
//   node scripts/lab-guard.mjs <workspace>...                                         these workspaces
// scripts/t3-provision.mjs applies the same to every workspace it writes for the lab (ITHOMIINI_LAB_URL set).
import { existsSync, readdirSync, readFileSync, statSync, writeFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

/** The rules and the sandbox for a lab workspace. `home`: the user's home; `source`: the lab's checkout; `cache`: the lab's data folder. */
export function labGuard({ home = homedir(), source = null, cache = join(home, '.cache', 'ithomiini-lab'), sandboxOn = true } = {}) {
  const secret = [join(home, '.wacli'), join(home, '.config', 'ithomiini-wa-bridge'), join(home, '.local', 'share', 'ithomiini-wa-bridge'), join(home, '.local', 'state', 'ithomiini-wa-bridge')];
  const deny = [
    // Commands (matched on the command's text, whatever wraps them: bash -c, env, timeout, an absolute path)
    'Bash(wacli*)',
    'Bash(*wacli*)',
    'Bash(*systemd-run*)',
    'Bash(at *)',
    'Bash(*crontab*)',
    'Bash(*systemctl*)',
    'Bash(*ithomiini-wa-bridge*)',
    'Bash(*.wacli*)',
    // The files
    ...secret.map(p => `Read(/${p}/**)`),
    // Scheduling tools of T3 and of Claude Code itself
    'mcp__t3-code__schedule_task',
    'mcp__t3-code__run_scheduled_task_now',
    'mcp__t3-code__delete_scheduled_task',
    'mcp__t3-code__list_scheduled_tasks',
    'CronCreate',
    'CronDelete',
    'RemoteTrigger',
  ];
  const sandbox = !sandboxOn ? null : {
    enabled: true,
    autoAllowBashIfSandboxed: true,
    allowUnsandboxedCommands: false,
    filesystem: {
      denyRead: [...secret, join(home, '.local', 'bin', 'wacli')],
      allowWrite: [source, cache, '/tmp'].filter(Boolean),
    },
  };
  return { deny, sandbox };
}

/** `settings` (a parsed settings.json) with the guard added; other settings and rules are kept. */
export function applyLabGuard(settings, options) {
  const guard = labGuard(options);
  settings.permissions ??= {};
  settings.permissions.deny = [...new Set([...(settings.permissions.deny ?? []), ...guard.deny])];
  if (!guard.sandbox) return settings; // the permission rules only (a workspace whose chats need the lab app's loopback address)
  const own = settings.sandbox ?? {};
  settings.sandbox = {
    ...own,
    ...guard.sandbox,
    filesystem: {
      ...(own.filesystem ?? {}),
      denyRead: [...new Set([...(own.filesystem?.denyRead ?? []), ...guard.sandbox.filesystem.denyRead])],
      allowWrite: [...new Set([...(own.filesystem?.allowWrite ?? []), ...guard.sandbox.filesystem.allowWrite])],
    },
  };
  return settings;
}

function applyTo(workspace, options) {
  const file = join(workspace, '.claude', 'settings.json');
  let settings = {};
  try {
    settings = JSON.parse(readFileSync(file, 'utf8'));
  } catch {
    /* a workspace without settings yet */
  }
  writeFileSync(file, JSON.stringify(applyLabGuard(settings, options), null, 2), { mode: 0o600 });
  return file;
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const args = process.argv.slice(2);
  const take = name => {
    const i = args.indexOf(name);
    return i >= 0 ? args.splice(i, 2)[1] : null;
  };
  const labHome = take('--lab-home');
  const source = take('--src');
  // The sandbox gives Bash a network of its own (no 127.0.0.1 of this PC), which the UI audits' browsers need: these get the rules only.
  const noSandbox = (take('--no-sandbox-for') ?? 'uiaudit,uidark').split(',');
  const targets = labHome
    ? readdirSync(join(labHome, 'workspaces'), { withFileTypes: true }).filter(d => d.isDirectory() && existsSync(join(labHome, 'workspaces', d.name, '.claude'))).map(d => join(labHome, 'workspaces', d.name))
    : args;
  if (!targets.length) {
    console.error('Usage: lab-guard.mjs --lab-home <T3 home> [--src <checkout>] | <workspace>...');
    process.exit(1);
  }
  for (const t of targets) if (statSync(t).isDirectory()) console.log('guarded', applyTo(t, { source, sandboxOn: !noSandbox.includes(t.split('/').pop()) }), noSandbox.includes(t.split('/').pop()) ? '(rules only)' : '(rules and sandbox)');
}
