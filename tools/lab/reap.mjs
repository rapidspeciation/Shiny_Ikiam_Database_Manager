#!/usr/bin/env node
// Gives this PC's memory back from work that is over (tools/lab/README.md, «Memory»). Every 5 minutes as a user timer
// (--install), or by hand. Three kinds of leftovers:
//   - dev apps (tools/lab/dev-app.sh) whose chat ended: stopped;
//   - headless browsers of Playwright left without the script that drove them, or a chat's older than --browser-hours: stopped;
//   - chats of the lab T3 (~/.t3-ithomiini-lab) whose title matches --chats (a scenario run's: each answers once) and
//     that have been idle --chat-idle-min minutes: archived. T3 keeps an idle chat's Claude (about 300 MB) for 30
//     minutes; a run of 40 scenarios at once is then 12 GB. Archiving ends it; an archived chat can be unarchived.
// Never anything else: no other T3, no service, no process outside those three kinds.
//   node tools/lab/reap.mjs [--dry-run] [--chats <regex>] [--chat-idle-min 5] [--browser-hours 2]
//   node tools/lab/reap.mjs --install | --uninstall      the user timer (ithomiini-lab-reap.timer), which runs a copy
//                                                        in the lab folder ($LAB/reaper), so no checkout is needed
import { execFileSync } from 'node:child_process';
import { copyFileSync, existsSync, mkdirSync, readFileSync, readdirSync, writeFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const args = process.argv.slice(2);
const opt = (k, d) => (args.includes(k) ? args[args.indexOf(k) + 1] : d);
const dry = args.includes('--dry-run');
const LAB = process.env.ITHOMIINI_LAB_DIR || join(homedir(), '.cache/ithomiini-lab');
const T3HOME = process.env.LAB_T3_HOME || join(homedir(), '.t3-ithomiini-lab');
const T3PORT = process.env.LAB_T3_PORT || '3775';
const CHATS = new RegExp(opt('--chats', '^WA soul (lab|critic) '));
const CHAT_IDLE_MS = Number(opt('--chat-idle-min', 5)) * 60_000;
const BROWSER_MS = Number(opt('--browser-hours', 2)) * 3600_000;
const ORPHAN_MS = 10 * 60_000;
const systemctl = (...a) => execFileSync('systemctl', ['--user', ...a], { encoding: 'utf8' });
// The app's T3 commands: from the checkout, or beside this file in the timer's copy.
const T3COMMANDS = [join(here, '../../server/t3commands.mjs'), join(here, 't3commands.mjs')].find(existsSync);
const say = (...a) => console.log(dry ? '[dry run]' : '', ...a);

if (args.includes('--install') || args.includes('--uninstall')) {
  const dir = join(homedir(), '.config/systemd/user');
  if (args.includes('--uninstall')) {
    systemctl('disable', '--now', 'ithomiini-lab-reap.timer');
    console.log('Timer off (the unit files stay in', dir + ')');
    process.exit(0);
  }
  mkdirSync(dir, { recursive: true });
  const copy = join(LAB, 'reaper');
  mkdirSync(copy, { recursive: true, mode: 0o700 });
  copyFileSync(join(here, 'reap.mjs'), join(copy, 'reap.mjs'));
  copyFileSync(T3COMMANDS, join(copy, 't3commands.mjs'));
  writeFileSync(
    join(dir, 'ithomiini-lab-reap.service'),
    `[Unit]\nDescription=Ithomiini lab: stop finished dev apps and stray headless browsers, archive idle scenario chats (tools/lab/reap.mjs)\n\n[Service]\nType=oneshot\nExecStart=${process.execPath} ${join(copy, 'reap.mjs')}\nNice=10\n`,
  );
  writeFileSync(
    join(dir, 'ithomiini-lab-reap.timer'),
    `[Unit]\nDescription=Ithomiini lab reaper every 5 minutes\n\n[Timer]\nOnBootSec=5min\nOnUnitActiveSec=5min\nAccuracySec=30s\n\n[Install]\nWantedBy=timers.target\n`,
  );
  systemctl('daemon-reload');
  systemctl('enable', '--now', 'ithomiini-lab-reap.timer');
  console.log('Timer on: ithomiini-lab-reap.timer (journalctl --user -u ithomiini-lab-reap)');
  process.exit(0);
}

const alive = pid => existsSync(`/proc/${pid}`);
const comm = pid => {
  try {
    return readFileSync(`/proc/${pid}/comm`, 'utf8').trim();
  } catch {
    return null;
  }
};
const ppid = pid => {
  try {
    return Number(/^PPid:\s+(\d+)/m.exec(readFileSync(`/proc/${pid}/status`, 'utf8'))[1]);
  } catch {
    return 0;
  }
};
const ticks = Number(execFileSync('getconf', ['CLK_TCK'], { encoding: 'utf8' }).trim()) || 100;
const uptime = () => Number(readFileSync('/proc/uptime', 'utf8').split(' ')[0]);
/** How long the process has run, ms. */
const age = pid => {
  try {
    const fields = readFileSync(`/proc/${pid}/stat`, 'utf8').split(') ')[1].split(' ');
    return (uptime() - Number(fields[19]) / ticks) * 1000;
  } catch {
    return 0;
  }
};

/** Whether a `claude` or `codex` (a chat) is above the process. */
const underChat = pid => {
  for (let p = ppid(pid); p > 1; p = ppid(p)) if (['claude', 'codex'].includes(comm(p))) return true;
  return false;
};

// 1. Dev apps whose chat ended.
function devApps() {
  let units = [];
  try {
    units = systemctl('list-units', 'ithomiini-dev-*', '--plain', '--no-legend', '--state=active')
      .split('\n')
      .map(l => l.split(/\s+/)[0])
      .filter(Boolean);
  } catch {}
  for (const unit of units) {
    const env = systemctl('show', '-p', 'Environment', '--value', unit);
    const owner = Number(/DEV_APP_OWNER=(\d+)/.exec(env)?.[1] ?? 0);
    if (owner > 0 && (!alive(owner) || !['claude', 'codex'].includes(comm(owner)))) {
      say(`${unit}: its chat (pid ${owner}) ended: stopped`);
      if (!dry) systemctl('stop', unit);
    }
  }
}

// 2. Playwright's headless browsers: the main process (no --type=) of a browser under ms-playwright.
function browsers() {
  for (const name of readdirSync('/proc')) {
    if (!/^\d+$/.test(name)) continue;
    const pid = Number(name);
    let cmd;
    try {
      cmd = readFileSync(`/proc/${pid}/cmdline`, 'utf8').split('\0');
    } catch {
      continue;
    }
    if (!cmd[0]?.includes('/ms-playwright/') || cmd.some(a => a.startsWith('--type='))) continue;
    const parent = ppid(pid);
    const orphan = parent <= 1 || comm(parent) === 'systemd';
    const t = age(pid);
    // Old ones only when a chat started them (T3's own browser, under t3, is never touched).
    if ((orphan && t > ORPHAN_MS) || (t > BROWSER_MS && underChat(pid))) {
      say(`headless browser ${pid}: ${orphan ? 'its script is gone' : 'running'} for ${Math.round(t / 60_000)} min: stopped`);
      if (!dry) {
        try {
          process.kill(pid, 'SIGTERM');
        } catch {}
      }
    }
  }
}

// 3. The lab T3's idle scenario chats.
async function chats() {
  const db = join(T3HOME, 'userdata/statev2.sqlite');
  const tokenFile = join(LAB, 't3-admin-token');
  if (!existsSync(db) || !existsSync(tokenFile)) return;
  const { DatabaseSync } = await import('node:sqlite');
  const d = new DatabaseSync(db, { readOnly: true });
  let rows;
  try {
    rows = d
      .prepare(
        `SELECT t.thread_id AS id, t.title,
           MAX(COALESCE(r.completed_at, r.requested_at)) AS last,
           SUM(CASE WHEN r.status IN ('completed','failed','cancelled','interrupted','rolled_back') THEN 0 ELSE 1 END) AS open
         FROM orchestration_v2_projection_threads t JOIN orchestration_v2_projection_runs r USING (thread_id)
         WHERE t.archived_at IS NULL AND t.deleted_at IS NULL GROUP BY t.thread_id`,
      )
      .all();
  } finally {
    d.close();
  }
  // Past 40 minutes T3 has let the chat's Claude go already: nothing to give back, and older chats stay as they are.
  const idle = rows.filter(r => {
    const quiet = Date.now() - Date.parse(r.last);
    return CHATS.test(r.title) && !r.open && quiet >= CHAT_IDLE_MS && quiet < 40 * 60_000;
  });
  if (!idle.length) return;
  say(`lab T3: ${idle.length} idle chat(s) matching ${CHATS}: archived`);
  if (dry) return;
  const { t3Commands } = await import(T3COMMANDS);
  const send = t3Commands({ local: `http://127.0.0.1:${T3PORT}`, tokenFile });
  for (const r of idle) {
    try {
      await send({ type: 'thread.archive', threadId: r.id });
    } catch (e) {
      console.error(`${r.title}: ${e.message}`);
    }
  }
}

devApps();
browsers();
await chats();
