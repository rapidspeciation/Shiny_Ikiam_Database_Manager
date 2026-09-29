// Updating T3 Code (a stock install on the same host) from the Asistente tab.
// T3's own "update" button only shows when a newer desktop client connects, so
// admins get one here: the installed and newest stable versions, the Claude
// sessions T3 has open (an update restarts T3 and cuts them), and a button
// that starts the systemd user unit ithomiini-t3-update.service (`t3 update`).
import { execFile } from 'node:child_process';
import { readFile, readdir } from 'node:fs/promises';
import { join } from 'node:path';

const fail = (code, message, status = 400) => Object.assign(new Error(message), { code, status });

const RELEASES = 'https://api.github.com/repos/pingdotgg/t3code/releases?per_page=100';
const UNIT = 'ithomiini-t3-update.service';
const STABLE = /^v?(\d+)\.(\d+)\.(\d+)$/;

const run = (bin, args) =>
  new Promise(resolve =>
    execFile(bin, args, { timeout: 10_000 }, (error, stdout) => resolve({ ok: !error, out: String(stdout || '').trim() })),
  );

export function newer(a, b) {
  const x = STABLE.exec(a || '');
  const y = STABLE.exec(b || '');
  if (!x || !y) return false;
  for (let i = 1; i <= 3; i++) if (Number(x[i]) !== Number(y[i])) return Number(x[i]) > Number(y[i]);
  return false;
}

let latestCache = { at: 0, version: null };
async function latestStable(fetchImpl) {
  if (Date.now() - latestCache.at < 3600_000 && latestCache.version) return latestCache.version;
  const res = await fetchImpl(RELEASES, { headers: { accept: 'application/vnd.github+json', 'user-agent': 'ithomiini-app' } });
  if (!res.ok) throw fail('T3_RELEASES', `GitHub respondió ${res.status}`, 502);
  const tag = (await res.json()).map(r => r.tag_name).find(t => STABLE.test(t || ''));
  latestCache = { at: Date.now(), version: tag ? tag.replace(/^v/, '') : null };
  return latestCache.version;
}

/** Claude sessions T3 keeps open (its agent processes read chat turns as stream-json). */
async function openSessions() {
  let n = 0;
  for (const pid of await readdir('/proc').catch(() => [])) {
    if (!/^\d+$/.test(pid)) continue;
    const cmd = await readFile(`/proc/${pid}/cmdline`, 'utf8').catch(() => '');
    if (cmd.includes('--input-format\0stream-json') && cmd.includes('--permission-prompt-tool')) n++;
  }
  return n;
}

export function t3Admin({ home = '/home/ubuntu/.t3', systemctl = 'systemctl', fetchImpl = fetch } = {}) {
  const log = join(home, 'userdata', 'logs', 't3-update.log');
  return {
    async status() {
      const state = JSON.parse(await readFile(join(home, 'runtime', 'service-state.json'), 'utf8').catch(() => '{}'));
      const [latest, sessions, unit, tail] = await Promise.all([
        latestStable(fetchImpl).catch(() => null),
        openSessions(),
        run(systemctl, ['--user', 'is-active', UNIT]),
        readFile(log, 'utf8').catch(() => ''),
      ]);
      const current = state.activeVersion || null;
      return {
        current,
        latest,
        updateAvailable: newer(latest, current),
        sessions,
        updating: unit.out === 'activating' || unit.out === 'active',
        log: tail.trim().split('\n').slice(-6).join('\n'),
      };
    },
    async update() {
      const started = await run(systemctl, ['--user', 'start', '--no-block', UNIT]);
      if (!started.ok) throw fail('T3_UPDATE', 'No se pudo iniciar la actualización de T3', 500);
      return { started: true };
    },
  };
}
