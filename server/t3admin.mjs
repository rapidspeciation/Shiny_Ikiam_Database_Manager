// Updating T3 Code (a stock install on the same host) from the Asistente tab.
// T3's own "update" button only shows when a newer desktop client connects, so
// admins get one here: the installed and newest stable versions, the Claude
// sessions T3 has open (an update restarts T3 and cuts them), and a button
// that starts the systemd user unit ithomiini-t3-update.service (`t3 update`).
// Also each person's sign-in link to T3 (t3Pairing), made with the admin token.
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

/** Scopes only the admin token keeps (people's links never get them); review:write is not granted from 0.0.46. */
const ADMIN_ONLY = new Set(['access:read', 'access:write', 'relay:write', 'review:write']);

/**
 * A one-time T3 Code sign-in link for the person, made with the broker's admin
 * token (T3 is a stock install: its own pairing flow, nothing patched). The link
 * gets T3's own set for a client: on 0.0.45 the five scopes asked for by name
 * before, from 0.0.46 finer ones too. An admin token issued by an older T3 lacks
 * those (T3 answers 403): then the link gets the client scopes the token has.
 */
export async function t3Pairing(t3, user, { fetchImpl = fetch } = {}) {
  const token = (await readFile(t3.tokenFile, 'utf8')).trim();
  const headers = { authorization: `Bearer ${token}`, 'content-type': 'application/json' };
  const ask = scopes =>
    fetchImpl(`${t3.local}/api/auth/pairing-token`, {
      method: 'POST',
      headers,
      body: JSON.stringify({ label: `ithomiini:${user.username}`, ...(scopes ? { scopes } : {}) }),
      signal: AbortSignal.timeout(10_000),
    });
  let response = await ask(null);
  if (response.status === 403) {
    const session = await fetchImpl(`${t3.local}/api/auth/session`, { headers, signal: AbortSignal.timeout(10_000) });
    const held = session.ok ? await session.json() : {};
    const scopes = (held.permissions ?? held.scopes ?? []).filter(scope => !ADMIN_ONLY.has(scope));
    if (scopes.length) response = await ask(scopes);
  }
  if (!response.ok) throw fail('T3_UNAVAILABLE', `T3 Code answered ${response.status}`, 502);
  const { credential, expiresAt } = await response.json();
  return { url: `${t3.url}/pair#token=${encodeURIComponent(credential)}`, expiresAt };
}
