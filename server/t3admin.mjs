// Updating T3 Code (a stock install on the same host) from the Asistente tab.
// T3's own "update" button only shows when a newer desktop client connects, so
// admins get one here. Before: the version T3 runs and the newest stable one, its
// release notes ("Before you update" quoted), whether the jump is a major one (0.0.x → 0.1.0
// moved T3 to Orchestrator V2: its first start copies the chats into a new database, one way),
// and the Claude sessions T3 has open (an update restarts T3 and cuts them). The update itself
// is scripts/t3-update.mjs (a copy of T3's database, `t3 update`, T3's sign-in token for the app
// issued again by the new version, a wait until T3 answers on it), started as the systemd user unit
// ithomiini-t3-update@<version>.service on the server, or a command in the lab; its progress and
// log are read here while it runs. After: checks of what the app needs from T3. A version T3 had
// before stays in runtime/versions, and going back to it is the same update to that version.
// Also each person's sign-in link to T3 (t3Pairing), made with the admin token.
import { execFile, spawn } from 'node:child_process';
import { readFile, readdir } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { join } from 'node:path';
import { t3CommandWay } from './t3commands.mjs';
import { allowFraming, canInject, injectBridge, validHttpOrigin } from './t3bridge.mjs';

const fail = (code, message, status = 400) => Object.assign(new Error(message), { code, status });

const RELEASES = 'https://api.github.com/repos/pingdotgg/t3code/releases?per_page=100';
const NOTES = version => `https://github.com/pingdotgg/t3code/releases/tag/v${version}`;
const UNIT = version => `ithomiini-t3-update@${version}.service`;
const STABLE = /^v?(\d+)\.(\d+)\.(\d+)$/;
const BACKUP_DIR = /^\d{4}-\d\d-\d\dT\d\d-\d\d-\d\d-\d{3}Z-(\d+\.\d+\.\d+)$/;
/** The first stable T3 of Orchestrator V2 (its nightlies started at 0.0.46). */
const V2 = '0.1.0';
/** The update's steps, as scripts/t3-update.mjs names them; done and failed end it. */
const ENDED = new Set(['done', 'failed']);

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

/** A major jump: the first number that is not 0 changes (0.0.45 → 0.1.0, 0.1.3 → 0.2.0, 1.4.0 → 2.0.0). */
export function majorJump(from, to) {
  const x = STABLE.exec(from || '');
  const y = STABLE.exec(to || '');
  if (!x || !y) return false;
  for (let i = 1; i <= 3; i++) {
    if (Number(x[i]) !== Number(y[i])) return true;
    if (Number(x[i]) !== 0) return false;
  }
  return false;
}

/** The "Before you update" part of release notes (markdown), up to its next heading; null without one. */
export function beforeYouUpdate(body) {
  const text = String(body ?? '').replace(/\r\n?/g, '\n');
  const start = text.search(/^#{2,4}\s*Before you update\s*$/im);
  if (start < 0) return null;
  const rest = text.slice(start).replace(/^.*\n/, '');
  const end = rest.search(/^#{1,4}\s/m);
  const part = (end < 0 ? rest : rest.slice(0, end)).trim();
  return part ? part.slice(0, 4000) : null;
}

let releasesCache = { at: 0, list: null };
/** T3's stable releases, newest first: { version, name, url, publishedAt, before }. */
async function stableReleases(fetchImpl) {
  if (Date.now() - releasesCache.at < 3600_000 && releasesCache.list) return releasesCache.list;
  const res = await fetchImpl(RELEASES, { headers: { accept: 'application/vnd.github+json', 'user-agent': 'ithomiini-app' } });
  if (!res.ok) throw fail('T3_RELEASES', `GitHub answered ${res.status}`, 502);
  const list = (await res.json())
    .filter(r => STABLE.test(r.tag_name || '') && !r.draft && !r.prerelease)
    .map(r => {
      const version = r.tag_name.replace(/^v/, '');
      return { version, name: r.name || `T3 Code v${version}`, url: r.html_url || NOTES(version), publishedAt: r.published_at ?? null, before: beforeYouUpdate(r.body) };
    });
  releasesCache = { at: Date.now(), list };
  return list;
}

/** The processes on this host as pid → { ppid, cmd } (Linux /proc; empty elsewhere). */
async function processes() {
  const all = new Map();
  for (const pid of await readdir('/proc').catch(() => [])) {
    if (!/^\d+$/.test(pid)) continue;
    const statText = await readFile(`/proc/${pid}/stat`, 'utf8').catch(() => '');
    // The command name in parentheses may hold spaces: the fields after its last ')' are fixed.
    const ppid = Number(statText.slice(statText.lastIndexOf(')') + 2).split(' ')[1]);
    if (!ppid) continue;
    all.set(Number(pid), { ppid, cmd: () => readFile(`/proc/${pid}/cmdline`, 'utf8').catch(() => '') });
  }
  return all;
}

const SHELLS = new Set(['bash', 'sh', 'zsh', 'dash', 'fish']);
/** Shells T3 keeps open for its terminals (its server's own children): an update closes them too. */
export async function openTerminals(root, list = processes) {
  if (!root) return 0;
  let n = 0;
  for (const p of (await list()).values()) {
    if (p.ppid !== root) continue;
    const program = (await p.cmd()).split('\0')[0].split('/').pop();
    if (SHELLS.has(program)) n++;
  }
  return n;
}

/**
 * Claude sessions T3 keeps open (its agent processes read chat turns as stream-json): those under
 * T3's server (`root`), so another T3 on the host is not counted; all of them when T3's pid is not known.
 */
export async function openSessions(root, list = processes) {
  const all = await list();
  const under = pid => {
    for (let at = pid, hops = 0; at > 1 && hops < 64; hops++) {
      const parent = all.get(at)?.ppid;
      if (parent === root) return true;
      at = parent;
    }
    return false;
  };
  let n = 0;
  for (const [pid, p] of all) {
    if (root && !under(pid)) continue;
    const cmd = await p.cmd();
    if (cmd.includes('--input-format\0stream-json') && cmd.includes('--permission-prompt-tool')) n++;
  }
  return n;
}

/**
 * The log of the last run (from its first line, «[time] T3 Code a → b»), as a terminal shows it: a line
 * rewritten in place (t3's download progress, with \r) shows its last state. Its last 60 lines.
 */
export function runLog(text) {
  const lines = String(text ?? '')
    .split('\n')
    .map(line => line.split('\r').filter(part => part.trim()).pop() ?? '');
  let first = lines.length;
  while (first > 0 && !/^\[[^\]]+\] T3 Code \S+ → /.test(lines[first - 1])) first--;
  return (first > 0 ? lines.slice(first - 1) : lines).slice(-60).join('\n').trim();
}

const readJson = async file => JSON.parse(await readFile(file, 'utf8').catch(() => 'null'));
const alive = pid => {
  try {
    process.kill(pid, 0);
    return true;
  } catch (e) {
    return e.code === 'EPERM';
  }
};

/**
 * `home`: T3's home; `local`: where T3 answers on this host; `command`: the lab's update command
 * (it gets the version as its argument), else the systemd unit.
 */
export function t3Admin({ home = '/home/ubuntu/.t3', local = 'http://127.0.0.1:3773', command = null, systemctl = 'systemctl', fetchImpl = fetch, spawnImpl = spawn } = {}) {
  const logFile = join(home, 'userdata', 'logs', 't3-update.log');
  const runFile = join(home, 'userdata', 'logs', 't3-update.json');
  const versionsDir = join(home, 'runtime', 'versions');

  /** What the running T3 says of itself (its version and orchestration protocol); null when it does not answer. */
  async function describe() {
    try {
      const r = await fetchImpl(`${local}/.well-known/t3/environment`, { signal: AbortSignal.timeout(3000) });
      return r.ok ? await r.json() : null;
    } catch {
      return null;
    }
  }
  /** Versions T3 keeps in runtime/versions or preserved in backups (complete ones), newest first. */
  async function installed() {
    const kept = new Set();
    for (const name of await readdir(versionsDir).catch(() => []))
      if (STABLE.test(name) && existsSync(join(versionsDir, name, '.install-complete'))) kept.add(name);
    const backupsDir = join(home, 'backups');
    for (const name of await readdir(backupsDir).catch(() => [])) {
      const m = BACKUP_DIR.exec(name);
      if (m && existsSync(join(backupsDir, name, 'runtime-version', '.install-complete'))) kept.add(m[1]);
    }
    return [...kept].sort((a, b) => (newer(a, b) ? -1 : newer(b, a) ? 1 : 0));
  }
  /** The update under way or last run (scripts/t3-update.mjs), with whether it still runs. */
  async function lastRun() {
    const last = await readJson(runFile);
    if (!last) return null;
    return { ...last, running: !ENDED.has(last.step) && !!last.pid && alive(last.pid) };
  }

  return {
    describe,
    async status() {
      const [running, state, releases, runtime, last, logText, kept] = await Promise.all([
        describe(),
        readJson(join(home, 'runtime', 'service-state.json')),
        stableReleases(fetchImpl).catch(() => null),
        readJson(join(home, 'userdata', 'server-runtime.json')),
        lastRun(),
        readFile(logFile, 'utf8').catch(() => ''),
        installed(),
      ]);
      const current = running?.serverVersion ?? state?.activeVersion ?? null;
      const protocol = running?.orchestrationProtocolVersion ?? null;
      const latest = releases?.[0] ?? null;
      const updateAvailable = newer(latest?.version, current);
      const root = running && runtime?.pid ? runtime.pid : null;
      const [sessions, terminals] = await Promise.all([openSessions(root), openTerminals(root)]);
      return {
        current,
        protocol,
        answering: !!running,
        latest: latest?.version ?? null,
        updateAvailable,
        release: updateAvailable ? latest : null,
        // The jump to the newest: a major one, and (from V1) the one-way copy of T3's chats into V2's database.
        jump: updateAvailable
          ? { major: majorJump(current, latest.version), orchestratorV2: protocol !== 2 && !newer(V2, latest.version) }
          : null,
        sessions,
        terminals,
        updating: !!last?.running,
        run: last,
        log: runLog(logText),
        // Versions to go back to: kept by T3, older than the one running.
        previous: kept.filter(v => newer(current, v)),
        notesUrl: latest ? NOTES(latest.version) : null,
      };
    },
    /**
     * Starts the update to `version`: the newest stable one, or one kept in runtime/versions
     * (going back). The person saw that version in the dialog, so a newer release meanwhile
     * does not change what happens.
     */
    async update({ version } = {}) {
      const v = String(version ?? '');
      if (!STABLE.test(v)) throw fail('T3_VERSION', 'Elige una versión de T3', 400);
      const [last, releases, kept] = await Promise.all([lastRun(), stableReleases(fetchImpl).catch(() => null), installed()]);
      if (last?.running) throw fail('T3_UPDATING', 'Ya se está actualizando T3', 409);
      const known = releases?.some(r => r.version === v) || kept.includes(v);
      if (!known) throw fail('T3_VERSION', 'Esa versión de T3 no está publicada ni guardada en este servidor', 400);
      if (command) {
        // The lab: its own command, detached (it restarts the lab's T3, which must outlive this app).
        const child = spawnImpl('sh', ['-c', `${command} ${v} >> '${logFile.replace(/'/g, "'\\''")}' 2>&1`], { detached: true, stdio: 'ignore' });
        child.unref();
        return { started: true, version: v };
      }
      const started = await run(systemctl, ['--user', 'start', '--no-block', UNIT(v)]);
      if (!started.ok) throw fail('T3_UPDATE', 'No se pudo iniciar la actualización de T3', 500);
      return { started: true, version: v };
    },
  };
}

/** Normalizes Fetch Headers or a plain header map to a lowercase-keyed plain object. */
function plainHeaders(headers) {
  if (!headers) return {};
  if (typeof headers.entries === 'function') {
    const out = {};
    for (const [k, v] of headers.entries()) out[k.toLowerCase()] = v;
    return out;
  }
  const out = {};
  for (const [k, v] of Object.entries(headers)) out[k.toLowerCase()] = v;
  return out;
}

/** Splits CSP header value(s) into comma-separated policies of trimmed directives. */
function cspPolicies(value) {
  const list = Array.isArray(value) ? value : value == null ? [] : [value];
  return list
    .flatMap(item => String(item).split(','))
    .map(raw => raw.split(';').map(d => d.trim()).filter(Boolean))
    .filter(d => d.length);
}

/** Whether a <meta http-equiv="Content-Security-Policy"> tag appears in the HTML. */
const META_CSP = /<meta\b[^>]*http-equiv\s*=\s*["']?content-security-policy["']?[^>]*>/i;

/**
 * What the app needs from T3, checked after an update (or on demand): T3 answers; the app's token is
 * valid and holds what a sign-in link needs; a sign-in link is made; the app's commands get through
 * (a chat made, a message sent: their way is tried, nothing is sent); the chats are read from T3's
 * database; the bridge can inject into T3's page (canInject, no <meta> CSP, bridge fits); and framing
 * headers (X-Frame-Options, CSP frame-ancestors) rewrite cleanly for the Asistente iframe.
 * `chats`: server/t3chats.mjs; `user`: the admin checking; `appOrigin`: the app's public origin.
 * Each check: { id, ok, detail }.
 */
export async function t3Checks({ t3, admin, chats, user, appOrigin, fetchImpl = fetch, WebSocketImpl = globalThis.WebSocket }) {
  const checks = [];
  const check = async (id, fn) => {
    try {
      const detail = await fn();
      checks.push({ id, ok: true, detail: String(detail ?? '') });
    } catch (e) {
      checks.push({ id, ok: false, detail: String(e.message ?? e).slice(0, 300) });
    }
  };
  const running = await admin.describe();
  await check('answers', () => {
    if (!running) throw new Error('T3 does not answer');
    return `${running.serverVersion} · protocol ${running.orchestrationProtocolVersion}`;
  });
  await check('token', async () => {
    const token = (await readFile(t3.tokenFile, 'utf8')).trim();
    const r = await fetchImpl(`${t3.local}/api/auth/session`, { headers: { authorization: `Bearer ${token}` }, signal: AbortSignal.timeout(5000) });
    const session = r.ok ? await r.json() : null;
    if (!session?.authenticated) throw new Error(`T3 does not accept the app's token (${r.status})`);
    const held = session.permissions ?? session.scopes ?? [];
    // V2's client set: a token issued by an older T3 lacks part of it, and sign-in links get only what it has.
    const missing = running?.orchestrationProtocolVersion >= 2 ? ['settings:write', 'providers:manage', 'filesystem:read'].filter(s => !held.includes(s)) : [];
    if (missing.length) throw new Error(`the token comes from an older T3: sign-in links lack ${missing.join(', ')} (T3 shows «Permissions have changed»)`);
    return `${held.length} permissions`;
  });
  await check('pairing', async () => {
    const link = await t3Pairing(t3, user, { fetchImpl });
    return `link until ${link.expiresAt}`;
  });
  await check('commands', async () => {
    const way = await t3CommandWay({ local: t3.local, tokenFile: t3.tokenFile, fetchImpl, WebSocketImpl });
    return way === 'socket' ? 'over its socket (V2)' : 'over HTTP (V1)';
  });
  await check('chats', () => {
    chats?.reopen?.();
    if (!chats?.available) throw new Error("T3's database cannot be read");
    const own = chats.projectOf(user.username, { fresh: true });
    const list = own ? chats.chatsOf(user.username, 500) : [];
    const owner = list[0] ? chats.ownerOf(list[0].id) : null;
    if (list[0] && owner !== user.username) throw new Error(`your latest chat reads as ${owner ?? 'nobody'}'s`);
    return `${chats.file?.split('/').pop() ?? ''} · ${own ? `your project, ${list.length} chats` : 'no project of yours yet'}`;
  });
  let pageSnap = null;
  const loadPage = async () => {
    if (pageSnap) return pageSnap;
    const r = await fetchImpl(`${t3.local}/`, {
      headers: { accept: 'text/html', 'accept-encoding': 'identity' },
      signal: AbortSignal.timeout(5000),
    });
    const status = r.status ?? (r.ok ? 200 : 500);
    const headers = r.headers ? plainHeaders(r.headers) : { 'content-type': 'text/html' };
    const html = await r.text();
    pageSnap = { status, headers, html };
    return pageSnap;
  };
  let originForCheck = 'https://app.invalid';
  let invalidExplicitOrigin = false;
  if (appOrigin !== undefined) {
    let parsedOrigin = '';
    try {
      parsedOrigin = appOrigin ? new URL(appOrigin).origin : '';
    } catch {
      parsedOrigin = '';
    }
    if (validHttpOrigin(parsedOrigin)) originForCheck = parsedOrigin;
    else invalidExplicitOrigin = true;
  }
  await check('page', async () => {
    const { status, headers, html } = await loadPage();
    if (invalidExplicitOrigin) throw new Error('APP_PUBLIC_URL is unset or invalid: bridge script injection is disabled');
    if (!canInject(status, headers)) {
      if (status !== 200) throw new Error(`T3's page returned HTTP ${status}`);
      const ct = String(headers['content-type'] ?? '');
      if (!/^text\/html/i.test(ct)) throw new Error(`T3's page is ${ct || 'missing content-type'}, not text/html`);
      const enc = headers['content-encoding'];
      if (enc && String(enc).toLowerCase() !== 'identity')
        throw new Error(`T3's page is compressed (${enc}) despite accept-encoding: identity`);
      throw new Error(`T3's Content-Security-Policy blocks bridge script injection (${headers['content-security-policy']})`);
    }
    if (META_CSP.test(html)) throw new Error("T3's page carries a <meta> Content-Security-Policy that may block the bridge script");
    if (injectBridge(html, originForCheck) === html) throw new Error("the bridge's script does not fit T3's page");
    return 'bridge fits';
  });
  await check('framing', async () => {
    const { status, headers } = await loadPage();
    if (status !== 200) throw new Error(`T3's page returned HTTP ${status}`);
    const hadXfo = 'x-frame-options' in headers;
    const hadFrameAncestors = cspPolicies(headers['content-security-policy']).some(directives =>
      directives.some(d => d.split(/\s+/, 1)[0].toLowerCase() === 'frame-ancestors'),
    );
    if (invalidExplicitOrigin) {
      if (hadXfo || hadFrameAncestors)
        throw new Error("APP_PUBLIC_URL is unset or invalid: the bridge cannot rewrite T3's framing headers for the iframe");
      return 'no framing headers (APP_PUBLIC_URL unset)';
    }
    const rewritten = allowFraming(headers, originForCheck);
    const leftXfo = Object.keys(rewritten).find(k => k.toLowerCase() === 'x-frame-options');
    if (leftXfo) throw new Error(`X-Frame-Options (${rewritten[leftXfo]}) still blocks iframe embedding after rewrite`);
    const cspOut = Object.entries(rewritten)
      .filter(([k]) => k.toLowerCase() === 'content-security-policy')
      .map(([, v]) => v);
    for (const directives of cspPolicies(cspOut)) {
      for (const d of directives) {
        if (d.split(/\s+/, 1)[0].toLowerCase() !== 'frame-ancestors') continue;
        const tokens = d.split(/\s+/).slice(1);
        if (!tokens.includes(originForCheck))
          throw new Error(`CSP frame-ancestors (${d}) still blocks ${originForCheck} after rewrite`);
      }
    }
    return hadXfo || hadFrameAncestors ? 'iframe allowed (framing headers rewritten)' : 'iframe allowed (no framing headers)';
  });
  return checks;
}

/** Scopes only the admin token keeps (people's links never get them); review:write is not granted from 0.0.46. */
const ADMIN_ONLY = new Set(['access:read', 'access:write', 'relay:write', 'review:write']);

/**
 * A one-time T3 Code sign-in link for the person, made with the broker's admin
 * token (T3 is a stock install: its own pairing flow, nothing patched). The link
 * gets T3's own set for a client: on 0.0.45 the five scopes asked for by name
 * before, from 0.1.0 (V2) finer ones too. An admin token issued by an older T3 lacks
 * those (T3 answers 403): then the link gets the client scopes the token has
 * (the update issues the token again, scripts/t3-update.mjs).
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
