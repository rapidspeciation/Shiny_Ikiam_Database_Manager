#!/usr/bin/env node
// Updates T3 Code (a stock install) to one version, as the Asistente tab's «Actualizar T3» asks
// (server/t3admin.mjs). On the server it is the user unit ithomiini-t3-update@<version>.service
// (deploy/); in the lab, tools/lab/t3.sh --update. In order:
//  1. a copy of T3's database (the file the running T3 uses: statev2.sqlite from 0.1.0, state.sqlite
//     before), its settings and service state, in <home>/backups/<time>-<from version>; the newest
//     --keep stay. T3's own `t3 update` keeps none;
//  2. `t3 update <version> --yes` (--allow-downgrade to go back to a version still in runtime/versions):
//     it downloads the version, moves ~/.local/bin/t3 to it and restarts T3's service;
//  3. (the lab, which has no service) --restart, the command that starts T3 again;
//  4. waits until T3 answers on the new version (/.well-known/t3/environment). The first start of
//     0.1.0 copies the old database into statev2.sqlite before it answers;
//  5. (--token-file) the app's token for T3 issued again by the version now running, in its database
//     (a token from an older T3 lacks the newer one's permissions, and its sign-in links with it; after
//     going back, the older database does not know the newer token), and the earlier tokens of the
//     app (same --token-label) revoked.
// Each step goes to the log (stdout, appended by the unit) and to <home>/userdata/logs/t3-update.json,
// which the app reads to show the progress.
//   node scripts/t3-update.mjs [<version>] [--home ~/.t3] [--t3 ~/.local/bin/t3] [--local http://127.0.0.1:3773]
//        [--restart '<command>'] [--keep 3] [--wait 900] [--token-file <file> --token-label ithomiini-broker]
// Without <version>: the newest stable release.
import { spawn } from 'node:child_process';
import { chmodSync, copyFileSync, existsSync, mkdirSync, readFileSync, readdirSync, renameSync, rmSync, statSync, statfsSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';
import { DatabaseSync } from 'node:sqlite';
import { parseArgs } from 'node:util';

const STABLE = /^\d+\.\d+\.\d+$/;
const { values: opts, positionals } = parseArgs({
  allowPositionals: true,
  options: {
    home: { type: 'string', default: process.env.T3CODE_HOME || join(process.env.HOME, '.t3') },
    t3: { type: 'string', default: join(process.env.HOME, '.local/bin/t3') },
    local: { type: 'string', default: 'http://127.0.0.1:3773' },
    restart: { type: 'string' },
    keep: { type: 'string', default: '3' },
    wait: { type: 'string', default: '900' },
    'token-file': { type: 'string' },
    'token-label': { type: 'string', default: 'ithomiini-broker' },
  },
});
const home = opts.home;
const statusFile = join(home, 'userdata', 'logs', 't3-update.json');
const status = { pid: process.pid, startedAt: new Date().toISOString(), step: 'checking', from: null, to: null, backup: null, steps: [] };
const say = text => console.log(`[${new Date().toISOString()}] ${text}`);
function save(patch) {
  Object.assign(status, patch);
  try {
    mkdirSync(join(home, 'userdata', 'logs'), { recursive: true });
    writeFileSync(`${statusFile}.tmp`, JSON.stringify(status, null, 1));
    renameSync(`${statusFile}.tmp`, statusFile);
  } catch (e) {
    say(`(progress not saved: ${e.message})`);
  }
}
/** A step done: its name, how long it took and what it found. */
function done(step, started, detail = '') {
  status.steps.push({ step, seconds: Math.round((Date.now() - started) / 100) / 10, detail });
  say(`${step}: ${detail || 'ok'} (${Math.round((Date.now() - started) / 1000)} s)`);
}
function fail(message) {
  say(`FAILED: ${message}`);
  save({ step: 'failed', error: message, endedAt: new Date().toISOString() });
  process.exit(1);
}
const order = (a, b) => {
  const x = a.split('.').map(Number);
  const y = b.split('.').map(Number);
  for (let i = 0; i < 3; i++) if (x[i] !== y[i]) return x[i] - y[i];
  return 0;
};

/** What the running T3 says of itself (null: it does not answer). */
async function running() {
  try {
    const r = await fetch(new URL('/.well-known/t3/environment', opts.local), { signal: AbortSignal.timeout(5000) });
    return r.ok ? await r.json() : null;
  } catch {
    return null;
  }
}
/** The version T3's service runs (runtime/service-state.json), when T3 does not answer. */
function selected() {
  try {
    return JSON.parse(readFileSync(join(home, 'runtime', 'service-state.json'), 'utf8')).activeVersion ?? null;
  } catch {
    return null;
  }
}
async function newestStable() {
  const r = await fetch('https://api.github.com/repos/pingdotgg/t3code/releases?per_page=30', {
    headers: { accept: 'application/vnd.github+json', 'user-agent': 'ithomiini-app' },
    signal: AbortSignal.timeout(20_000),
  });
  if (!r.ok) throw new Error(`GitHub answered ${r.status}`);
  const tag = (await r.json()).map(x => String(x.tag_name ?? '').replace(/^v/, '')).find(v => STABLE.test(v));
  if (!tag) throw new Error('no stable release on GitHub');
  return tag;
}

/** The database the running T3 uses: statev2.sqlite once 0.1.0 made it (unless an older T3 runs again). */
function databaseOf(protocol) {
  const v2 = join(home, 'userdata', 'statev2.sqlite');
  const v1 = join(home, 'userdata', 'state.sqlite');
  if (protocol === 1 || !existsSync(v2)) return existsSync(v1) ? v1 : null;
  return v2;
}

/** A consistent copy of T3's database (VACUUM INTO reads it in one transaction while T3 runs) and its settings. */
function backup(from, protocol) {
  const db = databaseOf(protocol);
  const stamp = new Date().toISOString().replace(/[:.]/g, '-');
  const root = join(home, 'backups');
  const dir = join(root, `${stamp}-${from ?? 'unknown'}`);
  mkdirSync(dir, { recursive: true, mode: 0o700 });
  if (db) {
    const size = statSync(db).size;
    const free = statfsSync(root);
    if (free.bavail * free.bsize < size * 2 + 1e9) throw new Error(`not enough disk for a copy of ${db} (${Math.round(size / 1e6)} MB)`);
    const source = new DatabaseSync(db, { readOnly: true, timeout: 10_000 });
    try {
      source.exec(`VACUUM INTO '${join(dir, db.split('/').pop()).replace(/'/g, "''")}'`);
    } finally {
      source.close();
    }
  }
  for (const file of ['userdata/settings.json', 'userdata/keybindings.json', 'userdata/environment-id', 'runtime/service-state.json'])
    if (existsSync(join(home, file))) copyFileSync(join(home, file), join(dir, file.split('/').pop()));
  // The app's token as this version knows it (step 5 replaces it): put back by hand, it works again after going back.
  if (opts['token-file'] && existsSync(opts['token-file'])) {
    copyFileSync(opts['token-file'], join(dir, 'app-token'));
    chmodSync(join(dir, 'app-token'), 0o600);
  }
  // The newest copies stay (each about the size of the database).
  const keep = Math.max(1, Number(opts.keep) || 3);
  const ours = readdirSync(root)
    .filter(name => /^\d{4}-\d\d-\d\dT\d\d-\d\d-\d\d-\d{3}Z-/.test(name))
    .sort()
    .reverse();
  for (const old of ours.slice(keep)) rmSync(join(root, old), { recursive: true, force: true });
  return { dir, database: db };
}

/** Runs a command, its output into the log; resolves with its exit code. */
function run(command, args, env) {
  return new Promise(resolve => {
    const child = spawn(command, args, { env, stdio: ['ignore', 'inherit', 'inherit'] });
    child.on('error', e => (say(`${command}: ${e.message}`), resolve(127)));
    child.on('exit', code => resolve(code ?? 1));
  });
}
/** A command's output (not logged: it may be a token); rejects when it fails. */
function capture(command, args, env) {
  return new Promise((resolve, reject) => {
    const child = spawn(command, args, { env, stdio: ['ignore', 'pipe', 'pipe'] });
    let out = '';
    let err = '';
    child.stdout.on('data', d => (out += d));
    child.stderr.on('data', d => (err += d));
    child.on('error', reject);
    child.on('exit', code => (code === 0 ? resolve(out) : reject(new Error(`${args.slice(0, 3).join(' ')}: ${err.trim().split('\n').pop() || `code ${code}`}`))));
  });
}

/**
 * The app's token issued by the T3 now running (in the database it uses), checked, written over the
 * old one, and the app's earlier tokens (same label) revoked. Returns how many were revoked.
 */
async function newToken(file, label) {
  const env = t3Env();
  const token = (await capture(opts.t3, ['auth', 'session', 'issue', '--base-dir', home, '--label', label, '--ttl', '3650d', '--token-only'], env)).trim();
  if (!/^\S{20,}$/.test(token)) throw new Error('T3 gave no token');
  const r = await fetch(new URL('/api/auth/session', opts.local), { headers: { authorization: `Bearer ${token}` }, signal: AbortSignal.timeout(10_000) });
  if (!r.ok || !(await r.json()).authenticated) throw new Error(`T3 does not accept the token it issued (${r.status})`);
  writeFileSync(`${file}.new`, `${token}\n`, { mode: 0o600 });
  renameSync(`${file}.new`, file);
  const sessions = JSON.parse(await capture(opts.t3, ['auth', 'session', 'list', '--base-dir', home, '--json'], env));
  const ours = (Array.isArray(sessions) ? sessions : (sessions.sessions ?? []))
    .filter(x => x.method === 'bearer-access-token' && x.client?.label === label)
    .sort((a, b) => String(b.issuedAt).localeCompare(String(a.issuedAt)));
  for (const old of ours.slice(1)) await capture(opts.t3, ['auth', 'session', 'revoke', '--base-dir', home, old.sessionId], env);
  return ours.length - 1;
}

/** The environment T3's own commands get: this home, and nothing of another T3 (a shell opened by a T3 chat carries its settings). */
function t3Env() {
  const env = Object.fromEntries(Object.entries(process.env).filter(([k]) => !/^(T3CODE_|T3_SERVICE_|T3_BOOT_)/.test(k)));
  return { ...env, T3CODE_HOME: home };
}

const asked = positionals[0];
if (asked && !STABLE.test(asked)) fail(`'${asked}' is not a stable version (like 0.1.0)`);
save({});
let started = Date.now();
const before = await running();
const from = before?.serverVersion ?? selected();
const protocol = before?.orchestrationProtocolVersion ?? null;
let to;
try {
  to = asked ?? (await newestStable());
} catch (e) {
  fail(`the newest version could not be checked: ${e.message}`);
}
const back = !!from && STABLE.test(from) && order(to, from) < 0;
save({ from, to, back, protocolBefore: protocol });
say(`T3 Code ${from ?? '(not answering)'} → ${to}${back ? ' (back to an earlier version)' : ''}; home ${home}`);
if (from === to) {
  save({ step: 'done', endedAt: new Date().toISOString(), note: 'already on this version' });
  say('Already on this version: nothing to do.');
  process.exit(0);
}
if (back && !existsSync(join(home, 'runtime', 'versions', to, '.install-complete')))
  fail(`${to} is not in ${join(home, 'runtime', 'versions')}: only a version T3 had before can be gone back to`);
done('check', started, `${from} → ${to}`);

save({ step: 'backup' });
started = Date.now();
try {
  const copy = backup(from, protocol);
  save({ backup: copy.dir });
  done('backup', started, copy.database ? `${copy.database} → ${copy.dir}` : `no database found; settings → ${copy.dir}`);
} catch (e) {
  fail(`the copy of T3's database failed, nothing was changed: ${e.message}`);
}

save({ step: 'installing' });
started = Date.now();
const code = await run(opts.t3, ['update', '--base-dir', home, '--yes', ...(back ? ['--allow-downgrade'] : []), to], t3Env());
if (code !== 0) fail(`t3 update ended with code ${code} (T3 keeps running ${from})`);
done('t3 update', started);

if (opts.restart) {
  save({ step: 'restarting' });
  started = Date.now();
  // No service here (the lab): this home's T3 is started again on the version `t3 update` installed.
  writeFileSync(join(home, 'runtime', 'service-state.json'), `${JSON.stringify({ protocol: 3, activeVersion: to }, null, 2)}\n`);
  const restarted = await run('sh', ['-c', opts.restart], t3Env());
  if (restarted !== 0) fail(`the restart command ended with code ${restarted}`);
  done('restart', started);
}

save({ step: 'waiting' });
started = Date.now();
const deadline = Date.now() + Number(opts.wait) * 1000;
let now = null;
while (Date.now() < deadline) {
  now = await running();
  if (now?.serverVersion === to) break;
  await new Promise(resolve => setTimeout(resolve, 2000));
}
if (now?.serverVersion !== to) fail(`T3 does not answer on ${to} after ${opts.wait} s (it says: ${now?.serverVersion ?? 'nothing'})`);
const v2 = join(home, 'userdata', 'statev2.sqlite');
const copied = protocol === 1 && now.orchestrationProtocolVersion >= 2 && existsSync(v2);
done('T3 answers', started, `${now.serverVersion}, protocol ${now.orchestrationProtocolVersion}${copied ? `; its chats copied into statev2.sqlite (${Math.round(statSync(v2).size / 1e6)} MB)` : ''}`);
save({ protocolAfter: now.orchestrationProtocolVersion });
if (opts['token-file']) {
  save({ step: 'token' });
  started = Date.now();
  try {
    const revoked = await newToken(opts['token-file'], opts['token-label']);
    done('token', started, `the app's token issued by ${to}${revoked ? `; ${revoked} older revoked` : ''}`);
  } catch (e) {
    // T3 itself is updated: the app's checks show what the old token still allows.
    say(`token: not issued again (${e.message}); the app keeps its old token`);
    save({ tokenError: e.message });
  }
}
save({ step: 'done', endedAt: new Date().toISOString() });
say('Done.');
