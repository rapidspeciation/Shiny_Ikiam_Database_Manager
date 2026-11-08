#!/usr/bin/env node
// The app's memory and speed on one lab instance (LOCAL_MODE, as tools/lab/app.sh runs it), measured on fixed
// workloads so a change can be compared with the run before it (README.md beside this file).
//
// Each repeat starts the app from a fresh copy of the lab's database (copy-on-write, in its own folder under
// ~/.cache/ithomiini-perf), runs the workloads, and stops it:
//   cold_start   spawn → listening → first sync done, then 20 s idle
//   pages        Chromium (Playwright): first load of Inicio, then Buscador, Colecta, Monitoreo, Fotos, Asistente, Inicio
//   search       /api/search for fixed queries, each twice (first and again)
//   sync         a whole-workbook sync (POST /api/sync, the lab's in-memory sheets)
//   tools        the assistant's tools over MCP: host_plants with an area and a contact sheet, maps_snapshot,
//                gallery_specimens, identify_species on one photo, rack_read_photos; each twice
//   offline      the offline page's model download (every part, as the page asks for them)
//   concurrent   5 clients replaying the pages' API calls for 30 s
// Meanwhile: every process of the app (server, identifier, ffmpeg…) sampled every 250 ms (RSS, swap, peak, CPU by
// thread name), and /health asked every 200 ms (how long it waits = how late the event loop is).
//   node tools/perf/app-bench/run.mjs --label baseline [--repeats 3] [--only pages,search] [--profile]
// --profile: one more start with --cpu-prof and --heap-prof (written when the app stops), summarised in the result.
// Never writes to the lab's own files: only to its run folder (and two photos named perf-bench-* in the lab
// T3's attachments folder, which the tools read the photos from).
import { spawn, spawnSync } from 'node:child_process';
import { createHash, randomBytes } from 'node:crypto';
import { copyFileSync, existsSync, mkdirSync, readFileSync, readdirSync, writeFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import { performance } from 'node:perf_hooks';
import { DatabaseSync } from 'node:sqlite';
import { fileURLToPath } from 'node:url';
import { summarizeCpuProfiles, summarizeHeapProfiles } from './profiles.mjs';

const HERE = resolve(dirname(fileURLToPath(import.meta.url)), '../../..');
const LAB = process.env.ITHOMIINI_LAB_DIR || join(homedir(), '.cache/ithomiini-lab');
const LAB_T3_HOME = process.env.LAB_T3_HOME || join(homedir(), '.t3-ithomiini-lab');
const IDENT = process.env.PERF_IDENTIFIER_DIR || join(homedir(), '.cache/ithomiini-perf/identifier');
const PLAYWRIGHT = process.env.LAB_PLAYWRIGHT || '/home/franz/.local/share/agent-tools/node_modules/playwright/index.mjs';

const argv = process.argv.slice(2);
const opt = (name, fallback) => {
  const i = argv.indexOf(name);
  return i < 0 ? fallback : argv[i + 1];
};
const has = name => argv.includes(name);
const LABEL = opt('--label', 'run');
const REPEATS = Number(opt('--repeats', 3));
const PORT = Number(opt('--port', 8805));
const PROXY_PORT = Number(opt('--proxy-port', 3786));
const ALL = ['cold_start', 'pages', 'search', 'sync', 'tools', 'offline', 'concurrent'];
const ONLY = (opt('--only', ALL.join(',')) || '').split(',').filter(Boolean);
const PROFILE = has('--profile');
// The app's NODE_OPTIONS (dev-app.sh's own, --max-old-space-size=2048, when not given), its unit's memory ceiling
// (the identifier runs in it too) and time limit, and any other setting (--env K=V, repeatable: an experiment's switch).
const NODE_OPTIONS = opt('--node-options', '');
const MEMORY_MAX = opt('--max', '5G');
const TTL = opt('--ttl', '30m');
const CPU_WEIGHT = opt('--cpu-weight', '');
const EXTRA_ENV = Object.fromEntries(argv.flatMap((a, i) => (a === '--env' ? [argv[i + 1].split(/=(.*)/s).slice(0, 2)] : [])));
const MIN_AVAILABLE_GB = Number(opt('--min-available-gb', 2.5));
// Before each start: wait (up to 30 min) until this much is available, so the app and the browser fit together
// (the app peaks near 3.5 GB in LOCAL_MODE) and the run does not push the PC into swap.
const WAIT_GB = Number(opt('--wait-memory-gb', 6.5));
const WAIT_ABSENT = opt('--wait-absent', 'memaudit_[0-9]+/harness');
const CONCURRENT_S = Number(opt('--concurrent-seconds', 30));
const CLIENTS = Number(opt('--clients', 5));
const URL_ = `http://127.0.0.1:${PORT}`;
const stamp = new Date().toISOString().replace(/[-:]/g, '').replace(/\.\d+Z$/, 'Z');
const OUT = resolve(opt('--out', join(homedir(), '.cache/ithomiini-perf', `${LABEL}-${stamp}`)));
mkdirSync(OUT, { recursive: true, mode: 0o700 });

const log = (...a) => console.log(new Date().toISOString().slice(11, 19), ...a);
const sleep = ms => new Promise(r => setTimeout(r, ms));
const quantile = (xs, q) => {
  if (!xs.length) return null;
  const s = [...xs].sort((a, b) => a - b);
  return s[Math.min(s.length - 1, Math.floor(q * s.length))];
};
const r1 = n => (n == null ? null : Math.round(n * 10) / 10);
const stats = xs => ({ n: xs.length, p50: r1(quantile(xs, 0.5)), p95: r1(quantile(xs, 0.95)), max: r1(xs.length ? Math.max(...xs) : null) });

function memAvailableGb() {
  const m = /MemAvailable:\s+(\d+)/.exec(readFileSync('/proc/meminfo', 'utf8'));
  return m ? Number(m[1]) / 1024 / 1024 : 0;
}
function needMemory(what, gb = MIN_AVAILABLE_GB) {
  const avail = memAvailableGb();
  if (avail < gb) throw new Error(`${what}: MemAvailable ${avail.toFixed(2)} GB < ${gb} GB; not started`);
  return avail;
}

// ---------- the app's processes ----------
const TICK = 100; // USER_HZ
const PAGE_KB = 4;
function readProc(pid) {
  try {
    const status = readFileSync(`/proc/${pid}/status`, 'utf8');
    const kb = key => Number(new RegExp(`^${key}:\\s+(\\d+)`, 'm').exec(status)?.[1] ?? 0);
    const stat = readFileSync(`/proc/${pid}/stat`, 'utf8');
    const f = stat.slice(stat.lastIndexOf(')') + 2).split(' ');
    const cmd = readFileSync(`/proc/${pid}/cmdline`, 'utf8').replaceAll('\0', ' ');
    return {
      pid,
      kind: kindOf(cmd),
      rssKb: kb('VmRSS'),
      swapKb: kb('VmSwap'),
      hwmKb: kb('VmHWM'),
      anonKb: kb('RssAnon'),
      threads: kb('Threads'),
      cpuS: (Number(f[11]) + Number(f[12])) / TICK,
      childCpuS: (Number(f[13]) + Number(f[14])) / TICK,
    };
  } catch {
    return null;
  }
}
function kindOf(cmd) {
  if (cmd.includes('server/index.mjs')) return 'server';
  if (cmd.includes('identifier/worker.py')) return 'identifier';
  if (cmd.includes('ffmpeg')) return 'ffmpeg';
  if (/python/.test(cmd)) return 'python';
  return cmd.split(' ')[0].split('/').pop() || 'other';
}
function children(pid) {
  const out = [];
  try {
    for (const tid of readdirSync(`/proc/${pid}/task`)) {
      const c = readFileSync(`/proc/${pid}/task/${tid}/children`, 'utf8').trim();
      if (c) out.push(...c.split(/\s+/).map(Number));
    }
  } catch {}
  return out;
}
function tree(root) {
  const seen = [];
  const walk = pid => {
    const p = readProc(pid);
    if (!p) return;
    // A node child of the app (the photo reader, the query worker) is not the server itself.
    if (p.kind === 'server' && pid !== root) p.kind = 'node-child';
    seen.push(p);
    for (const c of children(pid)) walk(c);
  };
  walk(root);
  return seen;
}
/** CPU seconds of the server's threads by name (main thread, workers, GC, libuv pool…). */
function threadCpu(pid) {
  const by = {};
  try {
    for (const tid of readdirSync(`/proc/${pid}/task`)) {
      try {
        const comm = readFileSync(`/proc/${pid}/task/${tid}/comm`, 'utf8').trim();
        const stat = readFileSync(`/proc/${pid}/task/${tid}/stat`, 'utf8');
        const f = stat.slice(stat.lastIndexOf(')') + 2).split(' ');
        const name = comm.replace(/\d+$/, '#');
        by[name] = (by[name] ?? 0) + (Number(f[11]) + Number(f[12])) / TICK;
      } catch {}
    }
  } catch {}
  return by;
}

/** Samples the app's processes every 250 ms; `mark(name)` starts a phase, `phase()` summarises it. */
function sampler(rootPid, app = null) {
  const samples = [];
  const cpuSeen = new Map(); // pid → { kind, cpuS } (last seen, so exited children still count)
  let current = null;
  const take = () => {
    const procs = tree(rootPid);
    const t = performance.now();
    const row = { t, phase: current?.name ?? null, procs: procs.map(p => ({ pid: p.pid, kind: p.kind, rssKb: p.rssKb, swapKb: p.swapKb })) };
    // The unit's own count (tools/lab/dev-app.sh): every process of the app, page cache of its files included.
    // anon: the app's own memory (heap, buffers); file: page cache of what it read (the snapshot, the models), which
    // the kernel takes back first.
    if (app?.cgroup) row.cgroupKb = { mem: cgroupKb(app, 'memory.current'), swap: cgroupKb(app, 'memory.swap.current'), ...cgroupStat(app) };
    for (const p of procs) cpuSeen.set(p.pid, { kind: p.kind, cpuS: p.cpuS, hwmKb: Math.max(p.hwmKb, cpuSeen.get(p.pid)?.hwmKb ?? 0) });
    samples.push(row);
    return row;
  };
  const timer = setInterval(take, 250);
  const cpuByKind = () => {
    const by = {};
    for (const { kind, cpuS } of cpuSeen.values()) by[kind] = (by[kind] ?? 0) + cpuS;
    return by;
  };
  const begin = name => {
    take();
    current = { name, t0: performance.now(), from: samples.length - 1, cpu0: cpuByKind(), threads0: threadCpu(rootPid) };
  };
  const end = () => {
    take();
    const rows = samples.slice(current.from);
    const total = r => r.procs.reduce((a, p) => a + p.rssKb + p.swapKb, 0);
    const kinds = new Set(rows.flatMap(r => r.procs.map(p => p.kind)));
    const per = {};
    for (const k of kinds) {
      const rss = rows.map(r => r.procs.filter(p => p.kind === k).reduce((a, p) => a + p.rssKb, 0));
      const swap = rows.map(r => r.procs.filter(p => p.kind === k).reduce((a, p) => a + p.swapKb, 0));
      per[k] = { peakRssMb: r1(Math.max(...rss) / 1024), endRssMb: r1(rss.at(-1) / 1024), peakSwapMb: r1(Math.max(...swap) / 1024) };
    }
    const cpu1 = cpuByKind();
    const threads1 = threadCpu(rootPid);
    const cpu = {};
    for (const k of Object.keys(cpu1)) cpu[k] = r1((cpu1[k] ?? 0) - (current.cpu0[k] ?? 0));
    const threads = {};
    for (const k of Object.keys(threads1)) {
      const d = threads1[k] - (current.threads0[k] ?? 0);
      if (d >= 0.05) threads[k] = r1(d);
    }
    const out = {
      seconds: r1((performance.now() - current.t0) / 1000),
      peakTotalMb: r1(Math.max(...rows.map(total)) / 1024),
      ...(rows.some(r => r.cgroupKb?.mem != null)
        ? {
            peakCgroupMb: r1(Math.max(...rows.map(r => (r.cgroupKb?.mem ?? 0) + (r.cgroupKb?.swap ?? 0))) / 1024),
            peakAnonMb: r1(Math.max(...rows.map(r => (r.cgroupKb?.anon ?? 0) + (r.cgroupKb?.swap ?? 0))) / 1024),
            endAnonMb: r1(((rows.at(-1).cgroupKb?.anon ?? 0) + (rows.at(-1).cgroupKb?.swap ?? 0)) / 1024),
          }
        : {}),
      endTotalMb: r1(total(rows.at(-1)) / 1024),
      processes: per,
      cpuS: cpu,
      serverThreadsCpuS: threads,
    };
    current = null;
    return out;
  };
  return {
    begin,
    end,
    take,
    samples,
    peakHwmMb: () => Object.fromEntries([...cpuSeen.entries()].map(([pid, v]) => [`${v.kind}:${pid}`, r1(v.hwmKb / 1024)])),
    stop: () => clearInterval(timer),
  };
}

/** /health every 200 ms: how long each answer takes is how late the event loop is. */
function prober() {
  let lat = [];
  let errors = 0;
  let on = true;
  (async () => {
    while (on) {
      const t = performance.now();
      try {
        const r = await fetch(`${URL_}/health`, { signal: AbortSignal.timeout(30_000) });
        await r.arrayBuffer();
        lat.push(performance.now() - t);
      } catch {
        errors++;
      }
      await sleep(Math.max(0, 200 - (performance.now() - t)));
    }
  })();
  return {
    take() {
      const out = { ...stats(lat), p99: r1(quantile(lat, 0.99)), errors };
      lat = [];
      errors = 0;
      return out;
    },
    stop: () => (on = false),
  };
}

// ---------- the run folder ----------
function baseCopy() {
  const base = join(OUT, 'base');
  if (existsSync(join(base, 'app.sqlite'))) return base;
  mkdirSync(base, { recursive: true });
  // A consistent copy even while the lab's own app runs (VACUUM INTO reads one snapshot of the database).
  for (const file of ['app.sqlite', 'sheets.sqlite']) {
    const src = join(LAB, 'app', file);
    if (!existsSync(src)) continue;
    const db = new DatabaseSync(src, { readOnly: true });
    db.exec(`VACUUM INTO '${join(base, file).replaceAll("'", "''")}'`);
    db.close();
  }
  // The bench's own token for the assistant's tools (MCP), for the admin `lab`.
  const token = randomBytes(24).toString('base64url');
  const db = new DatabaseSync(join(base, 'app.sqlite'));
  const user = db.prepare("SELECT id FROM users WHERE username = 'lab' AND active = 1").get();
  if (!user) throw new Error('No active user lab in the lab database');
  db.prepare('INSERT INTO ai_tokens (token_hash,user_id,label,created_at) VALUES (?,?,?,?)').run(
    createHash('sha256').update(token).digest('hex'),
    user.id,
    'perf bench',
    new Date().toISOString(),
  );
  db.close();
  writeFileSync(join(base, 'token'), token, { mode: 0o600 });
  return base;
}
function repeatDir(name) {
  const base = baseCopy();
  const dir = join(armOut ?? OUT, name);
  // tools/lab/dev-app.sh's folder: app.sqlite, photos/ and app.log in it; the lab's admin login, so no setup.
  const app = dir;
  mkdirSync(app, { recursive: true });
  for (const file of ['app.sqlite', 'sheets.sqlite']) if (existsSync(join(base, file))) cp(join(base, file), join(app, file));
  for (const sub of ['photos', 'drive-photos', 'clutch-photos', 'reference-photos', 'gallery-public', 'knowledge'])
    if (existsSync(join(LAB, 'app', sub))) cp(join(LAB, 'app', sub), join(app, sub));
  copyFileSync(join(LAB, 'credentials.json'), join(app, 'credentials.json'));
  // The identifier's files (read only): the lab's, with the offline page's head, small model and parts as on the
  // server (~/.cache/ithomiini-perf/identifier, README.md), else the lab's alone.
  const ident = existsSync(IDENT) ? IDENT : join(LAB, 'identifier');
  return { dir, app, ident, token: readFileSync(join(base, 'token'), 'utf8') };
}
function cp(src, dst) {
  const r = spawnSync('cp', ['-a', '--reflink=auto', src, dst]);
  if (r.status !== 0) throw new Error(`cp ${src}: ${r.stderr}`);
}
function attachments() {
  const dir = join(LAB_T3_HOME, 'userdata', 'attachments');
  mkdirSync(dir, { recursive: true });
  const butterfly = join(LAB, 'test-batch', 'IMG_5001.JPG');
  const files = { rack: 'perf-bench-rack.jpg', butterfly: 'perf-bench-butterfly.jpg' };
  if (!existsSync(join(dir, files.rack))) copyFileSync(join(HERE, 'tests/fixtures/rack-bottom-fk00274230.jpg'), join(dir, files.rack));
  if (!existsSync(join(dir, files.butterfly)) && existsSync(butterfly)) copyFileSync(butterfly, join(dir, files.butterfly));
  return files;
}

// ---------- the app ----------
// Started by tools/lab/dev-app.sh (its own user unit: memory ceiling, time limit, the memory check, LOCAL_MODE on
// the lab's snapshot, one tools worker, 2 GB heap), with what the workloads need on top: the identifier, the test
// photos, the lab T3 (Asistente tab, chat attachments) and, for --profile, the profilers.
const UNIT = `ithomiini-dev-${PORT}`;
// The checkout whose app is measured (--app-dir, or each arm's in turn): its server and its dev-app.sh.
let APP = HERE;
let armOut = null;
const devApp = () => join(APP, 'tools/lab/dev-app.sh');
function extraEnv(run, profileDir) {
  return {
    IDENTIFIER_DIR: run.ident,
    DRIVE_LOCAL_PHOTOS: join(LAB, 'test-batch'),
    ITHOMIINI_LAB_DRIVE_SANDBOX: '0',
    ITHOMIINI_T3_URL: `http://127.0.0.1:${PROXY_PORT}`,
    ITHOMIINI_T3_LOCAL: 'http://127.0.0.1:3775',
    ITHOMIINI_T3_PROXY_PORT: String(PROXY_PORT),
    ITHOMIINI_T3_ADMIN_TOKEN_FILE: join(LAB, 't3-admin-token'),
    ITHOMIINI_T3_HOME: LAB_T3_HOME,
    ITHOMIINI_T3_PROVISION: 'direct',
    ITHOMIINI_T3_WORKSPACES: join(LAB_T3_HOME, 'workspaces'),
    ITHOMIINI_SHARED: run.app,
    ITHOMIINI_MCP_URL: `${URL_}/api/ai/mcp`,
    ITHOMIINI_SRC: APP,
    ITHOMIINI_DOCS: join(APP, 'docs'),
    ITHOMIINI_KNOWLEDGE: join(run.app, 'knowledge'),
    ...(NODE_OPTIONS || profileDir
      ? {
          NODE_OPTIONS: [
            NODE_OPTIONS || '--max-old-space-size=2048',
            ...(profileDir ? ['--cpu-prof', `--cpu-prof-dir=${profileDir}`, '--heap-prof', `--heap-prof-dir=${profileDir}`] : []),
          ].join(' '),
        }
      : {}),
    ...EXTRA_ENV,
  };
}
const mainPid = () => Number(spawnSync('systemctl', ['--user', 'show', '-p', 'MainPID', '--value', `${UNIT}.service`], { encoding: 'utf8' }).stdout.trim() || 0);
async function startApp(run, { profileDir = null } = {}) {
  needMemory('lab app');
  const busy = spawnSync('ss', ['-ltn', `sport = :${PORT}`], { encoding: 'utf8' }).stdout.split('\n').length > 2;
  if (busy) throw new Error(`port ${PORT} is in use`);
  const args = ['--port', String(PORT), '--dir', run.app, '--ttl', TTL, '--max', MEMORY_MAX];
  // The workbook's rows file (server/file-sheets.mjs, where the arm has it) made before the clock starts.
  const fileSheets = join(APP, 'server/file-sheets.mjs');
  if (existsSync(fileSheets)) spawnSync(process.execPath, [fileSheets, 'ensure', join(LAB, 'snapshot.json')], { stdio: 'ignore' });
  for (const [k, v] of Object.entries(extraEnv(run, profileDir))) args.push('--env', `${k}=${v}`);
  const t0 = performance.now();
  const launcher = spawn(devApp(), args, { cwd: APP, stdio: ['ignore', 'pipe', 'pipe'] });
  const out = [];
  launcher.stdout.on('data', d => out.push(d));
  launcher.stderr.on('data', d => out.push(d));
  const launched = new Promise(r => launcher.once('exit', code => r(code)));
  let pid = 0;
  while (!(pid = mainPid())) {
    if (launcher.exitCode !== null) throw new Error(`dev-app.sh: ${Buffer.concat(out).toString().trim()}`);
    // dev-app.sh waits its turn behind another start (its start lock) before it starts the unit.
    if (performance.now() - t0 > 300_000) {
      launcher.kill();
      throw new Error('dev-app.sh: no process after 5 min');
    }
    await sleep(50);
  }
  // On the server (--cpu-weight 20): the live app first when both want the cores.
  if (CPU_WEIGHT) spawnSync('systemctl', ['--user', 'set-property', '--runtime', `${UNIT}.service`, `CPUWeight=${CPU_WEIGHT}`]);
  const cg = readFileSync(`/proc/${pid}/cgroup`, 'utf8').trim().split('\n').find(l => l.startsWith('0::'))?.slice(3);
  return { pid, t0, launched, launcherOut: out, cgroup: cg ? join('/sys/fs/cgroup', cg) : null };
}
async function waitReady(app) {
  let listenS = null;
  for (;;) {
    if (!existsSync(`/proc/${app.pid}`)) throw new Error(`app exited during start (see ${UNIT}'s app.log)`);
    try {
      const h = await (await fetch(`${URL_}/health`, { signal: AbortSignal.timeout(5000) })).json();
      listenS ??= (performance.now() - app.t0) / 1000;
      if (h.sync && h.sync !== 'syncing' && h.sync !== 'not_synced') return { listenS: r1(listenS), readyS: r1((performance.now() - app.t0) / 1000), sync: h.sync };
    } catch {}
    if (performance.now() - app.t0 > 600_000) throw new Error('app not ready after 10 min');
    await sleep(250);
  }
}
async function stopApp(app) {
  await Promise.race([app.launched, sleep(60_000)]);
  spawnSync(devApp(), ['--stop', '--port', String(PORT)], { encoding: 'utf8' });
  for (let i = 0; i < 600 && existsSync(`/proc/${app.pid}`); i++) await sleep(100);
}
function cgroupStat(app) {
  try {
    const text = readFileSync(join(app.cgroup, 'memory.stat'), 'utf8');
    const kb = key => Number(new RegExp(`^${key} (\\d+)`, 'm').exec(text)?.[1] ?? 0) / 1024;
    return { anon: kb('anon'), file: kb('file') };
  } catch {
    return {};
  }
}
function cgroupKb(app, file) {
  try {
    return Number(readFileSync(join(app.cgroup, file), 'utf8').trim()) / 1024;
  } catch {
    return null;
  }
}

// ---------- an HTTP client of the app ----------
async function login() {
  const c = JSON.parse(readFileSync(join(LAB, 'credentials.json'), 'utf8'));
  const r = await fetch(`${URL_}/api/auth/login`, {
    method: 'POST',
    headers: { 'content-type': 'application/json', origin: URL_ },
    body: JSON.stringify({ username: c.username, password: c.password }),
  });
  if (!r.ok) throw new Error(`login ${r.status}`);
  const cookie = /ithomiini_session=[^;]+/.exec(r.headers.get('set-cookie') ?? '')?.[0];
  const { csrf } = await r.json();
  return { cookie, csrf };
}
async function timed(session, path, { method = 'GET', body = null, headers = {}, timeoutMs = 300_000 } = {}) {
  const t = performance.now();
  const r = await fetch(`${URL_}${path}`, {
    method,
    headers: {
      ...(session ? { cookie: session.cookie, 'x-csrf-token': session.csrf } : {}),
      ...(body ? { 'content-type': 'application/json' } : {}),
      origin: URL_,
      ...headers,
    },
    body: body ? JSON.stringify(body) : undefined,
    signal: AbortSignal.timeout(timeoutMs),
  });
  const buf = Buffer.from(await r.arrayBuffer());
  return { status: r.status, ms: performance.now() - t, bytes: buf.length, wireBytes: Number(r.headers.get('content-length') ?? buf.length), buf };
}
let rpcId = 0;
async function mcp(token, method, params) {
  const t = performance.now();
  const r = await fetch(`${URL_}/api/ai/mcp`, {
    method: 'POST',
    headers: { authorization: `Bearer ${token}`, 'content-type': 'application/json', accept: 'application/json, text/event-stream' },
    body: JSON.stringify({ jsonrpc: '2.0', id: ++rpcId, method, params }),
    signal: AbortSignal.timeout(600_000),
  });
  const text = await r.text();
  const ms = performance.now() - t;
  let body = null;
  try {
    body = JSON.parse(text);
  } catch {}
  const content = body?.result?.content ?? [];
  const isError = !!(body?.error || body?.result?.isError);
  const first = String(content.find(c => c.type === 'text')?.text ?? '');
  return { status: r.status, ms, bytes: Buffer.byteLength(text), isError, head: first.slice(0, 160), digest: createHash('sha256').update(first).digest('hex').slice(0, 12), error: body?.error?.message ?? (isError ? String(content[0]?.text ?? '').slice(0, 300) : null), images: content.filter(c => c.type === 'image').length };
}

// ---------- the workloads ----------
const TABS = [
  ['Inicio', '/inicio'],
  ['Buscador', '/tablas'],
  ['Colecta', '/colecta'],
  ['Monitoreo', '/monitoreo'],
  ['Fotos', '/fotos'],
  ['Asistente', '/asistente'],
  ['Inicio (again)', '/inicio'],
];
// What pages ask for over and over (long polls, the open tab's refreshes, T3's socket and traces): it does not keep
// a tab from being "settled".
const POLL = /\/api\/(pulse|staged|events\/stream|table\/changes|chat\/proposals|observability|auth\/session)|\/ws\b|socket/;
let captured = []; // the pages' GET API calls, replayed by the concurrent clients
async function pages(session) {
  needMemory('headless browser');
  const { chromium } = await import(PLAYWRIGHT);
  // PERF_CHROMIUM: a browser build of its own, where Playwright's own one is not installed (the server).
  const browser = await chromium.launch({ headless: true, ...(process.env.PERF_CHROMIUM ? { executablePath: process.env.PERF_CHROMIUM } : {}) });
  try {
    const context = await browser.newContext({ serviceWorkers: 'block', viewport: { width: 1366, height: 900 } });
    const [name, value] = session.cookie.split('=');
    await context.addCookies([{ name, value, url: URL_ }]);
    const page = await context.newPage();
    const cdp = await context.newCDPSession(page);
    await cdp.send('Network.enable');
    const reqs = new Map();
    let inflight = 0;
    let lastActivity = performance.now();
    let acc = null;
    cdp.on('Network.requestWillBeSent', e => {
      reqs.set(e.requestId, { url: e.request.url, method: e.request.method, t: performance.now(), type: e.type });
      inflight++;
      if (!POLL.test(e.request.url)) lastActivity = performance.now();
    });
    const done = (e, failed) => {
      const q = reqs.get(e.requestId);
      if (!q) return;
      reqs.delete(e.requestId);
      inflight = Math.max(0, inflight - 1);
      if (!POLL.test(q.url)) lastActivity = performance.now();
      if (!acc) return;
      const ms = performance.now() - q.t;
      acc.requests++;
      acc.bytes += failed ? 0 : e.encodedDataLength;
      const u = new URL(q.url);
      if (u.pathname.startsWith('/api/')) {
        acc.api.push({ path: u.pathname + u.search, ms: r1(ms), bytes: failed ? 0 : e.encodedDataLength });
        if (q.method === 'GET' && u.origin === URL_) captured.push(u.pathname + u.search);
      }
      if (failed) acc.failed++;
    };
    cdp.on('Network.loadingFinished', e => done(e, false));
    cdp.on('Network.loadingFailed', e => done(e, true));
    // Settled: nothing in flight for 750 ms (long polls and T3's socket aside: at most 30 s).
    const settle = async t0 => {
      for (;;) {
        await sleep(100);
        const quiet = performance.now() - lastActivity;
        const pending = [...reqs.values()].filter(q => !POLL.test(q.url) && performance.now() - q.t < 20_000).length;
        if ((pending === 0 && quiet > 750) || performance.now() - t0 > 30_000) return performance.now() - lastActivity > 750 ? lastActivity - t0 : performance.now() - t0;
      }
    };
    const out = [];
    for (const [i, [label, path]] of TABS.entries()) {
      acc = { requests: 0, bytes: 0, failed: 0, api: [] };
      const t0 = performance.now();
      if (i === 0) await page.goto(`${URL_}/#${path}`, { waitUntil: 'domcontentloaded' });
      else await page.evaluate(h => (location.hash = h), `#${path}`);
      const settledMs = await settle(t0);
      // "Usable": the tab's own content is there (no «Cargando» left in the main area).
      const usableMs = await page
        .waitForFunction(() => !/Cargando|Loading/.test(document.querySelector('main')?.innerText ?? document.body.innerText), null, { timeout: 30_000 })
        .then(() => performance.now() - t0)
        .catch(() => null);
      out.push({ tab: label, settledMs: r1(settledMs), usableMs: r1(usableMs), requests: acc.requests, kb: r1(acc.bytes / 1024), failed: acc.failed, api: acc.api });
      log(`  ${label}: settled ${r1(settledMs)} ms, ${acc.requests} requests, ${r1(acc.bytes / 1024)} kB`);
    }
    acc = null;
    return out;
  } finally {
    await browser.close();
  }
}

const QUERIES = ['Mechanitis', 'Oleria onega', 'Hypothyris anastasia', 'Tena', 'female', 'CAM0', 'Napo', 'Ithomia'];
async function search(session) {
  const out = [];
  for (const q of QUERIES)
    for (const pass of ['first', 'again']) {
      const r = await timed(session, `/api/search?q=${encodeURIComponent(q)}`);
      // What it found, and a digest of the whole answer but its timing (the same answer from a faster search).
      let found = null;
      let digest = null;
      try {
        const body = JSON.parse(r.buf.toString('utf8'));
        found = body.sheets?.map(x => `${x.module}:${x.total}`).join(' ');
        digest = createHash('sha256').update(JSON.stringify(body, (k, v) => (k === 'took' ? undefined : v))).digest('hex').slice(0, 12);
      } catch {}
      out.push({ q, pass, status: r.status, ms: r1(r.ms), kb: r1(r.bytes / 1024), found, digest });
    }
  return out;
}
async function sync(session) {
  const r = await timed(session, '/api/sync', { method: 'POST', body: {} });
  return { status: r.status, ms: r1(r.ms), body: r.buf.toString('utf8').slice(0, 300) };
}
async function tools(token, files) {
  const init = await mcp(token, 'initialize', { protocolVersion: '2025-03-26', capabilities: {}, clientInfo: { name: 'perf-bench', version: '1' } });
  const list = await mcp(token, 'tools/list', {});
  const calls = [
    ['host_plants', { butterfly: 'Mechanitis polymnia', area: 'Napo', photo_sheet: true }],
    ['maps_snapshot', { species: ['Mechanitis polymnia'] }],
    ['gallery_specimens', { species: ['Mechanitis polymnia'], limit: 20, photo_sheet: true }],
    ['identify_species', { files: [files.butterfly], side: 'East' }],
    ['rack_read_photos', { photos: [files.rack] }],
  ];
  const out = [{ tool: 'tools/list', ms: r1(list.ms), kb: r1(list.bytes / 1024), init: r1(init.ms) }];
  for (const [name, args] of calls)
    for (const pass of ['first', 'again']) {
      if (name === 'identify_species' && pass === 'first' && memAvailableGb() < 3.5) {
        out.push({ tool: name, pass, skipped: `MemAvailable ${memAvailableGb().toFixed(1)} GB < 3.5 GB` });
        continue;
      }
      const r = await mcp(token, 'tools/call', { name, arguments: args });
      out.push({ tool: name, pass, ms: r1(r.ms), kb: r1(r.bytes / 1024), images: r.images, error: r.error, head: r.head, digest: r.digest });
      log(`  ${name} ${pass}: ${r1(r.ms)} ms, ${r1(r.bytes / 1024)} kB${r.error ? ` (error: ${r.error.slice(0, 120)})` : ''}`);
    }
  return out;
}
async function offline(session) {
  const enc = { 'accept-encoding': 'zstd, gzip' };
  const list = await timed(session, '/prueba-offline/models/list.json', { headers: enc });
  if (list.status !== 200) return { available: false, status: list.status, body: list.buf.toString('utf8').slice(0, 200) };
  const { files, partBytes } = JSON.parse(list.buf.toString('utf8'));
  const out = { available: true, files: [] };
  const t0 = performance.now();
  for (const f of files) {
    const parts = Math.ceil(f.bytes / partBytes);
    let wire = 0;
    const ms = [];
    for (let i = 0; i < parts; i++) {
      const r = await timed(session, `/prueba-offline/models/${encodeURIComponent(f.name)}?part=${i}`, { headers: enc });
      wire += r.wireBytes;
      ms.push(r.ms);
    }
    out.files.push({ name: f.name, mb: r1(f.bytes / 2 ** 20), wireMb: r1(wire / 2 ** 20), parts, partMs: stats(ms) });
  }
  out.totalS = r1((performance.now() - t0) / 1000);
  return out;
}
async function concurrent(session) {
  const paths = [...new Set(captured)].filter(p => !/\/api\/(pulse|staged|outbox|t3\/)/.test(p));
  if (!paths.length) paths.push('/api/bootstrap', '/api/summary', '/api/search?q=Mechanitis');
  for (const q of QUERIES.slice(0, 4)) paths.push(`/api/search?q=${encodeURIComponent(q)}`);
  const by = {};
  let n = 0;
  let errors = 0;
  const until = performance.now() + CONCURRENT_S * 1000;
  const sessions = [session, ...(await Promise.all(Array.from({ length: CLIENTS - 1 }, login)))];
  await Promise.all(
    sessions.map(async (s, c) => {
      let i = c;
      while (performance.now() < until) {
        const path = paths[i++ % paths.length];
        try {
          const r = await timed(s, path, { timeoutMs: 60_000 });
          (by[path.split('?')[0]] ??= []).push(r.ms);
          if (r.status >= 400) errors++;
          n++;
        } catch {
          errors++;
        }
      }
    }),
  );
  const all = Object.values(by).flat();
  return {
    clients: CLIENTS,
    seconds: CONCURRENT_S,
    requests: n,
    errors,
    perSecond: r1(n / CONCURRENT_S),
    ...stats(all),
    endpoints: Object.fromEntries(Object.entries(by).map(([k, v]) => [k, stats(v)])),
  };
}

// ---------- one repeat ----------
async function waitMemory() {
  const t0 = performance.now();
  // Another heavy job's processes (default: the memory audit's harness, which starts its own app instances).
  const other = () => (WAIT_ABSENT ? spawnSync('pgrep', ['-f', WAIT_ABSENT]).status === 0 : false);
  // tools/lab/dev-app.sh runs at most DEV_APP_LIMIT (4) at once.
  const slots = () =>
    spawnSync('systemctl', ['--user', 'list-units', 'ithomiini-dev-*', '--plain', '--no-legend', '--state=active'], { encoding: 'utf8' })
      .stdout.split('\n')
      .filter(l => l.trim() && !l.startsWith(`${UNIT}.service`)).length;
  while (memAvailableGb() < WAIT_GB || other() || slots() >= (Number(process.env.DEV_APP_LIMIT) || 4)) {
    if (performance.now() - t0 > 120 * 60_000) throw new Error(`gave up waiting for memory after 2 h`);
    if (Math.round((performance.now() - t0) / 1000) % 120 < 10) log(`waiting: ${memAvailableGb().toFixed(1)} of ${WAIT_GB} GB available${other() ? `, ${WAIT_ABSENT} running` : ''}, ${slots()} other dev apps`);
    await sleep(10_000);
  }
  return r1((performance.now() - t0) / 1000);
}
async function repeat(name, { profileDir = null } = {}) {
  const waitedS = await waitMemory();
  const run = repeatDir(name);
  const files = attachments();
  const result = { name, phases: {}, workloads: {}, waitedForMemoryS: waitedS, memAvailableAtStartGb: r1(memAvailableGb()) };
  log(`${name}${armOut ? ` (${APP})` : ''}: starting the app (MemAvailable ${memAvailableGb().toFixed(1)} GB)`);
  let app = null;
  for (let tries = 0; !app; tries++) {
    try {
      app = await startApp(run, { profileDir });
    } catch (e) {
      // Another chat took the last dev-app slot meanwhile: wait for one again.
      if (!/dev apps run already/.test(e.message) || tries >= 30) throw e;
      log(`  ${e.message.trim()}; waiting`);
      await sleep(30_000);
      await waitMemory();
    }
  }
  const s = sampler(app.pid, app);
  let probe = null;
  try {
    s.begin('cold_start');
    const ready = await waitReady(app);
    const startPhase = s.end();
    result.workloads.cold_start = ready;
    result.phases.cold_start = startPhase;
    log(`  ready: listening ${ready.listenS} s, synced ${ready.readyS} s; peak ${startPhase.peakTotalMb} MB`);
    probe = prober();
    s.begin('idle');
    await sleep(20_000);
    result.phases.idle = { ...s.end(), probe: probe.take() };
    const session = await login();
    const phase = async (key, fn) => {
      if (!ONLY.includes(key)) return;
      log(`  ${key}…`);
      probe.take();
      s.begin(key);
      try {
        result.workloads[key] = await fn();
      } catch (e) {
        result.workloads[key] = { error: e.message };
        log(`  ${key} failed: ${e.message}`);
      }
      result.phases[key] = { ...s.end(), probe: probe.take() };
      try {
        result.phases[key].healthEventLoop = (await (await fetch(`${URL_}/health`)).json()).eventLoop;
      } catch {}
    };
    await phase('pages', () => pages(session));
    await phase('search', () => search(session));
    await phase('sync', () => sync(session));
    await phase('tools', () => tools(run.token, files));
    await phase('offline', () => offline(session));
    await phase('concurrent', () => concurrent(session));
    s.begin('idle_end');
    await sleep(10_000);
    result.phases.idle_end = { ...s.end(), probe: probe.take() };
    result.peakHwmMb = s.peakHwmMb();
    result.cgroupPeakMb = { mem: r1(cgroupKb(app, 'memory.peak') / 1024), swap: r1(cgroupKb(app, 'memory.swap.peak') / 1024) };
  } finally {
    probe?.stop();
    s.stop();
    await stopApp(app);
  }
  await sleep(2000);
  const orphans = spawnSync('pgrep', ['-f', `${run.ident} `], { encoding: 'utf8' }).stdout.trim();
  if (orphans) result.orphans = orphans.split('\n');
  writeFileSync(join(run.dir, 'samples.json'), JSON.stringify(s.samples));
  writeFileSync(join(run.dir, 'result.json'), JSON.stringify(result, null, 1));
  return result;
}

// Arms (--arms base=<dir>,e1=<dir>): the same workloads on several checkouts, their repeats interleaved (ABBA…), so
// what else the PC does meanwhile weighs on each alike. Each arm's results in <out>/<label>/results.json.
const ARMS = opt('--arms', '')
  ? opt('--arms', '')
      .split(',')
      .map(a => {
        const [label, dir] = a.split('=');
        return { label, dir: resolve(dir) };
      })
  : [{ label: LABEL, dir: resolve(opt('--app-dir', HERE)), single: true }];
const metaOf = arm => ({
  label: arm.label,
  at: new Date().toISOString(),
  appDir: arm.dir,
  commit: spawnSync('git', ['-C', arm.dir, 'rev-parse', '--short', 'HEAD'], { encoding: 'utf8' }).stdout.trim(),
  dirty: !!spawnSync('git', ['-C', arm.dir, 'status', '--porcelain', 'server'], { encoding: 'utf8' }).stdout.trim(),
  node: process.version,
  host: readFileSync('/etc/hostname', 'utf8').trim(),
  cpus: spawnSync('nproc', { encoding: 'utf8' }).stdout.trim(),
  nodeOptions: NODE_OPTIONS || '(dev-app.sh default)',
  memoryMax: MEMORY_MAX,
  cpuWeight: CPU_WEIGHT || null,
  env: EXTRA_ENV,
  only: ONLY,
  repeats: REPEATS,
  arms: ARMS.map(a => a.label),
});
const use = arm => {
  APP = arm.dir;
  armOut = arm.single ? null : join(OUT, arm.label);
  if (armOut) mkdirSync(armOut, { recursive: true });
};
const results = Object.fromEntries(ARMS.map(a => [a.label, []]));
const metas = Object.fromEntries(ARMS.map(a => [a.label, metaOf(a)]));
for (let i = 1; i <= REPEATS; i++)
  for (const arm of i % 2 ? ARMS : [...ARMS].reverse()) {
    use(arm);
    results[arm.label].push(await repeat(`r${i}`));
    captured = [];
  }
for (const arm of ARMS) {
  use(arm);
  let profiles = null;
  if (PROFILE) {
    const dir = join(armOut ?? OUT, 'profile');
    mkdirSync(dir, { recursive: true });
    const prof = await repeat('profile', { profileDir: dir });
    profiles = { cpu: summarizeCpuProfiles(dir), heap: summarizeHeapProfiles(dir), phases: prof.phases };
  }
  const file = join(armOut ?? OUT, 'results.json');
  writeFileSync(file, JSON.stringify({ meta: metas[arm.label], results: results[arm.label], profiles }, null, 1));
  log(`results: ${file}`);
}
