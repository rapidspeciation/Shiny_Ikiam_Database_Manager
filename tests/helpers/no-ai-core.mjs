// No test may use Franz's AI subscriptions (his rule, 9 Nov 2026: "I never want a test to use my ai subscriptions").
// This guard runs in every test process: the server tests load it with `node --test --import ./tests/helpers/no-ai.mjs`
// (package.json, scripts/deploy.sh), the frontend tests with vitest's setupFiles (frontend/src/test-no-ai.mjs).
// It refuses, and writes down:
//   - starting a model CLI (claude, codex, t3, opencode, gemini, ...), wacli (WhatsApp) or ssh (the server), unless it is a
//     stub a test wrote in a temporary folder or in tests/ (the real ones are also shadowed on PATH by tests/helpers/no-ai-bin);
//   - a connection to an AI host (Anthropic, OpenAI, Gemini's API, OpenRouter, ElevenLabs, ...), to T3 Code (its public
//     hosts, and 127.0.0.1:3773-3775 where T3 servers listen on Franz's PC and on the server) or to a Claude Code, T3 or
//     Codex unix socket;
// and takes the AI tokens and the AI and T3 addresses out of the environment, so a test meant to use fakes cannot fall
// back to the machine's. Node processes a test starts get the guard too (NODE_OPTIONS) and report to it.
// A refused attempt fails the test it happened in and the run. Loopback test servers and every other host are untouched
// (the server tests' own fakes listen on ephemeral ports).
import childProcess from 'node:child_process';
import net from 'node:net';
import workerThreads, { SHARE_ENV } from 'node:worker_threads';
import { syncBuiltinESMExports } from 'node:module';
import { accessSync, appendFileSync, constants, existsSync, readFileSync, rmSync, statSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { basename, delimiter, dirname, join, resolve, sep } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { promisify } from 'node:util';

const KEY = Symbol.for('ithomiini.noAiGuard');
const HERE = dirname(fileURLToPath(import.meta.url));
// tests/ (this file in tests/helpers, as in the database repo) or the folder it is in (the maps repo's test/).
const TESTS = basename(HERE) === 'helpers' ? dirname(HERE) : HERE;
const SHIMS = join(HERE, 'no-ai-bin');
const PRELOAD = join(HERE, 'no-ai.mjs');
// Python processes a test starts load the Python guard (no_ai_guard.py) through this folder's sitecustomize.py.
const PYDIR = join(HERE, 'no-ai-py');
const PYBOOT = join(PYDIR, 'boot.py');

/**
 * `python -I` / `-E` ignores PYTHONPATH: its args with boot.py put before the script (-c, -m or a file), which loads the
 * Python guard and then runs the script as given. null: not such a call (or a script read from stdin).
 */
export function pythonBoot(file, args) {
  if (!/^python[0-9.]*$/.test(basename(String(file))) || !Array.isArray(args) || args.includes(PYBOOT)) return null;
  let isolated = false;
  for (let i = 0; i < args.length; i++) {
    const a = String(args[i]);
    if (a === '-c' || a === '-m') return isolated ? [...args.slice(0, i), PYBOOT, a, ...args.slice(i + 1)] : null;
    if (a === '-' || a === '--') return isolated && a === '--' && i + 1 < args.length ? [...args.slice(0, i), PYBOOT, '--file', ...args.slice(i + 1)] : null;
    if (!a.startsWith('-')) return isolated ? [...args.slice(0, i), PYBOOT, '--file', ...args.slice(i)] : null;
    if (a === '-W' || a === '-X') i++;
    else if (/^-(?![WX])[A-Za-z]+$/.test(a) && /[IE]/.test(a)) isolated = true;
  }
  return null;
}
const SHELLS = ['sh', 'bash', 'zsh', 'dash', 'ksh', 'env'];

export const CLIS = ['claude', 'codex', 't3', 'opencode', 'gemini', 'ollama', 'aider', 'cursor-agent', 'qwen', 'crush', 'goose', 'amp', 'llm', 'deepseek', 'wacli', 'ssh'];
export const AI_HOSTS = [
  'anthropic.com', 'claude.ai', 'claude.com', 'openai.com', 'chatgpt.com', 'oaiusercontent.com', 'openrouter.ai',
  'generativelanguage.googleapis.com', 'aiplatform.googleapis.com', 'elevenlabs.io', 'deepseek.com', 'mistral.ai', 'groq.com',
  'x.ai', 'cohere.ai', 'cohere.com', 'together.xyz', 'together.ai', 'perplexity.ai', 'z.ai', 'bigmodel.cn', 'moonshot.ai',
  'moonshot.cn', 'fireworks.ai', 'replicate.com', 'opencode.ai', 'api-inference.huggingface.co',
];
export const T3_HOSTS = ['t3.codes', 't3.ithomiini-ikiam.com'];
export const T3_PORTS = [3773, 3774, 3775];
const SOCKETS = /\/cc-socks\/|\/\.t3[^/]*\/|\/\.codex\/|\/\.claude[^/]*\//;
// Taken out of every test's environment (a test that needs one sets it itself, to a fake).
const SECRET_ENV = /^(ANTHROPIC_|CLAUDE_CODE_OAUTH_TOKEN$|CLAUDE_CODE_MESSAGING_|OPENAI_|AI_API_KEY$|AI_MODEL$|ITHOMIINI_AI_|ITHOMIINI_T3_|T3CODE_|CLAUDE_CONFIG_DIR$|CODEX_HOME$|T3_URL$|GEMINI_|GOOGLE_API_KEY$|ELEVENLABS_|OPENROUTER_|DEEPSEEK_|MISTRAL_|GROQ_|OLLAMA_HOST$|WA_)/;

const LOOPBACK = h => !h || h === 'localhost' || h === '::1' || h === '[::1]' || /^127\./.test(h) || h === '0.0.0.0' || h === '::';
const under = (path, dir) => path === dir || path.startsWith(dir.endsWith(sep) ? dir : dir + sep);
// A stub a test made: in the temporary folder or in tests/ (fixtures, the guard's own shims excepted).
const stub = path => (under(path, tmpdir()) || under(path, '/tmp') || under(path, TESTS)) && !under(path, SHIMS);

function executable(file) {
  try {
    accessSync(file, constants.X_OK);
    return statSync(file).isFile();
  } catch {
    return false;
  }
}

function onPath(name, path) {
  for (const dir of String(path ?? '').split(delimiter)) {
    if (!dir) continue;
    const file = join(dir, name);
    try {
      accessSync(file, constants.X_OK);
      if (statSync(file).isFile()) return file;
    } catch {
      /* not here */
    }
  }
  return null;
}

/** The real CLI a command would start, or null (not one of ours, a test's stub, or not installed on that PATH). */
export function blockedCli(file, options = {}) {
  const f = String(file ?? '');
  const name = basename(f).replace(/\.(exe|cmd|js|mjs|cjs)$/, '');
  if (!CLIS.includes(name)) return null;
  if (f.includes('/')) {
    const abs = resolve(String(options?.cwd ?? process.cwd()), f);
    return stub(abs) ? null : abs;
  }
  const found = onPath(f, options?.env?.PATH ?? process.env.PATH);
  if (!found) return null;
  return under(found, SHIMS) || !stub(found) ? found : null;
}

/** The first blocked CLI a shell command line would start (commands at the start of each pipeline part). */
export function blockedInShell(command, options) {
  // A quoted `-c` script starts a command of its own (sh -c 'claude …', env -i PATH=… bash -c "…").
  const line = String(command ?? '').replace(/(\b(?:sh|bash|zsh|dash|ksh)\s+(?:-\S+\s+)*-[a-z]*c\s+)['"]/g, '$1;');
  const re = /(?:^|[;&|(`\n]|\$\()\s*(?:(?:[A-Za-z_][A-Za-z0-9_]*=\S*|exec|env|nohup|command|nice|-\S+)\s+)*([^\s;&|()`'"<>]+)/g;
  for (const m of line.matchAll(re)) {
    const hit = blockedCli(m[1], options);
    if (hit) return hit;
  }
  // A path to one anywhere in the line, when it is an executable file (an absolute path in a script is not seen by the
  // PATH shims; `ls /etc/ssh` or `git log -- server/t3` are not starts).
  const cli = CLIS.map(c => c.replace('-', '\\-')).join('|');
  for (const m of line.matchAll(new RegExp(`[^\\s;&|()<>'"\`=]*/(?:${cli})(?=$|[\\s;&|()<>'"\`])`, 'g'))) {
    const hit = executable(resolve(String(options?.cwd ?? process.cwd()), m[0])) && blockedCli(m[0], options);
    if (hit) return hit;
  }
  return null;
}

/** A CommonJS file: .cjs, or .js under a package.json without "type": "module". */
function commonJs(file) {
  if (/\.cjs$/.test(file)) return true;
  if (!/\.js$/.test(file)) return false;
  for (let dir = dirname(file); ; dir = dirname(dir)) {
    try {
      return JSON.parse(readFileSync(join(dir, 'package.json'), 'utf8')).type !== 'module';
    } catch {
      if (dirname(dir) === dir) return true;
    }
  }
}

/** Why a connection is refused, or null. */
export function blockedHost({ host, port, path } = {}) {
  if (path) return SOCKETS.test(String(path)) && !stub(String(path)) ? `unix socket ${path}` : null;
  const h = String(host ?? '').toLowerCase().replace(/\.$/, '').replace(/^\[|\]$/g, '');
  if (T3_PORTS.includes(Number(port))) return `T3 port ${h || 'localhost'}:${port}`; // on loopback, the tailnet or any address
  if (LOOPBACK(h)) return null;
  const suffix = list => list.find(d => h === d || h.endsWith(`.${d}`));
  if (suffix(AI_HOSTS)) return `AI host ${h}:${port ?? ''}`;
  if (suffix(T3_HOSTS)) return `T3 host ${h}:${port ?? ''}`;
  return null;
}

function connectTarget(args) {
  let o = args[0];
  if (Array.isArray(o)) o = o[0];
  if (o && typeof o === 'object') return { host: o.host, port: o.port, path: o.path };
  if (typeof o === 'string' && Number.isNaN(Number(o))) return { path: o };
  return { port: o, host: typeof args[1] === 'string' ? args[1] : undefined };
}

// The Python guard's folder first, the test's own PYTHONPATH after it.
const pythonPath = given => (String(given ?? '').split(delimiter).includes(PYDIR) ? given : [PYDIR, given].filter(Boolean).join(delimiter));

const where = () =>
  String(new Error().stack)
    .split('\n')
    .slice(3)
    .map(l => l.trim())
    .filter(l => !l.includes('no-ai-core.mjs') && !/\(node:|at node:/.test(l))
    .slice(0, 3)
    .join(' < ');

/**
 * Installs the guard in this process (once). `take()` returns the attempts written down since the last call, in this
 * process and in the node processes it started; `owner`: this process fails the run at exit if any are left.
 */
export function installNoAiGuard({ owner = false } = {}) {
  if (globalThis[KEY]) return globalThis[KEY];
  const own = join(tmpdir(), `no-ai-guard-${process.pid}-${Math.random().toString(36).slice(2, 8)}.log`);
  const parents = String(process.env.NOAI_GUARD_LOGS ?? '').split(delimiter).filter(Boolean);
  const logs = [...parents, own];
  const record = process.env.NOAI_GUARD_RECORD;
  let file = process.env.NOAI_GUARD_FILE ?? process.argv[1] ?? '?';

  // In the test's own process only: what a node process it starts gets in its env (and its PATH), the test gave it on purpose.
  const first = !process.env.NOAI_GUARD_PARENT;
  // A worker thread leaves the env as it got it (its parent's settings; with SHARE_ENV, the parent's own env).
  if (workerThreads.isMainThread) {
    if (first) for (const k of Object.keys(process.env)) if (SECRET_ENV.test(k)) delete process.env[k];
    process.env.NOAI_GUARD_LOGS = logs.join(delimiter);
    process.env.NOAI_GUARD_FILE = file;
    process.env.NOAI_GUARD_PARENT = String(process.pid);
    if (first && !String(process.env.PATH ?? '').split(delimiter).includes(SHIMS)) process.env.PATH = `${SHIMS}${delimiter}${process.env.PATH ?? ''}`;
    process.env.PYTHONPATH = pythonPath(process.env.PYTHONPATH);
    if (!String(process.env.NODE_OPTIONS ?? '').includes(PRELOAD))
      process.env.NODE_OPTIONS = `${process.env.NODE_OPTIONS ?? ''} --import=${PRELOAD}`.trim();
  }

  const block = (kind, target) => {
    const line = `no-ai guard: refused ${kind} ${target} (test ${file}, pid ${process.pid}) at ${where()}`;
    for (const log of [...logs, ...(record ? [record] : [])])
      try {
        appendFileSync(log, line.replace(/\n/g, ' ') + '\n');
      } catch {
        /* a parent gone: its own check is over */
      }
    return line;
  };

  // Child processes: a blocked CLI is replaced by a shell that says so and exits 1, as a missing or failing CLI would.
  const refusal = target => `echo 'no-ai guard: ${basename(target)} is not started in tests (tests/helpers/no-ai-core.mjs)' >&2; exit 1`;
  const wrap = (fn, kind) => {
    const orig = childProcess[fn];
    if (!orig) return;
    const check = (file, rest) => {
      if (rest[0] == null && rest.length > 1) rest = [[], ...rest.slice(1)]; // spawn(f, undefined, { env })
      const argIndex = Array.isArray(rest[0]) ? 0 : -1;
      const options = (argIndex === 0 ? rest[1] : rest[0]) ?? {};
      let opts = options && typeof options === 'object' && !Array.isArray(options) ? options : {};
      // A child given its own env keeps the guard (preload and reporting); its PATH stays as the test set it.
      if (opts.env && typeof opts.env === 'object') {
        opts = { ...opts, env: guardEnv(opts.env) };
        rest = [...rest];
        rest[argIndex === 0 ? 1 : 0] = opts;
      }
      // python -I / -E: the guard through boot.py (PYTHONPATH does not reach it).
      const boot = kind === 'file' && argIndex === 0 && pythonBoot(file, rest[0]);
      if (boot) rest = [boot, ...rest.slice(1)];
      // A shell (or env) started directly: its script and arguments are checked like a shell line.
      if (kind === 'file' && !opts.shell && SHELLS.includes(basename(String(file))) && argIndex === 0) {
        const line = rest[0].map(String).join(' ');
        const hit = blockedInShell(line, opts);
        if (hit) {
          block('CLI', `${hit} (${basename(String(file))} ${line.slice(0, 160)})`);
          return ['/bin/sh', [['-c', refusal(hit)], { ...opts, shell: false }, ...rest.slice(2)]];
        }
        return [file, rest];
      }
      if (kind === 'shell' || opts.shell) {
        const line = kind === 'shell' ? String(file) : [file, ...(argIndex === 0 ? rest[0] : [])].join(' ');
        const hit = blockedInShell(line, opts);
        if (!hit) return [file, rest];
        block('CLI', `${hit} (${line.slice(0, 160)})`);
        if (kind === 'shell') return [refusal(hit), rest];
        return ['/bin/sh', [['-c', refusal(hit)], { ...opts, shell: false }, ...rest.slice(argIndex === 0 ? 2 : 1)]];
      }
      const hit = blockedCli(file, opts);
      if (!hit) return [file, rest];
      block('CLI', `${hit} ${(argIndex === 0 ? rest[0] : []).join(' ').slice(0, 160)}`);
      return ['/bin/sh', argIndex === 0 ? [['-c', refusal(hit)], ...rest.slice(1)] : [['-c', refusal(hit)], ...rest]];
    };
    const wrapped = function (file, ...rest) {
      const [f, r] = check(file, rest);
      return orig.call(this, f, ...r);
    };
    // exec and execFile keep their promisified form ({ stdout, stderr }), checked the same way.
    if (orig[promisify.custom])
      wrapped[promisify.custom] = (file, ...rest) => {
        const [f, r] = check(file, rest);
        return orig[promisify.custom](f, ...r);
      };
    childProcess[fn] = wrapped;
  };
  const guardEnv = env => ({
    ...env,
    NODE_OPTIONS: String(env.NODE_OPTIONS ?? '').includes(PRELOAD) ? env.NODE_OPTIONS : `${env.NODE_OPTIONS ?? ''} --import=${PRELOAD}`.trim(),
    PYTHONPATH: pythonPath(env.PYTHONPATH),
    NOAI_GUARD_LOGS: process.env.NOAI_GUARD_LOGS,
    NOAI_GUARD_PARENT: process.env.NOAI_GUARD_PARENT,
    NOAI_GUARD_FILE: process.env.NOAI_GUARD_FILE,
    ...(record ? { NOAI_GUARD_RECORD: record } : {}),
  });
  for (const fn of ['spawn', 'spawnSync', 'execFile', 'execFileSync']) wrap(fn, 'file');
  for (const fn of ['exec', 'execSync']) wrap(fn, 'shell');
  // fork: only the env (a node child; its module is no CLI).
  const fork = childProcess.fork;
  childProcess.fork = function (modulePath, args, options) {
    if (args == null && options !== undefined) args = []; // fork(m, undefined, { env })
    const opts = Array.isArray(args) ? options : args;
    if (opts?.env && typeof opts.env === 'object') {
      const o = { ...opts, env: guardEnv(opts.env) };
      return Array.isArray(args) ? fork.call(this, modulePath, args, o) : fork.call(this, modulePath, o);
    }
    return fork.apply(this, arguments);
  };

  // Worker threads: --import does not reach them, so each one loads the guard before its own module (workerData,
  // argv and env as given; an env object gets the guard's settings). Eval workers are not used by the app.
  const Worker = workerThreads.Worker;
  workerThreads.Worker = class extends Worker {
    constructor(filename, options = {}) {
      const name = String(filename);
      const nonFile = filename instanceof URL ? filename.protocol !== 'file:' : /^[a-z][a-z0-9+.-]*:/i.test(name) && !/^file:/i.test(name);
      // Eval workers, CommonJS files, SHARE_ENV, data: and other URLs start as given, with the guard preloaded (execArgv:
      // when it was loaded by vitest's setupFiles, the parent's execArgv has no --import) and, for an env object, its settings.
      if (options?.eval || options?.env === SHARE_ENV || nonFile || commonJs(filename instanceof URL ? fileURLToPath(filename) : /^file:/i.test(name) ? fileURLToPath(name) : resolve(name))) {
        const env = options?.env && typeof options.env === 'object' ? guardEnv(options.env) : options?.env;
        const given = options?.execArgv ?? process.execArgv;
        const opts = { ...options, ...(env ? { env } : {}) };
        if (given.some(a => String(a).includes(PRELOAD))) {
          super(filename, opts); // the preload is in the execArgv the worker gets already (node --test --import)
          return;
        }
        let worker;
        try {
          worker = new Worker(filename, { ...opts, execArgv: [...given, `--import=${PRELOAD}`] });
        } catch (error) {
          // A parent flag a worker refuses (ERR_WORKER_INVALID_EXEC_ARGV): the worker's own flags and the preload.
          if (error?.code !== 'ERR_WORKER_INVALID_EXEC_ARGV') throw error;
          worker = new Worker(filename, { ...opts, execArgv: [...(options?.execArgv ?? []), `--import=${PRELOAD}`] });
        }
        return worker;
      }
      const url = filename instanceof URL || /^file:/.test(String(filename)) ? String(filename) : pathToFileURL(resolve(String(filename))).href;
      const env = options?.env && typeof options.env === 'object' ? guardEnv(options.env) : options?.env;
      super(`import(${JSON.stringify(pathToFileURL(PRELOAD).href)}).then(() => import(${JSON.stringify(url)}))`, { ...options, ...(env ? { env } : {}), eval: true });
    }
  };

  // Connections: refused before any name lookup, so it needs no network and is the same offline.
  const connect = net.Socket.prototype.connect;
  net.Socket.prototype.connect = function (...args) {
    const why = blockedHost(connectTarget(args));
    if (!why) return connect.apply(this, args);
    const line = block('connection to', why);
    const error = Object.assign(new Error(line), { code: 'ECONNREFUSED' });
    process.nextTick(() => this.destroy(error));
    return this;
  };
  syncBuiltinESMExports();

  let seen = 0;
  const lines = () => (existsSync(own) ? readFileSync(own, 'utf8').split('\n').filter(Boolean) : []);
  const guard = {
    shims: SHIMS,
    /** The test running now (the runners' beforeEach): named in what is written down, here and in processes it starts. */
    testing(name) {
      file = name;
      process.env.NOAI_GUARD_FILE = name;
    },
    take() {
      const all = lines();
      const fresh = all.slice(seen);
      seen = all.length;
      return fresh;
    },
  };
  process.on('exit', () => {
    const left = guard.take();
    if (left.length && owner) {
      process.stderr.write(`\n${left.join('\n')}\nno-ai guard: ${left.length} attempt(s) to use a model CLI, an AI host or T3 (above): the run fails.\n`);
      process.exitCode = 1;
    }
    rmSync(own, { force: true });
  });
  globalThis[KEY] = guard;
  return guard;
}

export const noAiGuard = () => globalThis[KEY] ?? null;
