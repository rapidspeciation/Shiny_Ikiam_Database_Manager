#!/usr/bin/env node
// The server tests (tests/*.test.mjs) as one `node --test` run with the no-AI guard (tests/helpers/no-ai.mjs), like
// `node --test --import ./tests/helpers/no-ai.mjs tests/*.test.mjs`, but cheaper: every test file run alone starts a
// node that loads the server's modules (about 0.3 s CPU each, 150 files). Files that cannot change what another file
// sees (shareable(): no environment, process handlers, mocks, hooks, globals, module settings or imported objects
// changed) run a few to a process, in groups; every other file runs alone, as before. The longest start first
// (durations of the last run on this machine), so the run does not end on one long file.
//
//   node scripts/test-server.mjs                   scripts/deploy.sh
//   node scripts/test-server.mjs --isolated        every file alone (the nightly run: a test passing only beside another
//                                                  file shows up there)
//   node scripts/test-server.mjs --shuffle <seed>  other groups and another order (to check that no order matters)
//   node scripts/test-server.mjs --plan            print the groups and the order, run nothing
//   --concurrency <n> (default: the cores), --durations <file> (default: the deploy cache, ITHOMIINI_DEPLOY_CACHE),
//   -- <more node --test arguments> (reporters).
import { spawn } from 'node:child_process';
import { mkdirSync, mkdtempSync, readFileSync, readdirSync, renameSync, rmSync, writeFileSync } from 'node:fs';
import { availableParallelism, homedir, tmpdir } from 'node:os';
import { dirname, isAbsolute, join, relative, resolve } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');

/**
 * What keeps a file alone: a pattern that changes state other files in its process would see, with the reason.
 * Conservative: a match anywhere in the file (a comment too) keeps it alone.
 */
export const ALONE = [
  [/\bprocess\.(env|on|once|off|addListener|removeListener|removeAllListeners|prependListener|chdir|exit|exitCode|argv|umask|title|stdin|emit|setUncaughtExceptionCaptureCallback)\b|\bprocess\.kill\(\s*process\.pid/, 'the process: environment, handlers, cwd, exit'],
  [/\bglobalThis\b|\bglobal\./, 'globals'],
  [/\bmock\b/, 'mocks (timers, methods, modules)'],
  // A test's own hooks (t.after) stay with that test; a file's (after(), test.after()) would run around every file.
  [/(^|[^.\w$])(before|after|beforeEach|afterEach)\s*\(|\btest\.(before|after|beforeEach|afterEach)\s*\(/m, "a file's hooks (in a shared process they would run around every file)"],
  [/\b(console|Date|Math|JSON|Promise|performance|crypto|fetch|setTimeout|setInterval|clearTimeout|Buffer|URL)(\.[\w$]+)*\s*=(?!=)/, 'a built-in replaced'],
  [/Object\.(defineProperty|assign|setPrototypeOf)\s*\(\s*(process|globalThis|console|Date|Math|[\w$]+\.prototype)\b|\.prototype\.[\w$]+\s*=(?!=)/, 'a built-in or prototype changed'],
  [/\bsyncBuiltinESMExports\b/, 'built-in modules changed'],
  [/\bset[A-Z][\w$]*Cost\s*\(|\bsetPasswordHashCost\b/, 'a module setting (setPasswordHashCost)'],
];
/**
 * Files kept alone by hand, with why (state the patterns cannot see). A test that changes a new module-level state in
 * a server module (a cache, a setting, a list it edits through a function) needs an entry here; the nightly run
 * (--isolated, every file alone) is the net for one that was missed.
 */
export const KEEP_ALONE = {
  'offline-id.test.mjs': "changes the models' hashes in server/offline-id.mjs (MODELS) for the run",
  'maps-render-checks.test.mjs': "imports tests/helpers/maps-render.mjs, whose test.after() checks that no render process survived",
  'maps-render-process.test.mjs': "imports tests/helpers/maps-render.mjs, whose test.after() checks that no render process survived",
};

/** The helpers (tests/helpers/*.mjs) a source imports, and the helpers those import, as absolute paths. */
function importedHelpers(file, src, seen = new Set()) {
  const dir = dirname(isAbsolute(file) ? file : join(ROOT, file));
  for (const m of src.matchAll(/(?:\bfrom\s*|\bimport\s*\(?\s*)['"](\.{1,2}\/[^'"]*helpers\/[^'"]+\.mjs)['"]/g)) {
    const path = resolve(dir, m[1]);
    if (seen.has(path)) continue;
    seen.add(path);
    let helper;
    try {
      helper = readFileSync(path, 'utf8');
    } catch {
      continue;
    }
    importedHelpers(path, helper, seen);
  }
  return seen;
}

/** Why a test file must run alone ([] when it can share a process). */
export function aloneReasons(file, src = readFileSync(file, 'utf8')) {
  const name = file.split('/').pop();
  const reasons = ALONE.filter(([re]) => re.test(src)).map(([, why]) => why);
  if (KEEP_ALONE[name]) reasons.push(KEEP_ALONE[name]);
  // A helper's own hooks, handlers or globals act on every file in the process too.
  for (const path of importedHelpers(file, src)) {
    const helper = readFileSync(path, 'utf8');
    for (const [re, why] of ALONE) if (re.test(helper)) reasons.push(`imports ${relative(join(ROOT, 'tests'), path)}: ${why}`);
  }
  // A write to something imported (`x.y = `, `x.push(`, `delete x.y`), as a module's exported object would change.
  const names = new Set();
  for (const m of src.matchAll(/^import\s+([^'";]+?)\s+from\s+['"][^'"]+['"]/gm)) {
    const spec = m[1];
    for (const n of [/^([\w$]+)/.exec(spec)?.[1], /\*\s+as\s+([\w$]+)/.exec(spec)?.[1]]) if (n) names.add(n);
    for (const part of /\{([^}]*)\}/.exec(spec)?.[1].split(',') ?? []) {
      const n = part.trim().split(/\s+as\s+/).pop();
      if (n) names.add(n);
    }
  }
  for (const n of names) {
    const e = n.replace(/\$/g, '\\$');
    const write = new RegExp(
      `(^|[^\\w$.])${e}(\\.[\\w$]+|\\[[^\\]\\n]+\\])+\\s*(=(?![=>])|\\+=|-=|\\+\\+|--|\\?\\?=|\\|\\|=)` +
        `|(^|[^\\w$.])${e}(\\.[\\w$]+|\\[[^\\]\\n]+\\])*\\.(push|splice|unshift|pop|shift|set|delete|clear|add|sort|reverse|fill)\\(` +
        `|delete\\s+${e}[.[]|Object\\.(assign|defineProperty)\\(\\s*${e}\\b`,
      'm',
    );
    if (write.test(src)) reasons.push(`changes ${n}, which it imports`);
  }
  return reasons;
}

const FALLBACK_MS = 400; // a file with no duration recorded yet
const START_MS = 300; // a process's own start and imports

/** Groups and order: units = [{ files: [...], ms }], the longest first. */
export function plan(files, { durations = {}, concurrency = availableParallelism(), isolated = false, seed = null } = {}) {
  const rand = seed === null ? null : mulberry32(seed);
  const est = f => durations[f]?.tests_ms ?? FALLBACK_MS;
  const wall = f => durations[f]?.wall_ms ?? est(f) + START_MS;
  const alone = [];
  const shared = [];
  for (const f of files) (isolated || aloneReasons(join(ROOT, f)).length ? alone : shared).push(f);
  // Enough groups to keep every core busy, each well under the longest file alone.
  const total = shared.reduce((a, f) => a + est(f), 0);
  const longest = Math.max(0, ...alone.map(wall));
  const count = shared.length ? Math.min(shared.length, Math.max(2 * concurrency, Math.ceil(total / Math.max(3000, longest / 2)))) : 0;
  const groups = Array.from({ length: count }, () => ({ files: [], ms: START_MS }));
  const order = rand ? shuffle(shared, rand) : [...shared].sort((a, b) => est(b) - est(a) || a.localeCompare(b));
  for (const f of order) {
    const g = rand ? groups[Math.floor(rand() * groups.length)] : groups.reduce((a, b) => (b.ms < a.ms ? b : a));
    g.files.push(f);
    g.ms += est(f);
  }
  const units = [...alone.map(f => ({ files: [f], ms: wall(f) })), ...groups.filter(g => g.files.length)];
  return rand ? shuffle(units, rand) : units.sort((a, b) => b.ms - a.ms || a.files[0].localeCompare(b.files[0]));
}

function mulberry32(a) {
  return () => {
    a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}
function shuffle(list, rand) {
  const a = [...list];
  for (let i = a.length - 1; i > 0; i--) {
    const j = Math.floor(rand() * (i + 1));
    [a[i], a[j]] = [a[j], a[i]];
  }
  return a;
}

function readDurations(file) {
  try {
    return JSON.parse(readFileSync(file, 'utf8'));
  } catch {
    return {};
  }
}

async function main(argv) {
  const dash = argv.indexOf('--');
  const extra = dash >= 0 ? argv.slice(dash + 1) : [];
  const own = dash >= 0 ? argv.slice(0, dash) : argv;
  const opt = name => (own.includes(name) ? own[own.indexOf(name) + 1] : undefined);
  const concurrency = Number(opt('--concurrency')) || availableParallelism();
  const cache = process.env.ITHOMIINI_DEPLOY_CACHE || join(homedir(), '.cache', 'ithomiini-deploy');
  const durationsFile = opt('--durations') ?? join(cache, 'server-test-durations.json');
  const seed = opt('--shuffle') === undefined ? null : Number(opt('--shuffle'));
  const files = readdirSync(join(ROOT, 'tests')).filter(f => f.endsWith('.test.mjs')).sort().map(f => `tests/${f}`);
  const durations = readDurations(durationsFile);
  const units = plan(files, { durations, concurrency, isolated: own.includes('--isolated'), seed });
  if (own.includes('--plan')) {
    for (const u of units) console.log(`${(u.ms / 1000).toFixed(1).padStart(5)} s  ${u.files.map(f => f.slice(6)).join(' ')}`);
    console.log(`${units.length} processes for ${files.length} files (${units.filter(u => u.files.length > 1).length} groups)`);
    return 0;
  }
  // A group is a test file of its own that imports its files, in a folder of this run's own.
  const dir = mkdtempSync(join(tmpdir(), 'server-tests-'));
  const clean = () => rmSync(dir, { recursive: true, force: true });
  const paths = units.map((u, i) => {
    if (u.files.length === 1) return u.files[0];
    const path = join(dir, `group-${String(i + 1).padStart(2, '0')}.test.mjs`);
    writeFileSync(path, `// ${u.files.join(' ')}\n${u.files.map(f => `import ${JSON.stringify(pathToFileURL(join(ROOT, f)).href)};`).join('\n')}\n`);
    return path;
  });
  const record = join(dir, 'durations.json');
  const args = [
    '--test',
    '--import',
    './tests/helpers/no-ai.mjs',
    `--test-concurrency=${concurrency}`,
    // A test that hangs fails after 3 min instead of holding the deploy forever (the longest take about 10 s).
    '--test-timeout=180000',
    '--test-reporter=spec',
    '--test-reporter-destination=stdout',
    '--test-reporter=./tests/helpers/durations-reporter.mjs',
    `--test-reporter-destination=${record}`,
    ...extra,
    ...paths,
  ];
  const child = spawn(process.execPath, args, { cwd: ROOT, stdio: 'inherit' });
  for (const s of ['SIGINT', 'SIGTERM', 'SIGHUP']) process.on(s, () => child.kill(s));
  const code = await new Promise(done => child.on('close', (c, signal) => done(c ?? (signal ? 1 : 0))));
  // This run's times, for the next one (kept for files that did not run this time).
  try {
    const now = readDurations(record);
    const merged = { ...durations };
    for (const [f, d] of Object.entries(now)) {
      const rel = relative(ROOT, f.startsWith('file:') ? fileURLToPath(f) : f);
      if (!/^tests\/[^/]+\.test\.mjs$/.test(rel)) continue; // a group's own entry
      merged[rel] = { tests_ms: d.tests_ms, wall_ms: d.wall_ms ?? merged[rel]?.wall_ms ?? null };
    }
    if (Object.keys(now).length) {
      mkdirSync(dirname(durationsFile), { recursive: true });
      writeFileSync(`${durationsFile}.tmp`, JSON.stringify(merged) + '\n');
      renameSync(`${durationsFile}.tmp`, durationsFile);
    }
  } catch (e) {
    console.error(`Test durations not kept: ${e.message}`);
  }
  clean();
  return code;
}

if (process.argv[1] && fileURLToPath(import.meta.url) === process.argv[1]) process.exit(await main(process.argv.slice(2)));
