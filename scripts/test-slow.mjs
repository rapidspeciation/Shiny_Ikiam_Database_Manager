#!/usr/bin/env node
// The slow tests: the ones that read real assets for seconds of CPU each (Franz's rack photo with ffmpeg and zxing).
// The deploy's tests (tests/*.test.mjs) check the same logic on a reading saved from the real one; this suite checks
// the real reads, and that the saved reading is still what the reader makes of the photo.
//
//   node scripts/test-slow.mjs               run it (the nightly timer; by hand)
//   node scripts/test-slow.mjs --if-changed  run it only when an input below changed since its last green run on this
//                                             machine (scripts/deploy.sh); otherwise say so and exit 0
//   node scripts/test-slow.mjs --inputs      print the inputs' hash and the files it covers
//
// Inputs: the code and files the slow tests read (INPUTS), node's version and ffmpeg's (an apt upgrade of ffmpeg is a
// change too). The last green run's hash is kept in the deploy cache (ITHOMIINI_DEPLOY_CACHE, as scripts/deploy.sh;
// --stamp <file> to choose). Runs with the no-AI guard, as every suite (tests/helpers/no-ai.mjs; its self-check
// tests/no-ai-guard.test.mjs runs here too).
import { createHash } from 'node:crypto';
import { spawn, spawnSync } from 'node:child_process';
import { existsSync, mkdirSync, readFileSync, readdirSync, statSync, writeFileSync } from 'node:fs';
import { availableParallelism, homedir } from 'node:os';
import { dirname, join, relative } from 'node:path';
import { fileURLToPath } from 'node:url';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');

/** What the slow tests depend on: a change to any of these runs them (folders: every file in them). */
export const INPUTS = [
  'server/rack-reader.mjs',
  'server/vendor/zxing-wasm',
  'server/rack-tools.mjs', // its default photo reader (createRackTools' readPhoto)
  'tests/fixtures/rack-bottom-fk00274230.jpg',
  'tests/fixtures/tube-bottom-fs50849233.jpg',
  'tests/fixtures/rack-reading-fk00274230.json',
  'tests/helpers/rack-reading.mjs',
  'tests/rack-tools.test.mjs',
  'tests/rack-deposit.test.mjs',
  'tests/slow',
  'scripts/test-slow.mjs',
];

/** The slow tests (tests/slow/*-real.test.mjs run the rack tests again on the real read), and the guard's self-check. */
export const FILES = ['tests/no-ai-guard.test.mjs', 'tests/slow'];

const walk = path => (statSync(path).isDirectory() ? readdirSync(path).sort().flatMap(name => walk(join(path, name))) : [path]);
const testFiles = paths => paths.flatMap(p => walk(join(ROOT, p))).filter(f => /\.test\.mjs$/.test(f)).map(f => relative(ROOT, f));

function version(cmd, args) {
  const r = spawnSync(cmd, args, { encoding: 'utf8' });
  return r.status === 0 ? r.stdout.split('\n')[0].trim() : `${cmd}: not found`;
}

/** The inputs' hash: every file's path and bytes, node's and ffmpeg's versions. */
export function inputsHash() {
  const h = createHash('sha256');
  const files = [];
  for (const p of INPUTS) {
    const full = join(ROOT, p);
    if (!existsSync(full)) {
      h.update(`missing ${p}\n`);
      continue;
    }
    for (const f of walk(full)) {
      files.push(relative(ROOT, f));
      h.update(`${relative(ROOT, f)}\n`).update(readFileSync(f)).update('\n');
    }
  }
  const versions = [`node ${process.version}`, version('ffmpeg', ['-version'])];
  h.update(versions.join('\n'));
  return { hash: h.digest('hex'), files, versions };
}

function run(files) {
  const args = ['--test', '--import', './tests/helpers/no-ai.mjs', `--test-concurrency=${availableParallelism()}`, ...testFiles(files)];
  return new Promise(done => {
    const child = spawn(process.execPath, args, { cwd: ROOT, stdio: 'inherit' });
    child.on('close', (code, signal) => done(code ?? (signal ? 1 : 0)));
  });
}

const argv = process.argv.slice(2);
const at = argv.indexOf('--stamp');
const stamp = at >= 0 ? argv[at + 1] : join(process.env.ITHOMIINI_DEPLOY_CACHE || join(homedir(), '.cache', 'ithomiini-deploy'), 'slow-suite.json');
const inputs = inputsHash();
if (argv.includes('--inputs')) {
  console.log(JSON.stringify(inputs, null, 1));
  process.exit(0);
}
if (argv.includes('--if-changed')) {
  let last = null;
  try {
    last = JSON.parse(readFileSync(stamp, 'utf8'));
  } catch {
    /* never green here */
  }
  if (last?.hash === inputs.hash) {
    console.log(`Slow tests not needed: their inputs are as at their last green run (${last.at}).`);
    process.exit(0);
  }
}
const started = Date.now();
const code = await run(FILES);
if (code) {
  console.error('Slow tests failed: nothing recorded as green.');
  process.exit(code);
}
mkdirSync(dirname(stamp), { recursive: true });
writeFileSync(stamp, JSON.stringify({ hash: inputs.hash, at: new Date().toISOString(), versions: inputs.versions }) + '\n');
console.log(`Slow tests passed in ${((Date.now() - started) / 1000).toFixed(1)} s.`);
