#!/usr/bin/env node
// The slow tests: the ones that read real assets for seconds of CPU each (Franz's rack photo with ffmpeg and zxing).
// The deploy's tests (tests/*.test.mjs) check the same logic on a reading saved from the real one; this suite checks
// the real reads, and that the saved reading is still what the reader makes of the photo.
//
//   node scripts/test-slow.mjs               run it (the nightly timer; by hand)
//   node scripts/test-slow.mjs --if-changed  run it only when an input below changed since its last green run on this
//                                             machine (scripts/deploy.sh); otherwise say so and exit 0
//   node scripts/test-slow.mjs --inputs      print the inputs' hash and the files it covers
//   --result <file>  (with a run or --if-changed) on success, what happened as JSON for the deploy's test record:
//                    {"slow":"pass"|"unchanged","inputs":<hash>,"lastGreenAt":<time>}
//
// Inputs: the bytes of the code and files the slow tests read (INPUTS), node's version and platform and ffmpeg's whole
// `-version` (an apt upgrade of ffmpeg is a change too). Only the content counts, never where the checkout is or which
// commit it is: another checkout or commit skips only when every input is byte for byte the same. The last green run's
// hash is kept in the deploy cache (ITHOMIINI_DEPLOY_CACHE, as scripts/deploy.sh; --stamp <file> to choose), written
// only when the inputs after the run are still the ones it started with. Runs with the no-AI guard, as every suite
// (tests/helpers/no-ai.mjs; its self-check tests/no-ai-guard.test.mjs runs here too).
import { createHash } from 'node:crypto';
import { spawn, spawnSync } from 'node:child_process';
import { existsSync, mkdirSync, readdirSync, readFileSync, realpathSync, statSync, writeFileSync } from 'node:fs';
import { availableParallelism, homedir } from 'node:os';
import { dirname, join, relative } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

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

/** A tool's whole `-version` output (its build and libraries too), or that it is missing. */
function versionOutput(cmd) {
  const r = spawnSync(cmd, ['-version'], { encoding: 'utf8' });
  return r.status === 0 ? r.stdout.trim() : `${cmd}: not found`;
}

/** What the slow tests ran on besides the files: node (version, platform) and ffmpeg's whole `-version`. */
export function toolVersions() {
  return [`node ${process.version} ${process.platform}-${process.arch}`, versionOutput('ffmpeg')];
}

/**
 * The inputs' hash: every input file's path, size and bytes under `root` (a missing input counts too), and the tools'
 * versions. Paths are relative to `root`: the same bytes in another checkout give the same hash, any changed byte
 * another one.
 */
export function inputsHash(root = ROOT, versions = toolVersions()) {
  const h = createHash('sha256');
  const files = [];
  for (const p of INPUTS) {
    const full = join(root, p);
    if (!existsSync(full)) {
      h.update(`missing\0${p}\0`);
      continue;
    }
    for (const f of walk(full)) {
      const bytes = readFileSync(f);
      files.push(relative(root, f));
      h.update(`file\0${relative(root, f)}\0${bytes.length}\0`).update(bytes);
    }
  }
  for (const v of versions) h.update(`tool\0${v.length}\0${v}`);
  return { hash: h.digest('hex'), files, versions: versions.map(v => v.split('\n')[0]) };
}

/** The last green run in `stamp` when it ran on exactly these inputs (`hash`), else null: the suite must run. */
export function lastGreen(stamp, hash) {
  try {
    const last = JSON.parse(readFileSync(stamp, 'utf8'));
    return typeof last?.hash === 'string' && last.hash === hash ? last : null;
  } catch {
    return null; // never green here
  }
}

function run(files) {
  const args = ['--test', '--import', './tests/helpers/no-ai.mjs', `--test-concurrency=${availableParallelism()}`, ...testFiles(files)];
  return new Promise(done => {
    const child = spawn(process.execPath, args, { cwd: ROOT, stdio: 'inherit' });
    child.on('close', (code, signal) => done(code ?? (signal ? 1 : 0)));
  });
}

const option = (argv, name) => {
  const at = argv.indexOf(name);
  return at >= 0 ? argv[at + 1] : undefined;
};
const writeResult = (file, result) => {
  if (file) writeFileSync(file, JSON.stringify(result) + '\n');
};

async function main(argv) {
  const stamp = option(argv, '--stamp') ?? join(process.env.ITHOMIINI_DEPLOY_CACHE || join(homedir(), '.cache', 'ithomiini-deploy'), 'slow-suite.json');
  const resultFile = option(argv, '--result');
  const inputs = inputsHash();
  if (argv.includes('--inputs')) {
    console.log(JSON.stringify(inputs, null, 1));
    return 0;
  }
  if (argv.includes('--if-changed')) {
    const last = lastGreen(stamp, inputs.hash);
    if (last) {
      console.log(`Slow tests not needed: their inputs (${inputs.hash.slice(0, 12)}) are as at their last green run (${last.at}).`);
      writeResult(resultFile, { slow: 'unchanged', inputs: inputs.hash, lastGreenAt: last.at });
      return 0;
    }
  }
  const started = Date.now();
  const code = await run(FILES);
  if (code) {
    console.error('Slow tests failed: nothing recorded as green.');
    return code;
  }
  const at = new Date().toISOString();
  // Green for the inputs it ran on: when a file changed meanwhile, the next run tests it again.
  if (inputsHash().hash === inputs.hash) {
    mkdirSync(dirname(stamp), { recursive: true });
    writeFileSync(stamp, JSON.stringify({ hash: inputs.hash, at, versions: inputs.versions }) + '\n');
  } else console.log('An input changed during the run: not recorded as green.');
  writeResult(resultFile, { slow: 'pass', inputs: inputs.hash, lastGreenAt: at });
  console.log(`Slow tests passed in ${((Date.now() - started) / 1000).toFixed(1)} s.`);
  return 0;
}

if (process.argv[1] && import.meta.url === pathToFileURL(realpathSync(process.argv[1])).href) process.exitCode = await main(process.argv.slice(2));
