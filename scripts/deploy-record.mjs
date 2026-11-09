#!/usr/bin/env node
// The pass record of a deploy (scripts/deploy.sh): which commit was tested, where, the result of each test job and
// the hashes of what is shipped, kept in the release as .deploy-record.json. claudeclaw installs a release only when
// its record is a pass for exactly the commit being installed and the files match the record's hashes
// (scripts/install-release.sh).
//   node scripts/deploy-record.mjs write <release dir> --commit <sha> --tree <tree> --jobs check=pass,server=pass,...
//        --slow '<scripts/test-slow.mjs --result JSON>' [--source pc|claudeclaw-local]
//   node scripts/deploy-record.mjs verify <release dir> --commit <sha> [--tree <tree>]
//   node scripts/deploy-record.mjs hash <release dir>     (prints both hashes)
// `artifact` is the built part (web/, the frontend built by the run, and server/instructions-history.json); `release`
// is every file shipped (the commit's files and the built part). Both are the sha256 of a sorted list of
// "<kind> <sha256 or link target> <x or -> <path>" lines. The links the install adds (rename-app, photo-reader) and the
// record itself are not part of them.
// The slow suite (scripts/test-slow.mjs --if-changed) is the job `slow`: "pass" when it ran and passed, "unchanged" when
// its inputs were byte for byte those of its last green run on that machine, so it was not run; `slowSuite` says which
// inputs (their hash) and when they last passed.
import { createHash } from 'node:crypto';
import { lstatSync, readFileSync, readdirSync, readlinkSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';
import { pathToFileURL } from 'node:url';

export const RECORD = '.deploy-record.json';
export const BUILT = ['web', 'server/instructions-history.json'];
export const JOBS = ['check', 'server', 'frontend', 'build'];
/** What the slow suite's job may be in a record that installs: it ran and passed, or its inputs were as when it last did. */
export const SLOW_OK = ['pass', 'unchanged'];
const HASH = /^[0-9a-f]{64}$/;
const ADDED_AT_INSTALL = new Set([RECORD, 'rename-app', 'photo-reader']);

function sha256File(path) {
  return createHash('sha256').update(readFileSync(path)).digest('hex');
}

/** The manifest lines of `rel` under `root` (a file, a link or a folder, recursively). */
function walk(root, rel, lines) {
  const path = join(root, rel);
  const st = lstatSync(path);
  if (st.isSymbolicLink()) lines.push(`l ${readlinkSync(path)} - ${rel}`);
  else if (st.isDirectory())
    for (const name of readdirSync(path).sort()) walk(root, rel ? `${rel}/${name}` : name, lines);
  else if (st.isFile()) lines.push(`f ${sha256File(path)} ${st.mode & 0o111 ? 'x' : '-'} ${rel}`);
  else throw new Error(`Not a file, folder or link: ${rel}`);
}

const digest = lines => createHash('sha256').update(lines.sort().join('\n') + '\n').digest('hex');

/** { artifact, release, files } of a release folder. A built path that is missing is an error. */
export function hashRelease(dir) {
  const all = [];
  for (const name of readdirSync(dir).sort()) if (!ADDED_AT_INSTALL.has(name)) walk(dir, name, all);
  const built = [];
  for (const rel of BUILT) {
    try {
      lstatSync(join(dir, rel));
    } catch {
      throw new Error(`The built ${rel} is missing from ${dir}`);
    }
    walk(dir, rel, built);
  }
  return { artifact: digest(built), release: digest(all), files: all.length };
}

/** Writes the record of a passing run into the release folder. */
export function writeRecord(dir, { commit, tree, jobs, slow, source = 'pc', now = new Date() }) {
  if (!/^[0-9a-f]{40}$/.test(commit ?? '')) throw new Error(`Not a commit: ${commit}`);
  if (!/^[0-9a-f]{40}$/.test(tree ?? '')) throw new Error(`Not a tree: ${tree}`);
  if (!SLOW_OK.includes(slow?.slow) || !HASH.test(slow?.inputs ?? ''))
    throw new Error(`No result of the slow suite (scripts/test-slow.mjs --result): ${JSON.stringify(slow)}`);
  const { artifact, release, files } = hashRelease(dir);
  const record = {
    version: 1,
    commit,
    tree,
    source,
    node: process.version,
    platform: `${process.platform}-${process.arch}`,
    testedAt: now.toISOString(),
    jobs: { ...jobs, slow: slow.slow },
    slowSuite: { ran: slow.slow === 'pass', inputs: slow.inputs, lastGreenAt: slow.lastGreenAt ?? null },
    artifact: { paths: BUILT, sha256: artifact },
    release: { sha256: release, files },
  };
  writeFileSync(join(dir, RECORD), JSON.stringify(record, null, 2) + '\n');
  return record;
}

/**
 * Checks a release folder against its record: a pass of every job, for exactly `commit` (and `tree` when given),
 * and the built part and every file as recorded. Returns the record; throws with the reason otherwise.
 */
export function verifyRelease(dir, { commit, tree } = {}) {
  let record;
  try {
    record = JSON.parse(readFileSync(join(dir, RECORD), 'utf8'));
  } catch (e) {
    throw new Error(`No readable test record (${RECORD}) in ${dir}: ${e.code ?? e.message}`);
  }
  if (record?.version !== 1) throw new Error(`Unknown record version ${record?.version}`);
  if (!commit) throw new Error('The commit to install is not given');
  if (record.commit !== commit) throw new Error(`The record is for commit ${record.commit}, not ${commit}`);
  if (tree && record.tree !== tree) throw new Error(`The record is for tree ${record.tree}, not ${tree}`);
  const failed = JOBS.filter(job => record.jobs?.[job] !== 'pass');
  if (!SLOW_OK.includes(record.jobs?.slow) || !HASH.test(record.slowSuite?.inputs ?? '')) failed.push('slow');
  if (failed.length) throw new Error(`The record has no pass for: ${failed.join(', ')}`);
  const now = hashRelease(dir);
  if (now.artifact !== record.artifact?.sha256)
    throw new Error(`The built frontend differs from the tested one (artifact ${now.artifact.slice(0, 12)}, record ${String(record.artifact?.sha256).slice(0, 12)})`);
  if (now.release !== record.release?.sha256)
    throw new Error(`The release files differ from the tested ones (${now.files} files, record ${record.release?.files})`);
  return record;
}

function parseArgs(argv) {
  const opts = {};
  for (let i = 0; i < argv.length; i++)
    if (argv[i].startsWith('--')) opts[argv[i].slice(2)] = argv[++i];
    else (opts._ ??= []).push(argv[i]);
  return opts;
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  const [command, ...rest] = process.argv.slice(2);
  const opts = parseArgs(rest);
  const dir = opts._?.[0];
  try {
    if (!dir) throw new Error('usage: deploy-record.mjs write|verify|hash <release dir> [options]');
    if (command === 'write') {
      const jobs = Object.fromEntries(String(opts.jobs ?? '').split(',').filter(Boolean).map(j => j.split('=')));
      let slow;
      try {
        slow = JSON.parse(opts.slow ?? '');
      } catch {
        slow = opts.slow;
      }
      const r = writeRecord(dir, { commit: opts.commit, tree: opts.tree, jobs, slow, source: opts.source });
      console.log(`Test record: ${r.commit.slice(0, 8)}, ${Object.entries(r.jobs).map(([k, v]) => `${k} ${v}`).join(', ')} (slow suite inputs ${r.slowSuite.inputs.slice(0, 12)}), frontend ${r.artifact.sha256.slice(0, 12)}`);
    } else if (command === 'verify') {
      const r = verifyRelease(dir, { commit: opts.commit, tree: opts.tree });
      console.log(`Test record ok: ${r.commit.slice(0, 8)} passed on ${r.source} (${r.node}, ${r.testedAt}), frontend ${r.artifact.sha256.slice(0, 12)}, ${r.release.files} files as tested; slow suite ${r.slowSuite.ran ? 'ran' : `unchanged since its last green run (${r.slowSuite.lastGreenAt})`}.`);
    } else if (command === 'hash') console.log(JSON.stringify(hashRelease(dir)));
    else throw new Error(`Unknown command ${command}`);
  } catch (e) {
    console.error(`Refused: ${e.message}`);
    process.exit(1);
  }
}
