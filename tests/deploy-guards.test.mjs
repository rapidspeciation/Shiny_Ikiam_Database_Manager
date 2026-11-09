// scripts/deploy.sh refuses to deploy, and touches the server not at all, when the team repo has commits this
// checkout lacks (the T3 assistant commits there too) or when one of the checks fails. The real script runs in a
// throwaway git repo; ssh, node, npm, npx and hostname are stand-ins on PATH that record their calls (ssh only
// records and fails, so no step can reach claudeclaw even if a guard is broken), and HOME is a temp folder.
import test from 'node:test';
import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { chmodSync, copyFileSync, mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';

const script = fileURLToPath(new URL('../scripts/deploy.sh', import.meta.url));
const STANDINS = ['ssh', 'node', 'npm', 'npx', 'hostname'];

// One stand-in for all: records "<name> <args>"; `hostname` is a PC's, `node --version` answers, ssh fails at once,
// and the check named in DEPLOY_TEST_FAIL (check, server, frontend, build) exits 1.
const STANDIN = `#!/bin/bash
name="\${0##*/}"
printf '%s %s\\n' "$name" "$*" >> "$DEPLOY_TEST_CALLS"
case "$name $*" in
  "hostname"*) echo test-pc; exit 0 ;;
  "node --version") echo v24.0.0-standin; exit 0 ;;
  "ssh "*) exit 97 ;;
  "node scripts/check.mjs"*) job=check ;;
  "node --test"*) job=server ;;
  "npm --prefix frontend test"*) job=frontend ;;
  "npx vite build"*) job=build ;;
esac
if [ -n "\${job:-}" ] && [ "$job" = "\${DEPLOY_TEST_FAIL:-}" ]; then echo "$job failed (stand-in)"; exit 1; fi
exit 0
`;

function fixture() {
  const dir = mkdtempSync(join(tmpdir(), 'deploy-guards-'));
  const bin = join(dir, 'bin');
  const home = join(dir, 'home');
  const repo = join(dir, 'repo');
  const team = join(dir, 'remote', 'rapidspeciation', 'Shiny_Ikiam_Database_Manager.git');
  for (const d of [bin, home, join(repo, 'scripts'), join(repo, 'frontend', 'node_modules'), join(repo, 'tests'), team])
    mkdirSync(d, { recursive: true });
  for (const name of STANDINS) {
    writeFileSync(join(bin, name), STANDIN);
    chmodSync(join(bin, name), 0o755);
  }
  copyFileSync(script, join(repo, 'scripts', 'deploy.sh'));
  // The frontend's lockfiles: the script's install check reads them.
  writeFileSync(join(repo, 'frontend', 'package-lock.json'), '{}\n');
  writeFileSync(join(repo, 'frontend', 'node_modules', '.package-lock.json'), '{}\n');
  const calls = join(dir, 'calls');
  // Only what the script needs: no DEPLOY_LOCAL, no ssh agent, no git config of this machine.
  const env = {
    PATH: `${bin}:${process.env.PATH}`,
    HOME: home,
    LANG: 'C.UTF-8',
    ITHOMIINI_DEPLOY_CACHE: join(dir, 'cache'),
    DEPLOY_TEST_CALLS: calls,
    GIT_CONFIG_NOSYSTEM: '1',
    GIT_AUTHOR_NAME: 'test',
    GIT_AUTHOR_EMAIL: 'test@example.invalid',
    GIT_COMMITTER_NAME: 'test',
    GIT_COMMITTER_EMAIL: 'test@example.invalid',
  };
  const sh = (cmd, cwd = repo) => {
    const r = spawnSync('bash', ['-c', cmd], { cwd, env, encoding: 'utf8' });
    assert.equal(r.status, 0, `${cmd}: ${r.stderr}`);
    return r.stdout.trim();
  };
  // Every command the script could reach the server with is a stand-in here.
  for (const name of STANDINS) assert.equal(sh(`command -v ${name}`), join(bin, name));
  sh(`git init -q --bare -b main ${JSON.stringify(team)}`, dir);
  sh('git init -q -b main && git add -A && git commit -q -m one');
  sh(`git remote add team ${JSON.stringify(team)} && git push -q team main`);
  const deploy = (more = {}) => {
    writeFileSync(calls, '');
    const r = spawnSync('bash', [join(repo, 'scripts', 'deploy.sh')], { cwd: repo, env: { ...env, ...more }, encoding: 'utf8', timeout: 20_000 });
    const made = readFileSync(calls, 'utf8').trim().split('\n').filter(Boolean);
    return { status: r.status, stderr: r.stderr, calls: made, ssh: made.filter(c => c.startsWith('ssh ')) };
  };
  return { dir, sh, deploy, close: () => rmSync(dir, { recursive: true, force: true }) };
}

test('deploy.sh, every check passing: it gets as far as the server (a stand-in ssh that fails at once)', () => {
  const f = fixture();
  try {
    const run = f.deploy();
    assert.equal(run.status, 97, run.stderr);
    assert.equal(run.ssh.length, 1);
    assert.match(run.ssh[0], /^ssh claudeclaw mkdir -p \/home\/ubuntu\/ithomiini\/releases\/\d{8}T\d{6}Z /);
    for (const check of ['node scripts/check.mjs', 'node --test', 'npm --prefix frontend test', 'npx vite build'])
      assert.ok(run.calls.some(c => c.startsWith(check)), `${check} ran`);
  } finally {
    f.close();
  }
});

test('deploy.sh: the team repo has a commit this checkout lacks: exit 1, nothing tested, nothing sent to the server', () => {
  const f = fixture();
  try {
    // The T3 assistant's commit on the team repo, not pulled here.
    f.sh('git push -q team "$(git commit-tree -p HEAD -m assistant "HEAD^{tree}")":refs/heads/main');
    const run = f.deploy();
    assert.equal(run.status, 1, run.stderr);
    assert.match(run.stderr, /team\/main has commits this checkout lacks/);
    assert.deepEqual(run.ssh, []);
    assert.deepEqual(run.calls.filter(c => !c.startsWith('hostname')), [], 'no check, install or build either');
  } finally {
    f.close();
  }
});

test('deploy.sh: a failed check (the typecheck, the server or frontend tests, the build): "nothing was deployed", exit 1, nothing sent to the server', () => {
  const f = fixture();
  try {
    for (const job of ['check', 'server', 'frontend', 'build']) {
      const run = f.deploy({ DEPLOY_TEST_FAIL: job });
      assert.equal(run.status, 1, `${job}: ${run.stderr}`);
      assert.match(run.stderr, new RegExp(`== ${job} failed:`));
      assert.match(run.stderr, /Checks failed: nothing was deployed/);
      assert.deepEqual(run.ssh, [], `${job}: no ssh call`);
    }
  } finally {
    f.close();
  }
});
