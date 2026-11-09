// A throwaway checkout for the tests of scripts/deploy.sh and scripts/install-release.sh: the real deploy scripts in a
// git repo with one small file per shipped path, origin and team remotes (bare repos in the same temp folder), and
// stand-ins on PATH that record their calls. ssh only records and fails, so no step can reach claudeclaw even if a
// guard is broken; systemctl only records; curl answers a healthy, idle /health; node runs the record and stamp
// scripts (and `node -e`) for real and only records the rest (the test jobs, t3-provision). HOME is a temp folder and
// the app root (ITHOMIINI_APP_ROOT) a scratch one, with its own unit folder and service name.
import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { chmodSync, copyFileSync, mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const root = fileURLToPath(new URL('../..', import.meta.url));
export const STANDINS = ['ssh', 'node', 'npm', 'npx', 'hostname', 'systemctl', 'curl'];
const REAL = ['scripts/deploy.sh', 'scripts/install-release.sh', 'scripts/deploy-record.mjs', 'scripts/deploy-stamp.mjs', 'scripts/wait-writes.sh', 'scripts/prune-releases.sh'];

// One stand-in for all: records "<name> <args>"; `hostname` is a PC's, ssh fails at once, and the check named in
// DEPLOY_TEST_FAIL (check, server, frontend, build) exits 1. The build writes web/ and the instructions history.
const STANDIN = `#!/bin/bash
name="\${0##*/}"
printf '%s %s\\n' "$name" "$*" >> "$DEPLOY_TEST_CALLS"
case "$name $*" in
  "hostname"*) echo test-pc; exit 0 ;;
  "node --version") echo v24.0.0-standin; exit 0 ;;
  "node -e"* | "node "*deploy-record.mjs* | "node "*deploy-stamp.mjs*) exec "$DEPLOY_TEST_REAL_NODE" "$@" ;;
  "ssh "*) exit 97 ;;
  "curl "*) echo '{"status":"ok","writing":{"applying":0,"inFlight":0,"unconfirmed":0}}'; exit 0 ;;
  "node scripts/check.mjs"*) job=check ;;
  "node --test"*) job=server ;;
  "npm --prefix frontend test"*) job=frontend ;;
  "node scripts/instructions-history.mjs"*) echo '[]' > server/instructions-history.json ;;
  "npx vite build"*) job=build ;;
esac
if [ -n "\${job:-}" ] && [ "$job" = "\${DEPLOY_TEST_FAIL:-}" ]; then echo "$job failed (stand-in)"; exit 1; fi
# DEPLOY_TEST_LINGER: a restart leaves a process running (as a stub restarting a dummy app does).
if [ "$name" = systemctl ] && [[ "$*" == *restart* ]] && [ -n "\${DEPLOY_TEST_LINGER:-}" ]; then (sleep 45 >/dev/null 2>&1 </dev/null &); fi
if [ "\${job:-}" = build ]; then mkdir -p ../web && echo "<p>\${DEPLOY_TEST_BUILD:-built}</p>" > ../web/index.html; fi
exit 0
`;

export function deployFixture() {
  const dir = mkdtempSync(join(tmpdir(), 'deploy-fixture-'));
  const bin = join(dir, 'bin');
  const home = join(dir, 'home');
  const repo = join(dir, 'repo');
  const app = join(dir, 'app');
  const team = join(dir, 'remote', 'rapidspeciation', 'Shiny_Ikiam_Database_Manager.git');
  const origin = join(dir, 'remote', 'origin.git');
  for (const d of [bin, home, repo, join(app, 'shared'), join(app, 'releases'), dirname(team)]) mkdirSync(d, { recursive: true });
  for (const name of STANDINS) {
    writeFileSync(join(bin, name), STANDIN);
    chmodSync(join(bin, name), 0o755);
  }
  const put = (rel, text, mode) => {
    mkdirSync(dirname(join(repo, rel)), { recursive: true });
    writeFileSync(join(repo, rel), text);
    if (mode) chmodSync(join(repo, rel), mode);
  };
  for (const rel of REAL) {
    mkdirSync(dirname(join(repo, rel)), { recursive: true });
    copyFileSync(join(root, rel), join(repo, rel));
  }
  // One small file for each shipped path; the steps that would build or install for real are stand-ins.
  for (const rel of ['server/index.mjs', 'docs/a.md', 'assistant/AGENTS.md', 'package.json', 'rename-app.commit', 'licenses/x.txt', 'PRODUCT.md', 'DESIGN.md', 'frontend/src/lib/a.ts', 'tools/identifier/a.py'])
    put(rel, `${rel}\n`);
  put('scripts/build-rename-app.sh', 'echo "Rename Photos app: up to date (stand-in)"\n');
  put('tools/wikiloc/install-worker.sh', 'echo "Wikiloc worker unchanged: not reinstalled. (stand-in)"\n');
  put('deploy/ithomiini.service', `[Service]\nExecStart=${join(bin, 'node')} server/index.mjs\n`);
  put('deploy/ithomiini-backup.timer', '[Timer]\n');
  put('frontend/package-lock.json', '{}\n');
  put('frontend/node_modules/.package-lock.json', '{}\n');
  put('.gitignore', 'node_modules/\nweb/\nserver/instructions-history.json\n');
  const calls = join(dir, 'calls');
  // Only what the scripts need: no DEPLOY_LOCAL, no ssh agent, no git config of this machine.
  const env = {
    PATH: `${bin}:${process.env.PATH}`,
    HOME: home,
    LANG: 'C.UTF-8',
    ITHOMIINI_DEPLOY_CACHE: join(dir, 'cache'),
    ITHOMIINI_APP_ROOT: app,
    ITHOMIINI_SYSTEMD_DIR: join(dir, 'units'),
    ITHOMIINI_SERVICE: 'ithomiini-fixture.service',
    ITHOMIINI_TIMERS: 'ithomiini-backup.timer',
    DEPLOY_PUBLIC_URL: '',
    DEPLOY_TEST_CALLS: calls,
    DEPLOY_TEST_REAL_NODE: process.execPath,
    GIT_CONFIG_NOSYSTEM: '1',
    GIT_AUTHOR_NAME: 'test',
    GIT_AUTHOR_EMAIL: 'test@example.invalid',
    GIT_COMMITTER_NAME: 'test',
    GIT_COMMITTER_EMAIL: 'test@example.invalid',
  };
  const sh = (cmd, cwd = repo, more = {}) => {
    const r = spawnSync('bash', ['-c', cmd], { cwd, env: { ...env, ...more }, encoding: 'utf8' });
    assert.equal(r.status, 0, `${cmd}: ${r.stderr}`);
    return r.stdout.trim();
  };
  // Every command the script could reach the server with is a stand-in here.
  for (const name of STANDINS) assert.equal(sh(`command -v ${name}`), join(bin, name));
  sh(`git init -q --bare -b main ${JSON.stringify(team)} && git init -q --bare -b main ${JSON.stringify(origin)}`, dir);
  sh('git init -q -b main && git add -A && git commit -q -m one');
  sh(`git remote add team ${JSON.stringify(team)} && git push -q team main && git remote add origin ${JSON.stringify(origin)} && git push -q origin main`);
  const run = (args = [], more = {}) => {
    writeFileSync(calls, '');
    const r = spawnSync('bash', [join(repo, 'scripts', 'deploy.sh'), ...args], { cwd: repo, env: { ...env, ...more }, encoding: 'utf8', timeout: 30_000 });
    const made = readFileSync(calls, 'utf8').trim().split('\n').filter(Boolean);
    return { status: r.status, stdout: r.stdout, stderr: r.stderr, calls: made, ssh: made.filter(c => c.startsWith('ssh ')) };
  };
  /** A PC's staged run of HEAD on the "server" (app/staged/<commit>): the commit's files, a build and its record. */
  const stage = ({ build = 'built', jobs = 'check=pass,server=pass,frontend=pass,build=pass', commit } = {}) => {
    const head = sh('git rev-parse HEAD');
    const staged = join(app, 'staged', head);
    mkdirSync(join(staged, 'web'), { recursive: true });
    sh(`git archive HEAD -- server docs assistant package.json rename-app.commit deploy scripts licenses PRODUCT.md DESIGN.md frontend/src/lib tools/wikiloc tools/identifier | tar -xf - -C ${JSON.stringify(staged)}`);
    writeFileSync(join(staged, 'web', 'index.html'), `<p>${build}</p>\n`);
    writeFileSync(join(staged, 'server', 'instructions-history.json'), '[]\n');
    const r = spawnSync(process.execPath, [join(repo, 'scripts', 'deploy-record.mjs'), 'write', staged, '--commit', commit ?? head, '--tree', sh('git rev-parse "HEAD^{tree}"'), '--jobs', jobs], { encoding: 'utf8' });
    assert.equal(r.status, 0, r.stderr);
    return staged;
  };
  return { dir, repo, app, home, sh, run, stage, close: () => rmSync(dir, { recursive: true, force: true }) };
}
