// A release is installed only with a passing test record for exactly its commit, whose hashes match its files
// (scripts/deploy-record.mjs, scripts/install-release.sh), and a deploy started on claudeclaw installs only a PC's
// staged run of its HEAD unless DEPLOY_LOCAL_TESTS=1 (scripts/deploy.sh). No network, no ssh: the scripts run in a
// throwaway checkout with stand-ins (tests/helpers/deploy-fixture.mjs) against a scratch app root.
import test from 'node:test';
import assert from 'node:assert/strict';
import { chmodSync, existsSync, mkdirSync, mkdtempSync, readFileSync, readdirSync, readlinkSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { hashRelease, verifyRelease, writeRecord } from '../scripts/deploy-record.mjs';
import { t3ProvisionStamp } from '../scripts/deploy-stamp.mjs';
import { deployFixture } from './helpers/deploy-fixture.mjs';

const COMMIT = '1'.repeat(40);
const TREE = '2'.repeat(40);
const PASS = { check: 'pass', server: 'pass', frontend: 'pass', build: 'pass' };

function release() {
  const dir = mkdtempSync(join(tmpdir(), 'deploy-record-'));
  for (const [rel, text] of [['server/index.mjs', 'app'], ['server/instructions-history.json', '[]'], ['web/index.html', '<p>built</p>'], ['web/assets/a.js', 'x()'], ['scripts/deploy.sh', 'echo']])
    mkdirSync(join(dir, rel, '..'), { recursive: true }), writeFileSync(join(dir, rel), text);
  return dir;
}

test('deploy record: a pass for the commit, its build and files as tested: verified', () => {
  const dir = release();
  try {
    const written = writeRecord(dir, { commit: COMMIT, tree: TREE, jobs: PASS });
    assert.equal(written.node, process.version);
    assert.equal(written.release.files, 5);
    const r = verifyRelease(dir, { commit: COMMIT, tree: TREE });
    assert.equal(r.artifact.sha256, hashRelease(dir).artifact);
    // The links the install adds beside it change nothing.
    writeFileSync(join(dir, 'rename-app'), 'link stand-in');
    verifyRelease(dir, { commit: COMMIT });
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});

test('deploy record: another commit or tree, a job without a pass, no record, a changed build or file: refused', () => {
  const dir = release();
  try {
    assert.throws(() => verifyRelease(dir, { commit: COMMIT }), /No readable test record/);
    writeRecord(dir, { commit: COMMIT, tree: TREE, jobs: PASS });
    assert.throws(() => verifyRelease(dir, { commit: '3'.repeat(40) }), /record is for commit 1{40}, not 3{40}/);
    assert.throws(() => verifyRelease(dir, { commit: COMMIT, tree: '4'.repeat(40) }), /record is for tree/);
    assert.throws(() => verifyRelease(dir, {}), /commit to install is not given/);
    writeFileSync(join(dir, 'web', 'index.html'), '<p>another build</p>');
    assert.throws(() => verifyRelease(dir, { commit: COMMIT }), /built frontend differs from the tested one/);
    writeFileSync(join(dir, 'web', 'index.html'), '<p>built</p>');
    verifyRelease(dir, { commit: COMMIT });
    writeFileSync(join(dir, 'server', 'index.mjs'), 'changed');
    assert.throws(() => verifyRelease(dir, { commit: COMMIT }), /release files differ from the tested ones/);
    writeFileSync(join(dir, 'server', 'index.mjs'), 'app');
    writeFileSync(join(dir, 'server', 'extra.mjs'), 'not tested');
    assert.throws(() => verifyRelease(dir, { commit: COMMIT }), /release files differ/);
    rmSync(join(dir, 'server', 'extra.mjs'));
    writeRecord(dir, { commit: COMMIT, tree: TREE, jobs: { ...PASS, server: 'fail' } });
    assert.throws(() => verifyRelease(dir, { commit: COMMIT }), /no pass for: server/);
    writeRecord(dir, { commit: COMMIT, tree: TREE, jobs: { check: 'pass' } });
    assert.throws(() => verifyRelease(dir, { commit: COMMIT }), /no pass for: server, frontend, build/);
    rmSync(join(dir, 'web'), { recursive: true });
    assert.throws(() => writeRecord(dir, { commit: COMMIT, tree: TREE, jobs: PASS }), /built web is missing/);
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});

const releasesOf = f => (existsSync(join(f.app, 'releases')) ? readdirSync(join(f.app, 'releases')) : []);
const current = f => (existsSync(join(f.app, 'current')) ? readlinkSync(join(f.app, 'current')) : null);

test('deploy.sh on claudeclaw, no PC run of HEAD: refused with what to do, nothing tested, built or installed', () => {
  const f = deployFixture();
  try {
    const run = f.run([], { DEPLOY_LOCAL: '1' });
    assert.equal(run.status, 1, run.stderr);
    assert.match(run.stderr, /No passing PC test run of [0-9a-f]{40} here/);
    assert.match(run.stderr, /scripts\/deploy\.sh --stage/);
    assert.match(run.stderr, /DEPLOY_LOCAL_TESTS=1/);
    assert.deepEqual(run.calls.filter(c => !/^(hostname|node -e)/.test(c)), []);
    assert.deepEqual(releasesOf(f), []);
    assert.equal(current(f), null);
  } finally {
    f.close();
  }
});

test('deploy.sh on claudeclaw: a staged run for another commit, with another build, or with a failed job: refused, the app unchanged', () => {
  const f = deployFixture();
  try {
    for (const [why, opts, edit, message] of [
      ['another commit', { commit: 'a'.repeat(40) }, null, /record is for commit a{40}/],
      ['a failed job', { jobs: 'check=pass,server=fail,frontend=pass,build=pass' }, null, /no pass for: server/],
      ['another build', {}, staged => writeFileSync(join(staged, 'web', 'index.html'), '<p>swapped</p>\n'), /built frontend differs/],
    ]) {
      const staged = f.stage(opts);
      edit?.(staged);
      const run = f.run([], { DEPLOY_LOCAL: '1' });
      assert.equal(run.status, 1, `${why}: ${run.stderr}`);
      assert.match(run.stderr, message, why);
      assert.match(run.stderr, /Not installed: release \S+ does not match a passing test run/, why);
      assert.equal(current(f), null, why);
      assert.deepEqual(releasesOf(f), [], `${why}: the assembled release is removed`);
      assert.ok(!run.calls.some(c => c.startsWith('systemctl')), `${why}: no unit touched`);
      assert.ok(!run.calls.some(c => /node scripts\/(check|t3-provision|test-server|test-slow)\.mjs|node --test/.test(c)), `${why}: nothing tested here, no workspace refreshed`);
      rmSync(staged, { recursive: true });
    }
  } finally {
    f.close();
  }
});

test('deploy.sh on claudeclaw: the PC run of HEAD is installed without testing here; the workspaces refresh only when their inputs changed', () => {
  const f = deployFixture();
  try {
    f.stage();
    writeFileSync(join(f.app, 'shared', 'marker'), '');
    const first = f.run([], { DEPLOY_LOCAL: '1' });
    assert.equal(first.status, 0, first.stderr);
    assert.match(first.stdout, /Test record ok: [0-9a-f]{8} passed on pc/);
    assert.ok(!first.calls.some(c => /node scripts\/(check|test-server|test-slow)\.mjs|node --test|npm --prefix frontend test|vite build/.test(c)), 'no test job here');
    const [r1] = releasesOf(f);
    assert.equal(current(f), `releases/${r1}`);
    assert.equal(readFileSync(join(f.app, 'releases', r1, 'web', 'index.html'), 'utf8'), '<p>built</p>\n', 'the PC build');
    const systemctl = first.calls.filter(c => c.startsWith('systemctl'));
    assert.deepEqual(systemctl, [
      'systemctl --user daemon-reload',
      'systemctl --user enable -q ithomiini-fixture.service',
      'systemctl --user enable -q --now ithomiini-backup.timer',
      'systemctl --user restart ithomiini-fixture.service',
    ]);
    assert.ok(first.calls.some(c => /node scripts\/t3-provision\.mjs --refresh-all$/.test(c)), 'workspaces refreshed (no hash yet)');
    // Restart, then health: the restart is the last change and the health check comes after it.
    const order = first.calls.map(c => (c.includes('restart') ? 'restart' : c.startsWith('curl') ? 'curl' : null)).filter(Boolean);
    assert.equal(order.at(-1), 'curl');
    assert.ok(order.indexOf('restart') < order.lastIndexOf('curl'));

    // The same inputs again: no unit copied or reloaded, no workspace refresh; current → the new one (a release of
    // its own even within the same second), previous kept. Its restart leaves a process running (a stub's dummy app):
    // that process does not hold the install's lock, so the next deploy goes on at once.
    const second = f.run([], { DEPLOY_LOCAL: '1', DEPLOY_TEST_LINGER: '1' });
    assert.equal(second.status, 0, second.stderr);
    assert.match(second.stdout, /workspaces unchanged: not refreshed/);
    assert.ok(!second.calls.some(c => /t3-provision\.mjs/.test(c) && !/deploy-stamp/.test(c)));
    assert.ok(!second.calls.includes('systemctl --user daemon-reload'));
    const r2 = releasesOf(f).find(r => r !== r1);
    assert.ok(r2 > r1);
    assert.equal(current(f), `releases/${r2}`);
    assert.equal(readFileSync(join(f.app, 'shared', 'previous-release'), 'utf8').trim(), `releases/${r1}`);

    // A workspace changed (or the hash is lost): refreshed again.
    mkdirSync(join(f.app, 'shared', 't3-workspaces', 'someone'), { recursive: true });
    const third = f.run([], { DEPLOY_LOCAL: '1' });
    assert.equal(third.status, 0, third.stderr);
    assert.ok(third.calls.some(c => /node scripts\/t3-provision\.mjs --refresh-all$/.test(c)));
    writeFileSync(join(f.app, 'shared', '.deploy-stamps', 't3-provision'), 'unreadable\n');
    const fourth = f.run([], { DEPLOY_LOCAL: '1' });
    assert.equal(fourth.status, 0, fourth.stderr);
    assert.ok(fourth.calls.some(c => /node scripts\/t3-provision\.mjs --refresh-all$/.test(c)));
  } finally {
    f.close();
  }
});

test('deploy.sh on claudeclaw with DEPLOY_LOCAL_TESTS=1: the full tests here, then the install with a record of this run', () => {
  const f = deployFixture();
  try {
    const run = f.run([], { DEPLOY_LOCAL: '1', DEPLOY_LOCAL_TESTS: '1', DEPLOY_TEST_BUILD: 'local build' });
    assert.equal(run.status, 0, run.stderr);
    for (const check of ['node scripts/check.mjs', 'node scripts/test-server.mjs', 'npm --prefix frontend test', 'npx vite build'])
      assert.ok(run.calls.some(c => c.startsWith(check)), `${check} ran`);
    const [r] = releasesOf(f);
    const record = JSON.parse(readFileSync(join(f.app, 'releases', r, '.deploy-record.json'), 'utf8'));
    assert.equal(record.source, 'claudeclaw-local');
    assert.equal(record.commit, f.sh('git rev-parse HEAD'));
    assert.equal(readFileSync(join(f.app, 'releases', r, 'web', 'index.html'), 'utf8'), '<p>local build</p>\n');
    // A failed job installs nothing.
    const failed = f.run([], { DEPLOY_LOCAL: '1', DEPLOY_LOCAL_TESTS: '1', DEPLOY_TEST_FAIL: 'server' });
    assert.equal(failed.status, 1);
    assert.match(failed.stderr, /Checks failed: nothing was deployed/);
    assert.equal(current(f), `releases/${r}`);
  } finally {
    f.close();
  }
});

test('install-release.sh: a scratch root never installs the live units', () => {
  const f = deployFixture();
  try {
    f.stage();
    const run = f.run([], { DEPLOY_LOCAL: '1', ITHOMIINI_SERVICE: 'ithomiini.service' });
    assert.equal(run.status, 1);
    assert.match(run.stderr, /A scratch root .* needs ITHOMIINI_SYSTEMD_DIR and ITHOMIINI_SERVICE of its own/);
    assert.ok(!run.calls.some(c => c.startsWith('systemctl')));
    assert.equal(current(f), null);
  } finally {
    f.close();
  }
});

test('the T3 workspaces hash: the same from a PC and from claudeclaw (file modes aside), another when an input changes', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'deploy-stamp-'));
  try {
    const release = join(dir, 'release');
    const shared = join(dir, 'shared');
    mkdirSync(join(release, 'assistant', 'skills', 'a'), { recursive: true });
    mkdirSync(join(shared, 't3-workspaces', 'pato', '.claude'), { recursive: true });
    writeFileSync(join(release, 'assistant', 'AGENTS.md'), 'brief');
    writeFileSync(join(release, 'assistant', 'skills', 'a', 'SKILL.md'), 'skill');
    const env = { HOME: join(dir, 'home'), ITHOMIINI_SHARED: shared };
    chmodSync(join(release, 'assistant', 'AGENTS.md'), 0o664);
    const first = await t3ProvisionStamp(release, env);
    chmodSync(join(release, 'assistant', 'AGENTS.md'), 0o600);
    assert.equal(await t3ProvisionStamp(release, env), first, 'a PC umask (600) and the claudeclaw one (664): one hash');
    writeFileSync(join(release, 'assistant', 'skills', 'a', 'SKILL.md'), 'skill, changed');
    const second = await t3ProvisionStamp(release, env);
    assert.notEqual(second, first, 'a skill changed');
    writeFileSync(join(shared, 't3-workspaces', 'pato', 'AGENTS.md'), 'edited by hand');
    assert.notEqual(await t3ProvisionStamp(release, env), second, 'the brief of a workspace changed');
    assert.notEqual(await t3ProvisionStamp(release, { ...env, ITHOMIINI_CLAUDE_LEAN: '0' }), await t3ProvisionStamp(release, env), 'a provision variable');
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});
