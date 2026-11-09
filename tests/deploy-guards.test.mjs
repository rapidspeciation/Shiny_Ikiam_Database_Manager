// scripts/deploy.sh refuses to deploy, and touches the server not at all, when the team repo has commits this
// checkout lacks (the T3 assistant commits there too), when the checkout is not exactly a pushed commit, or when one
// of the checks fails. The real script runs in a throwaway git repo (tests/helpers/deploy-fixture.mjs); ssh, node,
// npm, npx and hostname are stand-ins on PATH that record their calls (ssh only records and fails, so no step can
// reach claudeclaw even if a guard is broken), and HOME is a temp folder.
import test from 'node:test';
import assert from 'node:assert/strict';
import { writeFileSync } from 'node:fs';
import { join } from 'node:path';
import { deployFixture } from './helpers/deploy-fixture.mjs';

test('deploy.sh, every check passing: it gets as far as the server (a stand-in ssh that fails at once)', () => {
  const f = deployFixture();
  try {
    const run = f.run();
    assert.notEqual(run.status, 0, run.stderr);
    // The one connection: rsync uploading the release into releases/<UTC time> (nothing installed before it), and
    // the shared connection closed at the end.
    assert.equal(run.ssh.length, 2, run.ssh.join('\n'));
    assert.match(run.ssh[1], / -O exit claudeclaw$/);
    // A new release folder only: never an upload into an existing release.
    assert.match(run.ssh[0], /^ssh .*-o ControlMaster=auto .*claudeclaw mkdir -p '[^']+\/app\/releases' && test ! -e '[^']+\/app\/releases\/\d{8}T\d{6}Z' && rsync --server /);
    assert.match(run.ssh[0], /\/app\/releases\/\d{8}T\d{6}Z\/$/);
    for (const check of ['node scripts/check.mjs', 'node --test', 'npm --prefix frontend test', 'npx vite build'])
      assert.ok(run.calls.some(c => c.startsWith(check)), `${check} ran`);
    assert.match(run.stdout, /Test record: [0-9a-f]{8}, check pass, server pass, frontend pass, build pass/);
  } finally {
    f.close();
  }
});

test('deploy.sh: the team repo has a commit this checkout lacks: exit 1, nothing tested, nothing sent to the server', () => {
  const f = deployFixture();
  try {
    // The T3 assistant's commit on the team repo, not pulled here.
    f.sh('git push -q team "$(git commit-tree -p HEAD -m assistant "HEAD^{tree}")":refs/heads/main');
    const run = f.run();
    assert.equal(run.status, 1, run.stderr);
    assert.match(run.stderr, /team\/main has commits this checkout lacks/);
    assert.deepEqual(run.ssh, []);
    assert.deepEqual(run.calls.filter(c => !c.startsWith('hostname')), [], 'no check, install or build either');
  } finally {
    f.close();
  }
});

test('deploy.sh: a checkout that is not exactly a pushed commit (changed or new files, a commit not on the team repo or origin): exit 1, nothing tested or sent', () => {
  const f = deployFixture();
  try {
    writeFileSync(join(f.repo, 'server', 'index.mjs'), 'changed\n');
    let run = f.run();
    assert.equal(run.status, 1, run.stderr);
    assert.match(run.stderr, /Commit or remove these first/);
    f.sh('git checkout -q -- server/index.mjs');
    writeFileSync(join(f.repo, 'server', 'new.mjs'), 'untracked\n');
    run = f.run();
    assert.equal(run.status, 1, run.stderr);
    assert.match(run.stderr, /Commit or remove these first[\s\S]*server\/new\.mjs/);
    f.sh('git add -A && git commit -q -m two');
    run = f.run();
    assert.equal(run.status, 1, run.stderr);
    assert.match(run.stderr, /HEAD is not on team\/main yet/);
    f.sh('git push -q team main');
    run = f.run();
    assert.equal(run.status, 1, run.stderr);
    assert.match(run.stderr, /HEAD is not on origin\/main yet/);
    f.sh('git push -q origin "$(git commit-tree -p HEAD -m other "HEAD^{tree}")":refs/heads/main');
    run = f.run();
    assert.equal(run.status, 1, run.stderr);
    assert.match(run.stderr, /origin\/main has commits this checkout lacks/);
    for (const r of [run]) assert.deepEqual(r.calls.filter(c => !c.startsWith('hostname')), [], 'nothing tested or sent');
  } finally {
    f.close();
  }
});

test('deploy.sh: a failed check (the typecheck, the server or frontend tests, the build): "nothing was deployed", exit 1, nothing sent to the server', () => {
  const f = deployFixture();
  try {
    for (const job of ['check', 'server', 'frontend', 'build']) {
      const run = f.run([], { DEPLOY_TEST_FAIL: job });
      assert.equal(run.status, 1, `${job}: ${run.stderr}`);
      assert.match(run.stderr, new RegExp(`== ${job} failed:`));
      assert.match(run.stderr, /Checks failed: nothing was deployed/);
      assert.deepEqual(run.ssh, [], `${job}: no ssh call`);
    }
  } finally {
    f.close();
  }
});
