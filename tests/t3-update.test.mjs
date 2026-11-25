import test from 'node:test';
import assert from 'node:assert/strict';
import { execFile } from 'node:child_process';
import { createServer } from 'node:http';
import { existsSync, mkdirSync, mkdtempSync, readFileSync, readdirSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { DatabaseSync } from 'node:sqlite';
import { fileURLToPath } from 'node:url';

// scripts/t3-update.mjs, as the unit ithomiini-t3-update@<version> runs it: against a T3 home on disk,
// a stand-in `t3` (its update, auth commands) and a stand-in T3 server answering on a local port.

const SCRIPT = fileURLToPath(new URL('../scripts/t3-update.mjs', import.meta.url));
const NEW_TOKEN = 'new-token-0123456789abcdef';

function setup() {
  const root = mkdtempSync(join(tmpdir(), 't3-update-'));
  const home = join(root, 'home');
  mkdirSync(join(home, 'userdata', 'logs'), { recursive: true });
  mkdirSync(join(home, 'runtime', 'versions', '0.0.45'), { recursive: true });
  writeFileSync(join(home, 'runtime', 'versions', '0.0.45', '.install-complete'), '0.0.45\n');
  writeFileSync(join(home, 'runtime', 'service-state.json'), JSON.stringify({ protocol: 3, activeVersion: '0.0.45' }));
  writeFileSync(join(home, 'userdata', 'settings.json'), '{"a":1}');
  const db = new DatabaseSync(join(home, 'userdata', 'state.sqlite'));
  db.exec("CREATE TABLE projection_threads (thread_id TEXT); INSERT INTO projection_threads VALUES ('chat-1')");
  db.close();
  // The version the stand-in server says it runs (its `t3 update` changes it).
  writeFileSync(join(root, 'version'), '0.0.45');
  writeFileSync(join(root, 'token'), 'old-token\n');
  // The stand-in t3: `update [flags] <version>` installs it; auth issue / list / revoke.
  const t3 = join(root, 't3');
  writeFileSync(
    t3,
    `#!/bin/sh
echo "$@" >> '${root}/t3-calls'
case "$1 $2 $3" in
  "auth session issue") echo '${NEW_TOKEN}'; exit 0 ;;
  "auth session list") echo '[{"sessionId":"s-new","method":"bearer-access-token","client":{"label":"broker"},"issuedAt":"2026-10-19T10:00:00Z"},{"sessionId":"s-old","method":"bearer-access-token","client":{"label":"broker"},"issuedAt":"2026-09-28T00:00:00Z"},{"sessionId":"s-ana","method":"browser-session-cookie","client":{"label":"ithomiini:ana"},"issuedAt":"2026-10-01T00:00:00Z"}]'; exit 0 ;;
  "auth session revoke") exit 0 ;;
esac
if [ "$1" = update ]; then
  for v; do :; done
  [ "$v" = 0.9.9 ] && { echo "No release archive was published for t3@0.9.9." >&2; exit 1; }
  mkdir -p '${home}/runtime/versions/'"$v" && echo "$v" > '${home}/runtime/versions/'"$v"/.install-complete
  echo "Installed T3 Code $v"
  printf '%s' "$v" > '${root}/version'
  exit 0
fi
exit 2
`,
    { mode: 0o755 },
  );
  const server = createServer((req, res) => {
    if (req.url === '/.well-known/t3/environment') {
      const v = readFileSync(join(root, 'version'), 'utf8');
      res.writeHead(200, { 'content-type': 'application/json' });
      return res.end(JSON.stringify({ serverVersion: v, orchestrationProtocolVersion: v === '0.1.0' ? 2 : 1 }));
    }
    if (req.url === '/api/auth/session') {
      const ok = req.headers.authorization === `Bearer ${NEW_TOKEN}`;
      res.writeHead(ok ? 200 : 401, { 'content-type': 'application/json' });
      return res.end(JSON.stringify({ authenticated: ok }));
    }
    res.writeHead(404).end();
  });
  return { root, home, t3, server };
}

const run = (args, env = {}) =>
  new Promise(resolve =>
    execFile(process.execPath, [SCRIPT, ...args], { env: { ...process.env, ...env }, timeout: 30_000 }, (error, stdout, stderr) =>
      resolve({ code: error ? error.code : 0, stdout, stderr }),
    ),
  );

test('an update: the database copied first, t3 update to the version asked, T3 waited for, the app token issued again and the old one revoked', async () => {
  const { root, home, t3, server } = setup();
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  const local = `http://127.0.0.1:${server.address().port}`;
  try {
    const common = ['--home', home, '--t3', t3, '--local', local, '--wait', '10', '--token-file', join(root, 'token'), '--token-label', 'broker'];
    // A shell opened by another T3 carries its settings: they do not reach `t3`.
    const out = await run(['0.1.0', ...common], { T3_SERVICE_LAUNCHER_CONTEXT: '{"childVersion":"9"}', T3_BOOT_SERVICE_UNIT: 'other.service' });
    assert.equal(out.code, 0, out.stdout + out.stderr);
    assert.match(out.stdout, /T3 Code 0\.0\.45 → 0\.1\.0/);
    assert.ok(!out.stdout.includes(NEW_TOKEN), 'the token is never logged');
    const status = JSON.parse(readFileSync(join(home, 'userdata', 'logs', 't3-update.json'), 'utf8'));
    assert.deepEqual([status.step, status.from, status.to, status.back, status.protocolAfter], ['done', '0.0.45', '0.1.0', false, 2]);
    assert.deepEqual(status.steps.map(s => s.step), ['check', 'backup', 't3 update', 'T3 answers', 'token']);
    // The copy: a database that opens, and the settings beside it.
    const [backup] = readdirSync(join(home, 'backups'));
    assert.match(backup, /^\d{4}-\d\d-\d\dT\d\d-\d\d-\d\d-\d{3}Z-0\.0\.45$/);
    const copy = new DatabaseSync(join(home, 'backups', backup, 'state.sqlite'), { readOnly: true });
    assert.deepEqual(copy.prepare('SELECT thread_id FROM projection_threads').all().map(r => r.thread_id), ['chat-1']);
    copy.close();
    assert.ok(existsSync(join(home, 'backups', backup, 'settings.json')));
    assert.ok(existsSync(join(home, 'backups', backup, 'service-state.json')));
    assert.equal(readFileSync(join(home, 'backups', backup, 'app-token'), 'utf8'), 'old-token\n', 'the token before, to put back by hand');
    // t3 was asked for that version, in this home, and the app's token was made again (the old one revoked; people's sessions kept).
    const calls = readFileSync(join(root, 't3-calls'), 'utf8').trim().split('\n');
    assert.equal(calls[0], `update --base-dir ${home} --yes 0.1.0`);
    assert.match(calls[1], /^auth session issue --base-dir .* --label broker --ttl 3650d --token-only$/);
    assert.equal(calls.at(-1), `auth session revoke --base-dir ${home} s-old`);
    assert.equal(calls.filter(c => c.includes('revoke')).length, 1);
    assert.equal(readFileSync(join(root, 'token'), 'utf8'), `${NEW_TOKEN}\n`);

    // Again on the same version: nothing to do.
    const again = await run(['0.1.0', ...common]);
    assert.equal(again.code, 0);
    assert.match(again.stdout, /Already on this version/);

    // Back to a version T3 kept: allowed (--allow-downgrade); to one it never had: refused before anything changes.
    const back = await run(['0.0.45', ...common]);
    assert.equal(back.code, 0, back.stdout + back.stderr);
    assert.match(readFileSync(join(root, 't3-calls'), 'utf8'), /update --base-dir \S+ --yes --allow-downgrade 0\.0\.45/);
    const never = await run(['0.0.40', ...common]);
    assert.equal(never.code, 1);
    assert.match(never.stdout, /0\.0\.40 is not in .*only a version T3 had before/);
    assert.equal(JSON.parse(readFileSync(join(home, 'userdata', 'logs', 't3-update.json'), 'utf8')).step, 'failed');

    // t3 update failing: the run says so, and T3 keeps its version.
    const broken = await run(['0.9.9', ...common]);
    assert.equal(broken.code, 1);
    assert.match(broken.stdout, /t3 update ended with code 1 \(T3 keeps running 0\.0\.45\)/);
    assert.equal(readdirSync(join(home, 'backups')).length, 3, 'the newest 3 copies stay');
    assert.match(await run(['latest', ...common]).then(r => r.stdout), /not a stable version/);
  } finally {
    server.close();
  }
});

test('downgrade after T3 0.1.11+ prunes runtime/versions/<from>: restores runtime and pre-update statev2.sqlite cleanly, and rolls back DB if t3 update fails', async () => {
  const root = mkdtempSync(join(tmpdir(), 't3-update-v2-'));
  const home = join(root, 'home');
  mkdirSync(join(home, 'userdata', 'logs'), { recursive: true });
  mkdirSync(join(home, 'runtime', 'versions', '0.1.9'), { recursive: true });
  writeFileSync(join(home, 'runtime', 'versions', '0.1.9', '.install-complete'), '0.1.9\n');
  writeFileSync(join(home, 'runtime', 'versions', '0.1.9', 'marker.txt'), 'runtime-0.1.9\n');
  writeFileSync(join(home, 'runtime', 'service-state.json'), JSON.stringify({ protocol: 3, activeVersion: '0.1.9' }));
  const db0 = new DatabaseSync(join(home, 'userdata', 'statev2.sqlite'));
  db0.exec("CREATE TABLE migration_log (id INTEGER PRIMARY KEY, note TEXT); INSERT INTO migration_log VALUES (81, 'pre-0.1.11');");
  db0.close();
  writeFileSync(join(root, 'version'), '0.1.9');
  writeFileSync(join(root, 'fail-downgrade'), '0');
  writeFileSync(join(root, 'token'), 'tok-019\n');

  const t3 = join(root, 't3');
  writeFileSync(
    t3,
    `#!/bin/sh
echo "$@" >> '${root}/t3-calls'
case "$1 $2 $3" in
  "auth session issue") echo '${NEW_TOKEN}'; exit 0 ;;
  "auth session list") echo '[]'; exit 0 ;;
  "auth session revoke") exit 0 ;;
esac
if [ "$1" = update ]; then
  for v; do :; done
  if [ "$v" = 0.1.11 ]; then
    mkdir -p '${home}/runtime/versions/0.1.11' && echo "0.1.11" > '${home}/runtime/versions/0.1.11/.install-complete'
    # Simulate T3 0.1.11 pruning 0.1.9 on startup and migrating statev2.sqlite.
    rm -rf '${home}/runtime/versions/0.1.9'
    printf '%s' "0.1.11" > '${root}/version'
    exit 0
  fi
  if [ "$v" = 0.1.9 ]; then
    if [ "$(cat '${root}/fail-downgrade')" = 1 ]; then
      echo "simulated downgrade failure" >&2
      exit 1
    fi
    [ -f '${home}/runtime/versions/0.1.9/.install-complete' ] || { echo "missing 0.1.9 runtime" >&2; exit 1; }
    printf '%s' "0.1.9" > '${root}/version'
    exit 0
  fi
fi
exit 2
`,
    { mode: 0o755 },
  );
  const server = createServer((req, res) => {
    if (req.url === '/.well-known/t3/environment') {
      const v = readFileSync(join(root, 'version'), 'utf8');
      res.writeHead(200, { 'content-type': 'application/json' });
      return res.end(JSON.stringify({ serverVersion: v, orchestrationProtocolVersion: 2, databaseMigration: v === '0.1.11' ? 102 : 81 }));
    }
    if (req.url === '/api/auth/session') {
      res.writeHead(200, { 'content-type': 'application/json' });
      return res.end(JSON.stringify({ authenticated: true }));
    }
    res.writeHead(404).end();
  });
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  const local = `http://127.0.0.1:${server.address().port}`;
  const stopLog = join(root, 'stops');
  const common = [
    '--home', home, '--t3', t3, '--local', local, '--wait', '10',
    '--stop', `echo stopped >> '${stopLog}'`,
    '--token-file', join(root, 'token'), '--token-label', 'broker',
  ];
  try {
    // 1. Forward update 0.1.9 → 0.1.11: backs up statev2.sqlite + runtime/versions/0.1.9, then 0.1.11 prunes runtime/versions/0.1.9.
    const up = await run(['0.1.11', ...common]);
    assert.equal(up.code, 0, up.stdout + up.stderr);
    assert.equal(existsSync(join(home, 'runtime', 'versions', '0.1.9')), false, 'simulated 0.1.11 pruned 0.1.9');
    const [b019] = readdirSync(join(home, 'backups')).filter(n => n.endsWith('-0.1.9'));
    assert.ok(b019, '0.1.9 backup folder exists');
    assert.equal(readFileSync(join(home, 'backups', b019, 'runtime-version', 'marker.txt'), 'utf8'), 'runtime-0.1.9\n');
    const meta = JSON.parse(readFileSync(join(home, 'backups', b019, 'backup-meta.json'), 'utf8'));
    assert.deepEqual([meta.version, meta.databaseMigration, meta.runtimeBackedUp], ['0.1.9', 81, true]);

    // Simulate 0.1.11 migrating statev2.sqlite + leaving stale WAL/SHM files.
    const db11 = new DatabaseSync(join(home, 'userdata', 'statev2.sqlite'));
    db11.exec("INSERT INTO migration_log VALUES (102, 'migrated-0.1.11');");
    db11.close();
    writeFileSync(join(home, 'userdata', 'statev2.sqlite-wal'), 'stale-wal');
    writeFileSync(join(home, 'userdata', 'statev2.sqlite-shm'), 'stale-shm');

    // 2. If t3 update fails during downgrade after DB swap, statev2.sqlite is rolled back to 0.1.11's backup!
    writeFileSync(join(root, 'fail-downgrade'), '1');
    const failBack = await run(['0.1.9', ...common]);
    assert.equal(failBack.code, 1);
    assert.match(failBack.stdout, /Rolled back statev2\.sqlite/);
    const dbAfterFail = new DatabaseSync(join(home, 'userdata', 'statev2.sqlite'), { readOnly: true });
    assert.deepEqual(dbAfterFail.prepare('SELECT id FROM migration_log ORDER BY id').all().map(r => r.id), [81, 102]);
    dbAfterFail.close();

    // 3. Successful downgrade 0.1.11 → 0.1.9: restores pruned runtime + restores 0.1.9 statev2.sqlite + removes WAL/SHM.
    writeFileSync(join(root, 'fail-downgrade'), '0');
    rmSync(join(home, 'runtime', 'versions', '0.1.9'), { recursive: true, force: true });
    writeFileSync(join(home, 'userdata', 'statev2.sqlite-wal'), 'stale-wal-2');
    const down = await run(['0.1.9', ...common]);
    assert.equal(down.code, 0, down.stdout + down.stderr);
    assert.equal(readFileSync(join(home, 'runtime', 'versions', '0.1.9', 'marker.txt'), 'utf8'), 'runtime-0.1.9\n');
    assert.equal(existsSync(join(home, 'userdata', 'statev2.sqlite-wal')), false, 'stale WAL removed');
    assert.equal(existsSync(join(home, 'userdata', 'statev2.sqlite-shm')), false, 'stale SHM removed');
    assert.ok(readFileSync(stopLog, 'utf8').includes('stopped'), 'stop command ran before DB swap');
    const dbRestored = new DatabaseSync(join(home, 'userdata', 'statev2.sqlite'), { readOnly: true });
    assert.deepEqual(dbRestored.prepare('SELECT id FROM migration_log ORDER BY id').all().map(r => r.id), [81]);
    dbRestored.close();
    const downStatus = JSON.parse(readFileSync(join(home, 'userdata', 'logs', 't3-update.json'), 'utf8'));
    assert.ok(downStatus.steps.some(s => s.step === 'restore' && s.detail.includes('runtime 0.1.9') && s.detail.includes('statev2.sqlite')));

    // 4. Corrupt backup DB fails fast before touching anything.
    writeFileSync(join(root, 'version'), '0.1.11');
    writeFileSync(join(home, 'backups', b019, 'statev2.sqlite'), 'not a sqlite file');
    const corrupt = await run(['0.1.9', ...common]);
    assert.equal(corrupt.code, 1);
    assert.match(corrupt.stdout, /failed integrity check/);
  } finally {
    server.close();
    rmSync(root, { recursive: true, force: true });
  }
});
