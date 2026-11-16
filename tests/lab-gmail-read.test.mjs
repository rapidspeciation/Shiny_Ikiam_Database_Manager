import { test } from 'node:test';
import assert from 'node:assert/strict';
import { execFileSync, spawnSync } from 'node:child_process';
import { chmodSync, existsSync, mkdirSync, mkdtempSync, readFileSync, writeFileSync } from 'node:fs';
import { homedir, tmpdir } from 'node:os';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { ACCOUNT, classify, decide, runsGog } from '../scripts/lab-gmail-read.mjs';
import { evalSettings, GOG_DENY } from '../tools/assistant-eval/eval-settings.mjs';

mkdirSync(join(homedir(), '.cache', 'ithomiini-test'), { recursive: true });
const hookDir = mkdtempSync(join(homedir(), '.cache', 'ithomiini-test', 'lab-hooks-'));
const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');
// The google-account skill's command, as the S07 chat wrote it (8 Nov 2026 eval run).
const SKILL_READ = `set -a; . ~/.config/ithomiini/gog.env; set +a\ngog --readonly --account ${ACCOUNT} --client ithomiini gmail search 'LBI OR depósito newer_than:10d' 2>&1 | head -40`;

test('Gmail reads are planned with gog\'s guards on the project account; writes, other services and secrets are refused', () => {
  const r = classify(SKILL_READ).read;
  assert.ok(r, 'the skill\'s search is a read');
  assert.deepEqual(r.argv.slice(0, 3), ['--readonly', '--gmail-no-send', '--no-input']);
  assert.match(r.argv[3], /^--enable-commands-exact=gmail\.search,/);
  assert.ok(r.argv.includes(`--account=${ACCOUNT}`) && r.argv.includes('--client=ithomiini'));
  assert.deepEqual(r.argv.slice(-4), ['gmail', 'search', '--', 'LBI OR depósito newer_than:10d']);
  assert.equal(r.tail, 'head -40');
  assert.equal(r.stderr, true);
  for (const cmd of ['gog gmail get 19a --format metadata --json', 'gog gmail thread get 19a', 'gog gmail labels list', 'gog gmail messages search "from:lbi" --max 5 | jq .', 'gog mail find x'])
    assert.ok(classify(cmd).read, cmd);
  for (const cmd of [
    'gog gmail send --to a@example.org --subject s --body b',
    'gog --readonly gmail send --to a@example.org',
    'gog gmail reply 19a --body ok',
    'gog gmail drafts create --to a@example.org',
    'gog gmail labels create Nueva',
    'gog gmail labels modify 19a --add X',
    'gog gmail thread modify 19a --remove UNREAD',
    'gog gmail trash 19a',
    'gog gmail mark-read 19a',
    'gog gmail thread get 19a --download --out-dir /home',
    'gog drive ls',
    'gog -a someone@example.org gmail search x',
    'gog --access-token=abc gmail search x',
    'gog gmail search x; gog gmail send --to y',
    'gog gmail search x | gog gmail send --to y',
    'gog gmail search x | sh',
    'gog gmail search x > /tmp/out',
    'gog gmail search "$(cat q)"',
    'bash -c "gog gmail send --to y"',
    'timeout 20 /home/x/.local/bin/gog gmail send --to y',
    'cat ~/.config/gogcli/config.json',
    'GOG_KEYRING_PASSWORD=x gog gmail search y',
  ]) assert.ok(classify(cmd).deny, cmd);
  for (const cmd of ['ls -la', 'grep -rn gog server/', 'echo "gog gmail send"', 'node scripts/check.mjs']) {
    assert.equal(runsGog(cmd), false, cmd);
    assert.equal(classify(cmd).pass, true, cmd);
  }
});

test('the eval workspace allows a gog Gmail read (run by the hook, outside the sandbox) and refuses a send', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'lab-gmail-'));
  // A stand-in gog: records each call and prints its arguments (no Google, no keyring).
  const calls = join(dir, 'calls.txt');
  const gog = join(dir, 'gog');
  writeFileSync(gog, `#!/bin/sh\necho "$@" >> '${calls}'\necho "fake gog: $*"\n`);
  chmodSync(gog, 0o755);
  // The lab workspace's settings as provisioning leaves them (no Gmail hook in an older one), then the eval's.
  const lab = { permissions: { allow: ['Skill', 'mcp__db'], deny: ['Bash(*wacli*)', 'Bash(gog*)'] }, sandbox: { enabled: true, filesystem: { denyRead: ['/home/x/.wacli'], allowWrite: ['/tmp'] } } };
  const settings = await evalSettings(lab, { checkout: ROOT, home: '/home/x', hookDir: hookDir });
  for (const rule of GOG_DENY) assert.ok(!settings.permissions.deny.includes(rule), `no blanket ${rule}`);
  assert.ok(settings.permissions.deny.includes('Bash(*wacli*)') && settings.permissions.deny.includes('Bash(*gcloud*)'));
  for (const p of ['/home/x/.local/bin/gog', '/home/x/.config/gogcli', '/home/x/.local/share/gogcli']) assert.ok(settings.sandbox.filesystem.denyRead.includes(p), p);
  assert.equal(settings.sandbox.allowUnsandboxedCommands, false);
  const entries = settings.hooks.PreToolUse.filter(e => e.hooks.some(h => h.command.includes('lab-gmail-read-')));
  assert.equal(entries.length, 1);
  assert.equal(entries[0].matcher, 'Bash|Monitor');
  const hook = entries[0].hooks[0].command;
  assert.ok(hook.includes(join(hookDir, 'lab-gmail-read-')), 'the read-only copy, outside the sandbox\'s writable folders');
  // Applying it again (a second `workspace`) keeps one hook.
  await evalSettings(settings, { checkout: ROOT, home: '/home/x', hookDir: hookDir });
  assert.equal(settings.hooks.PreToolUse.length, 1);

  const env = { ...process.env, ITHOMIINI_LAB_GMAIL_GOG: gog, ITHOMIINI_LAB_GMAIL_TMP: dir, ITHOMIINI_LAB_GMAIL_LOG: join(dir, 'reads.log'), ITHOMIINI_LAB_GMAIL_PASSWORD_FILE: join(dir, 'none') };
  const ask = command => {
    const r = spawnSync('/bin/sh', ['-c', hook], { input: JSON.stringify({ tool_name: 'Bash', tool_input: { command, description: 'x' }, cwd: dir }), env, encoding: 'utf8' });
    assert.equal(r.status, 0, r.stderr);
    return r.stdout ? JSON.parse(r.stdout).hookSpecificOutput : null;
  };
  const read = ask(SKILL_READ);
  assert.equal(read.permissionDecision, 'allow');
  assert.equal(read.updatedInput.description, 'x');
  assert.match(read.updatedInput.command, /^cat '.+\/out\.txt' \| head -40$/);
  const shown = execFileSync('/bin/sh', ['-c', read.updatedInput.command], { encoding: 'utf8' });
  assert.match(shown, /^fake gog: --readonly --gmail-no-send --no-input --enable-commands-exact=\S+ --account=\S+ --client=ithomiini gmail search -- LBI OR depósito/);
  assert.match(readFileSync(join(dir, 'reads.log'), 'utf8'), /gmail search\texit 0/);

  const before = readFileSync(calls, 'utf8');
  const send = ask(`gog --account ${ACCOUNT} --client ithomiini gmail send --to someone@example.org --subject Hola --body prueba`);
  assert.equal(send.permissionDecision, 'deny');
  assert.match(send.permissionDecisionReason, /read-only/);
  assert.equal(readFileSync(calls, 'utf8'), before, 'gog never ran for the send');
  assert.equal(ask('ls -la'), null, 'other commands pass untouched');
  assert.ok(existsSync(gog));
});

test('review round 1 (16 Nov 2026): filters that write files refused; harmless variants of the skill\'s read accepted; Monitor refuses gog', () => {
  for (const cmd of ['gog gmail search x | sort -o /home/x/.bashrc', 'gog gmail search x | uniq - /home/x/f', 'gog gmail search x | cat /home/x/.wacli/db', 'gog gmail search x | tee f'])
    assert.ok(classify(cmd).deny, cmd);
  for (const cmd of [`${SKILL_READ}\n`, SKILL_READ.replace(/\n/g, '\r\n'), SKILL_READ.replace('gog.env;', 'gog.env 2>/dev/null;'), `gog -a ${ACCOUNT.toUpperCase()} gmail search x`, 'gog gmail search x --json --fields id'])
    assert.ok(classify(cmd).read, JSON.stringify(cmd));
  const monitor = decide({ tool_name: 'Monitor', tool_input: { command: 'gog gmail search x' } }, () => assert.fail('no read from Monitor'));
  assert.equal(monitor.hookSpecificOutput.permissionDecision, 'deny');
  assert.equal(decide({ tool_name: 'Monitor', tool_input: { command: 'tail -f log' } }), null);
  // a failed read behind a filter still fails the call
  const failed = decide({ tool_name: 'Bash', tool_input: { command: 'gog gmail search x | head' } }, () => "cat '/tmp/o' | head; exit 1");
  assert.match(failed.hookSpecificOutput.updatedInput.command, /; exit 1$/);
});
