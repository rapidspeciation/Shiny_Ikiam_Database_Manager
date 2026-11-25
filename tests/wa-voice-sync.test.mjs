import test from 'node:test';
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { syncWaVoice, voiceVariantOf } from '../scripts/wa-voice-sync.mjs';
import { deployFixture } from './helpers/deploy-fixture.mjs';

const SCRIPT = new URL('../scripts/wa-voice-sync.mjs', import.meta.url).pathname;

test('wa-voice-sync: detects and refreshes stale bridge core.md, whatsapp.md and whatsapp-bridge skill; respects D3l vs default and never clobbers custom workspace sources', () => {
  const dir = mkdtempSync(join(tmpdir(), 'wa-voice-sync-'));
  try {
    const repo = join(dir, 'repo');
    mkdirSync(join(repo, 'assistant', 'voice', 'whatsapp-d3l'), { recursive: true });
    mkdirSync(join(repo, 'tools', 'whatsapp', 'bridge-skill'), { recursive: true });
    writeFileSync(join(repo, 'assistant', 'voice', 'core.md'), 'NEW APP CORE\n');
    writeFileSync(join(repo, 'assistant', 'voice', 'whatsapp.md'), 'NEW WA HEADER\n');
    writeFileSync(join(repo, 'assistant', 'voice', 'whatsapp-d3l', 'core.md'), 'NEW D3L CORE\n');
    writeFileSync(join(repo, 'tools', 'whatsapp', 'bridge-skill', 'SKILL.md'), 'NEW SKILL\n');

    const bridgeHome = join(dir, 'wa-bridge');
    mkdirSync(join(bridgeHome, 'assistant', 'voice'), { recursive: true });
    writeFileSync(join(bridgeHome, 'assistant', 'voice', 'core.md'), 'OLD CORE\n');

    const workspaces = join(dir, 'workspaces');
    // 1. Plain workspace tracking repo assistant/voice
    const wsPlain = join(workspaces, 'ws-plain', '.claude', 'voice');
    mkdirSync(wsPlain, { recursive: true });
    writeFileSync(join(wsPlain, 'whatsapp.md'), 'PLAIN WA\n');
    writeFileSync(join(wsPlain, 'core.md'), 'OLD PLAIN CORE\n');
    writeFileSync(join(wsPlain, 'voice.json'), JSON.stringify({ files: ['whatsapp.md', 'core.md'], from: '/repo/assistant/voice', hash: 'old' }));

    // 2. D3l workspace tracking repo assistant/voice/whatsapp-d3l
    const wsD3l = join(workspaces, 'ws-d3l', '.claude', 'voice');
    mkdirSync(wsD3l, { recursive: true });
    writeFileSync(join(wsD3l, 'whatsapp.md'), 'D3L WA\n');
    writeFileSync(join(wsD3l, 'core.md'), 'OLD D3L CORE\n');
    writeFileSync(join(wsD3l, 'humor.md'), 'D3L HUMOR\n');
    writeFileSync(join(wsD3l, 'voice.json'), JSON.stringify({ files: ['whatsapp.md', 'core.md', 'humor.md'], from: '/repo/assistant/voice/whatsapp-d3l', hash: 'old' }));

    // 3. Custom external workspace (e.g. live copy outside repo): warn only, never overwrite
    const wsCustom = join(workspaces, 'ws-custom', '.claude', 'voice');
    mkdirSync(wsCustom, { recursive: true });
    writeFileSync(join(wsCustom, 'whatsapp.md'), 'LIVE WA\n');
    writeFileSync(join(wsCustom, 'core.md'), 'CUSTOM LIVE CORE\n');
    writeFileSync(join(wsCustom, 'humor.md'), 'LIVE HUMOR\n');
    writeFileSync(join(wsCustom, 'voice.json'), JSON.stringify({ files: ['whatsapp.md', 'core.md', 'humor.md'], from: '/home/franz/.config/ithomiini-wa-bridge/voice-d3l', hash: 'live' }));

    // Variant classification
    assert.equal(voiceVariantOf({ files: ['whatsapp.md', 'core.md'], from: '/repo/assistant/voice' }), 'default');
    assert.equal(voiceVariantOf({ files: ['whatsapp.md', 'core.md', 'humor.md'], from: '/custom' }), 'whatsapp-d3l');
    assert.equal(voiceVariantOf({ files: ['core.md'], from: '/repo/assistant/voice/whatsapp-d3l' }), 'whatsapp-d3l');

    // Check mode: reports stale items without writing anything.
    const check = syncWaVoice({ repoRoot: repo, bridgeHome, workspacesDir: workspaces, sync: false });
    assert.equal(check.bridgeInstalled, true);
    assert.equal(check.updated, 0);
    assert.equal(check.stale, 6);
    assert.equal(readFileSync(join(bridgeHome, 'assistant', 'voice', 'core.md'), 'utf8'), 'OLD CORE\n');

    // Sync mode: refreshes bridge files + repo-tracked workspaces (with matching variant), leaves custom workspace untouched.
    const synced = syncWaVoice({ repoRoot: repo, bridgeHome, workspacesDir: workspaces, sync: true, syncWorkspaces: true });
    assert.equal(synced.updated, 5);
    assert.equal(synced.stale, 1);
    assert.equal(readFileSync(join(bridgeHome, 'assistant', 'voice', 'core.md'), 'utf8'), 'NEW APP CORE\n');
    assert.equal(readFileSync(join(bridgeHome, 'assistant', 'voice', 'whatsapp.md'), 'utf8'), 'NEW WA HEADER\n');
    assert.equal(readFileSync(join(bridgeHome, 'tools', 'whatsapp', 'bridge-skill', 'SKILL.md'), 'utf8'), 'NEW SKILL\n');
    assert.equal(readFileSync(join(wsPlain, 'core.md'), 'utf8'), 'NEW APP CORE\n');
    assert.equal(readFileSync(join(wsPlain, 'whatsapp.md'), 'utf8'), 'PLAIN WA\n');
    assert.equal(readFileSync(join(wsD3l, 'core.md'), 'utf8'), 'NEW D3L CORE\n');
    assert.equal(readFileSync(join(wsD3l, 'humor.md'), 'utf8'), 'D3L HUMOR\n');
    assert.notEqual(JSON.parse(readFileSync(join(wsD3l, 'voice.json'), 'utf8')).hash, 'old');
    // Custom workspace is NOT overwritten, only reported stale with customSource: true.
    assert.equal(readFileSync(join(wsCustom, 'core.md'), 'utf8'), 'CUSTOM LIVE CORE\n');
    const customEntry = synced.workspaces.find(w => w.workspace === 'ws-custom');
    assert.equal(customEntry.status, 'stale');
    assert.equal(customEntry.customSource, true);

    // Absent bridge home: clean no-op, never creates it.
    const missingBridge = join(dir, 'never-installed');
    const noop = syncWaVoice({ repoRoot: repo, bridgeHome: missingBridge, sync: true });
    assert.equal(noop.bridgeInstalled, false);
    assert.equal(existsSync(missingBridge), false);

    // CLI exits 1 on stale without --warn-only, 0 with --warn-only.
    let code = 0;
    try {
      execFileSync(process.execPath, [SCRIPT, '--repo', repo, '--bridge-home', bridgeHome, '--workspaces', workspaces, '--check'], { stdio: 'pipe' });
    } catch (e) {
      code = e.status;
    }
    assert.equal(code, 1);
    const warned = execFileSync(process.execPath, [SCRIPT, '--repo', repo, '--bridge-home', bridgeHome, '--workspaces', workspaces, '--check', '--warn-only'], { encoding: 'utf8' });
    assert.equal(typeof warned, 'string');
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
});

test('deploy.sh + install-release.sh refresh local WA bridge core.md and repo-tracked workspace core.md without touching live WA or AI', () => {
  const f = deployFixture();
  try {
    // Local WA bridge installed on this PC with stale core.md.
    const bridgeHome = join(f.home, '.local', 'share', 'ithomiini-wa-bridge');
    mkdirSync(join(bridgeHome, 'assistant', 'voice'), { recursive: true });
    writeFileSync(join(bridgeHome, 'assistant', 'voice', 'core.md'), 'stale-pc-core\n');

    // Repo-tracked WhatsApp workspace on the "server" with stale core.md.
    const wsVoice = join(f.app, 'shared', 't3-workspaces', 'wa-group', '.claude', 'voice');
    mkdirSync(wsVoice, { recursive: true });
    writeFileSync(join(wsVoice, 'whatsapp.md'), 'wa-header\n');
    writeFileSync(join(wsVoice, 'core.md'), 'stale-ws-core\n');
    writeFileSync(join(wsVoice, 'voice.json'), JSON.stringify({ files: ['whatsapp.md', 'core.md'], from: '/repo/assistant/voice', hash: 'old' }));

    const run = f.run([], { DEPLOY_TEST_SSH: 'local', WA_BRIDGE_HOME: bridgeHome });
    assert.equal(run.status, 0, run.stdout + run.stderr);
    assert.equal(readFileSync(join(bridgeHome, 'assistant', 'voice', 'core.md'), 'utf8'), 'assistant/voice/core.md\n');
    assert.equal(readFileSync(join(bridgeHome, 'tools', 'whatsapp', 'bridge-skill', 'SKILL.md'), 'utf8'), 'tools/whatsapp/bridge-skill/SKILL.md\n');
    assert.equal(readFileSync(join(wsVoice, 'core.md'), 'utf8'), 'assistant/voice/core.md\n');
  } finally {
    f.close();
  }
});
