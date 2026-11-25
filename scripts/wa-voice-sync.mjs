#!/usr/bin/env node
// Checks and refreshes local WhatsApp copies of assistant/voice/core.md (plus whatsapp.md and the
// whatsapp-bridge skill) without touching live WhatsApp messages or calling any AI.
// Invoked after PC deploys by scripts/deploy.sh (--sync --warn-only) or run standalone:
//   node scripts/wa-voice-sync.mjs [--check | --sync] [--warn-only] [--bridge-home <dir>]
//                                  [--workspaces <dir>] [--sync-workspaces] [--json]
import { createHash } from 'node:crypto';
import { copyFileSync, existsSync, mkdirSync, readFileSync, readdirSync, realpathSync, writeFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { dirname, join } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');
const BRIDGE_FILES = [
  'assistant/voice/core.md',
  'assistant/voice/whatsapp.md',
  'tools/whatsapp/bridge-skill/SKILL.md',
];

const sha = text => createHash('sha256').update(text).digest('hex').slice(0, 16);

/** Determines which repo core.md variant a workspace voice.json tracks ('whatsapp-d3l' or 'default'). */
export function voiceVariantOf(manifest) {
  const files = Array.isArray(manifest?.files) ? manifest.files.map(String) : [];
  const from = String(manifest?.from ?? '');
  if (files.includes('humor.md') || /(?:^|\/)whatsapp-d3l\/?$/.test(from)) return 'whatsapp-d3l';
  return 'default';
}

/** Whether a workspace voice.json tracks a repo voice path (safe to auto-sync core.md without clobbering custom live edits). */
function tracksRepoVoice(manifest) {
  if (manifest?.syncCore === true) return true;
  const from = String(manifest?.from ?? '');
  return /(?:^|\/)assistant\/voice(?:\/whatsapp-d3l)?\/?$/.test(from);
}

/**
 * Checks or syncs WhatsApp copies of voice/core.md (and bridge skill):
 * - bridgeHome: the installed bridge dir (~/.local/share/ithomiini-wa-bridge); untouched if absent.
 * - workspacesDir: optional T3 workspaces dir containing WhatsApp workspaces (<ws>/.claude/voice/voice.json).
 * Never sends WhatsApp messages, never restarts services, and never calls any AI.
 */
export function syncWaVoice({
  repoRoot = ROOT,
  bridgeHome = process.env.WA_BRIDGE_HOME || join(homedir(), '.local', 'share', 'ithomiini-wa-bridge'),
  workspacesDir = null,
  sync = false,
  syncWorkspaces = false,
} = {}) {
  const result = { bridgeHome, bridgeInstalled: false, bridge: [], workspaces: [], stale: 0, updated: 0 };

  if (bridgeHome && existsSync(bridgeHome)) {
    result.bridgeInstalled = true;
    for (const rel of BRIDGE_FILES) {
      const src = join(repoRoot, rel);
      if (!existsSync(src)) continue;
      const dst = join(bridgeHome, rel);
      const srcText = readFileSync(src, 'utf8');
      const dstText = existsSync(dst) ? readFileSync(dst, 'utf8') : null;
      if (dstText === srcText) {
        result.bridge.push({ file: rel, status: 'ok', hash: sha(srcText) });
        continue;
      }
      if (sync) {
        mkdirSync(dirname(dst), { recursive: true, mode: 0o700 });
        copyFileSync(src, dst);
        result.updated++;
        result.bridge.push({ file: rel, status: 'updated', fromHash: dstText === null ? null : sha(dstText), toHash: sha(srcText) });
      } else {
        result.stale++;
        result.bridge.push({ file: rel, status: 'stale', fromHash: dstText === null ? null : sha(dstText), toHash: sha(srcText) });
      }
    }
  }

  if (workspacesDir && existsSync(workspacesDir)) {
    const names = readdirSync(workspacesDir, { withFileTypes: true }).filter(d => d.isDirectory()).map(d => d.name).sort();
    for (const name of names) {
      const voiceDir = join(workspacesDir, name, '.claude', 'voice');
      const manifestPath = join(voiceDir, 'voice.json');
      if (!existsSync(manifestPath)) continue;
      let manifest;
      try {
        manifest = JSON.parse(readFileSync(manifestPath, 'utf8'));
      } catch (e) {
        result.stale++;
        result.workspaces.push({ workspace: name, status: 'error', detail: `unreadable voice.json: ${e.message}` });
        continue;
      }
      const files = Array.isArray(manifest.files) ? manifest.files.map(String) : [];
      if (!files.includes('core.md')) continue;
      const variant = voiceVariantOf(manifest);
      const repoRel = variant === 'whatsapp-d3l' ? 'assistant/voice/whatsapp-d3l/core.md' : 'assistant/voice/core.md';
      const srcPath = join(repoRoot, repoRel);
      if (!existsSync(srcPath)) continue;
      const srcText = readFileSync(srcPath, 'utf8');
      const dstPath = join(voiceDir, 'core.md');
      const dstText = existsSync(dstPath) ? readFileSync(dstPath, 'utf8') : null;
      if (dstText === srcText) {
        result.workspaces.push({ workspace: name, variant, source: repoRel, status: 'ok', hash: sha(srcText) });
        continue;
      }
      if (sync && syncWorkspaces && tracksRepoVoice(manifest)) {
        try {
          // Read all sibling voice files BEFORE writing core.md so a missing file never leaves a partial update.
          const allTexts = files.map(f => {
            const base = f.replace(/^.*\//, '');
            return base === 'core.md' ? srcText : readFileSync(join(voiceDir, base), 'utf8');
          });
          writeFileSync(dstPath, srcText);
          const nextManifest = { ...manifest, hash: sha(allTexts.join('\0')), at: new Date().toISOString() };
          writeFileSync(manifestPath, `${JSON.stringify(nextManifest, null, 1)}\n`);
          result.updated++;
          result.workspaces.push({ workspace: name, variant, source: repoRel, status: 'updated', fromHash: dstText === null ? null : sha(dstText), toHash: sha(srcText) });
        } catch (e) {
          result.stale++;
          result.workspaces.push({ workspace: name, variant, source: repoRel, status: 'error', detail: e.message });
        }
      } else {
        result.stale++;
        result.workspaces.push({
          workspace: name,
          variant,
          source: repoRel,
          status: 'stale',
          customSource: !tracksRepoVoice(manifest),
          fromHash: dstText === null ? null : sha(dstText),
          toHash: sha(srcText),
          hint: 'run tools/whatsapp/bridge.mjs voice-push <chat> to refresh the workspace voice and brief',
        });
      }
    }
  }

  return result;
}

function main(argv) {
  const opt = name => {
    const i = argv.indexOf(name);
    return i >= 0 ? argv[i + 1] : undefined;
  };
  const sync = argv.includes('--sync') && !argv.includes('--check');
  const warnOnly = argv.includes('--warn-only');
  const asJson = argv.includes('--json');
  const res = syncWaVoice({
    repoRoot: opt('--repo') ?? ROOT,
    bridgeHome: opt('--bridge-home') ?? (process.env.WA_BRIDGE_HOME || join(homedir(), '.local', 'share', 'ithomiini-wa-bridge')),
    workspacesDir: opt('--workspaces') ?? null,
    sync,
    syncWorkspaces: argv.includes('--sync-workspaces'),
  });
  if (asJson) {
    console.log(JSON.stringify(res, null, 2));
  } else if (!res.bridgeInstalled && !res.workspaces.length) {
    console.log('WhatsApp voice sync: no local bridge install or workspaces checked.');
  } else {
    for (const b of res.bridge) {
      if (b.status === 'updated') console.log(`WhatsApp bridge updated: ${b.file} (${b.fromHash ?? 'missing'} → ${b.toHash})`);
      else if (b.status === 'stale') console.warn(`WhatsApp bridge stale: ${b.file} (${b.fromHash ?? 'missing'} ≠ ${b.toHash})`);
    }
    for (const w of res.workspaces) {
      if (w.status === 'updated') console.log(`WhatsApp workspace updated: ${w.workspace} (${w.source})`);
      else if (w.status === 'stale') console.warn(`WhatsApp workspace stale: ${w.workspace} core.md differs from ${w.source} (${w.hint})`);
      else if (w.status === 'error') console.warn(`WhatsApp workspace error: ${w.workspace} (${w.detail})`);
    }
    if (!res.updated && !res.stale) console.log('WhatsApp voice sync: up to date.');
  }
  return res.stale > 0 && !warnOnly ? 1 : 0;
}

if (process.argv[1] && import.meta.url === pathToFileURL(realpathSync(process.argv[1])).href) {
  process.exitCode = main(process.argv.slice(2));
}
