// Runs one assistant turn with the Claude Code CLI (`claude -p`), logged in on the
// server with a Claude subscription. The app's own tools reach Claude through the
// MCP endpoint /api/ai/mcp, authorized by a token that lives only for this turn.
// Claude may also read the project docs; it cannot run commands or edit files.

import { spawn } from 'node:child_process';
import { copyFile, mkdir } from 'node:fs/promises';
import { join } from 'node:path';

/**
 * Only Sonnet or Opus (the current ones: `sonnet` = claude-sonnet-5-5, `opus` =
 * claude-opus-5-5 on claudeclaw). Fable is far too expensive for this app and
 * older models are not wanted, so any other name falls back to Sonnet.
 */
export function allowedModel(name, fallback = 'sonnet') {
  const model = String(name || '').trim().toLowerCase();
  return /^(sonnet|opus)$|^claude-(sonnet|opus)-5-5(\[1m\])?$/.test(model) ? model : fallback;
}

export function claudeConfig(env = process.env) {
  return {
    bin: env.ITHOMIINI_CLAUDE_BIN || '',
    model: allowedModel(env.ITHOMIINI_CLAUDE_MODEL),
    // A subscription is personal: only these usernames get Claude, others use the API provider.
    users: new Set(
      String(env.ITHOMIINI_CLAUDE_USERS || '')
        .split(',')
        .map(s => s.trim())
        .filter(Boolean),
    ),
    configDir: env.ITHOMIINI_CLAUDE_CONFIG_DIR || '',
    // Sessions are stored per working directory, so it must not change between releases.
    workspace: env.ITHOMIINI_CLAUDE_WORKSPACE || '',
    timeoutMs: Number(env.ITHOMIINI_CLAUDE_TIMEOUT_MS || 300000),
  };
}

export const claudeAllowed = (claude, user) =>
  Boolean(claude.bin && claude.workspace && claude.users.has(user?.username));

/** Copies the release's CLAUDE.md into the stable workspace. */
export async function prepareWorkspace(claude, releaseRoot) {
  await mkdir(claude.workspace, { recursive: true });
  await copyFile(join(releaseRoot, 'assistant', 'CLAUDE.md'), join(claude.workspace, 'CLAUDE.md'));
}

/**
 * content: Anthropic message content blocks (text and base64 images).
 * Returns { text, sessionId, costUsd } or throws with the CLI's error.
 */
export function runClaude(claude, { content, system, mcpUrl, token, sessionId, resume, docsDir }) {
  const args = [
    '-p',
    '--input-format',
    'stream-json',
    '--output-format',
    'stream-json',
    '--verbose',
    '--model',
    claude.model,
    '--tools',
    'Read,Grep,Glob',
    '--allowedTools',
    'mcp__ithomiini,Read,Grep,Glob',
    '--permission-mode',
    'dontAsk',
    '--strict-mcp-config',
    '--mcp-config',
    JSON.stringify({
      mcpServers: {
        ithomiini: { type: 'http', url: mcpUrl, headers: { Authorization: `Bearer ${token}` } },
      },
    }),
    '--append-system-prompt',
    system,
    ...(docsDir ? ['--add-dir', docsDir] : []),
    ...(resume ? ['--resume', resume] : ['--session-id', sessionId]),
  ];
  const env = {
    HOME: process.env.HOME || '/home/ubuntu',
    PATH: '/usr/local/bin:/usr/bin:/bin',
    LANG: 'C.UTF-8',
    ...(claude.configDir ? { CLAUDE_CONFIG_DIR: claude.configDir } : {}),
    // Keep large MCP answers (a notebook page of rows) whole.
    MAX_MCP_OUTPUT_TOKENS: '60000',
    // The service cannot write the install folder; updates are done by hand.
    DISABLE_AUTOUPDATER: '1',
  };
  return new Promise((resolve, reject) => {
    const child = spawn(claude.bin, args, { cwd: claude.workspace, env, stdio: ['pipe', 'pipe', 'pipe'] });
    let buffer = '',
      stderr = '',
      result = null,
      session = resume || sessionId;
    const timer = setTimeout(() => child.kill('SIGTERM'), claude.timeoutMs);
    child.stdout.on('data', chunk => {
      buffer += chunk;
      let line;
      while ((line = buffer.indexOf('\n')) >= 0) {
        const text = buffer.slice(0, line).trim();
        buffer = buffer.slice(line + 1);
        if (!text) continue;
        let event;
        try {
          event = JSON.parse(text);
        } catch {
          continue;
        }
        if (event.session_id) session = event.session_id;
        if (event.type === 'result') result = event;
      }
    });
    child.stderr.on('data', chunk => (stderr = (stderr + chunk).slice(-4000)));
    child.on('error', e => {
      clearTimeout(timer);
      reject(e);
    });
    child.on('close', code => {
      clearTimeout(timer);
      if (result && !result.is_error && result.subtype === 'success')
        return resolve({
          text: String(result.result ?? ''),
          sessionId: session,
          costUsd: result.total_cost_usd ?? null,
        });
      const reason = result?.result || result?.subtype || stderr.trim().split('\n').at(-1) || `exit ${code}`;
      reject(
        Object.assign(new Error(`Claude: ${String(reason).slice(0, 300)}`), {
          missingSession: /no conversation found/i.test(reason),
        }),
      );
    });
    child.stdin.end(JSON.stringify({ type: 'user', message: { role: 'user', content } }) + '\n');
  });
}
