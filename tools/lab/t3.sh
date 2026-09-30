#!/usr/bin/env bash
# The lab's own T3 Code: a separate home (T3CODE_HOME, default ~/.t3-ithomiini-lab) and
# port (3775), so the other T3 installs and their sessions are never touched.
# Providers: Claude (the local `claude` CLI and its login) and Codex (the local `codex`).
#   tools/lab/t3.sh          run in the foreground
#   tools/lab/t3.sh --bg     run detached (log: $LAB/t3.log, pid: $LAB/t3.pid)
#   tools/lab/t3.sh --stop   stop the detached one
# Also writes $LAB/t3-admin-token (600): the bearer token the lab app uses to mint
# T3 sign-in links (Asistente tab) and the benchmark uses to sign in.
set -euo pipefail
LAB="${ITHOMIINI_LAB_DIR:-$HOME/.cache/ithomiini-lab}"
export T3CODE_HOME="${LAB_T3_HOME:-$HOME/.t3-ithomiini-lab}"
PORT="${LAB_T3_PORT:-3775}"
T3="${T3_BIN:-$HOME/.local/bin/t3}"
CLAUDE_BIN="${LAB_CLAUDE_BIN:-$(command -v claude)}"
CODEX_BIN="${LAB_CODEX_BIN:-$(command -v codex)}"
umask 077
mkdir -p "$LAB" "$T3CODE_HOME/userdata" "$T3CODE_HOME/workspaces"

stop() {
  if [ -f "$LAB/t3.pid" ] && kill -0 "$(cat "$LAB/t3.pid")" 2>/dev/null; then
    kill "$(cat "$LAB/t3.pid")" && echo "Lab T3 stopped"
  fi
  rm -f "$LAB/t3.pid"
}
[ "${1:-}" = "--stop" ] && { stop; exit 0; }

# Provider settings: only Claude and Codex; the Claude models are chosen by the
# benchmark (Opus 5.5 / Sonnet 5.5; Sonnet 5.5 is a custom slug in this T3 version).
node - "$T3CODE_HOME/userdata/settings.json" "$CLAUDE_BIN" "$CODEX_BIN" <<'EOF'
const fs = require('fs');
const [file, claude, codex] = process.argv.slice(2);
let s = {};
try { s = JSON.parse(fs.readFileSync(file, 'utf8')); } catch {}
s.providers = { ...s.providers, cursor: { enabled: false }, grok: { enabled: false }, opencode: { enabled: false } };
s.providerInstances = {
  ...s.providerInstances,
  claudeAgent: {
    driver: 'claudeAgent', enabled: true,
    config: { binaryPath: claude, homePath: '', launchArgs: '', autoCompactWindow: '',
      // A custom model gets no effort menu unless it declares one (as the catalog does for Opus 5.5).
      customModels: [{ slug: 'claude-sonnet-5-5', name: 'Claude Sonnet 5.5', capabilities: { optionDescriptors: [{
        id: 'effort', label: 'Reasoning', type: 'select',
        options: [{ id: 'low', label: 'Low' }, { id: 'medium', label: 'Medium', isDefault: true }, { id: 'high', label: 'High' },
          { id: 'xhigh', label: 'Extra High' }, { id: 'max', label: 'Max' }],
      }] } }] },
  },
  codex: {
    driver: 'codex', displayName: 'Codex', enabled: true,
    config: { binaryPath: codex, homePath: process.env.HOME + '/.codex', shadowHomePath: '', launchArgs: '', customModels: [] },
  },
  antigravity: { driver: 'antigravity', enabled: false, config: {} },
};
s.enableProviderUpdateChecks = false;
// New chats open like the live T3 (Opus 5.5 · Medium · 1M); a choice made later in T3 is kept.
s.defaultModelSelection ??= { instanceId: 'claudeAgent', model: 'claude-opus-5-5',
  options: [{ id: 'contextWindow', value: '1m' }, { id: 'effort', value: 'medium' }] };
fs.writeFileSync(file, JSON.stringify(s, null, 1), { mode: 0o600 });
EOF

# The admin token (for pairing links), issued once and kept.
token() {
  if [ ! -s "$LAB/t3-admin-token" ]; then
    "$T3" auth session issue --base-dir "$T3CODE_HOME" --label ithomiini-lab --ttl 365d --token-only > "$LAB/t3-admin-token.new"
    mv "$LAB/t3-admin-token.new" "$LAB/t3-admin-token"
    chmod 600 "$LAB/t3-admin-token"
  fi
}

cd "$T3CODE_HOME/workspaces"
if [ "${1:-}" = "--bg" ]; then
  stop
  nohup "$T3" serve --base-dir "$T3CODE_HOME" --host 127.0.0.1 --port "$PORT" --no-browser > "$LAB/t3.log" 2>&1 &
  echo $! > "$LAB/t3.pid"
  for _ in $(seq 60); do curl -fsS -o /dev/null "http://127.0.0.1:$PORT/" 2>/dev/null && break; sleep 1; done
  token
  echo "Lab T3 on http://127.0.0.1:$PORT (home $T3CODE_HOME, log $LAB/t3.log)"
else
  ( for _ in $(seq 60); do curl -fsS -o /dev/null "http://127.0.0.1:$PORT/" 2>/dev/null && break; sleep 1; done; token ) &
  exec "$T3" serve --base-dir "$T3CODE_HOME" --host 127.0.0.1 --port "$PORT" --no-browser
fi
