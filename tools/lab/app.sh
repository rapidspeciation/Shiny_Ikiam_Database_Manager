#!/usr/bin/env bash
# The lab's copy of the app, from this checkout: LOCAL_MODE=1 (an in-memory copy of
# the workbook seeded from the snapshot; nothing ever reaches Google Sheets), its own
# database in the lab folder, port 8795, and the lab T3 (tools/lab/t3.sh) in the
# Asistente tab, through the app's T3 proxy on port 3776 (its pages carry the
# bridge that tells Cambios propuestos which chat is open; server/t3bridge.mjs). Every start re-seeds the sheets from the snapshot
# ($LAB/snapshot.json; with LAB_SEED=bench from $LAB/seed.json, the snapshot with the
# benchmark cells emptied, tools/lab/seed.mjs), so a restart undoes any proposal applied.
#   tools/lab/app.sh          run in the foreground
#   tools/lab/app.sh --bg     run detached (log: $LAB/app.log, pid: $LAB/app.pid)
#   tools/lab/app.sh --stop
#   tools/lab/app.sh --refresh  only rewrite the lab workspace from this checkout (after editing assistant/)
# First start: creates the admin `lab` (password in $LAB/credentials.json, mode 600)
# and provisions its T3 workspace (scripts/t3-provision.mjs) when the lab T3 is up.
# Every later start refreshes that workspace, so the threads read this checkout's
# brief, skills and subagents.
set -euo pipefail
HERE="$(cd "$(dirname "$0")/../.." && pwd)"
LAB="${ITHOMIINI_LAB_DIR:-$HOME/.cache/ithomiini-lab}"
T3HOME="${LAB_T3_HOME:-$HOME/.t3-ithomiini-lab}"
PORT="${LAB_APP_PORT:-8795}"
T3PORT="${LAB_T3_PORT:-3775}"
T3PROXY="${LAB_T3_PROXY_PORT:-3776}"
URL="http://127.0.0.1:$PORT"
# The addresses people open (tools/lab/tailscale.sh writes them; default this PC only).
[ -f "$LAB/public.env" ] && . "$LAB/public.env"
PUBLIC_URL="${LAB_PUBLIC_URL:-$URL}"
T3_PUBLIC_URL="${LAB_T3_PUBLIC_URL:-http://127.0.0.1:$T3PROXY}"
umask 077
mkdir -p "$LAB/app"

stop() {
  if [ -f "$LAB/app.pid" ] && kill -0 "$(cat "$LAB/app.pid")" 2>/dev/null; then
    # The pid is the detached shell's; its node child goes first.
    pkill -P "$(cat "$LAB/app.pid")" 2>/dev/null || true
    kill "$(cat "$LAB/app.pid")" 2>/dev/null || true
    echo "Lab app stopped"
    sleep 1
  fi
  rm -f "$LAB/app.pid"
}
# The admin's T3 workspace from this checkout: `lab` the first time (token, T3 project),
# --refresh-all afterwards (brief, skills and subagents rewritten, token kept).
# The same settings reach the app: it makes the T3 project of anyone else who opens the
# Asistente tab (ITHOMIINI_T3_PROVISION=direct, server/t3projects.mjs).
env_provision=(
  T3CODE_HOME="$T3HOME" ITHOMIINI_SHARED="$LAB/app" DATABASE_PATH="$LAB/app/app.sqlite"
  ITHOMIINI_MCP_URL="$URL/api/ai/mcp" ITHOMIINI_T3_WORKSPACES="$T3HOME/workspaces"
  ITHOMIINI_SRC="$HERE" ITHOMIINI_LAB_URL="$PUBLIC_URL" ITHOMIINI_DOCS="$HERE/docs" ITHOMIINI_DENY_READ="$LAB:$HOME/.cache/ithomiini-test"
  ITHOMIINI_KNOWLEDGE="$LAB/app/knowledge"
  T3_BIN="${T3_BIN:-$HOME/.local/bin/t3}"
)
provision() { env "${env_provision[@]}" node "$HERE/scripts/t3-provision.mjs" "$1"; }
[ "${1:-}" = "--stop" ] && { stop; exit 0; }
[ "${1:-}" = "--refresh" ] && { provision --refresh-all; exit 0; }
[ -f "$LAB/snapshot.json" ] || { echo "No snapshot: run tools/lab/snapshot.sh first" >&2; exit 1; }

# The frontend, built when missing or older than its sources.
cd "$HERE"
[ -d frontend/node_modules ] || npm --prefix frontend ci --no-audit --no-fund
if [ ! -f web/index.html ] || [ -n "$(find frontend/src frontend/index.html -newer web/index.html -print -quit)" ]; then
  npm run build
fi

# The data the app starts from: the whole snapshot (for working on the app), or with
# LAB_SEED=bench the snapshot with the benchmark cases' cells emptied (bench.mjs needs it),
# rebuilt when the snapshot or cases changed.
SEED="$LAB/snapshot.json"
if [ "${LAB_SEED:-full}" = bench ]; then
  if [ ! -f "$LAB/seed.json" ] || [ "$LAB/snapshot.json" -nt "$LAB/seed.json" ] || [ "$LAB/cases.json" -nt "$LAB/seed.json" ]; then
    node --max-old-space-size=8192 tools/lab/seed.mjs
  fi
  SEED="$LAB/seed.json"
fi

[ -s "$LAB/setup-token" ] || head -c 24 /dev/urandom | base64 | tr -d '/+=' > "$LAB/setup-token"
env_app=(
  LOCAL_MODE=1
  SEED_FILE="$SEED"
  DATABASE_PATH="$LAB/app/app.sqlite"
  PHOTO_CACHE_DIR="$LAB/app/photos"
  APP_HOST=127.0.0.1
  APP_PORT="$PORT"
  APP_BASE_PATH=/
  APP_PUBLIC_URL="$PUBLIC_URL"
  SECURE_COOKIES=0
  SYNC_INTERVAL_MS=0
  SETUP_TOKEN="$(cat "$LAB/setup-token")"
  ITHOMIINI_T3_URL="$T3_PUBLIC_URL"
  ITHOMIINI_T3_LOCAL="http://127.0.0.1:$T3PORT"
  ITHOMIINI_T3_PROXY_PORT="$T3PROXY"
  ITHOMIINI_T3_ADMIN_TOKEN_FILE="$LAB/t3-admin-token"
  ITHOMIINI_T3_HOME="$T3HOME"
  ITHOMIINI_T3_PROVISION=direct
  "${env_provision[@]}"
  NODE_OPTIONS=--max-old-space-size=8192
)
# No Google credentials, keys or mail settings reach the lab app.
run_app() { env -u GOOGLE_CREDENTIALS_FILE -u WORKBOOK_ID -u SHEET_HOOK_SECRET "${env_app[@]}" node server/index.mjs; }

stop
[ -f "$T3HOME/workspaces/lab/.claude/t3-project" ] && provision --refresh-all
if [ "${1:-}" != "--bg" ]; then
  echo "Lab app on $URL (foreground; the first start takes a minute to load the snapshot)"
  exec env -u GOOGLE_CREDENTIALS_FILE -u WORKBOOK_ID -u SHEET_HOOK_SECRET "${env_app[@]}" node server/index.mjs
fi
nohup bash -c "$(declare -f run_app); $(declare -p env_app); run_app" > "$LAB/app.log" 2>&1 &
echo $! > "$LAB/app.pid"
for _ in $(seq 300); do
  state=$(curl -fsS "$URL/health" 2>/dev/null | node -e 'let s="";process.stdin.on("data",d=>s+=d).on("end",()=>{try{console.log(JSON.parse(s).sync)}catch{}})' || true)
  [ -n "$state" ] && [ "$state" != "syncing" ] && [ "$state" != "not_synced" ] && break
  sleep 2
done
echo "Lab app on $PUBLIC_URL (sync: ${state:-down}; log $LAB/app.log)"

# The admin, created once through the app's own setup flow.
if [ ! -s "$LAB/credentials.json" ]; then
  PASS=$(head -c 12 /dev/urandom | base64 | tr -d '/+=' | head -c 14)
  node -e '
    const [url, token, pass, file] = process.argv.slice(1);
    const body = { token, username: "lab", password: pass, displayName: "Lab" };
    fetch(url + "/api/auth/setup", { method: "POST", headers: { "content-type": "application/json", origin: url }, body: JSON.stringify(body) })
      .then(async r => { if (!r.ok) throw new Error("setup: " + r.status + " " + (await r.text()).slice(0, 200));
        require("fs").writeFileSync(file, JSON.stringify({ url, username: "lab", password: pass }, null, 1), { mode: 0o600 });
        console.log("Admin lab created (password in " + file + ")"); })
      .catch(e => { console.error(e.message); process.exit(1); });
  ' "$URL" "$(cat "$LAB/setup-token")" "$PASS" "$LAB/credentials.json"
fi

# The admin's T3 workspace, pointing at this app's database and MCP endpoint.
if [ ! -f "$T3HOME/workspaces/lab/.claude/t3-project" ]; then
  if curl -fsS -o /dev/null "http://127.0.0.1:$T3PORT/"; then
    provision lab
  else
    echo "Lab T3 is not running: start tools/lab/t3.sh, then run tools/lab/app.sh again to add the workspace"
  fi
fi
