#!/usr/bin/env bash
# The lab's copy of the app, from this checkout: LOCAL_MODE=1 (an in-memory copy of
# the workbook seeded from the snapshot; nothing ever reaches Google Sheets), its own
# database in the lab folder (and beside it sheets.sqlite, the sheets' copy the assistant's
# `query` reads: server/replica.mjs), port 8795, and the lab T3 (tools/lab/t3.sh) in the
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
# A Google Sheets copy of the workbook instead of the offline snapshot (to try what reads Google: Photo_links
# from the real photo folders on Drive, read-only): `--workbook <id>` once (kept in $LAB/workbook-id),
# `--offline` to go back. Its login is the app's own ($LAB/google.json, as on the server); the copy has its
# own database ($LAB/app-workbook). The team's workbook is refused, and no account that changes files on
# Drive reaches the lab (organize_photos and Drive batches stay off there).
if [ "${1:-}" = "--workbook" ]; then
  [ -n "${2:-}" ] || { echo "--workbook <spreadsheet id of a copy>" >&2; exit 1; }
  echo "$2" > "$LAB/workbook-id"
  shift 2
  set -- --bg "$@"
elif [ "${1:-}" = "--offline" ]; then
  rm -f "$LAB/workbook-id"
  shift
  set -- --bg "$@"
fi
WORKBOOK="$(cat "$LAB/workbook-id" 2>/dev/null || true)"
APPDIR="$LAB/app"
if [ -n "$WORKBOOK" ]; then
  REAL="$(cd "$HERE" && node -e 'import("./server/workbook.mjs").then(m => console.log(m.REAL_ID))')"
  [ "$WORKBOOK" = "$REAL" ] && { echo "Refused: $WORKBOOK is the team's workbook. Give a copy." >&2; exit 1; }
  [ -s "$LAB/google.json" ] || { echo "No $LAB/google.json (the app's Google login, mode 600): copy it from the server first." >&2; exit 1; }
  APPDIR="$LAB/app-workbook"
fi
mkdir -p "$APPDIR"

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
  T3CODE_HOME="$T3HOME" ITHOMIINI_SHARED="$APPDIR" DATABASE_PATH="$APPDIR/app.sqlite"
  ITHOMIINI_MCP_URL="$URL/api/ai/mcp" ITHOMIINI_T3_WORKSPACES="$T3HOME/workspaces"
  ITHOMIINI_SRC="$HERE" ITHOMIINI_LAB_URL="$PUBLIC_URL" ITHOMIINI_DOCS="$HERE/docs" ITHOMIINI_DENY_READ="$LAB:$HOME/.cache/ithomiini-test"
  ITHOMIINI_KNOWLEDGE="$APPDIR/knowledge"
  T3_BIN="${T3_BIN:-$HOME/.local/bin/t3}"
)
provision() { env "${env_provision[@]}" node "$HERE/scripts/t3-provision.mjs" "$1"; }
[ "${1:-}" = "--stop" ] && { stop; exit 0; }
[ "${1:-}" = "--refresh" ] && { provision --refresh-all; exit 0; }
[ -n "$WORKBOOK" ] || [ -f "$LAB/snapshot.json" ] || { echo "No snapshot: run tools/lab/snapshot.sh first" >&2; exit 1; }

# The frontend, built when missing or older than its sources.
cd "$HERE"
[ -d frontend/node_modules ] || npm --prefix frontend ci --no-audit --no-fund
# The Rename Photos app of the «Renombrar fotos» tab, from the checkout beside this repository when there is one
# (LAB_RENAME_APP_DIR names another; without any, the tab says the app is not installed).
RENAME_SRC="${LAB_RENAME_APP_DIR:-$(cd .. && pwd)/rename_photos_AI}"
if [ -d "$RENAME_SRC/web-app" ]; then RENAME_APP_DIR="$RENAME_SRC" APP_BASE_PATH=/ scripts/build-rename-app.sh || echo "Rename Photos app: not built" >&2; fi
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
if [ -n "$WORKBOOK" ]; then
  env_data=(
    # The copy, through the app's own login (as on the server); the photo folders read on Drive, read-only.
    WORKBOOK_ID="$WORKBOOK"
    GOOGLE_CREDENTIALS_FILE="$LAB/google.json"
    DATABASE_PATH="$APPDIR/app.sqlite"
    PHOTO_CACHE_DIR="$APPDIR/photos"
  )
else
  env_data=(
    LOCAL_MODE=1
    SEED_FILE="$SEED"
    DATABASE_PATH="$APPDIR/app.sqlite"
    PHOTO_CACHE_DIR="$APPDIR/photos"
    # A recorded listing of the photo folders on Drive, for sync_photo_links (server/drive.mjs LocalDrive).
    DRIVE_LISTING_FILE="$LAB/drive-listing.json"
    # The account that changes files in that listing (organize_photos): it owns the files of this owner, as on Drive.
    DRIVE_ACCOUNT="jmithominii@gmail.com"
    # The test photos shown as a folder of camera-named photos uploaded to Drive (drive_photos: server/drive-photos.mjs).
    DRIVE_LOCAL_PHOTOS="${LAB_UPLOAD_PHOTOS:-$LAB/test-batch}"
  )
fi
env_app=(
  "${env_data[@]}"
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
  # Emails (invitations, codes, password resets) are saved in $LAB/mail instead of sent.
  ITHOMIINI_GOG_BIN="$HERE/tools/lab/fake-gog.sh"
  GOG_ACCOUNT=lab
  NODE_OPTIONS=--max-old-space-size=8192
)
# No Google credentials or keys reach the lab app but the copy's login in workbook mode; never an account that
# changes files on Drive. Its mail goes to $LAB/mail.
run_app() { env -u GOOGLE_CREDENTIALS_FILE -u GOOGLE_DRIVE_CREDENTIALS_FILE -u WORKBOOK_ID -u SHEET_HOOK_SECRET "${env_app[@]}" node server/index.mjs; }

stop
# Its T3 (the Asistente tab's chats), started when it is not running (after a reboot, it is not).
if ! curl -fsS -o /dev/null -m 3 "http://127.0.0.1:$T3PORT/"; then
  setsid -f "$HERE/tools/lab/t3.sh" --bg > /dev/null 2>&1
  for _ in $(seq 1 30); do curl -fsS -o /dev/null -m 2 "http://127.0.0.1:$T3PORT/" && break; sleep 2; done
fi
# The workspace follows this checkout (and this mode's database, once the app has made it: below otherwise).
refreshed=""
if [ -f "$T3HOME/workspaces/lab/.claude/t3-project" ] && [ -s "$APPDIR/app.sqlite" ]; then provision --refresh-all; refreshed=1; fi
if [ "${1:-}" != "--bg" ]; then
  echo "Lab app on $URL (foreground; the first start takes a minute to load the snapshot)"
  exec env -u GOOGLE_CREDENTIALS_FILE -u GOOGLE_DRIVE_CREDENTIALS_FILE -u WORKBOOK_ID -u SHEET_HOOK_SECRET "${env_app[@]}" node server/index.mjs
fi
nohup bash -c "$(declare -f run_app); $(declare -p env_app); run_app" > "$LAB/app.log" 2>&1 &
echo $! > "$LAB/app.pid"
for _ in $(seq 300); do
  state=$(curl -fsS "$URL/health" 2>/dev/null | node -e 'let s="";process.stdin.on("data",d=>s+=d).on("end",()=>{try{console.log(JSON.parse(s).sync)}catch{}})' || true)
  [ -n "$state" ] && [ "$state" != "syncing" ] && [ "$state" != "not_synced" ] && break
  sleep 2
done
echo "Lab app on $PUBLIC_URL (sync: ${state:-down}; log $LAB/app.log)"

# The admin, created once through the app's own setup flow (in a database without people: the same password
# as the other mode's, so the lab's login stays one).
if [ -s "$LAB/credentials.json" ] && ! node -e '
  const c = JSON.parse(require("fs").readFileSync(process.argv[1], "utf8"));
  fetch(process.argv[2] + "/api/auth/login", { method: "POST", headers: { "content-type": "application/json", origin: process.argv[2] }, body: JSON.stringify({ username: c.username, password: c.password }) })
    .then(r => process.exit(r.ok ? 0 : 1), () => process.exit(1));
' "$LAB/credentials.json" "$URL"; then
  PASS="$(node -p 'JSON.parse(require("fs").readFileSync(process.argv[1], "utf8")).password' "$LAB/credentials.json")"
  mv "$LAB/credentials.json" "$LAB/credentials.json.kept"
fi
if [ ! -s "$LAB/credentials.json" ]; then
  PASS="${PASS:-$(head -c 12 /dev/urandom | base64 | tr -d '/+=' | head -c 14)}"
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
[ -z "$refreshed" ] && [ -f "$T3HOME/workspaces/lab/.claude/t3-project" ] && provision --refresh-all
if [ ! -f "$T3HOME/workspaces/lab/.claude/t3-project" ]; then
  if curl -fsS -o /dev/null "http://127.0.0.1:$T3PORT/"; then
    provision lab
  else
    echo "Lab T3 is not running: start tools/lab/t3.sh, then run tools/lab/app.sh again to add the workspace"
  fi
fi
