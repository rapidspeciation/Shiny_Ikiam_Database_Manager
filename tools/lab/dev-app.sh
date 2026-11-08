#!/usr/bin/env bash
# A short-lived copy of the lab app for one chat's work (screenshots, UI checks, a quick try), from this checkout:
# LOCAL_MODE on the lab's snapshot, its own folder and port, light settings (one tools worker, one search worker, no photo readings,
# idle workers and identifier stopped soon), no T3. It runs as its own user unit with a time limit (default 90 min)
# and a memory ceiling (default 3 GB: a lab app needs about 2), and is stopped as soon as the chat that started it
# ends (tools/lab/reap.mjs, every 5 minutes). Memory on this PC: tools/lab/README.md, «Memory».
#   tools/lab/dev-app.sh [--port 8821] [--fresh] [--dir <folder>] [--ttl 90m] [--max 3G] [--env K=V …]   start; waits until it answers
# Without --port, the first free port from 8821; --fresh: a new folder (<lab>/dev/<port>-<time>) instead of <lab>/dev/<port>.
# Several can run side by side (DEV_APP_LIMIT, 4 by default), each on its own copy of the workbook's rows
# (server/file-sheets.mjs) and its own database; none reaches the team's workbook. Agents: tools/lab/INSTANCES.md.
# --env (repeatable) adds or overrides one of the app's settings (the benchmark, tools/perf/app-bench, gives it the
# identifier's folder, its database copy's photos and T3's addresses this way).
#   tools/lab/dev-app.sh --stop --port 8821
#   tools/lab/dev-app.sh --list          each one's port, folder and memory
# The admin `lab` is made at the first start of a folder; its password is in <folder>/credentials.json (600).
# Refuses to start with less than 2.5 GB available (MemAvailable) or with DEV_APP_LIMIT of these running already.
set -euo pipefail
HERE="$(cd "$(dirname "$0")/../.." && pwd)"
LAB="${ITHOMIINI_LAB_DIR:-$HOME/.cache/ithomiini-lab}"
PORT="" DIR="" TTL="${DEV_APP_TTL:-90m}" MAX="${DEV_APP_MAX:-3G}" ACTION=start FRESH=0
LIMIT="${DEV_APP_LIMIT:-4}"
EXTRA=()
while [ $# -gt 0 ]; do
  case "$1" in
    --port) PORT="$2"; shift 2 ;;
    --dir) DIR="$2"; shift 2 ;;
    --ttl) TTL="$2"; shift 2 ;;
    --max) MAX="$2"; shift 2 ;;
    --env) [[ "$2" =~ ^[A-Z_][A-Z0-9_]*= ]] || { echo "--env K=V, not $2" >&2; exit 2; }; EXTRA+=(-E "$2"); shift 2 ;;
    --fresh) FRESH=1; shift ;;
    --stop) ACTION=stop; shift ;;
    --list) ACTION=list; shift ;;
    *) echo "Unknown option $1 (see the top of $0)" >&2; exit 2 ;;
  esac
done
if [ "$ACTION" = list ]; then
  for unit in $(systemctl --user list-units 'ithomiini-dev-*' --plain --no-legend --state=active | awk '{print $1}'); do
    mem=$(systemctl --user show -p MemoryCurrent --value "$unit")
    [[ "$mem" =~ ^[0-9]+$ ]] && mem="$((mem / 1048576)) MB" || mem="?"
    echo "$unit  memory $mem  $(systemctl --user show -p Description --value "$unit")"
  done
  exit 0
fi
if [ -z "$PORT" ] && [ "$ACTION" = start ]; then
  # The first port from 8821 that no app listens on and no dev app's unit holds.
  for p in $(seq 8821 8899); do
    systemctl --user is-active --quiet "ithomiini-dev-$p.service" && continue
    [ "$(ss -Hltn "sport = :$p" | wc -l)" -eq 0 ] && { PORT=$p; break; }
  done
fi
[[ "$PORT" =~ ^[0-9]+$ ]] || { echo "--port <number> is needed (or no free port in 8821-8899)" >&2; exit 2; }
UNIT="ithomiini-dev-$PORT"
if [ "$ACTION" = stop ]; then
  systemctl --user stop "$UNIT.service" 2>/dev/null && echo "Stopped $UNIT" || echo "$UNIT was not running"
  exit 0
fi
systemctl --user is-active --quiet "$UNIT.service" && { echo "$UNIT is running already (http://127.0.0.1:$PORT)"; exit 0; }
running=$(systemctl --user list-units 'ithomiini-dev-*' --plain --no-legend --state=active | wc -l)
[ "$running" -lt "$LIMIT" ] || { echo "$running dev apps run already (DEV_APP_LIMIT=$LIMIT; tools/lab/dev-app.sh --list): stop one first." >&2; exit 1; }
avail=$(awk '/MemAvailable/ {print int($2/1024)}' /proc/meminfo)
[ "$avail" -ge 2560 ] || { echo "Only ${avail} MB of memory available (2.5 GB needed): not started." >&2; exit 1; }
[ -f "$LAB/snapshot.json" ] || { echo "No $LAB/snapshot.json: run tools/lab/snapshot.sh first" >&2; exit 1; }
[ "$FRESH" = 1 ] && [ -z "$DIR" ] && DIR="$LAB/dev/$PORT-$(date +%Y%m%dT%H%M%S)"
DIR="${DIR:-$LAB/dev/$PORT}"
mkdir -p "$DIR"
chmod 700 "$DIR"
[ -s "$DIR/setup-token" ] || (umask 077; head -c 24 /dev/urandom | base64 | tr -d '/+=' > "$DIR/setup-token")
# The chat that asked for it: the nearest `claude` (or `codex`) above this shell; the reaper stops the app when it is gone.
owner=0
pid=$$
while [ "$pid" -gt 1 ]; do
  comm="$(cat "/proc/$pid/comm" 2>/dev/null || true)"
  if [ "$comm" = claude ] || [ "$comm" = codex ]; then owner=$pid; break; fi
  pid="$(awk '/^PPid:/ {print $2}' "/proc/$pid/status" 2>/dev/null || echo 1)"
done
[ -f "$HERE/web/index.html" ] || npm --prefix "$HERE" run build
# The workbook's rows file (server/file-sheets.mjs), made once per snapshot and checkout code, one start at a time:
# several starting together would each make it (1.9 GB each for a minute).
systemd-run --user --scope -q -p MemoryMax=3G flock "$LAB/.rows.lock" node "$HERE/server/file-sheets.mjs" ensure "$LAB/snapshot.json" \
  || echo "The rows file could not be made: this app keeps the workbook in memory (about 0.7 GB more)." >&2
systemd-run --user --unit="$UNIT" --collect --quiet \
  --description="Ithomiini dev app :$PORT ($DIR), for pid $owner, $TTL at most" \
  -p WorkingDirectory="$HERE" -p RuntimeMaxSec="$TTL" -p MemoryMax="$MAX" -p MemorySwapMax=1G \
  -p StandardOutput=append:"$DIR/app.log" -p StandardError=append:"$DIR/app.log" \
  -E DEV_APP_OWNER="$owner" \
  -E LOCAL_MODE=1 -E SEED_FILE="$LAB/snapshot.json" -E DATABASE_PATH="$DIR/app.sqlite" -E PHOTO_CACHE_DIR="$DIR/photos" \
  -E DRIVE_LISTING_FILE="$LAB/drive-listing.json" -E DRIVE_ACCOUNT=jmithominii@gmail.com \
  -E APP_HOST=127.0.0.1 -E APP_PORT="$PORT" -E APP_BASE_PATH=/ -E APP_PUBLIC_URL="http://127.0.0.1:$PORT" \
  -E ITHOMIINI_LAB_URL="http://127.0.0.1:$PORT" -E SECURE_COOKIES=0 -E SYNC_INTERVAL_MS=0 \
  -E SETUP_TOKEN="$(cat "$DIR/setup-token")" -E ITHOMIINI_GOG_BIN="$HERE/tools/lab/fake-gog.sh" -E GOG_ACCOUNT=lab \
  -E READINGS=0 -E ASSISTANT_TOOL_WORKERS=1 -E SEARCH_WORKERS=1 -E WORKER_IDLE_MINUTES=5 -E IDENTIFIER_IDLE_MINUTES=2 \
  -E NODE_OPTIONS=--max-old-space-size=2048 -E PATH="$PATH" -E HOME="$HOME" \
  "${EXTRA[@]}" \
  node server/index.mjs
URL="http://127.0.0.1:$PORT"
for _ in $(seq 120); do
  curl -fs -o /dev/null -m 2 "$URL/health" && break
  systemctl --user is-active --quiet "$UNIT.service" || { echo "$UNIT stopped while starting: $DIR/app.log" >&2; tail -n 20 "$DIR/app.log" >&2; exit 1; }
  sleep 2
done
# Ready once its first sync of the snapshot is done (about 20 s after it listens).
for _ in $(seq 120); do
  curl -fs -m 2 "$URL/health" | grep -qE '"sync":"(syncing|not_synced)"' || break
  sleep 2
done
if [ ! -s "$DIR/credentials.json" ]; then
  PASS="$(head -c 12 /dev/urandom | base64 | tr -d '/+=' | head -c 14)"
  node -e '
    const [url, token, pass, file] = process.argv.slice(1);
    fetch(url + "/api/auth/setup", { method: "POST", headers: { "content-type": "application/json", origin: url },
      body: JSON.stringify({ token, username: "lab", password: pass, displayName: "Lab" }) })
      .then(async r => { if (!r.ok) throw new Error("setup: " + r.status + " " + (await r.text()).slice(0, 200));
        require("fs").writeFileSync(file, JSON.stringify({ url, username: "lab", password: pass }, null, 1), { mode: 0o600 }); })
      .catch(e => { console.error(e.message); process.exit(1); });
  ' "$URL" "$(cat "$DIR/setup-token")" "$PASS" "$DIR/credentials.json"
fi
echo "Dev app on $URL (unit $UNIT, stops by itself after $TTL or when pid $owner ends; log $DIR/app.log; login $DIR/credentials.json)"
