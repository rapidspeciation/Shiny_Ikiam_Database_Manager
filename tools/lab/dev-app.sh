#!/usr/bin/env bash
# A short-lived copy of the lab app for one chat's work (screenshots, UI checks, a quick try), from this checkout:
# LOCAL_MODE on the lab's snapshot, its own folder and port, light settings (one tools worker, no photo readings,
# idle workers and identifier stopped soon), no T3. It runs as its own user unit with a time limit (default 90 min)
# and a memory ceiling (default 3 GB: a lab app needs about 2), and is stopped as soon as the chat that started it
# ends (tools/lab/reap.mjs, every 5 minutes). Memory on this PC: tools/lab/README.md, «Memory».
#   tools/lab/dev-app.sh --port 8821 [--dir <folder>] [--ttl 90m] [--max 3G] [--env K=V …]   start; waits until it answers
# --env (repeatable) adds or overrides one of the app's settings (the benchmark, tools/perf/app-bench, gives it the
# identifier's folder, its database copy's photos and T3's addresses this way).
#   tools/lab/dev-app.sh --stop --port 8821
#   tools/lab/dev-app.sh --list
# The admin `lab` is made at the first start of a folder; its password is in <folder>/credentials.json (600).
# Refuses to start with less than 2.5 GB available (MemAvailable) or with two of these running already.
set -euo pipefail
HERE="$(cd "$(dirname "$0")/../.." && pwd)"
LAB="${ITHOMIINI_LAB_DIR:-$HOME/.cache/ithomiini-lab}"
PORT="" DIR="" TTL="${DEV_APP_TTL:-90m}" MAX="${DEV_APP_MAX:-3G}" ACTION=start
EXTRA=()
while [ $# -gt 0 ]; do
  case "$1" in
    --port) PORT="$2"; shift 2 ;;
    --dir) DIR="$2"; shift 2 ;;
    --ttl) TTL="$2"; shift 2 ;;
    --max) MAX="$2"; shift 2 ;;
    --env) [[ "$2" =~ ^[A-Z_][A-Z0-9_]*= ]] || { echo "--env K=V, not $2" >&2; exit 2; }; EXTRA+=(-E "$2"); shift 2 ;;
    --stop) ACTION=stop; shift ;;
    --list) ACTION=list; shift ;;
    *) echo "Unknown option $1 (see the top of $0)" >&2; exit 2 ;;
  esac
done
if [ "$ACTION" = list ]; then
  systemctl --user list-units 'ithomiini-dev-*' --plain --no-legend --all || true
  exit 0
fi
[[ "$PORT" =~ ^[0-9]+$ ]] || { echo "--port <number> is needed" >&2; exit 2; }
UNIT="ithomiini-dev-$PORT"
if [ "$ACTION" = stop ]; then
  systemctl --user stop "$UNIT.service" 2>/dev/null && echo "Stopped $UNIT" || echo "$UNIT was not running"
  exit 0
fi
systemctl --user is-active --quiet "$UNIT.service" && { echo "$UNIT is running already (http://127.0.0.1:$PORT)"; exit 0; }
running=$(systemctl --user list-units 'ithomiini-dev-*' --plain --no-legend --state=active | wc -l)
[ "$running" -lt 2 ] || { echo "Two dev apps run already (tools/lab/dev-app.sh --list): stop one first." >&2; exit 1; }
avail=$(awk '/MemAvailable/ {print int($2/1024)}' /proc/meminfo)
[ "$avail" -ge 2560 ] || { echo "Only ${avail} MB of memory available (2.5 GB needed): not started." >&2; exit 1; }
[ -f "$LAB/snapshot.json" ] || { echo "No $LAB/snapshot.json: run tools/lab/snapshot.sh first" >&2; exit 1; }
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
  -E READINGS=0 -E ASSISTANT_TOOL_WORKERS=1 -E WORKER_IDLE_MINUTES=5 -E IDENTIFIER_IDLE_MINUTES=2 \
  -E NODE_OPTIONS=--max-old-space-size=2048 -E PATH="$PATH" -E HOME="$HOME" \
  "${EXTRA[@]}" \
  node server/index.mjs
URL="http://127.0.0.1:$PORT"
for _ in $(seq 120); do
  curl -fs -o /dev/null -m 2 "$URL/health" && break
  systemctl --user is-active --quiet "$UNIT.service" || { echo "$UNIT stopped while starting: $DIR/app.log" >&2; tail -n 20 "$DIR/app.log" >&2; exit 1; }
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
