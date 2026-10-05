#!/usr/bin/env bash
# Waits (up to $1 seconds) until the running app has no proposal being applied, no save being
# written and no save whose outcome is unknown, as its /health says (`writing`). Exits 1 when
# the time runs out, unless $2 is 1 (DEPLOY_FORCE). Run by scripts/deploy.sh before the restart.
set -u
wait_s="${1:-600}"
force="${2:-0}"
port="${APP_PORT:-8794}"
# The node of the service (over ssh, node may not be on the PATH).
here="$(cd "$(dirname "$0")/.." && pwd)"
node_bin="$(command -v node || sed -n 's/^ExecStart=\([^ ]*\/node\) .*/\1/p' "$here/deploy/ithomiini.service")"
deadline=$(( $(date +%s) + wait_s ))
said=""
while :; do
  health="$(curl -s --max-time 10 "http://127.0.0.1:$port/health" || true)"
  state="$(printf '%s' "$health" | "$node_bin" -e '
    let s = ""; process.stdin.on("data", d => (s += d)).on("end", () => {
      try {
        const w = JSON.parse(s).writing;
        if (!w) return console.log("unknown");
        console.log(w.applying || w.inFlight || w.unconfirmed ? `busy applying=${w.applying} writing=${w.inFlight} unconfirmed=${w.unconfirmed}` : "idle");
      } catch { console.log("down"); }
    });')"
  case "$state" in
    idle) echo "No save in progress: restarting."; exit 0 ;;
    down) echo "The app is not answering: restarting."; exit 0 ;;
    unknown) echo "The running release does not report its saves: restarting (it may cut a save in progress)."; exit 0 ;;
  esac
  if [ "$state" != "$said" ]; then echo "Waiting before the restart: $state"; said="$state"; fi
  if [ "$(date +%s)" -ge "$deadline" ]; then
    if [ "$force" = 1 ]; then echo "Still $state after ${wait_s} s: restarting anyway (DEPLOY_FORCE=1)."; exit 0; fi
    echo "Still $state after ${wait_s} s: not restarting. Settle the save (Historial → recover) or deploy with DEPLOY_FORCE=1." >&2
    exit 1
  fi
  sleep 3
done
