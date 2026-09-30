#!/usr/bin/env bash
# Opens the lab to your other devices over Tailscale (tailnet only, HTTPS): the app and
# its T3 each get a port of this machine's Tailscale name, and the lab app restarts with
# those addresses (the Asistente tab embeds T3 from its own address). Saved in
# $LAB/public.env, so later restarts keep them.
#   tools/lab/tailscale.sh          serve (app on LAB_TS_PORT, default 8509; T3 on LAB_TS_T3_PORT, 8510)
#   tools/lab/tailscale.sh --off    stop serving; the lab goes back to 127.0.0.1 only
set -euo pipefail
HERE="$(cd "$(dirname "$0")/../.." && pwd)"
LAB="${ITHOMIINI_LAB_DIR:-$HOME/.cache/ithomiini-lab}"
PORT="${LAB_APP_PORT:-8795}"
T3PORT="${LAB_T3_PORT:-3775}"
TS_PORT="${LAB_TS_PORT:-8509}"
TS_T3_PORT="${LAB_TS_T3_PORT:-8510}"
# Changing the serve config needs root once it holds folder entries (the other entries stay).
TS=(tailscale)
[ "$(id -u)" != 0 ] && sudo -n true 2>/dev/null && TS=(sudo tailscale)

if [ "${1:-}" = "--off" ]; then
  "${TS[@]}" serve --https="$TS_PORT" off || true
  "${TS[@]}" serve --https="$TS_T3_PORT" off || true
  rm -f "$LAB/public.env"
else
  NAME=$(tailscale status --json | node -e 'let s="";process.stdin.on("data",d=>s+=d).on("end",()=>console.log(JSON.parse(s).Self.DNSName.replace(/\.$/,"")))')
  "${TS[@]}" serve --bg --https="$TS_PORT" "http://127.0.0.1:$PORT" >/dev/null
  "${TS[@]}" serve --bg --https="$TS_T3_PORT" "http://127.0.0.1:$T3PORT" >/dev/null
  umask 077
  printf 'LAB_PUBLIC_URL=%s\nLAB_T3_PUBLIC_URL=%s\n' "https://$NAME:$TS_PORT" "https://$NAME:$TS_T3_PORT" > "$LAB/public.env"
fi
# The app restarts detached, so it outlives this shell.
"$HERE/tools/lab/app.sh" --stop
setsid -f "$HERE/tools/lab/app.sh" --bg > "$LAB/app-start.log" 2>&1
for _ in $(seq 150); do grep -q "Lab app on" "$LAB/app-start.log" 2>/dev/null && break; sleep 2; done
tail -1 "$LAB/app-start.log"
[ -f "$LAB/public.env" ] && echo "T3 on $(sed -n 's/^LAB_T3_PUBLIC_URL=//p' "$LAB/public.env")"
