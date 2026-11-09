#!/usr/bin/env bash
# Goes back to the release before (shared/previous-release) on claudeclaw: waits for saves in progress, points
# current at it, installs its units, restarts, checks /health and refreshes the T3 workspaces from it.
#   bash /home/ubuntu/ithomiini/current/scripts/rollback-release.sh [release name, default the previous one]
# The same scratch variables as scripts/install-release.sh.
set -euo pipefail
app="${ITHOMIINI_APP_ROOT:-/home/ubuntu/ithomiini}"
units="${ITHOMIINI_SYSTEMD_DIR:-$HOME/.config/systemd/user}"
service="${ITHOMIINI_SERVICE:-ithomiini.service}"
service_env="${ITHOMIINI_SERVICE_ENV:-$HOME/.config/ithomiini/service.env}"
if [ "$app" != /home/ubuntu/ithomiini ] && { [ -z "${ITHOMIINI_SYSTEMD_DIR:-}" ] || [ "$service" = ithomiini.service ]; }; then
  echo "A scratch root ($app) needs ITHOMIINI_SYSTEMD_DIR and ITHOMIINI_SERVICE of its own." >&2
  exit 1
fi
[ -n "${DEPLOY_SCRATCH_BIN:-}" ] && export PATH="$DEPLOY_SCRATCH_BIN:$PATH"
[ -n "${DEPLOY_SCRATCH_HOME:-}" ] && export HOME="$DEPLOY_SCRATCH_HOME"
cd "$app"
target="releases/${1:-$(basename "$(cat shared/previous-release)")}"
[ -d "$target" ] || { echo "No release $target" >&2; exit 1; }
from="$(readlink current)"
[ "$from" != "$target" ] || { echo "current is already $target" >&2; exit 1; }
exec 9>"$app/shared/.install.lock"
flock -w 900 9 || { echo "An install still runs after 15 minutes." >&2; exit 1; }
node="$(sed -n 's/^ExecStart=\([^ ]*\/node\) .*/\1/p' "$target/deploy/ithomiini.service")"
export PATH="$(dirname "$node"):$PATH" ITHOMIINI_SHARED="$app/shared"
bash "$target/scripts/wait-writes.sh" "${DEPLOY_WAIT:-600}" "${DEPLOY_FORCE:-0}"
echo "$from" >shared/previous-release
ln -sfn "$target" current
cp current/deploy/ithomiini*.service current/deploy/ithomiini-*.timer "$units/"
systemctl --user daemon-reload
systemctl --user restart "$service"
port="${APP_PORT:-$(sed -n 's/^APP_PORT=//p' "$service_env" 2>/dev/null | tail -n 1 || true)}"
deadline=$(( $(date +%s) + ${DEPLOY_HEALTH_WAIT:-90} ))
until curl -s --max-time 5 "http://127.0.0.1:${port:-8794}/health" | grep -q '"status":"ok"'; do
  [ "$(date +%s)" -lt "$deadline" ] || { echo "Health not ok after rolling back to $target." >&2; exit 1; }
  sleep 0.5
done
# The workspaces as that release makes them; the next deploy refreshes them again (its hash is forgotten).
rm -f -- shared/.deploy-stamps/t3-provision
(cd current && "$node" scripts/t3-provision.mjs --refresh-all)
echo "Rolled back: current is $target (was $from); health ok."
