#!/usr/bin/env bash
# Installs an uploaded release on claudeclaw (scripts/deploy.sh runs it over one ssh, or locally there):
#   bash <app>/releases/<release>/scripts/install-release.sh <release> <commit>
# 1. the release's test record (.deploy-record.json, scripts/deploy-record.mjs): a pass of every job (the slow suite:
#    a pass, or its inputs unchanged since its last green run) for exactly
#    <commit>, and the built frontend and every file as tested; otherwise nothing changes;
# 2. side by side, each a no-op when its input did not change: the Rename Photos app (keyed by rename-app.commit),
#    the Wikiloc worker (install-worker.sh's own content hash), the offline identifier parts (--if-needed), old
#    releases and staged runs pruned, and the hash of what the T3 workspaces refresh reads (scripts/deploy-stamp.mjs);
# 3. wait-writes (never restart in the middle of a save), then the T3 workspaces refresh when its hash changed or
#    is not recorded;
# 4. last: current → the release (shared/previous-release names the one before), the units, the restart, and the
#    app's /health until "status":"ok".
# The live install by default. A scratch install (tests) sets ITHOMIINI_APP_ROOT, ITHOMIINI_SYSTEMD_DIR,
# ITHOMIINI_SERVICE (and ITHOMIINI_TIMERS, ITHOMIINI_SERVICE_ENV, APP_PORT); DEPLOY_SCRATCH_BIN goes first on the
# PATH (its systemctl), DEPLOY_SCRATCH_HOME is the HOME of the steps. A scratch root never installs the live units.
set -euo pipefail
release="${1:?release name}"
commit="${2:?commit}"
app="${ITHOMIINI_APP_ROOT:-/home/ubuntu/ithomiini}"
units="${ITHOMIINI_SYSTEMD_DIR:-$HOME/.config/systemd/user}"
service="${ITHOMIINI_SERVICE:-ithomiini.service}"
timers="${ITHOMIINI_TIMERS:-ithomiini-backup.timer ithomiini-gog-keepalive.timer ithomiini-drive-sync.timer ithomiini-nightly-tests.timer}"
service_env="${ITHOMIINI_SERVICE_ENV:-$HOME/.config/ithomiini/service.env}"
if [ "$app" != /home/ubuntu/ithomiini ] && { [ -z "${ITHOMIINI_SYSTEMD_DIR:-}" ] || [ "$service" = ithomiini.service ]; }; then
  echo "A scratch root ($app) needs ITHOMIINI_SYSTEMD_DIR and ITHOMIINI_SERVICE of its own: the live units stay untouched." >&2
  exit 1
fi
[ -n "${DEPLOY_SCRATCH_BIN:-}" ] && export PATH="$DEPLOY_SCRATCH_BIN:$PATH"
[ -n "${DEPLOY_SCRATCH_HOME:-}" ] && export HOME="$DEPLOY_SCRATCH_HOME"
case "$release" in '' | */* | .*) echo "Bad release name $release" >&2; exit 1 ;; esac
dir="$app/releases/$release"
[ -d "$dir" ] || { echo "No release $dir" >&2; exit 1; }
node="${ITHOMIINI_NODE:-$(sed -n 's/^ExecStart=\([^ ]*\/node\) .*/\1/p' "$dir/deploy/ithomiini.service")}"
export PATH="$(dirname "$node"):$PATH"
export ITHOMIINI_SHARED="$app/shared"
mkdir -p "$app/shared/.deploy-stamps" "$units"
ms() { echo $(( $(date +%s%N) / 1000000 )); }
t0=$(ms)
said() { printf '  %-22s %5d ms  %s\n' "$1" $(( $(ms) - $2 )) "${3:-}"; }

# One install at a time (the deploy lock, deploy.lock*, is the people's; this is the script's own). No step gets the
# lock's descriptor (9>&-): a process a step leaves running would hold the lock after the install.
exec 9>"$app/shared/.install.lock"
flock -w 900 9 || { echo "Another install still runs after 15 minutes: nothing changed." >&2; exit 1; }

s=$(ms)
if ! "$node" "$dir/scripts/deploy-record.mjs" verify "$dir" --commit "$commit"; then
  echo "Not installed: release $release does not match a passing test run of $commit. The app is unchanged." >&2
  exit 1
fi
said verify "$s"

logs="$(mktemp -d "$app/shared/.install-$release.XXXXXX")"
trap 'rm -rf -- "$logs"' EXIT
run() { # run <name> <command...> in the background, its output and time in $logs
  local name="$1"; shift
  ( s=$(ms); set +e; "$@" >"$logs/$name" 2>&1 9>&-; rc=$?; echo "$rc $(( $(ms) - s ))" >"$logs/$name.rc" ) &
}
base=$(sed -n 's/^APP_BASE_PATH=//p' "$service_env" 2>/dev/null | tail -n 1 || true)
run rename-app env APP_BASE_PATH="${base:-/}" bash "$dir/scripts/build-rename-app.sh" "$app/shared/rename-app" "$app/shared/photo-reader"
run wikiloc bash "$dir/tools/wikiloc/install-worker.sh"
if [ -d "$app/shared/identifier" ]; then
  run precompress nice -n 10 "$node" "$dir/tools/identifier/precompress_offline_parts.mjs" --if-needed "$app/shared/identifier"
fi
run prune bash -c 'bash "$1/scripts/prune-releases.sh" "$2" "$3" && if [ -d "$2/staged" ]; then cd "$2/staged" && ls -1t | tail -n +4 | while read -r old; do rm -rf -- "$old"; done; fi' prune "$dir" "$app" "${KEEP_RELEASES:-5}"
run t3-stamp "$node" "$dir/scripts/deploy-stamp.mjs" t3-provision "$dir"
wait
report() { # report <name> [tolerated]: prints the step; returns its exit status
  local rc ms_
  read -r rc ms_ <"$logs/$1.rc"
  printf '  %-22s %5d ms  %s\n' "$1" "$ms_" "$(tail -n 1 "$logs/$1" | cut -c1-110)"
  [ "$rc" = 0 ] || { echo "== $1 failed (exit $rc):" >&2; tail -n 30 "$logs/$1" >&2; }
  return "$rc"
}
problems=""
if report rename-app; then
  ln -sfn "$app/shared/rename-app" "$dir/rename-app"
  ln -sfn "$app/shared/photo-reader" "$dir/photo-reader"
else
  echo 'Rename Photos app not built: the Renombrar fotos tab will say it is not installed.' >&2
fi
report wikiloc || problems="$problems wikiloc"
if [ -f "$logs/precompress.rc" ]; then report precompress || echo "Offline identifier parts not made: the page downloads plain bytes." >&2; fi
report prune || problems="$problems prune"
stamp_now=""
report t3-stamp >/dev/null && stamp_now="$(tail -n 1 "$logs/t3-stamp")"

# Never restart in the middle of a save (server /health `writing`); DEPLOY_FORCE=1 restarts anyway.
s=$(ms)
if ! bash "$dir/scripts/wait-writes.sh" "${DEPLOY_WAIT:-600}" "${DEPLOY_FORCE:-0}" 9>&-; then
  echo "Not installed: a save is still in progress. Current is still $(readlink "$app/current" 2>/dev/null || echo none)." >&2
  exit 1
fi
said wait-writes "$s"

# T3 workspaces (every person's brief, skills and subagents; tokens kept): refreshed when what they are made from
# changed. A missing or unreadable hash means a refresh.
s=$(ms)
stamp_file="$app/shared/.deploy-stamps/t3-provision"
if [ -n "$stamp_now" ] && [ "$stamp_now" = "$(cat "$stamp_file" 2>/dev/null || true)" ]; then
  said t3-provision "$s" "workspaces unchanged: not refreshed"
elif (cd "$dir" && "$node" scripts/t3-provision.mjs --refresh-all) >"$logs/t3-provision" 2>&1 9>&-; then
  # The hash after the refresh (it rewrites the files it is made of): the next deploy compares with this one.
  "$node" "$dir/scripts/deploy-stamp.mjs" t3-provision "$dir" >"$stamp_file.new" 2>/dev/null && mv "$stamp_file.new" "$stamp_file" || rm -f -- "$stamp_file" "$stamp_file.new"
  said t3-provision "$s" "$(grep -c '^Refreshed' "$logs/t3-provision" || true) workspaces refreshed"
else
  rm -f -- "$stamp_file"
  echo "== t3-provision failed:" >&2; tail -n 30 "$logs/t3-provision" >&2
  problems="$problems t3-provision"
fi

# Last: the switch, the units, the restart and the health check.
s=$(ms)
cd "$app"
if test -L current; then readlink current >shared/previous-release; fi
ln -sfn "releases/$release" current
changed=""
for unit in current/deploy/ithomiini*.service current/deploy/ithomiini-*.timer; do
  cmp -s "$unit" "$units/$(basename "$unit")" || { cp "$unit" "$units/"; changed=1; }
done
[ -z "$changed" ] || systemctl --user daemon-reload 9>&-
systemctl --user enable -q "$service" 9>&-
# shellcheck disable=SC2086
systemctl --user enable -q --now $timers 9>&-
systemctl --user restart "$service" 9>&-
said switch+restart "$s" "${changed:+units changed, daemon reloaded}"

s=$(ms)
port="${APP_PORT:-$(sed -n 's/^APP_PORT=//p' "$service_env" 2>/dev/null | tail -n 1 || true)}"
port="${port:-8794}"
deadline=$(( $(date +%s) + ${DEPLOY_HEALTH_WAIT:-90} ))
health=""
until printf '%s' "$health" | grep -q '"status":"ok"'; do
  if [ "$(date +%s)" -ge "$deadline" ]; then
    echo "Health not ok after ${DEPLOY_HEALTH_WAIT:-90} s: ${health:-no answer}" >&2
    echo "Roll back to $(cat shared/previous-release 2>/dev/null): bash $app/current/scripts/rollback-release.sh" >&2
    exit 1
  fi
  sleep 0.2
  health="$(curl -s --max-time 5 "http://127.0.0.1:$port/health" || true)"
done
said health "$s" "status ok"
if [ -n "$problems" ]; then
  echo "Installed release $release ($commit) in $(( $(ms) - t0 )) ms, but these steps failed:$problems" >&2
  exit 1
fi
echo "Installed release $release ($commit) in $(( $(ms) - t0 )) ms; previous: $(cat shared/previous-release 2>/dev/null || echo none)."
