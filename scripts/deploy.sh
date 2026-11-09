#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
# The release is exactly one commit, tested where it was built:
#   scripts/deploy.sh           on a PC: the full tests and the build here, then the release (the commit's files and
#                               this run's build, with its test record) goes to claudeclaw, which checks the record
#                               and installs it (scripts/install-release.sh): one ssh connection.
#   scripts/deploy.sh --stage   on a PC: the same tests and build; the release waits on claudeclaw (staged/<commit>)
#                               for a deploy started there.
#   scripts/deploy.sh           on claudeclaw (the T3 assistant's checkout, /home/ubuntu/ithomiini/src): installs
#                               only a commit a PC has staged after a passing run; DEPLOY_LOCAL_TESTS=1 runs the full
#                               tests here instead (slow on its 4 arm cores: for when the PC is off).
# claudeclaw tests every release again each night, off the deploy path (deploy/ithomiini-nightly-tests.timer).
stage_only=""
case "${1:-}" in
  --stage) stage_only=1 ;;
  "") ;;
  *) echo "usage: scripts/deploy.sh [--stage]" >&2; exit 2 ;;
esac
app="${ITHOMIINI_APP_ROOT:-/home/ubuntu/ithomiini}"
host="${DEPLOY_HOST:-claudeclaw}"
public="${DEPLOY_PUBLIC_URL-https://ithomiini-ikiam.com}"
# What a release holds from the commit (git archive); the build adds web/ and server/instructions-history.json.
# frontend/src/lib goes too: the assistant's Wikiloc tools run the monitoring code of the app (server/walks.mjs).
# tools/identifier: the AI Identifier's worker (identify_species); its models stay in shared/identifier (tools/identifier/setup.sh).
paths=(server docs assistant package.json rename-app.commit deploy scripts licenses PRODUCT.md DESIGN.md frontend/src/lib tools/wikiloc tools/identifier)
# Runs from a PC (over ssh to claudeclaw) or on claudeclaw itself (the T3 assistant's checkout, /home/ubuntu/ithomiini/src).
if [ "$(hostname)" = claudeclaw ] || [ "${DEPLOY_LOCAL:-}" = 1 ]; then
  local_install=1
  [ -z "$stage_only" ] || { echo "--stage is for a PC: on claudeclaw, scripts/deploy.sh installs a staged run." >&2; exit 2; }
  export PATH="${ITHOMIINI_NODE_DIR:-/home/ubuntu/.local/share/tiputini-runtime/node-v24.18.0-linux-arm64/bin}:$PATH"
  # Deploy only what is on GitHub, so every checkout (PCs and the server) can pull it.
  git fetch -q origin main
  if [ -n "$(git status --porcelain --untracked-files=no)" ] || [ "$(git rev-parse HEAD)" != "$(git rev-parse origin/main)" ]; then
    echo "Commit and push to origin/main first (git status / git push); the release must match GitHub." >&2
    exit 1
  fi
else
  local_install=""
  # The tests run on this checkout and the release is its commit: nothing changed or new beside it.
  if [ -n "$(git status --porcelain)" ]; then
    echo "Commit or remove these first (the release is exactly the commit HEAD, as tested):" >&2
    git status --porcelain | head -n 20 >&2
    exit 1
  fi
  # The server's assistant also commits to the team repo: never deploy over changes not pulled yet; and only a
  # commit that is on GitHub (origin and the team repo), so the server's checkout has exactly what was tested.
  remote="$(git remote -v | awk '/rapidspeciation\/Shiny_Ikiam_Database_Manager/ {print $1; exit}')"
  if [ -n "$remote" ]; then
    git fetch -q "$remote" main
    if ! git merge-base --is-ancestor "$remote/main" HEAD; then
      echo "$remote/main has commits this checkout lacks (e.g. made by the T3 assistant): git pull $remote main first." >&2
      exit 1
    fi
    [ "$(git rev-parse "$remote/main")" = "$(git rev-parse HEAD)" ] || { echo "HEAD is not on $remote/main yet: git push $remote HEAD:main first." >&2; exit 1; }
  fi
  if [ "$remote" != origin ] && git remote get-url origin >/dev/null 2>&1; then
    git fetch -q origin main
    if ! git merge-base --is-ancestor origin/main HEAD; then
      echo "origin/main has commits this checkout lacks: git pull origin main first." >&2
      exit 1
    fi
    [ "$(git rev-parse origin/main)" = "$(git rev-parse HEAD)" ] || { echo "HEAD is not on origin/main yet: git push origin HEAD:main first." >&2; exit 1; }
  fi
  [ -n "$remote" ] || git remote get-url origin >/dev/null 2>&1 || { echo "No origin or team remote: the release must be on GitHub." >&2; exit 1; }
fi
commit="$(git rev-parse HEAD)"
tree="$(git rev-parse 'HEAD^{tree}')"
# Kept from one deploy to the next on this machine, outside the releases: node's compiled code of each
# file, by its content (every test file starts a node that loads the server: about half that start).
cache="${ITHOMIINI_DEPLOY_CACHE:-$HOME/.cache/ithomiini-deploy}"
mkdir -p "$cache"
export NODE_COMPILE_CACHE="$cache/node-compile"
# The test jobs keep theirs there too: the type check's incremental file, the server tests' durations, the slow tests' last green run.
export ITHOMIINI_DEPLOY_CACHE="$cache"

# The full tests and the build of this checkout; every job's result goes to the test record.
run_tests() {
# The frontend's packages are installed again only when the lockfile, what is installed or node changed.
installed() { cat frontend/package-lock.json frontend/node_modules/.package-lock.json 2>/dev/null | sha256sum | cut -d' ' -f1; node --version; }
if [ "$(installed)" = "$(cat frontend/node_modules/.deploy-installed 2>/dev/null)" ]; then
  echo "Frontend packages as installed: npm ci not needed."
else
  npm --prefix frontend ci
  installed > frontend/node_modules/.deploy-installed
fi
# The one full test run of a change (the app-dev skill runs only the quick checks while developing):
# syntax and types (incremental: the cache above), server tests (scripts/test-server.mjs: a process per core) and
# frontend tests side by side, at low priority (on the server the app keeps answering meanwhile); the
# type check, the longest single step, a little ahead of the others. The build is made meanwhile, and
# deployed only if they all pass. It also writes server/instructions-history.json (the AI instructions
# page's history: the release has no .git); the types are checked beside it: vite only. The server tests run with
# the guard that refuses any model CLI, AI host or T3 (tests/helpers/no-ai.mjs, loaded by scripts/test-server.mjs; the
# frontend's is in its setupFiles). Beside them the slow suite (the real reads of the rack photo,
# scripts/test-slow.mjs): only when one of its inputs differs, byte for byte, from its last green run on this machine;
# the record says whether it ran ("pass") or was not needed ("unchanged"), with its inputs' hash.
logs="$(mktemp -d)"
# --- the test jobs (one block) ---
nice -n 5 node scripts/check.mjs >"$logs/check" 2>&1 & check=$!
nice -n 10 node scripts/test-server.mjs >"$logs/server" 2>&1 & server=$!
NO_COLOR=1 nice -n 10 npm --prefix frontend test >"$logs/frontend" 2>&1 & frontend=$!
nice -n 10 bash -c 'node scripts/instructions-history.mjs && cd frontend && npx vite build' >"$logs/build" 2>&1 & build=$!
nice -n 10 node scripts/test-slow.mjs --if-changed --result "$logs/slow.json" >"$logs/slow" 2>&1 & slow=$!
# --- end of the test jobs ---
failed=0
jobs_result=""
for job in check:$check server:$server frontend:$frontend build:$build slow:$slow; do
  if ! wait "${job#*:}"; then failed=1; echo "== ${job%%:*} failed:" >&2; tail -n 80 "$logs/${job%%:*}" >&2; fi
  [ "${job%%:*}" = slow ] || jobs_result="$jobs_result${jobs_result:+,}${job%%:*}=pass"
done
# The slow suite's result for the record: {"slow":"pass"|"unchanged","inputs":...}.
slow_result="$(cat "$logs/slow.json" 2>/dev/null || true)"
[ "$failed" = 1 ] || [ -n "$slow_result" ] || { failed=1; echo "== slow: no result from scripts/test-slow.mjs" >&2; }
grep -h -E '^ℹ (pass|fail) |Tests +[0-9]|^Type checked|^Instructions history|built in|^Slow tests' "$logs"/* || true
rm -rf "$logs"
[ "$failed" = 0 ] || { echo "Checks failed: nothing was deployed. Fix, commit, push and run scripts/deploy.sh again." >&2; exit 1; }
# The run tested exactly HEAD: nothing may have changed meanwhile.
[ "$(git rev-parse HEAD)" = "$commit" ] && [ -z "$(git status --porcelain --untracked-files=no)" ] || { echo "The checkout changed during the tests: nothing was deployed." >&2; exit 1; }
}

# assemble <release dir> <dir holding the build>: the commit's files and the build of the run.
assemble() {
  mkdir -p "$1/server"
  # umask 077: the same file modes from a PC and from claudeclaw (600 and 700, as the releases have had).
  (umask 077 && git archive --format=tar "$commit" -- "${paths[@]}" | tar -xf - -C "$1")
  # Hard links where the two are on one disk (claudeclaw: staged/ beside releases/), else a copy.
  cp -al "$2/web" "$1/" 2>/dev/null || cp -a "$2/web" "$1/"
  cp -a "$2/server/instructions-history.json" "$1/server/"
}
ms() { echo $(( $(date +%s%N) / 1000000 )); }
# A release's name: the UTC time, a second later when a release of that second exists here (claudeclaw); the upload
# from a PC refuses a name that exists there (never rsync into a release, the live one least of all).
new_release() {
  release="$(date -u +%Y%m%dT%H%M%SZ)"
  while [ -n "$local_install" ] && [ -e "$app/releases/$release" ]; do sleep 1; release="$(date -u +%Y%m%dT%H%M%SZ)"; done
}

after_install() {
  if [ -n "$public" ]; then
    curl -fsS --max-time 15 "$public/health" | grep -q '"status":"ok"' || { echo "$public/health is not \"status\":\"ok\": check it (rollback: bash $app/current/scripts/rollback-release.sh)." >&2; exit 1; }
    echo "Health ok at $public/health."
    # The deployed app through its API (scripts/smoke-live.mjs: /health and direct calls, never a chat or a model),
    # with a test account file: TEST_ACCOUNT_FILE.
    if [ -n "${TEST_ACCOUNT_FILE:-}" ]; then LIVE_SMOKE_URL="$public" node scripts/smoke-live.mjs; fi
  fi
}

if [ -n "$local_install" ]; then
  # On claudeclaw: a PC's passing run of exactly this commit (its build and record, staged/<commit>), or the full
  # tests here when asked.
  new_release
  dir="$app/releases/$release"
  staged="$app/staged/$commit"
  if [ -f "$staged/.deploy-record.json" ]; then
    echo "The PC's test run of $commit (staged $staged): installing its build."
    assemble "$dir" "$staged"
    cp "$staged/.deploy-record.json" "$dir/"
  elif [ "${DEPLOY_LOCAL_TESTS:-}" = 1 ]; then
    echo "DEPLOY_LOCAL_TESTS=1: the full tests here, at low priority."
    run_tests
    assemble "$dir" .
    node scripts/deploy-record.mjs write "$dir" --commit "$commit" --tree "$tree" --jobs "$jobs_result" --slow "$slow_result" --source claudeclaw-local
  else
    echo "No passing PC test run of $commit here ($staged): nothing was installed." >&2
    echo "On the PC: pull this commit, then scripts/deploy.sh --stage (it tests, builds and uploads; it installs nothing), and run this again." >&2
    echo "With the PC off: DEPLOY_LOCAL_TESTS=1 scripts/deploy.sh runs the full tests here (minutes, on the app's cores)." >&2
    exit 1
  fi
  if ! bash "$dir/scripts/install-release.sh" "$release" "$commit"; then
    [ -L "$app/current" ] && [ "$(readlink "$app/current")" = "releases/$release" ] || rm -rf -- "$dir"
    exit 1
  fi
  after_install
  exit 0
fi

run_tests
new_release
stage="$(mktemp -d "$cache/release.XXXXXX")"
ssh_opts="-o ControlMaster=auto -o ControlPath=$cache/ssh-%C -o ControlPersist=60"
# shellcheck disable=SC2086
trap 'rm -rf -- "$stage"; ssh $ssh_opts -O exit "$host" 2>/dev/null || true' EXIT
assemble "$stage" .
node scripts/deploy-record.mjs write "$stage" --commit "$commit" --tree "$tree" --jobs "$jobs_result" --slow "$slow_result" --source pc
if [ -n "$stage_only" ]; then dest="$app/staged/$commit"; fresh=""; else dest="$app/releases/$release"; fresh="test ! -e '$dest' && "; fi
# Only what changed since the live release travels: the rest is hard-linked from it (rsync --link-dest).
s=$(ms)
rsync -rlpD --checksum --delete --link-dest="$app/current/" -e "ssh $ssh_opts" \
  --rsync-path="mkdir -p '$(dirname "$dest")' && ${fresh}rsync" "$stage/" "$host:$dest/"
echo "Uploaded to $host:$dest in $(( $(ms) - s )) ms."
if [ -n "$stage_only" ]; then
  echo "Staged $commit: scripts/deploy.sh on claudeclaw (its checkout at this commit) installs it."
  exit 0
fi
# The scratch variables of scripts/install-release.sh, when set (tests); none in a live deploy.
forward=""
for v in ITHOMIINI_APP_ROOT ITHOMIINI_SYSTEMD_DIR ITHOMIINI_SERVICE ITHOMIINI_TIMERS ITHOMIINI_SERVICE_ENV APP_PORT \
  DEPLOY_SCRATCH_BIN DEPLOY_SCRATCH_HOME RENAME_APP_CACHE DEPLOY_WAIT DEPLOY_FORCE KEEP_RELEASES DEPLOY_HEALTH_WAIT; do
  [ -z "${!v:-}" ] || forward="$forward $v=$(printf %q "${!v}")"
done
s=$(ms)
# shellcheck disable=SC2086
ssh $ssh_opts "$host" "env$forward bash $dest/scripts/install-release.sh $release $commit"
echo "Installed on $host in $(( $(ms) - s )) ms."
after_install
