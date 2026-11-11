#!/usr/bin/env bash
# The full tests of the live release's commit on claudeclaw itself (arm64), off the deploy path: run each night by
# deploy/ithomiini-nightly-tests.timer at nice 19. A deploy is tested on a PC; this checks the same commit on the
# server's own node and cores. It writes a dated log and last-result.json in shared/nightly-tests/ and nothing else:
# no AI (the server tests run with tests/helpers/no-ai.mjs, the frontend's guard is in its setupFiles), no WhatsApp,
# no team chat, no live data (a fresh copy of the commit, its own HOME, none of the service's settings); of the live app
# it reads only its public /health (the wikiloc job, scripts/nightly-app-checks.mjs). One job besides the tests, `ranges`,
# works on the workbook: the fixed end rows of its rules kept ahead of its rows (server/sheet-ranges.mjs), from the live
# release with only the app's Google credential (its path from service.env); SHEET_RANGES_GUARD=0 there turns it off.
#   bash /home/ubuntu/ithomiini/current/scripts/nightly-tests.sh
# ITHOMIINI_APP_ROOT (default /home/ubuntu/ithomiini), NIGHTLY_SCRATCH_PARENT (default ~/.cache/ithomiini-nightly) and
# NIGHTLY_HEALTH_URL (default http://127.0.0.1:8794/health, the live app's) and ITHOMIINI_SERVICE_ENV (default
# ~/.config/ithomiini/service.env) for other setups.
set -uo pipefail
app="${ITHOMIINI_APP_ROOT:-/home/ubuntu/ithomiini}"
out="$app/shared/nightly-tests"
mkdir -p "$out"
stamp="$(date -u +%Y%m%dT%H%M%SZ)"
log="$out/$stamp.log"
exec >"$log" 2>&1
started="$(date -u +%FT%TZ)"
node="$(sed -n 's/^ExecStart=\([^ ]*\/node\) .*/\1/p' "$app/current/deploy/ithomiini.service")"
export PATH="$(dirname "$node"):/usr/local/bin:/usr/bin:/bin"
declare -A result=()
commit=""
finish() { # finish <ok|failed|skipped> <note>
  local jobs="" k
  for k in "${!result[@]}"; do jobs="$jobs${jobs:+,}\"$k\":\"${result[$k]}\""; done
  printf '{"outcome":"%s","note":"%s","commit":"%s","release":"%s","startedAt":"%s","finishedAt":"%s","log":"%s","jobs":{%s}}\n' \
    "$1" "$2" "$commit" "$(readlink "$app/current")" "$started" "$(date -u +%FT%TZ)" "$log" "$jobs" >"$out/last-result.json.new"
  mv "$out/last-result.json.new" "$out/last-result.json"
  echo "Nightly tests: $1 ($2)"
  # The newest 14 logs stay.
  ls -1 "$out" | grep -E '^[0-9]{8}T[0-9]{6}Z\.log$' | sort -r | tail -n +15 | while read -r old; do rm -f -- "$out/$old"; done
  [ "$1" != failed ]
  exit $?
}
commit="$(sed -n 's/^ *"commit": *"\([0-9a-f]\{40\}\)".*/\1/p' "$app/current/.deploy-record.json" 2>/dev/null | head -n 1)"
[ -n "$commit" ] || finish skipped "the live release has no test record (deployed before 9 Nov 2026)"
echo "Release $(readlink "$app/current"), commit $commit"
# A fresh copy of the commit from the server's checkout (fetched when the checkout lacks it).
if ! git -C "$app/src" cat-file -e "$commit^{commit}" 2>/dev/null; then git -C "$app/src" fetch -q origin || true; fi
git -C "$app/src" cat-file -e "$commit^{commit}" 2>/dev/null || finish failed "commit $commit not in $app/src"
mkdir -p "${NIGHTLY_SCRATCH_PARENT:-$HOME/.cache/ithomiini-nightly}"
scratch="$(mktemp -d "${NIGHTLY_SCRATCH_PARENT:-$HOME/.cache/ithomiini-nightly}/nightly-tests-$stamp.XXXXXX")"
trap 'rm -rf -- "$scratch"' EXIT
# The copy in src/, its HOME and temp folder beside it: a test's helper file in the temp folder must not fall under the
# copy's package.json ("type": "module").
mkdir -p "$scratch/src" "$scratch/home" "$scratch/tmp"
git -C "$app/src" archive "$commit" | tar -xf - -C "$scratch/src/" || finish failed "git archive failed"
cd "$scratch/src" || finish failed "no scratch copy"
[ -f tests/helpers/no-ai.mjs ] || finish failed "tests/helpers/no-ai.mjs is missing: not run without the no-AI guard"
grep -q 'test-no-ai' frontend/vite.config.ts || finish failed "the frontend's no-AI setup file is missing: not run"
# Only what the tests need: their own HOME and temp folder, the npm cache kept between nights; nothing of the
# service's environment (service.env, gog, the Claude homes).
clean=(env -i "PATH=$PATH" "HOME=$scratch/home" "TMPDIR=$scratch/tmp" LANG=C.UTF-8 NO_COLOR=1 CI=1
  "npm_config_cache=${HOME:-/home/ubuntu}/.cache/ithomiini-nightly/npm" "NODE_COMPILE_CACHE=${HOME:-/home/ubuntu}/.cache/ithomiini-nightly/node-compile")
job() { # job <name> <command...>: one at a time, at the lowest priority
  local name="$1" s rc; shift
  s=$(date +%s)
  echo "== $name: $*"
  nice -n 19 "${clean[@]}" "$@"
  rc=$?
  # 3: a warning (scripts/nightly-app-checks.mjs), kept in the result without failing the night.
  if [ "$rc" = 0 ]; then result[$name]=pass; elif [ "$rc" = 3 ] && { [ "$name" = wikiloc ] || [ "$name" = ranges ]; }; then result[$name]=warn; else result[$name]=fail; fi
  echo "== $name: ${result[$name]} in $(( $(date +%s) - s )) s"
}
# The workbook's fixed end rows (a conditional format's `$A$2:$A$20846`, a dropdown's list) moved ahead when its rows
# come within 1,000 of them, each move one line here and one record in sheet-ranges.jsonl (the rule as it was). The
# live release's own script (not the copy, whose packages npm installs), with the credential's path and nothing else.
service_env="${ITHOMIINI_SERVICE_ENV:-${HOME:-/home/ubuntu}/.config/ithomiini/service.env}"
setting() { sed -n "s/^$1=//p" "$service_env" 2>/dev/null | tail -n 1; }
if [ "$(setting SHEET_RANGES_GUARD)" = 0 ]; then
  echo "== ranges: off (SHEET_RANGES_GUARD=0 in $service_env)"
elif [ -z "$(setting GOOGLE_CREDENTIALS_FILE)" ]; then
  echo "== ranges: no GOOGLE_CREDENTIALS_FILE in $service_env"; result[ranges]=fail
else
  wb="$(setting WORKBOOK_ID)"
  job ranges env "GOOGLE_CREDENTIALS_FILE=$(setting GOOGLE_CREDENTIALS_FILE)" ${wb:+"WORKBOOK_ID=$wb"} \
    node "$app/current/scripts/nightly-app-checks.mjs" --sheet-ranges --log "$out/sheet-ranges.jsonl"
fi
job install npm --prefix frontend ci --no-audit --no-fund --prefer-offline
[ "${result[install]}" = pass ] || finish failed "npm ci failed"
job check node scripts/check.mjs
# As npm run test:nightly, one result each: the slow suite always (the real reads of the rack photo,
# scripts/test-slow.mjs; a deploy runs it only when its inputs changed), then every server test file in its own process
# (scripts/test-server.mjs --isolated; a deploy groups them), two at a time: the live app keeps the other cores. Both
# run whatever the other's result, both with the no-AI guard.
job slow node scripts/test-slow.mjs
job server node scripts/test-server.mjs --isolated --concurrency 2
job frontend npm --prefix frontend test -- --maxWorkers=2
# The live app's own state, from its /health (no login, no AI): the latest Wikiloc import failed or its worker stopped
# (fail), or a followed profile went 30 days unchecked (warn). One line in this log says which.
job wikiloc node scripts/nightly-app-checks.mjs "${NIGHTLY_HEALTH_URL:-http://127.0.0.1:8794/health}"
failed="" warned=""
for k in "${!result[@]}"; do
  [ "${result[$k]}" = fail ] && failed="$failed${failed:+, }$k"
  [ "${result[$k]}" = warn ] && warned="$warned${warned:+, }$k"
done
[ -z "$failed" ] || finish failed "failed: $failed${warned:+; warning: $warned}"
finish ok "all suites passed${warned:+; warning: $warned}"
