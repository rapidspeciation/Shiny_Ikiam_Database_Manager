#!/usr/bin/env bash
# One hill-climb run of the dev set on the current checkout: warm-up probe, then the first 6 dev tasks;
# stops early when they are clearly worse than the baseline (neutral ×1.10 or a check lost), else the rest.
#   tools/assistant-eval/iterate.sh <label>      (the eval app must already run from this checkout)
#   BASE=<dir>[,<repeat dir>..] (names under $O, required) sets the baseline; the first one decides the early stop.
#   EVAL_INSTANCE=<lab T3 instance> (default claude-eval-main, Franz's main login; claude-eval = the team's ithomiini login).
#   It must have run on the same instance (claude-eval-main by default), e.g. a run.mjs re-baseline.
#   SAMPLES=<n> samples per dev task (default 2, a 3rd on a 1-1 tie: run.mjs); the probe runs once.
#   Compare only runs made on the same instance: the main login loads extra personal skills/CLAUDE.md, so
#   switching needs its own bare-chat probe and baseline first.
set -euo pipefail
W="$(cd "$(dirname "$0")" && pwd)"
O=/home/franz/.cache/ithomiini-lab/assistant-eval
L="$1"
T="$(date +%Y%m%dT%H%M)"
# No default: the old 4 Nov baselines ran on claude-eval, which is not the default instance any more.
[ -n "${BASE:-}" ] || { echo "set BASE=<baseline dir>[,<repeat>..] under $O, run on the same instance as EVAL_INSTANCE" >&2; exit 2; }
EVAL_INSTANCE="${EVAL_INSTANCE:-claude-eval-main}"
SAMPLES="${SAMPLES:-2}"
BASES="$(echo "$BASE" | sed "s|[^,]*|$O/&|g")"
# The lab reaper stops a dev app whose starting chat ended: check it answers before spending a run.
curl -sf -m 5 http://127.0.0.1:8805/health > /dev/null || { echo "eval app :8805 is down: start it with eval-app.mjs start" >&2; exit 2; }
node "$W/run.mjs" "$W/tasks/dev.json" "$O/$L-probe-$T" --only E00 --samples 1 --instance "$EVAL_INSTANCE" --variant "$L"
node "$W/run.mjs" "$W/tasks/dev.json" "$O/$L-a-$T" --only D01,D02,D03,D04,D05,D06 --parallel 2 --samples "$SAMPLES" --instance "$EVAL_INSTANCE" --variant "$L"
if node "$W/stop-check.mjs" "$O/${BASE%%,*}" "$O/$L-a-$T"; then
  node "$W/run.mjs" "$W/tasks/dev.json" "$O/$L-b-$T" --only D07,D08,D09,D10,D11,D12,D13 --parallel 2 --samples "$SAMPLES" --instance "$EVAL_INSTANCE" --variant "$L"
  mkdir -p "$O/$L-dev-$T"
  cat "$O/$L-a-$T/results.jsonl" "$O/$L-b-$T/results.jsonl" > "$O/$L-dev-$T/results.jsonl"
  node "$W/compare.mjs" "$BASES" "$O/$L-dev-$T"
else
  echo "EARLY STOP: $L"
fi
