#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
# Runs from a PC (over ssh to claudeclaw) or on claudeclaw itself (the T3 assistant's
# checkout, /home/ubuntu/ithomiini/src): the same steps, run locally there.
if [ "$(hostname)" = claudeclaw ] || [ "${DEPLOY_LOCAL:-}" = 1 ]; then
  on_server() { bash -c "$1"; }
  export PATH="/home/ubuntu/.local/share/tiputini-runtime/node-v24.18.0-linux-arm64/bin:$PATH"
  # Deploy only what is on GitHub, so every checkout (PCs and the server) can pull it.
  git fetch -q origin main
  if [ -n "$(git status --porcelain --untracked-files=no)" ] || [ "$(git rev-parse HEAD)" != "$(git rev-parse origin/main)" ]; then
    echo "Commit and push to origin/main first (git status / git push); the release must match GitHub." >&2
    exit 1
  fi
else
  on_server() { ssh claudeclaw "$1"; }
  # The server's assistant also commits to the team repo: never deploy over changes not pulled yet.
  remote="$(git remote -v | awk '/rapidspeciation\/Shiny_Ikiam_Database_Manager/ {print $1; exit}')"
  if [ -n "$remote" ]; then
    git fetch -q "$remote" main
    if ! git merge-base --is-ancestor "$remote/main" HEAD; then
      echo "$remote/main has commits this checkout lacks (e.g. made by the T3 assistant): git pull $remote main first." >&2
      exit 1
    fi
  fi
fi
npm --prefix frontend ci
node scripts/check.mjs
npm test
# Also writes server/instructions-history.json (the AI instructions page's history: the release has no .git).
npm run build
release="$(date -u +%Y%m%dT%H%M%SZ)"
on_server "mkdir -p /home/ubuntu/ithomiini/releases/$release /home/ubuntu/ithomiini/shared /home/ubuntu/.config/systemd/user"
# frontend/src/lib goes too: the assistant's Wikiloc tools run the monitoring code of the app (server/walks.mjs).
tar --exclude='tools/wikiloc/node_modules' --exclude='tools/wikiloc/venv' --exclude='tools/wikiloc/__pycache__' -czf - server web docs assistant package.json deploy scripts licenses PRODUCT.md DESIGN.md frontend/src/lib tools/wikiloc | on_server "tar -xzf - -C /home/ubuntu/ithomiini/releases/$release"
# Never restart in the middle of a save: wait while a proposal is being applied, a save is being written
# or a save's outcome is still unknown (server /health `writing`). The app itself also finishes saves in
# progress when it is stopped (SIGTERM). DEPLOY_FORCE=1 deploys anyway (e.g. Google down for long).
on_server "bash /home/ubuntu/ithomiini/releases/$release/scripts/wait-writes.sh ${DEPLOY_WAIT:-600} ${DEPLOY_FORCE:-0}"
on_server "set -eu; cd /home/ubuntu/ithomiini; if test -L current; then readlink current > shared/previous-release; fi; ln -sfn releases/$release current; cp current/deploy/ithomiini*.service current/deploy/ithomiini-*.timer /home/ubuntu/.config/systemd/user/; systemctl --user daemon-reload; systemctl --user enable ithomiini.service; systemctl --user enable --now ithomiini-backup.timer ithomiini-gog-keepalive.timer; systemctl --user restart ithomiini.service"
on_server 'set -eu; cd /home/ubuntu/ithomiini/current; node=$(sed -n "s/^ExecStart=\([^ ]*\/node\) .*/\1/p" deploy/ithomiini.service); export PATH="$(dirname "$node"):$PATH"; bash tools/wikiloc/install-worker.sh'
# T3 Code workspaces: every person's brief (AGENTS.md, CLAUDE.md a link to it) and skills follow the release; tokens are kept.
on_server 'set -eu; cd /home/ubuntu/ithomiini/current; node=$(sed -n "s/^ExecStart=\([^ ]*\/node\) .*/\1/p" deploy/ithomiini.service); "$node" scripts/t3-provision.mjs --refresh-all'
# Old releases (about 4 MB each) are deleted: the newest KEEP_RELEASES stay, and always the current and previous ones.
on_server "bash /home/ubuntu/ithomiini/current/scripts/prune-releases.sh /home/ubuntu/ithomiini ${KEEP_RELEASES:-5}"
echo "Deployed release $release. Check /ithomiini/health before declaring success."
