#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
npm --prefix frontend ci
node scripts/check.mjs
npm test
npm run build
release="$(date -u +%Y%m%dT%H%M%SZ)"
ssh claudeclaw "mkdir -p /home/ubuntu/ithomiini/releases/$release /home/ubuntu/ithomiini/shared /home/ubuntu/.config/systemd/user"
# frontend/src/lib goes too: the assistant's Wikiloc tools run the monitoring code of the app (server/walks.mjs).
tar -czf - server web docs assistant package.json deploy scripts licenses PRODUCT.md DESIGN.md frontend/src/lib | ssh claudeclaw "tar -xzf - -C /home/ubuntu/ithomiini/releases/$release"
ssh claudeclaw "set -eu; cd /home/ubuntu/ithomiini; if test -L current; then readlink current > shared/previous-release; fi; ln -sfn releases/$release current; cp current/deploy/ithomiini*.service current/deploy/ithomiini-*.timer /home/ubuntu/.config/systemd/user/; systemctl --user daemon-reload; systemctl --user enable ithomiini.service; systemctl --user enable --now ithomiini-backup.timer ithomiini-gog-keepalive.timer ithomiini-drive-sync.timer; systemctl --user restart ithomiini.service"
# T3 Code workspaces: every person's brief (CLAUDE.md/AGENTS.md) and skills follow the release; tokens are kept.
ssh claudeclaw 'set -eu; cd /home/ubuntu/ithomiini/current; node=$(sed -n "s/^ExecStart=\([^ ]*\/node\) .*/\1/p" deploy/ithomiini.service); "$node" scripts/t3-provision.mjs --refresh-all'
echo "Deployed release $release. Check /ithomiini/health before declaring success."
