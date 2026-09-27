#!/bin/bash
# Installs the Wikiloc processor as a systemd user service on this computer.
# It is copied to ~/.local/share/ithomiini-wikiloc so it keeps running whatever
# branch the repository has checked out. Run again after changing the helper.
# Credentials go in ~/.config/ithomiini-wikiloc/worker.json (see README.md).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
dest="$HOME/.local/share/ithomiini-wikiloc"
node=$(command -v node)
mkdir -p "$dest" "$HOME/.config/systemd/user"
cp "$here/lib.mjs" "$here/worker.mjs" "$here/fetch.mjs" "$here/package.json" "$here/package-lock.json" "$dest/"
npm --prefix "$dest" ci --omit=dev --no-audit --no-fund >/dev/null
test -f "$HOME/.config/ithomiini-wikiloc/worker.json" || echo "Missing ~/.config/ithomiini-wikiloc/worker.json" >&2
cat > "$HOME/.config/systemd/user/ithomiini-wikiloc.service" <<UNIT
[Unit]
Description=Ithomiini: process Wikiloc links and profile checks from the app
After=network-online.target

[Service]
WorkingDirectory=$dest
ExecStart=$node $dest/worker.mjs
Restart=always
RestartSec=60

[Install]
WantedBy=default.target
UNIT
systemctl --user daemon-reload
systemctl --user enable --now ithomiini-wikiloc.service
systemctl --user restart ithomiini-wikiloc.service
echo "Installed. Logs: journalctl --user -u ithomiini-wikiloc -f"
