#!/bin/bash
# Installs the Camoufox Wikiloc processor on the app server.
# It is copied to ~/.local/share/ithomiini-wikiloc so it keeps running whatever
# branch the repository has checked out. Run again after changing the helper.
# Credentials go in ~/.config/ithomiini-wikiloc/worker.json (see README.md).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
dest="$HOME/.local/share/ithomiini-wikiloc"
node=$(command -v node)
python=${ITHOMIINI_WIKILOC_INSTALL_PYTHON:-python3}
config="$HOME/.config/ithomiini-wikiloc/worker.json"
test -f "$config" || { echo "Missing $config" >&2; exit 1; }
command -v Xvfb >/dev/null || { echo 'Install Xvfb first (sudo apt install xvfb python3-venv libgtk-3-0).' >&2; exit 1; }
mkdir -p "$dest" "$HOME/.config/systemd/user"
# Every deploy runs this: when the helper, this script and node are the same as last time and the
# worker is running, nothing is reinstalled or restarted (a Wikiloc job in progress goes on).
stamp=$(cd "$here" && cat lib.mjs worker.mjs fetch.mjs browser.mjs browser.py requirements.txt package.json package-lock.json install-worker.sh | sha256sum | cut -d' ' -f1)-$node
if [ "$(cat "$dest/.installed" 2>/dev/null)" = "$stamp" ] && systemctl --user is-active -q ithomiini-wikiloc.service; then
  echo "Wikiloc worker unchanged: not reinstalled."
  exit 0
fi
cp "$here/lib.mjs" "$here/worker.mjs" "$here/fetch.mjs" "$here/browser.mjs" "$here/browser.py" "$here/requirements.txt" "$here/package.json" "$here/package-lock.json" "$dest/"
"$python" -m venv "$dest/venv"
"$dest/venv/bin/python" -m pip install --disable-pip-version-check -q -r "$dest/requirements.txt"
"$dest/venv/bin/python" -m camoufox fetch official/152.0.4-beta.31
chmod 600 "$config"
cat > "$HOME/.config/systemd/user/ithomiini-wikiloc.service" <<UNIT
[Unit]
Description=Ithomiini: process Wikiloc links and profile checks from the app
After=network-online.target
Wants=network-online.target

[Service]
WorkingDirectory=$dest
ExecStart=$node $dest/worker.mjs
Environment=ITHOMIINI_WIKILOC_PYTHON=$dest/venv/bin/python
Restart=always
RestartSec=60
KillMode=control-group

[Install]
WantedBy=default.target
UNIT
systemctl --user daemon-reload
systemctl --user enable --now ithomiini-wikiloc.service
systemctl --user restart ithomiini-wikiloc.service
echo "$stamp" > "$dest/.installed"
echo "Installed. Logs: journalctl --user -u ithomiini-wikiloc -f"
