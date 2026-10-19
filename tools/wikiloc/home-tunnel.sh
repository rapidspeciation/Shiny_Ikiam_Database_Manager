#!/bin/bash
# The home computer's side of the Wikiloc route (README: «Through the home computer»): a user
# service that keeps an SSH connection to the app server with a reverse SOCKS forward, so the
# server's Wikiloc browser goes out through this computer's home address. Nothing is installed:
# only OpenSSH (7.6+; PermitRemoteOpen needs 8.7+) and systemd, already here.
#
#   bash tools/wikiloc/home-tunnel.sh            make the key (once) and print its line for the server
#   bash tools/wikiloc/home-tunnel.sh --install  then install and start the service
#   bash tools/wikiloc/home-tunnel.sh --remove   stop and remove the service (the key stays)
#
# SERVER (default ubuntu@158.101.98.20, the app server) and PORT (18080) can be changed.
set -euo pipefail
server=${SERVER:-ubuntu@158.101.98.20}
port=${PORT:-18080}
key="$HOME/.ssh/ithomiini_wikiloc_tunnel"
unit="$HOME/.config/systemd/user/ithomiini-wikiloc-tunnel.service"

if [ "${1:-}" = "--remove" ]; then
  systemctl --user disable --now ithomiini-wikiloc-tunnel.service 2>/dev/null || true
  rm -f "$unit"
  systemctl --user daemon-reload
  echo "Removed. On the server, delete the authorized_keys line ending in ithomiini-wikiloc-tunnel."
  exit 0
fi

[ -f "$key" ] || ssh-keygen -q -t ed25519 -N '' -C ithomiini-wikiloc-tunnel -f "$key"

if [ "${1:-}" != "--install" ]; then
  cat <<EOF
Add this line to ~/.ssh/authorized_keys of $server (one line). With it the key can only listen on
127.0.0.1:$port there (no shell, no other forward):

restrict,port-forwarding,permitlisten="127.0.0.1:$port",permitopen="127.0.0.1:9",command="/bin/false" $(cat "$key.pub")

Then: bash tools/wikiloc/home-tunnel.sh --install
EOF
  exit 0
fi

mkdir -p "$(dirname "$unit")"
cat > "$unit" <<UNIT
[Unit]
Description=Ithomiini: the app server's Wikiloc browser out through this computer's address (SOCKS on the server's 127.0.0.1:$port)
After=network-online.target
Wants=network-online.target

[Service]
# -R with a port only: a SOCKS proxy on the server whose connections leave from here, to web ports only.
ExecStart=/usr/bin/ssh -N -i $key -o IdentitiesOnly=yes -o BatchMode=yes -o ExitOnForwardFailure=yes \\
  -o ServerAliveInterval=30 -o ServerAliveCountMax=3 -o "PermitRemoteOpen=*:443 *:80" \\
  -R 127.0.0.1:$port $server
Restart=always
RestartSec=30

[Install]
WantedBy=default.target
UNIT
systemctl --user daemon-reload
systemctl --user enable --now ithomiini-wikiloc-tunnel.service
echo "Started. Check: systemctl --user status ithomiini-wikiloc-tunnel; on the server:"
echo "  curl -s --socks5-hostname 127.0.0.1:$port https://www.cloudflare.com/cdn-cgi/trace | grep -E '^(ip|loc)='"
echo "For it to run while nobody is logged in here: loginctl enable-linger $USER"
