#!/usr/bin/env bash
# Copies the team's workbook into the lab (read-only): runs scripts/cache-sandbox.mjs
# on the server (the Google credentials live only there), with its read-only
# Sheets connection, brings the file here (mode 600) and deletes the remote copy.
#   tools/lab/snapshot.sh            → $LAB/snapshot.json (the previous one kept as snapshot.prev.json)
# Env: ITHOMIINI_LAB_DIR (default ~/.cache/ithomiini-lab), LAB_SSH_HOST (default claudeclaw).
set -euo pipefail
LAB="${ITHOMIINI_LAB_DIR:-$HOME/.cache/ithomiini-lab}"
HOST="${LAB_SSH_HOST:-claudeclaw}"
umask 077
mkdir -p "$LAB"

# The server's own Node and source checkout; only GOOGLE_CREDENTIALS_FILE (and WORKBOOK_ID,
# when the service sets one) are passed on, nothing is printed from the env file.
REMOTE=$(ssh -o BatchMode=yes "$HOST" 'bash -s' <<'EOF'
set -euo pipefail
umask 077
NODE=$(grep -o '^ExecStart=[^ ]*' /home/ubuntu/ithomiini/current/deploy/ithomiini.service | cut -d= -f2)
ENVFILE=/home/ubuntu/.config/ithomiini/service.env
CRED=$(sed -n 's/^GOOGLE_CREDENTIALS_FILE=//p' "$ENVFILE" | tail -1 | tr -d '"'"'"'')
WB=$(sed -n 's/^WORKBOOK_ID=//p' "$ENVFILE" | tail -1 | tr -d '"'"'"'')
OUT=$(mktemp -d /tmp/ithomiini-lab-XXXXXX)/snapshot.json
cd /home/ubuntu/ithomiini/src
GOOGLE_CREDENTIALS_FILE="$CRED" ${WB:+WORKBOOK_ID="$WB"} "$NODE" scripts/cache-sandbox.mjs "$OUT" >&2
echo "$OUT"
EOF
)
trap 'ssh -o BatchMode=yes "$HOST" "rm -rf \"$(dirname "$REMOTE")\""' EXIT
case "$REMOTE" in /tmp/ithomiini-lab-*/snapshot.json) ;; *) echo "Unexpected remote path" >&2; exit 1 ;; esac

scp -q -o BatchMode=yes "$HOST:$REMOTE" "$LAB/snapshot.new.json"
chmod 600 "$LAB/snapshot.new.json"
node -e 'const s=JSON.parse(require("fs").readFileSync(process.argv[1],"utf8")); if(!s.sheets||!Object.keys(s.sheets).length) process.exit(1)' "$LAB/snapshot.new.json"
[ -f "$LAB/snapshot.json" ] && mv "$LAB/snapshot.json" "$LAB/snapshot.prev.json"
mv "$LAB/snapshot.new.json" "$LAB/snapshot.json"
date -u +%FT%TZ > "$LAB/snapshot.taken"
echo "Snapshot saved: $LAB/snapshot.json ($(du -h "$LAB/snapshot.json" | cut -f1)); restart tools/lab/app.sh to load it."
