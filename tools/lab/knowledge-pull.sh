#!/usr/bin/env bash
# The project's Drive documents for the lab: a read-only copy of the server's text mirror
# (shared/knowledge, kept by scripts/drive-sync.mjs with the project account) into the lab's
# KNOWLEDGE_DIR. The lab gets no Google account for Drive; it only copies Markdown from the server.
#   tools/lab/knowledge-pull.sh            copy the server's mirror as it is (the timer, twice a day)
#   tools/lab/knowledge-pull.sh --sync     first bring the server's mirror up to date with Drive
#                                           (the server's own sync unit, as its chats' sync_documents does)
# As the user unit ithomiini-drive-sync.service, the lab chats' sync_documents runs it with --sync.
set -euo pipefail
HOST="${LAB_KNOWLEDGE_HOST:-claudeclaw}"
REMOTE="${LAB_KNOWLEDGE_REMOTE:-/home/ubuntu/ithomiini/shared/knowledge/}"
LAB="${LAB_DIR:-$HOME/.cache/ithomiini-lab}"
APPDIR="$LAB/app"
[ -s "$LAB/workbook-id" ] && APPDIR="$LAB/app-workbook"
DEST="${LAB_KNOWLEDGE_DIR:-$APPDIR/knowledge}"
SSH=(ssh -o BatchMode=yes -o ConnectTimeout=15)
if [ "${1:-}" = "--sync" ]; then
  "${SSH[@]}" "$HOST" 'systemctl --user start ithomiini-drive-sync.service' || echo "Server sync failed; copying its mirror as it is" >&2
fi
mkdir -p "$DEST"
rsync -a --delete --chmod=Du=rwx,Dgo=,Fu=rw,Fgo= -e "${SSH[*]}" "$HOST:$REMOTE" "$DEST/"
echo "Documents copied to $DEST ($(find "$DEST" -name '*.md' | wc -l) files)"
