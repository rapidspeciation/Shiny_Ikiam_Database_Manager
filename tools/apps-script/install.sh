#!/usr/bin/env bash
# Installs or updates SheetEditHook.gs in the workbook (WORKBOOK_ID, the team's by default) with gogcli.
# It adds a script bound to that workbook (Extensions → Apps Script): agree it with the team first.
# Needs a gog login with the appscript service, and the Apps Script API turned on
# for that account (https://script.google.com/home/usersettings).
# After the first install, open the script and run `setup` once to create the triggers.
set -euo pipefail
cd "$(dirname "$0")"
WORKBOOK=${WORKBOOK_ID:-1QZj6YgHAJ9NmFXFPCtu-i-1NDuDmAdMF2Wogts7S2_4}
ACCOUNT=${GOG_ACCOUNT:-franz.chandi@gmail.com}
CLIENT=${GOG_CLIENT:-claudeclaw}
# One script per workbook (the old apps-script-id file belongs to the test copy's script).
ID_FILE=~/.config/ithomiini/apps-script-id.$WORKBOOK
gog() { command gog --account "$ACCOUNT" --client "$CLIENT" --no-input "$@"; }

secret=$(ssh claudeclaw 'grep "^SHEET_HOOK_SECRET=" ~/.config/ithomiini/service.env | cut -d= -f2-')
[ -n "$secret" ] || { echo "SHEET_HOOK_SECRET is not set on the server" >&2; exit 1; }

mkdir -p "$(dirname "$ID_FILE")"
if [ ! -s "$ID_FILE" ]; then
  gog --json appscript create --title "Ithomiini app: edits to the app" --parent-id "$WORKBOOK" |
    python3 -c 'import json,sys; print(json.load(sys.stdin)["project"]["scriptId"])' >"$ID_FILE"
fi
script_id=$(cat "$ID_FILE")

body=$(mktemp -p ~/.cache)
trap 'rm -f "$body"' EXIT
chmod 600 "$body"
SECRET="$secret" python3 - >"$body" <<'EOF'
import json, os
manifest = {"timeZone": "America/Guayaquil", "runtimeVersion": "V8", "exceptionLogging": "STACKDRIVER"}
print(json.dumps({"files": [
    {"name": "appsscript", "type": "JSON", "source": json.dumps(manifest, indent=2)},
    {"name": "SheetEditHook", "type": "SERVER_JS", "source": open("SheetEditHook.gs").read()},
    {"name": "Config", "type": "SERVER_JS",
     "source": "// Written by tools/apps-script/install.sh; shared with the app's SHEET_HOOK_SECRET.\n"
               f"const HOOK_SECRET = {json.dumps(os.environ['SECRET'])};\n"},
]}))
EOF
gog api call script v1 projects.updateContent --allow-write --force --params "{\"scriptId\":\"$script_id\"}" --body "@$body" >/dev/null
echo "Updated https://script.google.com/d/$script_id/edit"
