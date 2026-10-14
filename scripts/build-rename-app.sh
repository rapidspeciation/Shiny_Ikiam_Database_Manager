#!/usr/bin/env bash
# Builds the Rename Photos app (github.com/Fr4nzz/rename_photos_AI, its web-app/) for this site:
# the «Renombrar fotos» tab shows it in a frame, and the server serves it under <base>/rename-app/
# (server/index.mjs serveRenameApp).
#   scripts/build-rename-app.sh [out dir]     (default: rename-app/ beside server/)
# Its source: RENAME_APP_DIR (a checkout: the lab uses the one beside this repository), or else the
# commit named in rename-app.commit, fetched into a cache (RENAME_APP_CACHE). APP_BASE_PATH: the
# path this app is served under (/ by default). Built again only when the source or the path changed.
set -euo pipefail
here="$(cd "$(dirname "$0")/.." && pwd)"
out="$(mkdir -p "${1:-$here/rename-app}" && cd "${1:-$here/rename-app}" && pwd)"
base="${APP_BASE_PATH:-/}"
base="${base%/}/rename-app/"
repo="https://github.com/Fr4nzz/rename_photos_AI.git"

if [ -n "${RENAME_APP_DIR:-}" ]; then
  src="$RENAME_APP_DIR"
  [ -d "$src/web-app" ] || { echo "No web-app/ in RENAME_APP_DIR ($src)" >&2; exit 1; }
  # A checkout being worked on: its commit and whatever is changed in it.
  stamp="$(git -C "$src" rev-parse HEAD) $( { git -C "$src" status --porcelain -- web-app; git -C "$src" diff -- web-app; } | sha256sum | cut -c1-16)"
else
  commit="$(tr -d '[:space:]' < "$here/rename-app.commit")"
  src="${RENAME_APP_CACHE:-$HOME/.cache/ithomiini/rename-app-src}"
  if [ "$(git -C "$src" rev-parse HEAD 2>/dev/null || true)" != "$commit" ]; then
    [ -d "$src/.git" ] || git clone -q --filter=blob:none "$repo" "$src"
    git -C "$src" fetch -q origin
    git -C "$src" checkout -q --detach "$commit" || {
      echo "Commit $commit is not in $repo: push the Rename Photos repository first." >&2
      exit 1
    }
  fi
  stamp="$commit"
fi
stamp="$stamp $base"
if [ -f "$out/index.html" ] && [ "$(cat "$out/.built-from" 2>/dev/null || true)" = "$stamp" ]; then
  echo "Rename Photos app: up to date ($out)"
  exit 0
fi
cd "$src/web-app"
[ -d node_modules ] || npm ci --no-audit --no-fund
made="$(mktemp -d)"
trap 'rm -rf "$made"' EXIT
RENAME_APP_BASE="$base" npx vite build --outDir "$made" --emptyOutDir >/dev/null
find "$out" -mindepth 1 -delete
cp -a "$made"/. "$out"/
echo "$stamp" > "$out/.built-from"
echo "Rename Photos app: built for $base into $out ($(du -sh "$out" | cut -f1))"
