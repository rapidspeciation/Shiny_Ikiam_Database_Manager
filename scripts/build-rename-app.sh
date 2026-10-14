#!/usr/bin/env bash
# Builds the Rename Photos app (github.com/Fr4nzz/rename_photos_AI, its web-app/) for this site:
# the «Renombrar fotos» tab shows it in a frame, and the server serves it under <base>/rename-app/
# (server/index.mjs serveRenameApp).
#   scripts/build-rename-app.sh [out dir] [reader dir]   (default: rename-app/ and photo-reader/ beside server/)
# The second: its CAMID reader for the server (web-app/src/node/photoReader.ts, server/drive-photos.mjs),
# bundled with its models and its two native packages (onnxruntime-node, sharp; this platform's files only).
# Its source: RENAME_APP_DIR (a checkout: the lab uses the one beside this repository), or else the
# commit named in rename-app.commit, fetched into a cache (RENAME_APP_CACHE). APP_BASE_PATH: the
# path this app is served under (/ by default). Built again only when the source or the path changed.
set -euo pipefail
here="$(cd "$(dirname "$0")/.." && pwd)"
out="$(mkdir -p "${1:-$here/rename-app}" && cd "${1:-$here/rename-app}" && pwd)"
reader="$(mkdir -p "${2:-$(dirname "$out")/photo-reader}" && cd "${2:-$(dirname "$out")/photo-reader}" && pwd)"
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
web_done=""
if [ -f "$out/index.html" ] && [ "$(cat "$out/.built-from" 2>/dev/null || true)" = "$stamp" ]; then
  echo "Rename Photos app: up to date ($out)"
  web_done=1
fi
reader_done=""
if [ -f "$reader/photo-reader.mjs" ] && [ "$(cat "$reader/.built-from" 2>/dev/null || true)" = "$stamp" ]; then
  echo "Photo reader: up to date ($reader)"
  reader_done=1
fi
[ -n "$web_done" ] && [ -n "$reader_done" ] && exit 0
cd "$src/web-app"
# Its dependencies, again when they changed (a checkout's lock file).
lock="$(sha256sum package-lock.json | cut -c1-16)"
if [ ! -d node_modules ] || [ "$(cat node_modules/.lock-hash 2>/dev/null || true)" != "$lock" ]; then
  npm ci --no-audit --no-fund
  echo "$lock" > node_modules/.lock-hash
fi

if [ -z "$reader_done" ] && [ -f src/node/photoReader.ts ]; then
  # The reader: one file, the models beside it, and the two packages it loads, at the versions the app has.
  npx esbuild src/node/photoReader.ts --bundle --platform=node --format=esm --target=node20 \
    --external:onnxruntime-node --external:sharp --alias:@=./src --log-level=warning --outfile="$reader/photo-reader.mjs.new"
  mkdir -p "$reader/models"
  cp -a public/models/. "$reader/models/"
  versions="$(node -e "const l=require('./package-lock.json').packages;console.log(['onnxruntime-node','sharp'].map(p=>p+'@'+l['node_modules/'+p].version).join(' '))")"
  if [ "$(cat "$reader/.packages" 2>/dev/null || true)" != "$versions" ]; then
    rm -rf "$reader/node_modules"
    [ -f "$reader/package.json" ] || echo '{"name":"photo-reader","private":true,"type":"module"}' > "$reader/package.json"
    (cd "$reader" && npm install --no-audit --no-fund --no-save $versions >/dev/null)
    echo "$versions" > "$reader/.packages"
  fi
  # onnxruntime ships every platform's library, and GPU ones (300 MB): only this platform's, for the CPU, are kept.
  ort="$reader/node_modules/onnxruntime-node/bin/napi-v6"
  if [ -d "$ort" ]; then
    platform="$(node -p process.platform)"; arch="$(node -p process.arch)"
    find "$ort" -mindepth 1 -maxdepth 1 ! -name "$platform" -exec rm -rf {} +
    find "$ort/$platform" -mindepth 1 -maxdepth 1 ! -name "$arch" -exec rm -rf {} + 2>/dev/null || true
    find "$ort" \( -name 'libonnxruntime_providers_cuda*' -o -name 'libonnxruntime_providers_tensorrt*' \) -delete
  fi
  mv "$reader/photo-reader.mjs.new" "$reader/photo-reader.mjs"
  echo "$stamp" > "$reader/.built-from"
  echo "Photo reader: built into $reader ($(du -sh "$reader" | cut -f1))"
fi
[ -n "$web_done" ] && exit 0
made="$(mktemp -d)"
trap 'rm -rf "$made"' EXIT
RENAME_APP_BASE="$base" npx vite build --outDir "$made" --emptyOutDir >/dev/null
find "$out" -mindepth 1 -delete
cp -a "$made"/. "$out"/
echo "$stamp" > "$out/.built-from"
echo "Rename Photos app: built for $base into $out ($(du -sh "$out" | cut -f1))"
