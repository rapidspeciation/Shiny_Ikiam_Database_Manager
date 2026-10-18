#!/bin/bash
# Puts the AI Identifier's files where the app looks for them (IDENTIFIER_DIR, by default
# shared/identifier beside the database), once, on the app server. They are taken from the
# benchmark folder made on 17 Oct 2026 (~/bioclip-bench: the venv, the crop code, the head, the
# detector and the int8 encoder) as hard links: nothing is downloaded and no space is used twice.
# Since 18 Oct 2026 the detector is the nano V5N at 640 px: the folder needs the Space's v6_feature.py
# of that day, wing_seg_v5n.pt and its ONNX copy in app/assets, and Ultralytics 8.4.132 in the venv.
# The worker itself (tools/identifier/worker.py) comes with each release.
#
#   bash tools/identifier/setup.sh [dest] [source]
#   IDENTIFIER_CHECKLIST=/path/region_checklist.json   the Space's geographic prior (optional)
#
# Then a photo is identified once to check it (about 10 s), at low priority.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
dest=${1:-/home/ubuntu/ithomiini/shared/identifier}
src=${2:-/home/ubuntu/bioclip-bench}
encoder=onnx/bioclip25h_visual_int8.onnx

for f in venv/bin/python app/v6_feature.py app/scripts/wings_v6 app/assets/head_hier.pt app/assets/wing_seg_v5n.pt app/assets/wing_seg_v5n.onnx app/assets/field_wingtip_override_v1.json "$encoder"; do
  test -e "$src/$f" || { echo "Missing $src/$f" >&2; exit 1; }
done
check() { [ "$(sha256sum "$1" | cut -d' ' -f1)" = "$2" ] || { echo "$1: not the expected file (SHA-256)" >&2; exit 1; }; }
check "$src/app/assets/head_hier.pt" 749fbffd5f73f12c0f8598b5b2e3f575c0510b93a61c3f3c0123ab2ee8990830
check "$src/app/assets/wing_seg_v5n.pt" d74f9c1323d210aedbb15b1a446bf762b598446dae32449e972b3cba06b9b3ac
check "$src/app/assets/wing_seg_v5n.onnx" a48730107024e3557b0de6626314a398d745151966a7727a67788c49601c8f4b
check "$src/$encoder" bf27f8e52aa74c295fb725b98fb6bd2fd467f7d4a9e1e2e7f04c610f1245cca1
grep -q V5N_WEIGHT_SHA256 "$src/app/v6_feature.py" || { echo "$src/app/v6_feature.py: not the Space's crop code of 18 Oct 2026 (no V5N)" >&2; exit 1; }
version=$("$src/venv/bin/python" -I -c 'from importlib.metadata import version; print(version("ultralytics"))')
[ "$version" = 8.4.132 ] || { echo "Ultralytics $version in $src/venv: the crop was measured with 8.4.132" >&2; exit 1; }

mkdir -p "$dest/onnx" "$dest/app"
umask 077
# Hard links (the same disk); a copy where they cannot be made.
link() { cp -al "$1" "$2" 2>/dev/null || cp -a "$1" "$2"; }
[ -e "$dest/venv" ] || link "$src/venv" "$dest/venv"
[ -e "$dest/app/v6_feature.py" ] || link "$src/app/v6_feature.py" "$dest/app/"
[ -e "$dest/app/scripts" ] || link "$src/app/scripts" "$dest/app/"
[ -e "$dest/app/assets" ] || link "$src/app/assets" "$dest/app/"
[ -e "$dest/$encoder" ] || link "$src/$encoder" "$dest/$encoder"
if [ -n "${IDENTIFIER_CHECKLIST:-}" ]; then cp "$IDENTIFIER_CHECKLIST" "$dest/app/assets/region_checklist.json"; fi
find "$dest" -name __pycache__ -prune -exec rm -rf {} +
du -sh "$dest"

photo=$(ls "$src"/photos/*.jpg 2>/dev/null | head -1 || true)
if [ -n "$photo" ]; then
  scratch=$(mktemp -d)
  echo "{\"id\":\"1\",\"path\":\"$photo\",\"side\":\"East\"}" |
    YOLO_CONFIG_DIR=$scratch MPLCONFIGDIR=$scratch YOLO_OFFLINE=1 nice -n 10 "$dest/venv/bin/python" -I -B "$here/worker.py" --dir "$dest" --idle 1 2>"$scratch/err" | tail -n 1
  rm -rf "$scratch"
fi
