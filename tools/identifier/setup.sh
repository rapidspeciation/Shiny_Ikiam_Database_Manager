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
#   SEX_SRC=/path/sex-model   the collection sex model for the background photo readings (server/readings.mjs),
#     copied into <dest>/sex: a folder with yolo_wings_best.pt (the 4-wing detector of its crops),
#     cand_cv/seed*_fold*.pt (its 15 heads), train_sex.py (their Net) and sex_predictions.json (the gallery's,
#     its out-of-fold evidence and folds). From WingsClassificator: reports/sex_improvement_20260927/work/
#     yolo_wings_best.pt, reports/release_20261014/collection/runs/cand_cv/checkpoints/, .../collection/scripts/
#     train_sex.py, .../collection/public_data/sex_predictions.json. Without it the readings still give species.
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
if [ -n "${SEX_SRC:-}" ]; then
  for f in yolo_wings_best.pt train_sex.py sex_predictions.json; do test -f "$SEX_SRC/$f" || { echo "Missing $SEX_SRC/$f" >&2; exit 1; }; done
  check "$SEX_SRC/yolo_wings_best.pt" 89de549bfd0cc3316302cdaed45b751adb97c4b6cb5c83b5857c3c2703e35ec6
  check "$SEX_SRC/train_sex.py" 8624e713a231fbe3e8084dab207ecc9fd4b4026e102af2eb318be65b40fc110f
  [ "$(cat "$SEX_SRC"/cand_cv/seed*_fold*.pt 2>/dev/null | sha256sum | cut -d' ' -f1)" = d92233d71d3ba86e7c1740a29fa216e153c16f5956b993e287f4b641218c348a ] ||
    { echo "$SEX_SRC/cand_cv: not the 15 heads of release_20261014 cand_cv (SHA-256 of them in name order)" >&2; exit 1; }
  mkdir -p "$dest/sex/cand_cv"
  cp "$SEX_SRC/yolo_wings_best.pt" "$SEX_SRC/train_sex.py" "$SEX_SRC/sex_predictions.json" "$dest/sex/"
  cp "$SEX_SRC"/cand_cv/seed*_fold*.pt "$dest/sex/cand_cv/"
fi
find "$dest" -name __pycache__ -prune -exec rm -rf {} +
du -sh "$dest"

photo=$(ls "$src"/photos/*.jpg 2>/dev/null | head -1 || true)
if [ -n "$photo" ]; then
  scratch=$(mktemp -d)
  echo "{\"id\":\"1\",\"path\":\"$photo\",\"side\":\"East\"}" |
    YOLO_CONFIG_DIR=$scratch MPLCONFIGDIR=$scratch YOLO_OFFLINE=1 nice -n 10 "$dest/venv/bin/python" -I -B "$here/worker.py" --dir "$dest" --idle 1 2>"$scratch/err" | tail -n 1
  if [ -d "$dest/sex/cand_cv" ]; then
    # The readings' op: the photo as both views (its sex model answers, or says why it could not).
    echo "{\"id\":\"2\",\"op\":\"specimen\",\"photos\":[{\"key\":\"a\",\"path\":\"$photo\"}],\"side\":\"East\",\"ecuador\":true,\"sex\":{\"cam\":\"CAM000000\",\"d\":\"a\",\"v\":\"a\",\"species\":null}}" |
      YOLO_CONFIG_DIR=$scratch MPLCONFIGDIR=$scratch YOLO_OFFLINE=1 nice -n 19 "$dest/venv/bin/python" -I -B "$here/worker.py" --dir "$dest" --threads 2 --nice 19 --idle 1 2>"$scratch/err" | tail -n 1 | cut -c1-400
  fi
  rm -rf "$scratch"
fi
