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
#     copied into <dest>/sex: the 19 Oct 2026 CNN (release_20261019_sexcnn cnn_strips4), a folder with model.json,
#     cand_cv/seed*_fold*.onnx (MobileNetV3-Large, 5 folds x 3 seeds, on the 4 androconial strips cut with the nano
#     wing chain's boxes: each dorsal hindwing's costal half and each ventral forewing's inner-margin half; no detector,
#     no encoder) and sex_predictions.json (its out-of-fold evidence for the supported screen, and the folds):
#     WingsClassificator reports/release_20261019_sexcnn/sex-model/. Without it the readings still give species.
#
# Then a photo is identified once to check it (about 10 s), at low priority.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
dest=${1:-/home/ubuntu/ithomiini/shared/identifier}
src=${2:-/home/ubuntu/bioclip-bench}
encoder=onnx/bioclip25h_visual_int8.onnx

for f in venv/bin/python app/v6_feature.py app/scripts/wings_v6 app/assets/head_hier_b8127dbf.pt app/assets/wing_seg_v5n.pt app/assets/wing_seg_v5n.onnx app/assets/field_wingtip_override_v1.json "$encoder"; do
  test -e "$src/$f" || { echo "Missing $src/$f" >&2; exit 1; }
done
check() { [ "$(sha256sum "$1" | cut -d' ' -f1)" = "$2" ] || { echo "$1: not the expected file (SHA-256)" >&2; exit 1; }; }
check "$src/app/assets/head_hier_b8127dbf.pt" b8127dbf0591630c99a9e4af1c00eb8d50f217acfb795fe9e23093702e816d42
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
sha() { sha256sum "$1" | cut -d' ' -f1; }
if [ -n "${SEX_SRC:-}" ] && [ -f "$SEX_SRC/model.json" ]; then
  test -f "$SEX_SRC/sex_predictions.json" || { echo "Missing $SEX_SRC/sex_predictions.json" >&2; exit 1; }
  # the 15 networks (SHA-256 of them in name order), model.json and the evidence belong together
  nets=$(cat "$SEX_SRC"/cand_cv/seed*_fold*.onnx 2>/dev/null | sha256sum | cut -d' ' -f1)
  case "$nets:$(sha "$SEX_SRC/model.json"):$(sha "$SEX_SRC/sex_predictions.json")" in
    59463d268b3fb3bb837db11b36391a6c7c1aae890a71343abfc8e913e34e8382:ca1df44054ecbfeb581540cbc0547cc9ab6effc03c8311d2dce9cd8974c10653:dcf726d7263353c6421de7fc4097c4e488b58da5b36ebf1b78734143abd34431) echo "sex model: release_20261019_sexcnn cnn_strips4 (the 4 androconial strips, MobileNetV3, nano wing boxes)" ;;
    *) echo "$SEX_SRC: not a known sex model (networks, model.json and sex_predictions.json of the same release)" >&2; exit 1 ;;
  esac
  rm -rf "$dest/sex"
  mkdir -p "$dest/sex/cand_cv"
  cp "$SEX_SRC/model.json" "$SEX_SRC/sex_predictions.json" "$dest/sex/"
  cp "$SEX_SRC"/cand_cv/seed*_fold*.onnx "$dest/sex/cand_cv/"
elif [ -n "${SEX_SRC:-}" ]; then
  echo "$SEX_SRC: no model.json (the sex model is the 19 Oct 2026 CNN: reports/release_20261019_sexcnn/sex-model)" >&2
  exit 1
fi
find "$dest" -name __pycache__ -prune -exec rm -rf {} +
du -sh "$dest"

photo=$(ls "$src"/photos/*.jpg 2>/dev/null | head -1 || true)
if [ -n "$photo" ]; then
  scratch=$(mktemp -d)
  echo "{\"id\":\"1\",\"path\":\"$photo\",\"side\":\"East\"}" |
    YOLO_CONFIG_DIR=$scratch MPLCONFIGDIR=$scratch YOLO_OFFLINE=1 nice -n 10 "$dest/venv/bin/python" -I -B "$here/worker.py" --dir "$dest" --idle 1 2>"$scratch/err" | tail -n 1
  if [ -d "$dest/sex/cand_cv" ]; then
    # The readings' op: the photo as both views, its quarters as the four wings (its sex model answers, or says why
    # it could not).
    q='[{"box":[0.02,0.02,0.48,0.48],"kind":"forewing","half":"left","score":1},{"box":[0.52,0.02,0.98,0.48],"kind":"forewing","half":"right","score":1},{"box":[0.02,0.52,0.48,0.98],"kind":"hindwing","half":"left","score":1},{"box":[0.52,0.52,0.98,0.98],"kind":"hindwing","half":"right","score":1}]'
    echo "{\"id\":\"2\",\"op\":\"specimen\",\"photos\":[{\"key\":\"a\",\"path\":\"$photo\"}],\"side\":\"East\",\"ecuador\":true,\"sex\":{\"cam\":\"CAM000000\",\"d\":\"a\",\"v\":\"a\",\"species\":null,\"wings\":{\"d\":{\"wings\":$q,\"turnK\":0},\"v\":{\"wings\":$q,\"turnK\":0}}}}" |
      YOLO_CONFIG_DIR=$scratch MPLCONFIGDIR=$scratch YOLO_OFFLINE=1 nice -n 19 "$dest/venv/bin/python" -I -B "$here/worker.py" --dir "$dest" --threads 2 --nice 19 --idle 1 2>"$scratch/err" | tail -n 1 | cut -c1-400
  fi
  rm -rf "$scratch"
fi
