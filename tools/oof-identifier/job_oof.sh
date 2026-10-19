#!/usr/bin/env bash
# gallery_readings_20261019 OOF identifier heads: LSF array 1-10 (1-5 single folds 0-4, 6-10 paired folds 0-4).
# EPOCHS>0 = smoke (writes to smoke/). Inputs read-only; outputs published atomically to $G/out/<mode>_f<fold>/.
set -Eeuo pipefail
R=/lustre/scratch125/tol/teams/meier/users/fv3/insect_id
E=/software/treeoflife/conda/users/envs/team347/fv3/insectid
X=$R/runs/wings_autoresearch_identifier_20261013
S=$R/runs/photo_readings_20261018
G=$R/runs/gallery_readings_20261019/oof
I=$((LSB_JOBINDEX-1)); FOLD=$((I % 5)); MODE=single; [ "$I" -ge 5 ] && MODE=paired
EP=${EPOCHS:-0}
DEST=$G/out/${MODE}_f$FOLD; [ "$EP" != 0 ] && DEST=$G/smoke/${MODE}_f$FOLD
[ -f "$DEST/receipt.json" ] && { echo "done already: $DEST"; exit 0; }
NODE="/tmp/fv3_oof_${LSB_JOBID:?no LSF job}_${I}"
case "$(findmnt -no FSTYPE -T /tmp)" in lustre|nfs|nfs4|gpfs|beegfs) echo "shared /tmp" >&2; exit 1;; esac
trap 'rm -rf "$NODE"' EXIT
mkdir -p "$NODE/data_v1/extra"
nvidia-smi --query-gpu=name,memory.total,memory.used --format=csv
cp $X/data_v1/X.npy $X/data_v1/arrays.npz $X/data_v1/meta.json "$NODE/data_v1/"
cp $X/data_v1/extra/topup.npz "$NODE/data_v1/extra/"
tar -xf $S/readings_code.tar -C "$NODE" code/code/space/geo_prior.py code/assets/region_checklist.json
cp $R/runs/wingseg_v5_field_20261017/data/head_hier_deployed.pt "$NODE/head_hier.pt"
cp $S/out/ident_features.npy "$NODE/"
( cd "$NODE" && sha256sum head_hier.pt code/assets/region_checklist.json data_v1/extra/topup.npz | cut -c1-16 )
( cd $X/code && sha256sum hx_data.py | cut -c1-16 )
export PYTHONNOUSERSITE=1 OMP_NUM_THREADS=4
"$E/bin/python" $G/code/oof_fold.py --data "$NODE/data_v1" --hx-code $X/code --train-head $G/code/train_head_429b7afcf652.py \
  --space-code "$NODE/code/code/space" --checklist "$NODE/code/assets/region_checklist.json" --head-hier "$NODE/head_hier.pt" \
  --plan $G/code/oof_plan.json --feats "$NODE/ident_features.npy" --fold $FOLD --mode $MODE --epochs $EP --out "$NODE/out"
mkdir -p "$(dirname "$DEST")"; rm -rf "$DEST.partial"; cp -r "$NODE/out" "$DEST.partial"; mv "$DEST.partial" "$DEST"
echo OOF_DONE $MODE $FOLD
