# Deployed: identifier head 6fa6ab78 and wing detector wing_seg_s2 (5 Nov 2026)

Decided by the Ithomiini lead overnight for Franz ("take decisions for me"); release criteria written before the
comparisons: WingsClassificator `reports/wingseg_next_20261103/CANDIDATE.md` (numbers, rules, rollback).

- Release **20261105T040435Z** (commit 3fc6082c): head_hier 6fa6ab78 (alias_w075 recipe retrained on V5N serving crops,
  label-only fallbacks dropped, M. messenoides subspecies fixes). Previous: 20261105T034320Z.
- Release **20261105T043321Z** (commit 511b6352): wing detector wing_seg_s2 (V5N retrained on mined photos) for the
  worker and the offline page. Previous: 20261105T040435Z.
- Both: origin and team main; deploy.sh tests server 991/991, frontend 772/772, type check, build; health ok; deploy
  lock taken and released (`deploy.lock.released-20261105T0407Z`, `-20261105T0435Z`).
- Server files (nothing overwritten): `shared/identifier/app/assets/head_hier_6fa6ab78.pt`, `wing_seg_s2.pt`,
  `wing_seg_s2.onnx` (also in `~/bioclip-bench/app/assets`); `shared/identifier/offline/head_6fa6ab78.json`,
  `head_6fa6ab78_fp16.bin`; `app/v6_feature.py` b16a801b with the s2 SHA added to DETECTORS (old file
  `v6_feature.py.bak-20261105`, both folders). HF: `fr4nzzch/butterfly-id-classifier` 4bb06f90 (offline head),
  `fr4nzzch/butterfly-wing-detector-onnx` 793b7de4 (wing_seg_s2.onnx + card).
- Smoke on the server (worker of the live release, 7 photos): all answered, crops found; CAM070903d (M. m. deceptus)
  -> M. m. deceptus 0.90; CAM072041d (M. m. messenoides) -> species right, subspecies M. m. phasianita (known miss).
- Not changed: HF Space (gallery), Rename Photos app (keeps V5N NMS), out-of-fold species files, offline student.

Rollback:
- detector only: `ssh claudeclaw 'cd /home/ubuntu/ithomiini && ln -sfn releases/20261105T040435Z current && systemctl --user restart ithomiini.service'`
- head and detector: `ssh claudeclaw 'cd /home/ubuntu/ithomiini && ln -sfn releases/20261105T034320Z current && systemctl --user restart ithomiini.service'`
- then `ssh claudeclaw 'cd /home/ubuntu/ithomiini/current && node=$(sed -n "s/^ExecStart=\([^ ]*\/node\) .*/\1/p" deploy/ithomiini.service) && "$node" scripts/t3-provision.mjs --refresh-all'`
- lasting: `git revert 511b6352 3fc6082c` on main and `scripts/deploy.sh`.
