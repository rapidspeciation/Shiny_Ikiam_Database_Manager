# Deployed: experimental sex estimate on Identificar, team only (8 Nov 2026, W15 of the field-sex lane)

Franz approved (8 Nov). Rules and numbers: WingsClassificator `reports/field_sex_plan_20261104/PLAN.md` Amendment 8
(2fb7e641) and `reports/field_sex_explore_20261106/INAPP.md`.

- Release **20261108T013837Z** (origin main 2f703343: f3afd022, b37cd32c, fbee999a, 2f703343 on top of 9e317585).
  Previous: 20261108T011852Z. deploy.sh: server tests 1001 pass, frontend 794 pass, type check, build; health ok.
  Deploy lock taken and released (`deploy.lock.released-<UTC>`).
- Server file (new, nothing overwritten): `shared/identifier/offline/sex_v2s_ft_full_fp16.onnx`, 43,409,800 bytes,
  sha256 d6d5fb9d..., parts 2b96e1ab / 9f4a2f50 (pinned in `frontend/src/lib/sexModel.ts` and `server/offline-id.mjs`
  SEX). Sent plain on the signed-in `/prueba-offline/models/` route only; not on Hugging Face (training composites
  were built on iNaturalist template photos, so not clearly licence-clean for a public release).
- Constants: cutoff 2.9805 logit (P 0.952), no seguro 1.6157 to 4.8722 (P 0.834 to 0.992). Shown only for the 13
  genera of the training composites; other Ithomiini «no aplica»; nothing for the rest; nothing signed out.
- Live checks: signed out, list.json, the file, each part and a range answer 401; the Identificar page loads with no
  error, no request for the file and no block. Signed in: the release's own route code, run on the server with a
  signed-in user against the live identifier folder, lists the file with the pinned sha256 and serves both parts with
  the pinned hashes. The signed-in page itself was checked end to end on the lab with the same build (download,
  hembra / macho / no seguro, online mode, «no aplica», removal on sign-out); no team login was available for a live
  signed-in browser check.

Rollback:
- code: `ssh claudeclaw 'cd /home/ubuntu/ithomiini && ln -sfn releases/20261108T011852Z current && systemctl --user restart ithomiini.service'`
  then `ssh claudeclaw 'cd /home/ubuntu/ithomiini/current && node=$(sed -n "s/^ExecStart=\([^ ]*\/node\) .*/\1/p" deploy/ithomiini.service) && "$node" scripts/t3-provision.mjs --refresh-all'`
- the model file can stay (the previous release does not list it); to remove it: `ssh claudeclaw 'mv /home/ubuntu/ithomiini/shared/identifier/offline/sex_v2s_ft_full_fp16.onnx /home/ubuntu/ithomiini/shared/identifier/sex_v2s_ft_full_fp16.onnx.removed'`
- lasting: `git revert 2f703343 fbee999a b37cd32c f3afd022` on main, then `scripts/deploy.sh`.
