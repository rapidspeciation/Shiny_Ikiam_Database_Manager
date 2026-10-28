# Deploy plan: integration-20261028 (not deployed, not pushed)

1. What goes live: the live release 20261028T210959Z (rack-reader 640e963) plus assistant-maps-gallery (maps, host plants and Wings Gallery tools and skill, read-only), envelope-namefix (envelope reader round 4: words snapped to the workbook's names) and fotos-sex-check (Fotos tab «Comprobar N mariposas», POST /api/sex-check, worker.py sex op without torch). envelope-round5 is not included. No newer release was on claudeclaw on 28 Oct (checked just before this plan).
2. Checked here: server tests 738/738, frontend tests 659/659, syntax and typecheck (scripts/check.mjs), vite build: all pass.
3. First, push rename_photos_AI fbf23ad (rename-app.commit points at it; it is only local now):
   git -C /home/franz/Documents/CodeProjs/rename_photos_AI-sexcheck push origin fotos-sex-check
4. Deploy (from this PC; runs all tests again, then ships over ssh to claudeclaw):
   bash /home/franz/Documents/CodeProjs/ithomiini_database-integ1028/scripts/deploy.sh
5. Check: curl -s https://ithomiini-ikiam.com/health ("status":"ok"), then the Fotos tab loads its app and the sex check button shows.
6. Rollback (previous-release will then hold releases/20261028T210959Z):
   ssh claudeclaw 'set -eu; cd /home/ubuntu/ithomiini; ln -sfn "$(cat shared/previous-release)" current; systemctl --user restart ithomiini.service'
   then refresh the T3 skills (drops maps-gallery): ssh claudeclaw 'cd /home/ubuntu/ithomiini/current && node=$(sed -n "s/^ExecStart=\([^ ]*\/node\) .*/\1/p" deploy/ithomiini.service) && "$node" scripts/t3-provision.mjs --refresh-all'
7. Risks: if step 3 is skipped the deploy still goes on but the Fotos tab says the app is not installed (build-rename-app.sh cannot fetch fbf23ad). shared/rename-app is rebuilt in place for fbf23ad and is not undone by a rollback (the change is additive: one more embed message). worker.py's sex model now runs without torch for a model of the strips' own size; the background readings use the same code. The maps tools fetch the maps' data files (about 10 MB per tools worker) and Open-Meteo. deploy.sh runs the test suites in parallel on this 14 GB PC: close heavy apps first. No database schema change. Afterwards: merge the branch into main and push, so claudeclaw's src checkout and the team repo match the release.
