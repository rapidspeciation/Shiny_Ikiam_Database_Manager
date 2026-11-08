# Deploy note: the lab's workbook rows in a SQLite file (perf E5, 7–8 Nov 2026)

Not deployed, not pushed. Branch `perf/e5-workbook-sqlite-20261107`, based on the live commit 7048d726. Bench and
review: `/home/franz/Documents/CodeProjs/WingsClassificator/reports/pending_scout_20261107/perf-e5/REPORT.md`.

What changes: only an app started with `LOCAL_MODE=1` and `SEED_FILE` (the lab: `tools/lab/dev-app.sh`,
`tools/lab/app.sh`, the perf bench). Production (`GoogleSheets`) runs the same code paths as before; `server/index.mjs`
only imports `server/file-sheets.mjs`. Tests keep the in-memory rows.

- `server/file-sheets.mjs` (new): `FileSheets`, a `LocalSheets` whose rows live in a SQLite file. The first start
  after a new snapshot (or a change to `server/sheets.mjs`, `server/schema.mjs`, `docs/workbook-schema.json` or
  `server/file-sheets.mjs`) makes `<snapshot>.rows.sqlite` beside the snapshot in a child process: about 15–18 s and
  1.9 GB peak in that child (the lab's 5 Oct snapshot: 107,437 rows, 442 MB file). Every start copies it to
  `workbook-rows.sqlite` beside `DATABASE_PATH` (copy-on-write on btrfs; a full 442 MB copy on ext4), so a restart
  still undoes all lab writes.
- `server/sheets.mjs`: `LocalSheets` reaches its rows through a few methods (`findRow`, `liveRow`, `addRow`,
  `commitRows`, `sheetNames`, `maxRow`, `maxColumns`); in memory they do what the old code did.
- `server/index.mjs`: `LOCAL_SHEETS` (`file` by default, `memory` = the old in-memory rows). `server/store.mjs`:
  `close()` closes the sheets too.
- Disk: about 440 MB beside the snapshot (once) and, on ext4, about 440 MB in each lab app's folder.
- Lab instances (8 Nov, lab tooling only, nothing on the server's live app): `tools/lab/dev-app.sh` picks a free port
  from 8821, runs up to `DEV_APP_LIMIT` (4) side by side, starts them one at a time (`<lab>/.start.lock`) and returns
  once the first sync is done; `tools/lab/INSTANCES.md` is the agents' guide. The rows file is now one per code
  version (`snapshot.json.<code>.rows.sqlite`, 442 MB each); old ones can be deleted. The perf bench follows the
  same limit and lock.

Deploy (orchestrator, with the lock `claudeclaw:/home/ubuntu/ithomiini/deploy.lock`):
- First rebase onto the newer of origin/main and team/main (the scout fixes release `release/scout-fixes-20261107`
  lands first) and run `npm test` again: the branch merges cleanly with e464bd6d, dfb70b4c and cf58fa4e (checked
  with `git merge-tree`).
- Merge to main, push to origin and team, `scripts/deploy.sh`. On the server nothing changes for the live app;
  check https://ithomiini-ikiam.com/health shows `"status":"ok"`.
- Lab apps on this PC pick it up at their next start from a checkout with the commits. Their first start makes the
  rows file (log line `Workbook rows file made from …`); later starts log nothing extra.

Rollback:
- Without a code change: start the lab app with `LOCAL_SHEETS=memory` (e.g. `tools/lab/dev-app.sh --env
  LOCAL_SHEETS=memory`): the snapshot is parsed into memory as before.
- Server release: `ssh claudeclaw 'cd /home/ubuntu/ithomiini && ln -sfn releases/<the release before> current &&
  systemctl --user restart ithomiini.service'`.
- Lasting: `git revert` of this branch's commits (`git log 7048d726..perf/e5-workbook-sqlite-20261107`), then delete
  `<snapshot>.rows.sqlite` and the lab apps' `workbook-rows.sqlite` files.
- If a rows file cannot be made or opened, the app logs `Workbook rows kept in memory (<reason>)` and runs as before.
