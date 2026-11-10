# Small lab copies (9–10 Nov 2026)

Branch `lab-small-copies-20261110`, from team/main 49734237. Lab-only code: it changes only apps started with
`LOCAL_MODE=1` (tools/lab/dev-app.sh, tools/lab/app.sh, the perf bench). Not deployed to claudeclaw; it goes out
with the next deploy, where nothing changes for the live app.

## Problem

Swap on this PC filled up (13.3 of 14.9 GB) from parallel LOCAL_MODE copies of the app. Each copy ran
`JSON.parse` on the whole 372 MB snapshot (`server/index.mjs`, `SEED_FILE`) and kept it in `LocalSheets` for its
lifetime. After the start it also held about 1 GB of V8 garbage from the first sync, plus about 0.5 GB in the
checks' worker thread. That came to 2.3 GB per copy.

## Numbers (this PC, load average 15–20 from other work)

VmRSS + VmSwap of the server's main pid, 60 s after its unit started (`measure.py`, run JSONs in `runs-*/`):

| | before (rows in memory) | after (HEAD) |
|---|---:|---:|
| one copy, 60 s after its start | **2317 MB** (`runs-20261109T205546/before-1.json`) | **187 MB** (`runs-final-20261109T215226/final-1.json`); 204 MB on the last commit (`final-1-sqlitelock.json`) |
| one copy's start (unit start until its first sync is done) | 50 s | 17–24 s |
| 3 copies started back to back, each 60 s after its start | not run (see below) | **198, 188, 183 MB** (`final-3.json`) |
| 3 copies: own start / ready after each dev-app.sh call | — | 29–30 s / 30, 40, 40 s |
| peak during a start (VmHWM) | 1975 MB at 60 s and still rising | 0.9–1.3 GB |

- I did not run 3 old copies at once. They would have added about 7 GB, with swap already at 69 % and
  systemd-oomd killing at 90 % (it took the whole T3 on 31 Oct). The single-copy figure times three (about 7 GB)
  and Franz's own measurement (4 copies came to 4 GB in RAM and swap) stand in for it.
- Intermediate steps, one copy at 60 s:
  - the earlier perf branch alone (rows in SQLite): 1277 MB, settling to about 310 MB only after about 2.5 min;
  - plus the GC after the start: 486–1018 MB;
  - plus releasing the checks' worker: 195 MB.

## How it works

1. **Rows file** (from the unmerged perf/e5-workbook-sqlite-20261107 branch, cherry-picked): `server/file-sheets.mjs`
   `FileSheets` is a `LocalSheets` whose rows live in SQLite. The snapshot is parsed once, in a child process,
   into `~/.cache/ithomiini-lab/snapshot.json.<code hash>.rows.sqlite`. It is keyed by the snapshot's size and
   mtime and by a hash of the code that shapes the rows, and made again when either changes. Copies starting
   together make it once: they hold an exclusive SQLite transaction on `<rows file>.lock`, which the system lets
   go of when the holder ends. Each copy copies the file to its own folder (`workbook-rows.sqlite`, copy-on-write
   on btrfs, so free) and writes only there. The shared file never changes, and a restart undoes every write.
   `LocalSheets` reaches its rows through a few methods, so in memory (the tests) it does exactly what it did.
2. **GC after the start** (`server/index.mjs`, only when `LOCAL_MODE` and node runs with `--expose-gc`, which only
   dev-app.sh passes): once after the first sync, and once after the checks' warm-up (Inicio's summary, alerts,
   issues, suggested edits). It calls `gc({ type: 'major', execution: 'sync', flavor: 'last-resort' })`. A plain
   GC leaves the freed pages pooled for minutes; this one gives them back at once (0.1–0.3 s).
3. **Checks worker released** after the warm-up (`workerSlot.release()`, same condition). It had kept what the
   warm-up read, about 0.5 GB. The next check starts a new worker, which takes a second or two. Other idle workers
   still stop after 5 min.
4. **dev-app.sh:** no limit on how many copies (`DEV_APP_LIMIT` only when set). The start lock is held until the
   copy says `"settled":true` on `/health` (LOCAL_MODE only), at most 3 min, by a holder in the background. So no
   two starts overlap, and the script still answers right after the first sync. Other starts wait up to 30 min.
   `MemoryMax=3G` and `MemorySwapMax=1G` per copy stay as the safety net, and so does the 2.5 GB MemAvailable
   check.

## Checks

- Server suite (`node scripts/test-server.mjs`, no-AI guard): 1066 pass, 0 fail, 4 skipped on the final commit.
  Two earlier runs failed `tests/maps-render-deadline.test.mjs` once each. That test has a 2.5 s deadline and the
  load average was 15–20; it passes alone on this branch and on team/main, and on 3 later full runs. No maps code
  is touched.
- Frontend suite: 801/801.
- New tests:
  - `tests/file-sheets.test.mjs`: copies side by side share the rows file, it is made once, and each keeps its own
    writes. The build lock: no overlap, waiting, a killed holder lets go.
  - Existing: the rows file is made again when the snapshot changes.
  - `tests/worker-slot.test.mjs`: release.
  - `tests/checks-worker.test.mjs`: a lab copy runs the GC, stops the worker and says settled; without gc nothing
    changes.
- sol-loop: GPT-6.1 Sol (medium) and Opus 5.5 (high, claude-api-credits), 3 rounds (ledger below).

## Roll back

- One copy, no code change: `tools/lab/dev-app.sh --env LOCAL_SHEETS=memory` keeps the rows in memory as before.
  `--env NODE_OPTIONS=--max-old-space-size=2048` (without `--expose-gc`) turns off the GC and the worker release.
- Lasting: `git revert` the branch's commits (`git log 49734237..<main after merge>`), then delete
  `~/.cache/ithomiini-lab/snapshot.json.*.rows.sqlite*` and the copies' `workbook-rows.sqlite`. The live app
  (GoogleSheets) is unaffected either way.

## Left over

- `~/.cache/ithomiini-lab/snapshot.json.cc73f2c16abf4b9d.rows.sqlite` (442 MB) is the perf branch's code version.
  Delete it once nobody runs that branch. The current one is `snapshot.json.987114327472ef29.rows.sqlite`, made in advance (14 s).
- Memory while a copy is in use (forced syncs, assistant tools, the identifier) is unchanged and out of scope.

## Review ledger (sol-loop)

| Critic | Round 1 (whole change) | Round 2 (fixes) | Round 3 (build lock) |
|---|---|---|---|
| GPT Sol | 9/10 | 8/10 | 10/10 |
| Opus | 8/10 | 9/10 | 9/10 |

- Round 1:
  - Fixed: docs out of date (both critics); stale build-lock takeover (both); a test of `/health` settled; the 15 s
    worker idle was not needed once `release()` existed (measured 213 MB with 5 min, so it was dropped); the start
    lock waits 30 min instead of 10; `ensure` clears half-made files of killed builds.
- Round 2:
  - Fixed: Sol F7 (should). The pid-file lock's stale takeover could still let two builders in. It was replaced
    by an exclusive SQLite transaction, which the OS releases when the holder dies.
  - Fixed: Opus F7 (the table had round-1 numbers).
- Round 3:
  - Fixed: comments on the `-journal` file and on plain opens of the lock file; the test kills its holder
    processes on failure.
  - Declined (nits): a clearer error for a non-SQLite `.lock` (no real start can reach one, since the code hash
    changed the path), and imports the test may no longer use.
- Usage: not exposed.
