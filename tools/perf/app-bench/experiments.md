# Experiments (Phase 2, from main a6ad89e)

One change each, written here before it is measured. Each is run with `run.mjs` (3 repeats, the same
workloads) against the baseline of the same commit, and kept only when its metric improves beyond the
baseline's min–max range, no other key metric gets worse by more than its noise (the larger of 10 % and the
baseline's range), the behaviour fingerprint (search matches and digests, tool answers) is unchanged, and
`npm test` passes. Baseline: `base-a6ad89e` (the merged main, run through `tools/lab/dev-app.sh`).

Runs (folders under `~/.cache/ithomiini-perf/` on the PC, `/home/ubuntu/perf-bench/results/` on claudeclaw): `claw-round2`
(base, E1, E2, E3, E4, combo; 3 interleaved repeats each, 7 GB ceiling: the 5 GB of `claw-round1` was reached and
killed the app in its tools phase), `local-combo-*` (base, combo on the PC), `e1b-pool-*` (combo, E1b; search and
concurrent only), `claw-final` (base, final). `compare.mjs <folder>` prints each.

| id | change | metric it should move | status |
|---|---|---|---|
| E1 | `/api/search` in its own worker thread (stopped when idle), the main thread only waits | event loop max/p95 in `search` and `concurrent`; concurrent p95 | **kept**: search-phase event loop 572 → 5 ms (server), concurrent loop p95 −90 %, throughput +26 %; but searches queue in the one worker (concurrent p95 +17 %), hence E1b |
| E1b | two search workers (the second only while the first is busy); `SEARCH_WORKERS`, 1 in the lab | concurrent p95, search latency under 5 clients | **kept** for the server: 5 clients searching, throughput +88 %, p95 −37 %; costs ~300 MB while the second runs (+11.5 % peak, over the 10 % rule: a choice, the server has 23 GB; the lab keeps 1) |
| E1c | E1b plus `maxOldGenerationSizeMb: 256` for the search workers | search worker RSS | **dropped** before a run: in a worker on the lab copy, heap 166 → 174 MB and RSS 347 → 368 MB with the cap (no gain) |
| E2 | sync (lab): LocalSheets.readSheet copies and hashes a sheet 500 rows at a time with a turn for others in between (same rows, same digest) | sync event-loop max | **kept**: sync event loop max 2981 → 179 ms (server), 1632 → 114 ms (PC) |
| E3 | big answers compressed with the async zlib calls instead of `brotliCompressSync`/`gzipSync` | event loop max in `pages` and `concurrent` | **dropped**: search-phase loop p95 −23 % but concurrent p95 +21 % on the server (the 4 libuv threads queue the big answers); pages unchanged |
| E4 | the rack reader's `enlarge`: made once per size for both binarizers, its column weights once per size (the same pixels) | `tools.rack_read_photos.*_ms` | **kept**: 13.2 → 8.4 s a rack photo (server), 6.4 → 4.5 s (PC); the same 94 tubes |
| E5 | LOCAL_MODE: the in-memory workbook (LocalSheets rows) kept in SQLite instead | anon memory idle/peak, start time | deferred on 1 Nov (budget); run on 7 Nov, see "E5, 7 Nov" below |

## E5, deferred: the lab's workbook in SQLite

What it would save, from the 1 Nov profiles: LocalSheets keeps every seed row in Google's cell format
(`normalizeSeedRow`, ~600 MB live, plus ~120 MB of copies), and each start parses the 356 MB snapshot (the
start's peak, ~2 GB with the 2 GB heap of dev-app.sh, and ~15 s of CPU). Kept in a SQLite file instead, a lab app
would hold about 700 MB less and start without the parse.

Design: the seed parsed once per snapshot into `<snapshot>.sheets.sqlite` (one row per sheet row: its cells as
JSON), copied (copy-on-write) for each start, so a restart still undoes everything; `LocalSheets` keeps its API
with a row store behind `this.rows` (get a row, put a row, a sheet's rows in order, shift rows for
insert/delete dimension), the in-memory store kept for the tests' seeds (144 `new LocalSheets` in tests). About
15 methods touch `this.rows`; the formula emulation (`evaluate`, copy/paste of cells) reads cells one at a time.

Not run in this round: it changes the lab's data path end to end, and the week's budget for this work (5 %) did not
leave room for it and its review after E1–E4. Worth doing next if lab apps on this PC still matter.

## E5, 7 Nov: the plan, written before it is measured

From 7048d726 (the release on claudeclaw). Pass bar, set before the run: server RSS of one lab app down by at
least 400 MB on the bench, no endpoint more than 10 % slower at p50, the same answers on the bench's checks
(compare.mjs), `npm test` and the frontend tests passing.

- **Can the query copy serve it?** No: `server/replica.mjs`'s `sheets.sqlite` is made from the store's records
  (values only), while LocalSheets answers in Google's cell format (formulas, effective values, number formats,
  validation) and the app writes to it. A sibling file of its own is needed.
- **E5a, the change.** With `SEED_FILE` (the lab, the bench), `FileSheets` (a LocalSheets whose rows live in a
  SQLite file) replaces the in-memory rows. The snapshot is parsed once, in a child process, into
  `<snapshot>.rows.sqlite` (one row per sheet row: its row number, its cells as `v8.serialize`, which gives back
  exactly what `structuredClone` gives, holes included), rebuilt when the snapshot's size or time changes; each
  start copies it (copy-on-write where the disk can) beside `DATABASE_PATH`, so a restart still undoes
  everything. A write loads the rows it touches, changes them as today's code does and writes them back before it
  returns (no await in between, as now); an inserted or deleted row shifts the row numbers in SQL and rewrites the
  formulas that move. Reads in pieces of 500 rows as E2. Tests keep the in-memory rows (they reach into
  `sheets.rows`); a new test runs the same operations on both and compares every answer.
- **Expected:** −600 to −700 MB live heap (1 Nov heap profile: `normalizeSeedRow` 593 MB + 121 MB), no 356 MB
  `JSON.parse` at each start (faster start, lower start peak); sync about as fast (deserialising instead of
  cloning). Risk: an inserted row in Insectary_data rewrites every row with a formula (SQL instead of memory).
- **Way back:** `LOCAL_SHEETS=memory` keeps today's in-memory rows without a code change.

## Review (dual-critic-loop, 2 rounds, GPT-6.1 Sol + fresh Opus, on the combined diff a6ad89e..perf-combo)

Grades: Sol C → A, Opus A− → A−.

| round | critic | id | severity | outcome | reason |
|---|---|---|---|---|---|
| 1 | sol | F1 | blocker | fixed | E2's read in pieces could mix two moments (a write or a deleted row between pieces): a write counter, the copy started again when it moved, after three the whole sheet at once; tests/local-sheets-read.test.mjs fails without it |
| 1 | opus | F1 | nit | fixed | a search while the app closes got a 500: answered in the app's thread while the store is open |
| 1 | opus | F2 | nit | declined (comment) | after a 60 s worker timeout the search runs in the app; searches take 0.1–0.5 s |
| 1 | opus | F3 | nit | fixed | same as sol F1 |
| 1 | opus | F4 | nit | declined | dev-app.sh `${EXTRA[@]}` under bash < 4.4; the lab PC runs bash 5 |
| 2 | sol | – | – | – | all round-1 ids resolved or declined, no new findings |
| 2 | opus | F1 | nit | fixed | a test of the whole-sheet copy after three interrupted reads |
| 2 | opus | F2 | nit | fixed | a test of the search answered in the app after the worker closed |
| E1b, 1 | sol | – | – | – | grade A, no findings |
| E1b, 1 | opus | F1 | nit | fixed | `SEARCH_WORKERS=0` gave 2: now at least 1, as the assistant's setting |
| E1b, 1 | opus | F2 | nit | fixed | the comment: two searches at once can be one person typing fast |
| E1b, 1 | opus | F3 | nit | fixed | `LAB_SEARCH_WORKERS` in tools/lab/README.md |
| E1b, 1 | opus | F4 | nit | fixed | `SEARCH_WORKERS` in deploy/service.env.example |
| E1b, 1 | opus | F5 | nit | fixed | the one-worker case checks the answers too |
