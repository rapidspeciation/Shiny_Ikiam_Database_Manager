# Experiments (Phase 2, from main a6ad89e)

One change each, written here before it is measured. Each is run with `run.mjs` (3 repeats, the same
workloads) against the baseline of the same commit, and kept only when its metric improves beyond the
baseline's min–max range, no other key metric gets worse by more than its noise (the larger of 10 % and the
baseline's range), the behaviour fingerprint (search matches and digests, tool answers) is unchanged, and
`npm test` passes. Baseline: `base-a6ad89e` (the merged main, run through `tools/lab/dev-app.sh`).

| id | change | metric it should move | status |
|---|---|---|---|
| E1 | `/api/search` in its own worker thread (stopped when idle), the main thread only waits | event loop max/p95 in `search` and `concurrent`; concurrent p95 | registered |
| E2 | sync (lab): LocalSheets.readSheet copies and hashes a sheet 500 rows at a time with a turn for others in between (same rows, same digest) | sync event-loop max | registered |
| E3 | big answers compressed with the async zlib calls instead of `brotliCompressSync`/`gzipSync` | event loop max in `pages` and `concurrent` | registered |
| E4 | the rack reader's `enlarge`: made once per size for both binarizers, its column weights once per size (the same pixels) | `tools.rack_read_photos.*_ms` | registered |
| E5 | LOCAL_MODE: the in-memory workbook (LocalSheets rows) kept in SQLite instead | anon memory idle/peak, start time | deferred (budget), see below |

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
