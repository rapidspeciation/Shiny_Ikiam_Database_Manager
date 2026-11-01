# Experiments (Phase 2, from main a6ad89e)

One change each, written here before it is measured. Each is run with `run.mjs` (3 repeats, the same
workloads) against the baseline of the same commit, and kept only when its metric improves beyond the
baseline's min–max range, no other key metric gets worse by more than its noise (the larger of 10 % and the
baseline's range), the behaviour fingerprint (search matches and digests, tool answers) is unchanged, and
`npm test` passes. Baseline: `base-a6ad89e` (the merged main, run through `tools/lab/dev-app.sh`).

| id | change | metric it should move | status |
|---|---|---|---|
| E1 | `/api/search` in its own worker thread (stopped when idle), the main thread only waits | event loop max/p95 in `search` and `concurrent`; concurrent p95 | registered |
| E2 | sync: no `structuredClone` + hash of each sheet in LocalSheets.readSheet; in the store, the sheet hashed without building one string | `sync.ms`, sync event-loop max | registered |
| E3 | big answers compressed with the async zlib calls instead of `brotliCompressSync`/`gzipSync` | event loop max in `pages` and `concurrent` | registered |
| E4 | the rack reader's `enlarge` done by ffmpeg (or a faster loop) instead of per-pixel JS | `tools.rack_read_photos.*_ms` | registered |
| E5 | LOCAL_MODE: the in-memory workbook (LocalSheets rows) kept in SQLite instead | anon memory idle/peak, start time | registered (only if E1–E4 leave time) |
