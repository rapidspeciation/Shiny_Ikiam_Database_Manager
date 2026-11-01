# App benchmark: memory and speed on fixed workloads

`run.mjs` starts the lab's app (LOCAL_MODE, as `tools/lab/app.sh` does, but on its own port 8805 and T3 proxy
3786) from a fresh copy of the lab database, runs the workloads below, stops it, and repeats. `report.mjs` turns
the results into a table, or two runs into a before/after table.

```sh
node tools/perf/app-bench/run.mjs --label baseline --repeats 3 --profile
node tools/perf/app-bench/report.mjs ~/.cache/ithomiini-perf/baseline-<stamp>/results.json
node tools/perf/app-bench/report.mjs <after>/results.json --vs <before>/results.json
```

Workloads (each repeat): cold start (spawn → listening → first sync, then 20 s idle); the tabs in Chromium (Inicio,
Buscador, Colecta, Monitoreo, Fotos, Asistente, Inicio again); `/api/search` for 8 queries, each twice; a forced
sheet sync; the assistant's tools over MCP (`host_plants` with an area and a contact sheet, `maps_snapshot`,
`gallery_specimens`, `identify_species`, `rack_read_photos`; each twice); the offline page's model download; 5
clients replaying the tabs' API calls for 30 s.

Metrics per phase: peak and end RSS and swap of each process of the app (server, identifier, ffmpeg…), sampled
every 250 ms; CPU seconds per process and per server thread name (main thread, workers, GC); the delay of a
`/health` asked every 200 ms (how late the event loop is); per action the latency and the payload size.
`--profile` adds one start with `--cpu-prof` and `--heap-prof`, summarised in the results.

Rules it keeps: it refuses to start the app or the browser when MemAvailable is under 2.5 GB, and the identifier
when under 3.5 GB; one app and one browser at a time; nothing of the lab is written except two photos named
`perf-bench-*.jpg` in the lab T3's attachments folder (where the tools read chat photos from). The run folder
(`~/.cache/ithomiini-perf/<label>-<stamp>`) holds the database copies, the logs, the samples and the results;
the database copies are copy-on-write, so they cost little disk until written.
