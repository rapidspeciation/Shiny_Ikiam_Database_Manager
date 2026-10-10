# Local test lab: benchmarking models on notebook photos

A copy of the app and of T3 Code on one PC, to measure how well an AI model
(through T3 Code, the way the team uses it) transcribes notebook photos into
the workbook. Each model gets the same photos and the same prompt. Its
proposals are scored cell by cell against the rows as people corrected them
by hand in the workbook.

The lab never writes to the team's Google Sheet. The app runs with
`LOCAL_MODE=1`: the sheets are an in-memory copy, seeded from a read-only
snapshot, and no Google credentials reach it. It also never touches the other
T3 installs on the PC: it has its own home (`~/.t3-ithomiini-lab`) and ports
(app 8795, T3 3775, and the app's proxy of T3 3776).

Private files (snapshot, photos, cases, credentials, results) live in the lab
folder, `~/.cache/ithomiini-lab` (or `ITHOMIINI_LAB_DIR`), never in the
repository.

## Run

```sh
tools/lab/snapshot.sh           # 1. the workbook as it is now (read-only, via the server's credentials)
tools/lab/t3.sh --bg            # 2. the lab T3 (Claude + Codex providers)
tools/lab/app.sh --bg           # 3. the lab app; the first start creates the admin `lab` and its T3 workspace
node tools/lab/bench.mjs opus high               # 4. every case, each in its own new thread, in parallel
node tools/lab/bench.mjs gpt-6.1-sol medium --cases stocks-0929
node tools/lab/bench.mjs --history               # model × case, the latest run of each
node tools/lab/timeline.mjs <run> [case]         # where a thread's time went
tools/lab/tailscale.sh          # open the lab to your other devices (tailnet only); --off to close it
node tools/lab/sheet-edit.mjs Insectary_data 5012 Sex=female   # someone types in the lab's sheet (--no-hook: the app is not told)
```

- `tailscale.sh` serves the app (port 8509) and its T3 (port 8510) on this
  machine's Tailscale name over HTTPS, tailnet only, and restarts the app with
  those addresses. The Asistente tab embeds T3 from its own address. T3's port
  goes to the app's T3 proxy (3776), which passes everything to T3 and adds the
  bridge script to its pages, so Cambios propuestos follows the chat open in
  the Asistente's frame (`server/t3bridge.mjs`; the proxy is part of the app,
  so T3 in the frame reconnects when the app restarts). The addresses are saved in `public.env`, so later restarts keep them. Changing
  the serve config uses `sudo tailscale`, because it already holds folder
  entries; the other entries stay.

- `snapshot.sh` runs `scripts/cache-sandbox.mjs` on the server (`LAB_SSH_HOST`,
  default `claudeclaw`). That script only reads, over a read-only Sheets
  connection. The file is copied here with mode 600 and the remote copy is
  deleted. Run it again after people correct more rows, then restart the app:
  the ground truth is always read from the latest snapshot.
- `app.sh` builds the frontend if needed and starts the app from the whole
  snapshot. For a benchmark start it with `LAB_SEED=bench`: it then writes
  `seed.json`, which is the snapshot with every scored cell of the cases emptied, so a model cannot copy
  the answers from the sheet and its proposal holds everything it read. Then it
  starts the app on `http://127.0.0.1:8795`. Sign in as `lab`; the password is
  in `credentials.json`. The Asistente tab shows the lab T3 (through the proxy
  on `http://127.0.0.1:3776`). Every restart
  re-seeds the sheets, so it also undoes a proposal a model applied. `bench.mjs`
  refuses to run while a case's cells are not empty.
  Every start also refreshes the lab workspace (`scripts/t3-provision.mjs
  --refresh-all`) from the checkout it runs from: its brief, skills
  (`assistant/skills`) and subagents (`assistant/agents`). After editing those,
  `tools/lab/app.sh --refresh` rewrites the workspace without a restart; a
  change to the server needs a restart. Run the app from the checkout you are
  testing (check with `ss -ltnp | grep 8795`).
- `t3.sh` writes the lab T3's provider settings. Claude uses the local
  `claude` CLI and its login. Codex uses the local `codex` and `~/.codex`.
  Sonnet 5.5 is a custom model with an effort menu. It also writes
  `t3-admin-token`, which the app and the benchmark use to open T3.
- `bench.mjs <model> [effort]`: the model is `opus`, `sonnet` or
  `gpt-6.1-sol`, or any name as T3's model picker shows it. The effort is
  `low|medium|high|xhigh|max|ultra`. The script drives T3 with headless
  Chromium: new thread, model, effort, photos, prompt, send. It waits for the
  turns in T3's state database. It collects the thread's proposals from the lab
  app's database: the proposal ids in its tool results, or the run tag in the
  proposal title. Then it scores them:
  - A cell is right when the read value equals the truth after normalization.
    NA and blank count as equal. Dates are compared as days (serial numbers,
    ISO or dd/mm/yyyy). Sums are compared by their total (`=12+15` = `27`).
    Case and spaces are ignored. Notes need to be 85% similar.
  - A blank truth cell that was left out of the proposal is right.
  - A filled truth cell that is missing from the proposal counts as missing.
  - Two scorings, side by side (`scoreCase` in `lib.mjs`):
    - **legacy**, as every run before 1 Oct 2026 was scored, so the history
      stays comparable: every truth cell counts, and a cell the proposal marks
      doubtful (`change.doubts`) counts as left out, as `match_notebook` used
      to leave those out. Notes by their words; a note without words is skipped.
    - **new**: the case's `notOnPage` cells (truth not on the photo: death
      dates from the Muertes round, tubes not on the page, terms added later)
      are not counted; doubtful cells count by their value, and are reported
      apart (flagged right, flagged wrong; "wrong unflagged" is a wrong value
      nobody was warned about); a note of IDs only (the parent couple,
      `U8A♀ + C8B♂`) is right when it names the same butterflies; notes
      proposed where the sheet has none are counted (`extraNotes`, only in the
      case's scored note columns). `errors.csv` lists the new scoring's errors.
    Runs scored before both existed show only the legacy one in `--history`:
    `--rescore <run>` adds the new one.
  - Only the thread's own proposals count: made or revised while it ran, or
    tagged with its run. An id in its tool results may be another run's
    proposal of the same rows (`overlaps`).
  - Times: `first proposal` is when the thread's first proposal appeared in the
    app (what the person sees beside the chat), `time` the wall-clock time from
    the message to the last turn. Background subagents end a turn and start
    another when they finish, so a run is over only when no turn is running
    and no Claude transcript of the lab workspace has changed for 90 s.
    `--variant LABEL` names the skill version measured (in the run id and the
    history).
  - Output goes to `results/<run>/`: `table.md`, `scores.json`, `errors.csv`
    (every wrong or missing cell), `run.json` (threads, prompt) and screenshots.
    One line per case is appended to `results/history.jsonl`.
- The threads are set up one after another and then run in parallel. They
  can't be set up in parallel because T3 shares a project's draft between
  tabs. `--dry` prepares the threads without sending them. `--timeout MIN`
  sets the wait limit (default 40).
- `--rescore <run>` scores a run again, for example after a new snapshot. Its
  lines in `history.jsonl` are replaced. Only completed threads go into the
  history; a thread stopped by a usage limit does not.
- Notes are compared without the `d/m/yy INI:` signatures the app adds. A note
  is right when it matches the whole note or one of its entries, since later
  entries in the sheet are not on the page.
- `replay.mjs <run> --app <url> --token <token> --db <app.sqlite>` sends a
  run's recorded `match_notebook`, `update_proposal` and `propose_changes`
  calls (from the lab T3's database) again to an app running the current
  server, and scores the result like the bench: it measures a change to the
  tools without running the models again. Use a copy of the app (its own
  database, e.g. a copy of `app/app.sqlite` with an `ai_tokens` row for `lab`,
  and its own port), never the lab's running app. `update_proposal` indexes
  are mapped to the same sheet rows in the new proposal.

Stop with `tools/lab/app.sh --stop` and `tools/lab/t3.sh --stop`.

## A busy workbook (Google not answering)

After an edit to Insectary_data the team's workbook recalculates for minutes,
and meanwhile Google answers slowly, with 503, or not at all. The lab's sheets
can answer the same way, to see what the app does then: the banner everyone
gets, saves kept in the app and written when Google answers, the assistant's
`apply_proposal` answering `queued`, and the Emergidos / Clutches entries
going to Google Sheets with «Guardar en Google Sheets».

```sh
node tools/lab/busy.mjs 10                     # 10 minutes of 503 on every request
node tools/lab/busy.mjs 10 hang --probe 15     # no answer (each request given up after 3 s); asked again every 15 s
node tools/lab/busy.mjs 10 slow --delay 25     # answers after 25 s: "slow" (over 20 s)
node tools/lab/busy.mjs 0                      # answers again: what waited is written, in order
LOCAL_BUSY=unavailable:10 tools/lab/app.sh --bg   # start the app with the workbook busy for 10 minutes
```

- The state is asked again every minute while it is not ok (`--probe`
  changes that; `GOOGLE_PROBE_SECONDS` at start). Ending it with `busy.mjs 0`
  asks at once. `curl -s http://127.0.0.1:8795/health` shows
  `google`: the workbook's state, the saves waiting and the entries kept in the app.
- What to try: with the workbook busy, edit a cell in Buscador (it stays
  amber with stripes, «N esperando a Google Sheets» in the save bar); in
  Emergidos add butterflies on two devices (each gets its own IDs, the other's
  show «en la app · name»); press «Guardar en Google Sheets»: the entries show
  as being written and go once `busy.mjs 0` ends it. Edit the same cell in the
  sheet meanwhile (`sheet-edit.mjs … --no-hook`) to see a waiting save refused
  and shown in red instead of overwriting.

## The AI's work beside people's requests

The assistant's tool calls and the proposals' tables are built in worker
threads (`server/assistant-host.mjs`), so a chat drafting big proposals never
holds up anyone else's request. Two synthetic loads, no Google, no browser:

```sh
node tools/lab/proposal-load.mjs     # one process, in memory: each step's time and longest block
node tools/lab/worker-load.mjs       # the whole app on a database file, ASSISTANT_WORKER=0 then auto:
                                     # a chat drafts five 2,000-row proposals, Cambios propuestos open,
                                     # someone checks clutches every 100 ms; their p50/p99 and the app's lag
```

A whole-workbook sync (at every start and periodically) compares its rows a
slice at a time and writes them in transactions of up to 1,000 rows, so it
never holds up a request for long either. To measure it with the lab's snapshot
served as Google answers (after a restart, a periodic sync, every row of
Insectary_data moved by a row inserted at its top):

```sh
node tools/lab/sync-load.mjs                       # each step's time, longest block, p99 lag, a person's waits
node tools/lab/sync-load.mjs --profile /tmp/prof   # also a CPU profile of each step
```

The Revisión checks, the suggested edits, the alerts, Inicio's summaries and
Monitoreo's Wikiloc corrections read the whole copy (a second or two each) in
one worker thread (`server/checks-host.mjs`); the app keeps each last result
while the copy is unchanged. To measure them on a copy of the lab app's
database, in the app's thread, with only the checks in the worker (as before
the others moved) and all in the worker (a page after a restart, after a
change, pages asking while someone saves every 100 ms), and to compare the
results:

```sh
node tools/lab/checks-load.mjs                     # each step's time, longest block, p99 lag, a person's waits
node tools/lab/checks-load.mjs --mode worker --kinds summary
```

To find the requests of the main pages that still hold up everyone else (each
asked a first time, after a save in Insectary_data, and again; the longest
stretch the app answered no one), on the whole app over HTTP:

```sh
node tools/lab/pages-load.mjs                      # every page; those over 200 ms listed at the end
node tools/lab/pages-load.mjs --only table,emerged --over 100 --sheet Collection_data
```

How fast the pages open in a browser (Chromium): requests, bytes by type, the
scripts and API calls of the first view, first paint, largest paint and the
moment the page's own content shows. Each load starts with nothing cached;
`--warm` measures a second load (HTTP cache and service worker in place).
Production can be measured as a visitor (Inicio); signed-in pages on the lab app:

```sh
node tools/lab/load-time.mjs https://ithomiini-ikiam.com --phone --throttle slow4g --cpu 4 --runs 3
node tools/lab/load-time.mjs http://127.0.0.1:8795 --login ~/.cache/ithomiini-lab/credentials.json \
  --paths /inicio,/tablas,/colecta --throttle fast4g
```

## Cases

`cases.json` in the lab folder:

```json
{ "cases": [
  { "id": "stocks-p12", "sheet": "Insectary_stocks", "photos": ["stocks-p12.jpg"],
    "ranges": [["947", "976"]],
    "fields": ["SPECIES", "DATE LAID", "NUMBER OF EGGS", "NOTES"],
    "note": "free text" },
  { "id": "emergence-p3", "sheet": "Insectary_data", "photos": ["em-3a.jpg", "em-3b.jpg"],
    "labels": ["0VD", "1VD", "2VD"] }
] }
```

To add a case:

1. Copy the photos into `photos/` in the lab folder.
2. Name the rows. Use `labels` (the sheet's ID: `CLUTCH NUMBER`,
   `Insectary_ID`…) or `ranges` of labels in sheet order, meaning every row
   from the first to the last, as on a notebook page.
3. Choose the scored columns with `fields`. By default every column is scored
   except the ID; list columns to leave out in `skip`. Cells calculated by a
   formula are never scored, but typed sums are.
   Cells whose truth is not on the photo go in `notOnPage`, label → columns,
   e.g. `"notOnPage": { "0VD": ["Death_date"], "958": ["NUMBER OF PUPA"] }`:
   the new scoring leaves them out (the legacy one still counts them).
4. Make sure people have checked those rows in the workbook. Then run
   `snapshot.sh` and restart `app.sh`, which rebuilds the seed with the new
   case's cells emptied.

## Notes

- Claude and Codex run with this PC's user settings (`~/.claude`, `~/.codex`),
  including personal skills and MCP servers, which the server's T3 does not
  have. The lab workspace denies Claude reads of the lab folder, so a model cannot
  open the snapshot. Codex has no such rule. The prompt tells every model to
  read only the photos.
- Only the models the prompt names are used. Don't pick Fable models.
