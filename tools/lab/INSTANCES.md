# Lab instances of the app, side by side

Each instance is a copy of the app from the checkout you start it in. It runs on its own port, in its own folder,
with its own database and its own copy of the workbook's rows. Use one to try a feature branch, run a browser
test, or check a fix, without waiting for anyone else's instance.

```sh
tools/lab/dev-app.sh --fresh             # start; prints the URL, the log and the login file, once it is ready
tools/lab/dev-app.sh --list              # the ones running: unit, memory, folder
tools/lab/dev-app.sh --stop --port 8822  # stop yours when done
```

- **Port:** the first free one from 8821, or `--port <n>`.
- **Folder:** `--fresh` gives a new one, `~/.cache/ithomiini-lab/dev/<port>-<time>`. Without it, the folder is
  `dev/<port>`, and what an earlier instance on that port left in it (database, proposals, photos cache) is kept.
- **Login:** the admin `lab`; its password is in `<folder>/credentials.json`.
- **Code:** the instance runs the code of the checkout whose `tools/lab/dev-app.sh` you call. For a feature branch,
  call it from that branch's worktree. The first start in a checkout builds `web/` (`npm run build`), which needs
  `frontend/node_modules`.
- **Settings:** `--env K=V` adds or overrides one setting, e.g. `--env LOCAL_SHEETS=memory` to keep the rows in
  memory as before.
- **Limits:** no limit on how many (set `DEV_APP_LIMIT` for one). Each needs 2.5 GB available to start
  (MemAvailable), has a 3 GB ceiling (`--max`) and a 90-minute limit (`--ttl`). Starts take turns: a lock is held
  from the port choice until the copy has settled (`/health` says `"settled":true`: first sync and warm-up done,
  memory given back; 3 min at most). The script answers after the first sync and a holder in the background keeps
  the lock for the rest. Starting several from parallel scripts is safe; they queue at about 30-60 s each, and a
  start gives up after waiting 30 min.
- **Stopping:** `--stop`, else the time limit. On this PC the reaper (`node tools/lab/reap.mjs --install`, a user
  timer) also stops an instance once the Claude or Codex chat that started it has ended; an instance started from
  anywhere else (a plain shell, ssh) has no such chat and runs until `--stop` or its time limit.

## What it never touches

- **The team's workbook:** `LOCAL_MODE=1` on the lab's read-only snapshot (`tools/lab/snapshot.sh`), and no Google
  credentials reach it. A restart starts again from the snapshot, so whatever was written is gone.
- **Google:** mail and Google calls go through `tools/lab/fake-gog.sh`. Emails are written to
  `~/.cache/ithomiini-lab/mail/`.
- **Drive:** photos are a recorded listing (`drive-listing.json`).
- **WhatsApp:** answers only go to the outbox; nothing is sent unless you pass `WA_SENDER=wacli`.
- **T3:** none, unless you give `ITHOMIINI_T3_URL`. For chats, use the lab T3 on :3775, never the main T3 on
  :3774.
- **The public sites:** no gallery or maps updates (`SITE_UPDATE_PASSWORD_FILE` is unset).
- **Other instances:** an edit in one instance never shows in another. Each has its own `app.sqlite` and its own
  `workbook-rows.sqlite`.

## Memory and disk (measured 9 Nov 2026, this PC, load average 15-19 from other work)

| VmRSS + VmSwap of the server, 60 s after its unit started | before (rows in memory) | now |
|---|---:|---:|
| one copy | 2317 MB (start 50 s) | 187 MB (start 24 s) |
| 3 copies started back to back (they take turns) | not run: about 3 × 2.3 GB, too close to systemd-oomd's swap limit | 198, 188, 183 MB (own starts 29-30 s; ready 30, 40 and 40 s after each call) |
| peak during a start (VmHWM) | 1975 MB+ | 0.9-1.3 GB |

How a copy stays small:
- The workbook's rows live in a SQLite file, not in memory (see below).
- Once the first sync is done, and again once the checks' warm-up (Inicio's summaries, alerts, issues, suggested
  edits) is done, the server runs a full garbage collection that hands the freed pages back at once
  (`gc({flavor: 'last-resort'})`, only in LOCAL_MODE when node runs with `--expose-gc`, as dev-app.sh starts it).
  Without it V8 keeps about 1 GB of the start's garbage for minutes.
- After the warm-up the checks' worker thread stops (it kept what it read, about 0.5 GB); the next check starts
  another (a second or two). Other idle workers stop after 5 min as before (`WORKER_IDLE_MINUTES`).

The app's memory grows while it is used: on the perf bench, an instance that synced the whole workbook again and
ran the assistant's tools held 2.6-2.8 GB, and the identifier added about 1.5 GB while it ran. Stop copies when done.

- **The rows file:** the first start after a new snapshot, or with a version of the code that shapes the rows not
  seen before (`server/sheets.mjs`, `server/schema.mjs`, `docs/workbook-schema.json`, `server/file-sheets.mjs`),
  makes `~/.cache/ithomiini-lab/snapshot.json.<code>.rows.sqlite` once: about 15-60 s and 1.9 GB, in a child
  process. Copies starting together make it once (an exclusive lock on `<rows file>.lock` beside it, let go by the
  system when its holder ends); the others wait and use it. Later
  starts copy it to their folder (`workbook-rows.sqlite`) and write only there, so the shared file never changes
  and a restart undoes every write. Each code version keeps its own file (442 MB), so two branches run side by side
  without making each other's again; delete the ones of finished branches.
- **Disk per instance:** on btrfs (this PC) the copy shares the file and costs nothing until it is written. On ext4
  (claudeclaw) it is a real 442 MB copy in the instance's folder. Old `dev/<port>-<time>` folders can be deleted
  when their instance has stopped.
- **Rollback:** `--env LOCAL_SHEETS=memory` keeps the rows in memory as before; `--env NODE_OPTIONS=--max-old-space-size=2048`
  (without `--expose-gc`) turns the collection and the worker stop off.

## On claudeclaw

The same script, from a checkout there, with `ITHOMIINI_LAB_DIR` pointing at a lab folder that holds a
`snapshot.json`, e.g. `/home/ubuntu/perf-bench/lab` (the perf bench's).
