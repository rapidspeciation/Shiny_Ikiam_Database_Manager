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
- **Limits:** at most 4 at once (`DEV_APP_LIMIT`). Each needs 2.5 GB available to start (MemAvailable), has a 3 GB
  ceiling (`--max`) and a 90-minute limit (`--ttl`). It stops by itself when the chat that started it ends.

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

## Memory and disk (measured 8 Nov 2026)

| 3 instances side by side | this PC (15 GB, btrfs) | claudeclaw (24 GB, ext4) |
|---|---:|---:|
| start, until its first sync is done | 2.4 s to listen (the sync after it was not timed here) | 23 s (44 s when the rows file is made first) |
| server RSS in the first minute after the start | 1345, 1502, 1379 MB | 1524, 1440 MB |
| server RSS after about 2 minutes idle | 284 MB | 358 MB |
| the whole unit (memory.current, page cache of its files included) | 1474–1658 MB | 2206–2291 MB |

After a short idle, V8 gives back the memory of the start (the first sync's copies). The app's memory grows while
it is used: on the perf bench, an instance that synced the whole workbook again and ran the assistant's tools held
2.6–2.8 GB, and the identifier added about 1.5 GB while it ran. On this PC, plan on 3 instances, 4 at most, and stop
them when done.

- The workbook's rows live in a SQLite file instead of memory (about 0.7 GB less per instance). The first start
  after a new snapshot, or after a change to the code that shapes the rows, makes
  `~/.cache/ithomiini-lab/snapshot.json.rows.sqlite` once: about 11–18 s and 1.9 GB, one start at a time (a lock).
  Later starts copy it.
- Disk per instance: on btrfs (this PC) the copy shares the file and costs nothing until it is written. On ext4
  (claudeclaw) it is a real 442 MB copy in the instance's folder. Old `dev/<port>-<time>` folders can be deleted
  when their instance has stopped.

## On claudeclaw

The same script, from a checkout there, with `ITHOMIINI_LAB_DIR` pointing at a lab folder that holds a
`snapshot.json`, e.g. `/home/ubuntu/perf-bench/lab` (the perf bench's).
