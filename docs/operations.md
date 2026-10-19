# Operating the app

## Host and account setup

The app uses `https://ithomiini-ikiam.com/` (bought 29 Sep 2026 on Cloudflare; DNS-only A records for the apex, `www` and `t3` point at the server; served at the root: `APP_BASE_PATH=/`), with a separate service on loopback port 8794. T3 Code is at `https://t3.ithomiini-ikiam.com/`, same site as the app so it can sit inside the Asistente tab. The old DuckDNS names (`ithomiini-ikiam.duckdns.org`, `t3.…`), blocked on some university networks, redirect there with 308 (Caddy); their token is in `~/.config/ithomiini/duckdns-token`. The Cloudflare DNS token is in the keyring of Franz's PCs (`secret-tool lookup service cloudflare-api credential account-dns-token`), never in the repo.

The app used to live at `https://tbs-insect-gallery.duckdns.org/ithomiini/`, a name shared with other projects; that route was removed on 2026-09-28. Existing Tiputini services retain their own routes and data.

The setup screen requires a private random token and creates the first active administrator. Administrators create accounts and assign observer, editor, reviewer, or administrator roles. Passwords are hashed with scrypt; sessions use HttpOnly cookies and mutations require a CSRF token. Never publish the setup token. App access does not change Google Drive sharing permissions.

| Path | Contents |
| --- | --- |
| `/home/ubuntu/ithomiini/releases/` | Timestamped application releases |
| `/home/ubuntu/ithomiini/current` | Symlink to the active release |
| `/home/ubuntu/ithomiini/shared/database.sqlite` | Records, history, users, sessions, tasks, attachments, and assistant threads |
| `/home/ubuntu/ithomiini/shared/knowledge/` | Private approved meeting documents (hand-copied snapshot) |
| `/home/ubuntu/ithomiini/shared/knowledge/drive/` | Text mirror of the project Drive's documents and `manifest.json` (`ithomiini-drive-sync.service`, on request) |
| `/home/ubuntu/ithomiini/shared/backups/` | Verified daily SQLite backups |
| `/home/ubuntu/.config/ithomiini/` | Protected service settings and Google/AI credentials |

## Deploy and monitor

`scripts/deploy.sh` installs the frontend's locked dependencies, runs the syntax and type checks and all tests, builds the frontend into `web/`, uploads a release, switches the symlink, and restarts `ithomiini.service`. It also installs and restarts the server's Wikiloc worker. It then deletes old releases, keeping the newest `KEEP_RELEASES` (default 5) plus the current and previous ones (`scripts/prune-releases.sh`). Caddy configuration is installed separately after validation; `deploy/Caddyfile.fragment` shows the route. Preserve existing routes when updating Caddy.

```sh
ssh claudeclaw 'systemctl --user status ithomiini.service'
ssh claudeclaw 'journalctl --user -u ithomiini.service -n 60 --no-pager'
curl -fsS https://ithomiini-ikiam.com/health
```

Health confirms the process is running. The authenticated synchronization view shows whether the workbook is current. Startup and changed-workbook refreshes can take longer than ordinary requests because the workbook contains hundreds of thousands of formulas.

### Wikiloc imports

Links and followed-profile scans are processed by `ithomiini-wikiloc.service` on claudeclaw. Its Camoufox browser runs on a private Xvfb display and reads the public trail pages, with Cloudflare challenge handling. The app stores each walk and downloads its photos for review. This runs independently of any PC. GPX uploads retain their direct import path and supply GPS timestamps.

The worker is installed in `/home/ubuntu/.local/share/ithomiini-wikiloc`, including its Python virtual environment. Its editor-account credentials are in `/home/ubuntu/.config/ithomiini-wikiloc/worker.json`, mode 600. Keep that file across releases. The host needs `python3-venv`, `xvfb`, and GTK libraries; the installer pins its Python packages and fetches the Camoufox browser. See [Wikiloc importer](../tools/wikiloc/README.md).

```sh
ssh claudeclaw 'systemctl --user status ithomiini-wikiloc.service'
ssh claudeclaw 'journalctl --user -u ithomiini-wikiloc.service -n 60 --no-pager'
```

The Importar screen shows the worker heartbeat. Pending jobs stay in SQLite during service outages, and interrupted jobs become eligible for retry after 15 minutes. The former PC worker is disabled after server verification.

Edits made directly in Google Sheets arrive within seconds when the Apps Script trigger in `tools/apps-script` is installed in the workbook; it calls `POST /api/hooks/sheet-edit` with `SHEET_HOOK_SECRET`. Without it they wait for the 5-minute read. The synchronization status (`GET /api/sync`) shows the trigger's last report under `hook`.

### T3 bridge

Cambios propuestos (Asistente tab) follows the chat open in the T3 Code frame beside it. T3 is a stock install with no embedding API, so the app adds one script tag to T3's pages (`server/t3bridge.mjs`): inside a frame, the script posts the chat on screen to the app's origin (`APP_PUBLIC_URL`) on each navigation; a T3 tab of its own sends nothing. Caddy sends T3's page loads (GET, `Accept: text/html`, not `/api/*` or `/assets/*`) and `/__ithomiini/bridge.js` to the app, which recognises T3's host (`ITHOMIINI_T3_URL`), fetches the page from `ITHOMIINI_T3_LOCAL` and adds the tag; everything else, websockets included, goes from Caddy to T3 as before (`deploy/Caddyfile.fragment`). While the app is down, T3's pages come straight from T3, and the panel guesses the open chat from T3's trace log as it did before the bridge. Installing it: deploy the app first, then replace the `t3.ithomiini-ikiam.com` block of `/etc/caddy/Caddyfile` with the fragment's, `caddy validate`, and reload Caddy. Check: `curl -s -H 'Accept: text/html' https://t3.ithomiini-ikiam.com/ | grep __ithomiini` shows the tag.

### Updating T3 Code

Admins update T3 from the Asistente tab: the bar shows T3's version (amber when a newer stable release exists), and its dialog says what the update does before it starts: the version jump, the release notes and their "Before you update", a warning for a major version (0.0.x → 0.1.0 moved T3 to Orchestrator V2), and the Claude sessions it will cut. The update is `scripts/t3-update.mjs`, run by the user unit `ithomiini-t3-update@<version>.service` (`deploy/`; the dialog passes the version the admin saw):

1. a copy of T3's database (`statev2.sqlite` from 0.1.0, `state.sqlite` before; `VACUUM INTO`, consistent while T3 runs), its settings and `runtime/service-state.json`, in `~/.t3/backups/<time>-<from version>`; the newest 3 stay;
2. `t3 update <version> --yes` (with `--allow-downgrade` when going back): it downloads the version into `~/.t3/runtime/versions`, points `~/.local/bin/t3` at it and restarts `t3code.service`;
3. a wait until T3 answers on the new version (`/.well-known/t3/environment`, up to 15 minutes);
4. the app's T3 token (`ITHOMIINI_T3_ADMIN_TOKEN_FILE`, label `ithomiini-broker`) issued again by the version now running, and the earlier one revoked: a token from an older T3 lacks the newer one's permissions, and so do the sign-in links made with it (0.1.0 shows «Permissions have changed»); after going back, the older database does not know the newer token.

Its progress goes to `~/.t3/userdata/logs/t3-update.json` and its log to `t3-update.log`, both shown in the dialog while it runs. Afterwards the dialog checks what the app needs from T3 (`GET /api/admin/t3/checks`): T3 answers, the token and its permissions, a sign-in link, the way the app's commands go (HTTP up to 0.0.45, T3's socket from 0.1.0), the chats read from T3's database, and the bridge's place in T3's page. Each browser signs in to T3 again once after a version change (T3Frame), so its session gets the new version's permissions.

Going back: T3 keeps the versions it had in `~/.t3/runtime/versions`, and the dialog offers them («Volver a una versión anterior»): the same update to that version. From 0.1.0 back to 0.0.45, T3 reads `state.sqlite` again, as it was before the update: chats made in 0.1.0 stay in `statev2.sqlite` and show again when it is updated once more (that update does not copy again, so chats made meanwhile in 0.0.45 do not come along). The app follows the file of the version running (`userdata/server-runtime.json`). By hand: `systemctl --user start ithomiini-t3-update@0.0.45.service`, or `node scripts/t3-update.mjs 0.0.45 --home ~/.t3 --token-file ~/.config/ithomiini/t3-admin-token`. A database copy is restored with T3 stopped (`systemctl --user stop t3code.service`, copy the file back over the one in `userdata/`, removing its `-wal` and `-shm`, start it).

In the lab, `tools/lab/t3.sh --update [<version>]` runs the same script for the lab's T3 (no service: it starts it again), and the lab app's dialog uses it (`ITHOMIINI_T3_UPDATE_COMMAND`). The lab's T3 runs `$LAB/bin/t3`, a link into its home's `runtime/versions` that `t3 update` moves, as `~/.local/bin/t3` on the server (`tools/lab/t3.sh --install <release folder>` sets it up).

## Back up and restore

The daily timer creates a consistent SQLite backup and checks its integrity. It keeps the newest 14 daily backups. Attachments and assistant threads live in the same database. Administrators can also request a backup from the app.

```sh
ssh claudeclaw 'systemctl --user start ithomiini-backup.service'
ssh claudeclaw 'systemctl --user list-timers ithomiini-backup.timer'
```

To restore, stop only `ithomiini.service`, preserve the current database and its WAL/SHM files in a dated recovery directory, copy a verified backup to `shared/database.sqlite`, and restart the service. Do not restore over a running SQLite database. The service will reconcile with the workbook. Restoring the app database does not reverse Google Sheet edits; use reviewed history reversals for those.

To roll back code, read `shared/previous-release`, point `current` to that existing release, and restart `ithomiini.service`. Preserve the shared database. Review schema changes before rolling back to a release that cannot read them.

## Switching workbooks

The database caches one workbook (`settings.workbookId`; databases from before it cache the test copy) and the service refuses to start on another (`WORKBOOK_MISMATCH`): a plain sync would log every difference as an edit made in Google Sheets. `scripts/switch-workbook.mjs` moves it. It only reads the new workbook. Rows are matched by their identifiers (then by row), so a specimen keeps its record id, also when its row moved, and monitoring links follow it; rows only in the old workbook are retired. The differences are not logged: the Historial of the old workbook moves to `archived_actions`/`archived_changes` (with undo plans and Revisión verdicts) and starts empty; pending AI proposals are discarded. Users, sessions, monitoring walks and photos, envelope and photo curation, Wikiloc profiles and tokens stay.

```sh
# dry run on a temporary copy (safe while the service runs)
ssh claudeclaw 'cd ~/ithomiini/current && export DATABASE_PATH=/home/ubuntu/ithomiini/shared/database.sqlite GOOGLE_CREDENTIALS_FILE=/home/ubuntu/.config/ithomiini/google.json && node=$(sed -n "s/^ExecStart=\([^ ]*\/node\) .*/\1/p" deploy/ithomiini.service) && "$node" scripts/switch-workbook.mjs'
# apply: service stopped; writes a checked backup to shared/backups/before-workbook-switch-*.sqlite first
ssh claudeclaw 'systemctl --user stop ithomiini'
ssh claudeclaw 'cd ~/ithomiini/current && export DATABASE_PATH=/home/ubuntu/ithomiini/shared/database.sqlite GOOGLE_CREDENTIALS_FILE=/home/ubuntu/.config/ithomiini/google.json && node=$(sed -n "s/^ExecStart=\([^ ]*\/node\) .*/\1/p" deploy/ithomiini.service) && "$node" scripts/switch-workbook.mjs --apply'
ssh claudeclaw 'systemctl --user start ithomiini'
```

## Photo archive curation (envelopes, gallery, wings)

The envelope readings, the Wings Gallery's predictions and the wing models' sides and turns are read on the PC that has them, into two small bundles, and loaded into the app's database while it runs (each import replaces its earlier one). `photo_facts`, the Revisión tab and Photo_links readings use them.

```sh
# on the PC (C: the curation folder, e.g. ~/.local/share/sanger-envelope-sam3-20260924/ocr-next/curation-20261017-combined;
# M: ithomiini_maps on branch dorsal-ventral-wings)
node scripts/import-envelope-curation.mjs --bundle envelope-bundle.json.gz "$C" "$C/manifest-combined.csv"
node scripts/import-wing-sides.mjs --bundle wing-sides.json.gz --manifest "$C/manifest-combined.csv" "$M/models/dorsal_ventral/eval" "$M/models/dorsal_ventral/eval/new_photos_20261015"
scp envelope-bundle.json.gz wing-sides.json.gz claudeclaw:/home/ubuntu/ithomiini/shared/
# on the server, after the release that has these scripts is deployed
ssh claudeclaw 'cd ~/ithomiini/current && export DATABASE_PATH=/home/ubuntu/ithomiini/shared/database.sqlite && node=$(sed -n "s/^ExecStart=\([^ ]*\/node\) .*/\1/p" deploy/ithomiini.service) && "$node" scripts/import-envelope-curation.mjs /home/ubuntu/ithomiini/shared/envelope-bundle.json.gz && "$node" scripts/import-wing-sides.mjs /home/ubuntu/ithomiini/shared/wing-sides.json.gz'
```

## Data and AI boundaries

Google access uses the user's approved OAuth grant. The credential file contains `client_id`, `client_secret`, and `refresh_token`, with mode 600. If Google revokes the grant, renew consent; do not recover tokens from chat history.

The assistant uses a separate protected OpenRouter key file. Text queries use the configured DeepSeek model; photo and audio drafts use the configured Gemini model. Relevant excerpts are sent to that provider when needed. Assistant messages cannot execute shell commands or arbitrary SQL. Applying proposed edits requires a reviewed action through the same validation as forms.

The assistant's documents are the hand-copied snapshot of 69 meeting notes at the top of `shared/knowledge/` and a text mirror of the project Drive in `shared/knowledge/drive/`, kept by `scripts/drive-sync.mjs` (`ithomiini-drive-sync.service`), run only on request: the assistant's `sync_documents` tool or the command below. There is no timer. PDFs are read with `pdftotext` (poppler-utils, installed). The folders mirrored and the names never opened are in `deploy/drive-sync.json`: Meetings (with the call transcripts), Protocols, Reports, Insectary and Greenhouse Management and the presentations at the Drive's top; never Admin, Invoices, Photos, Videos, Data, Datalogger, the database backups or any name like InfoAccess/password/contraseña/factura/invoice/contrato. Sheets are never exported, and files over 30 MB are listed without text. gog always runs with `--readonly`; nothing is written to Drive. Only files whose modification time changed are exported again; files removed or trashed in Drive disappear from the mirror on the next run. A Drive document also in the hand-copied snapshot is read from the mirror.

Docs are exported as Markdown (embedded images left out), Slides as PowerPoint and read slide by slide with their speaker notes (a deck Drive will not export, over 10 MB with its images, is read one slide at a time with `gog slides read-slide`), PowerPoint and Word files are read from their XML, WebVTT transcripts as speaker paragraphs, and PDFs with `pdftotext` (first 80 pages) when poppler-utils is installed (`sudo apt install poppler-utils`); without it PDFs are listed as "sin texto" with their link. Each document keeps at most 400 KB of text. The assistant searches them (`search_knowledge`, BM25 over ~1500-character passages, accents ignored, filters by kind and date), lists them (`list_documents`, e.g. the last meeting) and reads them (`read_document`), and cites each document's Drive link when it answers from it.

The Photo_links sheet is brought up to date with the photo folders on Drive by the assistant's `sync_photo_links` tool (`server/photo-links.mjs`, `server/drive.mjs`): it lists the folders with the app's own Google login (its `drive.metadata.readonly` scope gives names, links, owners and capture times; no file is opened), shows what the sheet lacks and what looks wrong in the new photos (as a card of Cambios propuestos, with its rows and a progress bar), and writes it as ordinary saves once the person agrees, in the chat or with the card's «Aplicar». It needs no setup: the folders listed are those already in Photo_links, and the folder holding them is where new ones are looked for. The sheet's own script (`list_google_drive_files.gs` in the Wings Gallery's repository, menu «📷 Photo Database Tools») writes the same rows and still works.

The assistant's `organize_photos` changes the files themselves (rename, move, remove, new folders, shortcuts; `server/photo-files.mjs`), which the app's login cannot: it uses the project's Google account, an editor of the photo folders, from the credentials file named in `GOOGLE_DRIVE_CREDENTIALS_FILE` (`client_id`, `client_secret`, `refresh_token` with the `drive` scope, mode 600). Without that variable the tool answers that no account is set up. That account bins only the files it owns; another account's file is renamed `…_todelete` for its owner to delete. A plan is made only from its card's «Aplicar», never from the chat. Each plan's changes are kept in the table `drive_changes` so they can be taken back.

The «Fotos» tab shows the Rename Photos app (github.com/Fr4nzz/rename_photos_AI) in a frame: `scripts/build-rename-app.sh` builds it into `rename-app/` from the commit named in `rename-app.commit` (the lab: from the checkout beside this repository), and the server serves it under `<base>/rename-app/` with headers of its own (framed by this site only, WebAssembly and workers allowed). The photos stay on the person's computer; the open tab is the link between that app and the assistant's `rename_photos_*` tools (`server/rename-photos.mjs`: the batch's rows, small pictures on request, corrections to rows), all in memory. «Revisar con Claude» starts a chat of the person's T3 project with the request (`POST /api/rename/review`).

`update_websites` and the «Fotos» tab's site buttons start the «Update Database» workflow of the Wings Gallery's and the maps' repositories (`server/site-updates.mjs`), which publishes its site when it commits new data. They ask the same Cloudflare Workers the sites' own update buttons use (`ikiam-db-updater` and `ithomiini-maps-db-updater` on franz-chandi.workers.dev; each holds the GitHub token as its secret `GITHUB_TOKEN`), so the app has no GitHub token: the workers' password is in the file named by `SITE_UPDATE_PASSWORD_FILE` (mode 600). Without it the tool and the tab only report the last runs (GitHub's public API, 60 requests an hour from the server).

The «Fotos» tab's «Photo_links y sitios» reads Photo_links' folders without the assistant (`GET /api/photo-links/folders`, `POST /api/photo-links/read`): the reading is the person's own card of Cambios propuestos, applied with its «Aplicar» as the chat's are.

Photos uploaded to Drive without being renamed are read on the server (`drive_photos`, `server/drive-photos.mjs`): the Rename Photos app's reader runs as a child process from `photo-reader/` (built by `scripts/build-rename-app.sh` into `shared/photo-reader`, linked from each release; `PHOTO_READER_DIR` names another), about a second a photo with two of the four cores. Each photo is downloaded with the project account; a small copy of it and of its envelope stays under `DRIVE_PHOTOS_CACHE` (default `drive-photos/` beside the database, at most 1 GB, 14 days), and the whole photo in its `originals/` (at most `DRIVE_PHOTOS_ORIGINALS_GB`, default 5, the least used dropped first; none while the disk has under 10 GB free) for the assistant to look closer. Applying a batch makes a plan of `organize_photos`: a turn changes the photo's orientation tag and gives Drive the file back (the same file and link; RAW files too: CR2, ORF and the other TIFF-based ones).

What only another account can do (delete the `…_todelete` files it owns, or copy a folder's photos to take them over) is left ready for it in the team's sheet «Transfer Ownership & Free Up Space» (`server/transfer-sheet.mjs`; `TRANSFER_SHEET_ID` names another sheet): the same account writes the folder's ID, the subfolders option and the delete mode in the cells beside their labels, and the person signed in to the account concerned opens the sheet and presses «Copy files» or «Delete copied files». The step is left out while the sheet shows a run going on (its «Last Log», for four hours).

```sh
ssh claudeclaw 'systemctl --user start ithomiini-drive-sync.service'   # sync now
ssh claudeclaw 'journalctl --user -u ithomiini-drive-sync.service -n 20 --no-pager'
```

The last line of each run gives the counts per kind and status (exported, unchanged, deleted, failed). A failed file keeps its earlier text, is marked `error` in `drive/manifest.json` and is retried on the next run; a failed folder listing stops the run without deleting anything. Video-call links (Meet, Zoom, Teams) are left out of the text; the mirror is private to the server (mode 600).

## Known limits

- Sheet snapshot reconciliation can miss edits overwritten between snapshots and cannot reliably identify external editors.
- Reversal checks protect selected fields and observed later edits. Google Sheets does not provide a transactional compare-and-swap against concurrent direct edits.
- Saves and undos of several rows are one atomic Google batch update. If Google rejects it nothing is written and the same request can be retried; if the outcome is unknown (network loss after sending) the action is marked "Sin confirmar" until the startup check or an administrator's re-check in Historial settles it.
- Fields are found by their header name in the header row read with the rows (every sync, and the save's own read right before writing; `server/columns.mjs`). Moved or inserted columns keep working; a new column is ignored and never written. A known column missing or renamed makes only that field unavailable: it keeps its last known values, shows read-only, and saving it is refused naming the column. A known header written twice, a missing identifier column or an unrecognizable header row stops syncing and saving that sheet. Tablas shows what changed (reviewers and admins).
- Pre-made rows are extended by copying the last one (`server/premade.mjs`): formulas, formats and dropdowns, with the Insectary ID series continued (after Z9D a new round starts with its first free ID typed, as the team does). A save that needs a row past them makes 20 first. Protected ranges the app's credential cannot edit are left for the sheet's owner and named in the result.
- Sheets without identifier columns are matched by identical row content during sync; editing such a row is refused if it changed in the Sheet since the last sync.
- A biological mark may occur more than once historically. Select the intended source record from search results. Related-record matches are candidates, not globally enforced foreign keys; review downstream studies when correcting identifiers or reversing specimen observations.
- The new interface writes only to sheet columns. Older app-only observations (events) remain in the database and API but are not shown. Study eligibility and mutation definitions require researchers' recorded criteria.
- Offline: the app shell and the last loaded sheets are cached; unsaved changes stay on the device and are saved manually when the connection returns.
