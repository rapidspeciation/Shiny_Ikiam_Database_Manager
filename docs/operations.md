# Operating the app

## Host and account setup

The app uses `https://ithomiini-ikiam.duckdns.org/` (its own DuckDNS name, served at the root: `APP_BASE_PATH=/`), with a separate service on loopback port 8794. T3 Code is at `https://t3.ithomiini-ikiam.duckdns.org/`, same site as the app so it can sit inside the Asistente tab. The DuckDNS token is in `~/.config/ithomiini/duckdns-token` on the server; the server's address is fixed, so nothing updates it periodically.

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

`scripts/deploy.sh` installs the frontend's locked dependencies, runs the syntax and type checks and all tests, builds the frontend into `web/`, uploads a release, switches the symlink, and restarts only `ithomiini.service`. Caddy configuration is installed separately after validation; `deploy/Caddyfile.fragment` shows the route. Preserve existing routes when updating Caddy.

```sh
ssh claudeclaw 'systemctl --user status ithomiini.service'
ssh claudeclaw 'journalctl --user -u ithomiini.service -n 60 --no-pager'
curl -fsS https://ithomiini-ikiam.duckdns.org/health
```

Health confirms the process is running. The authenticated synchronization view shows whether the workbook is current. Startup and changed-workbook refreshes can take longer than ordinary requests because the workbook contains hundreds of thousands of formulas.

Edits made directly in Google Sheets arrive within seconds when the Apps Script trigger in `tools/apps-script` is installed in the workbook; it calls `POST /api/hooks/sheet-edit` with `SHEET_HOOK_SECRET`. Without it they wait for the 5-minute read. The synchronization status (`GET /api/sync`) shows the trigger's last report under `hook`.

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

## Data and AI boundaries

Google access uses the user's approved OAuth grant. The credential file contains `client_id`, `client_secret`, and `refresh_token`, with mode 600. If Google revokes the grant, renew consent; do not recover tokens from chat history.

The assistant uses a separate protected OpenRouter key file. Text queries use the configured DeepSeek model; photo and audio drafts use the configured Gemini model. Relevant excerpts are sent to that provider when needed. Assistant messages cannot execute shell commands or arbitrary SQL. Applying proposed edits requires a reviewed action through the same validation as forms.

The assistant's documents are the hand-copied snapshot of 69 meeting notes at the top of `shared/knowledge/` and a text mirror of the project Drive in `shared/knowledge/drive/`, kept by `scripts/drive-sync.mjs` (`ithomiini-drive-sync.service`), run only on request: the assistant's `sync_documents` tool or the command below. There is no timer. PDFs are read with `pdftotext` (poppler-utils, installed). The folders mirrored and the names never opened are in `deploy/drive-sync.json`: Meetings (with the call transcripts), Protocols, Reports, Insectary and Greenhouse Management and the presentations at the Drive's top; never Admin, Invoices, Photos, Videos, Data, Datalogger, the database backups or any name like InfoAccess/password/contraseña/factura/invoice/contrato. Sheets are never exported, and files over 30 MB are listed without text. gog always runs with `--readonly`; nothing is written to Drive. Only files whose modification time changed are exported again; files removed or trashed in Drive disappear from the mirror on the next run. A Drive document also in the hand-copied snapshot is read from the mirror.

Docs are exported as Markdown (embedded images left out), Slides as PowerPoint and read slide by slide with their speaker notes (a deck Drive will not export, over 10 MB with its images, is read one slide at a time with `gog slides read-slide`), PowerPoint and Word files are read from their XML, WebVTT transcripts as speaker paragraphs, and PDFs with `pdftotext` (first 80 pages) when poppler-utils is installed (`sudo apt install poppler-utils`); without it PDFs are listed as "sin texto" with their link. Each document keeps at most 400 KB of text. The assistant searches them (`search_knowledge`, BM25 over ~1500-character passages, accents ignored, filters by kind and date), lists them (`list_documents`, e.g. the last meeting) and reads them (`read_document`), and cites each document's Drive link when it answers from it.

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
