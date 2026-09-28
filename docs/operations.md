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
| `/home/ubuntu/ithomiini/shared/knowledge/` | Private approved meeting documents |
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

To restore, stop only `ithomiini.service`, preserve the current database and its WAL/SHM files in a dated recovery directory, copy a verified backup to `shared/database.sqlite`, and restart the service. Do not restore over a running SQLite database. The service will reconcile with the current test Sheet. Restoring the app database does not reverse Google Sheet edits; use reviewed history reversals for those.

To roll back code, read `shared/previous-release`, point `current` to that existing release, and restart `ithomiini.service`. Preserve the shared database. Review schema changes before rolling back to a release that cannot read them.

## Data and AI boundaries

Google access uses the user's approved OAuth grant. The credential file contains `client_id`, `client_secret`, and `refresh_token`, with mode 600. If Google revokes the grant, renew consent; do not recover tokens from chat history.

The assistant uses a separate protected OpenRouter key file. Text queries use the configured DeepSeek model; photo and audio drafts use the configured Gemini model. Relevant excerpts are sent to that provider when needed. Assistant messages cannot execute shell commands or arbitrary SQL. Applying proposed edits requires a reviewed action through the same validation as forms.

Meeting knowledge is a curated snapshot of the 69 reviewed documents. It is not a continuously synchronized Gmail or Drive mirror. Video-call access links are omitted. Refresh this corpus deliberately when adding later notes.

## Known limits

- Sheet snapshot reconciliation can miss edits overwritten between snapshots and cannot reliably identify external editors.
- Reversal checks protect selected fields and observed later edits. Google Sheets does not provide a transactional compare-and-swap against concurrent direct edits.
- Saves and undos of several rows are one atomic Google batch update. If Google rejects it nothing is written and the same request can be retried; if the outcome is unknown (network loss after sending) the action is marked "Sin confirmar" until the startup check or an administrator's re-check in Historial settles it.
- Before any write the app compares the live header row with its column map. If someone inserts, removes or renames a column in the Sheet, saving and syncing that sheet stop until the column map (`docs/workbook-schema.json`) is regenerated.
- Sheets without identifier columns are matched by identical row content during sync; editing such a row is refused if it changed in the Sheet since the last sync.
- A biological mark may occur more than once historically. Select the intended source record from search results. Related-record matches are candidates, not globally enforced foreign keys; review downstream studies when correcting identifiers or reversing specimen observations.
- The new interface writes only to sheet columns. Older app-only observations (events) remain in the database and API but are not shown. Study eligibility and mutation definitions require researchers' recorded criteria.
- Offline: the app shell and the last loaded sheets are cached; unsaved changes stay on the device and are saved manually when the connection returns.
