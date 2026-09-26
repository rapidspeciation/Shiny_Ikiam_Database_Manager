# Operating the app

## Host and account setup

The app uses `https://tbs-insect-gallery.duckdns.org/ithomiini/`, with a separate service on loopback port 8794. Caddy strips the `/ithomiini` prefix before forwarding. Existing Tiputini services retain their own routes and data.

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

`scripts/deploy.sh` runs syntax checks and tests, uploads a release, switches the symlink, and restarts only `ithomiini.service`. Caddy configuration is installed separately after validation; `deploy/Caddyfile.fragment` shows the route. Preserve existing routes when updating Caddy.

```sh
ssh claudeclaw 'systemctl --user status ithomiini.service'
ssh claudeclaw 'journalctl --user -u ithomiini.service -n 60 --no-pager'
curl -fsS https://tbs-insect-gallery.duckdns.org/ithomiini/health
```

Health confirms the process is running. The authenticated synchronization view shows whether the workbook is current. Startup and changed-workbook refreshes can take longer than ordinary requests because the workbook contains hundreds of thousands of formulas.

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
- Multi-record operations must expose partial results if a later record fails. Retrying uses operation IDs to avoid duplicate writes.
- A biological mark may occur more than once historically. Select the intended source record from search results. Related-record matches are candidates, not globally enforced foreign keys; review downstream studies when correcting identifiers or reversing specimen observations.
- App observations preserve extra details without claiming to update absent spreadsheet fields. Study eligibility and mutation definitions require researchers' recorded criteria.
- Camera scanning depends on browser support; typed ID search remains available. Offline work stays on the originating device until synchronized.
