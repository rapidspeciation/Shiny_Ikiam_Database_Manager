# Wikiloc importer

The app server runs a Camoufox Firefox browser to read the team's public
Wikiloc trails. It extracts waypoint notes, positions, photo URLs and the trail
line. The app downloads the photos and keeps each walk in
Monitoreo → Importar recorrido for review. Public pages have no GPS timestamps;
GPX imports still supply those. No Wikiloc login is used.

## Server worker

The `ithomiini-wikiloc` systemd user service runs on claudeclaw. Every 30 seconds
it asks the app for a job. A link job reads that trail. A profile job lists the
followed profile's public trails and imports new trails matching its title
pattern, with a four-second pause between trails.

Camoufox runs on a private virtual display provided by Xvfb. It keeps the same
browser context throughout a job and uses `playwright-captcha` for Cloudflare
challenges. Page variables such as `window.mapData` are read through Camoufox's
main-world evaluation. Browser processes close when the job completes or fails.

The app's existing `wikiloc-worker` editor account authenticates requests. Its
settings live in `~/.config/ithomiini-wikiloc/worker.json`, mode 600:

```json
{"app":"https://ithomiini-ikiam.com/","username":"wikiloc-worker","password":"…"}
```

Install on the server with Node 24 in `PATH`:

```sh
sudo apt install xvfb python3-venv libgtk-3-0
bash tools/wikiloc/install-worker.sh
```

The installer copies the helper into `~/.local/share/ithomiini-wikiloc`, creates
its Python virtual environment, installs the pinned packages in
`requirements.txt`, fetches Camoufox 152.0.4-beta.31, and starts the service.
The user's systemd linger setting must be enabled for operation without a login.
The server already has it enabled. `scripts/deploy.sh` ships the helper and reruns
the installer on subsequent app deployments.

```sh
systemctl --user status ithomiini-wikiloc
journalctl --user -u ithomiini-wikiloc -n 60 --no-pager
```

The Importar screen shows the worker's latest heartbeat. Queued jobs remain in
the app database if the service is unavailable; jobs left running after an
interruption become eligible for another attempt after 15 minutes.

## When Cloudflare's check does not clear

Cloudflare sometimes shows a check to the server's address (a data-centre IP) and sometimes
not: on 16 Oct 2026 three tries of one walk in 14 minutes all stopped at it ("Cloudflare
challenge did not clear"), while on 19 Oct the same walk loaded with no check at all, with
nothing changed on the server (same Camoufox and packages since 30 Sep; no profile or cookies
are kept between jobs). So:

- The browser opens Wikiloc's home page first, and a check that does not clear is tried once
  more on a fresh load of the page. What it saw goes to the worker's log (`browser: check on …
  (turnstile), try 1`, `check cleared in 12 s` or `did not clear`), and a screenshot of a page
  that did not clear is kept in `~/.cache/ithomiini-wikiloc/failures/` (the latest five).
- A job Cloudflare stopped is not failed at once: it goes back to the queue and is tried again
  after 5 and 20 minutes (`retry`, server/monitoring.mjs `RETRY_WAITS_MIN`), then it fails. The
  Importar screen shows when.
- A home computer can help as a fallback: the same worker with `"fallback": true` takes only the
  jobs the server could not do, at once, with Playwright's Chromium from a home connection
  (Cloudflare lets that through after the home page). Importar shows «respaldo del PC activo».

### Fallback on the home computer (optional)

On a computer with Node 24 and Playwright (its Chromium downloaded):

```sh
mkdir -p ~/.local/share/ithomiini-wikiloc-fallback ~/.config/ithomiini-wikiloc
cp tools/wikiloc/{lib.mjs,worker.mjs,browser.mjs,chromium.mjs,browser.py,package.json} ~/.local/share/ithomiini-wikiloc-fallback/
# The editor account the server's worker uses, plus "fallback": true (mode 600):
#   {"app":"https://ithomiini-ikiam.com/","username":"wikiloc-worker","password":"…","fallback":true}
$EDITOR ~/.config/ithomiini-wikiloc/fallback.json && chmod 600 ~/.config/ithomiini-wikiloc/fallback.json
cat > ~/.config/systemd/user/ithomiini-wikiloc-fallback.service <<UNIT
[Unit]
Description=Ithomiini: Wikiloc walks the server could not read (fallback)
After=network-online.target
[Service]
ExecStart=$(command -v node) %h/.local/share/ithomiini-wikiloc-fallback/worker.mjs
Environment=ITHOMIINI_WORKER_CONFIG=%h/.config/ithomiini-wikiloc/fallback.json
Environment=ITHOMIINI_WIKILOC_BROWSER=chromium
Environment=ITHOMIINI_WIKILOC_PLAYWRIGHT=<path to playwright's index.mjs>
Restart=always
RestartSec=60
[Install]
WantedBy=default.target
UNIT
systemctl --user daemon-reload && systemctl --user enable --now ithomiini-wikiloc-fallback
```

The old PC service (`ithomiini-wikiloc`, the Chromium worker before Camoufox) stays disabled:

```sh
systemctl --user disable --now ithomiini-wikiloc
```

## One-off imports

After installing the Python requirements and fetching Camoufox:

```sh
ITHOMIINI_WIKILOC_PYTHON="$HOME/.local/share/ithomiini-wikiloc/venv/bin/python" \
  npm run wikiloc -- <trail link> [more links] [--dry-run] [--app URL]
```

Without the environment override, the helper uses `venv/bin/python` beside its
scripts. The CLI asks for an app login once and keeps the session in
`~/.config/ithomiini-wikiloc/session.json`, mode 600. `--dry-run` only reads the
trail and prints a summary.
