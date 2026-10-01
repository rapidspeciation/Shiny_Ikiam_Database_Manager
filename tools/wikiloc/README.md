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

The old PC service can be disabled once the server worker is verified:

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
