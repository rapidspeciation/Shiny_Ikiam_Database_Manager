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

Camoufox runs on a private virtual display provided by Xvfb, and clicks Cloudflare's
checkbox, when one shows, with real X input (below). It keeps the same browser context
throughout a job. Page variables such as `window.mapData` are read through Camoufox's
main-world evaluation. Browser processes and the display close when the job completes or fails.

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

Cloudflare sometimes shows its "Verify you are human" checkbox (the 2026 layout: "Performing
security verification", the widget's iframe in a closed shadow root) to the server's address, a
data-centre IP, and sometimes not: on 16 Oct 2026 three tries of one walk stopped at it; on 19 Oct
the same walk loaded with no check. Its click solver (playwright-captcha) never found the widget
("Cloudflare iframes not found"), and Playwright's own clicks reach the box but the check then
fails from the server's address. What passes is a real click: Camoufox on a private Xvfb display
and pointer events from the X server itself (XTEST, xinput.py), moving like a hand.

Measured on 19 Oct 2026 (fresh browser each try; identi.io always shows the checkbox):

| From | Browser and click | identi.io | Wikiloc walk |
|---|---|---|---|
| claudeclaw | Camoufox headless, Playwright click | 0/5 | — |
| claudeclaw | Camoufox (Windows) on Xvfb, Playwright click | 0/5 | — |
| claudeclaw | same, X click straight to the box | 1/5 | — |
| claudeclaw | same, X click moving like a hand | 2/5 (~25 s) | 5/5 (no check that day) |
| claudeclaw | same, hand + idle moves and a later first click | 1/5 | — |
| claudeclaw | browser.py as shipped (X hand click, reload once) | 1/5 | 5/5; the two checks Wikiloc showed meanwhile cleared in 23 s (2 clicks) |
| home PC | Playwright Chromium headless, click | 0/3 | (passed before, after the home page) |
| home PC | Camoufox (Windows) headless, Playwright click | 5/5 (~10 s) | 5/5 (~6 s) |
| home PC | browser.py as shipped (no Xvfb: headless, page click) | 5/5 | 5/5 |

So the address weighs most: from a home connection the same browser passes almost always; from
the server, Wikiloc's checkbox has passed with real clicks so far, a stricter site's only
sometimes. Hence:

- browser.py: Camoufox presenting as Firefox on Windows; on a machine with Xvfb it runs on its own
  display and clicks the checkbox with X input (up to 4 clicks, ~10 s apart), else headless with
  page clicks; Wikiloc's home page first; a check that does not clear is tried once more on a
  fresh load. What it saw goes to the worker's log (`browser: check on …`, `checkbox clicked (X
  input), click 2`, `check cleared in 23 s`), and a screenshot of a page that did not clear is kept
  in `~/.cache/ithomiini-wikiloc/failures/` (the latest five).
- A job Cloudflare stopped goes back to the queue and is tried again after 5 and 20 minutes
  (`retry`, server/monitoring.mjs `RETRY_WAITS_MIN`), then fails. Importar shows when.
- A home computer can take those jobs at once as a fallback (below): the same worker with
  `"fallback": true`, Camoufox headless from a home connection. Importar shows «respaldo del PC
  activo».

Techniques read (not run, not added as dependencies) in open-source solvers, 19 Oct 2026:
EzSolver (MIT; real Chrome on Xvfb through nodriver, CDP click ~28 px into the widget),
cloudflare-solver by art3m4ik3 (MIT; Camoufox with a Windows fingerprint, jittered clicks),
turnaround (GPL-3; Playwright Chromium, opens the iframe's DOM, which a closed shadow root no
longer allows), Cloudflare-challenge (MIT; needs the closed CloakBrowser binary). xinput.py and
browser.py are our own code, taking the click position and the Windows fingerprint from the two
MIT ones.

### Fallback on the home computer (optional)

On a computer with Node 24 and Python 3 (Camoufox's own Firefox is downloaded once, ~100 MB):

```sh
dest=~/.local/share/ithomiini-wikiloc-fallback
mkdir -p "$dest" ~/.config/ithomiini-wikiloc
cp tools/wikiloc/{lib.mjs,worker.mjs,browser.mjs,chromium.mjs,browser.py,xinput.py,requirements.txt,package.json} "$dest/"
python3 -m venv "$dest/venv" && "$dest/venv/bin/pip" install -q -r "$dest/requirements.txt"
"$dest/venv/bin/python" -m camoufox fetch official/152.0.4-beta.31
# The editor account the server's worker uses, plus "fallback": true (mode 600):
#   {"app":"https://ithomiini-ikiam.com/","username":"wikiloc-worker","password":"…","fallback":true}
$EDITOR ~/.config/ithomiini-wikiloc/fallback.json && chmod 600 ~/.config/ithomiini-wikiloc/fallback.json
cat > ~/.config/systemd/user/ithomiini-wikiloc-fallback.service <<UNIT
[Unit]
Description=Ithomiini: Wikiloc walks the server could not read (fallback)
After=network-online.target
[Service]
ExecStart=$(command -v node) $dest/worker.mjs
Environment=ITHOMIINI_WORKER_CONFIG=%h/.config/ithomiini-wikiloc/fallback.json
Environment=ITHOMIINI_WIKILOC_PYTHON=$dest/venv/bin/python
Restart=always
RestartSec=60
[Install]
WantedBy=default.target
UNIT
systemctl --user daemon-reload && systemctl --user enable --now ithomiini-wikiloc-fallback
```

Without Xvfb on the computer the browser runs headless and clicks through the page, which is what
passes from a home connection. `ITHOMIINI_WIKILOC_BROWSER=chromium` uses Playwright's Chromium
instead (chromium.mjs), which did not pass identi.io's checkbox.

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
