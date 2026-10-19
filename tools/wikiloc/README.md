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
| claudeclaw, direct, time zone of the server's address (New York) | browser.py | 2/5 | — |
| claudeclaw through the home tunnel, server's zone (UTC) | browser.py (X click) | 1/5 | 4/5 (a check each time, 2 clicks, ~26 s; the miss: an interrupted navigation, now waited for) |
| claudeclaw through the home tunnel, Ecuador's zone and es-EC | browser.py as shipped (X click) | **5/5** (~30 s, 1 click) | **5/5** (19–29 s, 1 click when checked) |
| claudeclaw through the home tunnel, Ecuador's zone | headless, page click | 5/5 (32–167 s) | — |

So the address weighs most, and with it the browser's time zone and language matching that
address: through the home computer's tunnel with the server's UTC the check still failed 4 times
of 5; with Ecuador's zone and es-EC it passed every time. Hence:

- browser.py: Camoufox presenting as Firefox on Windows; on a machine with Xvfb it runs on its own
  display and clicks the checkbox with X input (up to 4 clicks, ~10 s apart), else headless with
  page clicks; Wikiloc's home page first; a check that does not clear is tried once more on a
  fresh load. What it saw goes to the worker's log (`browser: check on …`, `checkbox clicked (X
  input), click 2`, `check cleared in 23 s`), and a screenshot of a page that did not clear is kept
  in `~/.cache/ithomiini-wikiloc/failures/` (the latest five).
- A job Cloudflare stopped goes back to the queue and is tried again after 5 and 20 minutes
  (`retry`, server/monitoring.mjs `RETRY_WAITS_MIN`), then fails. Importar shows when.
- When the home computer's tunnel is up (below), the browser goes out through its home address:
  the route is chosen per job (`browser: route: home computer (SOCKS tunnel)` or `this server,
  direct`) and said in the job's message («vía el PC de casa» / «directo desde el servidor»).

Techniques read (not run, not added as dependencies) in open-source solvers, 19 Oct 2026:
EzSolver (MIT; real Chrome on Xvfb through nodriver, CDP click ~28 px into the widget),
cloudflare-solver by art3m4ik3 (MIT; Camoufox with a Windows fingerprint, jittered clicks),
turnaround (GPL-3; Playwright Chromium, opens the iframe's DOM, which a closed shadow root no
longer allows), Cloudflare-challenge (MIT; needs the closed CloakBrowser binary). xinput.py and
browser.py are our own code, taking the click position and the Windows fingerprint from the two
MIT ones.

## Through the home computer

The server keeps doing all the work; only its Wikiloc browser's traffic leaves through the home
computer's address, which Cloudflare lets through. The app server is not on the tailnet, so the
home computer opens the connection: an SSH session to the server with a *reverse dynamic* forward
(`ssh -R 127.0.0.1:18080`, OpenSSH 7.6+), which makes a SOCKS proxy on the server's
127.0.0.1:18080 whose connections leave from the home computer. Nothing new is installed on either
machine, and no sshd setting changes.

- The key can only do that: in the server's `authorized_keys` it is
  `restrict,port-forwarding,permitlisten="127.0.0.1:18080",permitopen="127.0.0.1:9",command="/bin/false"`
  (no shell, no other listening port, no forward into the server). The home computer's ssh lets
  the proxy reach web ports only (`PermitRemoteOpen=*:443 *:80`).
- browser.py checks the port with a SOCKS greeting at the start of each job; if it answers,
  Camoufox uses it, with names resolved at the far end (`network.proxy.socks_remote_dns`), so
  Cloudflare sees one address for the page and its lookups. If not (the computer is off), the job
  goes direct, with the real-click solver and the 5/20-minute retries.
  `ITHOMIINI_WIKILOC_PROXY` sets another address (`off`: never).
- Through the tunnel the browser takes the home computer's time zone and language
  (`ITHOMIINI_WIKILOC_HOME_ZONE`, default America/Guayaquil; `ITHOMIINI_WIKILOC_HOME_LOCALE`,
  es-EC): with the server's UTC behind an Ecuador address the check failed most times. The direct
  route can be given its own (`ITHOMIINI_WIKILOC_ZONE`, `_LOCALE`), which did not change much.
- The other fetches: the walk's photos come from Wikiloc's image CDN (wklcdn.com), which the app
  server downloads directly (no check there); `fetch.mjs` (one-off imports) uses the same browser,
  so the same route; GPX files come from people's devices.

Tested 19 Oct 2026 with a temporary key: from the server the proxy's exit was the home address
(Ecuador), web ports went through, port 22 did not, another listening port, a shell and a forward
into the server were refused, and the browser's requests carried names (resolved at home).

### Deploy steps

On the home computer (franz-gpdwin4), from the repository:

```sh
bash tools/wikiloc/home-tunnel.sh             # makes ~/.ssh/ithomiini_wikiloc_tunnel, prints the server's line
```

On the app server (claudeclaw), append that printed line to `~/.ssh/authorized_keys` of `ubuntu`
(one line, ending in `ithomiini-wikiloc-tunnel`). Then on the home computer:

```sh
bash tools/wikiloc/home-tunnel.sh --install   # user service ithomiini-wikiloc-tunnel, restarts on failure
loginctl enable-linger "$USER"                # only if it should run with nobody logged in
```

Check on the server: `curl -s --socks5-hostname 127.0.0.1:18080 https://www.cloudflare.com/cdn-cgi/trace | grep -E '^(ip|loc)='`
gives the home address. The next deploy of the app ships browser.py (the installer reruns).
To undo: `bash tools/wikiloc/home-tunnel.sh --remove` and delete the line on the server.

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
