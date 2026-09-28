# Wikiloc helper

Wikiloc has no API, and its Cloudflare check blocks the app server, but a
browser on a home connection passes. These scripts open the **public** trail
pages in a headless browser on such a computer and send what they find to the
app: each waypoint's note, position and photos, and the trail line (no GPS
times; the GPX has those). No Wikiloc login is used. Walks wait in
Monitoreo → Importar recorrido for review; the app server downloads the photos.
Use it only for the team's own trails; pages are opened a few seconds apart.

## Processor (links pasted or shared in the app, "Buscar nuevos en Wikiloc")

Runs on a computer that stays on (Franz's PC). Every 30 s it asks the app for
work: a link job reads that trail, a profile job lists the followed profile's
public trails and reads those whose title matches the profile's pattern
(default "monitoreo") and that are not in the app yet.

1. An app account for the processor (role editor), e.g. `wikiloc-worker`.
2. `~/.config/ithomiini-wikiloc/worker.json`, mode 600:
   `{"app": "https://ithomiini-ikiam.duckdns.org/", "username": "wikiloc-worker", "password": "…"}`
3. `tools/wikiloc/install-worker.sh` (copies the helper to
   `~/.local/share/ithomiini-wikiloc` and starts the systemd user service
   `ithomiini-wikiloc`; with `loginctl enable-linger` it runs without a login).

The Importar screen shows whether the processor was seen in the last two minutes.

## One-off from the command line

```
npm --prefix tools/wikiloc install
npm run wikiloc -- <trail link> [more links] [--dry-run] [--app URL]
```

Asks for an app login once (kept in `~/.config/ithomiini-wikiloc/session.json`, mode 600).
