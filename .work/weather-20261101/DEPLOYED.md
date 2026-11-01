# Deployed: weather_forecast assistant tool (1 Nov 2026, 17:24 UTC)

- Release **20261101T172433Z** (branch weather-tool-20261101 merged with team main; main pushed to origin and team).
  Before: 20261101T171743Z. Health ok. Deploy lock taken after the domain-facts chat released it, then released.
  T3 workspaces refreshed by deploy.sh (8). claudeclaw reaches api.open-meteo.com (200 in 0.45 s).
- Rollback:
  `ssh claudeclaw 'cd /home/ubuntu/ithomiini && ln -sfn releases/20261101T171743Z current && systemctl --user restart ithomiini.service'`
  then `ssh claudeclaw 'cd /home/ubuntu/ithomiini/current && node=$(sed -n "s/^ExecStart=\([^ ]*\/node\) .*/\1/p" deploy/ithomiini.service) && "$node" scripts/t3-provision.mjs --refresh-all'`
- What: MCP tool `weather_forecast` (server/weather-tool.mjs) for the in-app assistant and WhatsApp: place = database
  locality with coordinates, Open-Meteo geocoding (Ecuador first, says which match) or lat/lon; 1–8 days; daily
  min/max, rain mm and max probability; field-hour rows (default 7–17, America/Guayaquil), field-hour rain totals, the
  9–11 monitoring line; grid point, fetch time, «Weather data by Open-Meteo.com» (CC BY 4.0). 1 h cache per local date,
  8 s timeout, failures not cached. Skill assistant/skills/weather: when to call it; «dry» judged from the numbers only.
- Tests: server 895/895 (14 new mocked-HTTP weather tests), frontend 746/746, type check, build. Lab T3: 2 chats, archived;
  no timers; nothing sent to WhatsApp.
- Reviewed with dual-critic-loop: 1 round inside the build (fresh Opus 7 points, Sol 5), all fixed (35944ef, e604355).
