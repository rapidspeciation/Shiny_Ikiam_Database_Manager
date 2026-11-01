# Weather forecast tool (1 Nov 2026)

## Design

- `server/weather-tool.mjs`: one read-only MCP tool, `weather_forecast`, registered with the other tools in
  `server/assistant.mjs` (so the in-app chats and the WhatsApp chat both have it). The WhatsApp bridge shows the
  step as «mirando el pronóstico del clima» (`tools/whatsapp/bridge-lib.mjs`).
- Input: `place` (a Collection_location of Location_data, else a place name) or `lat` + `lon`; `days` 1–8 counted
  from today (today = day 1, so «mañana» needs 2, «esta semana» 7–8); `from_hour`/`to_hour` (default 7–17).
- Site lookup: Location_data names with coordinates first (`places()` now exported from `server/records-tool.mjs`;
  exact folded name, else the shortest name containing it, the rest in `other_matches`); else Open-Meteo geocoding
  with `countryCode=EC`, then worldwide (marked «outside Ecuador»). `place.matched_by` always says which.
- Forecast: `api.open-meteo.com/v1/forecast`, model `best_match`, `timezone=America/Guayaquil`, hourly temperature,
  precipitation probability, precipitation, cloud cover, wind at 10 m; daily min/max, precipitation sum and max
  probability. Coordinates rounded to 0.01° (≈1 km) for the request and cache key.
- Output per day: date dd/mm/yyyy, weekday, min/max °C, precipitation mm, max probability %, `hours` rows
  `[hour, °C, prob %, mm, cloud %, wind km/h]` for the field hours, and `monitoring` (09:00–11:00: temperature range
  and mean cloud at 9/10/11; rain and its probability for the hours ending at 10 and 11, since Open-Meteo's hourly
  precipitation and probability are for the preceding hour). Plus `grid` (the model point used and its elevation),
  `fetched` (local time), `source`. No «dry» thresholds: the skill tells the AI to compare the numbers and quote them.
- Issue time: `best_match` blends the models covering the place and its answer has no run time (checked live: its
  grid point matched none of GFS, ECMWF IFS 0.25° or ICON). The answer says so in `issued` and gives `fetched`.
  Pinning one model would give a run time (its `/data/<model>/static/meta.json`), at the cost of the blend.
- Cache: 1 h per request URL (point, days; hours are cut from the cached answer), geocoding too. Timeout 8 s
  (AbortController), then `{ error: "Open-Meteo did not answer within 8 s; try again in a minute.", place, source }`;
  HTTP errors carry Open-Meteo's `reason`. No new dependency (global fetch).
- Docs: `assistant/skills/weather/SKILL.md` (when to call, how to read, answer form) and a row in the brief's skill
  table (`assistant/AGENTS.md`). No UI change.

## Open-Meteo terms (checked 1 Nov 2026, https://open-meteo.com/en/terms)

- Free API only for non-commercial use: under 10 000 calls a day, 5 000 an hour, 600 a minute. The team's academic
  use with a 1 h cache stays far below.
- Data under CC BY 4.0: credit «Weather data by Open-Meteo.com» with a link. The tool's `source` carries it; the
  skill asks for «Datos: Open-Meteo.com» once per answer.

## Tests

- `tests/weather-tool.test.mjs` (9 tests, mocked fetch): tool offered over MCP + WhatsApp step label; locality lookup,
  day-first dates, weekday, field-hour rows, monitoring window; other hours and argument checks; partial and
  accent-free names; lat/lon input; geocoding fallback in Ecuador and «not found»; cache hit and expiry; timeout and
  HTTP 429; a missing hourly value.
- Full server suite (`node --test tests/*.test.mjs`): 890/890 pass on 95b6641; the weather file 14/14 after both
  reviews' fixes (with assistant-tools and whatsapp-bridge: 59/59). Frontend untouched (no `npm --prefix frontend test`).
- Live: `~/.local/share/ui-overhaul-20261031/weather/live-ikiam.json`, `live-tena-geocoded.json`, `live-raw-ikiam.json`
  (raw API). Tool output matched the raw answer (02/11: 21.0–29.3 °C, 3.0 mm, 73 %).

## Reviews

- Opus (fresh): nothing serious; 7 points fixed in 35944ef: unreadable/empty answers were cached for an hour;
  «Give lat and lon» invited made-up coordinates (now: ask the person, or the town's name alone); partial locality
  names looked exact (now whole words, ≥3 letters, `matched_by` says «partial name»); the 07:00 row's rain fell
  before the field hours (new `field_precip_mm`/`field_precip_prob_max` from the hours after `from_hour`); today's
  gone hours unmarked (`past_until`, `monitoring_past`); a match abroad only in `matched_by` (now `warning`); tests.
- GPT-6.1 Sol: 5 points. Body-read timeout cached: already fixed in 35944ef. Fixed after: the cache key adds the
  Guayaquil date (a forecast cached at 23:50 is not reused after midnight); 9–11 and field-hour rain totals are
  null unless every hour has a value (a missing hour no longer looks like 0 mm); skill wording «no encontré en la
  base una localidad con ese nombre y coordenadas»; dates in the skill's table and example dd/mm/yyyy.

## Lab assistant test

Lab T3 :3775, a new workspace `weathertest` provisioned with `scripts/t3-provision.mjs` against the dev app :8891
(this branch, before the review fixes), chats sent with the lab's `t3 thread start --wait`, then archived through
`tools/whatsapp/t3-turn.mjs` `archive`. Timers before/after identical; nothing sent to WhatsApp. Full answers:
`~/.local/share/ui-overhaul-20261031/weather/answers.md`. Example:

> Mañana (lun 02/11/2026) en Ikiam: 21–29 °C, con 3 mm de lluvia en todo el día. […] Monitoreo 9–11: 24–27 °C,
> 0,1 mm, máximo 12 % de probabilidad, nublado en promedio 73 %, poco viento. Tarde: la probabilidad de lluvia sube
> desde las 14:00 (46 %) hasta 64–73 % entre las 15:00 y las 17:00. […] Datos: Open-Meteo.com

«¿Qué día de esta semana conviene salir a colectar a Jatun Sacha?»: Jatun Sacha is not a Location_data locality, so
it used Open-Meteo's «Ahuano, Napo», said so and offered to repeat with the station's coordinates; a 6-day table and
«miércoles 04/11» as best (31 % max in field hours), with the small-differences caveat.

Leftovers: the `weathertest` workspace and project stay in the lab T3 (archived chats); `web/` copied from the main
checkout into the worktree (git-ignored) so the dev app could start without `node_modules`.
