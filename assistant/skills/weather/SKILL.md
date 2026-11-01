---
name: weather
description: The weather forecast for a collecting site (a locality of the database, a place name or coordinates) for today and up to a week ahead, hour by hour in the field hours and in the monitoring window (9:00–11:00). Use it for «¿cómo estará el clima mañana en Ikiam?», «¿qué día de esta semana conviene salir a colectar a Jatun Sacha?», «¿va a llover en el monitoreo?».
---

# Weather forecast

`weather_forecast` reads Open-Meteo's forecast (the free, non-commercial API)
for a site. Nothing is written.

| Question | Call |
|---|---|
| Tomorrow at a site | `place`, `days: 2` (today is the first day) |
| Which day this week | `place`, `days: 7` (or 8 to reach the same weekday next week) |
| Early or late field work | `from_hour`/`to_hour` (default 7 to 17) |
| A point with no name in the database | `lat` + `lon` the person gives (and `place` as its label) |

## The site

Give `place` as the person names it. The tool looks first among the
database's localities with coordinates (Location_data), then in Open-Meteo's
place search, Ecuador first. Say which one was used when it is not plainly the
place asked (`place.matched_by`, `place.name`, `other_matches`): «Usé Tena,
Napo (búsqueda de Open-Meteo); no encontré en la base una localidad con ese
nombre y coordenadas.» A partial name or a nearby town stands in for the site: say so and
offer to repeat it with the site's coordinates from the person. A match
outside Ecuador (`warning`) is nearly always the wrong place: ask.

## Reading it

- Each day: min/max temperature, the day's rain (mm) and its highest hourly
  chance of rain; `hours` rows `[hour, °C, rain chance %, rain mm, cloud %,
  wind km/h]`; `monitoring` for 9:00–11:00; `field_precip_mm` and
  `field_precip_prob_max` for the field hours. Rain at an hour is the rain of
  the hour ending then. Today's hours before `past_until` have gone: they are
  the model's, not what fell.
- Which days or hours look dry: compare the days with each other from these
  numbers (lowest chance and fewest mm in the field hours), and quote the
  numbers behind the choice. How much rain calls off a walk is the team's
  call: when the days are close, say so and leave it to whoever goes.
- It is a model's forecast for its nearest grid point (`grid`: a few km
  from the site, at its own `elevation_m`): in the Amazon showers are local
  and hard to time, and days 4 to 7 are less sure than tomorrow. Say it once when the
  choice rests on small differences.
- Credit the source once: «Datos: Open-Meteo.com».

## Answer

Short: the day or days asked, then the hours that matter. For «¿qué día
conviene?» a small table of the days (date dd/mm/yyyy, weekday, °C, rain mm, rain
chance in 9–11 and in the field hours) and one line with the best one or two.
The form (figures made up):

- «Mañana (lun 02/11/2026) en Ikiam: 21–29 °C; 3 mm en el día. Monitoreo 9–11:
  0 mm, 35–40 % de probabilidad, nublado 80 %. La lluvia viene después de las
  14:00 (60–70 %). Datos: Open-Meteo.com»
