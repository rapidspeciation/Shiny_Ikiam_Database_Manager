// The assistant's weather forecast (docs: assistant/skills/weather/SKILL.md):
//   weather_forecast  the next days' forecast for a collecting site: each day's summary, the field hours hour by hour,
//                     and the monitoring window (9:00–11:00), from Open-Meteo's free API.
// The site is a Collection_location of Location_data (its coordinates), a lat/lon, or a place name found with
// Open-Meteo's geocoding (Ecuador first). Answers are kept an hour per point and number of days; Open-Meteo's
// free API is for non-commercial use (under 10 000 calls a day, 5 000 an hour, 600 a minute), its data CC BY 4.0,
// credited in every answer. Data only: which days look dry is the assistant's reading of the numbers.

import { places } from './records-tool.mjs';

const FORECAST_URL = 'https://api.open-meteo.com/v1/forecast';
const GEOCODE_URL = 'https://geocoding-api.open-meteo.com/v1/search';
export const TIMEZONE = 'America/Guayaquil';
export const SOURCE = 'Weather data by Open-Meteo.com (CC BY 4.0, https://open-meteo.com/), model best_match';
/** The monitoring walks: always 9:00 to 11:00 (assistant/skills/monitoring). */
export const MONITORING = { from: 9, to: 11 };
const HOURLY = ['temperature_2m', 'precipitation_probability', 'precipitation', 'cloud_cover', 'wind_speed_10m'];
const DAILY = ['temperature_2m_min', 'temperature_2m_max', 'precipitation_sum', 'precipitation_probability_max'];
const WEEKDAYS = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'];

export const WEATHER_TOOLS = [
  {
    type: 'function',
    function: {
      name: 'weather_forecast',
      description: [
        "The weather forecast for a collecting site, today and the next days (Open-Meteo, America/Guayaquil time). For «¿cómo estará el clima mañana en Ikiam?» or «¿qué día de esta semana conviene salir a colectar a Jatun Sacha?».",
        "- The site: `place`, a Collection_location of the database's Location_data (its coordinates are used) or any place name (then Open-Meteo's geocoding, Ecuador first); or `lat` + `lon`. The answer's `place.matched_by` says which, with `other_matches`.",
        '- `days`: 1 to 8 counted from today (today is the first; tomorrow needs 2; «this week» 7 or 8). `from_hour`/`to_hour`: the field hours, default 7 to 17.',
        "- Returns per day: date (dd/mm/yyyy) and weekday, min/max temperature (°C), the day's precipitation (mm) and its highest hourly probability (%); `hours` rows [hour, temp °C, precipitation probability %, precipitation mm, cloud %, wind km/h]; `monitoring` (9:00–11:00). Precipitation at an hour is the hour that ends then. `source` is the credit to give.",
        "- The numbers are a model's forecast for a grid cell (`grid`, its elevation can differ from the site's): say so when it matters, and read dry or wet hours from the numbers themselves.",
      ].join('\n'),
      parameters: {
        type: 'object',
        properties: {
          place: { type: 'string' },
          lat: { type: 'number' },
          lon: { type: 'number' },
          days: { type: 'integer', description: '1 to 8, default 3' },
          from_hour: { type: 'integer', description: '0 to 23, default 7' },
          to_hour: { type: 'integer', description: '0 to 23, default 17' },
        },
      },
    },
  },
];
export const WEATHER_TOOL_NAMES = new Set(WEATHER_TOOLS.map(t => t.function.name));

const fold = value =>
  String(value ?? '')
    .toLowerCase()
    .normalize('NFD')
    .replace(/[̀-ͯ]/g, '')
    .replace(/\s+/g, ' ')
    .trim();
const coordinate = (value, limit) => (value !== null && value !== '' && Number.isFinite(Number(value)) && Math.abs(Number(value)) <= limit ? Number(value) : null);
const round = (x, d = 1) => (x === null || x === undefined || !Number.isFinite(x) ? null : Math.round(x * 10 ** d) / 10 ** d);
/** "2026-11-02" → "02/11/2026". */
const dayFirst = iso => `${iso.slice(8, 10)}/${iso.slice(5, 7)}/${iso.slice(0, 4)}`;
const weekday = iso => WEEKDAYS[new Date(`${iso}T12:00:00Z`).getUTCDay()];
/** A moment as Guayaquil's clock shows it (UTC−5 all year): "dd/mm/yyyy HH:MM". */
const localTime = ms => {
  const iso = new Date(ms - 5 * 3600_000).toISOString();
  return `${dayFirst(iso)} ${iso.slice(11, 16)}`;
};
const hourOf = time => Number(time.slice(11, 13));
const pad = h => `${String(h).padStart(2, '0')}:00`;

/** The day's hours as rows, and the monitoring window's line, from Open-Meteo's hourly arrays. Pure. */
export function dayParts(hourly, date, { from = 7, to = 17 } = {}) {
  const at = [];
  hourly.time.forEach((t, i) => t.startsWith(date) && at.push(i));
  const value = (key, i) => hourly[key]?.[i] ?? null;
  const row = i => [
    pad(hourOf(hourly.time[i])),
    round(value('temperature_2m', i)),
    value('precipitation_probability', i),
    round(value('precipitation', i)),
    value('cloud_cover', i),
    round(value('wind_speed_10m', i)),
  ];
  const hours = at.filter(i => hourOf(hourly.time[i]) >= from && hourOf(hourly.time[i]) <= to).map(row);
  // 9:00–11:00: temperature, cloud and wind at 9, 10 and 11; precipitation of the hours ending at 10 and 11.
  const inWindow = at.filter(i => hourOf(hourly.time[i]) >= MONITORING.from && hourOf(hourly.time[i]) <= MONITORING.to);
  const rainHours = inWindow.filter(i => hourOf(hourly.time[i]) > MONITORING.from);
  const nums = (key, list) => list.map(i => value(key, i)).filter(v => v !== null);
  const temps = nums('temperature_2m', inWindow);
  const probs = nums('precipitation_probability', rainHours);
  const mm = nums('precipitation', rainHours);
  const clouds = nums('cloud_cover', inWindow);
  const winds = nums('wind_speed_10m', inWindow);
  const monitoring = inWindow.length
    ? {
        window: `${pad(MONITORING.from)}–${pad(MONITORING.to)}`,
        temp_c: temps.length ? [round(Math.min(...temps)), round(Math.max(...temps))] : null,
        precip_prob_max: probs.length ? Math.max(...probs) : null,
        precip_mm: mm.length ? round(mm.reduce((a, b) => a + b, 0)) : null,
        cloud_mean: clouds.length ? Math.round(clouds.reduce((a, b) => a + b, 0) / clouds.length) : null,
        wind_max_kmh: winds.length ? round(Math.max(...winds)) : null,
      }
    : null;
  return { hours, monitoring };
}

/** Open-Meteo's answer as the tool gives it: one entry per day. Pure. */
export function forecastDays(data, { from = 7, to = 17 } = {}) {
  const daily = data?.daily;
  const hourly = data?.hourly;
  if (!daily?.time?.length || !hourly?.time?.length) return null;
  return daily.time.map((date, i) => ({
    date: dayFirst(date),
    weekday: weekday(date),
    temp_min_c: round(daily.temperature_2m_min?.[i]),
    temp_max_c: round(daily.temperature_2m_max?.[i]),
    precip_mm: round(daily.precipitation_sum?.[i]),
    precip_prob_max: daily.precipitation_probability_max?.[i] ?? null,
    ...dayParts(hourly, date, { from, to }),
  }));
}

export function createWeatherTool({ store = null, fetch: fetchFn = globalThis.fetch, now = () => Date.now(), timeoutMs = 8000, cacheMs = 3600_000 } = {}) {
  const cache = new Map();

  async function getJson(url) {
    const hit = cache.get(url);
    if (hit && now() - hit.at < cacheMs) return { ...hit.value, cached: true };
    const controller = new AbortController();
    const timer = setTimeout(() => controller.abort(), timeoutMs);
    try {
      const res = await fetchFn(url, { signal: controller.signal, headers: { accept: 'application/json' } });
      const body = await res.json().catch(() => null);
      if (!res.ok) throw new Error(`Open-Meteo answered ${res.status}${body?.reason ? `: ${body.reason}` : ''}`);
      const value = { body, at: now() };
      if (cache.size > 300) for (const [k, v] of cache) if (now() - v.at >= cacheMs) cache.delete(k);
      cache.set(url, { at: value.at, value });
      return value;
    } catch (e) {
      if (controller.signal.aborted) throw new Error(`Open-Meteo did not answer within ${timeoutMs / 1000} s`);
      throw e;
    } finally {
      clearTimeout(timer);
    }
  }

  /** The database's localities with coordinates: an exact name, else the names containing it (shortest first). */
  function fromDatabase(name) {
    let known;
    try {
      known = store?.db ? places(store.db) : new Map();
    } catch {
      known = new Map();
    }
    const key = fold(name);
    const exact = known.get(key);
    const hits = [...known.values()].filter(p => p !== exact && fold(p.name).includes(key)).sort((a, b) => a.name.length - b.name.length);
    const best = exact ?? hits.shift();
    if (!best) return null;
    return {
      name: best.name,
      lat: best.lat,
      lon: best.lon,
      matched_by: 'database locality (Location_data)',
      ...(hits.length ? { other_matches: hits.slice(0, 8).map(p => p.name) } : {}),
    };
  }

  async function fromGeocoding(name) {
    const url = country =>
      `${GEOCODE_URL}?${new URLSearchParams({ name, count: '5', language: 'es', format: 'json', ...(country ? { countryCode: country } : {}) })}`;
    let out = (await getJson(url('EC'))).body?.results ?? [];
    let abroad = false;
    if (!out.length) {
      out = (await getJson(url(null))).body?.results ?? [];
      abroad = out.length > 0;
    }
    if (!out.length) return null;
    const label = r => [r.name, r.admin2 !== r.name ? r.admin2 : null, r.admin1, r.country].filter(Boolean).join(', ');
    const [best, ...rest] = out;
    return {
      name: label(best),
      lat: best.latitude,
      lon: best.longitude,
      ...(Number.isFinite(best.elevation) ? { elevation_m: best.elevation } : {}),
      matched_by: abroad ? 'Open-Meteo geocoding (outside Ecuador: no match in Ecuador)' : 'Open-Meteo geocoding (Ecuador)',
      ...(rest.length ? { other_matches: rest.slice(0, 4).map(label) } : {}),
    };
  }

  async function run(name, args = {}) {
    if (!WEATHER_TOOL_NAMES.has(name)) return { error: `Unknown tool ${name}` };
    const days = args.days === undefined || args.days === null ? 3 : Math.trunc(Number(args.days));
    if (!(days >= 1 && days <= 8)) return { error: 'days must be 1 to 8 (today is the first)' };
    const from = args.from_hour === undefined || args.from_hour === null ? 7 : Math.trunc(Number(args.from_hour));
    const to = args.to_hour === undefined || args.to_hour === null ? 17 : Math.trunc(Number(args.to_hour));
    if (!(from >= 0 && to <= 23 && from <= to)) return { error: 'from_hour and to_hour must be 0 to 23, from_hour first' };

    let place;
    const lat = coordinate(args.lat, 90);
    const lon = coordinate(args.lon, 180);
    const named = typeof args.place === 'string' && args.place.trim() ? args.place.trim() : null;
    if (lat !== null && lon !== null) place = { ...(named ? { name: named } : {}), lat, lon, matched_by: 'coordinates given' };
    else if (named) {
      place = fromDatabase(named);
      if (!place) {
        try {
          place = await fromGeocoding(named);
        } catch (e) {
          return { error: `${named} is not among the database's localities with coordinates, and the place search failed (${e.message}). Give lat and lon.`, source: SOURCE };
        }
      }
      if (!place) return { error: `No place «${named}» among the database's localities with coordinates (Location_data) nor in Open-Meteo's geocoding. Give lat and lon.` };
    } else return { error: 'Give place (a Collection_location or a place name) or lat and lon' };

    // About 1 km: the forecast grid is coarser, and nearby asks share the hour's answer.
    const qlat = round(place.lat, 2);
    const qlon = round(place.lon, 2);
    const url = `${FORECAST_URL}?${new URLSearchParams({
      latitude: String(qlat),
      longitude: String(qlon),
      hourly: HOURLY.join(','),
      daily: DAILY.join(','),
      timezone: TIMEZONE,
      forecast_days: String(days),
    })}`;
    let got;
    try {
      got = await getJson(url);
    } catch (e) {
      return { error: `${e.message}; try again in a minute.`, place, source: SOURCE };
    }
    const list = forecastDays(got.body, { from, to });
    if (!list) return { error: 'Open-Meteo answered without a forecast; try again in a minute.', place, source: SOURCE };
    return {
      place: { ...place, lat: qlat, lon: qlon },
      grid: { lat: round(got.body.latitude, 3), lon: round(got.body.longitude, 3), elevation_m: got.body.elevation ?? null },
      timezone: TIMEZONE,
      fetched: localTime(got.at),
      ...(got.cached ? { cached: true } : {}),
      issued: 'not given: best_match blends the models that cover the place; the forecast is the latest Open-Meteo had at `fetched`',
      field_hours: `${pad(from)}–${pad(to)}`,
      hour_columns: ['hour', 'temp_c', 'precip_prob_%', 'precip_mm', 'cloud_%', 'wind_kmh'],
      hour_note: 'Precipitation and its probability at an hour are for the hour ending then; temperature, cloud and wind at that moment.',
      days: list,
      source: SOURCE,
    };
  }

  return { run };
}
