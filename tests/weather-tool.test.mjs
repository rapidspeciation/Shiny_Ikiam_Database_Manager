import test from 'node:test';
import assert from 'node:assert/strict';
import { Store } from '../server/store.mjs';
import { LocalSheets } from '../server/sheets.mjs';
import { mcpTools } from '../server/assistant.mjs';
import { stepLabel } from '../tools/whatsapp/bridge-lib.mjs';
import { WEATHER_TOOL_NAMES, createWeatherTool, dayParts } from '../server/weather-tool.mjs';

const seed = {
  Location_data: [
    { row: 2, values: { Collection_location: 'Ikiam', Latitude: -0.948557, Longitude: -77.86605 } },
    { row: 3, values: { Collection_location: 'Mariposario Ikiam', Latitude: -0.94839, Longitude: -77.865074 } },
    { row: 4, values: { Collection_location: 'Añangu, Yasuní', Latitude: -0.524863, Longitude: -76.384183 } },
    { row: 5, values: { Collection_location: 'Sin coordenadas' } },
  ],
};

/** Open-Meteo's answer for `days` days from 01/11/2026 (local times, as with timezone=America/Guayaquil). */
function forecast(days = 2) {
  const time = [];
  const dates = [];
  for (let d = 0; d < days; d++) {
    const date = `2026-11-${String(d + 1).padStart(2, '0')}`;
    dates.push(date);
    for (let h = 0; h < 24; h++) time.push(`${date}T${String(h).padStart(2, '0')}:00`);
  }
  const hour = t => Number(t.slice(11, 13));
  return {
    latitude: -0.9490334,
    longitude: -77.82062,
    elevation: 633,
    timezone: 'America/Guayaquil',
    hourly: {
      time,
      temperature_2m: time.map(t => 20 + hour(t) / 2),
      precipitation_probability: time.map(t => hour(t) * 4),
      precipitation: time.map(t => (hour(t) === 10 ? 1.25 : hour(t) === 11 ? 0.5 : hour(t) === 9 ? 9 : 0)),
      cloud_cover: time.map(t => (hour(t) >= 9 && hour(t) <= 11 ? 60 + (hour(t) - 9) * 10 : 30)),
      wind_speed_10m: time.map(t => hour(t) / 2),
    },
    daily: {
      time: dates,
      temperature_2m_min: dates.map(() => 20.04),
      temperature_2m_max: dates.map(() => 31.5),
      precipitation_sum: dates.map(() => 10.75),
      precipitation_probability_max: dates.map(() => 92),
    },
  };
}
const geocoded = {
  results: [
    { name: 'Tena', latitude: -0.9961, longitude: -77.81315, elevation: 610, country: 'Ecuador', admin1: 'Napo', admin2: 'Tena' },
    { name: 'Tenajas', latitude: -3.81048, longitude: -79.816, country: 'Ecuador', admin1: 'El Oro', admin2: 'Piñas' },
  ],
};

/** A fetch that answers from `route(url)` and records the URLs asked. */
function mockFetch(route) {
  const calls = [];
  const fn = async (url, { signal } = {}) => {
    calls.push(String(url));
    const body = await route(new URL(url), signal);
    return { ok: true, status: 200, json: async () => body };
  };
  return { fn, calls };
}
const openMeteo = url => {
  if (url.hostname.startsWith('geocoding')) return url.searchParams.get('countryCode') === 'EC' && url.searchParams.get('name') === 'Tena' ? geocoded : { results: [] };
  return forecast(Number(url.searchParams.get('forecast_days')));
};

async function setup(options = {}) {
  const store = new Store({ localMode: true }, { sheets: new LocalSheets(seed) });
  await store.sync({ sheets: Object.keys(seed) });
  const http = mockFetch(options.route ?? openMeteo);
  let clock = Date.parse('2026-11-01T12:30:00Z');
  const tool = createWeatherTool({ store, fetch: http.fn, now: () => clock, ...options.tool });
  return { tool, calls: http.calls, tick: ms => (clock += ms) };
}

test('weather_forecast is offered to the chats, and the WhatsApp bridge names its step', () => {
  const names = new Set(mcpTools().map(t => t.name));
  for (const n of WEATHER_TOOL_NAMES) assert.ok(names.has(n), n);
  assert.equal(stepLabel({ tool: 'mcp__ithomiini__weather_forecast' }), 'mirando el pronóstico del clima');
});

test('a database locality: its coordinates, days day first, field hours and the monitoring window', async () => {
  const { tool, calls } = await setup();
  const out = await tool.run('weather_forecast', { place: 'ikiam', days: 2 });
  assert.equal(out.error, undefined, out.error);
  assert.equal(out.place.name, 'Ikiam');
  assert.equal(out.place.matched_by, 'database locality (Location_data)');
  assert.deepEqual([out.place.lat, out.place.lon], [-0.95, -77.87]);
  assert.deepEqual(out.grid, { lat: -0.949, lon: -77.821, elevation_m: 633 });
  const asked = new URL(calls[0]);
  assert.equal(asked.searchParams.get('timezone'), 'America/Guayaquil');
  assert.equal(asked.searchParams.get('latitude'), '-0.95');
  assert.equal(asked.searchParams.get('forecast_days'), '2');
  assert.equal(out.days.length, 2);
  const [first] = out.days;
  assert.equal(first.date, '01/11/2026');
  assert.equal(first.weekday, 'Sunday');
  assert.deepEqual([first.temp_min_c, first.temp_max_c, first.precip_mm, first.precip_prob_max], [20, 31.5, 10.8, 92]);
  // 07:00 to 17:00 local, both included, as Open-Meteo's local times say.
  assert.equal(first.hours.length, 11);
  assert.deepEqual(first.hours[0], ['07:00', 23.5, 28, 0, 30, 3.5]);
  assert.equal(first.hours.at(-1)[0], '17:00');
  // 9–11: the rain of the hours ending at 10 and 11 (not the 9 mm ending at 9), temperature and cloud at 9, 10, 11.
  assert.deepEqual(first.monitoring, { window: '09:00–11:00', temp_c: [24.5, 25.5], precip_prob_max: 44, precip_mm: 1.8, cloud_mean: 70, wind_max_kmh: 5.5 });
  assert.equal(out.fetched, '01/11/2026 07:30');
  assert.match(out.source, /Open-Meteo\.com/);
});

test('other field hours, and checks on the arguments', async () => {
  const { tool } = await setup();
  const out = await tool.run('weather_forecast', { place: 'Ikiam', days: 1, from_hour: 6, to_hour: 8 });
  assert.deepEqual(out.days[0].hours.map(r => r[0]), ['06:00', '07:00', '08:00']);
  assert.equal(out.field_hours, '06:00–08:00');
  assert.ok((await tool.run('weather_forecast', { place: 'Ikiam', days: 9 })).error);
  assert.ok((await tool.run('weather_forecast', { place: 'Ikiam', from_hour: 12, to_hour: 8 })).error);
  assert.ok((await tool.run('weather_forecast', {})).error);
});

test('a part of a locality name: the shortest match, the others listed; accents ignored', async () => {
  const { tool } = await setup();
  const yasuni = await tool.run('weather_forecast', { place: 'yasuni' });
  assert.equal(yasuni.place.name, 'Añangu, Yasuní');
  const mariposario = await tool.run('weather_forecast', { place: 'mariposario' });
  assert.equal(mariposario.place.name, 'Mariposario Ikiam');
  assert.equal(mariposario.place.other_matches, undefined);
  assert.match(mariposario.place.matched_by, /partial name/);
  // An exact name wins and is said plainly; the localities containing it are listed.
  const ikiam = await tool.run('weather_forecast', { place: 'Ikiam' });
  assert.equal(ikiam.place.matched_by, 'database locality (Location_data)');
  assert.deepEqual(ikiam.place.other_matches, ['Mariposario Ikiam']);
  // Whole words only, and not for one or two letters: «ian» is not «Mariposario Ikiam».
  assert.notEqual((await tool.run('weather_forecast', { place: 'kiam' })).place?.name, 'Ikiam');
});

test('lat and lon are used as given', async () => {
  const { tool, calls } = await setup();
  const out = await tool.run('weather_forecast', { lat: -1.0634, lon: -77.6157, place: 'Jatun Sacha' });
  assert.equal(out.place.matched_by, 'coordinates given');
  assert.equal(out.place.name, 'Jatun Sacha');
  assert.equal(calls.length, 1);
  assert.equal(new URL(calls[0]).searchParams.get('longitude'), '-77.62');
});

test('a place not in the database: Open-Meteo geocoding in Ecuador, the match said; none at all is an error', async () => {
  const { tool, calls } = await setup();
  const out = await tool.run('weather_forecast', { place: 'Tena' });
  assert.equal(out.place.matched_by, 'Open-Meteo geocoding (Ecuador)');
  assert.equal(out.place.name, 'Tena, Napo, Ecuador');
  assert.deepEqual(out.place.other_matches, ['Tenajas, Piñas, El Oro, Ecuador']);
  assert.equal(new URL(calls[0]).searchParams.get('countryCode'), 'EC');
  const none = await tool.run('weather_forecast', { place: 'Sin coordenadas' });
  assert.match(none.error, /ask the person for the site's coordinates/i);
});

test('a place found only outside Ecuador: said in matched_by and in a warning', async () => {
  const { tool, calls } = await setup({
    route: url =>
      url.hostname.startsWith('geocoding')
        ? url.searchParams.get('countryCode')
          ? {}
          : { results: [{ name: 'Misahualli', latitude: 9.9, longitude: -84.1, country: 'Costa Rica', admin1: 'San José' }] }
        : forecast(3),
  });
  const out = await tool.run('weather_forecast', { place: 'Misahualy' });
  assert.match(out.place.matched_by, /outside Ecuador/);
  assert.match(out.warning, /check the place with the person/);
  assert.equal(calls.filter(u => u.includes('geocoding')).length, 2);
});

test('the place search failing: an error that asks for the coordinates', async () => {
  const { tool } = await setup({
    route: url => {
      if (url.hostname.startsWith('geocoding')) throw new Error('network down');
      return forecast(3);
    },
  });
  assert.match((await tool.run('weather_forecast', { place: 'Tena' })).error, /place search failed \(network down\)/);
});

test("rain in the field hours leaves out the hour before them; today's gone hours are marked", async () => {
  const { tool, tick } = await setup();
  const out = await tool.run('weather_forecast', { place: 'Ikiam', days: 2, from_hour: 10, to_hour: 12 });
  // Hours ending at 11 and 12 (0.5 + 0): not the 1.25 mm ending at 10.
  assert.equal(out.days[0].field_precip_mm, 0.5);
  assert.equal(out.days[0].field_precip_prob_max, 48);
  assert.equal(out.days[0].past_until, undefined);
  tick(6 * 3600_000); // 13:30 in Guayaquil
  const later = await tool.run('weather_forecast', { place: 'Ikiam', days: 2 });
  assert.equal(later.days[0].past_until, '13:00');
  assert.equal(later.days[0].monitoring_past, true);
  assert.equal(later.days[1].past_until, undefined);
});

test('the same point and days within the hour: one call to Open-Meteo, marked cached; after the hour, asked again', async () => {
  const { tool, calls, tick } = await setup();
  await tool.run('weather_forecast', { place: 'Ikiam', days: 3 });
  const again = await tool.run('weather_forecast', { place: 'Ikiam', days: 3, from_hour: 9, to_hour: 12 });
  assert.equal(calls.length, 1);
  assert.equal(again.cached, true);
  assert.equal(again.days[0].hours.length, 4);
  tick(3600_000);
  const later = await tool.run('weather_forecast', { place: 'Ikiam', days: 3 });
  assert.equal(calls.length, 2);
  assert.equal(later.cached, undefined);
});

test('an unreadable answer is an error and is not kept: the next call asks again', async () => {
  let n = 0;
  const store = new Store({ localMode: true }, { sheets: new LocalSheets(seed) });
  await store.sync({ sheets: Object.keys(seed) });
  const tool = createWeatherTool({
    store,
    fetch: async () => (++n === 1 ? { ok: true, status: 200, json: async () => { throw new SyntaxError('Unexpected end of JSON input'); } } : { ok: true, status: 200, json: async () => forecast(3) }),
  });
  assert.match((await tool.run('weather_forecast', { place: 'Ikiam' })).error, /Unexpected end of JSON/);
  const again = await tool.run('weather_forecast', { place: 'Ikiam' });
  assert.equal(again.error, undefined);
  assert.equal(n, 2);
});

test('Open-Meteo too slow or failing: a clear error with the place', async () => {
  const slow = await setup({
    tool: { timeoutMs: 20 },
    route: (url, signal) => new Promise((_, reject) => signal.addEventListener('abort', () => reject(new Error('aborted')))),
  });
  const out = await slow.tool.run('weather_forecast', { place: 'Ikiam' });
  assert.match(out.error, /did not answer within 0.02 s/);
  assert.equal(out.place.name, 'Ikiam');
  const store = new Store({ localMode: true }, { sheets: new LocalSheets(seed) });
  await store.sync({ sheets: Object.keys(seed) });
  const failing = createWeatherTool({ store, fetch: async () => ({ ok: false, status: 429, json: async () => ({ reason: 'Daily API request limit exceeded' }) }) });
  assert.match((await failing.run('weather_forecast', { place: 'Ikiam' })).error, /429: Daily API request limit exceeded/);
});

test('dayParts: an hour missing from the window leaves the rest', () => {
  const hourly = { time: ['2026-11-02T09:00', '2026-11-02T10:00'], temperature_2m: [22, null], precipitation_probability: [10, 30], precipitation: [0, 0.2], cloud_cover: [50, 70], wind_speed_10m: [3, 4] };
  const { hours, monitoring } = dayParts(hourly, '2026-11-02', { from: 9, to: 10 });
  assert.deepEqual(hours[1], ['10:00', null, 30, 0.2, 70, 4]);
  assert.deepEqual(monitoring.temp_c, [22, 22]);
  assert.equal(monitoring.precip_mm, 0.2);
});
