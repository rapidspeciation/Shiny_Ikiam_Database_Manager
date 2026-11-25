/**
 * Shared by fetch.mjs (links given on the command line) and worker.mjs (the
 * background processor): reading public Wikiloc pages with Camoufox,
 * and talking to the Ithomiini app.
 */
export { openBrowser } from './browser.mjs';

export const DEFAULT_APP = 'https://ithomiini-ikiam.com/';

/**
 * Whether a job's error is worth another try later (the worker reports `retry`, the app queues it again after
 * 5 and 20 minutes, server/monitoring.mjs finishJob) rather than a failure at once: Cloudflare's check, and a page or
 * the browser that did not answer in time. On 10 Nov 2026 a walk's page timed out once from the server
 * («Page.goto: Timeout 60000ms exceeded») and loaded on the next try; before, that failed the job for good.
 * Anything else fails at once: the app's own answers (a refused walk; a 502 during a deploy, which another try would
 * often pass, but the walk is still on Wikiloc to ask again). A link with no walk on it times out waiting for the
 * page's data, so it is tried three times before it fails.
 */
export const retryable = error =>
  /cloudflare|challenge|un momento|just a moment|timeout|timed out|NS_ERROR_NET|NS_ERROR_PROXY|NS_ERROR_CONNECTION|NS_ERROR_UNKNOWN_HOST|browser exited/i.test(
    String(error?.message || error),
  );

const MONTHS = ['ene', 'feb', 'mar', 'abr', 'may', 'jun', 'jul', 'ago', 'sep', 'oct', 'nov', 'dic'];
const EN = ['jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'];
const monthOf = word => {
  const abbr = String(word).toLowerCase().slice(0, 3);
  return MONTHS.indexOf(abbr) + 1 || EN.indexOf(abbr) + 1 || 0;
};
const iso = (y, m, d) => {
  if (!Number.isInteger(y) || !Number.isInteger(m) || !Number.isInteger(d) || y < 1990 || y > 2100) return null;
  const t = new Date(Date.UTC(y, m - 1, d));
  return t.getUTCMonth() === m - 1 && t.getUTCDate() === d ? t.toISOString().slice(0, 10) : null;
};
const fullYear = y => (y < 100 ? 2000 + y : y);

/**
 * Normalizes an ISO date/timestamp ("2022-12-08", "2022-12-09T14:22:10+0100")
 * or a standalone Spanish/English full date ("8 de diciembre de 2022", "08/12/2022")
 * to YYYY-MM-DD, validating the calendar date. Returns null if invalid.
 */
export function parseIsoDate(value) {
  const s = String(value || '').trim();
  if (!s) return null;
  const isoMatch = /^(\d{4})-(\d{2})-(\d{2})(?:$|[T\s])/.exec(s);
  if (isoMatch) return iso(+isoMatch[1], +isoMatch[2], +isoMatch[3]);
  const wordMatch = /^(\d{1,2})\s*(?:de\s*|[/.-])?\s*([a-záé]{3,})\.?\s*(?:de\s*|del\s*|[/.-])?\s*(\d{4}|\d{2})$/i.exec(s);
  if (wordMatch) {
    const m = monthOf(wordMatch[2]);
    return m ? iso(fullYear(+wordMatch[3]), m, +wordMatch[1]) : null;
  }
  const numMatch = /^(\d{1,2})[/.-](\d{1,2})[/.-](\d{4}|\d{2})$/.exec(s);
  if (numMatch) return iso(fullYear(+numMatch[3]), +numMatch[2], +numMatch[1]);
  return null;
}

/**
 * Extracts a normalized YYYY-MM recorded month from Wikiloc's "Fecha de realización"
 * ("abril 2025", "diciembre de 2022", "Dec 2022"), ISO date/month ("2022-12", "2022-12-08"),
 * or a title containing a full date. Returns null when unparseable.
 */
export function parseRecordedMonth(value) {
  const s = String(value || '').trim();
  if (!s) return null;
  const ym = /^(\d{4})-(\d{2})(?:$|-\d{2}(?:$|[T\s]))/.exec(s);
  if (ym) {
    if (s.length >= 10 && s[7] === '-') {
      const full = parseIsoDate(s);
      return full ? full.slice(0, 7) : null;
    }
    const y = +ym[1];
    const m = +ym[2];
    return y >= 1990 && y <= 2100 && m >= 1 && m <= 12 ? `${ym[1]}-${ym[2]}` : null;
  }
  const word = /\b([a-záé]{3,})\.?\s+(?:de\s+|del\s+)?(\d{4})\b/i.exec(s);
  if (word) {
    const m = monthOf(word[1]);
    const y = +word[2];
    if (m && y >= 1990 && y <= 2100) return `${y}-${String(m).padStart(2, '0')}`;
  }
  const full = parseIsoDate(s) || dateFromName(s);
  return full ? full.slice(0, 7) : null;
}

/**
 * Parses a distance in km from a number or Wikiloc text ("1,11 km", "5.42 km", "850 m").
 * Returns null for invalid, negative or out-of-range values.
 */
export function parseDistanceKm(value) {
  if (value === null || value === undefined || value === '') return null;
  if (typeof value === 'number') {
    return Number.isFinite(value) && value >= 0 && value <= 10000 ? Math.round(value * 100) / 100 : null;
  }
  const s = String(value).trim();
  if (!s) return null;
  const withUnit = /(\d+(?:[.,]\d+)?)\s*(km|kil[oó]metros?|kilometers?|m|metros?|meters?)\b/i.exec(s);
  if (withUnit) {
    const raw = Number(withUnit[1].replace(',', '.'));
    if (!Number.isFinite(raw) || raw < 0) return null;
    const km = /^k/i.test(withUnit[2]) ? raw : raw / 1000;
    return km <= 10000 ? Math.round(km * 100) / 100 : null;
  }
  if (/^\d+(?:[.,]\d+)?$/.test(s)) {
    const km = Number(s.replace(',', '.'));
    return Number.isFinite(km) && km >= 0 && km <= 10000 ? Math.round(km * 100) / 100 : null;
  }
  return null;
}

/** Total Haversine distance in km of a track [[lat, lon, ...], ...], or null when fewer than 2 points. */
export function trackDistanceKm(track) {
  if (!Array.isArray(track) || track.length < 2) return null;
  const R = 6371;
  const rad = d => (d * Math.PI) / 180;
  let total = 0;
  let prev = null;
  for (const p of track) {
    if (!Array.isArray(p)) continue;
    const lat = Number(p[0]);
    const lon = Number(p[1]);
    if (!Number.isFinite(lat) || !Number.isFinite(lon) || lat < -90 || lat > 90 || lon < -180 || lon > 180) continue;
    if (prev) {
      const dLat = rad(lat - prev[0]);
      const dLon = rad(lon - prev[1]);
      const a = Math.sin(dLat / 2) ** 2 + Math.cos(rad(prev[0])) * Math.cos(rad(lat)) * Math.sin(dLon / 2) ** 2;
      total += 2 * R * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
    }
    prev = [lat, lon];
  }
  return prev && total <= 10000 ? Math.round(total * 100) / 100 : null;
}

/**
 * Normalizes a Wikiloc trail metadata record (from readProfile, readTrail or saveWalk)
 * for multi-profile SQLite indexing. Returns null if no valid Wikiloc trail ID/URL exists.
 */
export function normalizeTrailMeta(raw, fallbackProfileId = null) {
  if (!raw || typeof raw !== 'object') return null;
  const rawUrl = String(raw.trailUrl ?? raw.trail_url ?? raw.url ?? '').trim().slice(0, 500);
  const idFromUrl =
    /^https:\/\/(?:[a-z]{2,3}\.|www\.)?wikiloc\.com\/[^\s?#]*?-(\d{6,12})\/?(?:[?#].*)?$/i.exec(rawUrl)?.[1] ??
    /^https:\/\/(?:[a-z]{2,3}\.|www\.)?wikiloc\.com\/wikiloc\/(?:view|open-trail-link)\.do\?(?:[^\s#]*&)?id=(\d{6,12})(?:[&#].*)?$/i.exec(rawUrl)?.[1] ??
    null;
  const rawId = raw.wikilocId ?? raw.wikiloc_id;
  const wikilocId = /^\d{6,12}$/.test(String(rawId ?? '')) ? String(rawId) : idFromUrl;
  if (!wikilocId) return null;
  const trailUrl =
    /^https:\/\/(?:[a-z]{2,3}\.|www\.)?wikiloc\.com\//i.test(rawUrl)
      ? rawUrl
      : `https://www.wikiloc.com/wikiloc/open-trail-link.do?id=${wikilocId}`;
  const rawProfile = String(raw.profileId ?? raw.profile_id ?? raw.author ?? fallbackProfileId ?? '').trim();
  const profileId = /^\d{3,12}$/.test(rawProfile) ? rawProfile : /user\.do\?id=(\d{3,12})/.exec(rawProfile)?.[1] ?? null;
  const title = String(raw.title ?? raw.name ?? '').replace(/\s+/g, ' ').trim().slice(0, 200) || `Wikiloc ${wikilocId}`;
  const placeText = String(raw.place ?? '').replace(/\s+/g, ' ').trim().slice(0, 200);
  const place = placeText || null;
  const uploadedDate = parseIsoDate(raw.uploadedDate ?? raw.uploaded_date ?? raw.created);
  const doneText = raw.recorded ?? raw.done ?? null;
  const explicitDate = parseIsoDate(raw.recordedDate ?? raw.recorded_date ?? raw.date);
  const recordedDate = explicitDate ?? walkDate(title, doneText, uploadedDate ?? raw.created);
  const recordedMonth =
    (recordedDate ? recordedDate.slice(0, 7) : null) ??
    parseRecordedMonth(raw.recordedMonth ?? raw.recorded_month) ??
    parseRecordedMonth(doneText) ??
    parseRecordedMonth(title);
  const distanceKm = parseDistanceKm(raw.distanceKm ?? raw.distance_km ?? raw.distance);
  return {
    wikilocId,
    profileId,
    trailUrl,
    title,
    place,
    recordedDate,
    recordedMonth,
    uploadedDate,
    distanceKm,
  };
}

/** Fast numeric ID-range and optional YYYY-MM month filter over a list of trail entries. */
export function filterTrailsByIdRange(trails, { minId = null, maxId = null, month = null } = {}) {
  const min = minId === null || minId === undefined ? null : Number(minId);
  const max = maxId === null || maxId === undefined ? null : Number(maxId);
  const targetMonth = month ? parseRecordedMonth(month) : null;
  return (Array.isArray(trails) ? trails : []).filter(t => {
    const id = Number(t?.wikilocId ?? t?.wikiloc_id);
    if (!Number.isFinite(id)) return false;
    if (min !== null && Number.isFinite(min) && id < min) return false;
    if (max !== null && Number.isFinite(max) && id > max) return false;
    if (targetMonth) {
      const m = t.recordedMonth ?? t.recorded_month ?? parseRecordedMonth(t.recordedDate ?? t.recorded_date ?? t.title);
      if (m !== targetMonth) return false;
    }
    return true;
  });
}

/**
 * Computes the tightest numeric Wikiloc ID bracket { minId, maxId } for a target date range
 * from already-indexed trails (Wikiloc trail IDs grow monotonically with upload time).
 */
export function bracketIdRange(indexedTrails, { dateFrom = null, dateTo = null } = {}) {
  const from = dateFrom ? parseIsoDate(dateFrom) : null;
  const to = dateTo ? parseIsoDate(dateTo) : null;
  const anchors = [];
  for (const t of Array.isArray(indexedTrails) ? indexedTrails : []) {
    const id = Number(t?.wikilocId ?? t?.wikiloc_id);
    const day =
      parseIsoDate(t?.uploadedDate ?? t?.uploaded_date) ??
      parseIsoDate(t?.recordedDate ?? t?.recorded_date);
    if (Number.isFinite(id) && day) anchors.push({ id, day });
  }
  anchors.sort((a, b) => a.id - b.id);
  let minId = null;
  let maxId = null;
  for (const a of anchors) {
    if (from && a.day < from) minId = a.id;
    if (to && a.day > to && maxId === null) maxId = a.id;
  }
  return { minId, maxId };
}

/**
 * Candidate dates written in a trail title, as typed in the field:
 * "14 mayo 2025", "21/sep/2026", "19/09/2026", "8/9/2025", "16/082024",
 * "13noviembre23", "13 sep" (no year), "18/06" (no year).
 */
export function titleDates(text) {
  const t = String(text || '').toLowerCase();
  const out = [];
  const word = /(\d{1,2})\s*(?:de\s*|\/|-|\.)?\s*([a-záé]{3,})\.?\s*(?:de\s*|del\s*|\/|-)?\s*(\d{4}|\d{2}(?!\d))?/g;
  for (const m of t.matchAll(word)) if (monthOf(m[2])) out.push({ d: +m[1], m: monthOf(m[2]), y: m[3] ? fullYear(+m[3]) : null });
  const num = /(\d{1,2})[/.-](\d{1,2})(?:[/.-]?(\d{4}|\d{2}(?!\d)))?/g;
  for (const m of t.matchAll(num)) out.push({ d: +m[1], m: +m[2], y: m[3] ? fullYear(+m[3]) : null });
  return out.filter(c => c.d >= 1 && c.d <= 31);
}

/** "Monitoreo ithomidos FCH 14 mayo 2025" → 2025-05-14 (title only). */
export function dateFromName(text) {
  const c = titleDates(text).find(c => c.y && c.m >= 1 && c.m <= 12);
  return c ? iso(c.y, c.m, c.d) : null;
}

/**
 * The walk's date: the day from the title, checked against the month and year
 * Wikiloc recorded ("Fecha de realización: abril 2025"), which is more reliable
 * than typed years ("27/4/2024" recorded in April 2025) and fills missing ones
 * ("Monitoreo 13 sep"). Returns null when the title has no usable day.
 */
export function walkDate(title, done, created) {
  const m = /([a-záé]+)\s+(?:de\s+)?(\d{4})/i.exec(String(done || ''));
  const doneMonth = m ? monthOf(m[1]) : 0;
  const doneYear = m ? +m[2] : null;
  const candidates = titleDates(title);
  if (!doneMonth || !doneYear) return dateFromName(title);
  const sameMonth = candidates.find(c => c.m === doneMonth);
  if (sameMonth) return iso(doneYear, doneMonth, sameMonth.d);
  // A month the title got wrong ("19/20/2025"): keep its day in the recorded month.
  const dayOnly = candidates.find(c => c.m < 1 || c.m > 12);
  if (dayOnly) return iso(doneYear, doneMonth, dayOnly.d);
  // No day in the title at all: the upload day, if it is in the recorded month
  // (walks were usually uploaded the same day). Review it in the app.
  const up = /^(\d{4})-(\d{2})-(\d{2})/.exec(String(created || ''));
  return up && +up[1] === doneYear && +up[2] === doneMonth ? `${up[1]}-${up[2]}-${up[3]}` : null;
}

async function open(page, url) {
  await page.goto(url, { waitUntil: 'domcontentloaded', timeout: 60_000 });
  for (let i = 0; i < 30 && /moment|momento|attention|cloudflare/i.test(await page.title()); i++) await page.waitForTimeout(1000);
}

/** Everything useful on a public trail page, as the page's own map has it. */
export async function readTrail(page, url) {
  await open(page, url);
  await page.waitForFunction(() => window.mapData?.waypoints, null, { timeout: 30_000 });
  await page.waitForTimeout(1500); // let the map draw the trail line
  const trail = await page.evaluate(() => {
    const md = window.mapData;
    const waypoints = (md.waypoints || []).map(w => {
      const card = document.getElementById(`wp-${w.id}`);
      const extra = [...(card?.querySelectorAll('.wpcard__body p, .description p') || [])]
        .map(p => p.innerText.trim())
        .filter(t => t && t !== w.name);
      return {
        lat: w.lat,
        lon: w.lon,
        ele: w.elevation ?? null,
        text: [w.name, ...extra].join(' ').replace(/\s+/g, ' ').trim(),
        // When the point was saved (ms since 1970): the time of a note that does not write it.
        time: Number.isFinite(Number(w.timestamp)) && Number(w.timestamp) > 0 ? new Date(Number(w.timestamp)).toISOString() : null,
        // The "Master" copy is the largest one Wikiloc serves.
        photos: (w.photos || []).map(p => p.url.replace(/(\d+)(?:Master)?\.jpe?g$/i, '$1Master.jpg')),
      };
    });
    const track = [];
    window.trailMap?.eachLayer?.(layer => {
      if (!layer.getLatLngs || track.length) return;
      for (const p of layer.getLatLngs().flat(3)) track.push([p.lat, p.lng, p.alt ?? null, null]);
    });
    const page = document.body.innerText;
    const done = /Fecha de realizaci[oó]n\s*\n?\s*([^\n]+)/i.exec(page)?.[1] || '';
    // When the recording started, and how long it lasted ("Tiempo\nuna hora 51 minutos"): the walk's start and end.
    const start = Number(md.mapData?.[0]?.trackStartTimestamp);
    const took = /\nTiempo\s*\n\s*([^\n]+)/.exec(page)?.[1] || '';
    const distance = /\n(?:Distancia|Distance)\s*\n\s*([^\n]+)/i.exec(page)?.[1] || '';
    const place =
      document.querySelector('.trail-location, .view__location, .trail-header__location, [itemprop="contentLocation"]')?.innerText?.replace(/\s+/g, ' ').trim() ||
      '';
    // The trail's own description (the page repeats the title there when it has none).
    const said = document.querySelector('.view__description .description')?.innerText?.trim() || '';
    const description = said && said !== (md.mapData?.[0]?.nom || '').trim() ? said : '';
    // The author's profile number (the first profile link on a trail page is its author).
    const author = /user\.do\?id=(\d+)/.exec([...document.querySelectorAll('a[href*="user.do?id="]')][0]?.href || '')?.[1] || null;
    const created = /"dateCreated"\s*:\s*"([^"]+)"/.exec(
      [...document.querySelectorAll('script[type="application/ld+json"]')].map(s => s.textContent).join('\n'),
    )?.[1];
    return {
      name: md.mapData?.[0]?.nom || document.title,
      track,
      waypoints,
      done,
      author,
      created,
      distance,
      place: place.slice(0, 200),
      href: location.href,
      started: Number.isFinite(start) && start > 0 ? new Date(start).toISOString() : null,
      took,
      description: description.slice(0, 2000),
    };
  });
  // A short link (loc.wiki/t/…, open-trail-link.do?id=…) ends on the trail's page: that address is kept.
  const trailPage = /^https:\/\/(?:[a-z]{2,3}\.)?wikiloc\.com\/[^?#]*-(\d{6,12})(?=[/?#]|$)/;
  const landed = trailPage.exec(trail.href || '');
  const id = /(\d{6,12})(?:[/?#&].*)?$/.exec(url)?.[1];
  const date = walkDate(trail.name, trail.done, trail.created);
  const uploadedDate = parseIsoDate(trail.created);
  const recordedMonth = (date ? date.slice(0, 7) : null) ?? parseRecordedMonth(trail.done) ?? parseRecordedMonth(trail.name);
  const distanceKm = parseDistanceKm(trail.distance) ?? trackDistanceKm(trail.track);
  return {
    url: !trailPage.test(url) && landed && (!id || landed[1] === id) ? landed[0] : url,
    name: trail.name,
    date,
    recordedDate: date,
    recordedMonth,
    uploadedDate,
    recorded: trail.done || null,
    place: trail.place || null,
    distanceKm,
    author: trail.author,
    profileId: trail.author,
    started: trail.started,
    minutes: durationMinutes(trail.took),
    description: trail.description || null,
    track: trail.track,
    waypoints: trail.waypoints,
  };
}

const NUMBER_WORDS = { un: 1, una: 1, uno: 1, dos: 2, tres: 3, cuatro: 4, cinco: 5, seis: 6 };
/** Wikiloc's "Tiempo" in minutes: "una hora 51 minutos", "31 minutos", "2 horas 5 minutos". */
export function durationMinutes(text) {
  const t = String(text || '').toLowerCase();
  const amount = word => (/^\d+$/.test(word) ? Number(word) : (NUMBER_WORDS[word] ?? NaN));
  const hours = /(\d+|[a-z]+)\s+horas?/.exec(t);
  const minutes = /(\d+|[a-z]+)\s+minutos?/.exec(t);
  if (!hours && !minutes) return null;
  const total = (hours ? amount(hours[1]) : 0) * 60 + (minutes ? amount(minutes[1]) : 0);
  return Number.isFinite(total) ? total : null;
}

/**
 * Public trails of a user-configured profile (newest first): links, titles, trail numbers
 * and any card-level place, distance and date metadata shown on the list pages.
 * Optional `minId` / `maxId` and `stopWhenKnown` stop pagination early once older trails are reached.
 */
export async function readProfile(page, userId, { minId = null, maxId = null, knownIds = null, stopWhenKnown = false, maxPages = 200 } = {}) {
  const trails = [];
  const seen = new Set();
  const known = knownIds ? new Set(knownIds) : null;
  const min = minId === null || minId === undefined ? null : Number(minId);
  const max = maxId === null || maxId === undefined ? null : Number(maxId);
  const profileId = /^\d{3,12}$/.test(String(userId ?? '')) ? String(userId) : null;
  let name = null;
  const limit = Math.max(1, Math.min(Number(maxPages) || 200, 200)) * 10;
  for (let from = 0; from < limit; from += 10) {
    await open(page, `https://es.wikiloc.com/wikiloc/user.do?id=${userId}&from=${from}&to=${from + 10}`);
    await page.waitForTimeout(2000);
    const found = await page.evaluate(() => ({
      name: /de (.+?) \| Wikiloc/.exec(document.title)?.[1] || null,
      links: [...document.querySelectorAll('a')]
        .filter(a => /-\d{6,12}$/.test(a.href) && a.innerText.trim())
        .map(a => {
          const card = a.closest('li, article, .trail-card, .trail__item, .row') || a.parentElement;
          const cardText = card?.innerText || '';
          const timeEl = card?.querySelector('time');
          const placeEl = card?.querySelector('.trail-location, .location, [itemprop="contentLocation"]');
          const distMatch = /\b(\d+(?:[.,]\d+)?\s*(?:km|m))\b/i.exec(cardText);
          return {
            url: a.href,
            title: a.innerText.trim().split('\n')[0],
            place: placeEl?.innerText?.replace(/\s+/g, ' ').trim() || null,
            distance: distMatch?.[1] || null,
            dateText: timeEl?.getAttribute('datetime') || timeEl?.innerText?.trim() || null,
          };
        }),
    }));
    name ||= found.name;
    // Each trail is linked more than once on the page (title, photo); keep one per trail number.
    let fresh = 0;
    let pageAllBelowMin = found.links.length > 0 && min !== null && Number.isFinite(min);
    let pageAllKnown = found.links.length > 0 && stopWhenKnown && !!known;
    for (const l of found.links) {
      const wikilocId = /-(\d{6,12})$/.exec(l.url)?.[1];
      if (!wikilocId || seen.has(wikilocId)) continue;
      seen.add(wikilocId);
      const idNum = Number(wikilocId);
      if (min !== null && Number.isFinite(min) && idNum >= min) pageAllBelowMin = false;
      if (known && !known.has(wikilocId)) pageAllKnown = false;
      fresh++;
      if (min !== null && Number.isFinite(min) && idNum < min) continue;
      if (max !== null && Number.isFinite(max) && idNum > max) continue;
      const cardIsoDate = parseIsoDate(l.dateText);
      const meta = normalizeTrailMeta(
        {
          url: l.url,
          wikilocId,
          title: l.title,
          place: l.place,
          distance: l.distance,
          recorded: cardIsoDate ? null : l.dateText,
          uploadedDate: cardIsoDate,
        },
        profileId,
      );
      if (meta) trails.push({ url: meta.trailUrl, title: meta.title, wikilocId: meta.wikilocId, ...meta });
    }
    if (!fresh || pageAllBelowMin || pageAllKnown) break;
    await page.waitForTimeout(2000);
  }
  return { name, trails };
}

/** A small client for the app API with a cookie session. */
export function appClient(app, login) {
  let session = null;
  async function request(path, { method = 'GET', body } = {}, retry = true) {
    if (!session) session = await login();
    const r = await fetch(`${app}api/${path}`, {
      method,
      headers: { 'content-type': 'application/json', cookie: session.cookie, 'x-csrf-token': session.csrf },
      body: body === undefined ? undefined : JSON.stringify(body),
    });
    const data = await r.json().catch(() => ({}));
    if (r.status === 401 && retry) {
      session = null;
      return request(path, { method, body }, false);
    }
    if (!r.ok) throw new Error(data.error?.message || `App answered ${r.status}`);
    return data;
  }
  return { request };
}

/** Signs in to the app and returns the session cookie and CSRF token. */
export async function signIn(app, username, password) {
  const r = await fetch(`${app}api/auth/login`, {
    method: 'POST',
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify({ username, password }),
  });
  const body = await r.json().catch(() => ({}));
  if (!r.ok) throw new Error(body.error?.message || `Login failed (${r.status})`);
  return { cookie: (r.headers.get('set-cookie') || '').split(';')[0], csrf: body.csrf };
}

export const photoCount = walk => walk.waypoints.reduce((n, w) => n + w.photos.length, 0);
