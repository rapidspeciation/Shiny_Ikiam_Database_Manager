/**
 * Shared by fetch.mjs (links given on the command line) and worker.mjs (the
 * background processor): reading public Wikiloc pages with Camoufox,
 * and talking to the Ithomiini app.
 */
export { openBrowser } from './browser.mjs';

export const DEFAULT_APP = 'https://ithomiini-ikiam.com/';

const MONTHS = ['ene', 'feb', 'mar', 'abr', 'may', 'jun', 'jul', 'ago', 'sep', 'oct', 'nov', 'dic'];
const EN = ['jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'];
const monthOf = word => {
  const abbr = String(word).toLowerCase().slice(0, 3);
  return MONTHS.indexOf(abbr) + 1 || EN.indexOf(abbr) + 1 || 0;
};
const iso = (y, m, d) => {
  const t = new Date(Date.UTC(y, m - 1, d));
  return t.getUTCMonth() === m - 1 && t.getUTCDate() === d ? t.toISOString().slice(0, 10) : null;
};
const fullYear = y => (y < 100 ? 2000 + y : y);

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
        // The "Master" copy is the largest one Wikiloc serves.
        photos: (w.photos || []).map(p => p.url.replace(/(\d+)(?:Master)?\.jpe?g$/i, '$1Master.jpg')),
      };
    });
    const track = [];
    window.trailMap?.eachLayer?.(layer => {
      if (!layer.getLatLngs || track.length) return;
      for (const p of layer.getLatLngs().flat(3)) track.push([p.lat, p.lng, p.alt ?? null, null]);
    });
    const done = /Fecha de realizaci[oó]n\s*\n?\s*([^\n]+)/i.exec(document.body.innerText)?.[1] || '';
    // The author's profile number (the first profile link on a trail page is its author).
    const author = /user\.do\?id=(\d+)/.exec([...document.querySelectorAll('a[href*="user.do?id="]')][0]?.href || '')?.[1] || null;
    const created = /"dateCreated"\s*:\s*"([^"]+)"/.exec(
      [...document.querySelectorAll('script[type="application/ld+json"]')].map(s => s.textContent).join('\n'),
    )?.[1];
    return { name: md.mapData?.[0]?.nom || document.title, track, waypoints, done, author, created };
  });
  return {
    url,
    name: trail.name,
    date: walkDate(trail.name, trail.done, trail.created),
    recorded: trail.done || null,
    author: trail.author,
    track: trail.track,
    waypoints: trail.waypoints,
  };
}

/** All public trails of a profile (newest first): links, titles and trail numbers. */
export async function readProfile(page, userId) {
  const trails = [];
  let name = null;
  for (let from = 0; from < 2000; from += 10) {
    await open(page, `https://es.wikiloc.com/wikiloc/user.do?id=${userId}&from=${from}&to=${from + 10}`);
    await page.waitForTimeout(2000);
    const found = await page.evaluate(() => ({
      name: /de (.+?) \| Wikiloc/.exec(document.title)?.[1] || null,
      links: [...document.querySelectorAll('a')]
        .filter(a => /-\d{6,12}$/.test(a.href) && a.innerText.trim())
        .map(a => ({ url: a.href, title: a.innerText.trim().split('\n')[0] })),
    }));
    name ||= found.name;
    // Each trail is linked more than once on the page (title, photo); keep one per trail number.
    let fresh = 0;
    for (const l of found.links) {
      const wikilocId = /-(\d{6,12})$/.exec(l.url)[1];
      if (trails.some(t => t.wikilocId === wikilocId)) continue;
      trails.push({ ...l, wikilocId });
      fresh++;
    }
    if (!fresh) break;
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
