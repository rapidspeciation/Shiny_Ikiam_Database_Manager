/**
 * Shared by fetch.mjs (links given on the command line) and worker.mjs (the
 * background processor): reading public Wikiloc pages with a headless
 * browser, and talking to the Ithomiini app.
 */
import { existsSync } from 'node:fs';

export const DEFAULT_APP = 'https://tbs-insect-gallery.duckdns.org/ithomiini/';

const MONTHS = ['ene', 'feb', 'mar', 'abr', 'may', 'jun', 'jul', 'ago', 'sep', 'oct', 'nov', 'dic'];
const EN = ['jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'];
/** "Monitoreo ithomidos FCH 14 mayo 2025" → 2025-05-14 */
export function dateFromName(text) {
  const m = /(\d{1,2})[\s-]*(?:de[\s-]*)?([a-záé]{3})[a-záé]*\.?[\s-]*(?:de[\s-]*)?(\d{4})/i.exec(text || '');
  if (!m) return null;
  const abbr = m[2].toLowerCase();
  const month = MONTHS.indexOf(abbr) + 1 || EN.indexOf(abbr) + 1;
  if (!month) return null;
  return `${m[3]}-${String(month).padStart(2, '0')}-${String(m[1]).padStart(2, '0')}`;
}

function browserPath() {
  if (process.env.CHROMIUM_PATH) return process.env.CHROMIUM_PATH;
  return ['/usr/bin/chromium', '/usr/bin/chromium-browser', '/usr/bin/google-chrome', '/usr/bin/google-chrome-stable'].find(existsSync);
}

/** A headless browser page; call close() when done. */
export async function openBrowser() {
  const { chromium } = await import('playwright-core');
  const browser = await chromium.launch({
    executablePath: browserPath(),
    headless: true,
    args: ['--disable-blink-features=AutomationControlled'],
  });
  const context = await browser.newContext({
    locale: 'es-EC',
    userAgent: 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36',
  });
  return { page: await context.newPage(), close: () => browser.close() };
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
    return { name: md.mapData?.[0]?.nom || document.title, track, waypoints };
  });
  return {
    url,
    name: trail.name,
    date: dateFromName(trail.name) || dateFromName(url.replace(/-/g, ' ')),
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
