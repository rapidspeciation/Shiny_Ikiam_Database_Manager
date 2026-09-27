#!/usr/bin/env node
/**
 * Sends monitoring walks from Wikiloc to the Ithomiini app, from their URLs.
 *
 *   npm run wikiloc -- https://es.wikiloc.com/rutas-senderismo/...-213523060 [more URLs]
 *
 * Wikiloc has no API and its Cloudflare check blocks servers, so the public
 * trail page is opened by a headless browser on this computer (a home
 * connection passes the check). From the page it takes each waypoint's note,
 * position, elevation and photos, and the trail line; no Wikiloc login is
 * used. The walk then waits in Monitoreo → Importar recorrido for review; the
 * app server downloads the photos. Pages are opened one at a time, a few
 * seconds apart. Only use it for your own team's trails.
 *
 * Options: --app <url> (default ITHOMIINI_APP or the live app), --dry-run
 * (print what would be sent). The app login is asked once and the session is
 * kept in ~/.config/ithomiini-wikiloc/session.json (readable only by you).
 * ITHOMIINI_USER / ITHOMIINI_PASSWORD avoid the prompt; CHROMIUM_PATH picks
 * the browser.
 */
import { existsSync, mkdirSync, readFileSync, writeFileSync, chmodSync } from 'node:fs';
import { homedir } from 'node:os';
import { join } from 'node:path';
import { createInterface } from 'node:readline';

const args = process.argv.slice(2);
const flag = name => {
  const i = args.indexOf(name);
  if (i < 0) return null;
  const [value] = args.splice(i, 2).slice(1);
  return value ?? '';
};
const dryRun = args.includes('--dry-run') && !!args.splice(args.indexOf('--dry-run'), 1);
const app = (flag('--app') || process.env.ITHOMIINI_APP || 'https://tbs-insect-gallery.duckdns.org/ithomiini/').replace(/\/?$/, '/');
const urls = args.filter(a => /^https:\/\/([a-z]{2}\.)?wikiloc\.com\//.test(a));
if (!urls.length) {
  console.error('Usage: npm run wikiloc -- <Wikiloc trail URL> [...] [--app URL] [--dry-run]');
  process.exit(1);
}

let chromium;
try {
  ({ chromium } = await import('playwright-core'));
} catch {
  console.error('Install the helper first: npm --prefix tools/wikiloc install');
  process.exit(1);
}

const MONTHS = ['ene', 'feb', 'mar', 'abr', 'may', 'jun', 'jul', 'ago', 'sep', 'oct', 'nov', 'dic'];
const EN = ['jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'];
/** "Monitoreo ithomidos FCH 14 mayo 2025" → 2025-05-14 */
export function dateFromName(text) {
  const m = /(\d{1,2})[\s-]*(?:de[\s-]*)?([a-záé]{3})[a-záé]*\.?[\s-]*(?:de[\s-]*)?(\d{4})/i.exec(text || '');
  if (!m) return null;
  const abbr = m[2].toLowerCase();
  const month = (MONTHS.indexOf(abbr) + 1 || EN.indexOf(abbr) + 1);
  if (!month) return null;
  return `${m[3]}-${String(month).padStart(2, '0')}-${String(m[1]).padStart(2, '0')}`;
}

function browserPath() {
  if (process.env.CHROMIUM_PATH) return process.env.CHROMIUM_PATH;
  return ['/usr/bin/chromium', '/usr/bin/chromium-browser', '/usr/bin/google-chrome', '/usr/bin/google-chrome-stable'].find(existsSync);
}

/** Everything useful on a public trail page, as the page's own map has it. */
async function readTrail(page, url) {
  await page.goto(url, { waitUntil: 'domcontentloaded', timeout: 60_000 });
  for (let i = 0; i < 30 && /moment|momento|attention|cloudflare/i.test(await page.title()); i++) await page.waitForTimeout(1000);
  await page.waitForFunction(() => window.mapData?.waypoints, null, { timeout: 30_000 });
  await page.waitForTimeout(1500); // let the map draw the trail line
  return page.evaluate(() => {
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
    return { name: md.mapData?.[0]?.nom || document.title, track, waypoints, done };
  });
}

function ask(question, hidden = false) {
  return new Promise(resolve => {
    const rl = createInterface({ input: process.stdin, output: process.stdout, terminal: true });
    if (hidden) rl._writeToOutput = text => text.includes(question) && rl.output.write(text);
    rl.question(question, answer => {
      rl.close();
      if (hidden) process.stdout.write('\n');
      resolve(answer.trim());
    });
  });
}

const sessionFile = join(homedir(), '.config', 'ithomiini-wikiloc', 'session.json');
async function session() {
  const saved = existsSync(sessionFile) ? JSON.parse(readFileSync(sessionFile, 'utf8')) : null;
  if (saved?.app === app) {
    const r = await fetch(`${app}api/auth/session`, { headers: { cookie: saved.cookie } });
    const body = await r.json().catch(() => ({}));
    if (body.user && body.csrf) return { cookie: saved.cookie, csrf: body.csrf };
  }
  const username = process.env.ITHOMIINI_USER || (await ask(`Usuario de la app (${app}): `));
  const password = process.env.ITHOMIINI_PASSWORD || (await ask('Contraseña: ', true));
  const r = await fetch(`${app}api/auth/login`, {
    method: 'POST',
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify({ username, password }),
  });
  const body = await r.json().catch(() => ({}));
  if (!r.ok) throw new Error(body.error?.message || `Login failed (${r.status})`);
  const cookie = (r.headers.get('set-cookie') || '').split(';')[0];
  mkdirSync(join(homedir(), '.config', 'ithomiini-wikiloc'), { recursive: true, mode: 0o700 });
  writeFileSync(sessionFile, JSON.stringify({ app, cookie }));
  chmodSync(sessionFile, 0o600);
  return { cookie, csrf: body.csrf };
}

const auth = dryRun ? null : await session();
const browser = await chromium.launch({
  executablePath: browserPath(),
  headless: true,
  args: ['--disable-blink-features=AutomationControlled'],
});
const context = await browser.newContext({
  locale: 'es-EC',
  userAgent: 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36',
});
const page = await context.newPage();
let failures = 0;
for (const [i, url] of urls.entries()) {
  if (i) await page.waitForTimeout(4000);
  try {
    const trail = await readTrail(page, url);
    const payload = {
      url,
      name: trail.name,
      date: dateFromName(trail.name) || dateFromName(url.replace(/-/g, ' ')),
      track: trail.track,
      waypoints: trail.waypoints,
    };
    const photos = payload.waypoints.reduce((n, w) => n + w.photos.length, 0);
    console.log(`${payload.name}: ${payload.waypoints.length} puntos, ${photos} fotos, ${payload.track.length} puntos de trazado, fecha ${payload.date || '¿?'}`);
    if (dryRun) {
      console.log(JSON.stringify(payload, null, 1).slice(0, 1500));
      continue;
    }
    const r = await fetch(`${app}api/monitoring/wikiloc`, {
      method: 'POST',
      headers: { 'content-type': 'application/json', cookie: auth.cookie, 'x-csrf-token': auth.csrf },
      body: JSON.stringify(payload),
    });
    const body = await r.json().catch(() => ({}));
    if (!r.ok) throw new Error(body.error?.message || `App answered ${r.status}`);
    console.log(`  → en la app para revisar${body.updated ? ' (actualizado)' : ''}${body.failed?.length ? `; ${body.failed.length} fotos no se pudieron bajar` : ''}`);
  } catch (e) {
    failures++;
    console.error(`${url}: ${e.message}`);
  }
}
await browser.close();
if (!dryRun && urls.length > failures) console.log(`Revisa en ${app}#/monitoreo?vista=importar`);
process.exit(failures ? 1 : 0);
