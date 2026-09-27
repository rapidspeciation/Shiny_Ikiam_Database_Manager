#!/usr/bin/env node
/**
 * Sends monitoring walks from Wikiloc to the Ithomiini app, from their links.
 *
 *   npm run wikiloc -- https://es.wikiloc.com/rutas-senderismo/...-213523060 [more links]
 *
 * Wikiloc has no API and its Cloudflare check blocks servers, so the public
 * trail page is opened by a headless browser on this computer (a home
 * connection passes the check). From the page it takes each waypoint's note,
 * position, elevation and photos, and the trail line; no Wikiloc login is
 * used. The walk then waits in Monitoreo → Importar recorrido for review; the
 * app server downloads the photos. Pages are opened one at a time, a few
 * seconds apart. Only use it for your own team's trails.
 *
 * Links pasted in the app are handled by worker.mjs instead (see README.md).
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
import { DEFAULT_APP, openBrowser, photoCount, readTrail, signIn } from './lib.mjs';

const args = process.argv.slice(2);
const flag = name => {
  const i = args.indexOf(name);
  if (i < 0) return null;
  const [value] = args.splice(i, 2).slice(1);
  return value ?? '';
};
const dryRun = args.includes('--dry-run') && !!args.splice(args.indexOf('--dry-run'), 1);
const app = (flag('--app') || process.env.ITHOMIINI_APP || DEFAULT_APP).replace(/\/?$/, '/');
const urls = args.filter(a => /^https:\/\/([a-z]{2}\.)?wikiloc\.com\//.test(a));
if (!urls.length) {
  console.error('Usage: npm run wikiloc -- <Wikiloc trail URL> [...] [--app URL] [--dry-run]');
  process.exit(1);
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
  const auth = await signIn(app, username, password);
  mkdirSync(join(homedir(), '.config', 'ithomiini-wikiloc'), { recursive: true, mode: 0o700 });
  writeFileSync(sessionFile, JSON.stringify({ app, cookie: auth.cookie }));
  chmodSync(sessionFile, 0o600);
  return auth;
}

let browser;
try {
  browser = await openBrowser();
} catch {
  console.error('Install the helper first: npm --prefix tools/wikiloc install');
  process.exit(1);
}
const auth = dryRun ? null : await session();
let failures = 0;
for (const [i, url] of urls.entries()) {
  if (i) await browser.page.waitForTimeout(4000);
  try {
    const walk = await readTrail(browser.page, url);
    console.log(
      `${walk.name}: ${walk.waypoints.length} puntos, ${photoCount(walk)} fotos, ${walk.track.length} puntos de trazado, fecha ${walk.date || '¿?'}`,
    );
    if (dryRun) {
      console.log(JSON.stringify(walk, null, 1).slice(0, 1500));
      continue;
    }
    const r = await fetch(`${app}api/monitoring/wikiloc`, {
      method: 'POST',
      headers: { 'content-type': 'application/json', cookie: auth.cookie, 'x-csrf-token': auth.csrf },
      body: JSON.stringify(walk),
    });
    const body = await r.json().catch(() => ({}));
    if (!r.ok) throw new Error(body.error?.message || `App answered ${r.status}`);
    console.log(
      `  → en la app para revisar${body.updated ? ' (actualizado)' : ''}${body.failed?.length ? `; ${body.failed.length} fotos no se pudieron bajar` : ''}`,
    );
  } catch (e) {
    failures++;
    console.error(`${url}: ${e.message}`);
  }
}
await browser.close();
if (!dryRun && urls.length > failures) console.log(`Revisa en ${app}#/monitoreo?vista=importar`);
process.exit(failures ? 1 : 0);
