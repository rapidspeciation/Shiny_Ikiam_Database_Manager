// A download in Identificar → Descargas that outlasts a dropped connection, in a real browser (12 Nov 2026: Franz
// switched wifi networks during the fast model's update and had to reload the page). On a throttled network:
//   1. the fast model («Rápido y liviano») starts; at --at of the bar the browser goes offline (context.setOffline);
//      the row must say «Esperando conexión…» and the bar must not go back; after --off seconds it comes back online
//      and the download must go on by itself (no reload) and finish, with Range requests for the rest;
//   2. (--reload) the model is removed and downloaded again; at --at the page is reloaded mid-download: Descargas must
//      offer «Continuar», which asks only for the rest and finishes;
//   3. the stored files are checked in the page: each one's SHA-256 against the list, as stored.json records it.
// No AI, no messages: it only reads pages and downloads the models.
//
//   node tools/lab/resume-download.mjs http://127.0.0.1:8823 --login <credentials.json> [--reload] [--throttle 1500]
//   node tools/lab/resume-download.mjs https://ithomiini-ikiam.com            (signed out: the models from Hugging Face)
// --throttle: download speed in KB/s (default 1500, about a fair 4G); --at: the bar's % when the signal drops
// (default 35); --off: seconds without signal (default 8); --shots <folder>: screenshots of each step.
import { mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { join } from 'node:path';

const args = process.argv.slice(2);
const base = (args.find(a => /^https?:/.test(a)) || '').replace(/\/$/, '');
const option = (name, dflt) => (args.includes(name) ? args[args.indexOf(name) + 1] : dflt);
const flag = name => args.includes(name);
if (!base) {
  console.error('usage: resume-download.mjs <app url> [--login credentials.json] [--reload] [--throttle KB/s] [--at %] [--off s] [--shots dir]');
  process.exit(2);
}
const PLAYWRIGHT = process.env.LAB_PLAYWRIGHT || join(homedir(), '.local/share/agent-tools/node_modules/playwright/index.mjs');
const { chromium } = await import(PLAYWRIGHT);
const credentials = option('--login', '') && JSON.parse(readFileSync(option('--login'), 'utf8'));
const kbps = Number(option('--throttle', '1500'));
const at = Number(option('--at', '35'));
const offSeconds = Number(option('--off', '8'));
const shots = option('--shots', '');
if (shots) mkdirSync(shots, { recursive: true });
const throttle = { offline: false, latency: 150, downloadThroughput: kbps * 1024, uploadThroughput: 750 * 1024 };

const browser = await chromium.launch({ headless: true });
const context = await browser.newContext({
  viewport: { width: 393, height: 851 },
  deviceScaleFactor: 2,
  isMobile: true,
  hasTouch: true,
  locale: 'es-EC',
});
const failures = [];
const check = (ok, what) => {
  console.log(`${ok ? 'ok  ' : 'FAIL'} ${what}`);
  if (!ok) failures.push(what);
};
const models = [];
context.on('request', r => {
  const u = r.url();
  if (/\/prueba-offline\/models\/[^/]+\?part=|huggingface\.co|\.hf\.co/.test(u) && !u.includes('list.json')) models.push({ url: u.replace(/\?.*part=/, '?part='), range: r.headers().range ?? null, at: Date.now() });
});
const statuses = [];
context.on('response', r => {
  const u = r.url();
  if (/\/prueba-offline\/models\/[^/]+\?part=|\.hf\.co/.test(u)) statuses.push({ url: u.split('?')[0].split('/').pop(), status: r.status(), range: r.request().headers().range ?? null, etag: r.headers().etag ?? null });
});

let page;
let cdp;
async function open() {
  page = await context.newPage();
  cdp = await context.newCDPSession(page);
  await cdp.send('Network.enable');
  await cdp.send('Network.emulateNetworkConditions', throttle);
  await page.goto(`${base}/identificar/#/identificar?vista=descargas`, { waitUntil: 'domcontentloaded' });
  await page.waitForSelector('[data-testid=dl-model-small]', { timeout: 60_000 });
  // The row once the list is known (its button, or «en el teléfono»).
  await page.waitForFunction(() => document.querySelector('[data-testid=dl-model-small] button'), null, { timeout: 60_000 });
}
const shot = async name => shots && page.screenshot({ path: join(shots, `${name}.png`), fullPage: false });
const bar = () => page.evaluate(() => Number(document.querySelector('[data-testid=dl-model-small] [role=progressbar]')?.getAttribute('aria-valuenow') ?? -1));
const rowText = () => page.evaluate(() => document.querySelector('[data-testid=dl-model-small]')?.textContent?.replace(/\s+/g, ' ').trim() ?? '');
async function untilBar(pct, timeout = 600_000) {
  await page.waitForFunction(p => Number(document.querySelector('[data-testid=dl-model-small] [role=progressbar]')?.getAttribute('aria-valuenow') ?? -1) >= p, pct, { timeout, polling: 200 });
}
/** Done: the row says it is on the phone, no bar, no error. */
async function untilDone(timeout = 900_000) {
  await page.waitForFunction(
    () => {
      const row = document.querySelector('[data-testid=dl-model-small]');
      return row && !row.querySelector('[role=progressbar]') && /en el teléfono|on the phone/.test(row.textContent ?? '') && !row.querySelector('[data-testid^=dl-continue]');
    },
    null,
    { timeout, polling: 500 },
  );
}
/** The small model's files as stored, each hashed in the page (Web Crypto over its parts), against stored.json. */
async function verifyStored() {
  return page.evaluate(async () => {
    const cache = await caches.open('ithomiini-prueba-offline');
    const root = new URL('../prueba-offline/', location.href).href;
    const stored = await (await cache.match(`${root}models/stored.json`))?.json();
    if (!stored) return { error: 'no stored.json' };
    const out = {};
    for (const [name, f] of Object.entries(stored.files)) {
      const parts = [];
      for (let i = 0; i * 32 * 1024 * 1024 < f.bytes; i++) {
        const r = await cache.match(`${root}models/${name}?part=${i}${f.slot ? `&v=${f.slot}` : ''}`);
        if (!r) return { error: `${name}: part ${i} missing` };
        parts.push(await r.blob());
      }
      const buf = await new Blob(parts).arrayBuffer();
      const hex = [...new Uint8Array(await crypto.subtle.digest('SHA-256', buf))].map(b => b.toString(16).padStart(2, '0')).join('');
      out[name] = { bytes: buf.byteLength, listedBytes: f.bytes, sha256: hex, listed: f.sha256 ?? null, role: f.role };
    }
    const pieces = (await cache.keys()).filter(r => r.url.includes('&piece=')).length;
    return { files: out, pieces };
  });
}
async function startOrRemove() {
  // A model already on the phone (an earlier run): removed first, so the whole download is measured.
  const remove = page.locator('[data-testid=dl-model-small] button.btn-danger');
  if ((await remove.count()) && !(await page.locator('[data-testid=dl-get-small], [data-testid=dl-continue-small]').count())) {
    page.once('dialog', d => d.accept());
    await remove.click();
    await page.waitForSelector('[data-testid=dl-get-small]', { timeout: 60_000 });
  }
}

if (credentials) {
  const login = await context.newPage();
  await login.goto(`${base}/#/entrar`);
  await login.fill('input[name="username"], input[autocomplete="username"]', credentials.username);
  await login.fill('input[type="password"]', credentials.password);
  await login.click('form button.btn-primary');
  await login.waitForFunction(() => !location.hash.startsWith('#/entrar') && document.querySelector('main'), null, { timeout: 60_000 });
  await login.close();
}

const t0 = Date.now();
await open();
await startOrRemove();
// ---------------------------------------------------------------- 1. offline midway, back online, no reload
models.length = 0;
statuses.length = 0;
await page.click('[data-testid=dl-get-small]');
await untilBar(at);
const before = await bar();
await shot('1-before-offline');
await context.setOffline(true);
const offAt = Date.now();
await page.waitForSelector('[data-testid=dl-waiting-small]', { timeout: 30_000 });
const waitingText = await rowText();
check(/Esperando conexión…|Waiting for connection…/.test(waitingText), `offline: the row says «Esperando conexión…» (${waitingText.slice(0, 90)})`);
check(!(await page.locator('[data-testid=dl-model-small] [role=alert], [data-testid=downloads] [role=alert]').count()), 'offline: no error shown');
const bars = [];
while (Date.now() - offAt < offSeconds * 1000) {
  bars.push(await bar());
  await page.waitForTimeout(500);
}
await shot('2-offline-waiting');
check(bars.every(b => b >= before - 1), `offline: the bar stays where it was (${before} % → ${Math.min(...bars)}..${Math.max(...bars)} %)`);
await context.setOffline(false);
// Playwright's setOffline clears the throttle: set again.
await cdp.send('Network.emulateNetworkConditions', throttle);
const onAt = Date.now();
await page.waitForFunction(() => !document.querySelector('[data-testid=dl-waiting-small]'), null, { timeout: 120_000, polling: 250 });
const resumedMs = Date.now() - onAt;
check(resumedMs < 45_000, `online again: it goes on by itself in ${(resumedMs / 1000).toFixed(1)} s`);
await untilDone();
await shot('3-done');
const ranges1 = models.filter(m => m.at > offAt && m.range);
check(ranges1.length > 0, `after the drop: Range requests for the rest (${ranges1.slice(0, 3).map(m => `${m.url.split('/').pop()} ${m.range}`).join('; ')})`);
const s206 = statuses.filter(s => s.status === 206);
check(s206.length > 0 && s206.every(s => s.etag), `the server answered 206 with an ETag (${s206.length}×, e.g. ${s206[0]?.url} ${s206[0]?.etag})`);
let v = await verifyStored();
const okFiles = v.files && Object.values(v.files).every(f => f.bytes === f.listedBytes && (!f.listed || f.sha256 === f.listed));
check(!!okFiles, `stored files hashed in the page match the list (${v.error ?? Object.entries(v.files).map(([n, f]) => `${n} ${f.sha256 === f.listed ? 'sha ok' : f.listed ? 'SHA MISMATCH' : 'no sha listed'}`).join(', ')})`);
check(v.pieces === 0, `no pieces left over (${v.pieces})`);
const phase1 = { before, resumedMs, ranges: ranges1.length, statuses206: s206.length };

// ---------------------------------------------------------------- 2. a reload midway: «Continuar»
let phase2 = null;
if (flag('--reload')) {
  await startOrRemove();
  models.length = 0;
  statuses.length = 0;
  await page.click('[data-testid=dl-get-small]');
  await untilBar(at);
  const kept = await bar();
  await page.close();
  await open();
  const cont = page.locator('[data-testid=dl-continue-small]');
  await cont.waitFor({ timeout: 60_000 });
  const label = (await cont.textContent())?.trim();
  const partial = (await page.locator('[data-testid=dl-partial-small]').textContent())?.replace(/\s+/g, ' ').trim();
  await shot('4-after-reload-continue');
  check(true, `after the reload: «${label}» (${partial}); the bar was at ${kept} %`);
  // Only the model begun: the other model and the sex model share files with it (the detector) but were not started.
  const others = await page.locator('[data-testid=dl-continue-full], [data-testid=sex-continue], [data-testid=dl-partial-full], [data-testid=sex-partial]').count();
  check(others === 0, `after the reload: no «Continuar» on the models not begun (${others})`);
  models.length = 0;
  await cont.click();
  await untilDone();
  const first = models.find(m => /student|bioclip|head|wing/.test(m.url) && m.range);
  check(!!first, `«Continuar» asks only for the rest (${first ? `${first.url.split('/').pop()} ${first.range}` : 'no Range request'})`);
  v = await verifyStored();
  const ok2 = v.files && Object.values(v.files).every(f => f.bytes === f.listedBytes && (!f.listed || f.sha256 === f.listed));
  check(!!ok2, 'after «Continuar»: stored files match the list');
  phase2 = { kept, label, partial, first };
  await shot('5-done-after-continue');
}

const summary = { base, signedIn: !!credentials, throttleKBps: kbps, at, offSeconds, seconds: Math.round((Date.now() - t0) / 1000), phase1, phase2, failures, stored: v.files };
console.log(JSON.stringify(summary, null, 1));
if (option('--json', '')) writeFileSync(option('--json'), JSON.stringify(summary, null, 1));
await browser.close();
process.exit(failures.length ? 1 : 0);
