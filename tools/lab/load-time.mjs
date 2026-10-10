#!/usr/bin/env node
// How fast the app's pages open in a real browser (Chromium through Playwright): every load in a
// new browser context (nothing cached, no service worker) unless --warm, which loads each page
// twice in the same context and reports the second load (HTTP cache and service worker in place).
// For each page: requests, bytes transferred by type (as sent over the network, compressed),
// the scripts loaded, the API calls (size, time), and the times from navigation start:
// first paint, DOMContentLoaded, largest paint, "usable" (the page's own content is shown: no
// "Loading…" left in it, a grid has rows) and "settled" (the last request of the load ended).
//   node tools/lab/load-time.mjs <url> [--paths /inicio,/tablas] [--throttle slow4g|fast4g|none]
//        [--cpu 4] [--phone] [--login credentials.json] [--warm] [--runs 3] [--json out.json] [--quiet]
// --login signs in through the app's form first (credentials file {username,password}, e.g.
// $LAB/credentials.json); the session cookie is then shared by the context's loads.
// Read-only: it opens pages and signs in, nothing else.
import { writeFileSync } from 'node:fs';
import { readFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { join } from 'node:path';

const PLAYWRIGHT = process.env.LAB_PLAYWRIGHT || join(homedir(), '.local/share/ithomiini-wikiloc/node_modules/playwright-core/index.mjs');
const { chromium } = await import(PLAYWRIGHT);

const argv = process.argv.slice(2);
const option = (name, fallback) => {
  const i = argv.indexOf(name);
  return i < 0 ? fallback : argv[i + 1];
};
const flag = name => argv.includes(name);
const base = (argv.find(a => /^https?:/.test(a)) || 'http://127.0.0.1:8795').replace(/\/+$/, '');
const paths = option('--paths', '/inicio').split(',');
const runs = Number(option('--runs', 1));
const cpu = Number(option('--cpu', 1));
const throttles = {
  none: null,
  // Chrome DevTools' presets.
  slow4g: { latency: 150, downloadThroughput: (1.6 * 1024 * 1024) / 8, uploadThroughput: (750 * 1024) / 8 },
  fast4g: { latency: 40, downloadThroughput: (9 * 1024 * 1024) / 8, uploadThroughput: (1.5 * 1024 * 1024) / 8 },
};
const throttle = throttles[option('--throttle', 'none')];
const credentials = option('--login', '') && JSON.parse(readFileSync(option('--login'), 'utf8'));
const phone = flag('--phone');
const quiet = flag('--quiet');
// The live store's long poll (GET /api/pulse?wait=1) stays open on purpose: not part of the load.
const LONG = /\/api\/pulse\?wait=1/;

const browser = await chromium.launch({ executablePath: process.env.CHROMIUM || '/usr/bin/chromium', headless: true });

// Largest paint and first paint, kept by the page itself from its first moment.
const initScript = () => {
  window.__lcp = 0;
  new PerformanceObserver(list => {
    for (const e of list.getEntries()) window.__lcp = e.startTime;
  }).observe({ type: 'largest-contentful-paint', buffered: true });
};

// The page shows its own content: something in <main>, no "Loading…"/"Cargando…" left in it,
// a grid with rows when there is a grid, a chart drawn when the page has chart spaces.
const usable = () => {
  const main = document.querySelector('main') || document.querySelector('#app');
  if (!main || main.innerText.trim().length < 40) return false;
  if (/Cargando|Loading/.test(main.innerText)) return false;
  const grid = main.querySelector('.tabulator');
  if (grid && !grid.querySelector('.tabulator-row')) return false;
  window.__usable ||= performance.now();
  return true;
};
// The first chart drawn (Inicio only).
const charts = () => {
  if (!document.querySelector('[_echarts_instance_] svg')) return false;
  window.__charts ||= performance.now();
  return true;
};

async function newContext() {
  const context = await browser.newContext({
    serviceWorkers: flag('--warm') ? 'allow' : 'block',
    ...(phone
      ? { viewport: { width: 393, height: 851 }, deviceScaleFactor: 2.75, isMobile: true, hasTouch: true, userAgent: 'Mozilla/5.0 (Linux; Android 14; Pixel 7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Mobile Safari/537.36' }
      : { viewport: { width: 1440, height: 900 } }),
  });
  await context.addInitScript(initScript);
  if (credentials) {
    const page = await context.newPage();
    await page.goto(`${base}/#/entrar`);
    await page.fill('input[name="username"], input[autocomplete="username"]', credentials.username);
    await page.fill('input[type="password"]', credentials.password);
    await page.click('form button.btn-primary');
    await page.waitForFunction(() => !location.hash.startsWith('#/entrar') && document.querySelector('main'), null, { timeout: 60000 });
    await page.close();
  }
  return context;
}

async function load(context, path, { cold = true } = {}) {
  const page = await context.newPage();
  const cdp = await context.newCDPSession(page);
  await cdp.send('Network.enable');
  // Cold: what signing in loaded (scripts, styles, fonts) is not in the cache either.
  if (cold) await cdp.send('Network.clearBrowserCache');
  if (throttle) await cdp.send('Network.emulateNetworkConditions', { offline: false, ...throttle });
  if (cpu > 1) await cdp.send('Emulation.setCPUThrottlingRate', { rate: cpu });
  const requests = new Map();
  let t0 = null;
  cdp.on('Network.requestWillBeSent', e => {
    t0 ??= e.timestamp;
    requests.set(e.requestId, { url: e.request.url, type: e.type, start: e.timestamp, bytes: 0, size: 0 });
  });
  cdp.on('Network.responseReceived', e => {
    const r = requests.get(e.requestId);
    if (!r) return;
    r.type = e.type;
    r.status = e.response.status;
    r.encoding = e.response.headers['content-encoding'] || e.response.headers['Content-Encoding'] || '';
    r.fromCache = e.response.fromDiskCache || e.response.fromServiceWorker || e.response.fromPrefetchCache;
    r.sw = e.response.fromServiceWorker;
  });
  cdp.on('Network.dataReceived', e => {
    const r = requests.get(e.requestId);
    if (r) r.size += e.dataLength;
  });
  cdp.on('Network.loadingFinished', e => {
    const r = requests.get(e.requestId);
    if (r) Object.assign(r, { end: e.timestamp, bytes: e.encodedDataLength });
  });
  cdp.on('Network.loadingFailed', e => {
    const r = requests.get(e.requestId);
    if (r) Object.assign(r, { end: e.timestamp, failed: true });
  });
  await page.goto(`${base}/#${path}`, { waitUntil: 'commit' });
  await page.waitForFunction(usable, null, { polling: 50, timeout: 300000 });
  if (path === '/inicio') await page.waitForFunction(charts, null, { polling: 50, timeout: 120000 }).catch(() => {});
  // Settled: no request in flight for 1.5 s.
  for (let quiet = 0; quiet < 1500; ) {
    await new Promise(r => setTimeout(r, 250));
    const pending = [...requests.values()].some(r => r.end == null && !/^(data|blob):/.test(r.url) && !LONG.test(r.url));
    quiet = pending ? 0 : quiet + 250;
  }
  const timing = await page.evaluate(() => {
    const nav = performance.getEntriesByType('navigation')[0];
    const fcp = performance.getEntriesByName('first-contentful-paint')[0];
    return {
      fcp: fcp?.startTime ?? null,
      dcl: nav?.domContentLoadedEventEnd ?? null,
      load: nav?.loadEventEnd ?? null,
      lcp: window.__lcp || null,
      usable: window.__usable || null,
      charts: window.__charts || null,
      navStart: performance.timeOrigin,
    };
  });
  const list = [...requests.values()].filter(r => !/^(data|blob):/.test(r.url) && !LONG.test(r.url));
  const settled = Math.max(...list.map(r => ((r.end ?? r.start) - t0) * 1000));
  await page.close();
  const byType = {};
  for (const r of list) {
    const k = /\/api\//.test(r.url) ? 'api' : r.type || 'Other';
    byType[k] ||= { n: 0, bytes: 0, size: 0 };
    byType[k].n++;
    byType[k].bytes += r.bytes;
    byType[k].size += r.size;
  }
  const short = url => url.replace(base, '').replace(/^\/assets\//, '');
  return {
    path,
    timing: { ...timing, settled },
    requests: list.length,
    bytes: list.reduce((s, r) => s + r.bytes, 0),
    byType,
    scripts: list
      .filter(r => r.type === 'Script')
      .map(r => ({ file: short(r.url), bytes: r.bytes, size: r.size, encoding: r.encoding, cached: !!r.fromCache, at: Math.round((r.start - t0) * 1000), ms: Math.round(((r.end ?? r.start) - r.start) * 1000) })),
    api: list
      .filter(r => /\/api\//.test(r.url))
      .map(r => ({ url: short(r.url), status: r.status, bytes: r.bytes, size: r.size, encoding: r.encoding, sw: !!r.sw, at: Math.round((r.start - t0) * 1000), ms: Math.round(((r.end ?? r.start) - r.start) * 1000) })),
    other: list
      .filter(r => r.type !== 'Script' && !/\/api\//.test(r.url))
      .map(r => ({ url: short(r.url), type: r.type, bytes: r.bytes, encoding: r.encoding, cached: !!r.fromCache })),
  };
}

const kb = n => `${(n / 1024).toFixed(1)} kB`;
const ms = n => (n == null ? '—' : `${Math.round(n)} ms`);
const results = [];
for (const path of paths) {
  for (let run = 0; run < runs; run++) {
    const context = await newContext();
    let result = await load(context, path);
    if (flag('--warm')) {
      // The service worker takes the page over on its second load.
      await new Promise(r => setTimeout(r, 1000));
      result = await load(context, path, { cold: false });
    }
    await context.close();
    results.push(result);
    const t = result.timing;
    console.log(
      `${path} #${run + 1}: fcp ${ms(t.fcp)} · dcl ${ms(t.dcl)} · lcp ${ms(t.lcp)} · usable ${ms(t.usable)} · charts ${ms(t.charts)} · settled ${ms(t.settled)} · ${result.requests} req · ${kb(result.bytes)}`,
    );
    if (!quiet) {
      console.log('  by type: ' + Object.entries(result.byType).map(([k, v]) => `${k} ${v.n}×${kb(v.bytes)} (${kb(v.size)} raw)`).join(' · '));
      for (const s of result.scripts) console.log(`  js  ${s.file.padEnd(48)} ${kb(s.bytes).padStart(9)} ${kb(s.size).padStart(9)} ${s.encoding || '-'}${s.cached ? ' cached' : ''} @${s.at}+${s.ms}`);
      for (const a of result.api) console.log(`  api ${a.url.padEnd(48)} ${kb(a.bytes).padStart(9)} ${kb(a.size).padStart(9)} ${a.encoding || '-'} ${a.status}${a.sw ? ' sw' : ''} @${a.at}+${a.ms}`);
    }
  }
}
if (option('--json')) writeFileSync(option('--json'), JSON.stringify(results, null, 1));
await browser.close();
