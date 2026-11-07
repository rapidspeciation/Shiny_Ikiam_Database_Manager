// The map link the assistant sends, opened in a real headless browser on the live maps app: it must show records.
// Runs when PLAYWRIGHT_MODULE names a playwright(-core) module (as scripts/browser-smoke.mjs) and the network is up;
// skipped otherwise. 6 Nov 2026: a link cut at its first %20 by WhatsApp opened as sp=Tithorea, 0 of 7,850 records.
import test from 'node:test';
import assert from 'node:assert/strict';
import { pathToFileURL } from 'node:url';
import { mapsUrl } from '../server/maps-tools.mjs';

const module = process.env.PLAYWRIGHT_MODULE;
const TAXA = ['Tithorea harmonia', 'Mechanitis messenoides', 'Oleria onega'];
const SANGER_ONLY = 7850;

/** "2,744 of 105,719 records" once every source asked has loaded (or the time is up). */
async function recordCount(page, timeoutMs = 120_000) {
  const end = Date.now() + timeoutMs;
  let last = null;
  while (Date.now() < end) {
    const t = await page.evaluate(() => document.body?.innerText ?? '');
    const m = t.match(/([\d,]+)\s*of\s*([\d,]+)\s*records/i);
    if (m) {
      last = { shown: Number(m[1].replace(/,/g, '')), total: Number(m[2].replace(/,/g, '')), empty: /No records match/.test(t) };
      if (last.total > SANGER_ONLY && last.shown > 0) return last;
    }
    await page.waitForTimeout(1000);
  }
  return last;
}

test('a map link for three taxa opens with their records (live maps app, headless browser)', { skip: !module && 'PLAYWRIGHT_MODULE not set', timeout: 240_000 }, async () => {
  const url = mapsUrl({ species: TAXA, view: { lng: -77.69, lat: -0.95, zoom: 7 } });
  assert.doesNotMatch(url, /[%()\s]/, 'nothing a chat app could end the link at');
  const pw = await import(pathToFileURL(module).href);
  const { chromium } = pw.chromium ? pw : pw.default;
  const browser = await chromium.launch();
  try {
    const page = await browser.newPage({ viewport: { width: 412, height: 900 }, isMobile: true });
    await page.goto(url, { waitUntil: 'domcontentloaded', timeout: 90_000 });
    const count = await recordCount(page);
    assert.ok(count, `no record count on ${url}`);
    assert.ok(count.shown > 0 && !count.empty, `0 records on ${url}: ${JSON.stringify(count)}`);
    assert.ok(count.total > SANGER_ONLY, `the other sources did not load: ${JSON.stringify(count)}`);
    const sp = new URL(page.url()).searchParams.get('sp')?.split(',') ?? [];
    assert.deepEqual([...sp].sort(), [...TAXA].sort(), 'the app kept the three taxa');
  } finally {
    await browser.close();
  }
});
