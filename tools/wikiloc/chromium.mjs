/**
 * The home computer's fallback browser (ITHOMIINI_WIKILOC_BROWSER=chromium): Playwright's
 * Chromium, headless, from a home connection, which Cloudflare lets through after the first
 * page (Wikiloc's home) has loaded. The same page operations as browser.mjs (Camoufox).
 * Playwright comes from ITHOMIINI_WIKILOC_PLAYWRIGHT (a path to its index.mjs) or the
 * `playwright` package; Chromium from Playwright's own download.
 */
const HOME_PAGE = 'https://es.wikiloc.com/';
const CHALLENGE = /just a moment|un momento|attention required|verify you are human|verifica que eres humano|performing security verification/i;

export async function openChromium({ log = () => {} } = {}) {
  const where = process.env.ITHOMIINI_WIKILOC_PLAYWRIGHT || 'playwright';
  const { chromium } = await import(where);
  const browser = await chromium.launch({ headless: true, args: ['--disable-blink-features=AutomationControlled'] });
  const context = await browser.newContext({
    locale: 'es-EC',
    userAgent: 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36',
  });
  const page = await context.newPage();
  let warmed = false;
  /** Waits for Cloudflare's JavaScript check to clear (Chromium gets no checkbox to click here). */
  async function cleared(url) {
    for (let i = 0; i < 45; i++) {
      if (!CHALLENGE.test(await page.title())) return;
      await page.waitForTimeout(1000);
    }
    throw new Error(`Cloudflare challenge did not clear (${url})`);
  }
  async function goto(url) {
    if (!warmed) {
      warmed = true;
      await page.goto(HOME_PAGE, { waitUntil: 'domcontentloaded', timeout: 60_000 }).catch(() => {});
      await page.waitForTimeout(6000);
      await cleared(HOME_PAGE).catch(e => log(e.message));
    }
    await page.goto(url, { waitUntil: 'domcontentloaded', timeout: 60_000 });
    await page.waitForTimeout(2000);
    if (CHALLENGE.test(await page.title())) log(`check on ${url}`);
    await cleared(url);
  }
  return {
    page: {
      goto,
      title: () => page.title(),
      evaluate: fn => page.evaluate(fn),
      waitForTimeout: ms => page.waitForTimeout(ms),
      waitForFunction: (fn, arg, options) => page.waitForFunction(fn, arg, options),
    },
    close: () => browser.close(),
  };
}
