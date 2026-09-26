#!/usr/bin/env node
// Browser QA uses an existing local Playwright installation, not an app dependency.
import { readFileSync, mkdirSync, writeFileSync } from 'node:fs';
import { pathToFileURL } from 'node:url';
import assert from 'node:assert/strict';
const { chromium } = await import(pathToFileURL(process.env.PLAYWRIGHT_MODULE));
const account = JSON.parse(readFileSync(process.env.TEST_ACCOUNT_FILE));
const base = process.env.LIVE_SMOKE_URL || 'http://127.0.0.1:8795/ithomiini/';
const directory = process.env.BROWSER_RESULT_DIR || '.local/runtime/browser';
mkdirSync(directory, { recursive: true, mode: 0o700 });
const browser = await chromium.launch({ executablePath: '/usr/bin/chromium', headless: true, args: ['--no-sandbox'] });
const errors = [], failures = [], checks = [];
try {
  for (const [name, size] of Object.entries({ desktop: { width: 1440, height: 1000 }, phone: { width: 390, height: 844 } })) {
    const context = await browser.newContext({ viewport: size, ...(name === 'phone' ? { isMobile: true, hasTouch: true } : {}) });
    const page = await context.newPage();
    page.on('pageerror', error => errors.push(`${name}: ${error.message}`));
    page.on('response', response => { if (response.status() >= 400 && response.url().startsWith(base)) failures.push(`${name}: ${response.status()} ${new URL(response.url()).pathname}`); });
    await page.goto(base);
    await page.locator('#auth-form').waitFor();
    await page.locator('[name=username]').fill(account.username);
    await page.locator('[name=password]').fill(account.password);
    await page.locator('#auth-form button[type=submit]').click();
    await page.locator('.shell').waitFor();
    await page.waitForTimeout(700);
    await page.screenshot({ path: `${directory}/${name}-daily.png`, fullPage: true });
    const routes = name === 'desktop' ? ['/register', '/module/Collection_data', '/workflows', '/workflow/death', '/workflow/round', '/explore?view=charts&kind=stages', '/explore?view=map', '/history', '/assistant', '/settings', '/tasks'] : ['/workflow/capture', '/workflow/death', '/register'];
    for (const route of routes) {
      await page.goto(`${base}#${route}`);
      await page.waitForTimeout(route.startsWith('/explore') ? 1600 : 500);
      const horizontalOverflow = await page.evaluate(() => document.documentElement.scrollWidth > innerWidth + 2);
      checks.push({ viewport: name, route, horizontalOverflow, title: await page.locator('h1').first().innerText().catch(() => '') });
      if (horizontalOverflow) console.log(`Horizontal overflow: ${name} ${route}`);
      if (route === '/module/Collection_data' || route === '/workflow/capture') await page.screenshot({ path: `${directory}/${name}-form-or-records.png`, fullPage: true });
    }
    await context.close();
  }
} finally { await browser.close(); }
writeFileSync(`${directory}/results.json`, JSON.stringify({ errors, failures, checks }, null, 2));
console.log(JSON.stringify({ errors, failures, routesChecked: checks.length, overflowCount: checks.filter(c => c.horizontalOverflow).length }));
assert.deepEqual(errors, []);
assert.deepEqual(failures, []);
