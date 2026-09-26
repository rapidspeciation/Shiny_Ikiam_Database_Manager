#!/usr/bin/env node
import { readFileSync } from 'node:fs';
import { pathToFileURL } from 'node:url';
import assert from 'node:assert/strict';
const { chromium } = await import(pathToFileURL(process.env.PLAYWRIGHT_MODULE));
const account = JSON.parse(readFileSync(process.env.TEST_ACCOUNT_FILE));
const base = process.env.LIVE_SMOKE_URL || 'http://127.0.0.1:8795/ithomiini/';
const marker = 'OFFLINE-VERIFICATION-' + Date.now();
const browser = await chromium.launch({ executablePath: '/usr/bin/chromium', headless: true, args: ['--no-sandbox'] });
try {
  const context = await browser.newContext({ viewport: { width: 390, height: 844 }, isMobile: true, hasTouch: true });
  const page = await context.newPage();
  await page.goto(base);
  await page.locator('[name=username]').fill(account.username);
  await page.locator('[name=password]').fill(account.password);
  await page.locator('#auth-form button[type=submit]').click();
  await page.locator('.shell').waitFor();
  await page.evaluate(() => navigator.serviceWorker.ready);
  await page.goto(`${base}#/workflow/round`);
  await page.locator('#event-form').waitFor();
  await context.setOffline(true);
  await page.reload();
  await page.locator('#event-form').waitFor();
  await page.getByLabel('Jaula', { exact: true }).fill(marker);
  await page.locator('#event-form button[type=submit]').click();
  await page.waitForTimeout(500);
  const queued = await page.evaluate(async () => (await import('./api.js')).getOutbox());
  assert.equal(queued.length, 1, 'One observation should remain queued');
  await context.setOffline(false);
  await page.waitForFunction(async () => (await import('./api.js')).getOutbox().length === 0, null, { timeout: 15000 });
  await page.waitForFunction(async marker => (await (await fetch('./api/events?kind=stage_round')).json()).events.some(e => e.values.Jaula === marker), marker, { timeout: 15000 });
  const events = await page.evaluate(async () => (await (await fetch('./api/events?kind=stage_round')).json()).events);
  assert.equal(events.filter(e => e.values.Jaula === marker).length, 1);
  console.log('Offline cold reload, queued phone observation, reauthentication, and single replay verified');
} finally { await browser.close(); }
