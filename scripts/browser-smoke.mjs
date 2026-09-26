#!/usr/bin/env node
// Browser check of the main workflows against a running app (local seed or the test Sheet).
// PLAYWRIGHT_MODULE: path to an existing playwright-core index.js (not an app dependency).
// TEST_ACCOUNT_FILE: JSON {username,password} of an editor account.
// It records a death on one butterfly and then undoes it, leaving the data as it was.
import { readFileSync, mkdirSync } from 'node:fs';
import { pathToFileURL } from 'node:url';
import assert from 'node:assert/strict';

const playwright = await import(pathToFileURL(process.env.PLAYWRIGHT_MODULE));
const { chromium } = playwright.chromium ? playwright : playwright.default;
const account = JSON.parse(readFileSync(process.env.TEST_ACCOUNT_FILE, 'utf8'));
const base = process.env.LIVE_SMOKE_URL || 'http://127.0.0.1:8795/ithomiini/';
const directory = process.env.BROWSER_RESULT_DIR || '.local/runtime/browser';
const deathId = process.env.SMOKE_INSECTARY_ID; // an ID whose Death_date is empty
mkdirSync(directory, { recursive: true, mode: 0o700 });

const browser = await chromium.launch({ executablePath: '/usr/bin/chromium', headless: true, args: ['--no-sandbox'] });
const errors = [];
try {
  for (const [name, options] of Object.entries({
    desktop: { viewport: { width: 1440, height: 900 } },
    phone: { viewport: { width: 390, height: 844 }, isMobile: true, hasTouch: true },
  })) {
    const page = await (await browser.newContext(options)).newPage();
    page.on('pageerror', e => errors.push(`${name}: ${e.message}`));
    await page.goto(base);
    await page.fill('input[name=username]', account.username);
    await page.fill('input[name=password]', account.password);
    await page.click('form button.btn-primary');
    await page.waitForSelector('.tabulator-row', { timeout: 30000 });
    for (const tab of ['tablas', 'colecta', 'muertes', 'tubos', 'emergidos', 'historial', 'asistente']) {
      await page.goto(`${base}#/${tab}`);
      await page.waitForTimeout(1500);
      await page.screenshot({ path: `${directory}/${name}-${tab}.png` });
    }
    if (name !== 'desktop' || !deathId) continue;

    await page.goto(`${base}#/muertes`);
    const picker = page.locator('.toolbar input').first();
    await picker.fill(deathId);
    await page.keyboard.press('Enter');
    await page.fill('input[list="death-causes"]', 'Smoke test');
    await page.click('button:has-text("Cargar")');
    await page.waitForSelector('.tabulator-cell.is-dirty');
    await page.click('button:has-text("Guardar en la hoja")');
    await page.waitForSelector('text=guardados en Google Sheets', { timeout: 30000 });

    await page.goto(`${base}#/historial`);
    await page.waitForSelector('tbody tr');
    assert.match(await page.locator('tbody tr').first().innerText(), new RegExp(deathId));
    await page.locator('tbody tr').first().locator('input[type=checkbox]').check();
    await page.click('button:has-text("Deshacer selección")');
    await page.click('button:has-text("Deshacer en la hoja")');
    await page.waitForSelector('text=Cambios deshechos', { timeout: 30000 });
  }
  assert.deepEqual(errors, []);
  console.log('Browser smoke passed');
} finally {
  await browser.close();
}
