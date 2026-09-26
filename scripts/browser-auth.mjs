#!/usr/bin/env node
// Run with PLAYWRIGHT_MODULE pointing to an existing playwright-core installation.
import assert from 'node:assert/strict';
import { pathToFileURL } from 'node:url';
import { createApp } from '../server/index.mjs';

const { chromium } = await import(pathToFileURL(process.env.PLAYWRIGHT_MODULE));
const browser = await chromium.launch({ executablePath: '/usr/bin/chromium', headless: true, args: ['--no-sandbox'] });
const errors = [];

async function checkCredentialForm(page, setup) {
  const form = page.locator('#auth-form');
  await form.waitFor();
  assert.equal(await form.getAttribute('method'), 'post');
  assert.equal(await form.getAttribute('action'), `/ithomiini/api/auth/${setup ? 'setup' : 'login'}`);
  assert.equal(await form.locator('input[type=password]').count(), 1, 'Only the account password belongs in the credential form');
  assert.equal(await form.locator('[name=username]').getAttribute('autocomplete'), 'username');
  assert.equal(await form.locator('[name=password]').getAttribute('autocomplete'), setup ? 'new-password' : 'current-password');
}

try {
  for (const useLink of [true, false]) {
    const app = await createApp({ databasePath: ':memory:', localMode: true, secureCookies: false, syncIntervalMs: 0, setupToken: 'browser-test-setup' });
    const address = await app.listen(0, '127.0.0.1');
    const context = await browser.newContext({ serviceWorkers: 'block' });
    try {
      const page = await context.newPage();
      page.on('pageerror', error => errors.push(error.message));
      await page.goto(`http://127.0.0.1:${address.port}/ithomiini/#/setup${useLink ? '?token=browser-test-setup' : ''}`);
      await checkCredentialForm(page, true);
      if (useLink) {
        assert.equal(await page.locator('input[name=token]').getAttribute('type'), 'hidden');
        assert.ok(!page.url().includes('token='), 'The setup token is removed from the address bar');
      } else {
        await page.locator('input[name=token]').fill('browser-test-setup');
        await page.locator('[data-action=language][data-lang=en]').click();
      }
      await page.locator('[name=displayName]').fill('Browser Test');
      await page.locator('[name=username]').fill('invalid username');
      await page.locator('[name=password]').fill('test12');
      await page.locator('#auth-form button[type=submit]').click();
      await page.locator('[role=alert]').waitFor();
      assert.equal(await page.locator('input[name=token]').inputValue(), 'browser-test-setup', 'Setup token survives validation failure');
      await checkCredentialForm(page, true);
      await page.locator('[name=displayName]').fill('Browser Test');
      await page.locator('[name=username]').fill('browser_user');
      await page.locator('[name=password]').fill('test12');
      await page.locator('#auth-form button[type=submit]').click();
      await page.locator('.shell').waitFor();
      const session = await page.evaluate(async () => (await fetch('./api/auth/session')).json());
      assert.equal(session.user.username, 'browser_user');
      assert.equal(session.user.displayName, 'Browser Test');
      await page.locator('[data-action=logout]').click();
      await checkCredentialForm(page, false);
      assert.equal(await page.locator('#auth-form [name=token]').count(), 0);
      await page.locator('[name=username]').fill('browser_user');
      await page.locator('[name=password]').fill('test12');
      await page.locator('#auth-form button[type=submit]').click();
      await page.locator('.shell').waitFor();
      console.log(`${useLink ? 'Link' : 'Manual'} setup, validation retry, saved username and subsequent login verified`);
    } finally {
      await context.close();
      await app.close();
    }
  }
  assert.deepEqual(errors, []);
} finally {
  await browser.close();
}
