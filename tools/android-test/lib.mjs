// Drives Chrome in the Android emulator (~/.local/share/android-test): the page through
// Chrome's DevTools socket, real touches and the keyboard state through adb.
import { execFileSync } from 'node:child_process';
import { readFileSync, writeFileSync } from 'node:fs';
import { chromium } from '/home/franz/.local/share/ithomiini-wikiloc/node_modules/playwright-core/index.mjs';
export const ADB = process.env.HOME + '/.local/share/android-test/sdk/platform-tools/adb';
export const adb = (...args) => execFileSync(ADB, args, { encoding: 'utf8' });
export const creds = JSON.parse(readFileSync(process.env.HOME + '/.config/ithomiini-wikiloc/worker.json', 'utf8'));
export const sleep = ms => new Promise(r => setTimeout(r, ms));
/** Is the on-screen keyboard over the page? (The visible area is much shorter than the layout.) */
export const keyboardOver = page => page.evaluate(() => visualViewport.height < innerHeight - 120);
/** A screenshot of the phone, keyboard included. */
export const screenshot = file => writeFileSync(file, execFileSync(ADB, ['exec-out', 'screencap', '-p'], { maxBuffer: 64e6 }));
export async function connect() {
  adb('forward', 'tcp:9222', 'localabstract:chrome_devtools_remote');
  const browser = await chromium.connectOverCDP('http://127.0.0.1:9222');
  const context = browser.contexts()[0];
  const page = context.pages().find(p => p.url().includes('ithomiini')) || context.pages()[0];
  return { browser, context, page };
}
/** Real touches through Chrome's input pipeline (as a finger: gestures, focus and keyboard behave as on a phone). */
export async function tap(page, x, y, tapCount = 1) {
  const cdp = await page.context().newCDPSession(page);
  await cdp.send('Input.synthesizeTapGesture', { x, y, tapCount, gestureSourceType: 'touch' });
  await cdp.detach();
}
/** Closes the keyboard (as a person would, by leaving the field) and waits until Android hides it. */
export async function closeKeyboard(page) {
  await page.evaluate(() => document.activeElement?.blur());
  // The back key closes the keyboard on Android (Chrome keeps it after a blur).
  for (let i = 0; i < 3 && (await keyboardOver(page)); i++) { adb('shell', 'input', 'keyevent', '4'); await sleep(600) }
}
export async function login(page) {
  const session = await page.evaluate(async () => (await (await fetch('api/auth/session')).json()).user);
  if (session) return;
  await page.goto(new URL('#/entrar', creds.app).href);
  await page.fill('input[name=username]', creds.username); await page.fill('input[name=password]', creds.password);
  await page.click('button:has-text("Entrar")'); await page.waitForSelector('text=Últimos IDs usados', { timeout: 60000 });
}
