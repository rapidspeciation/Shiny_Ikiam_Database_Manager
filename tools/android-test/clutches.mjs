// Clutches as cards on the phone (components/clutches), driven on the emulator with Gboard.
// It saves counts and notes and marks checks, so it refuses any app but a LOCAL one
// (LOCAL_MODE, its own database):
//   adb reverse tcp:8797 tcp:8797      (the phone's localhost:8797 is this PC's; a secure context)
//   APP=http://localhost:8797/ CREDS=~/.cache/ithomiini-lab/credentials.json node tools/android-test/clutches.mjs
// ROTATION=1 runs it with the phone on its side (and puts it back upright at the end); IME=gboard below.
// Screenshots go to $SHOTS (default /tmp), named clutches-<portrait|landscape>-N-….png.
import { readFileSync } from 'node:fs';
import { adb, keyboardOver, screenshot, sleep, tap } from './lib.mjs';
import { chromium } from '/home/franz/.local/share/ithomiini-wikiloc/node_modules/playwright-core/index.mjs';

const APP = process.env.APP || 'http://localhost:8797/';
if (!/localhost|127\.0\.0\.1/.test(APP)) throw new Error('Only a local app: this test saves clutch counts');
const creds = JSON.parse(readFileSync(process.env.CREDS.replace(/^~/, process.env.HOME), 'utf8'));
const SHOTS = process.env.SHOTS || '/tmp';
const landscape = process.env.ROTATION === '1';
const tag = landscape ? 'landscape' : 'portrait';
const shot = name => screenshot(`${SHOTS}/clutches-${tag}-${name}.png`);
const report = (what, value) => console.log(`${what}:`, typeof value === 'string' ? value : JSON.stringify(value));

// IME=gboard types with Gboard for this run (SwiftKey, once text came through adb, folds into its bar
// and shows a "physical keyboard" tip over the page on a phone on its side); the keyboard is put back after.
const previousIme = adb('shell', 'settings', 'get', 'secure', 'default_input_method').trim();
if (process.env.IME === 'gboard') adb('shell', 'ime', 'set', 'com.google.android.inputmethod.latin/com.android.inputmethod.latin.LatinIME');
adb('shell', 'settings', 'put', 'system', 'accelerometer_rotation', '0');
adb('shell', 'settings', 'put', 'system', 'user_rotation', landscape ? '1' : '0');
await sleep(1500);
if (!adb('shell', 'cat', '/proc/net/unix').includes('chrome_devtools_remote')) {
  adb('shell', 'am', 'start', '-a', 'android.intent.action.VIEW', '-d', APP, 'com.android.chrome');
  await sleep(6000);
}
adb('forward', 'tcp:9222', 'localabstract:chrome_devtools_remote');
const browser = await chromium.connectOverCDP('http://127.0.0.1:9222');
const context = browser.contexts()[0];
// Its own tab, so another test on this phone keeps its page.
const page = context.pages().find(p => p.url().startsWith(APP)) || (await context.newPage());
await page.bringToFront();
page.on('pageerror', e => console.log('page error:', e.message));
if (!page.url().startsWith(APP)) await page.goto(APP, { waitUntil: 'commit', timeout: 120000 });
await page.waitForLoadState('domcontentloaded', { timeout: 120000 });
const signed = await page.evaluate(async () => (await (await fetch('api/auth/session')).json()).user);
if (!signed)
  await page.evaluate(
    async ({ username, password }) =>
      fetch('api/auth/login', { method: 'POST', headers: { 'content-type': 'application/json' }, body: JSON.stringify({ username, password }) }),
    { username: creds.username, password: creds.password },
  );
await page.goto(new URL('#/clutches', APP).href, { waitUntil: 'commit' });
// Cards (the phone's default), the ongoing list, no search left from a run before.
await page.evaluate(() => {
  for (const k of Object.keys(localStorage)) if (k.includes('entry-mode:clutches')) localStorage.removeItem(k);
  for (const k of Object.keys(sessionStorage)) if (k.includes('clutches:')) sessionStorage.removeItem(k);
  // Drafts left by an interrupted run of this test (a local copy only: see APP above).
  for (const k of Object.keys(localStorage)) if (/^ithomiini:pending:[^:]+$/.test(k)) localStorage.removeItem(k);
});
await page.reload({ waitUntil: 'commit', timeout: 120000 });
await page.waitForSelector('ul > li', { timeout: 90000 });
await sleep(2000);
const box = async locator => {
  await locator.scrollIntoViewIfNeeded();
  await sleep(250);
  const b = await locator.boundingBox();
  return [b.x + b.width / 2, b.y + b.height / 2];
};
const tapOn = async locator => tap(page, ...(await box(locator)));
const type = async text => {
  adb('shell', 'input', 'text', text);
  await sleep(900);
};
async function closeKeyboard() {
  await page.evaluate(() => document.activeElement?.blur());
  for (let i = 0; i < 10 && (await keyboardOver(page)); i++) await sleep(300);
  if (await keyboardOver(page)) {
    adb('shell', 'input', 'keyevent', '111');
    await sleep(800);
  }
}
/** Is the focused box and the footer's button inside what is visible (above the keyboard)? */
const inView = () =>
  page.evaluate(() => {
    const vv = visualViewport;
    const bottom = vv.offsetTop + vv.height;
    const r = document.activeElement.getBoundingClientRect();
    // On a phone on its side the footer steps aside while typing (display: none): "hidden".
    const el = [...document.querySelectorAll('footer')].pop();
    const f = el && el.offsetParent !== null ? el.getBoundingClientRect() : null;
    return {
      keyboard: vv.height < innerHeight - 120,
      field: r.top >= vv.offsetTop && r.bottom <= bottom + 1,
      footer: !f ? 'hidden' : f.top >= vv.offsetTop - 1 && f.bottom <= bottom + 1,
      fieldAboveFooter: !f || r.bottom <= f.top + 1,
    };
  });

// 1. The ongoing clutches as cards.
report('mode buttons', await page.locator('[role=group] button[aria-pressed=true]').first().getAttribute('aria-pressed'));
report('first cards', (await page.locator('ul > li .text-xl').allInnerTexts()).slice(0, 5));
await shot('1-list');
// 2. Open the first clutch.
await tapOn(page.locator('ul > li > button').first());
await sleep(1200);
const editor = page.locator('[role=dialog], [role=region]').last();
report('editor', (await editor.locator('header').innerText()).replace(/\n/g, ' · '));
await shot('2-editor');
// 3. Larvae: type a count with Gboard (keyboard up, box and Save visible), "Counted".
const larvae = editor.locator('section').nth(1);
const before = (await larvae.innerText()).split('\n').slice(0, 2).join(' ');
await tapOn(larvae.locator('input[inputmode=numeric]'));
await sleep(1500);
// Measured before typing: text sent through adb makes SwiftKey fold into its bar (a "hardware" keyboard).
report('count box focused (keyboard, box visible, footer visible, box above footer)', await inView());
await shot('3-keyboard');
await type('2');
await sleep(600);
// SwiftKey folded into its bar (after adb's text) lies over the page without resizing it: leave the box first.
await closeKeyboard();
await tapOn(larvae.locator('button', { hasText: 'Counted' }));
await sleep(600);
report(`larvae ${before} → counted 2`, (await larvae.innerText()).split('\n').slice(0, 9).join(' '));
await closeKeyboard();
await sleep(500);
await shot('4-counted');
// 4. Remove the last term (undo), then +1 hatched.
await tapOn(larvae.locator('button', { hasText: 'Remove' }));
await sleep(400);
report('after remove last', (await larvae.innerText()).split('\n').slice(0, 8).join(' '));
await tapOn(larvae.locator('input[inputmode=numeric]'));
await sleep(1000);
await type('1');
await closeKeyboard();
await tapOn(larvae.locator('button', { hasText: 'hatched' }));
await sleep(500);
await closeKeyboard();
report('after +1', (await larvae.innerText()).split('\n').slice(0, 9).join(' '));
// 5. A note typed with the keyboard.
const note = editor.locator('textarea');
await tapOn(note);
await sleep(1500);
report('note box focused', await inView());
await shot('5-note-keyboard');
await type('1%slarva%sdead');
await closeKeyboard();
await tapOn(editor.locator('button', { hasText: 'Add note' }));
await sleep(400);
// 6. Save · checked.
report('footer', (await editor.locator('footer').innerText()).replace(/\n/g, ' | '));
await tapOn(editor.locator('footer button.btn-primary'));
await sleep(5000);
report('notices', await page.locator('[role=status]').allInnerTexts());
report('editor message', await page.locator('[role=alert]').allInnerTexts());
await shot('6-saved');
if (await page.locator('[role=dialog]').count()) await tapOn(page.locator('[role=dialog] button[aria-label=Close]'));
await sleep(600);
// 7. Today's changes.
await tapOn(page.locator('[role=tab]', { hasText: 'Today' }));
await sleep(1500);
report('today', (await page.locator('main ul').first().innerText().catch(() => '')).replace(/\n/g, ' | '));
await shot('7-today');
await tapOn(page.locator('[role=tab]', { hasText: 'In progress' }));
await sleep(600);
// 8. A card's "Checked, no change", then the new clutch form with the keyboard on the eggs.
await tapOn(page.locator('ul > li button', { hasText: 'Checked, no change' }).first());
await sleep(1200);
await shot('8-checked');
await tapOn(page.locator('button[aria-label="New clutch"]'));
await sleep(800);
await tapOn(page.locator('[role=dialog] input[inputmode=numeric]'));
await sleep(1500);
report('eggs box focused (keyboard, box visible, footer visible)', await inView());
await shot('9-new-keyboard');
await type('15');
await closeKeyboard();
await tapOn(page.locator('[role=dialog] button[aria-label=Close]'));
await sleep(500);
if (landscape) adb('shell', 'settings', 'put', 'system', 'user_rotation', '0');
if (process.env.IME === 'gboard') adb('shell', 'ime', 'set', previousIme);
await browser.close();
