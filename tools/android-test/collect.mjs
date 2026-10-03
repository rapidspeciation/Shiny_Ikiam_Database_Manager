// Colecta as cards (components/collect), driven on the emulator with its real keyboard. Point it at a
// LOCAL app (LOCAL_MODE, never production: it saves a collecting day and undoes it):
//   adb reverse tcp:8811 tcp:8811      (the phone's localhost:8811 is this PC's; a secure context)
//   APP=http://localhost:8811/ CREDS=~/.cache/ithomiini-lab/credentials.json node tools/android-test/collect.mjs
// Screenshots go to $SHOTS (default /tmp), named collect-<step>.png.
import { readFileSync } from 'node:fs';
import { adb, keyboardOver, screenshot, sleep, tap } from './lib.mjs';
import { chromium } from '/home/franz/.local/share/ithomiini-wikiloc/node_modules/playwright-core/index.mjs';

const APP = process.env.APP || 'http://localhost:8811/';
if (!/localhost|127\.0\.0\.1/.test(APP)) throw new Error('Only a local app: this test saves and undoes a collecting day');
const creds = JSON.parse(readFileSync(process.env.CREDS.replace(/^~/, process.env.HOME), 'utf8'));
const SHOTS = process.env.SHOTS || '/tmp';
const shot = name => screenshot(`${SHOTS}/collect-${name}.png`);
const report = (what, value) => console.log(`${what}:`, typeof value === 'string' ? value : JSON.stringify(value));

adb('shell', 'settings', 'put', 'system', 'accelerometer_rotation', '0');
adb('shell', 'settings', 'put', 'system', 'user_rotation', '0');
adb('forward', 'tcp:9222', 'localabstract:chrome_devtools_remote');
const browser = await chromium.connectOverCDP('http://127.0.0.1:9222');
// Its own tab (another test may be driving another app's tab on the same phone).
const page = browser.contexts()[0].pages().find(p => p.url().startsWith(APP)) || (await browser.contexts()[0].newPage());
page.on('pageerror', e => console.log('page error:', e.message));
if (!page.url().startsWith(APP)) await page.goto(APP);
await page.bringToFront();
const signed = await page.evaluate(async () => (await (await fetch('api/auth/session')).json()).user);
if (!signed)
  await page.evaluate(
    async ({ username, password }) =>
      fetch('api/auth/login', { method: 'POST', headers: { 'content-type': 'application/json' }, body: JSON.stringify({ username, password }) }),
    { username: creds.username, password: creds.password },
  );
await page.goto(new URL('#/colecta', APP).href);
// A fresh day: no list or day left from an earlier run, and the default mode (cards).
await page.evaluate(() => {
  for (const k of Object.keys(localStorage)) if (k.startsWith('ithomiini:collect') || k === 'ithomiini:entry-mode:collect') localStorage.removeItem(k);
  for (const k of Object.keys(sessionStorage)) if (k.startsWith('ithomiini:collect')) sessionStorage.removeItem(k);
});
await page.reload();
await page.waitForSelector('[data-day]', { timeout: 90000 });
await page.waitForFunction(() => !/Loading Collection_data|Cargando Collection_data/.test(document.body.innerText), null, { timeout: 120000 });
await sleep(1500);

const tapOn = async locator => {
  await locator.evaluate(el => el.scrollIntoView({ block: 'center' }));
  await sleep(300);
  const b = await locator.boundingBox();
  await tap(page, b.x + b.width / 2, b.y + b.height / 2);
  await sleep(400);
};
/** The first character alone (the keyboard's toolbar changes on the first key), then the rest. */
const type = async text => {
  adb('shell', 'input', 'text', text.slice(0, 1));
  await sleep(700);
  if (text.length > 1) adb('shell', 'input', 'text', text.slice(1));
  await sleep(900);
};
/** The box being typed in, inside the part of the screen above the keyboard? */
const fieldInView = () =>
  page.evaluate(() => {
    const el = document.activeElement;
    if (!el || el === document.body) return { field: false };
    const r = el.getBoundingClientRect();
    const vv = visualViewport;
    return { keyboard: vv.height < innerHeight - 120, field: r.top >= vv.offsetTop - 1 && r.bottom <= vv.offsetTop + vv.height + 1 };
  });
async function closeKeyboard() {
  await page.evaluate(() => document.activeElement?.blur());
  for (let i = 0; i < 10 && (await keyboardOver(page)); i++) await sleep(300);
  if (await keyboardOver(page)) {
    adb('shell', 'input', 'keyevent', '111');
    await sleep(800);
  }
  if (!page.url().includes('#/colecta')) throw new Error('left Colecta while closing the keyboard');
  await sleep(400);
}

await shot('1-day');
// 1. The day with taps only: place, two collectors, the identifier.
const chips = heading => page.locator(`section[aria-label] h2:has-text("${heading}") + div button`);
const placeHeading = (await page.evaluate(() => document.documentElement.lang)) === 'es' ? 'Lugar' : 'Place';
await tapOn(chips(placeHeading).first());
const who = (await chips('Who collected').count()) ? 'Who collected' : 'Quiénes colectaron';
await tapOn(chips(who).nth(0));
await tapOn(chips(who).nth(1));
const ident = (await chips('Who identified').count()) ? 'Who identified' : 'Quién identificó';
await tapOn(chips(ident).first());
report('day', await page.locator('[data-day]').innerText());
await tapOn(page.locator('section[aria-label] button.btn-primary').last());
await sleep(600);
await shot('2-first-card');

// 2. First butterfly: a species chip, ♀, to the insectary; the time typed with the keyboard.
let card = page.locator('[data-card]').last();
await tapOn(card.locator('button.rounded-full').first());
await tapOn(card.locator('[data-sex=female]'));
// Measured right after the tap: text sent with adb folds the keyboard into its bar (as for a hardware keyboard).
await tapOn(card.locator('[data-field="time"]'));
await sleep(1500);
report('time box above the keyboard', await fieldInView());
await shot('3-typing-time');
await type('1015');
await closeKeyboard();
report('time read as', await card.locator('[data-field="time"]').inputValue());

// 3. Another like this, made male and preserved; its weight typed.
await tapOn(card.locator('button', { hasText: /Another like this|Otra igual/ }));
card = page.locator('[data-card]').last();
await tapOn(card.locator('[data-sex=male]'));
await tapOn(card.locator('button[aria-pressed]', { hasText: /Preserved|Preservada/ }));
report('CAM and tube suggested', [await card.locator('[data-field="cam"]').inputValue(), await card.locator('[data-field="tube"]').inputValue()]);
await tapOn(card.locator('[data-field="weight"]'));
await sleep(1500);
report('weight box above the keyboard', await fieldInView());
await shot('4-typing-weight');
await type('0.152');
await closeKeyboard();

// 4. A third from the species search, typed in the notebook's shorthand.
await tapOn(page.locator('[data-add]'));
card = page.locator('[data-card]').last();
await tapOn(card.locator('[data-species]'));
await sleep(1500);
report('keyboard up for the search', await keyboardOver(page));
await type('mech%smess'); // adb's text: %s is a space
report('options', (await page.locator('[role=option]').allInnerTexts()).slice(0, 3).map(s => s.replace(/\n/g, ' ')));
await shot('5-species-search');
await tapOn(page.locator('[role=option]').nth(1));
await closeKeyboard();
await tapOn(card.locator('[data-sex=male]'));
await shot('6-third-card');

// 5. Save the day, then undo it: the cards come back.
const save = page.locator('footer button.btn-primary');
report('footer', (await page.locator('footer').innerText()).replace(/\n+/g, ' | '));
await tapOn(save);
await shot('7-summary');
await tapOn(page.locator('[role=dialog] button.btn-primary'));
await page.waitForFunction(() => /Undo|Deshacer/.test(document.querySelector('footer')?.innerText || ''), null, { timeout: 30000 });
report('after save', (await page.locator('footer').innerText()).replace(/\n+/g, ' | '));
await shot('8-saved');
await tapOn(page.locator('footer button', { hasText: /Undo|Deshacer/ }));
await page.waitForFunction(() => document.querySelectorAll('[data-card]').length > 0, null, { timeout: 30000 });
report('cards after undo', await page.locator('[data-card]').count());
await shot('9-undone');
await browser.close();
