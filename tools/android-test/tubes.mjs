// Tubos as cards (components/tubes/TubesCards.vue) on the emulator with Gboard. It types but never saves;
// still, point it at a LOCAL app (LOCAL_MODE, its own database):
//   adb reverse tcp:8847 tcp:8847
//   APP=http://localhost:8847/ CREDS=~/.cache/ithomiini-lab/credentials.json node tools/android-test/tubes.mjs Z8D-A2E Z9D A0E
// Arguments: what to paste in the search (an ID range), then two cards in a row that get a tube box.
// It checks: the cards appear; a tube box opens the number pad and stays above it; Enter goes to the
// next card's tube box with its suggestion selected (the next tube typed replaces it).
// Screenshots go to $SHOTS (default /tmp): tubes-1-cards.png, tubes-2-keyboard.png, tubes-3-next.png.
import { readFileSync } from 'node:fs';
import { adb, closeKeyboard, connect, keyboardOver, screenshot, sleep, tap } from './lib.mjs';

const APP = process.env.APP || 'http://localhost:8847/';
if (!/localhost|127\.0\.0\.1/.test(APP)) throw new Error('Only a local app');
const creds = JSON.parse(readFileSync(process.env.CREDS.replace(/^~/, process.env.HOME), 'utf8'));
const SHOTS = process.env.SHOTS || '/tmp';
const [range, first, second] = process.argv.slice(2);
if (!second) throw new Error('Give a range to paste and two cards of it, e.g. Z8D-A2E Z9D A0E');

const { browser, context } = await connect();
const page = await context.newPage();
page.on('pageerror', e => console.log('page error:', e.message));
await page.goto(APP);
await page.evaluate(
  async ({ username, password }) =>
    fetch('api/auth/login', { method: 'POST', headers: { 'content-type': 'application/json' }, body: JSON.stringify({ username, password }) }),
  { username: creds.username, password: creds.password },
);
await page.goto(new URL('#/tubos', APP).href);
// A fresh screen in the default mode (cards).
await page.evaluate(() => {
  Object.keys(sessionStorage).filter(k => k.startsWith('ithomiini:tubes:')).forEach(k => sessionStorage.removeItem(k));
  localStorage.removeItem('ithomiini:entry-mode:tubes');
});
await page.reload();
await page.bringToFront();
const search = page.locator('input[enterkeyhint="go"]');
await search.waitFor({ timeout: 90000 });
await page.waitForFunction(() => !document.body.innerText.includes('Loading Insectary_data'), null, { timeout: 180000 });
await sleep(1000);
const center = async locator => {
  await locator.evaluate(el => el.scrollIntoView({ block: 'center' }));
  await sleep(300);
  const b = await locator.boundingBox();
  return [b.x + b.width / 2, b.y + b.height / 2];
};
let failed = false;
const check = (what, ok, detail = '') => {
  console.log(`${ok ? 'ok  ' : 'FAIL'} ${what}${detail ? `: ${detail}` : ''}`);
  if (!ok) failed = true;
};

await tap(page, ...(await center(search)));
await sleep(800);
adb('shell', 'input', 'text', range);
await sleep(400);
adb('shell', 'input', 'keyevent', '66');
await sleep(1500);
check('cards added', (await page.locator('[data-card]').count()) > 1, `${await page.locator('[data-card]').count()} cards`);
await closeKeyboard(page);
await sleep(800);
await screenshot(`${SHOTS}/tubes-1-cards.png`);

await tap(page, ...(await center(page.locator(`[data-box="${first}:0"]`))));
await sleep(1500);
const box = await page.evaluate(() => {
  const el = document.activeElement;
  const r = el?.getBoundingClientRect();
  return { box: el?.getAttribute('data-box'), inputmode: el?.getAttribute('inputmode'), bottom: r?.bottom ?? 0, visible: visualViewport.offsetTop + visualViewport.height };
});
check('the tube box has the focus', box.box === `${first}:0`, box.box);
check('number pad', box.inputmode === 'numeric' && (await keyboardOver(page)));
check('the box is above the keyboard', box.bottom <= box.visible, `${Math.round(box.bottom)} ≤ ${Math.round(box.visible)}`);
await screenshot(`${SHOTS}/tubes-2-keyboard.png`);

const before = await page.locator(`[data-box="${first}:0"]`).inputValue();
adb('shell', 'input', 'text', before.replace(/\d$/, d => String((Number(d) + 5) % 10)));
await sleep(400);
adb('shell', 'input', 'keyevent', '66');
await sleep(1500);
const next = await page.evaluate(() => {
  const el = document.activeElement;
  return { box: el?.getAttribute('data-box'), selected: el ? el.selectionEnd - el.selectionStart : 0, length: el?.value.length ?? 0 };
});
check("Enter goes to the next card's tube", next.box === `${second}:0`, next.box);
check('its suggestion is selected', next.length > 0 && next.selected === next.length, `${next.selected}/${next.length}`);
await screenshot(`${SHOTS}/tubes-3-next.png`);

await closeKeyboard(page);
await page.evaluate(() => Object.keys(sessionStorage).filter(k => k.startsWith('ithomiini:tubes:')).forEach(k => sessionStorage.removeItem(k)));
await page.close();
await browser.close();
if (failed) process.exit(1);
