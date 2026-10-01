// Muertes as cards (components/deaths/DeathsCards.vue), driven on the emulator with Gboard. Point it at a
// LOCAL app (LOCAL_MODE, never production: it saves deaths and undoes them):
//   adb reverse tcp:8796 tcp:8796      (the phone's localhost:8796 is this PC's; a secure context)
//   APP=http://localhost:8796/ CREDS=~/.cache/ithomiini-lab/credentials.json DB=/tmp/app.sqlite node tools/android-test/deaths.mjs A7E A6E A5E
// The IDs must be alive in that copy. ORIENT=landscape turns the phone sideways first (and back at the end).
// Screenshots go to $SHOTS (default /tmp), named deaths-<orientation>-<step>.png.
import { execFileSync } from 'node:child_process';
import { readFileSync } from 'node:fs';
import { adb, keyboardOver, screenshot, sleep, tap } from './lib.mjs';
import { chromium } from '/home/franz/.local/share/ithomiini-wikiloc/node_modules/playwright-core/index.mjs';

const APP = process.env.APP || 'http://localhost:8796/';
if (!/localhost|127\.0\.0\.1/.test(APP)) throw new Error('Only a local app: this test saves and undoes deaths');
const creds = JSON.parse(readFileSync(process.env.CREDS.replace(/^~/, process.env.HOME), 'utf8'));
const SHOTS = process.env.SHOTS || '/tmp';
const ORIENT = process.env.ORIENT === 'landscape' ? 'landscape' : 'portrait';
const ids = process.argv.slice(2);
if (ids.length < 3) throw new Error('Give three living Insectary IDs');
const shot = name => screenshot(`${SHOTS}/deaths-${ORIENT}-${name}.png`);
const sql = q => (process.env.DB ? execFileSync('sqlite3', [process.env.DB, q], { encoding: 'utf8' }).trim() : '');
const cells = id =>
  sql(
    `select json_extract(values_json,'$.Death_date')||' '||ifnull(json_extract(values_json,'$.Death_cause'),'-')||' '||ifnull(json_extract(values_json,'$.CAM_ID'),'-')||' '||ifnull(json_extract(values_json,'$.Tube_1_id'),'-')||' '||ifnull(json_extract(values_json,'$.Tube_1_tissue'),'-')||' '||ifnull(json_extract(values_json,'$.Tube_2_tissue'),'-')||' '||ifnull(json_extract(values_json,'$.T1_Preservation_medium'),'-') from records where sheet='Insectary_data' and observed=1 and json_extract(values_json,'$.Insectary_ID')='${id}'`,
  );

/** Turns the phone (auto-rotate off; 0 upright, 1 sideways) and waits for Chrome to follow. */
async function rotate(to) {
  adb('shell', 'settings', 'put', 'system', 'accelerometer_rotation', '0');
  adb('shell', 'settings', 'put', 'system', 'user_rotation', to === 'landscape' ? '1' : '0');
  await sleep(2500);
}
await rotate(ORIENT);
adb('forward', 'tcp:9222', 'localabstract:chrome_devtools_remote');
const browser = await chromium.connectOverCDP('http://127.0.0.1:9222');
const page = browser.contexts()[0].pages().find(p => p.url().startsWith(APP)) || browser.contexts()[0].pages()[0];
page.on('pageerror', e => console.log('page error:', e.message));
if (!page.url().startsWith(APP)) await page.goto(APP);
// Taps go to the tab on screen: this one (another test may have left its own in front).
await page.bringToFront();
const signed = await page.evaluate(async () => (await (await fetch('api/auth/session')).json()).user);
if (!signed)
  await page.evaluate(
    async ({ username, password }) =>
      fetch('api/auth/login', { method: 'POST', headers: { 'content-type': 'application/json' }, body: JSON.stringify({ username, password }) }),
    { username: creds.username, password: creds.password },
  );
await page.goto(new URL('#/muertes', APP).href);
// A fresh screen: no cards or cause left from an earlier run, and the mode the device gets by default (cards).
await page.evaluate(() => {
  Object.keys(sessionStorage).filter(k => k.startsWith('ithomiini:deaths:')).forEach(k => sessionStorage.removeItem(k));
  localStorage.removeItem('ithomiini:entry-mode:deaths');
});
await page.reload();
const search = page.locator('input[enterkeyhint="go"]');
await search.waitFor({ timeout: 60000 });
await page.waitForFunction(() => !document.body.innerText.includes('Loading Insectary_data'), null, { timeout: 90000 });
await sleep(1500);
const report = (what, value) => console.log(`${what}:`, typeof value === 'string' ? value : JSON.stringify(value));
report('screen', await page.evaluate(() => `${innerWidth}x${innerHeight}, two columns: ${!!document.querySelector('aside') && getComputedStyle(document.querySelector('aside')).display !== 'none'}`));
const box = async locator => {
  // Centred in its column: scrolled to an edge it could sit under the sticky search bar.
  await locator.evaluate(el => el.scrollIntoView({ block: 'center' }));
  await sleep(250);
  const b = await locator.boundingBox();
  return [b.x + b.width / 2, b.y + b.height / 2];
};
const tapOn = async locator => {
  const [x, y] = await box(locator);
  try {
    await tap(page, x, y);
  } catch (e) {
    const view = await page.evaluate(() => [innerWidth, innerHeight, visualViewport.offsetTop, visualViewport.height]);
    throw new Error(`${e.message}: tap at ${Math.round(x)},${Math.round(y)}; window and visible area ${view.map(Math.round)}`);
  }
};
/**
 * Types with the keyboard: the first character on its own, since sideways Gboard swaps its toolbar
 * for the suggestion strip on the first key and adb's instant text would land before the caret moves.
 */
const type = async text => {
  adb('shell', 'input', 'text', text.slice(0, 1));
  await sleep(700);
  if (text.length > 1) adb('shell', 'input', 'text', text.slice(1));
  await sleep(900);
};
const footer = async () => (await page.locator('footer').innerText().catch(() => '')).replace(/\n+/g, ' | ');
const cardIds = () => page.locator('section li .text-xl').allInnerTexts();
/** What the person sees while typing: the box and the Save button inside the visible part of the screen. */
const inView = () =>
  page.evaluate(() => {
    const vv = visualViewport;
    const seen = el => {
      const r = el.getBoundingClientRect();
      return r.top >= vv.offsetTop - 1 && r.bottom <= vv.offsetTop + vv.height + 1;
    };
    const field = document.activeElement;
    const save = document.querySelector('footer button.btn-primary');
    return {
      keyboard: vv.height < innerHeight - 120,
      visible: Math.round(vv.height),
      field: !!field && seen(field),
      save: !!save && seen(save),
      fieldAboveSave: !!field && !!save && field.getBoundingClientRect().bottom <= save.closest('footer').getBoundingClientRect().top + 1,
    };
  });
/** Leaves the box (Chrome then hides the keyboard); the escape key only if it stays, since back can leave the page. */
async function closeKeyboard() {
  await page.evaluate(() => document.activeElement?.blur());
  for (let i = 0; i < 10 && (await keyboardOver(page)); i++) await sleep(300);
  if (await keyboardOver(page)) {
    adb('shell', 'input', 'keyevent', '111');
    await sleep(800);
  }
  if (!page.url().includes('#/muertes')) throw new Error('left Muertes while closing the keyboard');
  // Sideways the page is only ~300 px tall: wait until the keyboard has gone completely.
  await page.waitForFunction(() => visualViewport.height >= innerHeight - 1, null, { timeout: 5000 }).catch(() => {});
  await sleep(400);
}

report('mode', (await page.locator('button[aria-pressed=true][aria-label="Cards"], button[aria-pressed=true]:has-text("Cards")').count()) ? 'cards' : 'table');
// 1. Search an alive ID with Gboard: suggestions with status, keyboard up, field in view.
await tapOn(search);
await sleep(1200);
await type(ids[0].slice(0, 2));
report('keyboard while typing', await keyboardOver(page));
report('suggestions', (await page.locator('[role=option]').allInnerTexts()).slice(0, 3).map(s => s.replace(/\n/g, ' · ')));
report('search box in view', await inView());
await shot('1-search');
// 2. Tap the suggestion: its card appears, the keyboard stays for the next ID.
await tapOn(page.locator('[role=option]', { hasText: ids[0] }).first());
await sleep(800);
report('after tapping a suggestion: keyboard', await keyboardOver(page));
report('focus still in the search box', await page.evaluate(() => document.activeElement?.getAttribute('enterkeyhint') === 'go'));
for (const id of ids.slice(1, 3)) {
  await type(id);
  adb('shell', 'input', 'keyevent', '66');
  await sleep(900);
}
report('cards', await cardIds());
await shot('2-three-cards');
// 3. A cause button, not preserved, Save.
await closeKeyboard();
await sleep(600);
await tapOn(page.locator('button[aria-pressed]', { hasText: /^\s*Unknown\s*$/ }).first());
await tapOn(page.locator('button[aria-pressed]', { hasText: 'Not preserved' }));
await sleep(500);
report('footer', await footer());
await shot('3-cause');
await tapOn(page.locator('footer button.btn-primary'));
await page.waitForSelector('footer:has-text("saved to Google Sheets")', { timeout: 30000 });
report('after save', await footer());
for (const id of ids) report(`  ${id} in the DB (date cause CAM tube1 tissue1 tissue2 medium1)`, cells(id));
await shot('4-saved');
// 4. Undo: the cells go back, the cards come back.
await tapOn(page.locator('footer button', { hasText: 'Undo' }));
await page.waitForFunction(() => !document.querySelector('footer')?.textContent?.includes('saved to'), null, { timeout: 30000 });
await sleep(800);
report('cards after undo', await cardIds());
for (const id of ids) report(`  ${id} in the DB`, cells(id));
await shot('5-undone');
// 5. Preserved: Killed_Preserved; the CAM and tube boxes show right under «Preserved», in view.
for (const id of ids.slice(1)) await tapOn(page.locator(`button[aria-label="Remove ${id}"]`));
await tapOn(page.locator('button[aria-pressed]', { hasText: 'Killed_Preserved' }).first());
await sleep(2500);
const camBox = page.locator(`[data-sample="${ids[0]}:cam"]`);
const tube = page.locator(`[data-sample="${ids[0]}:tube"]`);
report('preserved: boxes in view without scrolling', await page.evaluate(() => {
  const row = document.querySelector('[data-row]');
  const vv = visualViewport;
  const r = row?.getBoundingClientRect();
  return !!r && r.top >= vv.offsetTop && r.bottom <= vv.offsetTop + vv.height;
}));
await page.waitForFunction(sel => !!document.querySelector(sel)?.value, `[data-sample="${ids[0]}:tube"]`, { timeout: 20000 });
const freeTube = await tube.inputValue();
report('suggested CAM / tube', [await camBox.inputValue(), freeTube]);
await shot('6-preserved');
// The tube emptied: the card, the list and the Save bar say what is missing.
await page.evaluate(sel => {
  const el = document.querySelector(sel);
  el.value = '';
  el.dispatchEvent(new Event('input', { bubbles: true }));
}, `[data-sample="${ids[0]}:tube"]`);
await sleep(400);
report('footer with a missing tube', await footer());
await shot('7-missing-tube');
// Tapping the message opens the box with the keyboard: the box and Save stay visible above it.
await tapOn(page.locator('footer button', { hasText: 'Tube missing' }));
await sleep(1800);
report('typing the tube (keyboard, visible height, field visible, save visible, field above save)', await inView());
// The next free tube, typed back (one another butterfly has would be flagged before Save).
await type(freeTube);
report('after typing (the keyboard shows its suggestions)', await inView());
await sleep(500);
report('after typing: footer', await footer());
await shot('8-tube-keyboard');
await closeKeyboard();
await tapOn(page.locator('footer button.btn-primary'));
await page.waitForSelector('footer:has-text("saved to Google Sheets")', { timeout: 30000 });
report(`${ids[0]} preserved in the DB`, cells(ids[0]));
await shot('9-preserved-saved');
// 6. Check an already dead ID: its status without registering anything.
await tapOn(search);
await sleep(1000);
await type(ids[0]);
report('dead ID suggestion', (await page.locator('[role=option]').allInnerTexts()).slice(0, 1).map(s => s.replace(/\n/g, ' · ')));
adb('shell', 'input', 'keyevent', '66');
await sleep(800);
await closeKeyboard();
report('card', (await page.locator('section li').first().innerText()).replace(/\n/g, ' · '));
// 7. The editor, from the card; close.
await tapOn(page.locator('section li > button').first());
await sleep(800);
report('editor', (await page.locator('[role=dialog] header').innerText()).replace(/\n/g, ' · '));
await shot('10-editor');
await tapOn(page.locator('[role=dialog] footer button.btn-primary'));
// 8. The table, then back to the cards: the card is still there.
await tapOn(page.locator('button[aria-pressed]', { hasText: 'Table' }).or(page.locator('button[aria-label="Table"]')).first());
await sleep(1500);
report('table: chosen', await page.locator('text=/Chosen IDs \\(\\d+\\)/').innerText().catch(() => '-'));
await shot('11-table');
await tapOn(page.locator('button[aria-pressed]', { hasText: 'Cards' }).or(page.locator('button[aria-label="Cards"]')).first());
await sleep(1000);
report('cards again', await cardIds());
// Leave the screen clean (the preserved death stays in the local copy).
await tapOn(page.locator('button', { hasText: 'Remove all' }));
await sleep(500);
await page.evaluate(() => localStorage.removeItem('ithomiini:entry-mode:deaths'));
await browser.close();
if (ORIENT === 'landscape') await rotate('portrait');
