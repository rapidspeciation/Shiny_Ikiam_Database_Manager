// Muertes on the phone (components/deaths), driven on the emulator with Gboard. Point it at a
// LOCAL app (LOCAL_MODE, never production: it saves deaths and undoes them):
//   adb reverse tcp:8796 tcp:8796      (the phone's localhost:8796 is this PC's; a secure context)
//   APP=http://localhost:8796/ CREDS=~/.cache/ithomiini-lab/credentials.json DB=/tmp/app.sqlite node tools/android-test/deaths.mjs A7E A6E A5E
// The IDs must be alive in that copy. Screenshots go to $SHOTS (default /tmp).
import { execFileSync } from 'node:child_process';
import { readFileSync } from 'node:fs';
import { adb, keyboardOver, screenshot, sleep, tap } from './lib.mjs';
import { chromium } from '/home/franz/.local/share/ithomiini-wikiloc/node_modules/playwright-core/index.mjs';

const APP = process.env.APP || 'http://localhost:8796/';
if (!/localhost|127\.0\.0\.1/.test(APP)) throw new Error('Only a local app: this test saves and undoes deaths');
const creds = JSON.parse(readFileSync(process.env.CREDS.replace(/^~/, process.env.HOME), 'utf8'));
const SHOTS = process.env.SHOTS || '/tmp';
const ids = process.argv.slice(2);
if (ids.length < 3) throw new Error('Give three living Insectary IDs');
const shot = name => screenshot(`${SHOTS}/deaths-${name}.png`);
const sql = q => (process.env.DB ? execFileSync('sqlite3', [process.env.DB, q], { encoding: 'utf8' }).trim() : '');
const cells = id =>
  sql(`select json_extract(values_json,'$.Death_date')||' '||ifnull(json_extract(values_json,'$.Death_cause'),'-')||' '||ifnull(json_extract(values_json,'$.CAM_ID'),'-')||' '||ifnull(json_extract(values_json,'$.Tube_1_id'),'-')||' '||ifnull(json_extract(values_json,'$.T1_Preservation_medium'),'-') from records where sheet='Insectary_data' and observed=1 and json_extract(values_json,'$.Insectary_ID')='${id}'`);

adb('forward', 'tcp:9222', 'localabstract:chrome_devtools_remote');
const browser = await chromium.connectOverCDP('http://127.0.0.1:9222');
const page = browser.contexts()[0].pages().find(p => p.url().startsWith(APP)) || browser.contexts()[0].pages()[0];
page.on('pageerror', e => console.log('page error:', e.message));
if (!page.url().startsWith(APP)) await page.goto(APP);
const signed = await page.evaluate(async () => (await (await fetch('api/auth/session')).json()).user);
if (!signed)
  await page.evaluate(
    async ({ username, password }) =>
      fetch('api/auth/login', { method: 'POST', headers: { 'content-type': 'application/json' }, body: JSON.stringify({ username, password }) }),
    { username: creds.username, password: creds.password },
  );
await page.goto(new URL('#/muertes', APP).href);
// A fresh screen: no cards or cause left from an earlier run.
await page.evaluate(() => Object.keys(sessionStorage).filter(k => k.startsWith('ithomiini:deaths:phone')).forEach(k => sessionStorage.removeItem(k)));
await page.reload();
const search = page.locator('input[enterkeyhint="go"]');
await search.waitFor({ timeout: 60000 });
await page.waitForFunction(() => !document.body.innerText.includes('Loading Insectary_data'), null, { timeout: 90000 });
await sleep(1500);
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
const footer = async () => (await page.locator('footer').innerText().catch(() => '')).replace(/\n+/g, ' | ');
const cardIds = () => page.locator('section li .text-xl').allInnerTexts();
const report = (what, value) => console.log(`${what}:`, value);
/** Leaves the box (Chrome then hides the keyboard); the back key only if it stays, since back can leave the page. */
async function closeKeyboard() {
  await page.evaluate(() => document.activeElement?.blur());
  for (let i = 0; i < 10 && (await keyboardOver(page)); i++) await sleep(300);
  if (await keyboardOver(page)) {
    adb('shell', 'input', 'keyevent', '111');
    await sleep(800);
  }
  if (!page.url().includes('#/muertes')) throw new Error('left Muertes while closing the keyboard');
}

// 1. Search an alive ID with Gboard: suggestions with status, keyboard up, field in view.
await tapOn(search);
await sleep(1200);
await type(ids[0].slice(0, 2));
report('keyboard while typing', await keyboardOver(page));
report('suggestions', (await page.locator('[role=option]').allInnerTexts()).slice(0, 3).map(s => s.replace(/\n/g, ' · ')));
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
// 3. A cause button, not preserved, Save: the footer is above the keyboard when it is open.
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
for (const id of ids) report(`  ${id} in the DB`, cells(id));
await shot('4-saved');
// 4. Undo: the cells go back, the cards come back.
await tapOn(page.locator('footer button', { hasText: 'Undo' }));
await page.waitForFunction(() => !document.querySelector('footer')?.textContent?.includes('saved to'), null, { timeout: 30000 });
await sleep(800);
report('cards after undo', await cardIds());
for (const id of ids) report(`  ${id} in the DB`, cells(id));
await shot('5-undone');
// 5. One preserved: Killed_Preserved, CAM and tube suggested; type a tube with the keyboard up.
for (const id of ids.slice(1)) await tapOn(page.locator(`button[aria-label="Remove ${id}"]`));
await tapOn(page.locator('button[aria-pressed]', { hasText: 'Killed_Preserved' }).first());
await sleep(2500);
const tube = page.locator('section li input').nth(1);
report('suggested CAM / tube', [await page.locator('section li input').nth(0).inputValue(), await tube.inputValue()]);
await tapOn(tube);
await sleep(1500);
const visible = await page.evaluate(() => {
  const vv = visualViewport;
  const r = document.activeElement.getBoundingClientRect();
  const f = document.querySelector('footer').getBoundingClientRect();
  return { keyboard: vv.height < innerHeight - 120, field: r.top >= vv.offsetTop && r.bottom <= vv.offsetTop + vv.height, save: f.bottom <= vv.offsetTop + vv.height + 1 && f.top >= vv.offsetTop, fieldAboveSave: r.bottom <= f.top };
});
report('typing the tube (keyboard, field visible, save visible, field above save)', visible);
await shot('6-preserved-keyboard');
await closeKeyboard();
report('footer', await footer());
await tapOn(page.locator('footer button.btn-primary'));
await page.waitForSelector('footer:has-text("saved to Google Sheets")', { timeout: 30000 });
report(`${ids[0]} preserved in the DB`, cells(ids[0]));
await shot('7-preserved-saved');
// 6. Check an already dead ID: its status without registering anything.
await tapOn(search);
await sleep(1000);
await type(ids[0]);
report('dead ID suggestion', (await page.locator('[role=option]').allInnerTexts()).slice(0, 1).map(s => s.replace(/\n/g, ' · ')));
await shot('8-dead-check');
adb('shell', 'input', 'keyevent', '66');
await sleep(800);
await closeKeyboard();
report('card', (await page.locator('section li').first().innerText()).replace(/\n/g, ' · '));
// 7. The editor, from the card; swipe to nowhere, close.
await tapOn(page.locator('section li > button').first());
await sleep(800);
report('editor', (await page.locator('[role=dialog] header').innerText()).replace(/\n/g, ' · '));
await shot('9-editor');
await tapOn(page.locator('[role=dialog] footer button.btn-primary'));
// Leave the screen clean (the preserved death stays in the local copy).
await tapOn(page.locator('button', { hasText: 'Remove all' }));
await sleep(500);
await shot('10-end');
await browser.close();
