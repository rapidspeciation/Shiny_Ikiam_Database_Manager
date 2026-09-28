import { writeFileSync } from 'node:fs';
import { execFileSync } from 'node:child_process';
import { ADB, adb, closeKeyboard, connect, keyboardOver, login, sleep, tap } from './lib.mjs';
const shot = name => writeFileSync(`${process.env.HOME}/.cache/ithomiini-test/android-${name}.png`, execFileSync(ADB, ['exec-out', 'screencap', '-p'], { maxBuffer: 64e6 }));
const { page } = await connect(); await login(page);
await page.goto('https://ithomiini-ikiam.duckdns.org/#/colecta'); await page.reload(); await page.waitForSelector('text=Filas a añadir'); await sleep(3000);
await page.locator('label:has-text("Collection_location") input').fill('Cavernas Templo de Ceremonia');
await page.locator('label:has-text("Filas a añadir") input').fill('4'); await page.locator('button:has-text("Añadir 4 filas")').click();
await page.waitForSelector('.collect-grid .tabulator-row'); await sleep(1200); await closeKeyboard(page); await sleep(600);
const cell = (r, f) => page.locator('.collect-grid .tabulator-row').nth(r).locator(`.tabulator-cell[tabulator-field="${f}"]`);
const centre = async (r, f, dx = 0) => { await cell(r, f).scrollIntoViewIfNeeded(); await sleep(300); const b = await cell(r, f).boundingBox(); return [dx ? b.x + b.width - dx : b.x + b.width / 2, b.y + b.height / 2] };
// 1. double tap SPECIES, type with Gboard, Enter
let [x, y] = await centre(1, 'species');
await tap(page, x, y); await sleep(150); await tap(page, x, y); await sleep(1200);
adb('shell', 'input', 'text', 'Ithomia%ssal'); await sleep(1500); shot('typing');
console.log('typing: keyboard', await keyboardOver(page), '| box has', await page.evaluate(() => document.activeElement?.value));
adb('shell', 'input', 'keyevent', '66'); await sleep(1200);
console.log('after Enter: species =', await cell(1, 'species').innerText(), '| keyboard', await keyboardOver(page));
await closeKeyboard(page); await sleep(800);
// 2. the arrow of Sex: only the list, no keyboard
[x, y] = await centre(1, 'sex'); await tap(page, x, y); await sleep(900);
const [ax, ay] = await centre(1, 'sex', 10); await tap(page, ax, ay); await sleep(1200); shot('arrow');
console.log('arrow: list', (await page.locator('.tabulator-edit-list-item').allInnerTexts()).join(', '), '| keyboard', await keyboardOver(page));
const item = await page.locator('.tabulator-edit-list-item', { hasText: /^female$/ }).boundingBox();
await tap(page, item.x + item.width / 2, item.y + item.height / 2); await sleep(900);
console.log('after choosing: sex =', await cell(1, 'sex').innerText());
page.once('dialog', d => d.accept()); await page.locator('button[title="Vaciar lista"]').first().click();
process.exit(0);
