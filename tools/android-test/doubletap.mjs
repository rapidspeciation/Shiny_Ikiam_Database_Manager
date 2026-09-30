import { writeFileSync } from 'node:fs';
import { execFileSync } from 'node:child_process';
import { ADB, closeKeyboard, connect, keyboardOver, login, sleep, tap } from './lib.mjs';
const { page } = await connect();
await login(page);
await page.goto('https://ithomiini-ikiam.com/#/colecta'); await page.reload(); await page.waitForSelector('text=Filas a añadir'); await sleep(3000);
await page.locator('label:has-text("Collection_location") input').fill('Cavernas Templo de Ceremonia');
await page.locator('label:has-text("Filas a añadir") input').fill('6'); await page.locator('button:has-text("Añadir 6 filas")').click();
await page.waitForSelector('.collect-grid .tabulator-row'); await sleep(1500);
const cell = page.locator('.collect-grid .tabulator-row').nth(4).locator(`.tabulator-cell[tabulator-field="${process.argv[2] || "notes"}"]`);
await cell.scrollIntoViewIfNeeded(); await closeKeyboard(page); await sleep(800);
console.log('keyboard before the taps:', await keyboardOver(page), '| visible height', await page.evaluate(() => Math.round(visualViewport.height)));
await page.evaluate(() => {
  window.__log = []; const t0 = performance.now(); const log = m => window.__log.push(`${Math.round(performance.now() - t0)}ms ${m}`);
  for (const ev of ['focusin', 'focusout']) document.addEventListener(ev, e => log(`${ev} ${e.target.tagName}.${(e.target.className || '').toString().slice(0, 30)}`), true);
  window.addEventListener('resize', () => log(`window resize ${innerWidth}x${innerHeight}`));
  visualViewport.addEventListener('resize', () => log(`visualViewport ${Math.round(visualViewport.height)}`));
  new MutationObserver(() => log(`body class: ${document.body.className}`)).observe(document.body, { attributes: true, attributeFilter: ['class'] });
  for (const ev of ['click', 'dblclick']) document.addEventListener(ev, e => log(`${ev} on ${e.target.closest?.('.tabulator-cell')?.getAttribute('tabulator-field') || (e.target.tagName + ' "' + (e.target.textContent || '').trim().slice(0, 20) + '" ' + (e.target.closest('[class]')?.className || '').toString().slice(0, 40))}`), true);
});
const b = await cell.boundingBox(); const x = b.x + b.width / 2, y = b.y + b.height / 2;
console.log('cell centre', Math.round(x), Math.round(y), '| visible height', await page.evaluate(() => innerHeight));
await tap(page, x, y); await sleep(150);
console.log('at the tap point before the 2nd tap:', await page.evaluate(([x, y]) => { const el = document.elementFromPoint(x, y); return el?.tagName + ' ' + (el?.className || '').toString().slice(0, 50) + ' "' + (el?.textContent || '').trim().slice(0, 20) + '"' }, [x, y]));
await tap(page, x, y);
const samples = [];
for (let i = 0; i < 20; i++) {
  await sleep(150);
  samples.push(await page.evaluate(() => ({
    editing: !!document.querySelector('.tabulator-editing'), active: document.activeElement?.tagName, vv: Math.round(visualViewport.height), full: innerHeight,
  })).then(s => `${(i + 1) * 150}ms editing=${s.editing} focus=${s.active} visible=${s.vv}/${s.full} keyboard=${s.vv < s.full - 120}`));
  if (i === 5) writeFileSync(process.env.HOME + '/.cache/ithomiini-test/android-dt.png', execFileSync(ADB, ['exec-out', 'screencap', '-p'], { maxBuffer: 64e6 }));
}
console.log(samples.join('\n'));
console.log('--- events\n' + (await page.evaluate(() => window.__log)).join('\n'));
await page.keyboard.press('Escape').catch(() => {});
page.once('dialog', d => d.accept()); await page.locator('button[title="Vaciar lista"]').first().click();
process.exit(0);
