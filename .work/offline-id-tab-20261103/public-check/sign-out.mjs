// Signed in on the page with team reference photos on screen, then «Cerrar sesión»: the page reloads signed out,
// with no team photo (api/photo) or inventory left. node sign-out.mjs <base/> <out> <photo> <credentials.json>
import { readFileSync, mkdirSync } from 'node:fs';
const { chromium } = await import('/home/franz/.local/share/agent-tools/node_modules/playwright/index.mjs');
const [base, out, photo, credFile] = process.argv.slice(2);
mkdirSync(out, { recursive: true });
const cred = JSON.parse(readFileSync(credFile, 'utf8'));
const browser = await chromium.launch();
const ctx = await browser.newContext({ locale: 'es-EC', viewport: { width: 1366, height: 900 } });
await ctx.addInitScript(() => { try { localStorage.setItem('ui:locale', 'es') } catch {} });
const page = await ctx.newPage();
await page.request.post(`${base}api/auth/login`, { data: { username: cred.username, password: cred.password } });
await page.goto(`${base}identificar/`, { waitUntil: 'networkidle' });
if (await page.getByRole('button', { name: 'Descargar' }).count()) {
  await page.getByRole('button', { name: 'Descargar' }).click();
}
await page.getByRole('button', { name: /Tomar foto|Otra foto/ }).waitFor({ timeout: 300_000 });
await page.locator('input[type=file]:not([capture])').setInputFiles(photo);
await page.getByText(/Posible|No estamos seguros/).first().waitFor({ timeout: 180_000 });
await page.waitForTimeout(4000);
const teamImgs = async () => page.locator('img[src*="/api/photo/"], img[src*="/api/identify/ref-photo"]').count();
console.log('signed in: team/proxy images', await teamImgs(), 'panel', await page.getByText('Congelados para genomas de referencia').count());
console.log('credit links', await page.locator('figcaption a[href*="inaturalist.org/photos"]').count(), await page.locator('figcaption a[href*="creativecommons.org"]').count());
await page.getByRole('button', { name: /Lab/ }).first().click();
page.once('dialog', d => d.accept());
await page.getByRole('button', { name: 'Cerrar sesión' }).click();
await page.waitForLoadState('load');
await page.waitForTimeout(5000);
console.log('after sign-out: url', page.url(), 'header', (await page.locator('header').innerText()).replace(/\s+/g, ' ').slice(0, 80));
console.log('after sign-out: team/proxy images', await teamImgs(), 'panel', await page.getByText('Congelados para genomas de referencia').count(), 'result', (await page.locator('h1').first().innerText()).replace(/\s+/g, ' '));
console.log('field credit links', await page.locator('figcaption a[href*="inaturalist.org/photos"]').count(), await page.locator('figcaption a[href*="creativecommons.org"]').count());
const stockKept = await page.evaluate(async () => !!(await (await caches.open('ithomiini-prueba-offline')).match(new URL('../prueba-offline/models/frozen-stock.json', location.href).href)));
console.log('inventory kept on the device:', stockKept);
await page.screenshot({ path: `${out}/g-after-sign-out-desktop.png`, fullPage: true });
await browser.close();
