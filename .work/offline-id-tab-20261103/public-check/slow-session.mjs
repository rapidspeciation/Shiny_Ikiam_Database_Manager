// Signed in, the session answers after 10 s (beyond the page's 8 s wait): no reload loop, the team view arrives.
import { readFileSync } from 'node:fs';
const { chromium } = await import('/home/franz/.local/share/agent-tools/node_modules/playwright/index.mjs');
const [base, photo, credFile] = process.argv.slice(2);
const cred = JSON.parse(readFileSync(credFile, 'utf8'));
const b = await chromium.launch();
const ctx = await b.newContext({ locale: 'es-EC', viewport: { width: 1366, height: 900 } });
await ctx.addInitScript(() => { try { localStorage.setItem('ui:locale', 'es') } catch {} });
const page = await ctx.newPage();
await page.request.post(`${base}api/auth/login`, { data: { username: cred.username, password: cred.password } });
let loads = 0;
page.on('load', () => loads++);
await ctx.route('**/api/auth/session', async r => { await new Promise(x => setTimeout(x, 10_000)); await r.continue(); });
await page.goto(`${base}identificar/`);
await page.getByRole('button', { name: /Tomar foto|Otra foto|Descargar/ }).first().waitFor({ timeout: 60_000 });
await page.waitForTimeout(14_000);
console.log('loads', loads, 'header', (await page.locator('header').innerText()).replace(/\s+/g, ' ').slice(0, 70));
if (await page.getByRole('button', { name: 'Descargar' }).count()) {
  await page.getByRole('button', { name: 'Descargar' }).click();
  await page.getByRole('button', { name: /Tomar foto/ }).waitFor({ timeout: 300_000 });
}
await page.locator('input[type=file]:not([capture])').setInputFiles(photo);
await page.getByText(/Posible|No estamos seguros/).first().waitFor({ timeout: 120_000 });
await page.waitForTimeout(3000);
console.log('loads', loads, 'panel', await page.getByText('Congelados para genomas de referencia').count());
await b.close();
