// Does «Guardar fotos» work today? Phone width. Signed out (or signed in with a credentials file, on-phone mode):
// model download, field set saved (by itself or by the button), then offline: reload, identify, Compare photos shown.
// node today.mjs <base/> <out> <photo> [credentials.json]
import { readFileSync, mkdirSync, writeFileSync } from 'node:fs';
const { chromium } = await import('/home/franz/.local/share/agent-tools/node_modules/playwright/index.mjs');
const [base, out, photo, credFile] = process.argv.slice(2);
mkdirSync(out, { recursive: true });
const browser = await chromium.launch();
const ctx = await browser.newContext({ locale: 'es-EC', viewport: { width: 390, height: 844 }, deviceScaleFactor: 2, isMobile: true, hasTouch: true });
await ctx.addInitScript(() => { try { if (!localStorage.getItem('ui:locale')) localStorage.setItem('ui:locale', 'es') } catch {} });
const page = await ctx.newPage();
const reqs = [];
page.on('request', r => reqs.push(`${r.method()} ${r.url()}`));
const log = [];
const t0 = Date.now();
const step = s => { const l = `${((Date.now() - t0) / 1000).toFixed(0)}s ${s}`; console.log(l); log.push(l); };
const shot = name => page.screenshot({ path: `${out}/${name}.png`, fullPage: true });
await page.goto(`${base}identificar/`, { waitUntil: 'networkidle' });
if (credFile) {
  const cred = JSON.parse(readFileSync(credFile, 'utf8'));
  const login = await page.request.post(`${base}api/auth/login`, { data: { username: cred.username, password: cred.password } });
  step(`login ${login.status()}`);
  await page.evaluate(() => localStorage.setItem('identify:mode', 'phone'));
  await page.reload({ waitUntil: 'networkidle' });
}
await page.getByRole('button', { name: 'Descargar' }).waitFor({ timeout: 60_000 });
await shot('a-setup');
await page.getByRole('button', { name: 'Descargar' }).click();
await page.getByRole('button', { name: /Tomar foto/ }).waitFor({ timeout: 600_000 });
step('model downloaded');
await page.getByText('Modelo y almacenamiento').click();
const saved = page.getByText(/fotos guardadas/);
const auto = await saved.waitFor({ timeout: 120_000 }).then(() => true, () => false);
if (!auto) {
  step('not saved by itself; tapping Guardar fotos');
  await page.getByRole('button', { name: /Guardar fotos/ }).click();
  await saved.waitFor({ timeout: 300_000 });
}
step(`field (${auto ? 'by itself' : 'button'}): ${await saved.innerText()}`);
const fieldReq = reqs.filter(r => /identificar\/field\/\d+\.jpg/.test(r)).length;
step(`field photo requests: ${fieldReq}`);
const cached = await page.evaluate(async () => (await (await caches.open('ithomiini-prueba-offline')).keys()).filter(r => /identificar\/field\/\d+\.jpg/.test(r.url)).length);
step(`field photos in Cache Storage: ${cached}`);
await page.getByText('Modelo y almacenamiento').scrollIntoViewIfNeeded();
await shot('b-field-saved');
// Offline: reload, identify, and the Compare photos.
await ctx.setOffline(true);
await page.reload({ waitUntil: 'load' });
await page.getByRole('button', { name: /Tomar foto|Otra foto/ }).waitFor({ timeout: 60_000 });
await page.locator('input[type=file]:not([capture])').setInputFiles(photo);
await page.getByText(/Posible|No estamos seguros/).first().waitFor({ timeout: 180_000 });
await page.waitForTimeout(2500);
const imgs = async () => page.evaluate(() => [...document.images].filter(i => /identificar\/field\//.test(i.src)).map(i => ({ src: i.src.split('/').pop(), ok: i.complete && i.naturalWidth > 0 })));
step(`offline result: ${(await page.locator('h1').first().innerText()).replace(/\s+/g, ' ')}`);
const compareText = await page.locator('section', { hasText: 'Compare' }).first().innerText().catch(() => '');
step(`compare caption: ${compareText.replace(/\s+/g, ' ').slice(0, 220)}`);
// Open every genus group so the candidates' thumbnails load.
for (const b of await page.locator('button[aria-expanded=false]').all()) await b.click().catch(() => {});
await page.waitForTimeout(1500);
const all = await imgs();
step(`offline field images on screen: ${all.length}, loaded ${all.filter(i => i.ok).length}: ${all.map(i => `${i.src}${i.ok ? '' : '(broken)'}`).join(' ')}`);
await shot('c-offline-compare');
await ctx.setOffline(false);
writeFileSync(`${out}/log.txt`, log.join('\n') + '\n');
writeFileSync(`${out}/requests.json`, JSON.stringify({ api: [...new Set(reqs.filter(r => /\/api\//.test(r)).map(r => r.replace(/https?:\/\/[^/]+/, '')))], hosts: [...new Set(reqs.map(r => new URL(r.split(' ')[1]).host))] }, null, 1));
await browser.close();
