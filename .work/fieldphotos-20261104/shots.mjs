// The field photos choice at phone width, signed out: download panel, saved by itself with the default, another
// choice (fewer photos, the rest removed), offline Compare with subspecies photos. node shots.mjs <base/> <out> <photo>
import { mkdirSync, writeFileSync } from 'node:fs';
const { chromium } = await import('/home/franz/.local/share/agent-tools/node_modules/playwright/index.mjs');
const [base, out, photo] = process.argv.slice(2);
mkdirSync(out, { recursive: true });
const browser = await chromium.launch();
const ctx = await browser.newContext({ locale: 'es-EC', viewport: { width: 390, height: 844 }, deviceScaleFactor: 2, isMobile: true, hasTouch: true });
await ctx.addInitScript(() => { try { if (!localStorage.getItem('ui:locale')) localStorage.setItem('ui:locale', 'es') } catch {} });
const page = await ctx.newPage();
const reqs = [];
page.on('request', r => reqs.push(`${r.method()} ${r.url()}`));
const errors = [];
page.on('console', m => m.type() === 'error' && errors.push(m.text()));
const log = [];
const t0 = Date.now();
const step = s => { const l = `${((Date.now() - t0) / 1000).toFixed(0)}s ${s}`; console.log(l); log.push(l); };
const shot = (name, el) => (el ? el.screenshot({ path: `${out}/${name}.png` }) : page.screenshot({ path: `${out}/${name}.png`, fullPage: true }));
const fieldFiles = () => page.evaluate(async () => (await (await caches.open('ithomiini-prueba-offline')).keys()).filter(r => /identificar\/field\/[^/]+\.(jpe?g|png)$/.test(r.url)).length);
const text = async loc => ((await loc.innerText().catch(() => '')) || '').replace(/\s+/g, ' ').trim();

await page.goto(`${base}identificar/`, { waitUntil: 'networkidle' });
await page.getByRole('button', { name: 'Descargar' }).waitFor({ timeout: 60_000 });
const box = page.getByTestId('field-in-download');
await box.getByText(/unos .* MB/).waitFor({ timeout: 20_000 }).catch(() => {});
step(`download panel: ${await text(box)}`);
await shot('a-download-panel');
await box.getByText('Elegir cuáles').click();
await page.waitForTimeout(300);
step(`choice in panel: ${await text(box)}`);
await shot('b-download-panel-choice', box);
await page.getByRole('button', { name: 'Descargar' }).click();
await page.getByRole('button', { name: /Tomar foto/ }).waitFor({ timeout: 600_000 });
step('model downloaded');
await page.getByText('Modelo y almacenamiento').click();
const saved = page.getByText(/fotos guardadas/);
await saved.waitFor({ timeout: 300_000 });
step(`saved by itself: ${await saved.innerText()}; files in cache ${await fieldFiles()}`);
const details = page.locator('details', { hasText: 'Modelo y almacenamiento' });
await details.scrollIntoViewIfNeeded();
await shot('c-box-default', details);
// Another choice: Ecuador's species, no subspecies.
await details.getByRole('radio', { name: /Solo Ecuador/ }).first().check();
await details.getByRole('radio', { name: /Ninguna/ }).check();
step(`after choosing EC/none: ${await text(details.getByTestId('field-size'))}; button: ${await text(details.getByRole('button', { name: /Guardar|Actualizar/ }))}`);
await shot('d-box-other-choice', details);
await details.getByRole('button', { name: 'Guardar esta elección' }).click();
await page.getByText(/^195 fotos guardadas/).waitFor({ timeout: 120_000 });
step(`saved EC/none: ${await saved.innerText()}; files in cache ${await fieldFiles()}`);
// Back to the default.
await details.getByRole('radio', { name: /Todas/ }).first().check();
await details.getByRole('radio', { name: /Solo Ecuador/ }).nth(1).check();
await details.getByRole('button', { name: 'Guardar esta elección' }).click();
await page.getByText(/^411 fotos guardadas/).waitFor({ timeout: 120_000 });
step(`saved default again: ${await saved.innerText()}; files in cache ${await fieldFiles()}; button: ${await text(details.getByRole('button', { name: /Guardar|Actualizar/ }))}`);
// Offline: Compare with subspecies photos.
await ctx.setOffline(true);
await page.reload({ waitUntil: 'load' });
await page.getByRole('button', { name: /Tomar foto|Otra foto/ }).waitFor({ timeout: 60_000 });
await page.locator('input[type=file]:not([capture])').setInputFiles(photo);
await page.getByText(/Posible|No estamos seguros/).first().waitFor({ timeout: 180_000 });
await page.waitForTimeout(2500);
step(`offline result: ${await text(page.locator('h1').first())} | ${await text(page.getByTestId('ssp-hint'))}`);
const cmp = page.locator('section', { has: page.getByRole('heading', { name: /Compare/ }) }).first();
step(`compare: ${await text(cmp.locator('figcaption').nth(1))}`);
await shot('e-offline-compare');
const next = cmp.getByRole('button', { name: 'Foto siguiente' });
for (let i = 0; i < 3 && (await next.count()); i++) {
  await next.click();
  await page.waitForTimeout(600);
  const ok = await cmp.locator('img').nth(1).evaluate(i => i.complete && i.naturalWidth > 0).catch(() => false);
  step(`next photo: ${await text(cmp.locator('figcaption').nth(1))} loaded=${ok}`);
  if (i === 0) await shot('f-offline-compare-subspecies', cmp);
}
await ctx.setOffline(false);
await page.evaluate(() => localStorage.setItem('ui:locale', 'en'));
await page.reload({ waitUntil: 'networkidle' });
await page.getByText('Model and storage').click();
const det2 = page.locator('details', { hasText: 'Model and storage' });
await det2.scrollIntoViewIfNeeded();
step(`english box: ${await text(det2)}`);
await shot('g-box-english', det2);
writeFileSync(`${out}/log.txt`, log.join('\n') + `\nconsole errors: ${errors.slice(0, 8).join(' | ')}\n`);
writeFileSync(`${out}/requests.json`, JSON.stringify({ api: [...new Set(reqs.filter(r => /\/api\//.test(r)).map(r => r.replace(/https?:\/\/[^/]+/, '')))], hosts: [...new Set(reqs.map(r => new URL(r.split(' ')[1]).host))] }, null, 1));
await browser.close();
