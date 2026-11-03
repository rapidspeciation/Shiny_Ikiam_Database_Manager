// Signed-out visitor on the Identificar page: Hugging Face download, identify, paste, drop, offline reload.
// node signed-out.mjs <base url ending in /> <out dir> <photo> [phone|desktop] [browser]
import { readFileSync, mkdirSync, writeFileSync } from 'node:fs';
const { chromium, webkit } = await import('/home/franz/.local/share/agent-tools/node_modules/playwright/index.mjs');
const [base, out, photo, form = 'phone', engine = 'chromium'] = process.argv.slice(2);
mkdirSync(out, { recursive: true });
const browser = await (engine === 'webkit' ? webkit : chromium).launch();
const ctx = await browser.newContext(form === 'phone' ? { locale: 'es-EC', viewport: { width: 390, height: 844 }, deviceScaleFactor: 2, hasTouch: true, isMobile: engine !== 'webkit' } : { locale: 'es-EC', viewport: { width: 1366, height: 900 } });
const lang = process.env.LANG_UI || 'es';
await ctx.addInitScript(l => { try { localStorage.setItem('ui:locale', l) } catch {} }, lang);
const page = await ctx.newPage();
const log = { requests: [], console: [] };
page.on('request', r => log.requests.push(`${r.method()} ${r.url().replace(/\?.*$/, m => (m.length > 60 ? m.slice(0, 60) + '…' : m))}`));
page.on('console', m => (m.type() === 'error' || m.type() === 'warning') && log.console.push(m.text()));
const t0 = Date.now();
const step = (s) => console.log(`${((Date.now() - t0) / 1000).toFixed(1)}s ${s}`);
await page.goto(`${base}identificar/`, { waitUntil: 'networkidle' });
step(`isolated=${await page.evaluate(() => crossOriginIsolated)}`);
await page.getByRole('button', { name: 'Descargar' }).waitFor();
await page.screenshot({ path: `${out}/a-setup-${form}.png`, fullPage: true });
const tabs = await page.locator('header').innerText();
step(`header: ${tabs.replace(/\s+/g, ' ').slice(0, 200)}`);
await page.getByRole('button', { name: 'Descargar' }).click();
await page.getByRole('button', { name: /Tomar foto/ }).waitFor({ timeout: 300_000 });
step('downloaded');
await page.screenshot({ path: `${out}/b-ready-${form}.png`, fullPage: true });
await page.locator('input[type=file]:not([capture])').setInputFiles(photo);
await page.getByText(/Posible|No estamos seguros/).first().waitFor({ timeout: 180_000 });
await page.waitForTimeout(1500);
step(`result: ${(await page.locator('h1').first().innerText()).replace(/\s+/g, ' ')}`);
await page.screenshot({ path: `${out}/c-result-${form}.png`, fullPage: true });
// Paste: a synthetic ClipboardEvent with the image as a file item.
const b64 = readFileSync(photo).toString('base64');
const pasted = async (kind) => {
  await page.evaluate(async ({ b64, kind }) => {
    const bytes = Uint8Array.from(atob(b64), c => c.charCodeAt(0));
    const dt = new DataTransfer();
    dt.items.add(new File([bytes], 'pegada.jpg', { type: 'image/jpeg' }));
    if (kind === 'paste') document.dispatchEvent(new ClipboardEvent('paste', { clipboardData: dt, bubbles: true, cancelable: true }));
    else {
      document.dispatchEvent(new DragEvent('dragover', { dataTransfer: dt, bubbles: true, cancelable: true }));
      document.dispatchEvent(new DragEvent('drop', { dataTransfer: dt, bubbles: true, cancelable: true }));
    }
  }, { b64, kind });
};
const lastAt = () => page.evaluate(() => JSON.parse(sessionStorage.getItem('identify:last') || 'null')?.at);
for (const kind of ['paste', 'drop']) {
  const before = await lastAt();
  await pasted(kind);
  await page.waitForFunction(b => JSON.parse(sessionStorage.getItem('identify:last') || 'null')?.at !== b, before, { timeout: 60_000 }).then(() => step(`${kind}: new result`), () => step(`${kind}: NO new result`));
}
const ithomiini = await page.locator('text=Ithomiini').count();
step(`badge Ithomiini count=${ithomiini}; stock panel=${await page.getByText('Congelados').count()}`);
// Field photos (static set) and the About line.
await page.getByText('Modelo y almacenamiento').click();
await page.getByRole('button', { name: /Guardar fotos|Actualizar fotos/ }).click();
await page.getByText(/fotos guardadas/).waitFor({ timeout: 180_000 });
step(`field: ${await page.getByText(/fotos guardadas/).innerText()}`);
await page.getByText('Acerca de y licencias').last().click();
await page.screenshot({ path: `${out}/d-about-${form}.png`, fullPage: true });
// Offline: reload, identify again.
await ctx.setOffline(true);
await page.reload({ waitUntil: 'load' });
await page.getByRole('button', { name: /Tomar foto|Otra foto/ }).waitFor({ timeout: 60_000 });
await page.locator('input[type=file]:not([capture])').setInputFiles(photo);
await page.getByText(/Posible|No estamos seguros/).first().waitFor({ timeout: 180_000 });
await page.waitForTimeout(1000);
step(`offline result: ${(await page.locator('h1').first().innerText()).replace(/\s+/g, ' ')}; status: ${(await page.locator('main').innerText()).match(/Listo para usar sin internet|Ahora sin señal/g)}`);
await page.screenshot({ path: `${out}/e-offline-${form}.png`, fullPage: true });
await ctx.setOffline(false);
const api = log.requests.filter(r => /\/api\//.test(r));
const hosts = [...new Set(log.requests.map(r => new URL(r.split(' ')[1]).host))];
writeFileSync(`${out}/requests-${form}-${engine}.json`, JSON.stringify({ hosts, api, console: log.console, requests: log.requests }, null, 1));
step(`hosts: ${hosts.join(', ')}`);
step(`api calls: ${api.join(' | ')}`);
step(`console errors: ${log.console.slice(0, 5).join(' | ')}`);
await browser.close();
