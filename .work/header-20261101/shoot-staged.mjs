// #/emergidos with one staged (unsaved) change shown in the header: the count is put in the live store, nothing is sent or saved.
//   node shoot-staged.mjs --out <dir> [--base http://127.0.0.1:8871] [--widths 640,1024] [--themes light,dark]
import { readFileSync, writeFileSync, mkdirSync } from 'node:fs'
const { chromium } = await import('/home/franz/.local/share/agent-tools/node_modules/playwright/index.mjs')
const arg = (k, d) => { const i = process.argv.indexOf('--' + k); return i > 0 ? process.argv[i + 1] : d }
const base = arg('base', 'http://127.0.0.1:8871'); const out = arg('out')
const widths = arg('widths', '640,1024').split(',').map(Number); const themes = arg('themes', 'light,dark').split(',')
const cred = JSON.parse(readFileSync('/home/franz/.local/share/ui-overhaul-20261031/header-app/credentials.json', 'utf8'))
mkdirSync(out, { recursive: true })
const browser = await chromium.launch(); const res = []
for (const theme of themes) for (const w of widths) {
  const ctx = await browser.newContext({ viewport: { width: w, height: 700 }, colorScheme: theme })
  await ctx.addInitScript(([t]) => { try { localStorage.setItem('ui:locale', 'es'); localStorage.setItem('ui:theme', t) } catch {} }, [theme])
  await ctx.route('**/*', r => { const m = r.request().method(); if (m === 'GET' || m === 'HEAD' || /\/api\/auth\/login$/.test(r.request().url())) return r.continue(); return r.abort() })
  const page = await ctx.newPage()
  await page.goto(base + '/#/entrar'); await page.waitForSelector('input[name="username"]')
  await page.fill('input[name="username"]', cred.username); await page.fill('input[name="password"]', cred.password)
  await page.click('form button.btn-primary'); await page.waitForFunction(() => !location.hash.includes('entrar'), null, { timeout: 30000 }).catch(() => {})
  await page.goto(`${base}/#/emergidos`); await page.waitForTimeout(2000)
  const now = new Date().toISOString()
  await page.evaluate(item => {
    const pinia = document.querySelector('#app').__vue_app__.config.globalProperties.$pinia
    pinia.state.value.live.items = [item]
  }, { id: 'x1', entryId: 'e1', purpose: 'emergidos', kind: 'edit', sheet: 'Emergidos', recordId: 'R1', clientId: null, rowId: 'R1', label: null, values: {}, actor: 'u', actorName: 'Lab', createdAt: now, updatedAt: now, status: 'staged' })
  await page.waitForTimeout(600)
  const m = await page.evaluate(() => {
    const h = document.querySelector('header'); const hr = h.getBoundingClientRect(); const p = []
    if (document.documentElement.scrollWidth > document.documentElement.clientWidth) p.push('page overflow')
    const nav = h.querySelector('nav'); const nr = nav.getBoundingClientRect()
    for (const a of nav.querySelectorAll('a,button')) { const r = a.getBoundingClientRect(); if (r.right > nr.right + 0.5 || r.right > hr.right + 0.5) p.push('tab past row: ' + a.textContent.trim()) }
    const right = h.querySelector('.ml-auto')?.getBoundingClientRect(); const more = h.querySelector('[data-more-trigger]')?.getBoundingClientRect()
    if (right && more && more.right > right.left + 0.5 && Math.abs(more.top - right.top) < 30 && w >= 1024) p.push('Más overlaps right buttons')
    return { problems: p, tabs: [...nav.querySelectorAll('a,button')].map(a => a.textContent.trim().replace(/\s+/g, ' ')), h: Math.round(hr.height) }
  })
  const hh = m.h
  await page.screenshot({ path: `${out}/emergidos-staged__${w}-es-${theme}.png`, clip: { x: 0, y: 0, width: w, height: hh + 8 } })
  res.push({ w, theme, ...m }); console.log(w, theme, JSON.stringify(m))
  await ctx.close()
}
await browser.close(); writeFileSync(`${out}/staged-checks.json`, JSON.stringify(res, null, 1))
