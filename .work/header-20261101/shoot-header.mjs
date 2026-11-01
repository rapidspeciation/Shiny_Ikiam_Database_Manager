// Header shots and checks: #/inicio, #/muertes, #/colecta (+ the «Más» menu open where there is one), per width × language × theme.
//   node shoot-header.mjs --out <dir> [--base http://127.0.0.1:8871] [--widths 390,640,768,1024,1280,1440] [--themes light,dark] [--langs es,en]
// Writes <out>/<view>__<width>-<lang>-<theme>.png (header strip) and <out>/checks.json; prints the problems found.
import { readFileSync, writeFileSync, mkdirSync } from 'node:fs'
const { chromium } = await import('/home/franz/.local/share/agent-tools/node_modules/playwright/index.mjs')
const arg = (k, d) => { const i = process.argv.indexOf('--' + k); return i > 0 ? process.argv[i + 1] : d }
const base = arg('base', 'http://127.0.0.1:8871')
const out = arg('out'); if (!out) throw new Error('--out <dir>')
const widths = arg('widths', '390,640,768,1024,1280,1440').split(',').map(Number)
const themes = arg('themes', 'light,dark').split(',')
const langs = arg('langs', 'es,en').split(',')
const views = ['inicio', 'muertes', 'colecta']
const cred = JSON.parse(readFileSync(arg('cred', '/home/franz/.local/share/ui-overhaul-20261031/header-app/credentials.json'), 'utf8'))
mkdirSync(out, { recursive: true })
const results = []
const browser = await chromium.launch()
// Measured in the page: overflow and scrollbars inside the header, tabs cut off by their row.
const measure = () => {
  const h = document.querySelector('header'); const p = []
  if (!h) return { problems: ['no header'] }
  if (document.documentElement.scrollWidth > document.documentElement.clientWidth) p.push(`page overflow ${document.documentElement.scrollWidth - document.documentElement.clientWidth}px`)
  for (const el of [h, ...h.querySelectorAll('*')]) {
    if (el.closest('[data-more-menu],[data-user-menu],[aria-hidden="true"]')) continue
    const s = getComputedStyle(el)
    if (el.scrollWidth > el.clientWidth + 1 && el.clientWidth > 0 && (s.overflowX === 'auto' || s.overflowX === 'scroll' || s.overflowX === 'hidden'))
      p.push(`overflow ${el.tagName}.${String(el.className).slice(0, 40)} ${el.scrollWidth}>${el.clientWidth}`)
  }
  const hr = h.getBoundingClientRect()
  const tabs = [...h.querySelectorAll('nav a, nav button')].filter(a => a.offsetParent && !a.closest('[data-more-menu]'))
  for (const a of tabs) {
    const r = a.getBoundingClientRect(); let c = a.parentElement; let cut = false
    while (c && c !== h.parentElement) { const cr = c.getBoundingClientRect(); const s = getComputedStyle(c); if (s.overflowX !== 'visible' && (r.left < cr.left - 0.5 || r.right > cr.right + 0.5)) cut = true; c = c.parentElement }
    if (cut || r.right > hr.right + 0.5 || r.left < hr.left - 0.5 || a.scrollWidth > a.clientWidth + 1) p.push(`tab cut: ${a.textContent.trim()}`)
  }
  return { problems: p, tabs: tabs.map(a => a.textContent.trim().replace(/\s+/g, ' ')), headerHeight: Math.round(hr.height) }
}
for (const theme of themes) for (const lang of langs) for (const w of widths) {
  const phone = w < 800
  const ctx = await browser.newContext({ viewport: { width: w, height: 700 }, colorScheme: theme, ...(phone ? { isMobile: true, hasTouch: true, deviceScaleFactor: 2 } : {}) })
  await ctx.addInitScript(([l, t]) => { try { localStorage.setItem('ui:locale', l); localStorage.setItem('ui:theme', t) } catch {} }, [lang, theme])
  await ctx.route('**/*', r => { const m = r.request().method(); if (m === 'GET' || m === 'HEAD' || /\/api\/auth\/login$/.test(r.request().url())) return r.continue(); return r.abort() })
  const page = await ctx.newPage()
  await page.goto(base + '/#/entrar'); await page.waitForSelector('input[name="username"]')
  await page.fill('input[name="username"]', cred.username); await page.fill('input[name="password"]', cred.password)
  await page.click('form button.btn-primary'); await page.waitForFunction(() => !location.hash.includes('entrar'), null, { timeout: 30000 }).catch(() => {})
  for (const v of views) {
    await page.goto(`${base}/#/${v}`); await page.waitForTimeout(1500)
    const m = await page.evaluate(measure)
    const name = `${v}__${w}-${lang}-${theme}`
    const hh = await page.evaluate(() => Math.ceil(document.querySelector('header')?.getBoundingClientRect().bottom ?? 120))
    await page.screenshot({ path: `${out}/${name}.png`, clip: { x: 0, y: 0, width: w, height: hh + 8 } })
    const rec = { view: v, width: w, lang, theme, ...m }
    const more = page.locator('[data-more-trigger]')
    if (v === 'muertes' && await more.count() && await more.isVisible()) {
      await more.click(); await page.waitForTimeout(300)
      const box = await page.evaluate(() => { const r = document.querySelector('[data-more-menu]')?.getBoundingClientRect(); return r ? Math.ceil(r.bottom) : 0 })
      await page.screenshot({ path: `${out}/${v}-more-open__${w}-${lang}-${theme}.png`, clip: { x: 0, y: 0, width: w, height: Math.max(box, hh) + 8 } })
      rec.moreItems = await page.evaluate(() => [...document.querySelectorAll('[data-more-menu] a')].map(a => a.textContent.trim()))
      await page.keyboard.press('Escape'); await page.waitForTimeout(150)
      rec.escClosed = await page.evaluate(() => !document.querySelector('[data-more-menu]') && document.activeElement?.hasAttribute('data-more-trigger'))
    }
    results.push(rec)
    if (m.problems.length) console.log(name, m.problems.join(' | '))
  }
  await ctx.close()
}
await browser.close()
writeFileSync(`${out}/checks.json`, JSON.stringify(results, null, 1))
console.log(`${results.length} views, ${results.filter(r => r.problems.length).length} with problems`)
