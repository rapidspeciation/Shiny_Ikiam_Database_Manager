// Menus close on a click outside, Escape (focus back on their button) and a change of page: user menu, «Más», Buscador's ⋯ (phone).
import { readFileSync } from 'node:fs'
const { chromium } = await import('/home/franz/.local/share/agent-tools/node_modules/playwright/index.mjs')
const base = process.argv[2] ?? 'http://127.0.0.1:8871'
const cred = JSON.parse(readFileSync('/home/franz/.local/share/ui-overhaul-20261031/header-app/credentials.json', 'utf8'))
const b = await chromium.launch()
const out = []
for (const [w, phone] of [[1280, false], [390, true]]) {
  const ctx = await b.newContext({ viewport: { width: w, height: 800 }, ...(phone ? { isMobile: true, hasTouch: true } : {}) })
  await ctx.route('**/*', r => { const m = r.request().method(); if (m === 'GET' || m === 'HEAD' || /\/api\/auth\/login$/.test(r.request().url())) return r.continue(); return r.abort() })
  const p = await ctx.newPage()
  await p.goto(base + '/#/entrar'); await p.waitForSelector('input[name="username"]')
  await p.fill('input[name="username"]', cred.username); await p.fill('input[name="password"]', cred.password)
  await p.click('form button.btn-primary'); await p.waitForTimeout(2000)
  const menus = [['user', 'header button[aria-expanded]:not([data-more-trigger])', '[data-user-menu]'], ['more', '[data-more-trigger]', '[data-more-menu]']]
  for (const [name, btn, box] of menus) {
    await p.goto(base + '/#/muertes'); await p.waitForTimeout(1500)
    const r = { width: w, menu: name }
    await p.click(btn); r.opened = await p.locator(box).count() === 1
    await p.mouse.click(w / 2, 600); await p.waitForTimeout(200); r.outsideClosed = await p.locator(box).count() === 0
    await p.focus(btn); await p.keyboard.press('Enter'); await p.waitForTimeout(150)
    await p.keyboard.press('Tab'); await p.keyboard.press('Escape'); await p.waitForTimeout(150)
    r.escClosed = await p.locator(box).count() === 0
    r.focusBack = await p.evaluate(sel => document.activeElement === document.querySelector(sel), btn)
    await p.click(btn); await p.evaluate(() => (location.hash = '#/historial')); await p.waitForTimeout(400)
    r.routeClosed = await p.locator(box).count() === 0
    out.push(r)
  }
  if (!phone) {
    // A click inside an embedded page (the Asistente's T3 frame) never reaches this document.
    await p.goto(base + '/#/muertes'); await p.waitForTimeout(1500)
    await p.evaluate(() => { const f = document.createElement('iframe'); f.srcdoc = '<button style="width:300px;height:200px">x</button>'; f.style.cssText = 'position:fixed;left:200px;top:400px;width:320px;height:220px;z-index:99'; document.body.append(f) })
    await p.waitForTimeout(300)
    const r = { width: w, menu: 'user, click in an iframe' }
    await p.click(menus[0][1]); r.opened = await p.locator('[data-user-menu]').count() === 1
    await p.mouse.click(350, 500); await p.waitForTimeout(300); r.iframeClosed = await p.locator('[data-user-menu]').count() === 0
    out.push(r)
  }
  if (phone) {
    await p.goto(base + '/#/tablas'); await p.waitForTimeout(2500)
    const dots = p.locator('button[aria-expanded]').filter({ hasNot: p.locator('[data-more-trigger]') }).last()
    const r = { width: w, menu: 'buscador ⋯' }
    const sel = '.buscador-tools.is-open'
    await dots.click(); await p.waitForTimeout(150); r.opened = await p.locator(sel).count() === 1
    await p.keyboard.press('Escape'); await p.waitForTimeout(150); r.escClosed = await p.locator(sel).count() === 0
    out.push(r)
  }
  await ctx.close()
}
await b.close()
for (const r of out) console.log(JSON.stringify(r))
