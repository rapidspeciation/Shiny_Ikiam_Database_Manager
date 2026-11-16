// Headless check of Identificar's answers on the lab copy: real server identifications of a few photos, then
// restored answers (the page's session store) for each answer kind, at 390 px and 1440 px. Usage:
//   node shots.mjs <base url> <credentials.json> <photos dir> <out dir>
import { readFileSync, writeFileSync, mkdirSync } from 'node:fs'
import { chromium } from '/home/franz/.local/share/agent-tools/node_modules/playwright/index.mjs'

const [base, credFile, photos, out] = process.argv.slice(2)
mkdirSync(out, { recursive: true })
const cred = JSON.parse(readFileSync(credFile, 'utf8'))
const log = []
const crop = 'data:image/jpeg;base64,' + readFileSync(`${photos}/mpolymnia.jpg`).toString('base64')

const server = (o) => ({ subspecies: [], recorded: null, crop: 'success', wingBox: null, prior: null, ms: 900, ...o })
const restored = (o) => ({ at: '2026-11-15T21:00:00.000Z', source: 'online', crop, status: 'success', ms: 900, server: server(o), photo: 'AA==', side: null })
const answers = {
  species: { species: [['Mechanitis polymnia', 0.91], ['Mechanitis lysimnia', 0.05], ['Melinaea menophilus', 0.02]], genus: [['Mechanitis', 0.96], ['Melinaea', 0.02]], tribe: [['Ithomiini', 0.99]], subfamily: [['Danainae', 0.99]] },
  genus: { species: [['Mechanitis polymnia', 0.55], ['Mechanitis lysimnia', 0.22], ['Mechanitis mazaeus', 0.08], ['Melinaea menophilus', 0.06]], genus: [['Mechanitis', 0.86], ['Melinaea', 0.07]], tribe: [['Ithomiini', 0.97]], subfamily: [['Danainae', 0.98]] },
  tribe: { species: [['Mechanitis polymnia', 0.31], ['Melinaea menophilus', 0.24], ['Hypothyris anastasia', 0.12], ['Heliconius erato', 0.07]], genus: [['Mechanitis', 0.38], ['Melinaea', 0.3], ['Hypothyris', 0.14]], tribe: [['Ithomiini', 0.88], ['Heliconiini', 0.08]], subfamily: [['Danainae', 0.9]] },
  tribeOutside: { species: [['Heliconius erato', 0.12], ['Mechanitis polymnia', 0.1], ['Oleria onega', 0.08], ['Melinaea menophilus', 0.07]], genus: [['Mechanitis', 0.3], ['Oleria', 0.25], ['Heliconius', 0.12]], tribe: [['Ithomiini', 0.86], ['Heliconiini', 0.12]], subfamily: [['Danainae', 0.87]] },
  unsure: { species: [['Mechanitis polymnia', 0.34], ['Heliconius erato', 0.28], ['Melinaea menophilus', 0.15]], genus: [['Mechanitis', 0.4], ['Heliconius', 0.3], ['Melinaea', 0.17]], tribe: [['Ithomiini', 0.62], ['Heliconiini', 0.3]], subfamily: [['Danainae', 0.63], ['Heliconiinae', 0.3]] },
  notIthomiini: { species: [['Heliconius erato', 0.93], ['Heliconius melpomene', 0.04]], genus: [['Heliconius', 0.98]], tribe: [['Heliconiini', 0.99]], subfamily: [['Heliconiinae', 0.99]] },
}

const browser = await chromium.launch()
const sizes = { phone: { viewport: { width: 390, height: 844 }, deviceScaleFactor: 2, isMobile: true, hasTouch: true }, desktop: { viewport: { width: 1440, height: 900 } } }

async function signedIn(size) {
  const ctx = await browser.newContext({ ...sizes[size], locale: 'es-EC' })
  const r = await ctx.request.post(`${base}/api/auth/login`, { data: { username: cred.username, password: cred.password }, headers: { origin: base } })
  if (!r.ok()) throw new Error(`login ${r.status()}`)
  return ctx
}
const read = (page) =>
  page.evaluate(() => {
    const q = (id) => {
      const el = document.querySelector(`[data-testid=${id}]`)?.cloneNode(true)
      el?.querySelectorAll('svg').forEach((s) => s.remove())
      return el?.textContent?.replace(/\s+/g, ' ').trim() ?? null
    }
    const h = document.querySelector('[data-testid=headline]')
    return {
      kicker: h?.previousElementSibling?.textContent?.replace(/\s+/g, ' ').trim() ?? null,
      headline: q('headline'),
      p: q('headline-p'),
      label: q('ithomiini-badge') ?? q('ranks-above'),
      notIthomiini: q('not-ithomiini'),
      speciesUnsure: q('species-unsure'),
      rungs: [...document.querySelectorAll('[data-testid=rungs] li')].map(li => [...li.children].map(c => c.textContent.trim()).filter(Boolean).join(' ') + (li.getAttribute('data-sure') === 'true' ? ' ✓' : '')),
      ssp: q('ssp-hint'),
      top: h?.getAttribute('data-top'),
      topP: h?.getAttribute('data-top-p'),
      testingLine: !!document.querySelector('[data-testid=tribe-rung-on]') || /para pruebas/.test(document.body.textContent ?? ''),
      overflowX: document.documentElement.scrollWidth > document.documentElement.clientWidth,
    }
  })

for (const size of ['phone', 'desktop']) {
  const ctx = await signedIn(size)
  const page = await ctx.newPage()
  // 1. Real identifications on the lab's server identifier (signed in: «En línea»).
  if (size === 'desktop') {
    for (const name of ['mpolymnia', 'herato', 'oonega', 'mmenophilus', 'hanastasia', 'dismorphia']) {
      await page.goto(`${base}/identificar/`, { waitUntil: 'networkidle' })
      await page.evaluate(() => sessionStorage.clear())
      await page.goto(`${base}/identificar/`, { waitUntil: 'networkidle' })
      await page.setInputFiles('input[type=file][multiple]', `${photos}/${name}.jpg`)
      try {
        await page.waitForSelector('[data-testid=headline]', { timeout: 120000 })
        await page.waitForTimeout(1500)
        const r = await read(page)
        log.push({ size, case: `real:${name}`, ...r })
        await page.screenshot({ path: `${out}/${size}_real_${name}.png`, fullPage: false })
      } catch (e) {
        log.push({ size, case: `real:${name}`, error: String(e).slice(0, 200) })
        await page.screenshot({ path: `${out}/${size}_real_${name}_error.png` })
      }
    }
  }
  // 1b. The phone's own model (desktop context): «En este teléfono», the first download, then two photos.
  if (size === 'desktop') {
    try {
      await page.goto(`${base}/identificar/`, { waitUntil: 'networkidle' })
      await page.evaluate(() => sessionStorage.clear())
      await page.goto(`${base}/identificar/`, { waitUntil: 'networkidle' })
      await page.getByRole('radio', { name: /En este teléfono|On this phone/ }).click()
      await page.click('[data-testid=first-get]', { timeout: 15000 })
      await page.waitForTimeout(4000)
      const refused = await page.evaluate(() => [...document.querySelectorAll('[data-testid=first-download] p')].map(p => p.textContent.trim()).find(t => /not available|no está disponible/.test(t)) ?? null)
      if (refused) throw new Error(`phone model: ${refused}`)
      await page.waitForSelector('[data-testid=first-download]', { state: 'detached', timeout: 400000 })
      for (const name of ['mpolymnia2', 'herato', 'hanastasia']) {
        const prev = await page.evaluate(() => document.querySelector('[data-testid=headline]')?.getAttribute('data-top-p') ?? null)
        await page.setInputFiles('input[type=file][multiple]', `${photos}/${name}.jpg`)
        await page.waitForFunction((prev) => {
          const h = document.querySelector('[data-testid=headline]')
          return !!h && h.getAttribute('data-top-p') !== prev
        }, prev, { timeout: 180000 })
        await page.waitForTimeout(2500)
        const r = await read(page)
        const where = await page.evaluate(() => [...document.querySelectorAll('p')].map(p => p.textContent.trim()).find(t => /Analizada|Analysed|Analyzed/.test(t)) ?? null)
        log.push({ size, case: `phone-model:${name}`, where, ...r })
        await page.screenshot({ path: `${out}/${size}_phonemodel_${name}.png` })
      }
    } catch (e) {
      log.push({ size, case: 'phone-model', error: String(e).slice(0, 300) })
      await page.screenshot({ path: `${out}/${size}_phonemodel_error.png` })
    }
    // Back to «En línea» for the restored server answers.
    await page.getByRole('radio', { name: /En línea|Online/ }).click()
    await page.waitForTimeout(500)
  }
  // 2. Each answer kind, restored from the session store.
  for (const [kind, a] of Object.entries(answers)) {
    for (const lang of ['es', 'en']) {
      if (lang === 'en' && size === 'desktop' && kind !== 'genus' && kind !== 'tribe') continue
      if (lang === 'en' && size === 'phone' && kind !== 'genus') continue
      await page.goto(`${base}/identificar/`, { waitUntil: 'networkidle' })
      await page.evaluate(([v, l]) => {
        sessionStorage.setItem('identify:last', JSON.stringify(v))
        localStorage.setItem('ui:locale', l)
      }, [restored(a), lang])
      await page.reload({ waitUntil: 'networkidle' })
      await page.waitForSelector('[data-testid=headline]', { timeout: 30000 })
      await page.waitForTimeout(800)
      const r = await read(page)
      log.push({ size, case: `restored:${kind}:${lang}`, ...r })
      await page.screenshot({ path: `${out}/${size}_${kind}_${lang}.png`, fullPage: false })
    }
  }
  // Dark mode, the tribe answer.
  await page.emulateMedia({ colorScheme: 'dark' })
  await page.evaluate(([v]) => { sessionStorage.setItem('identify:last', JSON.stringify(v)); localStorage.setItem('ui:locale', 'es') }, [restored(answers.tribe)])
  await page.reload({ waitUntil: 'networkidle' })
  await page.waitForSelector('[data-testid=headline]')
  await page.waitForTimeout(800)
  await page.screenshot({ path: `${out}/${size}_tribe_es_dark.png` })
  await ctx.close()
}
await browser.close()
writeFileSync(`${out}/checks.json`, JSON.stringify(log, null, 1))
console.log(JSON.stringify(log, null, 1))
