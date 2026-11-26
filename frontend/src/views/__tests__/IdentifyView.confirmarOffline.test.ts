import { readFileSync } from 'node:fs'
import { resolve } from 'node:path'
import { afterEach, describe, expect, it, vi } from 'vitest'
import { createApp, nextTick } from 'vue'
import { createPinia, setActivePinia } from 'pinia'
import { createMemoryHistory, createRouter } from 'vue-router'
import { SEX_FILE } from '../../lib/sexModel'
import type { ConfirmarVisualManifest } from '../../lib/confirmarVisualRouter'

// Identificar's visual guides for a phone that works without signal first, and for a photo with no side of the Andes
// (design audit of 25 Nov 2026, 3.2 and 3.3): the guides' list is asked while the page is idle with signal and again
// when that failed; a photo taken inside the ridge band shows no guide made for the other side alone; a guide shown
// with no side says so and takes the side. A photo goes through the page as «Elegir foto» sends it; the server and
// the phone's models are stand-ins. No AI.

const REAL_MANIFEST = JSON.parse(
  readFileSync(resolve(import.meta.dirname, '../../../public/identificar/confirmar/confirmar_visual_manifest.json'), 'utf8'),
) as ConfirmarVisualManifest

/** What the stand-ins answer: the server's reading (the last one stays for a re-read), the photo's place. */
const world = vi.hoisted(() => ({
  answers: [] as unknown[],
  last: null as unknown,
  sides: [] as (string | null)[],
  gps: null as { lat: number; lon: number } | null,
  country: null as string | null,
  /** The files the page hands the engine to keep for airplane mode. */
  pageFiles: null as null | (() => string[]),
}))

vi.mock('../../lib/offlineEngine', async orig => {
  const real = await orig<typeof import('../../lib/offlineEngine')>()
  const engine = {
    list: async () => ({ files: [{ name: SEX_FILE.name, role: 'sex', bytes: SEX_FILE.bytes, sha256: SEX_FILE.sha256, parts: SEX_FILE.parts }] }),
    ready: async () => ({ small: true, full: false }),
    stored: async () => null,
    toDownload: () => ({ plain: 1, wire: 1, total: 1 }),
    offer: (which: string) => ({ state: which === 'small' ? 'current' : 'missing', plain: 1, wire: 1, total: 1, from: null, to: null, fromBytes: null, toBytes: null, notes: null }),
    sexReady: async () => false,
    sexInfo: async () => null,
    sexLoaded: () => null,
    sexToDownload: () => ({ plain: SEX_FILE.bytes, wire: SEX_FILE.bytes, total: SEX_FILE.bytes }),
    busy: () => false,
    saved: async () => ({ done: 0, total: 0 }),
    pageOffline: async () => false,
    stock: async () => null,
    refreshStock: async () => {},
    forgetStock: async () => {},
    forgetSex: async () => {},
    loadHead: async () => null,
    loaded: () => false,
    lastMs: {},
    loadGeneration: () => 0,
  }
  return {
    ...real,
    createEngine: (_root: string, pageFiles: () => string[]) => {
      world.pageFiles = pageFiles
      return engine
    },
  }
})
vi.mock('../../lib/identifyRefs', async orig => {
  const real = await orig<typeof import('../../lib/identifyRefs')>()
  return { ...real, referencePhotos: async () => ({ refs: {}, pending: [] }), fieldIndex: async () => ({ species: [], subspecies: [] }) }
})
vi.mock('../../lib/identifyOnline', async orig => {
  const real = await orig<typeof import('../../lib/identifyOnline')>()
  return {
    ...real,
    identifyOnServer: async (_photo: string, side: string | null) => {
      world.sides.push(side)
      world.last = world.answers.shift() ?? world.last
      return world.last
    },
    photoCanvas: async () => Object.assign(document.createElement('canvas'), { width: 400, height: 300 }),
    jpegBase64: () => 'AA==',
    squareCrop: () => 'data:image/jpeg;base64,AA==',
  }
})
vi.mock('../../lib/offlinePhoto', async orig => {
  const real = await orig<typeof import('../../lib/offlinePhoto')>()
  return {
    ...real,
    takePhoto: async (_input: unknown, identify: (p: unknown) => unknown) =>
      identify({ blob: new Blob(['x'], { type: 'image/jpeg' }), bytes: new ArrayBuffer(4), name: 'a.jpg', type: 'image/jpeg', size: 4 }),
  }
})
vi.mock('../../lib/photoCountry', async orig => {
  const real = await orig<typeof import('../../lib/photoCountry')>()
  return { ...real, warmShapes: () => {}, photoCountry: async () => world.country, exifGps: () => world.gps }
})

const answer = (species: [string, number][], subspecies: [string, number][] = []) => ({
  species,
  subspecies,
  genus: [[species[0][0].split(' ')[0], 0.96]],
  recorded: null,
  crop: 'success',
  wingBox: null,
  prior: null,
  ms: 400,
})

let unmount = () => {}
afterEach(() => {
  unmount()
  unmount = () => {}
  vi.unstubAllEnvs()
  delete (navigator as { serviceWorker?: unknown }).serviceWorker
  sessionStorage.clear()
  localStorage.clear()
  vi.unstubAllGlobals()
  vi.restoreAllMocks()
  document.body.innerHTML = ''
  Object.assign(world, { answers: [], last: null, sides: [], gps: null, country: null, pageFiles: null })
})

/** The page opened with no photo yet. `list`: what each request for the guides' list gets, in turn (the last one stays). */
async function openIdentify(opts: { flag?: boolean; list?: ('ok' | 'fail' | 'http500')[]; online?: boolean; neverIdle?: boolean; worker?: boolean } = {}) {
  vi.resetModules()
  vi.stubEnv('VITE_CONFIRM_VISUAL_CARDS', opts.flag === false ? '0' : '1')
  const turns = [...(opts.list ?? ['ok'])]
  const listRequests: string[] = []
  vi.stubGlobal('matchMedia', () => ({ matches: false, addEventListener() {}, removeEventListener() {} }))
  // The browser says when it is idle: here right after the page's own first work.
  const idle = { asked: 0, cancelled: 0 }
  vi.stubGlobal('requestIdleCallback', (run: () => void) => {
    idle.asked++
    return setTimeout(run, opts.neverIdle ? 3_600_000 : 0)
  })
  vi.stubGlobal('cancelIdleCallback', (id: ReturnType<typeof setTimeout>) => {
    idle.cancelled++
    clearTimeout(id)
  })
  // A page the service worker has (it keeps the list itself), or a first visit.
  if (opts.worker) Object.defineProperty(navigator, 'serviceWorker', { configurable: true, value: { controller: {} } })
  const signal = { on: opts.online !== false }
  vi.spyOn(navigator, 'onLine', 'get').mockImplementation(() => signal.on)
  vi.stubGlobal('fetch', async (input: RequestInfo | URL) => {
    const url = String(input)
    if (url.includes('confirmar_visual_manifest.json')) {
      listRequests.push(url)
      const turn = turns.length > 1 ? turns.shift() : turns[0]
      if (turn === 'fail') throw new TypeError('Failed to fetch')
      if (turn === 'http500') return new Response('', { status: 500 })
      return new Response(JSON.stringify(REAL_MANIFEST), { status: 200, headers: { 'content-type': 'application/json' } })
    }
    return new Response('{}', { status: 404 })
  })
  vi.spyOn(HTMLCanvasElement.prototype, 'getContext').mockReturnValue({ drawImage() {}, putImageData() {}, createImageData: () => ({ data: [] }) } as never)
  vi.spyOn(HTMLCanvasElement.prototype, 'toDataURL').mockReturnValue('data:image/jpeg;base64,AA==')
  localStorage.setItem('ui:locale', 'es')

  const { locale, t, tn } = await import('../../lib/i18n')
  locale.value = 'es'
  const { useSession } = await import('../../stores/session')
  const pinia = createPinia()
  setActivePinia(pinia)
  const session = useSession()
  session.ready = true
  session.user = { id: 7, username: 'lab', displayName: 'Lab', role: 'admin' } as never

  const { default: IdentifyView } = await import('../IdentifyView.vue')
  const router = createRouter({
    history: createMemoryHistory(),
    routes: [
      { path: '/identificar', component: IdentifyView },
      { path: '/:p(.*)', component: { template: '<div />' } },
    ],
  })
  await router.push('/identificar')
  const host = document.createElement('div')
  document.body.append(host)
  const app = createApp(IdentifyView)
  app.use(pinia).use(router)
  app.config.globalProperties.$t = t
  app.config.globalProperties.$tn = tn
  app.mount(host)
  unmount = () => app.unmount()

  const settle = async () => {
    await vi.dynamicImportSettled()
    for (let i = 0; i < 20; i++) await new Promise(r => setTimeout(r, 0))
    await vi.dynamicImportSettled()
    await nextTick()
  }
  await settle()
  /** A photo chosen with «Elegir foto», read by the server stand-in. */
  const sendPhoto = async (read: ReturnType<typeof answer>) => {
    world.answers.push(read)
    host.querySelector<HTMLInputElement>('input[type=file]:not([capture])')!.dispatchEvent(new Event('change'))
    await settle()
  }
  const card = () => host.querySelector('[data-testid="confirmar-visual-card"]')
  return { host, settle, sendPhoto, card, listRequests, idle, signal }
}

describe('IdentifyView: the guides\' list for a phone that works without signal first', () => {
  it('asks for the list once, when the page is idle with signal, before any photo', async () => {
    const { host, listRequests, idle, sendPhoto, card } = await openIdentify()
    expect(host.querySelector('[data-testid="headline"]')).toBeNull() // no photo yet
    expect(idle.asked).toBe(1)
    expect(listRequests).toHaveLength(1)
    expect(listRequests[0]).toContain('identificar/confirmar/confirmar_visual_manifest.json')
    // The photo that comes later needs no second request.
    await sendPhoto(answer([['Heliconius erato', 0.6], ['Heliconius melpomene', 0.35]]))
    expect(card()!.getAttribute('data-card-id')).toBe('p01_west')
    expect(listRequests).toHaveLength(1)
  })

  it('a list that could not be fetched is asked again at the next identification', async () => {
    const { listRequests, sendPhoto, card } = await openIdentify({ list: ['fail', 'ok'] })
    expect(listRequests).toHaveLength(1) // asked while idle; the request failed
    await sendPhoto(answer([['Heliconius erato', 0.6], ['Heliconius melpomene', 0.35]]))
    expect(listRequests).toHaveLength(2)
    expect(card()!.getAttribute('data-card-id')).toBe('p01_west')
  })

  it('a server error is asked again too, at each new photo until the list comes', async () => {
    const { listRequests, sendPhoto, card } = await openIdentify({ list: ['http500', 'http500', 'ok'] })
    expect(listRequests).toHaveLength(1)
    await sendPhoto(answer([['Heliconius erato', 0.6], ['Heliconius melpomene', 0.35]]))
    expect(listRequests).toHaveLength(2)
    expect(card()).toBeNull() // no list, no guide; the result itself is there
    await sendPhoto(answer([['Heliconius erato', 0.6], ['Heliconius melpomene', 0.35]]))
    expect(listRequests).toHaveLength(3)
    expect(card()!.getAttribute('data-card-id')).toBe('p01_west')
  })

  it('opened without signal: nothing is asked until the signal returns, then the list is fetched', async () => {
    const { listRequests, idle, signal, settle } = await openIdentify({ online: false })
    expect(idle.asked).toBe(0)
    expect(listRequests).toHaveLength(0)
    signal.on = true
    dispatchEvent(new Event('online'))
    await settle()
    expect(listRequests).toHaveLength(1)
  })

  it('a page left before the browser was idle asks for nothing after', async () => {
    const { idle, listRequests } = await openIdentify({ neverIdle: true })
    expect(idle.asked).toBe(1)
    expect(listRequests).toHaveLength(0)
    unmount()
    unmount = () => {}
    expect(idle.cancelled).toBe(1)
  })

  it('a photo read before the browser was idle fetches the list itself', async () => {
    const { listRequests, sendPhoto, card } = await openIdentify({ neverIdle: true })
    expect(listRequests).toHaveLength(0)
    await sendPhoto(answer([['Heliconius erato', 0.6], ['Heliconius melpomene', 0.35]]))
    expect(listRequests).toHaveLength(1)
    expect(card()!.getAttribute('data-card-id')).toBe('p01_west')
  })

  it('the list is no file of the page (a model\'s download never waits on it); the worker\'s cache gets it when the worker did not see it pass', async () => {
    // A first visit: no worker has the page yet.
    const added: string[] = []
    const opened: string[] = []
    const fakeCaches = { open: async (name: string) => (opened.push(name), { add: async (url: string) => void added.push(url) }) }
    vi.stubGlobal('caches', fakeCaches)
    await openIdentify()
    expect(world.pageFiles!().some(u => u.includes('confirmar_visual_manifest'))).toBe(false)
    expect(new Set(opened)).toEqual(new Set(['ithomiini-prueba-offline'])) // the identifier's cache, which the worker reads
    expect(added).toHaveLength(1)
    expect(added[0]).toMatch(/\/identificar\/confirmar\/confirmar_visual_manifest\.json$/)
    // A page the worker has: it kept the list itself as it passed.
    unmount()
    unmount = () => {}
    added.length = 0
    vi.stubGlobal('caches', fakeCaches)
    await openIdentify({ worker: true })
    expect(added).toHaveLength(0)
    // A list that did not come is not kept; a cache that refuses is not an error of the page.
    unmount()
    unmount = () => {}
    vi.stubGlobal('caches', { open: async () => Promise.reject(new Error('quota')) })
    const { listRequests, sendPhoto, card } = await openIdentify({ list: ['fail', 'ok'] })
    expect(listRequests).toHaveLength(1)
    await sendPhoto(answer([['Heliconius erato', 0.6], ['Heliconius melpomene', 0.35]]))
    expect(card()!.getAttribute('data-card-id')).toBe('p01_west')
  })

  it('the page\'s files for airplane mode are what the document lists, a script asked for later included', async () => {
    await openIdentify()
    const lazy = Object.assign(document.createElement('link'), { rel: 'modulepreload', href: 'https://example.test/assets/ConfirmarVisualCard-abc.js' })
    const lazyOld = Object.assign(document.createElement('link'), { rel: 'preload', href: 'https://example.test/assets/info-abc.js' })
    lazyOld.setAttribute('as', 'script')
    const font = Object.assign(document.createElement('link'), { rel: 'preload', href: 'https://example.test/fonts/a.woff2' })
    font.setAttribute('as', 'font')
    document.head.append(lazy, lazyOld, font)
    try {
      const kept = world.pageFiles!()
      expect(kept[0]).toBe(location.href.split('#')[0])
      expect(kept).toContain('https://example.test/assets/ConfirmarVisualCard-abc.js')
      expect(kept).toContain('https://example.test/assets/info-abc.js')
      expect(kept).not.toContain('https://example.test/fonts/a.woff2')
    } finally {
      lazy.remove()
      lazyOld.remove()
      font.remove()
    }
  })

  it('with the guides off the page asks for nothing, idle or at a photo', async () => {
    const { listRequests, idle, sendPhoto, card, host } = await openIdentify({ flag: false })
    expect(idle.asked).toBe(0)
    await sendPhoto(answer([['Heliconius erato', 0.6], ['Heliconius melpomene', 0.35]]))
    expect(host.querySelector('[data-testid="headline"]')).not.toBeNull()
    expect(card()).toBeNull()
    expect(listRequests).toHaveLength(0)
  })
})

describe('IdentifyView: a photo with no side of the Andes', () => {
  const TANDAYAPA = { lat: 0.0044, lon: -78.6769 }

  it('Tandayapa (inside the ridge band, clearly west of the line): a guide for the East alone is not shown', async () => {
    const { sendPhoto, card, host } = await openIdentify()
    Object.assign(world, { gps: TANDAYAPA, country: 'EC' })
    await sendPhoto(answer([['Eresia eunice', 0.6], ['Eresia pelonia', 0.3]]))
    expect(host.querySelector('[data-testid="headline"]')!.getAttribute('data-top')).toBe('Eresia eunice')
    expect(card()).toBeNull()
    expect(host.querySelector('[data-testid="confirmar-honest-note"]')).toBeNull()
    // The same two species from a photo with no position: the guide, saying it is the East's.
    Object.assign(world, { gps: null, country: null })
    await sendPhoto(answer([['Eresia eunice', 0.6], ['Eresia pelonia', 0.3]]))
    expect(card()!.getAttribute('data-card-id')).toBe('w6_05')
    expect(host.querySelector('[data-testid="confirmar-region-unknown-text"]')!.textContent).toBe('Región no determinada: esta guía es de Oriente')
  })

  it('Tandayapa: a guide for the West is shown with the line, and choosing the side there sets the page\'s side', async () => {
    const { sendPhoto, card, host, settle } = await openIdentify()
    Object.assign(world, { gps: TANDAYAPA, country: 'EC' })
    await sendPhoto(answer([['Heliconius cydno', 0.6], ['Heliconius eleuchia', 0.3]]))
    expect(card()!.getAttribute('data-card-id')).toBe('w4_11')
    expect(host.querySelector('[data-testid="confirmar-region-unknown-text"]')!.textContent).toBe('Región no determinada: esta guía es de Occidente')
    const checked = () => host.querySelector('[data-testid="settings"] [role="radio"][aria-checked="true"]')!.textContent!.trim()
    expect(checked()).toBe('No sé')

    host.querySelector<HTMLButtonElement>('[data-testid="confirmar-side-West"]')!.click()
    await settle()
    // The page's own setting, kept for the next photos; the server read the photo again with it.
    expect(checked()).toBe('Oeste (Costa)')
    expect(localStorage.getItem('identify:side')).toBe('West')
    expect(world.sides).toEqual([null, 'West'])
    // The guide stays, now for a known side: the line is gone, its badge names the region.
    expect(card()!.getAttribute('data-card-id')).toBe('w4_11')
    expect(host.querySelector('[data-testid="confirmar-region-unknown"]')).toBeNull()
    expect(host.querySelector('[data-testid="confirmar-region-badge"]')!.textContent!.trim()).toBe('Occidente')
  })

  it('the other side chosen on a one-sided guide takes the guide away (there is none for that side)', async () => {
    const { sendPhoto, card, host, settle } = await openIdentify()
    await sendPhoto(answer([['Eresia eunice', 0.6], ['Eresia pelonia', 0.3]]))
    expect(card()!.getAttribute('data-card-id')).toBe('w6_05')
    host.querySelector<HTMLButtonElement>('[data-testid="confirmar-side-West"]')!.click()
    await settle()
    expect(card()).toBeNull()
    expect(host.querySelector('[data-testid="settings"] [role="radio"][aria-checked="true"]')!.textContent!.trim()).toBe('Oeste (Costa)')
  })

  it('the head\'s subspecies as it names them reach the four sides that were unreachable', async () => {
    const { sendPhoto, card } = await openIdentify()
    for (const [species, subspecies, id] of [
      [[['Heliconius cydno', 0.55], ['Heliconius timareta', 0.35]], [['Heliconius cydno alithea', 0.5]], 'p14'],
      [[['Heliconius cydno', 0.55], ['Heliconius eleuchia', 0.35]], [['Heliconius cydno alithea', 0.5]], 'w4_11'],
      [[['Eresia eunice', 0.55], ['Eresia pelonia', 0.35]], [['Eresia eunice gudruna', 0.5]], 'w6_05'],
      [[['Eresia pelonia', 0.55], ['Eresia eunice', 0.35]], [['Eresia pelonia ithomiola', 0.5]], 'w6_05'],
    ] as [[string, number][], [string, number][], string][]) {
      await sendPhoto(answer(species, subspecies))
      expect(card()?.getAttribute('data-card-id'), `${subspecies[0][0]}`).toBe(id)
    }
  })
})
