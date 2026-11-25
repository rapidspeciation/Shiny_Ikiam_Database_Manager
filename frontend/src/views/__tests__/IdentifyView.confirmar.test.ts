import { readFileSync } from 'node:fs'
import { resolve } from 'node:path'
import { afterEach, describe, expect, it, vi } from 'vitest'
import { createApp, nextTick } from 'vue'
import { createPinia, setActivePinia } from 'pinia'
import { createMemoryHistory, createRouter } from 'vue-router'
import { locale, t, tn } from '../../lib/i18n'
import { SEX_FILE } from '../../lib/sexModel'
import { useSession } from '../../stores/session'
import type { ConfirmarVisualManifest } from '../../lib/confirmarVisualRouter'

const REAL_MANIFEST = JSON.parse(
  readFileSync(resolve(import.meta.dirname, '../../../public/identificar/confirmar/confirmar_visual_manifest.json'), 'utf8'),
) as ConfirmarVisualManifest

const confirmarFlag = vi.hoisted(() => ({ enabled: false }))
vi.mock('../../lib/confirmarVisualRouter', async orig => {
  const real = await orig<typeof import('../../lib/confirmarVisualRouter')>()
  return {
    ...real,
    confirmVisualCardsEnabled: () => confirmarFlag.enabled,
  }
})

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
  return { ...real, createEngine: () => engine }
})
vi.mock('../../lib/identifyRefs', async orig => {
  const real = await orig<typeof import('../../lib/identifyRefs')>()
  return { ...real, referencePhotos: async () => ({ refs: {}, pending: [] }), fieldIndex: async () => ({ species: [], subspecies: [] }) }
})
vi.mock('../../lib/identifyOnline', async orig => {
  const real = await orig<typeof import('../../lib/identifyOnline')>()
  return {
    ...real,
    identifyOnServer: async (_photo: string, side: string | null) => ({
      species: [
        ['Heliconius erato', 0.62],
        ['Heliconius melpomene', 0.34],
      ],
      subspecies: [],
      genus: [['Heliconius', 0.98]],
      recorded: null,
      crop: 'success',
      wingBox: null,
      prior: side,
      ms: 400,
    }),
  }
})

let unmount = () => {}
afterEach(async () => {
  unmount()
  confirmarFlag.enabled = false
  const routerMod = await import('../../lib/confirmarVisualRouter')
  routerMod.resetConfirmarManifestCache()
  sessionStorage.clear()
  localStorage.clear()
  vi.unstubAllGlobals()
  vi.restoreAllMocks()
  document.body.innerHTML = ''
  locale.value = 'en'
})

function makeStoredResult(
  species: [string, number][],
  subspecies: [string, number][] = [],
  side: 'East' | 'West' | null = null,
) {
  return {
    at: '2026-11-25T16:00:00.000Z',
    source: 'online',
    crop: 'data:image/jpeg;base64,AA==',
    status: 'success',
    ms: 500,
    photo: 'AA==',
    side,
    server: {
      species,
      subspecies,
      genus: [[species[0][0].split(' ')[0], 0.96]],
      recorded: null,
      crop: 'success',
      wingBox: null,
      prior: side,
      ms: 500,
    },
  }
}

async function mountIdentify(
  stored: Record<string, unknown>,
  opts: { flag: boolean; lang?: 'es' | 'en' },
) {
  confirmarFlag.enabled = opts.flag
  const fetchedUrls: string[] = []
  vi.stubGlobal('matchMedia', () => ({ matches: false, addEventListener() {}, removeEventListener() {} }))
  vi.stubGlobal('fetch', async (input: RequestInfo | URL) => {
    const url = String(input)
    fetchedUrls.push(url)
    if (url.includes('confirmar_visual_manifest.json')) {
      return new Response(JSON.stringify(REAL_MANIFEST), { status: 200, headers: { 'content-type': 'application/json' } })
    }
    return new Response('{}', { status: 404 })
  })
  vi.spyOn(HTMLCanvasElement.prototype, 'getContext').mockReturnValue({
    drawImage() {},
    putImageData() {},
    createImageData: () => ({ data: [] }),
  } as never)
  vi.spyOn(HTMLCanvasElement.prototype, 'toDataURL').mockReturnValue('data:image/jpeg;base64,AA==')

  sessionStorage.setItem('identify:last', JSON.stringify(stored))
  const lang = opts.lang ?? 'es'
  localStorage.setItem('ui:locale', lang)
  locale.value = lang

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
    for (let i = 0; i < 20; i++) await new Promise(r => setTimeout(r, 0))
    await nextTick()
  }
  await settle()
  return { host, settle, fetchedUrls }
}

describe('IdentifyView: Confirmar visual cards integration', () => {
  it('flag OFF renders zero Confirmar elements, zero Confirmar copy, and makes zero manifest fetches', async () => {
    const { host, fetchedUrls } = await mountIdentify(
      makeStoredResult([
        ['Heliconius erato', 0.6],
        ['Heliconius melpomene', 0.35],
      ]),
      { flag: false, lang: 'es' },
    )
    expect(host.querySelector('[data-testid="confirmar-visual-card"]')).toBeNull()
    expect(host.querySelector('[data-testid="confirmar-honest-note"]')).toBeNull()
    expect(host.textContent).not.toContain('Guía visual')
    expect(host.textContent).not.toContain('Visual guide')
    expect(fetchedUrls.some(u => u.includes('confirmar_visual_manifest.json'))).toBe(false)
  })

  it('flag ON renders p01 visual card, switches region pill Occidente <-> Oriente, and syncs ES/EN locale', async () => {
    const { host, settle } = await mountIdentify(
      makeStoredResult([
        ['Heliconius erato', 0.6],
        ['Heliconius melpomene', 0.35],
      ]),
      { flag: true, lang: 'es' },
    )
    const card = host.querySelector('[data-testid="confirmar-visual-card"]')
    expect(card).not.toBeNull()
    expect(card!.getAttribute('data-card-id')).toBe('p01_west')
    expect(host.querySelector<HTMLImageElement>('[data-testid="confirmar-card-image"]')!.src).toContain('/cards/es/p01_west.webp')

    // Tap region pill to switch to Oriente Napo
    host.querySelector<HTMLButtonElement>('[data-testid="confirmar-region-opt-p01_east_napo"]')!.click()
    await settle()
    expect(host.querySelector('[data-testid="confirmar-visual-card"]')!.getAttribute('data-card-id')).toBe('p01_east_napo')
    expect(host.querySelector<HTMLImageElement>('[data-testid="confirmar-card-image"]')!.src).toContain('/cards/es/p01_east_napo.webp')

    // Switch app locale to English -> image URL and active region pill switch cleanly (no duplicate region badge)
    locale.value = 'en'
    await settle()
    expect(host.querySelector<HTMLImageElement>('[data-testid="confirmar-card-image"]')!.src).toContain('/cards/en/p01_east_napo.webp')
    expect(host.querySelector('[data-testid="confirmar-region-badge"]')).toBeNull()
    expect(host.querySelector('[data-testid="confirmar-region-opt-p01_east_napo"]')!.textContent?.trim()).toBe('East · Napo & Amazonia')
  })

  it('flag ON shows honest note (never a wrong card) for deferred p01 Pastaza ring and inseparable Wave 3B pairs', async () => {
    const { host } = await mountIdentify(
      makeStoredResult(
        [
          ['Heliconius erato', 0.6],
          ['Heliconius melpomene', 0.35],
        ],
        [
          ['Heliconius erato notabilis', 0.55],
          ['Heliconius melpomene plesseni', 0.3],
        ],
        'East',
      ),
      { flag: true, lang: 'es' },
    )
    expect(host.querySelector('[data-testid="confirmar-visual-card"]')).toBeNull()
    const deferred = host.querySelector('[data-testid="confirmar-honest-note"]')
    expect(deferred).not.toBeNull()
    expect(deferred!.getAttribute('data-status')).toBe('unavailable_deferred_ring')

    unmount()
    document.body.innerHTML = ''
    const { host: h2 } = await mountIdentify(
      makeStoredResult([
        ['Gorgythion begga', 0.54],
        ['Gorgythion beggina', 0.41],
      ]),
      { flag: true, lang: 'en' },
    )
    expect(h2.querySelector('[data-testid="confirmar-visual-card"]')).toBeNull()
    const insep = h2.querySelector('[data-testid="confirmar-honest-note"]')
    expect(insep).not.toBeNull()
    expect(insep!.getAttribute('data-status')).toBe('not_reliably_separable_in_photos')
    expect(insep!.textContent).toContain('almost identical')
  })
})
