import { readFileSync } from 'node:fs'
import { resolve } from 'node:path'
import { afterEach, describe, expect, it, vi } from 'vitest'
import { createApp, nextTick } from 'vue'
import { createPinia, setActivePinia } from 'pinia'
import { createMemoryHistory, createRouter } from 'vue-router'
import { SEX_FILE } from '../../lib/sexModel'
import type { ConfirmarVisualManifest } from '../../lib/confirmarVisualRouter'

// The reference photos of /identificar/: the subspecies the model names first (Franz's phone, 26 Nov 2026).
const REAL_MANIFEST = JSON.parse(
  readFileSync(resolve(import.meta.dirname, '../../../public/identificar/confirmar/confirmar_visual_manifest.json'), 'utf8'),
) as ConfirmarVisualManifest

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
// H. melpomene's photos as the server sends them: a red-band postman first (the curation's hero), then a rayed one;
// and iNaturalist's photos of the subspecies the model names.
const photo = (id: string, level: 'species' | 'subspecies' | 'specimen', taxon?: string) => ({
  src: `https://x.test/${id}.jpg`,
  credit: `© ${id}`,
  license: level === 'specimen' ? '' : 'CC-BY',
  level,
  ...(taxon ? { taxon } : {}),
  source: level === 'specimen' ? null : `https://www.inaturalist.org/photos/${id.replace(/\D/g, '') || '1'}`,
  country: 'EC',
})
vi.mock('../../lib/identifyRefs', async orig => {
  const real = await orig<typeof import('../../lib/identifyRefs')>()
  return {
    ...real,
    referencePhotos: async (names: string[]) => ({
      refs: Object.fromEntries(
        names.map(n => [
          n,
          {
            photos:
              n === 'Heliconius melpomene'
                ? [photo('postman1', 'species'), photo('rayed2', 'species'), photo('malleti3', 'subspecies', 'Heliconius melpomene malleti'), photo('cam4', 'specimen')]
                : [photo(`${n.split(' ')[1]}9`, 'species')],
            taxonUrl: null,
          },
        ]),
      ),
      pending: [],
    }),
    savedPhotos: async () => ({}),
    subspeciesRefPhotos: async (name: string) => ({
      photos: name === 'Heliconius melpomene schunkei' ? [photo('schunkei7', 'subspecies', name), photo('schunkei8', 'subspecies', name)] : [],
      pending: false,
    }),
    fieldIndex: async () => ({ species: [], subspecies: [] }),
  }
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
  vi.unstubAllEnvs()
  sessionStorage.clear()
  localStorage.clear()
  vi.unstubAllGlobals()
  vi.restoreAllMocks()
  document.body.innerHTML = ''
})

function makeStoredResult(
  species: [string, number][],
  subspecies: [string, number][] = [],
  side: 'East' | 'West' | null = null,
  country: string | null = null,
) {
  return {
    at: '2026-11-25T16:00:00.000Z',
    source: 'online',
    crop: 'data:image/jpeg;base64,AA==',
    status: 'success',
    ms: 500,
    photo: 'AA==',
    side,
    country,
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
  vi.resetModules()
  vi.stubEnv('VITE_CONFIRM_VISUAL_CARDS', opts.flag ? '1' : '0')
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

  const { locale, t, tn } = await import('../../lib/i18n')
  locale.value = lang
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
  return { host, settle, fetchedUrls, locale }
}

const shownRef = (host: HTMLElement) => host.querySelector<HTMLImageElement>('[data-testid="zoom-ref"] img')?.getAttribute('src')
/** The caption under the reference photo (the figure that holds it). */
const refCaption = (host: HTMLElement) => host.querySelector('[data-testid="zoom-ref"]')?.closest('figure')?.querySelector('figcaption')?.textContent ?? ''
const species: [string, number][] = [
  ['Heliconius melpomene', 0.79],
  ['Heliconius elevatus', 0.09],
  ['Heliconius xanthocles', 0.05],
  ['Heliconius aoede', 0.01],
]

describe('IdentifyView: reference photos of the subspecies the model names first', () => {
  it('H. melpomene read as schunkei: a schunkei photo first, labelled as it, ahead of the species and its curated hero', async () => {
    const { host } = await mountIdentify(makeStoredResult(species, [['Heliconius melpomene schunkei', 0.77]], 'East', 'EC'), { flag: false, lang: 'es' })
    expect(shownRef(host)).toBe('https://x.test/schunkei7.jpg')
    expect(refCaption(host)).toContain('Subespecie schunkei.')
  })

  it('no subspecies named: the order exactly as before (the species photo first)', async () => {
    const { host } = await mountIdentify(makeStoredResult(species, [], 'East', 'EC'), { flag: false, lang: 'es' })
    expect(shownRef(host)).toBe('https://x.test/postman1.jpg')
    expect(refCaption(host)).not.toContain('Subespecie')
  })

  it('a subspecies with no photo of its own: the order as before', async () => {
    const { host } = await mountIdentify(makeStoredResult(species, [['Heliconius melpomene rosina', 0.7]], 'East', 'EC'), { flag: false, lang: 'es' })
    expect(shownRef(host)).toBe('https://x.test/postman1.jpg')
  })

  it('a subspecies already in the species list (a pack’s) comes first with no extra photos', async () => {
    const { host } = await mountIdentify(makeStoredResult(species, [['Heliconius melpomene malleti', 0.6]], 'East', 'EC'), { flag: false, lang: 'es' })
    expect(shownRef(host)).toBe('https://x.test/malleti3.jpg')
    expect(refCaption(host)).toContain('Subespecie malleti.')
  })
})
