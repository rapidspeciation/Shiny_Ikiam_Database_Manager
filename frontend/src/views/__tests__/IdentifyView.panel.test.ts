import { afterEach, describe, expect, it, vi } from 'vitest'
import { createApp, nextTick } from 'vue'
import { createPinia, setActivePinia } from 'pinia'
import { createMemoryHistory, createRouter } from 'vue-router'
import { locale, t, tn } from '../../lib/i18n'
import { SEX_FILE } from '../../lib/sexModel'
import { useSession } from '../../stores/session'

// Identificar's panel (13 Nov 2026) with a server result restored from this browser session: the other candidates as
// thumbnails that switch the comparison in place, the sex after the comparison, no «Tipo de foto» question, and the
// photos opening full screen (Esc closes). The phone's models and the server are stand-ins. No AI.

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

const server = vi.hoisted(() => ({ answers: [] as unknown[], onCall: null as null | (() => void) }))
vi.mock('../../lib/identifyOnline', async orig => {
  const real = await orig<typeof import('../../lib/identifyOnline')>()
  return {
    ...real,
    identifyOnServer: async () => {
      server.onCall?.()
      return server.answers.shift()
    },
    photoCanvas: async () => Object.assign(document.createElement('canvas'), { width: 400, height: 300 }),
    jpegBase64: () => 'AA==',
    squareCrop: () => 'data:image/jpeg;base64,AA==',
  }
})
vi.mock('../../lib/offlinePhoto', async orig => {
  const real = await orig<typeof import('../../lib/offlinePhoto')>()
  return { ...real, takePhoto: async (input: { value: string }, identify: (p: unknown) => unknown) => identify({ blob: new Blob(['x'], { type: 'image/jpeg' }) }) }
})

let unmount = () => {}
afterEach(() => {
  unmount()
  sessionStorage.clear()
  localStorage.clear()
  vi.unstubAllGlobals()
  vi.restoreAllMocks()
  document.body.innerHTML = ''
  locale.value = 'en'
})

async function mountWith(stored: Record<string, unknown>) {
  vi.stubGlobal('matchMedia', () => ({ matches: false, addEventListener() {}, removeEventListener() {} }))
  vi.stubGlobal('fetch', async () => new Response('{}', { status: 404 }))
  // jsdom has no canvas: the page's small thumbnail is drawn on a stand-in
  vi.spyOn(HTMLCanvasElement.prototype, 'getContext').mockReturnValue({ drawImage() {}, putImageData() {}, createImageData: () => ({ data: [] }) } as never)
  vi.spyOn(HTMLCanvasElement.prototype, 'toDataURL').mockReturnValue('data:image/jpeg;base64,AA==')
  sessionStorage.setItem('identify:last', JSON.stringify(stored))
  localStorage.setItem('ui:locale', 'es')
  locale.value = 'es'
  const pinia = createPinia()
  setActivePinia(pinia)
  const session = useSession()
  session.ready = true
  session.user = { id: 7, username: 'lab', displayName: 'Lab', role: 'admin' } as never
  const { default: IdentifyView } = await import('../IdentifyView.vue')
  const router = createRouter({ history: createMemoryHistory(), routes: [{ path: '/identificar', component: IdentifyView }, { path: '/:p(.*)', component: { template: '<div />' } }] })
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
  return { host, settle }
}

const three = {
  species: [['Mechanitis polymnia', 0.6], ['Mechanitis lysimnia', 0.2], ['Hypothyris ninonia', 0.1]],
  subspecies: [],
  genus: [['Mechanitis', 0.8], ['Hypothyris', 0.1]],
  recorded: null,
  crop: 'success',
  wingBox: null,
  prior: null,
  ms: 900,
}
const restored = { at: '2026-11-13T06:00:00.000Z', source: 'online', crop: 'data:image/jpeg;base64,AA==', status: 'success', ms: 900, server: three, photo: 'AA==', side: null }

describe('IdentifyView: the panel', () => {
  it('the other candidates are thumbnails; a tap compares that species in place', async () => {
    const { host, settle } = await mountWith(restored)
    const tiles = [...host.querySelectorAll<HTMLButtonElement>('[data-testid=candidates] > button')]
    expect(tiles.map(b => b.dataset.candidate)).toEqual(['Mechanitis polymnia', 'Mechanitis lysimnia', 'Hypothyris ninonia'])
    expect(tiles[0].getAttribute('aria-pressed')).toBe('true')
    tiles[2].click()
    await settle()
    expect(host.querySelector('[data-testid=compare]')!.textContent).toContain('Comparando con Hypothyris ninonia')
    expect(host.querySelectorAll<HTMLButtonElement>('[data-testid=candidates] > button')[2].getAttribute('aria-pressed')).toBe('true')
    // the headline stays the AI's first species
    expect(host.querySelector('[data-testid=headline]')!.getAttribute('data-top')).toBe('Mechanitis polymnia')
  })

  it('the sex comes after the comparison; no «Tipo de foto» question; the add and clear controls at the top', async () => {
    const { host } = await mountWith(restored)
    const compare = host.querySelector('[data-testid=compare]')!
    const sex = host.querySelector('[data-testid=sex-panel]')!
    expect(compare.compareDocumentPosition(sex) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy()
    expect(host.textContent).not.toMatch(/Tipo de foto|En la mano|Colección/)
    const bar = host.querySelector('[data-testid=photo-bar]')!
    expect(bar.compareDocumentPosition(compare) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy()
    expect(host.querySelector('input[type=file][multiple]')).not.toBeNull()
  })

  it('«Es esta» marks the species compared as the person\'s choice, and the sex panel sends it with the sex', async () => {
    const { host, settle } = await mountWith(restored)
    host.querySelector<HTMLButtonElement>('[data-testid=pick-this]')!.click()
    await settle()
    expect(host.querySelector('[data-testid=pick-this]')!.getAttribute('aria-pressed')).toBe('true')
    expect(host.querySelector('[data-testid=candidates] [data-candidate="Mechanitis polymnia"] .rounded-full')).not.toBeNull()
  })

  it('the person\'s photo and the reference open full screen; Esc closes', async () => {
    const { host, settle } = await mountWith(restored)
    host.querySelector<HTMLButtonElement>('[data-testid=zoom-mine]')!.click()
    await settle()
    expect(document.querySelector('[data-testid=photo-viewer]')).not.toBeNull()
    expect(document.querySelector('[data-testid=viewer-name]')!.textContent).toContain('Su foto')
    expect(document.body.style.overflow).toBe('hidden')
    dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape' }))
    await settle()
    for (let i = 0; i < 5 && document.querySelector('[data-testid=photo-viewer]'); i++) {
      dispatchEvent(new PopStateEvent('popstate'))
      await settle()
    }
    expect(document.querySelector('[data-testid=photo-viewer]')).toBeNull()
  })

  it('«Borrar fotos» while a photo is being read: nothing comes back when the answer arrives', async () => {
    localStorage.setItem('identify:log', JSON.stringify([{ at: restored.at, thumb: 'data:image/jpeg;base64,AA==', ai: 'Mechanitis polymnia', p: 0.6, pick: null, side: null, saved: true }]))
    sessionStorage.setItem('identify:shots', JSON.stringify([restored.at]))
    const { host, settle } = await mountWith(restored)
    let answer: (v: unknown) => void = () => {}
    server.answers.push(new Promise(ok => (answer = ok)))
    host.querySelector<HTMLInputElement>('input[type=file]:not([capture])')!.dispatchEvent(new Event('change'))
    await settle()
    expect(server.answers).toHaveLength(0) // sent, waiting for the server
    host.querySelector<HTMLButtonElement>('[data-testid=clear-photos]')!.click()
    await settle()
    expect(host.querySelector('[data-testid=photo-start]')).not.toBeNull()
    answer({ ...three, species: [['Melinaea ludovica', 0.9]], genus: [['Melinaea', 0.9]] })
    await settle()
    expect(host.querySelector('[data-testid=headline]')).toBeNull()
    expect(host.querySelector('[data-testid=photo-start]')).not.toBeNull()
    // the buttons work again at once
    expect(host.querySelector<HTMLButtonElement>('[data-testid=add-photos]')!.disabled).toBe(false)
  })
})
