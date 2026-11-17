import { afterEach, describe, expect, it, vi } from 'vitest'
import { createApp, nextTick } from 'vue'
import { createPinia, setActivePinia } from 'pinia'
import { createMemoryHistory, createRouter } from 'vue-router'
import { locale, t, tn } from '../../lib/i18n'
import { useSession } from '../../stores/session'
import type { VisitorPlace } from '../../lib/identifyOnline'

// Identificar signed out with the server taking visitors' photos (ANON_SERVER_IDENTIFY, lib/identifyOnline.ts): the
// mode a visitor starts in, the queue place while a side change is read again, and the place given up when the
// visitor leaves through the app. The phone's models and the server are stand-ins. No AI.

const phone = vi.hoisted(() => ({ ready: false }))
vi.mock('../../lib/offlineEngine', async orig => {
  const real = await orig<typeof import('../../lib/offlineEngine')>()
  const engine = {
    list: async () => ({ files: [] }),
    ready: async () => ({ small: phone.ready, full: false }),
    stored: async () => null,
    toDownload: () => ({ plain: 1, wire: 1, total: 1 }),
    offer: () => ({ state: phone.ready ? 'current' : 'missing', plain: 1, wire: 1, total: 1, from: null, to: null, fromBytes: null, toBytes: null, notes: null }),
    sexReady: async () => false,
    sexInfo: async () => null,
    sexLoaded: () => null,
    sexToDownload: () => ({ plain: 1, wire: 1, total: 1 }),
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

/** The visitor's queue: each call says a place, then waits until the test answers or the page gives it up. */
const queue = vi.hoisted(() => ({ calls: [] as { side: unknown; signal?: AbortSignal; place: (p: VisitorPlace) => void; answer: (a: unknown) => void }[] }))
vi.mock('../../lib/identifyOnline', async orig => {
  const real = await orig<typeof import('../../lib/identifyOnline')>()
  return {
    ...real,
    visitorServer: async () => ({ on: true, waitS: 2 }),
    identifyAsVisitor: (_photo: string, side: unknown, { onPlace, signal }: { onPlace?: (p: VisitorPlace) => void; signal?: AbortSignal } = {}) =>
      new Promise((resolve, reject) => {
        signal?.addEventListener('abort', () => reject(new DOMException('aborted', 'AbortError')))
        queue.calls.push({ side, signal, place: p => onPlace?.(p), answer: resolve })
      }),
    identifyOnServer: async () => {
      throw new Error('the team path is not used signed out')
    },
    photoCanvas: async () => Object.assign(document.createElement('canvas'), { width: 400, height: 300 }),
    jpegBase64: () => 'AA==',
    squareCrop: () => 'data:image/jpeg;base64,AA==',
  }
})
vi.mock('../../lib/offlinePhoto', async orig => {
  const real = await orig<typeof import('../../lib/offlinePhoto')>()
  return { ...real, takePhoto: async (_input: unknown, identify: (p: unknown) => unknown) => identify({ blob: new Blob(['x'], { type: 'image/jpeg' }), bytes: new ArrayBuffer(4) }) }
})

let unmount = () => {}
afterEach(() => {
  unmount()
  queue.calls = []
  phone.ready = false
  sessionStorage.clear()
  localStorage.clear()
  vi.unstubAllGlobals()
  vi.restoreAllMocks()
  document.body.innerHTML = ''
  locale.value = 'en'
})

async function mountSignedOut(stored: Record<string, unknown> | null = null) {
  vi.stubGlobal('matchMedia', () => ({ matches: false, addEventListener() {}, removeEventListener() {} }))
  vi.stubGlobal('fetch', async () => new Response('{}', { status: 404 }))
  vi.spyOn(HTMLCanvasElement.prototype, 'getContext').mockReturnValue({ drawImage() {}, putImageData() {}, createImageData: () => ({ data: [] }) } as never)
  vi.spyOn(HTMLCanvasElement.prototype, 'toDataURL').mockReturnValue('data:image/jpeg;base64,AA==')
  if (stored) sessionStorage.setItem('identify:last', JSON.stringify(stored))
  localStorage.setItem('ui:locale', 'es')
  locale.value = 'es'
  const pinia = createPinia()
  setActivePinia(pinia)
  const session = useSession()
  session.ready = true
  session.user = null
  const { default: IdentifyView } = await import('../IdentifyView.vue')
  const router = createRouter({ history: createMemoryHistory(), routes: [{ path: '/identificar', component: IdentifyView }, { path: '/buscar', component: { template: '<div />' } }] })
  await router.push('/identificar')
  const host = document.createElement('div')
  document.body.append(host)
  const app = createApp({ template: '<RouterView />' })
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
  return { host, settle, router }
}

const modeOf = (host: HTMLElement) =>
  [...host.querySelectorAll<HTMLButtonElement>('[data-testid=mode] [role=radio]')].find(b => b.getAttribute('aria-checked') === 'true')?.textContent?.trim()

const answer = { species: [['Oleria onega', 0.9]], subspecies: [], genus: [['Oleria', 0.95]], recorded: null, crop: 'success', wingBox: null, prior: null, ms: 400 }

describe('IdentifyView signed out, the server open to visitors', () => {
  it('a visitor with nothing on the phone starts online; one whose phone has a model stays on the phone', async () => {
    const first = await mountSignedOut()
    expect(modeOf(first.host)).toMatch(/En línea/)
    unmount()
    phone.ready = true
    const second = await mountSignedOut()
    expect(modeOf(second.host)).toMatch(/En este teléfono/)
  })

  it('a side change read again in the queue shows the place and the wait', async () => {
    const restored = { at: '2026-11-17T06:00:00.000Z', source: 'online', visitor: true, crop: 'data:image/jpeg;base64,AA==', status: 'success', ms: 400, server: answer, photo: 'AA==', side: null }
    const { host, settle } = await mountSignedOut(restored)
    expect(host.querySelector('[data-testid=headline]')!.textContent).toContain('Oleria onega')
    const east = [...host.querySelectorAll<HTMLButtonElement>('[data-testid=settings] [role=radio]')].find(b => /Este/.test(b.textContent ?? ''))!
    east.click()
    await settle()
    expect(queue.calls).toHaveLength(1)
    expect(queue.calls[0].side).toBe('East')
    queue.calls[0].place({ state: 'waiting', place: 3, ahead: 2, waitS: 4 })
    await settle()
    expect(host.textContent).toContain('En cola: 2 fotos antes que la suya, unos 4 s.')
    queue.calls[0].answer({ ...answer, prior: 'East' })
    await settle()
    expect(host.textContent).not.toContain('En cola')
  })

  it('leaving through the app while waiting gives the place up', async () => {
    const { host, settle, router } = await mountSignedOut()
    const input = host.querySelector<HTMLInputElement>('input[type=file][multiple]')!
    Object.defineProperty(input, 'files', { configurable: true, value: [new File(['x'], 'a.jpg', { type: 'image/jpeg' })] })
    input.dispatchEvent(new Event('change'))
    await settle()
    expect(queue.calls).toHaveLength(1)
    queue.calls[0].place({ state: 'waiting', place: 2, ahead: 1, waitS: 3 })
    await settle()
    expect(queue.calls[0].signal!.aborted).toBe(false)
    await router.push('/buscar')
    await settle()
    expect(queue.calls[0].signal!.aborted).toBe(true)
  })
})
