import { afterEach, describe, expect, it, vi } from 'vitest'
import { createApp, nextTick } from 'vue'
import { createPinia, setActivePinia } from 'pinia'
import { createMemoryHistory, createRouter } from 'vue-router'
import { locale, t, tn } from '../../lib/i18n'
import { SEX_FILE } from '../../lib/sexModel'
import { useSession } from '../../stores/session'

// Identificar with a server result restored from this browser session (the page's own restore path): the in-hand
// line for an Ithomiini, the general sex line for a Monarch (asked again after the restore), and the general line
// gone once the page is switched to «En este teléfono». The phone's models and the server are stand-ins. No AI.

vi.mock('../../lib/offlineEngine', async orig => {
  const real = await orig<typeof import('../../lib/offlineEngine')>()
  const engine = {
    list: async () => ({ files: [{ name: SEX_FILE.name, role: 'sex', bytes: SEX_FILE.bytes, sha256: SEX_FILE.sha256, parts: SEX_FILE.parts }] }),
    ready: async () => ({ small: false, full: false }),
    stored: async () => null,
    toDownload: () => ({ plain: 1, wire: 1, total: 1 }),
    sexReady: async () => false,
    sexToDownload: () => ({ plain: SEX_FILE.bytes, wire: SEX_FILE.bytes, total: SEX_FILE.bytes }),
    pageOffline: async () => false,
    stock: async () => null,
    refreshStock: async () => {},
    forgetStock: async () => {},
    forgetSex: async () => {},
    loadHead: async () => null,
    loaded: () => false,
    lastMs: {},
  }
  return { ...real, createEngine: () => engine }
})
vi.mock('../../lib/identifyRefs', async orig => {
  const real = await orig<typeof import('../../lib/identifyRefs')>()
  return { ...real, referencePhotos: async () => ({ refs: {}, pending: [] }), fieldIndex: async () => ({ species: [], subspecies: [] }) }
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

const answer = (species: string, p: number, fieldSex: unknown) => ({
  species: [[species, p]],
  subspecies: [],
  genus: [[species.split(' ')[0], Math.min(1, p + 0.01)]],
  recorded: null,
  crop: 'success',
  wingBox: null,
  prior: null,
  ms: 900,
  ...(fieldSex ? { fieldSex } : {}),
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

const stored = (server: unknown, extra: Record<string, unknown> = {}) => ({
  at: '2026-11-11T06:00:00.000Z',
  source: 'online',
  crop: 'data:image/jpeg;base64,AA==',
  status: 'success',
  ms: 900,
  server,
  photo: 'AA==',
  side: null,
  ...extra,
})

describe('IdentifyView: the two sex lines', () => {
  it('an Ithomiini keeps the in-hand line, even if a general block came with it', async () => {
    const call = { species: 'Mechanitis messenoides', status: 'call', sex: 'male', pct: 97, version: 'field-sex/1' }
    const { host } = await mountWith(stored(answer('Mechanitis messenoides', 0.93, call)))
    expect(host.querySelector('[data-testid=sex-estimate]')).not.toBeNull()
    expect(host.querySelector('[data-testid=sex-estimate]')!.textContent).toContain('experimental: fotos en mano')
    expect(host.querySelector('[data-testid=field-sex]')).toBeNull()
  })

  it('a Monarch: asked again after a restore; «Sí, viva» shows the call; «En este teléfono» hides the line', async () => {
    const call = { species: 'Danaus plexippus', status: 'call', sex: 'male', pct: 96, version: 'field-sex/1' }
    const { host, settle } = await mountWith(stored(answer('Danaus plexippus', 0.97, call), { live: 'yes' }))
    expect(host.querySelector('[data-testid=sex-estimate]')).toBeNull()
    expect(host.querySelector('[data-testid=field-sex-ask]')).not.toBeNull()
    expect(host.querySelector('[data-testid=field-sex-call]')).toBeNull()
    ;(host.querySelector('[data-testid=field-sex-yes]') as HTMLButtonElement).click()
    await settle()
    expect(host.querySelector('[data-testid=field-sex-call]')!.textContent!.replace(/\s+/g, ' ').trim()).toBe('♂ Macho 96 %')
    const phone = [...host.querySelectorAll<HTMLButtonElement>('[data-testid=mode] button')].find(b => b.textContent!.includes('En este teléfono'))!
    phone.click()
    await settle()
    expect(host.querySelector('[data-testid=field-sex]')).toBeNull()
  })
})
