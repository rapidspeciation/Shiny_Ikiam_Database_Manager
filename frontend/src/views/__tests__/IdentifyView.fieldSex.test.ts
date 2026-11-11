import { afterEach, describe, expect, it, vi } from 'vitest'
import { createApp, nextTick } from 'vue'
import { createPinia, setActivePinia } from 'pinia'
import { createMemoryHistory, createRouter } from 'vue-router'
import { locale, t, tn } from '../../lib/i18n'
import { SEX_FILE } from '../../lib/sexModel'
import { useSession } from '../../stores/session'

// Identificar with a server result restored from this browser session (the page's own restore path), then a new photo
// read through a stand-in server: the in-hand line for an Ithomiini; the general sex line (a bar) for a Monarch,
// whatever its region, with no question asked; kept through a signal drop, gone once the person picks «En este
// teléfono» (the phone has the small model here, so the result stays on screen). The phone's models and the server
// are stand-ins. No AI.

vi.mock('../../lib/offlineEngine', async orig => {
  const real = await orig<typeof import('../../lib/offlineEngine')>()
  const engine = {
    list: async () => ({ files: [{ name: SEX_FILE.name, role: 'sex', bytes: SEX_FILE.bytes, sha256: SEX_FILE.sha256, parts: SEX_FILE.parts }] }),
    ready: async () => ({ small: true, full: false }),
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

const server = vi.hoisted(() => ({ answers: [] as unknown[] }))
vi.mock('../../lib/identifyOnline', async orig => {
  const real = await orig<typeof import('../../lib/identifyOnline')>()
  return {
    ...real,
    identifyOnServer: async () => server.answers.shift(),
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

const block = (o: Record<string, unknown> = {}) => ({ species: 'Danaus plexippus', status: 'call', p: 0.03, sex: 'male', sure: true, pct: 96, version: 'field-sex/1', ...o })
const callText = (host: Element) => host.querySelector('[data-testid=field-sex-call]')?.textContent!.replace(/\s+/g, ' ').trim()

describe('IdentifyView: the two sex lines', () => {
  it('an Ithomiini keeps the in-hand line, even if a general block came with it', async () => {
    const { host } = await mountWith(stored(answer('Mechanitis messenoides', 0.93, block({ species: 'Mechanitis messenoides' }))))
    expect(host.querySelector('[data-testid=sex-estimate]')).not.toBeNull()
    expect(host.querySelector('[data-testid=sex-estimate]')!.textContent).toContain('experimental: fotos en mano')
    expect(host.querySelector('[data-testid=field-sex]')).toBeNull()
  })

  it('a Monarch: the answer and its % at once, no question; «En este teléfono» hides the line', async () => {
    const { host, settle } = await mountWith(stored(answer('Danaus plexippus', 0.97, block())))
    expect(host.querySelector('[data-testid=sex-estimate]')).toBeNull()
    expect(callText(host)).toBe('♂ Macho 96 %')
    expect(host.querySelector('[data-testid=field-sex-tested]')!.textContent).toContain('Probado en fotos de mariposas vivas.')
    expect(host.querySelector('[data-testid=field-sex] button')).toBeNull()
    const phone = [...host.querySelectorAll<HTMLButtonElement>('[data-testid=mode] button')].find(b => b.textContent!.includes('En este teléfono'))!
    phone.click()
    await settle()
    // the result is still on screen (the phone has a model), only the general line is gone
    expect(host.querySelector('[data-testid=result-source]')).not.toBeNull()
    expect(host.querySelector('[data-testid=field-sex]')).toBeNull()
  })

  it('a Monarch the identifier is not sure of (headline not a species): the bar, the note, no %', async () => {
    const { host } = await mountWith(stored(answer('Danaus plexippus', 0.55, block({ status: 'species_unsure', pct: undefined }))))
    expect(host.querySelector('[data-testid=field-sex]')).not.toBeNull()
    expect(callText(host)).toBe('♂ Macho')
    expect(host.querySelector('[data-testid=field-sex-note]')!.textContent).toContain('La especie no es segura; el sexo puede estar mal.')
    expect(host.querySelector('[data-testid=field-sex-p]')).toBeNull()
  })

  it('a Monarch in the band: «No seguro» and the side, no %', async () => {
    const { host } = await mountWith(stored(answer('Danaus plexippus', 0.97, block({ status: 'band', p: 0.7, sex: 'female', sure: false, pct: undefined }))))
    expect(callText(host)).toBe('No seguro (se inclina a hembra)')
    expect(host.querySelector('[data-testid=field-sex-p]')).toBeNull()
  })

  it('another species outside the Ithomiini: nothing', async () => {
    const { host } = await mountWith(stored(answer('Danaus gilippus', 0.97, null)))
    expect(host.querySelector('[data-testid=field-sex]')).toBeNull()
    expect(host.querySelector('[data-testid=sex-estimate]')).toBeNull()
  })

  it('a signal drop does not hide a shown answer (the person did not pick «En este teléfono»)', async () => {
    const { host, settle } = await mountWith(stored(answer('Danaus plexippus', 0.97, block({ p: 0.97, sex: 'female', pct: 97 }))))
    dispatchEvent(new Event('offline'))
    await settle()
    const phone = [...host.querySelectorAll<HTMLButtonElement>('[data-testid=mode] button')].find(b => b.textContent!.includes('En este teléfono'))!
    expect(phone.getAttribute('aria-checked')).toBe('true') // the page now reads on the phone ...
    expect(host.querySelector('[data-testid=result-source]')).not.toBeNull()
    expect(callText(host)).toBe('♀ Hembra 97 %') // ... the answer stays
    dispatchEvent(new Event('online'))
  })

  it('a new photo shows its own answer', async () => {
    const { host, settle } = await mountWith(stored(answer('Danaus plexippus', 0.97, block())))
    expect(callText(host)).toBe('♂ Macho 96 %')
    server.answers.push(answer('Danaus plexippus', 0.96, block({ status: 'band', p: 0.3, sex: 'male', sure: false, pct: undefined })))
    host.querySelector<HTMLInputElement>('input[type=file]:not([capture])')!.dispatchEvent(new Event('change'))
    await settle()
    expect(server.answers).toHaveLength(0)
    expect(callText(host)).toBe('No seguro (se inclina a macho)')
  })
})
