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

async function mountWith(stored: Record<string, unknown>, { signedOut = false } = {}) {
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
  session.user = signedOut ? null : ({ id: 7, username: 'lab', displayName: 'Lab', role: 'admin' } as never)
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
    const opener = host.querySelector<HTMLButtonElement>('[data-testid=zoom-mine]')!
    opener.focus()
    opener.click()
    await settle()
    expect(document.querySelector('[data-testid=photo-viewer]')).not.toBeNull()
    // the page behind is inert; Tab goes round the viewer's own controls
    expect(host.closest('[inert]')).not.toBeNull()
    expect(document.activeElement?.getAttribute('data-testid')).toBe('viewer-close')
    for (let i = 0; i < 6; i++) dispatchEvent(new KeyboardEvent('keydown', { key: 'Tab' }))
    expect(document.querySelector('[data-testid=photo-viewer]')!.contains(document.activeElement)).toBe(true)
    expect(document.querySelector('[data-testid=viewer-name]')!.textContent).toContain('Su foto')
    expect(document.body.style.overflow).toBe('hidden')
    dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape' }))
    await settle()
    for (let i = 0; i < 5 && document.querySelector('[data-testid=photo-viewer]'); i++) {
      dispatchEvent(new PopStateEvent('popstate'))
      await settle()
    }
    expect(document.querySelector('[data-testid=photo-viewer]')).toBeNull()
    expect(host.closest('[inert]')).toBeNull()
    expect(document.activeElement).toBe(opener)
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

  it('«Borrar fotos» while chosen photos are still being read: none of them is identified', async () => {
    localStorage.setItem('identify:log', JSON.stringify([{ at: restored.at, thumb: 'data:image/jpeg;base64,AA==', ai: 'Mechanitis polymnia', p: 0.6, pick: null, side: null, saved: true }]))
    sessionStorage.setItem('identify:shots', JSON.stringify([restored.at]))
    const { host, settle } = await mountWith(restored)
    let calls = 0
    server.onCall = () => void calls++
    // two photos whose bytes come late (a slow phone decoding large files)
    const reads: ((b: ArrayBuffer) => void)[] = []
    const late = (name: string) => ({ name, type: 'image/jpeg', arrayBuffer: () => new Promise<ArrayBuffer>(ok => reads.push(ok)) })
    const input = host.querySelector<HTMLInputElement>('input[type=file][multiple]')!
    Object.defineProperty(input, 'files', { configurable: true, value: [late('a.jpg'), late('b.jpg')] })
    input.dispatchEvent(new Event('change'))
    await settle()
    host.querySelector<HTMLButtonElement>('[data-testid=clear-photos]')!.click()
    await settle()
    reads.forEach(ok => ok(new ArrayBuffer(4)))
    await settle()
    reads.forEach(ok => ok(new ArrayBuffer(4)))
    await settle()
    server.onCall = null
    expect(calls).toBe(0)
    expect(host.querySelector('[data-testid=headline]')).toBeNull()
    expect(host.querySelector('[data-testid=photo-start]')).not.toBeNull()
  })

  it('a history item read by the server, tapped signed out: no viewer, the reason by the item, nothing sent', async () => {
    const at = '2026-11-12T10:00:00.000Z'
    const { historyStore } = await import('../../lib/identifyHistory')
    await (await historyStore()).put({ at, photo: new Blob(['x'], { type: 'image/jpeg' }), result: JSON.stringify({ ...restored, at }), compared: null, pick: null, takenAt: null, country: null, versions: { id: null, sex: null } })
    localStorage.setItem('identify:log', JSON.stringify([{ at, thumb: 'data:image/jpeg;base64,AA==', ai: 'Mechanitis polymnia', p: 0.6, pick: null, side: null, saved: true }]))
    let calls = 0
    server.onCall = () => void calls++
    const { host, settle } = await mountWith({}, { signedOut: true })
    host.querySelector<HTMLButtonElement>('[data-testid=history-load]')!.click()
    for (let i = 0; i < 3; i++) await settle()
    server.onCall = null
    expect(document.querySelector('[data-testid=photo-viewer]')).toBeNull()
    expect(host.querySelector('[data-testid=history-reason]')!.textContent).toContain('Inicie sesión para volver a analizarla.')
    expect(calls).toBe(0)
    expect(host.querySelector('[data-testid=headline]')).toBeNull()
  })

  it('the Ithomiini badge comes before the kicker sentence, never after a colon', async () => {
    const { host } = await mountWith(restored)
    const kicker = host.querySelector('[data-testid=ithomiini-badge]')!.parentElement!
    expect(kicker.firstElementChild!.getAttribute('data-testid')).toBe('ithomiini-badge')
    expect(kicker.textContent!.trim()).not.toMatch(/:\s*Ithomiini$/)
  })
})

// The answer's wording (15 Nov 2026): the guess always named, as the Wings Gallery (the species and its genus, each
// «Incierto» with its score below the engine's cut), the level that is clear beside it, the tribe and subfamily rung
// always on (no «para pruebas» line), and the guess's tribe and subfamily as one small label (the Ithomiini badge when
// the tribe is Ithomiini). The tribe names come from the head's taxonomy file.
describe('IdentifyView: the answer', () => {
  const answer = (server: Record<string, unknown>) => ({ ...restored, server: { ...three, ...server } })
  // The words shown (an icon's own label left out).
  const text = (host: HTMLElement, id: string) => {
    const el = host.querySelector(`[data-testid=${id}]`)?.cloneNode(true) as HTMLElement | undefined
    el?.querySelectorAll('svg').forEach(s => s.remove())
    return el?.textContent?.replace(/\s+/g, ' ').trim() ?? null
  }
  const rows = (host: HTMLElement) =>
    [...host.querySelectorAll('[data-testid=rungs] li')].map(li => [
      [...li.children].map(c => c.textContent!.trim()).filter(Boolean).join(' '),
      li.getAttribute('data-sure'),
    ])

  it('species clear: the species, its score, the tribe · subfamily as the Ithomiini badge; nothing uncertain; no testing line', async () => {
    const { host } = await mountWith(answer({ species: [['Mechanitis polymnia', 0.9], ['Mechanitis lysimnia', 0.05]], genus: [['Mechanitis', 0.95]], tribe: [['Ithomiini', 0.97]], subfamily: [['Danainae', 0.98]] }))
    expect(text(host, 'headline')).toBe('Mechanitis polymnia')
    expect(text(host, 'headline-p')).toBe('90 %')
    expect(host.querySelector('[data-testid=species-unsure]')).toBeNull()
    expect(host.querySelector('[data-testid=rungs]')).toBeNull()
    expect(text(host, 'ithomiini-badge')).toBe('Ithomiini · Danainae')
    expect(host.querySelector('[data-testid=ithomiini-badge]')!.getAttribute('title')).toBe('Tribu Ithomiini · Subfamilia Danainae')
    expect(host.querySelectorAll('[data-testid=ithomiini-badge], [data-testid=ranks-above]').length).toBe(1)
    expect(host.querySelector('[data-testid=tribe-rung-on]')).toBeNull()
    expect(host.textContent).not.toMatch(/para pruebas|for testing|Lo más probable/)
  })

  it('species uncertain, genus clear: the species named «Incierto» with its score, the genus as the clear level', async () => {
    const { host } = await mountWith(restored)
    expect(text(host, 'headline')).toBe('Mechanitis polymnia')
    expect(text(host, 'headline-p')).toBe('60 %')
    expect(text(host, 'species-unsure')).toBe('Incierto')
    expect(rows(host)).toEqual([['Género Mechanitis 80 %', 'true']])
    // The genus is clear: the green badge.
    expect(text(host, 'ithomiini-badge')).toBe('Ithomiini · Danainae')
    expect(host.querySelector('[data-testid=likeliest]')).toBeNull()
  })

  it('both uncertain, tribe clear: the top species stays the guess, its genus «Incierto», the tribe said of the photo, no label', async () => {
    const { host, settle } = await mountWith(
      answer({
        species: [['Heliconius erato', 0.12], ['Mechanitis polymnia', 0.1], ['Oleria onega', 0.08]],
        genus: [['Mechanitis', 0.3], ['Oleria', 0.25], ['Heliconius', 0.12]],
        tribe: [['Ithomiini', 0.86], ['Heliconiini', 0.12]],
        subfamily: [['Danainae', 0.87]],
      }),
    )
    // Heliconius erato is the top species, outside the clear tribe: still the guess (Franz: always the top species).
    await settle()
    expect(text(host, 'headline')).toBe('Heliconius erato')
    expect(text(host, 'headline-p')).toBe('12 %')
    expect(text(host, 'species-unsure')).toBe('Incierto')
    expect(rows(host)).toEqual([
      ['Género Heliconius 12 % Incierto', 'false'],
      ['La foto parece de la tribu Ithomiini 86 %', 'true'],
    ])
    expect(host.querySelector('[data-testid=rung-tribe] svg')!.getAttribute('aria-label')).toBe('Probable')
    // The tribe row names Ithomiini: no label saying it again.
    expect(host.querySelector('[data-testid=ranks-above], [data-testid=ithomiini-badge], [data-testid=not-ithomiini]')).toBeNull()
  })

  it('subfamily clear, the second genus small (no pair): its row, no label', async () => {
    const { host } = await mountWith(
      answer({
        species: [['Mechanitis polymnia', 0.3], ['Danaus plexippus', 0.1]],
        genus: [['Mechanitis', 0.35], ['Danaus', 0.1]],
        tribe: [['Ithomiini', 0.5], ['Danaini', 0.35]],
        subfamily: [['Danainae', 0.9]],
      }),
    )
    expect(text(host, 'headline')).toBe('Mechanitis polymnia')
    expect(rows(host)).toEqual([
      ['Género Mechanitis 35 % Incierto', 'false'],
      ['La foto parece de la subfamilia Danainae 90 %', 'true'],
    ])
    expect(host.querySelector('[data-testid=ranks-above], [data-testid=ithomiini-badge], [data-testid=not-ithomiini]')).toBeNull()
  })

  it('nothing clear, one genus (the second small): the species and the genus both «Incierto» with their scores, nothing shown as certain', async () => {
    const { host } = await mountWith(answer({ species: [['Mechanitis polymnia', 0.4], ['Oleria onega', 0.1]], genus: [['Mechanitis', 0.45], ['Oleria', 0.1]], tribe: [['Ithomiini', 0.75]], subfamily: [['Danainae', 0.78]] }))
    expect(text(host, 'headline')).toBe('Mechanitis polymnia')
    expect(text(host, 'headline-p')).toBe('40 %')
    expect(text(host, 'species-unsure')).toBe('Incierto')
    expect(rows(host)).toEqual([['Género Mechanitis 45 % Incierto', 'false']])
    expect(host.textContent).not.toContain('No estamos seguros')
    // Nothing clear: the ranks as a grey label, never the green badge.
    expect(text(host, 'ranks-above')).toBe('Ithomiini · Danainae')
    expect(host.querySelector('[data-testid=ithomiini-badge]')).toBeNull()
  })

  it('nothing clear on a non-Ithomiini: a grey label and no «No parece una Ithomiini.»', async () => {
    const { host } = await mountWith(answer({ species: [['Heliconius erato', 0.4], ['Oleria onega', 0.1]], genus: [['Heliconius', 0.45], ['Oleria', 0.1]], tribe: [['Heliconiini', 0.5]], subfamily: [] }))
    expect(text(host, 'ranks-above')).toBe('Heliconiini')
    expect(host.querySelector('[data-testid=not-ithomiini]')).toBeNull()
  })

  it('two genera of different tribes, nothing clear: the pair, no label, no «No parece una Ithomiini.»', async () => {
    const { host } = await mountWith(answer({ species: [['Heliconius erato', 0.4], ['Oleria onega', 0.3]], genus: [['Heliconius', 0.45], ['Oleria', 0.3]], tribe: [['Heliconiini', 0.5]], subfamily: [] }))
    expect(host.querySelector('[data-testid=headline]')!.getAttribute('data-pair')).toBe('Heliconius|Oleria')
    expect(host.querySelector('[data-testid=ranks-above], [data-testid=ithomiini-badge], [data-testid=not-ithomiini]')).toBeNull()
  })

  // The genus not clear (17 Nov 2026): two genera, each with its best species and its total (the server: BioCLIP cuts,
  // the two totals summing to at least 0.70, the second at least 0.15). The screenshot's photo (9981 lab, 16 Nov 2026).
  const twoGenera = {
    species: [['Mechanitis messenoides', 0.4], ['Melinaea isocomma', 0.37], ['Melinaea menophilus', 0.05], ['Mechanitis polymnia', 0.04]],
    genus: [['Mechanitis', 0.47], ['Melinaea', 0.43], ['Oleria', 0.02]],
    tribe: [['Ithomiini', 0.96]],
    subfamily: [['Danainae', 0.97]],
  }

  it('genus not clear: «X sp o Y sp» with each genus\'s total, «Incierto», the tribe as the small label, no rows', async () => {
    const { host } = await mountWith(answer(twoGenera))
    // each number said to be the genus's total
    expect(text(host, 'pair-0')).toBe('Mechanitis messenoides género 47 %')
    expect(text(host, 'pair-1')).toBe('o Melinaea isocomma género 43 %')
    const h = host.querySelector('[data-testid=headline]')!
    expect(h.getAttribute('data-pair')).toBe('Mechanitis|Melinaea')
    expect(h.getAttribute('data-top')).toBe('Mechanitis messenoides')
    expect([...h.querySelectorAll('i')].map(i => i.textContent)).toEqual(['Mechanitis messenoides', 'Melinaea isocomma'])
    expect(text(host, 'species-unsure')).toBe('Incierto')
    expect(text(host, 'pair-note')).toBe('Incierto Cada porcentaje es el de todo el género (la suma de sus especies).')
    expect(host.querySelector('[data-testid=headline-p]')).toBeNull()
    expect(host.querySelector('[data-testid=rungs]')).toBeNull()
    // the clear tribe, shared by both species: the green label, said once
    expect(text(host, 'ithomiini-badge')).toBe('Ithomiini · Danainae')
    expect(host.querySelectorAll('[data-testid=ithomiini-badge], [data-testid=ranks-above], [data-testid=not-ithomiini]').length).toBe(1)
    // the strip stays, the genus written out where two genera share the initial
    expect([...host.querySelectorAll('[data-testid=candidate-name]')].map(e => e.textContent!.trim())).toEqual([
      'Mechanitis messenoides',
      'Melinaea isocomma',
      'Melinaea menophilus',
      'Mechanitis polymnia',
    ])
  })

  it('genus not clear, nothing above clear: the pair with a grey label', async () => {
    const { host } = await mountWith(answer({ ...twoGenera, tribe: [['Ithomiini', 0.7]], subfamily: [['Danainae', 0.75]] }))
    expect(host.querySelector('[data-testid=headline]')!.getAttribute('data-pair')).toBe('Mechanitis|Melinaea')
    expect(text(host, 'ranks-above')).toBe('Ithomiini · Danainae')
    expect(host.querySelector('[data-testid=ithomiini-badge]')).toBeNull()
  })

  it('genus not clear, the two totals summing under the cut: the top species and its rows as before', async () => {
    const { host } = await mountWith(
      answer({ ...twoGenera, species: [['Mechanitis messenoides', 0.1], ['Oleria onega', 0.08]], genus: [['Mechanitis', 0.11], ['Oleria', 0.08]] }),
    )
    expect(text(host, 'headline')).toBe('Mechanitis messenoides')
    expect(host.querySelector('[data-testid=headline]')!.getAttribute('data-pair')).toBeNull()
    expect(rows(host)).toEqual([
      ['Género Mechanitis 11 % Incierto', 'false'],
      ['La foto parece de la tribu Ithomiini 96 %', 'true'],
    ])
  })

  it('two genera of different tribes under a clear tribe: no shared label; the tribe as its own row about the photo', async () => {
    const { host } = await mountWith(
      answer({
        species: [['Mechanitis messenoides', 0.4], ['Heliconius erato', 0.25], ['Oleria onega', 0.1]],
        genus: [['Mechanitis', 0.5], ['Heliconius', 0.25], ['Oleria', 0.15]],
        tribe: [['Ithomiini', 0.85]],
        subfamily: [],
      }),
    )
    expect(host.querySelector('[data-testid=headline]')!.getAttribute('data-pair')).toBe('Mechanitis|Heliconius')
    expect(host.querySelector('[data-testid=ranks-above], [data-testid=ithomiini-badge], [data-testid=not-ithomiini]')).toBeNull()
    expect(rows(host)).toEqual([['La foto parece de la tribu Ithomiini 85 %', 'true']])
  })

  it('under two genera the subspecies line names its species in full', async () => {
    const { host } = await mountWith(answer({ ...twoGenera, subspecies: [['Mechanitis messenoides deceptus', 0.388]] }))
    expect(text(host, 'ssp-hint')).toBe('Si fuera Mechanitis messenoides, la subespecie más parecida: deceptus (97 % de esa especie)')
  })

  it('BioCLIP, the two totals summing under 0.70: the top species and its rows as before', async () => {
    const { host } = await mountWith(
      answer({ ...twoGenera, species: [['Mechanitis messenoides', 0.3], ['Melinaea isocomma', 0.2]], genus: [['Mechanitis', 0.4], ['Melinaea', 0.25]] }),
    )
    expect(host.querySelector('[data-testid=headline]')!.getAttribute('data-pair')).toBeNull()
    expect(rows(host)).toEqual([
      ['Género Mechanitis 40 % Incierto', 'false'],
      ['La foto parece de la tribu Ithomiini 96 %', 'true'],
    ])
  })

  it('genus clear or species clear: no pair', async () => {
    const { host } = await mountWith(restored)
    expect(host.querySelector('[data-testid=headline]')!.getAttribute('data-pair')).toBeNull()
    expect(text(host, 'headline')).toBe('Mechanitis polymnia')
  })

  it('in English: «or» between the two', async () => {
    const { host, settle } = await mountWith(answer(twoGenera))
    locale.value = 'en'
    await settle()
    expect(text(host, 'pair-0')).toBe('Mechanitis messenoides genus 47 %')
    expect(text(host, 'pair-1')).toBe('or Melinaea isocomma genus 43 %')
    expect(text(host, 'pair-note')).toBe('Uncertain Each percentage is for the whole genus (the sum of its species).')
  })

  it('«Es esta» on the headline species is sent confirmed, also when only the genus is clear', async () => {
    const { host, settle } = await mountWith(restored)
    host.querySelector<HTMLButtonElement>('[data-testid=pick-this]')!.click()
    await settle()
    // The label handed to the sex panel (what it sends with the sex), read from the panel's own props.
    const panel = host.querySelector('section.border-l-4') as HTMLElement & { __vueParentComponent?: { props: Record<string, unknown> } }
    expect(panel.__vueParentComponent!.props.species).toMatchObject({ name: 'Mechanitis polymnia', confirmed: true })
  })

  it('an uncertain species: a subspecies only as «if it is this species»', async () => {
    const { host } = await mountWith(answer({ subspecies: [['Mechanitis polymnia casabranca', 0.4]] }))
    expect(text(host, 'ssp-hint')).toMatch(/^Si fuera esta especie, la subespecie más parecida: M\. polymnia casabranca \(67 % de esa especie\)/)
  })

  it('a species with a subfamily and no tribe: «Subfamilia …» as the label', async () => {
    const { host } = await mountWith(answer({ species: [['Dismorphia theucharila', 0.91]], genus: [['Dismorphia', 0.95]], tribe: [], subfamily: [['Dismorphiinae', 0.97]] }))
    expect(text(host, 'ranks-above')).toBe('Subfamilia Dismorphiinae')
    expect(text(host, 'not-ithomiini')).toBe('No parece una Ithomiini.')
  })

  it('a species outside Ithomiini: its tribe as a plain label, then «No parece una Ithomiini.»', async () => {
    const { host } = await mountWith(answer({ species: [['Heliconius erato', 0.92]], genus: [['Heliconius', 0.95]], tribe: [['Heliconiini', 0.96]], subfamily: [['Heliconiinae', 0.97]] }))
    // The file leaves out a subfamily it does not trust: the tribe alone.
    expect(text(host, 'ranks-above')).toBe('Heliconiini')
    expect(text(host, 'not-ithomiini')).toBe('No parece una Ithomiini.')
    expect(host.querySelector('[data-testid=ithomiini-badge]')).toBeNull()
  })
})
