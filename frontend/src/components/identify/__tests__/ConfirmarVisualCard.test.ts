import { afterEach, describe, expect, it, vi } from 'vitest'
import { createApp, h, nextTick, ref } from 'vue'
import ConfirmarVisualCard from '../ConfirmarVisualCard.vue'
import { locale, t, tn } from '../../../lib/i18n'
import type { ConfirmarCardMatch } from '../../../lib/confirmarVisualRouter'

let unmount = () => {}
afterEach(() => {
  unmount()
  document.body.innerHTML = ''
  locale.value = 'en'
})

function mountCard(match: ConfirmarCardMatch, onSelect?: (id: string) => void) {
  const host = document.createElement('div')
  document.body.append(host)
  const app = createApp(ConfirmarVisualCard, {
    match,
    userPhotoSrc: 'data:image/jpeg;base64,AA==',
    onSelectCard: onSelect,
  })
  app.config.globalProperties.$t = t
  app.config.globalProperties.$tn = tn
  app.mount(host)
  unmount = () => app.unmount()
  return host
}

const activeMatch = (over: Partial<ConfirmarCardMatch> = {}): ConfirmarCardMatch => ({
  pairId: 'p01',
  cardId: 'p01_west',
  status: 'active',
  displayKind: 'card',
  regionFallback: true,
  guideSide: 'West',
  canSwitchRegion: true,
  imageUrl: 'https://example.test/identificar/confirmar/cards/es/p01_west.webp?v=1234567890',
  imageWidth: 1400,
  imageHeight: 1539,
  regionBadge: 'Occidente',
  regionDetail: 'Occidente (Chocó y estribaciones occidentales)',
  viewBadge: 'Anverso y reverso',
  taxonA: 'Heliconius erato cyrbia',
  taxonB: 'Heliconius melpomene cythera',
  altText: 'Guía visual de campo para comparar Heliconius erato cyrbia y Heliconius melpomene cythera (Occidente)',
  honestNote: null,
  alternateCards: [
    { cardId: 'p01_west', regionBadge: 'Occidente', regionDetail: null, sideAndes: ['West'] },
    { cardId: 'p01_east_napo', regionBadge: 'Oriente · Napo y Amazonía', regionDetail: null, sideAndes: ['East'] },
  ],
  ...over,
})

describe('ConfirmarVisualCard', () => {
  it('renders active card, region switcher pills (>=44px tap targets, aria-pressed), and opens/closes zoomable PhotoViewer lightbox', async () => {
    locale.value = 'es'
    const selected: string[] = []
    const host = mountCard(activeMatch(), id => selected.push(id))

    expect(host.querySelector('[data-testid="confirmar-visual-card"]')).not.toBeNull()
    expect(host.querySelector('[data-testid="confirmar-region-badge"]')).toBeNull()
    expect(host.querySelector('[data-testid="confirmar-view-badge"]')!.textContent?.trim()).toBe('Anverso y reverso')
    expect(host.querySelector('[data-testid="confirmar-pair-title"]')!.textContent).toContain('Heliconius erato cyrbia')

    const westBtn = host.querySelector<HTMLButtonElement>('[data-testid="confirmar-region-opt-p01_west"]')!
    const eastBtn = host.querySelector<HTMLButtonElement>('[data-testid="confirmar-region-opt-p01_east_napo"]')!
    expect(westBtn.getAttribute('aria-pressed')).toBe('true')
    expect(eastBtn.getAttribute('aria-pressed')).toBe('false')
    expect(eastBtn.className).toContain('min-h-11')
    expect(eastBtn.className).toContain('border-control')
    eastBtn.click()
    expect(selected).toEqual(['p01_east_napo'])

    const zoomBtn = host.querySelector<HTMLButtonElement>('[data-testid="confirmar-zoom-trigger"]')!
    expect(zoomBtn.className).toContain('min-h-11')
    zoomBtn.click()
    await nextTick()

    const viewer = document.body.querySelector('[data-testid="photo-viewer"]')
    expect(viewer).not.toBeNull()
    document.body.querySelector<HTMLButtonElement>('[data-testid="viewer-close"]')!.click()
    await nextTick()
    expect(document.body.querySelector('[data-testid="photo-viewer"]')).toBeNull()
  })

  it('hides the region switcher and shows the region badge when region is locked by side or subspecies', () => {
    locale.value = 'es'
    const host = mountCard(activeMatch({ canSwitchRegion: false, regionFallback: false }))
    expect(host.querySelector('[data-testid="confirmar-region-switcher"]')).toBeNull()
    expect(host.querySelector('[data-testid="confirmar-region-badge"]')!.textContent?.trim()).toBe('Occidente')
  })

  it('renders crisp honest note for inseparable/deferred pairs with zero internal machinery', () => {
    locale.value = 'en'
    const host = mountCard({
      ...activeMatch(),
      cardId: null,
      status: 'not_reliably_separable_in_photos',
      displayKind: 'honest_note',
      canSwitchRegion: false,
      imageUrl: null,
      regionBadge: 'East & West Ecuador',
      taxonA: 'Gorgythion begga',
      taxonB: 'Gorgythion beggina',
      honestNote: "These two look almost identical in field photos; a photo alone can't tell them apart.",
      alternateCards: [],
    })

    expect(host.querySelector('[data-testid="confirmar-visual-card"]')).toBeNull()
    const note = host.querySelector('[data-testid="confirmar-honest-note"]')
    expect(note).not.toBeNull()
    expect(note!.textContent).toContain('Gorgythion begga')
    expect(note!.textContent).toContain('almost identical')
    expect(note!.textContent).not.toContain('not_reliably_separable')
  })

  it('names the pair one taxon per line: the species strongest, its subspecies lighter', () => {
    locale.value = 'es'
    const host = mountCard(activeMatch())
    const title = host.querySelector('[data-testid="confirmar-pair-title"]')!
    const lines = [...title.querySelectorAll(':scope > span')]
    expect(lines).toHaveLength(2)
    expect(lines[0].textContent!.trim()).toBe('Heliconius erato cyrbia')
    expect(lines[1].textContent!.replace(/\s+/g, ' ').trim()).toBe('vs. Heliconius melpomene cythera')
    expect([...title.querySelectorAll('.species')].map(e => e.textContent)).toEqual(['Heliconius erato', 'Heliconius melpomene'])
    expect([...title.querySelectorAll('.form')].map(e => e.textContent)).toEqual(['cyrbia', 'cythera'])
    expect(title.classList.contains('by-form')).toBe(false)
  })

  it('two forms of one species: the subspecies is what differs, so it reads strongest; a species pair has no empty form', () => {
    const forms = mountCard(activeMatch({ taxonA: 'Hypothyris euclea intermedia', taxonB: 'Hypothyris euclea pyrippe' }))
    expect(forms.querySelector('[data-testid="confirmar-pair-title"]')!.classList.contains('by-form')).toBe(true)
    unmount()
    const species = mountCard(activeMatch({ taxonA: 'Mechanitis menapis', taxonB: 'Mechanitis polymnia' }))
    const title = species.querySelector('[data-testid="confirmar-pair-title"]')!
    expect(title.classList.contains('by-form')).toBe(false)
    expect(title.querySelector('.form')).toBeNull()
    expect(title.textContent!.replace(/\s+/g, ' ').trim()).toBe('Mechanitis menapis vs. Mechanitis polymnia')
  })

  it('says what a tap does under the plate, never over its text, in both languages', () => {
    locale.value = 'es'
    const es = mountCard(activeMatch())
    const trigger = es.querySelector('[data-testid="confirmar-zoom-trigger"]')!
    const plate = trigger.querySelector('.confirmar-plate')!
    const hint = trigger.querySelector('[data-testid="confirmar-zoom-hint"]')!
    // One target: the plate and its «Ampliar» row; the row is after the plate, not inside it.
    expect(plate.contains(hint)).toBe(false)
    expect(plate.compareDocumentPosition(hint) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy()
    expect(hint.textContent!.trim()).toBe('Ampliar la guía')
    expect(hint.className).toContain('min-h-11')
    expect(plate.querySelector('.absolute.right-2')).toBeNull()
    // A computer has the action beside the badges instead (the card no taller than before): each width shows one.
    expect(hint.className).toContain('sm:hidden')
    const button = es.querySelector<HTMLButtonElement>('[data-testid="confirmar-zoom-button"]')!
    expect(button.className).toContain('max-sm:hidden')
    expect(button.textContent!.trim()).toBe('Ampliar la guía')
    expect(plate.contains(button)).toBe(false)
    unmount()
    locale.value = 'en'
    const en = mountCard(activeMatch())
    expect(en.querySelector('[data-testid="confirmar-zoom-hint"]')!.textContent!.trim()).toBe('Enlarge the guide')
  })

  it('a computer keeps the pair on one line and opens the viewer from the button beside the badges', async () => {
    locale.value = 'es'
    const host = mountCard(activeMatch())
    const lines = [...host.querySelectorAll('[data-testid="confirmar-pair-title"] > span')]
    // One taxon per line only below the phone width.
    expect(lines.map(l => l.className)).toEqual(['max-sm:block', 'max-sm:block'])
    expect(host.querySelector('[data-testid="confirmar-pair-title"]')!.textContent!.replace(/\s+/g, ' ').trim()).toBe(
      'Heliconius erato cyrbia vs. Heliconius melpomene cythera',
    )
    // One keyboard stop for the zoom: the button; the plate answers the mouse.
    expect(host.querySelector('[data-testid="confirmar-zoom-trigger"]')!.getAttribute('tabindex')).toBe('-1')
    expect(host.querySelector('[data-testid="confirmar-zoom-button"]')!.hasAttribute('tabindex')).toBe(false)
    host.querySelector<HTMLButtonElement>('[data-testid="confirmar-zoom-button"]')!.click()
    await nextTick()
    expect(document.body.querySelector('[data-testid="photo-viewer"]')).not.toBeNull()
    document.body.querySelector<HTMLButtonElement>('[data-testid="viewer-close"]')!.click()
    await nextTick()
  })

  it('a phone has the plate itself as the one stop (the button beside the badges is not shown there)', () => {
    vi.stubGlobal('matchMedia', (query: string) => ({ matches: /max-width: 639/.test(query), addEventListener() {}, removeEventListener() {} }))
    const host = mountCard(activeMatch())
    expect(host.querySelector('[data-testid="confirmar-zoom-trigger"]')!.hasAttribute('tabindex')).toBe(false)
    vi.unstubAllGlobals()
  })

  it('holds the plate\'s shape and says it is loading until its file arrives (no dark box, no jump)', async () => {
    locale.value = 'es'
    const host = mountCard(activeMatch())
    const plate = host.querySelector<HTMLElement>('.confirmar-plate')!
    expect(plate.style.aspectRatio.replace(/\s/g, '')).toBe('1400/1539')
    expect(plate.className).not.toContain('bg-photo-mat')
    expect(host.querySelector('[data-testid="confirmar-img-loading"]')!.textContent!.trim()).toBe('Cargando guía…')
    const img = host.querySelector<HTMLImageElement>('[data-testid="confirmar-card-image"]')!
    expect(img.getAttribute('loading')).toBe('eager')
    expect(img.classList.contains('opacity-0')).toBe(true)
    img.dispatchEvent(new Event('load'))
    await nextTick()
    expect(host.querySelector('[data-testid="confirmar-img-loading"]')).toBeNull()
    expect(img.classList.contains('opacity-0')).toBe(false)
    // A file that does not arrive: said in words, and nothing to open.
    img.dispatchEvent(new Event('error'))
    await nextTick()
    expect(host.querySelector('[data-testid="confirmar-img-error"]')!.textContent).toContain('No se pudo cargar')
    expect(host.querySelector('[data-testid="confirmar-zoom-trigger"]')).toBeNull()
  })

  it('reveals a cached plate that is already complete on mount and after switching imageUrl', async () => {
    const completeDesc = Object.getOwnPropertyDescriptor(HTMLImageElement.prototype, 'complete')
    const widthDesc = Object.getOwnPropertyDescriptor(HTMLImageElement.prototype, 'naturalWidth')
    Object.defineProperty(HTMLImageElement.prototype, 'complete', { configurable: true, get: () => true })
    Object.defineProperty(HTMLImageElement.prototype, 'naturalWidth', { configurable: true, get: () => 1400 })
    try {
      const current = ref(activeMatch())
      const host = document.createElement('div')
      document.body.append(host)
      const app = createApp({ render: () => h(ConfirmarVisualCard, { match: current.value }) })
      app.config.globalProperties.$t = t
      app.config.globalProperties.$tn = tn
      app.mount(host)
      unmount = () => app.unmount()
      await nextTick()
      await nextTick()
      expect(host.querySelector('[data-testid="confirmar-img-loading"]')).toBeNull()
      expect(host.querySelector('[data-testid="confirmar-card-image"]')!.classList.contains('opacity-0')).toBe(false)

      // Switching region/language changes imageUrl; a cached second plate must also reveal without a fresh load event.
      current.value = activeMatch({
        cardId: 'p01_east_napo',
        imageUrl: 'https://example.test/identificar/confirmar/cards/es/p01_east_napo.webp?v=1234567890',
      })
      await nextTick()
      await nextTick()
      expect(host.querySelector('[data-testid="confirmar-img-loading"]')).toBeNull()
      expect(host.querySelector('[data-testid="confirmar-card-image"]')!.classList.contains('opacity-0')).toBe(false)
    } finally {
      if (completeDesc) Object.defineProperty(HTMLImageElement.prototype, 'complete', completeDesc)
      if (widthDesc) Object.defineProperty(HTMLImageElement.prototype, 'naturalWidth', widthDesc)
    }
  })

  it('the full-screen viewer gives the plate the room: it names the guide and its region, not the pair again', async () => {
    locale.value = 'es'
    const host = mountCard(activeMatch({ canSwitchRegion: false }))
    host.querySelector<HTMLButtonElement>('[data-testid="confirmar-zoom-trigger"]')!.click()
    await nextTick()
    const viewer = document.body.querySelector('[data-testid="photo-viewer"]')!
    expect(viewer.querySelector('[data-testid="viewer-name"]')!.textContent!.trim()).toBe('Guía visual')
    expect(viewer.textContent).toContain('Occidente · Anverso y reverso')
    expect(viewer.textContent).not.toContain('Heliconius erato cyrbia vs.')
    // The person's own photo is one swipe away.
    expect(viewer.textContent).toContain('1 / 2')
    document.body.querySelector<HTMLButtonElement>('[data-testid="viewer-close"]')!.click()
    await nextTick()
  })

  it('an honest note names the pair the same way and its region once, as a chip', () => {
    locale.value = 'es'
    const host = mountCard({
      ...activeMatch(),
      cardId: null,
      status: 'inseparable_in_photos',
      displayKind: 'honest_note',
      canSwitchRegion: false,
      imageUrl: null,
      regionBadge: 'Oriente',
      taxonA: 'Brevioleria aelia oncidia',
      taxonB: 'Brevioleria arzalia loronia',
      honestNote: 'Estas dos especies son casi idénticas en fotos de campo; una foto sola no basta para separarlas.',
      alternateCards: [],
    })
    const note = host.querySelector('[data-testid="confirmar-honest-note"]')!
    const lines = [...note.querySelectorAll('.confirmar-pair > span')].map(e => e.textContent!.replace(/\s+/g, ' ').trim())
    expect(lines).toEqual(['Brevioleria aelia oncidia', 'vs. Brevioleria arzalia loronia'])
    expect([...note.querySelectorAll('.form')].map(e => e.textContent)).toEqual(['oncidia', 'loronia'])
    const region = note.querySelector('[data-testid="confirmar-note-region"]')!
    expect(region.textContent!.trim()).toBe('Oriente')
    // A badge as the card's (a fill, no outline: it is not a control), before the names.
    expect(region.className).not.toContain('border')
    expect(region.compareDocumentPosition(note.querySelector('.confirmar-pair')!) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy()
    expect(note.textContent!.split('Oriente')).toHaveLength(2)
    expect(note.querySelector('button')).toBeNull()
  })

  it('no side known and a guide for one side only: one line says so, with the two sides to choose from', () => {
    locale.value = 'es'
    const sides: string[] = []
    const host = document.createElement('div')
    document.body.append(host)
    const app = createApp(ConfirmarVisualCard, {
      match: activeMatch({ pairId: 'w6_05', cardId: 'w6_05', guideSide: 'East', canSwitchRegion: false, regionBadge: 'Oriente', alternateCards: [{ cardId: 'w6_05', regionBadge: 'Oriente', regionDetail: null, sideAndes: ['East'] }] }),
      onSelectSide: (s: string) => sides.push(s),
    })
    app.mount(host)
    unmount = () => app.unmount()

    const line = host.querySelector('[data-testid="confirmar-region-unknown"]')!
    expect(line.querySelector('[data-testid="confirmar-region-unknown-text"]')!.textContent).toBe('Región no determinada: esta guía es de Oriente')
    // At the top of the card, before its badges and its plate.
    const card = host.querySelector('[data-testid="confirmar-visual-card"]')!
    expect(card.firstElementChild).toBe(line)
    // The two sides, West first, each a full tap target; plain buttons (neither is a state: a tap sets the page's side).
    const group = line.querySelector('[data-testid="confirmar-side-switch"]')!
    expect(group.getAttribute('role')).toBe('group')
    expect(group.getAttribute('aria-label')).toBe('Lado de los Andes de la foto')
    const buttons = [...group.querySelectorAll('button')]
    expect(buttons.map(b => b.textContent!.trim())).toEqual(['Occidente', 'Oriente'])
    expect(buttons.some(b => b.hasAttribute('aria-pressed'))).toBe(false)
    for (const b of buttons) expect(b.className).toContain('min-h-11')
    buttons[0].click()
    buttons[1].click()
    expect(sides).toEqual(['West', 'East'])
    // The line named the region: the badge does not say it again.
    expect(host.querySelector('[data-testid="confirmar-region-badge"]')).toBeNull()
    expect(card.textContent!.split('Oriente')).toHaveLength(3) // the line and the button
  })

  it('a badge that says more than the side stays beside the line', () => {
    locale.value = 'en'
    const host = mountCard(activeMatch({ guideSide: 'East', canSwitchRegion: false, regionBadge: 'East Andes' }))
    expect(host.querySelector('[data-testid="confirmar-region-unknown-text"]')!.textContent).toBe('Region not determined: this guide is for East Ecuador')
    expect(host.querySelector('[data-testid="confirmar-region-badge"]')!.textContent!.trim()).toBe('East Andes')
  })

  it('the line is in the reader\'s language and names the guide\'s own side', () => {
    locale.value = 'en'
    const east = mountCard(activeMatch({ guideSide: 'East', canSwitchRegion: false }))
    expect(east.querySelector('[data-testid="confirmar-region-unknown-text"]')!.textContent).toBe('Region not determined: this guide is for East Ecuador')
    expect([...east.querySelectorAll('[data-testid="confirmar-side-switch"] button')].map(b => b.textContent!.trim())).toEqual(['West', 'East'])
    unmount()
    const west = mountCard(activeMatch({ guideSide: 'West', canSwitchRegion: false }))
    expect(west.querySelector('[data-testid="confirmar-region-unknown-text"]')!.textContent).toBe('Region not determined: this guide is for West Ecuador')
    unmount()
    locale.value = 'es'
    const oeste = mountCard(activeMatch({ guideSide: 'West', canSwitchRegion: false }))
    expect(oeste.querySelector('[data-testid="confirmar-region-unknown-text"]')!.textContent).toBe('Región no determinada: esta guía es de Occidente')
  })

  it('no line when the side is known, when the guide is for both sides, or on a note', () => {
    locale.value = 'es'
    // The side came from the person, the GPS or a subspecies.
    const known = mountCard(activeMatch({ regionFallback: false, canSwitchRegion: false }))
    expect(known.querySelector('[data-testid="confirmar-region-unknown"]')).toBeNull()
    unmount()
    // One card for both sides (Heliconius cydno / timareta): no side to say.
    const both = mountCard(activeMatch({ guideSide: null, canSwitchRegion: false, regionBadge: 'Occidente vs. Oriente' }))
    expect(both.querySelector('[data-testid="confirmar-region-unknown"]')).toBeNull()
    expect(both.querySelector('[data-testid="confirmar-side-switch"]')).toBeNull()
    unmount()
    const note = mountCard({ ...activeMatch(), cardId: null, status: 'inseparable_in_photos', displayKind: 'honest_note', imageUrl: null, guideSide: null, honestNote: 'x' })
    expect(note.querySelector('[data-testid="confirmar-region-unknown"]')).toBeNull()
  })

  it('a pair with a guide per region says the line and keeps its own region switch as the one control', () => {
    locale.value = 'es'
    const host = mountCard(activeMatch())
    expect(host.querySelector('[data-testid="confirmar-region-unknown-text"]')!.textContent).toBe('Región no determinada: esta guía es de Occidente')
    expect(host.querySelector('[data-testid="confirmar-side-switch"]')).toBeNull()
    expect(host.querySelector('[data-testid="confirmar-region-switcher"]')).not.toBeNull()
  })

  it('renders nothing for hidden status', () => {
    const host = mountCard({
      ...activeMatch(),
      cardId: null,
      status: 'no_card_for_side',
      displayKind: 'hidden',
      canSwitchRegion: false,
      imageUrl: null,
    })
    expect(host.querySelector('[data-testid="confirmar-visual-card"]')).toBeNull()
    expect(host.querySelector('[data-testid="confirmar-honest-note"]')).toBeNull()
  })

  it('a guide of #1 its look-alike unranked: the line saying so, pills for #1\'s guides, and the runner-up note under it', async () => {
    locale.value = 'es'
    const host = document.createElement('div')
    document.body.append(host)
    const picked: string[] = []
    const app = createApp(ConfirmarVisualCard, {
      match: activeMatch({
        cardId: 'p01_east_napo',
        regionFallback: false,
        guideSide: 'East',
        canSwitchRegion: false,
        regionBadge: 'Oriente · Napo y Amazonía',
        taxonA: 'Heliconius erato lativitta',
        taxonB: 'Heliconius melpomene malleti',
        source: 'top1_comimic',
        guideOptions: [
          { pairId: 'p01', label: 'Heliconius erato' },
          { pairId: 'w6_99', label: 'Heliconius timareta' },
        ],
        sideNote: {
          pairId: 'w5_03',
          status: 'not_reliably_separable_in_photos',
          taxonA: 'Heliconius elevatus willmotti',
          taxonB: 'Heliconius melpomene',
          regionBadge: 'Oriente',
          honestNote: 'Estas dos especies son casi idénticas en fotos de campo; una foto sola no basta para separarlas.',
        },
      }),
      onSelectGuide: (id: string) => picked.push(id),
    })
    app.config.globalProperties.$t = t
    app.config.globalProperties.$tn = tn
    app.mount(host)
    unmount = () => app.unmount()

    expect(host.querySelector('[data-testid="confirmar-comimic-line"]')!.textContent).toContain('no está entre las respuestas probables')
    const pills = host.querySelectorAll<HTMLButtonElement>('[data-testid="confirmar-guide-switcher"] button')
    expect([...pills].map(b => b.textContent!.replace(/\s+/g, ' ').trim())).toEqual(['vs. H. erato', 'vs. H. timareta'])
    expect(pills[0].getAttribute('aria-pressed')).toBe('true')
    expect(pills[1].className).toContain('min-h-11')
    pills[1].click()
    expect(picked).toEqual(['w6_99'])
    const note = host.querySelector('[data-testid="confirmar-side-note"]')!
    expect(note.getAttribute('data-pair-id')).toBe('w5_03')
    expect(note.textContent).toContain('elevatus')
    expect(note.textContent).toContain('casi idénticas')
    // One space between the words, none before the colon (the phone shot of 26 Nov 2026 read «willmottivs.» and «melpomene :»).
    expect(host.querySelector('[data-testid="confirmar-side-note-body"]')!.textContent!.replace(/\s+/g, ' ').trim()).toBe(
      'Heliconius elevatus willmotti vs. Heliconius melpomene: Estas dos especies son casi idénticas en fotos de campo; una foto sola no basta para separarlas.',
    )
    // The note sits under the plate, never in its place.
    expect(host.querySelector('[data-testid="confirmar-card-image"]')).not.toBeNull()
    expect(host.querySelector('[data-testid="confirmar-honest-note"]')).toBeNull()
  })

  it('one guide, from a runner-up, with no note: no pills, no line, no note', () => {
    const host = mountCard(activeMatch({ source: 'runner_up', guideOptions: [], sideNote: null }))
    expect(host.querySelector('[data-testid="confirmar-guide-switcher"]')).toBeNull()
    expect(host.querySelector('[data-testid="confirmar-comimic-line"]')).toBeNull()
    expect(host.querySelector('[data-testid="confirmar-side-note"]')).toBeNull()
  })

  it('shows the unranked co-mimic line only for co-mimic sources, never for ranked runner-up or compared pairs', () => {
    locale.value = 'en'
    for (const source of ['runner_up', 'compared', 'runner_up_pair', 'intraspecific'] as const) {
      const host = mountCard(activeMatch({ source }))
      expect(host.querySelector('[data-testid="confirmar-comimic-line"]'), source).toBeNull()
      unmount()
    }
    for (const source of ['top1_comimic', 'compared_comimic', 'runner_up_comimic'] as const) {
      const host = mountCard(activeMatch({ source }))
      expect(host.querySelector('[data-testid="confirmar-comimic-line"]')?.textContent, source).toContain("not among the AI's likely answers")
      unmount()
    }
  })

  it('renders unanchored pair pill labels as "H. erato vs. H. melpomene" without duplicating "vs."', () => {
    locale.value = 'en'
    const host = mountCard(
      activeMatch({
        source: 'runner_up_pair',
        guideOptions: [
          { pairId: 'p01', label: 'Heliconius erato vs. Heliconius melpomene' },
          { pairId: 'w3a_04', label: 'Mechanitis mazaeus vs. Mechanitis polymnia' },
        ],
      }),
    )
    const pills = [...host.querySelectorAll<HTMLButtonElement>('[data-testid="confirmar-guide-switcher"] button')]
    expect(pills.map(b => b.textContent!.replace(/\s+/g, ' ').trim())).toEqual([
      'H. erato vs. H. melpomene',
      'M. mazaeus vs. M. polymnia',
    ])
    for (const pill of pills) {
      expect(pill.textContent).not.toMatch(/vs\.\s+.*\svs\./i)
    }
  })
})
