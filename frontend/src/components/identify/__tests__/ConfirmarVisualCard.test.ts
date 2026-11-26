import { afterEach, describe, expect, it, vi } from 'vitest'
import { createApp, nextTick } from 'vue'
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
})
