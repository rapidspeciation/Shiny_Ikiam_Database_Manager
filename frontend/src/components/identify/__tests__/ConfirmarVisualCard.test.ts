import { afterEach, describe, expect, it } from 'vitest'
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
  imageUrl: 'https://example.test/identificar/confirmar/cards/es/p01_west.webp',
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
  it('renders active card, region switcher pills (>=44px tap targets), and opens/closes zoomable PhotoViewer lightbox', async () => {
    locale.value = 'es'
    const selected: string[] = []
    const host = mountCard(activeMatch(), id => selected.push(id))

    expect(host.querySelector('[data-testid="confirmar-visual-card"]')).not.toBeNull()
    // When the region switcher is shown, the region badge is hidden so the region is not repeated twice
    expect(host.querySelector('[data-testid="confirmar-region-badge"]')).toBeNull()
    expect(host.querySelector('[data-testid="confirmar-view-badge"]')!.textContent?.trim()).toBe('Anverso y reverso')
    expect(host.querySelector('[data-testid="confirmar-pair-title"]')!.textContent).toContain('Heliconius erato cyrbia')

    const eastBtn = host.querySelector<HTMLButtonElement>('[data-testid="confirmar-region-opt-p01_east_napo"]')!
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

  it('renders crisp honest note for inseparable/deferred pairs with zero internal machinery', () => {
    locale.value = 'en'
    const host = mountCard({
      ...activeMatch(),
      cardId: null,
      status: 'not_reliably_separable_in_photos',
      displayKind: 'honest_note',
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

  it('renders nothing for hidden status', () => {
    const host = mountCard({
      ...activeMatch(),
      cardId: null,
      status: 'no_card_for_side',
      displayKind: 'hidden',
      imageUrl: null,
    })
    expect(host.querySelector('[data-testid="confirmar-visual-card"]')).toBeNull()
    expect(host.querySelector('[data-testid="confirmar-honest-note"]')).toBeNull()
  })
})
