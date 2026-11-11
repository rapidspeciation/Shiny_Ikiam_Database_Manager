import { afterEach, describe, expect, it } from 'vitest'
import { createApp, h, nextTick } from 'vue'
import { locale, t } from '../../lib/i18n'
import type { FieldSexState, LiveAnswer } from '../../lib/fieldSex'
import FieldSexEstimate from '../identify/FieldSexEstimate.vue'

// Identificar's general sex line: what each state shows, and the live-butterfly tap. No model, no AI.

let host: HTMLElement | null = null
function show(state: FieldSexState, onLive: (a: LiveAnswer | undefined) => void = () => {}) {
  host = document.createElement('div')
  document.body.append(host)
  const app = createApp({ render: () => h(FieldSexEstimate, { state, onLive }) })
  app.config.globalProperties.$t = t
  app.mount(host)
  return host
}
afterEach(() => {
  host?.remove()
  host = null
  locale.value = 'en'
})
const text = (el: Element | null) => el?.textContent?.replace(/\s+/g, ' ').trim() ?? ''

describe('FieldSexEstimate', () => {
  it('asks first; the tap reports yes or no; no call is shown before', async () => {
    locale.value = 'es'
    const got: (LiveAnswer | undefined)[] = []
    const el = show({ kind: 'ask', species: 'Danaus plexippus' }, a => got.push(a))
    expect(text(el.querySelector('[data-testid=field-sex-ask]'))).toBe('¿Es una foto de una mariposa viva (no de colección ni montada)?')
    expect(el.querySelector('[data-testid=field-sex-call]')).toBeNull()
    ;(el.querySelector('[data-testid=field-sex-yes]') as HTMLButtonElement).click()
    ;(el.querySelector('[data-testid=field-sex-no]') as HTMLButtonElement).click()
    await nextTick()
    expect(got).toEqual(['yes', 'no'])
  })
  it('a call: sex and its whole %, labelled experimental, with the notes (Spanish)', () => {
    locale.value = 'es'
    const el = show({ kind: 'call', species: 'Danaus plexippus', sex: 'female', pct: 97 })
    expect(text(el.querySelector('[data-testid=field-sex-call]'))).toBe('♀ Hembra 97 %')
    const all = text(el)
    expect(all).toContain('experimental: mariposas vivas')
    expect(all).toContain('Poanes zabulon')
    expect(all).toContain('93 % de las hembras y el 99 % de los machos')
    expect(all).toContain('No vale para ejemplares de colección ni montados.')
    expect(all).toContain('sin validar hasta la próxima')
  })
  it('a male call in English; "change" clears the answer', async () => {
    const got: (LiveAnswer | undefined)[] = []
    const el = show({ kind: 'call', species: 'Papilio troilus', sex: 'male', pct: 92 }, a => got.push(a))
    expect(text(el.querySelector('[data-testid=field-sex-call]'))).toBe('♂ Male 92 %')
    expect(text(el)).toContain('Not valid for collection or pinned specimens.')
    ;(el.querySelector('[data-testid=field-sex-change]') as HTMLButtonElement).click()
    await nextTick()
    expect(got).toEqual([undefined])
  })
  it('answered no: no call, says why', () => {
    locale.value = 'es'
    const el = show({ kind: 'declined', species: 'Danaus plexippus' })
    expect(el.querySelector('[data-testid=field-sex-call]')).toBeNull()
    expect(text(el.querySelector('[data-testid=field-sex-declined]'))).toContain('solo se comprobó en fotos de mariposas vivas')
  })
  it('no em dashes in any of its texts, in either language', () => {
    for (const lang of ['es', 'en'] as const) {
      locale.value = lang
      for (const state of [
        { kind: 'ask', species: 'Danaus plexippus' },
        { kind: 'call', species: 'Danaus plexippus', sex: 'male', pct: 97 },
        { kind: 'declined', species: 'Danaus plexippus' },
      ] as FieldSexState[]) {
        const el = show(state)
        expect(el.textContent).not.toMatch(/—/)
        host?.remove()
        host = null
      }
    }
  })
})
