import { afterEach, describe, expect, it } from 'vitest'
import { createApp, h } from 'vue'
import { locale, t } from '../../lib/i18n'
import type { FieldSexState } from '../../lib/fieldSex'
import FieldSexEstimate from '../identify/FieldSexEstimate.vue'

// Identificar's general sex line: the bar, the marker and what each region says. No model, no AI.

let host: HTMLElement | null = null
function show(state: FieldSexState) {
  host = document.createElement('div')
  document.body.append(host)
  const app = createApp({ render: () => h(FieldSexEstimate, { state }) })
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
const st = (o: Partial<FieldSexState>): FieldSexState => ({ species: 'Danaus plexippus', status: 'call', p: 0.97, sex: 'female', sure: true, pct: 97, note: null, ...o })
const marker = (el: Element) => (el.querySelector('[data-testid=field-sex-marker]') as HTMLElement).style.left

describe('FieldSexEstimate', () => {
  it('a clear answer: sex and %, the marker at P(female), the band and the notes; no question (Spanish)', () => {
    locale.value = 'es'
    const el = show(st({}))
    expect(text(el.querySelector('[data-testid=field-sex-call]'))).toBe('♀ Hembra 97 %')
    expect(marker(el)).toBe('97%')
    const all = text(el)
    expect(all).toContain('experimental: mariposas vivas')
    expect(all).toContain('El % estima cuántas veces acierta una respuesta así; nunca supera lo medido en la prueba.')
    expect(text(el.querySelector('[data-testid=field-sex-tested]'))).toBe('Probado en fotos de mariposas vivas.')
    expect(all).toContain('♂ macho')
    expect(all).toContain('no seguro')
    expect(all).toContain('♀ hembra')
    expect(all).toContain('cuando respondió, acertó con el 93 % de las hembras')
    expect(all).not.toMatch(/llamada|¿Es una foto|Sí, viva/)
    expect(el.querySelector('button')).toBeNull()
  })
  it('in the band: «No seguro» and the side it leans to, no %', () => {
    locale.value = 'es'
    const el = show(st({ status: 'band', p: 0.4, sex: 'male', sure: false, pct: null }))
    expect(text(el.querySelector('[data-testid=field-sex-call]'))).toBe('No seguro (se inclina a macho)')
    expect(el.querySelector('[data-testid=field-sex-p]')).toBeNull()
    expect(text(el)).not.toMatch(/\d+ %.*estima/)
    expect(marker(el)).toBe('40%')
  })
  it('a cell under 90 %: bar and marker, no answer, no %', () => {
    locale.value = 'es'
    const el = show(st({ status: 'cell_under_90', p: 0.03, sex: 'male', pct: null }))
    expect(text(el.querySelector('[data-testid=field-sex-call]'))).toBe('Sin respuesta')
    expect(text(el.querySelector('[data-testid=field-sex-cell]'))).toContain('menos del 90 %')
    expect(el.querySelector('[data-testid=field-sex-p]')).toBeNull()
    expect(marker(el)).toBe('3%')
  })
  it('species not sure: the side without %, and the note', () => {
    locale.value = 'es'
    const el = show(st({ status: 'species_unsure', p: 0.95, sex: 'female', pct: null, note: 'species' }))
    expect(text(el.querySelector('[data-testid=field-sex-call]'))).toBe('♀ Hembra')
    expect(text(el.querySelector('[data-testid=field-sex-note]'))).toBe('La especie no es segura; el sexo puede estar mal.')
    const unsure = show(st({ status: 'species_unsure', p: 0.6, sex: 'female', sure: false, pct: null, note: 'species' }))
    expect(text(unsure.querySelector('[data-testid=field-sex-call]'))).toBe('No seguro (se inclina a hembra)')
  })
  it('English', () => {
    const el = show(st({ status: 'call', p: 0.02, sex: 'male', pct: 92 }))
    expect(text(el.querySelector('[data-testid=field-sex-call]'))).toBe('♂ Male 92 %')
    expect(text(el)).toContain('Tested on photos of live butterflies.')
    expect(text(el)).toContain('The % estimates how often an answer like this is right; it never exceeds what the test measured.')
    const band = show(st({ status: 'band', p: 0.7, sex: 'female', sure: false, pct: null }))
    expect(text(band.querySelector('[data-testid=field-sex-call]'))).toBe('Not sure (leans to female)')
    const sp = show(st({ status: 'species_unsure', pct: null, note: 'species' }))
    expect(text(sp.querySelector('[data-testid=field-sex-note]'))).toBe('The species is uncertain; the sex may be wrong.')
  })
  it('no em dashes in any of its texts, in either language', () => {
    for (const lang of ['es', 'en'] as const) {
      locale.value = lang
      for (const s of [
        st({}),
        st({ status: 'band', p: 0.5, sure: false, pct: null }),
        st({ status: 'cell_under_90', pct: null }),
        st({ status: 'species_unsure', pct: null, note: 'species' }),
        st({ status: 'no_wings', pct: null, note: 'wings' }),
      ]) {
        expect(show(s).textContent).not.toMatch(/—/)
      }
    }
  })
})
