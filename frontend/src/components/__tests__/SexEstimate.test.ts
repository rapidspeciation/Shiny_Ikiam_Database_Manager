import { afterEach, describe, expect, it } from 'vitest'
import { createApp, h, nextTick } from 'vue'
import { t } from '../../lib/i18n'
import SexEstimate, { type SexState } from '../identify/SexEstimate.vue'

// Identificar's experimental sex estimate: what each state shows.

let host: HTMLElement | null = null
function show(state: SexState, onDownload = () => {}) {
  host = document.createElement('div')
  document.body.append(host)
  const app = createApp({ render: () => h(SexEstimate, { state, onDownload }) })
  app.config.globalProperties.$t = t
  app.mount(host)
  return host
}
afterEach(() => {
  host?.remove()
  host = null
})

const text = (el: Element | null) => el?.textContent?.replace(/\s+/g, ' ').trim() ?? ''

describe('SexEstimate', () => {
  it('a score above the band: female, with the calibrated % of the call and the label', () => {
    const el = show({ kind: 'ready', z: 6 }) // raw P 0.998, calibrated 0.802
    expect(text(el.querySelector('[data-testid=sex-call]'))).toContain('♀')
    expect(text(el.querySelector('[data-testid=sex-p]'))).toBe('80 %')
    expect(text(el)).not.toMatch(/99[.,]8/)
    expect(text(el)).toMatch(/experimental/)
    expect(text(el)).toMatch(/in-hand|en mano/)
  })

  it('a high model P inside the band is not sure; below the band male', async () => {
    let el = show({ kind: 'ready', z: 3 }) // raw P 0.953, calibrated 0.539
    expect(text(el.querySelector('[data-testid=sex-call]'))).toMatch(/Not sure|No seguro/)
    expect(text(el.querySelector('[data-testid=sex-p]'))).toBe('(♀ 54 %)')
    host!.remove()
    el = show({ kind: 'ready', z: Math.log(0.8 / 0.2) }) // raw P(hembra) 0.8: a male call, shown as the % male
    await nextTick()
    expect(text(el.querySelector('[data-testid=sex-call]'))).toContain('♂')
    expect(text(el.querySelector('[data-testid=sex-p]'))).toBe('63 %')
    // The cutoff and the band edges are on the scale, calibrated.
    expect(text(el)).toMatch(/39[.,]7 %/)
    expect(text(el)).toMatch(/(corte|cutoff) ♀ 53[.,]7 %/)
    expect(text(el)).toMatch(/♀ 39[.,]7 %/)
    expect(text(el)).toMatch(/71[.,]8 %/)
  })

  it('outside the trained genera: «no aplica», no score', () => {
    const el = show({ kind: 'genus', genus: 'Pseudoscada' })
    expect(text(el.querySelector('[data-testid=sex-na]'))).toContain('Pseudoscada')
    expect(el.querySelector('[data-testid=sex-call]')).toBeNull()
  })

  it('not on the phone yet: a download button with the size', () => {
    let clicked = 0
    const el = show({ kind: 'download', mb: 43 }, () => clicked++)
    const b = el.querySelector<HTMLButtonElement>('[data-testid=sex-download]')!
    expect(text(b)).toContain('43 MB')
    b.click()
    expect(clicked).toBe(1)
  })

  // Model C (field-sex-C-20261111): its manifest's calibration (WingsClassificator APP_PREP.md, 11 Nov 2026).
  const C = { cut: { t: 0.9918, lo: -0.1859, hi: 1.1956 }, display: { a: 1.0038, b: -0.5417 }, genera: ['Oleria', 'Mechanitis'] }
  const left = (el: Element | null) => parseFloat((el as HTMLElement).style.left)

  it('model C: its own call, %, marks and scale (2.6 logit below lo to 2.1 above hi, no fixed floor or ceiling)', () => {
    const el = show({ kind: 'ready', z: 2.2, calibration: C })
    expect(text(el.querySelector('[data-testid=sex-call]'))).toContain('♀')
    expect(text(el.querySelector('[data-testid=sex-p]'))).toBe('84 %')
    expect(text(el.querySelector('[data-testid=sex-tick-lo]'))).toMatch(/^♀ 32[.,]6 %$/)
    expect(text(el.querySelector('[data-testid=sex-tick-t]'))).toMatch(/(corte|cutoff) ♀ 61[.,]2 %/)
    expect(text(el.querySelector('[data-testid=sex-tick-hi]'))).toMatch(/♀ 65[.,]9 %/)
    // The scale -2.7859 to 3.2956: lo at 42.7 %, the marker (z 2.2) at 82.0 %, no empty right part.
    expect(left(el.querySelector('[data-testid=sex-tick-lo]'))).toBeCloseTo(42.75, 1)
    expect(left(el.querySelector('.sex-marker'))).toBeCloseTo(81.98, 1)
    expect(left(el.querySelector('[data-testid=sex-tick-hi]'))).toBeCloseTo(65.47, 1)
  })

  it('the labels cannot overlap: the cutoff has its own row, lo\'s label ends at lo, hi\'s starts at hi', () => {
    for (const cal of [C, undefined]) {
      const el = show({ kind: 'ready', z: 0.5, calibration: cal })
      const t = el.querySelector('[data-testid=sex-tick-t]')!, lo = el.querySelector('[data-testid=sex-tick-lo]')!, hi = el.querySelector('[data-testid=sex-tick-hi]')!
      expect(t.parentElement).not.toBe(lo.parentElement)
      expect(lo.parentElement).toBe(hi.parentElement)
      expect(lo.className).toContain('-translate-x-full') // drawn to the left of lo
      expect(hi.className).not.toMatch(/translate-x/) // drawn to the right of hi
      host!.remove()
    }
  })

  it('model B keeps its scale (about -1 to 7)', () => {
    const el = show({ kind: 'ready', z: 3 })
    // lo 1.6157 on -0.9843 to 6.9722: 32.7 % (was 32.7 % on -1 to 7).
    expect(left(el.querySelector('[data-testid=sex-tick-lo]'))).toBeCloseTo(32.68, 1)
    expect(left(el.querySelector('[data-testid=sex-tick-hi]'))).toBeCloseTo(73.61, 1)
  })
})
