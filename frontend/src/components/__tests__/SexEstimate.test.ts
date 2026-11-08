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
    expect(text(el)).toMatch(/(corte|cutoff) 53[.,]7 %/)
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
})
