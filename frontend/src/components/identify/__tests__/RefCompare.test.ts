import { afterEach, describe, expect, it } from 'vitest'
import { createApp, h, nextTick } from 'vue'
import RefCompare from '../RefCompare.vue'
import { t } from '../../../lib/i18n'

let unmount = () => {}
afterEach(() => unmount())

function mount(props: Record<string, unknown>) {
  const el = document.createElement('div')
  document.body.appendChild(el)
  const app = createApp({ render: () => h(RefCompare, props as never) })
  app.config.globalProperties.$t = t
  app.mount(el)
  unmount = () => {
    app.unmount()
    el.remove()
  }
  return el
}

const refs = { photos: [{ src: '/r/a.jpg', credit: 'x', level: 'species' }] }
const base = { mine: '/m.jpg', name: 'Ithomia salapia', loading: false, online: true }

describe('RefCompare', () => {
  it('a loaded reference photo shows no empty-state panel', async () => {
    const el = mount({ ...base, refs })
    expect(el.querySelector('[data-ref-loading]')).not.toBeNull()
    el.querySelectorAll('img')[1].dispatchEvent(new Event('load'))
    await nextTick()
    expect(el.querySelector('[data-ref-loading]')).toBeNull()
    expect(el.textContent).not.toContain(t('No encontramos fotos de esta especie.'))
  })
  it('no photos: says so', async () => {
    const el = mount({ ...base, refs: { photos: [] } })
    await nextTick()
    expect(el.textContent).toContain(t('No encontramos fotos de esta especie.'))
  })
  it('Campo / Museo: two chips switch the reference photo between the live photos and the museum ones (wing crop first)', async () => {
    const zooms: unknown[][] = []
    const wing = { src: '/identificar/wings/CAM1-abcdef12.webp', credit: 'Ikiam / Wellcome Sanger Institute', license: '', level: 'species', kind: 'preserved', specimen: 'CAM1' }
    const two = { photos: [...refs.photos, { src: '/r/b.jpg', credit: 'y', level: 'species' }] }
    const el = mount({ ...base, refs: two, wing, onZoom: (...a: unknown[]) => zooms.push(a) })
    await nextTick()
    expect(el.querySelector('[data-testid=ref-set-field]')!.textContent).toMatch(/2/)
    expect(el.querySelector('[data-testid=ref-set-museum]')!.textContent).toMatch(/1/)
    expect(el.querySelector('[data-testid=ref-set-field]')!.getAttribute('aria-checked')).toBe('true')
    expect(el.querySelector('[data-testid=zoom-ref] img')!.getAttribute('src')).toBe('/r/a.jpg')
    el.querySelector<HTMLButtonElement>('[data-testid=ref-set-museum]')!.click()
    await nextTick()
    expect(el.querySelector('[data-testid=zoom-ref] img')!.getAttribute('src')).toBe(wing.src)
    expect(el.querySelector('[data-testid=preserved]')!.textContent).toContain('CAM1')
    el.querySelector<HTMLButtonElement>('[data-testid=zoom-ref]')!.click()
    expect(zooms).toEqual([['ref', wing.src]])
  })
  it('no museum photo: no chips', async () => {
    const el = mount({ ...base, refs })
    await nextTick()
    expect(el.querySelector('[data-testid=ref-sets]')).toBeNull()
  })
  it('a field set the thumbnail would not take (a plate): «Museo» first; the person can still choose «Campo»', async () => {
    const wing = { src: '/identificar/wings/CAM2-abcdef12.webp', credit: 'Ikiam / Wellcome Sanger Institute', license: '', level: 'species', kind: 'preserved', specimen: 'CAM2' }
    const el = mount({ ...base, refs, wing, preferMuseum: true })
    await nextTick()
    expect(el.querySelector('[data-testid=ref-set-museum]')!.getAttribute('aria-checked')).toBe('true')
    expect(el.querySelector('[data-testid=preserved]')!.textContent).toContain('preservado CAM2')
    el.querySelector<HTMLButtonElement>('[data-testid=ref-set-field]')!.click()
    await nextTick()
    expect(el.querySelector('[data-testid=zoom-ref] img')!.getAttribute('src')).toBe('/r/a.jpg')
  })
  it('a team specimen\'s sex in the app\'s words', async () => {
    const el = mount({ ...base, refs: { photos: [{ src: '/s.jpg', credit: 'CAM9', license: '', level: 'specimen', sex: 'female' }] } })
    await nextTick()
    expect(el.textContent).toContain('CAM9 · ♀ hembra')
    expect(el.textContent).not.toContain('female')
  })
})
