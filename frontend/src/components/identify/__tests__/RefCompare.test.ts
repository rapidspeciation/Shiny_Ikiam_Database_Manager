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
  it('the species\' wing crop sits under the reference photo with its specimen, and opens full screen', async () => {
    const zooms: unknown[][] = []
    const wing = { src: '/identificar/wings/CAM1-abcdef12.webp', credit: 'Ikiam / Wellcome Sanger Institute', license: '', level: 'species', kind: 'preserved', specimen: 'CAM1' }
    const el = mount({ ...base, refs, wing, onZoom: (...a: unknown[]) => zooms.push(a) })
    await nextTick()
    expect(el.querySelector('[data-testid=wing-chip]')!.textContent).toContain('CAM1')
    el.querySelector<HTMLButtonElement>('[data-testid=zoom-wing]')!.click()
    el.querySelector<HTMLButtonElement>('[data-testid=zoom-ref]')!.click()
    expect(zooms).toEqual([['wing', wing.src], ['ref', '/r/a.jpg']])
  })
  it('no wing crop: no wing card', async () => {
    const el = mount({ ...base, refs })
    await nextTick()
    expect(el.querySelector('[data-testid=zoom-wing]')).toBeNull()
  })
})
