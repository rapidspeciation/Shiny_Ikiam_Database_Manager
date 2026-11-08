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
})
