import { afterEach, describe, expect, it } from 'vitest'
import { createApp, h } from 'vue'
import SheetApply from '../assistant/SheetApply.vue'
import PhotoThumbs from '../assistant/PhotoThumbs.vue'
import { t, tn } from '../../lib/i18n'

let unmount = () => {}
afterEach(() => {
  unmount()
  document.body.innerHTML = ''
})

function mount(component: object, props: Record<string, unknown>) {
  const host = document.createElement('div')
  document.body.append(host)
  const app = createApp({ render: () => h(component, props) })
  app.config.globalProperties.$t = t
  app.config.globalProperties.$tn = tn
  app.mount(host)
  unmount = () => app.unmount()
  return host
}

describe("a sheet's «Aplicar»", () => {
  it('applies its rows on click; once they are written there is nothing to click', () => {
    let applied = 0
    const host = mount(SheetApply, { sheet: 'Collection_data', rows: 1, written: 0, onApply: () => applied++ })
    host.querySelector('button')!.click()
    expect(applied).toBe(1)
    unmount()
    expect(mount(SheetApply, { sheet: 'Insectary_data', rows: 0, written: 2 }).querySelector('button')).toBeNull()
  })
})

describe('the photos above a read-only table', () => {
  it('one thumbnail per photo; a click opens the review on that photo', () => {
    const opened: number[] = []
    const host = mount(PhotoThumbs, {
      id: 'table-1',
      page: { photos: 2, photoKey: 'abc' },
      photos: [
        { photo: 0, from: 3, to: 7, rows: 2 },
        { photo: 1, from: 2, to: 2, rows: 1 },
      ],
      onPhoto: (n: number) => opened.push(n),
    })
    expect(host.querySelectorAll('[data-photo-thumb]')).toHaveLength(2)
    expect(host.querySelector('img')!.getAttribute('src')).toBe('api/proposals/table-1/photos/0?size=thumb&v=abc')
    ;(host.querySelectorAll('a')[1] as HTMLAnchorElement).click()
    expect(opened).toEqual([1])
  })
})
