import { afterEach, describe, expect, it, vi } from 'vitest'
import { createApp, nextTick } from 'vue'
import { createPinia } from 'pinia'
import { createMemoryHistory, createRouter } from 'vue-router'
import { t, tn } from '../../lib/i18n'

// The Buscador's ⋯ tools menu (phones) closes on a change of page, like the header's menus.

vi.mock('../../lib/api', () => ({ api: async () => ({}) }))

let unmount = () => {}
afterEach(() => {
  unmount()
  document.body.innerHTML = ''
})

describe('TablesView', () => {
  it('closes the ⋯ tools menu on a change of page', async () => {
    const { default: TablesView } = await import('../TablesView.vue')
    const router = createRouter({
      history: createMemoryHistory(),
      routes: [{ path: '/tablas', component: TablesView }],
    })
    await router.push('/tablas')
    const host = document.createElement('div')
    document.body.append(host)
    const app = createApp({ template: '<RouterView />' })
    app.use(createPinia()).use(router)
    app.config.globalProperties.$t = t
    app.config.globalProperties.$tn = tn
    app.mount(host)
    unmount = () => app.unmount()
    for (let i = 0; i < 5; i++) await nextTick()
    const tools = host.querySelector<HTMLElement>('.buscador-tools')!
    const button = host.querySelector<HTMLElement>('button[aria-label="Más herramientas"]')!
    button.click()
    await nextTick()
    expect(tools.classList.contains('is-open')).toBe(true)
    await router.push('/tablas?otra=1')
    for (let i = 0; i < 3; i++) await nextTick()
    expect(host.querySelector('.buscador-tools')!.classList.contains('is-open')).toBe(false)
  })
})
