import { afterEach, describe, expect, it, vi } from 'vitest'
import { createApp, nextTick } from 'vue'
import { createPinia } from 'pinia'
import { createMemoryHistory, createRouter } from 'vue-router'
import { t, tn } from '../../lib/i18n'

// The Buscador's ⋯ tools menu (phones) closes on a change of page, like the header's menus.

// A search answers with no sheet; anything else with nothing.
vi.mock('../../lib/api', () => ({ api: async (path: string) => (String(path).startsWith('search?') ? { query: 'Q5E', sheets: [], took: 0 } : {}) }))

let unmount = () => {}
afterEach(() => {
  unmount()
  document.body.innerHTML = ''
  localStorage.clear()
  vi.unstubAllGlobals()
})

/** The page on a phone (the width query matches) or a computer. */
async function mountAt(phone: boolean, path = '/tablas') {
  vi.stubGlobal('matchMedia', (query: string) => ({
    matches: phone && /max-width: 639/.test(query),
    addEventListener() {},
    removeEventListener() {},
  }))
  const { default: TablesView } = await import('../TablesView.vue')
  const router = createRouter({ history: createMemoryHistory(), routes: [{ path: '/tablas', component: TablesView }] })
  await router.push(path)
  const host = document.createElement('div')
  document.body.append(host)
  const app = createApp({ template: '<RouterView />' })
  app.use(createPinia()).use(router)
  app.config.globalProperties.$t = t
  app.config.globalProperties.$tn = tn
  app.mount(host)
  unmount = () => app.unmount()
  for (let i = 0; i < 5; i++) await nextTick()
  return { host, router }
}
const pressed = (host: HTMLElement) =>
  [...host.querySelectorAll<HTMLElement>('[data-phone-views] .view-switch button')].map(b => `${b.dataset.view}:${b.getAttribute('aria-pressed')}`)

describe('the Buscador on a phone', () => {
  it('shows its rows as a list unless the grid or the form is chosen, and keeps the choice on the device', async () => {
    const { locale } = await import('../../lib/i18n')
    const before = locale.value
    locale.value = 'es'
    const { host } = await mountAt(true)
    expect(pressed(host)).toEqual(['list:true', 'grid:false', 'form:false'])
    const labels = [...host.querySelectorAll('[data-phone-views] .view-switch button')].map(b => b.textContent!.trim())
    expect(labels).toEqual(['Lista', 'Tabla', 'Formulario'])
    // The grid's help is the grid's: not beside the list.
    expect(host.querySelector('[data-phone-hint]')).toBeNull()
    host.querySelector<HTMLElement>('[data-phone-views] [data-view="grid"]')!.click()
    await nextTick()
    expect(pressed(host)).toEqual(['list:false', 'grid:true', 'form:false'])
    expect(host.querySelector('[data-phone-hint]')).not.toBeNull()
    expect(JSON.parse(localStorage.getItem('ithomiini:phone-view')!)).toBe('grid')
    locale.value = before
  })
  it('opens with the view last chosen; the results have the list and the grid (the form is one row of a sheet)', async () => {
    localStorage.setItem('ithomiini:phone-view', JSON.stringify('form'))
    const { host } = await mountAt(true, '/tablas?buscar=Q5E')
    // A form choice shows the results as the list.
    expect(pressed(host)).toEqual(['list:true', 'grid:false'])
  })
  it('a computer keeps the full grid: no switch', async () => {
    const { host } = await mountAt(false)
    expect(host.querySelector('[data-phone-views]')).toBeNull()
  })
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
