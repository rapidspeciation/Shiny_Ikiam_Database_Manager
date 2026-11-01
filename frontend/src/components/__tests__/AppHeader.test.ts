import { afterEach, describe, expect, it, vi } from 'vitest'
import { createApp, nextTick } from 'vue'
import { createPinia } from 'pinia'
import { createMemoryHistory, createRouter } from 'vue-router'
import { t, tn } from '../../lib/i18n'
import AppHeader from '../AppHeader.vue'
import { useSession } from '../../stores/session'

// The header's two menus («Más» and the user's name) close the way people expect: outside click, Escape (focus back
// on their button) and a change of page.

vi.mock('../../lib/api', () => ({ api: async () => ({}) }))
vi.mock('../../router', () => ({
  tabs: [
    { path: '/inicio', label: 'Inicio', open: true },
    { path: '/tablas', label: 'Buscador' },
    { path: '/colecta', label: 'Colecta' },
    { path: '/tubos', label: 'Tubos' },
    { path: '/revision', label: 'Revisión' },
  ],
}))
vi.mock('../DatabaseBadge.vue', () => ({ default: { render: () => null } }))
class Observer {
  observe() {}
  disconnect() {}
}
vi.stubGlobal('ResizeObserver', Observer)

let unmount = () => {}
afterEach(() => {
  unmount()
  document.body.innerHTML = ''
})

async function mount() {
  const view = { template: '<div />' }
  const router = createRouter({
    history: createMemoryHistory(),
    routes: ['/inicio', '/tablas', '/colecta', '/tubos', '/revision'].map(path => ({ path, component: view })),
  })
  await router.push('/inicio')
  const pinia = createPinia()
  const host = document.createElement('div')
  document.body.append(host)
  const app = createApp(AppHeader)
  app.use(pinia).use(router)
  app.config.globalProperties.$t = t
  app.config.globalProperties.$tn = tn
  const session = useSession(pinia)
  session.user = { id: 'u', displayName: 'Ana', role: 'admin' } as never
  app.mount(host)
  unmount = () => app.unmount()
  await nextTick()
  const tick = async () => {
    for (let i = 0; i < 3; i++) await nextTick()
  }
  return { router, tick, more: () => host.querySelector<HTMLElement>('[data-more-trigger]')!, host }
}
const press = (el: Element) => el.dispatchEvent(new PointerEvent('pointerdown', { bubbles: true }))
const escape = () => document.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', bubbles: true }))

describe('AppHeader menus', () => {
  it('«Más» closes on an outside click, on Escape (focus back on it) and on a change of page', async () => {
    const h = await mount()
    h.more().click()
    await h.tick()
    expect(h.host.querySelector('[data-more-menu]')).not.toBeNull()
    press(document.body)
    await h.tick()
    expect(h.host.querySelector('[data-more-menu]')).toBeNull()

    h.more().click()
    await h.tick()
    h.host.querySelector<HTMLElement>('[data-more-menu] a')!.focus()
    escape()
    await h.tick()
    expect(h.host.querySelector('[data-more-menu]')).toBeNull()
    expect(document.activeElement).toBe(h.more())

    h.more().click()
    await h.tick()
    await h.router.push('/tablas')
    await h.tick()
    expect(h.host.querySelector('[data-more-menu]')).toBeNull()
  })

  it('↓ on «Más» opens the menu and, when it is already open, moves into it', async () => {
    const h = await mount()
    const down = () => h.more().dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowDown', bubbles: true, cancelable: true }))
    h.more().click()
    await h.tick()
    expect(document.activeElement).not.toBe(h.host.querySelector('[data-more-menu] a'))
    down()
    await h.tick()
    expect(document.activeElement).toBe(h.host.querySelector('[data-more-menu] a'))
  })

  it('the user menu closes on an outside click, on Escape (focus back on the name) and on a change of page', async () => {
    const h = await mount()
    const name = () => h.host.querySelector<HTMLElement>('button[aria-expanded][title="Ana"]')!
    const open = () => h.host.querySelector('[data-user-menu]')
    name().click()
    await h.tick()
    expect(open()).not.toBeNull()
    press(document.body)
    await h.tick()
    expect(open()).toBeNull()

    name().click()
    await h.tick()
    escape()
    await h.tick()
    expect(open()).toBeNull()
    expect(document.activeElement).toBe(name())

    name().click()
    await h.tick()
    await h.router.push('/tablas')
    await h.tick()
    expect(open()).toBeNull()
  })
})
