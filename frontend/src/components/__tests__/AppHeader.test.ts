import { afterEach, describe, expect, it, vi } from 'vitest'
import { createApp, nextTick } from 'vue'
import { createPinia } from 'pinia'
import { createMemoryHistory, createRouter } from 'vue-router'
import { t, tn } from '../../lib/i18n'
import AppHeader from '../AppHeader.vue'
import { useSession } from '../../stores/session'
import { usePending } from '../../stores/pending'
import { THEME_KEY, oppositeTheme, theme, themePref } from '../../lib/theme'

// The header's two menus («Más» and the user's name) close the way people expect: outside click, Escape (focus back
// on their button) and a change of page.

vi.mock('../../lib/api', () => ({ api: async () => ({}), setCsrf: () => {} }))
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

describe('AppHeader theme toggle', () => {
  const button = (h: { host: HTMLElement }) => h.host.querySelector<HTMLElement>('[data-theme-button]')!
  const systemItem = (h: { host: HTMLElement }) => h.host.querySelector<HTMLElement>('[data-theme-system]')
  const openMenu = async (h: Awaited<ReturnType<typeof mount>>) => {
    h.host.querySelector<HTMLElement>('button[aria-expanded][title="Ana"]')!.click()
    await h.tick()
  }
  afterEach(() => {
    themePref.value = 'system'
  })

  it('is a two-state sun/moon button showing the theme in effect, with the state in aria-pressed and the tooltip', async () => {
    themePref.value = 'system'
    const h = await mount()
    const was = theme.value
    expect(button(h).getAttribute('aria-pressed')).toBe(String(was === 'dark'))
    expect(button(h).title).toBe(`Tema: ${was === 'dark' ? 'oscuro' : 'claro'} (sistema)`)
    button(h).click()
    await h.tick()
    expect(theme.value).toBe(oppositeTheme(was))
    expect(themePref.value).toBe(oppositeTheme(was))
    expect(localStorage.getItem(THEME_KEY)).toBe(oppositeTheme(was))
    expect(button(h).getAttribute('aria-pressed')).toBe(String(theme.value === 'dark'))
    expect(button(h).title).toBe(`Tema: ${theme.value === 'dark' ? 'oscuro' : 'claro'} (elegido)`)
    expect(button(h).querySelector('svg')).not.toBeNull()
    // Never a third state: pressing twice is back to the first theme.
    button(h).click()
    await h.tick()
    expect(theme.value).toBe(was)
  })

  it('offers «Usar el tema del sistema» in the user menu only while a choice is stored, and it goes back to System', async () => {
    themePref.value = 'system'
    const h = await mount()
    await openMenu(h)
    expect(systemItem(h)).toBeNull()
    button(h).click()
    await h.tick()
    expect(systemItem(h)).not.toBeNull()
    systemItem(h)!.click()
    await h.tick()
    expect(themePref.value).toBe('system')
    expect(localStorage.getItem(THEME_KEY)).toBeNull()
    expect(systemItem(h)).toBeNull()
  })
})

describe('AppHeader sign-out', () => {
  afterEach(() => {
    vi.useRealTimers()
    vi.unstubAllGlobals()
    vi.stubGlobal('ResizeObserver', Observer)
  })

  it('«Cerrar sesión» with an unsaved change: no check and no automatic save run after it, and the change is kept', async () => {
    const h = await mount()
    vi.useFakeTimers()
    vi.stubGlobal('confirm', () => true)
    const session = useSession()
    session.user = { id: 'u', username: 'ana', displayName: 'Ana', role: 'admin' } as never
    const pending = usePending()
    pending.creates.push({ clientId: 'c1', module: 'Insectary_data', label: 'Nueva', values: { Sex: 'female' } })
    pending.persist()
    expect(vi.getTimerCount()).toBe(2)
    const ran: string[] = []
    pending.$onAction(({ name }) => void ran.push(name))
    h.host.querySelector<HTMLElement>('button[aria-expanded][title="Ana"]')!.click()
    await h.tick()
    const out = [...h.host.querySelectorAll<HTMLElement>('[data-user-menu] button')].find(b => b.textContent?.includes('Cerrar sesión'))!
    out.click()
    await h.tick()
    expect(session.user).toBeNull()
    await vi.advanceTimersByTimeAsync(120_000)
    expect(ran).not.toContain('check')
    expect(ran).not.toContain('runAutoSave')
    expect(pending.changeCount).toBe(0)
    expect(JSON.parse(localStorage.getItem('ithomiini:pending:ana')!).creates).toHaveLength(1)
  })
})
