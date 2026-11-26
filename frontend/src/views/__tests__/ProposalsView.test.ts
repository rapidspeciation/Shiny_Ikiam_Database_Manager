import { afterEach, describe, expect, it, vi } from 'vitest'
import { createApp, defineComponent, h, nextTick } from 'vue'
import { createPinia } from 'pinia'
import { createMemoryHistory, createRouter } from 'vue-router'
import { t, tn } from '../../lib/i18n'

// Cambios propuestos on its own page: the live list full screen, and under it the way back, a finger can hit.
const shown: Record<string, unknown>[] = []
vi.mock('../../components/assistant/ProposalsLive.vue', () => ({
  default: defineComponent({
    props: { layout: String, only: String, chat: String, photo: Boolean },
    setup(props) {
      shown.push(props)
      return () => h('div', { class: 'live-stub' })
    },
  }),
}))
vi.mock('../../components/DatabaseBadge.vue', () => ({ default: defineComponent({ setup: () => () => h('span', { class: 'badge-stub' }) }) }))

let unmount = () => {}
afterEach(() => {
  unmount()
  shown.length = 0
  document.body.innerHTML = ''
})

async function mount(path: string) {
  const { default: ProposalsView } = await import('../ProposalsView.vue')
  const router = createRouter({
    history: createMemoryHistory(),
    routes: [
      { path: '/propuestas/:id?', component: ProposalsView },
      { path: '/asistente', component: { render: () => null } },
    ],
  })
  await router.push(path)
  const host = document.createElement('div')
  document.body.append(host)
  const app = createApp({ template: '<RouterView />' })
  app.use(createPinia()).use(router)
  app.config.globalProperties.$t = t
  app.config.globalProperties.$tn = tn
  app.mount(host)
  unmount = () => app.unmount()
  for (let i = 0; i < 4; i++) await nextTick()
  return host
}

describe('ProposalsView', () => {
  it("shows one proposal by its address, with a way back to its place in the Asistente that a finger can hit", async () => {
    const host = await mount('/propuestas/p1?foto=1')
    expect(shown[0]).toMatchObject({ layout: 'page', only: 'p1', chat: '', photo: true })
    const back = host.querySelector<HTMLAnchorElement>('[data-back]')!
    expect(back.textContent).toContain('Volver al Asistente')
    expect(back.getAttribute('href')).toContain('/asistente?propuesta=p1')
    // Taller on a phone, with an invisible 44 px area around it on touch screens.
    expect(back.className).toContain('hit-target')
    expect(back.className).toContain('max-sm:min-h-9')
  })
  it("shows a chat's proposals by its address", async () => {
    await mount('/propuestas?chat=all')
    expect(shown[0]).toMatchObject({ layout: 'page', only: '', chat: 'all', photo: false })
  })
})
