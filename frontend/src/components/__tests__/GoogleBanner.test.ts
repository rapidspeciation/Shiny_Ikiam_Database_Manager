import { afterEach, describe, expect, it } from 'vitest'
import { createApp, nextTick } from 'vue'
import { createPinia, setActivePinia } from 'pinia'
import GoogleBanner from '../GoogleBanner.vue'
import { useLive } from '../../stores/live'
import { locale } from '../../lib/i18n'

let unmount = () => {}
afterEach(() => {
  unmount()
  document.body.innerHTML = ''
  locale.value = 'es'
})

function mount() {
  const pinia = createPinia()
  setActivePinia(pinia)
  const host = document.createElement('div')
  document.body.append(host)
  const app = createApp(GoogleBanner)
  app.use(pinia)
  app.mount(host)
  unmount = () => app.unmount()
  return { host, live: useLive() }
}

describe('GoogleBanner', () => {
  it('says nothing while Google answers and nothing waits', () => {
    const { host } = mount()
    expect(host.textContent?.trim()).toBe('')
  })

  it('tells everyone the sheet is busy and how many saves wait; then that they are being written', async () => {
    const { host, live } = mount()
    locale.value = 'en'
    live.workbook = { state: 'busy' }
    live.waiting = 2
    await nextTick()
    expect(host.querySelector('[role="status"]')?.textContent).toContain(
      'The Google Sheet is busy (recalculating): saves are kept here and written when it answers; 2 waiting',
    )
    live.workbook = { state: 'ok' }
    await nextTick()
    expect(host.textContent).toContain('Writing 2 saves that were waiting to the Google Sheet')
    live.waiting = 0
    await nextTick()
    expect(host.querySelector('[role="status"]')).toBeNull()
  })
})
