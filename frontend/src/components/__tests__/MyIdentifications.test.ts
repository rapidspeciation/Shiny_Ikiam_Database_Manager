import { afterEach, describe, expect, it } from 'vitest'
import { createApp, h, nextTick } from 'vue'
import MyIdentifications from '../identify/MyIdentifications.vue'
import { t } from '../../lib/i18n'

// «Mis identificaciones»: «Reprocesar» only on the items the view's gate marks (lib/identifyHistory.ts reprocessable);
// on a phone the button takes its own line under the name and date, inline from sm up; the reason line apart.
let unmount = () => {}
afterEach(() => unmount())
function mount(entries: unknown[]) {
  const el = document.createElement('div')
  document.body.appendChild(el)
  const app = createApp({ render: () => h(MyIdentifications, { entries, current: null, busy: false } as never) })
  app.config.globalProperties.$t = t
  app.mount(el)
  unmount = () => (app.unmount(), el.remove())
  return el
}
const item = (at: string, extra: Record<string, unknown>) => ({ at, thumb: '', ai: 'Mechanitis polymnia', p: 0.56, pick: null, side: null, saved: true, reprocess: false, reason: null, ...extra })

describe('MyIdentifications', () => {
  it('the button only where the gate says; on phones on its own line after the text, inline from sm', async () => {
    const el = mount([item('2026-11-12T10:00:00Z', { reprocess: true }), item('2026-11-12T11:00:00Z', { reason: 'Inicie sesión para volver a analizarla.' })])
    await nextTick()
    const rows = el.querySelectorAll('[data-testid=my-identification]')
    const line = rows[0].querySelector('[data-testid=history-reprocess-line]')!
    expect(line.className).toMatch(/order-last basis-full/)
    expect(line.className).toMatch(/sm:order-none sm:basis-auto/)
    expect(line.querySelector('[data-testid=history-reprocess]')).not.toBeNull()
    expect(rows[0].className).toMatch(/flex-wrap/)
    // the reason is said, and no button where the gate said none
    expect(rows[1].querySelector('[data-testid=history-reprocess]')).toBeNull()
    expect(rows[1].querySelector('[data-testid=history-reason]')!.textContent).toContain('Inicie sesión')
  })
})
