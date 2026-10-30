import { afterEach, describe, expect, it, vi } from 'vitest'
import { createApp, h, nextTick } from 'vue'
import RackChanges from '../assistant/RackChanges.vue'
import RackDepositHead from '../assistant/RackDepositHead.vue'
import type { RackDeposit } from '../../lib/rackDeposit'
import type { ProposalChange } from '../../lib/proposals'
import { t, tn } from '../../lib/i18n'

let unmount = () => {}
afterEach(() => {
  unmount()
  vi.unstubAllGlobals()
  document.body.innerHTML = ''
})

function mount(props: Record<string, unknown>, component: object = RackChanges) {
  const host = document.createElement('div')
  document.body.append(host)
  const app = createApp({ render: () => h(component, props) })
  app.config.globalProperties.$t = t
  app.config.globalProperties.$tn = tn
  app.mount(host)
  unmount = () => app.unmount()
  return host
}
const deposit = (over: Partial<RackDeposit> = {}): RackDeposit => ({
  responsable: 'Franz',
  ultracongelador: 'LBI062',
  sheet: 'Ithomiini_tube_locations_18Jun26',
  unset: [],
  protected: true,
  racks: [
    {
      reading: 'r1',
      rack_id: 'FK00274230',
      code_beside: 'A12',
      wells: { A1: { t: 'FS1', s: 'new' } },
      missing: [],
      problems: [],
      need_rows: [],
      elsewhere: [],
      duplicates: [],
      unset: [],
    },
  ],
  ...over,
})
const row: ProposalChange = { index: 0, key: 'n1', recordId: null, sheet: 'Ithomiini_tube_locations_18Jun26', row: null, label: '', create: true, values: { Tube_ID: 'FS1' } }
const flush = () => new Promise(r => setTimeout(r, 0))

describe('a rack deposit the app cannot write: its changes by hand', () => {
  it('the download waits, with why, while a well has no tube', () => {
    const d = deposit()
    d.racks[0].missing = ['B1']
    const host = mount({ proposalId: 'p1', deposit: d, changes: [row], editable: true, main: true })
    const button = host.querySelector<HTMLButtonElement>('[data-rack-changes]')!
    expect(button.disabled).toBe(true)
    expect(button.title).toMatch(/B1/)
    // «Aplicado a mano» only once the changes were downloaded.
    expect(host.querySelector<HTMLButtonElement>('[data-rack-by-hand]')!.disabled).toBe(true)
  })

  it('downloads the file, then «Aplicado a mano» asks once and marks it', async () => {
    const calls: string[] = []
    const replaced: unknown[] = []
    vi.stubGlobal(
      'fetch',
      vi.fn(async (url: string) => {
        calls.push(url)
        const body = url.endsWith('rack-changes')
          ? { file: 'Cambios.xlsx', download: 'https://x/api/rack-reports/aaaaaaaaaaaaaaaaaaaaaaaa/Cambios.xlsx', proposal: { id: 'p1' } }
          : { manual: { by: 'Franz' }, proposal: { id: 'p1', status: 'closed' } }
        return new Response(JSON.stringify(body), { status: 200 })
      }),
    )
    const clicked = vi.spyOn(HTMLAnchorElement.prototype, 'click').mockImplementation(() => {})
    const host = mount({ proposalId: 'p1', deposit: deposit({ changes_xlsx: { file: 'Cambios.xlsx', at: 'x' } }), changes: [row], editable: true, main: true, onReplace: (p: unknown) => replaced.push(p) })
    host.querySelector<HTMLButtonElement>('[data-rack-changes]')!.click()
    await flush()
    expect(calls).toEqual(['api/chat/proposals/p1/rack-changes'])
    expect(clicked).toHaveBeenCalledTimes(1)
    host.querySelector<HTMLButtonElement>('[data-rack-by-hand]')!.click()
    await nextTick()
    host.querySelector<HTMLButtonElement>('[data-rack-by-hand-yes]')!.click()
    await flush()
    expect(calls.at(-1)).toBe('api/chat/proposals/p1/rack-by-hand')
    expect(replaced.at(-1)).toMatchObject({ status: 'closed' })
  })

  it('not the main action (the app can write the sheet): only the download, no «Aplicado a mano»', () => {
    const host = mount({ proposalId: 'p1', deposit: deposit({ protected: false }), changes: [row], editable: true })
    expect(host.querySelector<HTMLButtonElement>('[data-rack-changes]')!.disabled).toBe(false)
    expect(host.querySelector('[data-rack-by-hand]')).toBeNull()
  })

  it("the deposit's head: the download beside the deposit sheet when the app can write; a pointer to the foot when not", () => {
    let host = mount({ proposalId: 'p1', deposit: deposit({ protected: false }), changes: [row], editable: true }, RackDepositHead)
    expect(host.querySelector('[data-rack-changes]')).not.toBeNull()
    expect(host.querySelector('[data-no-edit-access-head]')).toBeNull()
    unmount()
    host = mount({ proposalId: 'p1', deposit: deposit(), changes: [row], editable: true }, RackDepositHead)
    expect(host.querySelector('[data-rack-changes]')).toBeNull()
    expect(host.querySelector('[data-no-edit-access-head]')!.textContent).toMatch(/Ithomiini_tube_locations_18Jun26/)
  })
})
