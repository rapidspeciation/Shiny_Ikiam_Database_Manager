import { afterEach, describe, expect, it } from 'vitest'
import { createApp, h, nextTick } from 'vue'
import { t, tn } from '../../lib/i18n'
import MyContributions from '../identify/MyContributions.vue'
import { CONSENT_VERSION } from '../../lib/contribConfig'
import type { Item } from '../../lib/contribOutbox'

// «Mis envíos»: the statuses (with «sin identificar»), and items queued under the draft v1 held until v2 is accepted.

let host: HTMLElement | null = null
let reconsented = 0
const item = (over: Partial<Item> & { version?: string } = {}): Item => ({
  id: over.id ?? 'i1',
  userId: 'ana',
  createdAt: '2026-11-11T10:00:00.000Z',
  state: 'sent',
  attempts: 0,
  nextAt: 0,
  lastError: null,
  serverId: null,
  photo: null,
  sha256: null,
  thumb: null,
  mini: '',
  payload: {
    createdAt: '2026-11-11T10:00:00.000Z',
    takenAt: null,
    labels: { sex: 'female', photoKind: 'hand', species: { name: 'Mechanitis polymnia', rank: 'species', confirmed: true, freeText: false } },
    prediction: {},
    consent: { version: over.version ?? CONSENT_VERSION },
  },
  serverStatus: 'received',
  ...over,
})
function show(items: Item[]) {
  host = document.createElement('div')
  document.body.append(host)
  const app = createApp({ render: () => h(MyContributions, { items, paused: false, contact: 'person@example.org', onReconsent: () => void reconsented++ }) })
  app.config.globalProperties.$t = t
  app.config.globalProperties.$tn = tn
  app.mount(host)
}
const $ = (sel: string) => host!.querySelector<HTMLElement>(sel)
afterEach(() => {
  host?.remove()
  host = null
  reconsented = 0
  localStorage.clear()
})

describe('MyContributions', () => {
  it('shows «sin identificar» for an unsure or unrecognised name, and the other statuses', () => {
    show([item({ id: 'a', serverStatus: 'unidentified' }), item({ id: 'b', serverStatus: 'accepted' }), item({ id: 'c', serverStatus: 'rejected' })])
    const statuses = [...host!.querySelectorAll('[data-testid=contribution-status]')].map(e => e.textContent)
    expect(statuses.map(s => /sin identificar|unidentified/.test(s!))).toEqual([true, false, false])
    expect($('[data-testid=contrib-held]')).toBeNull()
  })

  it('an item queued under v1 waits for v2: the sheet once, then it is let out', async () => {
    show([item({ id: 'old', state: 'pending', version: 'v1-draft' }), item({ id: 'new' })])
    expect($('[data-testid=contrib-held]')).not.toBeNull()
    $('[data-testid=contrib-held-review]')!.click()
    await nextTick()
    expect($('[data-testid=consent-contact]')!.textContent).toMatch(/person@example\.org/)
    $('[data-testid=consent-accept]')!.click()
    await nextTick()
    expect(reconsented).toBe(1)
    expect(localStorage.getItem('identify:contrib-consent')).toBe(CONSENT_VERSION)
  })
})
