import { afterEach, describe, expect, it, vi } from 'vitest'
import { createApp, h, nextTick } from 'vue'
import { t, tn } from '../../lib/i18n'

// «Photo_links y sitios» of the «Fotos» tab: the folders «Ya las subí» names are read once, when the part opens.

const calls: { path: string; body?: unknown }[] = []
vi.mock('../../lib/api', () => ({
  api: async (path: string, options: { body?: unknown } = {}) => {
    calls.push({ path, body: options.body })
    if (path === 'photo-links/folders') return { folders: [{ path: 'Brasil_2025', link: '', files: 9 }] }
    if (path === 'photo-links/read') return { proposalId: '00000000-0000-4000-8000-000000000001', upToDate: false, notFound: [] }
    if (path.startsWith('sites')) return { sites: [], ready: true }
    return {}
  },
}))
vi.mock('../assistant/ProposalsLive.vue', () => ({ __esModule: true, __v_isVNode: false, default: { props: ['only'], render: () => h('div', { 'data-card': '' }) } }))

let unmount = () => {}
afterEach(() => {
  unmount()
  calls.length = 0
  document.body.innerHTML = ''
  localStorage.clear()
})

async function mount(preset: { folders: string[]; at: number } | null) {
  const { default: PhotoLinksPanel } = await import('../PhotoLinksPanel.vue')
  const used: number[] = []
  const host = document.createElement('div')
  document.body.append(host)
  const app = createApp({ render: () => h(PhotoLinksPanel, { preset, onUsed: () => used.push(1) }) })
  app.config.globalProperties.$t = t
  app.config.globalProperties.$tn = tn
  app.mount(host)
  unmount = () => app.unmount()
  for (let i = 0; i < 5; i++) await nextTick()
  await new Promise(resolve => setTimeout(resolve, 0))
  return { host, used }
}

describe('PhotoLinksPanel', () => {
  it('reads the folders it is opened with, once, and says it used them', async () => {
    const { used, host } = await mount({ folders: ['Brasil_2025/JPG', 'Brasil_2025/RAW'], at: 1 })
    const reads = calls.filter(c => c.path === 'photo-links/read')
    expect(reads).toEqual([{ path: 'photo-links/read', body: { only: ['Brasil_2025/JPG', 'Brasil_2025/RAW'], add: [] } }])
    expect(used).toEqual([1])
    expect(localStorage.getItem('photo-links:card')).toBe('00000000-0000-4000-8000-000000000001')
    expect(host.querySelector('[data-card]')).not.toBeNull()
  })

  it('opened without folders, reads nothing until asked', async () => {
    const { used } = await mount(null)
    expect(calls.filter(c => c.path === 'photo-links/read')).toEqual([])
    expect(used).toEqual([])
    expect(calls.map(c => c.path)).toContain('photo-links/folders')
  })
})
