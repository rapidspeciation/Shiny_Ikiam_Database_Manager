import { afterEach, describe, expect, it, vi } from 'vitest'
import { createApp, defineComponent, h, nextTick } from 'vue'
import { createPinia, setActivePinia } from 'pinia'
import { t, tn } from '../../../lib/i18n'

// A sheet's matches on a phone: lines to tap instead of a grid of small cells. The matches beside the first one come
// with the answer; the others are read by ranges, the newest first.

const KEYS = ['Insectary_ID', 'SPECIES', 'Sex', 'Notes_Insectary_data']
const wire = (row: number, id: string, note = '') => ({ id: `rec${row}`, row, version: 1, observed: true, v: [id, 'Mechanitis lysimnia', 'female', note], f: [] })
/** The rows the server has, by row number. */
const SHEET = new Map<number, ReturnType<typeof wire>>()
const asked: string[] = []
vi.mock('../../../lib/api', () => ({
  api: async (path: string) => {
    const query = new URLSearchParams(String(path).split('?')[1])
    if (!String(path).startsWith('search/rows')) return {}
    asked.push(`${query.get('from')}-${query.get('to')}`)
    const [from, to] = [Number(query.get('from')), Number(query.get('to'))]
    return { module: 'Insectary_data', from, to, rows: [...SHEET.values()].filter(r => r.row >= from && r.row <= to) }
  },
}))
// The grid is the other view: here a box that says it was built.
vi.mock('../../SheetGrid.vue', () => ({ default: defineComponent({ setup: () => () => h('div', { class: 'grid-stub' }) }) }))
// The row's form: a box with the row it was opened for and a way to close it.
vi.mock('../../RowDrawer.vue', () => ({
  default: defineComponent({
    props: { rowId: String, rows: Array },
    emits: ['close'],
    setup: (props, { emit }) => () => h('div', { class: 'drawer-stub', 'data-row-id': props.rowId, onClick: () => emit('close') }),
  }),
}))

let unmount = () => {}
afterEach(() => {
  unmount()
  SHEET.clear()
  asked.length = 0
  document.body.innerHTML = ''
  vi.unstubAllGlobals()
})
const settle = async () => {
  for (let i = 0; i < 12; i++) await nextTick()
  await new Promise(r => setTimeout(r, 0))
  for (let i = 0; i < 4; i++) await nextTick()
}

async function mount(matches: number[], list: boolean, events: boolean[] = []) {
  // No observer: a section is near the screen at once.
  vi.stubGlobal('IntersectionObserver', undefined)
  for (const row of matches) SHEET.set(row, wire(row, `Q${row}E`, row === 500 ? 'encontrada en la jaula 3' : ''))
  const focus = matches[matches.length - 1]
  SHEET.set(focus - 1, wire(focus - 1, 'vecina'))
  const pinia = createPinia()
  setActivePinia(pinia)
  const { useSession } = await import('../../../stores/session')
  const session = useSession()
  session.modules = [
    {
      id: 'Insectary_data',
      label: 'Insectary_data',
      group: 'insectary',
      sheet: 'Insectary_data',
      sheetId: 1,
      headerRow: 1,
      fields: KEYS.map(key => ({ key, label: key, type: 'text' as const })),
      identityFields: ['Insectary_ID'],
      recordCount: 13698,
    },
  ]
  const { default: SearchSection } = await import('../SearchSection.vue')
  const result = {
    module: 'Insectary_data',
    total: matches.length,
    exact: 0,
    idExact: 0,
    columns: ['Notes_Insectary_data', 'Insectary_ID'],
    matches,
    truncated: false,
    focus,
    first: 2,
    last: 13698,
    unavailable: [],
    // The rows beside the match the sheet opens at.
    window: { from: focus - 12, to: focus + 12, rows: [...SHEET.values()].filter(r => Math.abs(r.row - focus) <= 12).sort((a, b) => a.row - b.row) },
  }
  const host = document.createElement('div')
  document.body.append(host)
  const app = createApp({ render: () => h(SearchSection, { result, query: 'jaula', pinned: true, list, onForm: (open: boolean) => events.push(open) }) })
  app.use(pinia)
  app.config.globalProperties.$t = t
  app.config.globalProperties.$tn = tn
  app.mount(host)
  unmount = () => app.unmount()
  await settle()
  return host
}
const titles = (host: HTMLElement) => [...host.querySelectorAll('.row-item .row-title')].map(e => e.textContent!.trim())

describe("a sheet's matches as a list (a phone)", () => {
  it('lists the matches, not their neighbours: the one the sheet opens at first, then the newest', async () => {
    const host = await mount([10, 500, 9000], true)
    expect(host.querySelector('.grid-stub')).toBeNull()
    expect(titles(host)).toEqual(['Q9000E', 'Q500E', 'Q10E'])
    // The two matches away from the first were read, each by its own short range.
    expect(asked.sort()).toEqual(['10-10', '500-500'])
    // No small stepping buttons: the list is the way to the other matches.
    expect(host.querySelector('header button[title="Coincidencia siguiente"]')).toBeNull()
    // The cell where the text is, marked.
    const match = host.querySelector('[data-row="500"] [data-match]')!
    expect(match.textContent).toContain('Notes_Insectary_data')
    expect(match.querySelector('mark')!.textContent).toBe('jaula')
  })
  it("a tap opens the row's form over the list, and tells the results to wait while it is open", async () => {
    const events: boolean[] = []
    const host = await mount([10, 500, 9000], true, events)
    host.querySelector<HTMLElement>('[data-row="500"]')!.click()
    await settle()
    const drawer = host.querySelector<HTMLElement>('.drawer-stub')!
    expect(drawer.dataset.rowId).toBe('rec500')
    expect(events).toEqual([true])
    expect(host.querySelector('[data-row="500"]')!.classList.contains('is-current')).toBe(true)
    drawer.click()
    await settle()
    expect(host.querySelector('.drawer-stub')).toBeNull()
    expect(events).toEqual([true, false])
  })
  it('reads many matches a screenful at a time, the rest on request', async () => {
    const many = Array.from({ length: 45 }, (_, i) => 100 + i * 200)
    const host = await mount(many, true)
    // The first one came with the answer; the 20 newest others were read.
    expect(titles(host)).toHaveLength(21)
    const more = host.querySelector<HTMLButtonElement>('[data-more-matches]')!
    expect(more.textContent).toContain('Ver 24 coincidencias más')
    expect(more.className).toContain('min-h-11')
    more.click()
    await settle()
    expect(titles(host)).toHaveLength(41)
    host.querySelector<HTMLButtonElement>('[data-more-matches]')!.click()
    await settle()
    expect(titles(host)).toHaveLength(45)
    expect(host.querySelector('[data-more-matches]')).toBeNull()
  })
  it('the grid is the other view (and the only one on a computer)', async () => {
    const host = await mount([10, 500, 9000], false)
    expect(host.querySelector('.grid-stub')).not.toBeNull()
    expect(host.querySelector('[data-match-list]')).toBeNull()
    expect(asked).toEqual([])
    expect(host.querySelector('header button[title="Coincidencia siguiente"]')).not.toBeNull()
  })
})
