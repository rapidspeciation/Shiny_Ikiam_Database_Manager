import { afterEach, describe, expect, it, vi } from 'vitest'
import { createApp, defineComponent, h, nextTick } from 'vue'
import { createPinia, setActivePinia } from 'pinia'
import { locale, t, tn } from '../../lib/i18n'
import type { Proposal } from '../../lib/proposals'

// Cambios propuestos on a phone: the stacked list first (every change before «Aplicar»), the table to edit, and
// «Aplicar» / «Descartar» a finger can hit. A computer keeps the table.

vi.mock('../../lib/api', () => ({ api: async () => ({}), learnItems: () => {} }))
/** The table (Tabulator) stands in as a box that says which cell it was asked to show. */
const focused: string[] = []
vi.mock('../assistant/ProposalSheet.vue', () => ({
  default: defineComponent({
    props: { sheet: String, changes: Array, fields: Array },
    setup(props, { expose }) {
      expose({ focusCell: (key: string, field: string) => (focused.push(`${key}:${field}`), true) })
      return () => h('div', { class: 'sheet-stub', 'data-sheet': props.sheet })
    },
  }),
}))

let unmount = () => {}
afterEach(() => {
  unmount()
  focused.length = 0
  document.body.innerHTML = ''
  localStorage.clear()
  sessionStorage.clear()
  vi.unstubAllGlobals()
  locale.value = 'en'
})

const proposal = (over: Partial<Proposal> = {}): Proposal =>
  ({
    id: 'p1',
    status: 'pending',
    revision: 1,
    reason: 'Muertes del cuaderno p. 41',
    fields: ['Death_date', 'Death_cause', 'Notes_Insectary_data', 'Sex'],
    types: { Death_date: 'date' },
    sheets: ['Insectary_data'],
    changes: [
      {
        index: 0,
        key: 'a',
        recordId: 'a',
        sheet: 'Insectary_data',
        row: 13687,
        label: 'Q5E',
        values: { Death_date: 46347, Death_cause: 'Natural_death', Notes_Insectary_data: 'Ala rota' },
        rowValues: { Death_date: 46300, Death_cause: 'Killed_Preserved', Notes_Insectary_data: '' },
      },
      {
        index: 1,
        key: 'b',
        recordId: 'b',
        sheet: 'Insectary_data',
        row: 13688,
        label: 'Q6E',
        values: { Sex: 'female' },
        rowValues: { Sex: 'male' },
        doubts: { Sex: { reason: 'tinta corrida' } },
      },
    ],
    ...over,
  }) as unknown as Proposal

async function mount(phone: boolean, props: Record<string, unknown> = {}) {
  vi.stubGlobal('matchMedia', (query: string) => ({
    matches: phone && /max-width: 639/.test(query),
    addEventListener() {},
    removeEventListener() {},
  }))
  const pinia = createPinia()
  setActivePinia(pinia)
  const { useSession } = await import('../../stores/session')
  const session = useSession()
  session.user = { id: 7, username: 'lab', displayName: 'Lab', role: 'admin' } as never
  const { default: ProposalGrid } = await import('../ProposalGrid.vue')
  const host = document.createElement('div')
  document.body.append(host)
  const app = createApp({ render: () => h(ProposalGrid, { proposal: proposal(), ...props }) })
  app.use(pinia)
  app.config.globalProperties.$t = t
  app.config.globalProperties.$tn = tn
  app.mount(host)
  unmount = () => app.unmount()
  await nextTick()
  return host
}

describe('a proposal on a phone', () => {
  it('opens as the stacked list: every changed cell of every row, no table to scroll sideways', async () => {
    locale.value = 'es'
    const host = await mount(true)
    expect(host.querySelector('[data-proposal-stack]')).not.toBeNull()
    expect(host.querySelector('.sheet-stub')).toBeNull()
    const cells = [...host.querySelectorAll<HTMLElement>('.stack-field')].map(f => f.dataset.field)
    expect(cells).toEqual(['Death_date', 'Death_cause', 'Notes_Insectary_data', 'Sex'])
    const buttons = [...host.querySelectorAll<HTMLElement>('[data-phone-view] button')]
    expect(buttons.map(b => `${b.dataset.view}:${b.getAttribute('aria-pressed')}`)).toEqual(['list:true', 'table:false'])
    expect(buttons[1].textContent).toContain('Editar en tabla')
  })
  it('«Aplicar» and «Descartar» are finger-sized and say how many rows', async () => {
    locale.value = 'es'
    const applied: unknown[] = []
    let discarded = 0
    const host = await mount(true, { onApply: (...args: unknown[]) => applied.push(args), onDiscard: () => discarded++ })
    const [apply, discard] = [...host.querySelectorAll<HTMLButtonElement>('[data-apply-bar] button')]
    expect(apply.textContent).toContain('Aplicar 2 filas')
    expect(apply.className).toContain('max-sm:min-h-12')
    expect(apply.className).toContain('max-sm:flex-1')
    expect(discard.textContent).toContain('Descartar')
    expect(discard.className).toContain('max-sm:min-h-12')
    discard.click()
    expect(discarded).toBe(1)
    // A doubtful cell nobody reviewed: «Aplicar» asks first, as from the table.
    apply.click()
    for (let i = 0; i < 6; i++) await nextTick()
    expect(applied).toHaveLength(0)
    expect(host.querySelector('[role="alertdialog"]')!.textContent).toContain('1 celda dudosa sin revisar')
  })
  it('a cell that waits for a decision opens in the table, which then stays the view', async () => {
    locale.value = 'es'
    const host = await mount(true)
    host.querySelector<HTMLElement>('button.stack-field[data-field="Sex"]')!.click()
    for (let i = 0; i < 4; i++) await nextTick()
    expect(host.querySelector('.sheet-stub')).not.toBeNull()
    expect(host.querySelector('[data-proposal-stack]')).toBeNull()
    expect(focused).toEqual(['b:Sex'])
    // A jump is for this card, now: the view kept on the device is still the list (the next proposal opens as it).
    expect(localStorage.getItem('ithomiini:proposal-phone-view')).toBeNull()
    expect([...host.querySelectorAll<HTMLElement>('[data-phone-view] button')].map(b => b.getAttribute('aria-pressed'))).toEqual(['false', 'true'])
    // Back to the list with its switch.
    host.querySelector<HTMLElement>('[data-phone-view] [data-view="list"]')!.click()
    await nextTick()
    expect(host.querySelector('[data-proposal-stack]')).not.toBeNull()
    // The switch itself is the choice that is kept.
    host.querySelector<HTMLElement>('[data-phone-view] [data-view="table"]')!.click()
    await nextTick()
    expect(JSON.parse(localStorage.getItem('ithomiini:proposal-phone-view')!)).toBe('table')
  })
  it("the list has every column «Aplicar» writes, the folded template's too", async () => {
    locale.value = 'es'
    const paged = proposal({
      fields: ['Insectary_ID', 'Sex', 'Stock_of_origin'],
      page: { sheet: 'Insectary_data', photos: 0, keys: ['Insectary_ID'], columns: ['Insectary_ID', 'Sex'] } as never,
      changes: [
        { index: 0, key: 'n', clientId: 'n', recordId: null, sheet: 'Insectary_data', row: null, label: 'S2E', create: true, values: { Insectary_ID: 'S2E', Sex: 'female', Stock_of_origin: 'NA' } },
      ] as never,
    })
    const host = await mount(true, { proposal: paged })
    // Stock_of_origin only carries the template's NA: folded in the table, counted here and opened with a tap.
    expect([...host.querySelectorAll<HTMLElement>('.stack-field')].map(f => f.dataset.field)).toEqual(['Insectary_ID', 'Sex'])
    const fold = host.querySelector<HTMLButtonElement>('[data-template]')!
    expect(fold.textContent).toContain('+1 columna de plantilla')
    fold.click()
    await nextTick()
    expect([...host.querySelectorAll<HTMLElement>('.stack-field')].map(f => f.dataset.field)).toContain('Stock_of_origin')
  })
  it('beside its photo the table stays (the rows follow the page there)', async () => {
    const host = await mount(true, { reviewing: true })
    expect(host.querySelector('[data-phone-view]')).toBeNull()
    expect(host.querySelector('.sheet-stub')).not.toBeNull()
  })
})

describe('a proposal on a computer', () => {
  it('is the table, with no switch', async () => {
    const host = await mount(false)
    expect(host.querySelector('[data-phone-view]')).toBeNull()
    expect(host.querySelector('[data-proposal-stack]')).toBeNull()
    expect(host.querySelector('.sheet-stub')).not.toBeNull()
  })
})
