import { afterEach, describe, expect, it, vi } from 'vitest'
import { createApp, defineComponent, h, nextTick, ref } from 'vue'
import { createPinia } from 'pinia'
import { t } from '../../../lib/i18n'

// The search answers, one per query, resolved by the test.
const answers = new Map<string, (reply: unknown) => void>()
vi.mock('../../../lib/search', () => ({
  MIN_QUERY: 2,
  searchSheets: (q: string) => new Promise(resolve => answers.set(q, resolve)),
}))
vi.mock('../../../stores/tables', () => ({ useTables: () => ({ load: async () => ({}) }) }))
// Each sheet's grid: a stub that shows the answer it was built from and can open or close its row form.
const sections: { query: string; form: (open: boolean) => void }[] = []
vi.mock('../SearchSection.vue', () => ({
  default: defineComponent({
    props: { result: Object, query: String, pinned: Boolean },
    emits: ['form', 'open', 'history'],
    setup(props, { emit }) {
      sections.push({ query: props.query!, form: open => emit('form', open) })
      return () => h('div', { class: 'section' }, props.query)
    },
  }),
}))
const SearchResults = (await import('../SearchResults.vue')).default

const reply = (q: string) => ({ query: q, took: 1, sheets: [{ module: 'Insectary_data' }] })
const settle = async () => {
  for (let i = 0; i < 4; i++) await nextTick()
}

let unmount = () => {}
afterEach(() => {
  unmount()
  answers.clear()
  sections.length = 0
  document.body.innerHTML = ''
})

async function mount(start: string) {
  const query = ref(start)
  const host = document.createElement('div')
  document.body.append(host)
  const app = createApp({ render: () => h(SearchResults, { query: query.value, pin: 'Insectary_data' }) })
  app.use(createPinia())
  app.config.globalProperties.$t = t
  app.config.globalProperties.$tn = (n: number, one: string) => t(one, { n })
  app.mount(host)
  unmount = () => app.unmount()
  return { query, shown: () => host.querySelector('.section')?.textContent }
}

describe('SearchResults while a row form is open', () => {
  it('keeps the grids until the form is closed, then shows the newer answer', async () => {
    const view = await mount('P4E')
    answers.get('P4E')!(reply('P4E'))
    await settle()
    expect(view.shown()).toBe('P4E')

    // A row of the shown results is opened, then the search box's delayed search answers.
    sections[0].form(true)
    view.query.value = 'P5E'
    await settle()
    answers.get('P5E')!(reply('P5E'))
    await settle()
    expect(view.shown()).toBe('P4E')
    expect(sections).toHaveLength(1)

    sections[0].form(false)
    await settle()
    expect(view.shown()).toBe('P5E')
  })

  it('shows an answer at once when no form is open', async () => {
    const view = await mount('P4E')
    answers.get('P4E')!(reply('P4E'))
    await settle()
    view.query.value = 'P6E'
    await settle()
    answers.get('P6E')!(reply('P6E'))
    await settle()
    expect(view.shown()).toBe('P6E')
  })
})
