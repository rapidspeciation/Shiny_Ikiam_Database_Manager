import { afterEach, describe, expect, it } from 'vitest'
import { createApp, h, nextTick, ref } from 'vue'
import RowList from '../RowList.vue'
import { locale, t, tn } from '../../../lib/i18n'
import type { RowListItem } from '../../../lib/rowList'

// The Buscador's rows on a phone: one finger-sized line per row, a tap opens it.
let unmount = () => {}
afterEach(() => {
  unmount()
  document.body.innerHTML = ''
  locale.value = 'en'
})

const item = (n: number, over: Partial<RowListItem> = {}): RowListItem => ({
  id: `r${n}`,
  row: n,
  title: `Q${n}E`,
  species: 'Mechanitis lysimnia',
  facts: ['female', 'clutch 1012(2)'],
  match: null,
  isNew: false,
  ...over,
})

function mount(props: Record<string, unknown>) {
  const host = document.createElement('div')
  document.body.append(host)
  const state = ref(props)
  const app = createApp({ render: () => h(RowList, state.value as never) })
  app.config.globalProperties.$t = t
  app.config.globalProperties.$tn = tn
  app.mount(host)
  unmount = () => app.unmount()
  return { host, state }
}

describe('RowList', () => {
  it('is one button per row, with its ID, species, facts and row number; a tap opens the row', () => {
    locale.value = 'es'
    const opened: string[] = []
    const { host } = mount({ items: [item(1), item(2)], onOpen: (id: string) => opened.push(id) })
    const rows = host.querySelectorAll<HTMLButtonElement>('button.row-item')
    expect(rows).toHaveLength(2)
    expect(rows[0].textContent).toContain('Q1E')
    expect(rows[0].querySelector('.row-species')!.textContent).toBe('Mechanitis lysimnia')
    expect(rows[0].querySelector('.row-facts')!.textContent).toBe('female · clutch 1012(2)')
    expect(rows[0].querySelector('.row-number')!.textContent).toBe('fila 1')
    // The whole line is the target (no small row number to hit).
    expect(rows[0].tagName).toBe('BUTTON')
    rows[1].click()
    expect(opened).toEqual(['r2'])
  })
  it('marks the searched text and shows the cell where it is', () => {
    const { host } = mount({
      items: [item(1, { match: { field: 'Notes_Insectary_data', text: '…en el piso de la jaula 3' } })],
      mark: 'jaula',
    })
    const match = host.querySelector('[data-match]')!
    expect(match.querySelector('.row-match-field')!.textContent).toBe('Notes_Insectary_data')
    expect(match.querySelector('mark')!.textContent).toBe('jaula')
    // An ID that is the searched text is marked in the title.
    unmount()
    const again = mount({ items: [item(5)], mark: 'q5e' })
    expect(again.host.querySelector('.row-title mark')!.textContent).toBe('Q5E')
  })
  it('draws the lines it is given; the rest is asked for with a finger-sized button', async () => {
    let asked = 0
    const { host, state } = mount({ items: [item(1), item(2)], left: 70, moreLabel: 'Mostrar 60 filas más · quedan 70', onMore: () => asked++ })
    expect(host.querySelectorAll('button.row-item')).toHaveLength(2)
    const more = host.querySelector<HTMLButtonElement>('[data-more]')!
    expect(more.textContent!.trim()).toBe('Mostrar 60 filas más · quedan 70')
    expect(more.className).toContain('min-h-11')
    more.click()
    expect(asked).toBe(1)
    // While more are read the button waits; with none left it goes. Lines already drawn stay as the list grows.
    state.value = { ...state.value, items: [item(1), item(2), item(3)], busy: true }
    await nextTick()
    expect(host.querySelectorAll('button.row-item')).toHaveLength(3)
    expect(host.querySelector<HTMLButtonElement>('[data-more]')!.disabled).toBe(true)
    state.value = { ...state.value, left: 0, busy: false }
    await nextTick()
    expect(host.querySelector('[data-more]')).toBeNull()
  })
  it('a species holding the searched text shows whole (never cut where the match is)', () => {
    const long = item(1, { species: 'Hypothyris euclea intermedia forma occidentalis' })
    const { host } = mount({ items: [long, item(2)], mark: 'occidentalis' })
    const [matched, other] = [...host.querySelectorAll('.row-species')]
    expect(matched.classList.contains('is-whole')).toBe(true)
    expect(matched.querySelector('mark')!.textContent).toBe('occidentalis')
    expect(other.classList.contains('is-whole')).toBe(false)
  })
  it('a row with edits not saved yet says so on its line', () => {
    locale.value = 'es'
    const { host } = mount({ items: [item(1, { dirty: true }), item(2)] })
    const [dirty, clean] = [...host.querySelectorAll('button.row-item')]
    expect(dirty.classList.contains('is-dirty')).toBe(true)
    expect(dirty.textContent).toContain('sin guardar')
    expect(clean.classList.contains('is-dirty')).toBe(false)
    expect(clean.textContent).not.toContain('sin guardar')
  })
  it('a row not saved yet says so, and one without an ID is still a line to open', () => {
    locale.value = 'es'
    const { host } = mount({ items: [item(0, { row: 0, title: '', species: '', facts: [], isNew: true })], current: 'r0' })
    const row = host.querySelector('button.row-item')!
    expect(row.classList.contains('is-new')).toBe(true)
    expect(row.classList.contains('is-current')).toBe(true)
    expect(row.textContent).toContain('Sin ID')
    expect(row.textContent).toContain('nueva')
    expect(row.querySelector('.row-number')).toBeNull()
  })
})
