import { afterEach, describe, expect, it } from 'vitest'
import { createApp, h } from 'vue'
import ProposalStack from '../assistant/ProposalStack.vue'
import { locale, t, tn } from '../../lib/i18n'
import type { ProposalChange } from '../../lib/proposals'

// Cambios propuestos on a phone: every changed cell of every row on the screen, old over new, nothing sideways.
let unmount = () => {}
afterEach(() => {
  unmount()
  document.body.innerHTML = ''
  locale.value = 'en'
})

const change = (key: string, values: ProposalChange['values'], extra: Partial<ProposalChange> = {}): ProposalChange => ({
  index: 0,
  key,
  recordId: key,
  sheet: 'Insectary_data',
  row: 13687,
  label: 'Q5E',
  values,
  rowValues: { Death_date: 46300, Death_cause: 'Killed_Preserved', Notes_Insectary_data: '', Sex: 'male' },
  ...extra,
})
const FIELDS = ['Death_date', 'Death_cause', 'Notes_Insectary_data', 'Sex']
const TYPES = { Death_date: 'date', Death_cause: 'text', Notes_Insectary_data: 'text', Sex: 'text' }

function mount(rows: ProposalChange[], onCell?: (key: string, field: string) => void) {
  const host = document.createElement('div')
  document.body.append(host)
  const app = createApp({ render: () => h(ProposalStack, { rows, fields: FIELDS, types: TYPES, onCell }) })
  app.config.globalProperties.$t = t
  app.config.globalProperties.$tn = tn
  app.mount(host)
  unmount = () => app.unmount()
  return host
}

describe('ProposalStack', () => {
  it('shows each changed cell with the sheet\'s value over the new one, all three columns of the row', () => {
    locale.value = 'es'
    const host = mount([
      change('a', { Death_date: 46347, Death_cause: 'Natural_death', Notes_Insectary_data: 'Ala rota' }, { note: 'Cuaderno de muertes, p. 41' }),
    ])
    const row = host.querySelector('[data-row="a"]')!
    expect(row.textContent).toContain('Q5E')
    expect(row.textContent).toContain('fila 13687')
    expect(row.textContent).toContain('Cuaderno de muertes, p. 41')
    const fields = [...row.querySelectorAll<HTMLElement>('.stack-field')]
    expect(fields.map(f => f.dataset.field)).toEqual(['Death_date', 'Death_cause', 'Notes_Insectary_data'])
    // Old over new: the sheet's date struck, the assistant's under it, as the grid writes dates.
    const [date, cause, note] = fields
    expect(date.querySelector('[data-old] s')!.textContent).toBe('5-Oct-26')
    expect(date.querySelector('[data-new] b')!.textContent).toBe('21-Nov-26')
    expect(date.querySelector('[data-old]')!.compareDocumentPosition(date.querySelector('[data-new]')!) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy()
    expect(cause.querySelector('[data-old] s')!.textContent).toBe('Killed_Preserved')
    expect(cause.querySelector('[data-new] b')!.textContent).toBe('Natural_death')
    // An empty cell says so, in both places.
    expect(note.querySelector('[data-old] i')!.textContent).toBe('vacío')
    expect(note.querySelector('[data-new] b')!.textContent).toBe('Ala rota')
    // Whose value it is, once (beside the value), in the table's colour.
    expect(date.classList.contains('is-ai')).toBe(true)
    expect(date.querySelector('.stack-tag')).toBeNull()
    expect(date.querySelector('[data-new] .stack-side')!.textContent).toBe('IA')
    // Nothing to tap: a plain cell is not a button.
    expect(date.tagName).toBe('DIV')
  })
  it('a new row has no sheet value to show', () => {
    locale.value = 'es'
    const host = mount([{ index: 1, key: 'n', clientId: 'n', recordId: null, sheet: 'Insectary_data', row: null, label: 'S2E', create: true, values: { Sex: 'female' } }])
    const row = host.querySelector('[data-row="n"]')!
    expect(row.textContent).toContain('fila nueva')
    expect(row.querySelector('[data-old]')).toBeNull()
    expect(row.querySelector('[data-new] b')!.textContent).toBe('female')
  })
  it('a doubtful or unreadable cell is a finger-sized button that opens it in the table', () => {
    locale.value = 'es'
    const opened: string[] = []
    const host = mount(
      [
        change('d', { Sex: 'female' }, { doubts: { Sex: { reason: 'tinta corrida' } } }),
        change('u', {}, { label: 'Q6E', unreadable: { Sex: { reason: 'mancha de agua' } } }),
        change('p', { Sex: 'female' }, { label: 'Q7E', personEdits: { Sex: {} } }),
      ],
      (key, field) => opened.push(`${key}:${field}`),
    )
    const doubtful = host.querySelector<HTMLElement>('[data-row="d"] .stack-field')!
    expect(doubtful.tagName).toBe('BUTTON')
    expect(doubtful.dataset.state).toBe('doubtful')
    expect(doubtful.textContent).toContain('dudosa')
    expect(doubtful.textContent).toContain('tinta corrida')
    expect(doubtful.textContent).toContain('Revisarla en la tabla')
    doubtful.click()
    const unreadable = host.querySelector<HTMLElement>('[data-row="u"] .stack-field')!
    expect(unreadable.textContent).toContain('ilegible')
    expect(unreadable.textContent).toContain('Escribirla en la tabla')
    unreadable.click()
    expect(opened).toEqual(['d:Sex', 'u:Sex'])
    // The person's own value says «tú», and is not a button.
    const typed = host.querySelector<HTMLElement>('[data-row="p"] .stack-field')!
    expect(typed.tagName).toBe('DIV')
    expect(typed.querySelector('[data-new] .stack-side')!.textContent).toBe('tú')
  })
  it('a cell edited in the sheet keeps the sheet\'s value and shows the proposal\'s set aside', () => {
    locale.value = 'es'
    const host = mount([change('k', { Sex: 'female' }, { sheetChanged: { Sex: { read: 'male', now: 'unknown' } } })])
    const cell = host.querySelector<HTMLElement>('.stack-field')!
    expect(cell.dataset.state).toBe('kept')
    expect(cell.querySelector('[data-new] b')!.textContent).toBe('unknown')
    expect(cell.querySelector('[data-aside] s')!.textContent).toBe('female')
    expect(cell.textContent).toContain('editada en la hoja')
  })
  it('says so when a table changes nothing, in English too', () => {
    locale.value = 'en'
    const host = mount([change('a', { Sex: 'male' })])
    expect(host.querySelector('.stack-row')).toBeNull()
    expect(host.textContent).toContain('This table changes no cell.')
  })
})
