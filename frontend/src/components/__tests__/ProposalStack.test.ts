import { afterEach, describe, expect, it } from 'vitest'
import { createApp, h, nextTick } from 'vue'
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

function mount(rows: ProposalChange[], onCell?: (key: string, field: string) => void, extra: Record<string, unknown> = {}) {
  const host = document.createElement('div')
  document.body.append(host)
  const app = createApp({ render: () => h(ProposalStack, { rows, fields: FIELDS, types: TYPES, onCell, ...extra }) })
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
  it('says a note all the rows share once, above them; a row\'s own note stays on the row', async () => {
    locale.value = 'es'
    const shared = mount([
      change('a', { Sex: 'female' }, { note: 'Cuaderno de muertes, p. 41' }),
      change('b', { Sex: 'female' }, { label: 'Q6E', note: 'Cuaderno de muertes, p. 41' }),
    ])
    expect(shared.querySelector('[data-shared-note]')!.textContent).toBe('Cuaderno de muertes, p. 41')
    expect(shared.querySelectorAll('.stack-row [data-note]')).toHaveLength(0)
    expect(shared.textContent!.split('Cuaderno de muertes, p. 41')).toHaveLength(2)
    unmount()
    const own = mount([change('a', { Sex: 'female' }, { note: 'Línea 3' }), change('b', { Sex: 'female' }, { label: 'Q6E', note: 'Línea 4' })])
    expect(own.querySelector('[data-shared-note]')).toBeNull()
    expect([...own.querySelectorAll('.stack-row [data-note]')].map(e => e.textContent!.trim())).toEqual(['Línea 3', 'Línea 4'])
  })
  it('a row that is not written says so, with why, and shows no value as if it were', () => {
    locale.value = 'es'
    const host = mount([
      {
        index: 3,
        key: 't',
        clientId: 't',
        recordId: null,
        sheet: 'Insectary_data',
        row: null,
        label: 'S2E',
        create: true,
        values: { Sex: 'female' },
        rowTaken: { row: 13699 } as ProposalChange['rowTaken'],
      },
    ])
    const row = host.querySelector('[data-row="t"]')!
    expect(row.classList.contains('is-refused')).toBe(true)
    expect(row.textContent).toContain('no se escribe')
    expect(row.textContent).toContain('Su fila sin usar (13699) ya se usó en la hoja')
    expect(row.textContent).not.toContain('fila nueva')
    expect(row.querySelector('.stack-field')).toBeNull()
  })
  it("folds the template's columns as the table does: counted on the row, opened on request, written all the same", async () => {
    locale.value = 'es'
    const host = mount(
      [{ index: 1, key: 'n', clientId: 'n', recordId: null, sheet: 'Insectary_data', row: null, label: 'S2E', create: true, values: { Sex: 'female', Death_cause: 'NA', Notes_Insectary_data: 'NOT_COLLECTED' } }],
      undefined,
      { template: ['Death_cause', 'Notes_Insectary_data'] },
    )
    const cells = () => [...host.querySelectorAll<HTMLElement>('.stack-field')].map(f => f.dataset.field)
    expect(cells()).toEqual(['Sex'])
    const fold = host.querySelector<HTMLButtonElement>('[data-template]')!
    expect(fold.textContent).toContain('+2 columnas de plantilla')
    expect(fold.getAttribute('aria-expanded')).toBe('false')
    fold.click()
    await nextTick()
    expect(cells()).toEqual(['Death_cause', 'Notes_Insectary_data', 'Sex'].sort((a, b) => FIELDS.indexOf(a) - FIELDS.indexOf(b)))
    expect(host.querySelector('[data-template]')!.textContent).toContain('Plegar la plantilla')
  })
  it('a fold every row shares is said once above the rows, not on each', async () => {
    locale.value = 'es'
    const row = (key: string): ProposalChange => ({ index: 1, key, clientId: key, recordId: null, sheet: 'Insectary_data', row: null, label: key, create: true, values: { Sex: 'female', Death_cause: 'NA' } })
    const host = mount([row('a'), row('b'), row('c')], undefined, { template: ['Death_cause'] })
    const folds = host.querySelectorAll<HTMLButtonElement>('[data-template]')
    expect(folds).toHaveLength(1)
    expect(folds[0].closest('.stack-row')).toBeNull()
    expect(folds[0].textContent).toContain('Cada fila: +1 columna de plantilla')
    expect(host.querySelectorAll('.stack-field')).toHaveLength(3)
    folds[0].click()
    await nextTick()
    expect(host.querySelectorAll('.stack-field')).toHaveLength(6)
  })
  it('keeps the table\'s marks and reads dates in the interface language', () => {
    locale.value = 'es'
    const host = mount([change('a', { Death_date: 46128, Death_cause: 'Natural_death' }, { inferred: ['Death_cause'], highlight: true, rowValues: { Death_date: 45883, Death_cause: '' } })])
    const row = host.querySelector('[data-row="a"]')!
    expect(row.classList.contains('is-marked')).toBe(true)
    expect(row.textContent).toContain('marcada')
    expect(row.querySelector('[data-field="Death_cause"] .stack-tag')!.textContent).toBe('deducida')
    // 14 Aug 2025 → 16 Apr 2026: Spanish months here; the table keeps the sheet's own.
    expect(row.querySelector('[data-field="Death_date"] [data-old] s')!.textContent).toBe('14-Ago-25')
    expect(row.querySelector('[data-field="Death_date"] [data-new] b')!.textContent).toBe('16-Abr-26')
    unmount()
    locale.value = 'en'
    const en = mount([change('a', { Death_date: 46128 }, { rowValues: { Death_date: 45883 } })])
    expect(en.querySelector('[data-new] b')!.textContent).toBe('16-Apr-26')
  })
  it('says so when a table changes nothing, in English too', () => {
    locale.value = 'en'
    const host = mount([change('a', { Sex: 'male' })])
    expect(host.querySelector('.stack-row')).toBeNull()
    expect(host.textContent).toContain('This table changes no cells.')
  })
})
