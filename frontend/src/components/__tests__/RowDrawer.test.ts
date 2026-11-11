import { afterEach, describe, expect, it, vi } from 'vitest'
import { createApp, h, nextTick } from 'vue'
import { createPinia, setActivePinia } from 'pinia'
import { t } from '../../lib/i18n'
import type { Field, TableRow } from '../../lib/types'

vi.mock('../../stores/session', () => ({
  useSession: () => ({ canEdit: true, settings: null, module: () => ({ identityFields: ['Insectary_ID'], sheetId: 0 }) }),
}))
vi.mock('../../lib/api', () => ({ api: async () => ({}) }))
const RowDrawer = (await import('../RowDrawer.vue')).default
const { usePending } = await import('../../stores/pending')

const columns = [
  { key: 'Insectary_ID', type: 'text' },
  { key: 'Wild_Reared', type: 'text' },
  { key: 'Notes_Insectary_data', type: 'text' },
] as unknown as Field[]
const row = {
  id: 'r1',
  row: 13679,
  values: { Insectary_ID: 'P7E', Wild_Reared: 'Reared', Notes_Insectary_data: '' },
  formulas: [],
  version: 1,
  observed: false,
} as unknown as TableRow

let unmount = () => {}
afterEach(() => {
  unmount()
  document.body.innerHTML = ''
})

async function mount() {
  const pinia = createPinia()
  setActivePinia(pinia)
  const host = document.createElement('div')
  document.body.append(host)
  const app = createApp({
    render: () =>
      h(RowDrawer, {
        module: 'Insectary_data',
        rowId: 'r1',
        rows: [row],
        creates: [],
        columns,
        options: { Wild_Reared: ['Reared', 'Wild'] },
        lockedFields: ['Insectary_ID'],
        createFormulas: [],
      }),
  })
  app.use(pinia)
  app.config.globalProperties.$t = t
  app.config.globalProperties.$tn = (n: number, one: string) => t(one, { n })
  app.mount(host)
  unmount = () => app.unmount()
  await nextTick()
  return host
}
const alerts = (host: HTMLElement) => [...host.querySelectorAll('[role=alert]')].map(e => e.textContent?.trim())

describe('RowDrawer: a value that will not be saved', () => {
  it('says why under the field, without repeating its name', async () => {
    const host = await mount()
    expect(alerts(host)).toEqual([])
    usePending().problems['r1:Wild_Reared'] = 'Wild_Reared: «prueba» no está en la lista de la hoja (lista fija de la hoja)'
    usePending().errors['r1:Notes_Insectary_data'] = 'La hoja cambió: vuelve a escribirlo'
    await nextTick()
    // The note comes first in the form (right under the ID).
    expect(alerts(host)).toEqual(['La hoja cambió: vuelve a escribirlo', '«prueba» no está en la lista de la hoja (lista fija de la hoja)'])
  })
})

describe('RowDrawer: field order and empty fields', () => {
  it('shows the note right under the ID, labelled «Nota», once', async () => {
    const host = await mount()
    const labels = [...host.querySelectorAll('label > span:first-child')].map(e => e.textContent?.replace(/\s+/g, '').trim())
    expect(labels).toEqual(['Insectary_ID', `${t('Nota')}Notes_Insectary_data`, 'Wild_Reared'])
    expect(host.querySelectorAll('label').length).toBe(3)
  })

  it('gives an empty date field a format hint, not a date that looks real', async () => {
    const date = { key: 'Death_date', type: 'date' } as unknown as Field
    columns.push(date)
    try {
      const host = await mount()
      const input = [...host.querySelectorAll('input')].find(i => i.placeholder)
      expect(input?.placeholder).toBe(t('d-mmm-aa'))
      expect(input?.placeholder).not.toMatch(/\d/)
    } finally {
      columns.pop()
    }
  })
})

describe('RowDrawer: a locked empty field', () => {
  it('says «vacío» in the light placeholder style, not a blank line', async () => {
    row.values.Insectary_ID = ''
    try {
      const host = await mount()
      const locked = host.querySelector('label p')!
      expect(locked.textContent?.trim()).toBe(t('vacío'))
      expect(locked.querySelector('span')?.className).toContain('text-fg-subtle')
    } finally {
      row.values.Insectary_ID = 'P7E'
    }
  })
})
