import { afterEach, describe, expect, it } from 'vitest'
import { createApp, h } from 'vue'
import SheetApply from '../assistant/SheetApply.vue'
import PhotoThumbs from '../assistant/PhotoThumbs.vue'
import { locale, t, tn } from '../../lib/i18n'
import { readOnlyRow, rowsToWrite, sheetApplies, type ProposalChange } from '../../lib/proposals'
import { tablePhotos } from '../../lib/rowsTable'

let unmount = () => {}
afterEach(() => {
  unmount()
  document.body.innerHTML = ''
  locale.value = 'es'
})

function mount(component: object, props: Record<string, unknown>) {
  const host = document.createElement('div')
  document.body.append(host)
  const app = createApp({ render: () => h(component, props) })
  app.config.globalProperties.$t = t
  app.config.globalProperties.$tn = tn
  app.mount(host)
  unmount = () => app.unmount()
  return host
}

/** A notebook page's two Insectary_data rows and a new Collection_data row for a wild-caught butterfly. */
const row = (index: number, sheet: string, extra: Partial<ProposalChange> = {}) =>
  ({ index, key: `k${index}`, recordId: `r${index}`, sheet, row: index + 2, label: `A${index}T`, values: { Sex: 'male' }, ...extra }) as ProposalChange

describe('a proposal with rows of two sheets, applied one sheet at a time', () => {
  const changes = [row(0, 'Insectary_data'), row(1, 'Insectary_data'), row(2, 'Collection_data', { create: true, recordId: null })]

  it('each sheet has its own rows to apply; one sheet alone has none of its own', () => {
    const per = sheetApplies(changes, rowsToWrite({ changes }))
    expect([...per.keys()]).toEqual(['Insectary_data', 'Collection_data'])
    expect(per.get('Insectary_data')).toEqual({ rows: [0, 1], written: 0 })
    expect(per.get('Collection_data')).toEqual({ rows: [2], written: 0 })
    expect(sheetApplies(changes.slice(0, 2), [0, 1]).size).toBe(0)
  })

  it('once one sheet is written: its rows read-only and not written again, the other still to apply', () => {
    const after = changes.map(c => (c.sheet === 'Insectary_data' ? { ...c, applied: 1 } : c))
    expect(rowsToWrite({ changes: after })).toEqual([2])
    expect(readOnlyRow(after[0])).toBe(true)
    expect(readOnlyRow(after[2])).toBe(false)
    const per = sheetApplies(after, rowsToWrite({ changes: after }))
    expect(per.get('Insectary_data')).toEqual({ rows: [], written: 2 })
    expect(per.get('Collection_data')).toEqual({ rows: [2], written: 0 })
  })

  it("a table's «Aplicar» says its rows and sheet and applies them; once written, says so", () => {
    let applied = 0
    const host = mount(SheetApply, { sheet: 'Collection_data', rows: 1, written: 0, onApply: () => applied++ })
    const button = host.querySelector('button')!
    expect(button.textContent).toContain('Aplicar 1 fila')
    expect(button.textContent).toContain('Collection_data')
    button.click()
    expect(applied).toBe(1)
    unmount()
    const done = mount(SheetApply, { sheet: 'Insectary_data', rows: 0, written: 2 })
    expect(done.querySelector('button')).toBeNull()
    expect(done.textContent).toContain('Insectary_data · 2 filas ya escritas en la hoja')
    locale.value = 'en'
    unmount()
    const en = mount(SheetApply, { sheet: 'Collection_data', rows: 3, written: 0 })
    expect(en.querySelector('button')!.textContent).toContain('Apply 3 rows')
  })
})

describe('the photos above a read-only table', () => {
  const rows = [
    { page: { photo: 0, line: 3 } },
    { page: { photo: 0, line: 7 } },
    { page: { photo: 1, line: 2 } },
    {},
  ]

  it('per photo: the lines of its rows and how many', () => {
    expect(tablePhotos(rows, 3)).toEqual([
      { photo: 0, from: 3, to: 7, rows: 2 },
      { photo: 1, from: 2, to: 2, rows: 1 },
      { photo: 2, from: 0, to: 0, rows: 0 },
    ])
  })

  it('the thumbnails as a proposal shows them: lines, rows and the note; a click opens the review on that photo', () => {
    const opened: number[] = []
    const host = mount(PhotoThumbs, {
      id: 'table-1',
      page: { photos: 2, photoKey: 'abc', photoNotes: ['Página 12', ''] },
      photos: tablePhotos(rows, 2),
      onPhoto: (n: number) => opened.push(n),
    })
    const thumbs = host.querySelectorAll('[data-photo-thumb]')
    expect(thumbs).toHaveLength(2)
    expect(thumbs[0].textContent).toContain('Foto 1 · Página 12 · Líneas 3–7 · 2 filas')
    expect(thumbs[1].textContent).toContain('Foto 2 · Líneas 2–2 · 1 fila')
    expect(host.querySelector('img')!.getAttribute('src')).toBe('api/proposals/table-1/photos/0?size=thumb&v=abc')
    ;(host.querySelectorAll('a')[1] as HTMLAnchorElement).click()
    expect(opened).toEqual([1])
  })
})
