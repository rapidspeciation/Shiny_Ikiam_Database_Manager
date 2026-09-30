import { afterEach, describe, expect, it } from 'vitest'
import { createApp, h, nextTick, ref } from 'vue'
import CellBar from '../CellBar.vue'
import type { CellBarInfo } from '../../lib/gridKit'
import { t } from '../../lib/i18n'

const note: CellBarInfo = {
  index: '00R',
  field: 'Notes_Insectary_data',
  column: 'Notes_Insectary_data',
  row: '00R',
  text: '12/10/24 FCH: dead pupa',
  editable: true,
  multiline: true,
}

let unmount = () => {}
afterEach(() => {
  unmount()
  document.body.innerHTML = ''
})

/** Mounts a bar showing `start`; returns its text box, what it asked the grid, and the shown cell. */
async function mount(start: CellBarInfo | null) {
  const info = ref<CellBarInfo | null>(start)
  const saved: [string, string, unknown][] = []
  const back: unknown[] = []
  const host = document.createElement('div')
  document.body.append(host)
  const app = createApp({
    render: () =>
      h(CellBar, {
        info: info.value,
        onSave: (target: CellBarInfo, text: string, move: unknown) => saved.push([target.index, text, move]),
        onBack: (move: unknown) => back.push(move),
      }),
  })
  app.config.globalProperties.$t = t
  app.mount(host)
  unmount = () => app.unmount()
  await nextTick()
  return { box: host.querySelector('textarea')!, where: host.querySelector('.cell-bar-where')!, info, saved, back }
}
const press = (box: HTMLElement, key: string, mods: KeyboardEventInit = {}) =>
  box.dispatchEvent(new KeyboardEvent('keydown', { key, bubbles: true, cancelable: true, ...mods }))
function type(box: HTMLTextAreaElement, text: string) {
  box.value = text
  box.dispatchEvent(new Event('input'))
}

describe('the cell bar', () => {
  it('shows the cell (column · row) and its whole text', async () => {
    const { box, where } = await mount(note)
    expect(where.textContent).toBe('Notes_Insectary_data · 00R')
    expect(box.value).toBe('12/10/24 FCH: dead pupa')
    expect(box.readOnly).toBe(false)
  })
  it('saves the typed text with Enter and moves down', async () => {
    const { box, saved } = await mount(note)
    box.focus()
    type(box, '12/10/24 FCH: dead pupa | 30/9/26 L: pupa muerta')
    press(box, 'Enter')
    expect(saved).toEqual([['00R', '12/10/24 FCH: dead pupa | 30/9/26 L: pupa muerta', 'down']])
  })
  it('gives the change up with Esc and goes back to the grid', async () => {
    const { box, saved, back } = await mount(note)
    box.focus()
    type(box, 'something else')
    press(box, 'Escape')
    await nextTick()
    expect(saved).toEqual([])
    expect(back).toEqual(['here'])
    expect(box.value).toBe(note.text)
  })
  it('saves on leaving the bar, into the cell it was typed for even if the selection moved', async () => {
    const { box, saved, info } = await mount(note)
    box.focus()
    type(box, 'pupa muerta')
    // A click on another cell moves the selection before the bar loses the focus.
    info.value = { ...note, index: '00S', row: '00S', text: '' }
    await nextTick()
    expect(box.value).toBe('pupa muerta')
    box.blur()
    expect(saved).toEqual([['00R', 'pupa muerta', null]])
  })
  it('breaks the line with Shift+Enter in a notes column; elsewhere Enter saves', async () => {
    const { box, saved, info } = await mount(note)
    box.focus()
    box.setSelectionRange(box.value.length, box.value.length)
    press(box, 'Enter', { shiftKey: true })
    expect(box.value).toBe('12/10/24 FCH: dead pupa\n')
    expect(saved).toEqual([])
    box.blur()
    info.value = { ...note, field: 'Insectary_ID', column: 'Insectary_ID', text: '00R', multiline: false }
    await nextTick()
    box.focus()
    type(box, '00T')
    press(box, 'Enter', { shiftKey: true })
    expect(saved.at(-1)).toEqual(['00R', '00T', 'up'])
  })
  it('does not save an unchanged text, nor a read-only cell', async () => {
    const { box, saved, back, info } = await mount(note)
    box.focus()
    press(box, 'Enter')
    expect(saved).toEqual([])
    expect(back).toEqual(['down'])
    info.value = { ...note, editable: false, readonly: 'Fórmula de la hoja (solo lectura)' }
    await nextTick()
    expect(box.readOnly).toBe(true)
    box.focus()
    press(box, 'Enter')
    expect(saved).toEqual([])
  })
  it('shows the lines under the text (the sheet, the assistant, a total)', async () => {
    const { box } = await mount({
      ...note,
      notes: [
        { label: 'Hoja', text: 'dead pupa', kind: 'sheet' },
        { label: 'IA', text: 'pupa muerta', kind: 'ai' },
      ],
    })
    expect(box.parentElement!.querySelector('.cell-bar-notes')!.textContent).toContain('Hoja: dead pupa')
    expect(box.parentElement!.querySelector('.cell-bar-notes')!.textContent).toContain('IA: pupa muerta')
  })
  it('waits for a cell when none is selected', async () => {
    const { box } = await mount(null)
    expect(box.disabled).toBe(true)
    expect(box.placeholder).toBe('Selecciona una celda para ver todo su texto')
  })
})
