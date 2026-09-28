import { describe, expect, it } from 'vitest'
import { KeybindingsModule, TabulatorFull as Tabulator } from 'tabulator-tables'
import type { CellComponent } from 'tabulator-tables'
import { tabulatorKeyCode } from '../gridKit'

const keyCode = (key: string, legacy = 0) =>
  (KeybindingsModule.prototype as unknown as { getKeyCode: (e: Partial<KeyboardEvent>) => number }).getKeyCode({ key, keyCode: legacy })

describe('typing punctuation in a grid', () => {
  it('does not take "(" & % \' $ # ! " for the arrows, Home, End or the page keys', () => {
    // Tabulator 6.5 read "(" as 40, the down arrow: "DY_(dry)" saved "DY_" and typed "dry)" in the row below.
    for (const key of ['(', '&', '%', "'", '$', '#', '!', '"', ')', '.', '-', ' ']) expect(keyCode(key)).toBe(0)
    expect(tabulatorKeyCode({ key: '(', keyCode: 57 }, () => 40)).toBe(0)
  })
  it('keeps the real arrows, Tab, Enter and letters (Ctrl+C, Ctrl+V)', () => {
    expect(keyCode('ArrowDown', 40)).toBe(40)
    expect(keyCode('ArrowUp', 38)).toBe(38)
    expect(keyCode('Tab', 9)).toBe(9)
    expect(keyCode('Enter', 13)).toBe(13)
    expect(keyCode('c', 67)).toBe(67)
    expect(keyCode('7', 55)).toBe(55)
  })
  it('leaves the row below alone when "DY_(dry)" is typed over a selected cell', async () => {
    const host = document.createElement('div')
    document.body.append(host)
    const table = new Tabulator(host, {
      data: [
        { id: 1, Rainfall: '' },
        { id: 2, Rainfall: 'WT_(wet)' },
      ],
      columns: [{ field: 'Rainfall', editor: 'input' }],
      selectableRange: 1,
      editTriggerEvent: 'dblclick',
    } as ConstructorParameters<typeof Tabulator>[1])
    await new Promise<void>(resolve => table.on('tableBuilt', () => resolve()))
    const first = table.getRows()[0].getCell('Rainfall') as CellComponent
    ;(table as unknown as { addRange: (a: CellComponent, b: CellComponent) => void }).addRange(first, first)
    for (const key of 'DY_(dry)') {
      const shift = /[A-Z_()]/.test(key)
      host.dispatchEvent(new KeyboardEvent('keydown', { key, shiftKey: shift, bubbles: true, cancelable: true }))
      host.dispatchEvent(new KeyboardEvent('keyup', { key, shiftKey: shift, bubbles: true }))
    }
    const selected = table.getRanges()[0].getCells().flat() as CellComponent[]
    expect(selected.map(c => c.getRow().getIndex())).toEqual([1])
    expect(table.getRows()[1].getData().Rainfall).toBe('WT_(wet)')
    table.destroy()
    host.remove()
  })
})
