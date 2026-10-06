import { describe, expect, it } from 'vitest'
import { KeybindingsModule, TabulatorFull as Tabulator } from 'tabulator-tables'
import type { CellComponent } from 'tabulator-tables'
import { attachPendingCut, cutCellId, spreadsheetKeys, tabulatorKeyCode } from '../gridKit'

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

const ARROWS: Record<string, number> = { ArrowDown: 40, ArrowUp: 38, ArrowLeft: 37, ArrowRight: 39 }
/** A grid of eight rows (A editable, B read-only in the cut test) with range selection, as the app builds its grids. */
async function grid() {
  const host = document.createElement('div')
  document.body.append(host)
  const table = new Tabulator(host, {
    data: [1, 2, 3, 4, 5, 6, 7, 8].map(id => ({ id, A: `a${id}`, B: `b${id}` })),
    columns: [
      // The row number, which range selection takes as its row header (as the app's grids).
      { field: 'id', headerSort: false },
      { field: 'A', editor: 'input' },
      { field: 'B', editor: 'input' },
    ],
    selectableRange: 1,
    selectableRangeColumns: true,
    selectableRangeRows: true,
    clipboard: true,
    clipboardCopyConfig: { columnHeaders: false, rowHeaders: false, formatCells: true },
    clipboardCopyRowRange: 'range',
    editTriggerEvent: 'dblclick',
    // jsdom has no layout: every row drawn.
    renderVertical: 'basic',
  } as ConstructorParameters<typeof Tabulator>[1])
  await new Promise<void>(resolve => table.on('tableBuilt', () => resolve()))
  // (Tabulator sets a new range's cells a moment later.)
  const select = (from: CellComponent, to = from) => {
    ;(table as unknown as { addRange: (a: CellComponent, b: CellComponent) => void }).addRange(from, to)
    return new Promise(resolve => setTimeout(resolve))
  }
  const cell = (id: number, field: string) => table.getRows()[id - 1].getCell(field) as CellComponent
  const press = (key: string, options: KeyboardEventInit = {}) =>
    (table as unknown as { rowManager: { element: HTMLElement } }).rowManager.element.dispatchEvent(
      new KeyboardEvent('keydown', { key, keyCode: ARROWS[key], bubbles: true, cancelable: true, ...options }),
    )
  const selected = () =>
    (table.getRanges()[0].getCells().flat() as CellComponent[]).map(c => `${c.getField()}${c.getRow().getIndex()}`)
  const done = () => {
    table.destroy()
    host.remove()
  }
  return { host, table, select, cell, press, selected, done }
}

describe('moving after stretching a selection with Shift and the arrows', () => {
  it('goes on from the corner that was moving, as in Google Sheets', async () => {
    const g = await grid()
    await g.select(g.cell(2, 'A'))
    for (let i = 0; i < 4; i++) g.press('ArrowDown', { shiftKey: true })
    expect(g.selected()).toEqual(['A2', 'A3', 'A4', 'A5', 'A6'])
    // Down: the row under the last selected one (not the one under the first).
    g.press('ArrowDown')
    expect(g.selected()).toEqual(['A7'])
    // Up after stretching up: the row above the top end.
    await g.select(g.cell(6, 'A'))
    g.press('ArrowUp', { shiftKey: true })
    g.press('ArrowUp', { shiftKey: true })
    g.press('ArrowUp')
    expect(g.selected()).toEqual(['A3'])
    // Across too: stretched right to B, then Down goes down from B.
    await g.select(g.cell(1, 'A'))
    g.press('ArrowRight', { shiftKey: true })
    g.press('ArrowDown')
    expect(g.selected()).toEqual(['B2'])
    g.done()
  })
})

describe('cutting cells (Ctrl+X)', () => {
  it('copies the selection as Ctrl+C does and clears its editable cells, keeping the read-only ones', async () => {
    const g = await grid()
    let copied = ''
    // The browser's copy command, as jsdom has none: the grid's copy event with a clipboard to write to.
    const exec = document.execCommand
    document.execCommand = (command: string) => {
      const event = new Event(command, { bubbles: true, cancelable: true })
      const setData = (type: string, text: string) => type === 'text/plain' && (copied = text)
      Object.defineProperty(event, 'clipboardData', { value: { setData } })
      g.table.element.dispatchEvent(event)
      return true
    }
    const keys = spreadsheetKeys(() => g.table, (_row, field) => field === 'A', () => {})
    g.host.addEventListener('keydown', keys)
    await g.select(g.cell(2, 'A'), g.cell(3, 'B'))
    g.press('x', { ctrlKey: true })
    document.execCommand = exec
    expect(copied).toBe('a2\tb2\na3\tb3')
    expect(g.table.getData().slice(0, 4).map(r => [r.A, r.B])).toEqual([
      ['a1', 'b1'],
      [null, 'b2'],
      [null, 'b3'],
      ['a4', 'b4'],
    ])
    g.done()
  })
})

describe('a cut kept until it is pasted (attachPendingCut)', () => {
  /** The browser's copy command, as jsdom has none: the grid's copy event with a clipboard to write to. */
  function clipboard(table: Tabulator) {
    const out = { text: '' }
    const exec = document.execCommand
    document.execCommand = (command: string) => {
      const event = new Event(command, { bubbles: true, cancelable: true })
      const setData = (type: string, text: string) => type === 'text/plain' && (out.text = text)
      Object.defineProperty(event, 'clipboardData', { value: { setData } })
      table.element.dispatchEvent(event)
      return true
    }
    return { out, restore: () => (document.execCommand = exec) }
  }
  const values = (g: Awaited<ReturnType<typeof grid>>) => g.table.getData().slice(0, 4).map(r => [r.A, r.B])

  it('only marks the cells on Ctrl+X; the paste moves them in one go, keeping the read-only ones', async () => {
    const g = await grid()
    const clip = clipboard(g.table)
    const canEdit = (_row: unknown, field: string) => field === 'A'
    const cuts = attachPendingCut(g.table, g.host, canEdit)
    const keys = spreadsheetKeys(() => g.table, canEdit, () => {}, undefined, { cut: () => cuts.start() })
    g.host.addEventListener('keydown', keys)
    await g.select(g.cell(2, 'A'), g.cell(3, 'B'))
    g.press('x', { ctrlKey: true })
    expect(clip.out.text).toBe('a2\tb2\na3\tb3')
    // Nothing emptied yet.
    expect(values(g)).toEqual([
      ['a1', 'b1'],
      ['a2', 'b2'],
      ['a3', 'b3'],
      ['a4', 'b4'],
    ])
    expect(cuts.pending?.cells.map(c => `${c.field}${c.row}`)).toEqual(['A2', 'B2', 'A3', 'B3'])
    // Another text pasted (copied elsewhere since): not this cut.
    expect(cuts.take('something else')).toBeNull()
    // The same text, as a system may give it back (CRLF, a last line end): the cut.
    const cut = cuts.take('a2\tb2\r\na3\tb3\r\n')!
    expect(cut).toBe(cuts.pending)
    // Pasted one row up (A1:B2): the cells written there stay, the rest of the cut is emptied (B only copied).
    g.cell(1, 'A').setValue('a2')
    g.cell(2, 'A').setValue('a3')
    cuts.finish(cut, new Set([cutCellId(1, 'A'), cutCellId(1, 'B'), cutCellId(2, 'A'), cutCellId(2, 'B')]))
    expect(values(g)).toEqual([
      ['a2', 'b1'],
      ['a3', 'b2'],
      [null, 'b3'],
      ['a4', 'b4'],
    ])
    // Done: pasting again copies.
    expect(cuts.pending).toBeNull()
    clip.restore()
    cuts.destroy()
    g.done()
  })

  it('is left as it was by Esc, a new copy, or editing a cell', async () => {
    const g = await grid()
    const clip = clipboard(g.table)
    const cuts = attachPendingCut(g.table, g.host, () => true)
    await g.select(g.cell(2, 'A'))
    cuts.start()
    expect(cuts.pending?.text).toBe('a2')
    g.press('Escape')
    expect(cuts.pending).toBeNull()
    // Ctrl+C after Ctrl+X: the copy is what the clipboard holds, the cut cells untouched.
    cuts.start()
    g.table.copyToClipboard('range')
    expect(cuts.pending).toBeNull()
    expect(clip.out.text).toBe('a2')
    cuts.start()
    g.cell(4, 'A').edit(true)
    expect(cuts.pending).toBeNull()
    expect(values(g)[1]).toEqual(['a2', 'b2'])
    clip.restore()
    cuts.destroy()
    g.done()
  })
})
