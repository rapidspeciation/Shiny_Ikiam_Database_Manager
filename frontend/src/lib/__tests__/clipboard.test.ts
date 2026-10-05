import { describe, expect, it } from 'vitest'
import { TabulatorFull as Tabulator } from 'tabulator-tables'
import type { CellComponent } from 'tabulator-tables'
import { copyText, toTsv } from '../clipboard'
import { isoToSerial } from '../dates'
import { attachCopyMarker, plainCopy } from '../gridKit'
import { parseBlock } from '../paste'

const date = { key: 'Date', type: 'date' as const }

describe('a cell as copied', () => {
  it('gives dates as 2026-10-04, which Google Sheets reads as a date in any locale', () => {
    expect(copyText(isoToSerial('2026-10-04'), date)).toBe('2026-10-04')
    expect(copyText(isoToSerial('2031-01-15'), date)).toBe('2031-01-15')
    // Not a date the app knows: as the grid shows it.
    expect(copyText('sin fecha', date)).toBe('sin fecha')
  })
  it('gives times as hh:mm, numbers as numbers and codes as shown', () => {
    expect(copyText((9 * 60 + 5) / 1440, { key: 'Collection_time', type: 'number' })).toBe('09:05')
    expect(copyText(12, { key: 'N', type: 'number' })).toBe('12')
    expect(copyText(0.25, { key: 'Weight', type: 'number' })).toBe('0.25')
    expect(copyText('007', { key: 'Code', type: 'text' })).toBe('007')
    expect(copyText('=12+15', { key: 'Count', type: 'text' })).toBe('=12+15')
    expect(copyText(true)).toBe('TRUE')
    expect(copyText(null, date)).toBe('')
  })
})

describe('cells as tab-separated text', () => {
  it('puts cells between tabs and rows on lines', () => {
    expect(
      toTsv([
        ['CAM079895', '2026-10-04', '12'],
        ['CAM079896', '', '3'],
      ]),
    ).toBe('CAM079895\t2026-10-04\t12\nCAM079896\t\t3')
  })
  it('quotes a cell with a new line, a tab or a leading quote, as spreadsheets do, and reads it back', () => {
    const rows = [
      ['a', 'two\nlines', 'b'],
      ['"aff." note', 'tab\there', 'say "hi"'],
    ]
    const text = toTsv(rows)
    expect(text).toBe('a\t"two\nlines"\tb\n"""aff."" note"\t"tab\there"\tsay "hi"')
    expect(parseBlock(text)).toEqual(rows)
  })
  it('reads an unclosed quote as text', () => {
    expect(parseBlock('"aff. x\tb\nc\td')).toEqual([
      ['"aff. x', 'b'],
      ['c', 'd'],
    ])
    expect(parseBlock('a\t\nb\t')).toEqual([
      ['a', ''],
      ['b', ''],
    ])
  })
})

describe('copying from a grid', () => {
  it('puts only plain text on the clipboard: dates as dates, formulas as their values, and says how many formulas', async () => {
    const host = document.createElement('div')
    const box = document.createElement('div')
    box.append(host)
    document.body.append(box)
    const fields: Record<string, { key: string; type: 'text' | 'number' | 'date' }> = {
      ID: { key: 'ID', type: 'text' },
      Date: date,
      Total: { key: 'Total', type: 'number' },
    }
    const table: Tabulator = new Tabulator(host, {
      data: [
        { id: 1, ID: 'CAM079895', Date: isoToSerial('2026-10-04'), Total: 27 },
        { id: 2, ID: 'CAM079896', Date: isoToSerial('2026-10-05'), Total: 3 },
      ],
      columns: [
        { field: 'id', headerSort: false },
        { field: 'ID', formatter: () => '<b style="color:red">ID</b>' },
        { field: 'Date' },
        { field: 'Total' },
      ],
      selectableRange: 1,
      selectableRangeColumns: true,
      selectableRangeRows: true,
      clipboard: true,
      ...plainCopy(
        () => table,
        cell => copyText(cell.getValue(), fields[cell.getField()]),
      ),
      renderVertical: 'basic',
    } as unknown as ConstructorParameters<typeof Tabulator>[1])
    await new Promise<void>(resolve => table.on('tableBuilt', () => resolve()))
    const notices: string[] = []
    // Total is the sheet's formula.
    const marker = attachCopyMarker(table, box, m => notices.push(m), cell => cell.getField() === 'Total')
    const cell = (id: number, field: string) => table.getRows()[id - 1].getCell(field) as CellComponent
    ;(table as unknown as { addRange: (a: CellComponent, b: CellComponent) => void }).addRange(cell(1, 'ID'), cell(2, 'Total'))
    await new Promise(resolve => setTimeout(resolve))

    // The browser's copy command, as the test page has none: the grid's copy event with a clipboard to write to.
    const written: Record<string, string> = {}
    const exec = document.execCommand
    document.execCommand = (command: string) => {
      const event = new Event(command, { bubbles: true, cancelable: true })
      Object.defineProperty(event, 'clipboardData', { value: { setData: (type: string, text: string) => (written[type] = text) } })
      table.element.dispatchEvent(event)
      return true
    }
    table.copyToClipboard('range')
    document.execCommand = exec

    expect(Object.keys(written)).toEqual(['text/plain'])
    expect(written['text/plain']).toBe('CAM079895\t2026-10-04\t27\nCAM079896\t2026-10-05\t3')
    expect(notices.at(-1)).toBe(
      'Copiado: 6 celdas. Selecciona dónde pegar y pulsa Ctrl+V · Incluye 2 celdas con fórmula: pegadas en Google Sheets, sus valores reemplazan las fórmulas',
    )
    marker.destroy()
    table.destroy()
    box.remove()
  })
})
