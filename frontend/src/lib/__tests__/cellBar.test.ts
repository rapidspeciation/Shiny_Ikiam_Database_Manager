import { afterEach, describe, expect, it } from 'vitest'
import { TabulatorFull as Tabulator } from 'tabulator-tables'
import type { CellComponent } from 'tabulator-tables'
import { barKey, fitWidth, longText, needsRedraw, selectedCell, setFromBar, spread, scrollDelta, tileToSelection } from '../gridKit'

const key = (key: string, mods: Partial<KeyboardEvent> = {}) => ({
  key,
  shiftKey: false,
  altKey: false,
  ctrlKey: false,
  metaKey: false,
  isComposing: false,
  ...mods,
})

describe('keys in the cell bar', () => {
  it('saves with Enter and moves as in the cell: down, up with Shift, right with Tab', () => {
    expect(barKey(key('Enter'), false)).toEqual({ action: 'save', move: 'down' })
    expect(barKey(key('Enter', { shiftKey: true }), false)).toEqual({ action: 'save', move: 'up' })
    expect(barKey(key('Tab'), true)).toEqual({ action: 'save', move: 'right' })
    expect(barKey(key('Tab', { shiftKey: true }), true)).toEqual({ action: 'save', move: 'left' })
    // Ctrl+Enter saves and stays on the cell.
    expect(barKey(key('Enter', { ctrlKey: true }), true)).toEqual({ action: 'save', move: 'here' })
  })
  it('breaks the line with Shift+Enter or Alt+Enter only in a notes column', () => {
    expect(barKey(key('Enter', { shiftKey: true }), true)).toEqual({ action: 'newline' })
    expect(barKey(key('Enter', { altKey: true }), true)).toEqual({ action: 'newline' })
    expect(barKey(key('Enter', { altKey: true }), false)).toEqual({ action: 'save', move: 'here' })
  })
  it('gives up with Esc, and leaves other keys and a word being composed to the text box', () => {
    expect(barKey(key('Escape'), false)).toEqual({ action: 'cancel' })
    expect(barKey(key('a'), true)).toBeNull()
    expect(barKey(key('Enter', { isComposing: true }), true)).toBeNull()
  })
  it('takes notes and comments as long text', () => {
    expect(longText('Notes_Insectary_data')).toBe(true)
    expect(longText('Comments')).toBe(true)
    expect(longText('Insectary_ID')).toBe(false)
    expect(longText('SPECIES')).toBe(false)
  })
})

describe('fitting a column to its content', () => {
  const measure = (text: string) => text.length * 7
  it('takes the widest text plus the padding, within the limits', () => {
    expect(fitWidth(['ab', 'abcdef', 'abc'], measure, { padding: 12 })).toBe(6 * 7 + 12 + 1)
    // A long note stops at the maximum: the cell bar shows the rest.
    expect(fitWidth(['x'.repeat(300)], measure, { padding: 12, max: 480 })).toBe(480)
    // Never narrower than the column's name or its minimum width.
    expect(fitWidth(['1'], measure, { padding: 12, header: 120, min: 70 })).toBe(120)
    expect(fitWidth([], measure, { padding: 12, min: 70 })).toBe(70)
  })
  it('measures each different text once', () => {
    let calls = 0
    const counting = (text: string) => (calls++, measure(text))
    fitWidth(['NA', 'NA', 'NA', '', 'dead pupa'], counting, { padding: 12 })
    expect(calls).toBe(2)
  })
  it('samples a long sheet evenly, first and last rows included', () => {
    const rows = Array.from({ length: 100_000 }, (_, i) => i)
    const sample = spread(rows, 1000)
    expect(sample).toHaveLength(1000)
    expect(sample[0]).toBe(0)
    expect(sample.at(-1)).toBe(99_999)
    expect(spread([1, 2, 3], 1000)).toEqual([1, 2, 3])
  })
})

describe('redrawing a grid whose box changed size', () => {
  const box = { width: 1440, height: 687 }
  it('redraws Tablas only when it gets wider or narrower, or much taller or shorter', () => {
    // The cell bar grew by two lines: the rows area follows by CSS.
    expect(needsRedraw(box, { width: 1440, height: 651 }, true)).toBe(false)
    expect(needsRedraw(box, { width: 1440, height: 300 }, true)).toBe(true)
    expect(needsRedraw(box, { width: 1200, height: 687 }, true)).toBe(true)
  })
  it('redraws other grids on any change, and never on the first size seen', () => {
    expect(needsRedraw(box, { width: 1440, height: 651 })).toBe(true)
    expect(needsRedraw(box, { width: 1440.3, height: 687.2 })).toBe(false)
    expect(needsRedraw(null, box)).toBe(false)
  })
})

describe('editing through the cell bar', () => {
  let table: Tabulator | null = null
  afterEach(() => {
    table?.destroy()
    table = null
    document.body.innerHTML = ''
  })
  async function build() {
    const host = document.createElement('div')
    document.body.append(host)
    const t = new Tabulator(host, {
      data: [
        { id: 'a', Notes: 'dead pupa', Locked: 'x' },
        { id: 'b', Notes: '', Locked: 'y' },
      ],
      columns: [
        { field: 'Notes', editor: 'input' },
        { field: 'Locked', editor: 'input' },
      ],
      selectableRange: 1,
      editTriggerEvent: 'dblclick',
    } as ConstructorParameters<typeof Tabulator>[1])
    table = t
    await new Promise<void>(resolve => t.on('tableBuilt', () => resolve()))
    return t
  }
  const canEdit = (_row: unknown, field: string) => field === 'Notes'

  it('writes the text as the cell editor does: the grid hears cellEdited', async () => {
    const t = await build()
    const edited: [string, unknown][] = []
    t.on('cellEdited', (cell: CellComponent) => edited.push([cell.getField(), cell.getValue()]))
    expect(setFromBar(t, { index: 'b', field: 'Notes' }, '30/9/26 L: pupa muerta', canEdit)).toBe(true)
    expect(edited).toEqual([['Notes', '30/9/26 L: pupa muerta']])
    expect(t.getRow('b').getData().Notes).toBe('30/9/26 L: pupa muerta')
  })
  it('refuses a cell that cannot be edited, or a row that is gone', async () => {
    const t = await build()
    expect(setFromBar(t, { index: 'a', field: 'Locked' }, 'z', canEdit)).toBe(false)
    expect(t.getRow('a').getData().Locked).toBe('x')
    expect(setFromBar(t, { index: 'gone', field: 'Notes' }, 'z', canEdit)).toBe(false)
  })
  it('shows the cell where the selection began', async () => {
    const t = await build()
    const inner = t as unknown as { addRange: (a: CellComponent, b: CellComponent) => void }
    const first = t.getRow('a').getCell('Notes') as CellComponent
    const last = t.getRow('b').getCell('Locked') as CellComponent
    inner.addRange(last, first)
    await new Promise(resolve => setTimeout(resolve))
    const cell = selectedCell(t)
    expect(cell?.getRow().getIndex()).toBe('b')
    expect(cell?.getField()).toBe('Locked')
  })
})

describe('pasting over a selection', () => {
  it('repeats the copied block to fill a larger selection, as Google Sheets does', () => {
    const pair = [['Ithomia salapia', 'salapia']]
    // Copied SPECIES + Subspecies_Form of one row, pasted over three rows of SPECIES.
    expect(tileToSelection(pair, 3, 1)).toEqual([pair[0], pair[0], pair[0]])
    // One value over a 2 × 2 selection fills the four cells.
    expect(tileToSelection([['male']], 2, 2)).toEqual([
      ['male', 'male'],
      ['male', 'male'],
    ])
    // Two rows over five: the pattern repeats.
    expect(tileToSelection([['a'], ['b']], 5, 1).map(r => r[0])).toEqual(['a', 'b', 'a', 'b', 'a'])
  })
  it('pastes the block as it is into a single selected cell (it can add rows)', () => {
    const block = [
      ['x', 'y'],
      ['z', 'w'],
    ]
    expect(tileToSelection(block, 1, 1)).toEqual(block)
  })
})

describe('keeping the selected cell in sight', () => {
  it('scrolls only as far as needed, back or forward', () => {
    // A view from 100 to 500 (e.g. right of the frozen Fila and ID columns).
    expect(scrollDelta(150, 250, 100, 500)).toBe(0)
    expect(scrollDelta(40, 140, 100, 500)).toBe(-60)
    expect(scrollDelta(450, 560, 100, 500)).toBe(60)
  })
  it('shows the start of a cell wider than the view', () => {
    expect(scrollDelta(450, 1000, 100, 500)).toBe(350)
  })
})
