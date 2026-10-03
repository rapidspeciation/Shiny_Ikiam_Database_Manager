import { EditModule, KeybindingsModule, ResizeColumnsModule, SelectRangeModule } from 'tabulator-tables'
import type { CellComponent, ColumnDefinition, RowComponent, Tabulator } from 'tabulator-tables'
import { complete, pickChoice } from './paste'
import { t, tn } from './i18n'

/**
 * Tabulator 6.5 reads a typed character by its character code, so "(" (40)
 * was taken for the down arrow, "&" (38) up, "%" (37) left, "'" (39) right,
 * and "$ # ! \"" for Home, End and the page keys: typing "DY_(dry)" saved
 * "DY_" and moved to the row below, which got the rest. Only letters and
 * digits keep their code (the same as the key's old keyCode); every grid
 * gets this, since they all import this file before building a table.
 */
export function tabulatorKeyCode(e: Pick<KeyboardEvent, 'key' | 'keyCode'>, original: (e: KeyboardEvent) => number) {
  if (e.key?.length === 1 && !/^[\p{L}\p{N}]$/u.test(e.key)) return 0
  return original(e as KeyboardEvent)
}
{
  const keys = KeybindingsModule.prototype as unknown as { getKeyCode: (e: KeyboardEvent) => number }
  const original = keys.getKeyCode
  keys.getKeyCode = function (this: unknown, e: KeyboardEvent) {
    return tabulatorKeyCode(e, event => original.call(this, event))
  }
}

/**
 * How far to scroll so that the span from `start` to `end` shows between
 * `from` and `to`: 0 when it already does, negative to go back. A span wider
 * (or taller) than the view shows its start.
 */
export function scrollDelta(start: number, end: number, from: number, to: number) {
  if (start < from) return start - from
  if (end > to) return Math.min(end - to, start - from)
  return 0
}

/**
 * The cell the arrows, Tab or Enter move to stays in sight, as in Google
 * Sheets. Tabulator scrolls its own grid, but it left a cell reached going left
 * under the frozen columns (Fila, the ID), and a grid as tall as its rows (the
 * Colecta list) is scrolled by the page around it, which Tabulator leaves
 * alone. Every grid gets this, as the key codes above.
 */
type InnerColumn = { getElement: () => HTMLElement; getWidth: () => number; visible: boolean }
type InnerRange = {
  table: Tabulator & {
    rowManager: { element: HTMLElement }
    columnManager: { getElement: () => HTMLElement }
    modules: { frozenColumns?: { leftColumns: InnerColumn[]; rightColumns: InnerColumn[] } }
  }
  activeRange?: { end: { row: number; col: number } }
  getRowByRangePos: (position: number) => { getElement: () => HTMLElement } | undefined
  getColumnByRangePos: (position: number) => InnerColumn | undefined
  navigate: (jump: boolean, expand: boolean, dir: string) => boolean
}
{
  const range = SelectRangeModule.prototype as unknown as InnerRange
  const original = range.navigate
  range.navigate = function (this: InnerRange, jump: boolean, expand: boolean, dir: string) {
    const moved = original.call(this, jump, expand, dir)
    if (moved) keepInSight(this)
    return moved
  }
}
/**
 * Clicking a cell (and closing its editor) gives the focus back to the grid's
 * scrolling box, and Tabulator did it with a plain focus(): the browser then
 * scrolled whatever holds the grid (the chat, the Cambios propuestos panel, the
 * Colecta page) to show as much of that box as it could, so the view jumped
 * down under the pointer as a cell was clicked or double-clicked. The focus
 * stays; only the scrolling goes (the cell clicked is already in sight, and
 * keys that move the selection keep it in sight on their own, see keepInSight).
 */
type FocusRange = {
  table: { element: HTMLElement; rowManager: { element: HTMLElement } }
  blockKeydown: boolean
  restoreFocus: () => boolean
  finishEditingCell: () => void
}
{
  const range = SelectRangeModule.prototype as unknown as FocusRange
  // Tabulator also calls this when its data changes (a row added to Muertes' chosen IDs): the focus
  // comes back to the grid only from inside it, never taken from a field elsewhere being typed in.
  range.restoreFocus = function (this: FocusRange) {
    const focused = document.activeElement
    if (focused && focused !== document.body && !this.table.element.contains(focused)) return false
    this.table.rowManager.element.focus({ preventScroll: true })
    return true
  }
  range.finishEditingCell = function (this: FocusRange) {
    this.blockKeydown = true
    this.table.rowManager.element.focus({ preventScroll: true })
    setTimeout(() => (this.blockKeydown = false), 10)
  }
}

function keepInSight(range: InnerRange) {
  const end = range.activeRange?.end
  if (!end) return
  const { table } = range
  const holder = table.rowManager.element
  const column = range.getColumnByRangePos(end.col)
  const frozen = table.modules.frozenColumns
  // Across, inside the grid: clear of the frozen columns at either side.
  if (column && !frozen?.leftColumns.includes(column) && !frozen?.rightColumns.includes(column)) {
    const width = (columns: InnerColumn[] = []) => columns.reduce((sum, c) => sum + (c.visible ? c.getWidth() : 0), 0)
    const start = column.getElement().offsetLeft
    const from = holder.scrollLeft + width(frozen?.leftColumns)
    const to = holder.scrollLeft + holder.clientWidth - width(frozen?.rightColumns)
    holder.scrollLeft += scrollDelta(start, start + column.getWidth(), from, to)
  }
  // Up and down around the grid, once Tabulator has scrolled its own rows and drawn the new ones.
  requestAnimationFrame(() => {
    const row = range.getRowByRangePos(end.row)?.getElement()
    if (row?.isConnected) revealRow(table, row)
  })
}

/**
 * Scrolls the page or panel around a grid so a row shows below what stays
 * pinned at its top: the grid's column names, a sticky bar (data-sticky-bar).
 */
function revealRow(table: InnerRange['table'], row: HTMLElement) {
  const scroller = scrollParent(table.element)
  const view = scroller ? scroller.getBoundingClientRect() : { top: 0, bottom: window.innerHeight }
  const pinned = [...(scroller ?? document).querySelectorAll<HTMLElement>('[data-sticky-bar]')]
    .map(el => el.getBoundingClientRect())
    .filter(r => r.height && r.top <= view.top + 1 && r.bottom > view.top)
  const top = Math.max(view.top, table.columnManager.getElement().getBoundingClientRect().bottom, ...pinned.map(r => r.bottom))
  const r = row.getBoundingClientRect()
  const by = scrollDelta(r.top, r.bottom, top, view.bottom)
  if (!by) return
  if (scroller) scroller.scrollTop += by
  else window.scrollBy(0, by)
}

/** The same for a row chosen by the app (e.g. the first row just added), once it is drawn. */
export function revealGridRow(table: Tabulator, row: RowComponent) {
  requestAnimationFrame(() => {
    const el = row.getElement()
    if (el?.isConnected) revealRow(table as unknown as InnerRange['table'], el)
  })
}

/** The nearest element around `el` that scrolls up and down; null when it is the page itself. */
function scrollParent(el: HTMLElement): HTMLElement | null {
  for (let parent = el.parentElement; parent; parent = parent.parentElement) {
    const overflow = getComputedStyle(parent).overflowY
    if ((overflow === 'auto' || overflow === 'scroll') && parent.scrollHeight > parent.clientHeight + 1) return parent
  }
  return null
}

/**
 * Spreadsheet habits shared by the Tabulator grids (Tablas, the task screens,
 * the Colecta list): keys, fill down, clear, and the fill handle that copies
 * cells by dragging, with a mouse or a finger.
 */
export type CanEdit = (row: RowComponent, field: string) => boolean
type Notice = (message: string) => void

/** The single selected cell, if the selection is one cell. */
export function activeCell(table: Tabulator): CellComponent | null {
  const cells = table.getRanges()[0]?.getCells().flat() as CellComponent[] | undefined
  return cells?.length === 1 ? cells[0] : null
}

/** Copy the first row of the selection down to the rest (Ctrl+D). */
export function fillDown(table: Tabulator, canEdit: CanEdit, notice: Notice) {
  const range = table.getRanges()[0]
  if (!range) return notice(t('Selecciona un rango de celdas para rellenar'))
  const rows = range.getRows()
  if (rows.length < 2) return notice(t('Selecciona al menos dos filas para rellenar hacia abajo'))
  for (const column of range.getColumns()) {
    const field = column.getField()
    const source = rows[0].getCell(field).getValue()
    for (const row of rows.slice(1)) if (canEdit(row, field)) row.getCell(field).setValue(source)
  }
}

/** Clear the editable cells of the selection (Supr / Delete). */
export function clearRange(table: Tabulator, canEdit: CanEdit) {
  const range = table.getRanges()[0]
  if (!range) return
  for (const cell of range.getCells().flat() as CellComponent[]) if (canEdit(cell.getRow(), cell.getField())) cell.setValue(null)
}

// Keys typed while a cell's editor is still opening are kept and given to it,
// so a fast typist does not lose the first letters. An Enter or Tab pressed
// meanwhile waits for them too, then saves and moves on.
type Move = { table: Tabulator; key: string; shift: boolean }
let opening: { cell: CellComponent; text: string; then?: Move } | null = null
/** Keys typed on a cell are waiting for its editor: the grid must not redraw that cell now. */
export const typingPending = () => !!opening
function giveText(tries = 0) {
  if (!opening) return
  const input = opening.cell.getElement().querySelector('input')
  // An editor closed before it could open (e.g. the row was redrawn) is opened again once.
  if (!input && tries === 8) opening.cell.edit(true)
  if (!input) return tries < 20 ? requestAnimationFrame(() => giveText(tries + 1)) : void (opening = null)
  const { text, then } = opening
  opening = null
  input.value = text
  input.dispatchEvent(new Event('input', { bubbles: true }))
  // List editors (Tabulator's autocomplete) notice typing on keyup, not on input.
  input.dispatchEvent(new KeyboardEvent('keyup', { key: text.at(-1), bubbles: true }))
  input.setSelectionRange(input.value.length, input.value.length)
  if (then) saveAndMove(then, input)
}

/**
 * Spreadsheet keys: typing on a selected cell replaces its content; Enter or
 * F2 edits it in place; Ctrl+D fills down; Supr clears the selection.
 * `whyNot` explains a cell that cannot be typed in, when there is a reason to give.
 */
export function spreadsheetKeys(
  table: () => Tabulator | null,
  canEdit: CanEdit,
  notice: Notice,
  whyNot?: (row: RowComponent, field: string) => string | null,
) {
  return (event: KeyboardEvent) => {
    const t = table()
    const typing = event.key.length === 1 && !event.ctrlKey && !event.metaKey && !event.altKey
    if (opening && typing) {
      event.preventDefault()
      opening.text += event.key
      return
    }
    if (t && opening && !touchScreen && (event.key === 'Enter' || event.key === 'Tab')) {
      event.preventDefault()
      opening.then = { table: t, key: event.key, shift: event.shiftKey }
      return
    }
    if (!t || (event.target as HTMLElement).closest('input, textarea, select, .tabulator-editing')) return
    const cell = activeCell(t)
    const editable = !!cell && canEdit(cell.getRow(), cell.getField())
    if (cell && editable && typing) {
      event.preventDefault()
      opening = { cell, text: event.key }
      cell.edit(true)
      requestAnimationFrame(() => giveText())
    } else if (cell && editable && (event.key === 'Enter' || event.key === 'F2')) {
      event.preventDefault()
      cell.edit(true)
    } else if (cell && !editable && typing && whyNot?.(cell.getRow(), cell.getField())) {
      event.preventDefault()
      notice(whyNot(cell.getRow(), cell.getField())!)
    } else if ((event.ctrlKey || event.metaKey) && event.key.toLowerCase() === 'd') {
      event.preventDefault()
      fillDown(t, canEdit, notice)
    } else if (event.key === 'Delete') {
      event.preventDefault()
      clearRange(t, canEdit)
    }
  }
}

/**
 * Enter or Tab in a cell being edited saves it and moves on, as in Google
 * Sheets: Enter goes down, Tab right (with Shift, up and left). The next cell
 * is only selected, so typing replaces it and Enter edits it. (Tabulator kept
 * the edited cell selected, and a second Enter opened it again.)
 * Listen on the grid in the capture phase, before the editor sees the key.
 */
export function editingKeys(table: () => Tabulator | null) {
  return (event: KeyboardEvent) => {
    const input = event.target as HTMLElement
    const t = table()
    if (!t || touchScreen || !event.isTrusted || event.isComposing) return
    if ((event.key !== 'Enter' && event.key !== 'Tab') || event.ctrlKey || event.altKey || event.metaKey) return
    if (!input.closest('.tabulator-editing')) return
    event.preventDefault()
    event.stopPropagation()
    const move = { table: t, key: event.key, shift: event.shiftKey }
    // Letters typed a moment ago may not be in the box yet.
    if (opening) {
      opening.then = move
      giveText()
    } else saveAndMove(move, input)
  }
}

function saveAndMove({ table, key, shift }: Move, input: HTMLElement) {
  // The editor saves on its own Enter (a list takes the typed text, or the item picked with the arrows);
  // given only to the editor, so the grid does not take it as "edit the selected cell".
  const enter = new KeyboardEvent('keydown', { key: 'Enter', code: 'Enter', cancelable: true })
  Object.defineProperty(enter, 'keyCode', { get: () => 13 })
  input.dispatchEvent(enter)
  const inner = table as unknown as {
    modules: {
      edit?: { currentCell: unknown; cancelEdit: () => void }
      selectRange?: { navigate: (jump: boolean, expand: boolean, dir: string) => boolean }
    }
    rowManager: { element: HTMLElement }
  }
  // A list opened and left untouched does not save on Enter: nothing changed.
  if (inner.modules.edit?.currentCell) inner.modules.edit.cancelEdit()
  const dir = key === 'Enter' ? (shift ? 'up' : 'down') : shift ? 'left' : 'right'
  inner.modules.selectRange?.navigate(false, false, dir)
  inner.rowManager.element.focus({ preventScroll: true })
}

/** What scrolls while dragging: the grid itself (Tablas), or the page around it (the Colecta list). */
function scrollingAround(container: HTMLElement): HTMLElement | null {
  const box = container.querySelector<HTMLElement>('.tabulator-tableholder')
  if (box && box.scrollHeight > box.clientHeight + 1) return box
  return scrollParent(container)
}

/**
 * The fill handle (computers): a small square at the bottom-right corner of
 * the selected cells. Dragging it down copies
 * those cells to the rows it passes over, repeating them if several rows were
 * selected, as in Excel or Sheets. Read-only cells are skipped. Columns with a
 * `series` continue it instead (N5D → N6D → N7D, CAM079895 → CAM079896), as
 * Sheets continues a number.
 */
export function attachFillHandle(
  table: Tabulator,
  container: HTMLElement,
  {
    canEdit,
    onFilled,
    series,
  }: {
    canEdit: CanEdit
    onFilled?: (count: number) => void
    /** The value `step` places after `value` in a column's series, or null to copy. */
    series?: (field: string, value: unknown, step: number) => unknown
  },
) {
  const handle = document.createElement('div')
  handle.className = 'fill-handle'
  // Read on hover, so it follows the interface language.
  handle.addEventListener('pointerenter', () => (handle.title = t('Arrastra hacia abajo para copiar')))
  container.appendChild(handle)
  let source: { rows: RowComponent[]; fields: string[] } | null = null
  let dragging = false

  const holder = () => container.querySelector<HTMLElement>('.tabulator-tableholder')
  const scrolling = () => scrollingAround(container)
  const hide = () => (handle.style.display = 'none')

  function currentSource() {
    const range = table.getRanges()[0]
    if (!range) return null
    const rows = range.getRows()
    const fields = range
      .getColumns()
      .map(c => c.getField())
      .filter(f => f && !f.startsWith('__'))
    return rows.length && fields.length ? { rows, fields } : null
  }

  function place() {
    if (dragging) return
    source = currentSource()
    const box = holder()
    if (!source || !box) return hide()
    const el = source.rows.at(-1)!.getCell(source.fields.at(-1)!)?.getElement()
    if (!el?.isConnected) return hide()
    const cell = el.getBoundingClientRect()
    const view = box.getBoundingClientRect()
    if (cell.bottom < view.top || cell.bottom > view.bottom + 1 || cell.right < view.left || cell.right > view.right + 1)
      return hide()
    const origin = container.getBoundingClientRect()
    handle.style.display = 'block'
    handle.style.left = `${cell.right - origin.left}px`
    handle.style.top = `${cell.bottom - origin.top}px`
  }

  let marked: HTMLElement[] = []
  const unmark = () => {
    for (const el of marked) el.classList.remove('fill-target')
    marked = []
  }

  handle.addEventListener('pointerdown', down => {
    if (!source) return
    down.preventDefault()
    down.stopPropagation()
    handle.setPointerCapture(down.pointerId)
    dragging = true
    const { rows: from, fields } = source
    const active = table.getRows('active')
    const indexOf = new Map(active.map((r, i) => [r.getElement(), i]))
    const last = active.indexOf(from.at(-1)!)
    let end = last
    let pointerX = down.clientX
    let pointerY = down.clientY
    const mark = () => {
      unmark()
      for (let i = last + 1; i <= end; i++)
        for (const f of fields) {
          const el = active[i].getCell(f)?.getElement()
          if (el) {
            el.classList.add('fill-target')
            marked.push(el)
          }
        }
    }
    const track = (x: number, y: number) => {
      pointerX = x
      pointerY = y
      const row = document.elementFromPoint(x, y)?.closest('.tabulator-row') as HTMLElement | null
      const i = row ? indexOf.get(row) : undefined
      if (i !== undefined) {
        end = Math.max(last, i)
        mark()
      }
    }
    // Near the bottom edge the grid scrolls on its own, so long runs can be filled.
    const scroller = window.setInterval(() => {
      const box = scrolling()
      if (!box) return
      const view = box.getBoundingClientRect()
      if (pointerY > view.bottom - 24) {
        box.scrollTop += 24
        const y = pointerY
        track(pointerX, Math.min(y, view.bottom - 2))
        pointerY = y
      }
    }, 60)
    const move = (e: PointerEvent) => track(e.clientX, e.clientY)
    const up = () => {
      window.clearInterval(scroller)
      handle.removeEventListener('pointermove', move)
      handle.removeEventListener('pointerup', up)
      handle.removeEventListener('pointercancel', up)
      unmark()
      dragging = false
      let count = 0
      for (let i = last + 1; i <= end; i++) {
        const target = active[i]
        const src = from[(i - last - 1) % from.length]
        for (const f of fields)
          if (canEdit(target, f)) {
            const next = series?.(f, from.at(-1)!.getCell(f).getValue(), i - last)
            target.getCell(f).setValue(next ?? src.getCell(f).getValue())
            count++
          }
      }
      if (count) onFilled?.(end - last)
      place()
    }
    handle.addEventListener('pointermove', move)
    handle.addEventListener('pointerup', up)
    handle.addEventListener('pointercancel', up)
  })

  const later = () => requestAnimationFrame(place)
  for (const event of [
    'rangeAdded',
    'rangeChanged',
    'rangeRemoved',
    'scrollVertical',
    'scrollHorizontal',
    'renderComplete',
    'columnResized',
  ])
    table.on(event as 'renderComplete', later)
  return { place, destroy: () => handle.remove() }
}

/**
 * Pasting over a selection larger than what was copied fills all of it by
 * repeating the copied block, as Google Sheets does (one value pasted over
 * five selected cells fills the five). A single selected cell takes the block as is.
 */
export function tileToSelection(block: string[][], rows: number, columns: number): string[][] {
  const height = rows > 1 ? Math.max(rows, block.length) : block.length
  const width = columns > 1 ? Math.max(columns, block[0]?.length ?? 1) : (block[0]?.length ?? 1)
  return Array.from({ length: height }, (_, i) => {
    const line = block[i % block.length]
    return Array.from({ length: width }, (_, j) => line[j % line.length] ?? '')
  })
}

/**
 * The copied cells get a moving dashed border (as in Google Sheets) until Esc
 * or until a cell is edited, and a short notice says how many were copied.
 */
export function attachCopyMarker(table: Tabulator, container: HTMLElement, notice: Notice) {
  const box = document.createElement('div')
  box.className = 'copy-box'
  container.appendChild(box)
  let cells: CellComponent[] | null = null
  const hide = () => (box.style.display = 'none')
  function place() {
    const first = cells?.[0]?.getElement()
    const last = cells?.at(-1)?.getElement()
    if (!first?.isConnected || !last?.isConnected) return hide()
    const a = first.getBoundingClientRect()
    const z = last.getBoundingClientRect()
    const o = container.getBoundingClientRect()
    Object.assign(box.style, {
      display: 'block',
      left: `${a.left - o.left}px`,
      top: `${a.top - o.top}px`,
      width: `${z.right - a.left}px`,
      height: `${z.bottom - a.top}px`,
    })
  }
  const clear = () => {
    cells = null
    hide()
  }
  table.on('clipboardCopied', () => {
    cells = (table.getRanges()[0]?.getCells().flat() as CellComponent[] | undefined) ?? null
    place()
    const n = cells?.length ?? 0
    if (n)
      notice(
        tn(
          n,
          'Copiado: {n} celda. Selecciona dónde pegar y pulsa Ctrl+V',
          'Copiado: {n} celdas. Selecciona dónde pegar y pulsa Ctrl+V',
        ),
      )
  })
  table.on('cellEditing', clear)
  const later = () => requestAnimationFrame(place)
  for (const event of ['scrollVertical', 'scrollHorizontal', 'renderComplete', 'dataProcessed', 'columnResized'])
    table.on(event as 'renderComplete', later)
  const onKey = (e: KeyboardEvent) => e.key === 'Escape' && clear()
  container.addEventListener('keydown', onKey)
  return {
    clear,
    place,
    destroy: () => {
      container.removeEventListener('keydown', onKey)
      box.remove()
    },
  }
}

/**
 * Phones and tablets, as Google Sheets on a phone: tap a cell to select it,
 * tap it again (or "Editar") to edit; drag the round handle on the
 * selection's corner to stretch it over more cells; a bar offers Copiar,
 * Pegar, Rellenar ↓ and Borrar for the selection. One finger elsewhere scrolls.
 */
let touchedSheet: HTMLElement | null = null

export function attachTouchSheet(
  table: Tabulator,
  container: HTMLElement,
  { canEdit, notice }: { canEdit: CanEdit; notice: Notice },
) {
  const handle = document.createElement('div')
  handle.className = 'fill-handle is-touch'
  handle.addEventListener('pointerenter', () => (handle.title = t('Arrastra para ampliar la selección')))
  container.appendChild(handle)
  const bar = document.createElement('div')
  bar.className = 'touch-actions'
  // Labels are the Spanish keys, written in the interface language each time the bar appears.
  const labels: [HTMLButtonElement, string][] = []
  const button = (label: string, action: () => void) => {
    const b = document.createElement('button')
    b.type = 'button'
    b.textContent = t(label)
    labels.push([b, label])
    // Keep the selection: the tap on the bar must not reach the grid.
    b.addEventListener('pointerdown', e => e.preventDefault())
    b.addEventListener('click', e => {
      e.stopPropagation()
      // A tap that made the bar appear must not also press the button now under the finger.
      if (Date.now() - shownAt < 500) return
      action()
    })
    bar.appendChild(b)
  }
  let copied = ''
  let shownAt = 0
  table.on('clipboardCopied', (plain: string) => (copied = plain))
  button('Copiar', () => table.copyToClipboard('range'))
  button('Pegar', async () => {
    const text = (await navigator.clipboard?.readText?.().catch(() => '')) || copied
    if (!text) return notice(t('No hay nada copiado todavía'))
    const data = new DataTransfer()
    data.setData('text/plain', text)
    table.element.dispatchEvent(new ClipboardEvent('paste', { clipboardData: data, bubbles: true, cancelable: true }))
  })
  button('Rellenar ↓', () => fillDown(table, canEdit, notice))
  button('Borrar', () => clearRange(table, canEdit))
  button('Editar', () => {
    const cell = activeCell(table)
    if (cell && canEdit(cell.getRow(), cell.getField())) cell.edit(true)
  })
  container.appendChild(bar)

  // While the bar shows, the page keeps room for it at the bottom, so no row stays hidden under it.
  const barRoom = (open: boolean) => document.body.classList.toggle('touch-bar-open', open)
  const hide = () => {
    handle.style.display = 'none'
    if (bar.style.display !== 'none' && touchedSheet === container) barRoom(false)
    bar.style.display = 'none'
  }
  let dragging = false
  let lastTouch = 0
  let waiting: number | undefined
  /** The selected cell must not end up under the bar: the page (or grid) scrolls it above. */
  function keepAboveBar(cell: HTMLElement) {
    const barTop = bar.getBoundingClientRect().top
    const r = cell.getBoundingClientRect()
    if (!barTop || r.bottom <= barTop - 8) return
    const box = scrollingAround(container)
    if (box) box.scrollTop += r.bottom - barTop + 16
    else window.scrollBy(0, r.bottom - barTop + 16)
  }
  function place() {
    if (dragging) return
    // Not while tapping: the bar waits until the taps are over, or a double tap's second
    // tap would land on the bar that had just appeared over the cell ("Borrar"!).
    const since = Date.now() - lastTouch
    if (since < 500) {
      window.clearTimeout(waiting)
      waiting = window.setTimeout(place, 520 - since)
      return
    }
    // Only the grid last touched shows its handle and bar (a page can hold two grids,
    // and Tabulator selects each grid's first cell on its own).
    if (touchedSheet !== container) return hide()
    const range = table.getRanges()[0]
    const cells = range?.getCells().flat() as CellComponent[] | undefined
    const last = cells?.at(-1)?.getElement()
    if (!range || !last?.isConnected) return hide()
    const appearing = bar.style.display !== 'flex'
    if (appearing) {
      shownAt = Date.now()
      for (const [b, label] of labels) b.textContent = t(label)
    }
    bar.style.display = 'flex'
    barRoom(true)
    if (appearing) requestAnimationFrame(() => keepAboveBar(last))
    const cell = last.getBoundingClientRect()
    const view = (container.querySelector('.tabulator-tableholder') as HTMLElement | null)?.getBoundingClientRect()
    if (
      view &&
      (cell.bottom < view.top || cell.bottom > view.bottom + 1 || cell.right < view.left || cell.right > view.right + 1)
    ) {
      handle.style.display = 'none'
      return
    }
    const origin = container.getBoundingClientRect()
    handle.style.display = 'block'
    handle.style.left = `${cell.right - origin.left}px`
    handle.style.top = `${cell.bottom - origin.top}px`
  }

  /** The cell under a finger. */
  function cellAt(x: number, y: number): CellComponent | null {
    const el = document.elementFromPoint(x, y)?.closest('.tabulator-cell') as HTMLElement | null
    const rowEl = el?.closest('.tabulator-row')
    const field = el?.getAttribute('tabulator-field')
    if (!el || !rowEl || !field) return null
    const row = table.getRows('active').find(r => r.getElement() === rowEl)
    return (row?.getCell(field) as CellComponent | undefined) ?? null
  }

  // Dragging the handle stretches the selection to the cell under the finger (scrolling near the edge).
  handle.addEventListener('pointerdown', down => {
    const range = table.getRanges()[0]
    if (!range) return
    down.preventDefault()
    down.stopPropagation()
    handle.setPointerCapture(down.pointerId)
    dragging = true
    let x = down.clientX
    let y = down.clientY
    const stretch = () => {
      const cell = cellAt(x, y)
      if (cell) (range as unknown as { setEndBound: (c: CellComponent) => void }).setEndBound(cell)
    }
    const scroller = window.setInterval(() => {
      const box = scrollingAround(container)
      if (!box) return
      const view = box.getBoundingClientRect()
      if (y > view.bottom - 28) box.scrollTop += 24
      else if (y < view.top + 28) box.scrollTop -= 24
      else return
      stretch()
    }, 60)
    const move = (e: PointerEvent) => {
      x = e.clientX
      y = e.clientY
      stretch()
    }
    const up = () => {
      window.clearInterval(scroller)
      handle.removeEventListener('pointermove', move)
      handle.removeEventListener('pointerup', up)
      handle.removeEventListener('pointercancel', up)
      dragging = false
      place()
    }
    handle.addEventListener('pointermove', move)
    handle.addEventListener('pointerup', up)
    handle.addEventListener('pointercancel', up)
  })

  // A tap on the cell already selected edits it (the first tap selects it, as in Google Sheets);
  // so does a double tap. The selection is read as the finger comes down, before the tap selects.
  let selectedBefore: CellComponent | null = null
  const remember = () => {
    lastTouch = Date.now()
    selectedBefore = touchedSheet === container ? activeCell(table) : null
    if (touchedSheet !== container) {
      touchedSheet = container
      // After the tap is over: shown now, the bar could take the tap's own click.
      setTimeout(() => window.dispatchEvent(new Event('touch-sheet')), 400)
    }
  }
  container.addEventListener('pointerdown', remember, true)
  const onOtherSheet = () => place()
  window.addEventListener('touch-sheet', onOtherSheet)
  table.on('cellClick', (e: UIEvent, cell: CellComponent) => {
    const again = selectedBefore?.getRow() === cell.getRow() && selectedBefore.getField() === cell.getField()
    selectedBefore = null
    if (!again || !canEdit(cell.getRow(), cell.getField())) return
    const el = cell.getElement()
    if (el.classList.contains('tabulator-editing')) return
    // The ▾ arrow of a list cell opens only its list (openList), without the keyboard.
    if (el.classList.contains('has-choices') && e instanceof MouseEvent && e.clientX >= el.getBoundingClientRect().right - 22) return
    // Deferred: opened during the tap's own click, the editor would close as the grid takes the focus.
    setTimeout(() => cell.edit(true), 30)
  })

  const later = () => requestAnimationFrame(place)
  for (const event of [
    'rangeAdded',
    'rangeChanged',
    'rangeRemoved',
    'scrollVertical',
    'scrollHorizontal',
    'renderComplete',
    'columnResized',
  ])
    table.on(event as 'renderComplete', later)
  return {
    place,
    destroy: () => {
      container.removeEventListener('pointerdown', remember, true)
      window.removeEventListener('touch-sheet', onOtherSheet)
      window.clearTimeout(waiting)
      if (touchedSheet === container) {
        touchedSheet = null
        barRoom(false)
      }
      handle.remove()
      bar.remove()
    },
  }
}

const touchScreen = typeof window !== 'undefined' && window.matchMedia('(pointer: coarse)').matches
let arrowCell: CellComponent | null = null

/**
 * The ▾ arrow opens a cell's list. On a touch screen it opens only the list to
 * tap from, without the keyboard (a second tap on the selected cell edits with the keyboard).
 */
export function openList(cell: CellComponent) {
  arrowCell = touchScreen ? cell : null
  // Deferred: the click that selects the cell would otherwise close the list at once.
  setTimeout(() => cell.edit(true))
}

/**
 * Parameters for a list editor. Closing the list without choosing keeps the
 * value: Tabulator saved an empty cell then (on phones the keyboard takes the
 * focus away as it opens, and the cell was emptied). Clear with Supr / Borrar.
 */
export function listParams(values: string[] | Record<string, string>, cell: CellComponent, freetext = true) {
  const tapOnly = arrowCell === cell
  arrowCell = null
  return {
    values,
    autocomplete: !tapOnly,
    freetext: !tapOnly && freetext,
    allowEmpty: true,
    listOnEmpty: true,
    filterDelay: 50,
    emptyValue: cell.getValue() ?? null,

    // Opened with the arrow on a phone: only the list, no keyboard.
    ...(tapOnly ? { elementAttributes: { readonly: 'readonly', inputmode: 'none' } } : {}),
  }
}

type Size = { width: number; height: number }
/**
 * Whether a grid must be redrawn after its box changed size. A grid of fixed
 * height (Tablas) sizes its rows area by CSS, and keeps rows drawn well beyond
 * the view: a few lines more or less in height (the cell bar above it growing
 * with a long note) need no redraw, which in Tablas took ~300 ms.
 */
export function needsRedraw(before: Size | null, after: Size, followsHeight = false) {
  if (!before) return false
  if (Math.round(before.width) !== Math.round(after.width)) return true
  const by = Math.abs(Math.round(after.height) - Math.round(before.height))
  return followsHeight ? by > 120 : by > 0
}

/**
 * Redraws the grid when its size really changes, but never under an open
 * editor: on a phone the keyboard resizes the page as it opens, and redrawing
 * then threw the editor away (and closed the keyboard). `followsHeight`: the
 * grid has a fixed height (see needsRedraw).
 */
export function watchSize(table: () => Tabulator | null, element: HTMLElement, { followsHeight = false } = {}) {
  // The size the grid was last drawn at.
  let drawn: Size | null = null
  let timer: number | undefined
  const redrawWhenIdle = () => {
    window.clearTimeout(timer)
    if (element.querySelector('.tabulator-editing')) timer = window.setTimeout(redrawWhenIdle, 300)
    else {
      // Only a grid that finished building: one being rebuilt (e.g. on EN/ES) threw on redraw.
      const grid = table() as (Tabulator & { initialized?: boolean }) | null
      if (grid?.initialized) grid.redraw()
    }
  }
  const observer = new ResizeObserver(([entry]) => {
    const { width, height } = entry.contentRect
    if (!width || !height) return
    const size = { width, height }
    const redraw = needsRedraw(drawn, size, followsHeight)
    if (!drawn || redraw) drawn = size
    if (redraw) redrawWhenIdle()
  })
  observer.observe(element)
  return {
    disconnect: () => {
      window.clearTimeout(timer)
      observer.disconnect()
    },
  }
}

/**
 * When the phone's keyboard opens (it shrinks only the visible area, without a
 * window resize), the cell being edited is scrolled up above it. The page is
 * scrolled rather than the grid: Tabulator closes an open list when the grid
 * scrolls or the window resizes, and the list then saved an empty cell.
 */
function keepEditorVisible() {
  const cell = document.querySelector<HTMLElement>('.tabulator-editing')
  const view = window.visualViewport
  if (!cell || !view) return
  // Visible: below any pinned bar of the page (e.g. the Colecta list bar) and above the keyboard.
  const pinned = [...document.querySelectorAll<HTMLElement>('[data-sticky-bar]')]
    .map(el => el.getBoundingClientRect())
    .filter(r => r.height && r.bottom > view.offsetTop)
  const top = Math.max(view.offsetTop, ...pinned.map(r => r.bottom)) + 8
  const bottom = view.offsetTop + view.height - 12
  const r = cell.getBoundingClientRect()
  if (r.top >= top && r.bottom <= bottom) return
  // Centre it in that space.
  const by = r.top + r.height / 2 - (top + bottom) / 2
  const listOpen = !!document.querySelector('.tabulator-popup-container')
  for (let el = cell.parentElement; el; el = el.parentElement) {
    if (el.classList.contains('tabulator-tableholder')) {
      if (listOpen || el.scrollHeight <= el.clientHeight) continue
    } else {
      const overflow = getComputedStyle(el).overflowY
      if (!(overflow === 'auto' || overflow === 'scroll') || el.scrollHeight <= el.clientHeight) continue
    }
    el.scrollTop += by
    return
  }
  window.scrollBy(0, by)
}

/** While the phone's keyboard is open the app header steps aside (CSS), leaving the space to the grid. */
function followKeyboard() {
  const view = window.visualViewport
  if (!view) return
  const open = view.height < window.innerHeight * 0.8
  if (document.body.classList.contains('keyboard-open') !== open) document.body.classList.toggle('keyboard-open', open)
  // After the header has gone (or come back), place the edited cell.
  requestAnimationFrame(() => requestAnimationFrame(keepEditorVisible))
}
if (touchScreen && typeof window !== 'undefined' && window.visualViewport)
  window.visualViewport.addEventListener('resize', followKeyboard)

type Choices = string[] | Record<string, string>
const labelsOf = (values: Choices) => (Array.isArray(values) ? values : Object.values(values))
type EditorFn = (
  this: unknown,
  cell: CellComponent,
  onRendered: (callback: () => void) => void,
  success: (value: unknown) => void,
  cancel: () => void,
) => HTMLElement

/**
 * Editing a cell with a list of choices. Computers: Tabulator's list, filtered
 * as you type. Phones (checked on a real Android phone in the emulator):
 * Tabulator's list did not focus its box, so the keyboard never came, and it
 * closes on the window resize the keyboard fires as it opens. So a tap on the
 * selected cell gives a plain text box with the phone's own suggestions above the keyboard,
 * and the ▾ arrow gives the list alone (tap to choose, no keyboard).
 */
export function choiceEditor(values: (cell: CellComponent) => Choices, freetext = true): Partial<ColumnDefinition> {
  const list = (EditModule as unknown as { editors: Record<string, (...args: unknown[]) => HTMLElement> }).editors.list
  const editor: EditorFn = function (cell, onRendered, success, cancel) {
    const options = values(cell)
    // Typed text is completed to the first suggestion (Enter or Tab), as in Google Sheets.
    const save = (value: unknown) => success(typeof value === 'string' ? complete(value, labelsOf(options)) : value)
    if (!touchScreen || arrowCell === cell)
      return list.call(this, cell, onRendered, save, cancel, listParams(options, cell, freetext) as never)
    return suggestionBox(cell, onRendered, save, cancel, labelsOf(options))
  }
  return { editor: editor as never }
}

/**
 * A plain text editor that opens with the cell as a person reads and types it,
 * not as the sheet stores it: a date as 26/05/2026 rather than its serial
 * number 46168, a time as 09:05 (see editText in lib/cells.ts). What is typed
 * goes back through the grid's cellEdited, which reads it as a paste
 * (normalizeInput: 26/5/26, 26-May-26, 2026-05-26…). Left as it opened, the
 * cell is not changed.
 */
export function textEditor(text: (value: unknown) => string): Partial<ColumnDefinition> {
  const input = (EditModule as unknown as { editors: Record<string, (...args: unknown[]) => HTMLElement> }).editors.input
  const editor: EditorFn = function (cell, onRendered, success, cancel) {
    const shown = text(cell.getValue())
    // Tabulator's own box, given the cell with the text in place of its value.
    const view = Object.create(cell, { getValue: { value: () => shown } }) as CellComponent
    return input.call(this, view, onRendered, success, cancel, { selectContents: true })
  }
  return { editor: editor as never }
}

/** A text box with the choices as the phone's suggestions (a <datalist>); Enter or leaving it saves. */
function suggestionBox(
  cell: CellComponent,
  onRendered: (callback: () => void) => void,
  success: (value: unknown) => void,
  cancel: () => void,
  options: string[],
) {
  const input = document.createElement('input')
  input.type = 'text'
  input.value = String(cell.getValue() ?? '')
  Object.assign(input.style, { width: '100%', height: '100%', padding: '4px', boxSizing: 'border-box', border: '0' })
  const choices = document.createElement('datalist')
  choices.id = `choices-${Math.random().toString(36).slice(2)}`
  for (const option of options.slice(0, 1000)) choices.append(new Option(option))
  document.body.append(choices)
  input.setAttribute('list', choices.id)
  let done = false
  const finish = (save: boolean) => {
    if (done) return
    done = true
    choices.remove()
    if (save) success(input.value)
    else cancel()
  }
  input.addEventListener('blur', () => finish(true))
  input.addEventListener('keydown', e => {
    if (e.key === 'Enter') {
      e.preventDefault()
      finish(true)
    } else if (e.key === 'Escape') finish(false)
  })
  onRendered(() => {
    input.focus({ preventScroll: true })
    input.setSelectionRange(input.value.length, input.value.length)
  })
  return input
}

// ------------------------------------------------------------ the cell bar (components/CellBar.vue)

/** A line under the cell bar's text: the sheet's value, the assistant's, a sum's total, a doubt, where a value comes from. */
export interface CellBarNote {
  label?: string
  text: string
  kind?: 'sheet' | 'ai' | 'total' | 'doubt' | 'hint' | 'unreadable' | 'edited'
}
/** Another reading of a doubtful cell, offered beside it: `text` is written into the cell as if typed. */
export interface CellBarChoice {
  label: string
  text: string
}
/**
 * What the bar above a grid shows for the selected cell, as Google Sheets'
 * formula bar: where it is (column · row ID) and its whole text, editable
 * where the cell is. `index` (the row's index in the grid) and `field` say
 * where an edit goes, even if the selection has moved on meanwhile.
 */
export interface CellBarInfo {
  index: string
  field: string
  column: string
  row: string
  /** The text as the cell's editor opens with it (dates day first, a sum as =12+15). */
  text: string
  editable: boolean
  /** Shift+Enter or Alt+Enter start a new line (notes); elsewhere Enter always saves. */
  multiline: boolean
  /** Why a cell cannot be edited, when there is one to give (a formula, a column that does not apply). */
  readonly?: string
  notes?: CellBarNote[]
  /** Other readings to pick with a click (a doubtful cell's alternatives). */
  choices?: CellBarChoice[]
  /** The choices' label ("Other readings" when not given). */
  choicesLabel?: string
  /** A click puts the choice in the bar to complete (an unreadable cell's partial reading), instead of writing it. */
  choicesComplete?: boolean
}
export type Direction = 'up' | 'down' | 'left' | 'right'

/** Free-text columns where a line break belongs (notes, comments). */
export const longText = (field: string) => /note|comment|observ|descrip|remark/i.test(field)

/**
 * What a key does in the cell bar: Enter saves and goes down (Shift+Enter up),
 * Tab right (Shift+Tab left), Ctrl+Enter saves and stays, Esc gives up the
 * change. In a notes column Shift+Enter or Alt+Enter break the line instead.
 */
export function barKey(
  e: Pick<KeyboardEvent, 'key' | 'shiftKey' | 'altKey' | 'ctrlKey' | 'metaKey' | 'isComposing'>,
  multiline: boolean,
): { action: 'save'; move: Direction | 'here' } | { action: 'newline' } | { action: 'cancel' } | null {
  if (e.isComposing) return null
  if (e.key === 'Escape') return { action: 'cancel' }
  if (e.key === 'Tab') return { action: 'save', move: e.shiftKey ? 'left' : 'right' }
  if (e.key !== 'Enter') return null
  if (multiline && (e.shiftKey || e.altKey)) return { action: 'newline' }
  if (e.ctrlKey || e.metaKey || e.altKey) return { action: 'save', move: 'here' }
  return { action: 'save', move: e.shiftKey ? 'up' : 'down' }
}

type SelectInner = {
  modules: {
    selectRange?: {
      activeRange?: { start: { row: number; col: number }; destroyed?: boolean } | false
      getRowByRangePos: (
        pos: number,
      ) => { getCell: (column: unknown) => { getComponent: () => CellComponent } | false } | undefined
      getColumnByRangePos: (pos: number) => unknown
      navigate: (jump: boolean, expand: boolean, dir: string) => boolean
    }
  }
  rowManager: { element: HTMLElement }
}

/** The selection's active cell: where it began (a range dragged or stretched with Shift keeps it), as in Sheets. */
export function selectedCell(table: Tabulator): CellComponent | null {
  const select = (table as unknown as SelectInner).modules.selectRange
  const start = select?.activeRange ? select.activeRange.start : null
  if (select && start && start.row !== undefined && start.col !== undefined) {
    const row = select.getRowByRangePos(start.row)
    const column = select.getColumnByRangePos(start.col)
    const cell = row && column ? row.getCell(column) : null
    if (cell) return cell.getComponent()
  }
  return (table.getRanges()[0]?.getCells().flat()[0] as CellComponent | undefined) ?? null
}

/**
 * Calls `show` (once per frame) whenever the selected cell or what it holds
 * may have changed, so the bar follows the grid.
 */
export function followSelection(table: Tabulator, show: () => void) {
  let waiting = 0
  const later = () => {
    if (!waiting) waiting = requestAnimationFrame(() => ((waiting = 0), show()))
  }
  for (const event of [
    'rangeAdded',
    'rangeChanged',
    'rangeRemoved',
    'cellEdited',
    'dataProcessed',
    'rowUpdated',
    'renderComplete',
  ])
    table.on(event as 'renderComplete', later)
  return later
}

/**
 * Writes the bar's text into its cell the way a cell's editor does (the grid's
 * cellEdited reads, checks and records it, as a typed or pasted value).
 * False when the row is gone or the cell can no longer be edited.
 */
export function setFromBar(table: Tabulator, target: Pick<CellBarInfo, 'index' | 'field'>, text: string, canEdit: CanEdit) {
  const row = table.getRow(target.index)
  if (!row || !canEdit(row, target.field)) return false
  row.getCell(target.field)?.setValue(text)
  return true
}

/** Back to the grid after the bar (its keys work again), moving the selection as Enter or Tab would. */
export function backToGrid(table: Tabulator, move: Direction | 'here') {
  const inner = table as unknown as SelectInner
  inner.rowManager.element.focus({ preventScroll: true })
  if (move !== 'here') inner.modules.selectRange?.navigate(false, false, move)
}

// ------------------------------------------------------------ widening a column

/**
 * Widening a column by dragging its border in the header. Every grid asks for
 * the header's border only (columnDefaults resizable: 'header'): a finger
 * swiping across the rows started on a cell's border and changed that column's
 * width instead of scrolling. On top of Tabulator's drag:
 * - With a mouse, the pointer at the right edge of the grid (or of the screen)
 *   scrolls the grid on and keeps widening the column, so a column can grow
 *   wider than the room left on screen in one drag.
 * - With a finger, the border is held still a moment first (it turns green),
 *   then dragged: a swipe that starts on it scrolls as anywhere else.
 */
type InnerResize = {
  table: { rowManager: { element: HTMLElement }; options: { resizableColumnGuide?: boolean } }
  startX: number
  resize: (e: MouseEvent | TouchEvent | { clientX: number }, column: unknown) => void
  _mouseDown: (e: MouseEvent | TouchEvent | PlainDown, column: unknown, handle: HTMLElement) => void
  /** While a mouse drags a border: the grid's rows, whose right edge the border stops at. */
  widening?: HTMLElement | null
}
type PlainDown = { clientX: number; stopPropagation: () => void }
/** How close to the edge (px) the pointer starts the scrolling, and how long (ms) a finger holds the border. */
const RESIZE_EDGE = 24
const RESIZE_HOLD = 350
/** The right edge of what shows of the grid's rows (not their scrollbar), within the screen. */
const visibleRight = (holder: HTMLElement) =>
  Math.min(holder.getBoundingClientRect().left + holder.clientWidth, window.innerWidth)
{
  const resize = ResizeColumnsModule.prototype as unknown as InnerResize
  const original = resize._mouseDown
  resize._mouseDown = function (this: InnerResize, e, column, handle) {
    if (typeof TouchEvent !== 'undefined' && e instanceof TouchEvent) return holdToResize(this, e, column, handle, original)
    original.call(this, e, column, handle)
    if (e instanceof MouseEvent && !this.table.options.resizableColumnGuide) widenPastEdge(this, e, column)
  }
  // The border stays in sight at the grid's right edge however far right the pointer goes (the grid scrolls on).
  const resizeTo = resize.resize
  resize.resize = function (this: InnerResize, e, column) {
    const right = this.widening ? visibleRight(this.widening) - 3 : Infinity
    const x = 'clientX' in e ? e.clientX : undefined
    resizeTo.call(this, x !== undefined && x > right ? { clientX: right } : e, column)
  }
}

function widenPastEdge(resize: InnerResize, down: MouseEvent, column: unknown) {
  const holder = resize.table.rowManager.element
  resize.widening = holder
  let x = down.clientX
  // Only once dragged: pressing a border that is already by the edge (a double click to fit it) leaves it alone.
  let dragged = false
  let frame = 0
  const step = () => {
    const edge = visibleRight(holder) - RESIZE_EDGE
    if (dragged && x > edge) {
      // Faster the closer to (or past) the edge; the column grows by what the grid scrolls,
      // so its border stays at the edge.
      const by = Math.round(2 + 10 * Math.min(1, (x - edge) / RESIZE_EDGE))
      resize.startX -= by
      resize.resize({ clientX: x }, column)
      holder.scrollLeft += by
    }
    frame = requestAnimationFrame(step)
  }
  const move = (e: MouseEvent) => {
    x = e.clientX
    dragged ||= Math.abs(x - down.clientX) > 3
  }
  const up = () => {
    cancelAnimationFrame(frame)
    resize.widening = null
    document.removeEventListener('mousemove', move, true)
    window.removeEventListener('mouseup', up, true)
  }
  document.addEventListener('mousemove', move, true)
  window.addEventListener('mouseup', up, true)
  frame = requestAnimationFrame(step)
}

function holdToResize(
  resize: InnerResize,
  start: TouchEvent,
  column: unknown,
  handle: HTMLElement,
  original: InnerResize['_mouseDown'],
) {
  const touch = start.touches[0]
  if (!touch) return
  const { clientX, clientY } = touch
  let held = false
  const timer = window.setTimeout(() => {
    held = true
    handle.classList.add('is-resizing')
    navigator.vibrate?.(10)
    // Tabulator's drag from here on (it follows the finger's moves on the border).
    original.call(resize, { clientX, stopPropagation: () => {} }, column, handle)
  }, RESIZE_HOLD)
  const move = (e: TouchEvent) => {
    // Held: the finger widens the column and nothing scrolls. Not yet: moving is a swipe.
    if (held) {
      if (e.cancelable) e.preventDefault()
      return
    }
    const now = e.touches[0]
    if (now && Math.hypot(now.clientX - clientX, now.clientY - clientY) > 8) end()
  }
  const end = () => {
    window.clearTimeout(timer)
    handle.classList.remove('is-resizing')
    handle.removeEventListener('touchmove', move)
    handle.removeEventListener('touchend', end)
    handle.removeEventListener('touchcancel', end)
  }
  handle.addEventListener('touchmove', move, { passive: false })
  handle.addEventListener('touchend', end)
  handle.addEventListener('touchcancel', end)
}

// ------------------------------------------------------------ fitting a column to its content

/** Up to `limit` rows spread evenly over `rows` (the first and last included). */
export function spread<T>(rows: readonly T[], limit: number): T[] {
  if (rows.length <= limit) return [...rows]
  const out: T[] = []
  const step = (rows.length - 1) / (limit - 1)
  for (let i = 0; i < limit; i++) out.push(rows[Math.round(i * step)])
  return out
}

/**
 * The width that shows the widest of `texts` (and the header) whole: its text
 * plus the cell's padding, within `min` and `max` (longer texts are read in the
 * cell bar).
 */
export function fitWidth(
  texts: Iterable<string>,
  measure: (text: string) => number,
  { padding, header = 0, min = 0, max = 480 }: { padding: number; header?: number; min?: number; max?: number },
) {
  let widest = 0
  const seen = new Set<string>()
  for (const text of texts) {
    if (!text || seen.has(text)) continue
    seen.add(text)
    widest = Math.max(widest, measure(text))
  }
  return Math.round(Math.min(max, Math.max(min, header, widest ? widest + padding + 1 : 0)))
}

let measuring: CanvasRenderingContext2D | null | undefined
/** Text widths in a font, measured on a canvas (no layout); by letter count where there is no canvas. */
function measurer(font: string) {
  if (measuring === undefined) measuring = document.createElement('canvas').getContext('2d')
  const context = measuring
  if (!context) return (text: string) => text.length * 7.2
  context.font = font
  return (text: string) => context.measureText(text).width
}
const fontOf = (style: CSSStyleDeclaration) => `${style.fontStyle} ${style.fontWeight} ${style.fontSize} ${style.fontFamily}`
const px = (value: string) => parseFloat(value) || 0

/**
 * Double-clicking the border at the right of a column's name fits the column
 * to what it holds, as in Google Sheets or Excel: the rows on screen and a
 * sample of the rest (never all of a 100,000-row sheet), measured in the
 * grid's font, up to `max` pixels. (Tabulator's own double click measured every
 * cell drawn so far, one layout each, and had no limit.) `text` is a row's
 * cell as the grid shows it.
 */
export function attachColumnFit(
  table: Tabulator,
  element: HTMLElement,
  { text, max = 480 }: { text?: (data: Record<string, unknown>, field: string) => string; max?: number } = {},
) {
  const textOf = text ?? ((data, field) => String(data[field] ?? ''))
  const onDoubleClick = (event: MouseEvent) => {
    const handle = (event.target as HTMLElement | null)?.closest?.('.tabulator-col-resize-handle')
    const field = handle?.previousElementSibling?.getAttribute('tabulator-field')
    if (!handle || !field) return
    // Before Tabulator's own handler on the border, and no sort or selection from the header.
    event.preventDefault()
    event.stopPropagation()
    fitColumn(table, field, textOf, max)
  }
  element.addEventListener('dblclick', onDoubleClick, true)
  return { destroy: () => element.removeEventListener('dblclick', onDoubleClick, true) }
}

type InnerRow = { type: string; getData: () => Record<string, unknown> }
type InnerTable = {
  rowManager: { getDisplayRows: () => InnerRow[] }
  modules: {
    resizeColumns?: {
      dispatch: (event: string, column: unknown) => void
      dispatchExternal: (event: string, column: unknown) => void
    }
  }
}

/** Fits one column (see attachColumnFit); returns its new width, or null when the grid has no such column. */
export function fitColumn(
  table: Tabulator,
  field: string,
  text: (data: Record<string, unknown>, field: string) => string,
  max = 480,
) {
  const column = table.getColumn(field)
  if (!column) return null
  const inner = table as unknown as InnerTable
  const onScreen = table.getRows('visible')
  const cells = onScreen.map(row => row.getCell(field)?.getElement()).filter(el => el?.isConnected) as HTMLElement[]
  const rows = [
    ...onScreen.map(row => row.getData()),
    ...spread(
      inner.rowManager.getDisplayRows().filter(r => r.type === 'row'),
      1000,
    ).map(r => r.getData()),
  ]
  // The cells' font and padding as drawn (a list cell keeps room for its ▾).
  let padding = 13
  let font = '13px "Fira Sans"'
  if (cells.length) {
    const first = getComputedStyle(cells[0])
    font = fontOf(first)
    padding = Math.max(
      ...cells.slice(0, 30).map(el => {
        const s = getComputedStyle(el)
        return px(s.paddingLeft) + px(s.paddingRight) + px(s.borderLeftWidth) + px(s.borderRightWidth)
      }),
    )
  }
  // The header's name whole, with the room it keeps around it (padding, the sort arrow).
  const head = column.getElement()
  const title = head.querySelector<HTMLElement>('.tabulator-col-title')
  let header = 0
  if (title?.isConnected) {
    const s = getComputedStyle(title)
    const room = head.offsetWidth - (title.clientWidth - px(s.paddingLeft) - px(s.paddingRight))
    header = Math.ceil(measurer(fontOf(s))(title.textContent ?? '') + room)
  }
  const min = (column.getDefinition().minWidth as number | undefined) ?? 40
  const width = fitWidth(
    rows.map(data => text(data, field)),
    measurer(font),
    { padding, header, min, max },
  )
  if (width === column.getWidth()) return width
  column.setWidth(width)
  // As after dragging the border: the selection's outline and anything laid out by column follow.
  const internal = (column as unknown as { _column: unknown })._column
  inner.modules.resizeColumns?.dispatch('column-resized', internal)
  inner.modules.resizeColumns?.dispatchExternal('columnResized', column)
  return width
}

/**
 * While typing in a list cell, the suggestion Enter or Tab will take is
 * highlighted in the list. (Tabulator draws the items once and only hides and
 * shows them as you type, so they are marked after each key.)
 */
function markPick() {
  const input = document.activeElement
  if (!(input instanceof HTMLInputElement) || !input.closest('.tabulator-editing')) return
  const items = [...document.querySelectorAll<HTMLElement>('.tabulator-edit-list .tabulator-edit-list-item')]
  const shown = items.filter(el => el.offsetParent !== null)
  const pick = pickChoice(
    input.value,
    shown.map(el => el.textContent?.trim() ?? ''),
  )
  for (const el of items) el.classList.toggle('is-pick', !!pick && el.offsetParent !== null && el.textContent?.trim() === pick)
}
if (typeof document !== 'undefined') document.addEventListener('keyup', () => setTimeout(markPick, 80), true)
