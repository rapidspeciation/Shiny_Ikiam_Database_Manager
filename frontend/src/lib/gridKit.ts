import type { CellComponent, RowComponent, Tabulator } from 'tabulator-tables'

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
  if (!range) return notice('Selecciona un rango de celdas para rellenar')
  const rows = range.getRows()
  if (rows.length < 2) return notice('Selecciona al menos dos filas para rellenar hacia abajo')
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
  for (const cell of range.getCells().flat() as CellComponent[])
    if (canEdit(cell.getRow(), cell.getField())) cell.setValue(null)
}

/**
 * Spreadsheet keys: typing on a selected cell replaces its content; Enter or
 * F2 edits it in place; Ctrl+D fills down; Supr clears the selection.
 */
export function spreadsheetKeys(table: () => Tabulator | null, canEdit: CanEdit, notice: Notice) {
  // Keys typed while a cell's editor is still opening are kept and given to it,
  // so a fast typist does not lose the first letters.
  let opening: { cell: CellComponent; text: string } | null = null
  function giveText(tries = 0) {
    if (!opening) return
    const input = opening.cell.getElement().querySelector('input')
    if (!input) return tries < 20 ? requestAnimationFrame(() => giveText(tries + 1)) : void (opening = null)
    input.value = opening.text
    input.dispatchEvent(new Event('input', { bubbles: true }))
    // List editors (Tabulator's autocomplete) notice typing on keyup, not on input.
    input.dispatchEvent(new KeyboardEvent('keyup', { key: opening.text.at(-1), bubbles: true }))
    input.setSelectionRange(input.value.length, input.value.length)
    opening = null
  }
  return (event: KeyboardEvent) => {
    const t = table()
    const typing = event.key.length === 1 && !event.ctrlKey && !event.metaKey && !event.altKey
    if (opening && typing) {
      event.preventDefault()
      opening.text += event.key
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
    } else if ((event.ctrlKey || event.metaKey) && event.key.toLowerCase() === 'd') {
      event.preventDefault()
      fillDown(t, canEdit, notice)
    } else if (event.key === 'Delete') {
      event.preventDefault()
      clearRange(t, canEdit)
    }
  }
}

/** What scrolls while dragging: the grid itself (Tablas), or the page around it (the Colecta list). */
function scrollingAround(container: HTMLElement): HTMLElement | null {
  const box = container.querySelector<HTMLElement>('.tabulator-tableholder')
  if (box && box.scrollHeight > box.clientHeight + 1) return box
  for (let el = container.parentElement; el; el = el.parentElement) {
    const overflow = getComputedStyle(el).overflowY
    if ((overflow === 'auto' || overflow === 'scroll') && el.scrollHeight > el.clientHeight + 1) return el
  }
  return null
}

/**
 * The fill handle (computers): a small square at the bottom-right corner of
 * the selected cells. Dragging it down copies
 * those cells to the rows it passes over, repeating them if several rows were
 * selected, as in Excel or Sheets. Read-only cells are skipped.
 */
export function attachFillHandle(
  table: Tabulator,
  container: HTMLElement,
  { canEdit, onFilled }: { canEdit: CanEdit; onFilled?: (count: number) => void },
) {
  const handle = document.createElement('div')
  handle.className = 'fill-handle'
  handle.title = 'Arrastra hacia abajo para copiar'
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
            target.getCell(f).setValue(src.getCell(f).getValue())
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
  for (const event of ['rangeAdded', 'rangeChanged', 'rangeRemoved', 'scrollVertical', 'scrollHorizontal', 'renderComplete'])
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
    if (n) notice(`Copiado: ${n} ${n === 1 ? 'celda' : 'celdas'}. Selecciona dónde pegar y pulsa Ctrl+V`)
  })
  table.on('cellEditing', clear)
  const later = () => requestAnimationFrame(place)
  for (const event of ['scrollVertical', 'scrollHorizontal', 'renderComplete', 'dataProcessed'])
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
 * double-tap it (or "Editar") to edit; drag the round handle on the
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
  handle.title = 'Arrastra para ampliar la selección'
  container.appendChild(handle)
  const bar = document.createElement('div')
  bar.className = 'touch-actions'
  const button = (label: string, action: () => void) => {
    const b = document.createElement('button')
    b.type = 'button'
    b.textContent = label
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
    if (!text) return notice('No hay nada copiado todavía')
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
  function place() {
    if (dragging) return
    // Only the grid last touched shows its handle and bar (a page can hold two grids,
    // and Tabulator selects each grid's first cell on its own).
    if (touchedSheet !== container) return hide()
    const range = table.getRanges()[0]
    const cells = range?.getCells().flat() as CellComponent[] | undefined
    const last = cells?.at(-1)?.getElement()
    if (!range || !last?.isConnected) return hide()
    if (bar.style.display !== 'flex') shownAt = Date.now()
    bar.style.display = 'flex'
    barRoom(true)
    const cell = last.getBoundingClientRect()
    const view = (container.querySelector('.tabulator-tableholder') as HTMLElement | null)?.getBoundingClientRect()
    if (view && (cell.bottom < view.top || cell.bottom > view.bottom + 1 || cell.right < view.left || cell.right > view.right + 1)) {
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

  // A double tap edits the cell (one tap selects it, as in Google Sheets).
  let lastTap: { cell: CellComponent; at: number } | null = null
  const remember = () => {
    if (touchedSheet !== container) {
      touchedSheet = container
      // After the tap is over: shown now, the bar could take the tap's own click.
      setTimeout(() => window.dispatchEvent(new Event('touch-sheet')), 400)
    }
  }
  container.addEventListener('pointerdown', remember, true)
  const onOtherSheet = () => place()
  window.addEventListener('touch-sheet', onOtherSheet)
  table.on('cellClick', (_e: UIEvent, cell: CellComponent) => {
    const now = Date.now()
    const double =
      lastTap && now - lastTap.at < 450 && lastTap.cell.getRow() === cell.getRow() && lastTap.cell.getField() === cell.getField()
    lastTap = double ? null : { cell, at: now }
    // Deferred: opened during the tap's own click, the editor would close as the grid takes the focus.
    if (double && canEdit(cell.getRow(), cell.getField())) setTimeout(() => cell.edit(true), 30)
  })

  const later = () => requestAnimationFrame(place)
  for (const event of ['rangeAdded', 'rangeChanged', 'rangeRemoved', 'scrollVertical', 'scrollHorizontal', 'renderComplete'])
    table.on(event as 'renderComplete', later)
  return {
    place,
    destroy: () => {
      container.removeEventListener('pointerdown', remember, true)
      window.removeEventListener('touch-sheet', onOtherSheet)
      if (touchedSheet === container) {
        touchedSheet = null
        barRoom(false)
      }
      handle.remove()
      bar.remove()
    },
  }
}
