<script setup lang="ts">
import { onActivated, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { TabulatorFull as Tabulator } from 'tabulator-tables'
import type { CellComponent, ColumnDefinition, RowComponent } from 'tabulator-tables'
import 'tabulator-tables/dist/css/tabulator_simple.min.css'
import { displayValue } from '../../lib/cells'
import {
  attachCopyMarker,
  attachFillHandle,
  attachTouchSheet,
  choiceEditor,
  editingKeys,
  openList,
  spreadsheetKeys,
  tileToSelection,
  typingPending,
  watchSize,
  type CanEdit,
} from '../../lib/gridKit'
import { cellClass, cellTitle, choicesFor, lineState, type ReviewLine } from '../../lib/notebook'
import { parseBlock } from '../../lib/paste'
import type { Field } from '../../lib/types'

/**
 * The lines of a notebook page as a spreadsheet, beside its photo: one row per
 * notebook line with the value read for each column. Green: fills an empty
 * cell · red: the sheet says otherwise (struck-through sheet value → notebook)
 * · amber: doubtful reading (▾ for the other readings) · grey: formula, or only
 * the sheet has a value. Keys and fill handle as in the other grids; Space
 * ticks or unticks the row; ↑/↓ move the band on the photo.
 */
const props = defineProps<{
  lines: ReviewLine[]
  fields: string[]
  types: Record<string, Field['type']>
  keyFields: string[]
  options: Record<string, string[]>
  selected: number | null
  editable: boolean
}>()
const emit = defineEmits<{
  edit: [n: number, field: string, value: string | null]
  pick: [n: number, on: boolean]
  select: [n: number]
  cell: [n: number, field: string | null]
  notice: [message: string]
}>()

type Row = Record<string, string | number | boolean | null> & { __n: number }
const host = ref<HTMLDivElement>()
let table: Tabulator | null = null
let built = false
let index = new Map<number, ReviewLine>()
const touch = window.matchMedia('(pointer: coarse)').matches
const typeOf = (field: string) => props.types[field] ?? 'text'
const show = (field: string, value: unknown) =>
  displayValue((value ?? null) as never, { key: field, type: typeOf(field) })

const toRow = (line: ReviewLine): Row => {
  const row: Row = { __n: line.n, __pick: line.picked, __state: lineState(line).text, __raw: line.raw, __msg: messageOf(line) }
  for (const f of props.fields) row[f] = show(f, line.cells[f]?.value)
  return row
}
function messageOf(line: ReviewLine) {
  if (line.rowError) return line.rowError
  if (line.message) return line.message
  const bad = Object.entries(line.cells).find(([, c]) => c.status === 'error' || (c.doubt && c.include === false && c.message))
  return bad ? `${bad[0]}: ${bad[1].message}` : ''
}
const lineOf = (row: RowComponent) => index.get((row.getData() as Row).__n)
const usable = (line?: ReviewLine) => !!line && (line.status === 'match' || line.status === 'new')

/** Cells that can be typed in: the page's columns, except formulas and crossed-out lines. */
const canEdit: CanEdit = (row, field) => {
  const line = lineOf(row)
  if (!props.editable || !line || !props.fields.includes(field) || line.crossed) return false
  return line.cells[field]?.status !== 'formula'
}

function fieldFormatter(cell: CellComponent) {
  const field = cell.getField()
  const line = lineOf(cell.getRow())
  const c = line?.cells[field]
  const el = cell.getElement()
  for (const name of ['nb-fill', 'nb-conflict', 'nb-doubt', 'nb-error', 'nb-formula', 'nb-mismatch', 'nb-keep'])
    el.classList.remove(name)
  const cls = cellClass(c)
  if (cls) el.classList.add(...cls.split(' '))
  el.classList.toggle('nb-edited', !!c?.edited)
  el.classList.toggle('nb-left-out', !!c && ['fill', 'conflict', 'new'].includes(c.status) && !c.include)
  el.classList.toggle('has-choices', !!line && canEdit(cell.getRow(), field))
  el.title = cellTitle(field, c, typeOf(field))
  if (!c) return ''
  if (c.status === 'keep') return document.createTextNode(show(field, c.before))
  if (c.status === 'conflict' || c.mismatch) {
    const span = document.createElement('span')
    const old = document.createElement('s')
    old.className = 'nb-old'
    old.textContent = show(field, c.before)
    span.append(old, ` ${show(field, cell.getValue())}`)
    return span
  }
  return document.createTextNode(String(cell.getValue() ?? ''))
}

function columns(): ColumnDefinition[] {
  const width = (field: string) => {
    const longest = Math.max(
      field.length,
      ...props.lines.map(l => show(field, l.cells[field]?.value).length + (l.cells[field]?.status === 'conflict' ? show(field, l.cells[field]?.before).length + 1 : 0)),
    )
    return Math.max(64, Math.min(260, Math.round(longest * 7.1 + 26)))
  }
  return [
    {
      title: '✓',
      field: '__pick',
      frozen: true,
      width: 38,
      hozAlign: 'center',
      headerTooltip: 'Filas que se aplican (Espacio)',
      formatter: cell => {
        const line = lineOf(cell.getRow())
        const el = cell.getElement()
        el.classList.add('nb-pick')
        if (!usable(line) || !line!.changes) return line?.applied ? '✔' : ''
        return line!.picked ? '☑' : '☐'
      },
      cellClick: (_e, cell) => toggle(cell.getRow()),
    },
    { title: 'Lín.', field: '__n', frozen: true, width: 46, hozAlign: 'right', cssClass: 'row-number' },
    {
      title: 'Hoja',
      field: '__state',
      // On a phone the frozen columns would leave no room for the values.
      frozen: !touch,
      width: 92,
      formatter: cell => {
        const line = lineOf(cell.getRow())
        if (!line) return ''
        const state = lineState(line)
        const span = document.createElement('span')
        span.className = `nb-state is-${state.tone}`
        span.textContent = state.text
        cell.getElement().title = line.message || ''
        return span
      },
    },
    ...props.fields.map(
      field =>
        ({
          title: field,
          field,
          width: width(field),
          frozen: props.keyFields.includes(field),
          editable: (cell: CellComponent) => canEdit(cell.getRow(), field),
          formatter: fieldFormatter as never,
          ...choiceEditor(cell => choicesFor(lineOf(cell.getRow())?.cells[field], typeOf(field), props.options[field])),
        }) as ColumnDefinition,
    ),
    { title: 'Cuaderno (como está escrito)', field: '__raw', width: 280, cssClass: 'nb-raw' },
    { title: 'Aviso', field: '__msg', width: 320, cssClass: 'nb-msg' },
  ]
}

function toggle(row: RowComponent) {
  const line = lineOf(row)
  if (!props.editable || !usable(line) || !line!.changes) return
  emit('pick', line!.n, !line!.picked)
}

// ---- Keeping the grid in step with the lines, never under an open editor ----
let shown = new Map<number, string>()
let shownKeys = ''
let retry: number | undefined
const busy = () => !!host.value?.querySelector('.tabulator-editing') || typingPending()
function sync() {
  if (!table || !built) return
  if (busy()) {
    window.clearTimeout(retry)
    retry = window.setTimeout(sync, 250)
    return
  }
  index = new Map(props.lines.map(l => [l.n, l]))
  const rows = props.lines.map(toRow)
  const keys = `${props.fields.join('|')}\n${rows.map(r => r.__n).join('|')}`
  if (keys !== shownKeys) {
    if (props.fields.join('|') !== shownKeys.split('\n')[0]) table.setColumns(columns())
    void table.replaceData(rows).then(() => markSelected())
  } else {
    // Markers can change without the text (a cell confirmed, a row applied): the changed lines are redrawn.
    const changed = props.lines.filter(l => shown.get(l.n) !== JSON.stringify(l))
    if (changed.length)
      void table.updateData(changed.map(toRow)).then(() => {
        for (const l of changed) table?.getRow(l.n)?.reformat()
        markSelected()
      })
  }
  shownKeys = keys
  shown = new Map(props.lines.map(l => [l.n, JSON.stringify(l)]))
}

// ---- The selected line, shared with the photo -----------------------------
let fromGrid = false
function currentLine(): { n: number; field: string | null } | null {
  const range = table?.getRanges()[0]
  const row = range?.getRows()[0]
  if (!row) return null
  const field = range.getColumns()[0]?.getField() ?? null
  return { n: (row.getData() as Row).__n, field: field && props.fields.includes(field) ? field : null }
}
function onRange() {
  const at = currentLine()
  if (!at) return
  emit('cell', at.n, at.field)
  if (at.n !== props.selected) {
    fromGrid = true
    emit('select', at.n)
  }
  markSelected()
}
function markSelected() {
  if (!table) return
  for (const row of table.getRows()) row.getElement().classList.toggle('nb-row-selected', (row.getData() as Row).__n === props.selected)
  for (const row of table.getRows()) {
    const line = lineOf(row)
    row.getElement().classList.toggle('nb-row-off', !!line && usable(line) && line.changes > 0 && !line.picked)
    row.getElement().classList.toggle('nb-row-applied', !!line?.applied)
  }
}
/** A line chosen on the photo: its first changed (or first) cell is selected and scrolled to. */
function selectLine(n: number) {
  if (!table || !built) return
  const row = table.getRow(n)
  if (!row) return
  markSelected()
  if (currentLine()?.n === n) return
  const line = index.get(n)
  const field = props.fields.find(f => line?.cells[f]?.include || line?.cells[f]?.doubt) ?? props.fields[0]
  void table.scrollToRow(row, 'center', false).catch(() => {})
  try {
    ;(table as unknown as { addRange: (a: CellComponent, b: CellComponent) => void }).addRange(row.getCell(field), row.getCell(field))
  } catch {
    /* Range selection is off on some touch screens. */
  }
  // After a tap on the photo, ↑/↓ and typing go on in the grid.
  if (!touch) focus()
}
watch(
  () => props.selected,
  n => {
    if (fromGrid) fromGrid = false
    else if (n !== null) selectLine(n)
    markSelected()
  },
)

// ---- Paste: a block of cells from the selected one, skipping read-only cells ----
function pasteParser(text: string) {
  const range = table?.getRanges()[0]
  const first = (range?.getCells().flat() as CellComponent[] | undefined)?.[0]
  if (!table || !range || !first) return false
  const block = tileToSelection(parseBlock(text) ?? [[text.replace(/\r?\n$/, '')]], range.getRows().length, range.getColumns().length)
  const visible = table.getColumns().filter(c => c.isVisible()).map(c => c.getField())
  const start = visible.indexOf(first.getField())
  const rows = table.getRows('active')
  const at = rows.indexOf(first.getRow())
  let skipped = 0
  block.forEach((line, i) => {
    const row = rows[at + i]
    if (!row) return
    line.forEach((value, j) => {
      const field = visible[start + j]
      if (!field) return
      if (canEdit(row, field)) row.getCell(field).setValue(value.trim())
      else skipped++
    })
  })
  if (skipped) emit('notice', `${skipped} celdas de solo lectura no se modificaron`)
  return false
}

const notice = (message: string) => emit('notice', message)
const onKeydown = spreadsheetKeys(() => table, canEdit, notice)
const onEditingKey = editingKeys(() => table)
/** Space ticks or unticks the selected rows (before the keys that would start typing a space). */
function onSpace(event: KeyboardEvent) {
  if (event.key !== ' ' || event.ctrlKey || event.metaKey || event.altKey) return
  if ((event.target as HTMLElement).closest('input, textarea, select, .tabulator-editing') || !table) return
  const rows = table.getRanges()[0]?.getRows() ?? []
  if (!rows.length) return
  event.preventDefault()
  event.stopImmediatePropagation()
  for (const row of rows) toggle(row)
}
let fill: { destroy: () => void } | null = null
let copied: ReturnType<typeof attachCopyMarker> | null = null
let sizeWatch: { disconnect: () => void } | null = null

onMounted(() => {
  if (!host.value) return
  index = new Map(props.lines.map(l => [l.n, l]))
  table = new Tabulator(host.value, {
    data: [],
    index: '__n',
    columns: columns(),
    height: '100%',
    layout: 'fitData',
    autoResize: false,
    placeholder: 'Sin líneas',
    selectableRange: 1,
    selectableRangeColumns: true,
    selectableRangeRows: true,
    selectableRangeClearCells: false,
    editTriggerEvent: 'dblclick',
    clipboard: true,
    clipboardCopyConfig: { columnHeaders: false, rowHeaders: false, formatCells: false },
    clipboardCopyRowRange: 'range',
    clipboardPasteParser: pasteParser,
    clipboardPasteAction: () => [],
    columnDefaults: { headerSort: false },
  } as unknown as ConstructorParameters<typeof Tabulator>[1])
  table.on('tableBuilt', () => {
    built = true
    sync()
    if (props.selected !== null) setTimeout(() => selectLine(props.selected!), 50)
  })
  table.on('cellEdited', (cell: CellComponent) => {
    const line = lineOf(cell.getRow())
    if (!line) return
    const text = String(cell.getValue() ?? '').trim()
    emit('edit', line.n, cell.getField(), text || null)
  })
  table.on('rangeAdded', onRange)
  table.on('rangeChanged', onRange)
  // The ▾ arrow at a cell's right edge opens its list straight away.
  table.on('cellClick', (event: UIEvent, cell: CellComponent) => {
    const el = cell.getElement()
    if (!el.classList.contains('has-choices') || !(event instanceof MouseEvent)) return
    if (event.clientX >= el.getBoundingClientRect().right - 22) openList(cell)
  })
  const container = host.value.parentElement!
  fill = touch
    ? attachTouchSheet(table, container, { canEdit, notice })
    : attachFillHandle(table, container, {
        canEdit,
        onFilled: rows => notice(`Copiado a ${rows} ${rows === 1 ? 'fila' : 'filas'}`),
      })
  copied = attachCopyMarker(table, container, notice)
  host.value.addEventListener('keydown', onSpace, true)
  host.value.addEventListener('keydown', onKeydown)
  host.value.addEventListener('keydown', onEditingKey, true)
  sizeWatch = watchSize(() => table, host.value)
})
onActivated(() => {
  if (table && built && host.value?.offsetParent) table.redraw()
})
onBeforeUnmount(() => {
  window.clearTimeout(retry)
  fill?.destroy()
  copied?.destroy()
  sizeWatch?.disconnect()
  host.value?.removeEventListener('keydown', onSpace, true)
  host.value?.removeEventListener('keydown', onKeydown)
  host.value?.removeEventListener('keydown', onEditingKey, true)
  table?.destroy()
  table = null
})
watch(() => [props.lines, props.fields, props.editable], sync, { deep: true })

/** Keys go to the grid (after a band was tapped on the photo, ↑/↓ keep working). */
function focus() {
  ;(table as unknown as { rowManager?: { element: HTMLElement } } | null)?.rowManager?.element.focus({ preventScroll: true })
}
defineExpose({ focus })
</script>

<template>
  <div class="sheet-grid nb-grid h-full">
    <div ref="host" class="h-full" tabindex="-1" />
  </div>
</template>
