<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { TabulatorFull as Tabulator } from 'tabulator-tables'
import type { CellComponent, ColumnDefinition, RowComponent } from 'tabulator-tables'
import 'tabulator-tables/dist/css/tabulator_simple.min.css'
import { displayValue, normalizeInput } from '../../lib/cells'
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
import { parseBlock } from '../../lib/paste'
import { ID_COLUMN, cellId, cellOf, rowKey, type ProposalChange } from '../../lib/proposals'
import type { CellValue, Field } from '../../lib/types'
import { listProblem, verificationsFor } from '../../lib/verifications'
import { locale, t, tn } from '../../lib/i18n'

/**
 * The rows of one sheet of a proposal as a spreadsheet, like the Colecta list:
 * Enter/Tab move, typing replaces, the fill handle and Ctrl+D copy down, blocks
 * paste from Excel or Sheets, list columns open the sheet's list, dates are
 * read day first. The assistant's values are green, the person's blue, an
 * existing row's other cells grey; cells the assistant just changed flash.
 * Edits go out through `edit` (the parent saves them to the proposal).
 */
export interface CellEdit {
  key: string
  field: string
  value: CellValue
  /** What the cell showed when the person started typing. */
  before: CellValue
}
const props = defineProps<{
  sheet: string
  changes: ProposalChange[]
  fields: string[]
  types: Record<string, string>
  /** Formula columns of the pre-made rows new rows go into. */
  newRowFormulas: string[]
  editable: boolean
  /** Ticks: 'pending' shows them, 'applied' shows ✓ on the rows written. */
  ticks: 'pending' | 'applied' | 'none'
  unticked: Set<string>
  applied: number[]
  /** Cells to flash (the assistant just changed them). */
  flash: Set<string>
}>()
const emit = defineEmits<{
  edit: [cells: CellEdit[]]
  toggle: [key: string]
  toggleAll: []
  remove: [key: string]
  notice: [message: string]
}>()

type Row = Record<string, CellValue> & {
  __key: string
  __tick: string
  __row: string
  __label: string
  __note: string
  __state: string
}

const host = ref<HTMLDivElement>()
let table: Tabulator | null = null
let built = false
let byKey = new Map<string, ProposalChange>()
const touch = typeof window !== 'undefined' && window.matchMedia('(pointer: coarse)').matches
// On a phone the ticks, row and ID scroll with the rest: kept in place they took half the screen.
const wide = typeof window !== 'undefined' && window.matchMedia('(min-width: 768px)').matches
const rules = computed(() => verificationsFor(props.sheet))
const fieldSet = computed(() => new Set(props.fields))
const typeOf = (field: string) => (props.types[field] ?? 'text') as Field['type']
const show = (field: string, value: CellValue | undefined) => displayValue(value, { key: field, type: typeOf(field) })

function info(key: string, field: string) {
  const change = byKey.get(key)
  return change ? cellOf(change, field, props.newRowFormulas) : null
}
const canEditCell = (key: string, field: string) =>
  props.editable && fieldSet.value.has(field) && !!info(key, field) && info(key, field)!.kind !== 'locked'
const canEdit: CanEdit = (row, field) => canEditCell((row.getData() as Row).__key, field)
/** A list to pick from: the sheet's dropdown, except for identifiers (typed or pasted). */
const choicesOf = (field: string) => (ID_COLUMN.test(field) ? null : rules.value?.lists[field]?.values)
const hasChoices = (field: string) => !!choicesOf(field)?.size

function toRow(c: ProposalChange): Row {
  const key = rowKey(c)
  const tick =
    props.ticks === 'pending'
      ? props.unticked.has(key)
        ? '0'
        : '1'
      : props.ticks === 'applied' && props.applied.includes(c.index)
        ? 'applied'
        : ''
  const out = {
    __key: key,
    __tick: tick,
    __row: c.row ? String(c.row) : t('nueva'),
    __label: c.label,
    __note: c.note ?? '',
  } as Row
  let state = ''
  for (const f of props.fields) {
    const cell = cellOf(c, f, props.newRowFormulas)
    out[f] = cell.value
    state += cell.kind[0] + (props.flash.has(cellId(key, f)) ? '*' : '')
  }
  // Markers can change without the value (whose edit it is, a flash): part of the row's signature.
  out.__state = state + JSON.stringify(c.personEdits ?? null) + (props.editable ? 'e' : '')
  return out
}

/** The cell as the table shows it: the value, and in an existing row the sheet's value struck through. */
function formatter(field: string) {
  return (cell: CellComponent) => {
    const row = cell.getData() as Row
    const el = cell.getElement()
    const c = info(row.__key, field)
    if (!c) return ''
    const change = byKey.get(row.__key)!
    const changed = c.kind === 'proposed' || c.kind === 'person'
    const problem = changed ? listProblem(rules.value, field, c.value) : null
    el.classList.toggle('is-proposed', c.kind === 'proposed')
    el.classList.toggle('is-person', c.kind === 'person')
    el.classList.toggle('is-sheet', c.kind === 'sheet')
    el.classList.toggle('is-formula', c.kind === 'locked')
    el.classList.toggle('is-invalid', !!problem)
    el.classList.toggle('is-flash', props.flash.has(cellId(row.__key, field)))
    el.classList.toggle('has-choices', canEditCell(row.__key, field) && hasChoices(field))
    const was = c.was === undefined ? '' : show(field, c.was) || t('vacío')
    const before = change.replaceFormula?.includes(field) ? 'Antes: {value} (fórmula)' : 'Antes: {value}'
    el.title = [
      problem,
      c.kind === 'proposed' && !change.create ? t(before, { value: was }) : '',
      c.kind === 'person'
        ? [
            t('Editado por ti'),
            c.aiProposed ? t('la IA proponía: {value}', { value: show(field, c.ai) || t('vacío') }) : '',
            change.create ? '' : t('en la hoja: {value}', { value: was }),
          ]
            .filter(Boolean)
            .join(' · ')
        : '',
      c.kind === 'locked' ? t('Fórmula de la hoja: no se escribe') : '',
      c.kind === 'sheet' && props.editable ? t('Valor actual de la hoja; escribe para cambiarlo') : '',
    ]
      .filter(Boolean)
      .join('\n')
    const text = show(field, c.value)
    if (!changed || change.create || c.was === undefined || show(field, c.was) === text) return document.createTextNode(text)
    const box = document.createElement('span')
    box.textContent = text || t('vacío')
    const old = document.createElement('s')
    old.className = 'was'
    old.textContent = show(field, c.was) || t('vacío')
    box.append(' ', old)
    return box
  }
}

function tickFormatter(cell: CellComponent) {
  const value = cell.getValue()
  if (value === 'applied') return '✓'
  if (!value) return ''
  const box = document.createElement('input')
  box.type = 'checkbox'
  box.checked = value === '1'
  box.tabIndex = -1
  box.style.pointerEvents = 'none'
  return box
}

function widthOf(field: string) {
  let chars = field.length + 2
  for (const c of props.changes) {
    const cell = cellOf(c, field, props.newRowFormulas)
    const text = show(field, cell.value) + (cell.was !== undefined && cell.kind !== 'sheet' ? ` ${show(field, cell.was)}` : '')
    chars = Math.max(chars, text.length)
  }
  return Math.max(70, Math.min(260, Math.round(chars * 7.2 + 28)))
}

function columns(): ColumnDefinition[] {
  const cols: ColumnDefinition[] = []
  if (props.ticks !== 'none')
    cols.push({
      title: '✓',
      field: '__tick',
      width: 34,
      frozen: wide,
      hozAlign: 'center',
      headerHozAlign: 'center',
      headerTooltip: t('Elegir todas las filas o ninguna'),
      formatter: tickFormatter as never,
      cellClick: (_e, cell) => props.ticks === 'pending' && emit('toggle', (cell.getData() as Row).__key),
      headerClick: () => props.ticks === 'pending' && emit('toggleAll'),
    })
  cols.push(
    { title: t('Fila'), field: '__row', width: 54, frozen: wide, hozAlign: 'right', cssClass: 'row-number', headerSort: false },
    {
      title: 'ID',
      field: '__label',
      frozen: wide,
      headerSort: false,
      cssClass: 'proposal-label',
      // The row's note (where its values come from) also on the ID, as the Nota column is at the far right.
      tooltip: (_e: MouseEvent, cell: CellComponent) => (cell.getData() as Row).__note,
    } as ColumnDefinition,
  )
  for (const field of props.fields) {
    const choices = hasChoices(field)
    cols.push({
      title: field,
      field,
      width: widthOf(field),
      minWidth: 70,
      headerSort: false,
      formatter: formatter(field) as never,
      // Copied as shown (dates 14-Aug-25, times 9:05), without the struck-through old value.
      formatterClipboard: ((cell: CellComponent) => show(field, cell.getValue())) as never,
      editable: (cell: CellComponent) => canEditCell((cell.getData() as Row).__key, field),
      ...(choices
        ? choiceEditor(() => [...(choicesOf(field) ?? [])])
        : { editor: 'input' as const, editorParams: { selectContents: true } }),
    } as ColumnDefinition)
  }
  cols.push({
    title: t('Nota'),
    field: '__note',
    headerSort: false,
    width: 260,
    cssClass: 'proposal-note',
    formatter: 'plaintext',
  })
  if (props.editable)
    cols.push({
      title: '',
      field: '__remove',
      width: 34,
      hozAlign: 'center',
      headerSort: false,
      cssClass: 'row-remove',
      formatter: () => '✕',
      tooltip: t('Quitar esta fila de la propuesta'),
      cellClick: (_e, cell) => emit('remove', (cell.getData() as Row).__key),
    } as ColumnDefinition)
  return cols
}

// ------------------------------------------------------------ edits
let normalizing = false
let outgoing: CellEdit[] = []
function send() {
  if (!outgoing.length) return
  const cells = outgoing
  outgoing = []
  emit('edit', cells)
}
function onCellEdited(cell: CellComponent) {
  if (normalizing) return
  const field = cell.getField()
  if (!fieldSet.value.has(field)) return
  const row = cell.getData() as Row
  const before = (cell.getOldValue() ?? null) as CellValue
  const result = normalizeInput(cell.getValue(), { key: field, type: typeOf(field) }, props.sheet)
  if (!result.ok) {
    emit('notice', result.message)
    normalizing = true
    cell.restoreOldValue()
    normalizing = false
    return
  }
  if (result.value !== cell.getValue()) {
    normalizing = true
    cell.setValue(result.value)
    normalizing = false
  }
  if (JSON.stringify(before) === JSON.stringify(result.value)) return
  const list = rules.value?.lists[field]
  const problem = list && listProblem(rules.value, field, result.value)
  if (problem) emit('notice', list.strict ? problem : t('{problem}: se guarda igual; corrígelo si es un error', { problem }))
  outgoing.push({ key: row.__key, field, value: result.value, before })
  // A paste or a fill sets many cells at once: they go out together.
  if (outgoing.length === 1) queueMicrotask(send)
}

/** The copied block as rows of { field: text }, from the first selected column on (as SheetGrid). */
function pasteParser(text: string) {
  const range = table?.getRanges()[0]
  if (!table || !range) return false
  const block = parseBlock(text) ?? [[text.replace(/\r?\n$/, '')]]
  const tiled = tileToSelection(block, range.getRows().length, range.getColumns().length)
  const visible = table.getColumns().filter(c => c.isVisible())
  const first = range.getColumns()[0]?.getField()
  const start = visible.findIndex(c => c.getField() === first)
  if (start < 0) return false
  const fields = visible.slice(start, start + (tiled[0]?.length ?? 0)).map(c => c.getField())
  return tiled.map(line => Object.fromEntries(fields.map((f, j) => [f, line[j]])))
}
function pasteRange(rowsData: Record<string, unknown>[]) {
  if (!table || !rowsData.length) return []
  const selected = table.getRanges()[0]?.getRows() || []
  if (!selected.length) return []
  const active = table.getRows('active')
  const start = active.indexOf(selected[0])
  if (start < 0) return []
  let skipped = 0
  const touched: RowComponent[] = []
  for (const [offset, row] of active.slice(start, start + rowsData.length).entries()) {
    for (const [field, raw] of Object.entries(rowsData[offset])) {
      if (!fieldSet.value.has(field)) continue
      if (!canEdit(row, field)) {
        skipped++
        continue
      }
      row.getCell(field).setValue(raw === undefined ? null : String(raw))
    }
    touched.push(row)
  }
  if (skipped) emit('notice', t('{n} celdas de solo lectura no se modificaron', { n: skipped }))
  return touched
}

// ------------------------------------------------------------ drawing
let shown = new Map<string, string>()
let shownOrder = ''
let shownColumns = ''
let stale = false
let retry: number | undefined
/** A cell is being edited, or keys typed wait for its editor: redrawing now would throw away what is typed. */
const busy = () => !!host.value?.querySelector('.tabulator-editing') || typingPending()
function sync() {
  if (!table || !built) return
  if (busy()) {
    stale = true
    window.clearTimeout(retry)
    retry = window.setTimeout(sync, 250)
    return
  }
  stale = false
  byKey = new Map(props.changes.map(c => [rowKey(c), c]))
  const rows = props.changes.map(toRow)
  // The language is part of it: the column titles and tooltips are in it.
  const layout = [props.fields.join('|'), props.editable, props.ticks, rules.value ? 1 : 0, locale.value].join('\n')
  if (layout !== shownColumns) {
    shownColumns = layout
    table.setColumns(columns())
    shownOrder = ''
  }
  const order = rows.map(r => r.__key).join('|')
  if (order !== shownOrder) table.replaceData(rows)
  else {
    const changed = rows.filter(r => shown.get(r.__key) !== JSON.stringify(r))
    if (changed.length)
      table.updateData(changed).then(() => {
        for (const r of changed) table?.getRow(r.__key)?.reformat()
      })
  }
  shownOrder = order
  shown = new Map(rows.map(r => [r.__key, JSON.stringify(r)]))
}

const onKeydown = spreadsheetKeys(
  () => table,
  canEdit,
  message => emit('notice', message),
)
const onEditingKey = editingKeys(() => table)
let fill: { destroy: () => void } | null = null
let copied: ReturnType<typeof attachCopyMarker> | null = null
let sizeWatch: { disconnect: () => void } | null = null

onMounted(() => {
  if (!host.value) return
  byKey = new Map(props.changes.map(c => [rowKey(c), c]))
  shownColumns = [props.fields.join('|'), props.editable, props.ticks, rules.value ? 1 : 0, locale.value].join('\n')
  table = new Tabulator(host.value, {
    data: [],
    index: '__key',
    columns: columns(),
    layout: 'fitData',
    autoResize: false,
    placeholder: t('Sin filas'),
    selectableRange: 1,
    selectableRangeColumns: true,
    selectableRangeRows: true,
    selectableRangeClearCells: false,
    editTriggerEvent: 'dblclick',
    clipboard: true,
    clipboardCopyConfig: { columnHeaders: false, rowHeaders: false, formatCells: true },
    clipboardCopyRowRange: 'range',
    clipboardPasteParser: pasteParser,
    clipboardPasteAction: pasteRange,
    columnDefaults: { headerSort: false },
  } as unknown as ConstructorParameters<typeof Tabulator>[1])
  table.on('tableBuilt', () => {
    built = true
    sync()
  })
  table.on('cellEdited', onCellEdited)
  table.on('cellEditCancelled', () => stale && sync())
  table.on('cellEdited', () => stale && setTimeout(sync))
  // The ▾ arrow opens the list at once.
  table.on('cellClick', (event: UIEvent, cell: CellComponent) => {
    const el = cell.getElement()
    if (!el.classList.contains('has-choices') || !(event instanceof MouseEvent)) return
    if (event.clientX >= el.getBoundingClientRect().right - 22) openList(cell)
  })
  const notice = (message: string) => emit('notice', message)
  const container = host.value.parentElement!
  fill = touch
    ? attachTouchSheet(table, container, { canEdit, notice })
    : attachFillHandle(table, container, {
        canEdit,
        onFilled: rows => notice(tn(rows, 'Copiado a {n} fila', 'Copiado a {n} filas')),
      })
  copied = attachCopyMarker(table, container, notice)
  host.value.addEventListener('keydown', onKeydown)
  host.value.addEventListener('keydown', onEditingKey, true)
  sizeWatch = watchSize(() => table, host.value)
  // Built while the panel was hidden (closed, or the tab in the background): drawn when it shows.
  let hidden = !host.value.offsetWidth
  shownWatch = new ResizeObserver(([entry]) => {
    const zero = !entry.contentRect.width
    if (hidden && !zero && table && built && !busy()) table.redraw(true)
    hidden = zero
  })
  shownWatch.observe(host.value)
})
let shownWatch: ResizeObserver | null = null
onBeforeUnmount(() => {
  window.clearTimeout(retry)
  fill?.destroy()
  copied?.destroy()
  sizeWatch?.disconnect()
  shownWatch?.disconnect()
  host.value?.removeEventListener('keydown', onKeydown)
  host.value?.removeEventListener('keydown', onEditingKey, true)
  table?.destroy()
  table = null
})
watch(
  () => [
    props.changes,
    props.fields,
    props.unticked,
    props.flash,
    props.editable,
    props.ticks,
    props.applied,
    rules.value,
    locale.value,
  ],
  sync,
)
</script>

<template>
  <div class="sheet-grid proposal-sheet">
    <div ref="host" tabindex="-1" />
  </div>
</template>
