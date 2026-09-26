<script setup lang="ts">
import { onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { TabulatorFull as Tabulator } from 'tabulator-tables'
import type { CellComponent, ColumnDefinition, RowComponent } from 'tabulator-tables'
import 'tabulator-tables/dist/css/tabulator_simple.min.css'
import { displayValue, normalizeInput } from '../lib/cells'
import type { CellValue, Field, TableRow } from '../lib/types'
import { type PendingCreate, usePending } from '../stores/pending'
import { useSession } from '../stores/session'
import RowDrawer from './RowDrawer.vue'

/**
 * Spreadsheet-like view of one sheet. Edits go to the pending store and are
 * only written to Google Sheets when the person presses "Guardar".
 */
const props = withDefaults(
  defineProps<{
    module: string
    rows: TableRow[]
    columns: Field[]
    creates?: PendingCreate[]
    frozen?: string[]
    options?: Record<string, string[]>
    lockedFields?: string[]
    labelField?: string
    search?: string
    headerFilters?: boolean
    newestFirst?: boolean
    height?: string
    /** Choices that depend on other cells of the row (e.g. subspecies of the row's species). */
    rowOptions?: Record<string, (row: Record<string, CellValue>) => string[]>
    /** Fields that are formulas in the rows new records will be written into. */
    createFormulas?: string[]
    /** Row ids to highlight, e.g. the IDs a person just loaded. */
    highlight?: string[]
  }>(),
  {
    creates: () => [],
    frozen: () => [],
    options: () => ({}),
    lockedFields: () => [],
    headerFilters: true,
    newestFirst: true,
    height: '100%',
    rowOptions: () => ({}),
    createFormulas: () => [],
    highlight: () => [],
  },
)
const emit = defineEmits<{ notice: [message: string]; removeCreate: [clientId: string] }>()

type GridRow = Record<string, CellValue> & { __id: string; __row: number | null; __new: string | null }

const pending = usePending()
const session = useSession()
const host = ref<HTMLDivElement>()
const drawerId = ref<string | null>(null)
let table: Tabulator | null = null
// Tabulator builds asynchronously; data changes before that point wait for it.
let built = false
let refreshWhenBuilt = false
let formulaIndex = new Map<string, Set<string>>()
let rowIndex = new Map<string, TableRow>()
let fieldIndex = new Map<string, Field>()

const touchDevice = typeof window !== 'undefined' && window.matchMedia('(pointer: coarse)').matches

function labelOf(values: Record<string, CellValue>) {
  const key = props.labelField || session.module(props.module)?.identityFields[0]
  return String((key && values[key]) || '')
}

function isFormula(data: GridRow, field: string) {
  return data.__new ? props.createFormulas.includes(field) : !!formulaIndex.get(data.__id)?.has(field)
}

function canEdit(data: GridRow, field: string) {
  const def = fieldIndex.get(field)
  return session.canEdit && !!def && !def.readonly && !props.lockedFields.includes(field) && !isFormula(data, field)
}

function indexRows() {
  formulaIndex = new Map(props.rows.map(r => [r.id, new Set(r.formulas)]))
  rowIndex = new Map(props.rows.map(r => [r.id, r]))
}

function buildData(): GridRow[] {
  indexRows()
  const created = props.creates.map(c => ({ ...c.values, __id: c.clientId, __row: null, __new: c.clientId }) as GridRow)
  const existing = props.rows.map(r => {
    const out: GridRow = { __id: r.id, __row: r.row, __new: null } as GridRow
    for (const f of props.columns) out[f.key] = pending.value(r, f.key)
    return out
  })
  return [...created, ...existing]
}

/** Columns edited with a dropdown list; their editable cells show a ▾ arrow. */
function hasChoices(field: string) {
  return !!props.rowOptions[field] || !!props.options[field]?.length
}

/** Clicking the ▾ arrow opens the list straight away, without a double click. */
function onCellClick(event: UIEvent, cell: CellComponent) {
  const el = cell.getElement()
  if (!el.classList.contains('has-choices') || !(event instanceof MouseEvent)) return
  // Deferred: the click that selects the cell would otherwise close the list at once.
  if (event.clientX >= el.getBoundingClientRect().right - 22) setTimeout(() => cell.edit(true))
}

function formatter(cell: CellComponent) {
  const data = cell.getData() as GridRow
  const field = cell.getField()
  const el = cell.getElement()
  const errorKey = `${data.__id}:${field}`
  el.classList.toggle('is-formula', isFormula(data, field))
  el.classList.toggle('is-locked', !isFormula(data, field) && !canEdit(data, field))
  el.classList.toggle('is-dirty', !data.__new && pending.isDirty(data.__id, field))
  el.classList.toggle('is-error', !!pending.errors[errorKey])
  el.classList.toggle('has-choices', hasChoices(field) && canEdit(data, field))
  el.title = pending.errors[errorKey] || (isFormula(data, field) ? 'Fórmula de la hoja (solo lectura)' : '')
  return document.createTextNode(displayValue(cell.getValue(), fieldIndex.get(field)))
}

function rowNumberFormatter(cell: CellComponent) {
  const data = cell.getData() as GridRow
  const el = cell.getElement()
  el.classList.toggle('is-new', !!data.__new)
  const text = data.__new ? 'nueva' : String(data.__row)
  return document.createTextNode(text)
}

function editorFor(field: Field): Partial<ColumnDefinition> {
  const params = (values: string[]) => ({
    values,
    autocomplete: true,
    freetext: true,
    allowEmpty: true,
    listOnEmpty: true,
    filterDelay: 50,
  })
  const dependent = props.rowOptions[field.key]
  if (dependent)
    return {
      editor: 'list',
      editorParams: ((cell: CellComponent) => params(dependent(cell.getData() as GridRow))) as never,
    }
  // Choices are read when the editor opens, so they stay current without rebuilding the grid.
  if (props.options[field.key]?.length)
    return { editor: 'list', editorParams: (() => params(props.options[field.key] || [])) as never }
  return { editor: 'input', editorParams: { selectContents: true } }
}

function textFilter(headerValue: string, rowValue: CellValue, _data: unknown, params: { field: Field }) {
  return displayValue(rowValue, params.field).toLowerCase().includes(String(headerValue).toLowerCase())
}

function columnDefs(): ColumnDefinition[] {
  const rowColumn: ColumnDefinition = {
    title: 'Fila',
    field: '__row',
    frozen: true,
    width: 62,
    hozAlign: 'right',
    headerSort: true,
    // Unsaved new rows stay on top whichever way the table is sorted.
    sorter: ((a: number | null, b: number | null, _ra: unknown, _rb: unknown, _col: unknown, dir: string) => {
      if (a === null || b === null) return a === b ? 0 : (a === null ? -1 : 1) * (dir === 'desc' ? -1 : 1)
      return a - b
    }) as never,
    editable: false,
    // Formatters return text nodes, which Tabulator accepts although its types say otherwise.
    formatter: rowNumberFormatter as never,
    cssClass: 'row-number',
    cellClick: (_e, cell) => {
      drawerId.value = (cell.getData() as GridRow).__id
    },
  }
  return [
    rowColumn,
    ...(props.columns.map(field => ({
      title: field.key,
      field: field.key,
      frozen: props.frozen.includes(field.key),
      minWidth: 70,
      maxInitialWidth: 240,
      headerTooltip: field.label,
      formatter: formatter as never,
      editable: (cell: CellComponent) => canEdit(cell.getData() as GridRow, field.key),
      ...(props.headerFilters
        ? {
            headerFilter: 'input' as const,
            headerFilterPlaceholder: 'filtrar',
            headerFilterFunc: textFilter,
            headerFilterFuncParams: { field },
          }
        : {}),
      sorter: field.type === 'number' || field.type === 'date' ? mixedSorter : 'string',
      ...editorFor(field),
    })) as ColumnDefinition[]),
  ]
}

function mixedSorter(a: CellValue, b: CellValue) {
  const na = typeof a === 'number' ? a : Number.NEGATIVE_INFINITY
  const nb = typeof b === 'number' ? b : Number.NEGATIVE_INFINITY
  return na - nb
}

let normalizing = false
function onCellEdited(cell: CellComponent) {
  if (normalizing) return
  const data = cell.getData() as GridRow
  const field = cell.getField()
  const def = fieldIndex.get(field)
  if (!def) return
  const result = normalizeInput(cell.getValue(), def)
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
  record(data, field, result.value)
  cell.getRow().reformat()
}

function record(data: GridRow, field: string, value: CellValue) {
  if (data.__new) {
    pending.updateCreate(data.__new, field, value)
    return
  }
  const row = rowIndex.get(data.__id)
  if (row) pending.setCell(props.module, row, labelOf(row.values), field, value)
}

/** Paste a block of cells starting at the selected cell, skipping read-only cells. */
function pasteRange(rowsData: Record<string, unknown>[]) {
  if (!table || !rowsData.length) return []
  const range = table.getRanges()[0]
  const selectedRows = range?.getRows() || []
  if (!selectedRows.length) return []
  const active = table.getRows('active')
  const firstId = (selectedRows[0].getData() as GridRow).__id
  const startIndex = active.findIndex(r => (r.getData() as GridRow).__id === firstId)
  if (startIndex < 0) return []
  // A single selected cell takes the whole pasted block; a larger selection is filled by repeating it.
  const count = selectedRows.length > 1 ? selectedRows.length : rowsData.length
  let skipped = 0
  const touched: RowComponent[] = []
  for (const [offset, row] of active.slice(startIndex, startIndex + count).entries()) {
    const values = rowsData[offset % rowsData.length]
    for (const [field, raw] of Object.entries(values)) {
      if (!fieldIndex.has(field)) continue
      if (!canEdit(row.getData() as GridRow, field)) {
        skipped++
        continue
      }
      row.getCell(field).setValue(raw === undefined ? null : String(raw))
    }
    touched.push(row)
  }
  if (skipped) emit('notice', `${skipped} celdas de solo lectura no se modificaron`)
  return touched
}

/** Copy the first row of the selection down to the rest (Ctrl+D). */
function fillDown() {
  if (!table) return
  const range = table.getRanges()[0]
  if (!range) return emit('notice', 'Selecciona un rango de celdas para rellenar')
  const rows = range.getRows()
  const columns = range.getColumns()
  if (rows.length < 2) return emit('notice', 'Selecciona al menos dos filas para rellenar hacia abajo')
  for (const column of columns) {
    const field = column.getField()
    if (!fieldIndex.has(field)) continue
    const source = rows[0].getCell(field).getValue()
    for (const row of rows.slice(1)) if (canEdit(row.getData() as GridRow, field)) row.getCell(field).setValue(source)
  }
}

/** Clear editable cells of the selection (Supr / Delete). */
function clearRange() {
  const range = table?.getRanges()[0]
  if (!range) return
  for (const cell of range.getCells().flat()) {
    if (fieldIndex.has(cell.getField()) && canEdit(cell.getData() as GridRow, cell.getField())) cell.setValue(null)
  }
}

/** The single selected cell, if the selection is one cell. */
function activeCell(): CellComponent | null {
  const cells = table?.getRanges()[0]?.getCells().flat() as CellComponent[] | undefined
  return cells?.length === 1 ? cells[0] : null
}

/**
 * Spreadsheet keys: typing on a selected cell replaces its content; Enter or
 * F2 edits it in place; Ctrl+D fills down; Supr clears the selection.
 */
function onKeydown(event: KeyboardEvent) {
  if (!table || (event.target as HTMLElement).closest('input, textarea, select, .tabulator-editing')) return
  const cell = activeCell()
  const editable = cell && fieldIndex.has(cell.getField()) && canEdit(cell.getData() as GridRow, cell.getField())
  if (editable && event.key.length === 1 && !event.ctrlKey && !event.metaKey && !event.altKey) {
    event.preventDefault()
    cell.edit(true)
    requestAnimationFrame(() => {
      const input = cell.getElement().querySelector('input')
      if (!input) return
      input.value = event.key
      input.dispatchEvent(new Event('input', { bubbles: true }))
      input.setSelectionRange(1, 1)
    })
  } else if (editable && (event.key === 'Enter' || event.key === 'F2')) {
    event.preventDefault()
    cell.edit(true)
  } else if ((event.ctrlKey || event.metaKey) && event.key.toLowerCase() === 'd') {
    event.preventDefault()
    fillDown()
  } else if (event.key === 'Delete') {
    event.preventDefault()
    clearRange()
  }
}

function applySearch() {
  if (!table || !built) return
  const q = (props.search || '').trim().toLowerCase()
  if (!q) return table.clearFilter(false)
  const fields = props.columns
  table.setFilter((data: GridRow) => {
    if (data.__new) return true
    return fields.some(f => displayValue(data[f.key], f).toLowerCase().includes(q))
  })
}

function build() {
  if (!host.value) return
  fieldIndex = new Map(props.columns.map(f => [f.key, f]))
  table?.destroy()
  built = false
  refreshWhenBuilt = false
  table = new Tabulator(host.value, {
    data: buildData(),
    index: '__id',
    columns: columnDefs(),
    height: props.height,
    layout: 'fitData',
    renderHorizontal: 'virtual',
    nestedFieldSeparator: false,
    placeholder: 'Sin filas',
    initialSort: props.newestFirst ? [{ column: '__row', dir: 'desc' }] : [],
    selectableRange: touchDevice ? false : 1,
    selectableRangeColumns: true,
    selectableRangeRows: true,
    selectableRangeClearCells: false,
    editTriggerEvent: touchDevice ? 'click' : 'dblclick',
    clipboard: true,
    clipboardCopyConfig: { columnHeaders: false, rowHeaders: false, formatCells: true },
    clipboardCopyRowRange: 'range',
    clipboardPasteParser: 'range',
    clipboardPasteAction: pasteRange,
    columnDefaults: { headerSortTristate: true },
    rowFormatter: (row: RowComponent) => {
      row.getElement().classList.toggle('is-highlight', props.highlight.includes((row.getData() as GridRow).__id))
    },
    // A custom paste action is supported at runtime but missing from the type definitions.
  } as unknown as ConstructorParameters<typeof Tabulator>[1])
  table.on('cellEdited', onCellEdited)
  table.on('cellClick', onCellClick)
  table.on('tableBuilt', () => {
    built = true
    if (refreshWhenBuilt) refresh()
    else applySearch()
  })
}

/** Redraw all rows from the stores, keeping scroll position and filters. */
function refresh() {
  if (!table || !host.value?.isConnected) return
  if (!built) {
    refreshWhenBuilt = true
    return
  }
  refreshWhenBuilt = false
  table.replaceData(buildData()).then(applySearch)
}

onMounted(() => {
  build()
  host.value?.addEventListener('keydown', onKeydown)
})
onBeforeUnmount(() => {
  host.value?.removeEventListener('keydown', onKeydown)
  table?.destroy()
  table = null
})

// Rebuild only when the set of columns or their editors really changes.
const layoutKey = () =>
  [
    props.module,
    props.columns.map(c => c.key).join('|'),
    Object.keys(props.options).sort().join('|'),
    props.lockedFields.join('|'),
  ].join('\n')
watch(layoutKey, build)
watch(() => [props.rows, props.creates.length, pending.revision], refresh)
watch(() => props.search, applySearch)

defineExpose({ refresh, fillDown })
</script>

<template>
  <div class="sheet-grid" :style="{ height }">
    <div ref="host" class="h-full" tabindex="-1" />
    <RowDrawer
      v-if="drawerId"
      :module="module"
      :row-id="drawerId"
      :rows="rows"
      :creates="creates"
      :columns="columns"
      :options="options"
      :locked-fields="lockedFields"
      :create-formulas="createFormulas"
      :label-field="labelField"
      @close="drawerId = null"
      @changed="refresh"
      @remove-create="
        id => {
          emit('removeCreate', id)
          drawerId = null
        }
      "
    />
  </div>
</template>
