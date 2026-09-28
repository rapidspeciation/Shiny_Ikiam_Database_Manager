<script setup lang="ts">
import { computed, onActivated, onBeforeUnmount, onDeactivated, onMounted, ref, toRaw, watch } from 'vue'
import { TabulatorFull as Tabulator } from 'tabulator-tables'
import type { CellComponent, ColumnDefinition, RowComponent } from 'tabulator-tables'
import 'tabulator-tables/dist/css/tabulator_simple.min.css'
import { displayValue, normalizeInput } from '../lib/cells'
import {
  attachCopyMarker,
  attachFillHandle,
  attachTouchSheet,
  fillDown as fillDownRange,
  choiceEditor,
  openList,
  spreadsheetKeys,
  watchSize,
  type CanEdit,
} from '../lib/gridKit'
import type { CellValue, Field, TableRow } from '../lib/types'
import { type PendingCreate, usePending } from '../stores/pending'
import { useSession } from '../stores/session'
import { useTables } from '../stores/tables'
import { listProblem, repeats, verificationsFor } from '../lib/verifications'
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
let refreshAfterEdit = false
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

// What the grid shows now, to send it only the rows that change afterwards.
let shownRows = new Map<string, TableRow>()
let shownEdits = new Map<string, string>()
let shownCreates = ''

const editKey = (id: string, edits: Record<string, { values: Record<string, CellValue> }>) =>
  edits[id] ? JSON.stringify(edits[id].values) : ''
const createsKey = () => JSON.stringify(props.creates.map(c => [c.clientId, c.values]))

function gridRow(r: TableRow, keys: string[], edits: Record<string, { values: Record<string, CellValue> }>): GridRow {
  const out = { __id: r.id, __row: r.row, __new: null } as GridRow
  const values = r.values
  for (const key of keys) out[key] = values[key] ?? null
  const edit = edits[r.id]
  if (edit) for (const key of keys) if (key in edit.values) out[key] = edit.values[key]
  return out
}

function buildData(): GridRow[] {
  indexRows()
  const created = props.creates.map(c => ({ ...c.values, __id: c.clientId, __row: null, __new: c.clientId }) as GridRow)
  // Read the few unsaved edits once, outside Vue's reactivity: asking the store
  // for every cell (13k rows × 50 columns) froze the page for seconds.
  const edits = toRaw(pending.edits)
  const keys = props.columns.map(f => f.key)
  shownRows = new Map(props.rows.map(r => [r.id, r]))
  shownEdits = new Map(Object.keys(edits).map(id => [id, editKey(id, edits)]))
  shownCreates = createsKey()
  return [...created, ...props.rows.map(r => gridRow(r, keys, edits))]
}

/**
 * Sends the grid only the rows that changed since it was drawn (a save, an
 * edit made in Google Sheets, an undone change). Returns false when too much
 * changed and the whole grid should be redrawn instead.
 */
function updateChanged(): boolean {
  if (!table || createsKey() !== shownCreates) return false
  const edits = toRaw(pending.edits)
  const keys = props.columns.map(f => f.key)
  const changed: TableRow[] = []
  const seen = new Set<string>()
  for (const r of props.rows) {
    seen.add(r.id)
    if (shownRows.get(r.id) !== r || (shownEdits.get(r.id) ?? '') !== editKey(r.id, edits)) changed.push(r)
  }
  const removed = [...shownRows.keys()].filter(id => !seen.has(id))
  if (changed.length + removed.length > 300) return false
  if (!changed.length && !removed.length) return true
  for (const r of changed) {
    formulaIndex.set(r.id, new Set(r.formulas))
    rowIndex.set(r.id, r)
    shownRows.set(r.id, r)
    const key = editKey(r.id, edits)
    if (key) shownEdits.set(r.id, key)
    else shownEdits.delete(r.id)
  }
  for (const id of removed) {
    shownRows.delete(id)
    shownEdits.delete(id)
    rowIndex.delete(id)
    table.deleteRow(id)
  }
  if (changed.length)
    table.updateOrAddData(changed.map(r => gridRow(r, keys, edits))).then(() => {
      // Tabulator repaints only cells whose value changed; the markers (unsaved,
      // error, formula) can change without the value, e.g. once a save lands.
      for (const r of changed) {
        const row = table?.getRow(r.id)
        if (row) decorateRow(row)
      }
      applySearch()
    })
  return true
}

/**
 * Column widths from the header and a sample of rows (the newest and the
 * oldest), so Tabulator does not measure every rendered cell ("fitData" did,
 * for seconds on the big sheets).
 */
function widthOf(field: Field) {
  const rows = props.rows
  const sample = rows.length > 400 ? [...rows.slice(0, 200), ...rows.slice(-200)] : rows
  let chars = field.key.length + 2
  for (const r of sample) {
    const text = displayValue(r.values[field.key] ?? null, field)
    if (text.length > chars) chars = text.length
    if (chars >= 32) break
  }
  return Math.max(70, Math.min(240, Math.round(chars * 7.2 + 28)))
}

/** Columns edited with a dropdown list; their editable cells show a ▾ arrow. */
function hasChoices(field: string) {
  return !!props.rowOptions[field] || !!props.options[field]?.length
}

/** Clicking the ▾ arrow opens the list straight away, without a double click. */
function onCellClick(event: UIEvent, cell: CellComponent) {
  const el = cell.getElement()
  if (!el.classList.contains('has-choices') || !(event instanceof MouseEvent)) return
  if (event.clientX >= el.getBoundingClientRect().right - 22) openList(cell)
}

/**
 * The Google Sheet's checks, shown as in the sheet: repeated IDs in pale red
 * (over the whole sheet, unsaved changes included), values outside the
 * column's list with a red corner. Formula cells are not checked.
 */
const tables = useTables()
const rules = computed(() => verificationsFor(props.module))
const repeated = computed(() => {
  void tables.versions[props.module]
  void pending.revision
  const edits = toRaw(pending.edits)
  const fields = rules.value?.unique || []
  const rows = (tables.tables[props.module]?.rows || props.rows)
    .filter(r => r.observed)
    .map(r => ({
      row: r.row,
      values: Object.fromEntries(fields.map(f => [f, f in (edits[r.id]?.values || {}) ? edits[r.id].values[f] : r.values[f]])),
    }))
  const created = props.creates.map(c => ({ row: null, values: c.values }))
  return repeats(rules.value, [...rows, ...created])
})
function checkTitle(field: string, value: CellValue, data: GridRow) {
  const holders = repeated.value.get(field)?.get(String(value ?? '').trim())
  if (holders) {
    const others = holders.filter(r => r !== data.__row).map(r => (r === null ? 'una fila nueva' : `fila ${r}`))
    const more = others.length > 5 ? ` y ${others.length - 5} más` : ''
    return { repeated: `Repetido en ${field}: también en ${others.slice(0, 5).join(', ')}${more}` }
  }
  const problem = listProblem(rules.value, field, value)
  return problem ? { invalid: problem } : {}
}

/** The cell's markers (formula, unsaved, error, repeated, outside the list…) and its hover text. */
function decorate(cell: CellComponent) {
  const data = cell.getData() as GridRow
  const field = cell.getField()
  const el = cell.getElement()
  const errorKey = `${data.__id}:${field}`
  const formula = isFormula(data, field)
  const check = formula ? {} : checkTitle(field, cell.getValue(), data)
  el.classList.toggle('is-formula', formula)
  el.classList.toggle('is-locked', !formula && !canEdit(data, field))
  el.classList.toggle('is-dirty', !data.__new && pending.isDirty(data.__id, field))
  el.classList.toggle('is-error', !!pending.errors[errorKey])
  el.classList.toggle('is-repeated', !!check.repeated)
  el.classList.toggle('is-invalid', !!check.invalid)
  el.classList.toggle('has-choices', hasChoices(field) && canEdit(data, field))
  el.title =
    pending.errors[errorKey] || check.repeated || check.invalid || (formula ? 'Fórmula de la hoja (solo lectura)' : '')
}

function formatter(cell: CellComponent) {
  decorate(cell)
  return document.createTextNode(displayValue(cell.getValue(), fieldIndex.get(field(cell))))
}
const field = (cell: CellComponent) => cell.getField()

/**
 * Updates the markers of a row's cells in place. (Tabulator's row.reformat()
 * redraws the row and, with columns drawn only when in view, put a cell at the
 * end of the row, shifting the others under the wrong headers.)
 */
function decorateRow(row: RowComponent) {
  for (const cell of row.getCells()) if (fieldIndex.has(cell.getField())) decorate(cell)
}

function rowNumberFormatter(cell: CellComponent) {
  const data = cell.getData() as GridRow
  const el = cell.getElement()
  el.classList.toggle('is-new', !!data.__new)
  const text = data.__new ? 'nueva' : String(data.__row)
  return document.createTextNode(text)
}

function editorFor(field: Field): Partial<ColumnDefinition> {
  const dependent = props.rowOptions[field.key]
  if (dependent) return choiceEditor(cell => dependent(cell.getData() as GridRow))
  // Choices are read when the editor opens, so they stay current without rebuilding the grid.
  if (props.options[field.key]?.length) return choiceEditor(() => props.options[field.key] || [])
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
    // Frozen columns go first: Tabulator only keeps them in line with their
    // headers at the edge (a frozen CAM_ID in the middle shifted the cells after it).
    ...([...props.columns.filter(f => props.frozen.includes(f.key)), ...props.columns.filter(f => !props.frozen.includes(f.key))].map(field => ({
      title: field.key,
      field: field.key,
      frozen: props.frozen.includes(field.key),
      width: widthOf(field),
      minWidth: 70,
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
  decorateRow(cell.getRow())
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

/** Editable in the grid's terms (Tabulator row + field), for the shared spreadsheet helpers. */
const editableCell: CanEdit = (row, field) => fieldIndex.has(field) && canEdit(row.getData() as GridRow, field)
const notice = (message: string) => emit('notice', message)
const fillDown = () => table && fillDownRange(table, editableCell, notice)
const onKeydown = spreadsheetKeys(() => table, editableCell, notice)
let fill: { destroy: () => void } | null = null
let copied: ReturnType<typeof attachCopyMarker> | null = null

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
    // Size changes are handled by watchSize, which ignores the tab being hidden and waits for open editors.
    autoResize: false,
    renderHorizontal: 'virtual',
    nestedFieldSeparator: false,
    placeholder: 'Sin filas',
    initialSort: props.newestFirst ? [{ column: '__row', dir: 'desc' }] : [],
    // Also on touch screens: a tap selects (a scrolling finger does not), see attachTouchSheet.
    selectableRange: 1,
    selectableRangeColumns: true,
    selectableRangeRows: true,
    selectableRangeClearCells: false,
    editTriggerEvent: 'dblclick',
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
  fill?.destroy()
  const container = host.value.parentElement
  // Computers: the fill handle. Touch screens: tap to select, a handle to stretch the selection, and an action bar.
  fill = !container
    ? null
    : touchDevice
      ? attachTouchSheet(table, container, { canEdit: editableCell, notice })
      : attachFillHandle(table, container, {
          canEdit: editableCell,
          onFilled: rows => notice(`Copiado a ${rows} ${rows === 1 ? 'fila' : 'filas'}`),
        })
  copied?.destroy()
  copied = host.value.parentElement ? attachCopyMarker(table, host.value.parentElement, notice) : null
  // A refresh that arrived while typing (e.g. an automatic save finished) runs after the edit.
  const afterEdit = () => {
    if (!refreshAfterEdit) return
    refreshAfterEdit = false
    setTimeout(refresh)
  }
  table.on('cellEdited', afterEdit)
  table.on('cellEditCancelled', afterEdit)
  table.on('tableBuilt', () => {
    built = true
    if (refreshWhenBuilt) refresh()
    else applySearch()
  })
}

/** Redraw all rows from the stores, keeping scroll position and filters. */
let active = true
let refreshWhenActive = false
function refresh() {
  if (!table || !host.value?.isConnected) return
  // A grid in a tab that is not showing catches up when it is shown again.
  if (!active) {
    refreshWhenActive = true
    return
  }
  if (!built) {
    refreshWhenBuilt = true
    return
  }
  refreshWhenBuilt = false
  // Never redraw under an open cell editor: it would throw away what is being typed.
  if (host.value.querySelector('.tabulator-editing')) {
    refreshAfterEdit = true
    return
  }
  if (!updateChanged()) table.replaceData(buildData()).then(applySearch)
}

onMounted(() => {
  build()
  host.value?.addEventListener('keydown', onKeydown)
  if (host.value) sizeWatch = watchSize(() => table, host.value)
})
// Kept alive while another tab is open (see App.vue): its size may have changed meanwhile.
onActivated(() => {
  active = true
  if (refreshWhenActive) {
    refreshWhenActive = false
    refresh()
  }
})
onDeactivated(() => (active = false))

// Size changes are followed by watchSize (lib/gridKit.ts), which ignores the tab being hidden and waits for open editors.
let sizeWatch: { disconnect: () => void } | null = null

onBeforeUnmount(() => {
  sizeWatch?.disconnect()
  fill?.destroy()
  copied?.destroy()
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
// Repaint what is on screen when the sheet's rules arrive or a repeat appears or goes.
watch([rules, repeated], () => {
  // Only the rows on screen are drawn; repainting them is enough (a full redraw re-measures the layout).
  if (table && built) for (const row of table.getRows('visible')) decorateRow(row)
})

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
