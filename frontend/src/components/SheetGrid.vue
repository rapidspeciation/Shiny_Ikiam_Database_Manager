<script setup lang="ts">
import { computed, onActivated, onBeforeUnmount, onDeactivated, onMounted, ref, toRaw, watch } from 'vue'
import { TabulatorFull as Tabulator } from 'tabulator-tables'
import type { CellComponent, ColumnDefinition, RowComponent } from 'tabulator-tables'
import 'tabulator-tables/dist/css/tabulator_simple.min.css'
import { displayValue, editText, normalizeInput } from '../lib/cells'
import { isSumField, sumTotal } from '../lib/sums'
import {
  attachColumnFit,
  attachCopyMarker,
  attachFillHandle,
  attachTouchSheet,
  backToGrid,
  fillDown as fillDownRange,
  choiceEditor,
  openList,
  editingKeys,
  followSelection,
  longText,
  selectedCell,
  setFromBar,
  spreadsheetKeys,
  textEditor,
  tileToSelection,
  watchSize,
  type CanEdit,
  type CellBarInfo,
  type Direction,
} from '../lib/gridKit'
import { parseBlock } from '../lib/paste'
import type { CellValue, Field, TableRow } from '../lib/types'
import { type PendingCreate, usePending } from '../stores/pending'
import { useSession } from '../stores/session'
import { useTables } from '../stores/tables'
import { listProblem, repeats, verificationsFor } from '../lib/verifications'
import RowDrawer from './RowDrawer.vue'
import CellBar from './CellBar.vue'
import { History } from 'lucide-vue-next'
import type { HistoryTarget, Stored } from '../lib/history'
import { locale, t, tn } from '../lib/i18n'

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
    /** Text searched: the cells showing it are marked (the Buscador). */
    mark?: string
    /** Row id brought into view and marked, e.g. a search match or a row opened from a link. */
    focusRow?: string | null
    /** The focused row's cell selected (when no searched text marks one), e.g. the cell a past view steps through. */
    focusField?: string | null
    /** Nothing can be edited (a sheet as it was): no editors, no row drawer, only Copiar on touch screens. */
    readonly?: boolean
    /** A sheet as it was: per row id, the cells that differ from now, with their value now (marked, "Ahora: …"). */
    compare?: Record<string, Record<string, Stored>>
    /** Per row id, cells outlined: those the save looked at changed. */
    touched?: Record<string, string[]>
    /** Row ids greyed: not created yet at the moment shown. */
    absent?: string[]
    /** A "Historial" action for the selected cell (cell bar, right click, touch bar): emits `history`. */
    cellHistory?: boolean
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
    mark: '',
    focusRow: null,
    focusField: null,
    readonly: false,
    compare: () => ({}),
    touched: () => ({}),
    absent: () => [],
    cellHistory: false,
  },
)
const emit = defineEmits<{
  notice: [message: string]
  removeCreate: [clientId: string]
  select: [id: string | null]
  /** Scrolled near the first or last row shown, for views that load more rows then (the Buscador). */
  edge: [side: 'top' | 'bottom']
  /** The history of a cell (or, field null, of its whole row) was asked for. */
  history: [target: HistoryTarget]
}>()

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
  return (
    !props.readonly &&
    session.canEdit &&
    !!def &&
    !def.readonly &&
    !props.lockedFields.includes(field) &&
    // A count kept as a sum (=12+15) can be rewritten; the server refuses any other formula.
    (!isFormula(data, field) || isSumField(props.module, field))
  )
}

function indexRows() {
  formulaIndex = new Map(props.rows.map(r => [r.id, new Set(r.formulas)]))
  rowIndex = new Map(props.rows.map(r => [r.id, r]))
}

// What the grid shows now, to send it only the rows that change afterwards.
let shownRows = new Map<string, TableRow>()
let shownEdits = new Map<string, string>()
let shownCreates = ''
let shownOrder = ''
const orderKey = () => (props.newestFirst ? '' : props.rows.map(r => r.id).join('|'))

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
  shownOrder = orderKey()
  return [...created, ...props.rows.map(r => gridRow(r, keys, edits))]
}

/**
 * Sends the grid only the rows that changed since it was drawn (a save, an
 * edit made in Google Sheets, an undone change). Returns false when too much
 * changed and the whole grid should be redrawn instead.
 */
function updateChanged(): boolean {
  if (!table || createsKey() !== shownCreates) return false
  // Task screens set their own order (loaded IDs on top): a new order is drawn as a whole, as
  // updateOrAddData would append new rows at the bottom and leave moved ones in place. Those grids are small.
  if (!props.newestFirst && orderKey() !== shownOrder) return false
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
  // Every typed change counts at once, so a repeated CAM turns red as it is entered.
  void pending.edited
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
    const others = holders
      .filter(r => r !== data.__row)
      .map(r => (r === null ? t('una fila nueva') : t('fila {row}', { row: r })))
    const rows = others.slice(0, 5).join(', ')
    return {
      repeated:
        others.length > 5
          ? t('Repetido en {field}: también en {rows} y {n} más', { field, rows, n: others.length - 5 })
          : t('Repetido en {field}: también en {rows}', { field, rows }),
    }
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
  // Refused by the server, or failing the app's checks before saving (e.g. a date with year 92026).
  // Reasons are kept in Spanish (the key of their English).
  const reason = pending.errors[errorKey] || pending.problems[errorKey]
  const error = reason ? t(reason) : ''
  // Counts kept as sums are editable, so they don't look like the sheet's own formulas.
  const formula = isFormula(data, field) && !isSumField(props.module, field)
  const check = formula ? {} : checkTitle(field, cell.getValue(), data)
  el.classList.toggle('is-formula', formula)
  el.classList.toggle('is-locked', !formula && !canEdit(data, field))
  el.classList.toggle('is-dirty', !data.__new && pending.isDirty(data.__id, field))
  el.classList.toggle('is-error', !!error)
  el.classList.toggle('is-repeated', !!check.repeated)
  el.classList.toggle('is-invalid', !!check.invalid)
  el.classList.toggle('has-choices', hasChoices(field) && canEdit(data, field))
  const mark = markText()
  el.classList.toggle('is-match', !!mark && displayValue(cell.getValue(), fieldIndex.get(field)).toLowerCase().includes(mark))
  // A sheet as it was: the cells that differ from now, and those the save changed.
  const now = props.compare[data.__id]
  const then = !!now && field in now
  el.classList.toggle('is-then', then)
  el.classList.toggle('is-touched', !!props.touched[data.__id]?.includes(field))
  el.title =
    (then ? t('Ahora: {value}', { value: shownStored(now[field], field) }) : '') ||
    error ||
    check.repeated ||
    check.invalid ||
    (formula ? t('Fórmula de la hoja (solo lectura)') : '')
}

/** A value as the log keeps it, as the grid shows it ("vacío" when empty, a formula as its text). */
function shownStored(value: Stored | undefined, field: string) {
  if (value && typeof value === 'object') return t('fórmula {formula}', { formula: value.formula })
  return displayValue(value ?? null, fieldIndex.get(field)) || t('vacío')
}

const markText = () => (props.mark || '').trim().toLowerCase()

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
  const text = data.__new ? t('nueva') : String(data.__row)
  return document.createTextNode(text)
}

function editorFor(field: Field): Partial<ColumnDefinition> {
  const dependent = props.rowOptions[field.key]
  if (dependent) return choiceEditor(cell => dependent(cell.getData() as GridRow))
  // Choices are read when the editor opens, so they stay current without rebuilding the grid.
  if (props.options[field.key]?.length) return choiceEditor(() => props.options[field.key] || [])
  // Dates open day first (26/05/2026), not as the sheet's serial number.
  return textEditor(value => editText(value as CellValue, field))
}

function textFilter(headerValue: string, rowValue: CellValue, _data: unknown, params: { field: Field }) {
  return displayValue(rowValue, params.field).toLowerCase().includes(String(headerValue).toLowerCase())
}

function columnDefs(): ColumnDefinition[] {
  const rowColumn: ColumnDefinition = {
    title: t('Fila'),
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
      // A sheet as it was has no row to open: the drawer edits the row as it is now.
      if (!props.readonly) drawerId.value = (cell.getData() as GridRow).__id
    },
    ...historyMenu(),
  }
  return [
    rowColumn,
    // Frozen columns go first: Tabulator only keeps them in line with their
    // headers at the edge (a frozen CAM_ID in the middle shifted the cells after it).
    ...([
      ...props.columns.filter(f => props.frozen.includes(f.key)),
      ...props.columns.filter(f => !props.frozen.includes(f.key)),
    ].map(field => ({
      // A column missing from the sheet keeps its last values, read-only, marked in its header.
      title: field.unavailable ? `${field.key} ⚠` : field.key,
      field: field.key,
      frozen: props.frozen.includes(field.key),
      width: widthOf(field),
      minWidth: 70,
      headerTooltip: field.unavailable ? t('{field}: falta en la hoja; último valor leído', { field: field.key }) : field.label,
      formatter: formatter as never,
      editable: (cell: CellComponent) => canEdit(cell.getData() as GridRow, field.key),
      ...(props.headerFilters
        ? {
            headerFilter: 'input' as const,
            headerFilterPlaceholder: t('filtrar'),
            headerFilterFunc: textFilter,
            headerFilterFuncParams: { field },
          }
        : {}),
      sorter: field.type === 'number' || field.type === 'date' ? mixedSorter : 'string',
      ...editorFor(field),
      ...historyMenu(),
    })) as ColumnDefinition[]),
  ]
}

/** A cell's history, asked for from the bar, the touch bar or a right click; new rows have none yet. */
function askHistory(id: string | undefined, field: string | null) {
  const r = id ? rowIndex.get(id) : undefined
  if (!r) return notice(t('Una fila nueva no tiene historial todavía'))
  emit('history', { module: props.module, recordId: r.id, field: field === '__row' ? null : field, row: r.row, label: labelOf(r.values) })
}
const historyOfCell = (cell: CellComponent | null, wholeRow = false) =>
  cell && askHistory((cell.getData() as GridRow).__id, wholeRow ? null : cell.getField())

/** Right click on a cell (computers): copy, and the cell's or row's history. */
function historyMenu(): Partial<ColumnDefinition> {
  if (!props.cellHistory || touchDevice) return {}
  return {
    contextMenu: [
      { label: () => t('Copiar'), action: () => table?.copyToClipboard('range') },
      { label: () => t('Historial de esta celda'), action: (_e: unknown, cell: CellComponent) => historyOfCell(cell) },
      { label: () => t('Historial de toda la fila'), action: (_e: unknown, cell: CellComponent) => historyOfCell(cell, true) },
    ],
  } as unknown as Partial<ColumnDefinition>
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
  const result = normalizeInput(cell.getValue(), def, props.module)
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

/**
 * The copied block as rows of { field: text }, from the first selected column
 * on. A selection narrower or shorter than the block still takes all of it,
 * repeated over every selected row, as in Google Sheets (Tabulator's "range"
 * parser cut the block to the selection's width: one column of it).
 */
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
  // The parser already repeated the block over the selection (tileToSelection).
  const count = rowsData.length
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
  if (skipped)
    emit('notice', tn(skipped, '{n} celda de solo lectura no se modificó', '{n} celdas de solo lectura no se modificaron'))
  return touched
}

/** Editable in the grid's terms (Tabulator row + field), for the shared spreadsheet helpers. */
const editableCell: CanEdit = (row, field) => fieldIndex.has(field) && canEdit(row.getData() as GridRow, field)
const notice = (message: string) => emit('notice', message)
const fillDown = () => table && fillDownRange(table, editableCell, notice)
const onKeydown = spreadsheetKeys(() => table, editableCell, notice)
const onEditingKey = editingKeys(() => table)
let fill: { destroy: () => void } | null = null
let copied: ReturnType<typeof attachCopyMarker> | null = null
let fit: { destroy: () => void } | null = null

/** The selected cell as the bar above the grid shows it (CellBar). */
const bar = ref<CellBarInfo | null>(null)
function describe(cell: CellComponent | null): CellBarInfo | null {
  if (!cell) return null
  const data = cell.getData() as GridRow
  const key = cell.getField()
  const row = labelOf(data) || (data.__new ? t('nueva') : t('fila {row}', { row: data.__row ?? '' }))
  if (key === '__row')
    return {
      index: data.__id,
      field: key,
      column: t('Fila'),
      row,
      text: data.__new ? t('nueva') : String(data.__row),
      editable: false,
      multiline: false,
    }
  const def = fieldIndex.get(key)
  if (!def) return null
  const editable = canEdit(data, key)
  const total = isSumField(props.module, key) ? sumTotal(data[key]) : null
  const now = props.compare[data.__id]
  const notes: CellBarInfo['notes'] = []
  if (total !== null) notes.push({ text: `= ${total}`, kind: 'total' })
  if (now && key in now) notes.push({ label: t('Ahora'), text: shownStored(now[key], key), kind: 'edited' })
  else if (props.absent.includes(data.__id)) notes.push({ text: t('Esta fila todavía no existía'), kind: 'hint' })
  return {
    index: data.__id,
    field: key,
    column: key,
    row,
    text: editText(data[key], def),
    editable,
    multiline: longText(key) && !hasChoices(key),
    readonly: editable
      ? ''
      : props.readonly
        ? t('Cómo estaba la hoja (solo lectura)')
        : isFormula(data, key)
          ? t('Fórmula de la hoja (solo lectura)')
          : t('Solo lectura'),
    notes,
  }
}
const showBar = () => (bar.value = table ? describe(selectedCell(table)) : null)
function saveFromBar(target: CellBarInfo, text: string, move: Direction | 'here' | null) {
  if (!table) return
  if (!setFromBar(table, target, text, editableCell)) notice(t('Esa celda ya no se puede editar'))
  if (move) backToGrid(table, move)
  showBar()
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
    // The grid fills the box under the cell bar; `height` sizes the whole component (bar included).
    height: '100%',
    layout: 'fitData',
    // Size changes are handled by watchSize, which ignores the tab being hidden and waits for open editors.
    autoResize: false,
    renderHorizontal: 'virtual',
    nestedFieldSeparator: false,
    placeholder: t('Sin filas'),
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
    clipboardPasteParser: pasteParser,
    clipboardPasteAction: pasteRange,
    // Widths change from the header's borders only (see gridKit): a finger on the rows scrolls.
    columnDefaults: { headerSortTristate: true, resizable: 'header' },
    rowFormatter: (row: RowComponent) => {
      const id = (row.getData() as GridRow).__id
      row.getElement().classList.toggle('is-highlight', props.highlight.includes(id))
      row.getElement().classList.toggle('is-focus', id === props.focusRow)
      row.getElement().classList.toggle('is-absent', props.absent.includes(id))
    },
    // A custom paste action is supported at runtime but missing from the type definitions.
  } as unknown as ConstructorParameters<typeof Tabulator>[1])
  table.on('cellEdited', onCellEdited)
  table.on('cellClick', onCellClick)
  followSelection(table, showBar)
  bar.value = null
  // The row of the selection, for panels beside the grid (e.g. a capture's photos in Monitoreo).
  const selected = () => emit('select', (table?.getRanges()[0]?.getRows()[0]?.getData() as GridRow | undefined)?.__id ?? null)
  table.on('rangeAdded', selected)
  table.on('rangeChanged', selected)
  fill?.destroy()
  const container = host.value.parentElement
  // Computers: the fill handle. Touch screens: tap to select, a handle to stretch the selection, and an action bar.
  fill = !container
    ? null
    : touchDevice
      ? attachTouchSheet(table, container, {
          canEdit: editableCell,
          notice,
          readonly: props.readonly,
          extra: props.cellHistory ? [{ label: 'Historial', action: cell => historyOfCell(cell) }] : [],
        })
      : attachFillHandle(table, container, {
          canEdit: editableCell,
          onFilled: rows => notice(tn(rows, 'Copiado a {n} fila', 'Copiado a {n} filas')),
        })
  copied?.destroy()
  copied = host.value.parentElement ? attachCopyMarker(table, host.value.parentElement, notice) : null
  // Double-clicking a column's right border fits it to the text shown (the rows on screen and a sample).
  fit?.destroy()
  fit = attachColumnFit(table, host.value, {
    text: (data, key) =>
      key === '__row' ? String(data.__row ?? t('nueva')) : displayValue(data[key] as CellValue, fieldIndex.get(key)),
  })
  // A refresh that arrived while typing (e.g. an automatic save finished) runs after the edit.
  const afterEdit = () => {
    if (!refreshAfterEdit) return
    refreshAfterEdit = false
    setTimeout(refresh)
  }
  table.on('cellEdited', afterEdit)
  table.on('cellEditCancelled', afterEdit)
  // A value outside a list the sheet does not enforce is saved (sometimes something else must be
  // written), marked with the red corner and a warning so a typo is noticed.
  table.on('cellEdited', (cell: CellComponent) => {
    const field = cell.getField()
    const list = rules.value?.lists[field]
    const problem = list && !list.strict ? listProblem(rules.value, field, cell.getValue()) : null
    if (problem) notice(t('{problem}: se guarda igual; corrígelo si es un error', { problem }))
  })
  table.on('tableBuilt', () => {
    built = true
    focusPending = !!props.focusRow
    if (refreshWhenBuilt) refresh()
    else {
      applySearch()
      showFocus()
    }
  })
  table.on('scrollVertical', (top: number) => {
    const box = holder()
    if (!box) return
    const near = 4 * 28
    if (top < near) emit('edge', 'top')
    else if (box.scrollHeight - top - box.clientHeight < near) emit('edge', 'bottom')
  })
}

const holder = () => host.value?.querySelector<HTMLElement>('.tabulator-tableholder') ?? null

/**
 * Rows about to be added (the Buscador loading while scrolling): the selected
 * cell stays selected and, for rows added above the ones on screen, the row at
 * the top stays where it is, instead of the view jumping by the rows added.
 */
let anchor: { id: string; offset: number } | null = null
let keepSelected = false
function keepView(above: boolean) {
  keepSelected = true
  if (!above) return
  const box = holder()
  const row = table?.getRows('visible')[0]
  if (!box || !row) return
  anchor = { id: (row.getData() as GridRow).__id, offset: row.getElement().getBoundingClientRect().top - box.getBoundingClientRect().top }
}
function restoreView() {
  const kept = anchor
  anchor = null
  const box = holder()
  if (!kept || !box || !table?.getRow(kept.id)) return
  // At the top first, then moved by where it was (a row half out of view stays half out).
  table
    .scrollToRow(kept.id, 'top', true)
    .then(() => {
      const row = table?.getRow(kept.id)
      if (row) box.scrollTop += row.getElement().getBoundingClientRect().top - box.getBoundingClientRect().top - kept.offset
    })
    .catch(() => {})
}

/** Selects one cell (as a click would, without taking the keyboard's focus); not on touch screens' grids that refuse it. */
function selectCell(cell: CellComponent) {
  try {
    ;(table as unknown as { addRange: (a: CellComponent, b: CellComponent) => void }).addRange(cell, cell)
  } catch {
    /* no range selection */
  }
}

/**
 * The focused row brought into view (centred) once it is in the grid, its cell
 * showing the searched text selected, so the bar above shows that cell.
 */
let focusPending = false
function showFocus() {
  const id = props.focusRow
  const row = id && table && built ? table.getRow(id) : null
  if (!focusPending || !table || !row) return
  focusPending = false
  table.scrollToRow(row, 'center', false).catch(() => {})
  const mark = markText()
  const data = row.getData() as GridRow
  const found =
    (mark ? props.columns.find(f => displayValue(data[f.key], f).toLowerCase().includes(mark))?.key : undefined) ??
    (props.focusField && fieldIndex.has(props.focusField) ? props.focusField : undefined)
  const key = found || props.frozen[0] || props.columns[0]?.key
  if (!key) return
  selectCell(row.getCell(key))
  // A match in a column off to the right (a tube, a note) is brought into view too.
  if (found && !props.frozen.includes(found)) table.scrollToColumn(found, 'middle', false).catch(() => {})
}

/** The selected cell, put back after the rows are drawn again (Tabulator goes back to the first cell). */
function keptSelection() {
  const cell = table && built ? selectedCell(table) : null
  const id = cell ? (cell.getData() as GridRow).__id : null
  const key = cell?.getField()
  return () => {
    const row = id && key && table ? table.getRow(id) : null
    if (row && fieldIndex.has(key!)) selectCell(row.getCell(key!))
  }
}
watch(
  () => props.focusRow,
  (now, before) => {
    for (const id of [before, now]) {
      // Tabulator answers false for a row it does not have.
      const row = id && table ? table.getRow(id) : null
      if (row) row.getElement().classList.toggle('is-focus', id === now)
    }
    focusPending = !!now
    showFocus()
  },
)

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
  const reselect = keepSelected ? keptSelection() : null
  keepSelected = false
  if (!updateChanged())
    table.replaceData(buildData()).then(() => {
      applySearch()
      showHighlight()
      restoreView()
      reselect?.()
      showFocus()
    })
  else {
    // Nothing was added above (or the rows were only updated): the view stays as it is.
    anchor = null
    showHighlight()
    showFocus()
  }
}

/** Newly highlighted rows (IDs just loaded) are scrolled into view once drawn. */
let highlightPending = false
function showHighlight() {
  const first = props.highlight[0]
  if (!highlightPending || !table || !first) return
  highlightPending = false
  if (table.getRow(first)) table.scrollToRow(first, 'top', false).catch(() => {})
}

onMounted(() => {
  build()
  host.value?.addEventListener('keydown', onKeydown)
  host.value?.addEventListener('keydown', onEditingKey, true)
  // Its height is fixed (the rows area follows by CSS): the cell bar growing a line or two needs no redraw.
  if (host.value) sizeWatch = watchSize(() => table, host.value, { followsHeight: true })
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
  fit?.destroy()
  host.value?.removeEventListener('keydown', onKeydown)
  host.value?.removeEventListener('keydown', onEditingKey, true)
  table?.destroy()
  table = null
})

// Rebuild only when the set of columns or their editors really changes (or the interface language).
const layoutKey = () =>
  [
    locale.value,
    props.module,
    props.columns.map(c => c.key).join('|'),
    Object.keys(props.options).sort().join('|'),
    props.lockedFields.join('|'),
  ].join('\n')
watch(layoutKey, build)
// Before the rows' watcher, so the refresh it starts knows to scroll.
watch(
  () => props.highlight.join('|'),
  (now, before) => (highlightPending = !!now && now !== before),
)
watch(() => [props.rows, props.creates.length, pending.revision], refresh)
watch(() => props.search, applySearch)
// Repaint what is on screen when the sheet's rules arrive, a repeat appears or goes, or a check changes.
watch([rules, repeated, () => JSON.stringify(pending.problems), () => JSON.stringify(pending.errors)], () => {
  // Only the rows on screen are drawn; repainting them is enough (a full redraw re-measures the layout).
  if (table && built) for (const row of table.getRows('visible')) decorateRow(row)
})

// Cells showing the searched text are marked again when it changes.
watch(markText, () => {
  if (table && built) for (const row of table.getRows('visible')) decorateRow(row)
})

defineExpose({ refresh, fillDown, keepView })
</script>

<template>
  <div class="sheet-grid flex flex-col" :style="{ height }">
    <CellBar :info="bar" @save="saveFromBar" @back="move => table && backToGrid(table, move)">
      <template v-if="cellHistory" #actions>
        <button
          type="button"
          class="cell-bar-action"
          :disabled="!bar || !rowIndex.has(bar.index)"
          :title="$t('Historial de esta celda: cada cambio, quién y cuándo')"
          @mousedown.prevent
          @click="bar && askHistory(bar.index, bar.field)"
        >
          <History :size="14" /> <span class="max-sm:hidden">{{ $t('Historial') }}</span>
        </button>
      </template>
    </CellBar>
    <!-- The grid's own box: the fill handle and the copied cells' border are placed in it, so they move with the grid. -->
    <div class="relative min-h-0 flex-1">
      <div ref="host" class="h-full" tabindex="-1" />
    </div>
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
