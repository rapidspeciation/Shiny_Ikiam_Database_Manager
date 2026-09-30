<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { TabulatorFull as Tabulator } from 'tabulator-tables'
import type { CellComponent, ColumnDefinition, RowComponent } from 'tabulator-tables'
import 'tabulator-tables/dist/css/tabulator_simple.min.css'
import { FileSpreadsheet, Sparkles } from 'lucide-vue-next'
import { displayValue, editText, normalizeInput } from '../../lib/cells'
import {
  attachColumnFit,
  attachCopyMarker,
  attachFillHandle,
  attachTouchSheet,
  backToGrid,
  choiceEditor,
  editingKeys,
  followSelection,
  longText,
  openList,
  selectedCell,
  setFromBar,
  spreadsheetKeys,
  textEditor,
  tileToSelection,
  typingPending,
  watchSize,
  type CanEdit,
  type CellBarInfo,
  type CellBarNote,
  type Direction,
} from '../../lib/gridKit'
import { parseBlock } from '../../lib/paste'
import { ID_COLUMN, cellId, cellOf, rowKey, selectionActions, type ProposalChange } from '../../lib/proposals'
import { isSumField, sumTotal } from '../../lib/sums'
import type { CellValue, Field } from '../../lib/types'
import { listProblem, verificationsFor } from '../../lib/verifications'
import { locale, t, tn } from '../../lib/i18n'
import CellBar from '../CellBar.vue'

/**
 * The rows of one sheet of a proposal as a spreadsheet, like the Colecta list:
 * Enter/Tab move, typing replaces, the fill handle and Ctrl+D copy down, blocks
 * paste from Excel or Sheets, list columns open the sheet's list, dates are
 * typed day first. The assistant's values are green, the person's blue, an
 * existing row's other cells grey; cells the assistant just changed flash;
 * counts written as sums show their total apart (=12+15 then "= 27"). The
 * selected cells can go back to the sheet's value (the assistant's is kept
 * aside, dashed, and not written) or take the assistant's value again. The ID
 * stays at the left and the column names at the top while scrolling (see
 * columns() and the table's maxHeight). Edits go out through `edit` (the
 * parent saves them to the proposal).
 */
export interface CellEdit {
  key: string
  field: string
  value: CellValue
  /** What the cell showed when the person started typing. */
  before: CellValue
  /** One of the buttons: back to the sheet's value, or the assistant's again (`value` then holds it). */
  use?: 'sheet' | 'ai'
}
const props = defineProps<{
  sheet: string
  changes: ProposalChange[]
  fields: string[]
  types: Record<string, string>
  /** Formula columns of the pre-made rows new rows go into. */
  newRowFormulas: string[]
  editable: boolean
  /** An applied proposal: the rows written (a ✓ beside them); null otherwise. */
  applied: number[] | null
  /** Cells to flash (the assistant just changed them). */
  flash: Set<string>
}>()
const emit = defineEmits<{
  edit: [cells: CellEdit[]]
  remove: [key: string]
  notice: [message: string]
}>()

type Row = Record<string, CellValue> & {
  __key: string
  __done: string
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
// On a phone only the ID stays in place (as the first column): row and ID together took half the screen.
const wide = typeof window !== 'undefined' && window.matchMedia('(min-width: 768px)').matches
const rules = computed(() => verificationsFor(props.sheet))
const fieldSet = computed(() => new Set(props.fields))
const typeOf = (field: string) => (props.types[field] ?? 'text') as Field['type']
const show = (field: string, value: CellValue | undefined) => displayValue(value, { key: field, type: typeOf(field) })
/** What a count written as a sum (=12+15) adds up to, shown after it; null for other cells. */
const totalOf = (field: string, value: CellValue | undefined) => (isSumField(props.sheet, field) ? sumTotal(value) : null)
/** The cell's text with a sum's total apart after it: "=12+15" then "= 27" in its own colour. */
function withTotal(field: string, value: CellValue | undefined, text: string): Node {
  const total = totalOf(field, value)
  if (total === null) return document.createTextNode(text)
  const box = document.createElement('span')
  const sum = document.createElement('span')
  sum.className = 'sum-total'
  sum.textContent = `= ${total}`
  box.append(text, sum)
  return box
}
/** The same as plain text, to size the column (the total's box counts as a few letters more). */
const textWithTotal = (field: string, value: CellValue | undefined) => {
  const total = totalOf(field, value)
  return show(field, value) + (total === null ? '' : `  = ${total}  `)
}

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
  const out = {
    __key: key,
    __done: props.applied?.includes(c.index) ? '✓' : '',
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
  out.__state = state + JSON.stringify(c.personEdits ?? null) + (props.editable ? 'e' : '') + Object.keys(c.values).length
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
    el.classList.toggle('is-reverted', c.kind === 'reverted')
    el.classList.toggle('is-sheet', c.kind === 'sheet')
    el.classList.toggle('is-formula', c.kind === 'locked')
    el.classList.toggle('is-invalid', !!problem)
    el.classList.toggle('is-flash', props.flash.has(cellId(row.__key, field)))
    el.classList.toggle('has-choices', canEditCell(row.__key, field) && hasChoices(field))
    const was = c.was === undefined ? '' : show(field, c.was) || t('vacío')
    const ai = show(field, c.ai) || t('vacío')
    const before = change.replaceFormula?.includes(field) ? 'Antes: {value} (fórmula)' : 'Antes: {value}'
    el.title = [
      problem,
      c.kind === 'proposed' && !change.create ? t(before, { value: was }) : '',
      c.kind === 'person'
        ? [
            t('Editado por ti'),
            c.aiProposed ? t('la IA proponía: {value}', { value: ai }) : '',
            change.create ? '' : t('en la hoja: {value}', { value: was }),
          ]
            .filter(Boolean)
            .join(' · ')
        : '',
      c.kind === 'reverted'
        ? [
            change.create ? t('Vacía: no se escribe') : t('Valor de la hoja: no cambia'),
            t('la IA proponía: {value}', { value: ai }),
          ].join(' · ')
        : '',
      c.kind === 'locked' ? t('Fórmula de la hoja: no se escribe') : '',
      c.kind === 'sheet' && props.editable ? t('Valor actual de la hoja; escribe para cambiarlo') : '',
    ]
      .filter(Boolean)
      .join('\n')
    const text = show(field, c.value)
    // Set back to the sheet: its value, then the assistant's struck through (kept aside, not written).
    if (c.kind === 'reverted') {
      const box = document.createElement('span')
      if (text) box.append(withTotal(field, c.value, text), ' ')
      const aside = document.createElement('s')
      aside.className = 'aside'
      aside.append(withTotal(field, c.ai, ai))
      box.append(aside)
      return box
    }
    if (!changed || change.create || c.was === undefined || show(field, c.was) === text) return withTotal(field, c.value, text)
    const box = document.createElement('span')
    // Emptied on purpose ({ clear: true } or the person deleted it): red, not a quiet "vacío".
    if (text) box.append(withTotal(field, c.value, text))
    else box.textContent = t('vaciar')
    box.classList.toggle('is-clear', !text)
    const old = document.createElement('s')
    old.className = 'was'
    old.append(withTotal(field, c.was, show(field, c.was) || t('vacío')))
    box.append(' ', old)
    return box
  }
}

/** The row number; a row with nothing left to write (every cell back to the sheet's value) is struck through. */
function rowFormatter(cell: CellComponent) {
  const row = cell.getData() as Row
  const change = byKey.get(row.__key)
  const skipped = props.editable && !!change && !Object.keys(change.values).length
  const el = cell.getElement()
  el.classList.toggle('is-skipped', skipped)
  el.title = skipped ? t('Esta fila no se escribe: no le queda ningún cambio') : ''
  return row.__row
}

/** A cell as drawn, as text: its value, then the sheet's value struck through or the assistant's set aside. */
function drawnText(change: ProposalChange, field: string) {
  const cell = cellOf(change, field, props.newRowFormulas)
  const beside =
    cell.kind === 'reverted'
      ? ` ${textWithTotal(field, cell.ai)}`
      : cell.was !== undefined && cell.kind !== 'sheet'
        ? ` ${textWithTotal(field, cell.was)}`
        : ''
  return textWithTotal(field, cell.value) + beside
}

function widthOf(field: string) {
  let chars = field.length + 2
  for (const c of props.changes) chars = Math.max(chars, drawnText(c, field).length)
  return Math.max(70, Math.min(260, Math.round(chars * 7.2 + 28)))
}

function columns(): ColumnDefinition[] {
  const cols: ColumnDefinition[] = []
  // The row's identity stays at the left while scrolling right, with its row number on a computer.
  // Frozen columns go first (Tabulator keeps them in line only at the edge): on a phone, the ID alone.
  const id = {
    title: 'ID',
    field: '__label',
    frozen: true,
    headerSort: false,
    cssClass: 'proposal-label',
    // The row's note (where its values come from) also on the ID, as the Nota column is at the far right.
    tooltip: (_e: MouseEvent, cell: CellComponent) => (cell.getData() as Row).__note,
  } as ColumnDefinition
  if (!wide) cols.push(id)
  if (props.applied)
    cols.push({
      title: '✓',
      field: '__done',
      width: 34,
      frozen: wide,
      hozAlign: 'center',
      headerHozAlign: 'center',
      headerTooltip: t('Filas escritas en la hoja'),
    })
  cols.push({
    title: t('Fila'),
    field: '__row',
    width: 54,
    frozen: wide,
    hozAlign: 'right',
    cssClass: 'row-number',
    headerSort: false,
    formatter: rowFormatter as never,
  })
  if (wide) cols.push(id)
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
        : // Dates open day first (26/05/2026), not as the sheet's serial number.
          textEditor(value => editText(value as CellValue, { key: field, type: typeOf(field) }))),
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

// ------------------------------------------------------------ the cell bar
/** The selected cell as the bar above the table shows it, with the sheet's and the assistant's values under it. */
const bar = ref<CellBarInfo | null>(null)
const editText$ = (field: string, value: CellValue | undefined) => editText(value ?? null, { key: field, type: typeOf(field) })
function describe(cell: CellComponent | null): CellBarInfo | null {
  if (!cell) return null
  const row = cell.getData() as Row
  const field = cell.getField()
  const base = { index: row.__key, field, row: row.__label, editable: false, multiline: false }
  // The row's own columns: its note (where the values come from), its ID and row number.
  if (field === '__note') return { ...base, column: t('Nota'), text: row.__note }
  if (field === '__label') return { ...base, column: 'ID', text: row.__label }
  if (field === '__row') return { ...base, column: t('Fila'), text: row.__row }
  const c = fieldSet.value.has(field) ? info(row.__key, field) : null
  const change = byKey.get(row.__key)
  if (!c || !change) return null
  const notes: CellBarNote[] = []
  const total = totalOf(field, c.value)
  if (total !== null) notes.push({ text: `= ${total}`, kind: 'total' })
  // What the sheet has now (an existing row), and what the assistant proposed when the person changed it.
  if (!change.create && (c.kind === 'proposed' || c.kind === 'person'))
    notes.push({ label: t('Hoja'), text: editText$(field, c.was) || t('vacío'), kind: 'sheet' })
  if (c.aiProposed && (c.kind === 'person' || c.kind === 'reverted'))
    notes.push({ label: t('IA'), text: editText$(field, c.ai) || t('vacío'), kind: 'ai' })
  const editable = canEditCell(row.__key, field)
  return {
    ...base,
    column: field,
    text: editText$(field, c.value),
    editable,
    multiline: longText(field) && !hasChoices(field),
    readonly: editable ? '' : c.kind === 'locked' ? t('Fórmula de la hoja: no se escribe') : '',
    notes,
  }
}
const showBar = () => (bar.value = table ? describe(selectedCell(table)) : null)
function saveFromBar(target: CellBarInfo, text: string, move: Direction | 'here' | null) {
  if (!table) return
  if (!setFromBar(table, target, text, canEdit)) emit('notice', t('Esa celda ya no se puede editar'))
  if (move) backToGrid(table, move)
  showBar()
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

// ------------------------------------------------------------ the sheet's value or the assistant's, for the selection
/** What the buttons can do with the selected cells (counts, for their labels). */
const actions = ref({ sheet: 0, ai: 0 })
function selected() {
  const out: { key: string; field: string; cell: NonNullable<ReturnType<typeof info>> }[] = []
  for (const range of table?.getRanges() ?? [])
    for (const cell of range.getCells().flat() as CellComponent[]) {
      const field = cell.getField()
      const key = (cell.getData() as Row).__key
      const c = fieldSet.value.has(field) && canEditCell(key, field) ? info(key, field) : null
      if (c) out.push({ key, field, cell: c })
    }
  return out
}
function updateActions() {
  const next = props.editable && table ? selectionActions(selected().map(s => s.cell)) : { sheet: 0, ai: 0 }
  if (next.sheet !== actions.value.sheet || next.ai !== actions.value.ai) actions.value = next
}
/**
 * "Valor de la hoja": the selected cells go back to what the sheet has (a new
 * row's to empty); the assistant's value is kept aside, marked, and not
 * written. "Valor de la IA": they take the assistant's value again.
 */
function use(which: 'sheet' | 'ai') {
  const cells: CellEdit[] = []
  for (const { key, field, cell } of selected()) {
    if (which === 'sheet' && (cell.kind === 'proposed' || cell.kind === 'person'))
      cells.push({ key, field, value: null, before: cell.value, use: 'sheet' })
    if (which === 'ai' && cell.aiProposed && (cell.kind === 'reverted' || cell.kind === 'person'))
      cells.push({ key, field, value: cell.ai ?? null, before: cell.value, use: 'ai' })
  }
  if (cells.length) emit('edit', cells)
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
const layoutKey = () =>
  // The language is part of it: the column titles and tooltips are in it.
  [props.fields.join('|'), props.editable, props.applied ? 1 : 0, rules.value ? 1 : 0, locale.value].join('\n')
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
  const layout = layoutKey()
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
  updateActions()
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
let fit: { destroy: () => void } | null = null

onMounted(() => {
  if (!host.value) return
  byKey = new Map(props.changes.map(c => [rowKey(c), c]))
  shownColumns = layoutKey()
  table = new Tabulator(host.value, {
    data: [],
    index: '__key',
    columns: columns(),
    layout: 'fitData',
    // At most the height of the panel it is in (less its title and buttons), so the column names stay
    // in sight while scrolling the rows (the panel or chat around it is a size container: 100cqh).
    // (Less the cell bar above it.)
    maxHeight: 'max(10rem, calc(100cqh - 10rem))',
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
  for (const event of ['rangeAdded', 'rangeChanged', 'rangeRemoved'] as const) table.on(event as 'dataChanged', updateActions)
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
  fit = attachColumnFit(table, host.value, {
    text: (data, field) => {
      const change = byKey.get(String(data.__key))
      return fieldSet.value.has(field) && change ? drawnText(change, field) : String(data[field] ?? '')
    },
  })
  followSelection(table, showBar)
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
  fit?.destroy()
  sizeWatch?.disconnect()
  shownWatch?.disconnect()
  host.value?.removeEventListener('keydown', onKeydown)
  host.value?.removeEventListener('keydown', onEditingKey, true)
  table?.destroy()
  table = null
})
watch(
  () => [props.changes, props.fields, props.flash, props.editable, props.applied, rules.value, locale.value],
  sync,
)
</script>

<template>
  <div>
    <div v-if="$slots.default || editable" class="flex flex-wrap items-center gap-2 px-2 pt-1.5 text-[11px] text-stone-500">
      <slot />
      <template v-if="editable">
        <!-- Kept from taking the focus: the grid keeps its selection while the button is pressed. -->
        <button
          class="proposal-use"
          :disabled="!actions.sheet"
          :title="
            actions.sheet
              ? $t('Las celdas elegidas vuelven al valor de la hoja (en una fila nueva, vacías): la sugerencia de la IA queda marcada y no se aplica')
              : $t('Elige celdas cambiadas: arrastra sobre ellas o pulsa el nombre de una columna')
          "
          @mousedown.prevent
          @click="use('sheet')"
        >
          <FileSpreadsheet :size="12" /> {{ $t('Valor de la hoja') }}<template v-if="actions.sheet > 1"> ({{ actions.sheet }})</template>
        </button>
        <button
          class="proposal-use"
          :disabled="!actions.ai"
          :title="
            actions.ai
              ? $t('Las celdas elegidas vuelven a tomar el valor que propuso la IA')
              : $t('Elige celdas con una sugerencia de la IA que cambiaste o no se aplica')
          "
          @mousedown.prevent
          @click="use('ai')"
        >
          <Sparkles :size="12" /> {{ $t('Valor de la IA') }}<template v-if="actions.ai > 1"> ({{ actions.ai }})</template>
        </button>
      </template>
      <slot name="end" />
    </div>
    <div class="sheet-grid proposal-sheet">
      <CellBar :info="bar" @save="saveFromBar" @back="move => table && backToGrid(table, move)" />
      <!-- The grid's own box: the fill handle and the copied cells' border are placed in it, below the bar. -->
      <div class="relative">
        <div ref="host" tabindex="-1" />
      </div>
    </div>
  </div>
</template>
