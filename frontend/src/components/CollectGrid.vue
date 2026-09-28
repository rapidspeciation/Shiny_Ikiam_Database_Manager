<script setup lang="ts">
import { nextTick, onActivated, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { TabulatorFull as Tabulator } from 'tabulator-tables'
import type { CellComponent, ColumnDefinition, RowComponent } from 'tabulator-tables'
import 'tabulator-tables/dist/css/tabulator_simple.min.css'
import { FATES, HEADERS, SEX_VALUES, applies, notApplicable, type Column, type Draft } from '../lib/collect'
import {
  attachCopyMarker,
  attachFillHandle,
  attachTouchSheet,
  choiceEditor,
  openList,
  editingKeys,
  spreadsheetKeys,
  tileToSelection,
  typingPending,
  watchSize,
  type CanEdit,
} from '../lib/gridKit'
import { complete, parseBlock, stepId } from '../lib/paste'

/**
 * The Colecta list as a spreadsheet (on computers): select cells, copy and
 * paste ranges (also from Excel or Sheets), drag the fill handle, Ctrl+D,
 * typing replaces a cell. Every edit goes back to the list through `edit`,
 * which reads the value as the form does (♀/♂, "CAM · tubo", times…).
 */
const props = defineProps<{
  drafts: Draft[]
  places: string[]
  species: string[]
  subspeciesFor: (species: string) => string[]
  purposes: string[]
  /** Preservation_medium's list in the sheet. */
  mediums: string[]
  /** Spreads a block pasted at a row and column; false for a single value. */
  paste: (text: string, index: number, column: Column) => boolean
  /** The Insectary ID `step` places after `id` among the pre-made rows (the fill handle continues them). */
  nextId?: (id: string, step: number) => string | null
  /** Why a row's Insectary ID, CAM or tube cannot be saved (repeated, already used, no pre-made row). */
  idProblem?: (d: Draft, column: Column) => string | null
  /** A value outside the sheet's list for its column (red corner, as in Google Sheets). */
  cellProblem?: (d: Draft, column: Column) => string | null
}>()
const emit = defineEmits<{
  edit: [key: string, column: Column, text: string]
  remove: [key: string]
  notice: [message: string]
}>()

type Row = Record<Column, string> & { __key: string }
const host = ref<HTMLDivElement>()
let table: Tabulator | null = null
let built = false
let fill: { destroy: () => void } | null = null
let copied: ReturnType<typeof attachCopyMarker> | null = null
let sizeWatch: { disconnect: () => void } | null = null
const touch = window.matchMedia('(pointer: coarse)').matches

const toRow = (d: Draft): Row => ({
  __key: d.key,
  location: d.location,
  species: d.species,
  subspecies: d.subspecies,
  sex: d.sex,
  fate: d.fate,
  time: d.time,
  insectaryId: d.insectaryId,
  cam: d.cam,
  tube: d.tube,
  medium: d.medium,
  purpose: d.purpose,
  notes: d.notes,
})
const draftOf = (row: RowComponent) => props.drafts.find(d => d.key === (row.getData() as Row).__key)

/**
 * Insectary_ID only for butterflies going to the insectary (the suggestion can
 * be changed to the ID written on the wings); CAM, tube and medium only for
 * those preserved in the field.
 */
const canEdit: CanEdit = (row, field) => {
  if (field === '__key' || field === '__remove') return false
  const d = draftOf(row)
  return !d || applies(d, field as Column)
}
const whyNot = (row: RowComponent, field: string) => {
  const d = draftOf(row)
  return d && field in HEADERS && !applies(d, field as Column) ? notApplicable(field as Column) : null
}

/** Columns chosen from a list show a ▾ arrow; clicking it opens the list (see onCellClick). */
const choices = (values: () => string[]) => ({ cssClass: 'has-choices', ...choiceEditor(() => values()) })

const ID_COLUMNS: Column[] = ['insectaryId', 'cam', 'tube']
/**
 * A cell as the sheet will check it: grey where the column does not apply to
 * the row's Release_Collect, red corner for a value outside the sheet's list,
 * red for an ID, CAM or tube that is repeated or already used.
 */
const display = (field: Column, text?: (value: unknown) => string) => (cell: CellComponent) => {
  const d = draftOf(cell.getRow())
  const el = cell.getElement()
  const off = !!d && !applies(d, field)
  const invalid = (!off && d && props.cellProblem?.(d, field)) || null
  const error = (!off && d && ID_COLUMNS.includes(field) && props.idProblem?.(d, field)) || null
  el.classList.toggle('is-off', off)
  el.classList.toggle('is-invalid', !!invalid)
  el.classList.toggle('is-error', !!error)
  el.classList.toggle('is-id', field === 'insectaryId' && !off)
  el.title =
    error || invalid || (field === 'insectaryId' && !off ? 'El ID escrito en las alas (se sugiere el siguiente libre)' : '')
  return off ? '' : text ? text(cell.getValue()) : String(cell.getValue() ?? '')
}

function columns(): ColumnDefinition[] {
  const text = (field: Column, width: number, extra: Partial<ColumnDefinition> = {}) => ({
    title: HEADERS[field],
    field,
    width,
    formatter: display(field),
    editor: 'input' as const,
    editable: (cell: CellComponent) => canEdit(cell.getRow(), field),
    ...extra,
  })
  return [
    text('location', 200, choices(() => props.places)),
    text('species', 210, choices(() => props.species)),
    text('subspecies', 160, {
      cssClass: 'has-choices',
      ...choiceEditor(cell => props.subspeciesFor(String((cell.getData() as Row).species || ''))),
    }),
    text('sex', 90, {
      cssClass: 'has-choices',
      // Typing works too: f / h / ♀, m / ♂, ? (read like a pasted value).
      ...choiceEditor(() => [...SEX_VALUES]),
    }),
    text('fate', 210, {
      cssClass: 'has-choices',
      ...choiceEditor(() => Object.fromEntries(Object.entries(FATES).map(([k, f]) => [k, f.label]))),
      formatter: display('fate', value => FATES[value as keyof typeof FATES]?.label ?? ''),
    }),
    text('time', 110),
    text('insectaryId', 110),
    text('cam', 120),
    text('tube', 125),
    text('medium', 190, choices(() => props.mediums)),
    text('purpose', 150, choices(() => props.purposes)),
    text('notes', 220),
    {
      title: '',
      field: '__remove',
      width: 36,
      headerSort: false,
      hozAlign: 'center',
      formatter: () => '✕',
      cssClass: 'row-remove',
      cellClick: (_e, cell) => emit('remove', (cell.getData() as Row).__key),
    },
  ]
}

// What the grid shows, per row, to send it only the rows that change.
let shown = new Map<string, string>()
let shownOrder = ''
/** The last redraw sent to the grid (selecting a cell waits for it: new data resets the selection). */
let drawn: Promise<unknown> = Promise.resolve()
let builtNow = () => {}
const whenBuilt = new Promise<void>(resolve => (builtNow = resolve))
let stale = false
let repaintDue = false
let retry: number | undefined
/**
 * A cell is being edited, or keys typed on it wait for its editor. Redrawing
 * then threw the editor away with the letters typed so far (the list changes
 * while typing: IDs arrive, the row before was just saved).
 */
const busy = () => !!host.value?.querySelector('.tabulator-editing') || typingPending()
function later() {
  window.clearTimeout(retry)
  retry = window.setTimeout(flush, 250)
}
/** Whatever waited for an edit to end. */
function flush() {
  if (stale) sync()
  else if (repaintDue) repaint()
}
function sync() {
  if (!table || !built) return
  if (busy()) {
    stale = true
    return later()
  }
  stale = false
  const rows = props.drafts.map(toRow)
  const order = rows.map(r => r.__key).join('|')
  if (order !== shownOrder) {
    drawn = table.replaceData(rows)
  } else {
    const changed = rows.filter(r => shown.get(r.__key) !== JSON.stringify(r))
    // The list is short: repaint every row, since a change in one (e.g. an ID) can flag another.
    if (changed.length) drawn = table.updateData(changed).then(repaint)
  }
  shownOrder = order
  shown = new Map(rows.map(r => [r.__key, JSON.stringify(r)]))
}
function repaint() {
  if (!table) return
  if (busy()) {
    repaintDue = true
    return later()
  }
  repaintDue = false
  table.getRows().forEach(row => row.reformat())
}

/**
 * Enter after typing the start of a value takes the one option it begins
 * ("Ithomia sal" → "Ithomia salapia"); a new value is kept as typed. Sex and
 * Release_Collect are read the same way by the list (he → female, pres → Collected_Preserved).
 */
function completed(field: Column, text: string, row: Row) {
  const options: Partial<Record<Column, () => string[]>> = {
    location: () => props.places,
    species: () => props.species,
    subspecies: () => props.subspeciesFor(row.species),
    purpose: () => props.purposes,
    medium: () => props.mediums,
  }
  return options[field] ? complete(text, options[field]!()) : text
}

const onKeydown = spreadsheetKeys(() => table, canEdit, message => emit('notice', message), whyNot)
const onEditingKey = editingKeys(() => table)

onMounted(() => {
  if (!host.value) return
  table = new Tabulator(host.value, {
    data: [],
    index: '__key',
    columns: columns(),
    // Row numbers as Tabulator's row header, which range selection expects.
    rowHeader: { formatter: 'rownum', headerSort: false, resizable: false, frozen: true, width: 44, hozAlign: 'right', cssClass: 'row-number' },
    layout: 'fitData',
    // Size changes go through watchSize: a redraw under an open editor (the phone keyboard resizes the page) lost it.
    autoResize: false,
    headerSortClickElement: 'icon',
    placeholder: 'Sin filas',
    selectableRange: 1,
    selectableRangeColumns: true,
    selectableRangeRows: true,
    selectableRangeClearCells: false,
    editTriggerEvent: 'dblclick',
    clipboard: true,
    // Copied as shown (♀, Al insectario), which reads well in a spreadsheet and pastes back the same.
    clipboardCopyConfig: { columnHeaders: false, rowHeaders: false, formatCells: true },
    clipboardCopyRowRange: 'range',
    // Pasting goes through the list (it spreads blocks and adds rows); a single value goes to the cell.
    clipboardPasteParser: (text: string) => {
      const range = table?.getRanges()[0]
      const cell = (range?.getCells().flat() as CellComponent[] | undefined)?.[0]
      if (!range || !cell) return false
      const field = cell.getField() as Column
      const index = props.drafts.findIndex(d => d.key === (cell.getData() as Row).__key)
      if (index < 0 || !field || field === ('__remove' as Column)) return false
      // Fill the whole selection with the copied block, repeated (Google Sheets does the same).
      const copied = parseBlock(text) ?? [[text.replace(/\r?\n$/, '').trim()]]
      const block = tileToSelection(copied, range.getRows().length, range.getColumns().length)
      if (block.length === 1 && block[0].length === 1) {
        if (canEdit(cell.getRow(), field)) emit('edit', props.drafts[index].key, field, completed(field, block[0][0], cell.getData() as Row))
      } else props.paste(block.map(line => line.join('\t')).join('\n'), index, field)
      return false
    },
    clipboardPasteAction: () => [],
    columnDefaults: { headerSort: false },
  } as unknown as ConstructorParameters<typeof Tabulator>[1])
  table.on('tableBuilt', () => {
    built = true
    sync()
    builtNow()
  })
  // The ▾ arrow at a cell's right edge opens its list straight away.
  table.on('cellClick', (event: UIEvent, cell: CellComponent) => {
    const el = cell.getElement()
    if (!el.classList.contains('has-choices') || !(event instanceof MouseEvent) || !canEdit(cell.getRow(), cell.getField())) return
    if (event.clientX >= el.getBoundingClientRect().right - 22) openList(cell)
  })
  table.on('cellEdited', (cell: CellComponent) => {
    const row = cell.getData() as Row
    const field = cell.getField() as Column
    emit('edit', row.__key, field, completed(field, String(cell.getValue() ?? ''), row))
    // A value the list refused (e.g. a CAM typed as an Insectary ID) goes back to what the list holds.
    nextTick(() => {
      const d = props.drafts.find(x => x.key === row.__key)
      if (!d || !table) return
      if (JSON.stringify(toRow(d)) !== JSON.stringify(cell.getRow().getData())) {
        shown.delete(d.key)
        sync()
      } else flush()
    })
  })
  table.on('cellEditCancelled', () => nextTick(flush))
  const notice = (message: string) => emit('notice', message)
  fill = touch
    ? attachTouchSheet(table, host.value.parentElement!, { canEdit, notice })
    : attachFillHandle(table, host.value.parentElement!, {
        canEdit,
        // Dragging an ID continues it, as Sheets continues a number; other columns copy.
        series: (field, value, step) => {
          const id = String(value ?? '').trim()
          if (!id) return null
          if (field === 'insectaryId') return props.nextId?.(id, step) ?? null
          return field === 'cam' || field === 'tube' ? stepId(id, step) : null
        },
        onFilled: rows => notice(`Copiado a ${rows} ${rows === 1 ? 'fila' : 'filas'}`),
      })
  copied = attachCopyMarker(table, host.value.parentElement!, message => emit('notice', message))
  host.value.addEventListener('keydown', onKeydown)
  host.value.addEventListener('keydown', onEditingKey, true)
  sizeWatch = watchSize(() => table, host.value)
})
onActivated(() => table?.redraw())
onBeforeUnmount(() => {
  window.clearTimeout(retry)
  fill?.destroy()
  sizeWatch?.disconnect()
  copied?.destroy()
  host.value?.removeEventListener('keydown', onKeydown)
  host.value?.removeEventListener('keydown', onEditingKey, true)
  table?.destroy()
  table = null
})
watch(() => props.drafts.map(d => JSON.stringify(d)).join('\n'), sync)

/**
 * Select a cell and bring it into view (e.g. the first row just added). After
 * the new rows are drawn: new data puts the selection back on the first cell.
 */
async function focusCell(index: number, field: Column) {
  // The list's first rows also build the grid.
  await whenBuilt
  await drawn.catch(() => {})
  const row = table?.getRows()[index]
  if (!row || !table) return
  table.scrollToRow(row, 'center', false).catch(() => {})
  try {
    ;(table as unknown as { addRange: (a: CellComponent, b: CellComponent) => void }).addRange(row.getCell(field), row.getCell(field))
  } catch {
    /* Range selection is off on touch screens. */
  }
  // Keys go where Tabulator listens for them (its rows), as after a click.
  ;(table as unknown as { rowManager: { element: HTMLElement } }).rowManager.element.focus({ preventScroll: true })
}
defineExpose({ focusCell })
</script>

<template>
  <div class="sheet-grid collect-grid">
    <div ref="host" tabindex="-1" />
  </div>
</template>
